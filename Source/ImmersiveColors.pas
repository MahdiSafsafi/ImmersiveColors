// **************************************************************************************************
//
// Copyright (c) 2016-2019 Mahdi Safsafi.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// **************************************************************************************************

// https://github.com/MahdiSafsafi/ImmersiveColors

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF FPC}

unit ImmersiveColors;

interface

{$IFDEF MSWINDOWS}
{$DEFINE WINOS}
{$ENDIF MSWINDOWS}

{$IF DEFINED (DCC)}


uses
  System.Classes,
  System.TypInfo,
  System.SysUtils,
  System.UITypes,
{$IFDEF WINOS}
  System.Win.Registry,
  WinApi.Windows,
  WinApi.Messages,
{$ENDIF WINOS}
  Vcl.Graphics;
{$ELSE IF DEFINED (FPC)}


uses
  SysUtils,
  Classes,
  Registry,
  Graphics,
  Windows;
{$ENDIF}

{$IFDEF FPC}


type
  TAlphaColor = Cardinal;
{$ENDIF FPC}

{$I ImmersiveType.inc}


const
  INVALID_COLORSET = Cardinal(-1);

type
  TColorChangedEvent = procedure(UserTag: Pointer);
  TEnumImmersiveColorNamesProc = function(ColorType: TImmersiveColorType;
    const Name: string; UserTag: Pointer): Boolean;

  /// <summary> Returns Immersive ColorSet count.
  /// </summary>
function GetImmersiveColorSetCount(): Cardinal; {$IFNDEF WINOS}inline; {$ENDIF}

/// <summary> Returns active immersive ColorSet.
/// </summary>
function GetActiveImmersiveColorSet(): Cardinal; {$IFNDEF WINOS}inline; {$ENDIF}

/// <summary> Returns the name of a given ColorType.
/// </summary>
function GetImmersiveColorName(ColorType: TImmersiveColorType): string;

/// <summary> Enumerate all ColorType names.
/// </summary>
procedure EnumImmersiveColorTypeNames(Proc: TEnumImmersiveColorNamesProc; UserTag: Pointer);

/// <summary> Returns color's value from a given ColorSet and ColorType.
/// </summary>
function GetImmersiveColor(ColorSet: Cardinal; ColorType: TImmersiveColorType;
  Default: TAlphaColor = clBlack): TAlphaColor; {$IFNDEF WINOS}inline; {$ENDIF}

/// <summary> Returns the active color's value from a given ColorType.
/// </summary>
function GetActiveImmersiveColor(ColorType: TImmersiveColorType;
  Default: TAlphaColor = clBlack): TAlphaColor; {$IFNDEF WINOS}inline; {$ENDIF}

/// <summary> Compares two ColorSet.
/// </summary>
function SameColorSet(ColorSet1, ColorSet2: Cardinal): Boolean;

/// <summary> Register a procedure that will be called when the system settings change.
/// </summary>
function RegisterNotifyEvent(const Event: TColorChangedEvent; UserTag: Pointer): Boolean; {$IFNDEF WINOS}inline; {$ENDIF}
function UnRegisterNotifyEvent(const Event: TColorChangedEvent): Boolean; {$IFNDEF WINOS}inline; {$ENDIF}

/// <summary> Returns True if the Dark Mode is currently in use.
/// </summary>
function IsDarkThemeActive(): Boolean; {$IFNDEF WINOS}inline; {$ENDIF}

/// <summary> Returns True if the OS supports immersive colors.
/// </summary>
function OsSupportsImmersiveColors(): Boolean; {$IFNDEF WINOS}inline; {$ENDIF}

function IsColorTypeDark(ColorType: TImmersiveColorType): Boolean;
function IsColorTypeLight(ColorType: TImmersiveColorType): Boolean;
function GetRivalColorType(ColorType: TImmersiveColorType): TImmersiveColorType;
function AlphaColorToColor(AlphaColor: TAlphaColor): TColor; inline;

implementation

// === global variables ===//
var
  ThemeLibModuleHandle: HMODULE = 0;
  FOsSupportsImmersiveColors: Boolean = False;
  FColorSetChangedEvents: TList = nil;
  FIsWindows8: Boolean = False;
  FIsWindows10: Boolean = False;

type
  TEventData = record
    Event: Pointer;
    UserTag: Pointer;
  end;

  PEventData = ^TEventData;

  TImmersiveEntry = record
    Name: PWideChar;
    Flags: SmallInt;
    Rival: SmallInt;
  end;

  PImmersiveEntry = ^TImmersiveEntry;

{$I ImmersiveTable.inc}

{$IFDEF WINOS}


type
  TSystemSettingsChanged = class(TThread)
  private
    FKeyHandle: HKEY;
    FNotifyFilter: DWORD;
    FEvent: THandle;
    FSuccess: Boolean;
  protected
    procedure Execute; override;
  public
    procedure Release;
    constructor Create; overload;
  end;

const
  themelib = 'uxtheme.dll';
  UM_SYSTEM_SETTINGS_CHANGED = WM_USER + $50;

  { ===> Undocumented UxTheme functions <=== }

type
  TGetImmersiveColorSetCount = function: Cardinal; stdcall;
  TGetImmersiveColorFromColorSetEx = function(dwImmersiveColorSet: UInt; dwImmersiveColorType: Integer; bIgnoreHighContrast: Bool;
    dwHighContrastCacheMode: UInt): UInt; stdcall;
  TGetImmersiveColorTypeFromName = function(pName: PWideChar): Integer; stdcall;
  TGetImmersiveUserColorSetPreference = function(bForceCheckRegistry: Bool; bSkipCheckOnFail: Bool): Integer; stdcall;
  TGetImmersiveColorNamedTypeByIndex = function(dwIndex: UInt): IntPtr; stdcall;
  TStaticWndMethod = function(Handle: HWND; MSG: UInt; WPARAM: WPARAM; LPARAM: LPARAM): LRESULT; stdcall;

var
  VGetImmersiveColorSetCount: TGetImmersiveColorSetCount = nil;
  VGetImmersiveColorFromColorSetEx: TGetImmersiveColorFromColorSetEx = nil;
  VGetImmersiveColorTypeFromName: TGetImmersiveColorTypeFromName = nil;
  VGetImmersiveUserColorSetPreference: TGetImmersiveUserColorSetPreference = nil;
  VGetImmersiveColorNamedTypeByIndex: TGetImmersiveColorNamedTypeByIndex = nil;
  UtilWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TPUtilWindow');
  Window: HWND = INVALID_HANDLE_VALUE;
  SystemSettingsChanged: TSystemSettingsChanged = nil;

function StaticAllocateHWnd(const AMethod: TStaticWndMethod): HWND;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  UtilWindowClass.hInstance := hInstance;
  TempClass := Default (TWndClass);
  ClassRegistered := GetClassInfo(hInstance, UtilWindowClass.lpszClassName,
    TempClass);
  if not ClassRegistered or (@TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassRegistered then
      UnregisterClass(UtilWindowClass.lpszClassName, hInstance);
    RegisterClass(UtilWindowClass);
  end;
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, UtilWindowClass.lpszClassName,
    '', WS_POPUP { + 0 } , 0, 0, 0, 0, 0, 0, hInstance, nil);
  if Assigned(AMethod) then
    SetWindowLongPtr(Result, GWL_WNDPROC, IntPtr(@AMethod));
end;

procedure StaticDeallocateHWnd(Wnd: HWND);
begin
  DestroyWindow(Wnd);
end;

function WndProc(Handle: HWND; MSG: UInt; WPARAM: WPARAM;
  LPARAM: LPARAM): LRESULT; stdcall;
  procedure DoNotify;
  var
    i: Integer;
    Event: TColorChangedEvent;
    P: PEventData;
  begin
    for i := 0 to FColorSetChangedEvents.Count - 1 do
    begin
      P := FColorSetChangedEvents[i];
      if (Assigned(P)) then
      begin
        @Event := P^.Event;
        Event(P^.UserTag);
      end;
    end;
  end;

var
  Message: TMessage;

begin
  case MSG of
    UM_SYSTEM_SETTINGS_CHANGED:
      begin
        DoNotify();
        Result := 0;
        exit();
      end;
    WM_SETTINGCHANGE:
      begin
        Message.MSG := MSG;
        Message.WPARAM := WPARAM;
        Message.LPARAM := LPARAM;
        if (lstrcmpi(TWMSettingChange(Message).Section, 'ImmersiveColorSet') = 0) then
        begin
          DoNotify();
        end;
      end;
  end;
  Result := DefWindowProc(Handle, MSG, WPARAM, LPARAM);
end;

function IsWindows8(): Boolean;
var
  Vesrion: TOSVersionInfo;
begin
  Vesrion.dwOSVersionInfoSize := SizeOf(Vesrion);
  Result := GetVersionEx(Vesrion);
  if (Result) then
    Result := (Vesrion.dwMajorVersion = 6) and ((Vesrion.dwMinorVersion = 2)
      or (Vesrion.dwMinorVersion = 3));
end;

function IsWindows10(): Boolean;
var
  Vesrion: TOSVersionInfo;
begin
  Vesrion.dwOSVersionInfoSize := SizeOf(Vesrion);
  Result := GetVersionEx(Vesrion);
  if (Result) then
    Result := (Vesrion.dwMajorVersion = 10);
end;

function SameColorSet(ColorSet1, ColorSet2: Cardinal): Boolean;
var
  LColorType: TImmersiveColorType;
  LColor: TAlphaColor;
  LColor2: TAlphaColor;
begin
  if (ColorSet1 = ColorSet2) then
    exit(True);
  for LColorType := Low(TImmersiveColorType) to High(TImmersiveColorType) do
  begin
    LColor := GetImmersiveColor(ColorSet1, LColorType);
    LColor2 := GetImmersiveColor(ColorSet2, LColorType);
    if (LColor <> LColor2) then
      exit(False);
  end;
  Result := True;
end;

function OsSupportsImmersiveColors(): Boolean;
begin
  Result := FOsSupportsImmersiveColors;
end;

function IsDarkThemeActive(): Boolean;
const
  Personalize = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  AppsUseLightTheme = 'AppsUseLightTheme';
var
  Registry: TRegistry;
begin
  // TODO: use RegNotifyChangeKeyValue !
  if (not FIsWindows10) then
    exit(False);
  Result := False;
  Registry := TRegistry.Create();
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if (Registry.KeyExists(Personalize)) then
    begin
      Registry.OpenKey(Personalize, False);
      if (Registry.ValueExists(AppsUseLightTheme)) then
        Result := not Registry.ReadBool(AppsUseLightTheme);
      Registry.CloseKey();
    end;
  finally
    Registry.Free();
  end;
end;

function GetActiveImmersiveColorSet(): Cardinal;
begin
  if (FOsSupportsImmersiveColors) then
    Result := VGetImmersiveUserColorSetPreference(False, False)
  else
    Result := INVALID_COLORSET;
end;

function GetImmersiveColor(ColorSet: Cardinal;
  ColorType: TImmersiveColorType; Default: TAlphaColor = clBlack): TAlphaColor;
var
  i: UInt;
  Entry: PImmersiveEntry;
begin
  if (FOsSupportsImmersiveColors) then
  begin
    Entry := @ImmersiveTable[ColorType];
    i := VGetImmersiveColorTypeFromName(Entry^.Name);
    Result := VGetImmersiveColorFromColorSetEx(ColorSet, i, False, 0);
  end
  else
  begin
    Result := Default;
  end;
end;

function GetActiveImmersiveColor(ColorType: TImmersiveColorType;
  Default: TAlphaColor = clBlack): TAlphaColor;
var
  ColorSet: Cardinal;
begin
  ColorSet := GetActiveImmersiveColorSet();
  if (ColorSet <> INVALID_COLORSET) then
    Result := GetImmersiveColor(ColorSet, ColorType, Default)
  else
    Result := Default;
end;

function GetImmersiveColorSetCount(): Cardinal;
begin
  if (FOsSupportsImmersiveColors) then
    Result := VGetImmersiveColorSetCount()
  else
    Result := 0;
end;

function FindEvent(Event: Pointer): Integer;
var
  i: Integer;
  P: PEventData;
begin
  for i := 0 to FColorSetChangedEvents.Count - 1 do
  begin
    P := FColorSetChangedEvents[i];
    if (Assigned(P) and (P^.Event = Event)) then
      exit(i);
  end;
  Result := -1;
end;

function RegisterNotifyEvent(const Event: TColorChangedEvent; UserTag: Pointer): Boolean;
var
  P: PEventData;
begin
  if (FOsSupportsImmersiveColors) then
  begin
    if FindEvent(@Event) <> -1 then
      exit(False);
    P := GetMemory(SizeOf(TEventData));
    P^.Event := @Event;
    P^.UserTag := UserTag;
    FColorSetChangedEvents.Add(P);
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function UnRegisterNotifyEvent(const Event: TColorChangedEvent): Boolean;
var
  i: Integer;
  P: PEventData;
begin
  i := FindEvent(@Event);
  Result := i <> -1;
  if (Result) then
  begin
    P := FColorSetChangedEvents[i];
    FreeMem(P);
    FColorSetChangedEvents[i] := nil;
  end;
end;

{ TSystemSettingsChanged }

constructor TSystemSettingsChanged.Create;
const
  Personalize = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
var
  ErrorCode: DWORD;
begin
  FSuccess := False;
  FEvent := CreateEvent(nil, True, False, nil);
  if (FEvent <> 0) then
  begin
    FNotifyFilter := REG_NOTIFY_CHANGE_NAME or
      REG_NOTIFY_CHANGE_ATTRIBUTES or
      REG_NOTIFY_CHANGE_LAST_SET or
      REG_NOTIFY_CHANGE_SECURITY;
    ErrorCode := RegOpenKeyEx(HKEY_CURRENT_USER, Personalize, 0, KEY_NOTIFY or KEY_READ, FKeyHandle);
    FSuccess := (ErrorCode = ERROR_SUCCESS);
    if not FSuccess then
      CloseHandle(FEvent);
  end;
  inherited;
end;

procedure TSystemSettingsChanged.Release;
begin
  if (FEvent <> 0) then
  begin
    SetEvent(FEvent);
  end;
end;

procedure TSystemSettingsChanged.Execute;
var
  ErrorCode: DWORD;
  ReturnCode: DWORD;
begin
  if not FSuccess then
    exit();

  while (True) do
  begin
    ErrorCode := RegNotifyChangeKeyValue(FKeyHandle, False, FNotifyFilter, FEvent, True);
    if (ErrorCode <> ERROR_SUCCESS) then
      break;
    ReturnCode := WaitForSingleObject(FEvent, INFINITE);
    case ReturnCode of
      WAIT_FAILED:
        break;
    end;
    if Terminated then
      break;
    ResetEvent(FEvent);
    if ((Window <> INVALID_HANDLE_VALUE) and IsWindow(Window)) then
      PostMessage(Window, UM_SYSTEM_SETTINGS_CHANGED, 0, 0)
    else
      break;
  end;
  RegCloseKey(FKeyHandle);
  CloseHandle(FEvent);
end;

{$ELSE !WINOS}


function SameColorSet(ColorSet1, ColorSet2: Cardinal): Boolean;
begin
  Result := (ColorSet1 = ColorSet2);
end;

function GetImmersiveColorSetCount(): Cardinal;
begin
  Result := 0;
end;

function GetActiveImmersiveColorSet(): Cardinal;
begin
  Result := INVALID_COLORSET;
end;

function GetImmersiveColor(ColorSet: Cardinal; ColorType: TImmersiveColorType;
  Default: TAlphaColor = clBlack): TAlphaColor;
begin
  Result := Default;
end;

function GetActiveImmersiveColor(ColorType: TImmersiveColorType;
  Default: TAlphaColor = clBlack): TAlphaColor;
begin
  Result := Default;
end;

function RegisterNotifyEvent(const Event: TColorChangedEvent; UserTag: Pointer): Boolean;
begin
  Result := False;
end;

function UnRegisterNotifyEvent(const Event: TColorChangedEvent): Boolean;
begin
  Result := False;
end;

function IsDarkThemeActive(): Boolean;
begin
  Result := False;
end;

function OsSupportsImmersiveColors(): Boolean;
begin
  Result := False;
end;
{$ENDIF WINOS}


function GetImmersiveColorName(ColorType: TImmersiveColorType): string;
var
  Entry: PImmersiveEntry;
begin
  Entry := @ImmersiveTable[ColorType];
  Result := string(Entry^.Name);
end;

procedure EnumImmersiveColorTypeNames(Proc: TEnumImmersiveColorNamesProc; UserTag: Pointer);
var
  LColorType: TImmersiveColorType;
  Entry: PImmersiveEntry;
begin
  for LColorType := Low(TImmersiveColorType) to High(TImmersiveColorType) do
  begin
    Entry := @ImmersiveTable[LColorType];
    if (not Proc(LColorType, Entry^.Name, UserTag)) then
      exit();
  end;
end;

function IsColorTypeDark(ColorType: TImmersiveColorType): Boolean;
var
  Entry: PImmersiveEntry;
begin
  Entry := @ImmersiveTable[ColorType];
  Result := (Entry^.Flags and 1) <> 0;
end;

function IsColorTypeLight(ColorType: TImmersiveColorType): Boolean;
var
  Entry: PImmersiveEntry;
begin
  Entry := @ImmersiveTable[ColorType];
  Result := (Entry^.Flags and 2) <> 0;
end;

function GetRivalColorType(ColorType: TImmersiveColorType): TImmersiveColorType;
var
  Entry: PImmersiveEntry;
begin
  Entry := @ImmersiveTable[ColorType];
  Result := TImmersiveColorType(Entry^.Rival);
end;

function AlphaColorToColor(AlphaColor: TAlphaColor): TColor;
begin
  Result := AlphaColor and $00FFFFFF;
end;

procedure DoInitialization();
begin
  FColorSetChangedEvents := TList.Create();
{$IFDEF WINOS}
  FIsWindows10 := IsWindows10();
  if not FIsWindows10 then
    FIsWindows8 := IsWindows8();

  if not(FIsWindows10 or FIsWindows8) then
    exit();
  ThemeLibModuleHandle := GetModuleHandle(themelib);
  if (ThemeLibModuleHandle <> 0) then
  begin
    VGetImmersiveColorSetCount := TGetImmersiveColorSetCount(GetProcAddress(ThemeLibModuleHandle, MakeIntResource(94)));
    FOsSupportsImmersiveColors := Assigned(VGetImmersiveColorSetCount);
    if (FOsSupportsImmersiveColors) then
    begin
      @VGetImmersiveColorFromColorSetEx := GetProcAddress(ThemeLibModuleHandle,
        MakeIntResource(95));
      @VGetImmersiveColorTypeFromName := GetProcAddress(ThemeLibModuleHandle,
        MakeIntResource(96));
      @VGetImmersiveUserColorSetPreference := GetProcAddress(ThemeLibModuleHandle,
        MakeIntResource(98));
      @VGetImmersiveColorNamedTypeByIndex := GetProcAddress(ThemeLibModuleHandle,
        MakeIntResource(100));
      Window := StaticAllocateHWnd(@WndProc);
      // SystemSettingsChanged := TSystemSettingsChanged.Create();
    end;
  end;
{$ENDIF !WINOS}
end;

procedure DoFinalization();
var
  P: PEventData;
  i: Integer;
begin
  for i := 0 to FColorSetChangedEvents.Count - 1 do
  begin
    P := FColorSetChangedEvents[i];
    if (Assigned(P)) then
      FreeMemory(P);
  end;
  FColorSetChangedEvents.Free();
{$IFDEF WINOS}
  if (Assigned(SystemSettingsChanged)) then
  begin
    SystemSettingsChanged.Terminate();
    SystemSettingsChanged.Release();
    FreeAndNil(SystemSettingsChanged);
  end;
  if (Window <> INVALID_HANDLE_VALUE) then
    StaticDeallocateHWnd(Window);
{$ENDIF !WINOS}
end;

initialization

DoInitialization();

finalization

DoFinalization();

end.
