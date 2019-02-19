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

unit ImmersiveColorsControls;

interface

uses
{$IFDEF DCC}
  System.SysUtils,
  System.Classes,
  System.Math,
  WinApi.Windows,
  WinApi.Messages,
  System.UITypes,
  System.Types,
  Vcl.Graphics,
  Vcl.StdCtrls,
  ImmersiveColors;
{$ELSE !DCC}
  SysUtils,
  Classes,
  Math,
  Windows,
  Messages,
  Types,
  Graphics,
  StdCtrls,
  ImmersiveColors;
{$ENDIF DCC}


type
  TCustomImmersiveColorsListBox = class(TCustomListBox)
  private
    FImmersiveColorSet: Cardinal;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetImmersiveColorSet(const Value: Cardinal);
  protected
    procedure LoadImmersiveColors();
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ImmersiveColorSet: Cardinal read FImmersiveColorSet write SetImmersiveColorSet;
  end;

  TCustomImmersiveColorSetListBox = class(TCustomListBox)
  private
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure CreateWnd; override;
    procedure LoadImmersiveColorSet();
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TImmersiveColorSetListBox = class(TCustomImmersiveColorSetListBox)
  published
    property Align;
{$IFDEF DCC}
    property AutoComplete;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property Ctl3D;
    property ParentCtl3D;
    property Touch;
    property StyleElements;
    property OnGesture;
    property OnMouseActivate;
{$ENDIF DCC}
    property Style;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TImmersiveColorsListBox = class(TCustomImmersiveColorsListBox)
  published
    property ImmersiveColorSet;
    property Align;
{$IFDEF DCC}
    property AutoComplete;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property Ctl3D;
    property ParentCtl3D;
    property Touch;
    property StyleElements;
    property OnGesture;
    property OnMouseActivate;
{$ENDIF DCC}
    property Style;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TImmersiveNotify = class(TComponent)
  private
    FNotify: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnChange: TNotifyEvent read FNotify write FNotify;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ImmersiveColors', [TImmersiveColorsListBox, TImmersiveColorSetListBox, TImmersiveNotify]);
end;

{$HINTS Off}


const
  MaxHue: Integer = 239;
  MaxSat: Integer = 240;
  MaxLum: Integer = 240;
  FontColors: array [Boolean] of TColor = (clBlack, clWhite);

procedure RGBtoHSLRange(RGB: TColor; var H1, S1, L1: Integer);
var
  R, G, B, D, Cmax, Cmin, h, s, l: double;
begin
  // This function was taken from here :
  // http://www.delphipraxis.net/157099-fast-integer-rgb-hsl.html
  h := H1;
  s := S1;
  l := L1;
  R := GetRValue(RGB) / 255;
  G := GetGValue(RGB) / 255;
  B := GetBValue(RGB) / 255;
{$IFDEF FPC}
  Cmax := Max(Round(R), Max(Round(G), Round(B)));
  Cmin := Min(Round(R), Min(Round(G), Round(B)));
{$ELSE !FPC}
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
{$ENDIF FPC}
  l := (Cmax + Cmin) / 2;
  if Cmax = Cmin then
  begin
    h := 0;
    s := 0;
  end
  else
  begin
    D := Cmax - Cmin;
    // calc L
    if l < 0.5 then
      s := D / (Cmax + Cmin)
    else
      s := D / (2 - Cmax - Cmin);
    // calc H
    if R = Cmax then
      h := (G - B) / D
    else if G = Cmax then
      h := 2 + (B - R) / D
    else
      h := 4 + (R - G) / D;
    h := h / 6;
    if h < 0 then
      h := h + 1;
  end;
  H1 := Round(h * MaxHue);
  S1 := Round(s * MaxSat);
  L1 := Round(l * MaxLum);
end;
{$HINTS On}


function IsDark(RGB: TColor): Boolean; inline;
var
  h, s, l: Integer;
begin
  RGBtoHSLRange(RGB, h, s, l);
  Result := l < 100;
end;

{ TCustomImmersiveColorsListBox }

function EnumImmersiveColorNamesProc(ColorType: TImmersiveColorType;
  const Name: string; UserTag: Pointer): Boolean;
var
  Self: TCustomImmersiveColorsListBox;
begin
  Self := UserTag;
  Self.Items.AddObject(Name, TObject(ColorType));
  Result := True;
end;

constructor TCustomImmersiveColorsListBox.Create(AOwner: TComponent);
begin
  inherited;
  inherited Style := lbOwnerDrawFixed;
  FImmersiveColorSet := GetActiveImmersiveColorSet();
end;

procedure TCustomImmersiveColorsListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  LName: string;
  LColor: TColor;
{$IFDEF FPC}
  TextStyle: TTextStyle;
{$ENDIF FPC}
begin
  LName := Items[index];
  LColor := AlphaColorToColor(GetImmersiveColor(FImmersiveColorSet,
    TImmersiveColorType(Items.Objects[Index])));
  Canvas.Brush.Color := LColor;
  Canvas.FillRect(Rect);
  Canvas.Font.Color := FontColors[IsDark(LColor)];
{$IFDEF DCC}
  Canvas.TextRect(Rect, LName, [tfSingleLine, tfCenter]);
{$ELSE !DCC}
  TextStyle := Default (TTextStyle);
  TextStyle.Alignment := taCenter;
  TextStyle.Layout := tlCenter;
  Canvas.TextRect(Rect, 0, 0, LName, TextStyle);
{$ENDIF DCC}
end;

procedure TCustomImmersiveColorsListBox.CreateWnd;
begin
  inherited;
  LoadImmersiveColors();
end;

procedure TCustomImmersiveColorsListBox.LoadImmersiveColors;
begin
  Items.BeginUpdate();
  EnumImmersiveColorTypeNames(EnumImmersiveColorNamesProc, Self);
  Items.EndUpdate();
end;

procedure TCustomImmersiveColorsListBox.SetImmersiveColorSet(const Value: Cardinal);
begin
  if (FImmersiveColorSet <> Value) then
  begin
    FImmersiveColorSet := Value;
    Invalidate();
  end;
end;

procedure TCustomImmersiveColorsListBox.WMSize(var Message: TWMSize);
begin
  inherited;
  Invalidate();
end;

{ TCustomImmersiveColorSetListBox }

constructor TCustomImmersiveColorSetListBox.Create(AOwner: TComponent);
begin
  inherited;
  inherited Style := lbOwnerDrawFixed;
end;

procedure TCustomImmersiveColorSetListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  LName: string;
  LColor: TColor;
{$IFDEF FPC}
  TextStyle: TTextStyle;
{$ENDIF FPC}
begin
  LName := Items[index];
  LColor := AlphaColorToColor(GetImmersiveColor(Cardinal(Items.Objects[Index]),
    ImmersiveStartBackground));
  Canvas.Brush.Color := LColor;
  Canvas.FillRect(Rect);
  if (not LName.IsEmpty) then
  begin
    Canvas.Font.Color := FontColors[IsDark(LColor)];
{$IFDEF DCC}
    Canvas.TextRect(Rect, LName, [tfSingleLine, tfCenter]);
{$ELSE !DCC}
    TextStyle := Default (TTextStyle);
    TextStyle.Alignment := taCenter;
    TextStyle.Layout := tlCenter;
    Canvas.TextRect(Rect, 0, 0, LName, TextStyle);
{$ENDIF DCC}
  end;
end;

procedure TCustomImmersiveColorSetListBox.CreateWnd;
begin
  inherited;
  LoadImmersiveColorSet();
end;

procedure TCustomImmersiveColorSetListBox.LoadImmersiveColorSet;
var
  i: Cardinal;
  n: Cardinal;
begin
  if (OsSupportsImmersiveColors) then
  begin
    Items.BeginUpdate();
    n := GetImmersiveColorSetCount();
    Items.AddObject('Active', TObject(GetActiveImmersiveColorSet));
    for i := 0 to n - 1 do
      Items.AddObject('', TObject(i));
    Items.EndUpdate();
  end;
end;

procedure TCustomImmersiveColorSetListBox.WMSize(var Message: TWMSize);
begin
  inherited;
  Invalidate();
end;

{ TImmersiveNotify }

procedure Changed(UserTag: Pointer);
var
  Self: TImmersiveNotify;
begin
  Self := UserTag;
  if (Assigned(Self) and Assigned(Self.FNotify)) then
    Self.FNotify(Self);
end;

constructor TImmersiveNotify.Create(AOwner: TComponent);
begin
  inherited;
  FNotify := nil;
  RegisterNotifyEvent(Changed, Self);
end;

destructor TImmersiveNotify.Destroy;
begin

  inherited;
end;

end.
