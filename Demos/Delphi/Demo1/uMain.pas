unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ImmersiveColorsControls,
  Vcl.ExtCtrls, System.UITypes;

type
  TMain = class(TForm)
    ImmersiveNotify1: TImmersiveNotify;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImmersiveNotify1Change(Sender: TObject);
  private
    { Private declarations }
    FLabels: TList;
  public
    { Public declarations }
    procedure UpdateColors;
  end;

var
  Main: TMain;

implementation

{$R *.dfm}


uses
  System.Math,
  ImmersiveColors;

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FLabels.Free();
end;

procedure TMain.FormCreate(Sender: TObject);
const
  LArray: array [0 .. 5] of TImmersiveColorType = (
    ImmersiveSaturatedDisabledText,
    ImmersiveSystemAccentLight1,
    ImmersiveSystemAccentLight2,
    ImmersiveSystemAccentLight3,
    ImmersiveLightSelectedTabText,
    ImmersiveControlLightSelectHighlightSelected
    );
var
  i: Integer;
  Y: Integer;
  LColorType: TImmersiveColorType;
  LLabel: TLabel;
begin
  FLabels := TList.Create();
  Y := 0;
  for i := 0 to Length(LArray) - 1 do
  begin
    LColorType := LArray[i];
    LLabel := TLabel.Create(Self);
    FLabels.Add(LLabel);
    LLabel.Parent := Self;
    LLabel.Caption := GetImmersiveColorName(LColorType);
    LLabel.Tag := NativeInt(LColorType);
    LLabel.Top := Y;
    LLabel.Left := 4;
    Inc(Y, LLabel.Height + 4);
  end;
  UpdateColors();
end;

procedure TMain.ImmersiveNotify1Change(Sender: TObject);
begin
  UpdateColors();
end;

procedure TMain.UpdateColors;
var
  i: Integer;
  LLabel: TLabel;
  LColorType: TImmersiveColorType;
  IsDark: Boolean;
begin
  IsDark := IsDarkThemeActive();
  if (IsDark) then
    Color := $202020
  else
    Color := clDefault;
  for i := 0 to FLabels.Count - 1 do
  begin
    LLabel := TLabel(FLabels[i]);
    LColorType := TImmersiveColorType(LLabel.Tag);
    if (IsDark) then
      LColorType := GetRivalColorType(LColorType);
    LLabel.Font.Color := AlphaColorToColor(GetActiveImmersiveColor(LColorType, clBlack));
  end;
end;

end.
