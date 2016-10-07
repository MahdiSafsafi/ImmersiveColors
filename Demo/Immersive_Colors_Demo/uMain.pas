unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uImmersiveColors, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TMain = class(TForm)
    LstColorTypes: TListBox;
    LstColorSets: TListBox;
    CheckBox1: TCheckBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    PaintBox1: TPaintBox;
    RichEdit1: TRichEdit;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label4: TLabel;
    Panel4: TPanel;
    PaintBox2: TPaintBox;
    CheckBox2: TCheckBox;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LstColorTypesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure LstColorSetsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure CheckBox1Click(Sender: TObject);
    procedure LstColorSetsClick(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox2MouseEnter(Sender: TObject);
    procedure PaintBox2MouseLeave(Sender: TObject);
    procedure PaintBox2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox2Paint(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    FImmersiveColors: TImmersiveColors;
    FColorSet: TColorSet;
    procedure RefreshColors;
    procedure ColorSetChanged(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

const
  AC_HOVER = 1;
  AC_DOWN = 2;

procedure TMain.CheckBox2Click(Sender: TObject);
begin
  PaintBox2.Enabled := CheckBox2.Checked;
end;

procedure TMain.CheckBox1Click(Sender: TObject);
begin
  LstColorSets.Enabled := not TCheckBox(Sender).Checked;
  if TCheckBox(Sender).Checked then
    FColorSet := FImmersiveColors.ActiveColorSet;
  RefreshColors;
end;

procedure TMain.ColorSetChanged(Sender: TObject);
begin
  RefreshColors;
end;

procedure TMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Label1.Caption := 'Try to change your desktop wallpaper' + #13#10 + 'if ColorSet is selected from wallpaper !';
  FImmersiveColors := TImmersiveColors.Create;
  FImmersiveColors.OnColorSetChanged := ColorSetChanged;
  FColorSet := FImmersiveColors.ActiveColorSet;
  for I := 0 to FImmersiveColors.ColorTypeNames.Count - 1 do
  begin
    LstColorTypes.Items.Add(FImmersiveColors.ColorTypeNames[I]);
  end;
  for I := 0 to FImmersiveColors.ColorSetCount do
    LstColorSets.Items.Add(IntToStr(I));

  RichEdit1.SelStart := 0;
  RichEdit1.SelLength := 6;
  RichEdit1.SelAttributes.Style := [fsBold];

  LstColorSets.Enabled := not TCheckBox(Sender).Checked;

  RefreshColors;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FImmersiveColors.Free;
end;

var
  MaxHue: Integer = 239;
  MaxSat: Integer = 240;
  MaxLum: Integer = 240;
{$HINTS Off}

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
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  l := (Cmax + Cmin) / 2;
  if Cmax = Cmin then
  begin
    h := 0;
    s := 0;
  end else begin
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
  H1 := round(h * MaxHue);
  S1 := round(s * MaxSat);
  L1 := round(l * MaxLum);
end;
{$HINTS On}

function IsDark(RGB: TColor): Boolean;
var
  h, s, l: Integer;
begin
  RGBtoHSLRange(RGB, h, s, l);
  Result := l < 100;
end;

procedure TMain.ListBox1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  LCanvas: TCanvas;
  s: String;
  c: TColor;
begin
  LCanvas := TListBox(Control).Canvas;
  s := TListBox(Control).Items[Index];
  c := FColorSet.Colors[ImmersiveStartHighlight];
  LCanvas.Brush.Color := TListBox(Control).Color;
  LCanvas.FillRect(Rect);
  if odFocused in State then
  begin
    LCanvas.Brush.Color := c;
    LCanvas.FillRect(Rect);
  end;
  LCanvas.Font.Color := clBlack;
  LCanvas.TextRect(Rect, s, [tfLeft]);
end;

procedure TMain.LstColorSetsClick(Sender: TObject);
begin
  if LstColorSets.ItemIndex > -1 then
  begin
    FColorSet := FImmersiveColors.ColorSets[LstColorSets.ItemIndex];
    RefreshColors;
  end;
end;

procedure TMain.LstColorSetsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  c: TColor;
  LCanvas: TCanvas;
begin
  LCanvas := TListBox(Control).Canvas;
  c := FImmersiveColors.ColorSets[Index].Colors[ImmersiveStartBackground];
  if not Control.Enabled then
    c := GetSysColor(COLOR_GRAYTEXT);
  LCanvas.Brush.Color := c;
  InflateRect(Rect, -2, -2);
  LCanvas.FillRect(Rect);
end;

procedure TMain.LstColorTypesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  FontColorSel: array [Boolean] of TColor = (clBlack, clWhite);
var
  c: TColor;
  s: String;
  LCanvas: TCanvas;
begin
  LCanvas := TListBox(Control).Canvas;
  s := TListBox(Control).Items[Index];
  c := FColorSet.GetColorFromColorTypeName(s);
  LCanvas.Brush.Color := c;
  LCanvas.FillRect(Rect);
  LCanvas.Font.Color := FontColorSel[IsDark(c)];
  LCanvas.TextRect(Rect, s, [tfCenter]);
end;

procedure TMain.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Brush.Color := FColorSet.Colors[ImmersiveStartBackground];
  PaintBox1.Canvas.FillRect(PaintBox1.ClientRect);
end;

procedure TMain.PaintBox2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PaintBox2.Tag := AC_DOWN;
  PaintBox2.Refresh;
end;

procedure TMain.PaintBox2MouseEnter(Sender: TObject);
begin
  PaintBox2.Tag := AC_HOVER;
  PaintBox2.Refresh;
end;

procedure TMain.PaintBox2MouseLeave(Sender: TObject);
begin
  PaintBox2.Tag := 0;
  PaintBox2.Refresh;
end;

procedure TMain.PaintBox2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PaintBox2.Tag := 0;
  PaintBox2.Refresh;
end;

procedure TMain.PaintBox2Paint(Sender: TObject);
const
  AC2BkgImm: array [0 .. 2] of TImmersiveColorType = ( //
    ImmersiveControlDarkSliderTrackFillRest, //
    ImmersiveControlDarkSliderTrackFillHover, //
    ImmersiveControlDarkSliderTrackFillPressed);
  AC2TxtImm: array [0 .. 2] of TImmersiveColorType = ( //
    ImmersiveControlDarkButtonTextRest, //
    ImmersiveControlDarkButtonTextHover, //
    ImmersiveControlDarkButtonTextPressed);
var
  cBkg, cTxt: TColor;
  R: TRect;
  s: String;
begin
  s := 'XButton!';
  R := PaintBox2.ClientRect;
  cBkg := FColorSet.Colors[AC2BkgImm[PaintBox2.Tag]];
  cTxt := FColorSet.Colors[AC2TxtImm[PaintBox2.Tag]];
  if not PaintBox2.Enabled then
  begin
    cBkg := FColorSet.Colors[ImmersiveControlDarkSliderTrackFillDisabled];
    cTxt := FColorSet.Colors[ImmersiveControlDarkButtonTextDisabled];
  end;
  PaintBox2.Canvas.Brush.Color := cBkg;
  PaintBox2.Canvas.FillRect(R);
  PaintBox2.Canvas.Font.Color := cTxt;
  PaintBox2.Canvas.TextRect(R, s, [tfSingleLine, tfVerticalCenter, tfCenter]);

end;

procedure TMain.RefreshColors;
begin
  LstColorTypes.Refresh;
  Label1.Font.Color := FColorSet.Colors[ImmersiveStartDesktopTilesBackground];
  Label2.Font.Color := FColorSet.Colors[ImmersiveLightWUWarning];
  RichEdit1.SelAttributes.Color := FColorSet.Colors[ImmersiveControlDarkRichEditHighlight];

  Refresh;
end;

end.
