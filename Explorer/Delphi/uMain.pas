unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ImmersiveColorsControls;

type
  TMain = class(TForm)
    ImmersiveColorsListBox1: TImmersiveColorsListBox;
    ImmersiveColorSetListBox1: TImmersiveColorSetListBox;
    ImmersiveNotify1: TImmersiveNotify;
    procedure ImmersiveColorSetListBox1Click(Sender: TObject);
    procedure ImmersiveNotify1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}


procedure TMain.ImmersiveColorSetListBox1Click(Sender: TObject);
var
  i: Integer;
begin
  i := ImmersiveColorSetListBox1.ItemIndex;
  if (i > -1) then
    ImmersiveColorsListBox1.ImmersiveColorSet := Cardinal(ImmersiveColorSetListBox1.Items.Objects[i]);
end;

procedure TMain.ImmersiveNotify1Change(Sender: TObject);
begin
  ImmersiveColorsListBox1.Refresh();
  ImmersiveColorSetListBox1.Refresh();
end;

end.
