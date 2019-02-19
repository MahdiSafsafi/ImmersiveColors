unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ImmersiveColorsControls;

type

  { TMain }

  TMain = class(TForm)
    ImmersiveColorSetListBox1: TImmersiveColorSetListBox;
    ImmersiveColorsListBox1: TImmersiveColorsListBox;
    procedure ImmersiveColorSetListBox1Click(Sender: TObject);
  private

  public

  end;

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }

procedure TMain.ImmersiveColorSetListBox1Click(Sender: TObject);
begin
  ImmersiveColorsListBox1.ImmersiveColorSet := ImmersiveColorSetListBox1.ItemIndex;
end;

end.

