program Immersive_Color_Demo;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Main},
  uImmersiveColors in '..\..\Src\uImmersiveColors.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
