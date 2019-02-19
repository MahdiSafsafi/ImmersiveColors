program Demo1;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Main},
  ImmersiveColors in '..\..\..\Source\ImmersiveColors.pas',
  ImmersiveColorsControls in '..\..\..\Components\Source\ImmersiveColorsControls.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
