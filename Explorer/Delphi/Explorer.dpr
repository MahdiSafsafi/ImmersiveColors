program Explorer;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Main},
  ImmersiveColors in '..\..\Source\ImmersiveColors.pas',
  ImmersiveColorsControls in '..\..\Components\Source\ImmersiveColorsControls.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;

end.
