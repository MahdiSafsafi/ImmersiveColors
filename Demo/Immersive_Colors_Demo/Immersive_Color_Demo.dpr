// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
program Immersive_Color_Demo;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Main},
  uImmersiveColors in '..\..\Source\uImmersiveColors.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
