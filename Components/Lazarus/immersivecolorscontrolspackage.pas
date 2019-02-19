{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ImmersiveColorsControlsPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  ImmersiveColorsControls, ImmersiveColors, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ImmersiveColorsControls', @ImmersiveColorsControls.Register);
end;

initialization
  RegisterPackage('ImmersiveColorsControlsPackage', @Register);
end.
