# ImmersiveColors
When MS released Windows 8, they introduced a new feature called **ImmerciveColors**. Based on your OS settings (wallpapers or color set) you may have a sets of colors that can be used to draw your UI. However this feature is not a quite known for the public since it’s still undocumented (even with Windows 10). So I wrote this simple project to demonstrate how to benefit from this feature.

# What's new ?
- Added FPC and lazarus support.
- Replaced TImmersiveColors class with a simplified functions: GetImmersiveColorSetCount, GetImmersiveColor, GetActiveImmersiveColor, ...
- Added default return value to make the library work on platform that do not have immersive colors support (XP, Win7, macOS, ...).
- Added many new function such as IsDarkThemeActive, GetRivalColorType,...
- Added three new components : TImmersiveColorsListBox, TImmersiveColorSetListBox, TImmersiveNotify.
- Improved Explorer.

# Example
```pascal
begin
  if (IsDarkThemeActive()) then
  begin
    // Dark Mode is active
    // If the target platform(Win8|Win10) supports immersive colors, GetActiveImmersiveColor
    // will return  accent color value, otherwise(Win7|macOS|...) it returns clWebDarkOrange.
    AlphaColor := GetActiveImmersiveColor(ImmersiveSystemAccentDark1, clWebDarkOrange);
  end
  else
  begin
    // not dark !
    AlphaColor := GetActiveImmersiveColor(ImmersiveSystemAccentLight1, clWebLightYellow);
  end;
  Color := AlphaColorToColor(AlphaColor);
end;
```
