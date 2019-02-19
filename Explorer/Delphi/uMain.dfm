object Main: TMain
  Left = 0
  Top = 0
  Caption = 'Explorer'
  ClientHeight = 201
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ImmersiveColorsListBox1: TImmersiveColorsListBox
    Left = 193
    Top = 0
    Width = 339
    Height = 201
    ImmersiveColorSet = 50
    Align = alClient
    Style = lbOwnerDrawFixed
    TabOrder = 0
  end
  object ImmersiveColorSetListBox1: TImmersiveColorSetListBox
    Left = 0
    Top = 0
    Width = 193
    Height = 201
    Align = alLeft
    Style = lbOwnerDrawFixed
    TabOrder = 1
    OnClick = ImmersiveColorSetListBox1Click
    ExplicitLeft = -6
  end
  object ImmersiveNotify1: TImmersiveNotify
    OnChange = ImmersiveNotify1Change
    Left = 264
    Top = 104
  end
end
