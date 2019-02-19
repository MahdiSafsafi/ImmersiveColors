object Main: TMain
  Left = 0
  Top = 0
  Caption = 'Main - try change system settings'
  ClientHeight = 201
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ImmersiveNotify1: TImmersiveNotify
    OnChange = ImmersiveNotify1Change
    Left = 296
    Top = 104
  end
end
