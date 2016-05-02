object Main: TMain
  Left = 0
  Top = 0
  Caption = 'Main'
  ClientHeight = 396
  ClientWidth = 749
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 145
    Top = 0
    Width = 367
    Height = 396
    Align = alClient
    Caption = 'GroupBox1'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 36
      Height = 14
      Caption = 'Label1'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 16
      Top = 71
      Width = 65
      Height = 13
      Caption = 'Warning Text'
    end
    object PaintBox1: TPaintBox
      Left = 2
      Top = 352
      Width = 363
      Height = 42
      Align = alBottom
      OnPaint = PaintBox1Paint
    end
    object PaintBox2: TPaintBox
      Left = 16
      Top = 264
      Width = 81
      Height = 73
      OnMouseDown = PaintBox2MouseDown
      OnMouseEnter = PaintBox2MouseEnter
      OnMouseLeave = PaintBox2MouseLeave
      OnMouseUp = PaintBox2MouseUp
      OnPaint = PaintBox2Paint
    end
    object ListBox1: TListBox
      Left = 16
      Top = 90
      Width = 121
      Height = 135
      Style = lbOwnerDrawFixed
      Items.Strings = (
        'Item 1'
        'Item 2'
        'Item 3'
        'Item 4'
        'Item 5')
      TabOrder = 0
      OnDrawItem = ListBox1DrawItem
    end
    object RichEdit1: TRichEdit
      Left = 143
      Top = 90
      Width = 185
      Height = 135
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Lines.Strings = (
        'Simple text !!!'
        'Simple text !!!'
        'Simple text !!!'
        'Simple text !!!')
      ParentFont = False
      TabOrder = 1
      Zoom = 100
    end
    object CheckBox2: TCheckBox
      Left = 184
      Top = 288
      Width = 97
      Height = 17
      Caption = 'Enable XButton'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox2Click
    end
  end
  object Panel1: TPanel
    Left = 512
    Top = 0
    Width = 237
    Height = 396
    Align = alRight
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object LstColorTypes: TListBox
      Left = 1
      Top = 42
      Width = 235
      Height = 353
      Style = lbOwnerDrawFixed
      Align = alClient
      TabOrder = 0
      OnDrawItem = LstColorTypesDrawItem
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 235
      Height = 41
      Align = alTop
      Caption = 'Panel3'
      ShowCaption = False
      TabOrder = 1
      object Label4: TLabel
        Left = 32
        Top = 16
        Width = 91
        Height = 13
        Caption = 'Color type names :'
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 396
    Align = alLeft
    Caption = 'Panel2'
    ShowCaption = False
    TabOrder = 2
    object LstColorSets: TListBox
      Left = 1
      Top = 57
      Width = 143
      Height = 338
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 20
      TabOrder = 0
      OnClick = LstColorSetsClick
      OnDrawItem = LstColorSetsDrawItem
    end
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 143
      Height = 56
      Align = alTop
      Caption = 'Panel4'
      ShowCaption = False
      TabOrder = 1
      object Label3: TLabel
        Left = 8
        Top = 34
        Width = 80
        Height = 13
        Caption = 'Select ColorSet :'
      end
      object CheckBox1: TCheckBox
        Left = 8
        Top = 11
        Width = 97
        Height = 22
        Caption = 'Use Active Color Set'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CheckBox1Click
      end
    end
  end
end
