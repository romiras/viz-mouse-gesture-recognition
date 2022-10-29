object FormGesture: TFormGesture
  Left = 196
  Top = 124
  Width = 800
  Height = 600
  Caption = 'Gesture input'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 17
  object PaintBox: TPaintBox
    Left = 0
    Top = 0
    Width = 596
    Height = 568
    Align = alClient
    Color = clWhite
    ParentColor = False
    OnMouseDown = PaintBoxMouseDown
    OnMouseMove = PaintBoxMouseMove
    OnMouseUp = PaintBoxMouseUp
  end
  object pnlInfo: TPanel
    Left = 596
    Top = 0
    Width = 196
    Height = 568
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Log: TMemo
      Left = 0
      Top = 0
      Width = 196
      Height = 467
      Align = alClient
      BevelKind = bkFlat
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
    object PnlCommands: TPanel
      Left = 0
      Top = 467
      Width = 196
      Height = 101
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnSave: TButton
        Left = 8
        Top = 38
        Width = 185
        Height = 43
        Caption = 'Save'
        TabOrder = 0
        OnClick = btnSaveClick
      end
    end
  end
end
