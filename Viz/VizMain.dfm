object FormGesture: TFormGesture
  Left = 173
  Top = 123
  Width = 800
  Height = 600
  Caption = 'Gesture'
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
    Height = 542
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
    Height = 542
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Log: TMemo
      Left = 0
      Top = 0
      Width = 196
      Height = 441
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
      Top = 441
      Width = 196
      Height = 101
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object lblTrain: TLabel
        Left = 10
        Top = 13
        Width = 35
        Height = 17
        Caption = 'Train:'
      end
      object txtLearn: TEdit
        Left = 52
        Top = 10
        Width = 54
        Height = 25
        TabOrder = 0
        OnKeyPress = txtLearnKeyPress
      end
      object btnTrain: TButton
        Left = 8
        Top = 54
        Width = 185
        Height = 27
        Caption = 'Train'
        TabOrder = 1
        OnClick = btnTrainClick
      end
    end
  end
  object pnlText: TPanel
    Left = 0
    Top = 542
    Width = 792
    Height = 26
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object FFNN: TFFNN
    InputCount = 32
    OutputCount = 2
    InputMin = -1
    InputMax = 1
    OutputMax = 1
    NLayers = <>
    BPSpeed = 0.1
    Inertion = 0.1
    Left = 416
    Top = 8
  end
end
