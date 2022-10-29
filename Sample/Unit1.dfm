object Form1: TForm1
  Left = 192
  Top = 107
  Width = 404
  Height = 312
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object TrainB: TButton
    Left = 10
    Top = 10
    Width = 92
    Height = 31
    Caption = 'Train'
    TabOrder = 0
    OnClick = TrainBClick
  end
  object LoadB: TButton
    Left = 138
    Top = 10
    Width = 92
    Height = 31
    Caption = 'Load'
    TabOrder = 1
    OnClick = LoadBClick
  end
  object SaveB: TButton
    Left = 138
    Top = 59
    Width = 92
    Height = 31
    Caption = 'Save'
    TabOrder = 2
    OnClick = SaveBClick
  end
  object ShowB: TButton
    Left = 10
    Top = 59
    Width = 92
    Height = 31
    Caption = 'Show'
    TabOrder = 3
    OnClick = ShowBClick
  end
  object Memo1: TMemo
    Left = 10
    Top = 108
    Width = 228
    Height = 149
    Lines.Strings = (
      'Memo1')
    TabOrder = 4
  end
  object FFNN1: TFFNN
    InputCount = 1
    OutputCount = 1
    InputMin = -1
    InputMax = 8
    OutputMin = 9
    OutputMax = 18
    NLayers = <
      item
        NeuronCount = 10
      end>
    BPSpeed = 0.1
    Inertion = 0.1
    Left = 248
    Top = 16
  end
end
