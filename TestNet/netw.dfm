object Form1: TForm1
  Left = 202
  Top = 178
  Width = 448
  Height = 220
  Caption = 'Feed Forward Neural Network sample'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Memo1: TMemo
    Left = 10
    Top = 20
    Width = 228
    Height = 139
    TabOrder = 0
  end
  object btTrain: TButton
    Left = 261
    Top = 49
    Width = 169
    Height = 41
    Caption = 'Train network'
    TabOrder = 1
    OnClick = btTrainClick
  end
  object btShow: TButton
    Left = 261
    Top = 110
    Width = 169
    Height = 41
    Caption = 'Show output'
    TabOrder = 2
    OnClick = btShowClick
  end
  object netw: TFFNN
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
    Left = 256
    Top = 8
  end
end
