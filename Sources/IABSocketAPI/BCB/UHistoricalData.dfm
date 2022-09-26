object FHistoricalData: TFHistoricalData
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Historical data'
  ClientHeight = 249
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 28
    Top = 90
    Width = 309
    Height = 11
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 18
    Top = 8
    Width = 37
    Height = 13
    Caption = 'Label1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 257
    Top = 199
    Width = 80
    Height = 42
    AutoSize = False
    Caption = '0 means no history and starts live tick stream.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Button1: TButton
    Left = 23
    Top = 192
    Width = 60
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
    OnClick = Button1Click
  end
  object RadioButton1: TRadioButton
    Left = 28
    Top = 39
    Width = 113
    Height = 17
    Caption = 'Bar data - OHLC'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 28
    Top = 119
    Width = 136
    Height = 17
    Caption = 'Ticks / time sales data'
    TabOrder = 2
    OnClick = RadioButton2Click
  end
  object ListBoxBidAsk: TListBox
    Left = 182
    Top = 107
    Width = 69
    Height = 86
    Enabled = False
    ItemHeight = 13
    TabOrder = 3
  end
  object ListBoxTickCount: TListBox
    Left = 257
    Top = 107
    Width = 70
    Height = 86
    Enabled = False
    ItemHeight = 13
    TabOrder = 4
  end
  object ListBoxBars: TListBox
    Left = 182
    Top = 4
    Width = 69
    Height = 80
    ItemHeight = 13
    TabOrder = 5
  end
  object ListBoxBarPeriod: TListBox
    Left = 257
    Top = 4
    Width = 70
    Height = 80
    ItemHeight = 13
    TabOrder = 6
  end
  object Button2: TButton
    Left = 89
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
end
