object frmEditFactor: TfrmEditFactor
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Condition Factor'
  ClientHeight = 290
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  
  TextHeight = 13
  object lblName: TLabel
    Left = 129
    Top = 7
    Width = 4
    Height = 16
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblConId: TLabel
    Left = 129
    Top = 26
    Width = 4
    Height = 16
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblConIdCaption: TLabel
    Left = 5
    Top = 26
    Width = 120
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Contract Id:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblIBid: TLabel
    Left = 129
    Top = 45
    Width = 4
    Height = 16
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblIBidCaption: TLabel
    Left = 5
    Top = 45
    Width = 120
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'IBid:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblValue1: TLabel
    Left = 201
    Top = 173
    Width = 25
    Height = 16
    Alignment = taRightJustify
    Caption = '0.00'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblExchange: TLabel
    Left = 129
    Top = 64
    Width = 4
    Height = 16
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblExchangeCaption: TLabel
    Left = 5
    Top = 64
    Width = 120
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Exchange:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblCurrency: TLabel
    Left = 129
    Top = 85
    Width = 4
    Height = 16
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblCurrencyCaption: TLabel
    Left = 5
    Top = 85
    Width = 120
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Currency:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblContractType: TLabel
    Left = 129
    Top = 106
    Width = 4
    Height = 16
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblTypeCaption: TLabel
    Left = 5
    Top = 106
    Width = 120
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Type:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblDecimals: TLabel
    Left = 5
    Top = 129
    Width = 120
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Decimals:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblReWeight: TLabel
    Left = 129
    Top = 152
    Width = 4
    Height = 16
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblNameCaption: TLabel
    Left = 5
    Top = 7
    Width = 120
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Instrument Name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblValueCaption: TLabel
    Left = 2
    Top = 174
    Width = 120
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Values:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblReWeightCaption: TLabel
    Left = 5
    Top = 152
    Width = 120
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'ReWeight:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblTickTypeCaption: TLabel
    Left = 5
    Top = 195
    Width = 120
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'TickTypes for check:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblAndOr: TLabel
    Left = 232
    Top = 195
    Width = 38
    Height = 16
    Caption = 'and/or'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblValue2: TLabel
    Left = 349
    Top = 173
    Width = 25
    Height = 16
    Alignment = taRightJustify
    Caption = '0.00'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 248
    Width = 480
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnSave: TBitBtn
      Left = 379
      Top = 1
      Width = 100
      Height = 41
      Action = aSave
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 278
      Top = 1
      Width = 100
      Height = 41
      Action = aCancel
      Cancel = True
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 1
    end
    object btnShowTradeChart: TBitBtn
      Left = 178
      Top = 1
      Width = 100
      Height = 41
      Action = aGraph
      Caption = 'Graph'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 2
    end
  end
  object seDecimals: TSpinEdit
    Left = 128
    Top = 128
    Width = 47
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object cbUseInAutoOrder: TCheckBox
    Left = 128
    Top = 220
    Width = 150
    Height = 17
    Caption = 'Replace from Template'
    TabOrder = 2
  end
  object cbTickType1: TComboBox
    Left = 128
    Top = 193
    Width = 100
    Height = 21
    Enabled = False
    TabOrder = 3
    OnChange = cbTickType1Change
  end
  object cbTickType2: TComboBox
    Left = 275
    Top = 193
    Width = 100
    Height = 21
    Enabled = False
    TabOrder = 4
    OnChange = cbTickType2Change
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 302
    Top = 10
    object aGraph: TAction
      Caption = 'Graph'
      ImageIndex = 48
      ImageName = 'Area_32x32'
      OnExecute = aGraphExecute
      OnUpdate = aGraphUpdate
    end
    object aSave: TAction
      Caption = 'Save'
      Enabled = False
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
      OnUpdate = aSaveUpdate
    end
    object aCancel: TAction
      Caption = 'Cancel'
      OnExecute = aCancelExecute
    end
  end
end
