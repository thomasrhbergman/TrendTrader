object frmEditInstrument: TfrmEditInstrument
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit Instrument'
  ClientHeight = 314
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  
  TextHeight = 13
  object lblDecimals: TLabel
    Left = 58
    Top = 177
    Width = 56
    Height = 16
    Alignment = taRightJustify
    Caption = 'Decimals:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblSecurityType: TLabel
    Left = 31
    Top = 227
    Width = 83
    Height = 16
    Alignment = taRightJustify
    Caption = 'Security Type:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblName: TLabel
    Left = 76
    Top = 81
    Width = 38
    Height = 16
    Alignment = taRightJustify
    Caption = 'Name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblDescription: TLabel
    Left = 46
    Top = 251
    Width = 68
    Height = 16
    Alignment = taRightJustify
    Caption = 'Description:'
    Color = clDefault
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lblSymbol: TLabel
    Left = 67
    Top = 33
    Width = 47
    Height = 16
    Alignment = taRightJustify
    Caption = 'Symbol:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblContractID: TLabel
    Left = 45
    Top = 9
    Width = 69
    Height = 16
    Alignment = taRightJustify
    Caption = 'Contract ID:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblCurrency: TLabel
    Left = 58
    Top = 105
    Width = 56
    Height = 16
    Alignment = taRightJustify
    Caption = 'Currency:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblExchange: TLabel
    Left = 55
    Top = 129
    Width = 59
    Height = 16
    Alignment = taRightJustify
    Caption = 'Exchange:'
    Color = clDefault
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lblPrimaryExchange: TLabel
    Left = 7
    Top = 153
    Width = 107
    Height = 16
    Alignment = taRightJustify
    Caption = 'Primary Exchange:'
    Color = clDefault
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lblLocalSymbol: TLabel
    Left = 34
    Top = 57
    Width = 80
    Height = 16
    Alignment = taRightJustify
    Caption = 'Local Symbol:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblMultiplier: TLabel
    Left = 59
    Top = 202
    Width = 57
    Height = 16
    Alignment = taRightJustify
    Caption = 'Multiplier:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 274
    Width = 480
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 10
    object btnOpen: TBitBtn
      Left = 369
      Top = -1
      Width = 109
      Height = 41
      Action = aSave
      Caption = 'Ok'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 259
      Top = -1
      Width = 109
      Height = 41
      Action = aCancel
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ModalResult = 2
      ParentFont = False
      TabOrder = 1
    end
  end
  object seDecimals: TSpinEdit
    Left = 118
    Top = 176
    Width = 65
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 7
    Value = 0
  end
  object cbSecurityType: TComboBox
    Left = 118
    Top = 226
    Width = 64
    Height = 21
    Enabled = False
    TabOrder = 9
  end
  object edName: TEdit
    Left = 118
    Top = 80
    Width = 360
    Height = 21
    ReadOnly = True
    TabOrder = 3
  end
  object edDescription: TEdit
    Left = 118
    Top = 250
    Width = 360
    Height = 21
    TabOrder = 11
  end
  object edSymbol: TEdit
    Left = 118
    Top = 32
    Width = 160
    Height = 21
    ReadOnly = True
    TabOrder = 1
  end
  object edContractID: TEdit
    Left = 118
    Top = 8
    Width = 96
    Height = 21
    ReadOnly = True
    TabOrder = 0
  end
  object edCurrency: TEdit
    Left = 118
    Top = 104
    Width = 65
    Height = 21
    ReadOnly = True
    TabOrder = 4
  end
  object edExchange: TEdit
    Left = 118
    Top = 128
    Width = 160
    Height = 21
    ReadOnly = True
    TabOrder = 5
  end
  object edPrimaryExchange: TEdit
    Left = 118
    Top = 152
    Width = 160
    Height = 21
    ReadOnly = True
    TabOrder = 6
  end
  object edLocalSymbol: TEdit
    Left = 118
    Top = 56
    Width = 160
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
  object seMultiplier: TSpinEdit
    Left = 118
    Top = 201
    Width = 65
    Height = 22
    MaxValue = 0
    MinValue = 0
    ReadOnly = True
    TabOrder = 8
    Value = 0
  end
  object ActionList: TActionList
    Left = 400
    Top = 8
    object aSave: TAction
      Caption = 'Ok'
      Hint = 'Save'
      OnExecute = aSaveExecute
    end
    object aCancel: TAction
      Caption = 'Cancel'
      ShortCut = 27
      OnExecute = aCancelExecute
    end
  end
end
