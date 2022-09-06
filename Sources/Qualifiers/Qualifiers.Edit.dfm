object frmQualifierEdit: TfrmQualifierEdit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Qualifier'
  ClientHeight = 501
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 456
    Width = 490
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 336
    ExplicitWidth = 617
    DesignSize = (
      490
      45)
    object btnSave: TBitBtn
      Left = 388
      Top = 2
      Width = 100
      Height = 40
      Action = aSave
      Anchors = [akTop, akRight]
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 0
      ExplicitLeft = 515
    end
    object btnCancel: TBitBtn
      Left = 287
      Top = 2
      Width = 100
      Height = 40
      Anchors = [akTop, akRight]
      Cancel = True
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
      ExplicitLeft = 414
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 490
    Height = 40
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 1025
    object lblName: TLabel
      Left = 105
      Top = 10
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
    object edtName: TEdit
      Left = 149
      Top = 9
      Width = 281
      Height = 21
      TabOrder = 0
    end
  end
  object pnlCompare: TPanel
    Left = 0
    Top = 40
    Width = 490
    Height = 225
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 2
    ExplicitTop = 0
    ExplicitWidth = 1025
    object pnlCompareClient: TPanel
      Left = 73
      Top = 2
      Width = 375
      Height = 221
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 621
      object pnlComparePriceTotal: TPanel
        Left = 0
        Top = 178
        Width = 375
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitTop = 0
        ExplicitWidth = 910
        object lblInequalityInstrument1: TLabel
          Left = 68
          Top = 4
          Width = 69
          Height = 16
          Alignment = taRightJustify
          Caption = 'Instrument1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblInequalityInstrument2: TLabel
          Left = 253
          Top = 5
          Width = 71
          Height = 16
          AutoSize = False
          Caption = 'Instrument2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblCalculateTotal2: TLabel
          Left = 256
          Top = 22
          Width = 25
          Height = 16
          Caption = '0.00'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblCalculateTotal1: TLabel
          Left = 110
          Top = 22
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
        object cbInequalityCompare: TComboBox
          Left = 143
          Top = 3
          Width = 100
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          Items.Strings = (
            'greater than'
            'less than'
            'between')
        end
      end
      object pnlInstrument1: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 369
        Height = 88
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        OnDragDrop = OnInstrumentDragDrop
        OnDragOver = OnInstrumentDragOver
        ExplicitTop = 0
        object lblInstrumentName1: TLabel
          Left = 20
          Top = 0
          Width = 115
          Height = 16
          Alignment = taRightJustify
          Caption = 'Instrument 1 Name:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblInstrumentNameVal1: TLabel
          Left = 141
          Top = 0
          Width = 99
          Height = 16
          Caption = 'Instrument Name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblContractVal1: TLabel
          Left = 141
          Top = 19
          Width = 63
          Height = 16
          Caption = 'Contract Id'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblContract1: TLabel
          Left = 67
          Top = 19
          Width = 68
          Height = 16
          Alignment = taRightJustify
          Caption = 'Contract Id:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblSymbolVal1: TLabel
          Left = 141
          Top = 38
          Width = 42
          Height = 16
          Caption = 'Symbol'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblSymbol1: TLabel
          Left = 88
          Top = 38
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
        object lblValue1: TLabel
          Left = 98
          Top = 58
          Width = 37
          Height = 16
          Caption = 'Value:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbInstrument1IBValue1: TComboBox
          Left = 140
          Top = 57
          Width = 100
          Height = 21
          TabOrder = 0
          OnChange = cbInstrument1IBValue1Change
        end
      end
      object pnlInstrument2: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 94
        Width = 369
        Height = 84
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        OnDragDrop = OnInstrumentDragDrop
        OnDragOver = OnInstrumentDragOver
        ExplicitTop = 0
        ExplicitWidth = 904
        object lblInstrumentName2: TLabel
          Left = 20
          Top = 0
          Width = 115
          Height = 16
          Alignment = taRightJustify
          Caption = 'Instrument 2 Name:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblInstrumentNameVal2: TLabel
          Left = 141
          Top = 0
          Width = 99
          Height = 16
          Caption = 'Instrument Name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblContractVal2: TLabel
          Left = 141
          Top = 19
          Width = 63
          Height = 16
          Caption = 'Contract Id'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblContract2: TLabel
          Left = 67
          Top = 19
          Width = 68
          Height = 16
          Alignment = taRightJustify
          Caption = 'Contract Id:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblSymbolVal2: TLabel
          Left = 141
          Top = 38
          Width = 42
          Height = 16
          Caption = 'Symbol'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblSymbol2: TLabel
          Left = 88
          Top = 38
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
        object lblValue2: TLabel
          Left = 98
          Top = 57
          Width = 37
          Height = 16
          Caption = 'Value:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbInstrument2IBValue1: TComboBox
          Left = 140
          Top = 56
          Width = 100
          Height = 21
          TabOrder = 0
          OnChange = cbInstrument2IBValue1Change
        end
      end
    end
    object pnlCompareSearch: TPanel
      Left = 448
      Top = 2
      Width = 40
      Height = 221
      Align = alRight
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 694
      DesignSize = (
        40
        221)
      object btnShowSearchCompare: TBitBtn
        Left = 0
        Top = 104
        Width = 40
        Height = 40
        Action = aShowSearchInstruments
        Anchors = [akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Images = DMImage.vil32
        ParentFont = False
        TabOrder = 0
      end
    end
    object pnlCompareLeft: TPanel
      Left = 2
      Top = 2
      Width = 71
      Height = 221
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object cbCompare: TCheckBox
        Left = 4
        Top = 109
        Width = 60
        Height = 17
        Caption = 'Compare'
        TabOrder = 0
      end
    end
  end
  object pnlTime: TPanel
    Left = 0
    Top = 410
    Width = 490
    Height = 45
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 3
    ExplicitTop = 147
    ExplicitWidth = 736
    object pnlTimeLeft: TPanel
      Left = 2
      Top = 2
      Width = 71
      Height = 41
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object cbTime: TCheckBox
        Left = 5
        Top = 13
        Width = 60
        Height = 17
        Caption = 'Time'
        TabOrder = 0
      end
    end
    object pnlTimeCenter: TPanel
      AlignWithMargins = True
      Left = 76
      Top = 5
      Width = 409
      Height = 38
      Margins.Bottom = 0
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      OnDragDrop = OnInstrumentDragDrop
      OnDragOver = OnInstrumentDragOver
      ExplicitLeft = 3
      ExplicitTop = 3
      ExplicitWidth = 663
      object lblFromTime: TLabel
        Left = 99
        Top = 11
        Width = 35
        Height = 16
        Alignment = taRightJustify
        Caption = 'From:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblToTime: TLabel
        Left = 249
        Top = 11
        Width = 20
        Height = 16
        Alignment = taRightJustify
        Caption = 'To:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object edFromTime: TDateTimePicker
        Left = 141
        Top = 9
        Width = 99
        Height = 21
        Date = 44131.000000000000000000
        Time = 44131.000000000000000000
        Kind = dtkTime
        TabOrder = 0
      end
      object edToTime: TDateTimePicker
        Left = 276
        Top = 9
        Width = 99
        Height = 21
        Date = 44131.000000000000000000
        Time = 44131.000000000000000000
        Kind = dtkTime
        TabOrder = 1
      end
    end
  end
  object pnlValue: TPanel
    Left = 0
    Top = 265
    Width = 490
    Height = 145
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 4
    ExplicitTop = 0
    ExplicitWidth = 744
    object pnlValueClient: TPanel
      Left = 73
      Top = 2
      Width = 375
      Height = 141
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 629
      object pnlValuePriceTotal: TPanel
        Left = 0
        Top = 87
        Width = 375
        Height = 58
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lbComparisonValue: TLabel
          Left = 28
          Top = 30
          Width = 109
          Height = 16
          Caption = 'Comparison Value:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbInequalityValue: TComboBox
          Left = 143
          Top = 2
          Width = 100
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          Items.Strings = (
            'greater than'
            'less than'
            'between')
        end
        object edComparisonValue: TNumberBox
          Left = 143
          Top = 29
          Width = 100
          Height = 21
          AcceptExpressions = True
          TabOrder = 1
          SpinButtonOptions.Placement = nbspCompact
          UseMouseWheel = True
          NegativeValueColor = clWindow
        end
      end
      object pnlInstrument: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 369
        Height = 84
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        OnDragDrop = OnInstrumentDragDrop
        OnDragOver = OnInstrumentDragOver
        ExplicitWidth = 623
        object lblInstrumentName: TLabel
          Left = 31
          Top = 0
          Width = 104
          Height = 16
          Alignment = taRightJustify
          Caption = 'Instrument Name:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblInstrumentNameVal: TLabel
          Left = 141
          Top = 0
          Width = 99
          Height = 16
          Caption = 'Instrument Name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblContractVal: TLabel
          Left = 141
          Top = 19
          Width = 63
          Height = 16
          Caption = 'Contract Id'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblContract: TLabel
          Left = 67
          Top = 19
          Width = 68
          Height = 16
          Alignment = taRightJustify
          Caption = 'Contract Id:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblSymbolVal: TLabel
          Left = 141
          Top = 38
          Width = 42
          Height = 16
          Caption = 'Symbol'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblSymbol: TLabel
          Left = 88
          Top = 38
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
        object lblValue: TLabel
          Left = 98
          Top = 57
          Width = 37
          Height = 16
          Caption = 'Value:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbInstrumentIBValue: TComboBox
          Left = 140
          Top = 56
          Width = 100
          Height = 21
          TabOrder = 0
          OnChange = cbInstrumentIBValueChange
        end
      end
    end
    object pnlValueSearch: TPanel
      Left = 448
      Top = 2
      Width = 40
      Height = 141
      Align = alRight
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 702
      DesignSize = (
        40
        141)
      object btnShowSearchValue: TBitBtn
        Left = 0
        Top = 48
        Width = 40
        Height = 40
        Action = aShowSearchInstruments
        Anchors = [akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Images = DMImage.vil32
        ParentFont = False
        TabOrder = 0
      end
    end
    object pnlValueLeft: TPanel
      Left = 2
      Top = 2
      Width = 71
      Height = 141
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object cbValue: TCheckBox
        Left = 5
        Top = 69
        Width = 60
        Height = 17
        Caption = 'Value'
        TabOrder = 0
      end
    end
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 448
    Top = 15
    object aShowSearchInstruments: TAction
      ImageIndex = 21
      ImageName = 'Zoom_32x32'
      OnExecute = aShowSearchInstrumentsExecute
    end
    object aSave: TAction
      Caption = 'Save'
      ImageIndex = 46
      ImageName = 'tick'
      ShortCut = 16467
      OnExecute = aSaveExecute
    end
    object aShowAutoTrades: TAction
      Hint = 'Open Autotrade List and Details'
    end
    object aCalculateInstrument1: TAction
      Caption = 'Calculate'
      ImageIndex = 68
      ImageName = 'CalculateNow_32x32'
      OnExecute = aCalculateInstrument1Execute
      OnUpdate = aCalculateInstrument1Update
    end
    object aCalculateInstrument2: TAction
      Caption = 'Calculate'
      ImageIndex = 68
      ImageName = 'CalculateNow_32x32'
      OnExecute = aCalculateInstrument2Execute
      OnUpdate = aCalculateInstrument2Update
    end
  end
end
