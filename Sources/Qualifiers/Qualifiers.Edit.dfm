object frmQualifierEdit: TfrmQualifierEdit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Qualifier'
  ClientHeight = 381
  ClientWidth = 617
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
    Top = 336
    Width = 617
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      617
      45)
    object btnSave: TBitBtn
      Left = 515
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
    end
    object btnCancel: TBitBtn
      Left = 414
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
    end
    object pnlInfo: TPanel
      Left = 0
      Top = 0
      Width = 369
      Height = 45
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object imgWarning: TVirtualImage
        Left = 0
        Top = 0
        Width = 32
        Height = 45
        Align = alLeft
        ImageCollection = DMImage.ImCollection32
        ImageWidth = 0
        ImageHeight = 0
        ImageIndex = 58
        ImageName = 'Warning_32x32'
        ExplicitTop = 2
      end
      object lblInfo: TLabel
        AlignWithMargins = True
        Left = 35
        Top = 3
        Width = 331
        Height = 39
        Align = alClient
        Caption = 
          'Changing the Document affects overall template in Template Creat' +
          'or'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 330
        ExplicitHeight = 13
      end
    end
  end
  object pnlAutoTrades: TPanel
    Left = 0
    Top = 313
    Width = 617
    Height = 23
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object cbBypass: TCheckBox
      Left = 265
      Top = 5
      Width = 113
      Height = 17
      Caption = 'Bypass'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object cbEnabled: TCheckBox
      Left = 6
      Top = 5
      Width = 249
      Height = 17
      Caption = 'Activate / Deactivate Qualifier Condition'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
  end
  object pcConditions: TPageControl
    Left = 0
    Top = 50
    Width = 617
    Height = 263
    ActivePage = tcCompare
    Align = alClient
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 2
    TabStop = False
    object tcCompare: TTabSheet
      Caption = 'Compare'
      object pnlSearchInstrument: TPanel
        Left = 569
        Top = 0
        Width = 40
        Height = 235
        Align = alRight
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          40
          235)
        object btnShowSearchForm: TBitBtn
          Left = 0
          Top = 85
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
      object pnlComparePrice: TPanel
        Left = 0
        Top = 0
        Width = 569
        Height = 235
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object pnlComparePriceTotal: TPanel
          Left = 0
          Top = 203
          Width = 569
          Height = 40
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblInequalityInstrument1: TLabel
            Left = 68
            Top = 5
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
            Top = 6
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
            Top = 23
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
            Top = 23
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
          object cbInequalityValue: TComboBox
            Left = 145
            Top = 3
            Width = 102
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
        object pnlInstrument2: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 105
          Width = 563
          Height = 98
          Margins.Bottom = 0
          Align = alTop
          BevelKind = bkFlat
          BevelOuter = bvNone
          TabOrder = 1
          OnDragDrop = OnInstrumentDragDrop
          OnDragOver = OnInstrumentDragOver
          object lblInstrumentName2: TLabel
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
            Left = 83
            Top = 57
            Width = 52
            Height = 16
            Caption = 'IB Value:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object lblInstrument2IBValue1: TLabel
            Left = 215
            Top = 78
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
          object lblInstrument2IBValue2: TLabel
            Left = 370
            Top = 78
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
          object lblCalculateValue2: TLabel
            AlignWithMargins = True
            Left = 398
            Top = 58
            Width = 34
            Height = 16
            Margins.Right = 5
            Caption = '=0.00'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
          end
          object cbInstrument2IBValue2: TComboBox
            Left = 295
            Top = 56
            Width = 100
            Height = 21
            TabOrder = 0
            OnChange = cbInstrument2IBValue2Change
          end
          object cbInstrument2IBValue1: TComboBox
            Left = 140
            Top = 56
            Width = 100
            Height = 21
            TabOrder = 1
            OnChange = cbInstrument2IBValue1Change
          end
          object cbTypeOperation2: TComboBox
            Left = 242
            Top = 56
            Width = 50
            Height = 21
            TabOrder = 2
            OnChange = cbTypeOperation2Change
          end
          object btnCalculateInstrument2: TButton
            Left = 460
            Top = 54
            Width = 100
            Height = 40
            Action = aCalculateInstrument2
            Images = DMImage.vil32
            TabOrder = 3
          end
        end
        object pnlInstrument1: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 563
          Height = 99
          Margins.Bottom = 0
          Align = alTop
          BevelKind = bkFlat
          BevelOuter = bvNone
          TabOrder = 2
          OnDragDrop = OnInstrumentDragDrop
          OnDragOver = OnInstrumentDragOver
          object lblInstrumentName1: TLabel
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
            Left = 83
            Top = 58
            Width = 52
            Height = 16
            Caption = 'IB Value:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object lblCalculateValue1: TLabel
            AlignWithMargins = True
            Left = 398
            Top = 59
            Width = 34
            Height = 16
            Margins.Right = 5
            Caption = '=0.00'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
          end
          object lblInstrument1IBValue1: TLabel
            Left = 215
            Top = 79
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
          object lblInstrument1IBValue2: TLabel
            Left = 370
            Top = 79
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
          object cbInstrument1IBValue2: TComboBox
            Left = 295
            Top = 57
            Width = 100
            Height = 21
            TabOrder = 0
            OnChange = cbInstrument1IBValue2Change
          end
          object cbInstrument1IBValue1: TComboBox
            Left = 140
            Top = 57
            Width = 100
            Height = 21
            TabOrder = 1
            OnChange = cbInstrument1IBValue1Change
          end
          object cbTypeOperation1: TComboBox
            Left = 242
            Top = 57
            Width = 50
            Height = 21
            TabOrder = 2
            OnChange = cbTypeOperation1Change
          end
          object btnCalculateInstrument1: TButton
            Left = 460
            Top = 55
            Width = 100
            Height = 40
            Action = aCalculateInstrument1
            Images = DMImage.vil32
            TabOrder = 3
          end
        end
      end
    end
    object tsEveryDay: TTabSheet
      Caption = 'Every Day'
      ImageIndex = 1
      object lblEveryDayTime: TLabel
        Left = 63
        Top = 11
        Width = 80
        Height = 16
        Alignment = taRightJustify
        Caption = 'Startup Time:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object edEveryDayTime: TDateTimePicker
        Left = 150
        Top = 9
        Width = 105
        Height = 21
        Date = 44131.000000000000000000
        Time = 44131.000000000000000000
        Kind = dtkTime
        TabOrder = 0
      end
    end
    object tsSpecificDateTime: TTabSheet
      Caption = 'Specific Date'
      ImageIndex = 2
      object lblSpecificDate: TLabel
        Left = 66
        Top = 11
        Width = 77
        Height = 16
        Alignment = taRightJustify
        Caption = 'Startup Date:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblSpecificTime: TLabel
        Left = 63
        Top = 37
        Width = 80
        Height = 16
        Alignment = taRightJustify
        Caption = 'Startup Time:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object edSpecificDate: TDateTimePicker
        Left = 150
        Top = 9
        Width = 105
        Height = 21
        Date = 44131.000000000000000000
        Time = 44131.000000000000000000
        TabOrder = 0
      end
      object edSpecificTime: TDateTimePicker
        Left = 150
        Top = 35
        Width = 105
        Height = 21
        Date = 44131.000000000000000000
        Time = 44131.000000000000000000
        Kind = dtkTime
        TabOrder = 1
      end
    end
  end
  object pnlTypeCondition: TPanel
    Left = 0
    Top = 0
    Width = 617
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object lblTypeCondition: TLabel
      Left = 53
      Top = 3
      Width = 90
      Height = 16
      Alignment = taRightJustify
      Caption = 'Type Condition:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblName: TLabel
      Left = 105
      Top = 25
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
    object cbTypeCondition: TComboBox
      Left = 149
      Top = 1
      Width = 281
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = cbTypeConditionChange
      Items.Strings = (
        'greater than'
        'less than'
        'between')
    end
    object edtName: TEdit
      Left = 149
      Top = 24
      Width = 281
      Height = 21
      TabOrder = 1
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
