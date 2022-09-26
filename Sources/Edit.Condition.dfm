object frmEditCondition: TfrmEditCondition
  Left = 0
  Top = 0
  Caption = 'Condition'
  ClientHeight = 284
  ClientWidth = 608
  Color = clBtnFace
  Constraints.MinHeight = 323
  Constraints.MinWidth = 610
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  ShowHint = True
  OnClick = rbPercentClick
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 608
    Height = 103
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnDragDrop = pnlTopDragDrop
    OnDragOver = pnlTopDragOver
    DesignSize = (
      608
      103)
    object lblDescription: TLabel
      AlignWithMargins = True
      Left = 51
      Top = 11
      Width = 33
      Height = 16
      Margins.Left = 10
      Caption = 'Name'
      Layout = tlCenter
    end
    object lblTypeCaption: TLabel
      AlignWithMargins = True
      Left = 56
      Top = 43
      Width = 28
      Height = 16
      Margins.Left = 10
      Caption = 'Type'
      Layout = tlCenter
    end
    object lblPriorityCaption: TLabel
      AlignWithMargins = True
      Left = 44
      Top = 76
      Width = 40
      Height = 16
      Margins.Left = 10
      Caption = 'Priority'
      Layout = tlCenter
    end
    object lblInstrument: TLabel
      Left = 240
      Top = 76
      Width = 67
      Height = 16
      Alignment = taRightJustify
      Caption = 'Instrument:'
    end
    object lblInstrumentName: TLabel
      Left = 313
      Top = 76
      Width = 157
      Height = 16
      Caption = '<Replace from AutoOrder>'
    end
    object edtName: TEdit
      Left = 101
      Top = 6
      Width = 347
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object cbConditionType: TComboBox
      Left = 101
      Top = 39
      Width = 241
      Height = 24
      Style = csDropDownList
      TabOrder = 1
      OnChange = cbConditionTypeChange
      Items.Strings = (
        'Realtime Value'
        'Time gap '
        'Gradient '
        'Corridor '
        'Corridor break '
        'Corridor position ')
    end
    object cbPriority: TComboBox
      Left = 101
      Top = 72
      Width = 110
      Height = 24
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'Normal'
      Items.Strings = (
        'Normal'
        'Priority'
        'Veto')
    end
    object btnShowSearchForm: TBitBtn
      Left = 558
      Top = 68
      Width = 32
      Height = 32
      Action = aShowSearchInstruments
      Anchors = [akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil16
      ParentFont = False
      TabOrder = 3
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 244
    Width = 608
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      608
      40)
    object btnSave: TBitBtn
      Left = 507
      Top = 0
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
      Left = 406
      Top = 0
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
  end
  object pcMain: TPageControl
    Left = 0
    Top = 103
    Width = 608
    Height = 141
    ActivePage = tabRealTime
    Align = alClient
    TabOrder = 2
    object tabRealTime: TTabSheet
      Caption = 'Real Time'
      ImageIndex = 2
      TabVisible = False
      object lblDivisionTickTypeCaption1: TLabel
        Left = 154
        Top = 13
        Width = 51
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'TickType'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblDivisionTickTypeCaption2: TLabel
        Left = 279
        Top = 13
        Width = 50
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'TickType'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblSecondParam: TLabel
        Left = 232
        Top = 34
        Width = 51
        Height = 16
        AutoSize = False
        Caption = '/ '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblComparisonRt: TLabel
        Left = 402
        Top = 13
        Width = 68
        Height = 16
        Caption = 'Comparison'
      end
      object lblConditionLimit: TLabel
        Left = 502
        Top = 13
        Width = 84
        Height = 16
        Caption = 'Condition Limit'
      end
      object gbMode: TGroupBox
        Left = 11
        Top = 19
        Width = 86
        Height = 88
        TabOrder = 0
        object rbPercent: TRadioButton
          Left = 12
          Top = 10
          Width = 37
          Height = 17
          Caption = '%'
          TabOrder = 0
          OnClick = rbPercentClick
        end
        object rbValue: TRadioButton
          Left = 12
          Top = 35
          Width = 57
          Height = 17
          Caption = 'Value'
          TabOrder = 1
          OnClick = rbPercentClick
        end
        object rbMOAP: TRadioButton
          Left = 12
          Top = 58
          Width = 57
          Height = 17
          Caption = 'MOAP'
          TabOrder = 2
          OnClick = rbPercentClick
        end
      end
      object cbTickType1: TComboBox
        Left = 124
        Top = 31
        Width = 100
        Height = 24
        TabOrder = 1
        OnChange = OnTickTypeChange
      end
      object cbTickType2: TComboBox
        Left = 250
        Top = 31
        Width = 100
        Height = 24
        TabOrder = 2
        OnChange = OnTickTypeChange
      end
      object cbInequalityRt: TComboBox
        Left = 376
        Top = 31
        Width = 120
        Height = 24
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnChange = OnTickTypeChange
        Items.Strings = (
          'greater than'
          'less than'
          'between')
      end
      object edtCondLimit: TNumberBox
        Left = 515
        Top = 31
        Width = 62
        Height = 24
        Alignment = taRightJustify
        Decimal = 4
        Mode = nbmFloat
        TabOrder = 4
        OnChange = OnTickTypeChange
      end
    end
    object tabTimeGap: TTabSheet
      Caption = 'Time Gap'
      ImageIndex = 3
      TabVisible = False
      object Label2: TLabel
        Left = 31
        Top = 25
        Width = 57
        Height = 16
        Caption = 'Valid time'
      end
      object Label3: TLabel
        Left = 208
        Top = 24
        Width = 11
        Height = 16
        Caption = 'to'
      end
      object dtpTimeStartValid: TDateTimePicker
        Left = 94
        Top = 19
        Width = 91
        Height = 29
        Date = 42350.000000000000000000
        Time = 0.791666666656965400
        DoubleBuffered = True
        Kind = dtkTime
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
      end
      object dtpTimeEndValid: TDateTimePicker
        Left = 235
        Top = 19
        Width = 91
        Height = 29
        Date = 42349.000000000000000000
        Time = 0.791666666656965400
        Kind = dtkTime
        TabOrder = 1
      end
    end
    object tabGradient: TTabSheet
      Caption = 'Gradient'
      ImageIndex = 4
      TabVisible = False
      object lblMonitoringCaption: TLabel
        Left = 11
        Top = 37
        Width = 60
        Height = 16
        Caption = 'Monitoring'
      end
      object lblMonitoringSec: TLabel
        Left = 143
        Top = 37
        Width = 23
        Height = 16
        Caption = 'sec.'
      end
      object lblComparisonGr: TLabel
        Left = 245
        Top = 13
        Width = 68
        Height = 16
        Caption = 'Comparison'
      end
      object lblGradientPercent: TLabel
        Left = 439
        Top = 37
        Width = 12
        Height = 16
        Caption = '%'
      end
      object edtMonitoring: TEdit
        Left = 83
        Top = 31
        Width = 48
        Height = 24
        Alignment = taCenter
        TabOrder = 0
        Text = '1800'
      end
      object cbInequalityGr: TComboBox
        Left = 218
        Top = 31
        Width = 120
        Height = 24
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Items.Strings = (
          'greater than'
          'less than'
          'between')
      end
      object edtGradientValue: TNumberBox
        Left = 371
        Top = 31
        Width = 62
        Height = 24
        Alignment = taRightJustify
        CurrencyFormat = 2
        Mode = nbmFloat
        TabOrder = 2
        OnChange = OnTickTypeChange
      end
    end
  end
  object alMain: TActionList
    Images = DMImage.vil32
    Left = 560
    Top = 6
    object aOpenTemplate: TAction
      ImageIndex = 5
      ImageName = 'Open_32x32'
    end
    object aSaveTemplate: TAction
      ImageIndex = 10
      ImageName = 'Save_32x32'
    end
    object aSaveTemplateAs: TAction
      ImageIndex = 26
      ImageName = 'SaveAll_32x32'
    end
    object aAddFactor: TAction
      Caption = 'Add Factor'
      ImageIndex = 34
      ImageName = 'AddFooter_32x32'
      OnExecute = aAddFactorExecute
      OnUpdate = aAddFactorUpdate
    end
    object aGraph: TAction
      Caption = 'Graph'
      ImageIndex = 48
      ImageName = 'Area_32x32'
      OnExecute = aGraphExecute
      OnUpdate = aAddFactorUpdate
    end
    object aSave: TAction
      Caption = 'Save'
      Enabled = False
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
      OnUpdate = aSaveUpdate
    end
    object aCalculateCond: TAction
      Caption = 'Calculate Condition'
      OnExecute = aCalculateCondExecute
    end
    object aRefreshPrice: TAction
      Caption = 'Refresh Price'
      OnExecute = aRefreshPriceExecute
      OnUpdate = aRefreshPriceUpdate
    end
    object aSyncChildFactors: TAction
      Caption = 'Sync Full Tree'
      OnExecute = aSyncChildFactorsExecute
    end
    object aShowSearchInstruments: TAction
      Hint = 'Search for instrument'
      ImageIndex = 21
      ImageName = 'Zoom_32x32'
      OnExecute = aShowSearchInstrumentsExecute
    end
  end
end
