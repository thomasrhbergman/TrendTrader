object frmParameters: TfrmParameters
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Parameters'
  ClientHeight = 223
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 180
    Width = 633
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      633
      43)
    object btnOk: TBitBtn
      Left = 533
      Top = 2
      Width = 100
      Height = 40
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ImageIndex = 46
      ImageName = 'tick'
      Images = DMImage.vil32
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 432
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
  end
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 633
    Height = 180
    ActivePage = tsCommon
    Align = alClient
    TabOrder = 1
    object tsCommon: TTabSheet
      Caption = 'Common Parameters'
      object lblPathToContractInspector: TLabel
        Left = 18
        Top = 76
        Width = 132
        Height = 13
        Caption = 'Path To ContractInspector:'
      end
      object lblMinMaxGradientValue: TLabel
        Left = 34
        Top = 103
        Width = 116
        Height = 13
        Caption = 'Min/Max gradient value:'
      end
      object gbAutoorder: TGroupBox
        Left = 0
        Top = 0
        Width = 449
        Height = 70
        Caption = 'Autoorder'
        TabOrder = 0
        object lblWaitingTime: TLabel
          Left = 8
          Top = 47
          Width = 246
          Height = 13
          Alignment = taRightJustify
          Caption = 'Waiting time since receiving the mother'#39's part (ms):'
        end
        object cbAllowEnterAutoorder: TCheckBox
          Left = 7
          Top = 18
          Width = 193
          Height = 17
          Caption = 'Allow enter lines in autoorder group'
          TabOrder = 0
        end
        object cbViewMarketScanner: TCheckBox
          Left = 259
          Top = 18
          Width = 128
          Height = 17
          Caption = 'View Market Scanner'
          TabOrder = 1
        end
        object seWaitingTime: TSpinEdit
          Left = 259
          Top = 43
          Width = 71
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 3000
        end
      end
      object edPathToContractInspector: TButtonedEdit
        Left = 156
        Top = 73
        Width = 362
        Height = 21
        Images = DMImage.vil16
        ReadOnly = True
        RightButton.DisabledImageIndex = 5
        RightButton.DisabledImageName = 'Open_32x32'
        RightButton.HotImageIndex = 5
        RightButton.HotImageName = 'Open_32x32'
        RightButton.ImageIndex = 5
        RightButton.ImageName = 'Open_32x32'
        RightButton.PressedImageIndex = 5
        RightButton.PressedImageName = 'Open_32x32'
        RightButton.Visible = True
        TabOrder = 1
        OnRightButtonClick = aGetPathToContractInspectorExecute
      end
      object edMinMaxGradientValue: TNumberBox
        Left = 156
        Top = 100
        Width = 80
        Height = 21
        AcceptExpressions = True
        TabOrder = 2
        Value = 10.000000000000000000
        SpinButtonOptions.Placement = nbspCompact
        UseMouseWheel = True
        NegativeValueColor = clRed
        OnChangeValue = OnPrecautionaryChangeValue
      end
      object cbIsShowCancelBox: TCheckBox
        Left = 156
        Top = 127
        Width = 231
        Height = 17
        Caption = 'Order cancellation: Show message box'
        TabOrder = 3
      end
    end
    object tsPrecautionarySettings: TTabSheet
      Caption = 'Define Precautionary Settings'
      ImageIndex = -1
      object lblPercentage: TLabel
        Left = 191
        Top = 16
        Width = 59
        Height = 13
        Alignment = taRightJustify
        Caption = 'Percentage:'
      end
      object lblTotalValueLimit: TLabel
        Left = 169
        Top = 40
        Width = 81
        Height = 13
        Alignment = taRightJustify
        Caption = 'Total Value Limit:'
      end
      object lblNumberOfTicks: TLabel
        Left = 170
        Top = 64
        Width = 80
        Height = 13
        Alignment = taRightJustify
        Caption = 'Number of Ticks:'
      end
      object lblAlgorithmTotalValueLimit: TLabel
        Left = 121
        Top = 88
        Width = 129
        Height = 13
        Alignment = taRightJustify
        Caption = 'Algorithm Total Value Limit:'
      end
      object lblAlgorithmSizeLimit: TLabel
        Left = 155
        Top = 112
        Width = 95
        Height = 13
        Alignment = taRightJustify
        Caption = 'Algorithm Size Limit:'
      end
      object lblMinAllowedPrice: TLabel
        Left = 379
        Top = 64
        Width = 86
        Height = 13
        Alignment = taRightJustify
        Caption = 'Min Allowed Price:'
      end
      object lblMaxAllowedPrice: TLabel
        Left = 375
        Top = 40
        Width = 90
        Height = 13
        Alignment = taRightJustify
        Caption = 'Max Allowed Price:'
      end
      object lblOrderQuantityMax: TLabel
        Left = 365
        Top = 16
        Width = 100
        Height = 13
        Alignment = taRightJustify
        Caption = 'Order Quantity Max:'
      end
      object edPercentage: TNumberBox
        Left = 254
        Top = 13
        Width = 80
        Height = 21
        AcceptExpressions = True
        CurrencyString = '%'
        Mode = nbmCurrency
        TabOrder = 0
        UseMouseWheel = True
        NegativeValueColor = clRed
        OnChangeValue = OnPrecautionaryChangeValue
      end
      object edTotalValueLimit: TNumberBox
        Left = 254
        Top = 37
        Width = 80
        Height = 21
        AcceptExpressions = True
        Mode = nbmFloat
        TabOrder = 1
        UseMouseWheel = True
        NegativeValueColor = clRed
        OnChangeValue = OnPrecautionaryChangeValue
      end
      object edNumberOfTicks: TNumberBox
        Left = 254
        Top = 61
        Width = 80
        Height = 21
        AcceptExpressions = True
        TabOrder = 2
        SpinButtonOptions.Placement = nbspCompact
        UseMouseWheel = True
        NegativeValueColor = clRed
        OnChangeValue = OnPrecautionaryChangeValue
      end
      object edAlgorithmTotalValueLimit: TNumberBox
        Left = 254
        Top = 85
        Width = 80
        Height = 21
        AcceptExpressions = True
        Mode = nbmFloat
        TabOrder = 3
        UseMouseWheel = True
        NegativeValueColor = clRed
        OnChangeValue = OnPrecautionaryChangeValue
      end
      object edAlgorithmSizeLimit: TNumberBox
        Left = 254
        Top = 109
        Width = 80
        Height = 21
        AcceptExpressions = True
        TabOrder = 4
        SpinButtonOptions.Placement = nbspCompact
        UseMouseWheel = True
        NegativeValueColor = clRed
        OnChangeValue = OnPrecautionaryChangeValue
      end
      object edOrderQuantityMax: TNumberBox
        Left = 469
        Top = 13
        Width = 80
        Height = 21
        AcceptExpressions = True
        TabOrder = 5
        SpinButtonOptions.Placement = nbspCompact
        UseMouseWheel = True
        NegativeValueColor = clRed
        OnChangeValue = OnPrecautionaryChangeValue
      end
      object edMinAllowedPrice: TNumberBox
        Left = 469
        Top = 61
        Width = 80
        Height = 21
        AcceptExpressions = True
        Mode = nbmFloat
        TabOrder = 6
        UseMouseWheel = True
        NegativeValueColor = clRed
        OnChangeValue = OnPrecautionaryChangeValue
      end
      object edMaxAllowedPrice: TNumberBox
        Left = 469
        Top = 37
        Width = 80
        Height = 21
        AcceptExpressions = True
        Mode = nbmFloat
        TabOrder = 7
        UseMouseWheel = True
        NegativeValueColor = clRed
        OnChangeValue = OnPrecautionaryChangeValue
      end
      object lbSecurityType: TListBox
        Left = 0
        Top = 0
        Width = 113
        Height = 152
        Align = alLeft
        ItemHeight = 13
        ScrollWidth = 5
        TabOrder = 8
        OnClick = lbSecurityTypeClick
      end
      object btnCopyToAll: TButton
        Left = 469
        Top = 105
        Width = 75
        Height = 25
        Action = aCopyToAll
        TabOrder = 9
      end
    end
    object tsEmergencySettings: TTabSheet
      Caption = 'Emergency Settings'
      ImageIndex = 6
      object gbEmergencySettings: TGroupBox
        Left = 0
        Top = 0
        Width = 249
        Height = 152
        Align = alLeft
        Caption = 'Default ordertype-'#1057'lose'
        TabOrder = 0
        object rbLMT: TRadioButton
          Left = 16
          Top = 41
          Width = 145
          Height = 17
          Caption = 'Limit order  Limit Price (%):'
          TabOrder = 1
          OnClick = rbLMTClick
        end
        object rbMKT: TRadioButton
          Left = 16
          Top = 18
          Width = 113
          Height = 17
          Caption = 'Market order'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbLMTClick
        end
        object edPercent: TSpinEdit
          Left = 166
          Top = 39
          Width = 64
          Height = 22
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxValue = 0
          MinValue = 0
          ParentFont = False
          TabOrder = 2
          Value = 0
        end
      end
    end
    object tsCoefficients: TTabSheet
      Caption = 'Adjustment Coefficients'
      ImageIndex = 5
      object cbUseVolatility: TCheckBox
        Left = 311
        Top = 9
        Width = 140
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Use Volatility Coefficients'
        TabOrder = 0
        OnClick = cbUseVolatilityClick
      end
      object pnlVolatility: TPanel
        AlignWithMargins = True
        Left = 332
        Top = 35
        Width = 290
        Height = 114
        Margins.Top = 35
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object lblAdjustmentCoefPrice: TLabel
          Left = 61
          Top = 3
          Width = 49
          Height = 13
          Alignment = taRightJustify
          Caption = 'Price (%):'
        end
        object lblAdjustmentCoefVolatility: TLabel
          Left = 44
          Top = 29
          Width = 66
          Height = 13
          Alignment = taRightJustify
          Caption = 'Volatility (%):'
        end
        object lblAdjustmentCoefNumberOfHours: TLabel
          Left = 25
          Top = 55
          Width = 85
          Height = 13
          Alignment = taRightJustify
          Caption = 'Number of Hours:'
        end
        object seAdjustmentCoefVolatility: TSpinEdit
          Left = 116
          Top = 26
          Width = 71
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 0
          Value = 60
        end
        object seAdjustmentCoefPrice: TSpinEdit
          Left = 116
          Top = 0
          Width = 71
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 100
        end
        object seAdjustmentCoefNumberOfHours: TSpinEdit
          Left = 116
          Top = 52
          Width = 71
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 24
        end
      end
      object pnlAdjustment: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 35
        Width = 290
        Height = 114
        Margins.Top = 35
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 2
        object lblMaxAdjustmentCoef: TLabel
          Left = 13
          Top = 29
          Width = 159
          Height = 13
          Alignment = taRightJustify
          Caption = 'Max Adjustment Coefficient (%):'
        end
        object lblAdjustmentCoef: TLabel
          Left = 36
          Top = 3
          Width = 136
          Height = 13
          Alignment = taRightJustify
          Caption = 'Adjustment Coefficient (%):'
        end
        object seAdjustmentCoefMax: TSpinEdit
          Left = 176
          Top = 26
          Width = 71
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 0
          Value = 2
        end
        object seAdjustmentCoef: TSpinEdit
          Left = 176
          Top = 0
          Width = 71
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 100
        end
      end
      object cbUseAdjustment: TCheckBox
        Left = 39
        Top = 9
        Width = 153
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Use Adjustment Coefficients'
        TabOrder = 3
        OnClick = cbUseAdjustmentClick
      end
    end
    object tsExchange: TTabSheet
      Caption = 'Exchange'
      ImageIndex = 3
      object lblOpeningTime: TLabel
        Left = 16
        Top = 6
        Width = 100
        Height = 13
        Alignment = taRightJustify
        Caption = 'Opening Time (CUT):'
      end
      object lblClosingTime: TLabel
        Left = 22
        Top = 32
        Width = 94
        Height = 13
        Alignment = taRightJustify
        Caption = #1057'losing Time (CUT):'
      end
      object edTimeCloseExchange: TDateTimePicker
        Left = 122
        Top = 30
        Width = 80
        Height = 21
        Date = 44033.000000000000000000
        Time = 0.729166666664241300
        Kind = dtkTime
        TabOrder = 0
      end
      object edTimeOpenExchange: TDateTimePicker
        Left = 122
        Top = 3
        Width = 80
        Height = 21
        Date = 44041.000000000000000000
        Time = 0.291666666664241300
        Kind = dtkTime
        TabOrder = 1
      end
      object cbShowQbalgo: TCheckBox
        Left = 120
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Show QBALGO'
        TabOrder = 2
      end
    end
    object tsGenericTickRequired: TTabSheet
      Caption = 'Generic Tick Required'
      ImageIndex = 8
      object lbGenericTickRequired: TCheckListBox
        Left = 0
        Top = 0
        Width = 625
        Height = 152
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object tsSchedule: TTabSheet
      Caption = 'Price Load Schedule'
      ImageIndex = 2
      object lblPriceScheduleCheck: TLabel
        Left = 9
        Top = 7
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Caption = 'Last Price:'
      end
      object lblDeleteInstruments: TLabel
        Left = 9
        Top = 30
        Width = 253
        Height = 13
        Caption = 'Delete Instruments from DB for the following  errors:'
      end
      object edPriceScheduleCheck: TDateTimePicker
        Left = 63
        Top = 3
        Width = 73
        Height = 21
        Date = 44033.000000000000000000
        Time = 0.736111111109494200
        Kind = dtkTime
        TabOrder = 0
      end
      object lbCodeErrors: TCheckListBox
        Left = 0
        Top = 52
        Width = 625
        Height = 100
        Align = alBottom
        ItemHeight = 13
        Items.Strings = (
          '200'
          '201'
          '203'
          '204'
          '205')
        TabOrder = 1
      end
    end
    object tsLogFile: TTabSheet
      Caption = 'Log File Parameters'
      ImageIndex = 1
      object lblMaxSizeOfLogFile: TLabel
        Left = 47
        Top = 34
        Width = 125
        Height = 13
        Alignment = taRightJustify
        Caption = 'Max Size Of Log File (Mb):'
      end
      object lblCountOfDays: TLabel
        Left = 95
        Top = 6
        Width = 75
        Height = 13
        Alignment = taRightJustify
        Caption = 'Count Of Days:'
      end
      object edMaxSizeOfLogFile: TSpinEdit
        Left = 176
        Top = 31
        Width = 71
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object edCountOfDays: TSpinEdit
        Left = 176
        Top = 3
        Width = 71
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 30
      end
    end
    object tsDatabaseBackup: TTabSheet
      Caption = 'Database Backup'
      ImageIndex = 4
      object lblPathToBackup: TLabel
        Left = 2
        Top = 30
        Width = 111
        Height = 13
        Caption = 'Path To Backup Folder:'
      end
      object lblBackupScheduleTime: TLabel
        Left = 41
        Top = 58
        Width = 72
        Height = 13
        Alignment = taRightJustify
        Caption = 'Schedule Time:'
      end
      object cbBackupIsActive: TCheckBox
        Left = 120
        Top = 6
        Width = 193
        Height = 17
        Caption = 'Database backup is active'
        TabOrder = 0
      end
      object edBackupScheduleTime: TDateTimePicker
        Left = 120
        Top = 54
        Width = 73
        Height = 21
        Date = 44033.000000000000000000
        Time = 0.625000000000000000
        Kind = dtkTime
        TabOrder = 1
      end
      object edPathToBackup: TButtonedEdit
        Left = 120
        Top = 27
        Width = 386
        Height = 21
        Images = DMImage.vil16
        ReadOnly = True
        RightButton.DisabledImageIndex = 5
        RightButton.DisabledImageName = 'Open_32x32'
        RightButton.HotImageIndex = 5
        RightButton.HotImageName = 'Open_32x32'
        RightButton.ImageIndex = 5
        RightButton.ImageName = 'Open_32x32'
        RightButton.PressedImageIndex = 5
        RightButton.PressedImageName = 'Open_32x32'
        RightButton.Visible = True
        TabOrder = 2
        OnRightButtonClick = aGetPathToBackupExecute
      end
    end
  end
  object OpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileName = 'D:\Work\RobotX\Sources'
    FileTypes = <
      item
        DisplayName = ''
      end>
    Options = [fdoFileMustExist]
    Left = 572
    Top = 104
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 572
    Top = 40
    object aGetPathToBackup: TAction
      OnExecute = aGetPathToBackupExecute
    end
    object aGetPathToContractInspector: TAction
      OnExecute = aGetPathToContractInspectorExecute
    end
    object aCopyToAll: TAction
      Caption = 'Copy To All'
      OnExecute = aCopyToAllExecute
    end
  end
end
