object frmScannerAllFilters: TfrmScannerAllFilters
  Left = 267
  Top = 68
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Scanner Extra Filter'
  ClientHeight = 524
  ClientWidth = 319
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 0
    Top = 0
    Width = 319
    Height = 16
    Align = alTop
    Alignment = taCenter
    Caption = 'Numbers should be less than 2 147 483 646'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitWidth = 297
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 479
    Width = 319
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnCancel: TBitBtn
      Left = 118
      Top = 4
      Width = 100
      Height = 40
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
      TabOrder = 0
    end
    object btnAddColumn: TBitBtn
      Left = 218
      Top = 4
      Width = 100
      Height = 40
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
      TabOrder = 1
    end
    object btnClear: TBitBtn
      Left = 18
      Top = 4
      Width = 100
      Height = 40
      Caption = 'Clear'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 2
      OnClick = btnClearClick
    end
  end
  object pnlMain: TGridPanel
    Left = 0
    Top = 16
    Width = 319
    Height = 463
    Align = alClient
    ColumnCollection = <
      item
        Value = 60.975609756097560000
      end
      item
        Value = 39.024390243902440000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = lblAbovePrice
        Row = 0
      end
      item
        Column = 1
        Control = edtAbovePrice
        Row = 0
      end
      item
        Column = 0
        Control = lblBelowPrice
        Row = 1
      end
      item
        Column = 1
        Control = edtBelowPrice
        Row = 1
      end
      item
        Column = 0
        Control = lblAboveVolume
        Row = 2
      end
      item
        Column = 1
        Control = edtAboveVolume
        Row = 2
      end
      item
        Column = 0
        Control = lblMarketCapAbove
        Row = 3
      end
      item
        Column = 1
        Control = edtMarketCapAbove
        Row = 3
      end
      item
        Column = 0
        Control = lblMarketCapBelow
        Row = 4
      end
      item
        Column = 1
        Control = edtMarketCapBelow
        Row = 4
      end
      item
        Column = 0
        Control = lblMoodyRatingAbove
        Row = 5
      end
      item
        Column = 1
        Control = edtMoodyRatingAbove
        Row = 5
      end
      item
        Column = 0
        Control = lblMoodyRatingBelow
        Row = 6
      end
      item
        Column = 1
        Control = edtMoodyRatingBelow
        Row = 6
      end
      item
        Column = 0
        Control = lblSPRatingAbove
        Row = 7
      end
      item
        Column = 1
        Control = edtSPRatingAbove
        Row = 7
      end
      item
        Column = 0
        Control = lblSPRatingBelow
        Row = 8
      end
      item
        Column = 1
        Control = edtSPRatingBelow
        Row = 8
      end
      item
        Column = 0
        Control = lblMaturityDateAbove
        Row = 9
      end
      item
        Column = 1
        Control = edtMaturityDateAbove
        Row = 9
      end
      item
        Column = 0
        Control = lblMaturityDateBelow
        Row = 10
      end
      item
        Column = 1
        Control = edtMaturityDateBelow
        Row = 10
      end
      item
        Column = 0
        Control = lblCouponRateAbove
        Row = 11
      end
      item
        Column = 1
        Control = edtCouponRateAbove
        Row = 11
      end
      item
        Column = 0
        Control = lblCouponRateBelow
        Row = 12
      end
      item
        Column = 1
        Control = edtCouponRateBelow
        Row = 12
      end
      item
        Column = 0
        Control = lblExcludeConvertible
        Row = 13
      end
      item
        Column = 1
        Control = edtExcludeConvertible
        Row = 13
      end
      item
        Column = 0
        Control = lblAverageOptionVolumeAbove
        Row = 14
      end
      item
        Column = 1
        Control = edtAverageOptionVolumeAbove
        Row = 14
      end
      item
        Column = 0
        Control = lblScannerSettingPairs
        Row = 15
      end
      item
        Column = 1
        Control = edtScannerSettingPairs
        Row = 15
      end
      item
        Column = 0
        Control = lblStockTypeFilter
        Row = 16
      end
      item
        Column = 1
        Control = edtStockTypeFilter
        Row = 16
      end>
    RowCollection = <
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 27.000000000000000000
      end>
    TabOrder = 1
    DesignSize = (
      319
      463)
    object lblAbovePrice: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'AbovePrice:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = 136
      ExplicitWidth = 58
      ExplicitHeight = 13
    end
    object edtAbovePrice: TEdit
      Left = 195
      Top = 2
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnKeyPress = DoKeyPress
    end
    object lblBelowPrice: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 28
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'BelowPrice:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = 71
      ExplicitTop = 42
      ExplicitWidth = 56
      ExplicitHeight = 13
    end
    object edtBelowPrice: TEdit
      Left = 195
      Top = 29
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnKeyPress = DoKeyPress
    end
    object lblAboveVolume: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 55
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'AboveVolume'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -82
      ExplicitTop = 74
      ExplicitWidth = 86
      ExplicitHeight = 16
    end
    object edtAboveVolume: TSpinEdit
      Left = 195
      Top = 55
      Width = 121
      Height = 26
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxValue = 2147483646
      MinValue = 0
      ParentFont = False
      TabOrder = 2
      Value = 0
    end
    object lblMarketCapAbove: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 82
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'MarketCapAbove (M):'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -43
      ExplicitTop = 105
      ExplicitWidth = 131
      ExplicitHeight = 16
    end
    object edtMarketCapAbove: TEdit
      Left = 195
      Top = 83
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      Text = '100'
      OnKeyPress = DoKeyPress
    end
    object lblMarketCapBelow: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 109
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'MarketCapBelow (M):'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -40
      ExplicitTop = 137
      ExplicitWidth = 128
      ExplicitHeight = 16
    end
    object edtMarketCapBelow: TEdit
      Left = 195
      Top = 110
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnKeyPress = DoKeyPress
    end
    object lblMoodyRatingAbove: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 136
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'MoodyRatingAbove:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -36
      ExplicitTop = 169
      ExplicitWidth = 124
      ExplicitHeight = 16
    end
    object edtMoodyRatingAbove: TEdit
      Left = 195
      Top = 137
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object lblMoodyRatingBelow: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 163
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'MoodyRatingBelow:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -33
      ExplicitTop = 201
      ExplicitWidth = 121
      ExplicitHeight = 16
    end
    object edtMoodyRatingBelow: TEdit
      Left = 195
      Top = 164
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
    end
    object lblSPRatingAbove: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 190
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'SPRatingAbove:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -12
      ExplicitTop = 233
      ExplicitWidth = 100
      ExplicitHeight = 16
    end
    object edtSPRatingAbove: TEdit
      Left = 195
      Top = 191
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
    end
    object lblSPRatingBelow: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 217
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'SPRatingBelow:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -9
      ExplicitTop = 265
      ExplicitWidth = 97
      ExplicitHeight = 16
    end
    object edtSPRatingBelow: TEdit
      Left = 195
      Top = 218
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
    end
    object lblMaturityDateAbove: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 244
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'MaturityDateAbove:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -30
      ExplicitTop = 297
      ExplicitWidth = 118
      ExplicitHeight = 16
    end
    object edtMaturityDateAbove: TEdit
      Left = 195
      Top = 245
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
    end
    object lblMaturityDateBelow: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 271
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'MaturityDateBelow:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -27
      ExplicitTop = 329
      ExplicitWidth = 115
      ExplicitHeight = 16
    end
    object edtMaturityDateBelow: TEdit
      Left = 195
      Top = 272
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
    end
    object lblCouponRateAbove: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 298
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'CouponRateAbove:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -31
      ExplicitTop = 361
      ExplicitWidth = 119
      ExplicitHeight = 16
    end
    object edtCouponRateAbove: TEdit
      Left = 195
      Top = 299
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
      OnKeyPress = DoKeyPress
    end
    object lblCouponRateBelow: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 325
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'CouponRateBelow:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -28
      ExplicitTop = 393
      ExplicitWidth = 116
      ExplicitHeight = 16
    end
    object edtCouponRateBelow: TEdit
      Left = 195
      Top = 326
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
      OnKeyPress = DoKeyPress
    end
    object lblExcludeConvertible: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 352
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ExcludeConvertible:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -31
      ExplicitTop = 425
      ExplicitWidth = 119
      ExplicitHeight = 16
    end
    object edtExcludeConvertible: TSpinEdit
      Left = 195
      Top = 352
      Width = 121
      Height = 26
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxValue = 2147483647
      MinValue = 0
      ParentFont = False
      TabOrder = 13
      Value = 0
    end
    object lblAverageOptionVolumeAbove: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 379
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'AverageOptionVolumeAbove:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -92
      ExplicitTop = 457
      ExplicitWidth = 180
      ExplicitHeight = 16
    end
    object edtAverageOptionVolumeAbove: TSpinEdit
      Left = 195
      Top = 379
      Width = 121
      Height = 26
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      MaxValue = 2147483646
      MinValue = 0
      ParentFont = False
      TabOrder = 14
      Value = 0
    end
    object lblScannerSettingPairs: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 406
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ScannerSettingPairs:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -37
      ExplicitTop = 489
      ExplicitWidth = 125
      ExplicitHeight = 16
    end
    object edtScannerSettingPairs: TEdit
      Left = 195
      Top = 407
      Width = 121
      Height = 24
      Anchors = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 15
    end
    object lblStockTypeFilter: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 433
      Width = 190
      Height = 27
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'StockTypeFilter:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = -10
      ExplicitTop = 521
      ExplicitWidth = 98
      ExplicitHeight = 16
    end
    object edtStockTypeFilter: TEdit
      Left = 195
      Top = 434
      Width = 121
      Height = 24
      Anchors = []
      CharCase = ecUpperCase
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 16
      Text = 'ALL'
    end
  end
end
