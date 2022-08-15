object frmCandidateMain: TfrmCandidateMain
  Left = 0
  Top = 0
  Caption = 'Scan'
  ClientHeight = 649
  ClientWidth = 1039
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 1055
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlAutoOrder: TPanel
    Left = 842
    Top = 0
    Width = 197
    Height = 583
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    ExplicitHeight = 630
    object gbSpecificationAutoTrade: TGroupBox
      AlignWithMargins = True
      Left = 0
      Top = 55
      Width = 197
      Height = 442
      Margins.Left = 0
      Margins.Top = 55
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Specification this AutoTrade'
      TabOrder = 0
      object gbAutoOrder: TGroupBox
        AlignWithMargins = True
        Left = 5
        Top = 84
        Width = 187
        Height = 128
        Margins.Bottom = 0
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object lblSingleOrderAmount: TLabel
          Left = 6
          Top = 28
          Width = 122
          Height = 16
          Caption = 'Single order amount:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbOrderCurrencyAdd: TSpeedButton
          Left = 139
          Top = 45
          Width = 23
          Height = 22
          Hint = 'Add currency to list'
          ImageIndex = 41
          ImageName = 'plus'
          Images = DMImage.vil16
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          OnClick = cbOrderCurrencyAddClick
        end
        object lblTotalOrderAmount: TLabel
          Left = 9
          Top = 5
          Width = 116
          Height = 16
          Caption = 'Total order amount:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblMaxRows: TLabel
          Left = 26
          Top = 101
          Width = 99
          Height = 16
          Caption = 'Limit of Symbols:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object seSingleOrderAmount: TSpinEdit
          Left = 6
          Top = 46
          Width = 65
          Height = 23
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = OnGUIToAutoTradeInfo
        end
        object cbOrderCurrency: TComboBox
          Left = 73
          Top = 46
          Width = 64
          Height = 22
          CharCase = ecUpperCase
          TabOrder = 2
          OnChange = OnGUIToAutoTradeInfo
          OnClick = OnGUIToAutoTradeInfo
        end
        object seTotalOrderAmount: TSpinEdit
          Left = 130
          Top = 3
          Width = 55
          Height = 23
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = OnGUIToAutoTradeInfo
        end
        object seMaxRows: TSpinEdit
          Left = 130
          Top = 99
          Width = 55
          Height = 23
          Hint = 'Number of symbols monitored in current Sequence'
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          Value = 50
          OnChange = OnGUIToAutoTradeInfo
        end
      end
      object rgWeightedFeed: TRadioGroup
        AlignWithMargins = True
        Left = 5
        Top = 366
        Width = 187
        Height = 65
        Hint = 'Ctrl+W'
        Margins.Top = 5
        Align = alTop
        Caption = 'Show Weighted/No weighted'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemIndex = 0
        Items.Strings = (
          'Show feed value'
          'Show weighted value')
        ParentFont = False
        TabOrder = 1
        OnClick = rgWeightedFeedClick
      end
      object pnlAutoOrderTop: TPanel
        Left = 2
        Top = 15
        Width = 193
        Height = 66
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object cbAutoRefresh: TCheckBox
          Left = 5
          Top = 4
          Width = 100
          Height = 17
          Caption = 'Auto Refresh'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = cbAutoRefreshClick
        end
        object cbAllowSendDuplicateOrder: TCheckBox
          Left = 5
          Top = 44
          Width = 188
          Height = 17
          Caption = 'Allow send duplicate order'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = OnGUIToAutoTradeInfo
        end
        object cbAutoOrderActive: TCheckBox
          Left = 5
          Top = 24
          Width = 180
          Height = 17
          Caption = 'Activate/Deactivate'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = cbAutoOrderActiveClick
        end
      end
      object gbHistoricalOptions: TGroupBox
        Left = 2
        Top = 212
        Width = 193
        Height = 149
        Align = alTop
        Caption = 'Historical Options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        object lblValidBarSize: TLabel
          Left = 9
          Top = 66
          Width = 82
          Height = 16
          Alignment = taRightJustify
          Caption = 'Valid bar size:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblDuration: TLabel
          Left = 38
          Top = 94
          Width = 53
          Height = 16
          Alignment = taRightJustify
          Caption = 'Duration:'
        end
        object cbValidBarSize: TComboBox
          Left = 97
          Top = 63
          Width = 85
          Height = 24
          TabOrder = 0
          OnChange = OnGUIToAutoTradeInfo
        end
        object cbHistDataKeepUpdated: TCheckBox
          Left = 4
          Top = 43
          Width = 182
          Height = 17
          Hint = 'For Calculation of Trendlines we use historical prices'
          Caption = 'Keep Historical Data Updated'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = OnGUIToAutoTradeInfo
        end
        object cbDurationTimeUnits: TComboBox
          Left = 97
          Top = 90
          Width = 85
          Height = 24
          TabOrder = 2
          OnChange = OnGUIToAutoTradeInfo
        end
        object edDuration: TNumberBox
          Left = 97
          Top = 117
          Width = 85
          Height = 24
          TabOrder = 3
          Value = 1.000000000000000000
          SpinButtonOptions.Placement = nbspCompact
          UseMouseWheel = True
          OnChange = OnGUIToAutoTradeInfo
        end
        object cbSubscribeHistoricalData: TCheckBox
          Left = 4
          Top = 22
          Width = 180
          Height = 17
          Hint = 'For Calculation of Trendlines we use historical prices'
          Caption = 'Subscribe Historical Data'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnClick = OnGUIToAutoTradeInfo
        end
      end
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 842
    Height = 583
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 630
    object pnlOptions: TPanel
      Left = 0
      Top = 0
      Width = 842
      Height = 50
      Align = alTop
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 0
      object lblAutoTradeTemplateCaption: TLabel
        Left = 205
        Top = 0
        Width = 38
        Height = 16
        Alignment = taRightJustify
        Caption = 'Name:'
        Color = clDefault
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lblMaxNumberOrder: TLabel
        Left = 483
        Top = 1
        Width = 88
        Height = 14
        Caption = 'Candidates limit:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object edAutoTradeTemplate: TEdit
        Left = 205
        Top = 22
        Width = 260
        Height = 21
        TabOrder = 0
        OnChange = OnGUIToAutoTradeInfo
      end
      object btnOpenStaticList: TBitBtn
        Left = 5
        Top = 7
        Width = 36
        Height = 36
        Action = aOpenStaticList
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object btnOpenIBScanning: TBitBtn
        Left = 40
        Top = 7
        Width = 36
        Height = 36
        Action = aOpenIBScanning
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
      object btnOpenTickColumns: TBitBtn
        Left = 76
        Top = 7
        Width = 36
        Height = 36
        Action = aOpenTickColumns
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
      object btnAddEmbargoColumn: TBitBtn
        Left = 148
        Top = 7
        Width = 36
        Height = 36
        Action = aAddEmbargoColumn
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object btnOpenGradientColumn: TBitBtn
        Left = 112
        Top = 7
        Width = 36
        Height = 36
        Action = aOpenGradientColumn
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
      end
      object seMaxNumberOrder: TSpinEdit
        Left = 483
        Top = 21
        Width = 88
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 6
        Value = 0
        OnChange = OnGUIToAutoTradeInfo
      end
    end
    object pnlContent: TPanel
      Left = 0
      Top = 50
      Width = 842
      Height = 533
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 580
      object vstCandidate: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 842
        Height = 450
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        DragType = dtVCL
        Header.AutoSizeIndex = -1
        Header.Height = 21
        Header.Options = [hoColumnResize, hoDrag, hoOwnerDraw, hoShowHint, hoShowImages, hoVisible]
        Header.SortColumn = 1
        HintMode = hmHint
        ParentShowHint = False
        PopupMenu = PopupMenu
        ShowHint = True
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toAutoChangeScale]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect]
        OnAdvancedHeaderDraw = vstCandidateAdvancedHeaderDraw
        OnBeforeCellPaint = vstCandidateBeforeCellPaint
        OnCompareNodes = vstCandidateCompareNodes
        OnDragAllowed = vstCandidateDragAllowed
        OnDragOver = vstCandidateDragOver
        OnDrawText = vstCandidateDrawText
        OnFreeNode = vstCandidateFreeNode
        OnGetText = vstCandidateGetText
        OnPaintText = vstCandidatePaintText
        OnGetHint = vstCandidateGetHint
        OnHeaderDblClick = vstCandidateHeaderDblClick
        OnHeaderDragging = vstCandidateHeaderDragging
        OnHeaderDrawQueryElements = vstCandidateHeaderDrawQueryElements
        OnHeaderMouseDown = vstCandidateHeaderMouseDown
        OnInitNode = vstCandidateInitNode
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        ExplicitHeight = 497
        Columns = <
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
            Position = 0
            Text = 'Symbol'
            Width = 114
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
            Position = 1
            Text = 'Ranking sum'
            Width = 80
          end>
      end
      object gbIsolate: TGroupBox
        Left = 0
        Top = 450
        Width = 842
        Height = 83
        Align = alBottom
        Caption = 'Excluded Instruments'
        Color = clBtnFace
        ParentBackground = False
        ParentColor = False
        TabOrder = 1
        Visible = False
        ExplicitTop = 497
        object lbIsolate: TListBox
          Left = 2
          Top = 15
          Width = 838
          Height = 66
          Align = alClient
          Color = clInfoBk
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 0
        end
      end
    end
    object lbColumns: TListBox
      Left = 600
      Top = 0
      Width = 439
      Height = 44
      ItemHeight = 13
      TabOrder = 2
      OnClick = lbColumnsClick
    end
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 630
    Width = 1039
    Height = 19
    DoubleBuffered = True
    Panels = <
      item
        Alignment = taRightJustify
        Text = 'Last Update: '
        Width = 80
      end
      item
        Text = ' '
        Width = 70
      end
      item
        Alignment = taRightJustify
        Text = 'Created Orders: '
        Width = 100
      end
      item
        Text = '0'
        Width = 30
      end
      item
        Alignment = taRightJustify
        Text = 'Last Created Order: '
        Width = 120
      end
      item
        Text = '0'
        Width = 30
      end
      item
        Alignment = taRightJustify
        Text = 'Total Number Of Scans: '
        Width = 135
      end
      item
        Text = '0'
        Width = 30
      end
      item
        Alignment = taRightJustify
        Text = 'Last Scan Count:'
        Width = 100
      end
      item
        Text = '0'
        Width = 30
      end
      item
        Alignment = taCenter
        Text = 'Auto Refresh'
        Width = 90
      end
      item
        Width = 50
      end>
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = False
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 583
    Width = 1039
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      1039
      47)
    object btnSave: TBitBtn
      Left = 932
      Top = 3
      Width = 100
      Height = 40
      Anchors = [akTop, akRight]
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ImageIndex = 46
      ImageName = 'tick'
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 0
      OnClick = btnSaveClick
    end
    object btnCancel: TBitBtn
      Left = 831
      Top = 3
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
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 32
    Top = 192
    object aExecute: TAction
      Hint = 'Execute'
      ImageIndex = 12
      ImageName = 'lightning'
      OnExecute = aExecuteExecute
    end
    object aExecuteDuplicateOrder: TAction
      Hint = 'Send Duplicate Order '
      ImageIndex = 14
      ImageName = 'lightning_add'
      OnExecute = aExecuteDuplicateOrderExecute
    end
    object aChangeWeigthValue: TAction
      Caption = 'Change weight value'
      OnExecute = aChangeWeigthValueExecute
      OnUpdate = aChangeWeigthValueUpdate
    end
    object aChangeWeigthValueColumn: TAction
      Caption = 'Change weight value for the column'
      OnExecute = aChangeWeigthValueColumnExecute
      OnUpdate = aChangeWeigthValueColumnUpdate
    end
    object aCopySelectedNodeToMonitor: TAction
      Caption = 'Transfer order to Monitor'
      OnExecute = aCopySelectedNodeToMonitorExecute
      OnUpdate = aCopySelectedNodeToMonitorUpdate
    end
    object aDeleteAllNodes: TAction
      Hint = 'Clear / Delete all nodes'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      SecondaryShortCuts.Strings = (
        'Shift+Del')
      ShortCut = 8238
      OnExecute = aDeleteAllNodesExecute
    end
    object aDeleteColumn: TAction
      Caption = 'Delete column'
      OnExecute = aDeleteColumnExecute
    end
    object aExit: TAction
      Caption = 'Exit'
      OnExecute = aExitExecute
    end
    object aInformationDialog: TAction
      ImageIndex = 6
      ImageName = 'Info_32x32'
      OnExecute = aInformationDialogExecute
    end
    object aInstrumentInfo: TAction
      Caption = 'Get Instrument Info'
      ShortCut = 16457
      OnExecute = aInstrumentInfoExecute
      OnUpdate = aInstrumentInfoUpdate
    end
    object aOpenTickColumns: TAction
      Hint = 'Add Tick Column'
      ImageIndex = 7
      ImageName = 'InsertColumns_32x32'
      OnExecute = aOpenTickColumnsExecute
    end
    object aOpenGradientColumn: TAction
      Hint = 'Add Gradient Column'
      ImageIndex = 67
      ImageName = 'AddCalculatedField_32x32'
      OnExecute = aOpenGradientColumnExecute
    end
    object aOpenAutoTradeTemplate: TAction
      ImageIndex = 5
      ImageName = 'Open_32x32'
      OnExecute = aOpenAutoTradeTemplateExecute
    end
    object aAddEmbargoColumn: TAction
      Hint = 'Add Embargo Column'
      ImageIndex = 20
      ImageName = 'UnprotectDocument_32x32'
      OnExecute = aAddEmbargoColumnExecute
    end
    object aOpenIBScanning: TAction
      Hint = 'Opens the IB scanning'
      ImageIndex = 8
      ImageName = 'Zoom_IB'
      OnExecute = aOpenIBScanningExecute
    end
    object aOpenStaticList: TAction
      Hint = 'Opens the Static lists'
      ImageIndex = 9
      ImageName = 'TextBox_32x32'
      OnExecute = aOpenStaticListExecute
    end
    object aSaveAutoTradeTemplate: TAction
      Hint = 
        'Save AutoTrade Template with existing Scan Sequence and OrderTem' +
        'plate'
      ImageIndex = 10
      ImageName = 'Save_32x32'
      OnExecute = aSaveAutoTradeTemplateExecute
    end
    object aSaveAutoTradeTemplateAs: TAction
      Hint = 
        'Save as New AutoTrade Template with existing Scan Sequence and O' +
        'rderTemplate'
      ImageIndex = 26
      ImageName = 'SaveAll_32x32'
      OnExecute = aSaveAutoTradeTemplateAsExecute
    end
    object aShowColumnDetails: TAction
      Caption = 'Open Column Setting'
      OnExecute = aShowColumnDetailsExecute
      OnUpdate = aShowColumnDetailsUpdate
    end
    object aShowWeighted: TAction
      ShortCut = 16471
      OnExecute = aShowWeightedExecute
    end
    object aShowGlobalSettings: TAction
      Hint = 'PreC Settings'
      ImageIndex = 63
      ImageName = 'PageSetup_32x32'
      OnExecute = aShowGlobalSettingsExecute
    end
    object aShowTradeChart: TAction
      Caption = 'Show Trade Chart'
      OnExecute = aShowTradeChartExecute
      OnUpdate = aShowTradeChartUpdate
    end
    object aShowPriceHistory: TAction
      Caption = 'Show Price Histroy'
      ShortCut = 16456
      OnExecute = aShowPriceHistoryExecute
    end
  end
  object PopupMenu: TPopupMenu
    Left = 120
    Top = 456
    object miChangeWeigthNodeValue: TMenuItem
      Action = aChangeWeigthValue
    end
    object miDeleteColumn: TMenuItem
      Action = aDeleteColumn
    end
    object miSaveAs: TMenuItem
      Caption = 'Save as new Scan Sequence'
      Hint = 'Save as new Scan Sequence'
    end
    object miDeleteAllNodes: TMenuItem
      Action = aDeleteAllNodes
      Caption = 'Clear / Delete all nodes'
    end
    object miCopySelectedNodeToMonitor: TMenuItem
      Action = aCopySelectedNodeToMonitor
    end
    object miShowColumnScanSettings: TMenuItem
      Action = aShowColumnDetails
      ShortCut = 32836
    end
    object miGetInstrumentInfo: TMenuItem
      Action = aInstrumentInfo
    end
    object miShowTradeChart: TMenuItem
      Action = aShowTradeChart
    end
    object miShowPriceHistory: TMenuItem
      Action = aShowPriceHistory
    end
    object miSep: TMenuItem
      Caption = '-'
    end
    object miExit: TMenuItem
      Action = aExit
    end
  end
  object TimerCalculateGradient: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = TimerCalculateGradientTimer
    Left = 128
    Top = 320
  end
  object TimerEmbargo: TTimer
    Enabled = False
    OnTimer = TimerEmbargoTimer
    Left = 128
    Top = 376
  end
  object BalloonHint: TBalloonHint
    Images = ilBalloonHint
    Delay = 100
    Left = 444
    Top = 175
  end
  object ilBalloonHint: TImageList
    Height = 32
    Width = 32
    Left = 445
    Top = 230
    Bitmap = {
      494C010101000800040020002000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000800000002000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000060E1B001F47
      84002759A5002759A6002759A6002759A6002759A6002759A6002759A6002759
      A6002759A6002759A6002759A6002759A6002759A6002759A6002759A6002759
      A6002759A6002759A6002759A6002759A6002759A6002759A6002759A6002759
      A5001F458100050B150000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001C407500558E
      C70096CBEF0091C9EE008CC7ED0088C5ED0088C5ED008EC8EE0096CBEF009ECF
      F000A3D2F100AAD5F200AFD8F300AED7F300ACD6F200A7D4F100A2D1F10097CC
      EF0091C9EE008AC5ED007FC0EB0077BCEA0070B9E90067B4E8005EB0E6005AAD
      E6004584C2001B3E720000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002451910092BE
      E200A4D3F20085C3EC0058AEE60055ACE50055ACE50055ACE50055ACE50055AC
      E50055ACE50055ACE50057ADE50078BDEA0078BDEA0057ADE50055ACE50055AC
      E50055ACE50055ACE50055ACE50055ACE50055ACE50055ACE50055ACE50065B3
      E80079B2DE002451910000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000142E52004981
      BE00B5DBF4007FC1EC005AAFE6005AAFE6005AAFE6005AAFE6005AAFE6005AAF
      E6005AAFE60063B3E700AAD3EE00778A9800778A9800AAD3EE0063B4E7005AAF
      E6005AAFE6005AAFE6005AAFE6005AAFE6005AAFE6005AAFE60060B2E70086C4
      ED00427FBD001939660000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000234F
      8900669ACB00B4DBF40068B6E8005FB2E7005FB2E7005FB2E7005FB2E7005FB2
      E7005FB2E70089C6ED006D79830039272000392720006D78820089C6ED005FB2
      E7005FB2E7005FB2E7005FB2E7005FB2E7005FB2E70061B3E70079BFEB00659C
      CE00275798000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002C64AA00A5CDEB00A0D1F20065B5E90065B5E90065B5E90065B5E90065B5
      E90065B5E90097CDF00066676C0049322A0049322A0066676C0097CDF00065B5
      E90065B5E90065B5E90065B5E90065B5E90065B5E90072BCEB0093C9EC002D66
      AD000C1B2E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000017345700508AC200C7E5F70084C5EE006AB9EA006AB9EA006AB9EA006AB9
      EA006AB9EA007CC1ED00A2C0D300655B5A00655A5A00A2C0D3007CC1ED006AB9
      EA006AB9EA006AB9EA006AB9EA006AB9EA0070BCEB0090CBEF00508BC2001B3E
      6700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000275891006EA2D000C8E5F70072BEEB0070BCEB0071BCEB0077BF
      EC007DC2ED0083C5EE009CD0F100B5DDF500B6DDF5009CD1F10083C5EE007DC2
      ED0077BFEC0071BCEB0070BCEB0072BDEB0085C6EE0069A1D1002A5F9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000306EB100B9DAF000ACD9F40085C7EE008ECCF00091CD
      F00091CDF00091CDF000B0DBF400777B8000777B8000B0DBF40091CDF00091CD
      F00091CDF0008ECCF00085C7EE0083C7EE009CCFEE003170B4000C1C2D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001A3A5C005C95C800E3F3FB00A0D4F20095CFF10095CF
      F10095CFF10095CFF100C1E3F7005348460053484600C1E3F70095CFF10095CF
      F10095CFF10095CFF10099D1F100AEDAF4005D96C9001E436900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002A60960082B1D800D7EDF9009BD3F1009AD2
      F1009AD2F1009AD2F100CBE8F8004C3832004C383200CBE8F8009AD2F1009AD2
      F1009AD2F1009BD3F100A9D9F3007EAFD8002D67A00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003579B700CFE5F500B6E0F600A0D5
      F300A0D5F300A5D7F400C2D8E5004E372F004E372F00C2D8E500A5D7F400A0D5
      F300A0D5F300A6D9F400B9DFF400367ABA000D1E2D0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001C406000639ECE00E3F3FB00A7D9
      F400A5D9F400AFDDF500B1C2CB00513A3200513A3200B1C2CB00AFDDF500A5D9
      F400A7D9F400B8E1F600649FCE001F486B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002F6B9E008ABADC00CFEB
      F900A9DCF500B8E1F700A1ABB100543C3400543C3400A1ABB100B8E1F700A9DC
      F500B4E0F60084B6DB00316FA400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003984BF00D1E9
      F600B6E2F600C0E6F70090949700563E3600563E360091959800C0E6F700B2E0
      F600BFE2F4003984C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001F47670069A6
      D200D8EFFB00C5E8F800989C9F005840380058403800989C9F00C5E8F800C1E6
      F90069A5D200214C6D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003273
      A30090BFE000C8EAF900D9EFF800B2BBBF00B2BBBF00D9EFF800C0E7F8008ABD
      DF003477A8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003D8DC500CDE9F700BFE7F900C5EAFA00C5EAFA00C0E7F900C5E6F6003D8D
      C600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000214C69006EACD600D2F0FB00C1E9F900C1E9F900CBEDFA006CACD600224F
      6D00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000367CA90093C5E300CBEDFA00CBEDFA0090C3E200367DAB000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004195CB00DBF3FB00DAF3FB004195CB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000285B7B0082BADD007DB7DC0026577600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000214C65001F475F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000080000000200000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF000000000000000000000000
      FFFFFFFF000000000000000000000000FFFFFFFF000000000000000000000000
      FFFFFFFF000000000000000000000000FFFFFFFF000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      E0000007000000000000000000000000F0000007000000000000000000000000
      F000000F000000000000000000000000F800001F000000000000000000000000
      FC00001F000000000000000000000000FC00003F000000000000000000000000
      FE00007F000000000000000000000000FF00007F000000000000000000000000
      FF0000FF000000000000000000000000FF8001FF000000000000000000000000
      FFC003FF000000000000000000000000FFC003FF000000000000000000000000
      FFE007FF000000000000000000000000FFF00FFF000000000000000000000000
      FFF00FFF000000000000000000000000FFF81FFF000000000000000000000000
      FFFC3FFF000000000000000000000000FFFC3FFF000000000000000000000000
      FFFE7FFF000000000000000000000000FFFFFFFF000000000000000000000000
      FFFFFFFF000000000000000000000000FFFFFFFF000000000000000000000000
      FFFFFFFF00000000000000000000000000000000000000000000000000000000
      000000000000}
  end
end
