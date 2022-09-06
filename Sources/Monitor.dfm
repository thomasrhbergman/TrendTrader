object frmMonitor: TfrmMonitor
  Left = 0
  Top = 0
  Width = 1107
  Height = 658
  AutoScroll = True
  Caption = 'Trend-Trader'
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 1100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTopOld: TPanel
    Left = 0
    Top = 0
    Width = 1091
    Height = 93
    Align = alTop
    TabOrder = 0
    Visible = False
    object gbConnectOld: TGroupBox
      Left = 1
      Top = 1
      Width = 50
      Height = 91
      Align = alLeft
      Caption = 'Connect'
      TabOrder = 0
      object btnLoginOld: TBitBtn
        Left = 7
        Top = 15
        Width = 36
        Height = 36
        ParentCustomHint = False
        Action = aLogin
        Images = DMImage.vil32
        Margin = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object btnRestartOld: TBitBtn
        Left = 7
        Top = 51
        Width = 36
        Height = 36
        Action = aRestart
        Images = DMImage.vil32
        TabOrder = 1
      end
    end
    object gbExplore: TGroupBox
      Left = 51
      Top = 1
      Width = 50
      Height = 91
      Align = alLeft
      Caption = 'Explore'
      TabOrder = 1
      object btnSearch: TBitBtn
        Left = 6
        Top = 15
        Width = 36
        Height = 36
        Action = aSearch
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object btnShowContractInspector: TBitBtn
        Left = 6
        Top = 52
        Width = 36
        Height = 36
        Action = aShowContractInspector
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
    end
    object gbExecute: TGroupBox
      Left = 101
      Top = 1
      Width = 296
      Height = 91
      Align = alLeft
      Caption = 'Execute'
      Color = clBtnFace
      ParentBackground = False
      ParentColor = False
      TabOrder = 2
      object lblAutoTrades: TLabel
        Left = 2
        Top = 50
        Width = 60
        Height = 13
        Alignment = taRightJustify
        Caption = 'AutoTrades:'
      end
      object lblQualifiers: TLabel
        Left = 2
        Top = 13
        Width = 49
        Height = 13
        Alignment = taRightJustify
        Caption = 'Qualifiers:'
      end
      object btnAutoTradesExecute: TBitBtn
        Left = 248
        Top = 64
        Width = 24
        Height = 24
        Action = aAutoTradesExecute
        Images = DMImage.vil16
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object cbAutoTrades: TDBLookupComboBox
        Left = 2
        Top = 65
        Width = 246
        Height = 21
        DataField = 'AUTOTRADESID'
        DataSource = DMod.dsAccounts
        DropDownWidth = 370
        KeyField = 'ID'
        ListField = 'NAME;ID'
        ListSource = DMod.dsAutoTrades
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnCloseUp = cbAutoTradesCloseUp
      end
      object btnQualifiersExecute: TBitBtn
        Left = 248
        Top = 26
        Width = 24
        Height = 24
        Action = aQualifiersExecute
        Images = DMImage.vil16
        TabOrder = 2
      end
      object cbQualifiers: TDBLookupComboBox
        Left = 2
        Top = 27
        Width = 246
        Height = 21
        DropDownWidth = 320
        KeyField = 'ID'
        ListField = 'NAME;ID'
        ListSource = DMod.dsQualifiers
        TabOrder = 3
      end
      object btnShowQualifiers: TBitBtn
        Left = 271
        Top = 26
        Width = 24
        Height = 24
        ParentCustomHint = False
        Action = aShowQualifiers
        Images = DMImage.vil16
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object btnShowAutoTrades: TBitBtn
        Left = 271
        Top = 64
        Width = 24
        Height = 24
        ParentCustomHint = False
        Action = aShowAutoTrades
        Images = DMImage.vil16
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
      end
    end
    object gbService: TGroupBox
      Left = 944
      Top = 1
      Width = 146
      Height = 91
      Align = alRight
      Caption = 'Service'
      TabOrder = 3
      object lblIPAddress: TLabel
        Left = 201
        Top = 87
        Width = 3
        Height = 13
      end
      object ChB_HideFilled: TCheckBox
        Left = 6
        Top = 14
        Width = 95
        Height = 25
        BiDiMode = bdLeftToRight
        Caption = 'Hide filled order'
        ParentBiDiMode = False
        TabOrder = 0
        WordWrap = True
      end
      object btnTWSTime: TButton
        Left = 6
        Top = 36
        Width = 100
        Height = 24
        Caption = 'Get TWS time'
        TabOrder = 1
        OnClick = btnTWSTimeClick
      end
      object btnActivateAllOld: TBitBtn
        Left = 107
        Top = 13
        Width = 36
        Height = 36
        Action = aActivateAll
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
      object btnStartHTTPServer: TButton
        Left = 6
        Top = 61
        Width = 100
        Height = 24
        Caption = 'Start HTTP Server'
        TabOrder = 3
        OnClick = btnStartHTTPServerClick
      end
      object btnShowDatabaseProperties: TBitBtn
        Left = 107
        Top = 50
        Width = 36
        Height = 36
        Action = aShowDatabaseProperties
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
    end
    object pnlActivityLog: TPanel
      Left = 397
      Top = 1
      Width = 437
      Height = 91
      Align = alClient
      BevelOuter = bvNone
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 4
      inline frameActivityLog: TframeActivityLog
        Left = 0
        Top = 0
        Width = 437
        Height = 91
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 437
        ExplicitHeight = 91
        inherited pnlActivityLog: TPanel
          Width = 437
          Height = 91
          ExplicitWidth = 437
          ExplicitHeight = 91
          inherited vstTree: TVirtualStringTree
            Width = 435
            Height = 67
            ExplicitWidth = 435
            ExplicitHeight = 67
            Columns = <
              item
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
                Position = 0
                Spacing = 0
                Text = 'Symbol'
                Width = 42
              end
              item
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
                Position = 1
                Text = 'Status'
                Width = 30
              end
              item
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
                Position = 2
                Text = 'Action'
                Width = 52
              end
              item
                Alignment = taRightJustify
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
                Position = 3
                Text = 'Quantity'
                Width = 49
              end
              item
                Alignment = taRightJustify
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
                Position = 4
                Text = 'Filled'
                Width = 45
              end
              item
                Position = 5
                Text = 'Price'
              end
              item
                Alignment = taRightJustify
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
                Position = 6
                Text = 'IBID'
                Width = 47
              end
              item
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
                Position = 7
                Text = 'Time'
                Width = 53
              end
              item
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
                Position = 8
                Text = 'Info'
              end>
          end
          inherited pnlAvailableFilters: TPanel
            Width = 435
            ExplicitWidth = 435
            inherited edtAvailableFilters: TEdit
              Width = 221
              Height = 19
              ExplicitWidth = 221
              ExplicitHeight = 19
            end
            inherited btnClearAvailableFilters: TBitBtn
              Left = 308
              ImageIndex = 43
              ImageName = 'RemovePivotField_32x32'
              Images = DMImage.vil16
              Glyph.Data = {00000000}
              ExplicitLeft = 308
            end
            inherited btnClear: TBitBtn
              Left = 330
              ExplicitLeft = 330
            end
          end
        end
      end
    end
    object GroupBox1: TGroupBox
      Left = 834
      Top = 1
      Width = 110
      Height = 91
      Align = alRight
      Caption = 'Account'
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 5
      object lblAcountRealisedCaption: TLabel
        Left = 2
        Top = 38
        Width = 40
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'R PnL:'
      end
      object lblAcountUnrealisedCaption: TLabel
        Left = 2
        Top = 56
        Width = 40
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'U PnL:'
      end
      object lblAcountRealised: TLabel
        Left = 44
        Top = 38
        Width = 64
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object lblAcountUnrealised: TLabel
        Left = 44
        Top = 56
        Width = 64
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object lblAccountBalansCaption: TLabel
        Left = 2
        Top = 73
        Width = 40
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Balans:'
      end
      object lblAccountBalans: TLabel
        Left = 44
        Top = 73
        Width = 64
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '0'
      end
      object cbAccounts: TComboBox
        Left = 4
        Top = 15
        Width = 103
        Height = 21
        TabOrder = 0
        OnChange = cbAccountsChange
      end
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 93
    Width = 1091
    Height = 485
    Align = alClient
    TabOrder = 1
    object pgcMain: TPageControl
      Left = 1
      Top = 60
      Width = 1089
      Height = 424
      ActivePage = tsOrders
      Align = alClient
      DockSite = True
      TabOrder = 0
      OnChange = pgcMainChange
      object tsOrders: TTabSheet
        Caption = 'Orders'
        object vstMonitor: TVirtualStringTree
          Left = 0
          Top = 38
          Width = 1081
          Height = 358
          Align = alClient
          BiDiMode = bdLeftToRight
          Colors.BorderColor = 14540253
          Colors.DisabledColor = clSilver
          Colors.FocusedSelectionColor = clSkyBlue
          Colors.GridLineColor = clSilver
          Colors.SelectionTextColor = 14540253
          Colors.TreeLineColor = 16250871
          Ctl3D = False
          CustomCheckImages = DMImage.ilCustomCheckImages
          DefaultNodeHeight = 20
          DragHeight = 400
          DragMode = dmAutomatic
          DragOperations = [doCopy, doMove, doLink]
          DragType = dtVCL
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          Header.AutoSizeIndex = -1
          Header.Height = 20
          Header.Options = [hoColumnResize, hoDblClickResize, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
          HintMode = hmHint
          Images = DMImage.vilDocumentState
          LineStyle = lsSolid
          ParentBiDiMode = False
          ParentCtl3D = False
          ParentFont = False
          ParentShowHint = False
          PopupMenu = pmMainTree
          ScrollBarOptions.AlwaysVisible = True
          ScrollBarOptions.VerticalIncrement = 40
          ShowHint = True
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoTristateTracking, toAutoChangeScale]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
          TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware]
          TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus]
          OnBeforeCellPaint = vstMonitorBeforeCellPaint
          OnChecking = vstMonitorChecking
          OnCompareNodes = vstMonitorCompareNodes
          OnDblClick = vstMonitorDblClick
          OnDragAllowed = vstMonitorDragAllowed
          OnDragOver = vstMonitorDragOver
          OnDragDrop = vstMonitorDragDrop
          OnDrawText = vstMonitorDrawText
          OnFreeNode = vstMonitorFreeNode
          OnGetText = vstMonitorGetText
          OnGetImageIndex = vstMonitorGetImageIndex
          OnGetHint = vstMonitorGetHint
          OnInitNode = vstMonitorInitNode
          OnKeyDown = vstMonitorKeyDown
          OnMeasureItem = vstMonitorMeasureItem
          OnStructureChange = vstMonitorStructureChange
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              CaptionAlignment = taCenter
              Color = clHighlightText
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus, coUseCaptionAlignment]
              Position = 0
              Text = 'Display Item'
              Width = 323
            end
            item
              Alignment = taRightJustify
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 1
              Text = 'Node ID'
              Width = 60
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus, coUseCaptionAlignment]
              Position = 2
              Text = 'Calc Type'
              Width = 80
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus, coUseCaptionAlignment, coEditable]
              Position = 3
              Text = 'Calc factor'
              Width = 70
            end
            item
              CaptionAlignment = taCenter
              Color = clWindow
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus, coUseCaptionAlignment]
              Position = 4
              Text = 'Value'
              Width = 218
            end
            item
              Alignment = taRightJustify
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
              Position = 5
              Text = '% Last/Close'
              Width = 521
            end>
        end
        object pnlOptions: TPanel
          Left = 0
          Top = 0
          Width = 1081
          Height = 38
          Align = alTop
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 1
          DesignSize = (
            1081
            38)
          object btnExportToExcel: TBitBtn
            Left = 1045
            Top = 0
            Width = 36
            Height = 36
            ParentCustomHint = False
            Action = aExportToExcel
            Anchors = [akTop, akRight]
            Images = DMImage.vil32
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
          object btnExportToCSV: TBitBtn
            Left = 1009
            Top = 0
            Width = 36
            Height = 36
            ParentCustomHint = False
            Action = aExportToCSV
            Anchors = [akTop, akRight]
            Images = DMImage.vil32
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
          end
          object btnPrint: TBitBtn
            Left = 973
            Top = 0
            Width = 36
            Height = 36
            ParentCustomHint = False
            Action = aPrint
            Anchors = [akTop, akRight]
            Images = DMImage.vil32
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
          end
          object btnSaveGroup: TBitBtn
            Left = 315
            Top = 0
            Width = 36
            Height = 36
            Action = aSaveGroupSet
            Images = DMImage.vil32
            TabOrder = 3
          end
          object btnOpenGroup: TBitBtn
            Left = 279
            Top = 0
            Width = 36
            Height = 36
            ParentCustomHint = False
            Action = aOpenGroupSet
            Images = DMImage.vil32
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
          end
          object edCurrentGroupName: TEdit
            Left = 40
            Top = 7
            Width = 236
            Height = 21
            TabOrder = 5
          end
          object btnColumnSettings: TBitBtn
            Left = 937
            Top = 0
            Width = 36
            Height = 36
            Action = aColumnSettings
            Anchors = [akTop, akRight]
            Images = DMImage.vil32
            Margin = 0
            Spacing = 2
            TabOrder = 6
          end
          object btnShowCalColumns: TBitBtn
            Left = 0
            Top = 0
            Width = 36
            Height = 36
            Action = aShowCalColumns
            Images = DMImage.vil32
            ParentShowHint = False
            ShowHint = True
            TabOrder = 7
          end
          object btnSaveGroupAs: TBitBtn
            Left = 351
            Top = 0
            Width = 36
            Height = 36
            ParentCustomHint = False
            Action = aSaveGroupSetAs
            Images = DMImage.vil32
            ParentShowHint = False
            ShowHint = True
            TabOrder = 8
          end
          object btnRegularRunQualifier: TBitBtn
            Left = 393
            Top = 0
            Width = 36
            Height = 36
            ParentCustomHint = False
            Action = aRegularRunQualifier
            Images = DMImage.vil32
            ParentShowHint = False
            ShowHint = True
            TabOrder = 9
          end
        end
      end
    end
    object pnlTop: TPanel
      Left = 1
      Top = 1
      Width = 1089
      Height = 59
      Align = alTop
      TabOrder = 1
      object gbConnect: TGroupBox
        Left = 1
        Top = 1
        Width = 136
        Height = 57
        Align = alLeft
        Caption = 'Connect'
        TabOrder = 0
        object btnLogin: TBitBtn
          Left = 7
          Top = 15
          Width = 36
          Height = 36
          ParentCustomHint = False
          Action = aLogin
          Images = DMImage.vil32
          Margin = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object btnRestart: TBitBtn
          Left = 49
          Top = 15
          Width = 36
          Height = 36
          Action = aRestart
          Images = DMImage.vil32
          TabOrder = 1
        end
        object btnActivateAll: TBitBtn
          Left = 91
          Top = 15
          Width = 36
          Height = 36
          Action = aActivateAll
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
      end
      object btnAutoTrades: TBitBtn
        Left = 180
        Top = 16
        Width = 80
        Height = 36
        Caption = 'AutoTrades'
        TabOrder = 1
        OnClick = btnAutoTradesClick
      end
      object btnQualifiers: TBitBtn
        Left = 270
        Top = 16
        Width = 80
        Height = 36
        Caption = 'Qualifiers'
        TabOrder = 2
        OnClick = btnQualifiersClick
      end
      object btnCandidatesProcess: TBitBtn
        Left = 360
        Top = 16
        Width = 80
        Height = 36
        Caption = 'Candidates process'
        TabOrder = 3
        WordWrap = True
        OnClick = btnCandidatesProcessClick
      end
      object btnQuantities: TBitBtn
        Left = 450
        Top = 16
        Width = 80
        Height = 36
        Caption = 'Quantities'
        TabOrder = 4
        WordWrap = True
        OnClick = btnQuantitiesClick
      end
      object btnOrderTemplates: TBitBtn
        Left = 540
        Top = 16
        Width = 80
        Height = 36
        Caption = 'Order templates'
        TabOrder = 5
        WordWrap = True
        OnClick = btnOrderTemplatesClick
      end
    end
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 578
    Width = 1091
    Height = 21
    DoubleBuffered = True
    Panels = <
      item
        Style = psOwnerDraw
        Text = 'IB Connected'
        Width = 80
      end
      item
        Style = psOwnerDraw
        Text = 'Recieve data'
        Width = 80
      end
      item
        Style = psOwnerDraw
        Width = 20
      end
      item
        Style = psOwnerDraw
        Text = 'NN Connected'
        Width = 90
      end
      item
        Style = psOwnerDraw
        Text = 'Recieve data'
        Width = 80
      end
      item
        Bevel = pbRaised
        Style = psOwnerDraw
        Text = 'O'
        Width = 20
      end
      item
        Bevel = pbRaised
        Style = psOwnerDraw
        Text = 'Line '
        Width = 60
      end
      item
        Alignment = taCenter
        Style = psOwnerDraw
        Width = 150
      end
      item
        Alignment = taRightJustify
        Text = 'Requests Queue: '
        Width = 100
      end
      item
        Alignment = taRightJustify
        Text = '0'
        Width = 50
      end
      item
        Width = 300
      end>
    ParentDoubleBuffered = False
    OnClick = sbMainClick
    OnDrawPanel = sbMainDrawPanel
  end
  object TimerTimeGap: TTimer
    Enabled = False
    Interval = 500
    OnTimer = OnConditionTimeGap
    Left = 456
    Top = 342
  end
  object pmMainTree: TPopupMenu
    OnPopup = pmMainTreePopup
    Left = 184
    Top = 319
    object miConditionFactor: TMenuItem
      Action = aShowEditDialog
      Caption = 'Enter/Modify'
      Default = True
    end
    object miShowTradeChart: TMenuItem
      Action = aShowTradeChart
      Caption = 'Chart'
    end
    object miShowConditionChart: TMenuItem
      Action = aShowConditionAlgosChart
    end
    object miDeleteNode: TMenuItem
      Action = aDeleteNode
    end
    object miDeleteAllNodes: TMenuItem
      Action = aDeleteAllNodes
    end
    object miDelimiter1: TMenuItem
      Caption = '-'
    end
    object miAddOrderGroup: TMenuItem
      Action = aAddOrderGroup
    end
    object miAddCondition: TMenuItem
      Action = aAddCondition
    end
    object miAddAlgos: TMenuItem
      Action = aAddAlgos
      ShortCut = 16449
    end
    object miAddFactor: TMenuItem
      Action = aAddFactor
    end
    object miDuplicateAlgos: TMenuItem
      Action = aDuplicateAlgos
    end
    object miDuplicateCondition: TMenuItem
      Action = aDuplicateCondition
    end
    object miDuplicateOrder: TMenuItem
      Action = aDuplicateOrder
    end
    object miGetInfo: TMenuItem
      Action = aShowInformationDialog
    end
    object miShowPriceHistory: TMenuItem
      Action = aShowPriceHistory
    end
    object miShowRuleInformationDialog: TMenuItem
      Action = aShowRuleInformationDialog
    end
    object miDelimiter2: TMenuItem
      Caption = '-'
    end
    object miActivateOrders: TMenuItem
      Action = aActivateOrders
    end
    object miInactivateOrders: TMenuItem
      Action = aInactivateOrders
    end
    object miOpenRelation: TMenuItem
      Action = aOpenRelation
    end
    object miTransformToOrders: TMenuItem
      Caption = 'Transform to order'
      object miTransformToOrder: TMenuItem
        Action = aTransformToOrder
      end
      object miTransformToBaseOrder: TMenuItem
        Action = aTransformToBaseOrder
      end
    end
    object miMakeRepetitiveOrder: TMenuItem
      Action = aMakeRepetitiveOrder
    end
    object miCancelBrokerOrder: TMenuItem
      Action = aCancelBrokerOrder
    end
    object miOrderStatus: TMenuItem
      Action = aShowOrderStatus
    end
    object miDelimiter3: TMenuItem
      Caption = '-'
    end
    object miUnhideAll: TMenuItem
      Action = aUnhideAll
    end
    object miUnhideLastNode: TMenuItem
      Action = aUnhideLastNode
    end
    object miHideNode: TMenuItem
      Action = aHideNode
    end
    object miDelimiter4: TMenuItem
      Caption = '-'
    end
    object miSaveGroupAs1: TMenuItem
      Action = aSaveGroupSetAs
      Caption = 'Save As New Order Groups...'
    end
    object miExpandTree: TMenuItem
      Action = aExpandTree
    end
    object miCollapsTree: TMenuItem
      Action = aCollapsTree
    end
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 184
    Top = 264
    object aActivateOrders: TAction
      Caption = 'Activate all orders'
      ShortCut = 8257
      OnExecute = aActivateOrdersExecute
      OnUpdate = aActivateOrdersUpdate
    end
    object aAddAlgos: TAction
      Caption = 'Add new algos'
      OnExecute = aAddAlgosExecute
      OnUpdate = aAddAlgosUpdate
    end
    object aAddCondition: TAction
      Caption = 'Add new condition'
      ShortCut = 16451
      OnExecute = aAddConditionExecute
      OnUpdate = aAddConditionUpdate
    end
    object aAddFactor: TAction
      Caption = 'Add new factor'
      ShortCut = 16454
      OnExecute = aAddFactorExecute
      OnUpdate = aAddFactorUpdate
    end
    object aAddOrderGroup: TAction
      Caption = 'Add new order group'
      ShortCut = 16455
      OnExecute = aAddOrderGroupExecute
      OnUpdate = aAddOrderGroupUpdate
    end
    object aCancelBrokerOrder: TAction
      Caption = 'Cancel broker order'
      Enabled = False
      ShortCut = 16392
      Visible = False
      OnExecute = aCancelBrokerOrderExecute
      OnUpdate = aCancelBrokerOrderUpdate
    end
    object aOpenCheckIBInstruments: TAction
      Category = 'Open Dialog'
      Caption = 'IB Instruments'
      OnExecute = aOpenCheckIBInstrumentsExecute
    end
    object aClose: TAction
      Caption = 'Close'
      OnExecute = aCloseExecute
    end
    object aColumnSettings: TAction
      Hint = 'Column settings'
      ImageIndex = 36
      ImageName = 'ListBullets_32x32'
      OnExecute = aColumnSettingsExecute
    end
    object aConnect: TAction
      Caption = 'Connect'
      OnExecute = aConnectExecute
    end
    object aDeleteAllNodes: TAction
      Caption = 'Delete all nodes'
      ShortCut = 8238
      OnExecute = aDeleteAllNodesExecute
      OnUpdate = aDeleteAllNodesUpdate
    end
    object aDeleteNode: TAction
      Caption = 'Delete selected node'
      ShortCut = 46
      OnExecute = aDeleteNodeExecute
      OnUpdate = aDeleteNodeUpdate
    end
    object aDisconnect: TAction
      Caption = 'Disconnect'
      OnExecute = aDisconnectExecute
    end
    object aDuplicateAlgos: TAction
      Caption = 'Duplicate algos'
      OnExecute = aDuplicateAlgosExecute
      OnUpdate = aDuplicateAlgosUpdate
    end
    object aDuplicateCondition: TAction
      Caption = 'Duplicate condition'
      OnExecute = aDuplicateConditionExecute
      OnUpdate = aDuplicateConditionUpdate
    end
    object aDuplicateOrder: TAction
      Caption = 'Duplicate order'
      ShortCut = 16452
      OnExecute = aDuplicateOrderExecute
      OnUpdate = aDuplicateOrderUpdate
    end
    object aHideNode: TAction
      Caption = 'Hide selected node'
      OnExecute = aHideNodeExecute
      OnUpdate = aHideNodeUpdate
    end
    object aInactivateOrders: TAction
      Caption = 'Inactivate all orders'
      ShortCut = 8260
      OnExecute = aInactivateOrdersExecute
      OnUpdate = aInactivateOrdersUpdate
    end
    object aMakeRepetitiveOrder: TAction
      Caption = 'Make repetitive'
      Hint = 'Double click - repetitive'
      ShortCut = 16466
      OnExecute = aMakeRepetitiveOrderExecute
      OnUpdate = aMakeRepetitiveOrderUpdate
    end
    object aOpenGroupSet: TAction
      Category = 'Open Dialog'
      ImageIndex = 5
      ImageName = 'Open_32x32'
      OnExecute = aOpenGroupSetExecute
      OnUpdate = aOpenGroupSetUpdate
    end
    object aOpenIBCommand: TAction
      Caption = 'Open IB command'
      OnExecute = aOpenIBCommandExecute
    end
    object aOpenLogFile: TAction
      Caption = 'Open log file'
      OnExecute = aOpenLogFileExecute
      OnUpdate = aOpenLogFileUpdate
    end
    object aOpenRelation: TAction
      Caption = 'Open relation'
      OnExecute = aOpenRelationExecute
      OnUpdate = aOpenRelationUpdate
    end
    object aRenameGroup: TAction
      Caption = 'Rename group'
      OnExecute = aRenameGroupExecute
      OnUpdate = aRenameGroupUpdate
    end
    object aRequestToIBByHistoricalData: TAction
      Category = 'Data'
      Caption = 'View Realtime and Historical Data && Feed'
      OnExecute = aRequestToIBByHistoricalDataExecute
      OnUpdate = aRequestToIBByHistoricalDataUpdate
    end
    object aSaveGroupSet: TAction
      Category = 'Open Dialog'
      ImageIndex = 10
      ImageName = 'Save_32x32'
      OnExecute = aSaveGroupSetExecute
      OnUpdate = aSaveGroupSetUpdate
    end
    object aSaveGroupSetAs: TAction
      Category = 'Open Dialog'
      Hint = 'Save as New Order Groups'
      ImageIndex = 26
      ImageName = 'SaveAll_32x32'
      OnExecute = aSaveGroupSetAsExecute
      OnUpdate = aSaveGroupSetAsUpdate
    end
    object aSearch: TAction
      Hint = 'Open Search Form'
      ImageIndex = 21
      ImageName = 'Zoom_32x32'
      ShortCut = 32838
      OnExecute = aSearchExecute
      OnUpdate = aSearchUpdate
    end
    object aShowConditionChart: TAction
      Category = 'Show Dialog'
      Caption = 'Chart'
      OnExecute = aShowConditionChartExecute
      OnUpdate = aShowConditionChartUpdate
    end
    object aShowConditionAlgosChart: TAction
      Category = 'Show Dialog'
      Caption = 'Condition chart'
      OnExecute = aShowConditionAlgosChartExecute
      OnUpdate = aShowConditionAlgosChartUpdate
    end
    object aShowEditDialog: TAction
      Category = 'Show Dialog'
      Caption = 'Show edit dialog'
      ShortCut = 16461
      OnExecute = aShowEditDialogExecute
      OnUpdate = aShowEditDialogUpdate
    end
    object aShowMonitorConditionChart: TAction
      Category = 'Show Dialog'
      Caption = 'Realtime chart'
      OnExecute = aShowMonitorConditionChartExecute
      OnUpdate = aShowMonitorConditionChartUpdate
    end
    object aShowGlobalParameters: TAction
      Category = 'Show Dialog'
      Caption = 'Show parameters'
      OnExecute = aShowGlobalParametersExecute
    end
    object aShowOrderStatus: TAction
      Category = 'Show Dialog'
      Caption = 'Order status'
      ShortCut = 16467
      OnExecute = aShowOrderStatusExecute
      OnUpdate = aShowOrderStatusUpdate
    end
    object aShowSumAlgos: TAction
      Category = 'Show Dialog'
      AutoCheck = True
      Caption = 'Show sum for algos in column broker'
      OnExecute = aShowSumAlgosExecute
    end
    object aPrint: TAction
      Category = 'Service'
      Hint = 'Print'
      ImageIndex = 15
      ImageName = 'Print_32x32'
      OnExecute = aPrintExecute
    end
    object aShowSumCondition: TAction
      Category = 'Show Dialog'
      AutoCheck = True
      Caption = 'Show sum for condition in column broker'
      OnExecute = aShowSumConditionExecute
    end
    object aShowTradeChart: TAction
      Category = 'Show Dialog'
      Caption = 'Open'
      ShortCut = 32836
      OnExecute = aShowTradeChartExecute
      OnUpdate = aShowTradeChartUpdate
    end
    object aTransformToBaseOrder: TAction
      Caption = 'Transform to Instrument Template'
      OnExecute = aTransformToBaseOrderExecute
      OnUpdate = aTransformToBaseOrderUpdate
    end
    object aTransformToOrder: TAction
      Caption = 'Transform to order'
      ShortCut = 16463
      OnExecute = aTransformToOrderExecute
      OnUpdate = aTransformToOrderUpdate
    end
    object aUnhideAll: TAction
      Caption = 'Unhide all nodes'
      OnExecute = aUnhideAllExecute
      OnUpdate = aUnhideAllUpdate
    end
    object aUnhideLastNode: TAction
      Caption = 'Unhide last node'
      OnExecute = aUnhideLastNodeExecute
      OnUpdate = aUnhideLastNodeUpdate
    end
    object aUseIBfeeds: TAction
      AutoCheck = True
      Caption = 'Use for NN instruments IB feeds'
      OnExecute = aUseIBfeedsExecute
    end
    object aShowExchangeRateInfo: TAction
      Category = 'Reports'
      Caption = 'Show Exchange Rate'
      OnExecute = aShowExchangeRateInfoExecute
    end
    object aExpandTree: TAction
      Caption = 'Expand Tree'
      ShortCut = 16453
      OnExecute = aExpandTreeExecute
    end
    object aCollapsTree: TAction
      Caption = 'Collaps Tree'
      ShortCut = 49219
      OnExecute = aCollapsTreeExecute
    end
    object aUseNN: TAction
      AutoCheck = True
      Caption = 'Use NordNet'
      Checked = True
      OnExecute = aUseNNExecute
    end
    object aUseIB: TAction
      AutoCheck = True
      Caption = 'Use Interactive Brokers'
      Checked = True
      OnExecute = aUseIBExecute
    end
    object aAutoTradesExecute: TAction
      Hint = 'Execute AutoTrades'
      ImageIndex = 12
      ImageName = 'lightning'
      OnExecute = aAutoTradesExecuteExecute
      OnUpdate = aAutoTradesExecuteUpdate
    end
    object aQualifiersExecute: TAction
      Hint = 'Qualifiers Execute'
      ImageIndex = 12
      ImageName = 'lightning'
      OnExecute = aQualifiersExecuteExecute
      OnUpdate = aQualifiersExecuteUpdate
    end
    object aShowQualifiers: TAction
      Category = 'Show Dialog'
      ImageIndex = 5
      ImageName = 'Open_32x32'
      OnExecute = aShowQualifiersExecute
    end
    object aShowInformationDialog: TAction
      Category = 'Show Dialog'
      Caption = 'Get Info'
      ShortCut = 16457
      OnExecute = aShowInformationDialogExecute
    end
    object aShowPriceInfo: TAction
      Category = 'Reports'
      Caption = 'Show Price Information'
      OnExecute = aShowPriceInfoExecute
    end
    object aShowRuleInformationDialog: TAction
      Category = 'Show Dialog'
      Caption = 'Show Rule Information'
      OnExecute = aShowRuleInformationDialogExecute
      OnUpdate = aShowRuleInformationDialogUpdate
    end
    object aShowInstrumentsWithoutFeedInfo: TAction
      Category = 'Reports'
      Caption = 'Show Instruments Without Feed'
      OnExecute = aShowInstrumentsWithoutFeedInfoExecute
    end
    object aShowTickByTick: TAction
      Category = 'Data'
      Caption = 'Tick By Tick'
      OnExecute = aShowTickByTickExecute
    end
    object aRestart: TAction
      Category = 'Connect'
      Hint = 'Restart'
      ImageIndex = 44
      ImageName = 'Refresh2_32x32'
      OnExecute = aRestartExecute
    end
    object aLogin: TAction
      Category = 'Connect'
      Hint = 'Login'
      ImageIndex = 54
      ImageName = 'bullet_green'
      OnExecute = aLoginExecute
    end
    object aShowPriceHistory: TAction
      Category = 'Show Dialog'
      Caption = 'Show Price History'
      ShortCut = 16456
      OnExecute = aShowPriceHistoryExecute
      OnUpdate = aShowPriceHistoryUpdate
    end
    object aShowContractInspector: TAction
      Category = 'Show Dialog'
      Hint = 'Open Contract Inspector'
      ImageIndex = 22
      ImageName = 'ContractInspector'
      OnExecute = aShowContractInspectorExecute
    end
    object aShowAutoTrades: TAction
      Category = 'Show Dialog'
      ImageIndex = 5
      ImageName = 'Open_32x32'
      OnExecute = aShowAutoTradesExecute
    end
    object aShowDatabaseProperties: TAction
      Category = 'Show Dialog'
      Hint = 'Database Properties'
      ImageIndex = 45
      ImageName = 'EditDataSource_32x32'
      OnExecute = aShowDatabasePropertiesExecute
    end
    object aShowCalColumns: TAction
      Category = 'Service'
      Hint = 'Show/Hide calculation columns'
      ImageIndex = 18
      ImageName = 'ReviewingPane_32x32'
      OnExecute = aShowCalColumnsExecute
    end
    object aActivateAll: TAction
      Category = 'Service'
      Hint = 'Activate All'
      ImageIndex = 12
      ImageName = 'lightning'
      OnExecute = aActivateAllExecute
    end
    object aExportToCSV: TAction
      Category = 'Service'
      Hint = 'Export To CSV'
      ImageIndex = 17
      ImageName = 'ExportToCSV_32x32'
      OnExecute = aExportToCSVExecute
    end
    object aExportToExcel: TAction
      Category = 'Service'
      Hint = 'Export To Excel'
      ImageIndex = 16
      ImageName = 'ExportToXLS_32x32'
      OnExecute = aExportToExcelExecute
    end
    object aShowCalculationStageInfo: TAction
      Category = 'Reports'
      Caption = 'Show Calculation Stage Info'
      OnExecute = aShowCalculationStageInfoExecute
    end
    object aShowOrdersTimestampInfo: TAction
      Category = 'Reports'
      Caption = 'Show Orders Timestamp'
      OnExecute = aShowOrdersTimestampInfoExecute
    end
    object aCheckIBInstruments: TAction
      Category = 'Data'
      Caption = 'Check Instruments'
      OnExecute = aCheckIBInstrumentsExecute
    end
    object aCheckLastPrice: TAction
      Category = 'Data'
      Caption = 'Check LastPrice'
      OnExecute = aCheckLastPriceExecute
    end
    object aShowSubscribersInfo: TAction
      Category = 'Reports'
      Caption = 'Show Subscribers Information'
      OnExecute = aShowSubscribersInfoExecute
    end
    object aShowPrecautionarySettingsInfo: TAction
      Category = 'Reports'
      Caption = 'Show Precautionary Settings'
      OnExecute = aShowPrecautionarySettingsInfoExecute
    end
    object aRegularRunQualifier: TAction
      Category = 'Service'
      Hint = 'Temporary set in unit xx line...'
      ImageIndex = 69
      ImageName = 'clock_play'
      OnExecute = aRegularRunQualifierExecute
      OnUpdate = aQualifiersExecuteUpdate
    end
  end
  object MainMenu: TMainMenu
    Left = 182
    Top = 386
    object miAction: TMenuItem
      Caption = 'Action'
      object miConnect: TMenuItem
        Action = aConnect
      end
      object miDisconnect: TMenuItem
        Action = aDisconnect
      end
      object miSep05: TMenuItem
        Caption = '-'
      end
      object miUseIB: TMenuItem
        Action = aUseIB
        AutoCheck = True
      end
      object miUseNN: TMenuItem
        Action = aUseNN
        AutoCheck = True
      end
      object miSep01: TMenuItem
        Caption = '-'
      end
      object miOpenIBCommand: TMenuItem
        Action = aOpenIBCommand
      end
      object miUseIBfeeds: TMenuItem
        Action = aUseIBfeeds
        AutoCheck = True
      end
      object miSep02: TMenuItem
        Caption = '-'
      end
      object miShowGlobalParameters: TMenuItem
        Action = aShowGlobalParameters
      end
      object miOpenLogFile: TMenuItem
        Action = aOpenLogFile
      end
      object miSep03: TMenuItem
        Caption = '-'
      end
      object miClose: TMenuItem
        Action = aClose
      end
    end
    object miTree: TMenuItem
      Caption = 'Tree'
      object miSearch: TMenuItem
        Action = aSearch
        Caption = 'Search'
      end
      object miColumnSettings: TMenuItem
        Action = aColumnSettings
        Caption = 'Column settings'
      end
      object miSep04: TMenuItem
        Caption = '-'
      end
      object miShowSumAlgos: TMenuItem
        Action = aShowSumAlgos
        AutoCheck = True
      end
      object miShowSumCondition: TMenuItem
        Action = aShowSumCondition
        AutoCheck = True
      end
    end
    object miData: TMenuItem
      Caption = 'Data'
      object miLoadHistoricalData: TMenuItem
        Action = aRequestToIBByHistoricalData
      end
      object miOpenCheckIBInstruments: TMenuItem
        Action = aOpenCheckIBInstruments
      end
      object miShowTickByTick: TMenuItem
        Action = aShowTickByTick
      end
      object miSep06: TMenuItem
        Caption = '-'
      end
      object miCheckIBInstruments: TMenuItem
        Action = aCheckIBInstruments
      end
      object miCheckLastPrice: TMenuItem
        Action = aCheckLastPrice
      end
    end
    object miChart: TMenuItem
      Caption = 'Chart'
      object miRealtimeChart: TMenuItem
        Action = aShowMonitorConditionChart
      end
    end
    object miReports: TMenuItem
      Caption = 'Reports'
      object miShowExchangeRate: TMenuItem
        Action = aShowExchangeRateInfo
      end
      object miShowPriceInformationDialog: TMenuItem
        Action = aShowPriceInfo
      end
      object miShowInstrumentsWithoutFeed: TMenuItem
        Action = aShowInstrumentsWithoutFeedInfo
      end
      object miShowCalculationStageInfo: TMenuItem
        Action = aShowCalculationStageInfo
      end
      object miShowOrdersTimestamp: TMenuItem
        Action = aShowOrdersTimestampInfo
      end
      object miShowSubscribersInformation: TMenuItem
        Action = aShowSubscribersInfo
      end
      object miShowPrecautionarySettingsInfo: TMenuItem
        Action = aShowPrecautionarySettingsInfo
      end
    end
  end
  object HTTPServer: TIdHTTPServer
    Bindings = <>
    ServerSoftware = 'RobotX'
    OnCommandGet = HTTPServerCommandGet
    Left = 453
    Top = 265
  end
  object ApplicationEvents: TApplicationEvents
    OnException = ApplicationEventsException
    OnIdle = ApplicationEventsIdle
    Left = 85
    Top = 267
  end
  object TimerRunQualifier: TTimer
    Enabled = False
    OnTimer = TimerRunQualifierTimer
    Left = 453
    Top = 430
  end
end
