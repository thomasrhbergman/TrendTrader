object frameDocumentsTree: TframeDocumentsTree
  Left = 0
  Top = 0
  Width = 426
  Height = 525
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlOrderGroupSet: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 525
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlOrderGroupSetTop: TPanel
      Left = 0
      Top = 0
      Width = 426
      Height = 89
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        426
        89)
      object lblViewType: TLabel
        Left = 8
        Top = 3
        Width = 53
        Height = 13
        Alignment = taRightJustify
        Caption = 'View Type:'
      end
      object lblSearchFor: TLabel
        Left = 24
        Top = 44
        Width = 37
        Height = 13
        Alignment = taRightJustify
        Caption = 'Search:'
      end
      object edtSearch: TEdit
        AlignWithMargins = True
        Left = 65
        Top = 41
        Width = 333
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edtSearchChange
      end
      object btnClearSearchText: TBitBtn
        Left = 400
        Top = 40
        Width = 22
        Height = 22
        Anchors = [akTop, akRight]
        ImageIndex = 43
        ImageName = 'RemovePivotField_32x32'
        Images = DMImage.vil16
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btnClearSearchTextClick
      end
      object rbFlat: TRadioButton
        Left = 65
        Top = 2
        Width = 50
        Height = 17
        Caption = 'Flat'
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = rbFlatClick
      end
      object rbTree: TRadioButton
        Left = 65
        Top = 20
        Width = 49
        Height = 17
        Caption = 'Tree'
        TabOrder = 3
        OnClick = rbFlatClick
      end
      object btnExportToCSVTemplate: TBitBtn
        Left = 352
        Top = 1
        Width = 36
        Height = 36
        ParentCustomHint = False
        Action = aExportToCSV
        Anchors = [akTop, akRight]
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object btnPrintTemplate: TBitBtn
        Left = 316
        Top = 1
        Width = 36
        Height = 36
        ParentCustomHint = False
        Action = aPrint
        Anchors = [akTop, akRight]
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
      end
      object btnExportToXLS: TBitBtn
        Left = 388
        Top = 1
        Width = 36
        Height = 36
        ParentCustomHint = False
        Action = aExportToXLS
        Anchors = [akTop, akRight]
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
      object pnlDescription: TPanel
        Left = 0
        Top = 65
        Width = 426
        Height = 24
        Align = alBottom
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 7
      end
    end
    object vstTree: TVirtualStringTree
      Left = 0
      Top = 89
      Width = 426
      Height = 436
      Align = alClient
      BiDiMode = bdLeftToRight
      Colors.BorderColor = 14540253
      Colors.FocusedSelectionColor = clSkyBlue
      Colors.GridLineColor = clSilver
      Colors.SelectionTextColor = 14540253
      Colors.TreeLineColor = 16250871
      Ctl3D = False
      DefaultNodeHeight = 20
      DragHeight = 400
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      Header.AutoSizeIndex = -1
      Header.Height = 20
      Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      Header.SortColumn = 0
      HintMode = hmHint
      Images = DMImage.vilDocumentState
      LineStyle = lsSolid
      ParentBiDiMode = False
      ParentCtl3D = False
      ParentFont = False
      ParentShowHint = False
      PopupMenu = pmTree
      ScrollBarOptions.AlwaysVisible = True
      ScrollBarOptions.VerticalIncrement = 40
      ShowHint = True
      TabOrder = 1
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoTristateTracking, toAutoChangeScale]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware]
      TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus]
      OnAfterCellPaint = vstTreeAfterCellPaint
      OnBeforeCellPaint = vstTreeBeforeCellPaint
      OnCompareNodes = vstTreeCompareNodes
      OnDblClick = vstTreeDblClick
      OnDragAllowed = vstTreeDragAllowed
      OnDragOver = vstTreeDragOver
      OnDragDrop = vstTreeDragDrop
      OnDrawText = vstTreeDrawText
      OnFreeNode = vstTreeFreeNode
      OnGetText = vstTreeGetText
      OnGetImageIndex = vstTreeGetImageIndex
      OnGetHint = vstTreeGetHint
      OnKeyDown = vstTreeKeyDown
      OnMeasureItem = vstTreeMeasureItem
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Columns = <
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 0
          Text = 'Items'
          Width = 200
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 1
          Text = 'Value'
          Width = 90
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 2
          Text = 'Calc Type'
          Width = 90
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 3
          Text = 'Record Id'
          Width = 55
        end>
    end
  end
  object pmTree: TPopupMenu
    OnPopup = pmTreePopup
    Left = 36
    Top = 112
    object miModify: TMenuItem
      Action = aModify
      Default = True
    end
    object miDeleteSelectedNode: TMenuItem
      Action = aDeleteSelectedNode
    end
    object miShowInformationDialog: TMenuItem
      Action = aShowInformationDialog
    end
    object miSeparator01: TMenuItem
      Caption = '-'
    end
    object miAddOrderGroup: TMenuItem
      Action = aAddOrderGroup
    end
    object miAddOrder: TMenuItem
      Action = aAddOrder
    end
    object miAddCondition: TMenuItem
      Action = aAddCondition
    end
    object miAddAlgos: TMenuItem
      Action = aAddAlgos
    end
    object miAddFactor: TMenuItem
      Action = aAddFactor
    end
    object miSeparator02: TMenuItem
      Caption = '-'
    end
    object miExpandTree: TMenuItem
      Action = aExpandTree
    end
    object miCollapsTree: TMenuItem
      Action = aCollapsTree
    end
    object miSeparator03: TMenuItem
      Caption = '-'
    end
    object miExpandSelectedNode: TMenuItem
      Action = aExpandSelectedNode
    end
    object miCollapsSelectedNode: TMenuItem
      Action = aCollapsSelectedNode
    end
  end
  object alTree: TActionList
    Images = DMImage.vil32
    Left = 36
    Top = 176
    object aExportToCSV: TAction
      Hint = 'Export to CSV'
      ImageIndex = 17
      ImageName = 'ExportToCSV_32x32'
      OnExecute = aExportToCSVExecute
    end
    object aExportToXLS: TAction
      ImageIndex = 16
      ImageName = 'ExportToXLS_32x32'
      OnExecute = aExportToXLSExecute
    end
    object aPrint: TAction
      Hint = 'Print'
      ImageIndex = 15
      ImageName = 'Print_32x32'
      OnExecute = aPrintExecute
    end
    object aModify: TAction
      Caption = 'Edit / Modify'
      OnExecute = aModifyExecute
      OnUpdate = aModifyUpdate
    end
    object aDeleteSelectedNode: TAction
      Caption = 'Delete Selected Node'
      Hint = 'Delete selected node'
      ShortCut = 46
      OnExecute = aDeleteSelectedNodeExecute
      OnUpdate = aModifyUpdate
    end
    object aShowInformationDialog: TAction
      Caption = 'Show Information'
      Hint = 'Show Information'
      ShortCut = 16457
      OnExecute = aShowInformationDialogExecute
    end
    object aAddOrderGroup: TAction
      Caption = 'Add New Order Group'
      OnExecute = aAddOrderGroupExecute
      OnUpdate = aModifyUpdate
    end
    object aAddOrder: TAction
      Caption = 'Add New Order'
      OnExecute = aAddOrderExecute
      OnUpdate = aModifyUpdate
    end
    object aAddCondition: TAction
      Caption = 'Add New Condition'
      OnExecute = aAddConditionExecute
      OnUpdate = aModifyUpdate
    end
    object aAddAlgos: TAction
      Caption = 'Add New Algos'
      OnExecute = aAddAlgosExecute
      OnUpdate = aModifyUpdate
    end
    object aAddFactor: TAction
      Caption = 'Add New Factor'
      OnExecute = aAddFactorExecute
      OnUpdate = aModifyUpdate
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
    object aExpandSelectedNode: TAction
      Caption = 'Expand Selected Node'
      OnExecute = aExpandSelectedNodeExecute
      OnUpdate = aExpandSelectedNodeUpdate
    end
    object aCollapsSelectedNode: TAction
      Caption = 'Collaps Selected Node'
      OnExecute = aCollapsSelectedNodeExecute
      OnUpdate = aExpandSelectedNodeUpdate
    end
  end
end
