inherited frmDockFormPosition: TfrmDockFormPosition
  Caption = 'Position'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  
  TextHeight = 13
  inherited pnlOptions: TPanel
    object lblOptions: TLabel [0]
      Left = 78
      Top = 10
      Width = 48
      Height = 13
      Caption = 'Group by:'
      Transparent = True
    end
    object cbOptions: TComboBox
      Left = 135
      Top = 6
      Width = 145
      Height = 21
      TabOrder = 4
      OnChange = cbOptionsChange
    end
    object btnCloseSelected: TBitBtn
      Left = 38
      Top = 1
      Width = 36
      Height = 36
      Hint = 'Close All Selected Positions'
      Action = aCloseAllLinesMarked
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnDragDrop = btnCloseSelectedDragDrop
      OnDragOver = btnCloseSelectedDragOver
    end
    object btnSellAll: TBitBtn
      Left = 2
      Top = 1
      Width = 36
      Height = 36
      Action = aCloseOneSelected
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
  end
  inherited pnlMain: TPanel
    inherited vstTree: TVirtualStringTree
      Alignment = taRightJustify
      DragMode = dmAutomatic
      DragType = dtVCL
      Header.MainColumn = 0
      Header.Options = [hoColumnResize, hoDrag, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      Header.SortColumn = 1
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toAutoChangeScale]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toFullRowSelect, toLevelSelectConstraint, toMultiSelect, toRightClickSelect]
      OnCompareNodes = vstTreeCompareNodes
      OnDragAllowed = vstTreeDragAllowed
      OnDrawText = vstTreeDrawText
      OnFreeNode = vstTreeFreeNode
      OnGetText = vstTreeGetText
      OnGetImageIndex = vstTreeGetImageIndex
      OnStartDrag = vstTreeStartDrag
      Columns = <
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
          Position = 0
          Text = 'Contract'
          Width = 110
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
          Position = 1
          Text = 'Broker'
          Width = 103
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
          Position = 2
          Text = 'Position'
          Width = 81
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
          Position = 3
          Text = 'Market price'
          Width = 94
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
          Position = 4
          Text = 'Market value'
          Width = 79
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
          Position = 5
          Text = 'Average cost'
          Width = 96
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
          Position = 6
          Text = 'Realized P/L'
          Width = 85
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
          Position = 7
          Text = 'Unrealized P/L'
          Width = 80
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
          Position = 8
          Text = 'Account'
          Width = 89
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
          Position = 9
          Text = 'Time'
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 10
          Text = 'ConId'
          Width = 100
        end>
    end
  end
  inherited ActionList: TActionList
    object aSelectAllNodes: TAction
      Caption = 'Select all nodes'
      Hint = 'Select all nodes'
      OnExecute = aSelectAllNodesExecute
      OnUpdate = aCloseOneSelectedUpdate
    end
    object aSelectProfitRealized: TAction
      Caption = 'Select Profit Realized'
      Hint = 'Select Profit Realized'
      OnExecute = aSelectProfitRealizedExecute
      OnUpdate = aCloseOneSelectedUpdate
    end
    object aSelectProfitUnrealized: TAction
      Caption = 'Select Profit Unrealized'
      Hint = 'Select Profit Unrealized'
      OnExecute = aSelectProfitUnrealizedExecute
      OnUpdate = aCloseOneSelectedUpdate
    end
    object aSelectLossRealized: TAction
      Caption = 'Select Loss Realized'
      Hint = 'Select Loss Realized'
      OnExecute = aSelectLossRealizedExecute
      OnUpdate = aCloseOneSelectedUpdate
    end
    object aSelectLossUnrealized: TAction
      Caption = 'Select Loss Unrealized'
      Hint = 'Select Loss Unrealized'
      OnExecute = aSelectLossUnrealizedExecute
      OnUpdate = aCloseOneSelectedUpdate
    end
    object aCloseOneSelected: TAction
      Hint = 'Close Selected Position'
      ImageIndex = 12
      ImageName = 'lightning'
      OnExecute = aCloseOneSelectedExecute
      OnUpdate = aSelectNodesUpdate
    end
    object aCloseAllLinesMarked: TAction
      Hint = 'Close All Positions'
      ImageIndex = 14
      ImageName = 'lightning_add'
      OnExecute = aCloseAllLinesMarkedExecute
      OnUpdate = aCloseOneSelectedUpdate
    end
  end
  inherited pmTree: TPopupMenu
    object miSelectAllNodes: TMenuItem
      Action = aSelectAllNodes
      Default = True
    end
    object miSep1: TMenuItem
      Caption = '-'
    end
    object miSelectProfitRealized: TMenuItem
      Action = aSelectProfitRealized
    end
    object miSelectProfitUnrealized: TMenuItem
      Action = aSelectProfitUnrealized
    end
    object miSep2: TMenuItem
      Caption = '-'
    end
    object miSelectLossRealized: TMenuItem
      Action = aSelectLossRealized
    end
    object miSelectLossUnrealized: TMenuItem
      Action = aSelectLossUnrealized
    end
    object miSep3: TMenuItem
      Caption = '-'
    end
    object miCloseAllLinesMarked: TMenuItem
      Action = aCloseAllLinesMarked
      Caption = 'Close All Lines Marked'
    end
    object miCloseOneSelected: TMenuItem
      Action = aCloseOneSelected
      Caption = 'Close One Selected'
    end
  end
end
