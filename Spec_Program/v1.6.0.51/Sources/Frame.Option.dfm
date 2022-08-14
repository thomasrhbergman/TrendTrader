object frameOption: TframeOption
  Left = 0
  Top = 0
  Width = 1054
  Height = 564
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 1054
    Height = 37
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      1054
      37)
    object btnExportToExcel: TBitBtn
      Left = 1017
      Top = 0
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aExportToExcel
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object btnExportToCSV: TBitBtn
      Left = 981
      Top = 0
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aExportToCSV
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object btnPrint: TBitBtn
      Left = 945
      Top = 0
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aPrint
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object btnColumnSettings: TBitBtn
      Left = 909
      Top = 0
      Width = 36
      Height = 36
      Action = aColumnSettings
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object btnRefresh: TBitBtn
      Left = 1
      Top = 0
      Width = 36
      Height = 36
      Action = aRefresh
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
  end
  object vstOption: TVirtualStringTree
    Left = 0
    Top = 37
    Width = 1054
    Height = 527
    Align = alClient
    DragOperations = []
    DragType = dtVCL
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Header.AutoSizeIndex = -1
    Header.Height = 18
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    LineStyle = lsSolid
    ParentFont = False
    ParentShowHint = False
    PopupMenu = pmOption
    ShowHint = True
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect, toMultiSelect, toAlwaysSelectNode]
    OnColumnResize = vstOptionColumnResize
    OnCompareNodes = vstOptionCompareNodes
    OnDragAllowed = vstOptionDragAllowed
    OnDrawText = vstOptionDrawText
    OnFreeNode = vstOptionFreeNode
    OnGetText = vstOptionGetText
    OnHeaderDragged = vstOptionHeaderDragged
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 0
        Tag = -1
        Text = 'LocalSymbol'
        Width = 135
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 1
        Tag = -1
        Text = 'ContractId'
        Width = 90
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 2
        Tag = -1
        Text = 'Underlying ConId'
        Width = 97
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 3
        Tag = -1
        Text = 'Expiry'
        Width = 120
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 4
        Tag = -1
        Text = 'Strike'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 5
        Tag = -1
        Text = 'undPrice'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 6
        Tag = -1
        Text = 'Delta'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 7
        Tag = -1
        Text = 'OptPrice'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 8
        Tag = -1
        Text = 'pvDividend'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 9
        Tag = -1
        Text = 'Gamma'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 10
        Tag = -1
        Text = 'Vega'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 11
        Tag = -1
        Text = 'Theta'
        Width = 70
      end>
  end
  object alOptions: TActionList
    Images = DMImage.vil32
    Left = 408
    Top = 128
    object aExportToExcel: TAction
      Hint = 'Export To Excel'
      ImageIndex = 16
      ImageName = 'ExportToXLS_32x32'
      OnExecute = aExportToExcelExecute
    end
    object aExportToCSV: TAction
      Hint = 'Export to CSV'
      ImageIndex = 17
      ImageName = 'ExportToCSV_32x32'
      OnExecute = aExportToCSVExecute
    end
    object aPrint: TAction
      Hint = 'Print'
      ImageIndex = 15
      ImageName = 'Print_32x32'
      OnExecute = aPrintExecute
    end
    object aColumnSettings: TAction
      Hint = 'Column Settings'
      ImageIndex = 36
      ImageName = 'ListBullets_32x32'
      OnExecute = aColumnSettingsExecute
    end
    object aClearResultList: TAction
      Caption = 'Clear Result List'
      OnExecute = aClearResultListExecute
    end
    object aDeleteItems: TAction
      Caption = 'Delete Items'
      OnExecute = aDeleteItemsExecute
    end
    object aRefresh: TAction
      Hint = 'Refresh'
      ImageIndex = 32
      ImageName = 'Refresh_32x32'
      OnExecute = aRefreshExecute
    end
  end
  object pmOption: TPopupMenu
    Left = 404
    Top = 200
    object miClearResultList: TMenuItem
      Action = aClearResultList
    end
    object miDeleteItems: TMenuItem
      Action = aDeleteItems
    end
  end
end
