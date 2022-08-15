object frameFuture: TframeFuture
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
  object pnlTop: TPanel
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
  object vstFuture: TVirtualStringTree
    Left = 0
    Top = 37
    Width = 1054
    Height = 527
    Align = alClient
    DragOperations = []
    DragType = dtVCL
    Header.AutoSizeIndex = -1
    Header.Height = 18
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    LineStyle = lsSolid
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect, toMultiSelect, toAlwaysSelectNode]
    OnColumnResize = vstFutureColumnResize
    OnCompareNodes = vstFutureCompareNodes
    OnDragAllowed = vstFutureDragAllowed
    OnFreeNode = vstFutureFreeNode
    OnGetText = vstFutureGetText
    OnHeaderClick = vstFutureHeaderClick
    OnHeaderDragged = vstFutureHeaderDragged
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 0
        Tag = -1
        Text = 'Broker'
        Width = 80
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 1
        Tag = -1
        Text = 'Contract Id'
        Width = 95
      end
      item
        BiDiMode = bdRightToLeft
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 2
        Tag = -1
        Text = 'Underlying ConId'
        Width = 98
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 3
        Tag = -1
        Text = 'IsIn'
        Width = 90
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 4
        Tag = -1
        Text = 'Name'
        Width = 110
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 5
        Tag = -1
        Text = 'Symbol'
        Width = 89
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 6
        Tag = -1
        Text = 'Local Symbol'
        Width = 74
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 7
        Tag = -1
        Text = 'Type'
        Width = 55
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 8
        Tag = -1
        Text = 'Group'
        Width = 55
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 9
        Tag = -1
        Text = 'Currency'
        Width = 60
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 10
        Tag = -1
        Text = 'Tradables'
        Width = 90
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 11
        Tag = -1
        Text = 'Sector'
        Width = 60
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 12
        Tag = -1
        Text = 'Exchange'
        Width = 70
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 13
        Tag = -1
        Text = 'Primary Exchange'
        Width = 70
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 14
        Tag = -1
        Text = 'Expiry'
        Width = 90
      end>
  end
  object alFuture: TActionList
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
  object pmFuture: TPopupMenu
    Left = 404
    Top = 192
    object miClearResultList: TMenuItem
      Action = aClearResultList
    end
    object miDeleteItems: TMenuItem
      Action = aDeleteItems
    end
  end
end
