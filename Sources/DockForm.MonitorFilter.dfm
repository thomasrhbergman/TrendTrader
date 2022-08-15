inherited frmMonitorFilter: TfrmMonitorFilter
  Caption = 'frmMonitorFilter'
  ClientHeight = 595
  ClientWidth = 1016
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 1032
  ExplicitHeight = 634
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlOptions: TPanel
    Width = 1016
    TabOrder = 1
    ExplicitWidth = 1016
    inherited btnExportToExcel: TBitBtn
      Left = 766
      ExplicitLeft = 766
    end
    inherited btnExportToCSV: TBitBtn
      Left = 730
      ExplicitLeft = 730
    end
    inherited btnPrint: TBitBtn
      Left = 694
      ExplicitLeft = 694
    end
    inherited btnColumnSettings: TBitBtn
      Left = 658
      ExplicitLeft = 658
    end
    object btnEditDocument: TBitBtn
      Left = 37
      Top = 1
      Width = 36
      Height = 36
      Action = aEditDocument
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 4
    end
    object btnClearTree: TBitBtn
      Left = 1
      Top = 1
      Width = 36
      Height = 36
      Action = aClearTree
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 5
    end
    object btnExportToHTML: TBitBtn
      Left = 802
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aExportToHTML
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
  end
  inherited pnlMain: TPanel
    Width = 839
    Height = 557
    TabOrder = 0
    ExplicitWidth = 839
    ExplicitHeight = 557
    inherited vstTree: TVirtualStringTree
      Width = 839
      Height = 557
      Alignment = taRightJustify
      Colors.BorderColor = 14540253
      Colors.FocusedSelectionColor = clSkyBlue
      Colors.GridLineColor = clMoneyGreen
      Colors.SelectionTextColor = 14540253
      Colors.TreeLineColor = 16250871
      Ctl3D = False
      CustomCheckImages = DMImage.ilCustomCheckImages
      DefaultNodeHeight = 20
      DragHeight = 400
      Font.Name = 'Consolas'
      Header.Height = 20
      Header.MainColumn = 0
      Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      HintMode = hmHint
      ParentBiDiMode = False
      ParentCtl3D = False
      ParentFont = False
      PopupMenu = nil
      ScrollBarOptions.AlwaysVisible = True
      ScrollBarOptions.VerticalIncrement = 40
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoTristateTracking, toAutoChangeScale]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware]
      TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toMultiSelect]
      OnAfterCellPaint = vstTreeAfterCellPaint
      OnBeforeCellPaint = vstTreeBeforeCellPaint
      OnChecking = vstTreeChecking
      OnCompareNodes = vstTreeCompareNodes
      OnDblClick = vstTreeDblClick
      OnDrawText = vstTreeDrawText
      OnFreeNode = vstTreeFreeNode
      OnGetText = vstTreeGetText
      OnGetImageIndex = vstTreeGetImageIndex
      OnGetHint = vstTreeGetHint
      OnKeyDown = vstTreeKeyDown
      ExplicitWidth = 839
      ExplicitHeight = 557
      Columns = <
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 0
          Text = 'Items'
          Width = 230
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 1
          Text = 'Value'
          Width = 230
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 2
          Text = 'Calc Type'
          Width = 230
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 3
          Text = '% Last/Close'
          Width = 130
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 4
          Text = 'Node ID'
          Width = 19
        end>
    end
  end
  object pnlFilter: TPanel [2]
    Left = 839
    Top = 38
    Width = 177
    Height = 557
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object lblOptions: TLabel
      Left = 5
      Top = 9
      Width = 48
      Height = 13
      Caption = 'Group by:'
      Transparent = True
    end
    object cbOptions: TComboBox
      Left = 59
      Top = 6
      Width = 110
      Height = 21
      TabOrder = 0
      OnChange = cbOptionsChange
    end
    object cbDocumentTypes: TCheckListBox
      Left = 3
      Top = 31
      Width = 165
      Height = 217
      ItemHeight = 13
      TabOrder = 1
    end
    object btnUncheckAll: TBitBtn
      Left = 3
      Top = 250
      Width = 165
      Height = 36
      Action = aUncheckAll
      Caption = 'Uncheck All'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 2
    end
    object btnFilter: TBitBtn
      Left = 3
      Top = 287
      Width = 165
      Height = 36
      Action = aFilter
      Caption = 'Refresh'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 3
    end
    object cbAutomaticUpdate: TCheckBox
      Left = 6
      Top = 327
      Width = 123
      Height = 17
      Caption = 'Automatic Update'
      TabOrder = 4
    end
  end
  inherited ActionList: TActionList
    object aEditDocument: TAction
      Hint = 'Edit Document'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
      OnExecute = aEditDocumentExecute
    end
    object aClearTree: TAction
      Hint = 'Clear Tree'
      ImageIndex = 65
      ImageName = 'DeleteList2_32x32'
      OnExecute = aClearTreeExecute
    end
    object aFilter: TAction
      Caption = 'Refresh'
      Hint = 'Refresh'
      ImageIndex = 32
      ImageName = 'Refresh_32x32'
      OnExecute = aFilterExecute
    end
    object aUncheckAll: TAction
      Caption = 'Uncheck All'
      ImageIndex = 59
      ImageName = 'Clear_32x32'
      OnExecute = aUncheckAllExecute
    end
    object aExportToHTML: TAction
      Hint = 'Export To HTML'
      ImageIndex = 66
      ImageName = 'ExportToHTML_32x32'
      OnExecute = aExportToHTMLExecute
    end
  end
  object timerMonitorUpdate: TTimer
    Enabled = False
    Interval = 250
    OnTimer = timerMonitorUpdateTimer
    Left = 160
    Top = 190
  end
end
