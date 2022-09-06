inherited frmListForm: TfrmListForm
  Margins.Right = 5
  Caption = 'frmListForm'
  ClientHeight = 422
  ClientWidth = 321
  Constraints.MinWidth = 337
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  ExplicitWidth = 337
  ExplicitHeight = 461
  PixelsPerInch = 96
  TextHeight = 13
  object vstList: TVirtualStringTree
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 311
    Height = 366
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AccessibleName = 'Active'
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    LineStyle = lsSolid
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware]
    TreeOptions.SelectionOptions = [toExtendedFocus, toAlwaysSelectNode]
    OnAfterCellPaint = vstListAfterCellPaint
    OnDblClick = vstListDblClick
    OnGetText = vstListGetText
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Name'
        Width = 230
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 1
        Text = 'Active'
        Width = 77
      end>
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 376
    Width = 321
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnNew: TBitBtn
      AlignWithMargins = True
      Left = 5
      Top = 3
      Width = 57
      Height = 40
      Margins.Left = 5
      Action = actNew
      Align = alLeft
      Caption = 'New'
      TabOrder = 0
    end
    object btnDelete: TBitBtn
      AlignWithMargins = True
      Left = 68
      Top = 3
      Width = 57
      Height = 40
      Action = actDelete
      Align = alLeft
      Caption = 'Delete'
      TabOrder = 1
    end
    object btnEdit: TBitBtn
      AlignWithMargins = True
      Left = 131
      Top = 3
      Width = 57
      Height = 40
      Action = actEdit
      Align = alLeft
      Caption = 'Edit'
      TabOrder = 2
    end
    object btnSelect: TBitBtn
      AlignWithMargins = True
      Left = 194
      Top = 3
      Width = 57
      Height = 40
      Action = actSelect
      Align = alLeft
      Caption = 'Select'
      TabOrder = 3
    end
    object btnExit: TBitBtn
      AlignWithMargins = True
      Left = 257
      Top = 3
      Width = 57
      Height = 40
      Action = actExit
      Align = alLeft
      Caption = 'Exit'
      TabOrder = 4
    end
  end
  object ActionList1: TActionList
    Left = 104
    Top = 104
    object actNew: TAction
      Caption = 'New'
      OnExecute = actNewExecute
    end
    object actDelete: TAction
      Caption = 'Delete'
      OnExecute = actDeleteExecute
    end
    object actEdit: TAction
      Caption = 'Edit'
      OnExecute = actEditExecute
    end
    object actSelect: TAction
      Caption = 'Select'
      OnExecute = actSelectExecute
    end
    object actExit: TAction
      Caption = 'Exit'
      OnExecute = actExitExecute
    end
  end
end
