object frmOrderTemplateEdit: TfrmOrderTemplateEdit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Order Template'
  ClientHeight = 422
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 377
    Width = 358
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      358
      45)
    object btnSave: TBitBtn
      Left = 256
      Top = 2
      Width = 100
      Height = 40
      Action = aSave
      Anchors = [akTop, akRight]
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 155
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
  object pnlTypeCondition: TPanel
    Left = 0
    Top = 0
    Width = 358
    Height = 377
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      358
      377)
    object lblName: TLabel
      Left = 17
      Top = 7
      Width = 38
      Height = 16
      Alignment = taRightJustify
      Caption = 'Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edtName: TEdit
      Left = 17
      Top = 29
      Width = 320
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object vstTree: TVirtualStringTree
      Left = 0
      Top = 72
      Width = 358
      Height = 305
      Align = alBottom
      BiDiMode = bdLeftToRight
      Colors.BorderColor = 14540253
      Colors.FocusedSelectionColor = clSkyBlue
      Colors.GridLineColor = clMoneyGreen
      Colors.SelectionTextColor = clBlack
      Colors.TreeLineColor = 16250871
      Ctl3D = False
      DefaultNodeHeight = 20
      DragHeight = 400
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      Header.AutoSizeIndex = 0
      Header.Height = 20
      Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoVisible, hoHeaderClickAutoSort]
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
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoTristateTracking, toAutoChangeScale]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware]
      TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toAlwaysSelectNode]
      OnAfterCellPaint = vstTreeAfterCellPaint
      OnBeforeCellPaint = vstTreeBeforeCellPaint
      OnDblClick = vstTreeDblClick
      OnDrawText = vstTreeDrawText
      OnGetText = vstTreeGetText
      OnGetImageIndex = vstTreeGetImageIndex
      OnGetHint = vstTreeGetHint
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Columns = <
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 0
          Text = 'Items'
          Width = 358
        end>
    end
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 232
    Top = 87
    object aSave: TAction
      Caption = 'Save'
      ImageIndex = 46
      ImageName = 'tick'
      ShortCut = 16467
      OnExecute = aSaveExecute
    end
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 40
    Top = 192
    object aShowInformation: TAction
      Caption = 'Show Information'
      Hint = 'Show Information'
      ShortCut = 16457
      OnExecute = aShowInformationExecute
      OnUpdate = aShowInformationUpdate
    end
    object aAddOrderGroup: TAction
      Caption = 'Add New OrderGroup'
      ShortCut = 16455
      OnExecute = aAddOrderGroupExecute
      OnUpdate = aAddOrderGroupUpdate
    end
    object aAddOrder: TAction
      Caption = 'Add New Order'
      ShortCut = 16463
      OnExecute = aAddOrderExecute
      OnUpdate = aAddOrderUpdate
    end
    object aAddCondition: TAction
      Caption = 'Add New Condition'
      ShortCut = 16451
      OnExecute = aAddConditionExecute
      OnUpdate = aAddConditionUpdate
    end
    object aClearTree: TAction
      Caption = 'Delete All Nodes'
      Hint = 'Delete All Nodes'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aClearTreeExecute
    end
    object aCollapsTree: TAction
      Caption = 'Collaps Tree'
      ShortCut = 49219
      OnExecute = aCollapsTreeExecute
    end
    object aDeleteSelectedNode: TAction
      Caption = 'aDeleteSelectedNode'
      Hint = 'Delete'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      ShortCut = 46
      OnExecute = aDeleteSelectedNodeExecute
      OnUpdate = aDeleteSelectedNodeUpdate
    end
    object aEdit: TAction
      Caption = 'aEdit'
      Hint = ' Edit'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
      ShortCut = 16461
      OnExecute = aEditExecute
      OnUpdate = aEditUpdate
    end
    object aExpandTree: TAction
      Caption = 'Expand Tree'
      ShortCut = 16453
      OnExecute = aExpandTreeExecute
    end
    object aOpen: TAction
      Caption = 'aOpen'
      Hint = 'Open Existing Qualifier'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
    end
    object aRefresh: TAction
      Caption = 'aRefresh'
      Hint = 'Refresh'
      ImageIndex = 32
      ImageName = 'Refresh_32x32'
    end
    object aSaveAs: TAction
      Caption = 'aSaveAs'
      Hint = 'Save As...'
      ImageIndex = 26
      ImageName = 'SaveAll_32x32'
    end
    object Action1: TAction
      Caption = 'Action1'
      Hint = 'The Template is Updated and Saved'
      ImageIndex = 10
      ImageName = 'Save_32x32'
      OnExecute = aSaveExecute
    end
    object aInfo: TAction
      Caption = 'aInfo'
      Hint = 'Nodes Info'
      ImageIndex = 6
      ImageName = 'Info_32x32'
      OnExecute = aInfoExecute
    end
  end
  object pmTree: TPopupMenu
    OnPopup = pmTreePopup
    Left = 37
    Top = 136
    object miEdit: TMenuItem
      Action = aEdit
      Caption = 'Edit / Modify'
      Default = True
    end
    object miDeleteSelectedNode: TMenuItem
      Action = aDeleteSelectedNode
      Caption = 'Delete'
    end
    object miClearTree: TMenuItem
      Action = aClearTree
      ShortCut = 16430
    end
    object miShowInformation: TMenuItem
      Action = aShowInformation
    end
    object miSep01: TMenuItem
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
    object miSep02: TMenuItem
      Caption = '-'
    end
    object miExpandTree: TMenuItem
      Action = aExpandTree
    end
    object miCollapsTree: TMenuItem
      Action = aCollapsTree
    end
  end
end
