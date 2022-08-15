object frmCandidateFilterList: TfrmCandidateFilterList
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Filter list'
  ClientHeight = 450
  ClientWidth = 286
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnCreate = FormCreate
  
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 406
    Width = 286
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnCancel: TBitBtn
      Left = 82
      Top = 3
      Width = 100
      Height = 40
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
      Left = 184
      Top = 3
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
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 286
    Height = 21
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      286
      21)
    object lblAvailableFilters: TLabel
      Left = 3
      Top = 3
      Width = 73
      Height = 13
      Alignment = taRightJustify
      Caption = 'Available filters'
    end
    object edtAvailableFilters: TEdit
      Left = 78
      Top = 0
      Width = 186
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edtAvailableFiltersChange
    end
    object btnClearSearchText: TBitBtn
      Left = 264
      Top = -1
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
  end
  object vstFilter: TVirtualStringTree
    Left = 0
    Top = 21
    Width = 286
    Height = 385
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Header.AutoSizeIndex = -1
    Header.Options = [hoAutoResize, hoColumnResize, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    TabOrder = 2
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnChecked = vstFilterChecked
    OnCompareNodes = vstFilterCompareNodes
    OnFreeNode = vstFilterFreeNode
    OnGetText = vstFilterGetText
    OnInitNode = vstFilterInitNode
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Name'
        Width = 286
      end>
  end
  object ActionListMain: TActionList
    Left = 32
    Top = 192
    object aSave: TAction
      Caption = 'Ok'
      ImageIndex = 3
    end
    object aCancel: TAction
      Caption = 'Cancel'
      OnExecute = aCancelExecute
    end
  end
end
