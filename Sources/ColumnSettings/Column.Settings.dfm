object frmColumnSettings: TfrmColumnSettings
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight]
  BorderStyle = bsDialog
  Caption = 'Column List'
  ClientHeight = 512
  ClientWidth = 363
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 466
    Width = 363
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnCancel: TBitBtn
      Left = 160
      Top = 4
      Width = 100
      Height = 40
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ModalResult = 2
      ParentFont = False
      TabOrder = 0
    end
    object btnAddColumn: TBitBtn
      Left = 262
      Top = 4
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
    Width = 363
    Height = 45
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      363
      45)
    object lblAvailableFilters: TLabel
      Left = 12
      Top = 26
      Width = 77
      Height = 13
      Alignment = taRightJustify
      Caption = 'Available filters:'
    end
    object lblColumnSettings: TLabel
      Left = 9
      Top = 3
      Width = 80
      Height = 13
      Alignment = taRightJustify
      Caption = 'Column settings:'
      Color = clDefault
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object edtSearch: TEdit
      Left = 93
      Top = 23
      Width = 248
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      TabOrder = 0
      OnChange = edtSearchChange
    end
    object btnClearSearchText: TBitBtn
      Left = 341
      Top = 22
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
    object btnDeleteColumnSettings: TBitBtn
      Left = 341
      Top = 0
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aDeleteColumnSettings
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object btnSaveColumnSettings: TBitBtn
      Left = 320
      Top = 0
      Width = 22
      Height = 22
      Action = aSaveColumnSettings
      Images = DMImage.vil16
      TabOrder = 3
    end
    object cbColumnSettings: TComboBox
      Left = 93
      Top = 0
      Width = 227
      Height = 21
      TabOrder = 4
      OnChange = cbColumnSettingsChange
    end
  end
  object vstColumns: TVirtualStringTree
    Left = 0
    Top = 45
    Width = 363
    Height = 421
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Header.AutoSizeIndex = -1
    Header.Height = 16
    Header.Options = [hoAutoResize, hoColumnResize, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.SortColumn = 2
    TabOrder = 2
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnChecked = vstColumnsChecked
    OnCompareNodes = vstColumnsCompareNodes
    OnFreeNode = vstColumnsFreeNode
    OnGetText = vstColumnsGetText
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 0
        Text = 'Name'
        Width = 291
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 1
        Text = 'Width'
        Width = 52
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 2
        Text = 'Position'
        Width = 20
      end>
  end
  object ActionListMain: TActionList
    Images = DMImage.vil16
    Left = 32
    Top = 192
    object aSave: TAction
      Caption = 'Ok'
      ImageIndex = 46
      ImageName = 'tick'
    end
    object aCancel: TAction
      Caption = 'Cancel'
      OnExecute = aCancelExecute
    end
    object aSaveColumnSettings: TAction
      Hint = 'Save Column Settings'
      ImageIndex = 10
      ImageName = 'Save_32x32'
      OnExecute = aSaveColumnSettingsExecute
    end
    object aDeleteColumnSettings: TAction
      Hint = 'Delete Column Settings'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aDeleteColumnSettingsExecute
    end
  end
end
