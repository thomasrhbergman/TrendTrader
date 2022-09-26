object frmCandidateStaticList: TfrmCandidateStaticList
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Static List'
  ClientHeight = 471
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 294
    Height = 54
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblName: TLabel
      Left = 14
      Top = 9
      Width = 31
      Height = 13
      Alignment = taRightJustify
      Caption = 'Name:'
    end
    object lblWeight: TLabel
      Left = 7
      Top = 32
      Width = 38
      Height = 13
      Alignment = taRightJustify
      Caption = 'Weight:'
    end
    object edtName: TEdit
      Left = 47
      Top = 6
      Width = 240
      Height = 21
      TabOrder = 0
    end
    object edtWeight: TNumberBox
      Left = 47
      Top = 29
      Width = 67
      Height = 21
      AcceptExpressions = True
      Mode = nbmFloat
      TabOrder = 1
      SpinButtonOptions.Placement = nbspCompact
      UseMouseWheel = True
    end
  end
  object vstTree: TVirtualStringTree
    Left = 0
    Top = 54
    Width = 294
    Height = 336
    Align = alClient
    DragMode = dmAutomatic
    DragOperations = [doCopy, doMove, doLink]
    DragType = dtVCL
    Header.AutoSizeIndex = -1
    Header.Height = 21
    Header.Options = [hoColumnResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    PopupMenu = pmStaticList
    TabOrder = 1
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toAutoChangeScale]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick, toEditOnDblClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnCompareNodes = vstTreeCompareNodes
    OnCreateEditor = vstTreeCreateEditor
    OnDragAllowed = vstTreeDragAllowed
    OnDragOver = vstTreeDragOver
    OnDragDrop = vstTreeDragDrop
    OnDrawText = vstTreeDrawText
    OnEditing = vstTreeEditing
    OnFreeNode = vstTreeFreeNode
    OnGetText = vstTreeGetText
    OnNewText = vstTreeNewText
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
        Position = 0
        Text = 'Instrument'
        Width = 184
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Hint = 'F2 Edit Ranking'
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
        Position = 1
        Text = 'Ranking'
        Width = 72
      end>
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 390
    Width = 294
    Height = 81
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnShowSearchForm: TBitBtn
      Left = 0
      Top = 0
      Width = 294
      Height = 40
      Action = aShowSearchForm
      Caption = 'Re-activate Search form'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 0
    end
    object btnOk: TBitBtn
      Left = 193
      Top = 40
      Width = 100
      Height = 41
      Action = aSave
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 1
    end
    object btnCancel: TBitBtn
      Left = 93
      Top = 40
      Width = 100
      Height = 41
      Action = aCancel
      Cancel = True
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 2
    end
  end
  object dsStaticLists: TDataSource
    DataSet = fbqStaticLists
    Left = 128
    Top = 196
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 32
    Top = 192
    object aShowSearchForm: TAction
      Caption = 'Re-activate Search form'
      Hint = 'Show search form'
      ImageIndex = 21
      ImageName = 'Zoom_32x32'
      OnExecute = aShowSearchFormExecute
    end
    object aDeleteRow: TAction
      Caption = 'Delete row'
      Hint = 'Delete row'
      ShortCut = 46
      OnExecute = aDeleteRowExecute
      OnUpdate = aDeleteRowUpdate
    end
    object aSave: TAction
      Caption = 'Save'
      Hint = 'Save'
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
    end
    object aCancel: TAction
      Caption = 'Cancel'
      Hint = 'Exit'
      ShortCut = 27
      OnExecute = aCancelExecute
    end
  end
  object pmStaticList: TPopupMenu
    Left = 32
    Top = 143
    object miDeleteRow: TMenuItem
      Caption = 'Delete row'
      Hint = 'Delete row'
      OnClick = aDeleteRowExecute
    end
  end
  object fbqStaticLists: TFDQuery
    Connection = DMod.ConnectionStock
    SQL.Strings = (
      'SELECT *'
      'FROM STATICLISTS'
      'WHERE VISIBLE IS TRUE'
      'ORDER BY NAME')
    Left = 128
    Top = 256
    object fbqStaticListsID: TIntegerField
      FieldName = 'ID'
      Origin = 'ID'
      Required = True
    end
    object fbqStaticListsWEIGHT: TSingleField
      FieldName = 'WEIGHT'
      Origin = 'WEIGHT'
    end
    object fbqStaticListsNAME: TStringField
      FieldName = 'NAME'
      Origin = 'NAME'
      Size = 100
    end
  end
end
