object frmCandidateStaticLists: TfrmCandidateStaticLists
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Static Lists'
  ClientHeight = 471
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object grdStaticLists: TDBGrid
    Left = 0
    Top = 38
    Width = 294
    Height = 352
    Align = alClient
    DataSource = dsStaticLists
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    PopupMenu = pmStaticLists
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnDblClick = aEditListExecute
    OnDragDrop = grdStaticListsDragDrop
    OnDragOver = grdStaticListsDragOver
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Title.Alignment = taCenter
        Title.Caption = 'Number'
        Width = 40
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NAME'
        Title.Alignment = taCenter
        Title.Caption = 'List Name'
        Width = 172
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'WEIGHT'
        Title.Alignment = taCenter
        Title.Caption = 'Weight'
        Width = 56
        Visible = True
      end>
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 390
    Width = 294
    Height = 81
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnShowSearchForm: TBitBtn
      Left = 0
      Top = 0
      Width = 294
      Height = 40
      Action = aShowSearchInstruments
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
      Action = aOk
      Caption = 'Ok'
      Default = True
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
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 294
    Height = 38
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object btnEditQualifier: TBitBtn
      Left = 36
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aEditList
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object btnAdd: TBitBtn
      Left = 0
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aAddList
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object btnDeleteSelectedNode: TBitBtn
      Left = 72
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aDeleteList
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object bUpdate: TBitBtn
      Left = 187
      Top = 1
      Width = 36
      Height = 36
      Action = aUpdate
      Images = DMImage.vil32
      Layout = blGlyphTop
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object bIntersection: TBitBtn
      Left = 258
      Top = 1
      Width = 36
      Height = 36
      Action = aIntersection
      Images = DMImage.vil32
      Layout = blGlyphTop
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object bReduce: TBitBtn
      Left = 223
      Top = 1
      Width = 36
      Height = 36
      Action = aReduce
      Images = DMImage.vil32
      Layout = blGlyphTop
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
    object bAddAll: TBitBtn
      Left = 151
      Top = 1
      Width = 36
      Height = 36
      Action = aAddAll
      Images = DMImage.vil32
      Layout = blGlyphTop
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
  end
  object dsStaticLists: TDataSource
    DataSet = fbqStaticLists
    Left = 112
    Top = 92
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 32
    Top = 144
    object aAddAll: TAction
      Hint = 'Add All'
      ImageIndex = 37
      ImageName = 'select_by_adding_to_selection'
      OnExecute = aAddAllExecute
      OnUpdate = aAddAllUpdate
    end
    object aUpdate: TAction
      Hint = 'Update'
      ImageIndex = 39
      ImageName = 'shape_move_forwards'
      OnExecute = aUpdateExecute
      OnUpdate = aAddAllUpdate
    end
    object aReduce: TAction
      Hint = 'Reduce'
      ImageIndex = 38
      ImageName = 'shape_move_backwards'
      OnExecute = aReduceExecute
      OnUpdate = aAddAllUpdate
    end
    object aIntersection: TAction
      Hint = 'Intersection'
      ImageIndex = 40
      ImageName = 'select_by_intersection'
      OnExecute = aIntersectionExecute
      OnUpdate = aAddAllUpdate
    end
    object aEditList: TAction
      Hint = 'Edit List'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
      OnExecute = aEditListExecute
      OnUpdate = aEditListUpdate
    end
    object aAddList: TAction
      Hint = 'Add List'
      ImageIndex = 52
      ImageName = 'AddItem_32x32'
      OnExecute = aAddListExecute
      OnUpdate = aAddListUpdate
    end
    object aDeleteList: TAction
      Hint = 'Delete List'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      ShortCut = 46
      OnExecute = aDeleteListExecute
      OnUpdate = aEditListUpdate
    end
    object aShowSearchInstruments: TAction
      Caption = 'Re-activate Search form'
      ImageIndex = 21
      ImageName = 'Zoom_32x32'
      OnExecute = aShowSearchInstrumentsExecute
    end
    object aCancel: TAction
      Caption = 'Cancel'
      OnExecute = aCancelExecute
    end
    object aOk: TAction
      Caption = 'Ok'
      OnExecute = aOkExecute
    end
  end
  object pmStaticLists: TPopupMenu
    Left = 32
    Top = 87
    object miNewList: TMenuItem
      Action = aAddList
      Caption = 'Add List'
    end
    object miEditList: TMenuItem
      Action = aEditList
      Caption = 'Edit List'
    end
    object miDeleteList: TMenuItem
      Action = aDeleteList
      Caption = 'Delete List'
    end
    object miSep01: TMenuItem
      Caption = '-'
    end
    object miAddAll: TMenuItem
      Action = aAddAll
      Caption = 'Add All'
    end
    object miUpdate: TMenuItem
      Action = aUpdate
      Caption = 'Update'
    end
    object miReduce: TMenuItem
      Action = aReduce
      Caption = 'Reduce'
    end
    object miIntersection: TMenuItem
      Action = aIntersection
      Caption = 'Intersection'
    end
  end
  object fbqStaticLists: TFDQuery
    Connection = DMod.ConnectionStock
    SQL.Strings = (
      'SELECT *'
      'FROM STATICLISTS'
      'WHERE VISIBLE IS TRUE')
    Left = 112
    Top = 152
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
