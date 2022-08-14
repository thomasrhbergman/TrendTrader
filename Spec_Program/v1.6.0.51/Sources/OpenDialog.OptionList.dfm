object frmOpenOptionList: TfrmOpenOptionList
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Open Option List'
  ClientHeight = 467
  ClientWidth = 390
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object grOptionList: TDBGrid
    Left = 0
    Top = 0
    Width = 390
    Height = 421
    Align = alClient
    DataSource = dsOptionList
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgCancelOnExit, dgMultiSelect, dgTitleClick, dgTitleHotTrack]
    PopupMenu = PopupMenu
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnDblClick = aOpenExecute
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Title.Alignment = taCenter
        Title.Caption = 'Id'
        Width = 53
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NAME'
        Title.Alignment = taCenter
        Title.Caption = 'Name'
        Width = 311
        Visible = True
      end>
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 421
    Width = 390
    Height = 46
    Align = alBottom
    TabOrder = 1
    object btnOpen: TBitBtn
      Left = 276
      Top = 2
      Width = 109
      Height = 41
      Action = aOpen
      Caption = 'Open'
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
      Left = 166
      Top = 2
      Width = 109
      Height = 41
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
    object btnDelete: TBitBtn
      Left = 56
      Top = 2
      Width = 109
      Height = 41
      Action = aDelete
      Caption = 'Delete'
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
  object dsOptionList: TDataSource
    DataSet = fbqOptionList
    Left = 72
    Top = 140
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 280
    Top = 40
    object aOpen: TAction
      Caption = 'Open'
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aOpenExecute
      OnUpdate = aOpenUpdate
    end
    object aDelete: TAction
      Caption = 'Delete'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      ShortCut = 46
      OnExecute = aDeleteExecute
      OnUpdate = aDeleteUpdate
    end
  end
  object PopupMenu: TPopupMenu
    Left = 184
    Top = 144
    object miOpen: TMenuItem
      Action = aOpen
      Default = True
    end
    object miDelete: TMenuItem
      Action = aDelete
    end
  end
  object fbqOptionList: TFDQuery
    SQL.Strings = (
      'SELECT * '
      'FROM OPTION_LIST'
      'ORDER BY ID')
    Left = 72
    Top = 192
  end
end
