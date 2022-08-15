object frmOpenCondition: TfrmOpenCondition
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select Condition'
  ClientHeight = 372
  ClientWidth = 404
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 410
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 13
  object grdConditions: TDBGrid
    Left = 0
    Top = 0
    Width = 404
    Height = 328
    Align = alClient
    DataSource = dsConditions
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgCancelOnExit, dgMultiSelect, dgTitleClick, dgTitleHotTrack]
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnDblClick = aOpenUpdate
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Title.Alignment = taCenter
        Title.Caption = 'Id'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DESCRIPTION'
        Title.Alignment = taCenter
        Title.Caption = 'Name'
        Width = 314
        Visible = True
      end>
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 328
    Width = 404
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOpen: TBitBtn
      Left = 292
      Top = 2
      Width = 110
      Height = 41
      Action = aOpen
      Caption = 'Open'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 182
      Top = 2
      Width = 110
      Height = 41
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
    object btnDelete: TBitBtn
      Left = 73
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
  object dsConditions: TDataSource
    DataSet = fbqConditions
    Left = 184
    Top = 84
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 280
    Top = 40
    object aOpen: TAction
      Caption = 'Open'
      OnUpdate = aOpenUpdate
    end
    object aDelete: TAction
      Caption = 'Delete'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aDeleteExecute
    end
  end
  object fbqConditions: TFDQuery
    Connection = DMod.FBConnectionStock
    UpdateObject = updConditions
    SQL.Strings = (
      'SELECT ID, DESCRIPTION'
      'FROM CONDITION'
      'WHERE IS_TEMPLATE IS TRUE'
      'ORDER BY DESCRIPTION')
    Left = 184
    Top = 152
  end
  object updConditions: TFDUpdateSQL
    Connection = DMod.FBConnectionStock
    DeleteSQL.Strings = (
      'delete from CONDITION'
      'where'
      '  DESCRIPTION = :OLD_DESCRIPTION')
    FetchRowSQL.Strings = (
      'Select '
      '  ID,'
      '  COND_TYPE,'
      '  COND_ACTIVE,'
      '  COND_VALUE,'
      '  GRADIENT,'
      '  WIDTH,'
      '  MONITORING,'
      '  UP_PROC,'
      '  START_DATE,'
      '  END_DATE,'
      '  START_TIME,'
      '  END_TIME,'
      '  PRIORITY,'
      '  KIND_CREATION,'
      '  TRAIL_BUY,'
      '  TRAIL_SELL,'
      '  DESCRIPTION,'
      '  IS_BREAKUP,'
      '  IS_BYPASS,'
      '  TICK_TYPE1,'
      '  TICK_TYPE2,'
      '  TYPE_OPERATION,'
      '  INEQUALITY_RT,'
      '  INEQUALITY_GR,'
      '  INEQUALITY_COL,'
      '  COND_VALUE_RELATIVE,'
      '  IS_TEMPLATE'
      'from CONDITION '
      'where'
      '  DESCRIPTION = :DESCRIPTION'
      '')
    Left = 96
    Top = 152
  end
end
