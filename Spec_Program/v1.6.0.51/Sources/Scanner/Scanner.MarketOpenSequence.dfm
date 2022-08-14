object frmScannerMarketOpenSequence: TfrmScannerMarketOpenSequence
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Market Scanner'
  ClientHeight = 372
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
  object grdScan: TDBGrid
    Left = 0
    Top = 0
    Width = 390
    Height = 326
    Align = alClient
    DataSource = dsScan
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgCancelOnExit, dgMultiSelect, dgTitleClick, dgTitleHotTrack]
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
        Width = 42
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NAME'
        Title.Alignment = taCenter
        Width = 317
        Visible = True
      end>
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 326
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
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 166
      Top = 2
      Width = 109
      Height = 41
      Action = aCancel
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
      Left = 55
      Top = 2
      Width = 109
      Height = 41
      Action = aDelete
      Caption = 'Delete'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 2
    end
  end
  object dsScan: TDataSource
    DataSet = fbqScan
    Left = 184
    Top = 76
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 304
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
    object aCancel: TAction
      Caption = 'Cancel'
      OnExecute = aCancelExecute
    end
  end
  object fbqScan: TFDQuery
    Connection = DMod.FBConnectionStock
    SQL.Strings = (
      'select * '
      'from SCAN_MARKET'
      'order by Id desc')
    Left = 184
    Top = 168
    object fbqScanID: TIntegerField
      FieldName = 'ID'
      Origin = 'ID'
      Required = True
    end
    object fbqScanNAME: TStringField
      FieldName = 'NAME'
      Origin = 'NAME'
      Size = 100
    end
  end
end
