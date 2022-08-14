object frmOpenAlgos: TfrmOpenAlgos
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select algos'
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
  PixelsPerInch = 96
  TextHeight = 13
  object grdAlgos: TDBGrid
    Left = 0
    Top = 0
    Width = 404
    Height = 328
    Align = alClient
    DataSource = dsAlgos
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Title.Caption = 'Id'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NAME'
        Title.Caption = 'Name of algos'
        Width = 266
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
      Left = 179
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
  end
  object dsAlgos: TDataSource
    DataSet = fbqAlgos
    Left = 184
    Top = 84
  end
  object ActionListMain: TActionList
    Left = 280
    Top = 40
    object aOpen: TAction
      Caption = 'Open'
      OnUpdate = aOpenUpdate
    end
  end
  object fbqAlgos: TFDQuery
    SQL.Strings = (
      'SELECT *'
      'FROM ALGORITMOS'
      'ORDER BY NAME')
    Left = 184
    Top = 144
  end
end
