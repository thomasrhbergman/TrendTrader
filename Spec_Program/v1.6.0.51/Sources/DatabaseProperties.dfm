object frmDatabaseProperties: TfrmDatabaseProperties
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Databases Properties'
  ClientHeight = 383
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblProtocol: TLabel
    Left = 103
    Top = 8
    Width = 102
    Height = 16
    Alignment = taRightJustify
    Caption = 'Server / Protocol:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblServerName: TLabel
    Left = 125
    Top = 32
    Width = 80
    Height = 16
    Alignment = taRightJustify
    Caption = 'Server Name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblPort: TLabel
    Left = 177
    Top = 56
    Width = 28
    Height = 16
    Alignment = taRightJustify
    Caption = 'Port:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblPathToStockXRobot: TLabel
    Left = 37
    Top = 80
    Width = 168
    Height = 16
    Alignment = taRightJustify
    Caption = 'Path To STOCKXROBOT.FDB:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblPathToDB_FEED: TLabel
    Left = 75
    Top = 128
    Width = 130
    Height = 16
    Alignment = taRightJustify
    Caption = 'Path To DB_FEED.FDB:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblConnectionStringStockXRobot: TLabel
    Left = 99
    Top = 104
    Width = 106
    Height = 16
    Alignment = taRightJustify
    Caption = 'Connection String:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblConnectionStringDBFeed: TLabel
    Left = 99
    Top = 152
    Width = 106
    Height = 16
    Alignment = taRightJustify
    Caption = 'Connection String:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblUserName: TLabel
    Left = 137
    Top = 176
    Width = 68
    Height = 16
    Alignment = taRightJustify
    Caption = 'User Name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblPassword: TLabel
    Left = 337
    Top = 176
    Width = 60
    Height = 16
    Alignment = taRightJustify
    Caption = 'Password:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblAddConnectParameters: TLabel
    Left = 24
    Top = 202
    Width = 181
    Height = 16
    Alignment = taRightJustify
    Caption = 'Additional Connect Parameters:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblDriverID: TLabel
    Left = 150
    Top = 320
    Width = 55
    Height = 16
    Alignment = taRightJustify
    Caption = 'Driver ID:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 346
    Width = 633
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 326
    object btnOk: TBitBtn
      Left = 525
      Top = 0
      Width = 106
      Height = 36
      Caption = 'Ok'
      Default = True
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
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 417
      Top = 0
      Width = 106
      Height = 36
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
    object btnTestConnect: TBitBtn
      Left = 292
      Top = 0
      Width = 110
      Height = 36
      Action = aTestConnect
      Caption = 'Test Connect'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 2
    end
    object btnCleanDatabases: TBitBtn
      Left = 147
      Top = 0
      Width = 145
      Height = 36
      Action = aCleanDatabases
      Caption = 'Clean Databases'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 3
    end
    object btnRunSQLScript: TBitBtn
      Left = 2
      Top = 0
      Width = 145
      Height = 36
      Action = aRunSQLScript
      Caption = 'Run Script'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 4
    end
  end
  object cbProtocol: TComboBox
    Left = 210
    Top = 7
    Width = 120
    Height = 21
    TabOrder = 1
    OnChange = cbProtocolChange
  end
  object cbServerName: TComboBox
    Left = 210
    Top = 31
    Width = 120
    Height = 21
    TabOrder = 2
    OnChange = OnConnectionStringChange
  end
  object cbPort: TComboBox
    Left = 210
    Top = 55
    Width = 120
    Height = 21
    TabOrder = 3
    OnChange = OnConnectionStringChange
  end
  object edPathToStockXRobot: TButtonedEdit
    Left = 210
    Top = 79
    Width = 420
    Height = 21
    Images = DMImage.vil16
    ReadOnly = True
    RightButton.DisabledImageIndex = 1
    RightButton.HotImageIndex = 5
    RightButton.HotImageName = 'Open_32x32'
    RightButton.ImageIndex = 5
    RightButton.ImageName = 'Open_32x32'
    RightButton.PressedImageIndex = 5
    RightButton.PressedImageName = 'Open_32x32'
    RightButton.Visible = True
    TabOrder = 4
    OnChange = OnConnectionStringChange
    OnRightButtonClick = edPathToStockXRobotRightButtonClick
  end
  object edPathToDBFeed: TButtonedEdit
    Left = 210
    Top = 127
    Width = 420
    Height = 21
    Images = DMImage.vil16
    ReadOnly = True
    RightButton.DisabledImageIndex = 5
    RightButton.DisabledImageName = 'Open_32x32'
    RightButton.HotImageIndex = 5
    RightButton.HotImageName = 'Open_32x32'
    RightButton.ImageIndex = 5
    RightButton.ImageName = 'Open_32x32'
    RightButton.PressedImageIndex = 5
    RightButton.PressedImageName = 'Open_32x32'
    RightButton.Visible = True
    TabOrder = 5
    OnChange = OnConnectionStringChange
    OnRightButtonClick = edPathToDBFeedRightButtonClick
  end
  object edConnectionStringStockXRobot: TEdit
    Left = 210
    Top = 103
    Width = 420
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 6
  end
  object edConnectionStringDBFeed: TEdit
    Left = 210
    Top = 151
    Width = 420
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 7
  end
  object edUserName: TEdit
    Left = 210
    Top = 175
    Width = 121
    Height = 21
    TabOrder = 8
    Text = 'SYSDBA'
  end
  object edPassword: TEdit
    Left = 402
    Top = 175
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 9
    Text = 'masterkey'
  end
  object memAddConnectParameters: TMemo
    Left = 211
    Top = 202
    Width = 312
    Height = 111
    TabOrder = 10
  end
  object edDriverID: TEdit
    Left = 211
    Top = 319
    Width = 120
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 11
  end
  object OpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileName = 'D:\Download'
    FileTypes = <
      item
        DisplayName = ''
      end>
    Options = [fdoFileMustExist]
    Left = 94
    Top = 240
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 24
    Top = 112
    object aCleanDatabases: TAction
      Caption = 'Clean Databases'
      ImageIndex = 59
      ImageName = 'Clear_32x32'
      OnExecute = aCleanDatabasesExecute
    end
    object aTestConnect: TAction
      Caption = 'Test Connect'
      OnExecute = aTestConnectExecute
    end
    object aRunSQLScript: TAction
      Caption = 'Run Script'
      ImageIndex = 64
      ImageName = 'sql'
      OnExecute = aRunSQLScriptExecute
    end
  end
  object FDConnection: TFDConnection
    Left = 32
    Top = 240
  end
end
