object frmExecuteScript: TfrmExecuteScript
  Left = 0
  Top = 0
  Caption = 'Execute Script'
  ClientHeight = 654
  ClientWidth = 961
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object splLog: TSplitter
    Left = 0
    Top = 536
    Width = 961
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 273
    ExplicitWidth = 207
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 961
    Height = 38
    Align = alTop
    TabOrder = 0
    object btnOpenScript: TBitBtn
      Left = 1
      Top = 1
      Width = 36
      Height = 36
      Action = aOpenScript
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 0
    end
    object btnExecuteScript: TBitBtn
      Left = 73
      Top = 1
      Width = 36
      Height = 36
      Action = aExecuteScript
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 1
    end
    object btnSaveScript: TBitBtn
      Left = 37
      Top = 1
      Width = 36
      Height = 36
      Action = aSaveScript
      Images = DMImage.vil32
      TabOrder = 2
    end
  end
  object memScript: TMemo
    Left = 0
    Top = 38
    Width = 961
    Height = 498
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object memLog: TMemo
    Left = 0
    Top = 539
    Width = 961
    Height = 115
    Align = alBottom
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 48
    Top = 112
    object aOpenScript: TAction
      Hint = 'Open Script'
      ImageIndex = 5
      ImageName = 'Open_32x32'
      ShortCut = 16463
      OnExecute = aOpenScriptExecute
    end
    object aExecuteScript: TAction
      Hint = 'Execute Script'
      ImageIndex = 12
      ImageName = 'lightning'
      OnExecute = aExecuteScriptExecute
      OnUpdate = aExecuteScriptUpdate
    end
    object aSaveScript: TAction
      Hint = 'Save Script'
      ImageIndex = 10
      ImageName = 'Save_32x32'
      ShortCut = 16467
      OnExecute = aSaveScriptExecute
      OnUpdate = aExecuteScriptUpdate
    end
  end
  object OpenDialog: TFileOpenDialog
    DefaultExtension = '*.sql'
    FavoriteLinks = <
      item
      end>
    FileTypes = <
      item
        DisplayName = 'SQL-files'
        FileMask = '*.sql'
      end
      item
        DisplayName = 'Text Files'
        FileMask = '*.txt'
      end
      item
        DisplayName = 'All Files'
        FileMask = '*.*'
      end>
    Options = [fdoOverWritePrompt, fdoFileMustExist, fdoCreatePrompt]
    Left = 94
    Top = 240
  end
  object FileSaveDialog: TFileSaveDialog
    DefaultExtension = '*.sql'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'SQL-files'
        FileMask = '*.sql'
      end
      item
        DisplayName = 'Text Files'
        FileMask = '*.txt'
      end
      item
        DisplayName = 'All Files'
        FileMask = '*.*'
      end>
    FileTypeIndex = 0
    Options = [fdoOverWritePrompt, fdoPathMustExist]
    Left = 96
    Top = 296
  end
  object FBQuery: TFDQuery
    Left = 584
    Top = 208
  end
end
