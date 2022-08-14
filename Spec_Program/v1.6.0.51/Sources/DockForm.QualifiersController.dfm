inherited frmDockFormQualifiersController: TfrmDockFormQualifiersController
  Caption = 'Qualifiers Controller'
  ClientHeight = 447
  ClientWidth = 1031
  OnDestroy = FormDestroy
  ExplicitWidth = 1047
  ExplicitHeight = 486
  
  TextHeight = 13
  inherited pnlOptions: TPanel
    Width = 1031
    TabOrder = 2
    ExplicitWidth = 1031
    inherited btnExportToExcel: TBitBtn
      Left = 994
      ExplicitLeft = 994
    end
    inherited btnExportToCSV: TBitBtn
      Left = 958
      ExplicitLeft = 958
    end
    inherited btnPrint: TBitBtn
      Left = 922
      ExplicitLeft = 922
    end
    inherited btnColumnSettings: TBitBtn
      Left = 886
      ExplicitLeft = 886
    end
    object btnDelete: TBitBtn
      Left = 0
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aDelete
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object btnStart: TBitBtn
      Left = 108
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aStart
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
    object btnStop: TBitBtn
      Left = 72
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aStop
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
    object btnDeleteAll: TBitBtn
      Left = 36
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aDeleteAll
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
    end
  end
  inherited pnlMain: TPanel
    Width = 1031
    Height = 5
    Align = alTop
    Visible = False
    ExplicitWidth = 1031
    ExplicitHeight = 5
    inherited vstTree: TVirtualStringTree
      Width = 1031
      Height = 5
      Visible = False
      ExplicitWidth = 1031
      ExplicitHeight = 5
    end
  end
  inline frameQualifiers: TframeQualifiers [2]
    Left = 0
    Top = 43
    Width = 1031
    Height = 404
    Align = alClient
    TabOrder = 0
    ExplicitTop = 43
    ExplicitWidth = 1031
    ExplicitHeight = 404
    inherited vstTree: TVirtualStringTree
      Width = 1031
      Height = 404
      ExplicitWidth = 1031
      ExplicitHeight = 404
    end
  end
  inherited ActionList: TActionList [3]
    Left = 48
    Top = 176
    object aDelete: TAction
      Hint = 'Delete'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aDeleteExecute
    end
    object aStop: TAction
      Hint = 'Stop'
      ImageIndex = 50
      ImageName = 'Pause_32x32'
      OnExecute = aStopExecute
    end
    object aStart: TAction
      Hint = 'Start'
      ImageIndex = 51
      ImageName = 'Play_32x32'
      OnExecute = aStartExecute
    end
    object aDeleteAll: TAction
      Hint = 'Emergency Delete'
      ImageIndex = 65
      ImageName = 'DeleteList2_32x32'
      ShortCut = 8238
      OnExecute = aDeleteAllExecute
    end
  end
  inherited pmTree: TPopupMenu
    Left = 23
  end
end
