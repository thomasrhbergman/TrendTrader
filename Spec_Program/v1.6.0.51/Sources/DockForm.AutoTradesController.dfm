inherited frmDockFormAutoTradesController: TfrmDockFormAutoTradesController
  Caption = 'AutoTrades Controller'
  ClientHeight = 398
  ClientWidth = 1089
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 1105
  ExplicitHeight = 437
  
  TextHeight = 13
  inherited pnlOptions: TPanel
    Width = 1089
    TabOrder = 2
    ExplicitWidth = 1089
    object lblGroupBy: TLabel [0]
      Left = 152
      Top = 11
      Width = 48
      Height = 13
      Caption = 'Group by:'
    end
    inherited btnExportToExcel: TBitBtn
      Left = 1052
      ExplicitLeft = 1052
    end
    inherited btnExportToCSV: TBitBtn
      Left = 1016
      ExplicitLeft = 1016
    end
    inherited btnPrint: TBitBtn
      Left = 980
      ExplicitLeft = 980
    end
    inherited btnColumnSettings: TBitBtn
      Left = 944
      ExplicitLeft = 944
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
    object cbGroup: TComboBox
      Left = 206
      Top = 8
      Width = 145
      Height = 21
      TabOrder = 7
      OnChange = cbGroupChange
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
      TabOrder = 8
    end
  end
  inherited pnlMain: TPanel
    Width = 1089
    Height = 5
    Align = alTop
    Visible = False
    ExplicitWidth = 1089
    ExplicitHeight = 5
    inherited vstTree: TVirtualStringTree
      Width = 1089
      Height = 5
      Visible = False
      ExplicitWidth = 1089
      ExplicitHeight = 5
    end
  end
  inline frameAutoTrades: TframeAutoTrades [2]
    Left = 0
    Top = 43
    Width = 1089
    Height = 355
    Align = alClient
    TabOrder = 0
    ExplicitTop = 43
    ExplicitWidth = 1089
    ExplicitHeight = 355
    inherited vstTree: TVirtualStringTree
      Width = 1089
      Height = 355
      ExplicitWidth = 1089
      ExplicitHeight = 355
    end
  end
  inherited ActionList: TActionList [3]
    Left = 120
    Top = 200
    object aDelete: TAction
      Hint = 'Break'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aDeleteExecute
    end
    object aDeleteAll: TAction
      Hint = 'Emergency Delete All'
      ImageIndex = 65
      ImageName = 'DeleteList2_32x32'
      OnExecute = aDeleteAllExecute
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
  end
  inherited pmTree: TPopupMenu
    Left = 119
  end
end
