inherited frmDockFormActiveOrders: TfrmDockFormActiveOrders
  Caption = 'Active Orders'
  ClientHeight = 393
  ClientWidth = 1021
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 1037
  ExplicitHeight = 432
  
  TextHeight = 13
  object lblOptions: TLabel [0]
    Left = 120
    Top = 11
    Width = 48
    Height = 13
    Caption = 'Group by:'
    Transparent = True
  end
  inherited pnlOptions: TPanel
    Width = 1021
    ExplicitWidth = 1021
    inherited btnExportToExcel: TBitBtn
      Left = 984
      ExplicitLeft = 984
    end
    inherited btnExportToCSV: TBitBtn
      Left = 948
      ExplicitLeft = 948
    end
    inherited btnPrint: TBitBtn
      Left = 912
      ExplicitLeft = 912
    end
    inherited btnColumnSettings: TBitBtn
      Left = 876
      ExplicitLeft = 876
    end
    object btnGlobalCancel: TBitBtn
      Left = 36
      Top = 1
      Width = 36
      Height = 36
      Action = aGlobalCancel
      ImageIndex = 65
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
  end
  inherited pnlMain: TPanel
    Width = 1021
    Height = 5
    Align = alTop
    Visible = False
    ExplicitWidth = 1021
    ExplicitHeight = 5
    inherited vstTree: TVirtualStringTree
      Width = 1021
      Height = 5
      Header.Height = 16
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
      ExplicitWidth = 1021
      ExplicitHeight = 5
    end
  end
  inline frameActiveOrders: TframeActiveOrders [3]
    Left = 0
    Top = 43
    Width = 1021
    Height = 350
    Align = alClient
    TabOrder = 2
    ExplicitTop = 43
    ExplicitWidth = 1021
    ExplicitHeight = 350
    inherited vstTree: TVirtualStringTree
      Width = 1021
      Height = 350
      ExplicitWidth = 1021
      ExplicitHeight = 350
    end
  end
  object cbOptions: TComboBox [4]
    Left = 174
    Top = 8
    Width = 145
    Height = 21
    TabOrder = 3
    OnChange = cbOptionsChange
  end
  object btnCancelOrder: TBitBtn [5]
    Left = 0
    Top = 1
    Width = 36
    Height = 36
    Action = aCancelOrder
    Images = DMImage.vil32
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
  end
  inherited ActionList: TActionList [6]
    object aCancelOrder: TAction
      Hint = 'Cancel order'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      ShortCut = 16392
      OnExecute = aCancelOrderExecute
      OnUpdate = aShowOrderUpdate
    end
    object aCancelAllOrders: TAction
      Caption = 'aCancelAllOrders'
      OnExecute = aCancelAllOrdersExecute
      OnUpdate = aCancelAllOrdersUpdate
    end
    object aShowOrder: TAction
      Caption = 'Show Order'
      ShortCut = 16461
      OnExecute = aShowOrderExecute
      OnUpdate = aShowOrderUpdate
    end
    object aGlobalCancel: TAction
      Hint = 'Global Cancel'
      ImageIndex = 65
      ImageName = 'DeleteList2_32x32'
      OnExecute = aGlobalCancelExecute
      OnUpdate = aGlobalCancelUpdate
    end
  end
  inherited pmTree: TPopupMenu
    object miCancelOrder: TMenuItem
      Action = aCancelOrder
      Caption = 'Cancel Order'
    end
    object miShowOrder: TMenuItem
      Action = aShowOrder
    end
  end
end
