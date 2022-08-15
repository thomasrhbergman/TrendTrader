inherited frmDockFormOrderStatus: TfrmDockFormOrderStatus
  Caption = 'Order Status'
  ClientHeight = 568
  ClientWidth = 1091
  Position = poDesigned
  OnCreate = FormCreate
  ExplicitWidth = 1107
  ExplicitHeight = 607
  
  TextHeight = 13
  inherited pnlOptions: TPanel
    Width = 1091
    ExplicitWidth = 1091
    object lblDateTo: TLabel [0]
      Left = 716
      Top = 12
      Width = 10
      Height = 13
      Caption = 'to'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblDateFrom: TLabel [1]
      Left = 480
      Top = 12
      Width = 66
      Height = 13
      Caption = 'Load From DB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblGroupBy: TLabel [2]
      Left = 4
      Top = 11
      Width = 48
      Height = 13
      Caption = 'Group by:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    inherited btnExportToExcel: TBitBtn
      Left = 1054
      ExplicitLeft = 1054
    end
    inherited btnExportToCSV: TBitBtn
      Left = 1018
      ExplicitLeft = 1018
    end
    inherited btnPrint: TBitBtn
      Left = 982
      ExplicitLeft = 982
    end
    inherited btnColumnSettings: TBitBtn
      Left = 946
      ExplicitLeft = 946
    end
    object btnLoadFromDB: TBitBtn
      Left = 893
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aLoadFromDB
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object edDateTo: TDateTimePicker
      Left = 731
      Top = 8
      Width = 93
      Height = 21
      Date = 44124.000000000000000000
      Time = 0.571414999998523900
      TabOrder = 5
    end
    object edDateFrom: TDateTimePicker
      Left = 552
      Top = 8
      Width = 93
      Height = 21
      Date = 44124.000000000000000000
      Time = 0.571360081019520300
      TabOrder = 6
    end
    object cbGroup: TComboBox
      Left = 55
      Top = 8
      Width = 161
      Height = 21
      TabOrder = 7
      OnChange = cbGroupChange
    end
    object cbShowSleepingOrders: TCheckBox
      Left = 338
      Top = 10
      Width = 123
      Height = 17
      Caption = 'Show Sleeping Orders'
      TabOrder = 8
      OnClick = cbShowSleepingOrdersClick
    end
    object edTimeFrom: TDateTimePicker
      Left = 646
      Top = 8
      Width = 67
      Height = 21
      Date = 44124.000000000000000000
      Time = 44124.000000000000000000
      Kind = dtkTime
      TabOrder = 9
    end
    object edTimeTo: TDateTimePicker
      Left = 825
      Top = 8
      Width = 67
      Height = 21
      Date = 44124.000000000000000000
      Time = 0.999988425923220300
      Kind = dtkTime
      TabOrder = 10
    end
  end
  inherited pnlMain: TPanel
    Width = 1091
    Height = 5
    Align = alTop
    Visible = False
    ExplicitWidth = 1091
    ExplicitHeight = 5
    inherited vstTree: TVirtualStringTree
      Width = 1091
      Height = 5
      Images = nil
      ParentFont = False
      ScrollBarOptions.AlwaysVisible = True
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus]
      ExplicitWidth = 1091
      ExplicitHeight = 5
    end
  end
  inline frameOrderStatus: TframeOrderStatus [2]
    Left = 0
    Top = 43
    Width = 1091
    Height = 525
    Align = alClient
    TabOrder = 2
    ExplicitTop = 43
    ExplicitWidth = 1091
    ExplicitHeight = 525
    inherited vstTree: TVirtualStringTree
      Width = 1091
      Height = 525
      ExplicitWidth = 1091
      ExplicitHeight = 525
    end
  end
  inherited ActionList: TActionList [3]
    Left = 208
    Top = 320
    object aShowOrderStatus: TAction
      Caption = 'Show Order Status'
      ShortCut = 16467
      OnUpdate = aShowOrderStatusUpdate
    end
    object aLoadFromDB: TAction
      Hint = 'Load From DB'
      ImageIndex = 32
      ImageName = 'Refresh_32x32'
      OnExecute = aLoadFromDBExecute
    end
  end
  inherited pmTree: TPopupMenu
    Left = 119
    Top = 320
    object miShowOrderStatus: TMenuItem
      Action = aShowOrderStatus
    end
  end
end
