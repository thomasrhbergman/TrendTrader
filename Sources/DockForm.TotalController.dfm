inherited frmDockFormTotalController: TfrmDockFormTotalController
  Caption = 'Total Controller'
  ClientHeight = 628
  ClientWidth = 1086
  OnDestroy = FormDestroy
  ExplicitWidth = 1102
  ExplicitHeight = 667
  PixelsPerInch = 96
  TextHeight = 13
  object splAutoTrades: TSplitter [0]
    Left = 0
    Top = 147
    Width = 1086
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitLeft = 8
    ExplicitTop = 200
    ExplicitWidth = 1031
  end
  object splOrderStatus: TSplitter [1]
    Left = 0
    Top = 294
    Width = 1086
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 384
    ExplicitWidth = 244
  end
  inherited pnlOptions: TPanel
    Width = 1086
    Height = 22
    TabOrder = 2
    ExplicitWidth = 1086
    ExplicitHeight = 22
    DesignSize = (
      1086
      22)
    inherited btnExportToExcel: TBitBtn
      Left = 1060
      Top = 0
      Width = 22
      Height = 22
      Images = DMImage.vil16
      ExplicitLeft = 1060
      ExplicitTop = 0
      ExplicitWidth = 22
      ExplicitHeight = 22
    end
    inherited btnExportToCSV: TBitBtn
      Left = 1038
      Top = 0
      Width = 22
      Height = 22
      Images = DMImage.vil16
      ExplicitLeft = 1038
      ExplicitTop = 0
      ExplicitWidth = 22
      ExplicitHeight = 22
    end
    inherited btnPrint: TBitBtn
      Left = 1016
      Top = 0
      Width = 22
      Height = 22
      Images = DMImage.vil16
      ExplicitLeft = 1016
      ExplicitTop = 0
      ExplicitWidth = 22
      ExplicitHeight = 22
    end
    inherited btnColumnSettings: TBitBtn
      Left = 994
      Top = 0
      Width = 22
      Height = 22
      Images = DMImage.vil16
      ExplicitLeft = 994
      ExplicitTop = 0
      ExplicitWidth = 22
      ExplicitHeight = 22
    end
    object btnDelete: TBitBtn
      Left = 0
      Top = 0
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aDeleteQualifier
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object btnStart: TBitBtn
      Left = 66
      Top = 0
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aStartQualifier
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
    object btnStop: TBitBtn
      Left = 44
      Top = 0
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aStopQualifier
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
    object btnDeleteAllQualifier: TBitBtn
      Left = 22
      Top = 0
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aDeleteAllQualifier
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
    end
  end
  inherited pnlMain: TPanel
    Top = 22
    Width = 1086
    Height = 5
    Align = alTop
    Visible = False
    ExplicitTop = 22
    ExplicitWidth = 1086
    ExplicitHeight = 5
    inherited vstTree: TVirtualStringTree
      Width = 1086
      Height = 5
      Visible = False
      ExplicitWidth = 1086
      ExplicitHeight = 5
    end
  end
  inline frameQualifiers: TframeQualifiers [4]
    Left = 0
    Top = 27
    Width = 1086
    Height = 120
    Align = alTop
    TabOrder = 0
    ExplicitTop = 27
    ExplicitWidth = 1086
    ExplicitHeight = 120
    inherited vstTree: TVirtualStringTree
      Width = 1086
      Height = 120
      OnFocusChanged = frameQualifiersvstTreeFocusChanged
      ExplicitWidth = 1086
      ExplicitHeight = 120
    end
  end
  inline frameAutoTrades: TframeAutoTrades [5]
    Left = 0
    Top = 174
    Width = 1086
    Height = 120
    Align = alTop
    TabOrder = 3
    ExplicitTop = 174
    ExplicitWidth = 1086
    ExplicitHeight = 120
    inherited vstTree: TVirtualStringTree
      Width = 1086
      Height = 120
      OnFocusChanged = frameAutoTradesvstTreeFocusChanged
      ExplicitWidth = 1086
      ExplicitHeight = 120
    end
  end
  object pnlOptionsAutotrades: TPanel
    Left = 0
    Top = 150
    Width = 1086
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      1086
      24)
    object btnExportToExcelAutotrades: TBitBtn
      Left = 1060
      Top = 1
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aExportToExcelAutotrades
      Anchors = [akTop, akRight]
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object btnExportToCSVAutotrades: TBitBtn
      Left = 1038
      Top = 1
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aExportToCSVAutotrades
      Anchors = [akTop, akRight]
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object btnPrintAutotrades: TBitBtn
      Left = 1016
      Top = 1
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aPrintAutotrades
      Anchors = [akTop, akRight]
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object btnColumnSettingsAutotrades: TBitBtn
      Left = 994
      Top = 1
      Width = 22
      Height = 22
      Action = aColumnSettingsAutotrades
      Anchors = [akTop, akRight]
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object btnDeleteAutotrades: TBitBtn
      Left = 0
      Top = 1
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aDeleteAutotrades
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object btnStartAutotrades: TBitBtn
      Left = 66
      Top = 1
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aStartAutotrade
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
    object btnStopAutotrades: TBitBtn
      Left = 44
      Top = 1
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aStopAutotrade
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
    object cbShowAllAutotrades: TCheckBox
      Left = 924
      Top = 3
      Width = 67
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Show All'
      TabOrder = 7
      OnClick = cbShowAllAutotradesClick
    end
    object btnDeleteAllAutotrades: TBitBtn
      Left = 22
      Top = 1
      Width = 22
      Height = 22
      ParentCustomHint = False
      Action = aDeleteAllAutotrades
      Images = DMImage.vil16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
    end
  end
  object pcOrders: TPageControl
    Left = 0
    Top = 297
    Width = 1086
    Height = 331
    ActivePage = tsActiveOrders
    Align = alClient
    TabOrder = 5
    object tsActiveOrders: TTabSheet
      Caption = 'Active Orders'
      inline frameActiveOrders: TframeActiveOrders
        Left = 0
        Top = 22
        Width = 1078
        Height = 281
        Align = alClient
        TabOrder = 0
        ExplicitTop = 22
        ExplicitWidth = 1078
        ExplicitHeight = 281
        inherited vstTree: TVirtualStringTree
          Width = 1078
          Height = 281
          PopupMenu = pmActiveOrders
          ExplicitWidth = 1078
          ExplicitHeight = 281
        end
      end
      object pnlOptionsActiveOrders: TPanel
        Left = 0
        Top = 0
        Width = 1078
        Height = 22
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          1078
          22)
        object lblOptionsActiveOrders: TLabel
          Left = 40
          Top = 3
          Width = 48
          Height = 13
          Caption = 'Group by:'
          Transparent = True
        end
        object btnExportToExcelActiveOrders: TBitBtn
          Left = 1056
          Top = 0
          Width = 22
          Height = 22
          ParentCustomHint = False
          Action = aExportToExcelActiveOrders
          Anchors = [akTop, akRight]
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object btnExportToCSVActiveOrders: TBitBtn
          Left = 1034
          Top = 0
          Width = 22
          Height = 22
          ParentCustomHint = False
          Action = aExportToCSVActiveOrders
          Anchors = [akTop, akRight]
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object btnPrintActiveOrders: TBitBtn
          Left = 1012
          Top = 0
          Width = 22
          Height = 22
          ParentCustomHint = False
          Action = aPrintActiveOrders
          Anchors = [akTop, akRight]
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object btnColumnSettingsActiveOrders: TBitBtn
          Left = 990
          Top = 0
          Width = 22
          Height = 22
          Action = aColumnSettingsActiveOrders
          Anchors = [akTop, akRight]
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object cbOptionsActiveOrders: TComboBox
          Left = 94
          Top = 0
          Width = 145
          Height = 21
          TabOrder = 4
          OnChange = cbOptionsActiveOrdersChange
        end
        object btnCancelActiveOrder: TBitBtn
          Left = 0
          Top = 0
          Width = 22
          Height = 22
          Action = aCancelActiveOrder
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
        end
        object cbShowAllActiveOrders: TCheckBox
          Left = 920
          Top = 2
          Width = 65
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'Show All'
          TabOrder = 6
          OnClick = cbShowAllActiveOrdersClick
        end
      end
    end
    object tsOrderStatus: TTabSheet
      Caption = 'Order Log'
      ImageIndex = 1
      inline frameOrderStatus: TframeOrderStatus
        Left = 0
        Top = 22
        Width = 1078
        Height = 281
        Align = alClient
        TabOrder = 0
        ExplicitTop = 22
        ExplicitWidth = 1078
        ExplicitHeight = 281
        inherited vstTree: TVirtualStringTree
          Width = 1078
          Height = 281
          ExplicitWidth = 1078
          ExplicitHeight = 281
        end
      end
      object pnlOptionsOrderStatus: TPanel
        Left = 0
        Top = 0
        Width = 1078
        Height = 22
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          1078
          22)
        object lblGroupByOrderStatus: TLabel
          Left = 9
          Top = 3
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
        object lblDateTo: TLabel
          Left = 565
          Top = 4
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
        object lblDateFrom: TLabel
          Left = 329
          Top = 4
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
        object btnExportToExcelOrderStatus: TBitBtn
          Left = 1056
          Top = 0
          Width = 22
          Height = 22
          ParentCustomHint = False
          Action = aExportToExcel
          Anchors = [akTop, akRight]
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object btnExportToCSVOrderStatus: TBitBtn
          Left = 1034
          Top = 0
          Width = 22
          Height = 22
          ParentCustomHint = False
          Action = aExportToCSV
          Anchors = [akTop, akRight]
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object btnPrintOrderStatus: TBitBtn
          Left = 1012
          Top = 0
          Width = 22
          Height = 22
          ParentCustomHint = False
          Action = aPrint
          Anchors = [akTop, akRight]
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object btnColumnSettingsOrderStatus: TBitBtn
          Left = 990
          Top = 0
          Width = 22
          Height = 22
          Action = aColumnSettings
          Anchors = [akTop, akRight]
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object btnLoadFromDBOrderStatus: TBitBtn
          Left = 740
          Top = -1
          Width = 22
          Height = 22
          ParentCustomHint = False
          Action = aLoadFromDBOrderStatus
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
        end
        object cbGroupOrderStatus: TComboBox
          Left = 63
          Top = 0
          Width = 161
          Height = 21
          TabOrder = 5
          OnChange = cbGroupOrderStatusChange
        end
        object cbShowAllOrderStatus: TCheckBox
          Left = 921
          Top = 2
          Width = 67
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'Show All'
          TabOrder = 6
          OnClick = cbShowAllOrderStatusClick
        end
        object cbShowSleepingOrders: TCheckBox
          Left = 777
          Top = 2
          Width = 129
          Height = 17
          Caption = 'Show Sleeping Orders'
          TabOrder = 7
          OnClick = cbShowSleepingOrdersClick
        end
        object edTimeToOrderStatus: TDateTimePicker
          Left = 673
          Top = 0
          Width = 67
          Height = 21
          Date = 44124.000000000000000000
          Time = 0.999988425923220300
          Kind = dtkTime
          TabOrder = 8
        end
        object edDateToOrderStatus: TDateTimePicker
          Left = 579
          Top = 0
          Width = 93
          Height = 21
          Date = 44124.000000000000000000
          Time = 0.571414999998523900
          TabOrder = 9
        end
        object edTimeFromOrderStatus: TDateTimePicker
          Left = 494
          Top = 0
          Width = 67
          Height = 21
          Date = 44124.000000000000000000
          Time = 44124.000000000000000000
          Kind = dtkTime
          TabOrder = 10
        end
        object edDateFromOrderStatus: TDateTimePicker
          Left = 400
          Top = 0
          Width = 93
          Height = 21
          Date = 44124.000000000000000000
          Time = 0.571360081019520300
          TabOrder = 11
        end
      end
    end
  end
  inherited ActionList: TActionList
    Left = 112
    Top = 88
    inherited aExportToExcel: TAction
      Category = 'Qualifiers'
    end
    inherited aExportToCSV: TAction
      Category = 'Qualifiers'
    end
    inherited aPrint: TAction
      Category = 'Qualifiers'
    end
    inherited aColumnSettings: TAction
      Category = 'Qualifiers'
    end
    object aDeleteQualifier: TAction
      Category = 'Qualifiers'
      Hint = 'Delete'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aDeleteQualifierExecute
    end
    object aStopQualifier: TAction
      Category = 'Qualifiers'
      Hint = 'Stop'
      ImageIndex = 50
      ImageName = 'Pause_32x32'
      OnExecute = aStopQualifierExecute
    end
    object aStartQualifier: TAction
      Category = 'Qualifiers'
      Hint = 'Start'
      ImageIndex = 51
      ImageName = 'Play_32x32'
      OnExecute = aStartQualifierExecute
    end
    object aExportToExcelAutotrades: TAction
      Category = 'Autotrades'
      Hint = 'Export To Excel'
      ImageIndex = 16
      ImageName = 'ExportToXLS_32x32'
      OnExecute = aExportToExcelAutotradesExecute
    end
    object aExportToCSVAutotrades: TAction
      Category = 'Autotrades'
      Hint = 'Export to CSV'
      ImageIndex = 17
      ImageName = 'ExportToCSV_32x32'
      OnExecute = aExportToCSVAutotradesExecute
    end
    object aPrintAutotrades: TAction
      Category = 'Autotrades'
      Hint = 'Print'
      ImageIndex = 15
      ImageName = 'Print_32x32'
      OnExecute = aPrintAutotradesExecute
    end
    object aColumnSettingsAutotrades: TAction
      Category = 'Autotrades'
      Hint = 'Column Settings'
      ImageIndex = 36
      ImageName = 'ListBullets_32x32'
      OnExecute = aColumnSettingsAutotradesExecute
    end
    object aExportToExcelActiveOrders: TAction
      Category = 'ActiveOrders'
      Hint = 'Export To Excel'
      ImageIndex = 16
      ImageName = 'ExportToXLS_32x32'
      OnExecute = aExportToExcelActiveOrdersExecute
    end
    object aExportToCSVActiveOrders: TAction
      Category = 'ActiveOrders'
      Hint = 'Export to CSV'
      ImageIndex = 17
      ImageName = 'ExportToCSV_32x32'
      OnExecute = aExportToCSVActiveOrdersExecute
    end
    object aColumnSettingsActiveOrders: TAction
      Category = 'ActiveOrders'
      Hint = 'Column Settings'
      ImageIndex = 36
      ImageName = 'ListBullets_32x32'
      OnExecute = aColumnSettingsActiveOrdersExecute
    end
    object aPrintActiveOrders: TAction
      Category = 'ActiveOrders'
      Hint = 'Print'
      ImageIndex = 15
      ImageName = 'Print_32x32'
      OnExecute = aPrintActiveOrdersExecute
    end
    object aCancelActiveOrder: TAction
      Category = 'ActiveOrders'
      Hint = 'Delete'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aCancelActiveOrderExecute
    end
    object aShowActiveOrder: TAction
      Category = 'ActiveOrders'
      Caption = 'Show Order'
      ShortCut = 16461
      OnExecute = aShowActiveOrderExecute
    end
    object aCancelAllActiveOrder: TAction
      Category = 'ActiveOrders'
      OnExecute = aCancelAllActiveOrderExecute
      OnUpdate = aCancelAllActiveOrderUpdate
    end
    object aExportToExcelOrderStatus: TAction
      Category = 'OrderStatus'
      Caption = 'aExportToExcelOrderStatus'
      Hint = 'Export To Excel'
      ImageIndex = 16
      ImageName = 'ExportToXLS_32x32'
      OnExecute = aExportToExcelOrderStatusExecute
    end
    object aExportToCSVOrderStatus: TAction
      Category = 'OrderStatus'
      Caption = 'aExportToCSVOrderStatus'
      Hint = 'Export to CSV'
      ImageIndex = 17
      ImageName = 'ExportToCSV_32x32'
      OnExecute = aExportToCSVOrderStatusExecute
    end
    object aPrintOrderStatus: TAction
      Category = 'OrderStatus'
      Caption = 'aPrintOrderStatus'
      Hint = 'Print'
      ImageIndex = 15
      ImageName = 'Print_32x32'
      OnExecute = aPrintOrderStatusExecute
    end
    object aColumnSettingsOrderStatus: TAction
      Category = 'OrderStatus'
      Caption = 'aColumnSettingsOrderStatus'
      Hint = 'Column Settings'
      ImageIndex = 36
      ImageName = 'ListBullets_32x32'
      OnExecute = aColumnSettingsOrderStatusExecute
    end
    object aLoadFromDBOrderStatus: TAction
      Category = 'OrderStatus'
      ImageIndex = 32
      ImageName = 'Refresh_32x32'
      OnExecute = aLoadFromDBOrderStatusExecute
    end
    object aDeleteAllQualifier: TAction
      Category = 'Qualifiers'
      Hint = 'Emergency Delete All'
      ImageIndex = 65
      ImageName = 'DeleteList2_32x32'
      ShortCut = 8238
      OnExecute = aDeleteAllQualifierExecute
    end
    object aDeleteAutotrades: TAction
      Category = 'Autotrades'
      Hint = 'Delete'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aDeleteAutotradesExecute
    end
    object aDeleteAllAutotrades: TAction
      Category = 'Autotrades'
      Hint = 'Emergency Delete All'
      ImageIndex = 65
      ImageName = 'DeleteList2_32x32'
      OnExecute = aDeleteAllAutotradesExecute
    end
    object aStartAutotrade: TAction
      Category = 'Autotrades'
      Hint = 'Start'
      ImageIndex = 51
      ImageName = 'Play_32x32'
      OnExecute = aStartAutotradeExecute
    end
    object aStopAutotrade: TAction
      Category = 'Autotrades'
      Hint = 'Stop'
      ImageIndex = 50
      ImageName = 'Pause_32x32'
      OnExecute = aStopAutotradeExecute
    end
    object aArmQualifiers: TAction
      Category = 'Qualifiers'
      Caption = 'Arm'
    end
    object aUnArmQualifiers: TAction
      Category = 'Qualifiers'
      Caption = 'Unarm'
    end
  end
  inherited pmTree: TPopupMenu
    Left = 191
    Top = 88
  end
  object pmActiveOrders: TPopupMenu
    Left = 47
    Top = 432
    object miCancelActiveOrder: TMenuItem
      Action = aCancelActiveOrder
      Caption = 'Cancel Order'
    end
    object miShowActiveOrder: TMenuItem
      Action = aShowActiveOrder
    end
  end
end
