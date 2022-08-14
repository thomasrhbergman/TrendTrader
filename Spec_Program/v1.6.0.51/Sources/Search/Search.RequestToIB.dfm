inherited frmRequestToIB: TfrmRequestToIB
  ActiveControl = frameRealtimeFeeds.vstTree
  PixelsPerInch = 96
  TextHeight = 13
  inherited sbMain: TStatusBar
    Panels = <
      item
        Text = 'Status:'
        Width = 45
      end
      item
        Width = 600
      end
      item
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
        Text = 'Head Timestamp: '
        Width = 210
      end
      item
        Text = 'Requested at:'
        Width = 130
      end
      item
        Text = 'Finished at:'
        Width = 130
      end
      item
        Text = 'Count: '
        Width = 90
      end
      item
        Width = 50
      end>
  end
  inherited pnlSearchParam: TPanel
    TabOrder = 2
    inherited pcBrokers: TPageControl
      inherited tsNordNetBroker: TTabSheet
        TabVisible = False
      end
      inherited tsTestBroker: TTabSheet
        TabVisible = False
      end
    end
  end
  inherited pnlCentral: TPanel
    TabOrder = 0
    inherited splContractDetails: TSplitter
      Color = clWhite
      ParentColor = False
      ExplicitWidth = 835
    end
    inherited splCategory: TSplitter
      Left = 654
      ExplicitLeft = 510
      ExplicitHeight = 498
    end
    inherited pnlInstruments: TPanel
      Width = 654
      ExplicitWidth = 654
      object splInstruments: TSplitter [0]
        Left = 0
        Top = 221
        Width = 654
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        Color = clWhite
        ParentColor = False
        ExplicitLeft = -6
        ExplicitTop = 189
        ExplicitWidth = 613
      end
      inherited splRealtimeFeeds: TSplitter
        Top = 125
        Width = 654
        ExplicitTop = 82
        ExplicitWidth = 764
      end
      inherited vstInstruments: TVirtualStringTree
        Width = 654
        Height = 89
        TabOrder = 3
        ExplicitWidth = 654
        ExplicitHeight = 89
      end
      inherited pnlInstrumentsTop: TPanel
        Width = 654
        ExplicitWidth = 654
        inherited btnExportToExcel: TBitBtn
          Left = 686
          ExplicitLeft = 686
        end
        inherited btnInstExportToCSV: TBitBtn
          Left = 650
          ExplicitLeft = 650
        end
        inherited btnInstPrint: TBitBtn
          Left = 614
          ExplicitLeft = 614
        end
        inherited btnColumnSettings: TBitBtn
          Left = 578
          ExplicitLeft = 578
        end
      end
      inherited pnlRealtimeFeeds: TGroupBox
        Top = 130
        Width = 654
        Height = 91
        ExplicitTop = 130
        ExplicitWidth = 654
        ExplicitHeight = 91
        inherited frameRealtimeFeeds: TframeRealtimeFeeds
          Width = 650
          Height = 74
          ExplicitWidth = 650
          ExplicitHeight = 74
          inherited vstTree: TVirtualStringTree
            Width = 650
            Height = 37
            ExplicitWidth = 650
            ExplicitHeight = 37
          end
          inherited pnlOptions: TPanel
            Width = 650
            ExplicitWidth = 650
            inherited btnExportToExcel: TBitBtn
              Left = 615
              ExplicitLeft = 615
            end
            inherited btnExportToCSV: TBitBtn
              Left = 579
              ExplicitLeft = 579
            end
            inherited btnPrint: TBitBtn
              Left = 543
              ExplicitLeft = 543
            end
            inherited btnColumnSettings: TBitBtn
              Left = 507
              ExplicitLeft = 507
            end
          end
        end
      end
      object pnlHistoricalData: TGroupBox
        Left = 0
        Top = 224
        Width = 654
        Height = 200
        Align = alBottom
        Caption = 'Historical Data'
        TabOrder = 0
        object pnlParams: TPanel
          Left = 2
          Top = 15
          Width = 650
          Height = 46
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object lblDuration: TLabel
            Left = 431
            Top = 2
            Width = 45
            Height = 13
            Alignment = taRightJustify
            Caption = 'Duration:'
          end
          object lblValidBarSize: TLabel
            Left = 337
            Top = 2
            Width = 62
            Height = 13
            Alignment = taRightJustify
            Caption = 'Valid bar size'
          end
          object lblDateFrom: TLabel
            Left = 2
            Top = 2
            Width = 48
            Height = 13
            Alignment = taRightJustify
            Caption = 'Date from'
          end
          object lblWhatToShow: TLabel
            Left = 598
            Top = 2
            Width = 74
            Height = 13
            Alignment = taRightJustify
            Caption = 'What To Show:'
          end
          object lblDateTo: TLabel
            Left = 169
            Top = 2
            Width = 10
            Height = 13
            Caption = 'to'
          end
          object dtDateBegin: TDateTimePicker
            Left = 2
            Top = 18
            Width = 85
            Height = 22
            Date = 44424.000000000000000000
            Time = 44424.000000000000000000
            TabOrder = 0
          end
          object dtTimeBegin: TDateTimePicker
            Left = 89
            Top = 18
            Width = 73
            Height = 22
            Date = 44424.000000000000000000
            Time = 0.416666666664241300
            Kind = dtkTime
            TabOrder = 1
          end
          object dtDateEnd: TDateTimePicker
            Left = 169
            Top = 18
            Width = 85
            Height = 22
            Date = 44424.000000000000000000
            Time = 44424.000000000000000000
            TabOrder = 2
          end
          object dtTimeEnd: TDateTimePicker
            Left = 257
            Top = 18
            Width = 73
            Height = 22
            Date = 44424.000000000000000000
            Time = 0.770833333335758700
            Kind = dtkTime
            TabOrder = 6
          end
          object cbValidBarSize: TComboBox
            Left = 337
            Top = 18
            Width = 85
            Height = 21
            TabOrder = 7
          end
          object cbDurationTimeUnits: TComboBox
            Left = 430
            Top = 18
            Width = 85
            Height = 21
            TabOrder = 3
          end
          object cbWhatToShow: TComboBox
            Left = 598
            Top = 18
            Width = 160
            Height = 21
            TabOrder = 5
          end
          object edDuration: TNumberBox
            Left = 517
            Top = 18
            Width = 73
            Height = 21
            TabOrder = 4
            Value = 1.000000000000000000
            SpinButtonOptions.Placement = nbspCompact
            UseMouseWheel = True
          end
        end
        inline frameHistoricalData: TframeHistoricalData
          Left = 2
          Top = 61
          Width = 650
          Height = 137
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          ExplicitLeft = 2
          ExplicitTop = 61
          ExplicitWidth = 650
          ExplicitHeight = 137
          inherited vstTree: TVirtualStringTree
            Width = 650
            Height = 100
            ExplicitLeft = 128
            ExplicitWidth = 650
            ExplicitHeight = 100
          end
          inherited pnlOptions: TPanel
            Width = 650
            ExplicitWidth = 650
            inherited btnExportToExcel: TBitBtn
              Left = 613
              ExplicitLeft = 613
            end
            inherited btnExportToCSV: TBitBtn
              Left = 577
              ExplicitLeft = 577
            end
            inherited btnPrint: TBitBtn
              Left = 541
              ExplicitLeft = 541
            end
            inherited btnColumnSettings: TBitBtn
              Left = 505
              ExplicitLeft = 505
            end
            inherited btnInfo: TBitBtn
              Left = 469
              ExplicitLeft = 469
            end
          end
        end
      end
    end
    inherited pnlCategory: TPanel
      Left = 659
      Width = 279
      ExplicitLeft = 659
      ExplicitWidth = 279
      inherited pnlCategoryTop: TPanel
        Width = 277
        ExplicitWidth = 277
        inherited btnColumnSettingsLists: TBitBtn
          Left = 240
          ExplicitLeft = 240
        end
        inherited btnClearSearchText: TBitBtn
          Left = 255
          ExplicitLeft = 255
        end
        inherited edtSearchLists: TEdit
          Width = 174
          ExplicitWidth = 174
        end
      end
      inherited vstSokidLists: TVirtualStringTree
        Width = 277
        AccessibleName = 'Historical Data'
        ExplicitWidth = 277
      end
    end
  end
  inherited ActionList: TActionList
    object aSubscribeHistoricalData: TAction
      ImageIndex = 12
      ImageName = 'lightning'
      OnExecute = aSubscribeHistoricalDataExecute
      OnUpdate = aSubscribeHistoricalDataUpdate
    end
  end
end
