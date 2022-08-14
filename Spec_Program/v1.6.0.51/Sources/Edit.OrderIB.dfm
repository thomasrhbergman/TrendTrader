object frmEditOrderIB: TfrmEditOrderIB
  Left = 0
  Top = 0
  Anchors = [akTop, akRight]
  Caption = 'Instrument'
  ClientHeight = 537
  ClientWidth = 784
  Color = clBtnFace
  Constraints.MinHeight = 540
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 496
    Width = 784
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOk: TBitBtn
      Left = 615
      Top = 0
      Width = 162
      Height = 41
      Action = aSave
      Caption = 'Ok'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 505
      Top = 0
      Width = 109
      Height = 41
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ModalResult = 2
      ParentFont = False
      TabOrder = 1
    end
    object btnStatus: TBitBtn
      Left = 1
      Top = 0
      Width = 90
      Height = 41
      Action = aOrderStatus
      Caption = 'Status'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object btnCancelOrder: TBitBtn
      Left = 359
      Top = 0
      Width = 147
      Height = 41
      Action = aCancelOrder
      Caption = 'Cancel Order'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
    end
    object btnBuy: TBitBtn
      Left = 360
      Top = 0
      Width = 145
      Height = 41
      Action = aBuy
      Caption = 'Buy Now'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      NumGlyphs = 2
      ParentFont = False
      TabOrder = 4
    end
    object btnMakeDefault: TBitBtn
      Left = 91
      Top = 0
      Width = 90
      Height = 41
      Action = aMakeDefault
      Caption = 'Make default'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object btnShowGlobalSettings: TBitBtn
      Left = 181
      Top = 0
      Width = 90
      Height = 41
      Action = aShowGlobalSettings
      Caption = 'PreC'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 6
    end
    object btnAddNode: TBitBtn
      Left = 271
      Top = 0
      Width = 90
      Height = 41
      Caption = 'Add'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 34
      ImageName = 'AddFooter_32x32'
      Images = DMImage.vil32
      ParentFont = False
      PopupMenu = pmAddNode
      TabOrder = 7
      OnClick = btnAddNodeClick
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 325
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblLastPrice: TLabel
      Left = 95
      Top = 112
      Width = 70
      Height = 13
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblLastPriceCaption: TLabel
      Left = 40
      Top = 112
      Width = 50
      Height = 13
      Alignment = taRightJustify
      Caption = 'Last Price:'
    end
    object lblSecureType: TLabel
      Left = 29
      Top = 150
      Width = 61
      Height = 13
      Caption = 'SecureType:'
    end
    object lblFilled: TLabel
      Left = 268
      Top = 112
      Width = 70
      Height = 13
      AutoSize = False
    end
    object lblSell: TLabel
      Left = 192
      Top = 3
      Width = 29
      Height = 23
      Caption = 'Sell'
      FocusControl = rbSell
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = rbSellClick
    end
    object lblBuy: TLabel
      Left = 121
      Top = 3
      Width = 31
      Height = 23
      Caption = 'Buy'
      FocusControl = rbBuy
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = rbBuyClick
    end
    object lblOrderType: TLabel
      Left = 31
      Top = 171
      Width = 59
      Height = 13
      Alignment = taRightJustify
      Caption = 'Order Type:'
    end
    object lblFilledCaption: TLabel
      Left = 237
      Top = 112
      Width = 28
      Height = 13
      Alignment = taRightJustify
      Caption = 'Filled:'
    end
    object lblTypeCaption: TLabel
      Left = 195
      Top = 74
      Width = 70
      Height = 13
      Alignment = taRightJustify
      Caption = 'Security Type:'
    end
    object lblType: TLabel
      Left = 268
      Top = 74
      Width = 70
      Height = 13
      AutoSize = False
    end
    object lblCurrencyCaption: TLabel
      Left = 42
      Top = 74
      Width = 48
      Height = 13
      Alignment = taRightJustify
      Caption = 'Currency:'
    end
    object lblCurrency: TLabel
      Left = 95
      Top = 74
      Width = 80
      Height = 13
      AutoSize = False
    end
    object lblExchangeCaption: TLabel
      Left = 39
      Top = 93
      Width = 51
      Height = 13
      Alignment = taRightJustify
      Caption = 'Exchange:'
    end
    object lblExchange: TLabel
      Left = 95
      Top = 93
      Width = 80
      Height = 13
      AutoSize = False
    end
    object lblDescription: TLabel
      Left = 33
      Top = 32
      Width = 57
      Height = 13
      Alignment = taRightJustify
      Caption = 'Description:'
    end
    object lblOrderIBId: TLabel
      Left = 268
      Top = 55
      Width = 70
      Height = 13
      AutoSize = False
    end
    object lblConIdCaption: TLabel
      Left = 30
      Top = 55
      Width = 60
      Height = 13
      Alignment = taRightJustify
      Caption = 'Contract ID:'
    end
    object lblConId: TLabel
      Left = 95
      Top = 55
      Width = 80
      Height = 13
      AutoSize = False
    end
    object lblOrderIBIdCaption: TLabel
      Left = 210
      Top = 55
      Width = 55
      Height = 13
      Alignment = taRightJustify
      Caption = 'Order IBId:'
    end
    object lblMultiplierCaption: TLabel
      Left = 219
      Top = 93
      Width = 46
      Height = 13
      Alignment = taRightJustify
      Caption = 'Multiplier:'
    end
    object lblMultiplier: TLabel
      Left = 268
      Top = 93
      Width = 70
      Height = 13
      AutoSize = False
    end
    object lblTickSize: TLabel
      Left = 95
      Top = 131
      Width = 70
      Height = 13
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblTickSizeCaption: TLabel
      Left = 46
      Top = 131
      Width = 44
      Height = 13
      Alignment = taRightJustify
      Caption = 'Tick Size:'
    end
    object pnlAdvanced: TPanel
      Left = 407
      Top = 0
      Width = 377
      Height = 325
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 4
      object lblAdvancedOrderType: TLabel
        Left = 6
        Top = 33
        Width = 110
        Height = 13
        Alignment = taRightJustify
        Caption = 'Advanced Order Type:'
      end
      object lblScope: TLabel
        Left = 83
        Top = 80
        Width = 33
        Height = 13
        Alignment = taRightJustify
        Caption = 'Scope:'
      end
      object lblTimeInForce: TLabel
        Left = 47
        Top = 57
        Width = 69
        Height = 13
        Alignment = taRightJustify
        Caption = 'Time In Force:'
      end
      object lblOcaName: TLabel
        Left = 36
        Top = 104
        Width = 80
        Height = 13
        Alignment = taRightJustify
        Caption = 'Group ID (OCA):'
      end
      object lblStatus: TLabel
        Left = 0
        Top = 0
        Width = 377
        Height = 23
        Align = alTop
        Alignment = taCenter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 6
      end
      object lblTriggerMethod: TLabel
        Left = 123
        Top = 199
        Width = 77
        Height = 13
        Alignment = taRightJustify
        Caption = 'Trigger Method:'
      end
      object pnlOrderStop: TPanel
        Left = 206
        Top = 268
        Width = 162
        Height = 22
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 0
        object dtpTimeStop: TDateTimePicker
          Left = 89
          Top = 0
          Width = 73
          Height = 22
          Date = 42459.000000000000000000
          Time = 0.776186967603280200
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          Kind = dtkTime
          ParentFont = False
          TabOrder = 0
          Visible = False
        end
        object dtpDateStop: TDateTimePicker
          Left = 0
          Top = 0
          Width = 85
          Height = 22
          Date = 42459.000000000000000000
          Time = 0.776186967603280200
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          Visible = False
        end
      end
      object pnlOrderStart: TPanel
        Left = 206
        Top = 244
        Width = 162
        Height = 22
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 1
        object dtpTimeStart: TDateTimePicker
          Left = 89
          Top = 0
          Width = 73
          Height = 22
          Date = 42459.000000000000000000
          Time = 0.776186967603280200
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          Kind = dtkTime
          ParentFont = False
          TabOrder = 0
          Visible = False
        end
        object dtpDateStart: TDateTimePicker
          Left = 0
          Top = 0
          Width = 85
          Height = 22
          Date = 42459.000000000000000000
          Time = 0.776186967603280200
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          Visible = False
        end
      end
      object btnReleaseAllOCAOrders: TButton
        Left = 171
        Top = 101
        Width = 145
        Height = 22
        Caption = 'Release All OCA orders'
        TabOrder = 2
        OnClick = btnReleaseAllOCAOrdersClick
      end
      object cbAdvancedOrderType: TComboBox
        Left = 118
        Top = 28
        Width = 250
        Height = 22
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
      end
      object seOcaGroupNumber: TSpinEdit
        Left = 118
        Top = 101
        Width = 47
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 4
        Value = 0
      end
      object grpRepetitive: TGroupBox
        Left = 118
        Top = 125
        Width = 250
        Height = 69
        Caption = 'Repetitive'
        TabOrder = 5
        object lblMaxNumAmount: TLabel
          Left = 24
          Top = 18
          Width = 135
          Height = 13
          Caption = 'Maximum number of amount'
        end
        object lblMaxNumShares: TLabel
          Left = 26
          Top = 44
          Width = 131
          Height = 13
          Caption = 'Maximum number of shares'
        end
        object seMaxNumAmount: TSpinEdit
          Left = 167
          Top = 15
          Width = 73
          Height = 23
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxValue = 0
          MinValue = 0
          ParentFont = False
          TabOrder = 0
          Value = 0
        end
        object seMaxNumShares: TSpinEdit
          Left = 167
          Top = 41
          Width = 73
          Height = 23
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxValue = 0
          MinValue = 0
          ParentFont = False
          TabOrder = 1
          Value = 0
        end
      end
      object chbOrderStop: TCheckBox
        Left = 118
        Top = 269
        Width = 77
        Height = 21
        Caption = 'Order Stop'
        TabOrder = 6
        Visible = False
        OnClick = chbOrderStopClick
      end
      object chbOrderStart: TCheckBox
        Left = 118
        Top = 245
        Width = 77
        Height = 21
        Caption = 'Order Start'
        TabOrder = 7
        Visible = False
        OnClick = chbOrderStartClick
      end
      object cbScope: TComboBox
        Left = 118
        Top = 76
        Width = 250
        Height = 22
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 8
        Text = 'ALL OR NONE'
        OnChange = cbScopeChange
        Items.Strings = (
          'ALL OR NONE'
          'FILL OR KILL'
          'IMMEDIATE OR CANCEL (IOC))'
          'ONE CANCELS ALL(OCA)')
      end
      object cbTimeInForce: TComboBox
        Left = 118
        Top = 52
        Width = 250
        Height = 22
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 9
        OnChange = cbTimeInForceChange
      end
      object cbTriggerMethod: TComboBox
        Left = 206
        Top = 196
        Width = 162
        Height = 22
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 10
        OnChange = cbScopeChange
      end
      object chbOrderIsFinal: TCheckBox
        Left = 118
        Top = 221
        Width = 91
        Height = 21
        Caption = 'Order is Final'
        TabOrder = 11
        OnClick = chbOrderStopClick
      end
    end
    object pnlOrderOptions: TPanel
      Left = 1
      Top = 189
      Width = 459
      Height = 124
      BevelOuter = bvNone
      TabOrder = 1
      object pnlQuantity: TPanel
        Left = 0
        Top = 0
        Width = 459
        Height = 24
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblQuantity: TLabel
          Left = 43
          Top = 5
          Width = 46
          Height = 13
          Alignment = taRightJustify
          Caption = 'Quantity:'
        end
        object pnlQuantityMult: TPanel
          Left = 199
          Top = 0
          Width = 260
          Height = 24
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          object lblOrderValueCaption: TLabel
            Left = 2
            Top = 5
            Width = 61
            Height = 13
            Caption = 'Order Value:'
          end
          object lblOrderValue: TLabel
            Left = 68
            Top = 5
            Width = 22
            Height = 13
            Caption = '0.00'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
        end
        object edQuantity: TNumberBox
          Left = 94
          Top = 2
          Width = 60
          Height = 21
          AcceptExpressions = True
          TabOrder = 0
          SpinButtonOptions.Placement = nbspCompact
          UseMouseWheel = True
          OnChangeValue = edQuantityChangeValue
        end
      end
      object pnlAuxPrice: TPanel
        Left = 0
        Top = 24
        Width = 459
        Height = 24
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object lblAuxPrice: TLabel
          Left = 39
          Top = 5
          Width = 49
          Height = 13
          Alignment = taRightJustify
          Caption = 'Aux Price:'
        end
        object pnlAuxPriceCalc: TPanel
          Left = 160
          Top = 0
          Width = 299
          Height = 24
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object lblAuxPriceCalc: TLabel
            Left = 220
            Top = 5
            Width = 22
            Height = 13
            Caption = '0.00'
          end
          object btnAuxPriceCalc: TButton
            Left = 130
            Top = 0
            Width = 86
            Height = 24
            Action = aAuxPriceCalc
            TabOrder = 0
          end
          object edAuxPricePerc: TNumberBox
            Left = 0
            Top = 2
            Width = 60
            Height = 21
            AcceptExpressions = True
            CurrencyString = '%'
            Mode = nbmCurrency
            MinValue = -100.000000000000000000
            MaxValue = 100.000000000000000000
            TabOrder = 1
            UseMouseWheel = True
            OnChangeValue = OnPercentChange
          end
          object cbAuxBasePrice: TComboBox
            Left = 62
            Top = 2
            Width = 62
            Height = 22
            Style = csOwnerDrawFixed
            DropDownCount = 15
            TabOrder = 2
            OnChange = OnBasePriceChange
            OnDrawItem = OnBasePriceDrawItem
            Items.Strings = (
              'Ask+5'
              'Ask+4'
              'Ask+3'
              'Ask+2'
              'Ask+1'
              'Ask'
              'Last'
              'Mid'
              'Bid'
              'Bid-1'
              'Bid-2'
              'Bid-3'
              'Bid-4')
          end
        end
        object edAuxPrice: TNumberBox
          Left = 94
          Top = 2
          Width = 60
          Height = 21
          AcceptExpressions = True
          CurrencyString = '%'
          Mode = nbmFloat
          TabOrder = 1
          UseMouseWheel = True
          OnChangeValue = SetModyfied
        end
      end
      object pnlLimitPrice: TPanel
        Left = 0
        Top = 48
        Width = 459
        Height = 24
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object lblLmtPrice: TLabel
          Left = 38
          Top = 5
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Caption = 'Limit Price:'
        end
        object pnlLimitPriceCalc: TPanel
          Left = 160
          Top = 0
          Width = 299
          Height = 24
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object lblLimitPriceCalc: TLabel
            Left = 220
            Top = 5
            Width = 22
            Height = 13
            Caption = '0.00'
          end
          object btnLimitPriceCalc: TButton
            Left = 130
            Top = 0
            Width = 86
            Height = 24
            Action = aLimitPriceCalc
            TabOrder = 0
          end
          object edLimitPerc: TNumberBox
            Left = 0
            Top = 2
            Width = 60
            Height = 21
            AcceptExpressions = True
            CurrencyString = '%'
            Mode = nbmCurrency
            MinValue = -100.000000000000000000
            MaxValue = 100.000000000000000000
            TabOrder = 1
            UseMouseWheel = True
            OnChangeValue = OnPercentChange
          end
          object cbLimitBasePrice: TComboBox
            Left = 62
            Top = 2
            Width = 62
            Height = 22
            Style = csOwnerDrawFixed
            DropDownCount = 15
            TabOrder = 2
            OnChange = OnBasePriceChange
            OnDrawItem = OnBasePriceDrawItem
            Items.Strings = (
              'Ask+5'
              'Ask+4'
              'Ask+3'
              'Ask+2'
              'Ask+1'
              'Ask'
              'Last'
              'Mid'
              'Bid'
              'Bid-1'
              'Bid-2'
              'Bid-3'
              'Bid-4')
          end
        end
        object edLimit: TNumberBox
          Left = 94
          Top = 2
          Width = 60
          Height = 21
          AcceptExpressions = True
          CurrencyString = '%'
          Mode = nbmFloat
          TabOrder = 1
          UseMouseWheel = True
          OnChangeValue = SetModyfied
        end
      end
      object pnlTrailStopPrice: TPanel
        Left = 0
        Top = 72
        Width = 459
        Height = 24
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 3
        object lblTrailStopPrice: TLabel
          Left = 14
          Top = 5
          Width = 75
          Height = 13
          Alignment = taRightJustify
          Caption = 'Trail Stop Price:'
        end
        object pnlTrailStopPriceCalc: TPanel
          Left = 160
          Top = 0
          Width = 299
          Height = 24
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object lblTrailStopPriceCalc: TLabel
            Left = 220
            Top = 5
            Width = 22
            Height = 13
            Caption = '0.00'
          end
          object btnTrailStopPriceCalc: TButton
            Left = 130
            Top = 0
            Width = 86
            Height = 24
            Action = aTrailStopPriceCalc
            TabOrder = 0
          end
          object edTrailStopPricePerc: TNumberBox
            Left = 0
            Top = 2
            Width = 60
            Height = 21
            AcceptExpressions = True
            CurrencyString = '%'
            Mode = nbmCurrency
            MinValue = -100.000000000000000000
            MaxValue = 100.000000000000000000
            TabOrder = 1
            UseMouseWheel = True
            OnChangeValue = OnPercentChange
          end
          object cbTrailStopBasePrice: TComboBox
            Left = 62
            Top = 2
            Width = 62
            Height = 22
            Style = csOwnerDrawFixed
            DropDownCount = 15
            TabOrder = 2
            OnChange = OnBasePriceChange
            OnDrawItem = OnBasePriceDrawItem
            Items.Strings = (
              'Ask+5'
              'Ask+4'
              'Ask+3'
              'Ask+2'
              'Ask+1'
              'Ask'
              'Last'
              'Mid'
              'Bid'
              'Bid-1'
              'Bid-2'
              'Bid-3'
              'Bid-4')
          end
        end
        object edTrailStopPrice: TNumberBox
          Left = 94
          Top = 2
          Width = 60
          Height = 21
          Hint = 'Always posivite'
          AcceptExpressions = True
          CurrencyString = '%'
          Mode = nbmFloat
          MaxValue = 100000.000000000000000000
          TabOrder = 1
          UseMouseWheel = True
          OnChangeValue = SetModyfied
        end
      end
      object pnlLmtOffset: TPanel
        Left = 0
        Top = 96
        Width = 459
        Height = 24
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 4
        object lblLmtOffset: TLabel
          Left = 29
          Top = 5
          Width = 59
          Height = 13
          Alignment = taRightJustify
          Caption = 'Limit Offset:'
        end
        object pnlLmtOffsetCalc: TPanel
          Left = 160
          Top = 0
          Width = 299
          Height = 24
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object lblLmtPriceOffsetCalc: TLabel
            Left = 220
            Top = 5
            Width = 22
            Height = 13
            Caption = '0.00'
          end
          object btnLmtPriceOffsetCalc: TButton
            Left = 130
            Top = 0
            Width = 86
            Height = 24
            Action = aLmtPriceOffsetCalc
            TabOrder = 0
          end
          object edLmtPriceOffsetPerc: TNumberBox
            Left = 0
            Top = 2
            Width = 60
            Height = 21
            AcceptExpressions = True
            CurrencyString = '%'
            Mode = nbmCurrency
            MaxValue = 100.000000000000000000
            TabOrder = 1
            UseMouseWheel = True
            OnChangeValue = OnPercentChange
          end
          object cbLmtOffsetBasePrice: TComboBox
            Left = 62
            Top = 2
            Width = 62
            Height = 22
            Style = csOwnerDrawFixed
            DropDownCount = 15
            TabOrder = 2
            OnChange = OnBasePriceChange
            OnDrawItem = OnBasePriceDrawItem
            Items.Strings = (
              'Ask+5'
              'Ask+4'
              'Ask+3'
              'Ask+2'
              'Ask+1'
              'Ask'
              'Last'
              'Mid'
              'Bid'
              'Bid-1'
              'Bid-2'
              'Bid-3'
              'Bid-4')
          end
        end
        object edLmtPriceOffset: TNumberBox
          Left = 94
          Top = 2
          Width = 60
          Height = 21
          Hint = 'Always posivite'
          AcceptExpressions = True
          CurrencyString = '%'
          Mode = nbmFloat
          MaxValue = 100000.000000000000000000
          TabOrder = 1
          UseMouseWheel = True
          OnChangeValue = SetModyfied
        end
      end
    end
    object rbSell: TRadioButton
      Left = 169
      Top = 6
      Width = 17
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = rbSellClick
    end
    object rbBuy: TRadioButton
      Left = 98
      Top = 6
      Width = 17
      Height = 17
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      TabStop = True
      OnClick = rbBuyClick
    end
    object cbOrderType: TComboBox
      Left = 95
      Top = 167
      Width = 256
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = cbOrderTypeChange
    end
    object edDescription: TEdit
      Left = 95
      Top = 28
      Width = 256
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
  end
  object pcInfo: TPageControl
    Left = 0
    Top = 325
    Width = 784
    Height = 171
    ActivePage = tsOrderInfo
    Align = alClient
    TabOrder = 2
    object tsOrderInfo: TTabSheet
      Caption = 'Order Info'
      object memoInfo: TMemo
        Left = 0
        Top = 0
        Width = 776
        Height = 143
        Margins.Left = 0
        Margins.Right = 0
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clBtnFace
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object ActionListOrder: TActionList
    Images = DMImage.vil32
    Left = 440
    Top = 152
    object aCancelOrder: TAction
      Caption = 'Cancel Order'
      OnExecute = aCancelOrderExecute
      OnUpdate = aCancelOrderUpdate
    end
    object aSave: TAction
      Caption = 'Ok'
      Enabled = False
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
      OnUpdate = aSaveUpdate
    end
    object aBuy: TAction
      Caption = 'Buy Now'
      ImageIndex = 55
      ImageName = 'ExpandFieldPivotTable_32x32'
      OnExecute = aBuyExecute
      OnUpdate = aBuyUpdate
    end
    object aOrderStatus: TAction
      Caption = 'Status'
      ShortCut = 16467
      OnExecute = aOrderStatusExecute
    end
    object aMakeDefault: TAction
      Caption = 'Make default'
      OnExecute = aMakeDefaultExecute
    end
    object aShowGlobalSettings: TAction
      Caption = 'PreC'
      ImageIndex = 63
      ImageName = 'PageSetup_32x32'
      OnExecute = aShowGlobalSettingsExecute
    end
    object aAuxPriceCalc: TAction
      Caption = 'Calc Price'
      OnExecute = aAuxPriceCalcExecute
    end
    object aLimitPriceCalc: TAction
      Caption = 'Calc Price'
      OnExecute = aLimitPriceCalcExecute
    end
    object aTrailStopPriceCalc: TAction
      Caption = 'Calc Amount'
      OnExecute = aTrailStopPriceCalcExecute
    end
    object aLmtPriceOffsetCalc: TAction
      Caption = 'Calc Amount'
      OnExecute = aLmtPriceOffsetCalcExecute
    end
    object aAddFactor: TAction
      Caption = 'Add Child Factor'
      OnExecute = aAddFactorExecute
    end
    object aAddCondition: TAction
      Caption = 'Add Child Condition'
      OnExecute = aAddConditionExecute
    end
    object aAddConditionAndFactor: TAction
      Caption = 'Add Child Condition and Factor'
      OnExecute = aAddConditionAndFactorExecute
    end
  end
  object pmAddNode: TPopupMenu
    Images = DMImage.vil16
    Left = 260
    Top = 373
    object miAddConditionandFactor: TMenuItem
      Action = aAddConditionAndFactor
    end
    object miAddCondition: TMenuItem
      Action = aAddCondition
    end
    object miAddFactor: TMenuItem
      Action = aAddFactor
    end
  end
end
