object frmOrderTemplate: TfrmOrderTemplate
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Order Base Template'
  ClientHeight = 389
  ClientWidth = 801
  Color = clBtnFace
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
    Top = 344
    Width = 801
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOk: TBitBtn
      Left = 658
      Top = 3
      Width = 100
      Height = 41
      Action = aSave
      Caption = 'Save'
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
      Left = 556
      Top = 3
      Width = 100
      Height = 41
      Cancel = True
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
    object pnlInfo: TPanel
      Left = 0
      Top = 0
      Width = 369
      Height = 45
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object imgWarning: TVirtualImage
        Left = 0
        Top = 0
        Width = 32
        Height = 45
        Align = alLeft
        ImageCollection = DMImage.ImCollection32
        ImageWidth = 0
        ImageHeight = 0
        ImageIndex = 58
        ImageName = 'Warning_32x32'
        ExplicitLeft = -3
        ExplicitTop = 2
      end
      object lblInfo: TLabel
        AlignWithMargins = True
        Left = 35
        Top = 3
        Width = 330
        Height = 13
        Align = alClient
        Caption = 
          'Changing the Document affects overall template in Template Creat' +
          'or'
        Layout = tlCenter
        WordWrap = True
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 801
    Height = 81
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblSell: TLabel
      Left = 223
      Top = 1
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
      Left = 152
      Top = 1
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
    object lblBroker: TLabel
      Left = 86
      Top = 56
      Width = 35
      Height = 13
      Alignment = taRightJustify
      Caption = 'Broker:'
    end
    object lblDescription: TLabel
      Left = 64
      Top = 31
      Width = 57
      Height = 13
      Alignment = taRightJustify
      Caption = 'Description:'
    end
    object rbSell: TRadioButton
      Left = 200
      Top = 4
      Width = 17
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = rbSellClick
    end
    object rbBuy: TRadioButton
      Left = 129
      Top = 4
      Width = 17
      Height = 17
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      TabStop = True
      OnClick = rbBuyClick
    end
    object cbBroker: TComboBox
      Left = 126
      Top = 52
      Width = 256
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnChange = cbBrokerChange
    end
    object edDescription: TEdit
      Left = 126
      Top = 27
      Width = 256
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
  object cpMain: TCardPanel
    Left = 0
    Top = 81
    Width = 801
    Height = 233
    Align = alTop
    ActiveCard = crdIB
    BevelOuter = bvNone
    TabOrder = 2
    object crdIB: TCard
      Left = 0
      Top = 0
      Width = 801
      Height = 233
      CardIndex = 0
      TabOrder = 0
      object pnlIB: TPanel
        Left = 0
        Top = 0
        Width = 801
        Height = 233
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object lblAvailableSecureType: TLabel
          Left = 6
          Top = -2
          Width = 121
          Height = 13
          Caption = 'Available Security Types:'
        end
        object lblOrderType: TLabel
          Left = 62
          Top = 46
          Width = 59
          Height = 13
          Alignment = taRightJustify
          Caption = 'Order Type:'
        end
        object lblSecureType: TLabel
          Left = 51
          Top = 22
          Width = 70
          Height = 13
          Alignment = taRightJustify
          Caption = 'Security Type:'
        end
        object pnlAdvanced: TPanel
          Left = 424
          Top = 0
          Width = 377
          Height = 233
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 2
          object lblAdvancedOrderType: TLabel
            Left = 6
            Top = 5
            Width = 110
            Height = 13
            Alignment = taRightJustify
            Caption = 'Advanced Order Type:'
          end
          object lblScope: TLabel
            Left = 83
            Top = 52
            Width = 33
            Height = 13
            Alignment = taRightJustify
            Caption = 'Scope:'
          end
          object lblTimeInForce: TLabel
            Left = 47
            Top = 29
            Width = 69
            Height = 13
            Alignment = taRightJustify
            Caption = 'Time In Force:'
          end
          object lblOcaName: TLabel
            Left = 36
            Top = 76
            Width = 80
            Height = 13
            Alignment = taRightJustify
            Caption = 'Group ID (OCA):'
          end
          object lblTriggerMethod: TLabel
            Left = 170
            Top = 76
            Width = 77
            Height = 13
            Alignment = taRightJustify
            Caption = 'Trigger Method:'
          end
          object pnlOrderStop: TPanel
            Left = 206
            Top = 211
            Width = 162
            Height = 22
            AutoSize = True
            BevelOuter = bvNone
            Enabled = False
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
            end
          end
          object pnlOrderStart: TPanel
            Left = 206
            Top = 187
            Width = 162
            Height = 22
            AutoSize = True
            BevelOuter = bvNone
            Enabled = False
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
            end
          end
          object cbAdvancedOrderType: TComboBox
            Left = 118
            Top = 0
            Width = 250
            Height = 22
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
          end
          object seOcaGroupNumber: TSpinEdit
            Left = 118
            Top = 73
            Width = 47
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 3
            Value = 0
          end
          object grpRepetitive: TGroupBox
            Left = 118
            Top = 97
            Width = 250
            Height = 69
            Caption = 'Repetitive'
            TabOrder = 4
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
            Top = 212
            Width = 77
            Height = 21
            Caption = 'Order Stop'
            Enabled = False
            TabOrder = 5
            OnClick = chbOrderStopClick
          end
          object chbOrderStart: TCheckBox
            Left = 118
            Top = 188
            Width = 77
            Height = 21
            Caption = 'Order Start'
            Enabled = False
            TabOrder = 6
            OnClick = chbOrderStartClick
          end
          object cbScope: TComboBox
            Left = 118
            Top = 48
            Width = 250
            Height = 22
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 7
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
            Top = 24
            Width = 250
            Height = 22
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 8
            OnChange = cbTimeInForceChange
          end
          object cbTriggerMethod: TComboBox
            Left = 250
            Top = 73
            Width = 118
            Height = 22
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 9
            OnChange = cbScopeChange
          end
          object chbOrderIsFinal: TCheckBox
            Left = 118
            Top = 165
            Width = 91
            Height = 21
            Caption = 'Order is Final'
            TabOrder = 10
            OnClick = chbOrderStopClick
          end
        end
        object pnlOrderOptions: TPanel
          Left = 8
          Top = 65
          Width = 369
          Height = 162
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 1
          object pnlAuxPrice: TPanel
            Left = 0
            Top = 0
            Width = 369
            Height = 24
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object lblAuxPrice: TLabel
              Left = 63
              Top = 5
              Width = 49
              Height = 13
              Alignment = taRightJustify
              Caption = 'Aux Price:'
            end
            object edAuxPrice: TNumberBox
              Left = 118
              Top = 1
              Width = 60
              Height = 21
              AcceptExpressions = True
              CurrencyString = '%'
              Mode = nbmCurrency
              MinValue = -100.000000000000000000
              MaxValue = 100.000000000000000000
              TabOrder = 0
              UseMouseWheel = True
              OnChangeValue = OnPercentChange
            end
            object cbAuxBasePrice: TComboBox
              Left = 180
              Top = 1
              Width = 62
              Height = 22
              Style = csOwnerDrawFixed
              DropDownCount = 15
              TabOrder = 1
              OnDrawItem = OnBasePricesDrawItem
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
          object pnlLimitPrice: TPanel
            Left = 0
            Top = 24
            Width = 369
            Height = 24
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object lblLmtPrice: TLabel
              Left = 62
              Top = 5
              Width = 51
              Height = 13
              Alignment = taRightJustify
              Caption = 'Limit Price:'
            end
            object edLimit: TNumberBox
              Left = 118
              Top = 1
              Width = 60
              Height = 21
              AcceptExpressions = True
              CurrencyString = '%'
              Mode = nbmCurrency
              MinValue = -100.000000000000000000
              MaxValue = 100.000000000000000000
              TabOrder = 0
              UseMouseWheel = True
              OnChangeValue = OnPercentChange
            end
            object cbLimitBasePrice: TComboBox
              Left = 180
              Top = 1
              Width = 62
              Height = 22
              Style = csOwnerDrawFixed
              DropDownCount = 15
              TabOrder = 1
              OnDrawItem = OnBasePricesDrawItem
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
          object pnlTrailStopPrice: TPanel
            Left = 0
            Top = 48
            Width = 369
            Height = 24
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
            object lblTrailStopPrice: TLabel
              Left = 38
              Top = 5
              Width = 75
              Height = 13
              Alignment = taRightJustify
              Caption = 'Trail Stop Price:'
            end
            object edTrailStopPrice: TNumberBox
              Left = 118
              Top = 1
              Width = 60
              Height = 21
              Hint = 'Always posivite'
              AcceptExpressions = True
              CurrencyString = '%'
              Mode = nbmCurrency
              MinValue = -100.000000000000000000
              MaxValue = 100.000000000000000000
              TabOrder = 0
              UseMouseWheel = True
              OnChangeValue = OnPercentChange
            end
            object cbTrailStopBasePrice: TComboBox
              Left = 180
              Top = 1
              Width = 62
              Height = 22
              Style = csOwnerDrawFixed
              DropDownCount = 15
              TabOrder = 1
              OnDrawItem = OnBasePricesDrawItem
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
          object pnlLmtOffset: TPanel
            Left = 0
            Top = 72
            Width = 369
            Height = 24
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 3
            object lblLmtOffset: TLabel
              Left = 53
              Top = 5
              Width = 59
              Height = 13
              Alignment = taRightJustify
              Caption = 'Limit Offset:'
            end
            object edLmtPriceOffset: TNumberBox
              Left = 118
              Top = 1
              Width = 60
              Height = 21
              Hint = 'Always posivite'
              AcceptExpressions = True
              CurrencyString = '%'
              Mode = nbmCurrency
              MaxValue = 100.000000000000000000
              TabOrder = 0
              UseMouseWheel = True
              OnChangeValue = OnPercentChange
            end
            object cbLmtOffsetBasePrice: TComboBox
              Left = 180
              Top = 1
              Width = 62
              Height = 22
              Style = csOwnerDrawFixed
              DropDownCount = 15
              TabOrder = 1
              OnDrawItem = OnBasePricesDrawItem
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
          object rbChildOrderPriceBased: TRadioGroup
            AlignWithMargins = True
            Left = 0
            Top = 101
            Width = 369
            Height = 60
            Margins.Left = 0
            Margins.Top = 5
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Child Order Price Based On:'
            ItemIndex = 0
            Items.Strings = (
              'Mother'#39's Filled Price'
              'TickTypes')
            TabOrder = 4
            OnClick = OnChildOrderPriceBasedChange
          end
        end
        object cbOrderType: TComboBox
          Left = 126
          Top = 42
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
        object cbSecureType: TComboBox
          Left = 126
          Top = 18
          Width = 256
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnChange = cbSecureTypeChange
        end
      end
    end
    object crdNN: TCard
      Left = 0
      Top = 0
      Width = 801
      Height = 233
      CardIndex = 1
      TabOrder = 1
      object pnlNN: TPanel
        Left = 0
        Top = 0
        Width = 801
        Height = 233
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Under development NN'
        TabOrder = 0
      end
    end
    object crdTest: TCard
      Left = 0
      Top = 0
      Width = 801
      Height = 233
      CardIndex = 2
      TabOrder = 2
      object pnlTest: TPanel
        Left = 0
        Top = 0
        Width = 801
        Height = 233
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Under development TEST'
        TabOrder = 0
      end
    end
  end
  object pnlActivateChild: TPanel
    Left = 0
    Top = 314
    Width = 801
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object cbActivateChildOrder: TCheckBox
      AlignWithMargins = True
      Left = 8
      Top = 6
      Width = 762
      Height = 18
      Caption = 
        'Activate a child-order-group for every received part of mother-o' +
        'rder'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object ActionListOrder: TActionList
    Images = DMImage.vil32
    Left = 440
    Top = 24
    object aSave: TAction
      Caption = 'Save'
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
    end
    object aCancel: TAction
      Caption = 'aCancel'
    end
  end
end
