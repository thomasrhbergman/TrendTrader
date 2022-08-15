object frmEditOrderNN: TfrmEditOrderNN
  Left = 0
  Top = 0
  Caption = 'Order / NordNet'
  ClientHeight = 672
  ClientWidth = 609
  Color = clBtnFace
  Constraints.MinHeight = 615
  Constraints.MinWidth = 560
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 609
    Height = 127
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblSell: TLabel
      Left = 98
      Top = 4
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
      OnClick = rbBuyClick
    end
    object lblBuy: TLabel
      Left = 34
      Top = 4
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
    object lblStatusCaption: TLabel
      Left = 137
      Top = 6
      Width = 54
      Height = 19
      Caption = 'Status: '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblStatus: TLabel
      Left = 189
      Top = 6
      Width = 60
      Height = 19
      Caption = 'Sleeping'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblDescriptionCaption: TLabel
      Left = 12
      Top = 42
      Width = 64
      Height = 14
      Alignment = taRightJustify
      Caption = 'Description:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblInstrumentCaption: TLabel
      Left = 10
      Top = 72
      Width = 65
      Height = 14
      Alignment = taRightJustify
      Caption = 'Instrument:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblInstrumentName: TLabel
      Left = 80
      Top = 68
      Width = 119
      Height = 19
      Caption = 'Volvo AB Serie B'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblInstrumentSymbol: TLabel
      Left = 81
      Top = 92
      Width = 65
      Height = 19
      Caption = 'Volvo AB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblAvgPrice: TLabel
      Left = 298
      Top = 6
      Width = 71
      Height = 19
      Alignment = taRightJustify
      Caption = 'Avg: 0.00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object rbSell: TRadioButton
      Left = 80
      Top = 8
      Width = 17
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = rbBuyClick
    end
    object rbBuy: TRadioButton
      Left = 16
      Top = 8
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
    object edtDescription: TEdit
      Left = 80
      Top = 35
      Width = 287
      Height = 27
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object memoInfo: TMemo
      Left = 373
      Top = 0
      Width = 236
      Height = 127
      Align = alRight
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      ScrollBars = ssVertical
      TabOrder = 3
    end
  end
  object pnlCenterOrderType: TPanel
    Left = 0
    Top = 127
    Width = 609
    Height = 181
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object pnlIceberg: TPanel
      Left = 336
      Top = 0
      Width = 273
      Height = 181
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object gbIceberg: TGroupBox
        Left = 0
        Top = 73
        Width = 273
        Height = 49
        Align = alTop
        Caption = '  Iceberg (open volume)  '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object lblPartOfOrderCaption: TLabel
          Left = 9
          Top = 22
          Width = 137
          Height = 14
          Alignment = taRightJustify
          Caption = 'Visible part of order (%):'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object seOpenVolume: TSpinEdit
          Left = 158
          Top = 15
          Width = 78
          Height = 29
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxValue = 100
          MinValue = 0
          ParentFont = False
          TabOrder = 0
          Value = 100
        end
      end
      object gbReference: TGroupBox
        Left = 0
        Top = 122
        Width = 273
        Height = 59
        Align = alClient
        Caption = '  Reference  (stockXrobot order ID)  '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object edtReference: TEdit
          Left = 9
          Top = 19
          Width = 227
          Height = 27
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
      end
      object gbTimeInForce: TGroupBox
        Left = 0
        Top = 0
        Width = 273
        Height = 73
        Align = alTop
        Caption = '  Time in force (valid_until)  '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        object rbTimeInForceToday: TRadioButton
          Left = 15
          Top = 22
          Width = 110
          Height = 17
          Caption = 'Today (default)'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbTimeInForceUntilClick
        end
        object rbTimeInForceUntil: TRadioButton
          Left = 15
          Top = 46
          Width = 110
          Height = 17
          Caption = 'Valid until'
          TabOrder = 1
          OnClick = rbTimeInForceUntilClick
        end
        object dtValidUntil: TDateTimePicker
          Left = 122
          Top = 41
          Width = 113
          Height = 27
          Date = 43040.000000000000000000
          Time = 0.524743761568970500
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
      end
    end
    object gbOrderTypes: TGroupBox
      Left = 0
      Top = 0
      Width = 336
      Height = 181
      Align = alClient
      Caption = '  Order types (order_type)  '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object lblVolumeCaption: TLabel
        Left = 84
        Top = 103
        Width = 45
        Height = 14
        Alignment = taRightJustify
        Caption = 'Volume:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object rbMarketOrder: TRadioButton
        Left = 16
        Top = 17
        Width = 125
        Height = 17
        Caption = 'Market order'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = OnOrderTypeChange
      end
      object rbLimitOrder: TRadioButton
        Left = 16
        Top = 37
        Width = 125
        Height = 17
        Caption = 'Limit order'
        TabOrder = 1
        OnClick = OnOrderTypeChange
      end
      object seVolume: TSpinEdit
        Left = 138
        Top = 96
        Width = 75
        Height = 29
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxValue = 0
        MinValue = 0
        ParentFont = False
        TabOrder = 2
        Value = 1000
      end
      object pnlLimit: TPanel
        Left = 2
        Top = 124
        Width = 332
        Height = 55
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 3
        Visible = False
        object edLimit: TEdit
          Left = 136
          Top = 18
          Width = 75
          Height = 27
          Color = clMoneyGreen
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          Text = '55.2'
          OnKeyPress = OnKeyPress
        end
        object gbLimitType: TGroupBox
          Left = 0
          Top = 0
          Width = 125
          Height = 55
          Align = alLeft
          Caption = '  Limit:  '
          TabOrder = 1
          object rbLimitTypeFix: TRadioButton
            Left = 12
            Top = 35
            Width = 100
            Height = 17
            Caption = 'Fix'
            TabOrder = 0
          end
          object rbLimitTypeRelative: TRadioButton
            Left = 12
            Top = 16
            Width = 100
            Height = 17
            Caption = 'Relative / LAST'
            Checked = True
            TabOrder = 1
            TabStop = True
            OnClick = OnLimitTypeChange
          end
        end
      end
      object rbCombinationTrailOrderMRK: TRadioButton
        Left = 16
        Top = 57
        Width = 194
        Height = 17
        Caption = 'Combination Trail order (MRK)'
        TabOrder = 4
        OnClick = OnOrderTypeChange
      end
      object rbCombinationTrailOrderLMT: TRadioButton
        Left = 16
        Top = 77
        Width = 194
        Height = 17
        Caption = 'Combination Trail order (LMT)'
        TabOrder = 5
        OnClick = OnOrderTypeChange
      end
    end
  end
  object pnlCenterCurrentValues: TPanel
    Left = 0
    Top = 308
    Width = 609
    Height = 74
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object gbCurrentValues: TGroupBox
      Left = 0
      Top = 0
      Width = 609
      Height = 74
      Align = alClient
      Color = clSkyBlue
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentColor = False
      ParentFont = False
      TabOrder = 0
      object lblMinPayedCaption: TLabel
        Left = 14
        Top = 23
        Width = 59
        Height = 14
        Alignment = taRightJustify
        Caption = 'Min payed:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblMaxPayedCaption: TLabel
        Left = 11
        Top = 42
        Width = 62
        Height = 14
        Alignment = taRightJustify
        Caption = 'Max payed:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblLastPayedCaption: TLabel
        Left = 175
        Top = 23
        Width = 63
        Height = 14
        Alignment = taRightJustify
        Caption = 'Last payed:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblEntryPriceCaption: TLabel
        Left = 175
        Top = 41
        Width = 63
        Height = 14
        Alignment = taRightJustify
        Caption = 'Entry price:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblMinPayed: TLabel
        Left = 95
        Top = 23
        Width = 28
        Height = 14
        Alignment = taRightJustify
        Caption = '0.00'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblMaxPayed: TLabel
        Left = 95
        Top = 42
        Width = 28
        Height = 14
        Alignment = taRightJustify
        Caption = '0.00'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblLastPayed: TLabel
        Left = 261
        Top = 23
        Width = 28
        Height = 14
        Alignment = taRightJustify
        Caption = '0.00'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblEntryPrice: TLabel
        Left = 261
        Top = 41
        Width = 28
        Height = 14
        Alignment = taRightJustify
        Caption = '0.00'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblChildCondFeed: TLabel
        Left = 13
        Top = 1
        Width = 162
        Height = 19
        Caption = 'Child condition feed'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object pnlFeedFrom: TPanel
        Left = 365
        Top = 16
        Width = 242
        Height = 56
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object lblFeedFrom: TLabel
          Left = 4
          Top = 1
          Width = 60
          Height = 14
          Alignment = taRightJustify
          Caption = 'Feed from:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblChildInstrumentName: TLabel
          Left = 79
          Top = 22
          Width = 5
          Height = 19
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbFeedFrom: TComboBox
          Left = 76
          Top = 0
          Width = 145
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          Text = 'Interactive Brokers'
          OnChange = cbFeedFromChange
          Items.Strings = (
            'Interactive Brokers'
            'NordNet Brokers')
        end
        object btnSelectInstrument: TButton
          Left = 1
          Top = 19
          Width = 75
          Height = 25
          Action = aSelectInstrument
          TabOrder = 1
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 627
    Width = 609
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MaxHeight = 45
    Constraints.MinHeight = 45
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object btnSave: TBitBtn
      Left = 496
      Top = 2
      Width = 110
      Height = 41
      Action = aSave
      Caption = 'Ok'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 381
      Top = 2
      Width = 110
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
    object btnBuy: TBitBtn
      Left = 268
      Top = 2
      Width = 109
      Height = 41
      Action = aBuy
      Caption = 'Buy Now'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ModalResult = 6
      NumGlyphs = 2
      ParentFont = False
      TabOrder = 2
    end
  end
  object pnlChildParams: TPanel
    Left = 0
    Top = 382
    Width = 609
    Height = 243
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    object pnlSingleTrailOrderParams: TPanel
      Left = 0
      Top = 0
      Width = 609
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblSingleTrailOrderParams: TLabel
        Left = 13
        Top = 2
        Width = 239
        Height = 19
        Caption = 'Single trail-order parameters'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object gbChildOrder: TGroupBox
      Left = 0
      Top = 25
      Width = 609
      Height = 41
      Align = alTop
      Caption = 
        '  Child-order / All values are relative average price from mothe' +
        'r-order  '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object cbActivateChildForRecievedPart: TCheckBox
        Left = 12
        Top = 16
        Width = 425
        Height = 17
        Caption = 'Activate one order for every recieved part of the mother-order'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object pnlTrail: TPanel
      Left = 0
      Top = 91
      Width = 609
      Height = 66
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object gbTrailTrigger: TGroupBox
        Left = 0
        Top = 0
        Width = 269
        Height = 66
        Align = alClient
        Caption = '  Trail trigger  '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object lblTrailSendSellCaption: TLabel
          Left = 11
          Top = 16
          Width = 121
          Height = 42
          Caption = 'Send sell order when last payed is from max payed:'
          WordWrap = True
        end
        object edtTrailTriggerSendSell: TEdit
          Left = 137
          Top = 28
          Width = 51
          Height = 22
          TabOrder = 0
          Text = '-0.50'
          OnKeyPress = OnKeyPressLessZero
        end
      end
      object gbReached: TGroupBox
        Left = 269
        Top = 0
        Width = 170
        Height = 66
        Align = alRight
        Caption = 'Reached (Fixed, if touched)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object cbActiveReached: TCheckBox
          Left = 3
          Top = 16
          Width = 105
          Height = 46
          Caption = 'Sell when reached from entry price:'
          Checked = True
          State = cbChecked
          TabOrder = 0
          WordWrap = True
          OnClick = cbActiveReachedClick
        end
        object edtConditionReached: TEdit
          Left = 110
          Top = 28
          Width = 50
          Height = 22
          Enabled = False
          TabOrder = 1
          Text = '+2.00'
          OnKeyPress = OnKeyPressOverZero
        end
      end
      object gbStopLoss: TGroupBox
        Left = 439
        Top = 0
        Width = 170
        Height = 66
        Align = alRight
        Caption = 'Floor (Fixed value, stop loss)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        object cbActiveStopLoss: TCheckBox
          Left = 3
          Top = 16
          Width = 105
          Height = 45
          Caption = 'Sell when reached from entry price:'
          Checked = True
          State = cbChecked
          TabOrder = 0
          WordWrap = True
          OnClick = cbActiveStopLossClick
        end
        object edtConditionStopLoss: TEdit
          Left = 110
          Top = 28
          Width = 50
          Height = 22
          Enabled = False
          TabOrder = 1
          Text = '-2.00'
          OnKeyPress = OnKeyPressLessZero
        end
      end
    end
    object gbTypeOfSendOrder: TGroupBox
      Left = 0
      Top = 182
      Width = 609
      Height = 61
      Align = alTop
      Caption = 'Send order as:'
      TabOrder = 3
      object lblTrailTriggerLimitCaption: TLabel
        Left = 105
        Top = 35
        Width = 29
        Height = 14
        Caption = 'Limit:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object lblFromLastPayed: TLabel
        Left = 194
        Top = 28
        Width = 291
        Height = 28
        Caption = 
          'LAST price (feed from NN mother order instrument) (at the moment' +
          ' a condition become true)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object edtTrailTriggerLimit: TEdit
        Left = 137
        Top = 33
        Width = 51
        Height = 21
        Enabled = False
        TabOrder = 0
        Text = '-0.75'
        OnKeyPress = OnKeyPress
      end
      object rbTrailTriggerLimitOrder: TRadioButton
        Left = 14
        Top = 35
        Width = 90
        Height = 17
        Caption = 'Limit order'
        TabOrder = 1
        OnClick = rbTrailTriggerLimitOrderClick
      end
      object rbTrailTriggerMarketOrder: TRadioButton
        Left = 14
        Top = 18
        Width = 90
        Height = 17
        Caption = 'Market order'
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = rbTrailTriggerLimitOrderClick
      end
    end
    object pnlChildConditionParams: TPanel
      Left = 0
      Top = 66
      Width = 609
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 4
      object lblChildConditionParams: TLabel
        Left = 13
        Top = 0
        Width = 220
        Height = 19
        Caption = 'Child condition parameters'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
    end
    object pnlChildOrderType: TPanel
      Left = 0
      Top = 157
      Width = 609
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 5
      object lblChildOrderType: TLabel
        Left = 13
        Top = 2
        Width = 131
        Height = 19
        Caption = 'Child order type'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
  end
  object ActionListOrder: TActionList
    Images = DMImage.vil32
    Left = 474
    Top = 16
    object aCancelOrder: TAction
      Caption = 'Cancel'
      OnExecute = aCancelOrderExecute
    end
    object aSave: TAction
      Caption = 'Ok'
      Enabled = False
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
      OnUpdate = aSaveUpdate
    end
    object aSelectInstrument: TAction
      Caption = 'Select instr.'
      OnExecute = aSelectInstrumentExecute
      OnUpdate = aSelectInstrumentUpdate
    end
    object aBuy: TAction
      Caption = 'Buy Now'
      ImageIndex = 55
      ImageName = 'ExpandFieldPivotTable_32x32'
      OnExecute = aBuyExecute
      OnUpdate = aBuyUpdate
    end
  end
end
