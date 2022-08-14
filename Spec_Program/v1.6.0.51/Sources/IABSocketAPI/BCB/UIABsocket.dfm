object FIABSocket: TFIABSocket
  Left = 367
  Top = 172
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'TWS API 10.11.01 - Do not trade on a live account!'
  ClientHeight = 701
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 345
    Width = 461
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = -8
    ExplicitTop = 541
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 588
    Width = 461
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = -8
    ExplicitTop = 550
  end
  object Splitter3: TSplitter
    Left = 0
    Top = 342
    Width = 461
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 205
    ExplicitWidth = 102
  end
  object MemoAcct: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 471
    Width = 455
    Height = 114
    Align = alBottom
    Lines.Strings = (
      'This memo: Account, portfolio, time updates, Contract details,')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object MemoError: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 351
    Width = 455
    Height = 114
    Align = alBottom
    Lines.Strings = (
      'This memo: Errors and connection')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object MemoOrderStat: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 231
    Width = 455
    Height = 108
    Align = alClient
    Lines.Strings = (
      'This memo: Order status, executions')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 682
    Width = 461
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object gbAbout: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 594
    Width = 455
    Height = 85
    Align = alBottom
    Caption = 'About'
    TabOrder = 4
    object Label1: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 445
      Height = 40
      Align = alTop
      AutoSize = False
      Caption = 
        'This demo and test app, was built using the Delphi / BCB. TIABSo' +
        'cketAPI component.  It does not require any dll'#39's, ActiveX, or .' +
        'net files  It talks directly to the TWS at the socket interface.' +
        '  Very fast and reliable!'
      WordWrap = True
      ExplicitLeft = 9
      ExplicitTop = 14
      ExplicitWidth = 377
    end
    object Label3: TLabel
      Left = 20
      Top = 64
      Width = 120
      Height = 13
      Caption = 'Delphi / BCB developers:'
    end
    object Label4: TLabel
      Left = 158
      Top = 64
      Width = 213
      Height = 13
      Cursor = crHandPoint
      Caption = 'https://www.hhssoftware.com/iabsocketapi/'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = Label4Click
    end
    object Bevel3: TBevel
      Left = -8
      Top = 528
      Width = 398
      Height = 71
      Shape = bsFrame
    end
  end
  object pTop: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 455
    Height = 222
    Align = alTop
    TabOrder = 0
    object gbOrder: TGroupBox
      AlignWithMargins = True
      Left = 216
      Top = 4
      Width = 234
      Height = 214
      Align = alLeft
      Caption = 'Order'
      TabOrder = 1
      DesignSize = (
        234
        214)
      object ListBoxOrderType: TListBox
        Left = 9
        Top = 18
        Width = 87
        Height = 125
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
      end
      object EditA: TEdit
        Left = 165
        Top = 129
        Width = 56
        Height = 21
        Hint = 'Aux price (stops), strike|Aux price (stops), or Strike price'
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 13
        Text = 'Aux/Strike'
      end
      object EditCur: TEdit
        Left = 107
        Top = 151
        Width = 56
        Height = 21
        Hint = 'Currency|Currency basis for trading instrument'
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 14
        Text = 'USD'
      end
      object EditExch: TEdit
        Left = 165
        Top = 84
        Width = 56
        Height = 21
        Hint = 
          'Exchange|Exchange for trading: NASDAQ, ISLAND, GLOBEX, SMART, et' +
          'c'
        Anchors = [akTop, akRight]
        CharCase = ecUpperCase
        ParentShowHint = False
        ShowHint = True
        TabOrder = 9
        Text = 'GLOBEX'
      end
      object EditExp: TEdit
        Left = 107
        Top = 106
        Width = 56
        Height = 21
        Hint = 
          'Expiry (yyyymmdd)|Expiry date (yyyymmdd) for future, option, oth' +
          'ers.'
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 10
      end
      object EditP: TEdit
        Left = 107
        Top = 129
        Width = 56
        Height = 21
        Hint = 'Price|Price of order, or stop order offset'
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 12
      end
      object EditSym: TEdit
        Left = 107
        Top = 84
        Width = 56
        Height = 21
        Hint = 
          'Symbol(ES)/LocalSymbol(ESZ8)|Symbol of stock or future or option' +
          ', etc.'
        Anchors = [akTop, akRight]
        CharCase = ecUpperCase
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        Text = 'ES'
      end
      object EditV: TEdit
        Left = 165
        Top = 106
        Width = 56
        Height = 21
        Hint = 'Quantity|Quantity of order'
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 11
        Text = '5'
      end
      object RBBuy: TRadioButton
        Left = 116
        Top = 60
        Width = 37
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Buy'
        TabOrder = 6
      end
      object RBSell: TRadioButton
        Left = 172
        Top = 59
        Width = 41
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Sell'
        TabOrder = 7
      end
      object ButtonPlaceOrder: TButton
        Left = 99
        Top = 14
        Width = 65
        Height = 19
        Hint = 'Place order'
        Anchors = [akTop, akRight]
        Caption = 'Place'
        TabOrder = 2
        OnClick = ButtonPlaceOrderClick
      end
      object ButtonModifyOrder: TButton
        Left = 99
        Top = 35
        Width = 65
        Height = 19
        Hint = 'Modify order'
        Anchors = [akTop, akRight]
        Caption = 'Modify'
        TabOrder = 4
        OnClick = ButtonModifyOrderClick
      end
      object ButtonVerifyOrder: TButton
        Left = 170
        Top = 14
        Width = 59
        Height = 19
        Hint = 'Verify order, get commission data'
        Anchors = [akTop, akRight]
        Caption = 'Verify'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 3
        OnClick = ButtonVerifyOrderClick
      end
      object ButtonCancelOrder: TButton
        Left = 170
        Top = 35
        Width = 59
        Height = 19
        Hint = 'Cancel order'
        Anchors = [akTop, akRight]
        Caption = 'Cx'
        TabOrder = 5
        OnClick = ButtonCancelOrderClick
      end
      object ButtonSnap: TButton
        Left = 170
        Top = 152
        Width = 49
        Height = 19
        Hint = 'Snapshot data'
        Anchors = [akTop, akRight]
        Caption = 'Snap'
        TabOrder = 15
        OnClick = ButtonSnapClick
      end
      object ComboSecType: TComboBox
        Left = 9
        Top = 150
        Width = 89
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
    end
    object gbData: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 206
      Height = 214
      Align = alLeft
      Caption = 'Data'
      TabOrder = 0
      DesignSize = (
        206
        214)
      object Bevel2: TBevel
        Left = 90
        Top = 19
        Width = 106
        Height = 95
        Anchors = [akTop, akRight]
        Shape = bsFrame
      end
      object Label2: TLabel
        Left = 136
        Top = 161
        Width = 25
        Height = 26
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = 'Log level'
        WordWrap = True
        ExplicitLeft = 127
      end
      object labelask: TLabel
        Left = 148
        Top = 50
        Width = 41
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = 'AskP'
      end
      object labelasksize: TLabel
        Left = 148
        Top = 65
        Width = 33
        Height = 13
        Hint = 'Ask Size'
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = 'AskS'
      end
      object labelbid: TLabel
        Left = 98
        Top = 50
        Width = 22
        Height = 13
        Hint = 'Bid Price'
        Anchors = [akTop, akRight]
        Caption = 'BidP'
      end
      object labelbidsize: TLabel
        Left = 104
        Top = 65
        Width = 22
        Height = 13
        Hint = 'Bid Size'
        Anchors = [akTop, akRight]
        Caption = 'BidS'
      end
      object LabelLast: TLabel
        Left = 98
        Top = 80
        Width = 27
        Height = 13
        Hint = 'Last Price'
        Anchors = [akTop, akRight]
        Caption = 'LastP'
      end
      object LabelOptTick: TLabel
        Left = 169
        Top = 96
        Width = 17
        Height = 13
        Hint = 'OPtion'
        Anchors = [akTop, akRight]
        Caption = 'Opt'
      end
      object LabelSize: TLabel
        Left = 152
        Top = 80
        Width = 37
        Height = 13
        Hint = 'Last Size'
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = 'LastS'
      end
      object LabelVol: TLabel
        Left = 98
        Top = 95
        Width = 58
        Height = 13
        Hint = 'Volume'
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = 'Volume'
      end
      object ButtonAcctPortfolio: TButton
        Left = 9
        Top = 91
        Width = 75
        Height = 21
        Caption = 'Acct / PF'
        TabOrder = 3
        OnClick = ButtonAcctPortfolioClick
      end
      object ButtonConnection: TButton
        Left = 9
        Top = 18
        Width = 75
        Height = 21
        Caption = 'Connect'
        TabOrder = 0
        OnClick = ButtonConnectionClick
        OnKeyPress = ButtonConnectionKeyPress
      end
      object ButtonContractInfo: TButton
        Left = 140
        Top = 26
        Width = 50
        Height = 19
        Anchors = [akTop, akRight]
        Caption = 'Contract'
        TabOrder = 8
        OnClick = ButtonContractInfoClick
      end
      object ButtonCxTS: TButton
        Left = 155
        Top = 139
        Width = 42
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'Cx TS'
        TabOrder = 12
        OnClick = ButtonCxTSClick
      end
      object ButtonExecutions: TButton
        Left = 9
        Top = 42
        Width = 75
        Height = 21
        Caption = 'Executions'
        TabOrder = 1
        OnClick = ButtonExecutionsClick
      end
      object ButtonHistory: TButton
        Left = 89
        Top = 115
        Width = 46
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'History'
        TabOrder = 9
        OnClick = ButtonHistoryClick
      end
      object ButtonLevel2Depth: TButton
        Left = 9
        Top = 115
        Width = 75
        Height = 21
        Caption = 'L2 / Depth'
        TabOrder = 4
        OnClick = ButtonLevel2DepthClick
      end
      object ButtonNewsBulletins: TButton
        Left = 9
        Top = 139
        Width = 75
        Height = 21
        Caption = 'Bulletins'
        TabOrder = 5
        OnClick = ButtonNewsBulletinsClick
      end
      object ButtonOpenOrders: TButton
        Left = 9
        Top = 66
        Width = 75
        Height = 21
        Caption = 'OpenOrders'
        TabOrder = 2
        OnClick = ButtonOpenOrdersClick
      end
      object ButtonRealTimebars: TButton
        Left = 139
        Top = 115
        Width = 57
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'RealTime'
        TabOrder = 10
        OnClick = ButtonRealTimebarsClick
      end
      object ButtonScanner: TButton
        Left = 9
        Top = 163
        Width = 75
        Height = 21
        Caption = 'Scanner'
        TabOrder = 6
        OnClick = ButtonScannerClick
      end
      object ButtonTickData: TButton
        Left = 94
        Top = 26
        Width = 40
        Height = 19
        Anchors = [akTop, akRight]
        Caption = 'Data'
        TabOrder = 7
        OnClick = ButtonTickDataClick
      end
      object ButtonTimeSales: TButton
        Left = 89
        Top = 139
        Width = 60
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'TimeSale'
        TabOrder = 11
        OnClick = ButtonHistoryClick
      end
      object ButtonTWStime: TButton
        Left = 89
        Top = 163
        Width = 46
        Height = 21
        Hint = 'Server or Exchange time'
        Anchors = [akTop, akRight]
        Caption = 'Time'
        TabOrder = 13
        OnClick = ButtonTWStimeClick
      end
      object ComboBoxLogLevel: TComboBox
        Left = 163
        Top = 163
        Width = 33
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemIndex = 1
        TabOrder = 14
        Text = '2'
        OnChange = ComboBoxLogLevelChange
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5')
      end
      object ButtonSearch: TButton
        Left = 9
        Top = 187
        Width = 75
        Height = 21
        Caption = 'Inst. search'
        TabOrder = 15
        OnClick = ButtonSearchClick
      end
      object ButtonWSHorizon: TButton
        Left = 90
        Top = 187
        Width = 75
        Height = 21
        Caption = 'WS horizon'
        TabOrder = 16
        OnClick = ButtonWSHorizonClick
      end
    end
  end
end
