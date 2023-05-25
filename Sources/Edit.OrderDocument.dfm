object frmOrderDocument: TfrmOrderDocument
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Order'
  ClientHeight = 279
  ClientWidth = 466
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
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 234
    Width = 466
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      466
      45)
    object btnOk: TBitBtn
      Left = 364
      Top = 3
      Width = 100
      Height = 41
      Action = aSave
      Anchors = [akTop, akRight]
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
      Left = 262
      Top = 3
      Width = 100
      Height = 41
      Anchors = [akTop, akRight]
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
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 466
    Height = 102
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
    object lblOrderType: TLabel
      Left = 62
      Top = 82
      Width = 59
      Height = 13
      Alignment = taRightJustify
      Caption = 'Order Type:'
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
    object cbOrderType: TComboBox
      Left = 126
      Top = 78
      Width = 256
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnChange = cbOrderTypeChange
    end
  end
  object cpMain: TCardPanel
    Left = 0
    Top = 126
    Width = 466
    Height = 108
    Align = alClient
    ActiveCard = crdIB
    BevelOuter = bvNone
    TabOrder = 2
    object crdIB: TCard
      Left = 0
      Top = 0
      Width = 466
      Height = 108
      CardIndex = 0
      TabOrder = 0
      object pnlIB: TPanel
        Left = 0
        Top = 0
        Width = 466
        Height = 108
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        OnDragDrop = pnlIBDragDrop
        OnDragOver = pnlIBDragOver
        DesignSize = (
          466
          108)
        object lblScope: TLabel
          Left = 88
          Top = 7
          Width = 33
          Height = 13
          Alignment = taRightJustify
          Caption = 'Scope:'
        end
        object lblTrailStopPrice: TLabel
          Left = 66
          Top = 32
          Width = 56
          Height = 13
          Alignment = taRightJustify
          Caption = 'Visible part:'
        end
        object lblInstrument: TLabel
          Left = 64
          Top = 79
          Width = 57
          Height = 13
          Alignment = taRightJustify
          Caption = 'Instrument:'
        end
        object lblInstrumentName: TLabel
          Left = 126
          Top = 79
          Width = 133
          Height = 13
          Caption = '<Replace from AutoOrder>'
        end
        object lblActiveTime: TLabel
          Left = 64
          Top = 56
          Width = 57
          Height = 13
          Alignment = taRightJustify
          Caption = 'Active time:'
        end
        object lblSeconds: TLabel
          Left = 193
          Top = 56
          Width = 39
          Height = 13
          Caption = 'seconds'
        end
        object cbScope: TComboBox
          Left = 126
          Top = 3
          Width = 256
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          Text = 'NONE'
          Items.Strings = (
            'NONE'
            'FILL OR KILL')
        end
        object edVisiblePart: TNumberBox
          Left = 126
          Top = 28
          Width = 60
          Height = 21
          Hint = 'Always posivite'
          AcceptExpressions = True
          CurrencyString = '%'
          Mode = nbmCurrency
          MaxValue = 100.000000000000000000
          TabOrder = 1
          Value = 100.000000000000000000
          UseMouseWheel = True
          OnChangeValue = OnPercentChange
        end
        object btnShowSearchForm: TBitBtn
          Left = 350
          Top = 70
          Width = 32
          Height = 32
          Action = aShowSearchInstruments
          Anchors = [akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          Images = DMImage.vil16
          ParentFont = False
          TabOrder = 2
        end
        object edActiveTime: TNumberBox
          Left = 145
          Top = 52
          Width = 41
          Height = 21
          Hint = 'Always posivite'
          AcceptExpressions = True
          Alignment = taRightJustify
          MaxValue = 100.000000000000000000
          TabOrder = 3
          Value = 100.000000000000000000
          UseMouseWheel = True
          OnChangeValue = OnPercentChange
        end
        object cbIsActiveTime: TCheckBox
          Left = 126
          Top = 55
          Width = 17
          Height = 17
          TabOrder = 4
          OnClick = cbIsActiveTimeClick
        end
      end
    end
    object crdNN: TCard
      Left = 0
      Top = 0
      Width = 466
      Height = 108
      CardIndex = 1
      TabOrder = 1
      object pnlNN: TPanel
        Left = 0
        Top = 0
        Width = 466
        Height = 108
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Under development NN'
        TabOrder = 0
      end
    end
    object crdTest: TCard
      Left = 0
      Top = 0
      Width = 466
      Height = 108
      CardIndex = 2
      TabOrder = 2
      object pnlTest: TPanel
        Left = 0
        Top = 0
        Width = 466
        Height = 108
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Under development TEST'
        TabOrder = 0
      end
    end
  end
  object pnlLimitPrice: TPanel
    Left = 0
    Top = 102
    Width = 466
    Height = 24
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object lblLmtPrice: TLabel
      Left = 70
      Top = 5
      Width = 51
      Height = 13
      Alignment = taRightJustify
      Caption = 'Limit Price:'
    end
    object edLimit: TNumberBox
      Left = 126
      Top = 2
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
      Left = 188
      Top = 2
      Width = 62
      Height = 22
      Style = csOwnerDrawFixed
      DropDownCount = 15
      TabOrder = 1
      Visible = False
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
  object ActionListOrder: TActionList
    Images = DMImage.vil32
    Left = 416
    Top = 40
    object aSave: TAction
      Caption = 'Save'
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
    end
    object aCancel: TAction
      Caption = 'aCancel'
    end
    object aShowSearchInstruments: TAction
      Hint = 'Search for instrument'
      ImageIndex = 21
      ImageName = 'Zoom_32x32'
      OnExecute = aShowSearchInstrumentsExecute
    end
    object aClear: TAction
      Hint = 'Clear Instrument'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
    end
  end
end
