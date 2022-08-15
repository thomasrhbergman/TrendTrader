object frmDefOptionalParameter: TfrmDefOptionalParameter
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Definition Optional Parameters'
  ClientHeight = 474
  ClientWidth = 692
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 429
    Width = 692
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      692
      45)
    object btnOk: TBitBtn
      Left = 596
      Top = 3
      Width = 95
      Height = 41
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ImageIndex = 46
      ImageName = 'tick'
      Images = DMImage.vil32
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 495
      Top = 3
      Width = 101
      Height = 41
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ModalResult = 2
      ParentFont = False
      TabOrder = 1
    end
  end
  object pnlLeft: TPanel
    Left = 273
    Top = 0
    Width = 153
    Height = 429
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object gbStdDeviations: TGroupBox
      Left = 0
      Top = 161
      Width = 153
      Height = 148
      Align = alTop
      Caption = ' Std Deviations '
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object rbDev1_0: TRadioButton
        Left = 10
        Top = 22
        Width = 113
        Height = 17
        Caption = '1'
        TabOrder = 0
      end
      object rbDev1_5: TRadioButton
        Left = 10
        Top = 45
        Width = 113
        Height = 17
        Caption = '1.5'
        TabOrder = 1
      end
      object rbDev2_0: TRadioButton
        Left = 10
        Top = 68
        Width = 113
        Height = 17
        Caption = '2'
        Checked = True
        TabOrder = 2
        TabStop = True
      end
      object rbDev2_5: TRadioButton
        Left = 10
        Top = 92
        Width = 113
        Height = 17
        Caption = '2.5'
        TabOrder = 3
      end
      object rbDevCustom: TRadioButton
        Left = 10
        Top = 115
        Width = 113
        Height = 17
        Caption = 'Custom'
        TabOrder = 4
      end
      object seDevCustom: TSpinEdit
        Left = 72
        Top = 113
        Width = 57
        Height = 26
        Enabled = False
        MaxValue = 0
        MinValue = 0
        TabOrder = 5
        Value = 0
      end
    end
    object gbStrikes: TGroupBox
      Left = 0
      Top = 0
      Width = 153
      Height = 161
      Align = alTop
      Caption = ' Strikes '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = OnStrikeClick
      object rbStrike4: TRadioButton
        Left = 10
        Top = 22
        Width = 113
        Height = 17
        Caption = '4'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = OnStrikeClick
      end
      object rbStrike6: TRadioButton
        Left = 10
        Top = 45
        Width = 113
        Height = 17
        Caption = '6'
        TabOrder = 1
        OnClick = OnStrikeClick
      end
      object rbStrike8: TRadioButton
        Left = 10
        Top = 68
        Width = 113
        Height = 17
        Caption = '8'
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = OnStrikeClick
      end
      object rbStrike10: TRadioButton
        Left = 10
        Top = 92
        Width = 113
        Height = 17
        Caption = '10'
        TabOrder = 3
        OnClick = OnStrikeClick
      end
      object rbStrikeCustom: TRadioButton
        Left = 10
        Top = 115
        Width = 113
        Height = 17
        Caption = 'Custom'
        TabOrder = 4
        OnClick = OnStrikeClick
      end
      object rbStrikeAll: TRadioButton
        Left = 10
        Top = 137
        Width = 113
        Height = 17
        Caption = 'All'
        TabOrder = 5
        OnClick = OnStrikeClick
      end
      object seStrikeCustom: TSpinEdit
        Left = 72
        Top = 113
        Width = 57
        Height = 26
        Enabled = False
        Increment = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 6
        Value = 12
        OnChange = OnStrikeClick
      end
    end
    object gbOptions: TGroupBox
      Left = 0
      Top = 309
      Width = 153
      Height = 119
      Align = alTop
      Caption = 'Options'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object lblMaxWeeks: TLabel
        Left = 10
        Top = 19
        Width = 93
        Height = 13
        Caption = 'Max Weeks/Month:'
      end
      object lblStrikePriceInterval: TLabel
        Left = 10
        Top = 58
        Width = 98
        Height = 13
        Caption = 'Strike Price Interval:'
      end
      object seMaxWeeks: TSpinEdit
        Left = 72
        Top = 36
        Width = 57
        Height = 22
        Enabled = False
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object seStrikePriceInterval: TSpinEdit
        Left = 72
        Top = 75
        Width = 57
        Height = 22
        Enabled = False
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
    end
  end
  object pnlStrikes: TPanel
    Left = 554
    Top = 0
    Width = 138
    Height = 429
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object lbStrikes: TCheckListBox
      Left = 0
      Top = 23
      Width = 138
      Height = 385
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
    object pnlStrikesTop: TPanel
      Left = 0
      Top = 0
      Width = 138
      Height = 23
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Strikes'
      TabOrder = 1
    end
    object pnlStrikesBottom: TPanel
      Left = 0
      Top = 408
      Width = 138
      Height = 21
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object cbStrikesSelectAll: TCheckBox
        Left = 6
        Top = 2
        Width = 97
        Height = 17
        Caption = 'Select All'
        TabOrder = 0
        OnClick = cbStrikesSelectAllClick
      end
    end
  end
  object pnlExpirations: TPanel
    Left = 426
    Top = 0
    Width = 128
    Height = 429
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 3
    object pnlExpirationsBottom: TPanel
      Left = 0
      Top = 408
      Width = 128
      Height = 21
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object cbExpirationsSelectAll: TCheckBox
        Left = 14
        Top = 2
        Width = 97
        Height = 17
        Caption = 'Select All'
        TabOrder = 0
        OnClick = cbExpirationsSelectAllClick
      end
    end
    object lbExpirations: TCheckListBox
      Left = 0
      Top = 23
      Width = 128
      Height = 385
      Align = alClient
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
    end
    object pnlExpirationsTop: TPanel
      Left = 0
      Top = 0
      Width = 128
      Height = 23
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Expirations'
      TabOrder = 2
    end
  end
  object gbSearchDerivatives: TGroupBox
    Left = 0
    Top = 0
    Width = 273
    Height = 429
    Align = alLeft
    Constraints.MinHeight = 216
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 4
    object lblExchangeC: TLabel
      Left = 99
      Top = 48
      Width = 59
      Height = 16
      Alignment = taRightJustify
      Caption = 'Exchange:'
      Color = clDefault
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblUnderlyingSymbol: TLabel
      Left = 47
      Top = 21
      Width = 111
      Height = 16
      Alignment = taRightJustify
      Caption = 'Underlying Symbol:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblPrimaryExchange: TLabel
      Left = 51
      Top = 75
      Width = 107
      Height = 16
      Alignment = taRightJustify
      Caption = 'Primary Exchange:'
      Color = clDefault
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblSecurityTypeInstr: TLabel
      Left = 12
      Top = 102
      Width = 147
      Height = 16
      Alignment = taRightJustify
      Caption = 'Underlying Security Type:'
      Color = clDefault
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblUnderlyingContractID: TLabel
      Left = 25
      Top = 129
      Width = 133
      Height = 16
      Alignment = taRightJustify
      Caption = 'Underlying Contract ID:'
      Color = clDefault
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblLastPrice: TLabel
      Left = 142
      Top = 156
      Width = 60
      Height = 16
      Caption = 'Last Price:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnRequestContractDetails: TButton
      Left = 31
      Top = 151
      Width = 105
      Height = 30
      Caption = 'Request Chain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = btnRequestContractDetailsClick
    end
    object cbUnderlyingSymbol: TComboBox
      Left = 160
      Top = 18
      Width = 100
      Height = 24
      CharCase = ecUpperCase
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      Sorted = True
      TabOrder = 0
      Text = 'INDU'
      Items.Strings = (
        'INDU'
        'OMXS30')
    end
    object cbSecurityTypeInstr: TComboBox
      Left = 160
      Top = 99
      Width = 100
      Height = 24
      CharCase = ecUpperCase
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object cbExchange: TComboBox
      Left = 160
      Top = 45
      Width = 100
      Height = 24
      CharCase = ecUpperCase
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      Sorted = True
      TabOrder = 1
      Text = 'OMS'
      Items.Strings = (
        'OMS')
    end
    object cbPrimaryExchange: TComboBox
      Left = 160
      Top = 72
      Width = 100
      Height = 24
      CharCase = ecUpperCase
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Sorted = True
      TabOrder = 2
      Text = 'ECBOT'
    end
    object cbContractId: TComboBox
      Left = 160
      Top = 126
      Width = 100
      Height = 24
      CharCase = ecUpperCase
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Sorted = True
      TabOrder = 5
    end
    object rgTradingClass: TRadioGroup
      Left = 4
      Top = 185
      Width = 132
      Height = 100
      Caption = 'Select Trading Class'
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 6
      OnClick = rgTradingClassClick
    end
    object gbMultiSelect: TGroupBox
      Left = 138
      Top = 185
      Width = 132
      Height = 100
      Caption = 'Show Multi Select'
      TabOrder = 7
      object cbTradingClass: TCheckListBox
        Left = 1
        Top = 17
        Width = 130
        Height = 81
        Align = alTop
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clBtnFace
        TabOrder = 0
        OnClick = cbTradingClassClick
      end
    end
    object gbOptionCharacteristics: TGroupBox
      Left = 1
      Top = 291
      Width = 271
      Height = 137
      Align = alBottom
      Caption = 'Option Characteristics'
      TabOrder = 8
      object lblSymbolCaption: TLabel
        Left = 3
        Top = 21
        Width = 157
        Height = 16
        Alignment = taRightJustify
        Caption = 'Symbol Name/Trade Class:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblLocalSymbolCaption: TLabel
        Left = 81
        Top = 115
        Width = 80
        Height = 16
        Alignment = taRightJustify
        Caption = 'Local Symbol:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblLocalSymbol: TLabel
        Left = 166
        Top = 115
        Width = 4
        Height = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblCurrency: TLabel
        Left = 166
        Top = 95
        Width = 4
        Height = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblCurrencyCaption: TLabel
        Left = 105
        Top = 95
        Width = 56
        Height = 16
        Alignment = taRightJustify
        Caption = 'Currency:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblSecurityType: TLabel
        Left = 166
        Top = 76
        Width = 4
        Height = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblSecurityTypeCaption: TLabel
        Left = 78
        Top = 76
        Width = 83
        Height = 16
        Alignment = taRightJustify
        Caption = 'Security Type:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblSymbol: TLabel
        Left = 166
        Top = 21
        Width = 4
        Height = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblContractID: TLabel
        Left = 166
        Top = 39
        Width = 4
        Height = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblContractIDCaption: TLabel
        Left = 92
        Top = 39
        Width = 69
        Height = 16
        Alignment = taRightJustify
        Caption = 'Contract ID:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblExchangeCaption: TLabel
        Left = 102
        Top = 57
        Width = 59
        Height = 16
        Alignment = taRightJustify
        Caption = 'Exchange:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblExchange: TLabel
        Left = 166
        Top = 57
        Width = 4
        Height = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
  end
end
