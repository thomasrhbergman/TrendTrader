object frmAutoTradesEdit: TfrmAutoTradesEdit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'AutoTrade Edit'
  ClientHeight = 497
  ClientWidth = 574
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 460
    Width = 574
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnSave: TBitBtn
      Left = 471
      Top = 0
      Width = 100
      Height = 36
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
      Left = 370
      Top = 0
      Width = 100
      Height = 36
      Action = aCancel
      Cancel = True
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
    object pnlInfo: TPanel
      Left = 0
      Top = 0
      Width = 369
      Height = 37
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object imgWarning: TVirtualImage
        Left = 0
        Top = 0
        Width = 32
        Height = 37
        Align = alLeft
        ImageCollection = DMImage.ImCollection32
        ImageWidth = 0
        ImageHeight = 0
        ImageIndex = 58
        ImageName = 'Warning_32x32'
        ExplicitTop = 2
        ExplicitHeight = 45
      end
      object lblInfo: TLabel
        AlignWithMargins = True
        Left = 35
        Top = 3
        Width = 331
        Height = 31
        Align = alClient
        Caption = 
          'Changing the Document affects overall template in Template Creat' +
          'or'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 330
        ExplicitHeight = 13
      end
    end
  end
  object gbRules: TGroupBox
    Left = 0
    Top = 45
    Width = 574
    Height = 296
    Align = alClient
    Caption = 'Specify Order Rules for the Autotrade'
    TabOrder = 0
    object cbOrderCurrencyAdd: TSpeedButton
      Left = 276
      Top = 44
      Width = 23
      Height = 22
      Hint = 'Add currency to list'
      ImageIndex = 41
      ImageName = 'plus'
      Images = DMImage.vil16
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = cbOrderCurrencyAddClick
    end
    object lblMaxRows: TLabel
      Left = 36
      Top = 95
      Width = 99
      Height = 16
      Alignment = taRightJustify
      Caption = 'Limit of Symbols:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblMaxNumberOrder: TLabel
      Left = 9
      Top = 70
      Width = 126
      Height = 16
      Alignment = taRightJustify
      Caption = 'Max number of order:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblSingleOrderAmount: TLabel
      Left = 13
      Top = 45
      Width = 122
      Height = 16
      Alignment = taRightJustify
      Caption = 'Single order amount:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblTotalOrderAmount: TLabel
      Left = 19
      Top = 20
      Width = 116
      Height = 16
      Alignment = taRightJustify
      Caption = 'Total order amount:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object cbOrderCurrency: TComboBox
      Left = 210
      Top = 44
      Width = 64
      Height = 21
      CharCase = ecUpperCase
      TabOrder = 0
      OnChange = OnGUIToAutoTradeInfo
    end
    object cbEnabled: TCheckBox
      Left = 141
      Top = 146
      Width = 188
      Height = 17
      Caption = 'Activate/Deactivate'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = OnGUIToAutoTradeInfo
    end
    object cbAllowSendDuplicateOrder: TCheckBox
      Left = 141
      Top = 123
      Width = 188
      Height = 17
      Caption = 'Allow send duplicate order'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = OnGUIToAutoTradeInfo
    end
    object edTotalOrderAmount: TNumberBox
      Left = 141
      Top = 19
      Width = 65
      Height = 21
      AcceptExpressions = True
      TabOrder = 3
      SpinButtonOptions.Placement = nbspCompact
      UseMouseWheel = True
      NegativeValueColor = clRed
      OnChange = OnGUIToAutoTradeInfo
      OnChangeValue = OnGUIToAutoTradeInfo
    end
    object edSingleOrderAmount: TNumberBox
      Left = 141
      Top = 44
      Width = 65
      Height = 21
      AcceptExpressions = True
      TabOrder = 4
      SpinButtonOptions.Placement = nbspCompact
      UseMouseWheel = True
      NegativeValueColor = clRed
      OnChange = OnGUIToAutoTradeInfo
      OnChangeValue = edMaxNumberOrderChangeValue
    end
    object edMaxNumberOrder: TNumberBox
      Left = 141
      Top = 69
      Width = 65
      Height = 21
      AcceptExpressions = True
      TabOrder = 5
      SpinButtonOptions.Placement = nbspCompact
      UseMouseWheel = True
      NegativeValueColor = clRed
      OnChange = OnGUIToAutoTradeInfo
      OnChangeValue = edMaxNumberOrderChangeValue
    end
    object edMaxRows: TNumberBox
      Left = 141
      Top = 94
      Width = 65
      Height = 21
      AcceptExpressions = True
      TabOrder = 6
      Value = 10.000000000000000000
      SpinButtonOptions.Placement = nbspCompact
      UseMouseWheel = True
      NegativeValueColor = clRed
      OnChange = OnGUIToAutoTradeInfo
    end
    object cbSubscribeHistoricalData: TCheckBox
      Left = 141
      Top = 166
      Width = 197
      Height = 17
      Hint = 'For Calculation of Trendlines we use historical prices'
      Caption = 'Subscribe Historical Data'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnClick = OnGUIToAutoTradeInfo
    end
    object gbHistoricalOptions: TGroupBox
      Left = 141
      Top = 189
      Width = 313
      Height = 100
      Caption = 'Historical Options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      object lblValidBarSize: TLabel
        Left = 15
        Top = 21
        Width = 82
        Height = 16
        Alignment = taRightJustify
        Caption = 'Valid bar size:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblDuration: TLabel
        Left = 44
        Top = 49
        Width = 53
        Height = 16
        Alignment = taRightJustify
        Caption = 'Duration:'
      end
      object cbValidBarSize: TComboBox
        Left = 103
        Top = 18
        Width = 85
        Height = 24
        TabOrder = 0
        OnChange = OnGUIToAutoTradeInfo
      end
      object cbHistDataKeepUpdated: TCheckBox
        Left = 103
        Top = 75
        Width = 197
        Height = 17
        Hint = 'For Calculation of Trendlines we use historical prices'
        Caption = 'Keep Historical Data Updated'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = OnGUIToAutoTradeInfo
      end
      object cbDurationTimeUnits: TComboBox
        Left = 103
        Top = 45
        Width = 85
        Height = 24
        TabOrder = 2
        OnChange = OnGUIToAutoTradeInfo
      end
      object edDuration: TNumberBox
        Left = 193
        Top = 45
        Width = 73
        Height = 24
        TabOrder = 3
        Value = 1.000000000000000000
        SpinButtonOptions.Placement = nbspCompact
        UseMouseWheel = True
        OnChange = OnGUIToAutoTradeInfo
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 574
    Height = 45
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object lblName: TLabel
      Left = 97
      Top = 10
      Width = 38
      Height = 16
      Alignment = taRightJustify
      Caption = 'Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnShowScannerMain: TBitBtn
      Left = 457
      Top = 7
      Width = 114
      Height = 25
      Action = aShowScanner
      Caption = 'Show Scanner'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Glyph.Data = {
        36050000424D3605000000000000360400002800000010000000100000000100
        08000000000000010000C30E0000C30E00000001000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A6000020400000206000002080000020A0000020C0000020E000004000000040
        20000040400000406000004080000040A0000040C0000040E000006000000060
        20000060400000606000006080000060A0000060C0000060E000008000000080
        20000080400000806000008080000080A0000080C0000080E00000A0000000A0
        200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
        200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
        200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
        20004000400040006000400080004000A0004000C0004000E000402000004020
        20004020400040206000402080004020A0004020C0004020E000404000004040
        20004040400040406000404080004040A0004040C0004040E000406000004060
        20004060400040606000406080004060A0004060C0004060E000408000004080
        20004080400040806000408080004080A0004080C0004080E00040A0000040A0
        200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
        200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
        200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
        20008000400080006000800080008000A0008000C0008000E000802000008020
        20008020400080206000802080008020A0008020C0008020E000804000008040
        20008040400080406000804080008040A0008040C0008040E000806000008060
        20008060400080606000806080008060A0008060C0008060E000808000008080
        20008080400080806000808080008080A0008080C0008080E00080A0000080A0
        200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
        200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
        200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
        2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
        2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
        2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
        2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
        2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
        2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
        2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003E3E3E3E3E3E
        3E3E3E3E3E3E3E3E3E3E3E3EA3A4F7ACF7A4F7A4A4A4F7AC3E3E3E3EA4F7F7F7
        F7F7F7F7F7F7F7F73E3E3E0799A2A39AA3A2A3A3A3A2A29AA33E3EF7919A9A9A
        9AE3E39A9A9A9A999A3E3E3E3E3E3E3E3E9A9A3E3E3E3E3E3E3E3E3E3E3E3E3E
        3E9A9A3E3E3E3E3E3E3E3E3E3E3E3E3E3E9A993E3E3E3E3E3E3E3E3E3E3E3E3E
        3E9A993E3E3E3E3E3E3E3E3E3E3E3E3E3E9A993E3E3E3E3E3E3E3E3E3E3E3E3E
        3E9A993E3E3E3E3E3E3E3E3E3E3E3E3E3E9A993E3E3E3E3E3E3E3E3E3E3E3E3E
        3E9A993E3E3E3E3E3E3E3E3E3E3E3E3E3E9A993E3E3E3E3E3E3E3E3E3E3E3E3E
        3E91913E3E3E3E3E3E3E3E3E3E3E3E3E3EF5ED3E3E3E3E3E3E3E}
      ParentFont = False
      TabOrder = 0
    end
    object edName: TEdit
      Left = 141
      Top = 8
      Width = 313
      Height = 21
      TabOrder = 1
      OnChange = OnGUIToAutoTradeInfo
    end
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 341
    Width = 574
    Height = 119
    Align = alBottom
    Caption = '  Info  '
    TabOrder = 3
    object edNote: TMemo
      Left = 2
      Top = 15
      Width = 570
      Height = 102
      Align = alClient
      TabOrder = 0
    end
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 471
    Top = 109
    object aSave: TAction
      Caption = 'Save'
      Hint = 'Save'
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
    end
    object aCancel: TAction
      Caption = 'Cancel'
      OnExecute = aCancelExecute
    end
    object aShowScanner: TAction
      Caption = 'Show Scanner'
      OnExecute = aShowScannerExecute
      OnUpdate = aShowScannerUpdate
    end
  end
end
