object frmCandidatePriceChangeColumn: TfrmCandidatePriceChangeColumn
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add PriceChange'
  ClientHeight = 158
  ClientWidth = 263
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 117
    Width = 263
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 95
    object btnAddColumn: TBitBtn
      Left = 121
      Top = 0
      Width = 142
      Height = 41
      Align = alRight
      Caption = 'Add column'
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
      Left = 20
      Top = 0
      Width = 101
      Height = 41
      Align = alRight
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
  object pnlNumberTicks: TPanel
    Left = 0
    Top = 0
    Width = 263
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblLastTicks: TLabel
      Left = 31
      Top = 7
      Width = 78
      Height = 13
      Caption = 'Last # ticks are:'
    end
    object lblLastTickType: TLabel
      Left = 121
      Top = 7
      Width = 28
      Height = 13
      Caption = 'none'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object seLastTickCount: TSpinEdit
      Left = 164
      Top = 4
      Width = 50
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 3
    end
  end
  object pnlLastPrice: TPanel
    Left = 0
    Top = 30
    Width = 263
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object lblLastPriceType: TLabel
      Left = 124
      Top = 8
      Width = 28
      Height = 13
      Caption = 'none'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cbLastPrice: TCheckBox
      Left = 34
      Top = 7
      Width = 75
      Height = 17
      Caption = 'Last Price ='
      TabOrder = 0
    end
  end
  object pnlWeight: TPanel
    Left = 0
    Top = 60
    Width = 263
    Height = 54
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object lblTimeWeight: TLabel
      Left = 35
      Top = 6
      Width = 74
      Height = 13
      Caption = 'Time Weight:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblWeight: TLabel
      Left = 19
      Top = 34
      Width = 92
      Height = 16
      Alignment = taRightJustify
      Caption = 'Column Weight:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edTimeWeight: TNumberBox
      Left = 164
      Top = 5
      Width = 50
      Height = 21
      Mode = nbmFloat
      TabOrder = 0
      Value = 10.000000000000000000
    end
    object edWeight: TNumberBox
      Left = 164
      Top = 30
      Width = 50
      Height = 21
      AcceptExpressions = True
      CurrencyString = '%'
      Mode = nbmFloat
      MaxValue = 100.000000000000000000
      TabOrder = 1
      Value = 1.000000000000000000
      UseMouseWheel = True
    end
  end
end
