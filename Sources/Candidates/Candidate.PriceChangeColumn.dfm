object frmCandidatePriceChangeColumn: TfrmCandidatePriceChangeColumn
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add PriceChange'
  ClientHeight = 136
  ClientWidth = 263
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 95
    Width = 263
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 202
    ExplicitWidth = 635
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
      ExplicitLeft = 464
      ExplicitTop = 1
      ExplicitHeight = 40
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
      ExplicitLeft = 362
      ExplicitTop = 1
      ExplicitHeight = 40
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
    ExplicitWidth = 332
    object lblLastTicks: TLabel
      Left = 10
      Top = 7
      Width = 78
      Height = 13
      Caption = 'Last # ticks are:'
    end
    object lblLastTickType: TLabel
      Left = 100
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
    ExplicitWidth = 332
    object lblLastPriceType: TLabel
      Left = 100
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
      Left = 10
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
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitWidth = 332
    object lblWeight: TLabel
      Left = 10
      Top = 6
      Width = 43
      Height = 13
      Caption = 'Weight:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edWeight: TNumberBox
      Left = 164
      Top = 5
      Width = 50
      Height = 21
      Mode = nbmFloat
      TabOrder = 0
      Value = 10.000000000000000000
    end
  end
end
