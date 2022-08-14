object frmScannerTickColumn: TfrmScannerTickColumn
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Column'
  ClientHeight = 104
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  
  TextHeight = 13
  object lblWeight: TLabel
    Left = 41
    Top = 34
    Width = 45
    Height = 16
    Alignment = taRightJustify
    Caption = 'Weight:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblTickType: TLabel
    Left = 30
    Top = 5
    Width = 56
    Height = 16
    Alignment = taRightJustify
    Caption = 'TickType:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 63
    Width = 392
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 121
    object btnAddColumn: TBitBtn
      Left = 249
      Top = 1
      Width = 142
      Height = 40
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
      Left = 147
      Top = 1
      Width = 100
      Height = 40
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
  object cbIBValue1: TComboBox
    Left = 92
    Top = 4
    Width = 120
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
  object cbIBValue2: TComboBox
    Left = 267
    Top = 4
    Width = 120
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
  object edWeight: TNumberBox
    Left = 92
    Top = 34
    Width = 60
    Height = 21
    AcceptExpressions = True
    CurrencyString = '%'
    Mode = nbmFloat
    MaxValue = 100.000000000000000000
    TabOrder = 3
    Value = 1.000000000000000000
    UseMouseWheel = True
  end
  object cbTypeOperation: TComboBox
    Left = 216
    Top = 4
    Width = 48
    Height = 21
    TabOrder = 4
  end
end
