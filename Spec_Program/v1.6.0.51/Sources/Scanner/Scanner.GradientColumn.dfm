object frmScannerGradientColumn: TfrmScannerGradientColumn
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Column'
  ClientHeight = 105
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
    Left = 265
    Top = 35
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
  object lblDuration: TLabel
    Left = 267
    Top = 8
    Width = 43
    Height = 16
    Alignment = taRightJustify
    Caption = 'Weeks:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 64
    Width = 392
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
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
  object edWeight: TNumberBox
    Left = 328
    Top = 35
    Width = 57
    Height = 21
    AcceptExpressions = True
    CurrencyString = '%'
    Mode = nbmFloat
    MaxValue = 100.000000000000000000
    TabOrder = 1
    Value = 1.000000000000000000
    UseMouseWheel = True
  end
  object cbCalcType: TComboBox
    Left = 8
    Top = 8
    Width = 130
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = OnChangeVisibility
  end
  object seDuration: TSpinEdit
    Left = 328
    Top = 7
    Width = 57
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 1
  end
end
