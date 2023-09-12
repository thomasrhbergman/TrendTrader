object frmCandidateTickColumn: TfrmCandidateTickColumn
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
  Position = poMainFormCenter
  TextHeight = 13
  object lblWeight: TLabel
    Left = 28
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
    Left = 17
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
  object lblValue: TLabel
    Left = 240
    Top = 34
    Width = 32
    Height = 16
    Alignment = taRightJustify
    Caption = 'Value'
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
      ParentFont = False
      TabOrder = 0
      OnClick = btnAddColumnClick
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
    Left = 79
    Top = 4
    Width = 134
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
    Left = 79
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
    OnChange = cbTypeOperationChange
  end
  object cbResult01: TCheckBox
    Left = 145
    Top = 33
    Width = 80
    Height = 17
    Caption = 'Result ( 0/1 )'
    TabOrder = 5
  end
  object cbResult01Inequality: TComboBox
    Left = 276
    Top = 34
    Width = 48
    Height = 21
    TabOrder = 6
    OnChange = cbTypeOperationChange
  end
  object edResult01Value: TNumberBox
    Left = 327
    Top = 34
    Width = 60
    Height = 21
    AcceptExpressions = True
    CurrencyString = '%'
    Mode = nbmFloat
    MaxValue = 100.000000000000000000
    TabOrder = 7
    Value = 1.000000000000000000
    UseMouseWheel = True
  end
end
