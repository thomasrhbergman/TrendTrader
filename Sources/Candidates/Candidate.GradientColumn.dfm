object frmCandidateGradientColumn: TfrmCandidateGradientColumn
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
    Left = 94
    Top = 35
    Width = 98
    Height = 16
    Alignment = taRightJustify
    Caption = 'Monitoring (sec):'
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
    ExplicitTop = 91
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
    Height = 22
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
  object pnlValues: TPanel
    Left = 146
    Top = 3
    Width = 238
    Height = 29
    BevelOuter = bvNone
    TabOrder = 3
    object Label1: TLabel
      Left = 38
      Top = 6
      Width = 49
      Height = 16
      Alignment = taRightJustify
      Caption = 'Between'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 154
      Top = 6
      Width = 21
      Height = 16
      Alignment = taCenter
      Caption = 'and'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edValue2: TNumberBox
      Left = 181
      Top = 5
      Width = 57
      Height = 21
      AcceptExpressions = True
      CurrencyString = '%'
      Mode = nbmFloat
      TabOrder = 0
      UseMouseWheel = True
    end
    object edValue1: TNumberBox
      Left = 93
      Top = 5
      Width = 57
      Height = 21
      AcceptExpressions = True
      CurrencyString = '%'
      Mode = nbmFloat
      TabOrder = 1
      UseMouseWheel = True
    end
  end
  object seDuration: TSpinEdit
    Left = 198
    Top = 35
    Width = 57
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 1
  end
end
