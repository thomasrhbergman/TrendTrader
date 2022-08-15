object frmQuantityEdit: TfrmQuantityEdit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Quantity'
  ClientHeight = 239
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 194
    Width = 360
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 336
    ExplicitWidth = 617
    DesignSize = (
      360
      45)
    object btnSave: TBitBtn
      Left = 258
      Top = 2
      Width = 100
      Height = 40
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
      ExplicitLeft = 515
    end
    object btnCancel: TBitBtn
      Left = 157
      Top = 2
      Width = 100
      Height = 40
      Anchors = [akTop, akRight]
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
      ExplicitLeft = 414
    end
  end
  object pnlTypeCondition: TPanel
    Left = 0
    Top = 0
    Width = 360
    Height = 194
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = -4
    ExplicitHeight = 185
    object lblName: TLabel
      Left = 17
      Top = 7
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
    object lblSingleOrderAmount: TLabel
      Left = 17
      Top = 105
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
      Left = 17
      Top = 56
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
    object lblCurrency: TLabel
      Left = 17
      Top = 151
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
    object edtName: TEdit
      Left = 17
      Top = 29
      Width = 320
      Height = 21
      TabOrder = 0
    end
    object edTotalOrderAmount: TNumberBox
      Left = 17
      Top = 78
      Width = 65
      Height = 21
      AcceptExpressions = True
      TabOrder = 1
      SpinButtonOptions.Placement = nbspCompact
      UseMouseWheel = True
      NegativeValueColor = clRed
    end
    object edSingleOrderAmount: TNumberBox
      Left = 17
      Top = 127
      Width = 65
      Height = 21
      AcceptExpressions = True
      TabOrder = 2
      SpinButtonOptions.Placement = nbspCompact
      UseMouseWheel = True
      NegativeValueColor = clRed
    end
    object cbOrderCurrency: TComboBox
      Left = 17
      Top = 169
      Width = 65
      Height = 21
      CharCase = ecUpperCase
      TabOrder = 3
    end
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 232
    Top = 87
    object aSave: TAction
      Caption = 'Save'
      ImageIndex = 46
      ImageName = 'tick'
      ShortCut = 16467
      OnExecute = aSaveExecute
    end
  end
end
