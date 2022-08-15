object frmScannerEditWeight: TfrmScannerEditWeight
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit Weight'
  ClientHeight = 80
  ClientWidth = 279
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
    Left = 11
    Top = 9
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
  object pnlBottom: TPanel
    Left = 0
    Top = 39
    Width = 279
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnSave: TBitBtn
      Left = 176
      Top = 1
      Width = 100
      Height = 40
      Caption = 'Save'
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
      Left = 73
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
  object edtWeight: TNumberBox
    Left = 63
    Top = 8
    Width = 65
    Height = 21
    AcceptExpressions = True
    Mode = nbmFloat
    TabOrder = 1
    Value = 1.000000000000000000
    SpinButtonOptions.Placement = nbspCompact
    UseMouseWheel = True
  end
end
