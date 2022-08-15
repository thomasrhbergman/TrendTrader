object frmEditSokidList: TfrmEditSokidList
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit Sokid List '
  ClientHeight = 97
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  
  TextHeight = 13
  object lblName: TLabel
    Left = 33
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
  object lblDescription: TLabel
    Left = 3
    Top = 34
    Width = 68
    Height = 16
    Alignment = taRightJustify
    Caption = 'Description:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 56
    Width = 427
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnOk: TBitBtn
      Left = 329
      Top = 0
      Width = 95
      Height = 41
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
      Left = 232
      Top = 0
      Width = 95
      Height = 41
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
  object edName: TEdit
    Left = 74
    Top = 4
    Width = 350
    Height = 21
    TabOrder = 0
  end
  object edDescription: TEdit
    Left = 74
    Top = 31
    Width = 350
    Height = 21
    TabOrder = 1
  end
end
