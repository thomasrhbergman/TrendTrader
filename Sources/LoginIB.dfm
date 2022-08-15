object frmLoginIB: TfrmLoginIB
  Left = 370
  Top = 241
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'IB Connection Parameters'
  ClientHeight = 130
  ClientWidth = 302
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  
  TextHeight = 13
  object lblIPAddress: TLabel
    Left = 8
    Top = 11
    Width = 186
    Height = 13
    Caption = 'IP Address (leave blank for local host):'
  end
  object lblPort: TLabel
    Left = 170
    Top = 38
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object lblClientID: TLabel
    Left = 149
    Top = 65
    Width = 45
    Height = 13
    Caption = 'Client ID:'
  end
  object eClientID: TEdit
    Left = 199
    Top = 62
    Width = 100
    Height = 21
    TabOrder = 2
    Text = '0'
  end
  object ePort: TEdit
    Left = 199
    Top = 35
    Width = 100
    Height = 21
    TabOrder = 1
    Text = '7496'
  end
  object eIPAddress: TEdit
    Left = 199
    Top = 8
    Width = 100
    Height = 21
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 88
    Width = 302
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      302
      42)
    object btnOk: TBitBtn
      Left = 202
      Top = 0
      Width = 100
      Height = 40
      Anchors = [akTop, akRight]
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
      OnClick = btnOkClick
    end
    object btnCancel: TBitBtn
      Left = 98
      Top = 0
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
    end
  end
end
