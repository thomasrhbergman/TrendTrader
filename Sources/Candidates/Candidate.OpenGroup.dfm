object frmCandidateOpenGroup: TfrmCandidateOpenGroup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Group'
  ClientHeight = 143
  ClientWidth = 200
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 98
    Width = 200
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOk: TBitBtn
      Left = 101
      Top = 2
      Width = 95
      Height = 41
      Caption = 'Ok'
      Default = True
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
      Left = 5
      Top = 2
      Width = 95
      Height = 41
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
  object rgTemplateGroup: TRadioGroup
    Left = 0
    Top = 0
    Width = 200
    Height = 98
    Align = alClient
    Ctl3D = True
    ItemIndex = 0
    Items.Strings = (
      'IND (Independent)'
      'OCA (One Kills All)'
      'SEQ (Sequential)')
    ParentCtl3D = False
    TabOrder = 1
  end
end
