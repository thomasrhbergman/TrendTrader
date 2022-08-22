object frmEditOrderGroup: TfrmEditOrderGroup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'OrderGroup'
  ClientHeight = 158
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 8
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object lblType: TLabel
    Left = 10
    Top = 55
    Width = 28
    Height = 13
    Caption = 'Type:'
  end
  object edOrderGroupName: TEdit
    Left = 45
    Top = 9
    Width = 218
    Height = 21
    TabOrder = 0
  end
  object rgType: TRadioGroup
    Left = 44
    Top = 36
    Width = 219
    Height = 64
    ItemIndex = 0
    Items.Strings = (
      'Normal'
      'OneCancelAll (OCA)')
    TabOrder = 2
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 115
    Width = 272
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 190
    ExplicitWidth = 590
    DesignSize = (
      272
      43)
    object btnSave: TBitBtn
      Left = 162
      Top = 0
      Width = 109
      Height = 41
      Action = aSave
      Anchors = [akTop, akRight]
      Caption = 'Save'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
      ExplicitLeft = 479
    end
    object btnCancel: TBitBtn
      Left = 53
      Top = 0
      Width = 109
      Height = 41
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
      ExplicitLeft = 370
    end
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 214
    Top = 19
    object aSave: TAction
      Caption = 'Save'
      Enabled = False
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
      OnUpdate = aSaveUpdate
    end
  end
end
