object frmEditOrderGroup: TfrmEditOrderGroup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'OrderGroup'
  ClientHeight = 233
  ClientWidth = 590
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  
  TextHeight = 13
  object lblName: TLabel
    Left = 2
    Top = 9
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object lblType: TLabel
    Left = 4
    Top = 49
    Width = 28
    Height = 13
    Caption = 'Type:'
  end
  object lblNormal: TLabel
    Left = 226
    Top = 52
    Width = 266
    Height = 13
    Caption = '(All orders in the group will be unrelated to each other) '
  end
  object lblSequentialStop: TLabel
    Left = 226
    Top = 122
    Width = 166
    Height = 13
    Caption = '(shoots the next order if all is well)'
  end
  object lblSequentialContinue: TLabel
    Left = 226
    Top = 99
    Width = 217
    Height = 13
    Caption = '(fires the next order if the error or cancelled)'
  end
  object lblCheckpointPeriod: TLabel
    Left = 225
    Top = 146
    Width = 86
    Height = 13
    Caption = 'Checkpoint Period'
  end
  object lblMs: TLabel
    Left = 369
    Top = 146
    Width = 13
    Height = 13
    Caption = 'ms'
  end
  object cbRepetitive: TCheckBox
    Left = 268
    Top = 8
    Width = 68
    Height = 17
    Caption = 'Repetitive'
    TabOrder = 1
  end
  object edOrderGroupName: TEdit
    Left = 39
    Top = 6
    Width = 218
    Height = 21
    TabOrder = 0
  end
  object rgType: TRadioGroup
    Left = 38
    Top = 33
    Width = 179
    Height = 136
    ItemIndex = 0
    Items.Strings = (
      'Normal'
      'OneCancelAll (OCA)'
      'Sequential Continue (SEQ)'
      'Sequential Stop (SEQ STOP)'
      'Modify Order (MOD)')
    TabOrder = 3
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 190
    Width = 590
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 690
    object btnSave: TBitBtn
      Left = 479
      Top = 0
      Width = 109
      Height = 41
      Action = aSave
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
    end
    object btnCancel: TBitBtn
      Left = 370
      Top = 0
      Width = 109
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
    object pnlInfo: TPanel
      Left = 0
      Top = 0
      Width = 369
      Height = 43
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object imgWarning: TVirtualImage
        Left = 0
        Top = 0
        Width = 32
        Height = 43
        Align = alLeft
        ImageCollection = DMImage.ImCollection32
        ImageWidth = 0
        ImageHeight = 0
        ImageIndex = 58
        ImageName = 'Warning_32x32'
        ExplicitTop = 2
        ExplicitHeight = 45
      end
      object lblInfo: TLabel
        AlignWithMargins = True
        Left = 35
        Top = 3
        Width = 331
        Height = 37
        Align = alClient
        Caption = 
          'Changing the Document affects overall template in Template Creat' +
          'or'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 330
        ExplicitHeight = 13
      end
    end
  end
  object edCheckpointPeriod: TNumberBox
    Left = 320
    Top = 143
    Width = 44
    Height = 21
    TabOrder = 4
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 520
    Top = 16
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
