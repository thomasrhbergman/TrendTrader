object frmEditOrderGroupSet: TfrmEditOrderGroupSet
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit Order Groups'
  ClientHeight = 109
  ClientWidth = 561
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  
  TextHeight = 13
  object lblName: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 66
    Width = 561
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnSave: TBitBtn
      Left = 464
      Top = 0
      Width = 95
      Height = 41
      Caption = 'Save'
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
      Left = 368
      Top = 0
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
  object edtName: TEdit
    Left = 41
    Top = 5
    Width = 495
    Height = 21
    TabOrder = 0
  end
  object pnlTypeUse: TPanel
    Left = 0
    Top = 28
    Width = 561
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object rbInstrumentTemplate: TRadioButton
      Tag = 2
      Left = 41
      Top = 19
      Width = 505
      Height = 17
      Caption = 
        'Instrument Template (XT generate but if Ordergroup opens from Mo' +
        'nitor instrument must be added)'
      TabOrder = 0
    end
    object rbAutoOrderTemplate: TRadioButton
      Tag = 1
      Left = 41
      Top = 1
      Width = 257
      Height = 17
      Caption = 'AutoOrder Template (XT generate order)'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
  end
end
