object frmQualifiersEdit: TfrmQualifiersEdit
  Left = 0
  Top = 0
  Caption = 'Edit Qualifier'
  ClientHeight = 201
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 584
    Height = 156
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = -5
    ExplicitTop = -3
    ExplicitWidth = 944
    ExplicitHeight = 416
    DesignSize = (
      584
      156)
    object lblNote: TLabel
      Left = 40
      Top = 42
      Width = 31
      Height = 16
      Alignment = taRightJustify
      Caption = 'Note:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object pnlName: TPanel
      Left = 0
      Top = 0
      Width = 584
      Height = 36
      Align = alTop
      BevelOuter = bvNone
      Constraints.MinWidth = 526
      TabOrder = 0
      ExplicitWidth = 595
      object lblName: TLabel
        Left = 33
        Top = 9
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
      object edName: TEdit
        Left = 77
        Top = 8
        Width = 501
        Height = 21
        TabOrder = 0
      end
    end
    object edNote: TMemo
      Left = 77
      Top = 42
      Width = 501
      Height = 74
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      ExplicitWidth = 492
    end
    object cbEnabled: TCheckBox
      Left = 83
      Top = 122
      Width = 188
      Height = 17
      Caption = 'Activate Qualifier'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 156
    Width = 584
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 416
    ExplicitWidth = 947
    DesignSize = (
      584
      45)
    object btnOk: TBitBtn
      Left = 481
      Top = 3
      Width = 100
      Height = 41
      Action = aSaveQualifier
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
      ExplicitLeft = 844
    end
    object btnCancel: TBitBtn
      Left = 379
      Top = 3
      Width = 100
      Height = 41
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ModalResult = 2
      ParentFont = False
      TabOrder = 1
      ExplicitLeft = 742
    end
    object pnlInfo: TPanel
      Left = 0
      Top = 0
      Width = 369
      Height = 45
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object imgWarning: TVirtualImage
        Left = 0
        Top = 0
        Width = 32
        Height = 45
        Align = alLeft
        ImageCollection = DMImage.ImCollection32
        ImageWidth = 0
        ImageHeight = 0
        ImageIndex = 58
        ImageName = 'Warning_32x32'
        ExplicitTop = 2
      end
      object lblInfo: TLabel
        AlignWithMargins = True
        Left = 35
        Top = 3
        Width = 331
        Height = 39
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
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 344
    Top = 7
    object aSaveQualifier: TAction
      Caption = 'Save'
      Hint = 'Save'
      ImageIndex = 46
      ImageName = 'tick'
      ShortCut = 16467
      OnExecute = aSaveQualifierExecute
    end
  end
end
