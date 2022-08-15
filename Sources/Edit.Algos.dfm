object frmEditAlgos: TfrmEditAlgos
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Calculated Value'
  ClientHeight = 118
  ClientWidth = 348
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object lblAlgosName: TLabel
    Left = 8
    Top = 8
    Width = 81
    Height = 13
    AutoSize = False
    Caption = 'Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblValueCaption: TLabel
    Left = 8
    Top = 53
    Width = 34
    Height = 14
    Caption = 'Value:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblValue: TLabel
    Left = 50
    Top = 53
    Width = 7
    Height = 14
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edAlgosName: TEdit
    Left = 8
    Top = 23
    Width = 322
    Height = 21
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 75
    Width = 348
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 102
    DesignSize = (
      348
      43)
    object btnOk: TBitBtn
      Left = 241
      Top = 2
      Width = 100
      Height = 40
      Action = aSave
      Anchors = [akTop, akRight]
      Caption = 'Ok'
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
      Left = 139
      Top = 2
      Width = 100
      Height = 40
      Anchors = [akTop, akRight]
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
    object btnShowChart: TBitBtn
      Left = 37
      Top = 2
      Width = 100
      Height = 40
      Action = aGraph
      Anchors = [akTop, akRight]
      Caption = 'Graph'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 2
    end
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 272
    Top = 8
    object aSave: TAction
      Caption = 'Ok'
      Enabled = False
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
      OnUpdate = aSaveUpdate
    end
    object aGraph: TAction
      Caption = 'Graph'
      Hint = 'Graph'
      ImageIndex = 48
      ImageName = 'Area_32x32'
      OnExecute = aGraphExecute
    end
  end
end
