object frmRelation: TfrmRelation
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Relation'
  ClientHeight = 123
  ClientWidth = 397
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  
  TextHeight = 13
  object Lb_ObjType1: TLabel
    Left = 4
    Top = 16
    Width = 101
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Parent object name:'
  end
  object Lb_ObjName1: TLabel
    Left = 111
    Top = 16
    Width = 279
    Height = 13
    AutoSize = False
    Caption = 'Lb_ObjName1'
  end
  object Lb_ObjName2: TLabel
    Left = 111
    Top = 35
    Width = 279
    Height = 13
    AutoSize = False
    Caption = 'Lb_ObjName2'
  end
  object Lb_ObjType2: TLabel
    Left = 25
    Top = 35
    Width = 80
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Object name:'
  end
  object Label1: TLabel
    Left = 14
    Top = 57
    Width = 91
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Calculation factor:'
  end
  object eCalcFactor: TEdit
    Left = 111
    Top = 54
    Width = 65
    Height = 21
    TabOrder = 0
    Text = '1.00000'
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 82
    Width = 397
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BitBtn1: TBitBtn
      Left = 290
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
    object Bb_Cancel: TBitBtn
      Left = 187
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
end
