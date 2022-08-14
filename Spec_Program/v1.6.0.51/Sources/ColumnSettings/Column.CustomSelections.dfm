object frmCustomColumnSelections: TfrmCustomColumnSelections
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Column Selections'
  ClientHeight = 411
  ClientWidth = 439
  Color = clBtnFace
  CustomTitleBar.CaptionAlignment = taCenter
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
    Top = 371
    Width = 439
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      439
      40)
    object btnSave: TBitBtn
      Left = 334
      Top = 0
      Width = 100
      Height = 40
      Anchors = [akTop, akRight]
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
      Left = 233
      Top = 0
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
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 439
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object cbShowFreezedColumns: TCheckBox
      Left = 202
      Top = 5
      Width = 149
      Height = 17
      Caption = 'Show Freezed Columns'
      Checked = True
      State = cbChecked
      TabOrder = 0
      Visible = False
    end
    object cbShowAllFactors: TCheckBox
      Left = 14
      Top = 5
      Width = 149
      Height = 17
      Caption = 'List all factor subscriptions'
      TabOrder = 1
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 25
    Width = 439
    Height = 346
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 200
      Height = 346
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object lbSrc: TListBox
        Left = 0
        Top = 0
        Width = 200
        Height = 346
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        Sorted = True
        TabOrder = 0
        OnDblClick = lbSrcDblClick
      end
    end
    object pnlRight: TPanel
      Left = 239
      Top = 0
      Width = 200
      Height = 346
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object lbDest: TListBox
        Left = 0
        Top = 0
        Width = 200
        Height = 346
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 0
        OnDblClick = lbDestDblClick
      end
    end
    object pnlCenter: TPanel
      Left = 200
      Top = 0
      Width = 39
      Height = 346
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object spbDown: TSpeedButton
        Left = 2
        Top = 178
        Width = 36
        Height = 36
        Action = aDown
        Images = DMImage.vil32
      end
      object spbUp: TSpeedButton
        Left = 2
        Top = 142
        Width = 36
        Height = 36
        Action = aUp
        Images = DMImage.vil32
      end
      object spbRemove: TSpeedButton
        Left = 2
        Top = 44
        Width = 36
        Height = 36
        Action = aRemove
        Images = DMImage.vil32
      end
      object spbInsert: TSpeedButton
        Left = 2
        Top = 8
        Width = 36
        Height = 36
        Action = aInsert
        Images = DMImage.vil32
      end
    end
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 312
    Top = 87
    object aInsert: TAction
      ImageIndex = 28
      ImageName = 'Right_32x32'
      OnExecute = aInsertExecute
    end
    object aRemove: TAction
      ImageIndex = 27
      ImageName = 'Left_32x32'
      OnExecute = aRemoveExecute
    end
    object aUp: TAction
      ImageIndex = 29
      ImageName = 'Up2_32x32'
      OnExecute = aUpExecute
    end
    object aDown: TAction
      ImageIndex = 30
      ImageName = 'Fill_32x32'
      OnExecute = aDownExecute
    end
  end
end
