object frmAutoTradesEdit: TfrmAutoTradesEdit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'AutoTrade Edit'
  ClientHeight = 327
  ClientWidth = 355
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 290
    Width = 355
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      355
      37)
    object btnSave: TBitBtn
      Left = 252
      Top = 0
      Width = 100
      Height = 36
      Action = aSave
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
    end
    object btnCancel: TBitBtn
      Left = 151
      Top = 0
      Width = 100
      Height = 36
      Action = aCancel
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
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 355
    Height = 290
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object lblName: TLabel
      Left = 13
      Top = 6
      Width = 33
      Height = 16
      Alignment = taRightJustify
      Caption = 'Name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblQualifier: TLabel
      Left = 13
      Top = 57
      Width = 48
      Height = 16
      Alignment = taRightJustify
      Caption = 'Qualifier'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnSelectQualifier: TSpeedButton
      Left = 322
      Top = 79
      Width = 23
      Height = 21
      ImageIndex = 9
      ImageName = 'TextBox_32x32'
      Images = DMImage.vil16
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = btnSelectQualifierClick
    end
    object lblCandidate: TLabel
      Left = 13
      Top = 106
      Width = 57
      Height = 16
      Alignment = taRightJustify
      Caption = 'Candidate'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnSelectCandidate: TSpeedButton
      Left = 322
      Top = 130
      Width = 23
      Height = 21
      ImageIndex = 9
      ImageName = 'TextBox_32x32'
      Images = DMImage.vil16
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = btnSelectCandidateClick
    end
    object lblQuantity: TLabel
      Left = 13
      Top = 159
      Width = 47
      Height = 16
      Alignment = taRightJustify
      Caption = 'Quantity'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnSelectQuantity: TSpeedButton
      Left = 322
      Top = 181
      Width = 23
      Height = 21
      ImageIndex = 9
      ImageName = 'TextBox_32x32'
      Images = DMImage.vil16
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = btnSelectQuantityClick
    end
    object lblOrderTemplate: TLabel
      Left = 13
      Top = 210
      Width = 91
      Height = 16
      Alignment = taRightJustify
      Caption = 'Order Template'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnSelectOrderTemplate: TSpeedButton
      Left = 322
      Top = 232
      Width = 23
      Height = 21
      ImageIndex = 9
      ImageName = 'TextBox_32x32'
      Images = DMImage.vil16
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = btnSelectOrderTemplateClick
    end
    object edName: TEdit
      Left = 8
      Top = 28
      Width = 313
      Height = 21
      TabOrder = 0
      OnChange = OnGUIToAutoTradeInfo
    end
    object cbActive: TCheckBox
      Left = 8
      Top = 259
      Width = 72
      Height = 17
      Caption = 'Active'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object edQualifier: TEdit
      Left = 8
      Top = 79
      Width = 313
      Height = 21
      ReadOnly = True
      TabOrder = 2
      OnChange = OnGUIToAutoTradeInfo
    end
    object edCandidate: TEdit
      Left = 8
      Top = 130
      Width = 313
      Height = 21
      ReadOnly = True
      TabOrder = 3
      OnChange = OnGUIToAutoTradeInfo
    end
    object edQuantity: TEdit
      Left = 8
      Top = 181
      Width = 313
      Height = 21
      ReadOnly = True
      TabOrder = 4
      OnChange = OnGUIToAutoTradeInfo
    end
    object edOrderTemplate: TEdit
      Left = 8
      Top = 232
      Width = 313
      Height = 21
      ReadOnly = True
      TabOrder = 5
      OnChange = OnGUIToAutoTradeInfo
    end
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 287
    Top = 13
    object aSave: TAction
      Caption = 'Save'
      Hint = 'Save'
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
    end
    object aCancel: TAction
      Caption = 'Cancel'
      OnExecute = aCancelExecute
    end
    object aShowScanner: TAction
      Caption = 'Show Scanner'
    end
  end
end
