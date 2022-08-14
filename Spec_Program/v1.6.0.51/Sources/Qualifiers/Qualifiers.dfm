object frmQualifiers: TfrmQualifiers
  Left = 0
  Top = 0
  Caption = 'Qualifiers'
  ClientHeight = 661
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  PopupMenu = frameQualifiers.pmTree
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 615
    Width = 484
    Height = 46
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      484
      46)
    object btnOpen: TBitBtn
      Left = 381
      Top = 2
      Width = 100
      Height = 41
      Action = aSelect
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 0
    end
    object btnEdit: TBitBtn
      Left = 241
      Top = 2
      Width = 140
      Height = 41
      Action = aEditQualifier
      Anchors = [akTop, akRight]
      Caption = 'Edit Qualifier'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 1
    end
    object btnDelete: TBitBtn
      Left = 88
      Top = 2
      Width = 152
      Height = 41
      Action = aDeleteQualifier
      Anchors = [akTop, akRight]
      Caption = 'Delete Qualifier'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 2
    end
  end
  inline frameQualifiers: TframeDocumentsTree
    Left = 0
    Top = 0
    Width = 484
    Height = 615
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ExplicitWidth = 484
    ExplicitHeight = 615
    inherited pnlOrderGroupSet: TPanel
      Width = 484
      Height = 615
      ExplicitWidth = 484
      ExplicitHeight = 615
      inherited pnlOrderGroupSetTop: TPanel
        Width = 484
        ExplicitWidth = 484
        inherited lblViewType: TLabel
          Left = 8
          Width = 53
          Height = 13
          ExplicitLeft = 8
          ExplicitWidth = 53
          ExplicitHeight = 13
        end
        inherited lblSearchFor: TLabel
          Left = 24
          Width = 37
          Height = 13
          ExplicitLeft = 24
          ExplicitWidth = 37
          ExplicitHeight = 13
        end
        inherited edtSearch: TEdit
          Width = 391
          ExplicitWidth = 391
        end
        inherited btnClearSearchText: TBitBtn
          Left = 458
          ExplicitLeft = 458
        end
        inherited btnExportToCSVTemplate: TBitBtn
          Left = 410
          ExplicitLeft = 410
        end
        inherited btnPrintTemplate: TBitBtn
          Left = 374
          ExplicitLeft = 374
        end
        inherited btnExportToXLS: TBitBtn
          Left = 446
          ExplicitLeft = 446
        end
        inherited pnlDescription: TPanel
          Width = 484
          ExplicitWidth = 484
        end
      end
      inherited vstTree: TVirtualStringTree
        Width = 484
        Height = 526
        Alignment = taRightJustify
        ExplicitWidth = 484
        ExplicitHeight = 526
        Columns = <
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 0
            Text = 'Items'
            Width = 204
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 1
            Text = 'Value'
            Width = 91
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 2
            Text = 'Calc Type'
            Width = 100
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 3
            Text = 'Record Id'
            Width = 70
          end>
      end
    end
    inherited pmTree: TPopupMenu
      Top = 136
      object miCreate: TMenuItem [0]
        Action = aCreate
      end
      inherited miModify: TMenuItem
        Caption = 'Edit Selected Node'
      end
      inherited miSeparator01: TMenuItem
        Visible = False
      end
      inherited miAddOrderGroup: TMenuItem
        Visible = False
      end
      inherited miAddOrder: TMenuItem
        Visible = False
      end
      inherited miAddCondition: TMenuItem
        Visible = False
      end
      inherited miAddAlgos: TMenuItem
        Visible = False
      end
      inherited miAddFactor: TMenuItem
        Visible = False
      end
      inherited miCollapsSelectedNode: TMenuItem [14]
      end
      inherited miExpandSelectedNode: TMenuItem [15]
      end
    end
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 96
    Top = 202
    object aCreate: TAction
      Caption = 'Create New Qualifier'
      ImageIndex = 52
      ImageName = 'AddItem_32x32'
      OnExecute = aCreateExecute
    end
    object aEditQualifier: TAction
      Caption = 'Edit Qualifier'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
      ShortCut = 16453
      OnExecute = aEditQualifierExecute
      OnUpdate = aEditQualifierUpdate
    end
    object aDeleteQualifier: TAction
      Caption = 'Delete Qualifier'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aDeleteQualifierExecute
    end
    object aSelect: TAction
      Caption = 'Ok'
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSelectExecute
    end
  end
end
