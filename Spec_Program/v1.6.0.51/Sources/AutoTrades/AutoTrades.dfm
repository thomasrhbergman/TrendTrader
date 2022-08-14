object frmAutoTrades: TfrmAutoTrades
  Left = 0
  Top = 0
  Caption = 'AutoTrade Templates'
  ClientHeight = 661
  ClientWidth = 482
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  PopupMenu = frameAutoTrades.pmTree
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 615
    Width = 482
    Height = 46
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      482
      46)
    object btnOpen: TBitBtn
      Left = 379
      Top = 1
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
      Left = 248
      Top = 1
      Width = 130
      Height = 41
      Action = aEdit
      Anchors = [akTop, akRight]
      Caption = 'Edit template'
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
      Left = 87
      Top = 1
      Width = 160
      Height = 41
      Action = aDelete
      Anchors = [akTop, akRight]
      Caption = 'Delete Autotrade'
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
  inline frameAutoTrades: TframeDocumentsTree
    Left = 0
    Top = 0
    Width = 482
    Height = 615
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ExplicitWidth = 482
    ExplicitHeight = 615
    inherited pnlOrderGroupSet: TPanel
      Width = 482
      Height = 615
      ExplicitWidth = 482
      ExplicitHeight = 615
      inherited pnlOrderGroupSetTop: TPanel
        Width = 482
        ExplicitWidth = 482
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
          Width = 386
          ExplicitWidth = 386
        end
        inherited btnClearSearchText: TBitBtn
          Left = 453
          ExplicitLeft = 453
        end
        inherited btnExportToCSVTemplate: TBitBtn
          Left = 407
          ExplicitLeft = 407
        end
        inherited btnPrintTemplate: TBitBtn
          Left = 371
          ExplicitLeft = 371
        end
        inherited btnExportToXLS: TBitBtn
          Left = 443
          ExplicitLeft = 443
        end
        inherited pnlDescription: TPanel
          Width = 482
          ExplicitWidth = 482
        end
      end
      inherited vstTree: TVirtualStringTree
        Width = 482
        Height = 526
        ExplicitWidth = 482
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
            Width = 87
          end>
      end
    end
    inherited pmTree: TPopupMenu
      object miCreate: TMenuItem [0]
        Action = aCreate
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
      inherited miSeparator02: TMenuItem
        Visible = False
      end
    end
    inherited alTree: TActionList
      inherited aModify: TAction
        Caption = 'Edit Selected Node'
      end
    end
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 96
    Top = 176
    object aCreate: TAction
      Caption = 'Create New Autotrade'
      ImageIndex = 52
      ImageName = 'AddItem_32x32'
      OnExecute = aCreateExecute
    end
    object aDelete: TAction
      Caption = 'Delete Autotrade'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      ShortCut = 46
      OnExecute = aDeleteExecute
    end
    object aCancel: TAction
      Caption = 'Cancel'
      OnExecute = aCancelExecute
    end
    object aEdit: TAction
      Caption = 'Edit template'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
      ShortCut = 16453
      OnExecute = aEditExecute
      OnUpdate = aEditUpdate
    end
    object aSelect: TAction
      Caption = 'Ok'
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSelectExecute
    end
  end
end
