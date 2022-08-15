object frmOrderGroupSet: TfrmOrderGroupSet
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Open OrderGroups'
  ClientHeight = 666
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  StyleElements = [seFont, seClient]
  
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 620
    Width = 484
    Height = 46
    Align = alBottom
    TabOrder = 1
    object btnOpen: TBitBtn
      Left = 276
      Top = 2
      Width = 206
      Height = 41
      Action = aSelect
      Caption = 'Open  Set of OrderGroup'
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
      Left = 166
      Top = 2
      Width = 109
      Height = 41
      Action = aEdit
      Caption = 'Edit'
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
      Left = 56
      Top = 2
      Width = 109
      Height = 41
      Action = aDelete
      Caption = 'Delete'
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
  inline frameOrderGroups: TframeDocumentsTree
    Left = 0
    Top = 0
    Width = 484
    Height = 620
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ExplicitWidth = 484
    ExplicitHeight = 620
    inherited pnlOrderGroupSet: TPanel
      Width = 484
      Height = 620
      ExplicitWidth = 484
      ExplicitHeight = 620
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
          Width = 390
          ExplicitWidth = 390
        end
        inherited btnClearSearchText: TBitBtn
          Left = 457
          ExplicitLeft = 457
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
        Height = 531
        PopupMenu = PopupMenu
        OnDblClick = aSelectExecute
        ExplicitWidth = 484
        ExplicitHeight = 531
        Columns = <
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 0
            Text = 'Items'
            Width = 230
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
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 3
            Text = 'Record Id'
            Width = 44
          end>
      end
    end
  end
  object ActionListMain: TActionList
    Images = DMImage.vil32
    Left = 144
    Top = 168
    object aSelect: TAction
      Caption = 'Open  Set of OrderGroup'
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSelectExecute
    end
    object aDelete: TAction
      Caption = 'Delete'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      ShortCut = 46
      OnExecute = aDeleteExecute
    end
    object aEdit: TAction
      Caption = 'Edit'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
      OnExecute = aEditExecute
    end
  end
  object PopupMenu: TPopupMenu
    Left = 144
    Top = 120
    object miEdit: TMenuItem
      Action = aEdit
    end
    object miDelete: TMenuItem
      Action = aDelete
    end
  end
end
