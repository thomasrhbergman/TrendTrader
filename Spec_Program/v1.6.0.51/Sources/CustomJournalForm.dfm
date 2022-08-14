object frmCustomJournalForm: TfrmCustomJournalForm
  Left = 0
  Top = 0
  Caption = 'vmsCustomJournalForm'
  ClientHeight = 570
  ClientWidth = 925
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  
  TextHeight = 13
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 925
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      925
      38)
    object btnExportToExcel: TBitBtn
      Left = 888
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aExportToExcel
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object btnExportToCSV: TBitBtn
      Left = 852
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aExportToCSV
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object btnPrint: TBitBtn
      Left = 816
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aPrint
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object btnColumnSettings: TBitBtn
      Left = 780
      Top = 1
      Width = 36
      Height = 36
      Action = aColumnSettings
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 38
    Width = 925
    Height = 532
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object vstTree: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 925
      Height = 532
      Align = alClient
      Header.AutoSizeIndex = -1
      Header.Height = 18
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      Images = DMImage.vilDocumentState
      LineStyle = lsSolid
      ParentShowHint = False
      PopupMenu = pmTree
      ShowHint = True
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect, toMultiSelect, toAlwaysSelectNode]
      OnAfterAutoFitColumn = vstTreeColumnResize
      OnColumnResize = vstTreeColumnResize
      OnHeaderDragged = vstTreeHeaderDragged
      OnMeasureItem = vstTreeMeasureItem
      Columns = <>
    end
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 32
    Top = 192
    object aExportToExcel: TAction
      Hint = 'Export To Excel'
      ImageIndex = 16
      ImageName = 'ExportToXLS_32x32'
      OnExecute = aExportToExcelExecute
    end
    object aExportToCSV: TAction
      Hint = 'Export to CSV'
      ImageIndex = 17
      ImageName = 'ExportToCSV_32x32'
      OnExecute = aExportToCSVExecute
    end
    object aPrint: TAction
      Hint = 'Print'
      ImageIndex = 15
      ImageName = 'Print_32x32'
      OnExecute = aPrintExecute
    end
    object aColumnSettings: TAction
      Hint = 'Column Settings'
      ImageIndex = 36
      ImageName = 'ListBullets_32x32'
      OnExecute = aColumnSettingsExecute
    end
  end
  object pmTree: TPopupMenu
    Left = 31
    Top = 256
  end
end
