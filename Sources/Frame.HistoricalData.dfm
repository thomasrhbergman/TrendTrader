inherited frameHistoricalData: TframeHistoricalData
  Width = 799
  Height = 477
  ParentFont = False
  ParentShowHint = False
  ShowHint = True
  ExplicitWidth = 799
  ExplicitHeight = 477
  inherited vstTree: TVirtualStringTree
    Top = 37
    Width = 799
    Height = 440
    Alignment = taRightJustify
    Font.Height = -10
    Header.Height = 20
    Header.MainColumn = 0
    ParentFont = False
    OnCompareNodes = vstTreeCompareNodes
    OnGetText = vstTreeGetText
    ExplicitLeft = 144
    ExplicitTop = 42
    ExplicitWidth = 799
    ExplicitHeight = 440
    Columns = <
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 0
        Text = 'Symbol'
        Width = 90
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 1
        Text = 'BarSize'
        Width = 90
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 2
        Text = 'Duration'
        Width = 90
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 3
        Text = 'TickType'
        Width = 90
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 4
        Text = 'Date'
        Width = 120
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 5
        Text = 'Open'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 6
        Text = 'High'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 7
        Text = 'Low'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 8
        Text = 'Close'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 9
        Text = 'WAP'
        Width = 70
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 10
        Text = 'Volume'
        Width = 70
      end>
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 799
    Height = 37
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      799
      37)
    object btnExportToExcel: TBitBtn
      Left = 762
      Top = 0
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aExportToExcel
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object btnExportToCSV: TBitBtn
      Left = 726
      Top = 0
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
      Left = 690
      Top = 0
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aPrint
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
    end
    object btnColumnSettings: TBitBtn
      Left = 654
      Top = 0
      Width = 36
      Height = 36
      Action = aColumnSettings
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
    object btnClear: TBitBtn
      Left = 36
      Top = 0
      Width = 36
      Height = 36
      Action = aClear
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object btnSavePriceToDB: TBitBtn
      Left = 108
      Top = 0
      Width = 36
      Height = 36
      Action = aSavePriceToDB
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object btnClearHistoricalDB: TBitBtn
      Left = 72
      Top = 0
      Width = 36
      Height = 36
      Action = aClearHistoricalDB
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
    object btnInfo: TBitBtn
      Left = 618
      Top = 0
      Width = 36
      Height = 36
      Action = aInfo
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object btnSubscribeHistoricalData: TBitBtn
      Left = 0
      Top = 0
      Width = 36
      Height = 36
      Hint = 'Save Price To DB'
      ImageIndex = 12
      ImageName = 'lightning'
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = aSavePriceToDBExecute
    end
  end
  object alMain: TActionList
    Images = DMImage.vil32
    Left = 128
    Top = 128
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
    object aClear: TAction
      Hint = 'Clear'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aClearExecute
      OnUpdate = aSavePriceToDBUpdate
    end
    object aClearHistoricalDB: TAction
      Hint = 'Delete historical price from DB'
      ImageIndex = 65
      ImageName = 'DeleteList2_32x32'
      OnExecute = aClearHistoricalDBExecute
      OnUpdate = aClearHistoricalDBUpdate
    end
    object aSavePriceToDB: TAction
      Hint = 'Save Price To DB'
      ImageIndex = 26
      ImageName = 'SaveAll_32x32'
      OnExecute = aSavePriceToDBExecute
      OnUpdate = aSavePriceToDBUpdate
    end
    object aInfo: TAction
      ImageIndex = 6
      ImageName = 'Info_32x32'
      OnExecute = aInfoExecute
    end
  end
end
