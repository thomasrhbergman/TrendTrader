inherited frameRealtimeFeeds: TframeRealtimeFeeds
  Width = 905
  Height = 477
  ParentFont = False
  ParentShowHint = False
  ShowHint = True
  ExplicitWidth = 905
  ExplicitHeight = 477
  inherited vstTree: TVirtualStringTree
    Top = 37
    Width = 905
    Height = 440
    Alignment = taRightJustify
    Header.Height = 20
    Header.MainColumn = 0
    ParentFont = False
    PopupMenu = pmFeeds
    OnCompareNodes = vstTreeCompareNodes
    OnDragAllowed = vstTreeDragAllowed
    OnDragOver = vstTreeDragOver
    OnDragDrop = vstTreeDragDrop
    OnGetText = vstTreeGetText
    ExplicitTop = 37
    ExplicitWidth = 905
    ExplicitHeight = 440
    Columns = <
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 0
        Text = 'Symbol'
        Width = 80
      end
      item
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 1
        Text = 'Name'
        Width = 120
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 2
        Text = 'Id'
      end
      item
        Alignment = taRightJustify
        CaptionAlignment = taCenter
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
        Position = 3
        Text = 'Last/Close'
        Width = 85
      end>
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 905
    Height = 37
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      905
      37)
    object lblShowFrom: TLabel
      Left = 59
      Top = 13
      Width = 110
      Height = 13
      Caption = 'View Time Price Cache:'
    end
    object lblShowTo: TLabel
      Left = 249
      Top = 13
      Width = 14
      Height = 13
      Caption = 'to:'
    end
    object btnExportToExcel: TBitBtn
      Left = 868
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
      Left = 832
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
      Left = 796
      Top = 0
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aPrint
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object btnColumnSettings: TBitBtn
      Left = 760
      Top = 0
      Width = 36
      Height = 36
      Action = aColumnSettings
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object btnClear: TBitBtn
      Left = 0
      Top = 0
      Width = 36
      Height = 36
      Action = aClear
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object edTimeBegin: TDateTimePicker
      Left = 175
      Top = 9
      Width = 70
      Height = 21
      Date = 44438.000000000000000000
      Time = 0.375000000000000000
      Kind = dtkTime
      TabOrder = 5
    end
    object edTimeEnd: TDateTimePicker
      Left = 267
      Top = 9
      Width = 70
      Height = 21
      Date = 44438.000000000000000000
      Time = 0.385416666664241300
      Kind = dtkTime
      TabOrder = 6
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
      Hint = 'Clear All'
      ImageIndex = 65
      ImageName = 'DeleteList2_32x32'
      OnExecute = aClearExecute
    end
    object aDelete: TAction
      Caption = 'Delete'
      ShortCut = 46
      OnExecute = aDeleteExecute
      OnUpdate = aDeleteUpdate
    end
    object aShowPriceHistory: TAction
      Caption = 'Show Price History'
      OnExecute = aShowPriceHistoryExecute
      OnUpdate = aDeleteUpdate
    end
    object aExportPriceHistoryToExcel: TAction
      Caption = 'to Excel'
      OnExecute = aExportPriceHistoryToExcelExecute
      OnUpdate = aDeleteUpdate
    end
    object aExportPriceHistoryToCSV: TAction
      Caption = 'to CSV'
      OnExecute = aExportPriceHistoryToCSVExecute
      OnUpdate = aDeleteUpdate
    end
  end
  object pmFeeds: TPopupMenu
    Left = 128
    Top = 208
    object miDelete: TMenuItem
      Action = aDelete
    end
    object miShowPriceHistory: TMenuItem
      Action = aShowPriceHistory
    end
    object miExportPriceHistory: TMenuItem
      Caption = 'Export Price History'
      object miExportPriceHistoryToExcel: TMenuItem
        Action = aExportPriceHistoryToExcel
      end
      object miExportPriceHistoryToCSV: TMenuItem
        Action = aExportPriceHistoryToCSV
      end
    end
  end
end
