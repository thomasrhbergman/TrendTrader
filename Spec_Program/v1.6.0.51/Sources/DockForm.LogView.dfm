inherited frmLogView: TfrmLogView
  Caption = 'Log View'
  ClientWidth = 1033
  Color = clWhite
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 1049
  
  TextHeight = 13
  inherited pnlOptions: TPanel
    Width = 1033
    TabOrder = 2
    ExplicitWidth = 1033
    inherited btnExportToExcel: TBitBtn
      Left = 837
      ExplicitLeft = 837
    end
    inherited btnExportToCSV: TBitBtn
      Left = 801
      ExplicitLeft = 801
    end
    inherited btnPrint: TBitBtn
      Left = 765
      ExplicitLeft = 765
    end
    inherited btnColumnSettings: TBitBtn
      Left = 729
      ExplicitLeft = 729
    end
    object btnClearLog: TBitBtn
      Left = 1
      Top = 2
      Width = 36
      Height = 36
      Action = aClearLog
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 4
    end
  end
  inherited pnlMain: TPanel
    Width = 873
    ExplicitWidth = 873
    object splInfo: TSplitter [0]
      Left = 0
      Top = 314
      Width = 873
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      Color = clWhite
      ParentColor = False
      ExplicitTop = 0
      ExplicitWidth = 317
    end
    inherited vstTree: TVirtualStringTree
      Width = 873
      Height = 314
      Header.MainColumn = 0
      ScrollBarOptions.AlwaysVisible = True
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoTristateTracking, toAutoChangeScale]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware]
      TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toAlwaysSelectNode]
      OnCompareNodes = vstTreeCompareNodes
      OnDrawText = vstTreeDrawText
      OnFocusChanged = vstTreeFocusChanged
      OnFreeNode = vstTreeFreeNode
      OnGetText = vstTreeGetText
      ExplicitWidth = 873
      ExplicitHeight = 314
      Columns = <
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 0
          Text = 'Time'
          Width = 85
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 1
          Text = 'Type'
          Width = 55
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 2
          Text = 'Action'
          Width = 68
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 3
          Text = 'Method'
          Width = 168
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 4
          Text = 'Order ID'
          Width = 62
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 5
          Text = 'Order Type'
          Width = 88
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 6
          Text = 'OCA'
          Width = 69
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 7
          Text = 'Info'
          Width = 265
        end
        item
          Position = 8
          Text = 'Last Price'
          Width = 10
        end>
    end
    object gbInfo: TGroupBox
      Left = 0
      Top = 317
      Width = 873
      Height = 215
      Align = alBottom
      Caption = 'Info'
      TabOrder = 1
      object memInfo: TMemo
        Left = 2
        Top = 15
        Width = 869
        Height = 198
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object pnlEvents: TPanel [2]
    Left = 873
    Top = 38
    Width = 160
    Height = 532
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object lbEvents: TCheckListBox
      Left = 0
      Top = 16
      Width = 160
      Height = 301
      Align = alClient
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
    end
    object pnlEventsTop: TPanel
      Left = 0
      Top = 0
      Width = 160
      Height = 16
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Events'
      TabOrder = 1
    end
    object pnlFilter: TPanel
      Left = 0
      Top = 317
      Width = 160
      Height = 215
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object lblOrderID: TLabel
        Left = 5
        Top = 7
        Width = 42
        Height = 13
        Caption = 'Order ID'
      end
      object seOrderID: TSpinEdit
        Left = 53
        Top = 2
        Width = 74
        Height = 22
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxValue = 0
        MinValue = 0
        ParentFont = False
        TabOrder = 0
        Value = 0
      end
      object btnFilter: TBitBtn
        Left = 5
        Top = 25
        Width = 150
        Height = 36
        Action = aFilter
        Caption = 'Filter'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Images = DMImage.vil32
        ParentFont = False
        TabOrder = 1
      end
      object btnFilterClear: TBitBtn
        Left = 5
        Top = 62
        Width = 150
        Height = 36
        Action = aFilterClear
        Caption = 'Uncheck All'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Images = DMImage.vil32
        ParentFont = False
        TabOrder = 2
      end
    end
  end
  inherited ActionList: TActionList
    object aFilter: TAction
      Caption = 'Filter'
      ImageIndex = 3
      ImageName = 'MasterFilter_32x32'
      OnExecute = aFilterExecute
    end
    object aFilterClear: TAction
      Caption = 'Uncheck All'
      ImageIndex = 59
      ImageName = 'Clear_32x32'
      OnExecute = aFilterClearExecute
    end
    object aClearLog: TAction
      Hint = 'Clear Log'
      ImageIndex = 65
      ImageName = 'DeleteList2_32x32'
      OnExecute = aClearLogExecute
    end
  end
end
