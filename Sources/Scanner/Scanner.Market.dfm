object frmScannerMarket: TfrmScannerMarket
  Left = 0
  Top = 0
  Caption = 'Market Scanner'
  ClientHeight = 661
  ClientWidth = 1002
  Color = clBtnFace
  Constraints.MinHeight = 700
  Constraints.MinWidth = 1000
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object splResult: TSplitter
    Left = 0
    Top = 346
    Width = 1002
    Height = 4
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 321
    ExplicitWidth = 984
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 25
    Width = 1002
    Height = 40
    Align = alTop
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    object lblCurrentSequenceName: TLabel
      Left = 13
      Top = 10
      Width = 95
      Height = 16
      Alignment = taRightJustify
      Caption = 'Market Scanner:'
      Color = clDefault
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object btnSave: TBitBtn
      Left = 400
      Top = 1
      Width = 36
      Height = 36
      Action = aSave
      Images = DMImage.vil32
      TabOrder = 2
    end
    object btnOpen: TBitBtn
      Left = 364
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aOpen
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object edCurrentSequenceName: TEdit
      Left = 111
      Top = 8
      Width = 250
      Height = 21
      TabOrder = 0
    end
    object btnInformationDialog: TBitBtn
      Left = 472
      Top = 1
      Width = 36
      Height = 36
      Action = aInformationDialog
      Images = DMImage.vil32
      TabOrder = 4
    end
    object pnlRight: TPanel
      Left = 654
      Top = 0
      Width = 348
      Height = 40
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 5
      object lblMaxRows: TLabel
        Left = 213
        Top = 10
        Width = 63
        Height = 16
        Caption = 'Max Rows:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object seMaxRows: TSpinEdit
        Left = 277
        Top = 6
        Width = 65
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxValue = 0
        MinValue = 0
        ParentFont = False
        TabOrder = 0
        Value = 10
        OnChange = seMaxRowsChange
      end
      object cbAutoRefresh: TCheckBox
        Left = 103
        Top = 10
        Width = 104
        Height = 17
        Caption = 'Auto Refresh'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object btnNewScan: TBitBtn
        Left = 1
        Top = 1
        Width = 96
        Height = 36
        Action = aNewScan
        Caption = 'Search'
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
    object btnSaveAs: TBitBtn
      Left = 436
      Top = 1
      Width = 36
      Height = 36
      Action = aSaveAs
      Images = DMImage.vil32
      TabOrder = 3
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 65
    Width = 1002
    Height = 281
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lbInstruments: TListBox
      Left = 0
      Top = 0
      Width = 193
      Height = 281
      Align = alLeft
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbInstrumentsClick
    end
    object pnlMainFilters: TPanel
      Left = 193
      Top = 0
      Width = 809
      Height = 281
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object splFilterMain: TSplitter
        Left = 237
        Top = 0
        Width = 4
        Height = 281
        ExplicitLeft = 224
        ExplicitTop = -16
      end
      object splParameter: TSplitter
        Left = 622
        Top = 0
        Width = 4
        Height = 281
        ExplicitLeft = 238
        ExplicitTop = -2
      end
      object splLocation: TSplitter
        Left = 0
        Top = 0
        Width = 4
        Height = 281
        ExplicitLeft = -10
        ExplicitTop = -16
      end
      object pnlLocation: TPanel
        Left = 4
        Top = 0
        Width = 233
        Height = 281
        Align = alLeft
        BevelInner = bvSpace
        BevelOuter = bvNone
        TabOrder = 0
        object vstLocation: TVirtualStringTree
          Left = 1
          Top = 1
          Width = 231
          Height = 279
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Header.AutoSizeIndex = 0
          Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toAutoChangeScale]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
          TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
          OnChecked = vstLocationChecked
          OnFreeNode = vstLocationFreeNode
          OnGetText = vstLocationGetText
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
              Position = 0
              Text = 'Location'
              Width = 231
            end>
        end
      end
      object pnlFilterMain: TPanel
        Left = 241
        Top = 0
        Width = 381
        Height = 281
        Align = alLeft
        BevelInner = bvSpace
        BevelOuter = bvNone
        Constraints.MinWidth = 380
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        object pnlFilterTop: TPanel
          Left = 1
          Top = 1
          Width = 379
          Height = 21
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Filter'
          Ctl3D = True
          ParentBackground = False
          ParentCtl3D = False
          TabOrder = 0
        end
        object pnlFilterBottom: TPanel
          Left = 1
          Top = 223
          Width = 379
          Height = 57
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnRemoveAll: TBitBtn
            Left = 283
            Top = 21
            Width = 95
            Height = 36
            Margins.Left = 1
            Margins.Top = 1
            Margins.Right = 1
            Margins.Bottom = 1
            Action = aRemoveAll
            Caption = 'Remove All'
            Images = DMImage.vil32
            Margin = 0
            Spacing = 2
            TabOrder = 0
          end
          object btnClearAllFilters: TBitBtn
            Left = 188
            Top = 21
            Width = 95
            Height = 36
            Margins.Left = 1
            Margins.Top = 1
            Margins.Right = 1
            Margins.Bottom = 1
            Action = aClearAllFilters
            Caption = 'Clear all'
            Images = DMImage.vil32
            Margin = 0
            Spacing = 2
            TabOrder = 1
          end
          object btnAddFilter: TBitBtn
            Left = 93
            Top = 21
            Width = 95
            Height = 36
            Margins.Left = 1
            Margins.Top = 1
            Margins.Right = 1
            Margins.Bottom = 1
            Action = aAddFilter
            Caption = 'More filters'
            Images = DMImage.vil32
            Margin = 0
            Spacing = 2
            TabOrder = 2
          end
          object btnExtraFilter: TBitBtn
            Left = -1
            Top = 21
            Width = 95
            Height = 36
            Margins.Left = 1
            Margins.Top = 1
            Margins.Right = 1
            Margins.Bottom = 1
            Action = aExtraFilter
            Caption = 'Add filter'
            Images = DMImage.vil32
            Margin = 0
            Spacing = 2
            TabOrder = 3
          end
          object cbUseExtraFilter: TCheckBox
            Left = 5
            Top = 3
            Width = 97
            Height = 17
            Caption = 'Use Extra Filter'
            TabOrder = 4
            OnClick = cbUseExtraFilterClick
          end
        end
        object sbFilter: TScrollBox
          Left = 1
          Top = 22
          Width = 379
          Height = 201
          HorzScrollBar.Visible = False
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clWindow
          ParentColor = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
      end
      object pnlParameter: TPanel
        Left = 626
        Top = 0
        Width = 183
        Height = 281
        Align = alClient
        BevelInner = bvSpace
        BevelOuter = bvNone
        TabOrder = 2
        object vstScanType: TVirtualStringTree
          Left = 1
          Top = 22
          Width = 181
          Height = 258
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Header.AutoSizeIndex = -1
          Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
          Header.SortColumn = 0
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toAutoChangeScale]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toFullRowSelect, toAlwaysSelectNode]
          OnChange = vstScanTypeChange
          OnCompareNodes = vstScanTypeCompareNodes
          OnDblClick = vstScanTypeDblClick
          OnFreeNode = vstScanTypeFreeNode
          OnGetText = vstScanTypeGetText
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable]
              Position = 0
              Text = 'Parameter'
              Width = 181
            end>
        end
        object pnlAvailableFilters: TPanel
          Left = 1
          Top = 1
          Width = 181
          Height = 21
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          DesignSize = (
            181
            21)
          object lblAvailableFilters: TLabel
            Left = 3
            Top = 3
            Width = 73
            Height = 13
            Alignment = taRightJustify
            Caption = 'Available filters'
          end
          object edtAvailableFilters: TEdit
            Left = 78
            Top = 0
            Width = 81
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = edtAvailableFiltersChange
          end
          object btnClearAvailableFilters: TBitBtn
            Left = 159
            Top = -1
            Width = 22
            Height = 22
            Action = aClearAvailableFilters
            Anchors = [akTop, akRight]
            Images = DMImage.vil16
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
          end
        end
      end
    end
  end
  object pcResult: TPageControl
    Left = 0
    Top = 350
    Width = 1002
    Height = 292
    ActivePage = tsResult
    Align = alClient
    TabOrder = 2
    object tsResult: TTabSheet
      Caption = 'Result'
      object splIsolate: TSplitter
        Left = 669
        Top = 0
        Height = 264
        Align = alRight
        ExplicitLeft = 520
        ExplicitTop = 136
        ExplicitHeight = 100
      end
      object pnlIsolate: TPanel
        Left = 672
        Top = 0
        Width = 322
        Height = 264
        Align = alRight
        TabOrder = 0
        object pnlIsolateTop: TPanel
          Left = 1
          Top = 1
          Width = 320
          Height = 59
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          DesignSize = (
            320
            59)
          object lblExcludedInstruments: TLabel
            Left = 96
            Top = 8
            Width = 121
            Height = 16
            Caption = 'Excluded instruments'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object btnIsolateColumnSettings: TBitBtn
            Left = 283
            Top = 0
            Width = 36
            Height = 36
            Action = aIsolateColumnSettings
            Anchors = [akTop, akRight]
            Images = DMImage.vil32
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
          object btnIsolateDelete: TBitBtn
            Left = 1
            Top = -1
            Width = 36
            Height = 36
            ParentCustomHint = False
            Action = aIsolateDelete
            Images = DMImage.vil32
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
          end
          object btnClearIsolateSearchText: TBitBtn
            Left = 298
            Top = 35
            Width = 22
            Height = 22
            Action = aClearIsolateSearchText
            Anchors = [akTop, akRight]
            Images = DMImage.vil16
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
          end
          object edtIsolateSearch: TEdit
            AlignWithMargins = True
            Left = 0
            Top = 36
            Width = 298
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
            OnChange = edtIsolateSearchChange
          end
        end
        object vstIsolate: TVirtualStringTree
          Left = 1
          Top = 60
          Width = 320
          Height = 203
          Align = alClient
          DragMode = dmAutomatic
          DragOperations = [doCopy, doMove, doLink]
          DragType = dtVCL
          Header.AutoSizeIndex = 0
          Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
          Header.SortColumn = 1
          LineStyle = lsSolid
          TabOrder = 0
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect, toMultiSelect, toAlwaysSelectNode]
          OnCompareNodes = vstIsolateCompareNodes
          OnDragAllowed = vstIsolateDragAllowed
          OnDragOver = vstIsolateDragOver
          OnDragDrop = vstIsolateDragDrop
          OnFreeNode = vstIsolateFreeNode
          OnGetText = vstIsolateGetText
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 0
              Text = 'Name'
              Width = 225
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 1
              Text = 'Symbol'
              Width = 100
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 2
              Text = 'Local Symbol'
              Width = 100
            end
            item
              CaptionAlignment = taCenter
              DefaultSortDirection = sdDescending
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 3
              Text = 'ConId'
              Width = 80
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 4
              Text = 'Currency'
              Width = 80
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 5
              Text = 'Exchange'
              Width = 80
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 6
              Text = 'Expiry'
              Width = 80
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 7
              Text = 'Description'
              Width = 100
            end>
        end
      end
      object pnlResult: TPanel
        Left = 0
        Top = 0
        Width = 669
        Height = 264
        Align = alClient
        TabOrder = 1
        object vstInstruments: TVirtualStringTree
          Left = 1
          Top = 37
          Width = 667
          Height = 226
          Align = alClient
          DragType = dtVCL
          Header.AutoSizeIndex = -1
          Header.Height = 21
          Header.Options = [hoColumnResize, hoDrag, hoShowHint, hoShowImages, hoVisible]
          ParentShowHint = False
          PopupMenu = pmInstruments
          ScrollBarOptions.AlwaysVisible = True
          ShowHint = True
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toAutoChangeScale]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick, toEditOnDblClick]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowVertGridLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
          OnCompareNodes = vstInstrumentsCompareNodes
          OnCreateEditor = vstInstrumentsCreateEditor
          OnDragAllowed = vstInstrumentsDragAllowed
          OnDrawText = vstInstrumentsDrawText
          OnEdited = vstInstrumentsEdited
          OnEditing = vstInstrumentsEditing
          OnEndDrag = vstInstrumentsEndDrag
          OnFreeNode = vstInstrumentsFreeNode
          OnGetText = vstInstrumentsGetText
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              Alignment = taRightJustify
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment, coEditable]
              Position = 1
              Tag = -1
              Text = 'Position'
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
              Position = 2
              Tag = -1
              Text = 'Instrument'
              Width = 325
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
              Position = 3
              Tag = -1
              Text = 'Symbol'
            end
            item
              Alignment = taRightJustify
              CaptionAlignment = taCenter
              Hint = 'F2 Edit Ranking'
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coFixed, coAllowFocus, coUseCaptionAlignment, coEditable]
              Position = 0
              Tag = -1
              Text = 'Ranking'
            end
            item
              Alignment = taRightJustify
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
              Position = 4
              Text = '% Last/Close'
              Width = 84
            end>
        end
        object pnlResultTop: TPanel
          Left = 1
          Top = 1
          Width = 667
          Height = 36
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          DesignSize = (
            667
            36)
          object lblStep: TLabel
            Left = 320
            Top = 9
            Width = 31
            Height = 16
            Alignment = taRightJustify
            Caption = 'Step:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object lblWeight: TLabel
            Left = 197
            Top = 9
            Width = 45
            Height = 16
            Alignment = taRightJustify
            Caption = 'Weight:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object btnColumnSettings: TBitBtn
            Left = 523
            Top = -1
            Width = 36
            Height = 36
            Action = aColumnSettings
            Anchors = [akTop, akRight]
            Images = DMImage.vil32
            Margin = 0
            Spacing = 2
            TabOrder = 0
          end
          object btnExport: TBitBtn
            Left = 595
            Top = -1
            Width = 36
            Height = 36
            Action = aExportToCSV
            Anchors = [akTop, akRight]
            Images = DMImage.vil32
            Spacing = 2
            TabOrder = 1
          end
          object btnPrint: TBitBtn
            Left = 559
            Top = -1
            Width = 36
            Height = 36
            Action = aPrint
            Anchors = [akTop, akRight]
            Images = DMImage.vil32
            Spacing = 2
            TabOrder = 2
          end
          object btnExportToExcel: TBitBtn
            Left = 631
            Top = -1
            Width = 36
            Height = 36
            ParentCustomHint = False
            Action = aExportToExcel
            Anchors = [akTop, akRight]
            Images = DMImage.vil32
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
          end
          object btnShowDragDropOptions: TBitBtn
            Left = 1
            Top = 0
            Width = 36
            Height = 36
            Action = aShowDragDropOptions
            Images = DMImage.vil32
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
          end
          object btnIntersection: TBitBtn
            Left = 151
            Top = -1
            Width = 38
            Height = 38
            Action = aIntersection
            Images = DMImage.vil32
            Layout = blGlyphTop
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
            TabOrder = 5
          end
          object btnReduce: TBitBtn
            Left = 113
            Top = -1
            Width = 38
            Height = 38
            Action = aReduce
            Images = DMImage.vil32
            Layout = blGlyphTop
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
            TabOrder = 6
          end
          object btnUpdate: TBitBtn
            Left = 75
            Top = -1
            Width = 38
            Height = 38
            Action = aUpdate
            Images = DMImage.vil32
            Layout = blGlyphTop
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
            TabOrder = 7
          end
          object btnAddAll: TBitBtn
            Left = 37
            Top = -1
            Width = 38
            Height = 38
            Action = aAddAll
            Images = DMImage.vil32
            Layout = blGlyphTop
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
            TabOrder = 8
          end
          object edtStep: TNumberBox
            Left = 356
            Top = 8
            Width = 66
            Height = 21
            AcceptExpressions = True
            Decimal = 1
            LargeStep = 1.000000000000000000
            Mode = nbmFloat
            TabOrder = 9
            SpinButtonOptions.Placement = nbspCompact
            UseMouseWheel = True
            NegativeValueColor = clRed
          end
          object edtWeight: TNumberBox
            Left = 248
            Top = 8
            Width = 66
            Height = 21
            AcceptExpressions = True
            Decimal = 1
            LargeStep = 1.000000000000000000
            Mode = nbmFloat
            TabOrder = 10
            SpinButtonOptions.Placement = nbspCompact
            UseMouseWheel = True
            NegativeValueColor = clRed
          end
        end
      end
    end
    object tsScanList: TTabSheet
      Caption = 'Scan List'
      ImageIndex = 1
      object pnlTopScanList: TPanel
        Left = 0
        Top = 0
        Width = 994
        Height = 36
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object btnCancelScan: TBitBtn
          Left = 1
          Top = 0
          Width = 36
          Height = 36
          Action = aCancelScan
          Images = DMImage.vil32
          Spacing = 2
          TabOrder = 0
        end
      end
      object vstScanList: TVirtualStringTree
        Left = 0
        Top = 36
        Width = 994
        Height = 228
        Align = alClient
        Header.AutoSizeIndex = -1
        Header.Height = 21
        Header.Options = [hoColumnResize, hoDrag, hoShowHint, hoShowImages, hoVisible]
        ParentShowHint = False
        ScrollBarOptions.AlwaysVisible = True
        ShowHint = True
        TabOrder = 1
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toAutoChangeScale]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick, toEditOnDblClick]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowVertGridLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
        OnCompareNodes = vstScanListCompareNodes
        OnFreeNode = vstScanListFreeNode
        OnGetText = vstScanListGetText
        OnGetNodeDataSize = vstScanListGetNodeDataSize
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coStyleColor]
            Position = 0
            Text = 'Scan Id'
            Width = 120
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coStyleColor]
            Position = 1
            Text = 'Count'
            Width = 120
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coStyleColor]
            Position = 2
            Text = 'Instrument'
            Width = 120
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Hint = 'F2 Edit Ranking'
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coStyleColor]
            Position = 3
            Text = 'Location Code'
            Width = 120
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coStyleColor]
            Position = 4
            Text = 'Scan Code'
            Width = 120
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coStyleColor]
            Position = 5
            Text = 'Number Of Rows'
            Width = 120
          end>
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1002
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object lblInstanceNum: TLabel
      Left = 13
      Top = 3
      Width = 124
      Height = 16
      Caption = 'Parent Instance Num:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblAutoTradesName: TLabel
      Left = 510
      Top = 3
      Width = 149
      Height = 16
      Caption = 'Parent AutoTrades Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 642
    Width = 1002
    Height = 19
    Panels = <
      item
        Alignment = taRightJustify
        Text = 'Status: '
        Width = 50
      end
      item
        Text = ' '
        Width = 400
      end
      item
        Alignment = taRightJustify
        Text = 'Scan Count: '
        Width = 80
      end
      item
        Text = '0'
        Width = 50
      end
      item
        Alignment = taRightJustify
        Text = 'Last Scan Id: '
        Width = 80
      end
      item
        Text = '0'
        Width = 50
      end
      item
        Alignment = taRightJustify
        Text = 'Update Scan:'
        Width = 80
      end
      item
        Alignment = taRightJustify
        Text = ' '
        Width = 60
      end
      item
        Width = 50
      end>
  end
  object pmMain: TPopupMenu
    Left = 56
    Top = 111
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 56
    Top = 168
    object aAddAll: TAction
      Category = 'Instruments'
      Hint = 'Add And Save New Column to Scan'
      ImageIndex = 37
      ImageName = 'select_by_adding_to_selection'
      OnExecute = aAddAllExecute
      OnUpdate = aAddAllUpdate
    end
    object aUpdate: TAction
      Category = 'Instruments'
      Hint = 'Update'
      ImageIndex = 39
      ImageName = 'shape_move_forwards'
      OnExecute = aUpdateExecute
      OnUpdate = aAddAllUpdate
    end
    object aReduce: TAction
      Category = 'Instruments'
      Hint = 'Reduce'
      ImageIndex = 38
      ImageName = 'shape_move_backwards'
      OnExecute = aReduceExecute
      OnUpdate = aAddAllUpdate
    end
    object aIntersection: TAction
      Category = 'Instruments'
      Hint = 'Intersection'
      ImageIndex = 40
      ImageName = 'select_by_intersection'
      OnExecute = aIntersectionExecute
      OnUpdate = aAddAllUpdate
    end
    object aNewScan: TAction
      Category = 'Scan'
      Caption = 'Search'
      ImageIndex = 12
      ImageName = 'lightning'
      OnExecute = aNewScanExecute
      OnUpdate = aNewScanUpdate
    end
    object aExtraFilter: TAction
      Category = 'Scan'
      Caption = 'Add filter'
      ImageIndex = 9
      ImageName = 'TextBox_32x32'
      OnExecute = aExtraFilterExecute
    end
    object aAddFilter: TAction
      Category = 'Scan'
      Caption = 'More filters'
      ImageIndex = 34
      ImageName = 'AddFooter_32x32'
      OnExecute = aAddFilterExecute
    end
    object aClearAllFilters: TAction
      Category = 'Scan'
      Caption = 'Clear all'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aClearAllFiltersExecute
    end
    object aRemoveAll: TAction
      Category = 'Scan'
      Caption = 'Remove All'
      ImageIndex = 35
      ImageName = 'DeleteFooter_32x32'
      OnExecute = aRemoveAllExecute
    end
    object aColumnSettings: TAction
      Category = 'Instruments'
      Hint = 'Column Settings'
      ImageIndex = 36
      ImageName = 'ListBullets_32x32'
      OnExecute = aColumnSettingsExecute
    end
    object aShowTradeChart: TAction
      Category = 'Scan'
      Caption = 'Chart'
      ShortCut = 16452
      OnExecute = aShowTradeChartExecute
      OnUpdate = aShowTradeChartUpdate
    end
    object aOpen: TAction
      Category = 'Scan'
      ImageIndex = 5
      ImageName = 'Open_32x32'
      OnExecute = aOpenExecute
    end
    object aSave: TAction
      Category = 'Scan'
      ImageIndex = 10
      ImageName = 'Save_32x32'
      OnExecute = aSaveExecute
    end
    object aSaveAs: TAction
      Category = 'Scan'
      Hint = 'Save As..'
      ImageIndex = 26
      ImageName = 'SaveAll_32x32'
      OnExecute = aSaveAsExecute
      OnUpdate = aSaveAsUpdate
    end
    object aPrint: TAction
      Category = 'Instruments'
      Hint = 'Print'
      ImageIndex = 15
      ImageName = 'Print_32x32'
      OnExecute = aPrintExecute
    end
    object aExportToCSV: TAction
      Category = 'Instruments'
      Hint = 'Export to CSV'
      ImageIndex = 17
      ImageName = 'ExportToCSV_32x32'
      OnExecute = aExportToCSVExecute
    end
    object aCancelScan: TAction
      Category = 'Scan'
      Hint = 'Cancel Scan'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aCancelScanExecute
    end
    object aInformationDialog: TAction
      Category = 'Scan'
      ImageIndex = 6
      ImageName = 'Info_32x32'
      ShortCut = 16457
      OnExecute = aInformationDialogExecute
    end
    object aExportToExcel: TAction
      Category = 'Instruments'
      Hint = 'Export To Excel'
      ImageIndex = 16
      ImageName = 'ExportToXLS_32x32'
      OnExecute = aExportToExcelExecute
    end
    object aIsolateColumnSettings: TAction
      Category = 'Isolate'
      Hint = 'Column Settings'
      ImageIndex = 36
      ImageName = 'ListBullets_32x32'
      OnExecute = aIsolateColumnSettingsExecute
    end
    object aIsolateDelete: TAction
      Category = 'Isolate'
      Hint = 'Delete From List'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aIsolateDeleteExecute
    end
    object aClearIsolateSearchText: TAction
      Category = 'Isolate'
      ImageIndex = 43
      ImageName = 'RemovePivotField_32x32'
      OnExecute = aClearIsolateSearchTextExecute
    end
    object aClearAvailableFilters: TAction
      Category = 'Scan'
      ImageIndex = 43
      ImageName = 'RemovePivotField_32x32'
      OnExecute = aClearAvailableFiltersExecute
    end
    object aShowDragDropOptions: TAction
      Category = 'Scan'
      Hint = 'Show Drag&Drop Options'
      ImageIndex = 63
      ImageName = 'PageSetup_32x32'
      OnExecute = aShowDragDropOptionsExecute
    end
  end
  object pmInstruments: TPopupMenu
    Left = 80
    Top = 488
    object miShowTradeChart: TMenuItem
      Action = aShowTradeChart
    end
  end
end
