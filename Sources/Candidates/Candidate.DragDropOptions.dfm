object frmCandidateDragDropOptions: TfrmCandidateDragDropOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Drag&Drop Options'
  ClientHeight = 628
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblCreateNew: TLabel
    Left = 8
    Top = 8
    Width = 161
    Height = 14
    Caption = 'Drag'#39'n'#39'Drop will create a new'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object rbScanColumn: TRadioButton
    Left = 8
    Top = 28
    Width = 113
    Height = 17
    Caption = 'Scan Column'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = DoButtonClick
  end
  object rbStaticList: TRadioButton
    Left = 8
    Top = 50
    Width = 113
    Height = 17
    Caption = 'Static List'
    TabOrder = 1
    OnClick = DoButtonClick
  end
  object pnlStaticList: TPanel
    Left = 0
    Top = 72
    Width = 472
    Height = 513
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object edStaticListName: TEdit
      Left = 151
      Top = 3
      Width = 296
      Height = 21
      Hint = 
        'If this field is empty, StaticList will have the same name as th' +
        'e first Instrument'
      Enabled = False
      TabOrder = 0
    end
    object rbCreateNewStaticList: TRadioButton
      Left = 22
      Top = 5
      Width = 126
      Height = 17
      Caption = 'Create New Static List'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = DoButtonClick
    end
    object rbAddToExistingStaticList: TRadioButton
      Left = 22
      Top = 28
      Width = 147
      Height = 17
      Caption = 'Add to Existing Static List'
      TabOrder = 2
      OnClick = DoButtonClick
    end
    object pnlStaticLists: TPanel
      Left = 0
      Top = 51
      Width = 472
      Height = 462
      Align = alBottom
      TabOrder = 3
      object pnlCategoryTop: TPanel
        Left = 1
        Top = 1
        Width = 470
        Height = 35
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          470
          35)
        object lblSearchLists: TLabel
          Left = 71
          Top = 8
          Width = 73
          Height = 13
          Caption = 'Filter By Name:'
        end
        object btnClearSearchText: TBitBtn
          Left = 447
          Top = 5
          Width = 22
          Height = 22
          Anchors = [akTop, akRight]
          ImageIndex = 43
          ImageName = 'RemovePivotField_32x32'
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = btnClearSearchTextClick
        end
        object edtSearchLists: TEdit
          AlignWithMargins = True
          Left = 150
          Top = 5
          Width = 296
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = edtSearchListsChange
        end
      end
      object vstTree: TVirtualStringTree
        Left = 1
        Top = 36
        Width = 470
        Height = 425
        Align = alClient
        Header.AutoSizeIndex = -1
        Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
        IncrementalSearchTimeout = 0
        LineStyle = lsSolid
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoTristateTracking, toAutoChangeScale]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus]
        OnFreeNode = vstTreeFreeNode
        OnGetText = vstTreeGetText
        OnPaintText = vstTreePaintText
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 0
            Text = 'Name'
            Width = 232
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 1
            Text = 'Symbol'
            Width = 115
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
            Text = 'Security Type '
            Width = 80
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 7
            Text = 'Broker'
            Width = 80
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 8
            Text = 'Expiry'
            Width = 80
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 9
            Text = 'Underlying ConId'
            Width = 80
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 10
            Text = 'Description'
            Width = 10
          end>
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 585
    Width = 472
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      472
      43)
    object btnOk: TBitBtn
      Left = 371
      Top = 2
      Width = 100
      Height = 40
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 271
      Top = 2
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
    object cbRememberLastOption: TCheckBox
      Left = 5
      Top = 13
      Width = 127
      Height = 17
      Caption = 'Remember Last Option'
      TabOrder = 2
    end
  end
end
