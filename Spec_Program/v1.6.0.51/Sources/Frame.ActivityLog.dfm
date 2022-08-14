object frameActivityLog: TframeActivityLog
  Left = 0
  Top = 0
  Width = 474
  Height = 191
  TabOrder = 0
  object pnlActivityLog: TPanel
    Left = 0
    Top = 0
    Width = 474
    Height = 191
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    object vstTree: TVirtualStringTree
      Left = 1
      Top = 23
      Width = 472
      Height = 167
      Align = alClient
      Alignment = taRightJustify
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Colors.BorderColor = 14540253
      Colors.FocusedSelectionColor = clSkyBlue
      Colors.GridLineColor = clMoneyGreen
      Colors.SelectionTextColor = 14540253
      Colors.TreeLineColor = 16250871
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      Header.AutoSizeIndex = 0
      Header.Height = 14
      Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      HintMode = hmHint
      LineStyle = lsSolid
      ParentCtl3D = False
      ParentFont = False
      ParentShowHint = False
      ScrollBarOptions.AlwaysVisible = True
      ScrollBarOptions.ScrollBars = ssVertical
      ScrollBarOptions.VerticalIncrement = 40
      ShowHint = True
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScroll, toAutoTristateTracking, toAutoChangeScale]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware]
      TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus]
      OnCompareNodes = vstTreeCompareNodes
      OnDrawText = vstTreeDrawText
      OnFreeNode = vstTreeFreeNode
      OnGetText = vstTreeGetText
      Columns = <
        item
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 0
          Spacing = 0
          Text = 'Symbol'
          Width = 79
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 1
          Text = 'Status'
          Width = 39
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 2
          Text = 'Action'
          Width = 60
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 3
          Text = 'Quantity'
          Width = 49
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 4
          Text = 'Filled'
          Width = 45
        end
        item
          Position = 5
          Text = 'Price'
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 6
          Text = 'IBID'
          Width = 47
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 7
          Text = 'Time'
          Width = 53
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 8
          Text = 'Info'
        end>
    end
    object pnlAvailableFilters: TPanel
      Left = 1
      Top = 1
      Width = 472
      Height = 22
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        472
        22)
      object lblAvailableFilters: TLabel
        Left = 6
        Top = 3
        Width = 73
        Height = 13
        Alignment = taRightJustify
        Caption = 'Available filters'
      end
      object edtAvailableFilters: TEdit
        Left = 86
        Top = 0
        Width = 258
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        CharCase = ecUpperCase
        TabOrder = 0
        OnChange = edtAvailableFiltersChange
      end
      object btnClearAvailableFilters: TBitBtn
        Left = 345
        Top = 0
        Width = 22
        Height = 21
        Anchors = [akTop, akRight]
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000000000000000000000000000000000004CB1224CB122
          4CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1
          224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224C
          B1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB122
          4CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1
          224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1221D1C804CB1224C
          B1224CB1224CB1224CB1221413714CB1224CB1224CB1224CB1224CB1224CB122
          4CB1224CB122201F860520D11D1C814CB1224CB1224CB122171675041CCE1413
          714CB1224CB1224CB1224CB1224CB1224CB12222218A3C56E20623D20520D11D
          1B804CB1221A197B041CCE041CCE3A4EDE1514714CB1224CB1224CB1224CB122
          4CB1224CB12223218A3D56E10723D30621D11D1C80041CCF041CCE3A4EDE1716
          754CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB12223228A3C56E206
          24D20621D2051ED03A4FDE1A197B4CB1224CB1224CB1224CB1224CB1224CB122
          4CB1224CB1224CB1224CB12222218A0728D50724D30621D11D1C804CB1224CB1
          224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB12225248D092FD708
          2BD53D56E20724D20621D11D1C804CB1224CB1224CB1224CB1224CB1224CB122
          4CB1224CB1222625910A37DB0A33D83D5BE423218A3C56E10624D20620D11D1C
          804CB1224CB1224CB1224CB1224CB1224CB1222726923F67E90B3CDE3E61E725
          238E4CB12223218A3C56E20724D23B51E01D1C804CB1224CB1224CB1224CB122
          4CB1224CB1222726923F67EA2625914CB1224CB1224CB1222222893B56E2201E
          854CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1222726924CB1224C
          B1224CB1224CB1224CB12223228A4CB1224CB1224CB1224CB1224CB1224CB122
          4CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1
          224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224C
          B1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB122}
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btnClearAvailableFiltersClick
      end
      object btnClear: TBitBtn
        Left = 367
        Top = 0
        Width = 40
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'Clear'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = btnClearClick
      end
    end
  end
end
