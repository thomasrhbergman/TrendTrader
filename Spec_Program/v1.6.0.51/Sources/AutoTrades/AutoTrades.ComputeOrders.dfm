inherited frmDockFormComputeOrders: TfrmDockFormComputeOrders
  Caption = 'Exit Orders'
  ClientHeight = 572
  ClientWidth = 1001
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 1017
  ExplicitHeight = 611
  
  TextHeight = 13
  inherited pnlOptions: TPanel
    Width = 1001
    ExplicitWidth = 1001
    object lblOptions: TLabel [0]
      Left = 45
      Top = 11
      Width = 48
      Height = 13
      Caption = 'Group by:'
      Transparent = True
    end
    object lblUpdateTime: TLabel [1]
      Left = 304
      Top = 11
      Width = 64
      Height = 13
      Caption = 'Update Time:'
    end
    inherited btnExportToExcel: TBitBtn
      Left = 963
      ExplicitLeft = 963
    end
    inherited btnExportToCSV: TBitBtn
      Left = 927
      ExplicitLeft = 927
    end
    inherited btnPrint: TBitBtn
      Left = 891
      ExplicitLeft = 891
    end
    inherited btnColumnSettings: TBitBtn
      Left = 855
      ExplicitLeft = 855
    end
    object btnRefresh: TBitBtn
      Left = 1
      Top = 1
      Width = 36
      Height = 36
      Action = aRefresh
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object cbGroup: TComboBox
      Left = 100
      Top = 8
      Width = 145
      Height = 21
      TabOrder = 5
      OnChange = cbGroupChange
    end
  end
  inherited pnlMain: TPanel
    Width = 1001
    Height = 534
    ExplicitWidth = 1001
    ExplicitHeight = 534
    object splTree: TSplitter [0]
      Left = 0
      Top = 530
      Width = 1001
      Height = 4
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = -6
      ExplicitTop = 250
      ExplicitWidth = 848
    end
    inherited vstTree: TVirtualStringTree
      Width = 1001
      Height = 327
      Header.MainColumn = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
      OnCompareNodes = vstTreeCompareNodes
      OnDrawText = vstTreeDrawText
      OnFreeNode = vstTreeFreeNode
      OnGetText = vstTreeGetText
      ExplicitWidth = 1001
      ExplicitHeight = 327
      Columns = <
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 0
          Text = 'Name'
          Width = 200
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 1
          Text = 'AutoTrades ID'
          Width = 90
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 2
          Text = 'Action'
          Width = 80
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 3
          Text = 'Contract ID'
          Width = 70
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 4
          Text = 'Order Type'
          Width = 80
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 5
          Text = 'Currency'
          Width = 70
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 6
          Text = 'Total Quantity'
          Width = 85
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 7
          Text = 'Sleeping'
          Width = 75
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 8
          Text = 'PendSubmit'
          Width = 75
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 9
          Text = 'Submitted'
          Width = 75
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 10
          Text = 'Executed'
          Width = 75
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 11
          Text = 'Cancelled'
          Width = 75
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 12
          Text = 'Error'
          Width = 75
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 13
          Text = 'Fill Price'
          Width = 75
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 14
          Text = 'Last Price'
          Width = 75
        end>
    end
    object pnlBottom: TPanel
      Left = 0
      Top = 327
      Width = 1001
      Height = 203
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object gbDiscardActions: TGroupBox
        AlignWithMargins = True
        Left = 578
        Top = 3
        Width = 280
        Height = 197
        Align = alRight
        Caption = 'Exit Pending And Submitted Orders'
        TabOrder = 0
        object btnExitAllOrdersX: TBitBtn
          Left = 6
          Top = 13
          Width = 270
          Height = 36
          Action = aExitAllOrdersX
          Caption = 'Pending In XT'
          TabOrder = 0
        end
        object btnExitAllOrdersIB: TBitBtn
          Left = 6
          Top = 50
          Width = 270
          Height = 36
          Action = aExitAllOrdersIB
          Caption = 'Pending In XT && IB'
          TabOrder = 1
        end
        object btnExitAllOrdersNN: TBitBtn
          Left = 6
          Top = 87
          Width = 270
          Height = 36
          Action = aExitAllOrdersNN
          Caption = 'Pending In XT && NN'
          TabOrder = 2
        end
        object btnExitAllOrders: TBitBtn
          Left = 6
          Top = 124
          Width = 270
          Height = 36
          Action = aExitAllOrders
          Caption = 'Pending In XT && All Brokers'
          TabOrder = 3
        end
        object btnExitSelectedOrders: TBitBtn
          Left = 6
          Top = 161
          Width = 270
          Height = 36
          Action = aExitSelectedOrders
          Caption = 'Pending Selected In XT && All Brokers'
          TabOrder = 4
        end
      end
      object rgChildOrdersActions: TRadioGroup
        AlignWithMargins = True
        Left = 864
        Top = 3
        Width = 134
        Height = 60
        Margins.Bottom = 140
        Align = alRight
        Caption = 'Action With Child Orders'
        ItemIndex = 0
        Items.Strings = (
          'Cancel Child Orders'
          'Execute Child Orders')
        TabOrder = 1
        TabStop = True
      end
      object gbSummary: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 569
        Height = 197
        Align = alClient
        Caption = 'Summary'
        TabOrder = 2
        object vstAccount: TVirtualStringTree
          Left = 2
          Top = 15
          Width = 565
          Height = 180
          Align = alClient
          Header.AutoSizeIndex = -1
          Header.Height = 18
          Header.MainColumn = 1
          Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
          LineStyle = lsSolid
          ParentShowHint = False
          PopupMenu = pmTree
          ShowHint = True
          TabOrder = 0
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect, toMultiSelect, toAlwaysSelectNode]
          OnCompareNodes = vstAccountCompareNodes
          OnDrawText = vstAccountDrawText
          OnFreeNode = vstAccountFreeNode
          OnGetText = vstAccountGetText
          Touch.InteractiveGestures = [igPan, igPressAndTap]
          Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
          Columns = <
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 0
              Text = 'Name'
              Width = 150
            end
            item
              Alignment = taRightJustify
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 1
              Text = 'Value'
              Width = 100
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 2
              Text = 'Info / Currency'
              Width = 150
            end>
        end
      end
    end
  end
  inherited ActionList: TActionList
    object aRefresh: TAction
      Hint = 'Refresh'
      ImageIndex = 32
      ImageName = 'Refresh_32x32'
      OnExecute = aRefreshExecute
    end
    object aExitAllOrdersIB: TAction
      Caption = 'Exit All Orders That Are Pending In :X Broker :IB'
      OnExecute = aExitAllOrdersIBExecute
    end
    object aExitAllOrdersNN: TAction
      Caption = 'Exit All Orders That Are Pending In :X Broker :NN'
      OnExecute = aExitAllOrdersNNExecute
    end
    object aExitAllOrdersX: TAction
      Caption = 'Exit All Orders That Are Pending In :X'
      OnExecute = aExitAllOrdersXExecute
    end
    object aExitAllOrders: TAction
      Caption = 'Exit All Orders That Are Pending In :X All Brokers'
      OnExecute = aExitAllOrdersExecute
    end
    object aExitSelectedOrders: TAction
      Caption = 'Exit Selected Orders That Are Pending In :X All Brokers'
      OnExecute = aExitSelectedOrdersExecute
    end
  end
end
