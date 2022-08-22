inherited frmDockFormTemplateCreator: TfrmDockFormTemplateCreator
  Caption = 'Template Creator'
  ClientHeight = 525
  ClientWidth = 1049
  Constraints.MinHeight = 400
  Constraints.MinWidth = 1065
  ShowHint = True
  OnCreate = FormCreate
  ExplicitWidth = 1065
  ExplicitHeight = 564
  PixelsPerInch = 96
  TextHeight = 13
  object splQualifierSet: TSplitter [0]
    Left = 643
    Top = 38
    Height = 487
    Align = alRight
    ExplicitLeft = 657
    ExplicitTop = 62
    ExplicitHeight = 580
  end
  inherited pnlOptions: TPanel
    Width = 1049
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    TabOrder = 2
    ExplicitWidth = 1049
    object lblCurrentQualifierName: TLabel [0]
      Left = 110
      Top = 10
      Width = 38
      Height = 16
      Alignment = taRightJustify
      Caption = 'Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    inherited btnExportToExcel: TBitBtn
      Left = 610
      ExplicitLeft = 610
    end
    inherited btnExportToCSV: TBitBtn
      Left = 574
      ExplicitLeft = 574
    end
    inherited btnPrint: TBitBtn
      Left = 538
      ExplicitLeft = 538
    end
    inherited btnColumnSettings: TBitBtn
      Left = 502
      ExplicitLeft = 502
    end
    object btnSaveAs: TBitBtn
      Left = 430
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aSaveAs
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object btnSave: TBitBtn
      Left = 393
      Top = 1
      Width = 36
      Height = 36
      Action = aSave
      Images = DMImage.vil32
      TabOrder = 5
    end
    object edCurrentQualifierName: TEdit
      Left = 151
      Top = 8
      Width = 240
      Height = 21
      TabOrder = 6
    end
    object btnEditQualifier: TBitBtn
      Left = 36
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aEdit
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
    end
    object btnAdd: TBitBtn
      Left = 0
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aAddQualifier
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
    end
    object btnDeleteSelectedNode: TBitBtn
      Left = 72
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aDeleteSelectedNode
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
    end
    object btnInfo: TBitBtn
      Left = 466
      Top = 1
      Width = 36
      Height = 36
      Action = aInfo
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
    end
  end
  inherited pnlMain: TPanel
    Width = 643
    Height = 487
    ExplicitWidth = 643
    ExplicitHeight = 487
    inherited vstTree: TVirtualStringTree
      Width = 643
      Height = 487
      Colors.BorderColor = 14540253
      Colors.FocusedSelectionColor = clSkyBlue
      Colors.GridLineColor = clMoneyGreen
      Colors.SelectionTextColor = clBlack
      Colors.TreeLineColor = 16250871
      Ctl3D = False
      DefaultNodeHeight = 20
      DragHeight = 400
      Font.Height = -9
      Header.Height = 20
      Header.MainColumn = 0
      Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
      Header.SortColumn = 0
      HintMode = hmHint
      ParentBiDiMode = False
      ParentCtl3D = False
      ParentFont = False
      ScrollBarOptions.AlwaysVisible = True
      ScrollBarOptions.VerticalIncrement = 40
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoTristateTracking, toAutoChangeScale]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware]
      TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toAlwaysSelectNode]
      OnAfterCellPaint = vstTreeAfterCellPaint
      OnBeforeCellPaint = vstTreeBeforeCellPaint
      OnDblClick = vstTreeDblClick
      OnDragAllowed = vstTreeDragAllowed
      OnDragOver = vstTreeDragOver
      OnDragDrop = vstTreeDragDrop
      OnDrawText = vstTreeDrawText
      OnFreeNode = vstTreeFreeNode
      OnGetText = vstTreeGetText
      OnGetImageIndex = vstTreeGetImageIndex
      OnGetHint = vstTreeGetHint
      OnKeyDown = vstTreeKeyDown
      ExplicitWidth = 643
      ExplicitHeight = 487
      Columns = <
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 0
          Text = 'Items'
          Width = 300
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 1
          Text = 'Value'
          Width = 200
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 2
          Text = 'Calc Type'
          Width = 124
        end>
    end
  end
  object pnlQualifierSet: TPanel [3]
    Left = 646
    Top = 38
    Width = 403
    Height = 487
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object pnlQualifierSetLeft: TPanel
      Left = 0
      Top = 0
      Width = 36
      Height = 487
      Align = alLeft
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 1
      object btnOpenQualifierSet: TBitBtn
        Left = 0
        Top = 89
        Width = 36
        Height = 36
        Action = aOpen
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object btnDeleteQualifierSet: TBitBtn
        Left = 0
        Top = 125
        Width = 36
        Height = 36
        ParentCustomHint = False
        Action = aDeleteQualifier
        Images = DMImage.vil32
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
    end
    inline frameQualifierSet: TframeDocumentsTree
      Left = 36
      Top = 0
      Width = 367
      Height = 487
      Align = alClient
      ParentShowHint = False
      PopupMenu = frameQualifierSet.pmTree
      ShowHint = True
      TabOrder = 0
      ExplicitLeft = 36
      ExplicitWidth = 367
      ExplicitHeight = 487
      inherited pnlOrderGroupSet: TPanel
        Width = 367
        Height = 487
        ExplicitWidth = 367
        ExplicitHeight = 487
        inherited pnlOrderGroupSetTop: TPanel
          Width = 367
          ExplicitWidth = 367
          inherited edtSearch: TEdit
            Width = 278
            ExplicitWidth = 278
          end
          inherited btnClearSearchText: TBitBtn
            Left = 344
            Top = 41
            ExplicitLeft = 344
            ExplicitTop = 41
          end
          inherited btnExportToCSVTemplate: TBitBtn
            Left = 294
            ExplicitLeft = 294
          end
          inherited btnPrintTemplate: TBitBtn
            Left = 258
            ExplicitLeft = 258
          end
          inherited btnExportToXLS: TBitBtn
            Left = 330
            ExplicitLeft = 330
          end
          inherited pnlDescription: TPanel
            Width = 367
            ExplicitWidth = 367
          end
        end
        inherited vstTree: TVirtualStringTree
          Width = 367
          Height = 398
          OnDblClick = aOpenExecute
          ExplicitWidth = 367
          ExplicitHeight = 398
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
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
              Position = 3
              Text = 'Record Id'
              Width = 10
            end>
        end
      end
      inherited pmTree: TPopupMenu
        object miOpenQualifierSet: TMenuItem [0]
          Action = aOpen
          Caption = 'Open Existing Qualifier'
          Default = True
        end
        object miDeleteQualifierSet: TMenuItem [1]
          Action = aDeleteQualifier
          Caption = 'Delete Qualifier'
        end
        inherited miModify: TMenuItem
          Caption = 'Edit / Modify Selected Node'
          Default = False
          Visible = False
        end
        inherited miDeleteSelectedNode: TMenuItem
          Visible = False
        end
        inherited miSeparator01: TMenuItem
          Visible = False
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
      end
      inherited alTree: TActionList
        Images = nil
        inherited aDeleteSelectedNode: TAction
          ShortCut = 0
        end
      end
    end
  end
  inherited ActionList: TActionList
    Left = 40
    object aShowInformation: TAction
      Caption = 'Show Information'
      Hint = 'Show Information'
      ShortCut = 16457
      OnExecute = aShowInformationDialogExecute
      OnUpdate = aShowInformationUpdate
    end
    object aAddQualifier: TAction
      Hint = 'New Qualifier'
      ImageIndex = 52
      ImageName = 'AddItem_32x32'
      OnExecute = aAddQualifierExecute
    end
    object aAddQualifierCondition: TAction
      Caption = 'Add New QualifierCondition'
      ShortCut = 16465
      OnExecute = aAddQualifierConditionExecute
      OnUpdate = aAddQualifierConditionUpdate
    end
    object aAddAutotrade: TAction
      Caption = 'Add New Autotrade'
      ShortCut = 16468
      OnExecute = aAddAutotradeExecute
      OnUpdate = aAddAutotradeUpdate
    end
    object aAddOrderGroupSet: TAction
      Caption = 'Add New OrderGroups'
      ShortCut = 16467
      OnExecute = aAddOrderGroupSetExecute
      OnUpdate = aAddOrderGroupSetUpdate
    end
    object aAddOrderGroup: TAction
      Caption = 'Add New OrderGroup'
      ShortCut = 16455
      OnExecute = aAddOrderGroupExecute
      OnUpdate = aAddOrderGroupUpdate
    end
    object aAddOrder: TAction
      Caption = 'Add New Order'
      ShortCut = 16463
      OnExecute = aAddOrderExecute
      OnUpdate = aAddOrderUpdate
    end
    object aAddOrderAsChild: TAction
      Caption = 'Add New Order as Child'
      OnExecute = aAddOrderAsChildExecute
      OnUpdate = aAddOrderAsChildUpdate
    end
    object aAddCondition: TAction
      Caption = 'Add New Condition'
      ShortCut = 16451
      OnExecute = aAddConditionExecute
      OnUpdate = aAddConditionUpdate
    end
    object aAddAlgos: TAction
      Caption = 'Add New Algos'
      ShortCut = 16449
      OnExecute = aAddAlgosExecute
      OnUpdate = aAddAlgosUpdate
    end
    object aAddFactor: TAction
      Caption = 'Add New Factor'
      ShortCut = 16454
      OnExecute = aAddFactorExecute
      OnUpdate = aAddFactorUpdate
    end
    object aClearTree: TAction
      Caption = 'Delete All Nodes'
      Hint = 'Delete All Nodes'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aClearTreeExecute
    end
    object aCollapsTree: TAction
      Caption = 'Collaps Tree'
      ShortCut = 49219
      OnExecute = aCollapsTreeExecute
    end
    object aDeleteSelectedNode: TAction
      Hint = 'Delete'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      ShortCut = 46
      OnExecute = aDeleteSelectedNodeExecute
      OnUpdate = aDeleteSelectedNodeUpdate
    end
    object aDeleteQualifier: TAction
      Hint = 'Delete Qualifier Set'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      ShortCut = 46
      OnExecute = aDeleteQualifierExecute
    end
    object aEdit: TAction
      Hint = ' Edit'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
      ShortCut = 16461
      OnExecute = aEditExecute
      OnUpdate = aEditUpdate
    end
    object aExpandTree: TAction
      Caption = 'Expand Tree'
      ShortCut = 16453
      OnExecute = aExpandTreeExecute
    end
    object aOpen: TAction
      Hint = 'Open Existing Qualifier'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
      OnExecute = aOpenExecute
    end
    object aRefresh: TAction
      Hint = 'Refresh'
      ImageIndex = 32
      ImageName = 'Refresh_32x32'
      OnExecute = aRefreshExecute
    end
    object aSaveAs: TAction
      Hint = 'Save As...'
      ImageIndex = 26
      ImageName = 'SaveAll_32x32'
      OnExecute = aSaveAsExecute
    end
    object aSave: TAction
      Hint = 'The Template is Updated and Saved'
      ImageIndex = 10
      ImageName = 'Save_32x32'
      OnExecute = aSaveExecute
    end
    object aInfo: TAction
      Hint = 'Nodes Info'
      ImageIndex = 6
      ImageName = 'Info_32x32'
      OnExecute = aInfoExecute
    end
  end
  inherited pmTree: TPopupMenu
    OnPopup = pmTreePopup
    Left = 29
    Top = 136
    object miEdit: TMenuItem
      Action = aEdit
      Caption = 'Edit / Modify'
      Default = True
    end
    object miDeleteSelectedNode: TMenuItem
      Action = aDeleteSelectedNode
      Caption = 'Delete'
    end
    object miClearTree: TMenuItem
      Action = aClearTree
      ShortCut = 16430
    end
    object miShowInformation: TMenuItem
      Action = aShowInformation
    end
    object miSep01: TMenuItem
      Caption = '-'
    end
    object miAddQualifierCondition: TMenuItem
      Action = aAddQualifierCondition
    end
    object miAddAutotrade: TMenuItem
      Action = aAddAutotrade
    end
    object miAddOrderGroups: TMenuItem
      Action = aAddOrderGroupSet
    end
    object miAddOrderGroup: TMenuItem
      Action = aAddOrderGroup
    end
    object miAddOrder: TMenuItem
      Action = aAddOrder
    end
    object miAddNewOrderasChild: TMenuItem
      Action = aAddOrderAsChild
    end
    object miAddCondition: TMenuItem
      Action = aAddCondition
    end
    object miAddAlgos: TMenuItem
      Action = aAddAlgos
    end
    object miAddFactor: TMenuItem
      Action = aAddFactor
    end
    object miSep02: TMenuItem
      Caption = '-'
    end
    object miExpandTree: TMenuItem
      Action = aExpandTree
    end
    object miCollapsTree: TMenuItem
      Action = aCollapsTree
    end
  end
end
