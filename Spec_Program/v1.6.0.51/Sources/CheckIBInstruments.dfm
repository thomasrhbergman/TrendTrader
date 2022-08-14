inherited frmCheckIBInstruments: TfrmCheckIBInstruments
  Caption = 'Check IB Instruments'
  ClientHeight = 643
  ClientWidth = 1028
  Constraints.MinHeight = 450
  Constraints.MinWidth = 800
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 1044
  ExplicitHeight = 682
  
  TextHeight = 13
  inherited pnlOptions: TPanel
    Width = 1028
    TabOrder = 2
    ExplicitWidth = 1028
    inherited btnExportToExcel: TBitBtn
      Left = 990
      Top = 0
      ExplicitLeft = 990
      ExplicitTop = 0
    end
    inherited btnExportToCSV: TBitBtn
      Left = 954
      Top = 0
      ExplicitLeft = 954
      ExplicitTop = 0
    end
    inherited btnPrint: TBitBtn
      Left = 918
      Top = 0
      ExplicitLeft = 918
      ExplicitTop = 0
    end
    inherited btnColumnSettings: TBitBtn
      Left = 846
      Top = 0
      ExplicitLeft = 846
      ExplicitTop = 0
    end
    object btnCheckLastPrice: TBitBtn
      Left = 109
      Top = 0
      Width = 36
      Height = 36
      Action = aCheckLastPrice
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object btnRevertIsolate: TBitBtn
      Left = 73
      Top = 0
      Width = 36
      Height = 36
      Action = aRevertIsolate
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
    object btnEdit: TBitBtn
      Left = 37
      Top = 0
      Width = 36
      Height = 36
      Action = aEdit
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
    object btnDelete: TBitBtn
      Left = 1
      Top = 0
      Width = 36
      Height = 36
      Action = aDelete
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
    end
    object btnImportFromCSV: TBitBtn
      Left = 882
      Top = 0
      Width = 36
      Height = 36
      Action = aImportFromCSV
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
    end
    object btnCheckInstruments: TBitBtn
      Left = 145
      Top = 0
      Width = 36
      Height = 36
      Action = aCheckInstruments
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
    end
  end
  object pnlBottom: TPanel [1]
    Left = 0
    Top = 601
    Width = 1028
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object pnlButtons: TPanel
      Left = 633
      Top = 0
      Width = 395
      Height = 42
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnExit: TBitBtn
        Left = 283
        Top = 1
        Width = 110
        Height = 41
        Action = aExit
        Caption = 'Ok'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Images = DMImage.vil32
        ParentFont = False
        TabOrder = 0
      end
    end
  end
  object pnlFilter: TPanel [2]
    Left = 789
    Top = 38
    Width = 239
    Height = 563
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object gbFilter: TGroupBox
      Left = 0
      Top = 0
      Width = 239
      Height = 305
      Align = alTop
      Caption = 'Filter'
      TabOrder = 0
      object lblIndustry: TLabel
        Left = 40
        Top = 152
        Width = 51
        Height = 16
        Caption = 'Industry:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblSecurityType: TLabel
        Left = 8
        Top = 125
        Width = 83
        Height = 16
        Alignment = taRightJustify
        Caption = 'Security Type:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblName: TLabel
        Left = 53
        Top = 71
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
      object lblCurrency: TLabel
        Left = 35
        Top = 98
        Width = 56
        Height = 16
        Alignment = taRightJustify
        Caption = 'Currency:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblSymbol: TLabel
        Left = 44
        Top = 44
        Width = 47
        Height = 16
        Alignment = taRightJustify
        Caption = 'Symbol:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblContractID: TLabel
        Left = 22
        Top = 17
        Width = 69
        Height = 16
        Alignment = taRightJustify
        Caption = 'Contract ID:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object btnFilterClear: TButton
        Left = 135
        Top = 257
        Width = 100
        Height = 41
        Action = aClearFilter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Images = DMImage.vil32
        ParentFont = False
        TabOrder = 0
      end
      object rgIsolate: TRadioGroup
        Left = 96
        Top = 179
        Width = 138
        Height = 76
        Caption = 'Isolate Filter'
        ItemIndex = 0
        Items.Strings = (
          'All instruments'
          'Isolate instruments'
          'Not isolate instruments')
        TabOrder = 1
      end
      object cbIndustry: TComboBox
        Left = 96
        Top = 149
        Width = 136
        Height = 24
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Sorted = True
        TabOrder = 2
      end
      object cbSecurityType: TComboBox
        Left = 96
        Top = 122
        Width = 64
        Height = 24
        CharCase = ecUpperCase
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
      end
      object edName: TEdit
        Left = 96
        Top = 68
        Width = 136
        Height = 24
        CharCase = ecUpperCase
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
      end
      object cbCurrency: TComboBox
        Left = 96
        Top = 95
        Width = 64
        Height = 24
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
      end
      object edSymbol: TEdit
        Left = 96
        Top = 41
        Width = 136
        Height = 24
        CharCase = ecUpperCase
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
      end
      object btnFilter: TButton
        Left = 34
        Top = 257
        Width = 100
        Height = 41
        Action = aFilter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Images = DMImage.vil32
        ParentFont = False
        TabOrder = 7
      end
      object seContractID: TSpinEdit
        Left = 96
        Top = 12
        Width = 136
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxValue = 0
        MinValue = 0
        ParentFont = False
        TabOrder = 8
        Value = 0
      end
    end
    object gbDependence: TGroupBox
      Left = 0
      Top = 305
      Width = 239
      Height = 152
      Align = alTop
      Caption = 'Matching Derivatives'
      TabOrder = 1
      object lbDependence: TListBox
        Left = 2
        Top = 15
        Width = 235
        Height = 135
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object btnCheckDependencies: TButton
      Left = 2
      Top = 458
      Width = 123
      Height = 25
      Caption = 'Check Dependencies'
      TabOrder = 2
      OnClick = btnCheckDependenciesClick
    end
  end
  inherited pnlMain: TPanel
    Width = 789
    Height = 563
    TabOrder = 3
    ExplicitWidth = 789
    ExplicitHeight = 563
    inherited vstTree: TVirtualStringTree
      Width = 789
      Height = 563
      Header.MainColumn = 0
      OnBeforeCellPaint = vstTreeBeforeCellPaint
      OnCompareNodes = vstTreeCompareNodes
      OnDrawText = vstTreeDrawText
      OnFocusChanged = vstTreeFocusChanged
      OnFreeNode = vstTreeFreeNode
      OnGetText = vstTreeGetText
      OnGetNodeDataSize = vstTreeGetNodeDataSize
      ExplicitWidth = 789
      ExplicitHeight = 563
      Columns = <
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 0
          Text = 'Contract Id'
          Width = 100
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 1
          Text = 'Symbol'
          Width = 80
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 2
          Text = 'Local Symbol'
          Width = 80
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 3
          Text = 'Name'
          Width = 200
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
          Text = 'Description'
          Width = 150
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 6
          Text = 'Security Type'
          Width = 80
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 7
          Text = 'Exchange'
          Width = 60
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 8
          Text = 'Primary Exchange'
          Width = 90
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 9
          Text = 'Decimals'
          Width = 60
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 10
          Text = 'Multiplier'
          Width = 60
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 11
          Text = 'Last Price'
          Width = 80
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 12
          Text = 'Expiry'
          Width = 100
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 13
          Text = 'Error Code'
          Width = 70
        end
        item
          CaptionAlignment = taCenter
          MinWidth = 100
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 14
          Text = 'Error Msg'
          Width = 100
        end
        item
          CaptionAlignment = taCenter
          MinWidth = 60
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 15
          Text = 'Isolate'
          Width = 60
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 16
          Text = 'Underlying ConId'
          Width = 80
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 17
          Text = 'Industry'
          Width = 80
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 18
          Text = 'Category'
          Width = 80
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 19
          Text = 'Subcategory'
          Width = 80
        end
        item
          Position = 20
          Width = 10
        end>
    end
  end
  inherited ActionList: TActionList
    object aExit: TAction
      Caption = 'Ok'
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aExitExecute
    end
    object aEdit: TAction
      Hint = 'Edit'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
      ShortCut = 16453
      OnExecute = aEditExecute
      OnUpdate = aDeleteUpdate
    end
    object aDelete: TAction
      Hint = 'Delete (Del)'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      ShortCut = 46
      OnExecute = aDeleteExecute
      OnUpdate = aDeleteUpdate
    end
    object aCheckLastPrice: TAction
      Hint = 'Check Last Price'
      ImageIndex = 2
      ImageName = 'Currency2_32x32'
      ShortCut = 16468
      OnExecute = aCheckLastPriceExecute
    end
    object aCheckInstruments: TAction
      ImageIndex = 60
      ImageName = 'google_webmaster_tools'
      OnExecute = aCheckInstrumentsExecute
    end
    object aFilter: TAction
      Caption = 'Filter'
      Hint = 'Filter (Ctrl+F)'
      ImageIndex = 3
      ImageName = 'MasterFilter_32x32'
      ShortCut = 16454
      OnExecute = aFilterExecute
    end
    object aClearFilter: TAction
      Caption = 'Clear'
      ImageIndex = 56
      ImageName = 'ClearFilter_32x32'
      OnExecute = aClearFilterExecute
    end
    object aRevertIsolate: TAction
      Hint = 'Revert Isolate (Ctrl+O)'
      ImageIndex = 31
      ImageName = 'transform_rotate'
      ShortCut = 16463
      OnExecute = aRevertIsolateExecute
      OnUpdate = aDeleteUpdate
    end
    object aImportFromCSV: TAction
      Hint = 'Import From CSV'
      ImageIndex = 47
      ImageName = 'tick_red'
      OnExecute = aImportFromCSVExecute
    end
    object aInfo: TAction
      ImageIndex = 6
      ImageName = 'Info_32x32'
      OnExecute = aInfoExecute
    end
  end
  object OpenDialog: TFileOpenDialog
    DefaultExtension = '*.csv'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'CSV-Files'
        FileMask = '*.csv'
      end
      item
        DisplayName = 'TXT-Files'
        FileMask = '*.txt'
      end
      item
        DisplayName = 'All Files'
        FileMask = '*.*'
      end>
    Options = [fdoPathMustExist, fdoFileMustExist, fdoCreatePrompt]
    Title = 'Import From CSV'
    Left = 32
    Top = 136
  end
end
