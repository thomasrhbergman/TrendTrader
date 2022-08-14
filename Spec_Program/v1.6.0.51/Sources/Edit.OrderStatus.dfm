object frmEditOrderStatus: TfrmEditOrderStatus
  Left = 0
  Top = 0
  Caption = 'Order Status'
  ClientHeight = 603
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 561
    Width = 560
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOk: TBitBtn
      Left = 450
      Top = 1
      Width = 109
      Height = 41
      Caption = 'Ok'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ImageIndex = 46
      ImageName = 'tick'
      Images = DMImage.vil32
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 38
    Width = 560
    Height = 435
    Align = alClient
    TabOrder = 0
    object splOrders: TSplitter
      Left = 82
      Top = 1
      Height = 433
      Beveled = True
      MinSize = 100
      ResizeStyle = rsLine
      ExplicitLeft = 91
      ExplicitTop = -2
      ExplicitHeight = 473
    end
    object splStatus: TSplitter
      Left = 242
      Top = 1
      Height = 433
      Beveled = True
      MinSize = 100
      ResizeStyle = rsLine
      ExplicitLeft = 251
      ExplicitTop = -4
      ExplicitHeight = 471
    end
    object pnlOrders: TPanel
      Left = 1
      Top = 1
      Width = 81
      Height = 433
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object lbOrders: TListBox
        Left = 0
        Top = 20
        Width = 81
        Height = 413
        Align = alClient
        Columns = 1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = lbOrdersClick
      end
      object pnlOrdersTop: TPanel
        Left = 0
        Top = 0
        Width = 81
        Height = 20
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Orders'
        TabOrder = 1
      end
    end
    object pnlStatus: TPanel
      Left = 85
      Top = 1
      Width = 157
      Height = 433
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object lbStatus: TListBox
        Left = 0
        Top = 20
        Width = 157
        Height = 413
        Style = lbOwnerDrawFixed
        Align = alClient
        Columns = 1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = lbStatusClick
        OnDrawItem = lbStatusDrawItem
      end
      object pnlStatusTop: TPanel
        Left = 0
        Top = 0
        Width = 157
        Height = 20
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Action'
        TabOrder = 1
      end
    end
    object pnlDetail: TPanel
      Left = 245
      Top = 1
      Width = 314
      Height = 433
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object meInfo: TMemo
        Left = 0
        Top = 20
        Width = 314
        Height = 413
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object pnlDetailTop: TPanel
        Left = 0
        Top = 0
        Width = 314
        Height = 20
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Detail'
        TabOrder = 1
      end
    end
  end
  object pnlInfo: TPanel
    Left = 0
    Top = 473
    Width = 560
    Height = 88
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblAutoTradesNameCaption: TLabel
      Left = 5
      Top = 71
      Width = 90
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'AutoTrades Name:'
    end
    object lblAutoTradesName: TLabel
      Left = 100
      Top = 71
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblQualifierCaption: TLabel
      Left = 5
      Top = 54
      Width = 90
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Qualifier Name:'
    end
    object lblQualifier: TLabel
      Left = 100
      Top = 54
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblSymbolCaption: TLabel
      Left = 5
      Top = 4
      Width = 90
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Symbol:'
    end
    object lblSymbol: TLabel
      Left = 100
      Top = 4
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSecurityTypeCaption: TLabel
      Left = 5
      Top = 21
      Width = 90
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Security Type:'
    end
    object lblSecurityType: TLabel
      Left = 100
      Top = 21
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblOrderActionCaption: TLabel
      Left = 5
      Top = 38
      Width = 90
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Action:'
    end
    object lblOrderAction: TLabel
      Left = 100
      Top = 38
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 560
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      560
      38)
    object btnExportToCSV: TBitBtn
      Left = 523
      Top = 1
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aExportToXML
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object btnImportFromCSV: TBitBtn
      Left = 487
      Top = 1
      Width = 36
      Height = 36
      Action = aImportFromXML
      Anchors = [akTop, akRight]
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 29
    Top = 201
    object aExportToXML: TAction
      Hint = 'Export To XML'
      ImageIndex = 61
      ImageName = 'ExportToXML_32x32'
      OnExecute = aExportToXMLExecute
    end
    object aImportFromXML: TAction
      Hint = 'Import From XML'
      ImageIndex = 62
      ImageName = 'ImportXML'
      OnExecute = aImportFromXMLExecute
    end
  end
  object OpenDialog: TFileOpenDialog
    DefaultExtension = '*.xml'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'XML-Files'
        FileMask = '*.xml'
      end
      item
        DisplayName = 'All Files'
        FileMask = '*.*'
      end>
    Options = [fdoPathMustExist, fdoFileMustExist, fdoCreatePrompt]
    Title = 'Import From XML'
    Left = 32
    Top = 136
  end
  object FileSaveDialog: TFileSaveDialog
    DefaultExtension = '*.xml'
    FavoriteLinks = <>
    FileName = 'OrderStatus.xml'
    FileTypes = <
      item
        DisplayName = 'XML-files'
        FileMask = '*.xml'
      end
      item
        DisplayName = 'All files'
        FileMask = '*.*'
      end>
    Options = [fdoOverWritePrompt, fdoPathMustExist]
    Left = 125
    Top = 143
  end
end
