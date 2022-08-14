object InformationDialog: TInformationDialog
  Left = 0
  Top = 0
  ClientHeight = 480
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 486
    Height = 480
    Align = alClient
    TabOrder = 0
    object wbMessage: TWebBrowser
      Left = 1
      Top = 1
      Width = 484
      Height = 439
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      ParentCustomHint = False
      Align = alClient
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      OnBeforeNavigate2 = wbMessageBeforeNavigate2
      ExplicitTop = 0
      ControlData = {
        4C000000063200005F2D00000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126201000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
    object pnlBottom: TPanel
      Left = 1
      Top = 440
      Width = 484
      Height = 39
      Align = alBottom
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
      DesignSize = (
        484
        39)
      object btnSave: TBitBtn
        Left = 264
        Top = 0
        Width = 110
        Height = 40
        Margins.Bottom = 0
        Action = aSave
        Anchors = [akTop, akRight]
        Caption = 'Save Info'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Images = DMImage.vil32
        ParentFont = False
        TabOrder = 0
      end
      object btnOk: TBitBtn
        Left = 374
        Top = 0
        Width = 110
        Height = 40
        Action = aExit
        Anchors = [akTop, akRight]
        Caption = 'Ok'
        Default = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Images = DMImage.vil32
        ModalResult = 1
        ParentFont = False
        TabOrder = 1
      end
    end
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 312
    Top = 96
    object aExit: TAction
      Caption = 'Ok'
      ImageIndex = 46
      ImageName = 'tick'
      ShortCut = 27
      OnExecute = aExitExecute
    end
    object aSave: TAction
      Caption = 'Save Info'
      ImageIndex = 10
      ImageName = 'Save_32x32'
      OnExecute = aSaveExecute
    end
  end
  object SaveTextFileDialog: TSaveTextFileDialog
    DefaultExt = '*.html'
    FileName = 'InformationDialog.html'
    Filter = '*.html'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Encodings.Strings = (
      'ANSI'
      'ASCII'
      'Unicode'
      'UTF-8'
      'UTF-7')
    Left = 176
    Top = 104
  end
end
