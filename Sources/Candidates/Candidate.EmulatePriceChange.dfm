object frmCandidateEmulatePriceChange: TfrmCandidateEmulatePriceChange
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Emulate Price Change'
  ClientHeight = 360
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poMainFormCenter
  OnClose = FormClose
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 319
    Width = 402
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnRun: TBitBtn
      Left = 307
      Top = 0
      Width = 95
      Height = 41
      Align = alRight
      Caption = 'Run'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ImageIndex = 46
      ImageName = 'tick'
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 0
      OnClick = btnRunClick
    end
    object btnGeneratePrices: TBitBtn
      Left = 5
      Top = 8
      Width = 137
      Height = 25
      Caption = 'Generate Prices'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 1
      OnClick = btnGeneratePricesClick
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 402
    Height = 41
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 1
    object lblNumberOfTicks: TLabel
      Left = 16
      Top = 13
      Width = 78
      Height = 13
      Caption = 'Number of ticks:'
    end
    object lblSeconds: TLabel
      Left = 277
      Top = 13
      Width = 39
      Height = 13
      Caption = 'seconds'
    end
    object edNumberOfTicks: TNumberBox
      Left = 100
      Top = 10
      Width = 37
      Height = 21
      Alignment = taRightJustify
      TabOrder = 0
      Value = 3.000000000000000000
    end
    object rbUp: TRadioButton
      Left = 145
      Top = 12
      Width = 33
      Height = 17
      Caption = 'Up'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object rbDown: TRadioButton
      Left = 184
      Top = 12
      Width = 42
      Height = 17
      Caption = 'Down'
      TabOrder = 2
    end
    object edSeconds: TNumberBox
      Left = 234
      Top = 10
      Width = 37
      Height = 21
      Alignment = taRightJustify
      TabOrder = 3
      Value = 10.000000000000000000
    end
    object cbLastPrice: TCheckBox
      Left = 328
      Top = 12
      Width = 65
      Height = 17
      Caption = 'Last Price'
      TabOrder = 4
    end
  end
  object pnlInstrument: TPanel
    Left = 0
    Top = 41
    Width = 402
    Height = 41
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 2
    object lblInstrument: TLabel
      Left = 16
      Top = 14
      Width = 285
      Height = 13
      AutoSize = False
      Caption = 'Instrument:'
    end
    object lblLastPrice: TLabel
      Left = 307
      Top = 14
      Width = 50
      Height = 13
      Caption = 'Last Price:'
    end
  end
  object grPrices: TDBGrid
    Left = 0
    Top = 82
    Width = 402
    Height = 237
    Align = alClient
    DataSource = dsMemTable
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnDrawColumnCell = grPricesDrawColumnCell
    Columns = <
      item
        Expanded = False
        FieldName = 'TimeStamp'
        Title.Caption = 'Time'
        Width = 155
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Price'
        Visible = True
      end>
  end
  object MemTable: TFDMemTable
    BeforePost = MemTableBeforePost
    FieldDefs = <>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 96
    Top = 112
    object MemTableTimeStamp: TDateTimeField
      FieldName = 'TimeStamp'
    end
    object MemTablePrice: TCurrencyField
      FieldName = 'Price'
      DisplayFormat = '0.0000'
    end
    object MemTableApplied: TBooleanField
      FieldName = 'Applied'
    end
  end
  object dsMemTable: TDataSource
    DataSet = MemTable
    Left = 152
    Top = 144
  end
  object TimerPriceChange: TTimer
    Enabled = False
    Interval = 200
    OnTimer = TimerPriceChangeTimer
    Left = 328
    Top = 120
  end
end
