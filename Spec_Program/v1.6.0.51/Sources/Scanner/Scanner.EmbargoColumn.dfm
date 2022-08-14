object frmScannerEmbargoColumn: TfrmScannerEmbargoColumn
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Embargo'
  ClientHeight = 243
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 202
    Width = 635
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnAddColumn: TBitBtn
      Left = 464
      Top = 1
      Width = 142
      Height = 40
      Caption = 'Add column'
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
    object btnCancel: TBitBtn
      Left = 362
      Top = 1
      Width = 101
      Height = 40
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
  end
  object rbReleaseTime: TRadioButton
    Left = 5
    Top = 6
    Width = 220
    Height = 17
    Caption = 'Release Time'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    TabStop = True
    OnClick = OnChangeEnabled
  end
  object rbHoldTime: TRadioButton
    Left = 5
    Top = 34
    Width = 220
    Height = 17
    Caption = 'Hold Time'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = OnChangeEnabled
  end
  object rbRankingSum: TRadioButton
    Left = 5
    Top = 62
    Width = 220
    Height = 17
    Caption = 'Ranking Sum'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = OnChangeEnabled
  end
  object rbRankingPosition: TRadioButton
    Left = 5
    Top = 90
    Width = 220
    Height = 17
    Caption = 'Ranking Position'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = OnChangeEnabled
  end
  object pnlConditions: TPanel
    Left = 220
    Top = 0
    Width = 415
    Height = 202
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 5
    object pnlReleaseTime: TPanel
      Left = 0
      Top = 0
      Width = 415
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 0
      object edReleaseTime: TDateTimePicker
        Left = 122
        Top = 3
        Width = 101
        Height = 21
        Date = 44057.000000000000000000
        Time = 0.375000000000000000
        Kind = dtkTime
        TabOrder = 0
      end
      object cbReleaseWorkingDays: TCheckBox
        Left = 230
        Top = 5
        Width = 121
        Height = 17
        Caption = 'Only working days'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
    end
    object pnlHoldTime: TPanel
      Left = 0
      Top = 28
      Width = 415
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
      object edHoldTime: TDateTimePicker
        Left = 122
        Top = 4
        Width = 101
        Height = 21
        Date = 44057.000000000000000000
        Time = 0.812500000000000000
        Kind = dtkTime
        TabOrder = 0
      end
      object cbHoldWorkingDays: TCheckBox
        Left = 230
        Top = 6
        Width = 122
        Height = 17
        Caption = 'Only working days'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
    end
    object pnlRankingSum: TPanel
      Left = 0
      Top = 56
      Width = 415
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 2
      object cbRankingSum: TComboBox
        Left = 122
        Top = 3
        Width = 101
        Height = 21
        TabOrder = 0
        OnChange = cbRankingSumChange
        Items.Strings = (
          'greater or equal'
          'less  or equal'
          'between')
      end
      object pnlRankingSumBetween: TPanel
        Left = 310
        Top = 0
        Width = 105
        Height = 28
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
        object lblRankingSumAnd: TLabel
          Left = 0
          Top = 7
          Width = 18
          Height = 13
          Caption = 'and'
        end
        object edRankingSum2: TNumberBox
          Left = 23
          Top = 3
          Width = 75
          Height = 21
          AcceptExpressions = True
          Mode = nbmFloat
          TabOrder = 0
          UseMouseWheel = True
        end
      end
      object edRankingSum1: TNumberBox
        Left = 230
        Top = 3
        Width = 75
        Height = 21
        AcceptExpressions = True
        Mode = nbmFloat
        TabOrder = 2
        UseMouseWheel = True
      end
    end
    object pnlRankingPosition: TPanel
      Left = 0
      Top = 84
      Width = 415
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 3
      object cbRankingPosition: TComboBox
        Left = 122
        Top = 3
        Width = 101
        Height = 21
        TabOrder = 0
        OnChange = cbRankingPositionChange
        Items.Strings = (
          'greater than'
          'less than'
          'between')
      end
      object pnlRankingPositionBetween: TPanel
        Left = 310
        Top = 0
        Width = 105
        Height = 28
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
        object lblRankingPositionAnd: TLabel
          Left = 0
          Top = 7
          Width = 18
          Height = 13
          Caption = 'and'
        end
        object edRankingPosition2: TNumberBox
          Left = 23
          Top = 4
          Width = 75
          Height = 21
          AcceptExpressions = True
          TabOrder = 0
          SpinButtonOptions.Placement = nbspCompact
          UseMouseWheel = True
        end
      end
      object edRankingPosition1: TNumberBox
        Left = 230
        Top = 4
        Width = 75
        Height = 21
        AcceptExpressions = True
        TabOrder = 2
        SpinButtonOptions.Placement = nbspCompact
        UseMouseWheel = True
      end
    end
    object pnlColumnValue: TPanel
      Left = 0
      Top = 112
      Width = 415
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 4
      object cbColumns: TComboBox
        Left = 5
        Top = 4
        Width = 112
        Height = 21
        TabOrder = 0
        Items.Strings = (
          'Last/Close (1.00)'
          'List nr 6 test(Step:6)'
          'Last (0.50)')
      end
      object pnlColumnValue2: TPanel
        Left = 117
        Top = 0
        Width = 298
        Height = 28
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object cbColumnValue: TComboBox
          Left = 5
          Top = 4
          Width = 101
          Height = 21
          TabOrder = 0
          OnChange = cbColumnValueChange
          Items.Strings = (
            'greater than'
            'less than'
            'between')
        end
        object pnlColumnValueBetween: TPanel
          Left = 193
          Top = 0
          Width = 105
          Height = 28
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          Visible = False
          object lblColumnValueAnd: TLabel
            Left = 0
            Top = 7
            Width = 18
            Height = 13
            Caption = 'and'
          end
          object edColumnValue2: TNumberBox
            Left = 24
            Top = 4
            Width = 74
            Height = 21
            AcceptExpressions = True
            Mode = nbmFloat
            TabOrder = 0
            UseMouseWheel = True
          end
        end
        object edColumnValue1: TNumberBox
          Left = 113
          Top = 4
          Width = 75
          Height = 21
          AcceptExpressions = True
          Mode = nbmFloat
          TabOrder = 2
          UseMouseWheel = True
        end
      end
    end
    object pnlValueExistInColumn: TPanel
      Left = 0
      Top = 140
      Width = 415
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 5
      object cbValueExistInColumn: TComboBox
        Left = 5
        Top = 4
        Width = 219
        Height = 21
        TabOrder = 0
      end
    end
    object pnlTimeIntervalFromDataObtained: TPanel
      Left = 0
      Top = 168
      Width = 415
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 6
      object lblTimeIntervalMs: TLabel
        Left = 311
        Top = 7
        Width = 17
        Height = 13
        Caption = 'ms.'
      end
      object cbTimeIntervalFromDataObtained: TComboBox
        Left = 5
        Top = 4
        Width = 219
        Height = 21
        TabOrder = 0
      end
      object edTimeIntervalFromDataObtained: TNumberBox
        Left = 230
        Top = 4
        Width = 75
        Height = 21
        AcceptExpressions = True
        TabOrder = 1
        SpinButtonOptions.Placement = nbspCompact
        UseMouseWheel = True
      end
    end
  end
  object rbColumnValue: TRadioButton
    Left = 5
    Top = 118
    Width = 220
    Height = 17
    Caption = 'Column With a Specific Value'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = OnChangeEnabled
  end
  object rbValueExistInColumn: TRadioButton
    Left = 5
    Top = 146
    Width = 220
    Height = 17
    Caption = 'Value Exist In Column'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = OnChangeEnabled
  end
  object rbTimeIntervalFromDataObtained: TRadioButton
    Left = 8
    Top = 174
    Width = 216
    Height = 17
    Caption = 'Time Interval From Data Obtained'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnClick = OnChangeEnabled
  end
end
