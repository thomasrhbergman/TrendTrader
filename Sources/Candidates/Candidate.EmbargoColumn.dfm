object frmCandidateEmbargoColumn: TfrmCandidateEmbargoColumn
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Embargo'
  ClientHeight = 133
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 92
    Width = 635
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 302
      Height = 13
      Caption = 'There are hidden outdated edits below!!! Note for programmer'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
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
    Left = 0
    Top = 210
    Width = 220
    Height = 17
    Caption = 'Release Time'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Visible = False
    OnClick = OnChangeEnabled
  end
  object rbHoldTime: TRadioButton
    Left = 0
    Top = 238
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
    Visible = False
    OnClick = OnChangeEnabled
  end
  object rbRankingSum: TRadioButton
    Left = 8
    Top = 8
    Width = 205
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
    Left = 0
    Top = 264
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
    Visible = False
    OnClick = OnChangeEnabled
  end
  object pnlConditions: TPanel
    Left = 220
    Top = 0
    Width = 415
    Height = 92
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 5
    object pnlReleaseTime: TPanel
      Left = 5
      Top = 208
      Width = 415
      Height = 28
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 0
      Visible = False
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
      Left = 5
      Top = 236
      Width = 415
      Height = 28
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
      Visible = False
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
      Top = 0
      Width = 415
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 2
      object lblRankingSumOperation: TLabel
        Left = 206
        Top = 6
        Width = 16
        Height = 13
        Caption = '>='
      end
      object cbRankingSum: TComboBox
        Left = 14
        Top = 3
        Width = 101
        Height = 21
        TabOrder = 0
        Visible = False
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
      Left = 5
      Top = 292
      Width = 415
      Height = 28
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 3
      Visible = False
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
      Left = 5
      Top = 320
      Width = 415
      Height = 28
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 4
      Visible = False
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
      Left = 5
      Top = 348
      Width = 415
      Height = 28
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 5
      Visible = False
      object cbValueExistInColumn: TComboBox
        Left = 5
        Top = 4
        Width = 219
        Height = 21
        TabOrder = 0
      end
    end
    object pnlTimeIntervalFromDataObtained: TPanel
      Left = 5
      Top = 376
      Width = 415
      Height = 28
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 6
      Visible = False
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
    object pnlTimePeriod: TPanel
      Left = 0
      Top = 28
      Width = 415
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 7
      object edTimeStart: TDateTimePicker
        Left = 83
        Top = 4
        Width = 101
        Height = 21
        Date = 44057.000000000000000000
        Time = 0.375000000000000000
        Kind = dtkTime
        TabOrder = 0
      end
      object edTimeFinish: TDateTimePicker
        Left = 192
        Top = 4
        Width = 101
        Height = 21
        Date = 44057.000000000000000000
        Time = 0.812500000000000000
        Kind = dtkTime
        TabOrder = 1
      end
      object cbTimePeriodWorkingDays: TCheckBox
        Left = 299
        Top = 6
        Width = 109
        Height = 17
        Caption = 'Only working days'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
    object pnlVolumeAmount: TPanel
      Left = 0
      Top = 56
      Width = 415
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 8
      object cbVolumeAmountTick: TComboBox
        Left = 109
        Top = 4
        Width = 112
        Height = 21
        TabOrder = 0
      end
      object cbVolumeAmountOperation: TComboBox
        Left = 225
        Top = 4
        Width = 101
        Height = 21
        TabOrder = 1
        OnChange = cbColumnValueChange
      end
      object edVolumeAmountValue1: TNumberBox
        Left = 333
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
  object rbColumnValue: TRadioButton
    Left = 0
    Top = 292
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
    Visible = False
    OnClick = OnChangeEnabled
  end
  object rbValueExistInColumn: TRadioButton
    Left = 0
    Top = 320
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
    Visible = False
    OnClick = OnChangeEnabled
  end
  object rbTimeIntervalFromDataObtained: TRadioButton
    Left = 3
    Top = 348
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
    Visible = False
    OnClick = OnChangeEnabled
  end
  object rbTimePeriod: TRadioButton
    Left = 8
    Top = 36
    Width = 205
    Height = 17
    Caption = 'Time Period'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnClick = OnChangeEnabled
  end
  object rbVolumeAmount: TRadioButton
    Left = 9
    Top = 64
    Width = 205
    Height = 17
    Caption = 'Volume / Amount'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    OnClick = OnChangeEnabled
  end
end
