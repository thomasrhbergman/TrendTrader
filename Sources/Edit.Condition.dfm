object frmEditCondition: TfrmEditCondition
  Left = 0
  Top = 0
  Caption = 'Condition'
  ClientHeight = 484
  ClientWidth = 791
  Color = clBtnFace
  Constraints.MinHeight = 323
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 16
  object pnlTop: TPanel
    Left = 0
    Top = 38
    Width = 791
    Height = 68
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblDescription: TLabel
      AlignWithMargins = True
      Left = 16
      Top = 6
      Width = 63
      Height = 16
      Margins.Left = 10
      Caption = 'Description'
      Layout = tlCenter
    end
    object lblTypeCaption: TLabel
      AlignWithMargins = True
      Left = 51
      Top = 40
      Width = 28
      Height = 16
      Margins.Left = 10
      Caption = 'Type'
      Layout = tlCenter
    end
    object lblPriorityCaption: TLabel
      AlignWithMargins = True
      Left = 451
      Top = 42
      Width = 40
      Height = 16
      Margins.Left = 10
      Caption = 'Priority'
      Layout = tlCenter
    end
    object edtDescription: TEdit
      Left = 96
      Top = 1
      Width = 347
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object cbConditionType: TComboBox
      Left = 96
      Top = 36
      Width = 241
      Height = 24
      Style = csDropDownList
      TabOrder = 1
      OnChange = cbConditionTypeChange
      Items.Strings = (
        'Realtime Value'
        'Time gap '
        'Gradient '
        'Corridor '
        'Corridor break '
        'Corridor position ')
    end
    object cbPriority: TComboBox
      Left = 498
      Top = 38
      Width = 110
      Height = 24
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'Normal'
      Items.Strings = (
        'Normal'
        'Priority'
        'Veto')
    end
    object cbBypass: TCheckBox
      Left = 498
      Top = 16
      Width = 93
      Height = 17
      Caption = 'Bypass'
      TabOrder = 3
    end
    object cbActivate: TCheckBox
      Left = 498
      Top = -1
      Width = 93
      Height = 17
      Caption = 'Activate'
      TabOrder = 4
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 444
    Width = 791
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      791
      40)
    object btnSave: TBitBtn
      Left = 690
      Top = 0
      Width = 100
      Height = 40
      Action = aSave
      Anchors = [akTop, akRight]
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 589
      Top = 0
      Width = 100
      Height = 40
      Anchors = [akTop, akRight]
      Cancel = True
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
    object btnShowChart: TBitBtn
      Left = 489
      Top = 0
      Width = 100
      Height = 40
      Action = aGraph
      Anchors = [akTop, akRight]
      Caption = 'Graph'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 2
    end
    object pnlInfo: TPanel
      Left = 0
      Top = 0
      Width = 369
      Height = 40
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      object imgWarning: TVirtualImage
        Left = 0
        Top = 0
        Width = 32
        Height = 40
        Align = alLeft
        ImageCollection = DMImage.ImCollection32
        ImageWidth = 0
        ImageHeight = 0
        ImageIndex = 58
        ImageName = 'Warning_32x32'
        ExplicitTop = 2
        ExplicitHeight = 45
      end
      object lblInfo: TLabel
        AlignWithMargins = True
        Left = 35
        Top = 3
        Width = 331
        Height = 34
        Align = alClient
        Caption = 
          'Changing the Document affects overall template in Template Creat' +
          'or'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 330
        ExplicitHeight = 13
      end
    end
    object btnAddFactor: TBitBtn
      Left = 380
      Top = 0
      Width = 110
      Height = 40
      Action = aAddFactor
      Anchors = [akTop, akRight]
      Caption = 'Add Factor'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Images = DMImage.vil32
      ParentFont = False
      TabOrder = 4
    end
  end
  object pcMain: TPageControl
    Left = 0
    Top = 106
    Width = 791
    Height = 338
    ActivePage = tsCondition
    Align = alClient
    TabOrder = 2
    OnChange = pcMainChange
    object tsCondition: TTabSheet
      Caption = 'Condition'
      object pnlMain: TPanel
        Left = 0
        Top = 0
        Width = 783
        Height = 307
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object pnlRealTimeValue: TPanel
          Left = 0
          Top = 0
          Width = 783
          Height = 185
          Margins.Top = 15
          Align = alTop
          TabOrder = 0
          object lblConditionLimit: TLabel
            Left = 692
            Top = 5
            Width = 84
            Height = 16
            Caption = 'Condition Limit'
          end
          object lblTickTypeCaption1: TLabel
            Left = 36
            Top = 5
            Width = 51
            Height = 16
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'TickType'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object lblTickTypeCaption2: TLabel
            Left = 186
            Top = 5
            Width = 50
            Height = 16
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'TickType'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object lblValue2: TLabel
            Left = 215
            Top = 56
            Width = 25
            Height = 16
            Alignment = taRightJustify
            Caption = '0.00'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object lblValue1: TLabel
            Left = 62
            Top = 57
            Width = 25
            Height = 16
            Alignment = taRightJustify
            Caption = '0.00'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object lblComparisonRt: TLabel
            Left = 592
            Top = 5
            Width = 68
            Height = 16
            Caption = 'Comparison'
          end
          object lblCompiledValue: TLabel
            AlignWithMargins = True
            Left = 261
            Top = 27
            Width = 34
            Height = 16
            Margins.Right = 5
            Caption = '=0.00'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
          end
          object lblDivisionValue: TLabel
            AlignWithMargins = True
            Left = 387
            Top = 26
            Width = 25
            Height = 16
            Margins.Right = 5
            Alignment = taRightJustify
            Caption = '0.00'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
          end
          object lblCompiledValueCaption: TLabel
            Left = 254
            Top = 5
            Width = 87
            Height = 16
            Caption = 'Compiled value'
          end
          object lblDivisionValueSmall: TLabel
            AlignWithMargins = True
            Left = 383
            Top = 43
            Width = 28
            Height = 12
            Hint = 'Green font= condition true'
            Margins.Right = 5
            Alignment = taRightJustify
            Caption = '0.0000'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -10
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
          end
          object lblCompiledValueSmall: TLabel
            AlignWithMargins = True
            Left = 270
            Top = 44
            Width = 28
            Height = 12
            Margins.Right = 5
            Caption = '0.0000'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -10
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Layout = tlCenter
          end
          object lblCalculatedValue: TLabel
            Left = 448
            Top = 5
            Width = 116
            Height = 16
            Caption = 'Calculated Condition'
          end
          object lblDivisionValueCaption: TLabel
            Left = 356
            Top = 5
            Width = 77
            Height = 16
            Caption = 'Division value'
          end
          object lblComparisonResult: TLabel
            Left = 565
            Top = 53
            Width = 120
            Height = 20
            Alignment = taCenter
            AutoSize = False
          end
          object cbTickType1: TComboBox
            Left = 6
            Top = 23
            Width = 100
            Height = 24
            TabOrder = 0
            OnChange = OnTickTypeChange
          end
          object cbTickType2: TComboBox
            Left = 157
            Top = 23
            Width = 100
            Height = 24
            TabOrder = 2
            OnChange = OnTickTypeChange
          end
          object cbTypeOperation: TComboBox
            Left = 108
            Top = 23
            Width = 47
            Height = 24
            TabOrder = 1
            Text = '+'
            OnChange = OnTickTypeChange
          end
          object cbInequalityRt: TComboBox
            Left = 566
            Top = 23
            Width = 120
            Height = 24
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            OnChange = OnTickTypeChange
            Items.Strings = (
              'greater than'
              'less than'
              'between')
          end
          object btnRefreshPrice: TButton
            Left = 355
            Top = 96
            Width = 124
            Height = 26
            Action = aRefreshPrice
            TabOrder = 4
          end
          object btnSyncChildFactors: TBitBtn
            Left = 481
            Top = 96
            Width = 124
            Height = 26
            Action = aSyncChildFactors
            Caption = 'Sync Full Tree'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            Images = DMImage.vil32
            Layout = blGlyphBottom
            ParentFont = False
            TabOrder = 5
          end
          object edtCondLimit: TNumberBox
            Left = 705
            Top = 23
            Width = 62
            Height = 24
            Alignment = taRightJustify
            Decimal = 4
            Mode = nbmFloat
            TabOrder = 6
            OnChange = OnTickTypeChange
          end
          object pnlDivisionValue: TPanel
            Left = 0
            Top = 97
            Width = 349
            Height = 81
            BevelKind = bkFlat
            BevelOuter = bvNone
            TabOrder = 7
            Visible = False
            object lblDivisionCalculateValueSmall: TLabel
              AlignWithMargins = True
              Left = 268
              Top = 46
              Width = 28
              Height = 12
              Margins.Right = 5
              Caption = '0.0000'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -10
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              Layout = tlCenter
            end
            object lblDivisionCalculateValue: TLabel
              AlignWithMargins = True
              Left = 259
              Top = 29
              Width = 34
              Height = 16
              Margins.Right = 5
              Caption = '=0.00'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              Layout = tlCenter
            end
            object lblDivisionTickTypeCaption2: TLabel
              Left = 184
              Top = 7
              Width = 50
              Height = 16
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'TickType'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object lblDivisionTickTypeCaption1: TLabel
              Left = 34
              Top = 7
              Width = 51
              Height = 16
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'TickType'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object lblDivisionValue2: TLabel
              Left = 213
              Top = 56
              Width = 25
              Height = 16
              Alignment = taRightJustify
              Caption = '0.00'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object lblDivisionValue1: TLabel
              Left = 60
              Top = 56
              Width = 25
              Height = 16
              Alignment = taRightJustify
              Caption = '0.00'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object cbDivisionTickType2: TComboBox
              Left = 155
              Top = 25
              Width = 100
              Height = 24
              TabOrder = 0
              OnChange = OnTickTypeChange
            end
            object cbDivisionTypeOperation: TComboBox
              Left = 106
              Top = 25
              Width = 47
              Height = 24
              TabOrder = 1
              Text = '+'
              OnChange = OnTickTypeChange
            end
            object cbDivisionTickType1: TComboBox
              Left = 4
              Top = 25
              Width = 100
              Height = 24
              TabOrder = 2
              OnChange = OnTickTypeChange
            end
          end
          object cbIsCalculateDivisionValue: TCheckBox
            Left = 6
            Top = 75
            Width = 153
            Height = 17
            Caption = 'Calculate division value'
            TabOrder = 8
            OnClick = cbIsCalculateDivisionValueClick
          end
          object edtCalculatedValue: TNumberBox
            Left = 481
            Top = 23
            Width = 62
            Height = 24
            Alignment = taRightJustify
            Decimal = 4
            Mode = nbmFloat
            ReadOnly = True
            TabOrder = 9
          end
        end
        object pnlTimeGap: TPanel
          Left = 0
          Top = 185
          Width = 783
          Height = 50
          Margins.Top = 15
          Align = alTop
          TabOrder = 1
          Visible = False
          object lblValidDates: TLabel
            Left = 9
            Top = 17
            Width = 63
            Height = 16
            Caption = 'Valid dates'
          end
          object lblValidTime: TLabel
            Left = 292
            Top = 17
            Width = 57
            Height = 16
            Caption = 'Valid time'
          end
          object lblValidDatesTo: TLabel
            Left = 173
            Top = 17
            Width = 11
            Height = 16
            Caption = 'to'
          end
          object lblValidTimeTo: TLabel
            Left = 469
            Top = 16
            Width = 11
            Height = 16
            Caption = 'to'
          end
          object dtpTimeStartValid: TDateTimePicker
            Left = 355
            Top = 11
            Width = 98
            Height = 29
            Date = 42350.000000000000000000
            Time = 0.791666666656965400
            DoubleBuffered = True
            Kind = dtkTime
            ParentDoubleBuffered = False
            ParentShowHint = False
            ShowHint = False
            TabOrder = 2
          end
          object dtpDateStartValid: TDateTimePicker
            Left = 78
            Top = 11
            Width = 90
            Height = 29
            Date = 42349.000000000000000000
            Time = 0.288239629633608300
            TabOrder = 0
          end
          object dtpDateEndValid: TDateTimePicker
            Left = 189
            Top = 11
            Width = 90
            Height = 29
            Date = 42349.000000000000000000
            Time = 0.288239629633608300
            TabOrder = 1
          end
          object dtpTimeEndValid: TDateTimePicker
            Left = 496
            Top = 11
            Width = 120
            Height = 29
            Date = 42349.000000000000000000
            Time = 0.791666666656965400
            Kind = dtkTime
            TabOrder = 3
          end
        end
        object pnlGradient: TPanel
          Left = 0
          Top = 235
          Width = 783
          Height = 81
          Margins.Top = 15
          Align = alTop
          TabOrder = 2
          object lblGradientCaption: TLabel
            Left = 175
            Top = 27
            Width = 168
            Height = 16
            Caption = 'Gradient value  (Price / Hour)'
          end
          object lblMonitoringSec: TLabel
            Left = 415
            Top = 55
            Width = 23
            Height = 16
            Caption = 'sec.'
          end
          object lblMonitoringCaption: TLabel
            Left = 283
            Top = 55
            Width = 60
            Height = 16
            Caption = 'Monitoring'
          end
          object lblComparisonGr: TLabel
            Left = 523
            Top = 5
            Width = 68
            Height = 16
            Caption = 'Comparison'
          end
          object edtGradient: TEdit
            Left = 355
            Top = 23
            Width = 98
            Height = 24
            TabOrder = 0
            OnKeyPress = edtTrailSellKeyPress
          end
          object pnlGradientRight: TPanel
            Left = 682
            Top = 1
            Width = 100
            Height = 79
            Align = alRight
            BevelOuter = bvNone
            Caption = '0.00'
            Ctl3D = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 1
            object lblGradientRight: TLabel
              Left = 0
              Top = 0
              Width = 100
              Height = 16
              Align = alTop
              Alignment = taCenter
              Caption = 'Gradient Value'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentColor = False
              ParentFont = False
              ExplicitWidth = 84
            end
          end
          object cbInequalityGr: TComboBox
            Left = 496
            Top = 23
            Width = 120
            Height = 24
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            Items.Strings = (
              'greater than'
              'less than'
              'between')
          end
          object edtMonitoring: TEdit
            Left = 355
            Top = 49
            Width = 48
            Height = 24
            Alignment = taCenter
            TabOrder = 3
            Text = '1800'
          end
        end
        object pnlCorridor: TPanel
          Left = 0
          Top = 316
          Width = 783
          Height = 54
          Margins.Top = 10
          Align = alTop
          TabOrder = 3
          object lblCorridorCaption: TLabel
            Left = 184
            Top = 26
            Width = 159
            Height = 16
            Caption = 'Corridor width  (% of Price)'
          end
          object lblComparisonCor: TLabel
            Left = 523
            Top = 3
            Width = 68
            Height = 16
            Caption = 'Comparison'
          end
          object edtWidth: TEdit
            Left = 355
            Top = 23
            Width = 98
            Height = 24
            TabOrder = 0
          end
          object pnlCorridorRight: TPanel
            Left = 682
            Top = 1
            Width = 100
            Height = 52
            Align = alRight
            BevelOuter = bvNone
            Caption = '0.00'
            Ctl3D = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 1
            object lblCorridorRight: TLabel
              Left = 0
              Top = 0
              Width = 100
              Height = 16
              Align = alTop
              Alignment = taCenter
              Caption = 'Corridor Value'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentColor = False
              ParentFont = False
              ExplicitWidth = 83
            end
          end
          object cbInequalityCor: TComboBox
            Left = 494
            Top = 23
            Width = 122
            Height = 24
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            Items.Strings = (
              'greater than'
              'less than'
              'between')
          end
        end
        object pnlCorridorPosition: TPanel
          Left = 0
          Top = 370
          Width = 783
          Height = 50
          Margins.Top = 10
          Align = alTop
          TabOrder = 4
          object lblCorridorPositionCaption: TLabel
            Left = 11
            Top = 15
            Width = 146
            Height = 16
            Caption = '% Up from corridor low '#8593
          end
          object edtUpProc: TEdit
            Left = 170
            Top = 11
            Width = 30
            Height = 24
            TabOrder = 0
          end
          object pnlCorridorPositionRight: TPanel
            Left = 682
            Top = 1
            Width = 100
            Height = 48
            Align = alRight
            BevelOuter = bvNone
            Caption = '0%'
            Ctl3D = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 1
          end
        end
        object pnlBreak: TPanel
          Left = 0
          Top = 470
          Width = 783
          Height = 50
          Margins.Top = 10
          Align = alTop
          TabOrder = 5
          object lblBreakCaption: TLabel
            Left = 10
            Top = 18
            Width = 92
            Height = 16
            Caption = 'If break through'
          end
          object rbBreakUp: TRadioButton
            Left = 115
            Top = 18
            Width = 28
            Height = 17
            Caption = #8593
            Checked = True
            TabOrder = 0
            TabStop = True
          end
          object rbBreakDown: TRadioButton
            Left = 150
            Top = 18
            Width = 28
            Height = 17
            Caption = #8595
            TabOrder = 1
          end
        end
        object pnlTrailBuy: TPanel
          Left = 0
          Top = 520
          Width = 783
          Height = 50
          Margins.Top = 10
          Align = alTop
          TabOrder = 6
          object lblTrailBuyIf: TLabel
            Left = 10
            Top = 18
            Width = 88
            Height = 16
            Caption = 'If LAST-price is'
          end
          object lblTrailBuyFrom: TLabel
            Left = 210
            Top = 18
            Width = 106
            Height = 16
            Caption = 'from min paid (%)'
          end
          object lblTrailBuyMaxCaption: TLabel
            Left = 413
            Top = 18
            Width = 31
            Height = 16
            Caption = 'MAX:'
          end
          object lblTrailBuyMax: TLabel
            Left = 447
            Top = 18
            Width = 20
            Height = 16
            Caption = '0.0'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblTrailBuyMinCaption: TLabel
            Left = 544
            Top = 18
            Width = 27
            Height = 16
            Caption = 'MIN:'
          end
          object lblTrailBuyMin: TLabel
            Left = 575
            Top = 18
            Width = 20
            Height = 16
            Caption = '0.0'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblTrailBuyEntryCaption: TLabel
            Left = 667
            Top = 18
            Width = 43
            Height = 16
            Caption = 'ENTRY:'
          end
          object lblTrailBuyEntry: TLabel
            Left = 714
            Top = 18
            Width = 20
            Height = 16
            Caption = '0.0'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edtTrailBuy: TEdit
            Left = 104
            Top = 15
            Width = 98
            Height = 24
            TabOrder = 0
          end
        end
        object pnlTrailSell: TPanel
          Left = 0
          Top = 570
          Width = 783
          Height = 50
          Margins.Top = 10
          Align = alTop
          TabOrder = 7
          object lblTrailSellEntry: TLabel
            Left = 714
            Top = 18
            Width = 20
            Height = 16
            Caption = '0.0'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblTrailSellEntryCaption: TLabel
            Left = 667
            Top = 18
            Width = 43
            Height = 16
            Caption = 'ENTRY:'
          end
          object lblTrailSellMin: TLabel
            Left = 575
            Top = 18
            Width = 20
            Height = 16
            Caption = '0.0'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblTrailSellMinCaption: TLabel
            Left = 544
            Top = 18
            Width = 27
            Height = 16
            Caption = 'MIN:'
          end
          object lblTrailSellMax: TLabel
            Left = 447
            Top = 18
            Width = 20
            Height = 16
            Caption = '0.0'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblTrailSellMaxCaption: TLabel
            Left = 413
            Top = 18
            Width = 31
            Height = 16
            Caption = 'MAX:'
          end
          object lblTrailSellFrom: TLabel
            Left = 210
            Top = 18
            Width = 109
            Height = 16
            Caption = 'from max paid (%)'
          end
          object lblTrailSellIf: TLabel
            Left = 10
            Top = 18
            Width = 88
            Height = 16
            Caption = 'If LAST-price is'
          end
          object edtTrailSell: TEdit
            Left = 104
            Top = 15
            Width = 98
            Height = 24
            TabOrder = 0
            OnExit = edtTrailSellExit
            OnKeyPress = edtTrailSellKeyPress
          end
        end
        object pnlDuration: TPanel
          Left = 0
          Top = 420
          Width = 783
          Height = 50
          Margins.Top = 10
          Align = alTop
          TabOrder = 8
          Visible = False
          object lblDuration: TLabel
            Left = 440
            Top = 17
            Width = 48
            Height = 16
            Caption = 'Duration'
          end
          object lblInitTime: TLabel
            Left = 232
            Top = 17
            Width = 47
            Height = 16
            Caption = 'Init time'
          end
          object dtpDuration: TDateTimePicker
            Left = 496
            Top = 10
            Width = 120
            Height = 29
            Date = 44603.000000000000000000
            Time = 0.006944444445252884
            Kind = dtkTime
            TabOrder = 0
          end
        end
      end
    end
    object tsHistory: TTabSheet
      Caption = 'History'
      ImageIndex = 1
      inline frameConditionHistory: TframeConditionHistory
        Left = 0
        Top = 0
        Width = 783
        Height = 307
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ExplicitWidth = 783
        ExplicitHeight = 307
        inherited vstTree: TVirtualStringTree
          Width = 783
          Height = 270
          ExplicitWidth = 783
          ExplicitHeight = 270
        end
        inherited pnlOptions: TPanel
          Width = 783
          ExplicitWidth = 783
          inherited btnExportToExcel: TBitBtn
            Left = 746
            ExplicitLeft = 746
          end
          inherited btnExportToCSV: TBitBtn
            Left = 710
            ExplicitLeft = 710
          end
          inherited btnPrint: TBitBtn
            Left = 674
            ExplicitLeft = 674
          end
          inherited btnColumnSettings: TBitBtn
            Left = 638
            ExplicitLeft = 638
          end
        end
      end
    end
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 791
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 3
    object lblTemplate: TLabel
      AlignWithMargins = True
      Left = 24
      Top = 10
      Width = 54
      Height = 16
      Margins.Left = 10
      Caption = 'Template'
      Layout = tlCenter
    end
    object btnSaveTemplate: TBitBtn
      Left = 371
      Top = 0
      Width = 36
      Height = 36
      Action = aSaveTemplate
      Images = DMImage.vil32
      TabOrder = 0
    end
    object btnOpenTemplate: TBitBtn
      Left = 335
      Top = 0
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aOpenTemplate
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object edCurrentTemplateName: TEdit
      Left = 96
      Top = 7
      Width = 236
      Height = 24
      TabOrder = 2
    end
    object btnSaveTemplateAs: TBitBtn
      Left = 407
      Top = 0
      Width = 36
      Height = 36
      ParentCustomHint = False
      Action = aSaveTemplateAs
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
  end
  object alMain: TActionList
    Images = DMImage.vil32
    Left = 320
    Top = 78
    object aOpenTemplate: TAction
      ImageIndex = 5
      ImageName = 'Open_32x32'
      OnExecute = aOpenTemplateExecute
    end
    object aSaveTemplate: TAction
      ImageIndex = 10
      ImageName = 'Save_32x32'
      OnExecute = aSaveTemplateExecute
    end
    object aSaveTemplateAs: TAction
      ImageIndex = 26
      ImageName = 'SaveAll_32x32'
      OnExecute = aSaveTemplateAsExecute
    end
    object aAddFactor: TAction
      Caption = 'Add Factor'
      ImageIndex = 34
      ImageName = 'AddFooter_32x32'
      OnExecute = aAddFactorExecute
      OnUpdate = aAddFactorUpdate
    end
    object aGraph: TAction
      Caption = 'Graph'
      ImageIndex = 48
      ImageName = 'Area_32x32'
      OnExecute = aGraphExecute
      OnUpdate = aAddFactorUpdate
    end
    object aSave: TAction
      Caption = 'Save'
      Enabled = False
      ImageIndex = 46
      ImageName = 'tick'
      OnExecute = aSaveExecute
      OnUpdate = aSaveUpdate
    end
    object aCalculateCond: TAction
      Caption = 'Calculate Condition'
      OnExecute = aCalculateCondExecute
    end
    object aRefreshPrice: TAction
      Caption = 'Refresh Price'
      OnExecute = aRefreshPriceExecute
      OnUpdate = aRefreshPriceUpdate
    end
    object aSyncChildFactors: TAction
      Caption = 'Sync Full Tree'
      OnExecute = aSyncChildFactorsExecute
    end
  end
end
