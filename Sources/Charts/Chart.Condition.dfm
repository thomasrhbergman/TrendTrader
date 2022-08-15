object frmConditionChart: TfrmConditionChart
  Left = 0
  Top = 0
  Caption = 'ConditionChart'
  ClientHeight = 492
  ClientWidth = 755
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 424
    Width = 755
    Height = 68
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object pnlCaption: TPanel
      Left = 560
      Top = 0
      Width = 195
      Height = 68
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object lblGradientCaption: TLabel
        Left = 55
        Top = 6
        Width = 45
        Height = 13
        Alignment = taRightJustify
        Caption = 'Gradient:'
      end
      object lblGradient: TLabel
        Left = 120
        Top = 6
        Width = 22
        Height = 13
        Caption = '0.00'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lblCorridorWidthCaption: TLabel
        Left = 28
        Top = 28
        Width = 72
        Height = 13
        Alignment = taRightJustify
        Caption = 'Corridor width:'
      end
      object lblCorridorWidth: TLabel
        Left = 120
        Top = 28
        Width = 22
        Height = 13
        Caption = '0.00'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lblPosCaption: TLabel
        Left = 15
        Top = 49
        Width = 85
        Height = 13
        Alignment = taRightJustify
        Caption = 'Position from low:'
      end
      object lblPos: TLabel
        Left = 120
        Top = 49
        Width = 22
        Height = 13
        Caption = '0.00'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
  end
  object ChartTrade: TChart
    Left = 0
    Top = 0
    Width = 755
    Height = 424
    BackWall.Brush.Gradient.Direction = gdBottomTop
    BackWall.Brush.Gradient.EndColor = clWhite
    BackWall.Brush.Gradient.StartColor = 15395562
    BackWall.Brush.Gradient.Visible = True
    BackWall.Transparent = False
    Foot.Font.Color = clBlue
    Foot.Font.Name = 'Verdana'
    Gradient.Direction = gdBottomTop
    Gradient.EndColor = clWhite
    Gradient.MidColor = 15395562
    Gradient.StartColor = 15395562
    Gradient.Visible = True
    LeftWall.Color = 14745599
    Legend.Font.Name = 'Verdana'
    Legend.Shadow.Transparency = 0
    Legend.Visible = False
    RightWall.Color = 14745599
    Title.Font.Name = 'Verdana'
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.Axis.Color = 4210752
    BottomAxis.Grid.Color = 11119017
    BottomAxis.LabelsFormat.Font.Name = 'Verdana'
    BottomAxis.TicksInner.Color = 11119017
    BottomAxis.Title.Font.Name = 'Verdana'
    DepthAxis.Axis.Color = 4210752
    DepthAxis.Grid.Color = 11119017
    DepthAxis.LabelsFormat.Font.Name = 'Verdana'
    DepthAxis.TicksInner.Color = 11119017
    DepthAxis.Title.Font.Name = 'Verdana'
    DepthTopAxis.Axis.Color = 4210752
    DepthTopAxis.Grid.Color = 11119017
    DepthTopAxis.LabelsFormat.Font.Name = 'Verdana'
    DepthTopAxis.TicksInner.Color = 11119017
    DepthTopAxis.Title.Font.Name = 'Verdana'
    LeftAxis.Axis.Color = 4210752
    LeftAxis.Grid.Color = 11119017
    LeftAxis.LabelsFormat.Font.Name = 'Verdana'
    LeftAxis.TicksInner.Color = 11119017
    LeftAxis.Title.Font.Name = 'Verdana'
    RightAxis.Axis.Color = 4210752
    RightAxis.Grid.Color = 11119017
    RightAxis.LabelsFormat.Font.Name = 'Verdana'
    RightAxis.TicksInner.Color = 11119017
    RightAxis.Title.Font.Name = 'Verdana'
    TopAxis.Axis.Color = 4210752
    TopAxis.Grid.Color = 11119017
    TopAxis.LabelsFormat.Font.Name = 'Verdana'
    TopAxis.TicksInner.Color = 11119017
    TopAxis.Title.Font.Name = 'Verdana'
    View3D = False
    Align = alClient
    Color = clBlack
    TabOrder = 1
    DefaultCanvas = 'TGDIPlusCanvas'
    PrintMargins = (
      15
      21
      15
      21)
    ColorPaletteIndex = 13
    object SeriesCondition: TFastLineSeries
      Selected.Hover.Visible = True
      SeriesColor = 7814144
      LinePen.Color = 7814144
      LinePen.Width = 4
      TreatNulls = tnDontPaint
      XValues.DateTime = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object SeriesBottom: TFastLineSeries
      Selected.Hover.Visible = True
      Active = False
      SeriesColor = clBlue
      LinePen.Color = clBlue
      LinePen.Width = 2
      TreatNulls = tnDontPaint
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object SeriesTop: TFastLineSeries
      Selected.Hover.Visible = True
      SeriesColor = clGreen
      LinePen.Color = clGreen
      LinePen.Width = 2
      TreatNulls = tnDontPaint
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
end
