unit Chart.Condition;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeEngine, VirtualTrees, VclTee.Series, Vcl.ExtCtrls,
  VclTee.Chart, InstrumentList, VclTee.TeeGDIPlus, VclTee.TeeProcs, Document, Vcl.StdCtrls, Common.Types,
  DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, DaImages, Global.Types;
{$ENDREGION}

type
  TfrmConditionChart = class(TCustomForm)
    ChartTrade: TChart;
    lblCorridorWidth: TLabel;
    lblCorridorWidthCaption: TLabel;
    lblGradient: TLabel;
    lblGradientCaption: TLabel;
    lblPos: TLabel;
    lblPosCaption: TLabel;
    pnlBottom: TPanel;
    pnlCaption: TPanel;
    SeriesCondition: TFastLineSeries;
    SeriesTop: TFastLineSeries;
    SeriesBottom: TFastLineSeries;
    procedure FormCreate(Sender: TObject);
  protected
  private
    FCondType: TConditionType;
    FInitValue: Double;
    FBeginTime: TDateTime;
    FValueArray: TConditionDoc.TValueArray;
    function GetValueArrayItem(Index: TConditionType): Double;
    procedure SetInstrumentName(const Value: string);
    procedure SetValueArrayItem(Index: TConditionType; const Value: Double);
  public
    procedure AddConditionValue(aTime: TDateTime; aValue: Double); overload;
    procedure AddConditionValue(aValue: Double); overload;
    procedure SetBottomLine(aStartTime, aEndTime: TDateTime; aStartPrice, aEndPrice: Double);
    procedure SetTopLine(aStartTime, aEndTime: TDateTime; aStartPrice, aEndPrice: Double); overload;
    procedure SetTopLine(aTime: TDateTime); overload;
    procedure Initialize(aValue: Double);

    property InstrumentName: string write SetInstrumentName;
    property CondType: TConditionType read FCondType write FCondType;
    property ValueArray[Index: TConditionType]: Double read GetValueArrayItem write SetValueArrayItem;
  end;

var
  frmConditionChart: TfrmConditionChart;

implementation

{$R *.dfm}

procedure TfrmConditionChart.FormCreate(Sender: TObject);
begin
  inherited;
  SeriesCondition.Clear;
  SeriesTop.Clear;
  SeriesBottom.Clear;
  FInitValue := 0;
  FBeginTime := Now;
end;

procedure TfrmConditionChart.Initialize(aValue: Double);
begin
  FInitValue := aValue;
  SetTopLine(Now);
  if (aValue > ChartTrade.LeftAxis.Maximum) then
    ChartTrade.LeftAxis.Maximum := aValue * 1.2;
  pnlBottom.Visible := not(CondType in [ctRealtimeValue]);
end;

procedure TfrmConditionChart.AddConditionValue(aTime: TDateTime; aValue: Double);
begin
  if (aValue > 0) then
  begin
    if (aValue > ChartTrade.LeftAxis.Maximum) then
      ChartTrade.LeftAxis.Maximum := aValue * 1.2;
    if CondType in [ctRealtimeValue] then
    begin
      if (FBeginTime > aTime) then
        FBeginTime := aTime;
      SetTopLine(aTime);
    end;
    SeriesCondition.AddXY(aTime, aValue);
//    General.LogPublisher.Write([ltLogWriter], Self, 'AddConditionValue', 'Date: ' + DateTimeToStr(aTime)  + ', Price: ' + aValue.ToString);
  end;
end;

procedure TfrmConditionChart.AddConditionValue(aValue: Double);
begin
  AddConditionValue(Now, aValue);
end;

procedure TfrmConditionChart.SetInstrumentName(const Value: string);
begin
  ChartTrade.Title.Text.Text := Value;
  Self.Caption := Value;
end;

procedure TfrmConditionChart.SetTopLine(aTime: TDateTime);
begin
  if (CondType in [ctRealtimeValue]) and (FInitValue > 0.1) then
  begin
    SeriesTop.BeginUpdate;
    try
      SeriesTop.Clear;
      SeriesTop.AddXY(FBeginTime, FInitValue);
      SeriesTop.AddXY(aTime, FInitValue);
    finally
      SeriesTop.EndUpdate;
    end;
//    General.LogPublisher.Write([ltLogWriter], Self, 'SetTopLine for RealtimeValue','Time: ' + DateTimeToStr(aTime) + ', Price: ' + aValue.ToString);
  end;
end;

procedure TfrmConditionChart.SetTopLine(aStartTime, aEndTime: TDateTime; aStartPrice, aEndPrice: Double);
begin
  if (not aStartPrice.IsNan) and (not aEndPrice.IsNan) and (aStartPrice > 0.1) and (aEndPrice > 0.1) then
  begin
    SeriesTop.BeginUpdate;
    try
      SeriesTop.Clear;
      SeriesTop.AddXY(aStartTime, aStartPrice);
      // aEndPrice := aEndPrice - (TConditionDoc.ctCorridor * FValueArray[TConditionDoc.ctCorridorPosition] / 100);
      SeriesTop.AddXY(aEndTime, aEndPrice);
    finally
      SeriesTop.EndUpdate;
    end;
//   General.LogPublisher.Write([ltLogWriter], Self, 'SetTopLine','StartTime: ' + DateTimeToStr(aStartTime) + ', StartPrice: ' + aStartPrice.ToString + ', EndTime: ' + DateTimeToStr(aEndTime) + ', EndPrice: ' + aEndPrice.ToString);
  end;
end;

procedure TfrmConditionChart.SetBottomLine(aStartTime, aEndTime: TDateTime; aStartPrice, aEndPrice: Double);
begin
  if (not aStartPrice.IsNan) and (not aEndPrice.IsNan) and (aStartPrice > 0.1) and (aEndPrice > 0.1) then
  begin
    if not SeriesBottom.Active then
      SeriesBottom.Active := True;
    SeriesBottom.BeginUpdate;
    try
      SeriesBottom.Clear;
      SeriesBottom.AddXY(aStartTime, aStartPrice);
      // aEndPrice := aEndPrice - (TConditionDoc.ctCorridor * FValueArray[TConditionDoc.ctCorridorPosition] / 100);
      SeriesBottom.AddXY(aEndTime, aEndPrice);
    finally
      SeriesBottom.EndUpdate;
    end;
//    General.LogPublisher.Write([ltLogWriter], Self, 'SetBottomLine','StartTime: ' + DateTimeToStr(aStartTime) + ', StartPrice: ' + aStartPrice.ToString + ', EndTime: ' + DateTimeToStr(aEndTime) + ', EndPrice: ' + aEndPrice.ToString);
  end;
end;

function TfrmConditionChart.GetValueArrayItem(Index: TConditionType): Double;
begin
  Result := FValueArray[Index];
end;

procedure TfrmConditionChart.SetValueArrayItem(Index: TConditionType; const Value: Double);
begin
  FValueArray[Index] := Value;
  case Index of
    ctGradient:
      begin
        lblGradient.Caption := FormatFloat(C_CURRENCY_FORMAT, Value);
      end;
    ctCorridor:
      begin
        lblCorridorWidth.Caption := FormatFloat(C_CURRENCY_FORMAT, Value);
      end;
    ctCorridorPosition:
      begin
        lblPos.Caption := FormatFloat('0%', Value);
      end;
  end;
end;

end.
