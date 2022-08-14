unit Chart.ConditionRealtime;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ExtCtrls, InstrumentList, System.Generics.Collections,
  VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.Series, Vcl.GraphUtil,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, DaImages;
{$ENDREGION}

type
  TSeriesInfo = record
    AlgosList      : TDictionary<PVirtualNode, Double>;
    ConditionValue : Double;
    SeriesIndex    : Integer;
    LegendTitle    : string;
    procedure Clear;
    constructor Create(aConditionValue: Double);
  end;

  TfrmConditionRealtimeChart = class(TCustomForm)
    ChartTrade: TChart;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSeriesInfoList: TDictionary<PVirtualNode, TSeriesInfo>;
  public
    procedure AddAlgos(aNodeCondition: PVirtualNode; aNodeAlgos: PVirtualNode);
    procedure AddConditionValue(aNodeCondition: PVirtualNode; aValue: Double);
    procedure AddSeries(aNodeCondition: PVirtualNode; aLegendTitle: string);
    procedure AddValue(aNodeAlgos: PVirtualNode; aValue: Double);
    procedure DeleteAlgos(aNodeAlgos: PVirtualNode);
    procedure DeleteSeries(aNodeCondition: PVirtualNode);
  end;

var
  frmConditionRealtimeChart: TfrmConditionRealtimeChart;

implementation

{$R *.dfm}

procedure TfrmConditionRealtimeChart.FormCreate(Sender: TObject);
begin
  inherited;
  ChartTrade.ClearChart;
  ChartTrade.View3D := False;
  ChartTrade.Legend.Visible := True;
  FSeriesInfoList := TDictionary<PVirtualNode, TSeriesInfo>.Create;
end;

procedure TfrmConditionRealtimeChart.FormDestroy(Sender: TObject);
var
  recSeries: TSeriesInfo;
begin
  for recSeries in FSeriesInfoList.Values do
    recSeries.Clear;
  FreeAndNil(FSeriesInfoList);
end;

procedure TfrmConditionRealtimeChart.AddSeries(aNodeCondition: PVirtualNode; aLegendTitle: string);
var
  BarSeries: TBarSeries;
  recSeriesInfo: TSeriesInfo;
begin
  if Assigned(aNodeCondition) and not FSeriesInfoList.ContainsKey(aNodeCondition) then
  begin
    BarSeries := TBarSeries.Create(ChartTrade);
    BarSeries.BarStyle    := bsRectangle;
    BarSeries.LegendTitle := aLegendTitle;
    BarSeries.ParentChart := ChartTrade;
    BarSeries.MultiBar    := mbSideAll;

    recSeriesInfo := TSeriesInfo.Create(0);
    recSeriesInfo.LegendTitle    := aLegendTitle;
    recSeriesInfo.ConditionValue := 0;
    recSeriesInfo.SeriesIndex    := ChartTrade.SeriesCount - 1;
    FSeriesInfoList.Add(aNodeCondition, recSeriesInfo);
  end;
end;

procedure TfrmConditionRealtimeChart.AddConditionValue(aNodeCondition: PVirtualNode; aValue: Double);
var
  recSeries: TSeriesInfo;
begin
  if Assigned(aNodeCondition) and FSeriesInfoList.ContainsKey(aNodeCondition) then
  begin
    recSeries := FSeriesInfoList.Items[aNodeCondition];
    recSeries.ConditionValue := aValue;
    FSeriesInfoList.AddOrSetValue(aNodeCondition, recSeries);
  end;
end;

procedure TfrmConditionRealtimeChart.AddAlgos(aNodeCondition: PVirtualNode; aNodeAlgos: PVirtualNode);
begin
  if Assigned(aNodeCondition) and
     Assigned(aNodeAlgos) and
     FSeriesInfoList.ContainsKey(aNodeCondition) and
     not FSeriesInfoList.Items[aNodeCondition].AlgosList.ContainsKey(aNodeAlgos) then
    FSeriesInfoList.Items[aNodeCondition].AlgosList.Add(aNodeAlgos, 0);
end;

function Brighten(AColor: TColor): TColor;
var
  H, S, L: Word;
begin
  ColorRGBToHLS(AColor, H, L, S);
  Result := ColorHLSToRGB(H, 125, S);
end;

procedure TfrmConditionRealtimeChart.AddValue(aNodeAlgos: PVirtualNode; aValue: Double);
var
  DegreeCondition: Double;
  recSeries: TSeriesInfo;
  Value: Double;
  Color: TColor;
begin
  if Assigned(aNodeAlgos) then
  begin
    for recSeries in FSeriesInfoList.Values do
      if recSeries.AlgosList.ContainsKey(aNodeAlgos) then
      begin
        recSeries.AlgosList.Items[aNodeAlgos] := aValue;
        ChartTrade.SeriesList[recSeries.SeriesIndex].BeginUpdate;
        try
          ChartTrade.Series[recSeries.SeriesIndex].Clear;
          Color := Brighten(ChartTrade.Series[recSeries.SeriesIndex].Color);
          ChartTrade.Series[recSeries.SeriesIndex].Add(100, recSeries.LegendTitle, Color);
          for Value in recSeries.AlgosList.Values do
          begin
            if (recSeries.ConditionValue > 0) then
              DegreeCondition := (Value / recSeries.ConditionValue) * 100
            else
              DegreeCondition := 0;
            ChartTrade.Series[recSeries.SeriesIndex].Add(DegreeCondition);
          end;

        finally
          ChartTrade.Series[recSeries.SeriesIndex].EndUpdate;
          ChartTrade.Series[recSeries.SeriesIndex].Repaint;
        end;
        Break;
      end;
  end;
end;

procedure TfrmConditionRealtimeChart.DeleteAlgos(aNodeAlgos: PVirtualNode);
var
  recSeries: TSeriesInfo;
begin
  for recSeries in FSeriesInfoList.Values do
    if recSeries.AlgosList.ContainsKey(aNodeAlgos) then
    begin
      recSeries.AlgosList.Remove(aNodeAlgos);
      Break;
    end;
end;

procedure TfrmConditionRealtimeChart.DeleteSeries(aNodeCondition: PVirtualNode);
var
  ChartSeries: TChartSeries;
begin
  if FSeriesInfoList.ContainsKey(aNodeCondition) then
  begin
    ChartSeries := ChartTrade.Series[FSeriesInfoList.Items[aNodeCondition].SeriesIndex];
    ChartSeries.Active := False;
    FSeriesInfoList.Items[aNodeCondition].Clear;
    FSeriesInfoList.Remove(aNodeCondition);
  end;
end;

{ TSeriesInfo }
constructor TSeriesInfo.Create(aConditionValue: Double);
begin
  ConditionValue := aConditionValue;
  SeriesIndex    := -1;
  AlgosList := TDictionary<PVirtualNode, Double>.Create;
end;

procedure TSeriesInfo.Clear;
begin
  FreeAndNil(AlgosList);
end;

end.
