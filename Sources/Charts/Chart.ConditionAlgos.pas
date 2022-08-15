unit Chart.ConditionAlgos;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Publishers,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeEngine, VirtualTrees, VclTee.Series, Vcl.ExtCtrls,
  VclTee.Chart, InstrumentList, VclTee.TeeGDIPlus, VclTee.TeeProcs, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, Publishers.Interfaces, Global.Types, IABSocketAPI, IABFunctions, IABSocketAPI_const, DaImages;
{$ENDREGION}

type
  TfrmConditionAlgosChart = class(TCustomForm, IUpdateFeeds)
    ChartTrade: TChart;
    SeriesCondition: TFastLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLastValue: Double;
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    procedure SetInstrumentName(const Value: string);
  public
    procedure AddSeries(aNode: PVirtualNode; aName: string);
    procedure AddValue(aNode: PVirtualNode; aValue: Double);
    procedure AddConditionValue(aValue: Double);
    procedure Initialize(aStartTime: TDateTime; aValue: Double);
    property InstrumentName: string write SetInstrumentName;
  end;

var
  frmConditionAlgosChart: TfrmConditionAlgosChart;

implementation

{$R *.dfm}

procedure TfrmConditionAlgosChart.FormCreate(Sender: TObject);
begin
  inherited;
  FLastValue := 0;
  TPublishers.FeedPublisher.Subscribe(Self);
end;

procedure TfrmConditionAlgosChart.FormDestroy(Sender: TObject);
begin
  TPublishers.FeedPublisher.Unsubscribe(Self);
end;

function TfrmConditionAlgosChart.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmConditionAlgosChart.Initialize(aStartTime: TDateTime; aValue: Double);
begin
//  if (aValue = 0) then
//    aValue := ChartTrade.LeftAxis.Maximum;
end;

procedure TfrmConditionAlgosChart.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
begin
//  SeriesCondition.AddXY(Now, 100);
  if (TickType = ttLast) then
    SeriesCondition.AddXY(Now, FLastValue);
end;

procedure TfrmConditionAlgosChart.AddConditionValue(aValue: Double);
begin
  FLastValue := aValue;
  if (aValue > ChartTrade.LeftAxis.Maximum) then
    ChartTrade.LeftAxis.Maximum := aValue * 1.2;
end;

procedure TfrmConditionAlgosChart.AddSeries(aNode: PVirtualNode; aName: string);
var
  LineSeries: TLineSeries;
begin
  LineSeries := TLineSeries.Create(Self);
  LineSeries.Title := aName;
  LineSeries.LinePen.Width := 2;
  LineSeries.Tag := Integer(aNode);
  ChartTrade.AddSeries(LineSeries);
end;

procedure TfrmConditionAlgosChart.AddValue(aNode: PVirtualNode; aValue: Double);
var
  i: Integer;
begin
  if (aValue > 0) and (FLastValue > aValue) then
  begin
//    aValue := (aValue / FLastValue) * 100;
    if (aValue > ChartTrade.LeftAxis.Maximum) then
      ChartTrade.LeftAxis.Maximum := aValue * 1.2;
//    if (aValue < ChartTrade.LeftAxis.Minimum) then
//      ChartTrade.LeftAxis.Minimum := aValue * 0.9;

    for i := 0 to ChartTrade.SeriesCount - 1 do
      if (ChartTrade.Series[i].Tag = Integer(aNode)) then
        ChartTrade.Series[i].AddXY(Now, aValue);
  end;
end;

procedure TfrmConditionAlgosChart.SetInstrumentName(const Value: string);
begin
  ChartTrade.Title.Text.Text := Value;
  Self.Caption := Value;
end;

end.
