unit Chart.Gradient;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VclTee.TeEngine, VclTee.Series, Vcl.ExtCtrls, VclTee.Chart, InstrumentList,
  VclTee.TeeGDIPlus, VCLTee.TeeProcs, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, Publishers.Interfaces, Global.Types, IABSocketAPI, IABFunctions, IABSocketAPI_const, DaImages,
  Publishers, Entity.Sokid, System.DateUtils, Common.Types, Math;
{$ENDREGION}

type
  TfrmGradientChartForm = class(TCustomForm, IGradientChange)
    ChartGradient: TChart;
    SeriesRealPrices: TFastLineSeries;
    SeriesCalcPrices: TFastLineSeries;
    SeriesLowLinePrices: TFastLineSeries;
    SeriesHighLinePrices: TFastLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FId: Integer;
    FDuration: Integer;
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IGradientChange
    procedure OnGradientChange(const aId, aDuration: integer; const aGradientRec: TGradientRecord);
    procedure SetInstrumentName(const Value: string);
  public
    procedure Initialize(const aId, aDuration: Integer); overload;
    procedure AddValue(aSeries: TFastLineSeries; aValue: Double; aDate: TDateTime);
    procedure RefreshChart(aGradientRec: TGradientRecord);
    property InstrumentName: string write SetInstrumentName;
  end;

var
  frmGradientChartForm: TfrmGradientChartForm;

implementation

{$R *.dfm}

procedure TfrmGradientChartForm.Initialize(const aId, aDuration: Integer);
begin
  FId := aId;
  FDuration := aDuration;
  TPublishers.GradientPublisher.Subscribe(Self);
  if SokidList.ContainsKey(aId) then
    SetInstrumentName(SokidList.Items[aId].Name);
end;

procedure TfrmGradientChartForm.OnGradientChange(const aId, aDuration: integer; const aGradientRec: TGradientRecord);
begin
  if (aId = FId) and (aDuration = FDuration) then
    RefreshChart(aGradientRec);
end;

procedure TfrmGradientChartForm.RefreshChart(aGradientRec: TGradientRecord);
var I: integer;
begin
  ChartGradient.LeftAxis.Minimum := 0;
  ChartGradient.LeftAxis.Maximum := 1;
  SeriesRealPrices.Clear;
  SeriesCalcPrices.Clear;
  SeriesLowLinePrices.Clear;
  SeriesHighLinePrices.Clear;
  for I := 0 to Length(aGradientRec.RealPrices) - 1 do
  begin
    AddValue(SeriesRealPrices, aGradientRec.RealPrices[I], aGradientRec.Time1[I]);
    AddValue(SeriesCalcPrices, aGradientRec.CalcPrices[I], aGradientRec.Time1[I]);
    AddValue(SeriesLowLinePrices, aGradientRec.LowLinePrices[I], aGradientRec.Time1[I]);
    AddValue(SeriesHighLinePrices, aGradientRec.HighLinePrices[I], aGradientRec.Time1[I]);
  end;
end;

procedure TfrmGradientChartForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;

procedure TfrmGradientChartForm.FormCreate(Sender: TObject);
begin
  inherited;
  FId := 0;
end;

procedure TfrmGradientChartForm.FormDestroy(Sender: TObject);
begin
  TPublishers.FeedPublisher.Unsubscribe(Self);
end;

function TfrmGradientChartForm.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmGradientChartForm.AddValue(aSeries: TFastLineSeries; aValue: Double; aDate: TDateTime);
var aMax, aMin: Double;
begin
  if (aValue > 0) then
  begin
    aMax := Max(ChartGradient.LeftAxis.Maximum, aValue * 1.5);
    aMin := Min(ChartGradient.LeftAxis.Minimum, aValue * 0.5);

    if (aMax > ChartGradient.LeftAxis.Maximum) then
    begin
      if ChartGradient.LeftAxis.Minimum >= aMax then
        ChartGradient.LeftAxis.Minimum := aMax - 1;
      ChartGradient.LeftAxis.Maximum := aMax;
    end;
    if (aMin < ChartGradient.LeftAxis.Minimum) then
    begin
      if ChartGradient.LeftAxis.Maximum <= aMin then
        ChartGradient.LeftAxis.Maximum := aMin + 1;
      ChartGradient.LeftAxis.Minimum := aMin;
    end;
    aSeries.AddXY(aDate, aValue);
  end;
end;

procedure TfrmGradientChartForm.SetInstrumentName(const Value: string);
begin
  ChartGradient.Title.Text.Text := Value;
  Self.Caption := Value + ' (Monitoring duration: '+ FDuration.ToString + ')';
end;

end.

