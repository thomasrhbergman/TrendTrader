unit Chart.Trade;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VclTee.TeEngine, VclTee.Series, Vcl.ExtCtrls, VclTee.Chart, InstrumentList,
  VclTee.TeeGDIPlus, VCLTee.TeeProcs, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, Publishers.Interfaces, Global.Types, IABSocketAPI, IABFunctions, IABSocketAPI_const, DaImages,
  Publishers, Entity.Sokid, System.DateUtils, Common.Types;
{$ENDREGION}

type
  TfrmTradeChartForm = class(TCustomForm, IUpdateFeeds)
    ChartTrade: TChart;
    SeriesTrade: TFastLineSeries;
    SeriesFake: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FContractId: Integer;
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    procedure SetInstrumentName(const Value: string);
  public
    procedure Initialize(const aContractId: Integer); overload;
    procedure AddValue(aValue: Double); overload;
    procedure AddValue(aValue: Double; aDate: TDateTime); overload;
    property InstrumentName: string write SetInstrumentName;
  end;

var
  frmTradeChartForm: TfrmTradeChartForm;

implementation

{$R *.dfm}

procedure TfrmTradeChartForm.Initialize(const aContractId: Integer);
var
  arrPrices : TArray<TPrice>;
  PriceList : TPriceList;
begin
  FContractId := aContractId;
  TPublishers.FeedPublisher.Subscribe(Self);
  if SokidList.ContainsKey(aContractId) then
  begin
    SetInstrumentName(SokidList.Items[aContractId].Name);
    PriceList := TMonitorLists.PriceCache.GetPriceList(aContractID);
    if Assigned(PriceList) then
      arrPrices := PriceList.GetLastPrices(function(const aPrice: TPrice): Boolean
                                           begin
                                             Result := not aPrice.IsHistorical and (aPrice.TickType = ttLast) and (Trunc(aPrice.TimeStamp) = Trunc(Date));
                                             if not Result then
                                               Result := aPrice.IsHistorical and (aPrice.TickType in [ttClose, ttLast]) and (Trunc(aPrice.TimeStamp) = Trunc(Date));
                                           end);
    for var Price in arrPrices do
      AddValue(Price.Value, Price.TimeStamp);
  end;
end;

procedure TfrmTradeChartForm.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
begin
  if (Id = FContractId) and (TickType = ttLast) then
    AddValue(Value, TimeStamp);
end;

procedure TfrmTradeChartForm.FormCreate(Sender: TObject);
begin
  inherited;
  SeriesFake.Transparency := 100;
  FContractId := 0;
end;

procedure TfrmTradeChartForm.FormDestroy(Sender: TObject);
begin
  TPublishers.FeedPublisher.Unsubscribe(Self);
end;

function TfrmTradeChartForm.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmTradeChartForm.AddValue(aValue: Double);
begin
  AddValue(aValue, Now);
end;

procedure TfrmTradeChartForm.AddValue(aValue: Double; aDate: TDateTime);
begin
  if (aValue > 0) then
  begin
    if (aValue > ChartTrade.LeftAxis.Maximum) then
      ChartTrade.LeftAxis.Maximum := aValue * 1.1;
    if (aValue < ChartTrade.LeftAxis.Minimum) then
      ChartTrade.LeftAxis.Minimum := aValue;

    SeriesTrade.AddXY(aDate, aValue);
  end;
end;

procedure TfrmTradeChartForm.SetInstrumentName(const Value: string);
begin
  ChartTrade.Title.Text.Text := Value;
  Self.Caption := Value;
end;

end.

