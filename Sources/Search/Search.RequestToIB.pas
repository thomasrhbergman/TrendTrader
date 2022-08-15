unit Search.RequestToIB;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls, Common.Types,
  VirtualTrees, IABFunctions, IABSocketAPI, DaModule, Data.DB, Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids, 
  DebugWriter, HtmlLib, Global.Types, Vcl.CheckLst, BrokerHelperAbstr, CustomForms, Document, Entity.Sokid,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const, System.Actions, Vcl.ActnList, Winapi.ShellAPI,
  IABFunctions.RequestsQueue, InstrumentList, DaImages, Search.Instruments, System.DateUtils, Utils, Publishers,
  Frame.HistoricalData, Vcl.NumberBox, MessageDialog, Frame.Custom, Frame.RealtimeFeeds, ParametersStore,
  Publishers.Interfaces;
{$ENDREGION}

type
  TfrmRequestToIB = class(TfrmSearchInstruments, IHistoricalData)
    aSubscribeHistoricalData: TAction;
    cbDurationTimeUnits: TComboBox;
    cbValidBarSize: TComboBox;
    cbWhatToShow: TComboBox;
    dtDateBegin: TDateTimePicker;
    dtDateEnd: TDateTimePicker;
    dtTimeBegin: TDateTimePicker;
    dtTimeEnd: TDateTimePicker;
    edDuration: TNumberBox;
    frameHistoricalData: TframeHistoricalData;
    lblDateFrom: TLabel;
    lblDateTo: TLabel;
    lblDuration: TLabel;
    lblValidBarSize: TLabel;
    lblWhatToShow: TLabel;
    pnlHistoricalData: TGroupBox;
    pnlParams: TPanel;
    splInstruments: TSplitter;
    procedure aSubscribeHistoricalDataExecute(Sender: TObject);
    procedure aSubscribeHistoricalDataUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    const
      C_PANEL_INFO           = 1;
      C_PANEL_HEAD_TIMESTAMP = 2;
      C_PANEL_REQUESTED      = 3;
      C_PANEL_FINISHED       = 4;
      C_PANEL_COUNT          = 5;

      C_IDENTITY_NAME = 'RequestToIB';
  private
    FIsLockControls: Boolean;
    FOldHeadTimestamp: TIABHeadTimestampEvent;
    FParametersStore: TParametersStore;
    function GetDateBegin: TDateTime;
    function GetDateEnd: TDateTime;
    function GetDuration: Integer;
    function GetDurationTimeUnits: Integer;
    function GetValidBarSize: Integer;
    function GetWhatToShow: Integer;
    procedure SetDateBegin(const Value: TDateTime);
    procedure SetDateEnd(const Value: TDateTime);
    procedure SetDuration(const Value: Integer);
    procedure SetDurationTimeUnits(const Value: Integer);
    procedure SetValidBarSize(const Value: Integer);
    procedure SetWhatToShow(const Value: Integer);
    procedure OnHeadTimestamp(Sender: TObject; DataId: Integer; HeadTimestamp: string);
    procedure OnLockControl(const aInfo: string);
    procedure OnUnlockControl(const aInfo: string = '');
    procedure SubscribeHistoricalData(const aDataId: Integer; const aSokidInfo: TSokidInfo; const aBarSize: TIABChartBarSize; const aDuration: Integer; const aWhatToShow: string);

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IHistoricalData
    procedure OnHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData); overload;
  protected
    procedure LoadParamsFromXml; override;
    procedure SaveParamsToXml; override;
  public
    procedure Initialize;
    procedure Deinitialize;
  published
    property DateBegin         : TDateTime read GetDateBegin         write SetDateBegin;
    property DateEnd           : TDateTime read GetDateEnd           write SetDateEnd;
    property Duration          : Integer   read GetDuration          write SetDuration;
    property DurationTimeUnits : Integer   read GetDurationTimeUnits write SetDurationTimeUnits;
    property ValidBarSize      : Integer   read GetValidBarSize      write SetValidBarSize;
    property WhatToShow        : Integer   read GetWhatToShow        write SetWhatToShow;
  end;

var
  frmRequestToIB: TfrmRequestToIB;

implementation

{$R *.dfm}

procedure TfrmRequestToIB.FormCreate(Sender: TObject);
begin
  inherited;
  FIsLockControls := False;
  cbValidBarSize.Items.Clear;
  for var ChartBarSize := Low(TIABChartBarSize) to High(TIABChartBarSize) do
    cbValidBarSize.Items.AddObject(ChartBarSizeString[ChartBarSize], TObject(ChartBarSize));

  cbDurationTimeUnits.Items.Clear;
  cbDurationTimeUnits.Items.AddObject('S Seconds', TObject(IAB_TIME_UNIT_SEC)); //IAB_TIME_UNIT_SEC   = 0;
  cbDurationTimeUnits.Items.AddObject('D Day', TObject(IAB_TIME_UNIT_DAY));     //IAB_TIME_UNIT_DAY   = 1;
  cbDurationTimeUnits.Items.AddObject('W Week', TObject(IAB_TIME_UNIT_WEEK));   //IAB_TIME_UNIT_WEEK  = 2;
  cbDurationTimeUnits.Items.AddObject('M Month', TObject(IAB_TIME_UNIT_MONTH)); //IAB_TIME_UNIT_MONTH = 3;
  cbDurationTimeUnits.Items.AddObject('Y Year', TObject(IAB_TIME_UNIT_YEAR));   //IAB_TIME_UNIT_YEAR  = 4;

  cbWhatToShow.Items.Clear;
  cbWhatToShow.Items.Add('TRADES');
  cbWhatToShow.Items.Add('MIDPOINT');
  cbWhatToShow.Items.Add('BID');
  cbWhatToShow.Items.Add('ASK');
  cbWhatToShow.Items.Add('BID_ASK');
  cbWhatToShow.Items.Add('ADJUSTED_LAST');
  cbWhatToShow.Items.Add('HISTORICAL_VOLATILITY');
  cbWhatToShow.Items.Add('OPTION_IMPLIED_VOLATILITY');
  cbWhatToShow.Items.Add('REBATE_RATE');
  cbWhatToShow.Items.Add('FEE_RATE');
//  cbWhatToShow.Items.Add('YIELD_BID');
//  cbWhatToShow.Items.Add('YIELD_ASK');
//  cbWhatToShow.Items.Add('YIELD_BID_ASK');
//  cbWhatToShow.Items.Add('YIELD_LAST');
  cbWhatToShow.ItemIndex := 0;
  frameHistoricalData.OnLockControl := OnLockControl;
  frameHistoricalData.OnUnlockControl := OnUnlockControl;
  frameHistoricalData.btnSubscribeHistoricalData.Action := aSubscribeHistoricalData;

  FParametersStore := TParametersStore.Create;
  DMod.CheckConnect;
  FParametersStore.Connection := DMod.ConnectionStock;
  FParametersStore.StoreComponent := Self;
  FParametersStore.IdentityName := C_IDENTITY_NAME;
  FParametersStore.PropertiesList.Add('DateBegin');
  FParametersStore.PropertiesList.Add('DateEnd');
  FParametersStore.PropertiesList.Add('Duration');
  FParametersStore.PropertiesList.Add('DurationTimeUnits');
  FParametersStore.PropertiesList.Add('ValidBarSize');
  FParametersStore.PropertiesList.Add('WhatToShow');
end;

procedure TfrmRequestToIB.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FParametersStore);
end;

procedure TfrmRequestToIB.FormShow(Sender: TObject);
begin
  inherited;
  FParametersStore.Restore;
  pnlHistoricalData.Height := pnlInstruments.Height div 3 + pnlInstrumentsTop.Height;
  vstInstruments.Height := pnlInstruments.Height div 3 - pnlInstrumentsTop.Height;
end;

procedure TfrmRequestToIB.Initialize;
begin
  TPublishers.HistoricalDataPublisher.Subscribe(Self);
  FOldHeadTimestamp  := IABClient.OnHeadTimestamp;
  IABClient.OnHeadTimestamp  := OnHeadTimestamp;
  frameHistoricalData.Initialize;
  if not DMod.fbtInstruments.Active then
    DMod.fbtInstruments.Active := True;
  LoadParamsFromXml;
  dtDateBegin.Date := Today;
  dtDateEnd.Date := Today;
//  dtTimeBegin.Time := Now;
  dtTimeEnd.Time := Now;
end;

procedure TfrmRequestToIB.Deinitialize;
begin
  TPublishers.HistoricalDataPublisher.Unsubscribe(Self);
  IABClient.OnHeadTimestamp  := FOldHeadTimestamp;
  SaveParamsToXml;
  frameHistoricalData.Deinitialize;
end;

procedure TfrmRequestToIB.LoadParamsFromXml;
begin
  inherited;

end;

procedure TfrmRequestToIB.SaveParamsToXml;
begin
  inherited;
  FParametersStore.Store;
end;

procedure TfrmRequestToIB.OnHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
resourcestring
  rsReceiving = 'Receiving Historical Data. Count of items=%d';
begin
  if (Item = 1) then
    OnLockControl(Format(rsReceiving, [Count]));
  if (Item > -1) then
    frameHistoricalData.AddHistoricalChartData(DataId, Item, Count, HistoricalChartDataElement, DateOf(dtDateBegin.Date) + TimeOf(dtTimeBegin.Time));
  if (Item = Count) then
  begin
    sbMain.Panels[C_PANEL_FINISHED].Text := 'Finished at: ' + FormatDateTime('hh:nn:ss', Now);
    sbMain.Panels[C_PANEL_COUNT].Text    := 'Count: ' + Count.ToString;
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnHistoricalData', 'DataId: ' + DataId.ToString + ', Count:' + Count.ToString);
    OnUnlockControl;
  end;
end;

procedure TfrmRequestToIB.OnLockControl(const aInfo: string);
begin
  FIsLockControls := True;
  sbMain.Panels[C_PANEL_INFO].Text := aInfo;
  sbMain.Refresh;
end;

procedure TfrmRequestToIB.OnUnlockControl(const aInfo: string = '');
begin
  FIsLockControls := False;
  sbMain.Panels[C_PANEL_INFO].Text := aInfo;
  sbMain.Refresh;
end;

procedure TfrmRequestToIB.OnHeadTimestamp(Sender: TObject; DataId: Integer; HeadTimestamp: string);
begin
  IABClient.CancelHeadTimestamp(DataId);
  sbMain.Panels[C_PANEL_HEAD_TIMESTAMP].Text := 'Head Timestamp: ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', GetDateFromHistoricalChart(HeadTimestamp));
  sbMain.Refresh;
end;

procedure TfrmRequestToIB.SubscribeHistoricalData(const aDataId: Integer; const aSokidInfo: TSokidInfo; const aBarSize: TIABChartBarSize; const aDuration: Integer; const aWhatToShow: string);
var
  InstrumentSpec: TIABInstrumentSpecItem;
  Expiry: string;
begin
  if (aSokidInfo.Expiry > 0) then
    Expiry := FormatDateTime('YYYYMMDD', aSokidInfo.Expiry)
  else
    Expiry := '';
  InstrumentSpec.ContractId      := aSokidInfo.ContractId;
  InstrumentSpec.Symbol          := aSokidInfo.Symbol;
  InstrumentSpec.LocalSymbol     := aSokidInfo.LocalSymbol;
  InstrumentSpec.SecurityType    := aSokidInfo.GetSecurityType;
  InstrumentSpec.Multiplier      := aSokidInfo.Multiplier;
  InstrumentSpec.Exchange        := aSokidInfo.Exchange;
  InstrumentSpec.PrimaryExchange := aSokidInfo.PrimaryExchange;
  InstrumentSpec.Currency        := aSokidInfo.Currency;
  InstrumentSpec.LocalSymbol     := aSokidInfo.LocalSymbol;
  InstrumentSpec.Right           := rtNone;
  InstrumentSpec.Expiry          := Expiry;
  IABClient.RequestHeadTimestamp(aSokidInfo.ContractId,
                                 InstrumentSpec,      //InstrumentSpec
                                 aWhatToShow,         //WhatToShow
                                 True,                //UseRTH
                                 True,                //IncludeExpired
                                 True);               //FormatDate

  IABClient.GetHistoricalData(aDataId,
                              aSokidInfo.Symbol,                                        //Symbol,
                              aSokidInfo.LocalSymbol,                                   //LocalSymbol,
                              aSokidInfo.Exchange,                                      //Exchange,
                              Expiry,                                                   //Expiry,
                              aSokidInfo.Currency,                                      //Currency,
                              '',                                                       //PrimaryExchange,
                              '',                                                       //Multiplier
                              aSokidInfo.GetSecurityType,                               //SecurityType,
                              rtNone,                                                   //Right,
                              0,                                                        //Strike,
                              DateTimeToIABDateTimeStr(DateOf(dtDateEnd.Date) + TimeOf(dtTimeEnd.Time)), //DateEnd
                              aDuration,                                                //DataDuration
                              Integer(cbDurationTimeUnits.Items.Objects[cbDurationTimeUnits.ItemIndex]),  //DurationTimeUnits
                              aBarSize,                                                 //BarSize
                              cdTrades,                                                 //DataBasis
                              True,                                                     //ExtendedHours
                              False,                                                    //IncludeExpired
                              False,                                                    //KeepUpdated
                              1,                                                        //DateFormat
                              0,                                                        //ContractId
                              '',                                                       //TradingClass
                              nil                                                       //ChartOptions
                              );
end;

procedure TfrmRequestToIB.aSubscribeHistoricalDataExecute(Sender: TObject);
resourcestring
  rsInstumentNotSelected = 'Instument not selected!';
  rsSubscribeHistoricalData = 'Request historical data. DataId: %d';
  rsPacingViolationsWarning = 'A pacing violation, one of the restrictions is not observed: ' + sLineBreak;
var
  Data: PSokidInfo;
  Node: PVirtualNode;
  DataId: Integer;
  pv: TPacingViolations;
begin
  inherited;
  Node := frameRealtimeFeeds.vstTree.FocusedNode;
  if not Assigned(Node) then
    ShowMessage(rsInstumentNotSelected)
  else
  begin
    Data := Node^.GetData;
    if Assigned(Data) and (Data^.Broker = TBrokerType.brIB) then
    begin
      if (TIABChartBarSize(cbValidBarSize.Items.Objects[cbValidBarSize.ItemIndex]) <= bs30sec) then
      begin
        pv := frameHistoricalData.CheckPacingViolations(Data^.Symbol,
                                                        cbWhatToShow.Text,
                                                        Data^.ContractId,
                                                        edDuration.ValueInt,
                                                        Integer(cbDurationTimeUnits.Items.Objects[cbDurationTimeUnits.ItemIndex]),
                                                        TIABChartBarSize(cbValidBarSize.Items.Objects[cbValidBarSize.ItemIndex]),
                                                        DateOf(dtDateEnd.Date) + TimeOf(dtTimeEnd.Time));
        if (pv <> pvNone) then
        begin
          TMessageDialog.ShowWarning(rsPacingViolationsWarning + pv.ToString);
          Exit;
        end;
      end;

      sbMain.Panels[C_PANEL_HEAD_TIMESTAMP].Text := 'Head Timestamp:';
      sbMain.Panels[C_PANEL_REQUESTED].Text      := 'Requested at: ' + FormatDateTime('hh:nn:ss', Now);
      DataId := General.GetNextID;
      OnLockControl(Format(rsSubscribeHistoricalData, [DataId]));

      frameHistoricalData.AddHistoricalParameters(DataId,
                                                  Data^.Symbol,
                                                  cbWhatToShow.Text,
                                                  Data^.ContractId,
                                                  edDuration.ValueInt,
                                                  Integer(cbDurationTimeUnits.Items.Objects[cbDurationTimeUnits.ItemIndex]),
                                                  TIABChartBarSize(cbValidBarSize.Items.Objects[cbValidBarSize.ItemIndex]),
                                                  DateOf(dtDateEnd.Date) + TimeOf(dtTimeEnd.Time));
      TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText,
                                                               'GetHistoricalData',
                                                               'DataId: ' + DataId.ToString +
                                                               ', Symbol: ' + Data^.Symbol +
                                                               ', LocalSymbol: ' + Data^.LocalSymbol +
                                                               ', Exchange: ' + Data^.Exchange +
                                                               ', Currency: ' + Data^.Currency +
                                                               ', SecurityType: ' + Data^.SecurityType +
                                                               ', DateEnd: ' + FormatDateTime('yyyymmdd hh:nn:ss', DateOf(dtDateEnd.Date) + TimeOf(dtTimeEnd.Time))+
                                                               ', Duration: ' + edDuration.Text +
                                                               ', DurationTimeUnits: ' + cbDurationTimeUnits.Text +
                                                               ', BarSize: ' + cbValidBarSize.Text);
      SubscribeHistoricalData(DataId, Data^, TIABChartBarSize(cbValidBarSize.Items.Objects[cbValidBarSize.ItemIndex]), edDuration.ValueInt, cbWhatToShow.Text);
    end;
  end;
end;

procedure TfrmRequestToIB.aSubscribeHistoricalDataUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := IABClient.Connected and
                             (pcBrokers.ActivePage = tsInteractiveBroker) and
                             (cbValidBarSize.ItemIndex > -1) and
                             not FIsLockControls;
end;

//Parameters Store
function TfrmRequestToIB.GetDateBegin: TDateTime;
begin
  Result := DateOf(dtDateBegin.Date) + TimeOf(dtTimeBegin.Time);
end;

function TfrmRequestToIB.GetDateEnd: TDateTime;
begin
  Result := DateOf(dtDateEnd.Date) + TimeOf(dtTimeEnd.Time);
end;

function TfrmRequestToIB.GetDuration: Integer;
begin
  Result := edDuration.ValueInt;
end;

function TfrmRequestToIB.GetDurationTimeUnits: Integer;
begin
  Result := cbDurationTimeUnits.ItemIndex;
end;

function TfrmRequestToIB.GetInstance: TObject;
begin
  Result := Self;
end;

function TfrmRequestToIB.GetValidBarSize: Integer;
begin
  Result := cbValidBarSize.ItemIndex;
end;

function TfrmRequestToIB.GetWhatToShow: Integer;
begin
  Result := cbWhatToShow.ItemIndex;
end;

procedure TfrmRequestToIB.SetDateBegin(const Value: TDateTime);
begin
  dtDateBegin.Date := DateOf(Value);
  dtTimeBegin.Time := TimeOf(Value);
end;

procedure TfrmRequestToIB.SetDateEnd(const Value: TDateTime);
begin
  dtDateEnd.Date := DateOf(Value);
  dtTimeEnd.Time := TimeOf(Value);
end;

procedure TfrmRequestToIB.SetDuration(const Value: Integer);
begin
  edDuration.ValueInt := Value;
end;

procedure TfrmRequestToIB.SetDurationTimeUnits(const Value: Integer);
begin
  cbDurationTimeUnits.ItemIndex := Value;
end;

procedure TfrmRequestToIB.SetValidBarSize(const Value: Integer);
begin
  cbValidBarSize.ItemIndex := Value;
end;

procedure TfrmRequestToIB.SetWhatToShow(const Value: Integer);
begin
  cbWhatToShow.ItemIndex := Value;
end;

end.
