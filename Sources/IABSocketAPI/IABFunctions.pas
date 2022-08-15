// http://interactivebrokers.github.io/tws-api/index.html

unit IABFunctions;

interface

{$REGION 'Region uses'}
uses
  System.SysUtils, System.Classes, System.Threading, System.Generics.Collections, System.Generics.Defaults, VCL.Forms,
  System.Diagnostics, IABSocketAPI, IABSocketAPI_const, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  Global.Types, Utils, System.Types, IABFunctions.RequestsQueue, IABFunctions.MarketRules, InstrumentList, Common.Types,
  System.IniFiles, Winapi.Windows, Publishers, IABFunctions.MessageCodes, System.DateUtils, IABFunctions.Helpers, HtmlLib,
  Vcl.Graphics;
{$ENDREGION}

type
  TOnUpdateFeeds = procedure(aId: Integer; aTickType: TIABTickType; aValue: Double) of object;
  TOnScannerNotify = procedure (Sender: TObject; ScanId: Integer) of object;
  TOnQueueOverflowNotify = procedure (Sender: TObject; Count: Integer) of object;

  TThreadIABSocket = class(TThread)
  private
    FIABClient: TIABSocket;
    FRequestsQueue: TIABRequestsQueue;
    FStopwatch: TStopwatch;
    procedure ExecuteRequest(aRequest: TIABRequest);
  protected
    procedure Execute; override;
  public
    constructor Create(const aIABClient: TIABSocket);
    destructor Destroy; override;
    property RequestsQueue : TIABRequestsQueue read FRequestsQueue;
    property IABClient     : TIABSocket        read FIABClient;
  end;

  TIABClient = class(TIABSocket)
  private
    FAskPrice            : Double;
    FAskSize             : BigDecimal;
    FBidPrice            : Double;
    FBidSize             : BigDecimal;
    FHistSubscribeList   : TSubscribeList;
    FLastPrice           : Double;
    FLastSize            : BigDecimal;
    FMarketRuleList      : TMarketRuleList;
    FMarketSubscribeList : TSubscribeList;
    FOCAGroup            : TDictionary<Integer, string>;
    FOnQueueOverflow     : TOnQueueOverflowNotify;
    FOrderId             : Integer;
    FPriceSizeData       : Boolean;
    FState               : TIABConnection;
    FThreadIABSocket     : TThreadIABSocket;
    FVolume              : BigDecimal;
  protected
    procedure DoConnectionState(Sender: TObject; State: TIABConnection);
    procedure DoEndOfStreamRead(Sender: TObject);
    procedure DoError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
    procedure DoExecution(Sender: TObject; Order: TIABOrder);
    procedure DoFundamentalData(Sender: TObject; DataID: Integer; xmlStrData: string);
    procedure DoHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
    procedure DoHistoricalDataUpdate(Sender: TObject; DataId: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
    procedure DoHistoricalTickData(Sender: TObject; DataId: Integer; TickData: TIABTickData);
    procedure DoInstrumentSpecDetails(Sender: TObject; Index: Integer);
    procedure DoMarketRule(Sender: TObject; RuleId, Item, Count: Integer; LowEdge, Increment: Double);
    procedure DoOpenOrder(Sender: TObject; Order: TIABOrder);
    procedure DoOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
    procedure DoRebuildFromTWS(Sender: TObject);
    procedure DoScannerData(Sender: TObject; Scan: TIABScan);
    procedure DoScannerParam (Sender: TObject; Parameters: string);
    procedure DoSecurityDefinitionOptionalParameter(Sender: TObject; DataID: Integer; Exchange: string; UnderlyingConId: Integer; TradingClass, Multiplier, Expirations, Strikes: string);
    procedure DoTickByTick(Sender: TObject; DataID: Integer; TickData: TIABTickData);
    procedure DoTickGeneric(Sender: TObject; DataId: Integer; TickType: TIABTickType; Value: Double);
    procedure DoTickOptionComputation(Sender: TObject; DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice: Double);
    procedure DoTickPrice(Sender: TObject; DataId: Integer; TickType: TIABTickType; Price: Double; TickAttrib: TIABTickAttrib);
    procedure DoTickSize(Sender: TObject; DataId: Integer; TickType: TIABTickType; Size: BigDecimal);
    procedure DoTickString(Sender: TObject; DataId: Integer; TickType: TIABTickType; Value: string);
    procedure DoUpdateFeeds(aId: Integer; aTickType: TIABTickType; aValue: Currency);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetContractFromOrder(const aOrder: TIABOrder): TIABContract;
    function GetTextOrderInfo(const Status: TIABOrderState; Order: TIABOrder = nil): string;
    function GetTickType(const aText: string): TIABTickType;
    function ScanExecute(const aScanCriteria: TIABScanCriteria): Integer;
    procedure CancelMktData(const DataId: Integer);
    procedure CancelScan(const ScanId: Integer);
    procedure ClearOrder(Order: TIABOrder = nil);
    procedure Connect(const aHost: string; const aPort: Integer; const aClientId: Integer);
    procedure Disconnect;
    procedure CancelHistoricalData(DataId: Integer); override;
    procedure GetHistoricalData(const aDataId: Integer; const aOrder: TIABOrder; const aHistoricalDataParams: THistoricalDataParams); overload;
    procedure reqMarketRule(const aMarketRuleIds: string);
    procedure reqMktData(const DataId: Integer; const Order: TIABOrder; const ExMarketData: TIABExMktDataSet = []);

    procedure SendRequest(const aCommand: TIABCommand; const aDataId: Integer; const aOrder: TIABOrder = nil; const aPriority: TQueuePriority = qpNormal); overload;
    procedure SendRequest(const aIABRequest: TIABRequest); overload;
    procedure SetOCAGroup(const aTempID: Integer; const aOCAGroup: string);

    property HistSubscribeList   : TSubscribeList         read FHistSubscribeList;
    property MarketRuleList      : TMarketRuleList        read FMarketRuleList;
    property MarketSubscribeList : TSubscribeList         read FMarketSubscribeList;
    property OnQueueOverflow     : TOnQueueOverflowNotify read FOnQueueOverflow write FOnQueueOverflow;
    property State               : TIABConnection         read FState           write FState;
  end;

var
  IABClient: TIABClient;

implementation

constructor TIABClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnConnectionState       := DoConnectionState;
  OnEndOfStreamRead       := DoEndOfStreamRead;
  OnError                 := DoError;
  OnExecution             := DoExecution;
  OnFundamentalData       := DoFundamentalData;
  OnHistoricalData        := DoHistoricalData;
  OnHistoricalDataUpdate  := DoHistoricalDataUpdate;
  OnHistoricalTickData    := DoHistoricalTickData;
  OnInstrumentSpecDetails := DoInstrumentSpecDetails;
  OnMarketRule            := DoMarketRule;
  OnOpenOrder             := DoOpenOrder;
  OnOrderStatus           := DoOrderStatus;
  OnRebuildFromTWS        := DoRebuildFromTWS;
  OnScannerData           := DoScannerData;
  OnScannerParam          := DoScannerParam;
  OnTickByTick            := DoTickByTick;
  OnTickGeneric           := DoTickGeneric;
  OnTickOptionComputation := DoTickOptionComputation;
  OnTickPrice             := DoTickPrice;
  OnTickSize              := DoTickSize;
  OnTickString            := DoTickString;
  OnSecurityDefinitionOptionalParameter := DoSecurityDefinitionOptionalParameter;

  ClearOrder;
  FState               := twsClosed;
  FOCAGroup            := TDictionary<Integer, string>.Create;
  FMarketRuleList      := TMarketRuleList.Create;
  FMarketSubscribeList := TSubscribeList.Create;
  FHistSubscribeList   := TSubscribeList.Create;
  FThreadIABSocket     := TThreadIABSocket.Create(Self);
end;

destructor TIABClient.Destroy;
begin
  FreeAndNil(FOCAGroup);
  FreeAndNil(FMarketRuleList);
  FreeAndNil(FMarketSubscribeList);
  FreeAndNil(FHistSubscribeList);
  if Assigned(FThreadIABSocket) then
    FThreadIABSocket.Terminate;
  inherited Destroy;
end;

procedure TIABClient.DoTickString(Sender: TObject; DataId: Integer; TickType: TIABTickType; Value: string);
var
  arr: TArray<string>;
  Price: Double;
  Time: TDateTime;
  UnixTime: Integer;
begin
  if (TickType = ttRtVolume) then
  begin
    arr := Value.Split([';']);
    if (Length(arr) < 6) then
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'DoTickString',
                                                             'Incorrect input format - DataId: ' + DataID.ToString +
                                                             ', TickType: ' + TickType.ToString +
                                                             ', Value: ' + Value)
    else
    begin
      Integer.TryParse(arr[2], UnixTime);
      Time := UnixToDateTime(UnixTime);
      Double.TryParse(arr[4], Price);    //VWAP
      TPublishers.FeedPublisher.UpdatePrice(DataId, TickType, Price, Time);
    end;
  end;
end;

procedure TIABClient.DoTickSize(Sender: TObject; DataId: Integer; TickType: TIABTickType; Size: BigDecimal);
begin
  FOrderId := DataId;
  if TickType in [ttBidSize, ttDelayedBidSize] then
    FBidSize := Size;
  if TickType in [ttAskSize, ttDelayedAskSize] then
    FAskSize := Size;
  if TickType in [ttLastSize, ttDelayedLastSize] then
    FLastSize := Size;
  if TickType in [ttVolume, ttDelayedVolume] then
    FVolume := Size;
  FPriceSizeData := True;

  DoUpdateFeeds(DataId, TickType, Size);
end;

procedure TIABClient.DoTickPrice(Sender: TObject; DataId: Integer; TickType: TIABTickType; Price: Double; TickAttrib: TIABTickAttrib);
begin
  FOrderId := DataId;
  if TickType in [ttBid, ttDelayedBid] then
    FBidPrice := Price;
  if TickType in [ttAsk, ttDelayedAsk] then
    FAskPrice := Price;
  if TickType in [ttLast, ttDelayedLast] then
    FLastPrice := Price;
  FPriceSizeData := True;

  DoUpdateFeeds(DataId, TickType, Price);
end;

procedure TIABClient.DoConnectionState(Sender: TObject; State: TIABConnection);
begin
  if (IABClient.State <> State) then
  begin
    Self.State := State;
    FMarketSubscribeList.Clear;
    FHistSubscribeList.Clear;
    case State of
      twsClosed, twsFailed:
        begin
          if Assigned(FThreadIABSocket) then
          begin
            FThreadIABSocket.Terminate;
            FThreadIABSocket := nil;
          end;
          if Assigned(OnQueueOverflow) then
            OnQueueOverflow(Self, 0);
        end;
      twsConnecting:
        ;
      twsReady:
        begin
          if not Assigned(FThreadIABSocket) then
            FThreadIABSocket := TThreadIABSocket.Create(Self);
          if not FThreadIABSocket.Started then
            FThreadIABSocket.Start;
          if Assigned(OnQueueOverflow) then
            OnQueueOverflow(Self, 0);
          GetOpenOrdersAccount;
          GetOpenOrdersClient;
          BindTWSOrdersToClient(True);
          RebuildFromTWS;
        end;
    end;
    TPublishers.ConnectionStatePublisher.OnConnectionState(Sender, State);
  end;
end;

procedure TIABClient.DoEndOfStreamRead(Sender: TObject);
begin
  if not FPriceSizeData then
    Exit;
  FPriceSizeData := False;
end;

procedure TIABClient.DoError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
begin
  TPublishers.ErrorPublisher.OnError(Sender, TempId, ErrorCode, ErrorMsg);
end;

procedure TIABClient.DoFundamentalData(Sender: TObject; DataID: Integer; xmlStrData: string);
begin
//  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'DoFundamentalData', 'DataID: ' + DataID.ToString + ', xmlStrData: ' + xmlStrData);
end;

procedure TIABClient.DoInstrumentSpecDetails(Sender: TObject; Index: Integer);
begin
  TPublishers.InstrumentSpecDetailsPublisher.OnInstrumentSpecDetails(Sender, Index);
end;

procedure TIABClient.DoHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
begin
  TPublishers.HistoricalDataPublisher.OnHistoricalData(Sender, DataId, Item, Count, HistoricalChartDataElement);
end;

procedure TIABClient.DoHistoricalDataUpdate(Sender: TObject; DataId: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
begin
  TPublishers.HistoricalDataPublisher.OnHistoricalData(Sender, DataId, 1, 1, HistoricalChartDataElement);
end;

procedure TIABClient.DoHistoricalTickData(Sender: TObject; DataId: Integer; TickData: TIABTickData);
begin
  TPublishers.TickByTickPublisher.OnHistoricalTickData(Sender, DataID, TickData);
  if TickData.TickType in [tdLast, tdAllLast] then
    TPublishers.FeedPublisher.UpdatePrice(DataId, ttLast, TickData.Price, TickData.Time);
  SendRequest(ibCancelTickByTickData, DataId);
end;

procedure TIABClient.DoTickByTick(Sender: TObject; DataID: Integer; TickData: TIABTickData);
begin
  TPublishers.TickByTickPublisher.OnTickByTick(Sender, DataID, TickData);
  if TickData.TickType in [tdLast, tdAllLast] then
    TPublishers.FeedPublisher.UpdatePrice(DataId, ttLast, TickData.Price, TickData.Time);
  SendRequest(ibCancelTickByTickData, DataId);
end;

procedure TIABClient.DoMarketRule(Sender: TObject; RuleId, Item, Count: Integer; LowEdge, Increment: Double);
var
  ArrMarketRule: TArrMarketRule;
begin
  if not FMarketRuleList.ContainsKey(RuleId) then
    FMarketRuleList.Add(RuleId, ArrMarketRule);
  ArrMarketRule := FMarketRuleList.Items[RuleId];

  if (Count > Length(ArrMarketRule)) then
    SetLength(ArrMarketRule, Count);

  ArrMarketRule[Item - 1].LowEdge    := LowEdge;
  ArrMarketRule[Item - 1].Increment := Increment;
  FMarketRuleList.AddOrSetValue(RuleId, ArrMarketRule);
  if (Item = Count) then
  begin
    TArray.Sort<TRuleItem>(ArrMarketRule, TComparer<TRuleItem>.Construct(
      function(const Left, Right: TRuleItem): Integer
      begin
        if (Left.LowEdge < Right.LowEdge) then
          Result := LessThanValue
        else
          Result := GreaterThanValue;;
      end));
    FMarketRuleList.SaveToDB(RuleId);
  end;
//  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'DoMarketRule', 'RuleId=' + RuleId.ToString + ', Item=' + Item.ToString + ', Count=' + Count.ToString + ', LowEdge=' + LowEdge.ToString + ', Increment=' + Increment.ToString);
end;

procedure TIABClient.DoOpenOrder(Sender: TObject; Order: TIABOrder);
begin
  TPublishers.OrderStatePublisher.OnOpenOrder(Sender, Order);
end;

procedure TIABClient.DoExecution(Sender: TObject; Order: TIABOrder);
begin
  TPublishers.OrderStatePublisher.OnExecution(Sender, Order);
end;

procedure TIABClient.DoOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
begin
  TPublishers.OrderStatePublisher.OnOrderStatus(Sender, Order, Status);
end;

procedure TIABClient.DoRebuildFromTWS(Sender: TObject);
begin
  TPublishers.OrderStatePublisher.OnRebuildFromTWS(Sender);
end;

procedure TIABClient.DoScannerData(Sender: TObject; Scan: TIABScan);
begin
  TPublishers.ScannerPublisher.OnScannerData(Sender, Scan);
end;

procedure TIABClient.DoScannerParam(Sender: TObject; Parameters: string);
begin
  TPublishers.ScannerPublisher.OnScannerParam(Sender, Parameters);
end;

procedure TIABClient.DoSecurityDefinitionOptionalParameter(Sender: TObject; DataID: Integer; Exchange: string; UnderlyingConId: Integer; TradingClass, Multiplier, Expirations, Strikes: string);
begin
  TPublishers.SecurityDefinitionOptionalParameterPublisher.OnSecurityDefinitionOptionalParameter(Sender, DataID, Exchange, UnderlyingConId, TradingClass, Multiplier, Expirations, Strikes);
end;

procedure TIABClient.DoTickOptionComputation(Sender: TObject; DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice: Double);
begin
  TPublishers.TickOptionComputationPublisher.OnTickOptionComputation(Sender, DataId, TickType, ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice);
end;

var
  Count: Integer = 0;

procedure TIABClient.DoUpdateFeeds(aId: Integer; aTickType: TIABTickType; aValue: Currency);
begin
  TPublishers.FeedPublisher.UpdatePrice(aId, aTickType, aValue, Now);
end;

procedure TIABClient.DoTickGeneric(Sender: TObject; DataId: Integer; TickType: TIABTickType; Value: Double);
begin
  DoUpdateFeeds(DataId, TickType, Value);
end;

procedure TIABClient.ClearOrder(Order: TIABOrder = nil);
begin
  if not Assigned(Order) then
    Order := Self.DefOrder;
  Order.Action          := iabIdle;
  Order.AuxPrice        := 0;
  Order.ContractId      := 0;
  Order.Currency        := '';
  Order.Exchange        := '';
  Order.Expiry          := '';
  Order.LocalSymbol     := '';
  Order.Multiplier      := '';
  Order.OCAGroup        := '';
  Order.OpenClose       := 'O';
  Order.OrderRef        := '';
  Order.OrderType       := otUnknown;
  Order.ParentId        := 0;
  Order.Price           := 0;
  Order.PrimaryExchange := '';
  Order.Quantity        := 0;
  Order.Right           := rtNone;
  Order.SecurityType    := stStock;
  Order.Strike          := 0;
  Order.Symbol          := '';
  Order.TrailingPercent := 0;
  Order.TrailStopPrice  := 0;
  Order.DeleteComboLeg(-1);
end;

procedure TIABClient.Connect(const aHost: string; const aPort: Integer; const aClientId: Integer);
begin
  Self.TWSPort := aPort;
  Self.ClientID := aClientId;
  if not aHost.IsEmpty then
    Self.TWSHostAddress := aHost;
  if not Self.Connected then
    Self.Connected := True;
end;

procedure TIABClient.Disconnect;
begin
  if Self.Connected then
    Self.Connected := False;
end;

function TIABClient.GetContractFromOrder(const aOrder: TIABOrder): TIABContract;
var
  Contract: TIABContract;
begin
  Contract := Default(TIABContract);
  IABSocketAPI.FillContractFromOrder(aOrder, Contract);
  Result := Contract;
end;

procedure TIABClient.GetHistoricalData(const aDataId: Integer; const aOrder: TIABOrder; const aHistoricalDataParams: THistoricalDataParams);
var
  Request: TIABRequest;
begin
  if Assigned(aOrder) and (aDataId > 0) then
  begin
    if not FHistSubscribeList.IsSubscribed(aDataId) then
    try
      if not(aOrder.SecurityType in [stCash]) then
      begin
        Request := TIABRequest.Create(ibGetHistoricalData,
                                      aOrder.ContractId,
                                      aOrder,
                                      qpNormal);
        Request.HistoricalDataParams := THistoricalDataParams.Create(aHistoricalDataParams.DurationTimeUnits,
                                                                     aHistoricalDataParams.DataDuration,
                                                                     aHistoricalDataParams.BarSize,
                                                                     aHistoricalDataParams.DataBasis,
                                                                     aHistoricalDataParams.SubscribeHistData);
        SendRequest(Request);
      end;
    finally
      FreeAndNil(aOrder);
    end;
    FHistSubscribeList.IncSubscribe(aDataId);
  end;
end;

procedure TIABClient.CancelHistoricalData(DataId: Integer);
var
  Request: TIABRequest;
begin
  if (DataID > 0) then
  begin
    if FHistSubscribeList.CanUnsubscribe(DataID) then
    begin
      Request := Default(TIABRequest);
      Request.Command  := ibCancelHistoricalData;
      Request.DataID   := DataID;
      Request.Priority := qpHigh;
      SendRequest(Request);
    end;
    FHistSubscribeList.DecSubscribe(DataID);
  end;
end;

function TIABClient.GetTextOrderInfo(const Status: TIABOrderState; Order: TIABOrder = nil): string;
var
  sb: TStringBuilder;
  OCAGroup: string;
  LastPrice: Double;
begin
  Result := '';
  if not Assigned(Order) then
    Order := Self.DefOrder;
  if not Assigned(Order) then
    Exit;

  if Order.TrailingPercent = UNSET_DOUBLE then
    Order.TrailingPercent := 0;
  if Order.TrailStopPrice = UNSET_DOUBLE then
    Order.TrailStopPrice := 0;
  if Order.AuxPrice = UNSET_DOUBLE then
    Order.AuxPrice := 0;
  if Order.LmtPriceOffset = UNSET_DOUBLE then
    Order.LmtPriceOffset := 0;

  LastPrice := TMonitorLists.PriceCache.GetLastPrice(Order.ContractId, ttLast);
  if (LastPrice = 0) then
    LastPrice := TMonitorLists.PriceCache.GetLastPrice(Order.Symbol, Order.Currency, Order.Exchange, ttLast);
  sb := TStringBuilder.Create;
  try
    FOCAGroup.TryGetValue(Order.TempId, OCAGroup);
    if OCAGroup.IsEmpty then
      OCAGroup := Order.OCAGroup;
    sb.AppendFormat('Status=%s,', [Status.ToString])
      .AppendFormat('TypeAndAction=%s %s,', [OrderTypeString[Order.OrderType], ActionString[Order.Action].ToUpper])
//      .AppendFormat('OrderType=%s,', [OrderTypeString[Order.OrderType]])
//      .AppendFormat('Action=%s,', [ActionString[Order.Action]])
      .AppendLine
      .AppendFormat('OrderID=%d,', [Order.TempId])
      .AppendFormat('ConId=%d,', [Order.ContractId])
      .AppendFormat('TimeStamp=%s,', [FormatDateTime('hh:mm:ss.zzz', Now)])
      .AppendFormat('Symbol=%s,', [Order.Symbol])
      .AppendFormat('SecurityType=%s,', [SecurityTypeString[Order.SecurityType]])
      .AppendFormat('Currency=%s,', [Order.Currency])
      .AppendFormat('Exchange=%s,', [Order.Exchange])
      .AppendFormat('Expiry=%s,', [Order.Expiry])
      .AppendFormat('OCAGroup=%s,', [OCAGroup])
      .AppendFormat('LocalSymbol=%s,', [Order.LocalSymbol])
      .AppendFormat('Multiplier=%s,', [Order.Multiplier])
      .AppendFormat('PermId=%d,', [Order.PermId])
      .AppendFormat('ParentId=%d,', [Order.ParentId])
      .AppendFormat('Strike=%g,', [Order.Strike])
      .AppendFormat('TimeInForce=%s,', [TimeInForceString[Order.TimeInForce]])
      .AppendFormat('TriggerMethod=%s,', [TTriggerMethod(Order.TriggerMethod).ToString])
      .AppendLine
      .Append('----- Quantity -----').AppendLine
      .AppendFormat('Quantity=%g,', [Order.Quantity])
      .AppendFormat('Filled=%f,', [Order.Filled])
      .AppendFormat('LatestFillQty=%g,', [Order.LatestFillQty])
      .AppendFormat('Remaining=%g,', [Order.Remaining])
      .AppendLine
      .Append('----- Price -----')
      .AppendLine;

    if (Status <> osFilled) then
      sb.AppendFormat('AddedLastPrice=%g,', [LastPrice]);

    sb.AppendFormat('FillPrice=%g,', [Order.FillPrice])
      .AppendFormat('lastFilledPrice=%g,', [Order.LatestFillPrice])
      .AppendLine
      .Append('----- Price Conditions -----').AppendLine;

    case Status of
      osCancelled:
        sb.Append('Cancelled');
      osPendSubmit:
        sb.Append('AddOrder');
      osSubmitted:
        sb.Append('Submitted');
      osFilled:
        sb.Append('Filled');
    end;
    sb.AppendFormat('LastPrice=%g,', [LastPrice]);

    case Order.OrderType of
      otTrail:
        sb.AppendFormat('TrailStopPrice=%g,', [Order.TrailStopPrice])
          .AppendFormat('TrailingPercent=%g,', [Order.TrailingPercent])
          .AppendFormat('TrailAmount=%g,', [Order.AuxPrice]);
      otTrailLimit:
        sb.AppendFormat('TrailStopPrice=%g,', [Order.TrailStopPrice])
          .AppendFormat('TrailingPercent=%g,', [Order.TrailingPercent])
          .AppendFormat('LmtPriceOffset=%g,', [Order.LmtPriceOffset])
          .AppendFormat('TrailAmount=%g,', [Order.AuxPrice]);
      otLimitTouch, otLimit, otLimitClose:
        sb.AppendFormat('LmtPrice=%g,', [Order.Price])
          .AppendFormat('LmtPriceOffset=%g,', [Order.LmtPriceOffset]);
      otStopLimit:
        sb.AppendFormat('StopPrice=%g,', [Order.AuxPrice])
          .AppendFormat('LmtPrice=%g,', [Order.Price])
          .AppendFormat('LmtPriceOffset=%g,', [Order.LmtPriceOffset]);
      otMarket, otMarketClose, otMarketOpen:
        sb.AppendFormat('AuxPrice=%g,', [Order.AuxPrice])
          .AppendFormat('Price=%g,', [Order.Price]);
      otPegPrimary, otPegMidPt:
        sb.AppendFormat('LmtPriceOffset=%g,', [Order.LmtPriceOffset])
          .AppendFormat('AuxPrice=%g,', [Order.AuxPrice])
          .AppendFormat('LmtPrice=%g,', [Order.Price])
          .AppendFormat('StopPrice=%g,', [Order.TrailStopPrice]);
      otPegMarket:
        sb.AppendFormat('AuxPrice=%g,', [Order.AuxPrice]);
    else
      sb.AppendFormat('StopPrice=%g,', [Order.TrailStopPrice])
        .AppendFormat('AuxPrice=%g,', [Order.AuxPrice])
        .AppendFormat('Price=%g,', [Order.Price])
        .AppendFormat('TrailingPercent=%g,', [Order.TrailingPercent]);
    end;
    sb.AppendFormat('Bid/Ask=%g/%g,', [TMonitorLists.PriceCache.GetLastPrice(Order.Symbol, Order.Currency, Order.Exchange, ttBid),
                                       TMonitorLists.PriceCache.GetLastPrice(Order.Symbol, Order.Currency, Order.Exchange, ttAsk)]);

    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

function TIABClient.GetTickType(const aText: string): TIABTickType;
begin
  Result := ttNotSet;
  for var TickType := System.Low(TIABTickType) to System.High(TIABTickType) do
    if (aText.ToUpper = TickType.ToString.ToUpper) then
      Exit(TickType);
end;

procedure TIABClient.reqMarketRule(const aMarketRuleIds: string);
var
  arrID: TArray<string>;
  RuleId: Integer;
begin
  arrID := GetUniqueList(aMarketRuleIds).Split([',']);
  for var Rule in arrID do
    if (not Rule.IsEmpty) then
    begin
      RuleId := StrToIntDef(Rule, 0);
      if (RuleId > 0) and (not FMarketRuleList.ContainsKey(RuleId)) then
      begin
        FMarketRuleList.Add(RuleId, []);
        Self.SendRequest(ibRequestMarketRule, RuleId);
      end;
    end;
end;

procedure TIABClient.reqMktData(const DataId: Integer; const Order: TIABOrder; const ExMarketData: TIABExMktDataSet = []);
var
  Request: TIABRequest;
begin
  if (DataID > 0) then
  begin
    if not FMarketSubscribeList.IsSubscribed(DataID) then
    begin
      Request := Default(TIABRequest);
      Request.Command      := ibGetMarketData;
      Request.DataID       := DataID;
      Request.Order        := Order;
      Request.ExMarketData := ExMarketData;
      Request.Priority     := qpHigh;
      SendRequest(Request);
    end;
    FMarketSubscribeList.IncSubscribe(DataID);
  end;
end;

procedure TIABClient.CancelMktData(const DataID: Integer);
var
  Request: TIABRequest;
begin
  if (DataID > 0) then
  begin
    if FMarketSubscribeList.CanUnsubscribe(DataID) then
    begin
      Request := Default(TIABRequest);
      Request.Command  := ibCancelMarketData;
      Request.DataID   := DataID;
      Request.Priority := qpHigh;
      SendRequest(Request);
    end;
    FMarketSubscribeList.DecSubscribe(DataID);
  end;
end;

function TIABClient.ScanExecute(const aScanCriteria: TIABScanCriteria): Integer;
var
  ScanId: Integer;
begin
  ScanId := General.GetNextScanID;
  IABClient.Scanner.NewScan(ScanId, aScanCriteria);
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'NewScan',
                                'ScanId=' + ScanId.ToString + sLineBreak +
                                'ScanCode=' + aScanCriteria.ScanCode + sLineBreak +
                                'LocationCode=' + aScanCriteria.LocationCode + sLineBreak +
                                'Instrument=' + aScanCriteria.Instrument);
  TPublishers.ScannerPublisher.OnScannerAdd(Self, ScanId);
  Result := ScanId;
end;

procedure TIABClient.CancelScan(const ScanId: Integer);
var
  i: Integer;
begin
  i := Self.Scanner.IndexOfScanId(ScanId);
  if (i > -1) then
  begin
    Self.Scanner.CancelScan(ScanId);
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'CancelScan',
                                  'ScanId=' + ScanId.ToString + sLineBreak +
                                  'ScanCode=' + Self.Scanner.Items[i].QueryCriteria.ScanCode + sLineBreak +
                                  'LocationCode=' + Self.Scanner.Items[i].QueryCriteria.LocationCode + sLineBreak +
                                  'Instrument=' + Self.Scanner.Items[i].QueryCriteria.Instrument);
    Self.Scanner.Delete(i);
  end;
  TPublishers.ScannerPublisher.OnScannerCancel(Self, ScanId);
end;

procedure TIABClient.SendRequest(const aCommand: TIABCommand; const aDataId: Integer; const aOrder: TIABOrder; const aPriority: TQueuePriority);
begin
  if not Application.Terminated and Connected and Assigned(FThreadIABSocket) and FThreadIABSocket.Started then
    FThreadIABSocket.RequestsQueue.PushItem(aCommand, aDataId, aOrder, aPriority);
end;

procedure TIABClient.SendRequest(const aIABRequest: TIABRequest);
begin
  if not Application.Terminated and Connected and Assigned(FThreadIABSocket) and FThreadIABSocket.Started then
    FThreadIABSocket.RequestsQueue.PushItem(aIABRequest);
end;

procedure TIABClient.SetOCAGroup(const aTempID: Integer; const aOCAGroup: string);
begin
  if (aTempID > 0) then
    if not FOCAGroup.ContainsKey(aTempID) then
      FOCAGroup.Add(aTempID, aOCAGroup);
end;

{ TThreadIABSocket }

constructor TThreadIABSocket.Create(const aIABClient: TIABSocket);
begin
  inherited Create(True);
  FIABClient := aIABClient;
  FreeOnTerminate := True;
  Priority := tpNormal;
  FRequestsQueue := TIABRequestsQueue.Create;
  FStopwatch := TStopwatch.StartNew;
end;

destructor TThreadIABSocket.Destroy;
begin
  FreeAndNil(FRequestsQueue);
  inherited;
end;

procedure TThreadIABSocket.Execute;
var
  WaitResult: TWaitResult;
  Requests: TIABRequest;
begin
  inherited;
  TThread.NameThreadForDebugging('IABFunctions.TThreadIABSocket');
  while not Terminated do
  begin
    WaitResult := FRequestsQueue.PopItem(Requests);
    if (WaitResult = TWaitResult.wrSignaled) then
    begin
      Sleep(18);
      ExecuteRequest(Requests);
    end;
  end;
end;

procedure TThreadIABSocket.ExecuteRequest(aRequest: TIABRequest);
var
  Info: string;
  DataEndDateTime: string;
begin
  try
    if Assigned(FIABClient) and (FIABClient is TIABClient) and FIABClient.Connected then
    begin
      case aRequest.Command of
        ibCancelAccountUpdates    : FIABClient.CancelAccountUpdates;
        ibCancelHistoricalData    : FIABClient.CancelHistoricalData(aRequest.DataID);
        ibCancelImpliedVolatility : FIABClient.CancelImpliedVolatility(aRequest.DataID);
        ibCancelMarketData        : TIABClient(FIABClient).CancelMarketData(aRequest.DataID);
        ibCancelOrder             : FIABClient.CancelOrder(aRequest.DataID);
        ibCancelPnL               : FIABClient.CancelPnL(aRequest.DataID);
        ibCancelTickByTickData    : TIABClient(FIABClient).CancelTickByTickData(aRequest.DataID);
        ibGetAccountUpdates       : TIABClient(FIABClient).GetAccountUpdates(aRequest.Details);
        ibGetCurrentTWSTime       : FIABClient.GetCurrentTWSTime;
        ibGetImpliedVolatility    : FIABClient.GetImpliedVolatility(aRequest.DataID, aRequest.Order, aRequest.Option, aRequest.Strike);
        ibGetInstrumentSpecs      : FIABClient.GetInstrumentSpecs(aRequest.DataID, aRequest.Order, aRequest.Order.Expiry.IsEmpty);
        ibGetMarketData           : FIABClient.GetMarketData(aRequest.DataID, aRequest.Order, aRequest.ExMarketData);
        ibGetMarketDataType       : FIABClient.RequestMarketDataType(aRequest.DataType);
        ibGetTickByTickData       : FIABClient.GetTickByTickData(aRequest.DataID, TIABClient(FIABClient).GetContractFromOrder(aRequest.Order), aRequest.TickDataType, Trunc(aRequest.Option), False);
        ibRequestMarketRule       : FIABClient.RequestMarketRule(aRequest.DataID);
        ibRequestMatchingSymbols  : FIABClient.RequestMatchingSymbols(aRequest.DataID, aRequest.Details);
        ibRequestPnL              : FIABClient.RequestPnL(aRequest.DataID, aRequest.Details, '');
        ibRequestSecDefOptParams  : FIABClient.RequestSecDefOptParams(aRequest.DataID, aRequest.Symbol, aRequest.Exchange, aRequest.SecurityType, aRequest.ContractId);
        ibGetHistoricalData:
          begin
            if aRequest.HistoricalDataParams.KeepUpdated then
              DataEndDateTime := ''
            else
              DataEndDateTime := DateTimeToIABDateTimeStr(Today + StrToTime('18:30:00'));
            FIABClient.GetHistoricalData(aRequest.DataID,
                                         aRequest.Order.Symbol,
                                         '',//aRequest.Order.LocalSymbol,
                                         aRequest.Order.Exchange,
                                         aRequest.Order.Expiry,
                                         aRequest.Order.Currency,
                                         aRequest.Order.PrimaryExchange,
                                         aRequest.Order.Multiplier,
                                         aRequest.Order.SecurityType,
                                         aRequest.Order.Right,
                                         aRequest.Order.Strike,
                                         DataEndDateTime,                                   //DataEndDateTime
                                         aRequest.HistoricalDataParams.DataDuration,        //DataDuration
                                         aRequest.HistoricalDataParams.DurationTimeUnits,   //DurationTimeUnits
                                         aRequest.HistoricalDataParams.BarSize,             //BarSize
                                         aRequest.HistoricalDataParams.DataBasis,           //DataBasis
                                         True,                                              //ExtendedHours
                                         False,                                             //IncludeExpired
                                         aRequest.HistoricalDataParams.KeepUpdated,         //KeepUpdated
                                         1,                                                 //DateFormat
                                         0,                                                 //ContractId
                                         '',                                                //TradingClass
                                         nil                                                //ChartOptions
                                         );
          end;
      end;
      Info := 'QueueCount=' + FRequestsQueue.Count.ToString + ', Command=' + IABCommandToString[aRequest.Command];
      if (aRequest.DataID > 0) then
        Info := Concat(Info, ', DataId=', aRequest.DataID.ToString);
      if Assigned(aRequest.Order) then
      begin
        if (aRequest.Order.ContractId > 0) then
          Info := Concat(Info, ', ContractId=', aRequest.Order.ContractId.ToString);
        if (not aRequest.Order.Symbol.IsEmpty) then
          Info := Concat(Info, ', Symbol=', aRequest.Order.Symbol);
      end
      else
      begin
        if (aRequest.ContractId > 0) then
          Info := Concat(Info, ', ContractId=', aRequest.ContractId.ToString);
        if (not aRequest.Symbol.IsEmpty) then
          Info := Concat(Info, ', Symbol=', aRequest.Symbol);
      end;
      TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'ExecuteRequest', Info);
      if Assigned(TIABClient(FIABClient).OnQueueOverflow) then
        TIABClient(FIABClient).OnQueueOverflow(Self, FRequestsQueue.Count);
    end;
  finally
    aRequest.Clear; //Order free
  end;
end;

end.
