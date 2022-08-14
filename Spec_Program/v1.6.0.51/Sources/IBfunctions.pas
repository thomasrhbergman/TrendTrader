 unit IBfunctions;
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Forms, SysUtils, Controls, StdCtrls, Dialogs, ComCtrls, twsapi, c_network, Utils,
  AccountUpdateUnit, FinancialAdvisorUnit, BrokerHelperAbstr, System.Classes, System.UITypes,
  System.AnsiStrings, DebugWriter, CodeSiteLogging;

const
  MKT_DEPTH_DATA_RESET = 317;

  OPERATION_INSERT = 0;
  OPERATION_UPDATE = 1;
  OPERATION_DELETE = 2;

  SIDE_ASK = 0;
  SIDE_BID = 1;

  IDTYPE_ORDER       = 0;
  IDTYPE_MARKET      = 1;
  IDTYPE_HISTORICAL  = 2;
  IDTYPE_POSITIONS   = 3;
  IDTYPE_ORDERSTATUS = 4;
  IDTYPE_OPENORDERS  = 5;
  IDTYPE_EXECUTIONS  = 6;

type
  TDisplayGroupList = procedure(reqId : Integer; const groups : string) of object;
  TDisplayGroupUpdated = procedure(reqId : Integer; const contractInfo : string) of object;
  TOnConnectNotify = procedure (Sender: TObject) of object;

  TClient = class (TTWSAPIClientBase)
  private
    FFinancialAdvisor: Boolean;
    FManagedAccounts: string;
    FOnConnectNotify: TOnConnectNotify;

    FfaGroupsXML: string;
    FfaProfilesXML: string;
    FfaAliasesXML: string;
    FfaError: Boolean;
    FClientId : Integer;

    FTicksList: TListBox;
    FOrderStatusList: TListBox;
    FErrorsList: TListBox;
    FBidList: TListView;
    FAskList: TListView;

    ID_Market_nr,
    ID_Historical_nr,
    ID_Positions_nr,
    ID_Orderstatus_nr,
    ID_Openorders_nr,
    ID_Execusions_nr : Integer;

    FMktDepthActive: boolean;

    FAccountUpdateForm : TAccountUpdateForm;

    FOnDisplayGroupList: TDisplayGroupList;
    FOnDisplayGroupUpdated: TDisplayGroupUpdated;
    FOpenFeed: TChangeFeedStatus;
    FCloseFeed: TChangeFeedStatus;

    procedure Print(const prop, value: string); overload;
    procedure Print(const prop: string; value: Double); overload;
    procedure Print(const prop: string; value: Integer); overload;

    procedure DisplayMultiline(list: TListBox; const s1, s2: string);

    procedure AddEntry(pList : TListView; position : Integer; const marketMaker : string; price : Double; size : Integer);
    procedure UpdateEntry(pList : TListView; position : Integer; const marketMaker : string; price : Double; size : Integer);
    procedure DeleteEntry(pList : TListView; position : Integer);
    procedure UpdateList(pList : TListView; position : Integer);

    procedure MktDepthFormUpdate(id, position : Integer; const marketMaker : string; operation, side : Integer; price : Double; size : Integer);
	  procedure MktDepthFormClear;
  protected
    function send(data : Pointer; sz : Integer) : Integer; override;
    function receive(data : Pointer; sz : Integer) : Integer; override;
    function isSocketOK() : Boolean; override;

    procedure accountDownloadEnd(const accountName : AnsiString); override;
    procedure accountSummary(reqId : Integer; const account, tag, value, curency : AnsiString); override;
    procedure accountSummaryEnd(reqId : Integer); override;
    procedure bondContractDetails(reqId : Integer; contractDetails : TContractDetails); override;
    procedure commissionReport(const commissionReport : TCommissionReport); override;
    procedure connectionClosed(); override;
    procedure contractDetails(reqId : Integer; contractDetails : TContractDetails); override;
    procedure contractDetailsEnd(reqId : Integer); override;
    procedure currentTime(time : Integer); override;
    procedure deltaNeutralValidation(reqId : Integer; underComp : TUnderComp); override;
    procedure displayGroupList(reqId : Integer; const groups : AnsiString); override;
    procedure displayGroupUpdated(reqId : Integer; const contractInfo : AnsiString); override;
    procedure error(id, errorCode : Integer; const errorString : AnsiString); override;
    procedure error_(const e : string);
    procedure execDetails(reqId : Integer; contract : TContract; const execution : TExecution); override;
    procedure execDetailsEnd(reqId : Integer); override;
    procedure fundamentalData(reqId : TTickerId; const data : AnsiString); override;
    procedure historicalData(reqId: TTickerId; const date: AnsiString; open, high, low, close: Double; volume, barCount: Integer; WAP: Double; hasGaps: Integer); override;
    procedure managedAccounts(const accountsList : AnsiString); override;
    procedure marketDataType(reqId : TTickerId; marketDataType : Integer); override;
    procedure openOrder(orderId : TOrderId; contract : TContract; order : TOrder; const orderstate : TOrderState); override;
    procedure openOrderEnd(); override;
    procedure orderStatus(orderId : TOrderId; const status : AnsiString; filled, remaining : Integer; avgFillPrice : Double; permId, parentId : Integer; lastFillPrice : Double; clientId : Integer; const whyHeld : AnsiString); override;
    procedure position(const account: AnsiString; contract: TContract; position: Integer; avgCost: Double); override;
    procedure positionEnd(); override;
    procedure realtimeBar(reqId : TTickerId; time : Integer; open, high, low, close : Double;  volume : Integer; wap : Double; count : Integer); override;
    procedure receiveFA(pFaDataType : TDataType; const cxml : AnsiString); override;
    procedure scannerData(reqId, rank : Integer; contractDetails : TContractDetails; const distance, benchmark, projection, legsStr : AnsiString); override;
    procedure scannerDataEnd(reqId : Integer); override;
    procedure scannerParameters(const xml : AnsiString); override;
    procedure tickEFP(tickerId : TTickerId; tickType : TTickType; basisPoints : Double; const formattedBasisPoints : AnsiString; totalDividends : Double; holdDays : Integer; const futureExpiry : AnsiString; dividendImpact, dividendsToExpiry : Double); override;
    procedure tickGeneric(tickerId : TTickerId; tickType : TTickType; value : Double); override;
    procedure tickOptionComputation(tickerId : TTickerId; tickType : TTickType; impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice : Double); override;
    procedure tickPrice(tickerId : TTickerId; field : TTickType; price : Double; canAutoExecute : Integer); override;
    procedure tickSize(tickerId : TTickerId; field : TTickType; size : Integer); override;
    procedure tickSnapshotEnd(reqId : Integer); override;
    procedure tickString(tickerId : TTickerId; tickType : TTickType; const value : AnsiString); override;
    procedure updateAccountTime(const timeStamp : AnsiString); override;
    procedure updateAccountValue(const key, val, currency, accountName : AnsiString); override;
    procedure updateMktDepth(id : TTickerId; position, operation, side : Integer; price : Double; size : Integer); override;
    procedure updateMktDepthL2(id : TTickerId; position : Integer; const marketMaker : AnsiString; operation, side : Integer; price : Double; size : Integer); override;
    procedure updateNewsBulletin(msgId, msgType : Integer; const newsMessage, originExch : AnsiString); override;
    procedure updatePortfolio(contract : TContract; position : Integer;  marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL : Double; const accountName : AnsiString); override;
    procedure verifyCompleted(isSuccessful : LongBool; const errorText : AnsiString); override;
    procedure verifyMessageAPI(const apiData : AnsiString); override;
    procedure winError(const str : AnsiString; lastError : Integer); override;
    procedure reqIds(numIds : Integer);
  public
    Socket : TSocketTCPIPClient;
    NextOrderIdByIB : TOrderId;  //variabel för att hämta orderId

    constructor Create(ATicksList, AOrderStatusList, AErrorsList: TListBox; ABidList, AAskList: TListView);
    destructor Destroy; override;

    function Connect(const host : AnsiString; port : Cardinal; clientId : Integer; extraAuth : Boolean) : Boolean; override;
    function IsConnected: Boolean;
    procedure Disconnect(); override;

    procedure FaClear;

    function sendBufferData() : Integer; override;
    procedure reqExecutions(const filter : TExecutionFilter);

    procedure reqPositions();
    procedure cancelPositions();

    procedure nextValidId(orderId : TOrderId); override;
    function getNextOrderId(idtype:Integer):TorderId;

    procedure AccountDownloadBegin(const AAccountCode : string);
    function AccountUpdateShowModal: integer;

    property ClientId: Integer read FClientId write FClientId;
    property AccountsList: string read FManagedAccounts;
    property MktDepthActive: boolean read FMktDepthActive write FMktDepthActive;

    property OnDisplayGroupList: TDisplayGroupList read FOnDisplayGroupList write FOnDisplayGroupList;
    property OnDisplayGroupUpdated: TDisplayGroupUpdated read FOnDisplayGroupUpdated write FOnDisplayGroupUpdated;
    property OnConnectNotify: TOnConnectNotify read FOnConnectNotify write FOnConnectNotify;
    property OnOpenFeed: TChangeFeedStatus read FOpenFeed write FOpenFeed;
    property OnCloseFeed: TChangeFeedStatus read FCloseFeed write FCloseFeed;
  end;

var
  IBClient: TClient;

implementation

uses
  Monitor;

function getField(tickType : TTickType) : string;
begin
  case tickType of
    BID_SIZE:                  Result := 'bidSize';
    BID:                       Result := 'bidPrice';
    ASK:                       Result := 'askPrice';
    ASK_SIZE:                  Result := 'askSize';
    LAST:                      Result := 'lastPrice';
    LAST_SIZE:                 Result := 'lastSize';
    HIGH:                      Result := 'high';
    LOW:                       Result := 'low';
    VOLUME:                    Result := 'volume';
    CLOSE:                     Result := 'close';
    BID_OPTION_COMPUTATION:    Result := 'bidOptComp';
    ASK_OPTION_COMPUTATION:    Result := 'askOptComp';
    LAST_OPTION_COMPUTATION:   Result := 'lastOptComp';
    MODEL_OPTION:              Result := 'optionModel';
    OPEN:                      Result := 'open';
    LOW_13_WEEK:               Result := '13WeekLow';
    HIGH_13_WEEK:              Result := '13WeekHigh';
    LOW_26_WEEK:               Result := '26WeekLow';
    HIGH_26_WEEK:              Result := '26WeekHigh';
    LOW_52_WEEK:               Result := '52WeekLow';
    HIGH_52_WEEK:              Result := '52WeekHigh';
    AVG_VOLUME:                Result := 'AvgVolume';
    OPEN_INTEREST:             Result := 'OpenInterest';
    OPTION_HISTORICAL_VOL:     Result := 'OptionHistoricalVolatility';
    OPTION_IMPLIED_VOL:        Result := 'OptionImpliedVolatility';
    OPTION_BID_EXCH:           Result := 'OptionBidExchStr';
    OPTION_ASK_EXCH:           Result := 'OptionAskExchStr';
    OPTION_CALL_OPEN_INTEREST: Result := 'OptionCallOpenInterest';
    OPTION_PUT_OPEN_INTEREST:  Result := 'OptionPutOpenInterest';
    OPTION_CALL_VOLUME:        Result := 'OptionCallVolume';
    OPTION_PUT_VOLUME:         Result := 'OptionPutVolume';
    INDEX_FUTURE_PREMIUM:      Result := 'IndexFuturePremium';
    BID_EXCH:                  Result := 'bidExch';
    ASK_EXCH:                  Result := 'askExch';
    AUCTION_VOLUME:            Result := 'auctionVolume';
    AUCTION_PRICE:             Result := 'auctionPrice';
    AUCTION_IMBALANCE:         Result := 'auctionImbalance';
    MARK_PRICE:                Result := 'markPrice';
    BID_EFP_COMPUTATION:       Result := 'bidEFP';
    ASK_EFP_COMPUTATION:       Result := 'askEFP';
    LAST_EFP_COMPUTATION:      Result := 'lastEFP';
    OPEN_EFP_COMPUTATION:      Result := 'openEFP';
    HIGH_EFP_COMPUTATION:      Result := 'highEFP';
    LOW_EFP_COMPUTATION:       Result := 'lowEFP';
    CLOSE_EFP_COMPUTATION:     Result := 'closeEFP';
    LAST_TIMESTAMP:            Result := 'lastTimestamp';
    SHORTABLE:                 Result := 'shortable';
    FUNDAMENTAL_RATIOS:        Result := 'fundamentals';
    RT_VOLUME:                 Result := 'RTVolume';
    HALTED:                    Result := 'halted';
    BID_YIELD:                 Result := 'bidYield';
    ASK_YIELD:                 Result := 'askYield';
    LAST_YIELD:                Result := 'lastYield';
    CUST_OPTION_COMPUTATION:   Result := 'custOptComp';
    TRADE_COUNT:               Result := 'trades';
    TRADE_RATE:                Result := 'trades/min';
    VOLUME_RATE:               Result := 'volume/min';
    LAST_RTH_TRADE:            Result := 'lastRTHTrade'
    else
      Result := 'unknown'
  end
end;

procedure ListBoxToBottom(lb : TListBox);
begin
  lb.TopIndex := lb.Items.Count - 1;
end;

{==  TClient  ==}

constructor TClient.Create(ATicksList, AOrderStatusList, AErrorsList: TListBox; ABidList, AAskList: TListView);
begin
  inherited Create();
  FOnDisplayGroupList := nil;
  FOnDisplayGroupUpdated := nil;
  FOnConnectNotify := nil;
  FOpenFeed := nil;
  FCloseFeed := nil;

  FAccountUpdateForm := TAccountUpdateForm.Create(nil);

  FTicksList := ATicksList;
  FOrderStatusList := AOrderStatusList;
  FErrorsList := AErrorsList;
  FBidList := ABidList;
  FAskList := AAskList;

  // Intiera startnummer för de olika ID_serierna
  ID_Market_nr     :=1001;
  ID_Historical_nr :=2001;
  ID_Positions_nr  :=3001;
  ID_Orderstatus_nr:=4001;
  ID_Openorders_nr :=5001;
  ID_Execusions_nr :=6001;
end;

destructor TClient.Destroy;
begin
  FAccountUpdateForm.Free;
  FOpenFeed := nil;
  FCloseFeed := nil;
  inherited;
end;

procedure TClient.Print(const prop : string; value : Integer);
var
  s : string;
begin
  if value = MaxInt then
    s := Format('  %s=', [prop])
  else
    s := Format('  %s=%d', [prop, value]);

  FOrderStatusList.Items.Add(s)
end;

procedure TClient.Print(const prop : string; value : Double);
var
  s : string;
begin
  if value >= UNSET_DOUBLE then
    s := Format('  %s=', [prop])
  else
    s := Format('  %s=%f', [prop, value]);

  FOrderStatusList.Items.Add(s)
end;

procedure TClient.Print(const prop, value : string);
begin
  FOrderStatusList.Items.Add(Format('  %s=%s', [prop, value]))
end;

procedure TClient.DisplayMultiline(list: TListBox; const s1, s2: string);
begin
  list.Items.Add(s1);
  list.Items.Text := s2
end;

procedure TClient.AddEntry(pList : TListView; position : Integer;
  const marketMaker : string; price : Double; size : Integer);
begin
  with pList.Items.Insert(position) do
  begin
    Caption := marketMaker;
    SubItems.Add(FloatToStr(price));
    SubItems.Add(IntToStr(size));
    SubItems.Add('0');
    SubItems.Add('')
  end;

  UpdateList(pList, position)
end;

procedure TClient.UpdateEntry(pList : TListView; position : Integer;
  const marketMaker : string; price : Double; size : Integer);
begin
  with pList.Items[position] do
  begin
    Caption := marketMaker;
    SubItems[0] := FloatToStr(price);
    SubItems[1] := IntToStr(size)
  end;

  UpdateList(pList, position)
end;

procedure TClient.DeleteEntry(pList : TListView; position : Integer);
begin
  pList.Items.Delete(position);
  UpdateList(pList, position)
end;

procedure TClient.UpdateList(pList : TListView; position : Integer);
var
  cumSize    : Integer;
  totalPrice : Double;
  i          : Integer;
  strSize    : string;
  strPrice   : string;
  atoiStrSize : Integer;
begin
  cumSize    := 0;
	totalPrice := 0.0;

	if (position > 0) and (position < pList.Items.Count) then
  begin
		cumSize := StrToInt(pList.Items[position - 1].SubItems[2]);
		totalPrice := StrToFloatEx(pList.Items[position - 1].SubItems[0]) * cumSize
	end;

	for i := position to pList.Items.Count - 1 do
	begin
		strPrice := pList.Items[i].SubItems[0];
		strSize  := pList.Items[i].SubItems[1];
	  atoiStrSize := StrToInt(strSize);
		cumSize := cumSize + atoiStrSize;
		totalPrice := totalPrice + StrToFloat(strPrice) * atoiStrSize;

    pList.Items[i].SubItems[2] := IntToStr(cumSize);
    pList.Items[i].SubItems[3] := FloatToStr(totalPrice / cumSize)
	end
end;

procedure TClient.MktDepthFormUpdate(id, position : Integer; const marketMaker : string;
  operation, side : Integer; price : Double; size : Integer);
var
  l: TListView;
begin
  if not FMktDepthActive then
		Exit;

  if side = SIDE_BID then
    l := FBidList
  else
    l := FAskList;

	case operation of
    OPERATION_INSERT : AddEntry(l, position, marketMaker, price, size);
    OPERATION_UPDATE : UpdateEntry(l, position, marketMaker, price, size);
	  OPERATION_DELETE : DeleteEntry(l, position)
  end
end;

procedure TClient.MktDepthFormClear;
begin
  FBidList.Clear;
  FAskList.Clear;
end;

function TClient.Connect(const host : AnsiString; port : Cardinal; clientId : Integer; extraAuth : Boolean) : Boolean;
var
  h : AnsiString;
begin
  if isConnected() then
  begin
    error(NO_VALID_ID, 501, 'Already connected');
    Result := False;
    Exit
  end;

  Disconnect();
  Socket := TSocketTCPIPClient.Create();

  h := host;
  if Length(h) = 0 then
    h := '127.0.0.1';

  if not Socket.Connect(h, port) then
  begin
    error(NO_VALID_ID, 521, 'Couldn''t connect to TWS.');
    Result := False;
    Exit
  end;

  FClientId := clientId;
  setClientId(clientId);
	setExtraAuth(extraAuth);

  baseConnect();
  onConnect();

  Result := True;
end;

function TClient.IsConnected: Boolean;
begin
  Result := Inherited IsConnected;
end;

procedure TClient.Disconnect;
begin
  if Socket <> nil then
  begin
    baseDisconnect();
    Socket.Close();
    Socket := nil
  end;
  if Assigned(OnCloseFeed) then
    OnCloseFeed();
end;

procedure TClient.FaClear;
begin
  FfaGroupsXML := '';
  FfaProfilesXML := '';
  FfaAliasesXML := '';
  FfaError := False;
end;

function TClient.sendBufferData() : Integer;
begin
  Result := inherited;
end;

procedure TClient.reqExecutions(const filter : TExecutionFilter);
begin
  inherited reqExecutions(NextOrderIdByIB - 1, filter);
end;

procedure TClient.reqIds(numIds: Integer);
begin
  inherited reqIds(numIds);
end;

function TClient.send;
begin
  Result := Socket.Send(data, sz)
end;

function TClient.receive;
begin
  Result := Socket.Receive(data, sz)
end;

function TClient.isSocketOK;
begin
  Result := Socket.Active;
end;

procedure TClient.tickPrice;
begin
//  F_Monitor.UpdateFromIB(tickerid,field,price);

  FTicksList.Items.Add(Format('id=%d  %s=%f', [tickerId, getField(field), price]));
  if Assigned(OnOpenFeed) then
    OnOpenFeed();
end;

procedure TClient.tickSize;
begin
//  F_Monitor.UpdateFromIB(tickerid,field,size);
end;

procedure TClient.tickOptionComputation;
var
  impliedVolStr, deltaStr, gammaStr, vegaStr, thetaStr,
  optPriceStr, pvDividendStr, undPriceStr : string;
begin
  impliedVolStr := 'N/A';
  deltaStr := 'N/A';
  gammaStr := 'N/A';
  vegaStr := 'N/A';
  thetaStr := 'N/A';
  optPriceStr := 'N/A';
  pvDividendStr := 'N/A';
  undPriceStr := 'N/A';

	if impliedVol < UNSET_DOUBLE then
		impliedVolStr := FloatToStr(impliedVol);

	if delta < UNSET_DOUBLE then
		deltaStr := FloatToStr(delta);

	if gamma < UNSET_DOUBLE then
		gammaStr := FloatToStr(gamma);

	if vega < UNSET_DOUBLE then
		vegaStr := FloatToStr(vega);

	if theta < UNSET_DOUBLE then
		thetaStr := FloatToStr(theta);

	if optPrice < UNSET_DOUBLE then
		optPriceStr := FloatToStr(optPrice);

	if pvDividend < UNSET_DOUBLE then
		pvDividendStr := FloatToStr(pvDividend);

	if undPrice < UNSET_DOUBLE then
    undPriceStr := FloatToStr(undPrice);
end;

procedure TClient.tickGeneric;
begin
//  F_Monitor.UpdateFromIB(tickerid,ticktype,value);

  FTicksList.Items.Add(Format('id=%d  %s=%g',[tickerId, getField(tickType), value]));
  ListBoxToBottom(FTicksList);
end;

procedure TClient.tickString;
begin
  FTicksList.Items.Add(Format('id=%d  %s=%s',[tickerId, getField(tickType), value]));
  ListBoxToBottom(FTicksList);

  if tickType=LAST_TIMESTAMP then
    F_Monitor.LastTimeStamp:=AnsiToString(value);
end;

procedure TClient.tickEFP;
begin
  FTicksList.Items.Add(Format(
    'id=%d  %s: basisPoints=%f / %s impliedFuture=%f holdDays=%d futureExpiry=%s dividendImpact=%f dividendsToExpiry=%f',
		[tickerId, getField(tickType), basisPoints, formattedBasisPoints, totalDividends, holdDays, futureExpiry, dividendImpact, dividendsToExpiry]
  ));
  ListBoxToBottom(FTicksList);
end;

// Responce on ReqOpenOrder
procedure TClient.orderStatus(orderId : TOrderId; const status : AnsiString; filled, remaining : Integer; avgFillPrice : Double; permId, parentId : Integer; lastFillPrice : Double; clientId : Integer; const whyHeld : AnsiString);
var
  sInfo: string;
begin
  FOrderStatusList.Items.Add('---- Order Status ----');
  Print('orderId', orderId);
  Print('status', AnsiToString(status));
  Print('filled', filled);
  Print('remaining', remaining);
  Print('avgFillPrice', avgFillPrice);
  Print('permId', permId);
  Print('parentId', parentId);
  Print('clientId', clientId);
  Print('lastFillPrice', lastFillPrice);
  Print('whyHeld', AnsiToString(whyHeld));

	FOrderStatusList.Items.Add(' ---- Order Status End ----');
  ListBoxToBottom(FOrderStatusList);
  sInfo := Format(
    'orderId: %d, status: %s, filled: %d, remaining: %d, avgFillPrice: %f, permId: %d, parentId: %d, clientId: %d, lastFillPrice: %f, whyHeld: %s',
    [orderId, status, filled, remaining, avgFillPrice, permId, parentId, clientId, lastFillPrice, whyHeld]);

//  F_Monitor.OrderList.AddStatus(orderId,
//                                filled,
//                                remaining,
//                                sInfo,
//                                'orderStatus',
//                                AnsiToString(status),
//                                avgFillPrice);
  LogWriter.Write('orderStatus', sInfo.Replace(',', '<br>'));
end;


procedure TClient.openOrder(orderId : TOrderId; contract : TContract; order : TOrder; const orderstate : TOrderState);
var
  i: Integer;
  comboleg: TComboLeg;
  str: string;
  sInfo: string;
begin
  FOrderStatusList.Items.Add('Contract:');

  Print('conId'          , contract.conId);
  Print('symbol'         , AnsiToString(contract.symbol));
  Print('secType'        , AnsiToString(contract.secType));
  Print('expiry'         , AnsiToString(contract.expiry));
  Print('strike'         , contract.strike);
  Print('right'          , AnsiToString(contract.right));
  Print('multiplier'     , AnsiToString(contract.multiplier));
  Print('exchange'       , AnsiToString(contract.exchange));
  Print('primaryExchange', AnsiToString(contract.primaryExchange));
  Print('currency'       , AnsiToString(contract.currency));
  Print('localSymbol'    , AnsiToString(contract.localSymbol));
  Print('tradingClass'   , AnsiToString(contract.tradingClass));
  Print('comboLegsDescrip', AnsiToString(contract.ComboLegsDescrip));

  FOrderStatusList.Items.Add('  comboLegs={');
  if (contract.ComboLegs <> nil) and (contract.ComboLegs.Count > 0) then
  begin
    for i := 0 to contract.ComboLegs.Count - 1 do
    begin
			comboleg := contract.ComboLegs[i];
			str := '';

      if (order.OrderComboLegs <> nil) and (contract.ComboLegs.Count = order.OrderComboLegs.Count) then
				if Abs(order.OrderComboLegs[i].Price - UNSET_DOUBLE) > 0.1 then
					str := Format('price=%f', [order.OrderComboLegs[i].Price]);

      FOrderStatusList.Items.Add(Format(
        '    leg %d: conId=%d ratio=%d action=%s exchange=%s openClose=%d shortSaleSlot=%d designatedLocation=%s exemptCode=%d %s',
        [(i + 1), comboleg.ConId, comboleg.Ratio, comboleg.Action, comboleg.Exchange, comboleg.OpenClose,
				 comboleg.ShortSaleSlot, comboleg.DesignatedLocation, comboleg.ExemptCode, str]
      ))
    end
  end;
  FOrderStatusList.Items.Add('  }');

  if contract.underComp <> nil then
  begin
    Print('conId', contract.underComp.conId);
    Print('delta', contract.underComp.delta);
    Print('price', contract.underComp.price)
  end;

  FOrderStatusList.Items.Add('Order:');
  Print('orderId'       , order.orderId);
  Print('clientId'      , order.clientId);
  Print('permId'        , order.permId);
  Print('action'        , AnsiToString(order.Action));
  Print('totalQuantity' , order.totalQuantity);
  Print('orderType'     , AnsiToString(order.orderType));
  Print('lmtPrice'      , order.lmtPrice);
  Print('auxPrice'      , order.auxPrice);

  FOrderStatusList.Items.Add('Extended Attrs:');
  Print('tif'                           , AnsiToString(order.tif));
  Print('ocaGroup'                      , AnsiToString(order.ocaGroup));
  Print('ocaType'                       , order.ocaType);
  Print('orderRef'                      , AnsiToString(order.orderRef));
  Print('transmit'                      , Integer(order.transmit));
  Print('parentId'                      , order.parentId);
  Print('blockOrder'                    , Integer(order.blockOrder));
  Print('sweepToFill'                   , Integer(order.sweepToFill));
  Print('displaySize'                   , order.displaySize);
  Print('triggerMethod'                 , order.triggerMethod);
  Print('outsideRth'                    , Integer(order.outsideRth));
  Print('hidden'                        , Integer(order.hidden));
  Print('goodAfterTime'                 , AnsiToString(order.goodAfterTime));
  Print('goodTillDate'                  , AnsiToString(order.goodTillDate));
  Print('overridePercentageConstraints' , Integer(order.overridePercentageConstraints));
  Print('rule80A'                       , AnsiToString(order.rule80A));
  Print('allOrNone'                     , Integer(order.allOrNone));
  Print('minQty'                        , order.minQty);
  Print('percentOffset'                 , order.percentOffset);
  Print('trailStopPrice'                , order.trailStopPrice);
  Print('trailingPercent'               , order.trailingPercent);
  Print('whatIf'                        , Integer(order.whatIf));
  Print('notHeld'                       , Integer(order.notHeld));

  Print('faGroup'         , AnsiToString(order.faGroup));
  Print('faProfile'       , AnsiToString(order.faProfile));
  Print('faMethod'        , AnsiToString(order.faMethod));
  Print('faPercentage'    , AnsiToString(order.faPercentage));

  Print('account'         , AnsiToString(order.account));
  Print('settlingFirm'    , AnsiToString(order.settlingFirm));
  Print('clearingAccount' , AnsiToString(order.clearingAccount));
  Print('clearingIntent'  , AnsiToString(order.clearingIntent));

  Print('openClose'          , AnsiToString(order.OpenClose));
  Print('origin'             , Integer(order.origin));
  Print('shortSaleSlot'      , order.ShortSaleSlot);
  Print('designatedLocation' , AnsiToString(order.DesignatedLocation));
  Print('exemptCode'         , order.ExemptCode);

  Print('discretionaryAmt'   , order.discretionaryAmt);
  Print('eTradeOnly'         , Integer(order.eTradeOnly));
  Print('firmQuoteOnly'      , Integer(order.firmQuoteOnly));
  Print('nbboPriceCap'       , order.nbboPriceCap);
  Print('optOutSmartRouting' , Integer(order.optOutSmartRouting));

  Print('auctionStrategy', Integer(order.auctionStrategy));

  Print('startingPrice'                  , order.startingPrice);
  Print('stockRefPrice'                  , order.stockRefPrice);
  Print('delta'                          , order.delta);
  Print('stockRangeLower'                , order.stockRangeLower);
  Print('stockRangeUpper'                , order.stockRangeUpper);
  Print('volatility'                     , order.volatility);
  Print('volatilityType'                 , order.volatilityType);
  Print('continuousUpdate'               , Integer(order.continuousUpdate));
  Print('referencePriceType'             , order.referencePriceType);
  Print('deltaNeutralOrderType'          , AnsiToString(order.deltaNeutralOrderType));
  Print('deltaNeutralAuxPrice'           , order.deltaNeutralAuxPrice);
  Print('deltaNeutralConId'              , order.deltaNeutralConId);
  Print('deltaNeutralSettlingFirm'       , AnsiToString(order.deltaNeutralSettlingFirm));
  Print('deltaNeutralClearingAccount'    , AnsiToString(order.deltaNeutralClearingAccount));
  Print('deltaNeutralClearingIntent'     , AnsiToString(order.deltaNeutralClearingIntent));
  Print('deltaNeutralOpenClose'          , AnsiToString(order.deltaNeutralOpenClose));
  Print('deltaNeutralShortSale'          , Integer(order.deltaNeutralShortSale));
  Print('deltaNeutralShortSaleSlot'      , order.deltaNeutralShortSaleSlot);
  Print('deltaNeutralDesignatedLocation' , AnsiToString(order.deltaNeutralDesignatedLocation));

  Print('basisPoints'     , order.basisPoints);
  Print('basisPointsType' , order.basisPointsType);
  FOrderStatusList.Items.Add('  smartComboRoutingParams={');
  if order.SmartComboRoutingParams <> nil then
    for i := 0 to order.SmartComboRoutingParams.count - 1 do
      with order.SmartComboRoutingParams[i] do
        Print('  ' + AnsiToString(tag), AnsiToString(value));
  FOrderStatusList.Items.Add('  }');

  Print('scaleInitLevelSize'       , order.scaleInitLevelSize);
  Print('scaleSubsLevelSize'       , order.scaleSubsLevelSize);
  Print('scalePriceIncrement'      , order.scalePriceIncrement);
  Print('scalePriceAdjustValue'    , order.scalePriceAdjustValue);
  Print('scalePriceAdjustInterval' , order.scalePriceAdjustInterval);
  Print('scaleProfitOffset'        , order.scaleProfitOffset);
  Print('scaleAutoReset'           , Integer(order.scaleAutoReset));
  Print('scaleInitPosition'        , order.scaleInitPosition);
  Print('scaleInitFillQty'         , order.scaleInitFillQty);
  Print('scaleRandomPercent'       , Integer(order.scaleRandomPercent));

  Print('hedgeType', AnsiToString(order.hedgeType));
  Print('hedgeParam', AnsiToString(order.hedgeParam));

  if Length(order.AlgoStrategy) > 0 then
  begin
    FOrderStatusList.Items.Add('  algoParams={');
    sInfo := 'algoParams={ ';
    if order.AlgoParams <> nil then
      for i := 0 to order.AlgoParams.Count - 1 do
        with order.AlgoParams[i] do
        begin
          Print('  ' + AnsiToString(Tag), AnsiToString(Value));
          sInfo := sInfo + Format('%s=%s; ', [Tag, Value])
        end;
    FOrderStatusList.Items.Add('  }');
    sInfo := sInfo +  '} '
  end;

  FOrderStatusList.Items.Add('OrderState:');
  Print('status'             , AnsiToString(orderstate.status));
  Print('initMargin'         , AnsiToString(orderstate.initMargin));
  Print('maintMargin'        , AnsiToString(orderstate.maintMargin));
  Print('equityWithLoan'     , AnsiToString(orderstate.equityWithLoan));
  Print('commission'         , orderstate.commission);
  Print('minCommission'      , orderstate.minCommission);
  Print('maxCommission'      , orderstate.maxCommission);
  Print('commissionCurrency' , AnsiToString(orderstate.commissionCurrency));
  Print('warningText'        , AnsiToString(orderstate.warningText));

  ListBoxToBottom(FOrderStatusList);

  sInfo := Format(
    'orderId: %d, strike: %f, status: %s, warningText: %s, ',
    [orderId, contract.Strike, orderstate.status, orderstate.warningText]) + sInfo;
//  F_Monitor.OrderList.AddStatus(orderId,
//                                0, //filled
//                                0, //remaining
//                                sInfo,
//                                'openOrder',
//                                AnsiToString(orderstate.status),
//                                0 // avgFillPrice
//                                );
  LogWriter.Write('openOrder', sInfo.Replace(',', '<br>'));
  contract.UnderComp.Free();
  contract.Free();
  order.Free();
end;

procedure TClient.openOrderEnd;
begin
  FOrderStatusList.Items.Add(' =============== end ===============');
  ListBoxToBottom(FOrderStatusList);
end;

procedure TClient.winError;
var
  lpMsgBuf: array [0 .. 1023] of AnsiChar;
  fullMsg: string;
begin
  // get windows error msg text
	FormatMessageA(
		FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
		nil,
		GetLastError(),
		LANG_NEUTRAL + (SUBLANG_DEFAULT shl 10), // Default language
		lpMsgBuf,
		1024,
		nil
  );

	// format msg
	if System.AnsiStrings.StrLen(lpMsgBuf) > 0 then
		fullMsg := Format('%s - %s (%d)', [str, lpMsgBuf, lastError])
	else
		fullMsg := Format('%s (%d)', [str, lastError]);

	// display it
	error(NO_VALID_ID, SYSTEM_ERROR, StringToAnsi(fullMsg))
end;

procedure TClient.connectionClosed;
begin
  ShowMessage('Connection closed');
end;

procedure TClient.updateAccountValue;
begin
  FAccountUpdateForm.UpdateAccountValue(AnsiToString(key), AnsiToString(val), AnsiToString(currency), AnsiToString(accountName));
end;

procedure TClient.updatePortfolio(contract : TContract; position : Integer;  marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL : Double; const accountName : AnsiString);
var
  sInfo: string;
begin
  sInfo :=
    Format(
      'conId=%d, symbol=%s, secType=%s, expiry=%s, strike=%f, right=%s, multiplier=%s, ' +
      'primaryExchange=%s, currency=%s, localSymbol=%s, tradingClass=%s, position=%d, ' +
      'mktPrice=%f, mktValue=%f, avgCost=%f, unrealizedPNL=%f, realizedPNL=%f, account=%s',
      [
       contract.ConId, contract.Symbol, contract.SecType, contract.Expiry,
       contract.Strike, contract.Right, contract.Multiplier, contract.PrimaryExchange,
			 contract.Currency, contract.LocalSymbol, contract.TradingClass,
		   position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName
      ]
    );
  LogWriter.Write(Self, 'updatePortfolio', sInfo.Replace(',', sLineBreak));

  FAccountUpdateForm.UpdatePortfolio(contract, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, AnsiToString(accountName));
  contract.UnderComp.Free();
  contract.Free();
end;

procedure TClient.updateAccountTime;
begin
  FAccountUpdateForm.UpdateAccountTime(AnsiToString(timeStamp));
end;

procedure TClient.accountDownloadEnd;
begin
  FAccountUpdateForm.AccountDownloadEnd(AnsiToString(accountName));

  FOrderStatusList.Items.Add(Format('Account Download End: %s', [accountName]));
  ListBoxToBottom(FOrderStatusList)
end;

procedure TClient.nextValidId(orderId : TOrderId);
begin
  NextOrderIdByIB := orderId;
end;

function TClient.getNextOrderId(idtype: Integer): Integer;
begin
  Result := -1;
  case idtype of
    IDTYPE_ORDER:
      begin
        result := IBClient.NextOrderIdByIB;
        Inc(IBClient.NextOrderIdByIB);
      end;
    IDTYPE_MARKET:
      begin
        Result := ID_Market_nr;
        Inc(ID_Market_nr);
      end;
    IDTYPE_HISTORICAL:
      begin
        Result := ID_Historical_nr;
        Inc(ID_Historical_nr);
      end;
    IDTYPE_POSITIONS:
      begin
        Result := ID_Positions_nr;
        Inc(ID_Positions_nr);
      end;
    IDTYPE_ORDERSTATUS:
      begin
        Result := ID_Orderstatus_nr;
        Inc(ID_Orderstatus_nr);
      end;
    IDTYPE_OPENORDERS:
      begin
        Result := ID_Openorders_nr;
        Inc(ID_Openorders_nr);
      end;
    IDTYPE_EXECUTIONS:
      begin
        Result := ID_Openorders_nr;
        Inc(ID_Openorders_nr);
      end;
  end;
end;

procedure TClient.contractDetails;
  var
    i : Integer;

begin
  FOrderStatusList.Items.Add(Format(
    'id =%d ===================================',
		[reqId]
  ));
  FOrderStatusList.Items.Add(' ---- Contract Details Ex begin ----');

  FOrderStatusList.Items.Add('Contract:');
  Print('conId'          , contractDetails.Summary.conId);
  Print('symbol'         , AnsiToString(contractDetails.Summary.symbol));
  Print('secType'        , AnsiToString(contractDetails.Summary.secType));

  Print('expiry'         , AnsiToString(contractDetails.Summary.expiry));
  Print('strike'         , contractDetails.Summary.strike);
  Print('right'          , AnsiToString(contractDetails.Summary.right));
  Print('multiplier'     , AnsiToString(contractDetails.Summary.multiplier));
  Print('exchange'       , AnsiToString(contractDetails.Summary.exchange));
  Print('primaryExchange', AnsiToString(contractDetails.Summary.primaryExchange));
  Print('currency'       , AnsiToString(contractDetails.Summary.currency));
  Print('localSymbol'    , AnsiToString(contractDetails.Summary.localSymbol));
  Print('tradingClass'   , AnsiToString(contractDetails.Summary.tradingClass));

  FOrderStatusList.Items.Add('Details:');
  Print('marketName'     , AnsiToString(contractDetails.marketName));
  Print('minTick'        , contractDetails.minTick);
  Print('priceMagnifier' , contractDetails.priceMagnifier);
  Print('orderTypes'     , AnsiToString(contractDetails.orderTypes));
  Print('validExchanges' , AnsiToString(contractDetails.validExchanges));
  Print('underConId'     , contractDetails.underConId);
  Print('longName'       , AnsiToString(contractDetails.longName));

  if contractDetails.Summary.SecType <> 'BOND' then
  begin
    Print('contractMonth'   , AnsiToString(contractDetails.contractMonth));
    Print('industry'        , AnsiToString(contractDetails.industry));
    Print('category'        , AnsiToString(contractDetails.category));
    Print('subcategory'     , AnsiToString(contractDetails.subcategory));
    Print('timeZoneId'      , AnsiToString(contractDetails.timeZoneId));
    Print('tradingHours'    , AnsiToString(contractDetails.tradingHours));
    Print('liquidHours'     , AnsiToString(contractDetails.liquidHours));
  end;

  Print('evRule'      , AnsiToString(contractDetails.evRule));
  Print('evMultiplier', contractDetails.evMultiplier);

  if contractDetails.Summary.SecType = 'BOND' then
  begin
    Print('cusip'             , AnsiToString(contractDetails.cusip));
    Print('ratings'           , AnsiToString(contractDetails.ratings));
    Print('descAppend'        , AnsiToString(contractDetails.descAppend));
    Print('bondType'          , AnsiToString(contractDetails.bondType));
    Print('couponType'        , AnsiToString(contractDetails.couponType));
    Print('callable'          , Integer(contractDetails.callable));
    Print('putable'           , Integer(contractDetails.putable));
    Print('coupon'            , contractDetails.coupon);
    Print('convertible'       , Integer(contractDetails.convertible));
    Print('maturity'          , AnsiToString(contractDetails.maturity));
    Print('issueDate'         , AnsiToString(contractDetails.issueDate));
    Print('nextOptionDate'    , AnsiToString(contractDetails.nextOptionDate));
    Print('nextOptionType'    , AnsiToString(contractDetails.nextOptionType));
    Print('nextOptionPartial' , Integer(contractDetails.nextOptionPartial));
    Print('notes'             , AnsiToString(contractDetails.notes));
  end;

  FOrderStatusList.Items.Add('  secIdList={');
  if contractDetails.SecId <> nil then
    for i := 0 to contractDetails.SecId.Count - 1 do
      with contractDetails.SecId[i] do
        Print('  ' + AnsiToString(Tag), AnsiToString(Value));
  FOrderStatusList.Items.Add('  }');

  FOrderStatusList.Items.Add(' ---- Contract Details Ex end ------');
  ListBoxToBottom(FOrderStatusList);

  contractDetails.Free()
end;

procedure TClient.bondContractDetails;
begin
  Self.contractDetails(reqId, contractDetails)
end;

procedure TClient.contractDetailsEnd;
begin
  FOrderStatusList.Items.Add(Format('id =%d =============== end ===============', [reqId]));
  ListBoxToBottom(FOrderStatusList)
end;

procedure TClient.execDetails(reqId : Integer; contract : TContract; const execution : TExecution);
var
  sInfo: string;
begin
//  F_Monitor.Update_PositionTree(ReqId,execution.Time);
  FOrderStatusList.Items.Add('---- Execution Details begin ----');
  FOrderStatusList.Items.Add(Format(
    'execDetails: reqId=%d',
		[reqId]
  ));

  FOrderStatusList.Items.Add('Contract');
  Print('conId'          , contract.conId);
  Print('symbol'         , AnsiToString(contract.symbol));
  Print('secType'        , AnsiToString(contract.secType));
  Print('expiry'         , AnsiToString(contract.expiry));
  Print('strike'         , contract.strike);
  Print('right'          , AnsiToString(contract.right));
  Print('multiplier'     , AnsiToString(contract.multiplier));
  Print('exchange'       , AnsiToString(contract.exchange));
  Print('primaryExchange', AnsiToString(contract.primaryExchange));
  Print('currency'       , AnsiToString(contract.currency));
  Print('localSymbol'    , AnsiToString(contract.localSymbol));
  Print('tradingClass'   , AnsiToString(contract.tradingClass));

  FOrderStatusList.Items.Add('Execution');
  Print('execId'      , AnsiToString(execution.execId));
  Print('orderId'     , execution.orderId);
  Print('clientId'    , execution.clientId);
  Print('permId'      , execution.permId);
  Print('time'        , AnsiToString(execution.time));
  Print('acctNumber'  , AnsiToString(execution.acctNumber));
  Print('exchange'    , AnsiToString(execution.exchange));
  Print('side'        , AnsiToString(execution.side));
  Print('shares'      , execution.shares);
  Print('price'       , execution.price);
  Print('liquidation' , execution.liquidation);
  Print('cumQty'      , execution.cumQty);
  Print('avgPrice'    , execution.avgPrice);
  Print('orderRef'    , AnsiToString(execution.orderRef));
  Print('evRule'      , AnsiToString(execution.evRule));
  Print('evMultiplier', execution.evMultiplier);

  FOrderStatusList.Items.Add('---- Execution Details End ----');
  ListBoxToBottom(FOrderStatusList);
  sInfo := Format(
    'reqId: %d, conId: %d, symbol: %s, secType: %s, expiry: %s, strike: %s, right: %s, multiplier: %s, exchange: %s, ' +
    'primaryExchange: %s, currency: %s, localSymbol: %s, tradingClass: %s, execId: %s, clientId: %d, permId: %d, time: %s, ' +
    'acctNumber: %s, exchange: %s, side: %s, shares: %d, price: %f, liquidation: %d, cumQty: %d, avgPrice: %f, orderRef: %s, ' +
    'evRule: %s, evMultiplier: %f',
		[reqId, contract.conId, contract.symbol, contract.secType, contract.expiry, contract.strike.ToString, contract.right, contract.multiplier,
    contract.exchange, contract.primaryExchange, contract.currency, contract.localSymbol, contract.tradingClass, execution.execId, execution.clientId,
    execution.permId, execution.time, execution.acctNumber, execution.exchange, execution.side, execution.shares, execution.price, execution.liquidation,
    execution.cumQty, execution.avgPrice, execution.orderRef, execution.evRule, execution.evMultiplier]);

//  F_Monitor.OrderList.AddStatus(execution.orderId,
//                                0,
//                                0,
//                                sInfo,
//                                'execDetails',
//                                '',
//                                0);

  LogWriter.Write('execDetails', sInfo.Replace(',', '<br>'));

  contract.UnderComp.Free();
  contract.Free();
end;

procedure TClient.execDetailsEnd(reqId : Integer);
begin
  FOrderStatusList.Items.Add(Format('reqId=%d =============== end ===============', [reqId]));
//  F_Monitor.OrderList.AddStatus(reqId,
//                                0,
//                                0,
//                                reqId.ToString,
//                                'execDetailsEnd',
//                                '',
//                                0);
  ListBoxToBottom(FOrderStatusList)
end;


procedure TClient.error(id, errorCode : Integer; const errorString : AnsiString);
var
  errorStr : string;
begin
	errorStr := errorStr + IntToStr(id);
	errorStr := errorStr + ' | ';
	errorStr := errorStr + 'Error Code: ';
	errorStr := errorStr + IntToStr(errorCode);
	errorStr := errorStr + ' | ';
	errorStr := errorStr + 'Error Msg: ';
	errorStr := errorStr + AnsiToString(errorString);
	error_(errorStr);

  //	for n := 0 to NUM_FA_ERROR_CODES - 1 do
  //    MainForm.FfaError := MainForm.FfaError or (errorCode = faErrorCodes[n]);

//  F_Monitor.OrderList.AddStatus(id,
//                                0,
//                                0,
//                                errorStr,
//                                'error',
//                                'error',
//                                0);

	if errorCode = MKT_DEPTH_DATA_RESET then
		MktDepthFormClear;
end;

procedure TClient.error_;
begin
  FErrorsList.Items.Add(e);

  FErrorsList.TopIndex := FErrorsList.Items.Count;
  F_Monitor.Stop_Waiting;
  ListBoxToBottom(FErrorsList)
end;

procedure TClient.updateMktDepth;
begin
  MktDepthFormUpdate(id, position, '', operation, side, price, size)
end;

procedure TClient.updateMktDepthL2;
begin
  MktDepthFormUpdate(id, position, AnsiToString(marketMaker), operation, side, price, size)
end;

procedure TClient.updateNewsBulletin;
begin
  // "IB News Bulletin"
	MessageDlg(
    Format(
      'MsgId=%d :: MsgType = %d :: Origin= %s :: Message= %s',
  	  [msgId, msgType, originExch, newsMessage]
    ),
    mtInformationDialog, [mbOK], 0
  )
end;

procedure TClient.managedAccounts(const accountsList : AnsiString);
begin
  FOrderStatusList.Items.Add(Format(
    'Connected : The list of managed accounts are : [%s]',
		[accountsList]
  ));
  ListBoxToBottom(FOrderStatusList);

  FFinancialAdvisor := True;
  FManagedAccounts := AnsiToString(accountsList);
  if Assigned(FOnConnectNotify) and isConnected() then
    FOnConnectNotify(Self);
end;

function faDataTypeStr(pFaDataType : TDataType) : string;
begin
	case pFaDataType of
    GROUPS   : Result := 'GROUPS';
    PROFILES : Result := 'PROFILES';
    ALIASES  : Result := 'ALIASES'
  end
end;

procedure TClient.receiveFA;
var
  FinancialAdvisorForm : TFinancialAdvisorForm;
begin
  case pFaDataType of
    GROUPS   : FfaGroupsXML   := AnsiToString(cxml);
    PROFILES : FfaProfilesXML := AnsiToString(cxml);
    ALIASES  : FfaAliasesXML  := AnsiToString(cxml);
  end;
  DisplayMultiline(FOrderStatusList, 'FA ' + faDataTypeStr(pFaDataType) + ':', AnsiToString(cxml));
  ListBoxToBottom(FOrderStatusList);

  if not FfaError and (Length(FfaGroupsXML) > 0) and
    (Length(FfaProfilesXML) > 0) and (Length(FfaAliasesXML) > 0) then
  begin
    FinancialAdvisorForm := TFinancialAdvisorForm.Create(GetParentForm(FOrderStatusList));
    try
      FinancialAdvisorForm.receiveInitialXML(FfaGroupsXML, FfaProfilesXML, FfaAliasesXML);
      if FinancialAdvisorForm.ShowModal() = mrOk then
      begin
        FinancialAdvisorForm.extractXML(FfaGroupsXML, FfaProfilesXML, FfaAliasesXML);
        replaceFA(GROUPS, StringToAnsi(FfaGroupsXML));
        replaceFA(PROFILES, StringToAnsi(FfaProfilesXML));
        replaceFA(ALIASES, StringToAnsi(FfaAliasesXML));
      end;
    finally
      FinancialAdvisorForm.Free;
    end;
  end;
end;

procedure TClient.historicalData(reqId: TTickerId; const date: AnsiString; open, high, low, close: Double; volume, barCount: Integer; WAP: Double; hasGaps: Integer);
var
  FormatSettings: TFormatSettings;
  dTime: TDateTime;
  sDate: string;
begin
  dTime := 0;
  sDate := AnsiToString(date);
  if date[2] in ['0' .. '9'] then
  begin
    FormatSettings := TFormatSettings.Create(GetThreadLocale);
    formatSettings.ShortDateFormat:='YYYY-MM-DD';
    formatSettings.DateSeparator  :='-';
    formatSettings.LongTimeFormat :='hh:nn:ss';
    formatSettings.TimeSeparator  :=':';
    dTime := StrToDateTime(Copy(sDate, 1, 4) + '-' + Copy(sDate, 5, 2) + '-' + Copy(sDate, 7, 2) + ' ' + Copy(sDate, 11, 8), formatSettings);

//    F_Monitor.SavePrice(reqId, twsapi.open, open, dTime, False);
//    F_Monitor.SavePrice(reqId, twsapi.HIGH, high, dTime, False);
//    F_Monitor.SavePrice(reqId, twsapi.LOW, low, dTime, False);
//    F_Monitor.SavePrice(reqId, twsapi.close, close, dTime, False);
//    F_Monitor.SavePrice(reqId, twsapi.volume, volume, dTime, False);
//    F_Monitor.SavePrice(reqId, twsapi.LAST, close, dTime, False);
  end
  else
  begin
//    F_Monitor.SavePrice(reqId, twsapi.open, open, Now, True);
//    F_Monitor.SavePrice(reqId, twsapi.HIGH, high, Now, True);
//    F_Monitor.SavePrice(reqId, twsapi.LOW, low, Now, True);
//    F_Monitor.SavePrice(reqId, twsapi.close, close, Now, True);
//    F_Monitor.SavePrice(reqId, twsapi.volume, volume, Now, True);
//    F_Monitor.SavePrice(reqId, twsapi.LAST, open, dTime, True);
  end;

  FTicksList.Items.Add
    (Format('id=%d date=%s open=%f high=%f low=%f close=%f volume=%d barCount = %d WAP=%f hasGaps=%d',
    [reqId, date, open, high, low, close, volume, barCount, WAP, hasGaps]));
  ListBoxToBottom(FTicksList);
end;

procedure TClient.scannerParameters;
begin
  DisplayMultiline(FOrderStatusList, 'SCANNER PARAMETERS: ', AnsiToString(xml));
  ListBoxToBottom(FOrderStatusList);
end;

procedure TClient.scannerData(reqId, rank : Integer; contractDetails : TContractDetails; const distance, benchmark, projection, legsStr : AnsiString);
begin
  FTicksList.Items.Add(Format(
    'id =%d rank=%d conId=%d symbol=%s secType=%s expiry=%s strike=%f right=%s ' +
    'exchange=%s currency=%s localSymbol=%s marketName=%s tradingClass=%s ' +
    'distance=%s benchmark=%s projection=%s legsStr=%s',
		[reqId, rank,	contractDetails.Summary.ConId,contractDetails.Summary.Symbol,
     contractDetails.Summary.SecType, contractDetails.Summary.Expiry,
     contractDetails.Summary.Strike, contractDetails.Summary.Right,
     contractDetails.Summary.Exchange, contractDetails.Summary.Currency,
     contractDetails.Summary.LocalSymbol, contractDetails.MarketName,
     contractDetails.Summary.TradingClass, distance, benchmark, projection, legsStr]
  ));
  ListBoxToBottom(FTicksList);

  contractDetails.Free;
end;

procedure TClient.scannerDataEnd;
begin
  FTicksList.Items.Add(Format('id =%d =============== end ===============',	[reqId]));
  ListBoxToBottom(FTicksList);
end;

procedure TClient.realtimeBar;
begin
  FTicksList.Items.Add(Format(
    'id=%d time=%d open=%f high=%f low=%f close=%f volume=%d WAP=%f count = %d',
		[reqId, time, open, high, low, close, volume, WAP, count]
  ));
  ListBoxToBottom(FTicksList);
end;

procedure TClient.currentTime(time : Integer);
const
  SecsPerDay = 24 *60 *60;
var
  dt : TDateTime;

  function UnixTime(DateTime: TDateTime): longint;
  begin
    result := Trunc((DateTime - EncodeDate(1970, 1, 1)) * SecsPerDay);
  end;

  function UnixDateTimeToDelphiDateTime(UnixDateTime: longint): TDateTime;
  begin
    result := EncodeDate(1970, 1, 1) + (UnixDateTime / SecsPerDay);
  end;

begin
  dt := time;
  ReplaceTime(dt, UnixDateTimeToDelphiDateTime(time + 3600));
  //  MainForm.lOrderStatus.Items.Add(FormatDateTime('yymmdd:hhnnss',UnixDateTimeToDelphiDateTime(time+3600)));
  //  ListBoxToBottom(MainForm.lOrderStatus);
end;

procedure TClient.fundamentalData;
begin
 showmessage(AnsiToString(data));
 DisplayMultiline(
    FTicksList,
    Format('fund data id=%d, len=%d', [reqId, Length(data)]),
    AnsiToString(data)
  );
  ListBoxToBottom(FTicksList);
end;

procedure TClient.deltaNeutralValidation;
begin
  FOrderStatusList.Items.Add(Format(
    'deltaNeutralValidation: reqId=%d',
		[reqId]
  ));

  Print('conId', underComp.ConId);
  Print('delta', underComp.Delta);
  Print('price', underComp.Price);

  ListBoxToBottom(FOrderStatusList);

  underComp.Free;
end;

procedure TClient.tickSnapshotEnd;
begin
  FTicksList.Items.Add(Format('id=%d =============== end ===============', [reqId]));
  ListBoxToBottom(FTicksList);
end;

procedure TClient.marketDataType;
var
  str : string;
begin
	case marketDataType of
		{REALTIME} 1 : str := Format('id=%d marketDataType=Real-Time', [reqId]);
		{FROZEN}   2 : str := Format('id=%d marketDataType=Frozen', [reqId])
		else
			str := Format('id=%d marketDataType=Unknown', [reqId])
	end;

	FTicksList.Items.Add(str);
  ListBoxToBottom(FTicksList);
end;

procedure TClient.commissionReport(const commissionReport : TCommissionReport);
begin
  FOrderStatusList.Items.Add('---- Commission Report ----');
  Print('execId', AnsiToString(commissionReport.execId));
  Print('commission', commissionReport.commission);
  Print('currency', AnsiToString(commissionReport.currency));
  Print('realizedPNL', commissionReport.realizedPNL);
  Print('yield', commissionReport.yield);
  Print('yieldRedemptionDate', commissionReport.yieldRedemptionDate);

	FOrderStatusList.Items.Add(' ---- Commission Report End ----');
  ListBoxToBottom(FOrderStatusList);
  FOrderStatusList.items.SaveToFile('commission.txt');
end;

procedure TClient.reqPositions;
begin
  inherited;
  LogWriter.Write('reqPositions');
  FOrderStatusList.Items.Add(' ---- reqPositions ----');
  ListBoxToBottom(FOrderStatusList);
end;

procedure TClient.cancelPositions;
begin
  inherited;
  LogWriter.Write('cancelPositions');
  FOrderStatusList.Items.Add(' ---- cancelPositions ----');
  ListBoxToBottom(FOrderStatusList);
end;

procedure TClient.position(const account: AnsiString; contract: TContract; position: Integer; avgCost: Double);
var
  sInfo: string;
begin
  FOrderStatusList.Items.Add(' ---- Position begin ----');
  FOrderStatusList.Items.Add(Format('account=%s', [account]));

  FOrderStatusList.Items.Add('Contract:');

  Print('conId'          , contract.conId);
  Print('symbol'         , AnsiToString(contract.symbol));
  Print('secType'        , AnsiToString(contract.secType));
  Print('expiry'         , AnsiToString(contract.expiry));
  Print('strike'         , contract.strike);
  Print('right'          , AnsiToString(contract.right));
  Print('multiplier'     , AnsiToString(contract.multiplier));
  Print('exchange'       , AnsiToString(contract.exchange));
  Print('primaryExchange', AnsiToString(contract.primaryExchange));
  Print('currency'       , AnsiToString(contract.currency));
  Print('localSymbol'    , AnsiToString(contract.localSymbol));
  Print('tradingClass'   , AnsiToString(contract.tradingClass));

  FOrderStatusList.Items.Add(Format('position=%d', [position]));
  FOrderStatusList.Items.Add(Format('avgCost=%f', [avgCost]));

  sInfo := Format(
    'onId: %d, symbol: %s, secType: %s, expiry: %s, strike: %s, right: %s, multiplier: %s, exchange: %s, ' +
    'primaryExchange: %s, currency: %s, localSymbol: %s, tradingClass: %s',
		[contract.conId, contract.symbol, contract.secType, contract.expiry, contract.strike.ToString, contract.right, contract.multiplier,
    contract.exchange, contract.primaryExchange, contract.currency, contract.localSymbol, contract.tradingClass]);

  LogWriter.Write('position', sInfo.Replace(',', '<br>'));

  FOrderStatusList.Items.Add(' ---- Position End ----');
  ListBoxToBottom(FOrderStatusList);

  contract.UnderComp.Free;
  contract.Free;
end;

procedure TClient.positionEnd;
begin
  FOrderStatusList.Items.Add('=============== end ===============');
  ListBoxToBottom(FOrderStatusList);
end;

procedure TClient.accountSummary;
begin
  FOrderStatusList.Items.Add('---- Account Summary begin ----');

	FOrderStatusList.Items.Add(Format(
    'accountSummary: reqId=%d account=%s tag=%s value=%s currency=%s',
		[reqId, account, tag, value, curency]
  ));

	FOrderStatusList.Items.Add(' ---- Account Summary End ----');
  ListBoxToBottom(FOrderStatusList);
end;

procedure TClient.accountSummaryEnd;
begin
  FOrderStatusList.Items.Add(Format(
    'reqId=%d =============== end ===============',
		[reqId]
  ));
  ListBoxToBottom(FOrderStatusList)
end;

procedure TClient.verifyMessageAPI;
begin
  // empty
end;

procedure TClient.verifyCompleted;
begin
  // empty
end;

procedure TClient.displayGroupList;
begin
  if Assigned(FOnDisplayGroupList) then
    try
      FOnDisplayGroupList(reqId, AnsiToString(groups));
    except
      // supress user exception
    end;
end;

procedure TClient.displayGroupUpdated;
begin
  if Assigned(FOnDisplayGroupUpdated) then
    try
      FOnDisplayGroupUpdated(reqId, AnsiToString(contractInfo));
    except
      // supress user exception
    end;
end;

procedure TClient.AccountDownloadBegin(const AAccountCode : string);
begin
  FAccountUpdateForm.AccountDownloadBegin(AAccountCode);
end;

function TClient.AccountUpdateShowModal: integer;
begin
  Result := FAccountUpdateForm.ShowModal;
end;

end.
