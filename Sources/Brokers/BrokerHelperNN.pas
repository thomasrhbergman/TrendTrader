unit BrokerHelperNN;

interface

{$REGION 'Region uses'}
uses
  BrokerHelperAbstr, Classes, System.Generics.Collections, System.Generics.Defaults, Vcl.Graphics, System.Threading
  {$IFDEF USE_CODE_SITE}, CodeSiteLogging{$ENDIF}, Entity.Sokid, Common.Types, Global.Types, Publishers,

  IPPeerClient, InstrumentList, DebugWriter, HtmlLib, NNfunctions, NNfunctions.Types;
{$ENDREGION}


const
  URL = 'https://api.test.nordnet.se/next/2';
  PUBLIC_KEY_FILE = 'NEXTAPI_TEST_public.PEM';

type
  TNordNetBroker = class(TNordNet)
  private
    FAccountNum     : Integer;
    FOnUpdatePrice  : TUpdatePriceItem;
    FOnUpdateTrade  : TUpdateTradeItem;
    FOrderList      : TDictionary<Integer, TOrder>;
    FPriceCache     : TPriceCache;
    procedure DoErrorItem(const Sender: TNordNet; const FeedType: TFeedType; const ErrorItem: TErrorFeedItem);
    procedure DoJsonArrived(const Sender: TNordNet; const FeedType: TFeedType; const JSON: string);
    procedure DoLeave(const Sender: TNordNet);
    procedure DoLogon(const Sender: TNordNet);
    procedure DoPriceItem(const Sender: TNordNet; const FeedType: TFeedType; const PriceItem: TPriceFeedItem);
    procedure DoRequest(const Sender: TNordNet; const FeedType: TFeedType; const JSON: string);
    procedure DoRequestError(const Sender: TNordNet; const Code, Message: string);
    procedure DoTradeItem(const Sender: TNordNet; const FeedType: TFeedType; const TradeItem: TTradeFeedItem);
    procedure DoResponse(const Sender: TNordNet; const Path: string; const HttpCode: Integer; const HttpStatus, HttpBody: string);
    procedure DoCheckOrderList;
  public
    class function Logon(const UserName, Password: string): string;
    constructor Create(const BaseURL: string);
    destructor Destroy; override;

    property AccountNum     : Integer          read FAccountNum;
    property PriceCache     : TPriceCache      read FPriceCache     write FPriceCache;
    property OnUpdatePrice  : TUpdatePriceItem read FOnUpdatePrice  write FOnUpdatePrice;
    property OnUpdateTrade  : TUpdateTradeItem read FOnUpdateTrade  write FOnUpdateTrade;
  end;

  TBrokerHelperNN = class(TBrokerHelperAbstr)
  public
    class function Active: Boolean; override;
    class function GetFields(IsIN: Integer): TArray<TFieldType>; override;
    class function GetSokidBySymbol(AFilter: string): TArray<TSokidInfo>; override;
    class function GetSokidByName(AFilter: string): TArray<TSokidInfo>; override;
    class function HelperName: string; override;
    class function Login(const UserName, Password: string): string; override;
    class procedure Leave; override;
    class procedure SetCloseFeed(CloseFeed: TChangeFeedStatus); override;
    class procedure SetDate(const ADate: TDateTime); override;
    class procedure SetOpenFeed(OpenFeed: TChangeFeedStatus); override;
    class procedure SubscribeAllFeeds(InstrumentId: string; MarketId: Integer); override;
    class procedure SubscribeFeedPrice(InstrumentId: string; MarketId: Integer); override;
    class procedure SubscribeFeedTrade(InstrumentId: string; MarketId: Integer); override;
    class procedure UnsubscribeAllFeeds(InstrumentId: string; MarketId: Integer); override;
  end;

var
  NordNetBroker: TNordNetBroker;

implementation

uses
  System.SysUtils, BrokerHelperFactory;

{ TNordNetBroker }

constructor TNordNetBroker.Create(const BaseURL: string);
begin
  inherited;
  FOnUpdatePrice := nil;
  FOnUpdateTrade := nil;
  FAccountNum     := -1;

  OnErrorItem    := DoErrorItem;
  OnJsonArrived  := DoJsonArrived;
  OnLeave        := DoLeave;
  OnLogon        := DoLogon;
  OnPriceItem    := DoPriceItem;
  OnRequest      := DoRequest;
  OnRequestError := DoRequestError;
  OnTradeItem    := DoTradeItem;
  OnResponse     := DoResponse;

  FOrderList := TDictionary<Integer, TOrder>.Create;
end;

destructor TNordNetBroker.Destroy;
begin
  FOrderList.Clear;
  FreeAndNil(FOrderList);
  inherited;
end;

procedure TNordNetBroker.DoPriceItem(const Sender: TNordNet; const FeedType: TFeedType; const PriceItem: TPriceFeedItem);
var
  NNPriceItem: TPriceItem;
begin
  if Assigned(FOnUpdatePrice) then
  begin
    NNPriceItem.Identifier := PriceItem.i;
    NNPriceItem.MarketId := PriceItem.m;
    NNPriceItem.TradeTimestamp := PriceItem.trade_timestamp;
    NNPriceItem.TickTimestamp := PriceItem.tick_timestamp;
    NNPriceItem.Bid := PriceItem.bid;
    NNPriceItem.BidSize := PriceItem.bid_volume;
    NNPriceItem.Ask := PriceItem.ask;
    NNPriceItem.AskSize := PriceItem.ask_volume;
    NNPriceItem.Close := PriceItem.close;
    NNPriceItem.High := PriceItem.high;
    NNPriceItem.Last := PriceItem.last;
    NNPriceItem.LastSize := PriceItem.last_volume;
    NNPriceItem.LotSize := PriceItem.lot_size;
    NNPriceItem.Low := PriceItem.low;
    NNPriceItem.Open := PriceItem.open;
    NNPriceItem.TurnOver := PriceItem.turnover;
    NNPriceItem.TurnOverSize := PriceItem.turnover_volume;

    FOnUpdatePrice(NNPriceItem);
  end;
end;

procedure TNordNetBroker.DoTradeItem(const Sender: TNordNet; const FeedType: TFeedType; const TradeItem: TTradeFeedItem);
var
  NNTradeItem: TTradeItem;
begin
  if Assigned(FOnUpdateTrade) then
  begin
    NNTradeItem.Identifier := TradeItem.i;
    NNTradeItem.MarketId := TradeItem.m;
    NNTradeItem.TradeTimestamp := TradeItem.trade_timestamp;
    NNTradeItem.Price := TradeItem.price;
    NNTradeItem.Volume := TradeItem.volume;
    NNTradeItem.BasePrice := TradeItem.baseprice;
    NNTradeItem.BrokerBuying := TradeItem.broker_buying;
    NNTradeItem.BrokerSelling := TradeItem.broker_selling;
    NNTradeItem.TradeType := TradeItem.trade_type;
    NNTradeItem.IsIn := TradeItem.isin;

    FOnUpdateTrade(NNTradeItem);
  end;
end;

procedure TNordNetBroker.DoCheckOrderList;
var
  i: Integer;
  OrderItem: TOrder;
  OrdersList: TArray<TOrder>;
begin
  if Active and (AccountNum > 0) then
  begin
    OrdersList := GetOrders(AccountNum, True);
    for i := Low(OrdersList) to High(OrdersList) do
    begin
      OrderItem := OrdersList[i];
      if (not FOrderList.ContainsKey(OrderItem.accno)) or
        (OrderItem.modified > FOrderList[OrderItem.accno].modified) then
      begin
        FOrderList.AddOrSetValue(OrderItem.accno, OrderItem);
        if Assigned(OnUpdateOrder) then
          OnUpdateOrder(Self, OrderItem);
      end;
    end;
  end;
end;

class function TNordNetBroker.Logon(const UserName, Password: string): string;
begin
  Result := '';
  try
    if not NordNetBroker.Active then
    begin
      NordNetBroker.Login(UserName, Password, PUBLIC_KEY_FILE);
    end;
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

procedure TNordNetBroker.DoLeave(const Sender: TNordNet);
begin
  FAccountNum := -1;
end;

procedure TNordNetBroker.DoLogon(const Sender: TNordNet);
var
  i: Integer;
  arrAccounts: TArray<TAccount>;
begin
  if Sender.Active then
  begin
    arrAccounts := GetAccounts;
    i := GetDefaultAccountIndex(arrAccounts);
    FAccountNum := arrAccounts[i].accno;
    DoCheckOrderList;
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'DoLogon', THtmlLib.GetColorTag('NordNet is active. Account number is: ' + FAccountNum.ToString, clNavy));
  end;
end;

procedure TNordNetBroker.DoRequestError(const Sender: TNordNet; const Code, Message: string);
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoRequestError', Code + ': '+ Message);
end;

procedure TNordNetBroker.DoResponse(const Sender: TNordNet; const Path: string; const HttpCode: Integer; const HttpStatus, HttpBody: string);
begin
//  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'DoResponse', 'Path: ' + Path + '<br>' + 'HttpStatus: ' + HttpStatus + '<br>HttpBody:<br><code>' + HttpBody + '</code>');
end;

procedure TNordNetBroker.DoErrorItem(const Sender: TNordNet; const FeedType: TFeedType; const ErrorItem: TErrorFeedItem);
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoErrorItem', ErrorItem.cmd + ': ' + ErrorItem.msg);
end;

procedure TNordNetBroker.DoJsonArrived(const Sender: TNordNet; const FeedType: TFeedType; const JSON: string);
begin

end;

procedure TNordNetBroker.DoRequest(const Sender: TNordNet; const FeedType: TFeedType; const JSON: string);
begin
//  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'DoRequest', JSON);
end;

{TBrokerHelperNN}

class function TBrokerHelperNN.Login(const UserName, Password: string): string;
begin
  Result := '';
  try
    if not NordNetBroker.Active then
    begin
      NordNetBroker.Login(UserName, Password, PUBLIC_KEY_FILE);
    end;
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

class procedure TBrokerHelperNN.Leave;
begin
  NordNetBroker.Leave;
end;

class function TBrokerHelperNN.GetSokidByName(AFilter: string): TArray<TSokidInfo>;
begin
  Result := GetSokidBySymbol(AFilter);
end;

class function TBrokerHelperNN.GetSokidBySymbol(AFilter: string): TArray<TSokidInfo>;
var
  i, j: Integer;
  FInstruments: TInstruments;
  TradablesCount: Integer;
begin
  if AFilter = EmptyStr then
    FInstruments := NordNetBroker.GetInstruments('*', 10000)
  else
    FInstruments := NordNetBroker.GetInstruments(AFilter, 10000);
  SetLength(Result, Length(FInstruments));
  for i := 0 to Length(FInstruments) - 1 do
  begin
    Result[i].Broker       := TBrokerType.brNN;
    Result[i].ContractId   := FInstruments[i].instrument_id;
    Result[i].IsIn         := FInstruments[i].isin_code;
    Result[i].SecurityType := FInstruments[i].instrument_type;
    Result[i].Group        := FInstruments[i].instrument_group_type;
    Result[i].Name         := FInstruments[i].name;
    Result[i].Symbol       := FInstruments[i].symbol;
    Result[i].Currency     := FInstruments[i].currency;
    Result[i].Sector       := FInstruments[i].sector;
    TradablesCount         := Length(FInstruments[i].tradables);
    SetLength(Result[i].Tradables, TradablesCount);
    for j := 0 to TradablesCount - 1 do
    begin
      Result[i].Tradables[j].MarketId := FInstruments[i].tradables[j].market_id;
      Result[i].Tradables[j].Identifier := FInstruments[i].tradables[j].identifier;
    end;
  end;
end;

class function TBrokerHelperNN.Active: Boolean;
begin
  Result := NordNetBroker.Active;
end;

class function TBrokerHelperNN.GetFields(IsIN: Integer): TArray<TFieldType>;
begin
//
end;

class procedure TBrokerHelperNN.SubscribeFeedPrice(InstrumentId: string; MarketId: Integer);
begin
  if NordNetBroker.Active then
  begin
    if not NordNetBroker.FeedActive[ftPublic] then
      NordNetBroker.OpenFeed(ftPublic);
    NordNetBroker.SubscribeFeedPrice(ftPublic, InstrumentId, MarketId);
  end;
end;

class procedure TBrokerHelperNN.SubscribeFeedTrade(InstrumentId: string; MarketId: Integer);
begin
  if NordNetBroker.Active then
  begin
    if not NordNetBroker.FeedActive[ftPublic] then
      NordNetBroker.OpenFeed(ftPublic);
//    NordNetBroker.SubscribeFeedTrade(ftPublic, InstrumentId, MarketId);
  end;
end;

class procedure TBrokerHelperNN.UnsubscribeAllFeeds(InstrumentId: string; MarketId: Integer);
begin
  inherited;
  if NordNetBroker.Active then
  begin
    if not NordNetBroker.FeedActive[ftPublic] then
      NordNetBroker.OpenFeed(ftPublic);

    if not NordNetBroker.FeedActive[ftPrivate] then
      NordNetBroker.OpenFeed(ftPrivate);

    NordNetBroker.UnsubscribeFeedPrice(ftPublic, InstrumentId, MarketId);
    NordNetBroker.UnsubscribeFeedTrade(ftPublic, InstrumentId, MarketId);
  end;
end;

class procedure TBrokerHelperNN.SubscribeAllFeeds(InstrumentId: string; MarketId: Integer);
begin
  if NordNetBroker.Active then
  begin
    if not NordNetBroker.FeedActive[ftPublic] then
      NordNetBroker.OpenFeed(ftPublic);

    if not NordNetBroker.FeedActive[ftPrivate] then
      NordNetBroker.OpenFeed(ftPrivate);

    NordNetBroker.SubscribeFeedPrice(ftPublic, InstrumentId, MarketId);
//    NordNetBroker.SubscribeFeedTrade(ftPublic, InstrumentId, MarketId);
  end;
end;

class procedure TBrokerHelperNN.SetDate(const ADate: TDateTime);
begin
  inherited;
//
end;

class procedure TBrokerHelperNN.SetOpenFeed(OpenFeed: TChangeFeedStatus);
begin
  inherited;
  NordNetBroker.OnOpenFeed := OpenFeed;
end;

class procedure TBrokerHelperNN.SetCloseFeed(CloseFeed: TChangeFeedStatus);
begin
  inherited;
  NordNetBroker.OnCloseFeed := CloseFeed;
end;

class function TBrokerHelperNN.HelperName: string;
begin
  Result := 'NN';
end;

initialization
  BrokerHelperFactory.RegBrokerHelper(TBrokerHelperNN.HelperName, TBrokerHelperNN);
  NordNetBroker := TNordNetBroker.Create(URL);

finalization
  BrokerHelperFactory.UnRegBrokerHelper(TBrokerHelperNN.ClassName);
  NordNetBroker.Free;

end.
