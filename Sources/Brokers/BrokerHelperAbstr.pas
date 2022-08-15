unit BrokerHelperAbstr;

interface

{$REGION 'Region uses'}
uses
  Classes, SysUtils, InstrumentList, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Entity.Sokid, Common.Types;
{$ENDREGION}

type
  TFieldType = record
    Id   : Integer;
    Name : string;
  end;

  TPriceItem = record
    Identifier     : string;
    MarketId       : Integer;
    TradeTimestamp : string;
    TickTimestamp  : string;
    Bid            : Double;
    BidSize        : Integer;
    Ask            : Double;
    AskSize        : Integer;
    Close          : Double;
    High           : Double;
    Last           : Double;
    LastSize       : Integer;
    LotSize        : string;
    Low            : Double;
    Open           : Double;
    TurnOver       : Double;
    TurnOverSize   : Integer;
  end;

  TUpdatePriceItem = procedure(const PriceItem: TPriceItem) of object;
  TChangeFeedStatus = procedure of object;

  TTradeItem = record
    Identifier     : string;
    MarketId       : Integer;
    TradeTimestamp : string;
    Price          : Double;
    Volume         : Integer;
    BasePrice      : Double;
    BrokerBuying   : string;
    BrokerSelling  : string;
    TradeType      : string;
    IsIn           : string;
  end;

  TUpdateTradeItem = procedure(const TradeItem: TTradeItem) of object;

  TBrokerHelperAbstr = class
  public
    class function Active: Boolean; virtual; abstract;
    class function GetFields(IsIN: integer): TArray<TFieldType>; virtual; abstract;
    class function GetSokidBySymbol(AFilter: string): TArray<TSokidInfo>; virtual; abstract;
    class function GetSokidByName(AFilter: string): TArray<TSokidInfo>; virtual; abstract;
    class function HelperName: string; virtual; abstract;
    class function Login(const UserName, Password: string): string; virtual; abstract;
    class procedure Leave; virtual; abstract;
    class procedure SetCloseFeed(CloseFeed: TChangeFeedStatus); virtual; abstract;
    class procedure SetDate(const ADate: TDateTime); virtual; abstract;
    class procedure SetOpenFeed(OpenFeed: TChangeFeedStatus); virtual; abstract;
    class procedure SetUpdatePrice(PriceArrived: TUpdatePriceItem); virtual; abstract;
    class procedure SubscribeAllFeeds(InstrumentId: string; MarketId: integer); virtual; abstract;
    class procedure SubscribeFeedPrice(InstrumentId: string; MarketId: integer); virtual; abstract;
    class procedure SubscribeFeedTrade(InstrumentId: string; MarketId: integer); virtual; abstract;
    class procedure UnsubscribeAllFeeds(InstrumentId: string; MarketId: integer); virtual; abstract;
  end;

  TBrokerHelperClass = class of TBrokerHelperAbstr;



implementation


end.
