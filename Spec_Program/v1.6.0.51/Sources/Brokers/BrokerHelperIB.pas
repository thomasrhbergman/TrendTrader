unit BrokerHelperIB;

interface

uses
  BrokerHelperAbstr, Classes, InstrumentList, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.SysUtils,
  Utils, Entity.Sokid, Common.Types;

type
  TBrokerHelperIB = class(TBrokerHelperAbstr)
  private
    class var FActive: Boolean;
  public
    class function Active: Boolean; override;
    class function GetFields(IsIN: integer): TArray<TFieldType>; override;
    class function GetSokidBySymbol(AFilter: string): TArray<TSokidInfo>; override;
    class function GetSokidByName(AFilter: string): TArray<TSokidInfo>; override;
    class function HelperName: string; override;
    class function Login(const UserName, Password: string): string; override;
    class procedure Leave; override;
    class procedure SetCloseFeed(CloseFeed: TChangeFeedStatus); override;
    class procedure SetDate(const ADate: TDateTime); override;
    class procedure SetOpenFeed(OpenFeed: TChangeFeedStatus); override;
    class procedure SetUpdatePrice(PriceArrived: TUpdatePriceItem); override;
    class procedure SubscribeAllFeeds(InstrumentId: string; MarketId: integer); override;
    class procedure SubscribeFeedPrice(InstrumentId: string; MarketId: integer); override;
    class procedure SubscribeFeedTrade(InstrumentId: string; MarketId: integer); override;
  end;

implementation

{ TBrokerHelperIB }

uses
   BrokerHelperFactory;

class function TBrokerHelperIB.Login(const UserName, Password: string): string;
begin
  Result := '';
  FActive := True;;
end;

class procedure TBrokerHelperIB.Leave;
begin
  FActive := False;
end;

class function TBrokerHelperIB.GetSokidByName(AFilter: string): TArray<TSokidInfo>;
var
  i: Integer;
  SokidInfo: TSokidInfo;
begin
  i := 0;
  for SokidInfo in SokidList.Values do
    if (SokidInfo.Broker = TBrokerType.brIB)then
    begin
      if not AFilter.IsEmpty and not SokidInfo.Name.ToUpper.Contains(AFilter.ToUpper) then
        Continue;
      SetLength(Result, i + 1);
      Result[i].AssignFrom(SokidInfo);
      Inc(i);
    end;
end;

class function TBrokerHelperIB.GetSokidBySymbol(AFilter: string): TArray<TSokidInfo>;
var
  i: Integer;
  SokidInfo: TSokidInfo;
begin
  i := 0;
  for SokidInfo in SokidList.Values do
    if (SokidInfo.Broker = TBrokerType.brIB)then
    begin
      if not AFilter.IsEmpty and not SokidInfo.Symbol.ToUpper.Contains(AFilter.ToUpper) then
        Continue;
      SetLength(Result, i + 1);
      Result[i].AssignFrom(SokidInfo);
      Inc(i);
    end;
end;

class function TBrokerHelperIB.Active: Boolean;
begin
  Result := FActive;
end;

class function TBrokerHelperIB.GetFields(IsIN: integer): TArray<TFieldType>;
begin
  //
end;

class procedure TBrokerHelperIB.SubscribeFeedPrice(InstrumentId: string; MarketId: integer);
begin
  //
end;

class procedure TBrokerHelperIB.SubscribeFeedTrade(InstrumentId: string; MarketId: integer);
begin
  //
end;

class procedure TBrokerHelperIB.SubscribeAllFeeds(InstrumentId: string; MarketId: integer);
begin
  //
end;

class procedure TBrokerHelperIB.SetCloseFeed(CloseFeed: TChangeFeedStatus);
begin
  inherited;

end;

class procedure TBrokerHelperIB.SetDate(const ADate: TDateTime);
begin
  inherited;
  //
end;

class procedure TBrokerHelperIB.SetOpenFeed(OpenFeed: TChangeFeedStatus);
begin
  inherited;

end;

class procedure TBrokerHelperIB.SetUpdatePrice(PriceArrived: TUpdatePriceItem);
begin
  //
end;

class function TBrokerHelperIB.HelperName: string;
begin
  Result := 'IB';
end;

initialization
  BrokerHelperFactory.RegBrokerHelper(TBrokerHelperIB.HelperName, TBrokerHelperIB);

finalization
  BrokerHelperFactory.UnRegBrokerHelper(TBrokerHelperIB.ClassName);

end.
