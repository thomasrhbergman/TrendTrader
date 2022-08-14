unit BrokerHelperTB;

interface

uses
  BrokerHelperAbstr, Classes, Vcl.ExtCtrls, System.SysUtils, BrokerHelperFactory, DaModule, TBfunctions,
  InstrumentList, Common.Types, Entity.Sokid {$IFDEF USE_CODE_SITE}, CodeSiteLogging{$ENDIF};

type
  TBrokerHelperTB = class(TBrokerHelperAbstr)
  private
    class var FActive: Boolean;
    class function GetSokid(AFieldName, AFilter: string): TArray<TSokidInfo>;
  public
    class function Active: Boolean; override;
    class function GetFields(IsIN: integer): TArray<TFieldType>; override;
    class function GetSokidBySymbol(AFilter: string): TArray<TSokidInfo>; override;
    class function GetSokidByName(AFilter: string): TArray<TSokidInfo>; override;
    class function HelperName: string; override;
    class function Login(const UserName, Password: string): string; override;
    class procedure Leave; override;
    class procedure SetDate(const ADate: TDateTime); override;
    class procedure SetUpdatePrice(PriceArrived: TUpdatePriceItem); override;
    class procedure SubscribeAllFeeds(InstrumentId: string; MarketId: integer); override;
    class procedure SubscribeFeedPrice(InstrumentId: string; MarketId: integer); override;
    class procedure SubscribeFeedTrade(InstrumentId: string; MarketId: integer); override;
  end;

var
  WorldIndexBroker: TWorldIndexBroker;

implementation

{ TBrokerHelperTB }

class function TBrokerHelperTB.Login(const UserName, Password: string): string;
begin
  Result := '';
end;

class procedure TBrokerHelperTB.Leave;
begin
  WorldIndexBroker.EndFeed;
  FActive := False;
end;

class function TBrokerHelperTB.GetSokid(AFieldName, AFilter: string): TArray<TSokidInfo>;
var
  i: Integer;
begin
  with DMod.fbqAktie do
  begin
    if AFilter <> EmptyStr then
    begin
      Filter := 'UPPER(' + AFieldName + ') like ' + QuotedStr('%' + AFilter.ToUpper + '%');
      Filtered := True;
    end;
    Open;
    try
      Last;
      SetLength(Result, RecordCount);
      First;
      i := 0;
      while not Eof do
      begin
        Result[i].Broker       := TBrokerType.brTest;
        Result[i].ContractId   := DMod.fbqAktieA_NR.AsInteger;
        Result[i].IsIN         := '';
        Result[i].SecurityType := 'STK';
        Result[i].Name         := DMod.fbqAktieA_NAMN.AsString;
        Result[i].Symbol       := DMod.fbqAktieA_NR.AsString;
        Result[i].Currency     := '';
        Next;
        Inc(i);
      end;
    finally
      Close;
      Filtered := False;
    end;
  end;
end;

class function TBrokerHelperTB.GetSokidByName(AFilter: string): TArray<TSokidInfo>;
begin
  Result := TBrokerHelperTB.GetSokid('A_NAMN', AFilter);
end;

class function TBrokerHelperTB.GetSokidBySymbol(AFilter: string): TArray<TSokidInfo>;
begin
  Result := TBrokerHelperTB.GetSokid('I_INDEX', AFilter);
end;

class function TBrokerHelperTB.Active: Boolean;
begin
  Result := FActive;
end;

class function TBrokerHelperTB.GetFields(IsIN: integer): TArray<TFieldType>;
begin
  //
end;

class procedure TBrokerHelperTB.SubscribeFeedPrice(InstrumentId: string; MarketId: integer);
begin
  //
end;

class procedure TBrokerHelperTB.SubscribeFeedTrade(InstrumentId: string; MarketId: integer);
begin
  //
end;

class procedure TBrokerHelperTB.SubscribeAllFeeds(InstrumentId: string; MarketId: integer);
begin
  //
end;

class procedure TBrokerHelperTB.SetDate(const ADate: TDateTime);
begin
  inherited;
  WorldIndexBroker.FilterDate := ADate;
end;

class procedure TBrokerHelperTB.SetUpdatePrice(PriceArrived: TUpdatePriceItem);
begin
  WorldIndexBroker.OnPriceArrived := PriceArrived;
  if Assigned(PriceArrived) then
  begin
    WorldIndexBroker.BeginFeed;
    FActive := True;
  end
  else
  begin
    WorldIndexBroker.EndFeed;
    FActive := False;
  end;
end;

class function TBrokerHelperTB.HelperName: string;
begin
  Result := 'TB';
end;

initialization
  BrokerHelperFactory.RegBrokerHelper(TBrokerHelperTB.HelperName, TBrokerHelperTB);
  WorldIndexBroker := TWorldIndexBroker.Create;

finalization
  BrokerHelperFactory.UnRegBrokerHelper(TBrokerHelperTB.ClassName);
  FreeAndNil(WorldIndexBroker);
end.
