unit IABFunctions.MarketData;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections, System.Threading, System.Types,
  System.Generics.Defaults,Winapi.Messages, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  IABSocketAPI, IABSocketAPI_const, IABFunctions, ArrayHelper, Generics.Helper, IABFunctions.RequestsQueue,
  Entity.Sokid, InstrumentList, Common.Types, Global.Types, Publishers, IABFunctions.Helpers;
{$ENDREGION}

type
  TIABMarket = class
  public
    class procedure RequestMarketData(const aContractId: Integer); overload;
    class procedure RequestMarketData(const aOrder: TIABOrder); overload;
    class procedure CancelMarketData(const DataId: Integer);
  end;

implementation

{ TIABMarket }

class procedure TIABMarket.RequestMarketData(const aContractId: Integer);
var
  Order: TIABOrder;
begin
  if IABClient.Connected and (aContractId > 0) and (not IABClient.MarketSubscribeList.IsSubscribed(aContractId)) then
  begin
    Order := nil;
    try
      if SokidList.ContainsKey(aContractId) then
        Order := SokidList.GetOrderByConID(aContractId);
      RequestMarketData(Order);
    finally
      FreeAndNil(Order);
    end;
  end;
end;

class procedure TIABMarket.RequestMarketData(const aOrder: TIABOrder);
var
  LastPrice : Double;
//  Request   : TIABRequest;
  SokidInfo : TSokidInfo;
begin
  if IABClient.Connected and Assigned(aOrder) then
  begin
    if not (aOrder.SecurityType in [stCash]) then
      if not TMonitorLists.InstrumentList.ContainsKey(aOrder.ContractId) then
        IABClient.SendRequest(ibGetInstrumentSpecs, 1, aOrder);

    LastPrice := 0;
    if SokidList.ContainsKey(aOrder.ContractId) then
    begin
      SokidInfo := SokidList.Items[aOrder.ContractId];
      LastPrice := SokidInfo.LastPrice;
      if not (aOrder.SecurityType in [stCash]) and not SokidInfo.MarketRuleIds.IsEmpty then
        IABClient.reqMarketRule(SokidInfo.MarketRuleIds);
    end;
    if not TMonitorLists.PriceCache.ContainsKey(aOrder.ContractId) then
      TMonitorLists.PriceCache.InitPrice(aOrder.ContractId, aOrder.Symbol, aOrder.Currency, aOrder.Exchange, LastPrice);
    IABClient.reqMktData(aOrder.ContractId, aOrder, General.GenericTickRequired);

    TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'TIBSubscribeMarket', 'RequestMarketData',
                                                  'Symbol=' + aOrder.Symbol +
                                                  ', ContractId=' + aOrder.ContractId.ToString +
                                                  ', Currency=' + aOrder.Currency +
                                                  ', Exchange=' + aOrder.Exchange +
                                                  ', PrimaryExchange=' + aOrder.PrimaryExchange +
                                                  ', Multiplier=' + aOrder.Multiplier +
                                                  ', GenericTickRequired=' + General.GenericTickRequired.ToString +
                                                  ', SecurityType=' + aOrder.SecurityType.ToString);

    //Maths: I suggest we as defeault has no subscription ( for now..)
    //I guess subscribe is for have  a real time chart running
//    if not (aOrder.SecurityType in [stCash]) then
//    begin
//      Request := Default(TIABRequest);
//      Request.Command := ibGetHistoricalData;
//      Request.DataId  := aOrder.ContractId;
//      Request.Order   := aOrder;
//      IABClient.SendRequest(Request);
//    end;
  end;
end;

class procedure TIABMarket.CancelMarketData(const DataId: Integer);
begin
  IABClient.CancelMktData(DataId);
end;

end.
