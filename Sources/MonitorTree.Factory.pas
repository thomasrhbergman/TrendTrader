unit MonitorTree.Factory;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, IABSocketAPI_const, IABFunctions, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, HtmlLib, Document, Qualifiers.Types, Scanner.Types, VirtualTrees, BrokerHelperAbstr, DaModule, Data.DB,
  HtmlConsts, AutoTrades.Types, Common.Types, System.StrUtils, Utils, Global.Types,
  InstrumentList, Entity.Sokid, IABFunctions.MarketRules, System.Generics.Defaults, System.Generics.Collections,
  System.DateUtils, DaModule.Utils, ArrayHelper, Monitor.Types, Order.Utils, System.Math, Publishers, MonitorTree.Helper,
  MonitorTree.Document, Candidate.Types;
{$ENDREGION}

type
  TTreeFactory = class
  private
    class procedure FillAlgos(const aNode: PVirtualNode; const aInstrumentData: Scanner.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon);
    class procedure FillCondition(const aNode: PVirtualNode; const aInstrumentData: Scanner.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon); overload;
    class procedure FillCondition(const aNode: PVirtualNode; const aInstrumentData: Candidate.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon); overload;
    class procedure FillFactor(const aNode: PVirtualNode; const aInstrumentData: Scanner.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon);
    class procedure FillOrder(const aNode: PVirtualNode; const aInstrumentData: Scanner.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon); overload;
    class procedure FillOrder(const aNode: PVirtualNode; const aInstrumentData: Candidate.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon); overload;
    class procedure FillOrderGroup(const aNode: PVirtualNode; const aInstrumentData: Scanner.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon); overload;
    class procedure FillOrderGroup(const aNode: PVirtualNode; const aInstrumentData: Candidate.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon); overload;
    class procedure SetPriceValue(const aOrderDoc: TOrderIBDoc; const aPrice: Double = 0);
  public
    class procedure FillOrderPrice(const aOrder: TCustomOrderDoc; const aPrice: Currency);
    class procedure FillDocuments(const aNode: PVirtualNode; const aInstrumentData: Scanner.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon; const aAfterLoadProc: TAfterLoadEachDocumentProc; const aAfterLoadTreeProc: TAfterLoadTreeProc); overload;
    class procedure FillDocuments(const aNode: PVirtualNode; const aInstrumentData: Candidate.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon; const aAfterLoadProc: TAfterLoadEachDocumentProc; const aAfterLoadTreeProc: TAfterLoadTreeProc); overload;
  end;

implementation

{ TTreeFactory }

class procedure TTreeFactory.FillDocuments(const aNode: PVirtualNode;
                                           const aInstrumentData: Scanner.Types.TInstrumentData;
                                           const aPrice: Currency;
                                           const aAutoTradesCommon: TAutoTradesCommon;
                                           const aAfterLoadProc: TAfterLoadEachDocumentProc;
                                           const aAfterLoadTreeProc: TAfterLoadTreeProc);
var
  NodeArray: TNodeArray;
  Data: PTreeData;
begin
  NodeArray := TTreeDocument.GetNodesList(aNode);
  for var Node in NodeArray do
    if Assigned(Node) then
    begin
      Node.States := Node.States + [vsVisible];
      Data := Node^.GetData;
      Data^.RecordId := -1;
      case Data.DocType of
        ntQualifier:
          ;
        ntAutoTrade:
          ;
        ntOrderGroupSet:
          ;
        ntOrderGroup:
          FillOrderGroup(Node, aInstrumentData, aPrice, aAutoTradesCommon);
        ntOrder:
          FillOrder(Node, aInstrumentData, aPrice, aAutoTradesCommon);
        ntCondition:
          FillCondition(Node, aInstrumentData, aPrice, aAutoTradesCommon);
        ntAlgos:
          FillAlgos(Node, aInstrumentData, aPrice, aAutoTradesCommon);
        ntFactor:
          FillFactor(Node, aInstrumentData, aPrice, aAutoTradesCommon);
      end;
      if Assigned(aAfterLoadProc) then
        aAfterLoadProc(Node);
    end;
    if Assigned(aAfterLoadTreeProc) then
      aAfterLoadTreeProc(aNode);
end;

class procedure TTreeFactory.FillDocuments(const aNode: PVirtualNode;
                                           const aInstrumentData: Candidate.Types.TInstrumentData;
                                           const aPrice: Currency;
                                           const aAutoTradesCommon: TAutoTradesCommon;
                                           const aAfterLoadProc: TAfterLoadEachDocumentProc;
                                           const aAfterLoadTreeProc: TAfterLoadTreeProc);
var
  NodeArray: TNodeArray;
  Data: PTreeData;
begin
  NodeArray := TTreeDocument.GetNodesList(aNode);
  for var Node in NodeArray do
    if Assigned(Node) then
    begin
      Node.States := Node.States + [vsVisible];
      Data := Node^.GetData;
      Data^.RecordId := -1;
      case Data.DocType of
        ntQualifier:
          ;
        ntAutoTrade:
          ;
        ntOrderGroupSet:
          ;
        ntOrderGroup:
          FillOrderGroup(Node, aInstrumentData, aPrice, aAutoTradesCommon);
        ntOrder:
          begin
            FillOrder(Node, aInstrumentData, aPrice, aAutoTradesCommon);
            Node.CheckState := csCheckedNormal;
          end;
        ntCondition:
          FillCondition(Node, aInstrumentData, aPrice, aAutoTradesCommon);
        ntAlgos:
          ;//FillAlgos(Node, aInstrumentData, aPrice, aAutoTradesCommon);
        ntFactor:
          ;//FillFactor(Node, aInstrumentData, aPrice, aAutoTradesCommon);
      end;
      if Assigned(aAfterLoadProc) then
        aAfterLoadProc(Node);
    end;
    if Assigned(aAfterLoadTreeProc) then
      aAfterLoadTreeProc(aNode);
end;

class procedure TTreeFactory.FillAlgos(const aNode: PVirtualNode; const aInstrumentData: Scanner.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon);
var
  Data: PTreeData;
begin
  Data := aNode^.GetData;
  if Assigned(Data^.AlgosDoc) then
  begin
    aNode.CheckType := ctNone;
    Data^.AlgosDoc.AutoTradesID       := aAutoTradesCommon.AutoTradesID;
    Data^.AlgosDoc.AutoTradesInstance := aAutoTradesCommon.AutoTradesInstance;
    Data^.AlgosDoc.QualifierID        := aAutoTradesCommon.QualifierID;
    Data^.AlgosDoc.QualifierInstance  := aAutoTradesCommon.QualifierInstance;
  end;
end;

class procedure TTreeFactory.FillCondition(const aNode: PVirtualNode; const aInstrumentData: Scanner.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon);
var
  Data: PTreeData;
begin
  Data := aNode.GetData;
  if Assigned(Data^.ConditionDoc) then
  begin
    aNode.CheckType := ctNone;
    Data^.ConditionDoc.UseInAutoOrder     := False;
    Data^.ConditionDoc.IsCondition        := Data^.ConditionDoc.Active and Data^.ConditionDoc.Bypass and (Data^.ConditionDoc.CalcValue > 0);
    Data^.ConditionDoc.AutoTradesID       := aAutoTradesCommon.AutoTradesID;
    Data^.ConditionDoc.AutoTradesInstance := aAutoTradesCommon.AutoTradesInstance;
    Data^.ConditionDoc.QualifierID        := aAutoTradesCommon.QualifierID;
    Data^.ConditionDoc.QualifierInstance  := aAutoTradesCommon.QualifierInstance;
  end;
end;

class procedure TTreeFactory.FillCondition(const aNode: PVirtualNode; const aInstrumentData: Candidate.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon);
var
  Data: PTreeData;
begin
  Data := aNode.GetData;
  if Assigned(Data^.ConditionDoc) then
  begin
    aNode.CheckType := ctNone;
    Data^.ConditionDoc.UseInAutoOrder     := False;
    Data^.ConditionDoc.IsCondition        := Data^.ConditionDoc.Active and Data^.ConditionDoc.Bypass and (Data^.ConditionDoc.CalcValue > 0);
    Data^.ConditionDoc.AutoTradesID       := aAutoTradesCommon.AutoTradesID;
    Data^.ConditionDoc.AutoTradesInstance := aAutoTradesCommon.AutoTradesInstance;
    Data^.ConditionDoc.QualifierID        := aAutoTradesCommon.QualifierID;
    Data^.ConditionDoc.QualifierInstance  := aAutoTradesCommon.QualifierInstance;
    Data^.ConditionDoc.Enabled            := True;
    if Data^.ConditionDoc.Instrument.SokidInfo.ContractId = 0 then
    begin
      Data^.ConditionDoc.Instrument.SokidInfo.ContractId := aInstrumentData.Id;
      if SokidList.ContainsKey(Data^.ConditionDoc.Instrument.SokidInfo.ContractId) then
        Data^.ConditionDoc.Instrument.SokidInfo.AssignFrom(SokidList.Items[Data^.ConditionDoc.Instrument.SokidInfo.ContractId]);
    end;
  end;
end;

class procedure TTreeFactory.FillFactor(const aNode: PVirtualNode; const aInstrumentData: Scanner.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon);
var
  Data: PTreeData;
begin
  Data := aNode^.GetData;
  if Assigned(Data^.FactorDoc) and Data^.FactorDoc.UseInAutoOrder then
  begin
    aNode.CheckType := ctNone;
    Data^.FactorDoc.ContractId      := aInstrumentData.Id;
    Data^.FactorDoc.BrokerType      := aInstrumentData.BrokerType;
    Data^.FactorDoc.ContractType    := aInstrumentData.SecurityType.ToString;
    Data^.FactorDoc.Currency        := aInstrumentData.Currency;
    Data^.FactorDoc.Exchange        := aInstrumentData.Exchange;
    Data^.FactorDoc.PrimaryExchange := aInstrumentData.PrimaryExchange;
    Data^.FactorDoc.Expiry          := aInstrumentData.Expiry;
    Data^.FactorDoc.InstrumentName  := aInstrumentData.Name;
    Data^.FactorDoc.LastPrice1      := aPrice;
    Data^.FactorDoc.LastPrice2      := 0;
    Data^.FactorDoc.Symbol          := aInstrumentData.Symbol;
    Data^.FactorDoc.LocalSymbol     := aInstrumentData.LocalSymbol;
    Data^.FactorDoc.UseInAutoOrder  := False;

    Data^.FactorDoc.AutoTradesID       := aAutoTradesCommon.AutoTradesID;
    Data^.FactorDoc.AutoTradesInstance := aAutoTradesCommon.AutoTradesInstance;
    Data^.FactorDoc.QualifierID        := aAutoTradesCommon.QualifierID;
    Data^.FactorDoc.QualifierInstance  := aAutoTradesCommon.QualifierInstance;

    TMonitorLists.InstrumentList.DeleteNode(aNode);
    TMonitorLists.InstrumentList.AddNode(Data^.FactorDoc.ContractId, aNode);
  end;
end;

class procedure TTreeFactory.FillOrder(const aNode: PVirtualNode; const aInstrumentData: Scanner.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon);
var
  Data: PTreeData;
  SokidInfo: TSokidInfo;
  ExtOptions: TCustomOrderDoc.TExtendedOptions;
  OrderIB: TOrderIBDoc;
begin
  Data := aNode^.GetData;
  if Assigned(Data^.OrderDoc) then
  begin
    aNode.CheckType  := ctCheckBox;
    aNode.CheckState := csCheckedNormal;

    SokidInfo := SokidList.GetItem(aInstrumentData.Id);
    if Data^.OrderDoc.MarketList.IsEmpty then
      Data^.OrderDoc.MarketList := SokidInfo.MarketRuleIds;
    Data^.OrderDoc.Multiplier         := StrToFloatDef(SokidInfo.Multiplier, 0);
    Data^.OrderDoc.Id                 := aInstrumentData.Id;
    Data^.OrderDoc.InstrumentName     := aInstrumentData.Name;
    Data^.OrderDoc.Currency           := aInstrumentData.Currency;
    Data^.OrderDoc.Exchange           := aInstrumentData.Exchange;
    Data^.OrderDoc.Symbol             := aInstrumentData.Symbol;
    Data^.OrderDoc.PrimaryExchange    := aInstrumentData.PrimaryExchange;
    Data^.OrderDoc.Quantity           := aAutoTradesCommon.Quantity;

    Data^.OrderDoc.AutoTradesID       := aAutoTradesCommon.AutoTradesID;
    Data^.OrderDoc.AutoTradesInstance := aAutoTradesCommon.AutoTradesInstance;
    Data^.OrderDoc.QualifierID        := aAutoTradesCommon.QualifierID;
    Data^.OrderDoc.QualifierInstance  := aAutoTradesCommon.QualifierInstance;

    if (Data^.OrderDoc.BrokerType = TBrokerType.brIB) then
    begin
      OrderIB := Data^.OrderDoc as TOrderIBDoc;
      ExtOptions := Default(TCustomOrderDoc.TExtendedOptions);
      ExtOptions.LimitPriceRelative     := OrderIB.Limit;
      ExtOptions.AuxPriceRelative       := OrderIB.AuxPrice;
      ExtOptions.TrailStopPriceRelative := OrderIB.TrailStopPrice;
      ExtOptions.LimitPriceOffset       := OrderIB.LmtPriceOffset;

      ExtOptions.AuxBasePrice       := OrderIB.ExtendedOptions.AuxBasePrice;
      ExtOptions.LimitBasePrice     := OrderIB.ExtendedOptions.LimitBasePrice;
      ExtOptions.LmtOffsetBasePrice := OrderIB.ExtendedOptions.LmtOffsetBasePrice;
      ExtOptions.TrailStopBasePrice := OrderIB.ExtendedOptions.TrailStopBasePrice;
      ExtOptions.BasisForPrice      := OrderIB.ExtendedOptions.BasisForPrice;

      OrderIB.Limit           := 0;
      OrderIB.AuxPrice        := 0;
      OrderIB.TrailStopPrice  := 0;
      OrderIB.LmtPriceOffset  := 0;
      OrderIB.ExtendedOptions := ExtOptions;
    end
    else if (Data^.OrderDoc.BrokerType = TBrokerType.brNN) then
    begin

    end;
    FillOrderPrice(Data^.OrderDoc, aPrice);
  end;
end;

class procedure TTreeFactory.FillOrder(const aNode: PVirtualNode; const aInstrumentData: Candidate.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon);
var
  Data: PTreeData;
  SokidInfo: TSokidInfo;
  ExtOptions: TCustomOrderDoc.TExtendedOptions;
  OrderIB: TOrderIBDoc;
begin
  Data := aNode^.GetData;
  if Assigned(Data^.OrderDoc) then
  begin
    //aNode.CheckType  := ctCheckBox;
    //aNode.CheckState := csCheckedNormal;

    SokidInfo := SokidList.GetItem(aInstrumentData.Id);
    if Data^.OrderDoc.MarketList.IsEmpty then
      Data^.OrderDoc.MarketList := SokidInfo.MarketRuleIds;
    Data^.OrderDoc.Multiplier         := StrToFloatDef(SokidInfo.Multiplier, 0);
    Data^.OrderDoc.Id                 := aInstrumentData.Id;
    Data^.OrderDoc.InstrumentName     := aInstrumentData.Name;
    Data^.OrderDoc.Currency           := aInstrumentData.Currency;
    Data^.OrderDoc.Exchange           := aInstrumentData.Exchange;
    Data^.OrderDoc.Symbol             := aInstrumentData.Symbol;
    Data^.OrderDoc.PrimaryExchange    := aInstrumentData.PrimaryExchange;
    Data^.OrderDoc.Quantity           := aAutoTradesCommon.Quantity;

    Data^.OrderDoc.AutoTradesID       := aAutoTradesCommon.AutoTradesID;
    Data^.OrderDoc.AutoTradesInstance := aAutoTradesCommon.AutoTradesInstance;
    Data^.OrderDoc.QualifierID        := aAutoTradesCommon.QualifierID;
    Data^.OrderDoc.QualifierInstance  := aAutoTradesCommon.QualifierInstance;

    if (Data^.OrderDoc.BrokerType = TBrokerType.brIB) then
    begin
      OrderIB := Data^.OrderDoc as TOrderIBDoc;
      ExtOptions := Default(TCustomOrderDoc.TExtendedOptions);
      ExtOptions.LimitPriceRelative     := OrderIB.Limit;
      ExtOptions.AuxPriceRelative       := OrderIB.AuxPrice;
      ExtOptions.TrailStopPriceRelative := OrderIB.TrailStopPrice;
      ExtOptions.LimitPriceOffset       := OrderIB.LmtPriceOffset;

      ExtOptions.AuxBasePrice       := OrderIB.ExtendedOptions.AuxBasePrice;
      ExtOptions.LimitBasePrice     := OrderIB.ExtendedOptions.LimitBasePrice;
      ExtOptions.LmtOffsetBasePrice := OrderIB.ExtendedOptions.LmtOffsetBasePrice;
      ExtOptions.TrailStopBasePrice := OrderIB.ExtendedOptions.TrailStopBasePrice;
      ExtOptions.BasisForPrice      := OrderIB.ExtendedOptions.BasisForPrice;

      OrderIB.Limit           := 0;
      OrderIB.AuxPrice        := 0;
      OrderIB.TrailStopPrice  := 0;
      OrderIB.LmtPriceOffset  := 0;
      OrderIB.ExtendedOptions := ExtOptions;
    end
    else if (Data^.OrderDoc.BrokerType = TBrokerType.brNN) then
    begin

    end;
    FillOrderPrice(Data^.OrderDoc, aPrice);
  end;
end;

class procedure TTreeFactory.SetPriceValue(const aOrderDoc: TOrderIBDoc; const aPrice: Double = 0);
var
  Price            : Double;
  AdjCoef          : Double;
  AuxPrice         : Currency;
  LimitPrice       : Currency;
  LimitPriceOffset : Currency;
  TrailStopPrice   : Currency;
  SokidInfo        : TSokidInfo;
  TickSize         : Double;
  LastPrice        : Currency;
  ExtOptions       : TCustomOrderDoc.TExtendedOptions;

  function GetAdjustmentCoef(aCustomCoef, aPrice: Double): Double;
  begin
    if not General.AdjustmentCoefficients.UseAdjustmentCoef then
      Result := aCustomCoef
    else
    begin
      Result := aCustomCoef * (1 + (General.AdjustmentCoefficients.Coef * Abs(100 - aPrice)/(100 * aPrice)));
      if Result > (General.AdjustmentCoefficients.CoefMax / 100) then
        Result := General.AdjustmentCoefficients.CoefMax / 100;
      if (Result = 0) then
        Result := aCustomCoef;
    end;
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'TTreeFactory.SetPriceValue',
                                                                     'CustomCoef=' + aCustomCoef.ToString +
                                                                     ', Price=' + aPrice.ToString +
                                                                     ', AdjCoef=' + Result.ToString);
    aOrderDoc.CalculationStage[csAdjCoef] := Result;
  end;

  function GetBasePriceValue(const aContractId: Integer; const aBasePrice: TBasePrice): Currency;
  var
    AskPrice: Double;
    BidPrice: Double;
    BasePriceName: string;
  begin
    Result        := LastPrice;
    BasePriceName := 'LAST';
    case aBasePrice of
      pbAsk .. pbAsk10:
        begin
          BasePriceName := 'ASK' + Abs(Ord(aBasePrice) - 100).ToString;
          AskPrice := TMonitorLists.PriceCache.GetLastPrice(aContractId, ttAsk);
          if (AskPrice = 0) then
            AskPrice := LastPrice;
          Result := AskPrice + TickSize * Abs(Ord(aBasePrice) - 100);
        end;
      pbBid10 .. pbBid:
        begin
          BasePriceName := 'BID' + Abs(Ord(aBasePrice) + 100).ToString;
          BidPrice := TMonitorLists.PriceCache.GetLastPrice(aContractId, ttBid);
          if (BidPrice = 0) then
            BidPrice := LastPrice;
          Result := BidPrice - TickSize * Abs(Ord(aBasePrice) + 100);
        end;
      pbHigh:
        begin
          BasePriceName := 'HIGH';
          Result := TMonitorLists.PriceCache.GetLastPrice(aContractId, ttHigh);
          if (Result = 0) then
            Result := LastPrice;
        end;
      pbLow:
        begin
          BasePriceName := 'LOW';
          Result := TMonitorLists.PriceCache.GetLastPrice(aContractId, ttLow);
          if (Result = 0) then
            Result := LastPrice;
        end;
      pbMid:
        begin
          BasePriceName := 'MID';
          BidPrice := TMonitorLists.PriceCache.GetLastPrice(aContractId, ttBid);
          if (BidPrice = 0) then
            BidPrice := LastPrice;
          AskPrice := TMonitorLists.PriceCache.GetLastPrice(aContractId, ttAsk);
          if (AskPrice = 0) then
            AskPrice := LastPrice;
          Result := BidPrice + (AskPrice - BidPrice) / 2;
        end;
    end;
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText,
                                                             'TTreeFactory.GetBasePriceValue',
                                                             'ContractId='  + aContractId.ToString +
                                                             ', Symbol='    + aOrderDoc.Symbol +
                                                             ', Price='     + aPrice.ToString +
                                                             ', LastPrice=' + FloatToStr(LastPrice) +
                                                             ', ' + BasePriceName + '=' + FloatToStr(Result));
  end;

begin
  SokidInfo := Default(TSokidInfo);
  if SokidList.ContainsKey(aOrderDoc.Id) then
    SokidInfo := SokidList.Items[aOrderDoc.Id];
  if not (SokidInfo.GetSecurityType in [stFuture]) then
    SokidInfo.MinimumTick := 0;
  LastPrice  := TMonitorLists.PriceCache.GetLastPrice(aOrderDoc.Id, ttLast);
  TickSize   := IABClient.MarketRuleList.GetMinTick(LastPrice, SokidInfo.MarketRuleIds, SokidInfo.MinimumTick);
  ExtOptions := aOrderDoc.ExtendedOptions;

  if (ExtOptions.Subordination = suChildOrder) then
    case ExtOptions.BasisForPrice of
      bpFillPrice:
        begin
          Price := aPrice;
          TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'TTreeFactory.SetPriceValue',
                                                                'Subordination=suChildOrder, ' +
                                                                'BasisForPrice=FillPrice');
        end;
      bpTickType:
        begin
          Price := 0;
          TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'TTreeFactory.SetPriceValue',
                                                                'Subordination=suChildOrder, ' +
                                                                'BasisForPrice=TickType');
        end;
    end
  else
    Price := aPrice;

  if (Price = 0) then
  begin
    AuxPrice         := GetBasePriceValue(aOrderDoc.Id, ExtOptions.AuxBasePrice);
    LimitPrice       := GetBasePriceValue(aOrderDoc.Id, ExtOptions.LimitBasePrice);
    LimitPriceOffset := GetBasePriceValue(aOrderDoc.Id, ExtOptions.LmtOffsetBasePrice);
    TrailStopPrice   := GetBasePriceValue(aOrderDoc.Id, ExtOptions.TrailStopBasePrice);
    aOrderDoc.CalculationStage[csAuxBasePrice]       := AuxPrice;
    aOrderDoc.CalculationStage[csLimitBasePrice]     := LimitPrice;
    aOrderDoc.CalculationStage[csLmtOffsetBasePrice] := LimitPriceOffset;
    aOrderDoc.CalculationStage[csTrailStopBasePrice] := TrailStopPrice;
  end
  else
  begin
    AuxPrice         := Price;
    LimitPrice       := Price;
    LimitPriceOffset := Price;
    TrailStopPrice   := Price;
    aOrderDoc.CalculationStage[csAuxBasePrice]       := 0;
    aOrderDoc.CalculationStage[csLimitBasePrice]     := 0;
    aOrderDoc.CalculationStage[csLmtOffsetBasePrice] := 0;
    aOrderDoc.CalculationStage[csTrailStopBasePrice] := 0;
  end;

  aOrderDoc.LatestFillPrice := Price;
  aOrderDoc.CalculationStage[csLatestFillPrice] := Price;
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'TTreeFactory.SetPriceValue',
                                                        'ContractId='               +  aOrderDoc.Id.ToString +
                                                        ', Symbol='                 + aOrderDoc.Symbol +
                                                        ', Price='                  + Price.ToString +
                                                        ', TickSize='               + TickSize.ToString +
                                                        ', LimitPriceRelative='     + ExtOptions.LimitPriceRelative.ToString +
                                                        ', AuxPriceRelative='       + ExtOptions.AuxPriceRelative.ToString +
                                                        ', TrailStopPriceRelative=' + ExtOptions.TrailStopPriceRelative.ToString +
                                                        ', LimitPriceOffset='       + ExtOptions.LimitPriceOffset.ToString);
  case aOrderDoc.OrderType of
    otLimit:
      begin
        AdjCoef := GetAdjustmentCoef(ExtOptions.LimitPriceRelative / 100, LimitPrice);
        aOrderDoc.Limit := IABClient.MarketRuleList.RoundToMinTick(LimitPrice + AdjCoef * LimitPrice, aOrderDoc.MarketList, SokidInfo.MinimumTick);
        aOrderDoc.CalculationStage[csLimitPrice] := aOrderDoc.Limit;
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'TTreeFactory.SetPriceValue',
                                                                     'Limit='    + ExtOptions.LimitPriceRelative.ToString +
                                                                     '%, Limit=' + aOrderDoc.Limit.ToString);
      end;
    otStop:
      begin
        AdjCoef := GetAdjustmentCoef(ExtOptions.AuxPriceRelative / 100, AuxPrice);
        aOrderDoc.AuxPrice := IABClient.MarketRuleList.RoundToMinTick(AuxPrice + AdjCoef * AuxPrice, aOrderDoc.MarketList, SokidInfo.MinimumTick);
        aOrderDoc.CalculationStage[csAuxPrice] := aOrderDoc.AuxPrice;
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'TTreeFactory.SetPriceValue',
                                                                     'AuxPrice='    + ExtOptions.AuxPriceRelative.ToString +
                                                                     '%, AuxPrice=' + aOrderDoc.AuxPrice.ToString);
      end;
    otStopLimit:
      begin
        AdjCoef := GetAdjustmentCoef(ExtOptions.AuxPriceRelative / 100, AuxPrice);
        aOrderDoc.AuxPrice := IABClient.MarketRuleList.RoundToMinTick(AuxPrice + AdjCoef * AuxPrice, aOrderDoc.MarketList, SokidInfo.MinimumTick);
        aOrderDoc.CalculationStage[csAuxPrice] := aOrderDoc.AuxPrice;
        AdjCoef := GetAdjustmentCoef(ExtOptions.LimitPriceRelative / 100, LimitPrice);
        aOrderDoc.Limit := IABClient.MarketRuleList.RoundToMinTick(LimitPrice + AdjCoef * LimitPrice, aOrderDoc.MarketList, SokidInfo.MinimumTick);
        aOrderDoc.CalculationStage[csLimitPrice] := aOrderDoc.Limit;
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'TTreeFactory.SetPriceValue',
                                                                     'Limit='       + ExtOptions.LimitPriceRelative.ToString +
                                                                     '%, Limit='    + aOrderDoc.Limit.ToString +
                                                                     ', AuxPrice='  + ExtOptions.AuxPriceRelative.ToString +
                                                                     '%, AuxPrice=' + aOrderDoc.AuxPrice.ToString);
      end;
    otLimitTouch:
      begin
        AdjCoef := GetAdjustmentCoef(ExtOptions.LimitPriceRelative / 100, LimitPrice);
        aOrderDoc.Limit := IABClient.MarketRuleList.RoundToMinTick(LimitPrice + AdjCoef * LimitPrice, aOrderDoc.MarketList, SokidInfo.MinimumTick);
        aOrderDoc.CalculationStage[csLimitPrice] := aOrderDoc.Limit;
        AdjCoef := GetAdjustmentCoef(ExtOptions.AuxPriceRelative / 100, AuxPrice);
        aOrderDoc.AuxPrice := IABClient.MarketRuleList.RoundToMinTick(AuxPrice + AdjCoef * AuxPrice, aOrderDoc.MarketList, SokidInfo.MinimumTick);
        aOrderDoc.CalculationStage[csAuxPrice] := aOrderDoc.AuxPrice;
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'TTreeFactory.SetPriceValue',
                                                              ', Limit='    + aOrderDoc.Limit.ToString +
                                                              ', AuxPrice=' + aOrderDoc.AuxPrice.ToString);
      end;
    otTrail:
      begin
        AdjCoef := GetAdjustmentCoef(ExtOptions.TrailStopPriceRelative / 100, TrailStopPrice);
        aOrderDoc.TrailStopPrice := Abs(IABClient.MarketRuleList.RoundToMinTick(TrailStopPrice + AdjCoef * TrailStopPrice, aOrderDoc.MarketList, SokidInfo.MinimumTick));
        aOrderDoc.CalculationStage[csTrailStopPrice] := aOrderDoc.TrailStopPrice;
        AdjCoef := GetAdjustmentCoef(ExtOptions.AuxPriceRelative / 100, AuxPrice);
        aOrderDoc.AuxPrice := Abs(IABClient.MarketRuleList.RoundToMinTick(AdjCoef * AuxPrice, aOrderDoc.MarketList, SokidInfo.MinimumTick));
        aOrderDoc.CalculationStage[csAuxPrice] := aOrderDoc.AuxPrice;
        aOrderDoc.TrailingPercent := UNSET_DOUBLE;
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'TTreeFactory.SetPriceValue',
                                                              'TrailStopPrice='    + ExtOptions.TrailStopPriceRelative.ToString +
                                                              '%, TrailStopPrice=' + aOrderDoc.TrailStopPrice.ToString +
                                                              ', AuxPrice='  + ExtOptions.AuxPriceRelative.ToString);
      end;
    otTrailLimit:
      begin
        AdjCoef := GetAdjustmentCoef(ExtOptions.AuxPriceRelative / 100, AuxPrice);
        aOrderDoc.AuxPrice := Abs(IABClient.MarketRuleList.RoundToMinTick(AdjCoef * AuxPrice, aOrderDoc.MarketList, SokidInfo.MinimumTick));
        aOrderDoc.CalculationStage[csAuxPrice] := aOrderDoc.AuxPrice;
        AdjCoef := GetAdjustmentCoef(ExtOptions.LimitPriceOffset / 100, LimitPriceOffset);
        aOrderDoc.LmtPriceOffset := SimpleRoundTo(LimitPriceOffset * AdjCoef, -2);
        aOrderDoc.CalculationStage[csLimitPriceOffset] := aOrderDoc.LmtPriceOffset;
        AdjCoef := GetAdjustmentCoef(ExtOptions.TrailStopPriceRelative / 100, TrailStopPrice);
        aOrderDoc.TrailStopPrice := Abs(IABClient.MarketRuleList.RoundToMinTick(TrailStopPrice + TrailStopPrice * AdjCoef, aOrderDoc.MarketList, SokidInfo.MinimumTick));
        aOrderDoc.CalculationStage[csTrailStopPrice] := aOrderDoc.TrailStopPrice;
      end;
  end;
end;

class procedure TTreeFactory.FillOrderPrice(const aOrder: TCustomOrderDoc; const aPrice: Currency);
var
  OrderIB: TOrderIBDoc absolute aOrder;
begin
  if (aOrder.AutoTradesID > 0) then
    case aOrder.BrokerType of
      brIB:
        SetPriceValue(OrderIB, aPrice);
      brNN:
        ;
      brTest:
        ;
    end;
end;

class procedure TTreeFactory.FillOrderGroup(const aNode: PVirtualNode; const aInstrumentData: Scanner.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon);
var
  Data: PTreeData;
begin
  aNode.CheckType := ctNone;
  Data := aNode^.GetData;
  if Assigned(Data^.OrderGroupDoc) then
  begin
    Data^.CreationType := ctProgramm;
    Data^.OrderGroupDoc.IsAutoOrder        := True;
    Data^.OrderGroupDoc.AutoTradesID       := aAutoTradesCommon.AutoTradesID;
    Data^.OrderGroupDoc.AutoTradesInstance := aAutoTradesCommon.AutoTradesInstance;
    Data^.OrderGroupDoc.QualifierID        := aAutoTradesCommon.QualifierID;
    Data^.OrderGroupDoc.QualifierInstance  := aAutoTradesCommon.QualifierInstance;
    aNode.States := aNode.States + [vsVisible];
  end;
end;

class procedure TTreeFactory.FillOrderGroup(const aNode: PVirtualNode; const aInstrumentData: Candidate.Types.TInstrumentData; const aPrice: Currency; const aAutoTradesCommon: TAutoTradesCommon);
var
  Data: PTreeData;
begin
  aNode.CheckType := ctNone;
  Data := aNode^.GetData;
  if Assigned(Data^.OrderGroupDoc) then
  begin
    Data^.CreationType := ctProgramm;
    Data^.OrderGroupDoc.IsAutoOrder        := True;
    Data^.OrderGroupDoc.AutoTradesID       := aAutoTradesCommon.AutoTradesID;
    Data^.OrderGroupDoc.AutoTradesInstance := aAutoTradesCommon.AutoTradesInstance;
    Data^.OrderGroupDoc.QualifierID        := aAutoTradesCommon.QualifierID;
    Data^.OrderGroupDoc.QualifierInstance  := aAutoTradesCommon.QualifierInstance;
    aNode.States := aNode.States + [vsVisible];
  end;
end;

end.
