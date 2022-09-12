unit Monitor.Info;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, IABSocketAPI_const, IABFunctions, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, HtmlLib, Document, Qualifiers.Types, Scanner.Types, VirtualTrees, BrokerHelperAbstr, DaModule, Data.DB,
  HtmlConsts, AutoTrades.Types, Common.Types, System.StrUtils, Utils, Global.Types,
  InstrumentList, Entity.Sokid, IABFunctions.MarketRules, System.Generics.Defaults, System.Generics.Collections,
  System.DateUtils, Monitor.Types, Order.Utils, ArrayHelper, Publishers, Publishers.Interfaces, System.Math,
  IABFunctions.Helpers, MonitorTree.Document;
{$ENDREGION}

type
  TDocumentInfo = class
  private const
    ColSpan = '21';
    NodeID = 'NodeID';
  public
    class function GetCalculationStageInfo(aTree: TBaseVirtualTree): string;
    class function GetExchangeRate: string;
    class function GetInstrumentsWithoutFeed: string;
    class function GetNodeInfo(const aNode: PVirtualNode; const aWithHTMLHeader: Boolean = True): string;
    class function GetOrdersTimestamp: string;
    class function GetPrecautionarySettingsInformation: string;
    class function GetPriceHistory(const aContractId: Integer; const aFilterFunc: TFilterFunc): string; overload;
    class function GetPriceHistory(const aNode: PVirtualNode): string; overload;
    class function GetPriceInformation: string;
    class function GetRuleInformation(aNode: PVirtualNode): string;
    class function GetScanColumnsInfo(aColumns: TAutoTradeColumns): string;
    class function GetSubscribersInformation: string;
  end;

implementation

{ TDocumentInfo }

class function TDocumentInfo.GetCalculationStageInfo(aTree: TBaseVirtualTree): string;

  function GetOrderGroupDocInfo(aOrderGroupDoc: TOrderGroupDoc; aLevel: Integer): string;
  begin
    Result := '<tr><td colspan="' + ColSpan + '" style="padding-left:' + (aLevel * 10).ToString  + 'px"><b>' + aOrderGroupDoc.Name + '</b></td></tr>';
  end;

  function GetOrderDocInfo(aOrderDoc: TCustomOrderDoc; aLevel: Integer): string;
  var
    BasisForPrice: string;
  begin
    BasisForPrice := '';
    if (aOrderDoc.ExtendedOptions.Subordination = suChildOrder) then
      BasisForPrice := 'BasisForPrice: ' + aOrderDoc.ExtendedOptions.BasisForPrice.ToString + '<br>';
    if ((aOrderDoc.ExtendedOptions.Subordination = suChildOrder) and (aOrderDoc.ExtendedOptions.BasisForPrice = bpTickType)) or
       (aOrderDoc.ExtendedOptions.Subordination <> suChildOrder) then
    begin
      if aOrderDoc.OrderType in [otTrail, otTrailLimit, otStop, otStopLimit] then
        BasisForPrice := BasisForPrice + 'AuxPrice: ' + aOrderDoc.CalculationStage[csAuxBasePrice].ToString + ' (' +
                                                        aOrderDoc.ExtendedOptions.AuxBasePrice.ToString + ')<br>';
      if aOrderDoc.OrderType in [otLimit, otStopLimit, otTrailLimit] then
        BasisForPrice := BasisForPrice + 'LimitPrice: ' + aOrderDoc.CalculationStage[csLimitBasePrice].ToString + ' (' +
                                                          aOrderDoc.ExtendedOptions.LimitBasePrice.ToString + ')<br>';
      if aOrderDoc.OrderType in [otTrail, otTrailLimit] then
        BasisForPrice := BasisForPrice + 'TrailStop: ' + aOrderDoc.CalculationStage[csLmtOffsetBasePrice].ToString + ' (' +
                                                         aOrderDoc.ExtendedOptions.TrailStopBasePrice.ToString +')<br>';
      if aOrderDoc.OrderType in [otTrailLimit] then
        BasisForPrice := BasisForPrice + 'LmtOffset: ' + aOrderDoc.CalculationStage[csTrailStopBasePrice].ToString + ' (' +
                                                         aOrderDoc.ExtendedOptions.LmtOffsetBasePrice.ToString + ')<br>';
    end;

    Result := THtmlLib.GetTableLineTag(VarArrayOf(['<font style="margin:' + (aLevel * 10).ToString + 'px">' + aOrderDoc.Id.ToString + '</font>',
                                                      aOrderDoc.OrderIBId,
                                                      aOrderDoc.Symbol,
                                                      aOrderDoc.Exchange,
                                                      aOrderDoc.OrderType.ToString,
                                                      IfThen(aOrderDoc.OrderAction = iabBuy, THtmlLib.GetColorTag(aOrderDoc.OrderAction.ToString, clBlue),THtmlLib.GetColorTag(aOrderDoc.OrderAction.ToString, clRed)),
                                                      aOrderDoc.CalculationStage[csLatestFillPrice],
                                                      aOrderDoc.CalculationStage[csAdjCoef],
                                                      aOrderDoc.CalculationStage[csAdjCoef] * aOrderDoc.CalculationStage[csLatestFillPrice],
                                                      aOrderDoc.CalculationStage[csLatestFillPrice] - aOrderDoc.CalculationStage[csAdjCoef] * aOrderDoc.CalculationStage[csLatestFillPrice],
                                                      IfThen(Abs(AOrderDoc.ExtendedOptions.LimitPriceRelative) > 0.0001, aOrderDoc.CalculationStage[csLimitPrice].ToString + ' / (' + (AOrderDoc.ExtendedOptions.LimitPriceRelative).ToString + '%)', '0/0'),
                                                      aOrderDoc.CalculationStage[csLatestFillPrice] * (1 + AOrderDoc.ExtendedOptions.LimitPriceRelative / 100),
                                                      aOrderDoc.CalculationStage[csAuxPrice].ToString + ' / (' + Abs(SimpleRoundTo(AOrderDoc.ExtendedOptions.AuxPriceRelative, -C_DECIMALS)).ToString + '%)',
                                                      aOrderDoc.CalculationStage[csLimitPrice].ToString + ' / (' + Abs(SimpleRoundTo(AOrderDoc.ExtendedOptions.LimitPriceRelative, -C_DECIMALS)).ToString + '%)',
                                                      aOrderDoc.CalculationStage[csTrailStopPrice].ToString + ' / (' + Abs(SimpleRoundTo(AOrderDoc.ExtendedOptions.TrailStopPriceRelative, -C_DECIMALS)).ToString + '%)',
                                                      aOrderDoc.CalculationStage[csAvgPrice],
                                                      aOrderDoc.CalculationStage[csFillPrice],
                                                      CurrToStr(TMonitorLists.PriceCache.GetLastPrice(aOrderDoc.Id, ttBid)) + ' / ' + CurrToStr(TMonitorLists.PriceCache.GetLastPrice(aOrderDoc.Id, ttAsk)),
                                                      CurrToStr(TMonitorLists.PriceCache.GetLastPrice(aOrderDoc.Id, ttBidSize)) + ' / ' + CurrToStr(TMonitorLists.PriceCache.GetLastPrice(aOrderDoc.Id, ttAskSize)),
                                                      BasisForPrice,
                                                      aOrderDoc.Description
                                                     ]));
  end;

  function GetConditionDocInfo(aConditionDoc: TConditionDoc; aLevel: Integer): string;
  var
    Table: string;
    Caption: string;
  begin
    Table := THtmlLib.GetTableTag(VarArrayOf(['Init Value',
                                              'Calculated Value',
                                              'Condition Value',
                                              'Description',
                                              'Active',
                                              'Type'
                                              ])) +
             THtmlLib.GetTableLineTag(VarArrayOf([CurrToStr(aConditionDoc.InitValue),
                                                     CurrToStr(aConditionDoc.CalcValue),
                                                     CurrToStr(aConditionDoc.CondLimit),
                                                     aConditionDoc.Description,
                                                     BoolToStr(aConditionDoc.Active, True),
                                                     aConditionDoc.CondType.ToString
                                                     ])) +
             C_HTML_TABLE_CLOSE;
    if aConditionDoc.IsCondition then
      Caption := THtmlLib.GetColorTag('Condition', clGreen)
    else
      Caption := THtmlLib.GetColorTag('Condition', clRed);
    if aConditionDoc.Bypass then
      Caption := Concat(Caption, THtmlLib.GetBoldText(' [Bypass]'));
    Result := THtmlLib.GetSpoilerTag(Caption, Table, 'colspan="' + ColSpan + '" style="padding-left:' + (aLevel * 10).ToString + 'px"');
  end;

var
  arr: TNodeArray;
  Data: PTreeData;
  Node: PVirtualNode;
  sb: TStringBuilder;
begin
  if Assigned(aTree) then
  begin
    sb := TStringBuilder.Create;
    try
      sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
        .Append(C_HTML_BODY_OPEN).AppendLine
        .Append(C_HTML_BREAK)
        .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK).AppendLine
        .Append(C_HTML_BREAK).AppendLine
        .Append(THtmlLib.GetBoldText('Const Adjustment Coefficient (%): ') + General.AdjustmentCoefficients.Coef.ToString).Append(C_HTML_BREAK).AppendLine
        .Append(THtmlLib.GetBoldText('Const Max Adjustment Coefficient (%): ') + General.AdjustmentCoefficients.CoefMax.ToString).Append(C_HTML_BREAK).AppendLine
        .Append(C_HTML_BREAK).AppendLine
        .Append(THtmlLib.GetTableTag(VarArrayOf(['ConId',
                                                 'OrderId',
                                                 'Symbol',
                                                 'Exchange',
                                                 'OrderType',
                                                 'Action',
                                                 csLatestFillPrice.ToString,
                                                 csAdjCoef.ToString,
                                                 'AdjCoef × LastPrice',
                                                 'LastPrice -/+ AdjCoef × LastPrice',
                                                 csLimitPrice.ToString + ' / %',
                                                 'Unadjusted Price',
                                                 csAuxPrice.ToString + ' / %',
                                                 csLimitPrice.ToString + ' / %',
                                                 csTrailStopPrice.ToString + ' / %',
                                                 csAvgPrice.ToString,
                                                 csFillPrice.ToString,
                                                 'Bid / Ask',
                                                 'BidSize / AskSize',
                                                 'Base for Calculation',
                                                 'Description'
                                                 ]), 'Calculation Stage Info')).AppendLine;
      arr := TTreeDocument.GetNodesList(aTree.RootNode);
      for Node in arr do
        if Assigned(Node) and (Node <> aTree.RootNode) then
        begin
          Data := Node^.GetData;
          if Assigned(Data^.OrderGroupDoc) and (Node^.ChildCount > 0) then
            sb.Append(GetOrderGroupDocInfo(Data^.OrderGroupDoc, aTree.GetNodeLevel(Node))).AppendLine;
          if Assigned(Data^.OrderDoc) then
            sb.Append(GetOrderDocInfo(Data^.OrderDoc, aTree.GetNodeLevel(Node))).AppendLine;
          if Assigned(Data^.ConditionDoc) then
            sb.Append(GetConditionDocInfo(Data^.ConditionDoc, aTree.GetNodeLevel(Node))).AppendLine;
        end;
         sb.Append(C_HTML_TABLE_CLOSE).Append(C_HTML_BREAK).AppendLine
           .Append(THtmlLib.GetBoldText('AdjCoef = LimitPrice × (1 + (AdjustmentCoefficient × Abs(100 - LastPrice)/(100 × LastPrice)))')).Append(C_HTML_BREAK).AppendLine
           .Append(C_HTML_BODY_CLOSE).AppendLine;
      Result := sb.ToString;
    finally
      FreeAndNil(sb);
    end;
  end;
end;

class function TDocumentInfo.GetExchangeRate: string;
var
  Currency: string;
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
      .Append(C_HTML_BREAK)
      .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK).AppendLine
      .Append(THtmlLib.GetTableTag(VarArrayOf(['Currency', 'Exchange']), 'Exchange Rate Information')).AppendLine;

    for Currency in TMonitorLists.CurrencyCache.Keys do
      sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Currency, TMonitorLists.CurrencyCache.GetLastExchange(Currency)]))).AppendLine;
    sb.Append(C_HTML_TABLE_CLOSE).Append(C_HTML_BREAK).AppendLine
      .Append(C_HTML_BODY_CLOSE).AppendLine;
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

class function TDocumentInfo.GetOrdersTimestamp: string;
var
  Data: PTreeData;
  Node: PVirtualNode;
  sb: TStringBuilder;

  function GetStatusItemInfo(aNode: PVirtualNode): string;
  var
    arr: TArrayRecord<TOrderItem.TStatusItem>;
  begin
    Result := '';
    arr := TMonitorLists.OrderList.GetStatusArray(aNode);
    if (arr.Count > 0 )then
    begin
      Result := THtmlLib.GetTableTag(VarArrayOf(['Event', 'Status', 'TimeStamp', 'LastPrice']));
      for var StatusItem in arr do
        Result := Concat(Result, THtmlLib.GetTableLineTag(VarArrayOf([StatusItem.Event,
                                                                      StatusItem.Status.ToString,
                                                                      FormatDateTime('hh:nn:ss.zzz', StatusItem.TimeStamp),
                                                                      FormatFloat(C_CURRENCY_FORMAT, StatusItem.LastPrice)
                                                                      ])));
      Result := Concat(Result, C_HTML_TABLE_CLOSE);
    end;
    Result := THtmlLib.GetSpoilerTag('Status List', Result, 'colspan="8" style="padding-left:10px"');
  end;

begin
  sb := TStringBuilder.Create;
  try
    sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
      .Append(C_HTML_BODY_OPEN).AppendLine
      .Append(C_HTML_BREAK)
      .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK).AppendLine
      .Append(THtmlLib.GetTableTag(VarArrayOf(['ConId',
                                               'OrderId',
                                               'Symbol',
                                               'Exchange',
                                               'OrderType',
                                               'Action',
                                               'Description',
                                               'Status'
                                               ]), 'Orders Timestamp Information')).AppendLine;


    for Node in TMonitorLists.OrderList.Keys do
      if Assigned(Node) then
      begin
        Data := Node^.GetData;
        if Assigned(Data^.OrderDoc) then
        begin
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Data^.OrderDoc.Id,
                                                         Data^.OrderDoc.OrderIBId,
                                                         Data^.OrderDoc.Symbol,
                                                         Data^.OrderDoc.Exchange,
                                                         Data^.OrderDoc.OrderType.ToString,
                                                         IfThen(Data^.OrderDoc.OrderAction = iabBuy,
                                                         THtmlLib.GetColorTag(Data^.OrderDoc.OrderAction.ToString, clBlue),
                                                         THtmlLib.GetColorTag(Data^.OrderDoc.OrderAction.ToString, clRed)),
                                                         Data^.OrderDoc.Description,
                                                         Data^.OrderDoc.OrderStatusText
                                                         ])))
            .Append(GetStatusItemInfo(Node));
        end;
      end;

    sb.Append(C_HTML_TABLE_CLOSE).Append(C_HTML_BREAK).AppendLine
      .Append(C_HTML_BODY_CLOSE).AppendLine;
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

class function TDocumentInfo.GetInstrumentsWithoutFeed: string;
var
  Data: PTreeData;
  RunNode: PVirtualNode;
  sb: TStringBuilder;
  arrNodes: TArray<PVirtualNode>;
  strOrders: string;
  strFactors: string;
begin
  Result := '';
  arrNodes := TMonitorLists.InstrumentList.GetNodesWithoutFeed;
  if (Length(arrNodes) > 0) then
  begin
    strOrders := '';
    strFactors := '';
    for RunNode in arrNodes do
      if Assigned(RunNode) then
      begin
        Data := RunNode^.GetData;
        if Assigned(Data) then
        begin
          if (Data^.DocType = ntOrder) then
            strOrders := strOrders + THtmlLib.GetTableLineTag(VarArrayOf([Data^.OrderDoc.Id,
                                                                          Data^.OrderDoc.Symbol,
                                                                          Data^.OrderDoc.InstrumentName,
                                                                          Data^.OrderDoc.Exchange,
                                                                          Data^.OrderDoc.Currency,
                                                                          Data^.OrderDoc.Description]))
          else if (Data^.DocType = ntFactor) then
            strFactors := strFactors + THtmlLib.GetTableLineTag(VarArrayOf([Data^.FactorDoc.ContractId,
                                                                            Data^.FactorDoc.Symbol,
                                                                            Data^.FactorDoc.LocalSymbol,
                                                                            Data^.FactorDoc.Currency,
                                                                            Data^.FactorDoc.InstrumentName]));
        end;
      end;

    sb := TStringBuilder.Create;
    try
      sb.Append(C_HTML_HEAD_OPEN)
        .Append(C_STYLE)
        .Append(C_HTML_HEAD_CLOSE)
        .Append(C_HTML_BODY_OPEN)
        .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK).AppendLine;
      if not strFactors.IsEmpty then
      begin
        sb.Append(THtmlLib.GetTableTag(VarArrayOf(['Id', 'Symbol', 'Local Symbol', 'Currency', 'Description']), 'Factors Without Feed'))
          .Append(strFactors)
          .Append(C_HTML_TABLE_CLOSE)
          .Append(C_HTML_BREAK);
      end;
      if not strOrders.IsEmpty then
      begin
        sb.Append(THtmlLib.GetTableTag(VarArrayOf(['Id', 'Symbol', 'Instrument Name', 'Exchange', 'Currency', 'Description']), 'Orders Without Feed'))
          .Append(strOrders)
          .Append(C_HTML_TABLE_CLOSE)
          .Append(C_HTML_BREAK);
      end;

      sb.Append(C_HTML_BREAK)
        .Append(C_HTML_BODY_CLOSE);
      Result := sb.ToString;
    finally
      FreeAndNil(sb);
    end;
  end;
end;

class function TDocumentInfo.GetNodeInfo(const aNode: PVirtualNode; const aWithHTMLHeader: Boolean = True): string;
resourcestring
  rsCondition = 'Compiled value (%f) is "%s" than condition value (%f)';
var
  Algos              : TAlgosDoc;
  AutoTrade          : TAutoTradeInfo;
  Condition          : TConditionDoc;
  Data               : PTreeData;
  Factor             : TFactorDoc;
  Order              : TCustomOrderDoc;
  OrderGroup         : TOrderGroupDoc;
  OrderGroupSet      : TOrderGroupSetDoc;
  Qualifier          : TQualifier;
  sb                 : TStringBuilder;
  TickType           : TIABTickType;

  {function GetAutoTradeIDs(aArray: TAutoTradesArray): string;
  begin
    for var AutoTradeInfo in aArray do
      Result := Result + ', ' + AutoTradeInfo.RecordId.ToString;
  end;}

begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    AutoTrade     := nil;
    OrderGroup    := nil;
    Order         := nil;
    Condition     := nil;
    Algos         := nil;
    Factor        := nil;
    OrderGroupSet := nil;
    Qualifier     := nil;

    case Data^.DocType of
      ntQualifier:
        Qualifier := Data^.Qualifier;
      ntAutoTrade:
        AutoTrade := Data^.AutoTrade;
      ntOrderGroupSet:
        OrderGroupSet := Data^.OrderGroupSetDoc;
      ntOrderGroup:
        if Assigned(Data^.OrderGroupDoc) then
          OrderGroup := Data^.OrderGroupDoc;
      ntOrder:
        if Assigned(Data^.OrderDoc) then
          Order := Data^.OrderDoc;
      ntCondition:
        if Assigned(Data^.ConditionDoc) then
          Condition := Data^.ConditionDoc;
      ntAlgos:
        if Assigned(Data^.AlgosDoc) then
          Algos := Data^.AlgosDoc;
      ntFactor:
        if Assigned(Data^.FactorDoc) then
          Factor := Data^.FactorDoc;
    end;

    sb := TStringBuilder.Create;
    try
      if aWithHTMLHeader then
        sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
          .Append(C_HTML_BODY_OPEN).AppendLine.Append(C_HTML_BREAK);

      sb.Append(DateTimeToStr(Now)).Append(C_HTML_BREAK).AppendLine
        .Append(THtmlLib.GetTableTag(VarArrayOf(['Name', 'Value']), Data^.DocType.ToString + ' Info')).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf([NodeID, Data.NodeID.ToString]))).AppendLine;

      case Data^.DocType of
        ntQualifier:
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Name', Qualifier.Name]))).AppendLine
            {.Append(THtmlLib.GetTableLineTag(VarArrayOf(['State', Qualifier.State.ToString]))).AppendLine}
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['TypeCondition', Qualifier.TypeCondition.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Condition', Qualifier.InequalityCompare.ToString]))).AppendLine
            {.Append(THtmlLib.GetTableLineTag(VarArrayOf(['AutoTradeInfo', GetAutoTradeIDs(Qualifier.AutoTrades)]))).AppendLine}
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Bypass', BoolToStr(Qualifier.Bypass, True)]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['IsCondition', BoolToStr(Qualifier.IsCondition, True)]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Instrument1', Qualifier.Instrument1.SokidInfo.Symbol]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Instrument2', Qualifier.Instrument2.SokidInfo.Symbol]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['StartupDate', FormatDateTime('yyyy.mm.dd hh:nn:ss',Qualifier.StartupDate)]))).AppendLine;
        ntAutoTrade:
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Name', AutoTrade.Name]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['InstanceNum', AutoTrade.InstanceNum.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Active', BoolToStr(AutoTrade.Active, True)]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['AllowSendDuplicateOrder', BoolToStr(AutoTrade.AllowSendDuplicateOrder, True)]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['AutoRefresh', BoolToStr(AutoTrade.AutoRefresh, True)]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['OrdersCount', AutoTrade.CreatedOrdersCount.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['QualifierId', AutoTrade.Qualifier.RecordId.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['QualifierInstance', AutoTrade.QualifierInstance.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['MaxNumberOrder', AutoTrade.MaxNumberOrder.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['MaxRows', AutoTrade.MaxRows.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['OrderAmount', AutoTrade.OrderAmount.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['OrderCurrency', AutoTrade.OrderCurrency]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Note', AutoTrade.Note]))).AppendLine;
        ntOrderGroupSet:
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Id', OrderGroupSet.RecordId.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Name', OrderGroupSet.Name]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Kind', OrderGroupSet.TypeUse.ToString]))).AppendLine;
        ntOrderGroup:
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Id', OrderGroup.RecordId.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Name', OrderGroup.Name]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Kind', OrderGroup.Kind.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Is AutoOrder Group', BoolToStr(OrderGroup.IsAutoOrder, True)]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Is Repetitive', BoolToStr(OrderGroup.IsRepetitive, True)]))).AppendLine;
        ntOrder:
          begin
            sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Contract Id', Order.Id.ToString]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Order Id (IBID)', Order.OrderIBId]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Description', Order.Description]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Broker Name', Order.BrokerType.ToString]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Symbol', Order.Symbol]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Instrument Name', Order.InstrumentName]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['OrderType', Order.OrderType.ToString]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Currency', Order.Currency]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Exchange', Order.Exchange]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Primary Exchange', Order.PrimaryExchange]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Action', Order.OrderAction.ToString]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Order Status', Order.OrderStatusText]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Quantity', Order.Quantity]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Filled', Order.Filled]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Avg Price', Order.AvgPrice]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Last Price', Order.LatestFillPrice]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Limit', Order.Limit]))).AppendLine;

              if (Order is TOrderIBDoc) then
                with TOrderIBDoc(Order) do
                  sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['SecurityType', SecurityType.ToString]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['AuxPrice', AuxPrice]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Local Symbol', LocalSymbol]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['RateType', RateType.ToString]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['LmtPriceOffset', LmtPriceOffset]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['TrailStopPrice', TrailStopPrice]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['TrailingPercent', TrailingPercent]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['ParentIBId', ParentIBId]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['OcaGroupNumber', OcaGroupNumber]))).AppendLine
              else if (Order is TOrderNNDoc) then
                with TOrderNNDoc(Order) do
                  sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['IdentifierList', IdentifierList]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['OpenVolume', OpenVolume]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Price', Price]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Reference', Reference]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['TrailTriggerLimit', TrailTriggerLimit]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['TrailTriggerSendSell', TrailTriggerSendSell]))).AppendLine;

            sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Info', Order.Info]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['MarketList', Order.MarketList]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Is Repetitive', BoolToStr(Order.IsRepetitive, True)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Is Executed', BoolToStr(Order.IsExecuted, True)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('Extended Options:'), C_HTML_NBSP]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Subordination', Order.ExtendedOptions.Subordination.ToString]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Aux Price Relative', Order.ExtendedOptions.AuxPriceRelative.ToString + ' %']))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Limit Price Offset', Order.ExtendedOptions.LimitPriceOffset.ToString + ' %']))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Limit Price Relative', Order.ExtendedOptions.LimitPriceRelative.ToString + ' %']))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['TrailStop Price Relative', Order.ExtendedOptions.TrailStopPriceRelative.ToString + ' %']))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['VolumePercent', Order.ExtendedOptions.VolumePercent.ToString + ' %']))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Open Volume', Order.ExtendedOptions.OpenVolume.ToString]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Basis For Price', Order.ExtendedOptions.BasisForPrice.ToString]))).AppendLine;
          end;
        ntCondition:
          begin
            sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Calculated (actual) value', THtmlLib.GetBoldText(CurrToStr(Condition.CalcValue))]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Condition Value', THtmlLib.GetBoldText(CurrToStr(Condition.CondLimit))]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Init Value', CurrToStr(Condition.InitValue)]))).AppendLine;

            case Condition.CondType of
              ctRealtimeValue, ctRealtimeAndTimeGap:
                begin
                  var tempStr, tempCaption: string;
                  if Condition.IsCondition then
                    tempCaption := THtmlLib.GetColorTag('Condition Test', clGreen)
                  else
                    tempCaption := THtmlLib.GetColorTag('Condition Test', clRed);

                  sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([tempCaption, Format(rsCondition, [Condition.CalcValue,
                                                                                                   Condition.InequalityRt.ToString,
                                                                                                   Condition.CondLimit])]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['TickType1', Condition.TickType1.ToString]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['TickType2', Condition.TickType2.ToString]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Operation', Condition.TypeOperation.ToString]))).AppendLine
                    .Append(THtmlLib.GetTableLineTag(VarArrayOf(['DivisionValue',  CurrToStr(Condition.DivisionValue)]))).AppendLine;
                end;
              ctGradient:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Gradient', CurrToStr(Condition.Gradient)]))).AppendLine;
              ctCorridor:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Cond Width', CurrToStr(Condition.CondWidth)]))).AppendLine
                  .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Gradient', CurrToStr(Condition.Gradient)]))).AppendLine;
              ctGradientAndCorridor:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Cond Width', CurrToStr(Condition.CondWidth)]))).AppendLine
                  .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Gradient', CurrToStr(Condition.Gradient)]))).AppendLine;
            end;

            sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Description', Condition.Description]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Active', BoolToStr(Condition.Active, True)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Priority', PriorityString[Condition.Priority]]))).AppendLine;

            sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Is Condition', BoolToStr(Condition.IsCondition, True)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Is Bypass', BoolToStr(Condition.Bypass, True)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Duration', TimeToStr(Condition.Duration)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Init Time', TimeToStr(Condition.InitTime)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Condition Text', Condition.ToString]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Type', Condition.CondType.ToString]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Kind Creation', KindCreationString[Condition.KindCreation]]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Is BreakUp', BoolToStr(Condition.IsBreakUp, True)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Start Date', DateToStr(Condition.StartDate)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Start Time', TimeToStr(Condition.StartTime)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['End Date', DateToStr(Condition.EndDate)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['End Time', TimeToStr(Condition.EndTime)]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Up Proc', Condition.UpProc.ToString]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Trail Buy', Condition.TrailBuy.ToString]))).AppendLine
              .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Trail Sell', Condition.TrailSell.ToString]))).AppendLine;
          end;
        ntAlgos:
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Name', Algos.Name]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Divisor', Algos.Divisor.ToString]))).AppendLine;
        ntFactor:
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Instrument Name', Factor.InstrumentName]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Symbol', Factor.Symbol]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Local Symbol', Factor.LocalSymbol]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Broker', Factor.BrokerType.ToAbbrevName]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['ContractId', Factor.ContractId.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Currency', Factor.Currency]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Current Value', CurrToStr(Factor.CurrentValue)]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Market List', Factor.MarketList]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Identifier List', Factor.IdentifierList]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Exchange', Factor.Exchange]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Primary Exchange', Factor.PrimaryExchange]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Contract Type', Factor.ContractType]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Tick Type1', Factor.TickType1.ToString]))).AppendLine
            .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Tick Type2', Factor.TickType2.ToString]))).AppendLine;
      end;
      sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['Record Id', Data^.RecordId.ToString]))).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Enabled', BoolToStr(Data^.Enabled, True)]))).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Creation Type', Data^.CreationType.ToString]))).AppendLine
        .Append(C_HTML_TABLE_CLOSE).Append(C_HTML_BREAK).AppendLine;

      if Data^.DocType in [ntCondition, ntAlgos, ntFactor] then
      begin
        sb.Append(THtmlLib.GetTableTag(VarArrayOf(['Name', 'Value']), 'Price (Actual Values - ' + TimeToStr(Now) + ')')).AppendLine;

        for TickType := ttBidSize to ttOptionHistoricalVol do
          case Data^.DocType of
//            ntOrderGroup:
//              if Assigned(Data^.OrderGroupDoc) then
//                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([TickType.ToString, Data^.OrderGroupDoc.TickValue[TickType]]))).AppendLine;
//            ntOrder:
//              if Assigned(Data^.OrderDoc) then
//                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([TickType.ToString, Data^.OrderDoc.LastPrice]))).AppendLine;
            ntCondition:
              if Assigned(Data^.ConditionDoc) then
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([TickType.ToString, Data^.ConditionDoc.TickValue[TickType]]))).AppendLine;
            ntAlgos:
              if Assigned(Data^.AlgosDoc) then
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([TickType.ToString, Data^.AlgosDoc.TickValue[TickType]]))).AppendLine;
            ntFactor:
              if Assigned(Data^.FactorDoc) then
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([TickType.ToString, TMonitorLists.PriceCache.GetLastPrice(Data^.FactorDoc.ContractId, TickType)]))).AppendLine;
          end;
        sb.Append(C_HTML_TABLE_CLOSE).AppendLine;
        if aWithHTMLHeader then
          sb.Append(C_HTML_BODY_CLOSE).AppendLine;
      end;
      Result := sb.ToString;
    finally
      FreeAndNil(sb);
    end;
  end;
end;

class function TDocumentInfo.GetPriceHistory(const aNode: PVirtualNode): string;
var
  ContractID : Integer;
  Data       : PTreeData;
begin
  Result := '';
  if Assigned(aNode) then
  begin
    Data       := aNode^.GetData;
    ContractID := 0;
    case Data^.DocType of
      ntOrder:
        if Assigned(Data^.OrderDoc) then
          ContractID := Data^.OrderDoc.Id;
      ntFactor:
        if Assigned(Data^.FactorDoc) then
          ContractID := Data^.FactorDoc.ContractId;
    end;
    if (ContractID > 0) then
      Result := GetPriceHistory(ContractID, nil);
  end;
end;

class function TDocumentInfo.GetPriceHistory(const aContractId: Integer; const aFilterFunc: TFilterFunc): string;
var
  InstInfo  : string;
  PriceList : TPriceList;
  sb        : TStringBuilder;
  Row       : Integer;
  arrPrices : TArray<TPrice>;
begin
  Result := '';
  if (aContractID > 0) then
  begin
    if SokidList.ContainsKey(aContractID) then
      InstInfo := SokidList.Items[aContractID].Symbol + ' (' + SokidList.Items[aContractID].Name + ') ';

    sb := TStringBuilder.Create;
    try
      sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
        .Append(C_HTML_BODY_OPEN).AppendLine
        .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK).AppendLine
        .Append(THtmlLib.GetTableTag(VarArrayOf(['n/n',
                                                 'TimeStamp',
                                                 'Last',
                                                 'LastSize',
                                                 'Bid',
                                                 'BidSize',
                                                 'Ask',
                                                 'AskSize',
                                                 'Close',
                                                 'Open',
                                                 'Low',
                                                 'High',
                                                 'Volume',
                                                 'Historical Volatility',
                                                 'History'
                                                 ]), 'Price Info for ' + InstInfo + aContractID.ToString)).AppendLine;
      PriceList := TMonitorLists.PriceCache.GetPriceList(aContractID);
      if Assigned(PriceList) then
      begin
        Row := 0;
        arrPrices := PriceList.GetLastPrices(aFilterFunc);
        for var Price in arrPrices do
          if (Price.TickType in [ttLast, ttLastSize, ttBid, ttBidSize, ttAsk, ttAskSize, ttClose, ttOpen, ttLow, ttHigh, ttVolume, ttOptionHistoricalVol]) then
          begin
            Inc(Row);
            case Price.TickType of
              ttLast:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               Price.Value,     //ttLast
                                                               0,               //ttLastSize
                                                               0,               //ttBid
                                                               0,               //ttBidSize
                                                               0,               //ttAsk
                                                               0,               //ttAskSize
                                                               0,               //ttClose
                                                               0,               //ttOpen
                                                               0,               //ttLow
                                                               0,               //ttHigh
                                                               0,               //ttVolume
                                                               0,               //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
              ttLastSize:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               0,               //ttLast
                                                               Price.Value,     //ttLastSize
                                                               0,               //ttBid
                                                               0,               //ttBidSize
                                                               0,               //ttAsk
                                                               0,               //ttAskSize
                                                               0,               //ttClose
                                                               0,               //ttOpen
                                                               0,               //ttLow
                                                               0,               //ttHigh
                                                               0,               //ttVolume
                                                               0,               //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
              ttBid:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               0,               //ttLast
                                                               0,               //ttLastSize
                                                               Price.Value,     //ttBid
                                                               0,               //ttBidSize
                                                               0,               //ttAsk
                                                               0,               //ttAskSize
                                                               0,               //ttClose
                                                               0,               //ttOpen
                                                               0,               //ttLow
                                                               0,               //ttHigh
                                                               0,               //ttVolume
                                                               0,               //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
              ttBidSize:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               0,               //ttLast
                                                               0,               //ttLastSize
                                                               0,               //ttBid
                                                               Price.Value,     //ttBidSize
                                                               0,               //ttAsk
                                                               0,               //ttAskSize
                                                               0,               //ttClose
                                                               0,               //ttOpen
                                                               0,               //ttLow
                                                               0,               //ttHigh
                                                               0,               //ttVolume
                                                               0,               //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
              ttAsk:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               0,               //ttLast
                                                               0,               //ttLastSize
                                                               0,               //ttBid
                                                               0,               //ttBidSize
                                                               Price.Value,     //ttAsk
                                                               0,               //ttAskSize
                                                               0,               //ttClose
                                                               0,               //ttOpen
                                                               0,               //ttLow
                                                               0,               //ttHigh
                                                               0,               //ttVolume
                                                               0,               //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
              ttAskSize:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               0,               //ttLast
                                                               0,               //ttLastSize
                                                               0,               //ttBid
                                                               0,               //ttBidSize
                                                               0,               //ttAsk
                                                               Price.Value,     //ttAskSize
                                                               0,               //ttClose
                                                               0,               //ttOpen
                                                               0,               //ttLow
                                                               0,               //ttHigh
                                                               0,               //ttVolume
                                                               0,               //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
              ttClose:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               0,               //ttLast
                                                               0,               //ttLastSize
                                                               0,               //ttBid
                                                               0,               //ttBidSize
                                                               0,               //ttAsk
                                                               0,               //ttAskSize
                                                               Price.Value,     //ttClose
                                                               0,               //ttOpen
                                                               0,               //ttLow
                                                               0,               //ttHigh
                                                               0,               //ttVolume
                                                               0,               //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
              ttOpen:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               0,               //ttLast
                                                               0,               //ttLastSize
                                                               0,               //ttBid
                                                               0,               //ttBidSize
                                                               0,               //ttAsk
                                                               0,               //ttAskSize
                                                               0,               //ttClose
                                                               Price.Value,     //ttOpen
                                                               0,               //ttLow
                                                               0,               //ttHigh
                                                               0,               //ttVolume
                                                               0,               //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
              ttLow:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               0,               //ttLast
                                                               0,               //ttLastSize
                                                               0,               //ttBid
                                                               0,               //ttBidSize
                                                               0,               //ttAsk
                                                               0,               //ttAskSize
                                                               0,               //ttClose
                                                               0,               //ttOpen
                                                               Price.Value,     //ttLow
                                                               0,               //ttHigh
                                                               0,               //ttVolume
                                                               0,               //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
              ttHigh:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               0,               //ttLast
                                                               0,               //ttLastSize
                                                               0,               //ttBid
                                                               0,               //ttBidSize
                                                               0,               //ttAsk
                                                               0,               //ttAskSize
                                                               0,               //ttClose
                                                               0,               //ttOpen
                                                               0,               //ttLow
                                                               Price.Value,     //ttHigh
                                                               0,               //ttVolume
                                                               0,               //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
              ttVolume:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               0,               //ttLast
                                                               0,               //ttLastSize
                                                               0,               //ttBid
                                                               0,               //ttBidSize
                                                               0,               //ttAsk
                                                               0,               //ttAskSize
                                                               0,               //ttClose
                                                               0,               //ttOpen
                                                               0,               //ttLow
                                                               0,               //ttHigh
                                                               Price.Value,     //ttVolume
                                                               0,               //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
              ttOptionHistoricalVol:
                sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Row,
                                                               FormatDateTime('hh:nn:ss.zzz', Price.TimeStamp),
                                                               0,               //ttLast
                                                               0,               //ttLastSize
                                                               0,               //ttBid
                                                               0,               //ttBidSize
                                                               0,               //ttAsk
                                                               0,               //ttAskSize
                                                               0,               //ttClose
                                                               0,               //ttOpen
                                                               0,               //ttLow
                                                               0,               //ttHigh
                                                               0,               //ttVolume
                                                               Price.Value,     //ttOptionHistoricalVol
                                                               IfThen(Price.IsHistorical, '✕', C_HTML_NBSP)
                                                               ]))).AppendLine;
            end;
          end;
        sb.Append(C_HTML_TABLE_CLOSE).AppendLine
          .Append(C_HTML_BODY_CLOSE).AppendLine;
      end;
      Result := sb.ToString;
    finally
      FreeAndNil(sb);
    end;
  end;
end;

class function TDocumentInfo.GetPriceInformation: string;

  function GetSymbol(aConId: Integer): string; inline;
  begin
    Result := SokidList.GetItem(aConId).Symbol;
  end;

  function GetLocalSymbol(aConId: Integer): string; inline;
  begin
    Result := SokidList.GetItem(aConId).LocalSymbol;
  end;

var
  sb : TStringBuilder;
  pair: TPair<Integer, TPriceList>;
  DateBegin: TDateTime;
  DateEnd: TDateTime;
  Volatility: Double;
  LastPrice: Double;
begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    if General.AdjustmentCoefficients.UseVolatilityCoef then
    begin
      DateEnd := Now;
      DateBegin := IncHour(DateEnd, -General.AdjustmentCoefficients.NumberOfHours);

      sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
        .Append(C_HTML_BREAK)
        .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK).AppendLine
        .Append(THtmlLib.GetTableTag(VarArrayOf(['Symbol',
                                                 'Local Symbol',
                                                 'Contract Id',
                                                 'Init Time',
                                                 'First Feed',
                                                 'Request To Response',
                                                 'Last Use',
                                                 'Last Price',
                                                 'Historical Volatility',
                                                 'Volatility ' + General.AdjustmentCoefficients.NumberOfHours.ToString + 'h.',
                                                 'VolCoef (' + (General.AdjustmentCoefficients.Volatility / 100).ToString + ')',
                                                 'PriceCoef (' + (General.AdjustmentCoefficients.Price / 100).ToString + ')',
                                                 'Last Price * VolCoef * PriceCoef'
                                                  ]), 'Price Information (Ticktype=Last Price)')).AppendLine;

      for pair in TMonitorLists.PriceCache do
      begin
        Volatility := DMod.CalculateVolatility(pair.Key, DateBegin, DateEnd);
        LastPrice := pair.Value.LastPrice[ttLast];
        if (pair.Value.FirstTimeStamp = 0) then
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText(GetSymbol(pair.Key)),
                                                         GetLocalSymbol(pair.Key),
                                                         pair.Key.ToString,
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.InitTimeStamp),
                                                         THtmlLib.GetColorTag(FormatDateTime('hh.nn.ss:zzz', pair.Value.FirstTimeStamp), clRed),
                                                         C_HTML_NBSP,
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.ResponseTimeStamp),
                                                         THtmlLib.GetColorTag(LastPrice.ToString, clMaroon),
                                                         FormatFloat(C_CURRENCY_FORMAT, pair.Value.LastPrice[ttOptionHistoricalVol]),
                                                         FormatFloat(C_CURRENCY_FORMAT, Volatility),
                                                         THtmlLib.GetColorTag(FormatFloat(C_CURRENCY_FORMAT, TOrderUtils.GetVolatilityCoef(Volatility)), clNavy),
                                                         THtmlLib.GetColorTag(FormatFloat(C_CURRENCY_FORMAT, TOrderUtils.GetPriceCoef(LastPrice)), clNavy),
                                                         THtmlLib.GetColorTag(FormatFloat(C_CURRENCY_FORMAT, TOrderUtils.GetVolatilityCoef(Volatility) * TOrderUtils.GetPriceCoef(LastPrice) * LastPrice), clTeal)
                                                         ]))).AppendLine
        else
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([GetSymbol(pair.Key),
                                                         GetLocalSymbol(pair.Key),
                                                         pair.Key.ToString,
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.InitTimeStamp),
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.FirstTimeStamp),
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.InitTimeStamp - pair.Value.FirstTimeStamp),
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.ResponseTimeStamp),
                                                         THtmlLib.GetColorTag(LastPrice.ToString, clMaroon),
                                                         FormatFloat(C_CURRENCY_FORMAT, pair.Value.LastPrice[ttOptionHistoricalVol]),
                                                         FormatFloat(C_CURRENCY_FORMAT, Volatility),
                                                         THtmlLib.GetColorTag(FormatFloat(C_CURRENCY_FORMAT, TOrderUtils.GetVolatilityCoef(Volatility)), clNavy),
                                                         THtmlLib.GetColorTag(FormatFloat(C_CURRENCY_FORMAT, TOrderUtils.GetPriceCoef(LastPrice)), clNavy),
                                                         THtmlLib.GetColorTag(FormatFloat(C_CURRENCY_FORMAT, TOrderUtils.GetVolatilityCoef(Volatility) * TOrderUtils.GetPriceCoef(LastPrice) * LastPrice), clTeal)
                                                         ]))).AppendLine
      end;
    end
    else
    begin
      sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
        .Append(C_HTML_BREAK)
        .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK).AppendLine
        .Append(THtmlLib.GetTableTag(VarArrayOf(['Symbol',
                                                 'Contract Id',
                                                 'Init Time',
                                                 'First Feed',
                                                 'Request To Response',
                                                 'Last Use'
                                                 ]), 'Price Information (Ticktype=Last Price)')).AppendLine;
      for pair in TMonitorLists.PriceCache do
      begin
        if (pair.Value.FirstTimeStamp = 0) then
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText(GetSymbol(pair.Key)),
                                                         pair.Key.ToString,
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.InitTimeStamp),
                                                         THtmlLib.GetColorTag(FormatDateTime('hh.nn.ss:zzz', pair.Value.FirstTimeStamp), clRed),
                                                         C_HTML_NBSP,
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.ResponseTimeStamp)
                                                         ]))).AppendLine
        else
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([GetSymbol(pair.Key),
                                                         pair.Key.ToString,
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.InitTimeStamp),
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.FirstTimeStamp),
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.InitTimeStamp - pair.Value.FirstTimeStamp),
                                                         FormatDateTime('hh.nn.ss:zzz', pair.Value.ResponseTimeStamp)
                                                         ]))).AppendLine;
      end;
    end;
    sb.Append(C_HTML_TABLE_CLOSE).Append(C_HTML_BREAK).AppendLine
      .Append(C_HTML_BODY_CLOSE).AppendLine;

    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

class function TDocumentInfo.GetRuleInformation(aNode: PVirtualNode): string;
var
  Data : PTreeData;
  sb   : TStringBuilder;
  ArrMarketRule: TArrMarketRule;
  RuleItem: TRuleItem;
  SokidInfo: TSokidInfo;
begin
  Result := '';
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data^.OrderDoc) then
    begin
      SokidInfo := SokidList.GetItem(Data^.OrderDoc.Id);
      if not (SokidInfo.GetSecurityType in [stFuture]) then
        SokidInfo.MinimumTick := 0;
      if (Data^.OrderDoc.MarketList.IsEmpty) then
        Data^.OrderDoc.MarketList := SokidInfo.MarketRuleIds;

      sb := TStringBuilder.Create;
      try
        sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
          .Append(C_HTML_BREAK)
          .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK).Append(C_HTML_BREAK).AppendLine;

        if (SokidInfo.MinimumTick > 0) then
        begin
          SetLength(ArrMarketRule, 1);
          ArrMarketRule[0].LowEdge    := 0;
          ArrMarketRule[0].Increment := SokidInfo.MinimumTick;
          sb.Append(THtmlLib.GetCenterText(THtmlLib.GetColorTag('MinimumTick is used for calculation', clRed))).Append(C_HTML_BREAK);
        end
        else
        begin
          ArrMarketRule := IABClient.MarketRuleList.GetMarketRules(Data^.OrderDoc.MarketList);
          if (Length(ArrMarketRule) = 0) then
          begin
            ArrMarketRule := IABClient.MarketRuleList.GetDefaultMarketRules;
            sb.Append(THtmlLib.GetCenterText(THtmlLib.GetColorTag('PriceRule not found. Use default rule', clRed))).Append(C_HTML_BREAK);
          end;
        end;

        sb.Append(THtmlLib.GetTableTag(VarArrayOf(['Range',
                                                   'Increment'
                                                   ]), 'Applicable Price Rule For ' + THtmlLib.GetBoldText(Data^.OrderDoc.Symbol) + '<br>(Exchange:' + Data^.OrderDoc.Exchange + ', MarketId:' + Data^.OrderDoc.MarketList + ')')).AppendLine;

        for RuleItem in ArrMarketRule do
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf(['price > ' + RuleItem.LowEdge.ToString, RuleItem.Increment]))).AppendLine;

        sb.Append(C_HTML_TABLE_CLOSE).Append(C_HTML_BREAK).AppendLine
          .Append(C_HTML_BODY_CLOSE).AppendLine;
        Result := sb.ToString;
      finally
        FreeAndNil(sb);
      end;
    end;
  end;
end;

class function TDocumentInfo.GetSubscribersInformation: string;
var
  sb: TStringBuilder;

  function GetPublisherInfo(const aPublisher: TCustomPublisher; const aIID: TGUID): string;
  var
    Item: TObject;
    ci: ICustomInterface;
  begin
    sb.Append(C_HTML_BREAK).AppendLine
      .Append(THtmlLib.GetTableTag(VarArrayOf(['n/n',
                                               'Class Name',
                                               'Unit Name'
                                               ]), aPublisher.ClassName));

    for var i := 0 to aPublisher.Count - 1 do
    begin
      Item := aPublisher.Items[i];
      if Assigned(Item) then
        if Supports(Item, aIID, ci) then
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([i + 1, Item.ClassName, Item.UnitName]))).AppendLine;
    end;
    sb.Append(C_HTML_TABLE_CLOSE)
      .Append(C_HTML_BREAK);
  end;

begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE)
      .Append(C_HTML_BODY_OPEN)
      .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK)
      .Append(THtmlLib.GetH2('Subscribers Information'));

    GetPublisherInfo(TPublishers.FeedPublisher, IUpdateFeeds);
    GetPublisherInfo(TPublishers.ScannerPublisher, IScanner);
    GetPublisherInfo(TPublishers.InstrumentSpecDetailsPublisher, IInstrumentSpecDetails);
    GetPublisherInfo(TPublishers.ConnectionStatePublisher, IConnectionState);
    GetPublisherInfo(TPublishers.OrderStatePublisher, IOrderState);
    GetPublisherInfo(TPublishers.TickByTickPublisher, IOnTickByTick);
    GetPublisherInfo(TPublishers.LogPublisher, ILogger);
    GetPublisherInfo(TPublishers.HistoricalDataPublisher, IHistoricalData);
    GetPublisherInfo(TPublishers.SecurityDefinitionOptionalParameterPublisher, ISecurityDefinitionOptionalParameter);
    GetPublisherInfo(TPublishers.TickOptionComputationPublisher, ITickOptionComputation);
    GetPublisherInfo(TPublishers.OrderStatusPublisher, IOrderStatus);
    GetPublisherInfo(TPublishers.OrderStatusPublisher, IOrderStatus);
    GetPublisherInfo(TPublishers.ErrorPublisher, IError);
    GetPublisherInfo(AutoTradesControllerPublisher, IAutoTradesController);
    GetPublisherInfo(QualifiersControllerPublisher, IQualifiersController);
    sb.Append(C_HTML_BODY_CLOSE).AppendLine;
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

class function TDocumentInfo.GetScanColumnsInfo(aColumns: TAutoTradeColumns): string;
var
  sb: TStringBuilder;
begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE)
      .Append(C_HTML_BODY_OPEN)
      .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK)
      .Append(C_HTML_BREAK)
      .Append(THtmlLib.GetTableTag(VarArrayOf(['ColumnId',
                                               'RecordId',
                                               'Caption',
                                               'Weight',
                                               'Position',
                                               'Width',
                                               'Info'
                                               ]), 'Columns Information'));

    for var ColumnsInfo in aColumns.Values do
      sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([ColumnsInfo.ColumnId,
                                                     ColumnsInfo.RecordId,
                                                     ColumnsInfo.Caption,
                                                     ColumnsInfo.Weight,
                                                     ColumnsInfo.ColumnPosition,
                                                     ColumnsInfo.ColumnWidth,
                                                     ColumnsInfo.ToList.Replace(sLineBreak, '<br>')
                                                     ]))).AppendLine;
    sb.Append(C_HTML_TABLE_CLOSE)
      .Append(C_HTML_BODY_CLOSE).AppendLine;
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

class function TDocumentInfo.GetPrecautionarySettingsInformation: string;
var
  sb: TStringBuilder;
  PreArr: TPrecautionarySettingArray;
begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE)
      .Append(C_HTML_BODY_OPEN)
      .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK)
      .Append(C_HTML_BREAK);

    for var st := Low(TIABSecurityType) to High(TIABSecurityType) do
      if General.PrecautionarySettings.TryGetValue(st, PreArr) then
      begin
        sb.Append(THtmlLib.GetTableTag(VarArrayOf(['Name',
                                                   'Value'
                                                  ]), 'Security Type: ' + st.ToString));
        for var PreType := Low(PreArr) to High(PreArr) do
          sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([PreType.ToString, PreArr[PreType]])));
        sb.Append(C_HTML_TABLE_CLOSE);
      end;
    sb.Append(C_HTML_BODY_CLOSE).AppendLine;

    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

end.
