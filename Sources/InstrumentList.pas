unit InstrumentList;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  Vcl.Forms, Winapi.Messages, DebugWriter, IABSocketAPI, IABSocketAPI_const, VirtualTrees, Utils,
  System.Threading, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Types, Entity.Price, Generics.Helper,
  Publishers.Interfaces, ArrayHelper, Common.Types, Global.Types, XmlFiles, Publishers, System.Math,
  NNfunctions.Types;
{$ENDREGION}

type
  TDocumentQueue = class(TList<PVirtualNode>)
  public
    procedure AddItem(aNode: PVirtualNode);
    procedure DeleteItem(aNode: PVirtualNode);
  end;

  TInstrumentItem = class
  public
    Id       : Integer;
    NodeList : TList<PVirtualNode>;
    procedure SetValue(const aDate: TDateTime; const aValue: Currency; const aTickType: TIABTickType);
    function ToString: string; override;
    constructor Create;
    destructor Destroy; override;
  end;

  TPriceChangeEvent = procedure(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime) of object;
  TKeyChangeNotify = procedure(Sender: TObject; const aId: Integer; const Action: TCollectionNotification) of object;
  TFactorList = class(TInterfacedObjectDictionary<Integer, TInstrumentItem>, IUpdateFeeds)
  private
    FPriceChangeEvent: TPriceChangeEvent;
    FDocumentQueue: TDocumentQueue;
  public
    function AddNode(aId: Integer; aNode: PVirtualNode): Boolean;
    function GetCountNodesWithoutFeed: Integer;
    //implementation ICustomInterface
    function GetInstance: TObject;
    function GetItem(aId: Integer): TInstrumentItem;
    function GetNodesWithoutFeed: TArray<PVirtualNode>;
    function IsNodeExists(aId: Integer): Boolean;
    function ToNodeArray: TNodeArray;
    function ToString: string; override;
    procedure DeleteNode(aId: Integer; aNode: PVirtualNode); overload;
    procedure DeleteNode(aNode: PVirtualNode); overload;

    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    procedure SetValue(const aId: Integer; const aDate: TDateTime; const aValue: Currency; const TickType: TIABTickType);
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer = 0);
    destructor Destroy; override;

    property DocumentQueue: TDocumentQueue    read FDocumentQueue    write FDocumentQueue;
    property DoPriceChange: TPriceChangeEvent read FPriceChangeEvent write FPriceChangeEvent;
  end;

  TOrderItem = class(TInstrumentItem)
  public type
    TStatusItem = class
    public
      AvgFillPrice : Double;
      LastPrice    : Currency;
      Event        : string;
      Filled       : Double;
      Info         : string;
      Remaining    : Double;
      Status       : TIABOrderState;
      TimeStamp    : TDateTime;
      constructor Create(aFilled, aRemaining: Double; aLastPrice: Currency; aInfo, aEvent: string; aStatus: TIABOrderState; aAvgFillPrice: Double);
    end;

    TAPIOrderItem = class
    public
      StatusList: TObjectList<TStatusItem>;
      constructor Create;
      destructor Destroy; override;
    end;
  public
    APIOrderList     : TObjectDictionary<Integer, TAPIOrderItem>;
    LastAvgFillPrice : Double;
    LastFilled       : Double;
    LastInfo         : string;
    LastRemaining    : Double;
    LastStatus       : TIABOrderState;
    LastFreezePrice  : Double;
    function ToXml(const aOrderId: Integer = 0): string;
    procedure Clear;
    procedure FromXml(const aText: string);
    constructor Create;
    destructor Destroy; override;
  end;

  TOrderList = class(TObjectDictionary<PVirtualNode, TOrderItem>)
  private
    FLinkedNodes: TObjectDictionary<PVirtualNode, PVirtualNode>;
    procedure SetStatusNode(aNodeOrder: PVirtualNode; aStatus: TIABOrderState; aInfo: string);
  public type
    TKindData = (kdAvgFillPrice, kdFilled, kdRemaining, kdStatus);
  public
    destructor Destroy; override;
    function GetLastOrderValues(aNodeOrder: PVirtualNode; aKindData: TKindData): string;
    function GetMasterLinkedNode(ANode: PVirtualNode): Pointer;
    function GetNodeOrder(aOrderId: Integer): PVirtualNode;
    function GetStatusText(aNodeOrder: PVirtualNode): string;
    function GetStatusArray(aNodeOrder: PVirtualNode): TArrayRecord<TOrderItem.TStatusItem>;
    function IsNodeLinked(aNode: PVirtualNode): Boolean;
    function ToString: string; override;
    procedure AddOrder(aNodeOrder: PVirtualNode); overload;
    procedure AddOrder(aNodeOrder: PVirtualNode; aInstumentNode: PVirtualNode); overload;
    procedure AddStatus(aNodeOrder: PVirtualNode; aOrderId: Integer; aFilled, aRemaining: Double; aInfo, aEvent: string; aCodeError: Integer; aStatus: TIABOrderState; aAvgFillPrice: Double); overload;
    procedure AddStatus(aOrderTempId: Integer; aFilled, aRemaining: Double; aInfo, aEvent: string; aCodeError: Integer; aStatus: TIABOrderState; aAvgFillPrice: Double); overload;
    procedure AddStatus(aOrder: TIABOrder; aInfo, aEvent: string; aCodeError: Integer; aStatus: TIABOrderState); overload;
    procedure AfterConstruction; override;
    procedure Clear; overload;
    procedure DeleteOrder(aNodeOrder: PVirtualNode);
    procedure SetLastStatus(aNodeOrder: PVirtualNode; aStatus: TIABOrderState);
  end;

  TAlgosItem = class
  public
    function ToString: string; override;
    constructor Create;
    destructor Destroy; override;
  end;

  TAlgosList = class(TObjectDictionary<PVirtualNode, TAlgosItem>)
  public
    function ToString: string; override;
    procedure AddItem(aNodeAlgos: PVirtualNode);
    procedure DeleteAlgos(aNodeAlgos: PVirtualNode);
  end;

  TAccountList = class(TInterfacedObjectDictionary<string, PVirtualNode>, IUpdateFeeds)
  private
    FPriceChangeEvent: TPriceChangeEvent;
  public
    //implementation ICustomInterface
    function GetInstance: TObject;
    function ToString: string; override;
    procedure AddAccount(aSymbol: string; aNodeAccount: PVirtualNode);
    procedure DeleteAccount(aNodeAccount: PVirtualNode);
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    property DoPriceChange: TPriceChangeEvent read FPriceChangeEvent write FPriceChangeEvent;
  end;

  TPrice = record
    TimeStamp    : TDateTime;
    IsHistorical : Boolean;
    TickType     : TIABTickType;
    Value        : Currency;
  end;

  TFilterFunc = reference to function(const aPrice: TPrice): Boolean;
  TPriceList = class(TList<TPrice>)
  private
    FComparer: IComparer<TPrice>;
  public
    Symbol: string;
    Currency: string;
    Exchange: string;
    LastPrice: array [ttBidSize .. ttMarket] of Currency;
    ReceivedTick: array [ttBidSize .. ttMarket] of Boolean;
    LastTimeStamp: TDateTime;
    FirstTimeStamp: TDateTime;
    InitTimeStamp: TDateTime;
    ResponseTimeStamp: TDateTime;
    function GetLastPrices(const aFilterFunc: TFilterFunc): TArray<TPrice>;
    function GetLastPricesBroken(const aFilterFunc: TFilterFunc): TArray<TPrice>;
    constructor Create;
    destructor Destroy; override;
  end;

  TPriceCache = class(TObjectDictionary<Integer, TPriceList>)
  const
    C_MIN_PRICE = 0.00001;
  private
    FThread: TThreadPriceCache;
  public
    function GetLastPrice(aDataId: Integer; aTickType: TIABTickType): Currency; overload; inline;
    function GetLastPrice(aSymbol, aCurrency, aExchange: string; aTickType: TIABTickType): Currency; overload;
    function GetPriceList(aDataId: Integer): TPriceList;
    procedure AddPrice(aDataId: Integer; aHistoricalChartData: TIABHistoricalChartData); overload;
    procedure AddPrice(aDataId: Integer; aTickType: TIABTickType; aValue: Currency; aTimeStamp: TDateTime; aIsHistorical: Boolean = False); overload;
    procedure InitPrice(aDataId: Integer; aSymbol, aCurrency, aExchange: string; aLastPrice: Double);
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer = 0);
    destructor Destroy; override;
    class var
      PriceCache: TPriceCache;
  end;

  TCurrencyCache = class(TDictionary<string, Integer>)
  private
    FPriceCache: TPriceCache;
  public
    function GetLastExchange(const aMainCurrency, aCurrency: string): Double; overload;
    function GetLastExchange(const aCurrency: string): Double; overload;
    property PriceCache: TPriceCache read FPriceCache write FPriceCache;
    class var
      CurrencyCache: TCurrencyCache;
  end;

  TPriceDictionary = TObjectDictionary<TDateTime, Double>;
  TConditionHistData = class(TObjectDictionary<Integer, TPriceDictionary>)
  type
    TCalcPrice = record
      TimeStamp : TDateTime;
      Price     : Double;
    end;
  private
    FArrayPrice: TArray<TCalcPrice>;
  public
    procedure AddPrice(aDataId: Integer; aTimeStamp: TDateTime; aPrice: Currency);
    function GetResultValue: TArray<TCalcPrice>;
  end;

  TOrderQueue = class(TQueue<PVirtualNode>)
  private
    FTask: ITask;
    procedure CreateTask;
  public
    destructor Destroy; override;
    procedure AddItem(aNode: PVirtualNode);
  end;

  TMonitorLists = class
  private
  public
    class var
      AlgosList           : TAlgosList;
      InstrumentChildList : TFactorList;
      InstrumentList      : TFactorList;
      OrderList           : TOrderList;
      OrderQueue          : TOrderQueue;
    class function PriceCache: TPriceCache;
    class function CurrencyCache: TCurrencyCache;
    class procedure CreateLists;
    class procedure DestroyLists;
  end;

implementation

uses
  Monitor.Interfaces, Monitor.Types;

{ TInstrumentItem }

constructor TInstrumentItem.Create;
begin
  NodeList := TList<PVirtualNode>.Create;
end;

destructor TInstrumentItem.Destroy;
begin
  NodeList.Clear;
  FreeAndNil(NodeList);
end;

procedure TInstrumentItem.SetValue(const aDate: TDateTime; const aValue: Currency; const aTickType: TIABTickType);
var
  Data: PTreeData;
  CurNode: PVirtualNode;
begin
  for CurNode in NodeList do
    if Assigned(CurNode) then
    begin
      Data := CurNode^.GetData;
      if Assigned(Data) then
      begin
        if Assigned(Data^.FactorDoc) then
        begin
          if (Data.FactorDoc.TickType1 = aTickType) then
            Data.FactorDoc.LastPrice1 := aValue;
          if (Data.FactorDoc.TickType2 = aTickType) then
            Data.FactorDoc.LastPrice2 := aValue;
        end
        else if Assigned(Data^.OrderDoc) then
          Data^.OrderDoc.LastPrice := aValue;
      end;
    end;
end;

function TInstrumentItem.ToString: string;
var
  CurNode: PVirtualNode;
begin
  Result := '';
  for CurNode in NodeList do
  begin
    if not Result.IsEmpty then
      Result := Result + ', ';
    Result := Result + IntToStr(Integer(CurNode));
  end;
  Result := Id.ToString + ':(' + Result + ')';
end;

{ TInstrumentList }

constructor TFactorList.Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer = 0);
begin
  inherited Create(Ownerships, ACapacity);
  FDocumentQueue := TDocumentQueue.Create;
end;

destructor TFactorList.Destroy;
begin
  FDocumentQueue.Clear;
  FreeAndNil(FDocumentQueue);
  inherited;
end;

procedure TFactorList.DeleteNode(aId: Integer; aNode: PVirtualNode);
begin
  if Self.ContainsKey(aId) then
   if Self[aId].NodeList.Contains(aNode) then
     Self[aId].NodeList.Remove(aNode);
  FDocumentQueue.DeleteItem(aNode);
end;

procedure TFactorList.DeleteNode(aNode: PVirtualNode);
var
  Item: TInstrumentItem;
begin
  for Item in Self.Values do
    if Item.NodeList.Contains(aNode) then
    begin
      Item.NodeList.Remove(aNode);
      Break;
    end;
  FDocumentQueue.DeleteItem(aNode);
end;

function TFactorList.GetCountNodesWithoutFeed: Integer;
begin
  Result := FDocumentQueue.Count;
end;

function TFactorList.GetInstance: TObject;
begin
  Result := Self;
end;

function TFactorList.GetItem(aId: Integer): TInstrumentItem;
begin
  Result := nil;
  if Assigned(Self) and Self.ContainsKey(aId) then
    Result := Self[aId];
end;

function TFactorList.GetNodesWithoutFeed: TArray<PVirtualNode>;
begin
  Result := FDocumentQueue.ToArray;
end;

function TFactorList.IsNodeExists(aId: Integer): Boolean;
begin
  if Self.ContainsKey(aId) then
    Result := Self[aId].NodeList.Count > 0
  else
    Result := False;
end;

procedure TFactorList.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
begin
  if Assigned(FPriceChangeEvent) then
    FPriceChangeEvent(Sender, Id, TickType, Value, TimeStamp);
end;

procedure TFactorList.SetValue(const aId: Integer; const aDate: TDateTime; const aValue: Currency; const TickType: TIABTickType);
var
  CurNode: PVirtualNode;
begin
  if Self.ContainsKey(aId) then
  begin
    Self[aId].SetValue(aDate, aValue, TickType);
    if (aValue > 0) then
      for CurNode in Self[aId].NodeList do
        FDocumentQueue.DeleteItem(CurNode);
  end;
end;

function TFactorList.ToNodeArray: TNodeArray;
var
  Index: Integer;
begin
  Result := [];
  Index := 0;
  for var Item in Self.Values do
  begin
    SetLength(Result, Length(Result) + Item.NodeList.Count);
    for var Node in Item.NodeList do
    begin
      Result[Index] := Node;
      Inc(Index);
    end;
  end;
end;

function TFactorList.ToString: string;
var
  Item: TInstrumentItem;
begin
  Result := '';
  for Item in Self.Values do
  begin
    if not Result.IsEmpty then
      Result := Result + ', ';
    Result := Result + Item.ToString;
  end;
end;

function TFactorList.AddNode(aId: Integer; aNode: PVirtualNode): Boolean;
var
  Item: TInstrumentItem;
begin
  Result := False;
  if not Self.ContainsKey(aId) then
  begin
    Item := TInstrumentItem.Create;
    Item.Id := aId;
    Self.Add(aId, Item);
    FDocumentQueue.AddItem(aNode);
  end;
  if Self[aId].NodeList.IndexOf(aNode) = -1 then
    Result := Self[aId].NodeList.Add(aNode) > -1;
end;

{ TOrderList }

procedure TOrderList.AddOrder(aNodeOrder: PVirtualNode);
var
  OrderItem: TOrderItem;
  Data: PTreeData;
begin
  if not Self.ContainsKey(aNodeOrder) then
  begin
    OrderItem := TOrderItem.Create;
    Self.Add(aNodeOrder, OrderItem);
    Data := aNodeOrder.GetData;
    if Assigned(Data^.OrderDoc) then
      TPublishers.OrderStatusPublisher.OnStatusUpdate(Data^.OrderDoc.OrderStatus, nil, aNodeOrder, '');
  end;
end;

procedure TOrderList.AddOrder(aNodeOrder, aInstumentNode: PVirtualNode);
begin
  if not FLinkedNodes.ContainsValue(aInstumentNode) then
  begin
    FLinkedNodes.AddOrSetValue(aNodeOrder, aInstumentNode);
    AddOrder(aNodeOrder);
  end;
end;

procedure TOrderList.AddStatus(aOrder: TIABOrder; aInfo, aEvent: string; aCodeError: Integer; aStatus: TIABOrderState);
begin
  if Assigned(aOrder) then
    AddStatus(aOrder.TempId, aOrder.Filled, aOrder.Remaining, aInfo, aEvent, aCodeError, aStatus, aOrder.FillPrice);
end;

procedure TOrderList.AddStatus(aOrderTempId: Integer; aFilled, aRemaining: Double; aInfo, aEvent: string; aCodeError: Integer; aStatus: TIABOrderState; aAvgFillPrice: Double);
var
  APIOrderItem : TOrderItem.TAPIOrderItem;
  i            : Integer;
  Monitor      : IMonitor;
  Node         : PVirtualNode;
  Order        : TIABOrder;
  OrderItem    : TOrderItem;
  StatusItem   : TOrderItem.TStatusItem;
begin
  for Node in Self.Keys do
    if Assigned(Node) then
    begin
      OrderItem := Self.Items[Node];
      if OrderItem.APIOrderList.ContainsKey(aOrderTempId) then
      begin
        if aEvent.ToUpper.StartsWith('ONORDERSTATUS', True) or aEvent.ToUpper.StartsWith('ERROR', True) then
        begin
          OrderItem.LastAvgFillPrice := aAvgFillPrice;
          OrderItem.LastFilled       := aFilled;
          OrderItem.LastRemaining    := aRemaining;
          OrderItem.LastStatus       := aStatus;
          OrderItem.LastInfo         := aInfo;
          if aEvent.ToUpper.StartsWith('ERROR', True) then
          begin
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'AddStatus', 'Status: ' + OrderStateString[aStatus] + ', ' + aInfo);
            if (aCodeError in [202]) then
            begin
              OrderItem.LastStatus := TIABOrderState.osCancelled;
              aStatus := TIABOrderState.osCancelled;
            end;
          end;
          SetStatusNode(Node, aStatus, aInfo);
        end;

        APIOrderItem := OrderItem.APIOrderList.Items[aOrderTempId];
        StatusItem   := TOrderItem.TStatusItem.Create(aFilled, aRemaining, 0, aInfo, aEvent, aStatus, aAvgFillPrice);
        APIOrderItem.StatusList.Add(StatusItem);
        if Supports(Application.MainForm, IMonitor, Monitor) then
        begin
          Order := nil;
          i := Monitor.GetIABClient.Orders.IndexOfTempId(aOrderTempId, -1);
          if (i > -1) then
            Order := Monitor.GetIABClient.Orders.Items[i];
          TPublishers.OrderStatusPublisher.OnStatusUpdate(aStatus, Order, Node, aInfo);
        end;
        Exit;
      end;
    end;
end;

procedure TOrderList.AddStatus(aNodeOrder: PVirtualNode; aOrderId: Integer; aFilled, aRemaining: Double; aInfo, aEvent: string; aCodeError: Integer; aStatus: TIABOrderState; aAvgFillPrice: Double);
var
  i            : Integer;
  APIOrderItem : TOrderItem.TAPIOrderItem;
  Order        : TIABOrder;
  OrderItem    : TOrderItem;
  StatusItem   : TOrderItem.TStatusItem;
  Monitor      : IMonitor;
begin
  if Assigned(aNodeOrder) then
  begin
    if not Self.ContainsKey(aNodeOrder) then
    begin
      OrderItem := TOrderItem.Create;
      Self.Add(aNodeOrder, OrderItem);
    end
    else
      OrderItem := Self.Items[aNodeOrder];
    if aEvent.ToUpper.StartsWith('ORDERSTATUS', True) or
       aEvent.ToUpper.StartsWith('ERROR', True) then
    begin
      OrderItem.LastAvgFillPrice := aAvgFillPrice;
      OrderItem.LastFilled       := aFilled;
      OrderItem.LastRemaining    := aRemaining;
      OrderItem.LastStatus       := aStatus;
      OrderItem.LastInfo         := aInfo;
      if aEvent.ToUpper.StartsWith('ERROR', True) then
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'AddStatus', 'Status: ' + OrderStateString[aStatus] + ', ' + aInfo);
        if (aCodeError in [202]) then
        begin
          OrderItem.LastStatus := TIABOrderState.osCancelled;
          aStatus              := TIABOrderState.osCancelled;
        end;
      end
      else
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'AddStatus', 'Status: ' + OrderStateString[aStatus] + ', ' + aInfo);
    end;
    if OrderItem.APIOrderList.ContainsKey(aOrderId) then
      APIOrderItem := OrderItem.APIOrderList[aOrderId]
    else
    begin
      APIOrderItem := TOrderItem.TAPIOrderItem.Create;
      Self[aNodeOrder].APIOrderList.Add(aOrderId, APIOrderItem);
    end;

    StatusItem := TOrderItem.TStatusItem.Create(aFilled, aRemaining, 0, aInfo, aEvent, aStatus, aAvgFillPrice);
    APIOrderItem.StatusList.Add(StatusItem);
    if Supports(Application.MainForm, IMonitor, Monitor) then
    begin
      Order := nil;
      i := Monitor.GetIABClient.Orders.IndexOfTempId(aOrderId, -1);
      if (i > -1) then
        Order := Monitor.GetIABClient.Orders.Items[i];
      TPublishers.OrderStatusPublisher.OnStatusUpdate(aStatus, Order, aNodeOrder, aInfo);
    end;
  end;
end;

procedure TOrderList.AfterConstruction;
begin
  inherited;
  FLinkedNodes := TObjectDictionary<PVirtualNode, PVirtualNode>.Create;
end;

procedure TOrderList.Clear;
begin
  inherited;
  FLinkedNodes.Clear;
end;

procedure TOrderList.SetStatusNode(aNodeOrder: PVirtualNode; aStatus: TIABOrderState; aInfo: string);
var
  Data: PTreeData;
begin
  if Assigned(aNodeOrder) then
  begin
    Data := aNodeOrder^.GetData;
    if Assigned(Data) and Assigned(Data.OrderDoc) then
    begin
      Data^.OrderDoc.OrderStatus := aStatus;
      Data^.OrderDoc.Info        := aInfo;
    end;
  end;
end;

procedure TOrderList.DeleteOrder(aNodeOrder: PVirtualNode);
begin
  if FLinkedNodes.ContainsKey(ANodeOrder) then
    FLinkedNodes.Remove(ANodeOrder);
  if Self.ContainsKey(aNodeOrder) then
    Self.Remove(aNodeOrder);
end;

destructor TOrderList.Destroy;
begin
  FreeAndNil(FLinkedNodes);
  inherited;
end;

function TOrderList.GetLastOrderValues(aNodeOrder: PVirtualNode; aKindData: TKindData): string;
var
  OrderItem: TOrderItem;
begin
  if Assigned(aNodeOrder) and Self.ContainsKey(aNodeOrder) then
  begin
    OrderItem := Self[aNodeOrder];
    case aKindData of
      kdAvgFillPrice:
        Result := OrderItem.LastAvgFillPrice.ToString;
      kdFilled:
        Result := OrderItem.LastFilled.ToString;
      kdRemaining:
        Result := OrderItem.LastRemaining.ToString;
      kdStatus:
        Result := OrderStateString[OrderItem.LastStatus];
    end;
  end
  else
    Result := '';
end;

function TOrderList.GetNodeOrder(aOrderId: Integer): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result := nil;
  for Node in Self.Keys do
    if Assigned(Node) and Self.Items[Node].APIOrderList.ContainsKey(aOrderId) then
      Exit(Node);
end;

function TOrderList.GetStatusArray(aNodeOrder: PVirtualNode): TArrayRecord<TOrderItem.TStatusItem>;
var
  OrderItem: TOrderItem;
begin
  Result.Clear;
  if Assigned(aNodeOrder) and Self.ContainsKey(aNodeOrder) then
  begin
    OrderItem := Self[aNodeOrder];
    for var APIOrderItem in OrderItem.APIOrderList.Values do
      for var StatusItem in APIOrderItem.StatusList do
        Result.Add(StatusItem);
  end;
end;

function TOrderList.GetStatusText(aNodeOrder: PVirtualNode): string;
var
  OrderItem: TOrderItem;
begin
  Result := '';
  if Assigned(aNodeOrder) and Self.ContainsKey(aNodeOrder) then
  begin
    OrderItem := Self[aNodeOrder];
    for var APIOrderItem in OrderItem.APIOrderList.Values do
    begin
      Result := Result + '================' + sLineBreak;
      for var StatusItem in APIOrderItem.StatusList do
        Result := Result + sLineBreak + StatusItem.Info + sLineBreak + '----------------' + sLineBreak;
    end;
  end;
end;

function TOrderList.IsNodeLinked(aNode: PVirtualNode): Boolean;
begin
  if Assigned(aNode) then
    Result := FLinkedNodes.ContainsValue(aNode)
  else
    Result := False;
end;

function TOrderList.GetMasterLinkedNode(aNode: PVirtualNode): Pointer;
var
  Node: PVirtualNode;
begin
  Result := nil;
  if Assigned(aNode) and FLinkedNodes.ContainsValue(aNode) then
  begin
    for Node in FLinkedNodes.Keys do
      if (FLinkedNodes.Items[Node] = aNode) then
      begin
        Result := Node;
        Break;
      end;
  end;
end;

procedure TOrderList.SetLastStatus(aNodeOrder: PVirtualNode; aStatus: TIABOrderState);
begin
  if Assigned(aNodeOrder) and Self.ContainsKey(aNodeOrder) then
    Self[aNodeOrder].LastStatus := aStatus;
end;

function TOrderList.ToString: string;
var
  OrderItem: TOrderItem;
begin
  Result := '';
  for OrderItem in Self.Values do
  begin
    if not Result.IsEmpty then
      Result := Result + ', ';
    Result := Result + OrderItem.ToString;
  end;
end;

{ TAlgosList }

procedure TAlgosList.AddItem(aNodeAlgos: PVirtualNode);
var
  Item: TAlgosItem;
begin
  if Assigned(aNodeAlgos) and not Self.ContainsKey(aNodeAlgos) then
  begin
    Item := TAlgosItem.Create;
    Self.Add(aNodeAlgos, Item);
  end;
end;

procedure TAlgosList.DeleteAlgos(aNodeAlgos: PVirtualNode);
begin
  if Self.ContainsKey(aNodeAlgos) then
    Self.Remove(aNodeAlgos);
end;

function TAlgosList.ToString: string;
var
  loAlgosItem: TAlgosItem;
begin
  for loAlgosItem in Self.Values do
  begin
    if not Result.IsEmpty then
      Result := Result + ', ';
    Result := Result + loAlgosItem.ToString;
  end;
end;

{ TAlgosItem }

constructor TAlgosItem.Create;
begin

end;

destructor TAlgosItem.Destroy;
begin

end;

function TAlgosItem.ToString: string;
begin

end;

{ TOrderItem }

constructor TOrderItem.Create;
begin
  inherited Create;
  APIOrderList := TObjectDictionary<Integer, TAPIOrderItem>.Create([doOwnsValues]);
  Self.Clear;
end;

destructor TOrderItem.Destroy;
begin
  FreeAndNil(APIOrderList);
  inherited;
end;

procedure TOrderItem.Clear;
begin
  APIOrderList.Clear;
  LastAvgFillPrice := 0;
  LastFilled       := 0;
  LastFreezePrice  := 0;
  LastInfo         := '';
  LastRemaining    := 0;
  LastStatus       := osSleeping;
end;

procedure TOrderItem.FromXml(const aText: string);
var
  XmlFile: TXmlFile;
  OrderItem: TAPIOrderItem;
  StatusItem: TStatusItem;
  IBID: Integer;
begin
  Self.Clear;
  if not aText.IsEmpty then
  begin
    OrderItem := TAPIOrderItem.Create;
    XmlFile := TXmlFile.Create;
    try
      XMLFile.XMLText         := aText;
      XMLFile.CurrentSection  := C_SECTION_ORDER_STATUS;
      while not XMLFile.IsLastKey do
      begin
        if XMLFile.ReadAttributes then
        begin
          StatusItem := TStatusItem.Create(XMLFile.Attributes.GetAttributeValue('Filled', 0),
                                           XMLFile.Attributes.GetAttributeValue('Remaining', 0),
                                           XMLFile.Attributes.GetAttributeValue('LastPrice', 0),
                                           XMLFile.Attributes.GetAttributeValue('Info', ''),
                                           XMLFile.Attributes.GetAttributeValue('Event', ''),
                                           TIABOrderState(XMLFile.Attributes.GetAttributeValue('Status', osSleeping)),
                                           XMLFile.Attributes.GetAttributeValue('AvgFillPrice', 0));
          OrderItem.StatusList.Add(StatusItem);
        end;
        XMLFile.NextKey;
      end;


      IBID := XmlFile.ReadValue(XmlFile.GetXPath(C_SECTION_ORDER_STATUS), 0);
      Self.APIOrderList.AddOrSetValue(IBID, OrderItem);
    finally
      FreeAndNil(XmlFile);
    end;
  end;
end;

function TOrderItem.ToXml(const aOrderId: Integer = 0): string;
var
  StatusItem: TStatusItem;
  XmlFile: TXmlFile;
begin
  Result := '';
  XmlFile := TXmlFile.Create;
  try
    if (aOrderId > 0) then
      XmlFile.WriteValue(XmlFile.GetXPath(C_SECTION_ORDER_STATUS), aOrderId);
    XmlFile.CurrentSection := C_SECTION_ORDER_STATUS;
    for var OrderItem in Self.APIOrderList.Values do
      for StatusItem in OrderItem.StatusList do
      begin
        XmlFile.Attributes.AddNode;
        XmlFile.Attributes.SetAttributeValue('AvgFillPrice', StatusItem.AvgFillPrice);
        XmlFile.Attributes.SetAttributeValue('LastPrice', StatusItem.LastPrice);
        XmlFile.Attributes.SetAttributeValue('Event', StatusItem.Event);
        XmlFile.Attributes.SetAttributeValue('Filled', StatusItem.Filled);
        XmlFile.Attributes.SetAttributeValue('Info', StatusItem.Info);
        XmlFile.Attributes.SetAttributeValue('Remaining', StatusItem.Remaining);
        XmlFile.Attributes.SetAttributeValue('Status', StatusItem.Status);
        XmlFile.Attributes.SetAttributeValue('TimeStamp', StatusItem.TimeStamp);
        XmlFile.WriteAttributes;
      end;
    Result := XmlFile.XMLText;
  finally
    FreeAndNil(XmlFile);
  end;
end;

{ TOrderItem.TStatusItem }

constructor TOrderItem.TStatusItem.Create(aFilled, aRemaining: Double; aLastPrice: Currency; aInfo, aEvent: string; aStatus: TIABOrderState; aAvgFillPrice: Double);
begin
  Self.AvgFillPrice := aAvgFillPrice;
  Self.Event        := aEvent;
  Self.Filled       := aFilled;
  Self.Info         := aInfo;
  Self.Remaining    := aRemaining;
  Self.Status       := aStatus;
  Self.TimeStamp    := Now;
  Self.LastPrice    := aLastPrice;
end;

{ TAccountList }

procedure TAccountList.AddAccount(aSymbol: string; aNodeAccount: PVirtualNode);
begin
  AddOrSetValue(aSymbol, aNodeAccount);
end;

procedure TAccountList.DeleteAccount(aNodeAccount: PVirtualNode);
var
  Symbol: string;
begin
  for Symbol in Self.Keys do
    if (Self[Symbol] = aNodeAccount) then
      Remove(Symbol);
end;

function TAccountList.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TAccountList.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
begin
  if Assigned(FPriceChangeEvent) then
    FPriceChangeEvent(Sender, Id, TickType, Value, TimeStamp);
end;

function TAccountList.ToString: string;
begin

end;

{ TOrderItem.TAPIOrderItem }

constructor TOrderItem.TAPIOrderItem.Create;
begin
  StatusList := TObjectList<TStatusItem>.Create(True);
end;

destructor TOrderItem.TAPIOrderItem.Destroy;
begin
  FreeAndNil(StatusList);
  inherited;
end;

{ TPriceCache }

procedure TPriceCache.AddPrice(aDataId: Integer; aHistoricalChartData: TIABHistoricalChartData);
var
  dTime     : TDateTime;
  PriceList : TPriceList;
  Price     : TPrice;
begin
  if (not Application.Terminated) and (aHistoricalChartData.Close > 0) then
  begin
    if not Self.ContainsKey(aDataId) then
    begin
      PriceList := TPriceList.Create;
      Self.Add(aDataId, PriceList);
    end
    else
      PriceList := Self.Items[aDataId];
    dTime := GetDateFromHistoricalChart(aHistoricalChartData.Date);

    Price := Default(TPrice);
    Price.TimeStamp    := dTime;
    Price.IsHistorical := True;

    Price.TickType     := ttHigh;
    Price.Value        := aHistoricalChartData.High;
    PriceList.Add(Price);

    Price.TickType     := ttClose;
    Price.Value        := aHistoricalChartData.Close;
    PriceList.Add(Price);

    Price.TickType     := ttLast;
    Price.Value        := aHistoricalChartData.Close;
    PriceList.Add(Price);

    Price.TickType     := ttLow;
    Price.Value        := aHistoricalChartData.Low;
    PriceList.Add(Price);

    Price.TickType     := ttOpen;
    Price.Value        := aHistoricalChartData.Open;
    PriceList.Add(Price);

    Price.TickType     := ttVolume;
    Price.Value        := aHistoricalChartData.Volume;
    PriceList.Add(Price);

    if (dTime > PriceList.LastTimeStamp) then
    begin
      PriceList.LastTimeStamp       := dTime;
      PriceList.LastPrice[ttLast]   := aHistoricalChartData.Close;
      PriceList.LastPrice[ttHigh]   := aHistoricalChartData.High;
      PriceList.LastPrice[ttClose]  := aHistoricalChartData.Close;
      PriceList.LastPrice[ttLow]    := aHistoricalChartData.Low;
      PriceList.LastPrice[ttOpen]   := aHistoricalChartData.Open;
      PriceList.LastPrice[ttVolume] := aHistoricalChartData.Volume;
      PriceList.ReceivedTick[ttLast]   := True;
      PriceList.ReceivedTick[ttHigh]   := True;
      PriceList.ReceivedTick[ttClose]  := True;
      PriceList.ReceivedTick[ttLow]    := True;
      PriceList.ReceivedTick[ttOpen]   := True;
      PriceList.ReceivedTick[ttVolume] := True;
    end;
    if (PriceList.FirstTimeStamp > dTime) then
      PriceList.FirstTimeStamp := dTime;

    if FThread.Started then
    begin
      FThread.PriceQueue.PushItem(TStoredPrice.Create(aDataId, Price.TimeStamp, aHistoricalChartData.High, ttHigh, True));
      FThread.PriceQueue.PushItem(TStoredPrice.Create(aDataId, Price.TimeStamp, aHistoricalChartData.Close, ttClose, True));
      FThread.PriceQueue.PushItem(TStoredPrice.Create(aDataId, Price.TimeStamp, aHistoricalChartData.Low, ttLow, True));
      FThread.PriceQueue.PushItem(TStoredPrice.Create(aDataId, Price.TimeStamp, aHistoricalChartData.Open, ttOpen, True));
      FThread.PriceQueue.PushItem(TStoredPrice.Create(aDataId, Price.TimeStamp, aHistoricalChartData.Volume, ttVolume, True));
    end;
  end;
end;

procedure TPriceCache.AddPrice(aDataId: Integer; aTickType: TIABTickType; aValue: Currency; aTimeStamp: TDateTime; aIsHistorical: Boolean);
resourcestring
  rsHigh = 'ConId=%d, Last(%f) > High(%f)';
  rsLow = 'ConId=%d, Last(%f) < Low(%f)';
var
  IsExists  : Boolean;
  PriceList : TPriceList;
  Price     : TPrice;
begin
  if (not Application.Terminated) and (aValue > C_MIN_PRICE) and (aTickType in [ttBidSize .. ttMotherFilledPrice]) then
  begin
    IsExists := False;
    if Self.ContainsKey(aDataId) then
    begin
      PriceList := Self.Items[aDataId];
      if (aTickType = ttLast) then
      begin
        if (aValue > PriceList.LastPrice[ttHigh]) then
        begin
          //check: ttLast cannot be more than ttHigh
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddWarning, Self, Format(rsHigh, [aDataId, aValue, PriceList.LastPrice[ttHigh]]));
          TPublishers.FeedPublisher.UpdatePrice(aDataId, ttHigh, aValue, aTimeStamp);
        end;
        if (aValue < PriceList.LastPrice[ttLow]) then
        begin
          //check: ttLast cannot be less than ttLow
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddWarning, Self, Format(rsLow, [aDataId, aValue, PriceList.LastPrice[ttLow]]));
          TPublishers.FeedPublisher.UpdatePrice(aDataId, ttLow, aValue, aTimeStamp);
        end;

        if (PriceList.FirstTimeStamp = 0) or (PriceList.FirstTimeStamp > aTimeStamp) then
          PriceList.FirstTimeStamp := aTimeStamp;
      end;
    end
    else
    begin
      PriceList := TPriceList.Create;
      Self.Add(aDataId, PriceList);
    end;

    if not IsExists then
    begin
      Price.TimeStamp    := aTimeStamp;
      Price.TickType     := aTickType;
      Price.Value        := aValue;
      Price.IsHistorical := aIsHistorical;
      PriceList.Add(Price);
    end;

    PriceList.LastPrice[aTickType]    := aValue;
    PriceList.ReceivedTick[aTickType] := True;
    PriceList.LastTimeStamp           := aTimeStamp;

    if FThread.Started then
      if aTickType in [ttLast, ttClose, ttHigh, ttLow, ttOpen, ttVolume, ttOptionHistoricalVol] then
        FThread.PriceQueue.PushItem(TStoredPrice.Create(aDataId, aTimeStamp, aValue, aTickType, aIsHistorical));
  end;
end;

constructor TPriceCache.Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer);
begin
  inherited Create(Ownerships, ACapacity);
  FThread := TThreadPriceCache.Create;
  FThread.Start;
end;

destructor TPriceCache.Destroy;
begin
  FThread.Terminate;
  WaitForSingleObject(FThread.Handle, INFINITE);
  inherited;
end;

procedure TPriceCache.InitPrice(aDataId: Integer; aSymbol, aCurrency, aExchange: string; aLastPrice: Double);
var
  PriceList: TPriceList;
begin
  if (not Application.Terminated) then
  begin
    if (not aSymbol.IsEmpty) and (not aCurrency.IsEmpty) then
      if not Self.ContainsKey(aDataId) then
      begin
        PriceList := TPriceList.Create;
        PriceList.Symbol        := aSymbol;
        PriceList.Currency      := aCurrency;
        PriceList.Exchange      := aExchange;
        PriceList.InitTimeStamp := Now;
        PriceList.LastPrice[ttLast] := aLastPrice;
        Self.Add(aDataId, PriceList);
      end;
    if not FThread.Started then
    begin
      FThread.Start;
      Sleep(100);
    end;
  end;
end;

function TPriceCache.GetLastPrice(aDataId: Integer; aTickType: TIABTickType): Currency;
begin
  Result := 0;
  if Self.ContainsKey(aDataId) then
  begin
    Self.Items[aDataId].ResponseTimeStamp := Now;
    Result := Self.Items[aDataId].LastPrice[aTickType];
  end;
end;

function TPriceCache.GetLastPrice(aSymbol, aCurrency, aExchange: string; aTickType: TIABTickType): Currency;
var
  PriceList: TPriceList;
begin
  Result := 0;
  for PriceList in Self.Values do
    if PriceList.Symbol.Equals(aSymbol) and PriceList.Currency.Equals(aCurrency) and PriceList.Exchange.Equals(aExchange) then
    begin
      PriceList.ResponseTimeStamp := Now;
      Result := PriceList.LastPrice[aTickType];
      Break;
    end;
end;

function TPriceCache.GetPriceList(aDataId: Integer): TPriceList;
begin
  Result := nil;
  if Self.ContainsKey(aDataId) then
    Result := Self.Items[aDataId];
end;

{ TPriceList }

constructor TPriceList.Create;
begin
  inherited;
  Symbol   := '';
  Currency := '';
  Exchange := '';
  for var TickType := Low(LastPrice) to High(LastPrice) do
    LastPrice[TickType] := 0;
  LastTimeStamp     := 0;
  FirstTimeStamp    := 0;
  InitTimeStamp     := 0;
  ResponseTimeStamp := 0;
  FComparer := TComparer<TPrice>.Construct(
    function(const Left, Right: TPrice): Integer
    begin
      Result := TComparer<Double>.Default.Compare(Left.TimeStamp, Right.TimeStamp);
    end);
end;

destructor TPriceList.Destroy;
begin
  Self.Clear;
  inherited;
end;

function TPriceList.GetLastPrices(const aFilterFunc: TFilterFunc): TArray<TPrice>;
var
  Index: Integer;
  IsCriterion : Boolean;
begin
  SetLength(Result, Self.Count);
  Index := 0;
  for var Price in Self do
  begin
    if Assigned(aFilterFunc) then
      IsCriterion := aFilterFunc(Price)
    else
      IsCriterion := True;
    if IsCriterion then
    begin
      Result[Index] := Price;
      Inc(Index);
    end;
  end;
  SetLength(Result, Index);
  TArray.Sort<TPrice>(Result, FComparer);
end;

function TPriceList.GetLastPricesBroken(const aFilterFunc: TFilterFunc): TArray<TPrice>;
var
  Index: Integer;
  IsCriterion: Boolean;
begin
  if Assigned(aFilterFunc) then
  begin
    SetLength(Result, Self.Count);
    Index := 0;
    for var i := Self.Count - 1 downto 0 do
    begin
      IsCriterion := aFilterFunc(Self.Items[i]);
      if IsCriterion then
      begin
        if (Self.Items[i].TickType = ttLast) then
        begin
          Result[Index] := Self.Items[i];
          Inc(Index);
        end
      end
      else
        Break;
    end;

    SetLength(Result, Index);
    TArray.Sort<TPrice>(Result, FComparer);
  end
  else
    Result := [];
end;

{ TOrderQueue }

procedure TOrderQueue.AddItem(aNode: PVirtualNode);
begin
  Self.Enqueue(aNode);
  if not Assigned(FTask) or (FTask.Status = TTaskStatus.Completed) then
    CreateTask;
  if (Self.Count > 0) and (FTask.Status = TTaskStatus.Created) then
    FTask.Start;
end;

destructor TOrderQueue.Destroy;
begin
  if Assigned(FTask) and (FTask.Status = TTaskStatus.Running) then
    FTask.Cancel;
  inherited;
end;

procedure TOrderQueue.CreateTask;
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'CreateTask');
  FTask := TTask.Create(
    procedure()
    var
      Data: PTreeData;
    begin
      TThread.NameThreadForDebugging('InstrumentList.TOrderQueue.FTask');
      while Self.Count > 0 do
      begin
        if (FTask.Status = TTaskStatus.Canceled) then
          Break;
        Data := PVirtualNode(Self.Dequeue).GetData;
        if Assigned(Data) and Assigned(Data.OrderDoc) then
          Data.OrderDoc.DoBuy;
      end;
    end);
  TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'CreateTask');
end;

{ TConditionHistData }

procedure TConditionHistData.AddPrice(aDataId: Integer; aTimeStamp: TDateTime; aPrice: Currency);
var
  PriceDictionary : TPriceDictionary;
begin
  if (aDataId > 0) then
  begin
    if Self.ContainsKey(aDataId) then
      PriceDictionary := Self.Items[aDataId]
    else
    begin
      PriceDictionary := TPriceDictionary.Create;
      Self.Add(aDataId, PriceDictionary);
    end;

    aTimeStamp := RoundToNearest(aTimeStamp, C_ROUND_SEC);
    if not PriceDictionary.ContainsKey(aTimeStamp) then
      PriceDictionary.Add(aTimeStamp, aPrice);
  end;
end;

function TConditionHistData.GetResultValue: TArray<TCalcPrice>;
var
  Counter: Integer;
  DataId : Integer;
  FoundIndex: Integer;
  loPriceDictionary : TPriceDictionary;
  Size: Integer;
  SortArray : TArray<TDateTime>;
  TimeStamp : TDateTime;
  SumPrice: Double;

  procedure ReallocateArray;
  begin
    Inc(Counter);
    if (Counter > Length(SortArray)) then
      SetLength(SortArray, Length(SortArray) * 2);
  end;

begin
  if (Length(FArrayPrice) = 0) then
  begin
    Counter := 0;
    Size := 10;
    SetLength(SortArray, Size);

    for DataId in Self.Keys do
    begin
      loPriceDictionary := Self.Items[DataId];
      for TimeStamp in loPriceDictionary.Keys do
        if not TArray.BinarySearch<TDateTime>(SortArray, TimeStamp, FoundIndex) then
        begin
          ReallocateArray;
          SortArray[Counter - 1] := TimeStamp;
        end;
    end;

    TArray.Sort<TDateTime>(SortArray);
    for TimeStamp in SortArray do
      if (TimeStamp > 0) then
      begin
        SumPrice := 0;
        for DataId in Self.Keys do
        begin
          if Self.Items[DataId].ContainsKey(TimeStamp) then
            SumPrice := SumPrice + Self.Items[DataId].Items[TimeStamp]
          else
          begin
            SumPrice := 0;
            Break;
          end;
        end;

        if (SumPrice > 0 ) then
        begin
          SetLength(FArrayPrice, Length(FArrayPrice) + 1);
          FArrayPrice[Length(FArrayPrice) - 1].TimeStamp := TimeStamp;
          FArrayPrice[Length(FArrayPrice) - 1].Price     := SumPrice;
        end;
      end;
  end;
  Result := FArrayPrice;
end;

{ FCurrencyCash }

function TCurrencyCache.GetLastExchange(const aMainCurrency, aCurrency: string): Double;
var
  ExchangeName: string;
  IsFraction: Boolean;
  IsExists: Boolean;
begin
  Result := 0;
  if Assigned(FPriceCache) then
  begin
    IsFraction := False;
    ExchangeName := aMainCurrency + '.' + aCurrency; // EUR.SEK
    IsExists := Self.ContainsKey(ExchangeName);

    if not IsExists then
    begin
      ExchangeName := aCurrency + '.' + aMainCurrency;
      IsExists := Self.ContainsKey(ExchangeName);
      IsFraction := True;
    end;

    if IsExists then
    begin
      Result := FPriceCache.GetLastPrice(Self.Items[ExchangeName], ttLast);
      if (Result = 0) then
        Result := FPriceCache.GetLastPrice(Self.Items[ExchangeName], ttClose);
      if IsFraction and (Result > 0) then
        Result := 1 / Result;
    end;
  end;
end;

function TCurrencyCache.GetLastExchange(const aCurrency: string): Double;
var
  arr: TArray<string>;
begin
  Result := 0;
  arr := aCurrency.Split(['.']);
  if (Length(arr) > 1) then
    Result := GetLastExchange(arr[0], arr[1]);
end;

{ TDocumentQueue }

procedure TDocumentQueue.AddItem(aNode: PVirtualNode);
begin
  if not Self.Contains(aNode) then
    Self.Add(aNode);
end;

procedure TDocumentQueue.DeleteItem(aNode: PVirtualNode);
begin
  if Self.Contains(aNode) then
    Self.Remove(aNode);
end;

{ TMonitorLists }

class procedure TMonitorLists.CreateLists;
begin
  AlgosList           := TAlgosList.Create([doOwnsValues]);
  InstrumentChildList := TFactorList.Create([doOwnsValues]);
  InstrumentList      := TFactorList.Create([doOwnsValues]);
  OrderList           := TOrderList.Create([doOwnsValues]);
//  OrderQueue          := TOrderQueue.Create;
end;

class function TMonitorLists.PriceCache: TPriceCache;
begin
  Result := TPriceCache.PriceCache;
end;

class function TMonitorLists.CurrencyCache: TCurrencyCache;
begin
  Result := TCurrencyCache.CurrencyCache;
end;

class procedure TMonitorLists.DestroyLists;
begin
  FreeAndNil(AlgosList);
  FreeAndNil(InstrumentChildList);
  FreeAndNil(InstrumentList);
  FreeAndNil(OrderList);
//  FreeAndNil(OrderQueue);
end;

initialization
  TPriceCache.PriceCache := TPriceCache.Create([doOwnsValues]);
  TCurrencyCache.CurrencyCache := TCurrencyCache.Create;
  TCurrencyCache.CurrencyCache.PriceCache := TPriceCache.PriceCache;

finalization
  if Assigned(TPriceCache.PriceCache) then
    FreeAndNil(TPriceCache.PriceCache);
  if Assigned(TCurrencyCache.CurrencyCache) then
    FreeAndNil(TCurrencyCache.CurrencyCache);

end.
