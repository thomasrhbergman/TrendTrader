unit Publishers;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  Vcl.Forms, Winapi.Messages, DebugWriter, IABSocketAPI, IABSocketAPI_const, System.Threading, System.Types,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Publishers.Interfaces, Common.Types, NNfunctions.Types,
  VirtualTrees;
{$ENDREGION}

type
  TCustomPublisher =  class(TList<TObject>)
  public
    procedure Subscribe(aItem: TObject); virtual;
    procedure Unsubscribe(aItem: TObject); virtual;
  end;

  TFeedPublisher = class(TCustomPublisher)
  public
    procedure UpdatePrice(aId: Integer; aTickType: TIABTickType; aValue: Currency; aTimeStamp: TDateTime);
  end;

  TTickOptionComputationPublisher = class(TCustomPublisher)
  public
    procedure OnTickOptionComputation(Sender: TObject; DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice: Double);
  end;

  TScannerPublisher = class(TCustomPublisher)
  public
    procedure OnScannerData(Sender: TObject; Scan: TIABScan);
    procedure OnScannerParam(Sender: TObject; Parameters: string);
    procedure OnScannerAdd(Sender: TObject; ScanId: Integer);
    procedure OnScannerCancel(Sender: TObject; ScanId: Integer);
  end;

  TInstrumentSpecDetailsPublisher = class(TCustomPublisher)
  public
    procedure OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
  end;

  TSecurityDefinitionOptionalParameterPublisher = class(TCustomPublisher)
  public
    procedure OnSecurityDefinitionOptionalParameter(Sender: TObject; DataID: Integer; Exchange: string; UnderlyingConId: Integer; TradingClass, Multiplier, Expirations, Strikes: string);
  end;

  TTickByTickPublisher = class(TCustomPublisher)
  public
    procedure OnTickByTick(Sender: TObject; DataID: Integer; TickData: TIABTickData);
    procedure OnHistoricalTickData(Sender: TObject; DataId: Integer; TickData: TIABTickData);
  end;

  TConnectionStatePublisher = class(TCustomPublisher)
  public
    procedure OnConnectionState(Sender: TObject; State: TIABConnection);
  end;

  TOrderStatusPublisher = class(TCustomPublisher)
  public
    procedure OnStatusUpdate(const aStatus: TIABOrderState; const aOrder: TIABOrder; aNodeOrder: PVirtualNode; const aInfo: string);
  end;

  TOrderStatePublisher = class(TCustomPublisher)
  public
    procedure OnCloseOrder(const aTempId: Integer);
    procedure OnExecution(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrder(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrderNN(const aOrderList: array of TOrder);
    procedure OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
    procedure OnRebuildFromTWS(Sender: TObject);
  end;

  TLogPublisher = class(TCustomPublisher)
  public
    procedure Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aInfo: string); overload;
    procedure Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aMethod, aInfo: string); overload;
    procedure Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aMethod, aUnit, aInfo: string); overload;
    procedure Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aObject: TObject; const aInfo: string); overload;
    procedure Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aObject: TObject; const aMethod, aInfo: string); overload;
    procedure Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aOrderID, aQuantity, aFilled: Integer; const aMethod, aSymbol, aInfo: string; const aStatus: TIABOrderState; const aAction: TIABAction; const aPrice: Double); overload;
  end;

  TErrorPublisher = class(TCustomPublisher)
  public
    procedure OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
  end;

  THistoricalDataPublisher = class(TCustomPublisher)
  public
    procedure OnHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
  end;

  TMonitorStructureChangePublisher = class(TCustomPublisher)
  public
    procedure OnMonitorStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
  end;

  TMotherFilledPriceChangePublisher = class(TCustomPublisher)
  public
    procedure OnMotherFilledPriceChange(const MotherNode: PVirtualNode);
  end;


  TPublishers = class
  class var
    ConnectionStatePublisher                     : TConnectionStatePublisher;
    ErrorPublisher                               : TErrorPublisher;
    FeedPublisher                                : TFeedPublisher;
    HistoricalDataPublisher                      : THistoricalDataPublisher;
    InstrumentSpecDetailsPublisher               : TInstrumentSpecDetailsPublisher;
    LogPublisher                                 : TLogPublisher;
    OrderStatePublisher                          : TOrderStatePublisher;
    OrderStatusPublisher                         : TOrderStatusPublisher;
    ScannerPublisher                             : TScannerPublisher;
    SecurityDefinitionOptionalParameterPublisher : TSecurityDefinitionOptionalParameterPublisher;
    TickByTickPublisher                          : TTickByTickPublisher;
    TickOptionComputationPublisher               : TTickOptionComputationPublisher;
    MonitorStructureChangePublisher              : TMonitorStructureChangePublisher;
    MotherFilledPriceChangePublisher             : TMotherFilledPriceChangePublisher;
  end;

implementation

{ TCustomPublisher }

procedure TCustomPublisher.Subscribe(aItem: TObject);
begin
  if (Self.IndexOf(aItem) < 0) then
    Self.Add(aItem);
end;

procedure TCustomPublisher.Unsubscribe(aItem: TObject);
begin
  if (Self.IndexOf(aItem) >= 0) then
    Self.Remove(aItem);
end;

{ TFeedPublisher }

procedure TFeedPublisher.UpdatePrice(aId: Integer; aTickType: TIABTickType; aValue: Currency; aTimeStamp: TDateTime);
var
  Item: TObject;
  oi: IUpdateFeeds;
begin
  if not Application.Terminated then
  begin
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) then
        if Supports(Item, IUpdateFeeds, oi) then
          oi.OnPriceChange(Item, aId, aTickType, aValue, aTimeStamp)
    end;
  end;
end;

{ TTickOptionComputationPublisher }

procedure TTickOptionComputationPublisher.OnTickOptionComputation(Sender: TObject; DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice: Double);
var
  Item: TObject;
  toc: ITickOptionComputation;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, ITickOptionComputation, toc) then
        toc.OnTickOptionComputation(Sender, DataId, TickType, ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice);
    end;
end;

{ TScannerPublisher }

procedure TScannerPublisher.OnScannerData(Sender: TObject; Scan: TIABScan);
var
  Item: TObject;
  oi: IScanner;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IScanner, oi) then
        oi.OnScannerData(Sender, Scan);
    end;
end;

procedure TScannerPublisher.OnScannerParam(Sender: TObject; Parameters: string);
var
  Item: TObject;
  oi: IScanner;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IScanner, oi) then
        oi.OnScannerParam(Sender, Parameters);
    end;
end;

procedure TScannerPublisher.OnScannerAdd(Sender: TObject; ScanId: Integer);
var
  Item: TObject;
  oi: IScanner;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IScanner, oi) then
        oi.OnScannerAdd(Sender, ScanId);
    end;
end;

procedure TScannerPublisher.OnScannerCancel(Sender: TObject; ScanId: Integer);
var
  Item: TObject;
  oi: IScanner;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IScanner, oi) then
        oi.OnScannerCancel(Sender, ScanId);
    end;
end;

{ TInstrumentSpecDetailsPublisher }

procedure TInstrumentSpecDetailsPublisher.OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
var
  Item: TObject;
  oi: IInstrumentSpecDetails;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IInstrumentSpecDetails, oi) then
        oi.OnInstrumentSpecDetails(Sender, Index);
    end;
end;

{ TSecurityDefinitionOptionalParameterPublisher }

procedure TSecurityDefinitionOptionalParameterPublisher.OnSecurityDefinitionOptionalParameter(Sender: TObject; DataID: Integer; Exchange: string; UnderlyingConId: Integer; TradingClass, Multiplier, Expirations, Strikes: string);
var
  Item: TObject;
  oi: ISecurityDefinitionOptionalParameter;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, ISecurityDefinitionOptionalParameter, oi) then
        oi.OnSecurityDefinitionOptionalParameter(Sender, DataID, Exchange, UnderlyingConId, TradingClass, Multiplier, Expirations, Strikes);
    end;
end;

{ TTickByTickPublisher }

procedure TTickByTickPublisher.OnHistoricalTickData(Sender: TObject; DataId: Integer; TickData: TIABTickData);
var
  Item: TObject;
  oi: IOnTickByTick;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IOnTickByTick, oi) then
        oi.OnHistoricalTickData(Sender, DataID, TickData);
    end;
end;

procedure TTickByTickPublisher.OnTickByTick(Sender: TObject; DataID: Integer; TickData: TIABTickData);
var
  Item: TObject;
  oi: IOnTickByTick;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IOnTickByTick, oi) then
        oi.OnTickByTick(Sender, DataID, TickData);
    end;
end;

{ TConnectionStatePublisher }

procedure TConnectionStatePublisher.OnConnectionState(Sender: TObject; State: TIABConnection);
var
  Item: TObject;
  oi: IConnectionState;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IConnectionState, oi) then
        oi.OnConnectionState(Sender, State);
    end;
end;

{ TOrderStatePublisher }

procedure TOrderStatePublisher.OnCloseOrder(const aTempId: Integer);
var
  Item: TObject;
  os: IOrderState;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IOrderState, os) then
        os.OnCloseOrder(aTempId);
    end;
end;

procedure TOrderStatePublisher.OnExecution(Sender: TObject; Order: TIABOrder);
var
  Item: TObject;
  os: IOrderState;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IOrderState, os) then
        os.OnExecution(Sender, Order);
    end;
end;

procedure TOrderStatePublisher.OnOpenOrder(Sender: TObject; Order: TIABOrder);
var
  Item: TObject;
  os: IOrderState;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IOrderState, os) then
        os.OnOpenOrder(Sender, Order);
    end;
end;

procedure TOrderStatePublisher.OnOpenOrderNN(const aOrderList: array of TOrder);
var
  Item: TObject;
  os: IOrderState;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IOrderState, os) then
        os.OnOpenOrderNN(aOrderList);
    end;
end;

procedure TOrderStatePublisher.OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
var
  Item: TObject;
  os: IOrderState;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IOrderState, os) then
        os.OnOrderStatus(Sender, Order, Status);
    end;
end;

procedure TOrderStatePublisher.OnRebuildFromTWS(Sender: TObject);
var
  Item: TObject;
  os: IOrderState;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IOrderState, os) then
        os.OnRebuildFromTWS(Sender);
    end;
end;

{ TLogPublisher }

procedure TLogPublisher.Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aInfo: string);
var
  Item: TObject;
  lo: ILogger;
begin
  if not Application.Terminated then
  begin
    LogWriter.Write(aDetailType, aInfo);
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, ILogger, lo) then
        if (lo.GetLogListenerType in aLogTypes) then
          lo.Write(aDetailType, aInfo);
    end;
  end;
end;

procedure TLogPublisher.Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aMethod, aInfo: string);
var
  Item: TObject;
  lo: ILogger;
begin
  if not Application.Terminated then
  begin
    LogWriter.Write(aDetailType, aMethod, aInfo);
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, ILogger, lo) then
        if (lo.GetLogListenerType in aLogTypes) then
          lo.Write(aDetailType, aMethod, aInfo);
    end;
  end;
end;

procedure TLogPublisher.Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aMethod, aUnit, aInfo: string);
var
  Item: TObject;
  lo: ILogger;
begin
  if not Application.Terminated then
  begin
    LogWriter.Write(aDetailType, aMethod, aUnit, aInfo);
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, ILogger, lo) then
        if (lo.GetLogListenerType in aLogTypes) then
          lo.Write(aDetailType, aMethod, aUnit, aInfo);
    end;
  end;
end;

procedure TLogPublisher.Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aObject: TObject; const aInfo: string);
var
  Item: TObject;
  lo: ILogger;
begin
  if not Application.Terminated then
  begin
    LogWriter.Write(aDetailType, aObject, aInfo);
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, ILogger, lo) then
        if (lo.GetLogListenerType in aLogTypes) then
          lo.Write(aDetailType, aObject, aInfo);
    end;
  end;
end;

procedure TLogPublisher.Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aObject: TObject; const aMethod, aInfo: string);
var
  Item: TObject;
  lo: ILogger;
begin
  if not Application.Terminated then
  begin
    LogWriter.Write(aDetailType, aObject, aMethod, aInfo);
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, ILogger, lo) then
        if (lo.GetLogListenerType in aLogTypes) then
          lo.Write(aDetailType, aObject, aMethod, aInfo);
    end;
  end;
end;

procedure TLogPublisher.Write(const aLogTypes: TLogListenerTypes; const aDetailType: TLogDetailType; const aOrderID, aQuantity, aFilled: Integer; const aMethod, aSymbol, aInfo: string; const aStatus: TIABOrderState; const aAction: TIABAction; const aPrice: Double);
resourcestring
  rsInfo = 'OrderID=%d, LastPrice=%f, Action=%s, Quantity=%d, Filled=%d, Symbol=%s, Status=%s, %s';
var
  Info: string;
  Item: TObject;
  lo: ILogger;
begin
  if not Application.Terminated then
  begin
    Info := Format(rsInfo, [aOrderID, aPrice, ActionString[aAction], aQuantity, aFilled, aSymbol, OrderStateString[aStatus], aInfo]);
    LogWriter.Write(aDetailType, aMethod, Info);
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, ILogger, lo) then
        if (lo.GetLogListenerType in aLogTypes) then
          lo.Write(aDetailType, aOrderID, aQuantity, aFilled, aMethod, aSymbol, aInfo, aStatus, aAction, aPrice);
    end;
  end;
end;

{ TOrderStatusPublisher }

procedure TOrderStatusPublisher.OnStatusUpdate(const aStatus: TIABOrderState; const aOrder: TIABOrder; aNodeOrder: PVirtualNode; const aInfo: string);
var
  Item: TObject;
  os: IOrderStatus;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IOrderStatus, os) then
        os.OnStatusUpdate(aStatus, aOrder, aNodeOrder, aInfo);
    end;
end;

{ TErrorPublisher }

procedure TErrorPublisher.OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
var
  Item: TObject;
  Err: IError;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IError, Err) then
        Err.OnError(Sender, TempId, ErrorCode, ErrorMsg);
    end;
end;

{ THistoricalDataPublisher }

procedure THistoricalDataPublisher.OnHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
var
  obj: TObject;
  hd: IHistoricalData;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      obj := Self.Items[i];
      if Assigned(obj) and Supports(obj, IHistoricalData, hd) then
        hd.OnHistoricalData(Sender, DataId, Item, Count, HistoricalChartDataElement);
    end;
end;

{ TMonitorStructureChangePublisher }

procedure TMonitorStructureChangePublisher.OnMonitorStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
var
  obj: TObject;
  ls: IMonitorStructureChange;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      obj := Self.Items[i];
      if Assigned(obj) and Supports(obj, IMonitorStructureChange, ls) then
        ls.OnMonitorStructureChange(Sender, Node, Reason);
    end;
end;

{ TMotherFilledPriceChangePublisher }

procedure TMotherFilledPriceChangePublisher.OnMotherFilledPriceChange(const MotherNode: PVirtualNode);
var
  obj: TObject;
  mf: IMotherFilledPriceChange;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      obj := Self.Items[i];
      if Assigned(obj) and Supports(obj, IMotherFilledPriceChange, mf) then
        mf.OnMotherFilledPriceChange(MotherNode);
    end;
end;

initialization
  TPublishers.ConnectionStatePublisher                     := TConnectionStatePublisher.Create;
  TPublishers.ErrorPublisher                               := TErrorPublisher.Create;
  TPublishers.FeedPublisher                                := TFeedPublisher.Create;
  TPublishers.HistoricalDataPublisher                      := THistoricalDataPublisher.Create;
  TPublishers.InstrumentSpecDetailsPublisher               := TInstrumentSpecDetailsPublisher.Create;
  TPublishers.LogPublisher                                 := TLogPublisher.Create;
  TPublishers.OrderStatePublisher                          := TOrderStatePublisher.Create;
  TPublishers.OrderStatusPublisher                         := TOrderStatusPublisher.Create;
  TPublishers.ScannerPublisher                             := TScannerPublisher.Create;
  TPublishers.SecurityDefinitionOptionalParameterPublisher := TSecurityDefinitionOptionalParameterPublisher.Create;
  TPublishers.TickByTickPublisher                          := TTickByTickPublisher.Create;
  TPublishers.TickOptionComputationPublisher               := TTickOptionComputationPublisher.Create;
  TPublishers.MonitorStructureChangePublisher              := TMonitorStructureChangePublisher.Create;
  TPublishers.MotherFilledPriceChangePublisher             := TMotherFilledPriceChangePublisher.Create;

finalization
  if Assigned(TPublishers.ConnectionStatePublisher) then
    FreeAndNil(TPublishers.ConnectionStatePublisher);
  if Assigned(TPublishers.FeedPublisher) then
    FreeAndNil(TPublishers.FeedPublisher);
  if Assigned(TPublishers.InstrumentSpecDetailsPublisher) then
    FreeAndNil(TPublishers.InstrumentSpecDetailsPublisher);
  if Assigned(TPublishers.LogPublisher) then
    FreeAndNil(TPublishers.LogPublisher);
  if Assigned(TPublishers.OrderStatePublisher) then
    FreeAndNil(TPublishers.OrderStatePublisher);
  if Assigned(TPublishers.ScannerPublisher) then
    FreeAndNil(TPublishers.ScannerPublisher);
  if Assigned(TPublishers.SecurityDefinitionOptionalParameterPublisher) then
    FreeAndNil(TPublishers.SecurityDefinitionOptionalParameterPublisher);
  if Assigned(TPublishers.TickByTickPublisher) then
    FreeAndNil(TPublishers.TickByTickPublisher);
  if Assigned(TPublishers.TickOptionComputationPublisher) then
    FreeAndNil(TPublishers.TickOptionComputationPublisher);
  if Assigned(TPublishers.OrderStatusPublisher) then
    FreeAndNil(TPublishers.OrderStatusPublisher);
  if Assigned(TPublishers.ErrorPublisher) then
    FreeAndNil(TPublishers.ErrorPublisher);
  if Assigned(TPublishers.HistoricalDataPublisher) then
    FreeAndNil(TPublishers.HistoricalDataPublisher);
  if Assigned(TPublishers.MonitorStructureChangePublisher) then
    FreeAndNil(TPublishers.MonitorStructureChangePublisher);
  if Assigned(TPublishers.MotherFilledPriceChangePublisher) then
    FreeAndNil(TPublishers.MotherFilledPriceChangePublisher);

end.

