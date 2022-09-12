unit Entity.OrderStatus;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults, Vcl.Forms,
  Winapi.Messages, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI, IABSocketAPI_const,
  IABFunctions, Data.DB, Common.Types, HtmlLib, System.IOUtils, System.Threading, System.Types, System.Math, VirtualTrees,
  DaModule.Constants, DaModule.Utils, System.Variants, Global.Types, Publishers, Publishers.Interfaces, Generics.Helper,
  InstrumentList, DaModule, Document, Monitor.Types, IABFunctions.Helpers, FireDAC.Comp.Client, IBX.IB,
  FireDAC.Stan.Param, FireDAC.Comp.DataSet, Utils;
{$ENDREGION}

type
  PStatusData = ^TStatusData;
  TStatusData = record
    RecordId           : Integer;
    NodeId             : Integer;
    QualifierId        : Integer;
    QualifierInstance  : Integer;
    AutoTradesId       : Integer;
    AutoTradesInstance : Integer;
    ScanSequenceId     : Integer;
    NodeType           : TNodeType;
    GroupText          : string;
    OrderId            : Integer;
    ConID              : Integer;
    Symbol             : string;
    Currency           : string;
    OrderType          : TIABOrderType;
    Action             : TIABAction;
    SecurityType       : TIABSecurityType;
    OrderScope         : Integer;
    TemplateId         : Integer;
    OCA                : Integer;
    MotherOrderId      : Integer;
    AddedTime          : TDateTime;
    PendSubmitTime     : TDateTime;
    PresubmittedTime   : TDateTime;
    PresubmittedLP     : Double;
    SubmittedTime      : TDateTime;
    SubmittedLP        : Double;
    Filled             : Double;
    FilledLP           : Double;
    CancelledTime      : TDateTime;
    Error              : string;
    Quantity           : Integer;
    LatestFillQty      : Double;
    Remaining          : Double;
    FillPrice          : Double;
    AvgFillPrice       : Double;
    LastPrice          : Double;
    AuxPrice           : Double;
    TrailingPercent    : Double;
    TrailStopPrice     : Double;
    LmtPriceOffset     : Double;
    LastTimestamp      : TDateTime;
    NodeOrder          : PVirtualNode;
    Status             : TIABOrderState;
    InCalc             : string;
    OrderStatusItems   : string;
    Info               : TArray<string>;
    IsArchived         : Boolean;
    function GetOrderStatusItemsXml: string;
    function ToString: string;
    procedure FromDB(aID: Integer);
    procedure Clear;
    procedure CheckValues;
  end;

  TThreadStorageOrders = class(TThread)
  private
    FConnection    : TFDConnection;
    FTransaction   : TFDTransaction;
    FQuery         : TFDQuery;
    FQueue         : TThreadedQueue<TStatusData>;
    procedure SaveRecordToDB(aStatusData: TStatusData);
    procedure CreateConnect;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property StatusQueue: TThreadedQueue<TStatusData> read FQueue;
  end;

  TStorageOrders = class(TInterfacedObjectDictionary<Integer, TStatusData>, IOrderStatus)
  private
    FThread: TThreadStorageOrders;
    function GetInstance: TObject;
    procedure OnStatusUpdate(const aStatus: TIABOrderState; const aOrder: TIABOrder; aNodeOrder: PVirtualNode; const aInfo: string);
  public
    procedure AfterConstruction; override;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TThreadKeeperOrders }

constructor TThreadStorageOrders.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Priority := tpLowest;
  FQueue := TThreadedQueue<TStatusData>.Create(10000, C_POP_TIMEOUT, C_PUSH_TIMEOUT);
  CreateConnect;
end;

procedure TThreadStorageOrders.CreateConnect;
begin
  FConnection := TFireBirdConnect.CreateDatabase(TFireBirdConnect.DBNameFeed);

  FTransaction := TFDTransaction.Create(FConnection);
  FConnection.Transaction := FTransaction;
  FTransaction.Connection := FConnection;
  FTransaction.Options.AutoCommit := false;

  FQuery := TFDQuery.Create(FConnection);
  FQuery.Connection    := FConnection;
  FQuery.Transaction := FTransaction;
end;

destructor TThreadStorageOrders.Destroy;
begin
  if FTransaction.Active then
    FTransaction.Commit;
  if FConnection.Connected then
    FConnection.Connected := False;
  FreeAndNil(FQuery);
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  FreeAndNil(FQueue);
  inherited Destroy;
end;

procedure TThreadStorageOrders.Execute;
var
  StatusData: TStatusData;
  WaitResult: TWaitResult;
begin
  inherited;
  try
    TThread.NameThreadForDebugging('Entity.OrderStatus.TThreadStorageOrders');
    if not FConnection.Connected then
      FConnection.Connected := True;
    while not Terminated do
    begin
      WaitResult := FQueue.PopItem(StatusData);
      if (WaitResult = TWaitResult.wrSignaled) then
        if (StatusData.OrderId > 0) then
          SaveRecordToDB(StatusData);
    end;
  except
    on E:Exception do
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'Execute', E.Message);
  end;
end;

procedure TThreadStorageOrders.SaveRecordToDB(aStatusData: TStatusData);
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM ORDER_STATUS WHERE ID=:RecordId';
  C_SQL_INSERT_TEXT =
    'INSERT INTO ORDER_STATUS ( ID, ORDER_ID, CON_ID, SYMBOL, CURRENCY, ORDER_TYPE, ORDER_ACTION, SECURITY_TYPE, ORDER_SCOPE, TEMPLATE_ID, AUTOTRADES_INSTANCE, PENDSUBMIT_TIME,' +
                              ' MOTHER_ORDER_ID, ADDED_TIME, PRESUBMITTED_TIME, PRESUBMITTED_LP, SUBMITTED_LP, FILLED, FILLED_LP, CANCELLED_TIME, ERROR_TEXT, QUANTITY, LATEST_FILL_QTY, REMAINING, FILL_PRICE, AVG_FILL_PRICE,' +
                              ' LAST_PRICE, AUX_PRICE, TRAILING_PERCENT, TRAIL_STOP_PRICE, LMT_PRICE_OFFSET, LAST_TIME, STATUS, IN_CALC, SCAN_SEQUENCE_ID, AUTO_TRADES_ID, ORDER_STATUS_ITEMS, QUALIFIER_ID, QUALIFIER_INSTANCE, SUBMITTED_TIME, NODE_ID)' + sLineBreak +
    '                  VALUES (:ID,:ORDER_ID,:CON_ID,:SYMBOL,:CURRENCY,:ORDER_TYPE,:ORDER_ACTION,:SECURITY_TYPE,:ORDER_SCOPE,:TEMPLATE_ID,:AUTOTRADES_INSTANCE,:PENDSUBMIT_TIME,' +
                              ':MOTHER_ORDER_ID,:ADDED_TIME,:PRESUBMITTED_TIME,:PRESUBMITTED_LP,:SUBMITTED_LP,:FILLED,:FILLED_LP,:CANCELLED_TIME,:ERROR_TEXT,:QUANTITY,:LATEST_FILL_QTY,:REMAINING,:FILL_PRICE,:AVG_FILL_PRICE,' +
                              ':LAST_PRICE,:AUX_PRICE,:TRAILING_PERCENT,:TRAIL_STOP_PRICE,:LMT_PRICE_OFFSET,:LAST_TIME,:STATUS,:IN_CALC,:SCAN_SEQUENCE_ID,:AUTO_TRADES_ID,:ORDER_STATUS_ITEMS,:QUALIFIER_ID,:QUALIFIER_INSTANCE,:SUBMITTED_TIME,:NODE_ID)';
  C_SQL_UPDATE_TEXT =
    'UPDATE ORDER_STATUS ' +
    '   SET ORDER_ID=:ORDER_ID, CON_ID=:CON_ID, SYMBOL=:SYMBOL, CURRENCY=:CURRENCY, ORDER_TYPE=:ORDER_TYPE, ORDER_ACTION=:ORDER_ACTION, SECURITY_TYPE=:SECURITY_TYPE,' +
    '       ORDER_SCOPE=:ORDER_SCOPE, TEMPLATE_ID=:TEMPLATE_ID, AUTOTRADES_INSTANCE=:AUTOTRADES_INSTANCE, MOTHER_ORDER_ID=:MOTHER_ORDER_ID, ADDED_TIME=:ADDED_TIME, ' +
    '       PRESUBMITTED_TIME=:PRESUBMITTED_TIME, PRESUBMITTED_LP=:PRESUBMITTED_LP, SUBMITTED_LP=:SUBMITTED_LP, FILLED=:FILLED, FILLED_LP=:FILLED_LP, CANCELLED_TIME=:CANCELLED_TIME,' +
    '       QUANTITY=:QUANTITY, LATEST_FILL_QTY=:LATEST_FILL_QTY, REMAINING=:REMAINING, FILL_PRICE=:FILL_PRICE, AVG_FILL_PRICE=:AVG_FILL_PRICE, LAST_PRICE=:LAST_PRICE, ' +
    '       AUX_PRICE=:AUX_PRICE, TRAILING_PERCENT=:TRAILING_PERCENT, TRAIL_STOP_PRICE=:TRAIL_STOP_PRICE, LMT_PRICE_OFFSET=:LMT_PRICE_OFFSET, LAST_TIME=:LAST_TIME,' +
    '       STATUS=:STATUS, IN_CALC=:IN_CALC, SCAN_SEQUENCE_ID=:SCAN_SEQUENCE_ID, AUTO_TRADES_ID=:AUTO_TRADES_ID, ERROR_TEXT=:ERROR_TEXT, NODE_ID=:NODE_ID,' +
    '       ORDER_STATUS_ITEMS=:ORDER_STATUS_ITEMS, QUALIFIER_ID=:QUALIFIER_ID, SUBMITTED_TIME=:SUBMITTED_TIME, QUALIFIER_INSTANCE=:QUALIFIER_INSTANCE, PENDSUBMIT_TIME=:PENDSUBMIT_TIME' +
    ' WHERE (ID=:ID)';
var
  IsExists: Boolean;
begin
  FQuery.Close;
  IsExists := False;
  FQuery.SQL.Text := C_SQL_EXISTS_TEXT;
  try
    FQuery.ParamByName('RecordId').AsInteger := aStatusData.RecordId;
    FQuery.Prepare;
    FQuery.Open;
    IsExists := FQuery.FieldByName('CNT').AsInteger > 0;
    FQuery.Close;
  except
    on E: Exception do
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveRecordToDB', E.Message + TDModUtils.GetQueryInfo(FQuery));
  end;

  if IsExists then
    FQuery.SQL.Text := C_SQL_UPDATE_TEXT
  else
    FQuery.SQL.Text := C_SQL_INSERT_TEXT;
  if not FTransaction.Active then
    FTransaction.StartTransaction;

  if (not Terminated) then
  begin
    aStatusData.CheckValues;
    FQuery.ParamByName('NODE_ID').AsInteger             := aStatusData.NodeId;
    FQuery.ParamByName('ID').AsInteger                  := aStatusData.RecordId;
    FQuery.ParamByName('NODE_ID').AsInteger             := aStatusData.NodeId;
    FQuery.ParamByName('ORDER_ID').AsInteger            := aStatusData.OrderId;
    FQuery.ParamByName('CON_ID').AsInteger              := aStatusData.ConID;
    FQuery.ParamByName('SYMBOL').AsString               := aStatusData.Symbol;
    FQuery.ParamByName('CURRENCY').AsString             := aStatusData.Currency;
    FQuery.ParamByName('ORDER_TYPE').AsInteger          := Ord(aStatusData.OrderType);
    FQuery.ParamByName('ORDER_ACTION').AsInteger        := Ord(aStatusData.Action);
    FQuery.ParamByName('SECURITY_TYPE').AsInteger       := Ord(aStatusData.SecurityType);
    FQuery.ParamByName('ORDER_SCOPE').AsInteger         := aStatusData.OrderScope;
    FQuery.ParamByName('TEMPLATE_ID').AsInteger         := aStatusData.TemplateId;
    FQuery.ParamByName('QUALIFIER_ID').AsInteger        := aStatusData.QualifierId;
    FQuery.ParamByName('AUTOTRADES_INSTANCE').AsInteger := aStatusData.AutoTradesInstance;
    FQuery.ParamByName('QUALIFIER_INSTANCE').AsInteger  := aStatusData.QualifierInstance;
    FQuery.ParamByName('MOTHER_ORDER_ID').AsInteger     := aStatusData.MotherOrderId;
    FQuery.ParamByName('ADDED_TIME').AsDateTime         := aStatusData.AddedTime;
    FQuery.ParamByName('PRESUBMITTED_TIME').AsDateTime  := aStatusData.PresubmittedTime;
    FQuery.ParamByName('PRESUBMITTED_LP').AsFloat       := aStatusData.PresubmittedLP;
    FQuery.ParamByName('PENDSUBMIT_TIME').AsDateTime    := aStatusData.PendSubmitTime;
    FQuery.ParamByName('SUBMITTED_TIME').AsDateTime     := aStatusData.SubmittedTime;
    FQuery.ParamByName('SUBMITTED_LP').AsFloat          := aStatusData.SubmittedLP;
    FQuery.ParamByName('FILLED').AsFloat                := aStatusData.Filled;
    FQuery.ParamByName('FILLED_LP').AsFloat             := aStatusData.FilledLP;
    FQuery.ParamByName('CANCELLED_TIME').AsDateTime     := aStatusData.CancelledTime;
    FQuery.ParamByName('ERROR_TEXT').AsString           := aStatusData.Error;
    FQuery.ParamByName('QUANTITY').AsInteger            := aStatusData.Quantity;
    FQuery.ParamByName('LATEST_FILL_QTY').AsFloat       := aStatusData.LatestFillQty;
    FQuery.ParamByName('REMAINING').AsFloat             := aStatusData.Remaining;
    FQuery.ParamByName('FILL_PRICE').AsFloat            := aStatusData.FillPrice;
    FQuery.ParamByName('AVG_FILL_PRICE').AsFloat        := aStatusData.AvgFillPrice;
    FQuery.ParamByName('LAST_PRICE').AsFloat            := aStatusData.LastPrice;
    FQuery.ParamByName('AUX_PRICE').AsFloat             := aStatusData.AuxPrice;
    FQuery.ParamByName('TRAILING_PERCENT').AsFloat      := aStatusData.TrailingPercent;
    FQuery.ParamByName('TRAIL_STOP_PRICE').AsFloat      := aStatusData.TrailStopPrice;
    FQuery.ParamByName('LMT_PRICE_OFFSET').AsFloat      := aStatusData.LmtPriceOffset;
    FQuery.ParamByName('LAST_TIME').AsDateTime          := aStatusData.LastTimestamp;
    FQuery.ParamByName('STATUS').AsInteger              := Ord(aStatusData.Status);
    FQuery.ParamByName('IN_CALC').AsString              := aStatusData.InCalc;
    FQuery.ParamByName('SCAN_SEQUENCE_ID').AsInteger    := aStatusData.ScanSequenceId;
    FQuery.ParamByName('AUTO_TRADES_ID').AsInteger      := aStatusData.AutoTradesId;
    FQuery.ParamByName('ORDER_STATUS_ITEMS').AsString   := aStatusData.OrderStatusItems;
    try
      FQuery.Prepare;
      FQuery.ExecSQL;
      FTransaction.Commit;
    except
      on E: EIBInterBaseError do
      begin
        if FTransaction.Active then
          FTransaction.Rollback;
        if (E.SQLCode <> -803) and (E.SQLCode <> -913) then
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveRecordToDB', E.Message +
                                      ', SQLCode: ' + E.SQLCode.ToString +
                                      ', IBErrorCode: ' +  E.IBErrorCode.ToString +
                                      TDModUtils.GetQueryInfo(FQuery) +
                                      '<br>' + aStatusData.ToString);
        FQueue.PopItem(aStatusData);
      end;
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveRecordToDB', E.Message + TDModUtils.GetQueryInfo(FQuery) + '<br>' + aStatusData.ToString);
    end;
  end;
end;

{ TStatusData }

procedure TStatusData.Clear;
begin
  SetLength(Info, 0);
  Self := Default(TStatusData);
end;

procedure TStatusData.FromDB(aID: Integer);
resourcestring
  C_SQL_TEXT = 'SELECT * FROM ORDER_STATUS WHERE ID=:ID';
var
  Query: TFDQuery;
  Connection: TFDConnection;
  Transaction: TFDTransaction;
begin
  Self.Clear;
  Connection := TFireBirdConnect.CreateDatabase(TFireBirdConnect.DBNameFeed);
  try
    Connection.Connected := True;

    Transaction := TFDTransaction.Create(Connection);
    try
      Connection.Transaction := Transaction;
      Transaction.Connection := Connection;
      Transaction.Options.AutoCommit := false;
      Transaction.StartTransaction;

      Query := TFDQuery.Create(Connection);
      try
        Query.Connection := Connection;
        Query.Transaction := Transaction;
        Query.SQL.Text := C_SQL_TEXT;
        Query.ParamByName('ID').AsInteger := aID;
        Query.Transaction.CommitRetaining;
        try
          Query.Prepare;
          Query.Open;
          if not Query.IsEmpty then
          begin
            Self.NodeId             := Query.FieldByName('NODE_ID').AsInteger;
            Self.RecordId           := Query.FieldByName('ID').AsInteger;
            Self.OrderId            := Query.FieldByName('ORDER_ID').AsInteger;
            Self.ConID              := Query.FieldByName('CON_ID').AsInteger;
            Self.Symbol             := Query.FieldByName('SYMBOL').AsString;
            Self.Currency           := Query.FieldByName('CURRENCY').AsString;
            Self.OrderType          := TIABOrderType(Query.FieldByName('ORDER_TYPE').AsInteger);
            Self.Action             := TIABAction(Query.FieldByName('ORDER_ACTION').AsInteger);
            Self.SecurityType       := TIABSecurityType(Query.FieldByName('SECURITY_TYPE').AsInteger);
            Self.OrderScope         := Query.FieldByName('ORDER_SCOPE').AsInteger;
            Self.TemplateId         := Query.FieldByName('TEMPLATE_ID').AsInteger;
            Self.QualifierId        := Query.FieldByName('QUALIFIER_ID').AsInteger;
            Self.QualifierInstance  := Query.FieldByName('QUALIFIER_INSTANCE').AsInteger;
            Self.AutoTradesInstance := Query.FieldByName('AUTOTRADES_INSTANCE').AsInteger;
            Self.MotherOrderId      := Query.FieldByName('MOTHER_ORDER_ID').AsInteger;
            Self.AddedTime          := Query.FieldByName('ADDED_TIME').AsDateTime;
            Self.PendSubmitTime     := Query.FieldByName('PENDSUBMIT_TIME').AsDateTime;
            Self.PresubmittedTime   := Query.FieldByName('PRESUBMITTED_TIME').AsDateTime;
            Self.PresubmittedLP     := Query.FieldByName('PRESUBMITTED_LP').AsFloat;
            Self.SubmittedTime      := Query.FieldByName('SUBMITTED_TIME').AsDateTime;
            Self.SubmittedLP        := Query.FieldByName('SUBMITTED_LP').AsFloat;
            Self.Filled             := Query.FieldByName('FILLED').AsFloat;
            Self.FilledLP           := Query.FieldByName('FILLED_LP').AsFloat;
            Self.CancelledTime      := Query.FieldByName('CANCELLED_TIME').AsDateTime;
            Self.Error              := Query.FieldByName('ERROR_TEXT').AsString;
            Self.Quantity           := Query.FieldByName('QUANTITY').AsInteger;
            Self.LatestFillQty      := Query.FieldByName('LATEST_FILL_QTY').AsFloat;
            Self.Remaining          := Query.FieldByName('REMAINING').AsFloat;
            Self.FillPrice          := Query.FieldByName('FILL_PRICE').AsFloat;
            Self.AvgFillPrice       := Query.FieldByName('AVG_FILL_PRICE').AsFloat;
            Self.LastPrice          := Query.FieldByName('LAST_PRICE').AsFloat;
            Self.AuxPrice           := Query.FieldByName('AUX_PRICE').AsFloat;
            Self.TrailingPercent    := Query.FieldByName('TRAILING_PERCENT').AsFloat;
            Self.TrailStopPrice     := Query.FieldByName('TRAIL_STOP_PRICE').AsFloat;
            Self.LmtPriceOffset     := Query.FieldByName('LMT_PRICE_OFFSET').AsFloat;
            Self.LastTimestamp      := Query.FieldByName('LAST_TIME').AsDateTime;
            Self.Status             := TIABOrderState(Query.FieldByName('STATUS').AsInteger);
            Self.InCalc             := Query.FieldByName('IN_CALC').AsString;
            Self.ScanSequenceId     := Query.FieldByName('SCAN_SEQUENCE_ID').AsInteger;
            Self.AutoTradesId       := Query.FieldByName('AUTO_TRADES_ID').AsInteger;
            Self.OrderStatusItems   := Query.FieldByName('ORDER_STATUS_ITEMS').AsString;
            if (Self.NodeId <= 0) then
              Self.NodeId := 1000000 + General.GetNextNodeID;
          end;
        except
          on E: Exception do
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'FromDB', 'Entity.OrderStatus', E.Message + TDModUtils.GetQueryInfo(Query));
        end;
      finally
        FreeAndNil(Query);
      end;
    finally
      if Transaction.Active then
        Transaction.Commit;
      FreeAndNil(Transaction);
    end;
  finally
    if Connection.Connected then
      Connection.Connected := False;
    FreeAndNil(Connection);
  end;
end;

function TStatusData.GetOrderStatusItemsXml: string;
var
  OrderItem: TOrderItem;
begin
  Result := '';
  if Assigned(Self.NodeOrder) then
    if TMonitorLists.OrderList.ContainsKey(Self.NodeOrder) then
    begin
      OrderItem := TMonitorLists.OrderList.Items[Self.NodeOrder];
      if Assigned(OrderItem) then
        Result := OrderItem.ToXml(Self.OrderId);
    end;
end;

function TStatusData.ToString: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('RecordId=%d', [RecordId]).AppendLine
      .AppendFormat('NodeId=%d', [NodeId]).AppendLine
      .AppendFormat('QualifierId=%d', [QualifierId]).AppendLine
      .AppendFormat('AutoTradesId=%d', [AutoTradesId]).AppendLine
      .AppendFormat('ScanSequenceId=%d', [ScanSequenceId]).AppendLine
      .AppendFormat('OrderId=%d', [OrderId]).AppendLine
      .AppendFormat('MotherOrderId=%d', [MotherOrderId]).AppendLine
      .AppendFormat('ConID=%d', [ConID]).AppendLine
      .AppendFormat('QualifierInstance=%d', [QualifierInstance]).AppendLine
      .AppendFormat('AutoTradesInstance=%d', [AutoTradesInstance]).AppendLine
      .AppendFormat('TemplateId=%d', [TemplateId]).AppendLine
      .AppendFormat('Symbol=%s', [Symbol]).AppendLine
      .AppendFormat('Currency=%s', [Currency]).AppendLine
      .AppendFormat('OrderType=%s', [OrderType.ToString]).AppendLine
      .AppendFormat('Action=%s', [Action.ToString]).AppendLine
      .AppendFormat('SecurityType=%s', [SecurityType.ToString]).AppendLine
      .AppendFormat('Quantity=%d', [Quantity]).AppendLine
      .AppendFormat('OrderScope=%d', [OrderScope]).AppendLine
      .AppendFormat('OCA=%d', [OCA]).AppendLine
      .AppendFormat('AddedTime=%s', [FormatDateTime('yyyy.mm.dd hh.nn.ss.zzz', AddedTime)]).AppendLine
      .AppendFormat('PresubmittedTime=%s', [FormatDateTime('yyyy.mm.dd hh.nn.ss.zzz', PresubmittedTime)]).AppendLine
      .AppendFormat('CancelledTime=%s', [FormatDateTime('yyyy.mm.dd hh.nn.ss.zzz', CancelledTime)]).AppendLine
      .AppendFormat('LastTimestamp=%s', [FormatDateTime('yyyy.mm.dd hh.nn.ss.zzz', LastTimestamp)]).AppendLine
      .AppendFormat('PresubmittedLP=%g', [PresubmittedLP]).AppendLine
      .AppendFormat('SubmittedTime=%s', [FormatDateTime('yyyy.mm.dd hh.nn.ss.zzz', SubmittedTime)]).AppendLine
      .AppendFormat('SubmittedLP=%g', [SubmittedLP]).AppendLine
      .AppendFormat('Filled=%g', [Filled]).AppendLine
      .AppendFormat('FilledLP=%g', [FilledLP]).AppendLine
      .AppendFormat('Error=%s', [Error]).AppendLine
      .AppendFormat('LatestFillQty=%g', [LatestFillQty]).AppendLine
      .AppendFormat('Remaining=%g', [Remaining]).AppendLine
      .AppendFormat('FillPrice=%g', [FillPrice]).AppendLine
      .AppendFormat('AvgFillPrice=%g', [AvgFillPrice]).AppendLine
      .AppendFormat('LastPrice=%g', [LastPrice]).AppendLine
      .AppendFormat('AuxPrice=%g', [AuxPrice]).AppendLine
      .AppendFormat('TrailingPercent=%g', [TrailingPercent]).AppendLine
      .AppendFormat('TrailStopPrice=%g', [TrailStopPrice]).AppendLine
      .AppendFormat('LmtPriceOffset=%g', [LmtPriceOffset]).AppendLine
      .AppendFormat('Status=%s', [Status.ToString]).AppendLine
      .AppendFormat('InCalc=%s', [InCalc]).AppendLine;
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

procedure TStatusData.CheckValues;
begin
  if Self.AuxPrice.IsNan or Self.AuxPrice.IsInfinity then
    Self.AuxPrice := 0;
end;

{ TStorageOrders }

constructor TStorageOrders.Create;
begin
  inherited Create([doOwnsValues]);
end;

destructor TStorageOrders.Destroy;
begin
  FThread.Terminate;
  WaitForSingleObject(FThread.Handle, INFINITE);
  TPublishers.OrderStatusPublisher.Unsubscribe(Self);
  inherited;
end;

procedure TStorageOrders.AfterConstruction;
begin
  inherited;
  TPublishers.OrderStatusPublisher.Subscribe(Self);
  FThread := TThreadStorageOrders.Create;
  FThread.Start;
end;

function TStorageOrders.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TStorageOrders.OnStatusUpdate(const aStatus: TIABOrderState; const aOrder: TIABOrder; aNodeOrder: PVirtualNode; const aInfo: string);
var
  MonitorData : PTreeData;
  OrderDoc    : TOrderIBDoc;
  OrderIBId   : Integer;
  StatusData  : TStatusData;
begin
  if not FThread.Started then
    Exit;

  if Assigned(aOrder) then
    if not Assigned(aNodeOrder) then
      aNodeOrder := TMonitorLists.OrderList.GetNodeOrder(aOrder.TempId);

  if Assigned(aNodeOrder) then
  begin
    MonitorData := PVirtualNode(aNodeOrder)^.GetData;
    if Assigned(MonitorData^.OrderDoc) then
    begin
      OrderDoc := TOrderIBDoc(MonitorData^.OrderDoc);
      if Assigned(aOrder) then
      begin
        OrderIBId := aOrder.TempId;
        if (aOrder.TrailingPercent = UNSET_DOUBLE) then
          aOrder.TrailingPercent := 0;
        if (aOrder.TrailStopPrice = UNSET_DOUBLE) then
          aOrder.TrailStopPrice := 0;
        if (aOrder.AuxPrice = UNSET_DOUBLE) then
          aOrder.AuxPrice := 0;
        if (aOrder.LmtPriceOffset = UNSET_DOUBLE) then
          aOrder.LmtPriceOffset := 0;
        if (aOrder.FilledQuantity = UNSET_DOUBLE) then
          aOrder.FilledQuantity := 0;
      end
      else
        OrderIBId := TOrderIBDoc(MonitorData^.OrderDoc).OrderIBId;

      if (OrderIBId > 0) then
      begin
        if not Self.ContainsKey(OrderIBId) then
        begin
          StatusData := Default(TStatusData);
          Self.AddOrSetValue(OrderIBId, StatusData);
        end
        else
          StatusData := Self.Items[OrderIBId];

        StatusData.NodeId             := MonitorData.NodeId;
        StatusData.Action             := OrderDoc.OrderAction;
        StatusData.AutoTradesId       := OrderDoc.AutoTradesId;
        StatusData.AutoTradesInstance := OrderDoc.AutoTradesInstance;
        StatusData.ConID              := OrderDoc.Id;
        StatusData.Currency           := OrderDoc.Currency;
        StatusData.IsArchived         := False;
        StatusData.LastPrice          := TMonitorLists.PriceCache.GetLastPrice(OrderDoc.Id, ttLast);
        StatusData.LastTimestamp      := Now;
        StatusData.MotherOrderId      := OrderDoc.ParentIBId;
        StatusData.NodeOrder          := aNodeOrder;
        StatusData.OCA                := OrderDoc.OcaGroupNumber;
        StatusData.OrderId            := OrderIBId;
        StatusData.OrderScope         := OrderDoc.Scope;
        StatusData.OrderStatusItems   := StatusData.GetOrderStatusItemsXml;
        StatusData.OrderType          := OrderDoc.OrderType;
        StatusData.QualifierId        := OrderDoc.QualifierId;
        StatusData.QualifierInstance  := OrderDoc.QualifierInstance;
        StatusData.Quantity           := OrderDoc.Quantity;
        StatusData.SecurityType       := OrderDoc.SecurityType;
        StatusData.Status             := aStatus;
        StatusData.Symbol             := OrderDoc.Symbol;

        if (StatusData.MotherOrderId <= 0) then
          if Assigned(OrderDoc.ParentOrder) then
            StatusData.MotherOrderId := OrderDoc.ParentOrder.OrderIBId;

        case aStatus of
          osPendSubmit:
            StatusData.AddedTime := Now;
          osPreSubmit:
            begin
              StatusData.PresubmittedTime := Now;
              StatusData.PresubmittedLP   := TMonitorLists.PriceCache.GetLastPrice(OrderDoc.Id, ttLast);
            end;
          osSubmitted:
          begin
            StatusData.SubmittedTime := Now;
            StatusData.SubmittedLP := TMonitorLists.PriceCache.GetLastPrice(OrderDoc.Id, ttLast);
          end;
          osCancelled:
            StatusData.CancelledTime := Now;
          osFilled, osPartlyFilled:
            begin
              if Assigned(aOrder) then
              begin
                StatusData.Filled       := aOrder.Filled;
                StatusData.FillPrice    := aOrder.FillPrice;
                StatusData.FilledLP     := aOrder.FillPrice;
                StatusData.AvgFillPrice := OrderDoc.AvgPrice;
              end
              else
                StatusData.FilledLP := TMonitorLists.PriceCache.GetLastPrice(OrderDoc.Id, ttLast);
            end;
          osError:
            StatusData.Error := aInfo;
        end;

        case OrderDoc.OrderType of
          otLimitTouch, otLimit, otLimitClose, otStopLimit:
            begin
              StatusData.InCalc := FormatFloat(C_CURRENCY_FORMAT, OrderDoc.ExtendedOptions.LimitPriceRelative) + '% / '+ FormatFloat(C_CURRENCY_FORMAT, OrderDoc.Limit);
              if Assigned(aOrder) then
                StatusData.LmtPriceOffset := aOrder.LmtPriceOffset;
            end;
          otStop:
            StatusData.InCalc := FormatFloat(C_CURRENCY_FORMAT, OrderDoc.ExtendedOptions.AuxPriceRelative) + '% / ' + FormatFloat(C_CURRENCY_FORMAT, OrderDoc.AuxPrice);
          otTrail:
            begin
              StatusData.InCalc := FormatFloat(C_CURRENCY_FORMAT, OrderDoc.ExtendedOptions.TrailStopPriceRelative) + '% / ' + FormatFloat(C_CURRENCY_FORMAT, OrderDoc.TrailStopPrice);
              if Assigned(aOrder) then
              begin
                StatusData.TrailingPercent := aOrder.TrailingPercent;
                StatusData.TrailStopPrice  := aOrder.TrailStopPrice;
              end;
            end;
          otMarket, otMarketClose, otMarketOpen:
            if Assigned(aOrder) then
              StatusData.AuxPrice := aOrder.AuxPrice;
        end;

        if Assigned(aOrder) then
        begin
          StatusData.LatestFillQty := aOrder.LatestFillQty;
          StatusData.AuxPrice      := aOrder.AuxPrice;
          if (OrderDoc.Quantity - aOrder.Filled) < StatusData.Remaining then
            StatusData.Remaining := OrderDoc.Quantity - aOrder.Filled;
        end;

        if (StatusData.RecordId <= 0) then
          StatusData.RecordId := DMod.GetNextValue('GEN_ORDER_STATUS_ID', DMod.ConnectionFeed);

        Self.AddOrSetValue(OrderIBId, StatusData);
        FThread.StatusQueue.PushItem(StatusData);
      end;
    end;
  end;
end;

end.
