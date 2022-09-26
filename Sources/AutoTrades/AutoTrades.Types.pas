unit AutoTrades.Types;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Generics.Collections,
  Winapi.msxml, Vcl.Graphics, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} DebugWriter, System.DateUtils,
  XmlFiles, Data.DB, DaModule, System.Math, HtmlLib, DaModule.Utils, VirtualTrees, Common.Types,
  Publishers, Global.Types, Publishers.Interfaces, Vcl.Forms, IABFunctions.RequestsQueue, IABSocketAPI_const,
  System.StrUtils, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet, Utils,
  Qualifiers.Types, Candidate.Types, Quantities.Types, OrderTemplate.Types;
{$ENDREGION}

type
  TTradesState = (tsSuspended, tsRestarted, tsExecuted, tsWorking, tsCancelled, tsFinished, tsNotConsidered);
  TTradesStateHelper = record helper for TTradesState
  private const
    TradesStateString: array [TTradesState] of string = ('Suspended', 'Restarted', 'Executed', 'Working', 'Cancelled', 'Finished', 'Not Considered');
    TradesStateColor: array [TTradesState] of TColor = (clWhite, clWhite, clWebMintcream, clWebMintcream, clWebGainsboro, clWebGainsboro, clSilver);
  public
    function ToString: string;
    function ToColor: TColor;
  end;

  TAutoTradeInfo = class;

  TOnConditionChanged = procedure(ASender: TAutoTradeInfo) of object;

  IAutoTrade = interface(ICustomInterface)
    ['{DAE179CA-C4F7-47C2-BB57-92488AF6B3F5}']
    function GetAutoTradeInfo: TAutoTradeInfo;
    function GetTradesState: TTradesState;
    procedure SetAutoTradeInfo(const aAutoTradeInfo: TAutoTradeInfo);
    procedure CloseAutoTrade(const aSilenceMode: Boolean = False);
    procedure SetTradesState(const aValue: TTradesState);
  end;

  TAutoTradeInfo = class(TBaseClass)
  private
    FOrderGroupId: Integer;
    FOnConditionChanged: TOnConditionChanged;
    function GetOrderGroupId: Integer;
    procedure SetOrderGroupId(const Value: Integer);
    procedure ConditionChanged;
  public
    [weak] AutoTradesInstance: IAutoTrade;
    InstanceNum: Integer;
    QualifierInstance: Integer;
    ColumnsInfo: string;
    OrderAmount: Integer;
    OrderCurrency: string;
    MaxRows: Integer;
    MaxNumberOrder: Integer;
    AllowSendDuplicateOrder: Boolean;
    AutoRefresh: Boolean;
    Active: Boolean;
    Enabled: Boolean;
    Note: string;
    Columns: string;
    CreatedOrdersCount: Integer;
    LastUpdate: TDateTime;
    ScanCount: Integer;
    TradesState: TTradesState;
    OwnerNode: PVirtualNode;
    TotalOrderAmount: Integer;
    CreateTime: TTime;
    HistoricalDataParams: THistoricalDataParams;

    Qualifier: TQualifier;
    Candidate: TCandidate;
    Quantity: TQuantity;
    OrderTemplate: TOrderTemplate;

    constructor Create; override;
    destructor Destroy; override;
    procedure FromDB(aID: Integer); override;
    procedure SaveToDB; override;
    class function GetListSQL: string; override;
    class function GetListCaption: string; override;
    class procedure DeleteFromDB(aID: Integer); override;

    function IsEquals(AAutoTradeInfo: TAutoTradeInfo): Boolean;
    function ToList: string;
    function ToValueString: string;
    procedure AssignFrom(aAutoTrade: TAutoTradeInfo);
    procedure Clear;
    procedure FromList(aValue: string);
    property OrderGroupId: Integer read GetOrderGroupId write SetOrderGroupId;
    property OnConditionChanged: TOnConditionChanged read FOnConditionChanged write FOnConditionChanged;
  end;

  IAutoTradesController = interface(ICustomInterface)
    ['{9ACBE119-C940-4DAD-B002-7CB7A4CEEB92}']
    procedure DeleteAutoTrade(const aInstanceNum: Integer; const aSilenceMode: Boolean = False);
    procedure SetInfo(const aInstanceNum: Integer; const aInfo: string);
    procedure UpdateState(const aAutoTrade: IAutoTrade);
  end;

  TAutoTradesControllerPublisher = class(TCustomPublisher)
  public
    procedure DeleteAutoTrade(const aInstanceNum: Integer; const aSilenceMode: Boolean = False);
    procedure SetInfo(const aInstanceNum: Integer; const aInfo: string);
    procedure UpdateState(const aAutoTrade: IAutoTrade);
  end;

  PTradesData = ^TTradesData;
  TTradesData = record
    [weak] AutoTradesInstance : IAutoTrade;
    InstanceNum        : Integer;
    AutotradesName     : string;
    Qualifier          : string;
    MarketScannerId    : string;
    AutoTradesId       : Integer;
    QualifierId        : Integer;
    QualifierInstance  : Integer;
    NodeType           : TNodeType;
    GroupText          : string;
    StartedTime        : TDateTime;
    StoppedTime        : TDateTime;
    RestartTime        : TDateTime;
    OrderGroupId       : Integer;
    State              : TTradesState;
    Currency           : string;
    MaxNumberOrder     : Integer;
    CreatedOrdersCount : Integer;
    MaxRows            : Integer;
    OrderAmount        : Integer;
    OrderRankingSum    : Integer;
    LastUpdate         : TDateTime;
    ScanCount          : Integer;
    Columns            : string;
    Info               : string;
    CreateTime         : TTime;
    procedure AssignFrom(aAutoTradeInfo: TAutoTradeInfo);
    procedure Clear;
  end;

  TAutoTradeCollection = class(TObjectList<TAutoTradeInfo>)
  public
    procedure LoadActiveAutotrades;
  end;

var
  AutoTradesControllerPublisher: TAutoTradesControllerPublisher;
  AutoTradeCollection: TAutoTradeCollection;

implementation

{ TAutoTradeInfo }

procedure TAutoTradeInfo.AssignFrom(aAutoTrade: TAutoTradeInfo);
begin
  Self.Clear;
  Self.InstanceNum             := aAutoTrade.InstanceNum;
  Self.RecordId                := aAutoTrade.RecordId;
  Self.QualifierInstance       := aAutoTrade.QualifierInstance;
  Self.ColumnsInfo             := aAutoTrade.ColumnsInfo;
  Self.OrderGroupId            := aAutoTrade.OrderGroupId;
  Self.Name                    := aAutoTrade.Name;
  Self.OrderAmount             := aAutoTrade.OrderAmount;
  Self.OrderCurrency           := aAutoTrade.OrderCurrency;
  Self.MaxRows                 := aAutoTrade.MaxRows;
  Self.MaxNumberOrder          := aAutoTrade.MaxNumberOrder;
  Self.AllowSendDuplicateOrder := aAutoTrade.AllowSendDuplicateOrder;
  Self.AutoRefresh             := aAutoTrade.AutoRefresh;
  Self.Active                  := aAutoTrade.Active;
  Self.Enabled                 := aAutoTrade.Enabled;
  Self.Note                    := aAutoTrade.Note;
  Self.Columns                 := aAutoTrade.Columns;
  Self.CreatedOrdersCount      := aAutoTrade.CreatedOrdersCount;
  Self.LastUpdate              := aAutoTrade.LastUpdate;
  Self.ScanCount               := aAutoTrade.ScanCount;
  Self.TradesState             := aAutoTrade.TradesState;
  Self.TotalOrderAmount        := aAutoTrade.TotalOrderAmount;
  Self.HistoricalDataParams    := aAutoTrade.HistoricalDataParams;
  Self.Qualifier.AssignFrom(aAutoTrade.Qualifier);
  Self.Candidate.AssignFrom(aAutoTrade.Candidate);
  Self.Quantity.AssignFrom(aAutoTrade.Quantity);
  Self.OrderTemplate.AssignFrom(aAutoTrade.OrderTemplate);
end;

procedure TAutoTradeInfo.Clear;
begin
  AutoRefresh := False;
  Active := False;
  Enabled := True;
  RecordId := -1;
  ColumnsInfo := '';
  OrderGroupId := -1;
  QualifierInstance := 0;
  AllowSendDuplicateOrder := False;
  InstanceNum := 0;
  LastUpdate := 0;
  ScanCount := 0;
  TotalOrderAmount := 0;
  HistoricalDataParams := Default(THistoricalDataParams);
  Qualifier.Clear;
  Candidate.Clear;
  Quantity.Clear;
  OrderTemplate.Clear;
end;

procedure TAutoTradeInfo.ConditionChanged;
begin
  if Assigned(FOnConditionChanged) then
    FOnConditionChanged(Self);
end;

constructor TAutoTradeInfo.Create;
begin
  Qualifier := TQualifier.Create;
  Qualifier.ManualFree := true;
  Qualifier.OnConditionChanged := ConditionChanged;
  Candidate := TCandidate.Create;
  Quantity := TQuantity.Create;
  OrderTemplate := TOrderTemplate.Create;

  Clear;
end;

class procedure TAutoTradeInfo.DeleteFromDB(aID: Integer);
resourcestring
  C_SQL_DELETE = 'DELETE FROM AUTOTRADES WHERE ID=%d';
begin
  DMod.ExecuteSQL(Format(C_SQL_DELETE, [aID]));
  DMod.RefreshQuery(DMod.fbqAutoTrades);
end;

destructor TAutoTradeInfo.Destroy;
begin
  if Assigned(Qualifier) then
  begin
    Qualifier.DeInitialize;
    Qualifier.Free;
  end;
  if Assigned(Candidate) then
    Candidate.Free;
  if Assigned(Quantity) then
    Quantity.Free;
  if Assigned(OrderTemplate) then
    OrderTemplate.Free;
  inherited;
end;

procedure TAutoTradeInfo.FromDB(aID: Integer);
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT * FROM AUTOTRADES WHERE ID=:ID';
var
  Query: TFDQuery;
  Num: Integer;
begin
  Num := InstanceNum;
  Self.Clear;
  InstanceNum := Num;
  if (aID > 0) then
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := C_SQL_SELECT_TEXT;
      Query.ParamByName('ID').AsInteger := aID;
      Query.Open;
      if not Query.IsEmpty then
      begin
        Self.RecordId                := aID;
        Self.Name                    := Query.FieldByName('NAME').AsString;
        Self.ColumnsInfo             := Query.FieldByName('COLUMNS_INFO').AsString;
        Self.OrderGroupId            := Query.FieldByName('ORDER_GROUP_ID').AsInteger;
        Self.OrderAmount             := Query.FieldByName('ORDER_AMOUNT').AsInteger;
        Self.AutoRefresh             := Query.FieldByName('AUTO_REFRESH').AsBoolean;
        Self.OrderCurrency           := Query.FieldByName('ORDER_CURRENCY').AsString;
        Self.MaxNumberOrder          := Query.FieldByName('MAX_NUMBER_ORDER').AsInteger;
        Self.TotalOrderAmount        := Query.FieldByName('TOTAL_ORDER_AMOUNT').AsInteger;
        Self.MaxRows                 := Query.FieldByName('MAX_ROWS').AsInteger;
        Self.AllowSendDuplicateOrder := Query.FieldByName('ALLOW_SEND_DUPLICATE_ORDER').AsBoolean;
        Self.Enabled                 := Query.FieldByName('ENABLED').AsBoolean;
        Self.Note                    := Query.FieldByName('NOTE').AsString;
        Self.CreateTime              := Now;
        Self.HistoricalDataParams.SubscribeHistData := Query.FieldByName('SUBSCRIBE_HIST_DATA').AsBoolean;
        Self.HistoricalDataParams.DataBasis         := TIABHistoricalDataType(Query.FieldByName('HIST_DATA_BASIS').AsInteger);
        Self.HistoricalDataParams.DataDuration      := Query.FieldByName('HIST_DATA_DURATION').AsInteger;
        Self.HistoricalDataParams.DurationTimeUnits := Query.FieldByName('HIST_DURATION_TIME_UNITS').AsInteger;
        Self.HistoricalDataParams.KeepUpdated       := Query.FieldByName('HIST_DATA_KEEP_UPDATED').AsBoolean;
        Self.HistoricalDataParams.SubscribeHistData := Query.FieldByName('SUBSCRIBE_HIST_DATA').AsBoolean;
        Self.HistoricalDataParams.BarSize           := TIABChartBarSize(Query.FieldByName('HIST_BAR_SIZE').AsInteger);

        Self.Qualifier.FromDB(Query.FieldByName('QUALIFIER_ID').AsInteger);
        Self.Candidate.FromDB(Query.FieldByName('CANDIDATE_ID').AsInteger);
        Self.Quantity.FromDB(Query.FieldByName('QUANTITY_ID').AsInteger);
        Self.OrderTemplate.FromDB(Query.FieldByName('ORDER_TEMPLATE_ID').AsInteger);
        Self.Active                  := Query.FieldByName('ACTIVE').AsBoolean;
      end;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TAutoTradeInfo.FromList(aValue: string);
begin

end;

function TAutoTradeInfo.IsEquals(AAutoTradeInfo: TAutoTradeInfo): Boolean;
begin
  Result := Self.ToList.Equals(AAutoTradeInfo.ToList);
end;

procedure TAutoTradeInfo.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM AUTOTRADES WHERE ID=:RecordId';
  C_SQL_INSERT_TEXT = 'INSERT INTO AUTOTRADES(ID, NAME, COLUMNS_INFO, ORDER_GROUP_ID, ORDER_AMOUNT, ORDER_CURRENCY, ' +
                                             'MAX_NUMBER_ORDER, TOTAL_ORDER_AMOUNT, MAX_ROWS, ALLOW_SEND_DUPLICATE_ORDER, ' +
                                             'NOTE, ENABLED, AUTO_REFRESH, SUBSCRIBE_HIST_DATA, HIST_DATA_KEEP_UPDATED, ' +
                                             'HIST_DURATION_TIME_UNITS, HIST_DATA_DURATION, HIST_DATA_BASIS, HIST_BAR_SIZE, '+
                                             'QUALIFIER_ID, CANDIDATE_ID, QUANTITY_ID, ORDER_TEMPLATE_ID, ACTIVE) ' +
                                    ' VALUES(:ID,:NAME,:COLUMNS_INFO,:ORDER_GROUP_ID,:ORDER_AMOUNT,:ORDER_CURRENCY, ' +
                                            ':MAX_NUMBER_ORDER,:TOTAL_ORDER_AMOUNT,:MAX_ROWS,:ALLOW_SEND_DUPLICATE_ORDER,' +
                                            ':NOTE,:ENABLED,:AUTO_REFRESH,:SUBSCRIBE_HIST_DATA,:HIST_DATA_KEEP_UPDATED,' +
                                            ':HIST_DURATION_TIME_UNITS,:HIST_DATA_DURATION,:HIST_DATA_BASIS,:HIST_BAR_SIZE,'+
                                            ':QUALIFIER_ID,:CANDIDATE_ID,:QUANTITY_ID,:ORDER_TEMPLATE_ID,:ACTIVE); ';
  C_SQL_UPDATE_TEXT = 'UPDATE AUTOTRADES SET '                                   +
                      'NAME=:NAME,  '                                            +
                      'COLUMNS_INFO=:COLUMNS_INFO, '                             +
                      'ORDER_GROUP_ID=:ORDER_GROUP_ID, '                   +
                      'ORDER_AMOUNT=:ORDER_AMOUNT, '                             +
                      'ORDER_CURRENCY=:ORDER_CURRENCY, '                         +
                      'TOTAL_ORDER_AMOUNT=:TOTAL_ORDER_AMOUNT, '                 +
                      'MAX_NUMBER_ORDER=:MAX_NUMBER_ORDER, '                     +
                      'MAX_ROWS=:MAX_ROWS, '                                     +
                      'ALLOW_SEND_DUPLICATE_ORDER=:ALLOW_SEND_DUPLICATE_ORDER, ' +
                      'AUTO_REFRESH=:AUTO_REFRESH, '                             +
                      'NOTE=:NOTE, '                                             +
                      'ENABLED=:ENABLED, '                                       +
                      'SUBSCRIBE_HIST_DATA=:SUBSCRIBE_HIST_DATA, '               +
                      'HIST_DURATION_TIME_UNITS=:HIST_DURATION_TIME_UNITS, '     +
                      'HIST_DATA_DURATION=:HIST_DATA_DURATION, '                 +
                      'HIST_DATA_BASIS=:HIST_DATA_BASIS, '                       +
                      'HIST_BAR_SIZE=:HIST_BAR_SIZE, '                           +
                      'HIST_DATA_KEEP_UPDATED=:HIST_DATA_KEEP_UPDATED, '         +

                      'QUALIFIER_ID=:QUALIFIER_ID, '                             +
                      'CANDIDATE_ID=:CANDIDATE_ID, '                             +
                      'QUANTITY_ID=:QUANTITY_ID, '                               +
                      'ORDER_TEMPLATE_ID=:ORDER_TEMPLATE_ID, '                   +
                      'ACTIVE=:ACTIVE '                                          +
                      'WHERE ID=:ID;';
var
  IsExists: Boolean;
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    IsExists := False;
    if (Self.RecordId > 0) then
    begin
      Query.SQL.Text := C_SQL_EXISTS_TEXT;
      try
        Query.ParamByName('RecordId').AsInteger := Self.RecordId;
        Query.Prepare;
        Query.Open;
        IsExists := Query.FieldByName('CNT').AsInteger > 0;
        Query.Close;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TAutoTradeInfo.SaveToDB', 'ScannerTypes', E.Message + TDModUtils.GetQueryInfo(Query));
          raise;
        end;
      end;
    end
    else
      Self.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');

    if IsExists then
      Query.SQL.Text := C_SQL_UPDATE_TEXT
    else
      Query.SQL.Text := C_SQL_INSERT_TEXT;

    if Self.Name.IsEmpty then
      Self.Name := 'AutoTrades nr' + Self.RecordId.ToString;

    Query.ParamByName('ID').AsInteger                         := Self.RecordId;
    Query.ParamByName('NAME').AsString                        := Copy(Self.Name, 0, 100);
    Query.ParamByName('COLUMNS_INFO').AsString                := Self.ColumnsInfo;
    Query.ParamByName('AUTO_REFRESH').AsBoolean               := Self.AutoRefresh;
    Query.ParamByName('ALLOW_SEND_DUPLICATE_ORDER').AsBoolean := Self.AllowSendDuplicateOrder;
    Query.ParamByName('MAX_NUMBER_ORDER').AsInteger           := Self.MaxNumberOrder;
    Query.ParamByName('MAX_ROWS').AsInteger                   := Self.MaxRows;
    Query.ParamByName('TOTAL_ORDER_AMOUNT').AsInteger         := Self.TotalOrderAmount;
    Query.ParamByName('ORDER_AMOUNT').AsInteger               := Self.OrderAmount;
    Query.ParamByName('ORDER_CURRENCY').AsString              := Self.OrderCurrency.Substring(0, 10);
    Query.ParamByName('ORDER_GROUP_ID').AsInteger             := Self.OrderGroupId;
    Query.ParamByName('NOTE').AsString                        := Self.Note.Substring(0, 500);
    Query.ParamByName('ENABLED').AsBoolean                    := Self.Enabled;
    Query.ParamByName('SUBSCRIBE_HIST_DATA').AsBoolean        := Self.HistoricalDataParams.SubscribeHistData;
    Query.ParamByName('HIST_DATA_KEEP_UPDATED').AsBoolean     := Self.HistoricalDataParams.KeepUpdated;
    Query.ParamByName('HIST_DURATION_TIME_UNITS').AsInteger   := Self.HistoricalDataParams.DurationTimeUnits;
    Query.ParamByName('HIST_DATA_DURATION').AsInteger         := Self.HistoricalDataParams.DataDuration;
    Query.ParamByName('HIST_DATA_BASIS').AsInteger            := Ord(Self.HistoricalDataParams.DataBasis);
    Query.ParamByName('HIST_BAR_SIZE').AsInteger              := Ord(Self.HistoricalDataParams.BarSize);

    Query.ParamByName('QUALIFIER_ID').Value                   := NullIf(Self.Qualifier.RecordId, -1);
    Query.ParamByName('CANDIDATE_ID').Value                   := NullIf(Self.Candidate.RecordId, -1);
    Query.ParamByName('QUANTITY_ID').Value                    := NullIf(Self.Quantity.RecordId, -1);
    Query.ParamByName('ORDER_TEMPLATE_ID').Value              := NullIf(Self.OrderTemplate.RecordId, -1);
    Query.ParamByName('ACTIVE').AsBoolean                     := Self.Active;
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TAutoTradeInfo.SaveToDB', 'ScannerTypes', E.Message + TDModUtils.GetQueryInfo(Query));
    end;
  finally
    FreeAndNil(Query);
    DMod.RefreshQuery(DMod.fbqAutoTrades);
  end;
end;

class function TAutoTradeInfo.GetListCaption: string;
begin
  Result := 'Autotrades list';
end;

class function TAutoTradeInfo.GetListSQL: string;
begin
  Result := 'SELECT ID, NAME, ACTIVE FROM AUTOTRADES ORDER BY LOWER(NAME)';
end;

function TAutoTradeInfo.GetOrderGroupId: Integer;
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT RECORD_ID FROM DOC_RELATIONS WHERE DOC_TYPE=3 AND PARENT_ID IN (SELECT ID FROM DOC_RELATIONS WHERE RECORD_ID=:RecordId)';
var
  Query: TFDQuery;
begin
  Result := FOrderGroupId;
  if (FOrderGroupId <= 0) then
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.Transaction := Query.Connection.Transaction;
      Query.SQL.Text := C_SQL_SELECT_TEXT;
      Query.ParamByName('RecordId').AsInteger := RecordId;
      try
        Query.Prepare;
        Query.Open;
        if not Query.IsEmpty then
          Result := Query.FieldByName('RECORD_ID').AsInteger;
        Query.Transaction.CommitRetaining;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TAutoTradeInfo', 'GetOrderGroupId', E.Message + TDModUtils.GetQueryInfo(Query));
          raise;
        end;
      end;
      FOrderGroupId := Result;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TAutoTradeInfo.SetOrderGroupId(const Value: Integer);
begin
  FOrderGroupId := Value;
end;

function TAutoTradeInfo.ToList: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('RecordId=%d', [RecordId]).AppendLine
      .AppendFormat('Name=%s', [Name]).AppendLine
      .AppendFormat('ColumnsInfo=%s', [ColumnsInfo]).AppendLine
      .AppendFormat('OrderGroupId=%d', [OrderGroupId]).AppendLine
      .AppendFormat('OrderAmount=%d', [OrderAmount]).AppendLine
      .AppendFormat('OrderCurrency=%s', [OrderCurrency]).AppendLine
      .AppendFormat('MaxNumberOrder=%d', [MaxNumberOrder]).AppendLine
      .AppendFormat('MaxRows=%d', [MaxRows]).AppendLine
      .AppendFormat('QualifierId=%d', [Qualifier.RecordId]).AppendLine
      .AppendFormat('QualifierInstance=%d', [QualifierInstance]).AppendLine
      .AppendFormat('TotalOrderAmount=%d', [TotalOrderAmount]).AppendLine
      .AppendFormat('AllowSendDuplicateOrder=%s', [BoolToStr(AllowSendDuplicateOrder, True)]).AppendLine
      .AppendFormat('HistoricalDataParams.SubscribeHistData=%s', [BoolToStr(HistoricalDataParams.SubscribeHistData, False)]).AppendLine
      .AppendFormat('HistoricalDataParams.KeepUpdated=%s', [BoolToStr(HistoricalDataParams.KeepUpdated, False)]).AppendLine
      .AppendFormat('HistoricalDataParams.DurationTimeUnits=%d', [HistoricalDataParams.DurationTimeUnits]).AppendLine
      .AppendFormat('HistoricalDataParams.DataDuration=%d', [HistoricalDataParams.DataDuration]).AppendLine
      .AppendFormat('HistoricalDataParams.DataBasis=%d', [Ord(HistoricalDataParams.DataBasis)]).AppendLine;
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

function TAutoTradeInfo.ToValueString: string;
begin
  Result := 'TOTAL:' + TotalOrderAmount.ToString +
            ' SINGLE:' + OrderAmount.ToString +
            ' MAX:' + MaxNumberOrder.ToString +
            ' LMT:' + MaxRows.ToString +
            IfThen(HistoricalDataParams.SubscribeHistData, ' HIST', '');
end;

{ TTradesData }

procedure TTradesData.AssignFrom(aAutoTradeInfo: TAutoTradeInfo);
begin
  Self.AutoTradesId       := aAutoTradeInfo.RecordId;
  Self.QualifierId        := aAutoTradeInfo.Qualifier.RecordId;
  Self.QualifierInstance  := aAutoTradeInfo.QualifierInstance;
  Self.AutotradesName     := aAutoTradeInfo.Name;
  Self.Columns            := aAutoTradeInfo.Columns;
  Self.CreatedOrdersCount := aAutoTradeInfo.CreatedOrdersCount;
  Self.Currency           := aAutoTradeInfo.OrderCurrency;
  Self.InstanceNum        := aAutoTradeInfo.InstanceNum;
  Self.LastUpdate         := aAutoTradeInfo.LastUpdate;
  Self.MaxNumberOrder     := aAutoTradeInfo.MaxNumberOrder;
  Self.MaxRows            := aAutoTradeInfo.MaxRows;
  Self.NodeType           := TNodeType.ntNode;
  Self.OrderAmount        := aAutoTradeInfo.OrderAmount;
  Self.OrderGroupId       := aAutoTradeInfo.OrderGroupId;
  Self.Qualifier          := DMod.GetStringValueFromSQL('SELECT NAME FROM QUALIFIERS WHERE ID=' + aAutoTradeInfo.Qualifier.RecordId.ToString, 'NAME');
  Self.ScanCount          := aAutoTradeInfo.ScanCount;
  Self.CreateTime         := aAutoTradeInfo.CreateTime;
end;

procedure TTradesData.Clear;
begin
  Self := Default(TTradesData);
end;

{ TAutoTradesControllerPublisher }

procedure TAutoTradesControllerPublisher.DeleteAutoTrade(const aInstanceNum: Integer; const aSilenceMode: Boolean = False);
var
  Item: TObject;
  atc: IAutoTradesController;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IAutoTradesController, atc) then
        atc.DeleteAutoTrade(aInstanceNum, aSilenceMode);
    end;
end;

procedure TAutoTradesControllerPublisher.SetInfo(const aInstanceNum: Integer; const aInfo: string);
var
  Item: TObject;
  atc: IAutoTradesController;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IAutoTradesController, atc) then
        atc.SetInfo(aInstanceNum, aInfo);
    end;
end;

procedure TAutoTradesControllerPublisher.UpdateState(const aAutoTrade: IAutoTrade);
var
  Item: TObject;
  atc: IAutoTradesController;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IAutoTradesController, atc) then
        atc.UpdateState(aAutoTrade);
    end;
end;

{ TTradesStateHelper }

function TTradesStateHelper.ToColor: TColor;
begin
  Result := TradesStateColor[Self];
end;

function TTradesStateHelper.ToString: string;
begin
  Result := TradesStateString[Self];
end;

{ TAutoTradeCollection }

procedure TAutoTradeCollection.LoadActiveAutotrades;
const C_SELECT_SQL =
  'SELECT ID FROM AUTOTRADES '+ sLineBreak +
  'WHERE ACTIVE = TRUE '+ sLineBreak +
  '  AND QUALIFIER_ID IS NOT NULL '+ sLineBreak +
  '  AND CANDIDATE_ID IS NOT NULL '+ sLineBreak +
  '  AND QUANTITY_ID IS NOT NULL '+ sLineBreak +
  '  AND ORDER_TEMPLATE_ID IS NOT NULL '+ sLineBreak +
  'ORDER BY LOWER(NAME)';

var LQuery: TFDQuery;
    LAutoTrade: TAutoTradeInfo;
begin
  Clear;
  LQuery := TFDQuery.Create(nil);
  try
    LQuery.Connection := DMod.ConnectionStock;
    LQuery.SQL.Text := C_SELECT_SQL;
    LQuery.Open;
    while not LQuery.Eof do
    begin
      LAutoTrade := TAutoTradeInfo.Create;
      LAutoTrade.FromDB(LQuery.FieldByName('ID').AsInteger);
      LAutoTrade.Qualifier.Initialize;
      Add(LAutoTrade);
      LQuery.Next;
    end;
  finally
    LQuery.Free;
  end;
end;

initialization
  AutoTradesControllerPublisher := TAutoTradesControllerPublisher.Create;
  AutoTradeCollection := TAutoTradeCollection.Create;
  AutoTradeCollection.OwnsObjects := true;

finalization
  if Assigned(AutoTradesControllerPublisher) then
    FreeAndNil(AutoTradesControllerPublisher);
  if Assigned(AutoTradeCollection) then
  begin
    AutoTradeCollection.Clear;
    FreeAndNil(AutoTradeCollection);
  end;

end.
