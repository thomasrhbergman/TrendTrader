unit IABFunctions.MarketRules;

interface

{$REGION 'Region uses'}
uses
  System.SysUtils, System.Classes, System.Threading, System.Generics.Collections, System.Generics.Defaults,
  DebugWriter, Utils, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Types, Data.DB, Common.Types,
  DaModule, HtmlLib, System.IniFiles, DaModule.Utils, System.Variants, Winapi.Windows, Global.Types,
  Publishers, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TRuleItem = record
    LowEdge: Double;
    Increment: Double;
    constructor Create(aLowEdge, aIncrement: Double);
  end;

  TArrMarketRule = TArray<TRuleItem>;

  TMarketRuleList = class(TDictionary<Integer, TArrMarketRule>)
    function GetDefaultMarketRules: TArrMarketRule;
    function GetMarketRules(const aMarketList: string): TArrMarketRule;
    function GetMinTick(const aValue: Double; const aMarketList: string; const aMinimumTick: Currency): Currency;
    function RoundToMinTick(const aValue: Double; const aMarketList: string; const aMinimumTick: Currency): Currency;
    procedure FromDB(const aRuleId: Integer);
    procedure LoadFromDB;
    procedure SaveToDB(const aRuleId: Integer);
  end;

  TSubscribeList = class(TDictionary<Integer, Integer>)
  private
    FCriticalSection : TRTLCriticalSection;
    function GetNumber: Integer;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function CanUnsubscribe(const DataID: Integer): Boolean;
    function IsSubscribed(const DataID: Integer): Boolean;
    procedure DecSubscribe(const DataID: Integer);
    procedure IncSubscribe(const DataID: Integer);
    property Number: Integer read GetNumber;
  end;

implementation

{ TMarketRuleList }

function TMarketRuleList.GetDefaultMarketRules: TArrMarketRule;
begin
  Result := TArrMarketRule.Create(TRuleItem.Create(0, 0.0001),
                                  TRuleItem.Create(0.5, 0.0002),
                                  TRuleItem.Create(1, 0.0005),
                                  TRuleItem.Create(2, 0.001),
                                  TRuleItem.Create(5, 0.002),
                                  TRuleItem.Create(10, 0.005),
                                  TRuleItem.Create(20, 0.01),
                                  TRuleItem.Create(50, 0.02),
                                  TRuleItem.Create(100, 0.05),
                                  TRuleItem.Create(200, 0.1),
                                  TRuleItem.Create(500, 0.2),
                                  TRuleItem.Create(1000, 0.5),
                                  TRuleItem.Create(2000, 1),
                                  TRuleItem.Create(5000, 2),
                                  TRuleItem.Create(10000, 5),
                                  TRuleItem.Create(20000, 10),
                                  TRuleItem.Create(50000, 20));
end;

function TMarketRuleList.GetMarketRules(const aMarketList: string): TArrMarketRule;
var
  arrMarkets: TArray<string>;
  Market: string;
begin
  Result := [];
  if not aMarketList.IsEmpty then
  begin
    arrMarkets := GetUniqueList(aMarketList).Split([',']);
    for Market in arrMarkets do
      if not Market.IsEmpty and Self.ContainsKey(StrToIntDef(Market, 0)) then
      begin
        Result := Self.Items[StrToIntDef(Market, 0)];
        Break;
      end;
  end;
end;

function TMarketRuleList.GetMinTick(const aValue: Double; const aMarketList: string; const aMinimumTick: Currency): Currency;
var
  ArrMarketRule: TArrMarketRule;
  i: Integer;
begin
  Result := aValue;
  if (aMinimumTick > 0) then
  begin
    SetLength(ArrMarketRule, 1);
    ArrMarketRule[0].LowEdge := 0;
    ArrMarketRule[0].Increment := aMinimumTick;
  end
  else
  begin
    ArrMarketRule := GetMarketRules(aMarketList);
    if (Length(ArrMarketRule) = 0) then
    begin
      ArrMarketRule := GetDefaultMarketRules;
      TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'GetMinTick', 'GetDefaultMarketRules');
    end;
  end;

  for i := High(ArrMarketRule) downto Low(ArrMarketRule) do
    if (aValue >= ArrMarketRule[i].LowEdge) then
    begin
      Result := ArrMarketRule[i].Increment;
      TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'GetMinTick',
                                            'MarketList=' + aMarketList +
                                            ', Value=' + aValue.ToString +
                                            ', LowEdge=' + ArrMarketRule[i].LowEdge.ToString +
                                            ', Increment=' + ArrMarketRule[i].Increment.ToString +
                                            ', MinimumTick=' + FloatToStr(aMinimumTick));

      Break;
    end;
end;

function TMarketRuleList.RoundToMinTick(const aValue: Double; const aMarketList: string; const aMinimumTick: Currency): Currency;
var
  MinTick: Currency;
begin
  Result := aValue;
  MinTick := GetMinTick(aValue, aMarketList, aMinimumTick);
  if (MinTick > 0) then
    Result := Round(aValue / MinTick) * MinTick;
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'RoundToMinTick',
                                          'MarketList=' + aMarketList +
                                          ', Value=' + aValue.ToString +
                                          ', RoundValue=' + FloatToStr(Result) +
                                          ', MinTick=' + FloatToStr(MinTick));
end;

procedure TMarketRuleList.LoadFromDB;
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT DISTINCT(RULE_ID) FROM MARKET_RULES';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_SELECT_TEXT;
    Query.Prepare;
    Query.Open;
    while not Query.Eof do
    begin
      Self.FromDB(Query.FieldByName('RULE_ID').AsInteger);
      Query.Next;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TMarketRuleList.FromDB(const aRuleId: Integer);
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT * FROM MARKET_RULES WHERE RULE_ID=:RuleId ORDER BY LOW_EDGE';
var
  Query: TFDQuery;
  ArrMarketRule: TArrMarketRule;
  i: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_SELECT_TEXT;
    Query.ParamByName('RuleId').AsInteger := aRuleId;
    Query.Prepare;
    Query.Open;
    Query.FetchAll;
    SetLength(ArrMarketRule, Query.RecordCount);
    i := 0;
    while not Query.Eof do
    begin
      ArrMarketRule[i].Increment := Query.FieldByName('INCREMENT').AsFloat;
      ArrMarketRule[i].LowEdge   := Query.FieldByName('LOW_EDGE').AsFloat;
      Inc(i);
      Query.Next;
    end;
    Self.AddOrSetValue(aRuleId, ArrMarketRule);
  finally
    FreeAndNil(Query);
  end;
end;

procedure TMarketRuleList.SaveToDB(const aRuleId: Integer);
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM MARKET_RULES WHERE RULE_ID=:RuleId';
  C_SQL_INSERT_TEXT = 'INSERT INTO MARKET_RULES (ID, RULE_ID, LOW_EDGE, INCREMENT) VALUES (:Id, :RuleId, :LowEdge, :Increment)';
var
  Query: TFDQuery;
  ArrMarketRule: TArrMarketRule;
  IsExists: Boolean;
begin
  if Self.ContainsKey(aRuleId) then
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.Transaction := Query.Connection.Transaction;
      Query.SQL.Text := C_SQL_EXISTS_TEXT;
      try
        Query.ParamByName('RuleId').AsInteger := aRuleId;
        Query.Prepare;
        Query.Open;
        IsExists := Query.FieldByName('CNT').AsInteger > 0;
        Query.Close;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveToDB', E.Message + TDModUtils.GetQueryInfo(Query));
          raise;
        end;
      end;

      if not IsExists then
      begin
        ArrMarketRule  := Self.Items[aRuleId];
        Query.SQL.Text := C_SQL_INSERT_TEXT;
        for var i := Low(ArrMarketRule) to High(ArrMarketRule) do
        begin
          Query.ParamByName('Id').AsInteger      := DMod.GetNextValue('GEN_MARKET_RULES_ID');
          Query.ParamByName('RuleId').AsInteger  := aRuleId;
          Query.ParamByName('LowEdge').AsFloat   := ArrMarketRule[i].LowEdge;
          Query.ParamByName('Increment').AsFloat := ArrMarketRule[i].Increment;
          try
            Query.Prepare;
            Query.ExecSQL;
            Query.Transaction.CommitRetaining;
          except
            on Er: Exception do
            begin
              TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveToDB', Er.Message + TDModUtils.GetQueryInfo(Query));
              raise;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

{ TRuleItem }

constructor TRuleItem.Create(aLowEdge, aIncrement: Double);
begin
  LowEdge   := aLowEdge;
  Increment := aIncrement;
end;

{ TSubscribeList }

constructor TSubscribeList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FCriticalSection);
end;

destructor TSubscribeList.Destroy;
begin
  DeleteCriticalSection(FCriticalSection);
  inherited;
end;

function TSubscribeList.GetNumber: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := Self.Count;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TSubscribeList.IncSubscribe(const DataID: Integer);
var
  cnt: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    cnt := 0;
    if Self.ContainsKey(DataID) then
      cnt := Self.Items[DataID];
    Inc(cnt);
    Self.AddOrSetValue(DataID, cnt);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TSubscribeList.DecSubscribe(const DataID: Integer);
var
  cnt: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    cnt := 0;
    if Self.ContainsKey(DataID) then
      cnt := Self.Items[DataID];
    Dec(cnt);
    if (cnt <= 0) then
      Self.Remove(DataID)
    else
      Self.AddOrSetValue(DataID, cnt);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TSubscribeList.CanUnsubscribe(const DataID: Integer): Boolean;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := False;
    if Self.ContainsKey(DataID) then
      Result := Self.Items[DataID] = 1;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TSubscribeList.IsSubscribed(const DataID: Integer): Boolean;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if Self.ContainsKey(DataID) then
      Result := Self.Items[DataID] > 0
    else
      Result := False;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

end.
