unit Entity.Sokid;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections, System.Threading, System.Math,
  System.Generics.Defaults, Vcl.Forms, Winapi.Messages, DebugWriter, System.Types, IABFunctions, Data.DB, HtmlLib,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI, IABSocketAPI_const, 
  DaModule, Common.Types, Utils, System.TypInfo,
  System.IOUtils, ArrayHelper, Generics.Helper, Publishers.Interfaces, IABFunctions.RequestsQueue, Global.Types,
  DaModule.Constants, DaModule.Utils, System.Variants, IABFunctions.MessageCodes, Publishers, IABFunctions.Helpers,
  System.DateUtils, FireDAC.Comp.Client, IBX.IB, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TTradable = record
    MarketId   : Integer;
    Identifier : string;
    procedure Clear;
  end;

  TTradables = TArray<TTradable>;
  TAdditionalFeature = (afMatchingSymbol, afRequestContractDetails);
  TAdditionalFeatureSet = set of TAdditionalFeature;

  TSokidState = (stNormal, stModified, stDeleted);

  PSokidInfo = ^TSokidInfo;
  TSokidInfo = record
    Broker            : TBrokerType;
    ContractId        : Integer;
    UnderlyingConId   : Integer;
    Name              : string;
    Currency          : string;
    Description       : string;
    Exchange          : string;
    PrimaryExchange   : string;
    Symbol            : string;
    LocalSymbol       : string;
    MarketRuleIds     : string;
    SecurityType      : string;
    State             : TSokidState;
    Decimals          : Integer;
    Multiplier        : string;
    LastPrice         : Double;
    Isolate           : Integer;
    TWSMessageItem    : TTWSMessageItem;
    Sector            : string;
    IsIn              : string;
    Group             : string;
    AdditionalFeature : TAdditionalFeatureSet;
    Tradables         : TTradables;
    Expiry            : TDateTime;
    Strike            : Double;
    MinimumTick       : Currency;
    Industry          : string;
    Category          : string;
    Subcategory       : string;
    constructor Create(aId: Integer);
    procedure AssignFrom(aSokidInfo: TSokidInfo);
    function Equal(aSokidInfo: TSokidInfo): Boolean;
    function GetSecurityType: TIABSecurityType;
    function ToString: string;
    procedure CheckDependencies;
    procedure Check;
    procedure Clear;
  end;

  TThreadSokid = class(TThread)
  private
    FConnection    : TFDConnection;
    FTransaction   : TFDTransaction;
    FQuery         : TFDQuery;
    FQueue         : TThreadedQueue<TSokidInfo>;
    procedure SaveRecordToDB(aSokidInfo: TSokidInfo);
    procedure CreateConnect;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property SokidQueue: TThreadedQueue<TSokidInfo> read FQueue;
  end;

  TSokidList = class(TInterfacedObjectDictionary<Integer, TSokidInfo>, IInstrumentSpecDetails,
                                                                       ISecurityDefinitionOptionalParameter,
                                                                       IOnTickByTick)
  private
    FAbortEvent          : TAbortEvent;
    FIsInstrumentChecked : Boolean;
    FIsPriceChecked      : Boolean;
    FOnFinishingProgress : TProgressEvent;
    FOnProgress          : TProgressEvent;
    FOnStartProgress     : TStartProgressEvent;
    FThread              : TThreadSokid;
    //implementation ICustomInterface
    function GetInstance: TObject;
    procedure OnTickByTick(Sender: TObject; DataID: Integer; TickData: TIABTickData);         //IOnTickByTick
    procedure OnHistoricalTickData(Sender: TObject; DataId: Integer; TickData: TIABTickData); //IOnTickByTick
    procedure OnInstrumentSpecDetails(Sender: TObject; Index: Integer);                       //IInstrumentSpecDetails
    procedure OnSecurityDefinitionOptionalParameter(Sender: TObject; DataID: Integer; Exchange: string; UnderlyingConId: Integer; TradingClass, Multiplier, Expirations, Strikes: string); //ISecurityDefinitionOptionalParameter
  public const
    C_ERROR_CODE_SECURITY          = 200;
    C_ERROR_SECURITY_NOT_AVAILABLE = 203;
    C_ERROR_CODE_SYMBOL            = 20001;
    C_ERROR_CODE_CURRENCY          = 20002;
    C_ERROR_CODE_EXCHANGE          = 20003;
    C_ERROR_CODE_EXPIRY            = 20004;
  public
    constructor Create;
    destructor Destroy; override;
    function GetItem(aId: Integer): TSokidInfo; overload;
    function GetItem(aSymbol: string): TSokidInfo; overload;
    function GetDependencies(const aContractId: Integer): TArray<TSokidInfo>;
    function GetOrderByConID(const aContractId: Integer): TIABOrder;
    function GetValueFromFeed(aContractId: Integer; aTickType: TIABTickType): Double;
    procedure CheckLastPrice;
    procedure CheckInstruments;
    procedure DeleteItem(aContractId: Integer);
    procedure LoadFromDB;
    procedure SaveAll;
    procedure SetValue(aItem: TSokidInfo);

    property IsChecked           : Boolean             read FIsPriceChecked;
    property OnProgress          : TProgressEvent      read FOnProgress          write FOnProgress;
    property OnFinishingProgress : TProgressEvent      read FOnFinishingProgress write FOnFinishingProgress;
    property OnStartProgress     : TStartProgressEvent read FOnStartProgress     write FOnStartProgress;
    property OnAbort             : TAbortEvent         read FAbortEvent          write FAbortEvent;
  end;

var
  SokidList: TSokidList;

implementation

{ TSokidItem }

constructor TSokidInfo.Create(aId: Integer);
begin
  ContractId      := aId;
  State           := stNormal;
  LastPrice       := 0;
  Decimals        := 2;
  Name            := '';
  Currency        := '';
  Description     := '';
  Exchange        := '';
  PrimaryExchange := '';
  Symbol          := '';
  MarketRuleIds   := '';
  SecurityType    := '';
  Multiplier      := '';
  Isolate         := 0;
  TWSMessageItem.ErrorCode := -1;
  TWSMessageItem.ErrorMsg  := '';
end;

function TSokidInfo.Equal(aSokidInfo: TSokidInfo): Boolean;
var
  i: Integer;
begin
  Result := (Self.ContractId = aSokidInfo.ContractId) and
    SameText(Self.IsIn, aSokidInfo.IsIn) and
    SameText(Self.SecurityType, aSokidInfo.SecurityType) and
    SameText(Self.Group, aSokidInfo.Group) and
    SameText(Self.Name, aSokidInfo.Name) and
    SameText(Self.Symbol, aSokidInfo.Symbol) and
    SameText(Self.LocalSymbol, aSokidInfo.LocalSymbol) and
    SameText(Self.Exchange, aSokidInfo.Exchange) and
    SameText(Self.PrimaryExchange, aSokidInfo.PrimaryExchange) and
    SameText(Self.Currency, aSokidInfo.Currency) and
    SameText(Self.PrimaryExchange, aSokidInfo.PrimaryExchange) and
    (Length(Self.Tradables) = Length(aSokidInfo.Tradables)) and
    SameText(Self.Sector, aSokidInfo.Sector);
  if Result then
    for i := 0 to Length(aSokidInfo.Tradables) - 1 do
    begin
      Result := Result and (Self.Tradables[i].MarketId = aSokidInfo.Tradables[i].MarketId) and
                SameText(Self.Tradables[i].Identifier, aSokidInfo.Tradables[i].Identifier);
    end;
end;

function TSokidInfo.GetSecurityType: TIABSecurityType;
var
  IsFound: Boolean;
begin
  Result := stStock;
  IsFound := False;
  for var st := Low(TIABSecurityType) to High(TIABSecurityType) do
    if (st.ToString.ToUpper = SecurityType.ToUpper) then
    begin
      Result := st;
      IsFound := True;
      Break;
    end;
  if not IsFound then
  begin
    Self.TWSMessageItem.ErrorCode := TSokidList.C_ERROR_CODE_SECURITY;
    Self.TWSMessageItem.ErrorMsg := GetTWSMessageItem(TSokidList.C_ERROR_CODE_SECURITY).ErrorMsg;
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, 'TSokidInfo.GetSecurityType', 'Entity.Sokid',
                                                                      'Symbol=' + Self.Symbol +
                                                                      ', ContractId=' + Self.ContractId.ToString +
                                                                      ', Security Type "' + Self.SecurityType + '" not found!');
  end;
end;

function TSokidInfo.ToString: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.Append('ContractId=').Append(Self.ContractId).AppendLine
      .Append('UnderlyingConId=').Append(Self.UnderlyingConId).AppendLine
      .Append('Symbol=').Append(Self.Symbol).AppendLine
      .Append('LocalSymbol=').Append(Self.LocalSymbol).AppendLine
      .Append('Name=').Append(Self.Name).AppendLine
      .Append('SecurityType=').Append(Self.SecurityType).AppendLine
      .Append('Exchange=').Append(Self.Exchange).AppendLine
      .Append('PrimaryExchange=').Append(Self.PrimaryExchange).AppendLine
      .Append('Currency=').Append(Self.Currency).AppendLine
      .Append('Description=').Append(Self.Description).AppendLine
      .Append('Broker=').Append(Self.Broker.ToString).AppendLine
      .Append('Decimals=').Append(Self.Decimals).AppendLine
      .Append('Isolate=').Append(Self.Isolate).AppendLine
      .Append('Expiry=').Append(Self.Expiry).AppendLine
      .Append('Strike=').Append(Self.Strike).AppendLine
      .Append('MinimumTick=').Append(Self.MinimumTick).AppendLine
      .Append('Industry=').Append(Self.Industry).AppendLine
      .Append('Category=').Append(Self.Category).AppendLine
      .Append('Subcategory=').Append(Self.Subcategory).AppendLine
      .Append('ErrorCode=').Append(Self.TWSMessageItem.ErrorCode).AppendLine
      .Append('ErrorMsg=').Append(Self.TWSMessageItem.ErrorMsg).AppendLine;
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

procedure TSokidInfo.AssignFrom(aSokidInfo: TSokidInfo);
begin
  Self.ContractId        := aSokidInfo.ContractId;
  Self.UnderlyingConId   := aSokidInfo.UnderlyingConId;
  Self.Broker            := aSokidInfo.Broker;
  Self.Name              := aSokidInfo.Name;
  Self.Currency          := aSokidInfo.Currency;
  Self.Description       := aSokidInfo.Description;
  Self.Exchange          := aSokidInfo.Exchange;
  Self.PrimaryExchange   := aSokidInfo.PrimaryExchange;
  Self.Symbol            := aSokidInfo.Symbol;
  Self.MarketRuleIds     := aSokidInfo.MarketRuleIds;
  Self.SecurityType      := aSokidInfo.SecurityType;
  Self.Decimals          := aSokidInfo.Decimals;
  Self.Multiplier        := aSokidInfo.Multiplier;
  Self.State             := aSokidInfo.State;
  Self.LastPrice         := aSokidInfo.LastPrice;
  Self.Isolate           := aSokidInfo.Isolate;
  Self.Sector            := aSokidInfo.Sector;
  Self.IsIn              := aSokidInfo.IsIn;
  Self.Group             := aSokidInfo.Group;
  Self.Expiry            := aSokidInfo.Expiry;
  Self.Strike            := aSokidInfo.Strike;
  Self.MinimumTick       := aSokidInfo.MinimumTick;
  Self.AdditionalFeature := aSokidInfo.AdditionalFeature;
  Self.Industry          := aSokidInfo.Industry;
  Self.Category          := aSokidInfo.Category;
  Self.Subcategory       := aSokidInfo.Subcategory;
  if not aSokidInfo.LocalSymbol.Equals(aSokidInfo.Symbol) then
    Self.LocalSymbol := aSokidInfo.LocalSymbol;
  Self.TWSMessageItem.ErrorCode := aSokidInfo.TWSMessageItem.ErrorCode;
  Self.TWSMessageItem.ErrorMsg  := aSokidInfo. TWSMessageItem.ErrorMsg;
  SetLength(Self.Tradables, Length(aSokidInfo.Tradables));
  for var i := 0 to Length(aSokidInfo.Tradables) - 1 do
  begin
    Self.Tradables[i].MarketId   := aSokidInfo.Tradables[i].MarketId;
    Self.Tradables[i].Identifier := aSokidInfo.Tradables[i].Identifier;
  end;
end;

procedure TSokidInfo.Check;
begin
  Self.TWSMessageItem.ErrorCode := 0;
  Self.TWSMessageItem.ErrorMsg := '';
  if Self.Symbol.IsEmpty or (Self.TWSMessageItem.ErrorCode = TSokidList.C_ERROR_CODE_SYMBOL) then
    Self.TWSMessageItem.ErrorCode := TSokidList.C_ERROR_CODE_SYMBOL
  else if Self.SecurityType.IsEmpty or (Self.TWSMessageItem.ErrorCode = TSokidList.C_ERROR_CODE_SECURITY) then
    Self.TWSMessageItem.ErrorCode := TSokidList.C_ERROR_CODE_SECURITY
  else if Self.Currency.IsEmpty or (Self.TWSMessageItem.ErrorCode = TSokidList.C_ERROR_CODE_CURRENCY) then
    Self.TWSMessageItem.ErrorCode := TSokidList.C_ERROR_CODE_CURRENCY
  else if Self.Exchange.IsEmpty or (Self.TWSMessageItem.ErrorCode = TSokidList.C_ERROR_CODE_EXCHANGE) then
    Self.TWSMessageItem.ErrorCode := TSokidList.C_ERROR_CODE_EXCHANGE
  else if ((Self.Expiry > 0) and (Self.Expiry < IncDay(Today))) or (Self.TWSMessageItem.ErrorCode = TSokidList.C_ERROR_CODE_EXPIRY) then
    Self.TWSMessageItem.ErrorCode := TSokidList.C_ERROR_CODE_EXPIRY;
  if (Self.TWSMessageItem.ErrorCode > 0) then
    Self.TWSMessageItem.ErrorMsg := GetTWSMessageItem(Self.TWSMessageItem.ErrorCode).ErrorMsg;
end;

procedure TSokidInfo.CheckDependencies;
var
  Request: TIABRequest;
begin
  if (Self.GetSecurityType in [stStock, stIndex]) and (Self.Exchange = 'SFB') and (Self.Currency = C_DEFAULT_CURRENCY) then
  begin
    Request := Default(TIABRequest);
    Request.Command      := ibRequestSecDefOptParams;
    Request.Symbol       := Self.Symbol;
    Request.Exchange     := '';
    Request.SecurityType := Self.SecurityType;
    Request.ContractId   := Self.ContractId;
    Request.DataID       := Self.ContractId;
    Request.Priority     := qpLow;
    IABClient.SendRequest(Request);
  end;
end;

procedure TSokidInfo.Clear;
begin
  Self := Default(TSokidInfo);
end;

{ TTradable }

procedure TTradable.Clear;
begin
  Self := Default(TTradable);
end;

{ TSokidList }

constructor TSokidList.Create;
begin
  inherited Create([doOwnsValues]);
  FThread := TThreadSokid.Create;
  FIsPriceChecked := False;
  FIsInstrumentChecked := False;
end;

destructor TSokidList.Destroy;
begin
  FThread.Terminate;
  WaitForSingleObject(FThread.Handle, INFINITE);
  inherited;
end;

function TSokidList.GetItem(aId: Integer): TSokidInfo;
begin
  if Self.ContainsKey(aId) then
    Result := Self.Items[aId]
  else
    Result := TSokidInfo.Create(aId);
end;

function TSokidList.GetDependencies(const aContractId: Integer): TArray<TSokidInfo>;
var
  SokidInfo: TSokidInfo;
  i: Integer;
begin
  Result := [];
  i := 0;
  SetLength(Result, Self.Count);
  for SokidInfo in Self.Values do
    if (SokidInfo.UnderlyingConId = aContractId) then
    begin
      Result[i] := SokidInfo;
      Inc(i);
    end;
  SetLength(Result, i);
end;

function TSokidList.GetInstance: TObject;
begin
  Result := Self;
end;

function TSokidList.GetItem(aSymbol: string): TSokidInfo;
var
  SokidInfo: TSokidInfo;
begin
  for SokidInfo in Self.Values do
    if (SokidInfo.Symbol = aSymbol) then
      Exit(SokidInfo);
  SokidInfo := Default(TSokidInfo);
end;

procedure TSokidList.DeleteItem(aContractId: Integer);
var
  SokidInfo: TSokidInfo;
begin
  if Self.ContainsKey(aContractId) then
  begin
    SokidInfo := Self.Items[aContractId];
    SokidInfo.State := stDeleted;
    if FThread.Started then
      FThread.SokidQueue.PushItem(SokidInfo);
    Self.Remove(aContractId);
  end;
end;

function TSokidList.GetOrderByConID(const aContractId: Integer): TIABOrder;
var
  SokidInfo: TSokidInfo;
begin
  Result := TIABOrder.Create;
  if IABClient.Connected and (aContractId > 0) then
  begin
    IABClient.ClearOrder(Result);
    if Self.ContainsKey(aContractId) then
    begin
      SokidInfo := Self.Items[aContractId];
      Result.ContractId      := aContractId;
      Result.Currency        := SokidInfo.Currency;
      Result.Exchange        := SokidInfo.Exchange;
      Result.PrimaryExchange := SokidInfo.PrimaryExchange;
      Result.Multiplier      := SokidInfo.Multiplier;
      Result.Symbol          := SokidInfo.Symbol;
      Result.SecurityType    := SokidInfo.GetSecurityType;
      if (Result.SecurityType in [stOption, stFuture, stIndex]) or not SokidInfo.Symbol.Equals(SokidInfo.LocalSymbol) then
        Result.LocalSymbol := SokidInfo.LocalSymbol;
    end;
  end;
end;

function TSokidList.GetValueFromFeed(aContractId: Integer; aTickType: TIABTickType): Double;
resourcestring
  C_SQL_TEXT = 'SELECT TICK_VALUE FROM TICK_DATA TD WHERE TD.CONID=:CONID AND TD.TICK_TYPE=:TICK_TYPE ORDER BY TD.ID DESC ROWS 1';
var
  Query: TFDQuery;
begin
  Result := 0;
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect(DMod.ConnectionFeed);
    Query.Connection := DMod.ConnectionFeed;
    Query.SQL.Text := C_SQL_TEXT;
    Query.ParamByName('CONID').AsInteger     := aContractId;
    Query.ParamByName('TICK_TYPE').AsInteger := Ord(aTickType);
    Query.Prepare;
    Query.Open;
    if not Query.IsEmpty then
      Result := Query.FieldByName('TICK_VALUE').AsFloat;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TSokidList.LoadFromDB;
resourcestring
  C_SQL_TEXT = 'SELECT * FROM SOKID_IB WHERE (SYMBOL IS NOT NULL) AND (CONID > 0)';
var
  Query: TFDQuery;
  SokidInfo: TSokidInfo;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_TEXT;
    Query.Prepare;
    Query.Open;
    while not Query.Eof do
    begin
      SokidInfo := TSokidInfo.Create(Query.FieldByName('CONID').AsInteger);
      SokidInfo.ContractId      := Query.FieldByName('CONID').AsInteger;
      SokidInfo.UnderlyingConId := Query.FieldByName('UNDERLYING_CONID').AsInteger;
      SokidInfo.Broker          := TBrokerType.FromInteger(Query.FieldByName('BROKER').AsInteger);
      SokidInfo.Currency        := Query.FieldByName('CURRENCY').AsString;
      SokidInfo.Exchange        := Query.FieldByName('EXCHANGE').AsString;
      SokidInfo.MarketRuleIds   := Query.FieldByName('MARKET_LIST').AsString;
      SokidInfo.Name            := Query.FieldByName('NAME').AsString;
      SokidInfo.Description     := Query.FieldByName('DESCRIPTION').AsString;
      SokidInfo.SecurityType    := Query.FieldByName('CONTRACTYPE').AsString;
      SokidInfo.Symbol          := Query.FieldByName('SYMBOL').AsString;
      SokidInfo.PrimaryExchange := Query.FieldByName('PRIMARY_EXCHANGE').AsString;
      SokidInfo.Isolate         := Query.FieldByName('ISOLATE').AsInteger;
      SokidInfo.Symbol          := Query.FieldByName('SYMBOL').AsString;
      SokidInfo.LastPrice       := Query.FieldByName('LAST_PRICE').AsFloat;
      SokidInfo.LocalSymbol     := Query.FieldByName('LOCAL_SYMBOL').AsString;
      SokidInfo.Sector          := Query.FieldByName('SECTOR').AsString;
      SokidInfo.IsIn            := Query.FieldByName('ISIN').AsString;
      SokidInfo.Group           := Query.FieldByName('GROUP').AsString;
      SokidInfo.Expiry          := Query.FieldByName('EXPIRATIONDATE').AsDateTime;
      SokidInfo.Strike          := Query.FieldByName('STRIKE').AsFloat;
      SokidInfo.Multiplier      := Query.FieldByName('MULTIPLIER').AsString;
      SokidInfo.Industry        := Query.FieldByName('INDUSTRY').AsString;
      SokidInfo.Category        := Query.FieldByName('CATEGORY').AsString;
      SokidInfo.Subcategory     := Query.FieldByName('SUBCATEGORY').AsString;
      SokidInfo.TWSMessageItem.ErrorCode := Query.FieldByName('ERROR_CODE').AsInteger;
      if SokidInfo.GetSecurityType in [stFuture] then
        SokidInfo.MinimumTick := Query.FieldByName('MINIMUM_TICK').AsFloat;

      SokidInfo.Check;
      Self.SetValue(SokidInfo);
      Query.Next;
    end
  finally
    FreeAndNil(Query);
  end;
  if not FThread.Started then
    FThread.Start;
end;

procedure TSokidList.OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
var
  SokidInfo: TSokidInfo;
  UnderlyingSokidInfo: TSokidInfo;
begin
  if Self.ContainsKey(IABClient.InstrumentSpecs.Items[Index].ContractId) then
    SokidInfo := Self.Items[IABClient.InstrumentSpecs.Items[Index].ContractId]
  else
    SokidInfo := TSokidInfo.Create(IABClient.InstrumentSpecs.Items[Index].ContractId);

  SokidInfo.Currency        := IABClient.InstrumentSpecs.Items[Index].Currency;
  SokidInfo.Exchange        := IABClient.InstrumentSpecs.Items[Index].Exchange;
  SokidInfo.PrimaryExchange := IABClient.InstrumentSpecs.Items[Index].PrimaryExchange;
  SokidInfo.MarketRuleIds   := IABClient.InstrumentSpecs.Items[Index].MarketRuleIds;
  SokidInfo.LocalSymbol     := IABClient.InstrumentSpecs.Items[Index].LocalSymbol;
  SokidInfo.Symbol          := IABClient.InstrumentSpecs.Items[Index].Symbol;
  SokidInfo.Name            := IABClient.InstrumentSpecs.Items[Index].LongName;
  SokidInfo.Sector          := IABClient.InstrumentSpecs.Items[Index].LastTradeTime;
  SokidInfo.Multiplier      := IABClient.InstrumentSpecs.Items[Index].Multiplier;
  SokidInfo.MinimumTick     := IABClient.InstrumentSpecs.Items[Index].MinimumTick;
  SokidInfo.Strike          := IABClient.InstrumentSpecs.Items[Index].Strike;
  SokidInfo.SecurityType    := IABClient.InstrumentSpecs.Items[Index].SecurityType.ToString;
  SokidInfo.Industry        := IABClient.InstrumentSpecs.Items[Index].Industry;
  SokidInfo.Category        := IABClient.InstrumentSpecs.Items[Index].Category;
  SokidInfo.Subcategory     := IABClient.InstrumentSpecs.Items[Index].Subcategory;
  SokidInfo.State           := stModified;
  SokidInfo.Broker          := TBrokerType.brIB;
  if SokidInfo.Description.IsEmpty then
    SokidInfo.Description := IABClient.InstrumentSpecs.Items[Index].Symbol;

  if not IABClient.InstrumentSpecs.Items[Index].Expiry.IsEmpty then
    SokidInfo.Expiry := Utils.GetExpiryDate(IABClient.InstrumentSpecs.Items[Index].Expiry);
  SokidInfo.Check;

  if (SokidInfo.TWSMessageItem.ErrorCode > 0) and
     (Pos(SokidInfo.TWSMessageItem.ErrorCode.ToString + ';', General.PriceScheduleCodeErrors) > 0) then
  begin
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddWarning, Self, 'OnInstrumentSpecDetails',
                                  'Instrument was deleted from DB' +
                                  ', TWS ErrorCode = ' + SokidInfo.TWSMessageItem.ErrorCode.ToString +
                                  ', ConId='           + SokidInfo.ContractId.ToString +
                                  ', Symbol='          + SokidInfo.Symbol +
                                  ', Description='     + SokidInfo.Description +
                                  ', Exchange='        + SokidInfo.Exchange +
                                  ', PrimaryExchange=' + SokidInfo.PrimaryExchange +
                                  ', MarketRuleIds='   + SokidInfo.MarketRuleIds);
    Self.DeleteItem(SokidInfo.ContractId);
  end
  else
  begin
    if (IABClient.InstrumentSpecs.Items[Index].SecurityType in [stOption, stFuture]) and
      Self.ContainsKey(IABClient.InstrumentSpecs.Items[Index].DataID) then
    begin
      UnderlyingSokidInfo := Self.Items[IABClient.InstrumentSpecs.Items[Index].DataID];
      if (UnderlyingSokidInfo.Symbol = SokidInfo.Symbol) then
        SokidInfo.UnderlyingConId := UnderlyingSokidInfo.ContractId;
    end;

    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'OnInstrumentSpecDetails',
                                  'ConId='             + SokidInfo.ContractId.ToString +
                                  ', Symbol='          + SokidInfo.Symbol +
                                  ', Description='     + SokidInfo.Description +
                                  ', Exchange='        + SokidInfo.Exchange +
                                  ', PrimaryExchange=' + SokidInfo.PrimaryExchange +
                                  ', MarketRuleIds='   + SokidInfo.MarketRuleIds);

    if (not SokidInfo.MarketRuleIds.IsEmpty) and (not SokidInfo.SecurityType.Equals('CASH')) then
      IABClient.reqMarketRule(SokidInfo.MarketRuleIds);
    Self.SetValue(SokidInfo);
  end;
end;

procedure TSokidList.OnSecurityDefinitionOptionalParameter(Sender: TObject; DataID: Integer; Exchange: string; UnderlyingConId: Integer; TradingClass, Multiplier, Expirations, Strikes: string);
begin
  if IABClient.Connected and Self.ContainsKey(DataID) then
  begin
    System.Threading.TTask.Create(
      procedure()
      var
        arr: TArray<string>;
        ExpirationsArray: TDateArray;
        StrikesArray: TDoubleArray;
        Order: TIABOrder;
        SokidInfo: TSokidInfo;
        IsExistsSokidInfo: Boolean;
        arrDependencies: TArray<TSokidInfo>;
        i: Integer;
        Expire: TDate;
        Strike: Double;
      begin
        TThread.NameThreadForDebugging('Entity.Sokid.TSokidList.OnSecurityDefinitionOptionalParameter');
        SokidInfo := Self.Items[DataID];
        arrDependencies := Self.GetDependencies(DataID);
        StrikesArray.Clear;
        if not Strikes.IsEmpty then
        begin
          arr := Strikes.Split([';']);
          for i := Low(arr) to High(arr) do
            if not arr[i].IsEmpty then
              StrikesArray.AddUnique(StrToFloatEx(arr[i]));
          StrikesArray.Sort;
        end;

        ExpirationsArray.Clear;
        if not Expirations.IsEmpty then
        begin
          arr := Expirations.Split([';']);
          for i := Low(arr) to High(arr) do
            if not arr[i].IsEmpty then
              ExpirationsArray.AddUnique(GetExpiryDate(arr[i]));
          ExpirationsArray.Sort;
        end;

        Order := GetOrderByConID(SokidInfo.ContractId);
        try
          for Expire in ExpirationsArray do
            if (Date < Expire) then
              for Strike in StrikesArray do
              begin
                if Application.Terminated or (not IABClient.Connected) then
                  Exit;

                IsExistsSokidInfo := False;
                for var SokidRun in arrDependencies do
                  if (SokidRun.Expiry = Expire) and (SokidRun.Strike = Strike) then
                  begin
                    IsExistsSokidInfo := True;
                    Break;
                  end;

                if IsExistsSokidInfo then
                  Continue;

                IABClient.ClearOrder(Order);
                Order.Symbol := SokidInfo.Symbol;
                Order.Currency := SokidInfo.Currency;
                Order.Exchange := Exchange;
                Order.Right := rtCall;
                Order.SecurityType := stOption; // SokidInfo.GetSecurityType;
                Order.Expiry := FormatDateTime('YYYYMMDD', Expire);
                Order.Strike := Strike;
//                TPublishers.LogPublisher.Write([ltLogWriter], Self, 'OnSecurityDefinitionOptionalParameter', 'Symbol: ' + Order.Symbol +
//                                                                               ', Strike: ' + Order.Strike.ToString +
//                                                                               ', Expiry: ' + Order.Expiry);
                IABClient.SendRequest(ibGetInstrumentSpecs, SokidInfo.ContractId, Order);
              end;
        finally
          FreeAndNil(Order);
        end;
      end).Start;
  end;
end;

procedure TSokidList.OnTickByTick(Sender: TObject; DataID: Integer; TickData: TIABTickData);
var
  SokidInfo: TSokidInfo;
begin
  if (TickData.TickType in [tdLast, tdAllLast]) and Self.ContainsKey(DataID) then
  begin
    SokidInfo := Self.Items[DataID];
    SokidInfo.LastPrice := TickData.Price;
    SokidList.SetValue(SokidInfo);
  end;
end;

procedure TSokidList.OnHistoricalTickData(Sender: TObject; DataId: Integer; TickData: TIABTickData);
begin
  OnTickByTick(Sender, DataID, TickData);
end;

procedure TSokidList.SaveAll;
var
  SokidItem: TSokidInfo;
  Key: Integer;
begin
  for Key in Self.Keys do
    if (Self.Items[Key].State = stModified) then
    begin
      SokidItem := Self.Items[Key];
      FThread.SokidQueue.PushItem(SokidItem);
      SokidItem.State := stNormal;
      Self.Items[Key] := SokidItem;
    end;
end;

procedure TSokidList.SetValue(aItem: TSokidInfo);
begin
  if not aItem.Exchange.StartsWith('QBALGO') then
  begin
    aItem.State := stModified;
    Self.AddOrSetValue(aItem.ContractId, aItem);
    if FThread.Started then
      FThread.SokidQueue.PushItem(aItem);
  end;
end;

procedure TSokidList.CheckInstruments;
begin
  if IABClient.Connected and not FIsInstrumentChecked then
    System.Threading.TTask.Create(
      procedure()
      var
        SokidInfo: TSokidInfo;
        Order: TIABOrder;
        Position: Integer;
      begin
        TThread.NameThreadForDebugging('Entity.Sokid.TSokidList.CheckInstruments');
        TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'CheckInstruments');
        Position := 0;
        FIsInstrumentChecked := True;
        try
          if Assigned(FOnStartProgress) then
            TThread.Queue(nil,
              procedure
              begin
                FOnStartProgress(Self.Count, teCheckInstruments);
              end);

          for var key in Self.Keys do
          begin
            SokidInfo := Self.Items[key];
            if Application.Terminated or (not IABClient.Connected) then
            begin
              if Assigned(FOnFinishingProgress) then
                TThread.Queue(nil,
                  procedure
                  begin
                    FOnFinishingProgress(Position, teCheckInstruments);
                  end);
              Exit;
            end
            else if Assigned(FAbortEvent) then
              if FAbortEvent(teCheckInstruments) then
              begin
                if Assigned(FOnFinishingProgress) then
                  TThread.Queue(nil,
                    procedure
                    begin
                      FOnFinishingProgress(Position, teCheckInstruments);
                    end);
                Exit;
              end;

            if Pos(SokidInfo.TWSMessageItem.ErrorCode.ToString, General.PriceScheduleCodeErrors) > 0 then
            begin
              TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddWarning, Self, 'CheckInstruments',
                                            'Instrument was deleted from DB' +
                                            ', TWS ErrorCode = ' + SokidInfo.TWSMessageItem.ErrorCode.ToString +
                                            ', ConId='           + SokidInfo.ContractId.ToString +
                                            ', Symbol='          + SokidInfo.Symbol +
                                            ', Description='     + SokidInfo.Description +
                                            ', Exchange='        + SokidInfo.Exchange +
                                            ', PrimaryExchange=' + SokidInfo.PrimaryExchange +
                                            ', MarketRuleIds='   + SokidInfo.MarketRuleIds);
              Self.DeleteItem(SokidInfo.ContractId);
            end
            else
            if (SokidInfo.Broker = TBrokerType.brIB) then
            begin
              SokidInfo.MarketRuleIds := '';
              Self.AddOrSetValue(key, SokidInfo);
              if not(SokidInfo.TWSMessageItem.ErrorCode = C_ERROR_CODE_EXPIRY) then
              begin
                Order := GetOrderByConID(SokidInfo.ContractId);
                try
                  IABClient.SendRequest(ibGetInstrumentSpecs, SokidInfo.ContractId, Order);
                  Sleep(20);
                  TThread.Queue(nil,
                    procedure
                    begin
                      Inc(Position);
                      TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'Position=' + Position.ToString);
                      if Assigned(FOnProgress) then
                        FOnProgress(Position, teCheckInstruments);
                    end);
                finally
                  FreeAndNil(Order);
                end;
              end;
            end;
          end;
          if Assigned(FOnFinishingProgress) then
            TThread.Queue(nil,
              procedure
              begin
                FOnFinishingProgress(Position, teCheckInstruments);
              end);

        finally
          TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'CheckInstruments');
          FIsInstrumentChecked := False;
        end;
      end).Start;
end;

procedure TSokidList.CheckLastPrice;
begin
  if IABClient.Connected and not FIsPriceChecked then
    System.Threading.TTask.Create(
      procedure()
      var
        SokidInfo: TSokidInfo;
        Order: TIABOrder;
        Request: TIABRequest;
        Position: Integer;
      begin
        TThread.NameThreadForDebugging('Entity.Sokid.TSokidList.CheckLastPrice');
        TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'CheckLastPrice');
        Position := 0;
        FIsPriceChecked := True;
        try
          if Assigned(FOnStartProgress) then
            TThread.Queue(nil,
              procedure
              begin
                FOnStartProgress(Self.Count, teCheckLastPrice);
              end);

          for var key in Self.Keys do
          begin
            SokidInfo := Self.Items[key];
            if Application.Terminated or (not IABClient.Connected) then
            begin
              if Assigned(FOnFinishingProgress) then
                TThread.Queue(nil,
                  procedure
                  begin
                    FOnFinishingProgress(Position, teCheckLastPrice);
                  end);
              Exit;
            end
            else if Assigned(FAbortEvent) then
              if FAbortEvent(teCheckLastPrice) then
              begin
                if Assigned(FOnFinishingProgress) then
                  TThread.Queue(nil,
                    procedure
                    begin
                      FOnFinishingProgress(Position, teCheckLastPrice);
                    end);
                Exit;
              end;

            if (SokidInfo.Broker = TBrokerType.brIB) then
            begin
              while (IABClient.MarketSubscribeList.Number >= 100) do
                Sleep(20);
              Order := Self.GetOrderByConID(SokidInfo.ContractId);
              try
                if not(SokidInfo.TWSMessageItem.ErrorCode = C_ERROR_CODE_EXPIRY) then
                begin
                  SokidInfo.MarketRuleIds := '';
                  Self.AddOrSetValue(key, SokidInfo);
                  IABClient.SendRequest(ibGetInstrumentSpecs, SokidInfo.ContractId, Order);
                end;

                Request := Default(TIABRequest);
                Request.Command      := ibGetMarketData;
                Request.DataId       := Order.ContractId;
                Request.Order        := Order;
                Request.Priority     := qpLow;
                IABClient.reqMktData(Order.ContractId, Order, [emdHistoricalVolatility, emdMiscellaneous]);
                Sleep(300);

                if IABClient.MarketSubscribeList.CanUnsubscribe(Order.ContractId) then
                  IABClient.CancelMktData(Order.ContractId);
              finally
                FreeAndNil(Order);
              end;
              TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'CheckLastPrice',
                                            'ConId=' + SokidInfo.ContractId.ToString +
                                            ', Symbol=' + SokidInfo.Symbol);
              TThread.Queue(nil,
                procedure
                begin
                  Inc(Position);
                  if Assigned(FOnProgress) then
                    FOnProgress(Position, teCheckLastPrice);
                end);
            end;
          end;

          if Assigned(FOnFinishingProgress) then
            TThread.Queue(nil,
              procedure
              begin
                FOnFinishingProgress(Position, teCheckLastPrice);
              end);

        finally
          TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'CheckLastPrice');
          FIsPriceChecked := False;
        end;
      end).Start;
end;

{ TThreadSokid }

constructor TThreadSokid.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Priority := tpLowest;
  FQueue := TThreadedQueue<TSokidInfo>.Create(C_QUEUE_DEPTH, C_POP_TIMEOUT, C_PUSH_TIMEOUT);
  CreateConnect;
end;

procedure TThreadSokid.CreateConnect;
begin
  FConnection := TFireBirdConnect.CreateDatabase(TFireBirdConnect.DBNameStock);

  FTransaction := TFDTransaction.Create(FConnection);
  FConnection.Transaction := FTransaction;
  FTransaction.Connection := FConnection;
  FTransaction.Options.AutoCommit := false;

  FQuery := TFDQuery.Create(FConnection);
  FQuery.Connection    := FConnection;
  FQuery.Transaction := FTransaction;
end;

destructor TThreadSokid.Destroy;
begin
  if FTransaction.Active then
    FTransaction.Commit;
  if FConnection.Connected then
    FConnection.Connected := False;
  FreeAndNil(FQuery);
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  FreeAndNil(FQueue);
  inherited;
end;

procedure TThreadSokid.Execute;
var
  SokidInfo: TSokidInfo;
  WaitResult: TWaitResult;
begin
  inherited;
  TThread.NameThreadForDebugging('Entity.Sokid.ThreadSokid');
  try
    if not FConnection.Connected then
      FConnection.Connected := True;
    while not Terminated do
    begin
      WaitResult := FQueue.PopItem(SokidInfo);
      if (WaitResult = TWaitResult.wrSignaled) then
        SaveRecordToDB(SokidInfo);
    end;
  except
    on E:Exception do
      TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, Self, 'Execute', E.Message);
  end;
end;

procedure TThreadSokid.SaveRecordToDB(aSokidInfo: TSokidInfo);
resourcestring
  C_SQL_TEXT   = 'SELECT * FROM SOKID_IB WHERE CONID=:ConID';
  C_SQL_INSERT = 'INSERT INTO SOKID_IB (NAME, DESCRIPTION, SYMBOL, CONID, EXCHANGE, CONTRACTYPE, CURRENCY, DECIMALS, MARKET_LIST,  MULTIPLIER, BROKER, ISOLATE, PRIMARY_EXCHANGE, LOCAL_SYMBOL,' +
                 '                      SECTOR, ISIN, "GROUP", EXPIRATIONDATE, MINIMUM_TICK, ERROR_CODE, INDUSTRY, CATEGORY, SUBCATEGORY, UNDERLYING_CONID, STRIKE)' +
                 '              VALUES (:NAME,:DESCRIPTION,:SYMBOL,:CONID,:EXCHANGE,:CONTRACTYPE,:CURRENCY,:DECIMALS,:MARKET_LIST,:MULTIPLIER,:BROKER,:ISOLATE,:PRIMARY_EXCHANGE,:LOCAL_SYMBOL,' +
                 '                      :SECTOR,:ISIN,:GROUP, :EXPIRATIONDATE,:MINIMUM_TICK,:ERROR_CODE,:INDUSTRY,:CATEGORY,:SUBCATEGORY,:UNDERLYING_CONID,:STRIKE);';
  C_SQL_UPDATE = 'UPDATE SOKID_IB SET NAME=:NAME,SYMBOL=:SYMBOL,EXCHANGE=:EXCHANGE,CURRENCY=:CURRENCY,DECIMALS=:DECIMALS,MARKET_LIST=:MARKET_LIST,CONTRACTYPE=:CONTRACTYPE,' +
                 '                    MULTIPLIER=:MULTIPLIER,DESCRIPTION=:DESCRIPTION,BROKER=:BROKER,ISOLATE=:ISOLATE,PRIMARY_EXCHANGE=:PRIMARY_EXCHANGE,LOCAL_SYMBOL=:LOCAL_SYMBOL,' +
                 '                    SECTOR=:SECTOR,ISIN=:ISIN,"GROUP"=:GROUP,EXPIRATIONDATE=:EXPIRATIONDATE,MINIMUM_TICK=:MINIMUM_TICK,ERROR_CODE=:ERROR_CODE,INDUSTRY=:INDUSTRY,' +
                 '                    CATEGORY=:CATEGORY,SUBCATEGORY=:SUBCATEGORY,UNDERLYING_CONID=:UNDERLYING_CONID,STRIKE=:STRIKE ' +
                 ' WHERE CONID=:CONID';
  C_SQL_DELETE = 'DELETE FROM SOKID_IB WHERE CONID=:CONID';
var
  bIsNeedInsert   : Boolean;
  bIsNeedUpdate   : Boolean;
  UnderlyingConId : Integer;
begin
  if not FTransaction.Active then
    FTransaction.StartTransaction;
  FQuery.Close;

  if (not Terminated) and (aSokidInfo.State = stDeleted) then
  begin
    FQuery.SQL.Text := C_SQL_DELETE;
    try
      FQuery.ParamByName('ConID').AsInteger := aSokidInfo.ContractId;
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
                                      '<br>' + aSokidInfo.ToString);
        SaveRecordToDB(aSokidInfo);
      end;
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveRecordToDB', E.Message + TDModUtils.GetQueryInfo(FQuery) + '<br>' + aSokidInfo.ToString);
    end;
  end
  else if (aSokidInfo.ContractId > 0) and (not aSokidInfo.Symbol.IsEmpty) then
  begin
    bIsNeedInsert := False;
    bIsNeedUpdate := False;
    UnderlyingConId := aSokidInfo.UnderlyingConId;
    FQuery.SQL.Text := C_SQL_TEXT;
    FQuery.ParamByName('ConID').AsInteger := aSokidInfo.ContractId;
    try
      FQuery.Prepare;
      FQuery.Open;
      if not FQuery.IsEmpty then
      begin
        UnderlyingConId := FQuery.FieldByName('UNDERLYING_CONID').AsInteger;
        bIsNeedUpdate := (aSokidInfo.State = stModified) or
                         ((FQuery.FieldByName('SYMBOL').AsString <> aSokidInfo.Symbol) and not aSokidInfo.Symbol.IsEmpty) or
                         ((FQuery.FieldByName('EXCHANGE').AsString <> aSokidInfo.Exchange) and not aSokidInfo.Exchange.IsEmpty) or
                         ((FQuery.FieldByName('NAME').AsString <> aSokidInfo.Name) and not aSokidInfo.Name.IsEmpty) or
                         ((FQuery.FieldByName('MARKET_LIST').AsString <> aSokidInfo.MarketRuleIds) and not aSokidInfo.MarketRuleIds.IsEmpty) or
                         ((FQuery.FieldByName('CURRENCY').AsString <> aSokidInfo.Currency) and not aSokidInfo.Currency.IsEmpty) or
                         ((FQuery.FieldByName('CONTRACTYPE').AsString <> aSokidInfo.SecurityType) and not aSokidInfo.SecurityType.IsEmpty) or
                         ((FQuery.FieldByName('PRIMARY_EXCHANGE').AsString <> aSokidInfo.PrimaryExchange) and not aSokidInfo.PrimaryExchange.IsEmpty) or
                         ((FQuery.FieldByName('LOCAL_SYMBOL').AsString <> aSokidInfo.LocalSymbol) and not aSokidInfo.LocalSymbol.IsEmpty) or
                         ((FQuery.FieldByName('SECTOR').AsString <> aSokidInfo.Sector) and not aSokidInfo.Sector.IsEmpty) or
                         ((FQuery.FieldByName('ISIN').AsString <> aSokidInfo.IsIn) and not aSokidInfo.IsIn.IsEmpty) or
                         ((FQuery.FieldByName('GROUP').AsString <> aSokidInfo.Group) and not aSokidInfo.Group.IsEmpty) or
                         ((FQuery.FieldByName('EXPIRATIONDATE').AsDateTime <> aSokidInfo.Expiry) and (aSokidInfo.Expiry > 0)) or
                         ((FQuery.FieldByName('STRIKE').AsFloat <> aSokidInfo.Strike) and (aSokidInfo.Strike > 0)) or
                         (FQuery.FieldByName('ISOLATE').AsInteger <> aSokidInfo.Isolate) or
                         (FQuery.FieldByName('MINIMUM_TICK').AsFloat <> aSokidInfo.MinimumTick) or
                         (FQuery.FieldByName('ERROR_CODE').AsInteger <> aSokidInfo.TWSMessageItem.ErrorCode) or
                         (FQuery.FieldByName('MULTIPLIER').AsString <> aSokidInfo.Multiplier) or
                         ((FQuery.FieldByName('UNDERLYING_CONID').AsInteger <> aSokidInfo.UnderlyingConId) and (aSokidInfo.UnderlyingConId > 0)) or
                         ((FQuery.FieldByName('INDUSTRY').AsString <> aSokidInfo.Industry) and not aSokidInfo.Industry.IsEmpty) or
                         ((FQuery.FieldByName('CATEGORY').AsString <> aSokidInfo.Category) and not aSokidInfo.Category.IsEmpty) or
                         ((FQuery.FieldByName('SUBCATEGORY').AsString <> aSokidInfo.Subcategory) and not aSokidInfo.Subcategory.IsEmpty) or
                         ((FQuery.FieldByName('DESCRIPTION').AsString <> aSokidInfo.Description) and not aSokidInfo.Description.IsEmpty);
      end
      else
        bIsNeedInsert := True;
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveRecordToDB', E.Message + TDModUtils.GetQueryInfo(FQuery));
    end;

    if (not Terminated) and (bIsNeedUpdate or bIsNeedInsert) then
    begin
      FQuery.Close;
      if bIsNeedInsert then
        FQuery.SQL.Text := C_SQL_INSERT
      else
        FQuery.SQL.Text := C_SQL_UPDATE;

      if aSokidInfo.Name.IsEmpty then
        aSokidInfo.Name := aSokidInfo.Symbol;
      if aSokidInfo.Description.IsEmpty then
        aSokidInfo.Description := aSokidInfo.Symbol;
      try
        FQuery.ParamByName('BROKER').AsInteger          := Ord(aSokidInfo.Broker);
        FQuery.ParamByName('CATEGORY').AsString         := aSokidInfo.Category.Substring(0, 100);
        FQuery.ParamByName('CONID').AsInteger           := aSokidInfo.ContractId;
        FQuery.ParamByName('CONTRACTYPE').AsString      := aSokidInfo.SecurityType.Substring(0, 10);
        FQuery.ParamByName('CURRENCY').AsString         := aSokidInfo.Currency.Substring(0, 10);
        FQuery.ParamByName('DECIMALS').AsInteger        := aSokidInfo.Decimals;
        FQuery.ParamByName('DESCRIPTION').AsString      := aSokidInfo.Description;
        FQuery.ParamByName('ERROR_CODE').AsInteger      := aSokidInfo.TWSMessageItem.ErrorCode;
        FQuery.ParamByName('EXCHANGE').AsString         := aSokidInfo.Exchange.Substring(0, 40);
        FQuery.ParamByName('GROUP').AsString            := aSokidInfo.Group.Substring(0, 10);
        FQuery.ParamByName('INDUSTRY').AsString         := aSokidInfo.Industry.Substring(0, 100);
        FQuery.ParamByName('ISIN').AsString             := aSokidInfo.IsIn.Substring(0, 15);
        FQuery.ParamByName('ISOLATE').AsInteger         := aSokidInfo.Isolate;
        FQuery.ParamByName('LOCAL_SYMBOL').AsString     := aSokidInfo.LocalSymbol.Substring(0, 100);
        FQuery.ParamByName('MARKET_LIST').AsString      := aSokidInfo.MarketRuleIds.Substring(0, 100);
        FQuery.ParamByName('MINIMUM_TICK').AsFloat      := aSokidInfo.MinimumTick;
        FQuery.ParamByName('MULTIPLIER').AsInteger      := StrToIntDef(aSokidInfo.Multiplier, 0);
        FQuery.ParamByName('NAME').AsString             := aSokidInfo.Name.Substring(0, 200);
        FQuery.ParamByName('PRIMARY_EXCHANGE').AsString := aSokidInfo.PrimaryExchange.Substring(0, 100);
        FQuery.ParamByName('SECTOR').AsString           := aSokidInfo.Sector.Substring(0, 10);
        FQuery.ParamByName('STRIKE').AsFloat            := aSokidInfo.Strike;
        FQuery.ParamByName('SUBCATEGORY').AsString      := aSokidInfo.Subcategory.Substring(0, 100);
        FQuery.ParamByName('SYMBOL').AsString           := aSokidInfo.Symbol.Substring(0, 20);
        if (aSokidInfo.UnderlyingConId > 0) then
          FQuery.ParamByName('UNDERLYING_CONID').AsInteger := aSokidInfo.UnderlyingConId
        else
          FQuery.ParamByName('UNDERLYING_CONID').AsInteger := UnderlyingConId;
        if not IsNan(aSokidInfo.Expiry) and (aSokidInfo.Expiry > 0) then
          FQuery.ParamByName('EXPIRATIONDATE').AsDateTime := aSokidInfo.Expiry
        else
          FQuery.ParamByName('EXPIRATIONDATE').Clear;
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
                                        '<br>' + aSokidInfo.ToString);
         if (E.SQLCode <> -530) then
          SaveRecordToDB(aSokidInfo);
        end;
        on E: Exception do
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveRecordToDB', E.Message + TDModUtils.GetQueryInfo(FQuery) + '<br>' + aSokidInfo.ToString);
      end;
      aSokidInfo.State := stNormal;
    end;
  end;
end;

initialization
  SokidList := TSokidList.Create;

finalization
  if Assigned(SokidList) then
    FreeAndNil(SokidList);

end.
