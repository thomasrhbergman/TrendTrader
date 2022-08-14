unit Qualifiers.Types;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, VirtualTrees, IABFunctions,
  IABSocketAPI, System.Generics.Collections, BrokerHelperAbstr, Winapi.msxml, Vcl.Graphics, Entity.Sokid,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const, DebugWriter, HtmlLib, Chart.Trade,
  System.DateUtils, XmlFiles, Data.DB, DaModule, InstrumentList, System.Math, System.Threading,
  Publishers.Interfaces, Generics.Helper, Common.Types, AutoTrades.Types, Bleeper, DaModule.Utils, ArrayHelper,
  Global.Types, Publishers, Vcl.Forms, IABFunctions.Helpers, IABFunctions.MarketData, Vcl.ExtCtrls,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TAutoTradesInstanceEvent = function (const aAutoTradeId, aQualifierId, aQualifierInstance: Integer): IAutoTrade of object;
  TAutoTradesArray = TArrayRecord<TAutoTradeInfo>;
  TTypeCondition = (tcRealtime, tcEveryDay, tcSpecificDate);
  TTypeConditionHelper = record helper for TTypeCondition
  private
    const TypeConditionString: array [TTypeCondition] of string = ('Realtime Value', 'Every Day', 'Specific Date');
  public
    function ToString: string;
  end;

  TQualifierInstrument = record
    SokidInfo     : TSokidInfo;
    TickType1     : TIABTickType;
    TickType2     : TIABTickType;
    PriceValue1   : Double;
    PriceValue2   : Double;
    TotalValue    : Double;
    TypeOperation : TTypeOperation;
    procedure AssignFrom(const aQualifierInstrument: TQualifierInstrument);
  end;

  TQualifierCondition = class;

  PQualifier = ^TQualifier;
  TQualifier = record
  private
    FAutoTradesInstanceEvent: TAutoTradesInstanceEvent;
  public
    RecordId: Integer;
    InstanceNum: Integer;
    Name: string;
    Enabled: Boolean;
    Description: string;
    Conditions: TArray<TQualifierCondition>;
    State: TTradesState;
    CreateTime: TTime;
    PreTime: TTime;
    PerTime: TTime;
    [weak] AutoTradesInstance: IAutoTrade;
    OwnerNode: PVirtualNode;
    procedure AssignFrom(const aQualifier: TQualifier; const aWithItems: Boolean = False);
    procedure Clear;
    procedure DisableAllConditions;
    procedure FromDB(const aID: Integer);
    procedure Initialize;
    procedure LoadItems;
    procedure SaveToDB;
    property OnExecuteAutoTradesInstance: TAutoTradesInstanceEvent read FAutoTradesInstanceEvent write FAutoTradesInstanceEvent;
  end;

  PQualifierCondition = ^TQualifierCondition;
  TQualifierCondition = class(TInterfacedPersistent, IUpdateFeeds)
  private const
    C_SECTION = 'Params';
    C_SECTION_INSTRUMENT1 = 'Instrument1';
    C_SECTION_INSTRUMENT2 = 'Instrument2';
  private
    FAutoTrades: TAutoTradesArray;
    FAutoTradesInstanceEvent: TAutoTradesInstanceEvent;
    FBypass: Boolean;
    FCreateTime: TTime;
    FEnabled: Boolean;
    FInequalityType: TInequalityType;
    FIsCondition: Boolean;
    FName: string;
    FOwnerNode: PVirtualNode;
    FQualifier: TQualifier;
    FRecordId: Integer;
    FStartupDate: TDateTime;
    FTypeCondition: TTypeCondition;
    FTimer: TTimer;
    procedure OnConditionTime(Sender: TObject);
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
  public
    Instrument1: TQualifierInstrument;
    Instrument2: TQualifierInstrument;
    function GetAutoTrades(const aRecordId: Integer): TAutoTradesArray;
    function ToString: string; override;
    function ToValueString: string;
    function ToXml: string;
    procedure AssignFrom(const aQualifierItem: TQualifierCondition);
    procedure Clear;
    procedure Deinitialize;
    procedure FromDB(const aID: Integer);
    procedure FromXml(const aXmlText: string);
    procedure Initialize;
    procedure SaveToDB;
    destructor Destroy; override;

    property AutoTrades     : TAutoTradesArray     read FAutoTrades     write FAutoTrades;
    property Bypass         : Boolean              read FBypass         write FBypass;
    property CreateTime     : TTime                read FCreateTime     write FCreateTime;
    property Enabled        : Boolean              read FEnabled        write FEnabled;
    property InequalityType : TInequalityType      read FInequalityType write FInequalityType;
    property IsCondition    : Boolean              read FIsCondition    write FIsCondition;
    property Name           : string               read FName           write FName;
    property OwnerNode      : PVirtualNode         read FOwnerNode      write FOwnerNode;
    property Qualifier      : TQualifier           read FQualifier      write FQualifier;
    property RecordId       : Integer              read FRecordId       write FRecordId;
    property StartupDate    : TDateTime            read FStartupDate    write FStartupDate;
    property TypeCondition  : TTypeCondition       read FTypeCondition  write FTypeCondition;
    property OnExecuteAutoTradesInstance: TAutoTradesInstanceEvent read FAutoTradesInstanceEvent write FAutoTradesInstanceEvent;
  end;

  TQualifierList = class(TList<TQualifier>)
  protected
    procedure Beeps;
  public
    constructor Create; overload;
    function GetInstance: TObject;
    function GetIndexByNum(const aInstanceNum: Integer): Integer;
    function GetItemByNum(const aInstanceNum: Integer): TQualifier;
  end;

  IQualifiersController = interface(ICustomInterface)
    ['{5157F21C-5841-433C-A354-B95DA9216598}']
    procedure UpdateState(const aQualifier: TQualifier);
    procedure DeleteQualifier(const aInstanceNum: Integer);
  end;

  TQualifiersControllerPublisher = class(TCustomPublisher)
  public
    procedure UpdateState(const aQualifier: TQualifier);
    procedure DeleteQualifier(const aInstanceNum: Integer);
  end;

var
  QualifiersControllerPublisher: TQualifiersControllerPublisher;

implementation

{ TQualifierCondition }

destructor TQualifierCondition.Destroy;
begin
  if Assigned(FTimer) then
    FreeAndNil(FTimer);
  inherited;
end;

procedure TQualifierCondition.AssignFrom(const aQualifierItem: TQualifierCondition);
begin
  Self.Clear;
  Self.TypeCondition  := aQualifierItem.TypeCondition;
  Self.Qualifier      := aQualifierItem.Qualifier;
  Self.RecordId       := aQualifierItem.RecordId;
  Self.Name           := aQualifierItem.Name;
  Self.Bypass         := aQualifierItem.Bypass;
  Self.InequalityType := aQualifierItem.InequalityType;
  Self.Enabled        := aQualifierItem.Enabled;
  Self.IsCondition    := aQualifierItem.IsCondition;
  Self.StartupDate    := aQualifierItem.StartupDate;
  Self.OwnerNode      := aQualifierItem.OwnerNode;
  Self.CreateTime     := aQualifierItem.CreateTime;

  Self.Instrument1.AssignFrom(aQualifierItem.Instrument1);
  Self.Instrument2.AssignFrom(aQualifierItem.Instrument2);
  Self.AutoTrades.Count := aQualifierItem.AutoTrades.Count;
  for var i := Low(aQualifierItem.AutoTrades.Items) to High(aQualifierItem.AutoTrades.Items) do
  begin
    Self.AutoTrades.Items[i].AssignFrom(aQualifierItem.AutoTrades.Items[i]);
    Self.AutoTrades.Items[i].QualifierInstance := aQualifierItem.Qualifier.InstanceNum;
  end;
end;

procedure TQualifierCondition.Clear;
begin
  Self.Name           := '';
  Self.RecordId       := -1;
  Self.Qualifier      := Default(TQualifier);
  Self.StartupDate    := 0;
  Self.CreateTime     := 0;
  Self.Enabled        := True;
  Self.Bypass         := False;
  Self.IsCondition    := False;
  Self.TypeCondition  := Low(TTypeCondition);
  Self.InequalityType := Low(TInequalityType);
  Self.Instrument1    := Default(TQualifierInstrument);
  Self.Instrument2    := Default(TQualifierInstrument);
  Self.OwnerNode      := nil;
  Self.AutoTrades.Clear;
end;

procedure TQualifierCondition.FromDB(const aID: Integer);
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT * FROM QUALIFIERS_CONDITION WHERE ID=:ID';
var
  Query: TFDQuery;
begin
  Self.Clear;
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
        Self.Name           := Query.FieldByName('NAME').AsString;
        if Query.FieldByName('QUALIFIER_ID').AsInteger <> 0 then
          Self.Qualifier.FromDB(Query.FieldByName('QUALIFIER_ID').AsInteger)
        else
          Self.Qualifier      := Default(TQualifier);
        Self.TypeCondition  := TTypeCondition(Query.FieldByName('TYPE_CONDITION').AsInteger);
        Self.AutoTrades     := GetAutoTrades(aID);
        Self.Enabled        := Query.FieldByName('ENABLED').AsBoolean;
        Self.Bypass         := Query.FieldByName('BYPASS').AsBoolean;
        Self.IsCondition    := False;
        Self.StartupDate    := Query.FieldByName('STARTUP_DATE').AsDateTime;
        Self.CreateTime     := Now;
        Self.Instrument1.SokidInfo.ContractId := Query.FieldByName('INSTRUMENT1').AsInteger;
        Self.Instrument2.SokidInfo.ContractId := Query.FieldByName('INSTRUMENT2').AsInteger;
        Self.FromXml(Query.FieldByName('XML_PARAMS').AsString);

        case Self.TypeCondition of
          tcRealtime:
            begin
              if SokidList.ContainsKey(Self.Instrument1.SokidInfo.ContractId) then
                Self.Instrument1.SokidInfo.AssignFrom(SokidList.Items[Self.Instrument1.SokidInfo.ContractId]);
              if SokidList.ContainsKey(Self.Instrument2.SokidInfo.ContractId) then
                Self.Instrument2.SokidInfo.AssignFrom(SokidList.Items[Self.Instrument2.SokidInfo.ContractId]);
            end;
        end;
      end;
      Self.RecordId := aID;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TQualifierCondition.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM QUALIFIERS_CONDITION WHERE ID=:ID';
  C_SQL_UPDATE_TEXT = 'UPDATE QUALIFIERS_CONDITION SET QUALIFIER_ID=:QUALIFIER_ID, TYPE_CONDITION=:TYPE_CONDITION, NAME=:NAME, ENABLED=:ENABLED,' + sLineBreak +
                                                      'BYPASS=:BYPASS, INSTRUMENT1=:INSTRUMENT1, INSTRUMENT2=:INSTRUMENT2, STARTUP_DATE=:STARTUP_DATE,' + sLineBreak +
                                                      'XML_PARAMS=:XML_PARAMS WHERE ID=:ID';

  C_SQL_INSERT_TEXT = 'INSERT INTO QUALIFIERS_CONDITION ( ID, QUALIFIER_ID, TYPE_CONDITION, NAME, ENABLED, BYPASS, INSTRUMENT1, INSTRUMENT2, STARTUP_DATE, XML_PARAMS)' + sLineBreak +
                                               ' VALUES (:ID,:QUALIFIER_ID,:TYPE_CONDITION,:NAME,:ENABLED,:BYPASS,:INSTRUMENT1,:INSTRUMENT2,:STARTUP_DATE,:XML_PARAMS)';
var
  Query: TFDQuery;
  IsExists: Boolean;
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
        Query.ParamByName('ID').AsInteger := Self.RecordId;
        Query.Prepare;
        Query.Open;
        IsExists := Query.FieldByName('CNT').AsInteger > 0;
        Query.Close;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'SaveToDB', 'QualifierCondition', E.Message + TDModUtils.GetQueryInfo(Query));
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
      Self.Name := 'QualifierCondition nr' + Self.RecordId.ToString;

    Query.ParamByName('ID').AsInteger             := Self.RecordId;
    Query.ParamByName('QUALIFIER_ID').AsInteger   := Self.Qualifier.RecordId;
    Query.ParamByName('TYPE_CONDITION').AsInteger := Ord(Self.TypeCondition);
    Query.ParamByName('NAME').AsString            := Self.Name.Substring(0, 100);
    Query.ParamByName('ENABLED').AsBoolean        := Self.Enabled;
    Query.ParamByName('BYPASS').AsBoolean         := Self.Bypass;
    Query.ParamByName('STARTUP_DATE').AsDateTime  := Self.StartupDate;
    Query.ParamByName('XML_PARAMS').AsString      := Self.ToXml;

    if (Self.Instrument1.SokidInfo.ContractId > 0) then
      Query.ParamByName('INSTRUMENT1').AsInteger := Self.Instrument1.SokidInfo.ContractId
    else
      Query.ParamByName('INSTRUMENT1').Clear;
    if (Self.Instrument2.SokidInfo.ContractId > 0) then
      Query.ParamByName('INSTRUMENT2').AsInteger := Self.Instrument2.SokidInfo.ContractId
    else
      Query.ParamByName('INSTRUMENT2').Clear;

    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'SaveToDB', 'QualifiersTypes', E.Message + TDModUtils.GetQueryInfo(Query));
    end;
  finally
    FreeAndNil(Query);
    DMod.RefreshQuery(DMod.fbqQualifiers);
  end;
end;

function TQualifierCondition.GetAutoTrades(const aRecordId: Integer): TAutoTradesArray;
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT RECORD_ID FROM DOC_RELATIONS WHERE DOC_TYPE = 2 AND PARENT_ID IN (SELECT ID FROM DOC_RELATIONS WHERE RECORD_ID=:RecordId)';
var
  Query: TFDQuery;
  AutoTradeInfo: TAutoTradeInfo;
begin
  Result.Clear;
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    Query.SQL.Text := C_SQL_SELECT_TEXT;
    Query.ParamByName('RecordId').AsInteger := aRecordId;
    try
      Query.Prepare;
      Query.Open;
      while not Query.Eof do
      begin
        if (Query.FieldByName('RECORD_ID').AsInteger > 0) then
        begin
          AutoTradeInfo.FromDB(Query.FieldByName('RECORD_ID').AsInteger);
          Result.Add(AutoTradeInfo);
        end;
        Query.Next;
      end;
      if Query.Transaction.Active then
        Query.Transaction.CommitRetaining;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TQualifierCondition', 'GetAutoTrades', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

function TQualifierCondition.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TQualifierCondition.Initialize;
begin
  if (Self.TypeCondition = tcRealtime) then
  begin
    if Assigned(FTimer) then
      FreeAndNil(FTimer);

    if Self.Bypass then
      OnPriceChange(Self, -1, ttNotSet, 1, Now) //bypass execute
    else
    begin
      if (Self.Instrument1.SokidInfo.ContractId > 0) then
      begin
        TIABMarket.RequestMarketData(Self.Instrument1.SokidInfo.ContractId);
        Self.Instrument1.PriceValue1 := TPriceCache.PriceCache.GetLastPrice(Self.Instrument1.SokidInfo.ContractId, Self.Instrument1.TickType1);
        if (Self.Instrument1.TickType2 < ttNotSet) then
          Self.Instrument1.PriceValue2 := TPriceCache.PriceCache.GetLastPrice(Self.Instrument1.SokidInfo.ContractId, Self.Instrument1.TickType2);
        Self.Instrument1.TotalValue := 0;
        TPublishers.FeedPublisher.Subscribe(Self);
      end;

      if (Self.Instrument2.SokidInfo.ContractId > 0) then
      begin
        TIABMarket.RequestMarketData(Self.Instrument2.SokidInfo.ContractId);
        Self.Instrument2.PriceValue1 := TPriceCache.PriceCache.GetLastPrice(Self.Instrument2.SokidInfo.ContractId, Self.Instrument2.TickType1);
        if (Self.Instrument2.TickType2 < ttNotSet) then
          Self.Instrument2.PriceValue2 := TPriceCache.PriceCache.GetLastPrice(Self.Instrument2.SokidInfo.ContractId, Self.Instrument2.TickType2);
        Self.Instrument2.TotalValue := 0;
        TPublishers.FeedPublisher.Subscribe(Self);
      end;
    end;

    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'TQualifierCondition.Initialize',
                               'Instrument1 - ' + THtmlLib.GetBoldText(Self.Instrument1.SokidInfo.Symbol) + ' (' + Self.Instrument1.SokidInfo.ContractId.ToString + ')<br>' +
                               'TickType1='     + Self.Instrument1.TickType1.ToString +
                               ', Value1='      + Self.Instrument1.PriceValue1.ToString +
                               ', TickType2='   + Self.Instrument1.TickType2.ToString +
                               ', Value2='      + Self.Instrument1.PriceValue2.ToString + '<br>' +
                               'Instrument2 - ' + THtmlLib.GetBoldText(Self.Instrument2.SokidInfo.Symbol) + ' (' + Self.Instrument2.SokidInfo.ContractId.ToString + ')<br>' +
                               'TickType1='     + Self.Instrument2.TickType1.ToString +
                               ', Value1='      + Self.Instrument2.PriceValue1.ToString +
                               ', TickType2='   + Self.Instrument2.TickType2.ToString +
                               ', Value2='      + Self.Instrument2.PriceValue2.ToString + '<br>' +
                               'Bypass='        + BoolToStr(Self.Bypass, True));
  end
  else
  if (Self.TypeCondition in [tcEveryDay, tcSpecificDate]) then
  begin
    if not Assigned(FTimer) then
      FTimer := TTimer.Create(nil);
    FTimer.OnTimer := Self.OnConditionTime;
    FTimer.Enabled := True;
  end;
end;

procedure TQualifierCondition.Deinitialize;
begin
  Self.Enabled := False;
  TPublishers.FeedPublisher.Unsubscribe(Self);
end;

procedure TQualifierCondition.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
var
  IsCondition: Boolean;
begin
  if (Self.Enabled) and
     (Assigned(FAutoTradesInstanceEvent)) and
     (Value > 0) and
     ((Instrument1.SokidInfo.ContractId = Id) or (Instrument2.SokidInfo.ContractId = Id) or Self.Bypass) and
     (Self.TypeCondition = tcRealtime)  then
  begin
    IsCondition := False;
    if Self.Bypass then
      IsCondition := True
    else
    begin
      //Instrument1
      if (Instrument1.SokidInfo.ContractId = Id) and (Value > 0) then
      begin
        if (Instrument1.TickType1 = TickType) then
        begin
          Instrument1.PriceValue1 := Value;
          if (Instrument1.TickType2 = ttNotSet) then
            Instrument1.TotalValue := Value
          else if (Instrument1.PriceValue2 > 0) then
            Instrument1.TotalValue := Instrument1.TypeOperation.Calc(Value, Instrument1.PriceValue2);
        end;
        if (Self.Instrument1.TickType2 = TickType) then
        begin
          Instrument1.PriceValue2 := Value;
          if (Instrument1.PriceValue1 > 0) then
             Instrument1.TotalValue := Instrument1.TypeOperation.Calc(Instrument1.PriceValue1, Value);
        end;
        QualifiersControllerPublisher.UpdateState(FQualifier);
      end;

      //Instrument2
      if (Instrument2.SokidInfo.ContractId = Id) and (Value > 0) then
      begin
        if (Instrument2.TickType1 = TickType) then
        begin
          Instrument2.PriceValue1 := Value;
          if (Instrument2.TickType2 = ttNotSet) then
            Instrument2.TotalValue := Value
          else if (Instrument2.PriceValue2 > 0) then
            Instrument2.TotalValue := Instrument2.TypeOperation.Calc(Value, Instrument2.PriceValue2);
        end;
        if (Instrument2.TickType2 = TickType) then
        begin
          Instrument2.PriceValue2 := Value;
          if (Instrument2.PriceValue1 > 0) then
            Instrument2.TotalValue := Instrument2.TypeOperation.Calc(Instrument2.PriceValue1, Value);
        end;
      end;
      QualifiersControllerPublisher.UpdateState(FQualifier);
    end;

    if (Instrument1.TotalValue <> 0) or (Instrument2.TotalValue <> 0) then
    begin
      FQualifier.State := tsWorking;
      if (FQualifier.PerTime = 0) then
        FQualifier.PerTime := TimeOf(Now);
    end;

    if (Instrument1.TotalValue <> 0) and (Instrument2.TotalValue <> 0) then
      IsCondition := FInequalityType.IsCondition(Instrument1.TotalValue, Instrument2.TotalValue);

    if IsCondition then
    begin
      TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnPriceChange',
                                 'Instrument1 - ' + THtmlLib.GetBoldText(Instrument1.SokidInfo.Symbol) + ' (' + Instrument1.SokidInfo.ContractId.ToString + ')<br>' +
                                 'TickType1='     + Instrument1.TickType1.ToString +
                                 ', Value1='      + Instrument1.PriceValue1.ToString +
                                 ', TickType2='   + Instrument1.TickType2.ToString +
                                 ', Value2='      + Instrument1.PriceValue2.ToString + '<br>' +
                                 'Instrument2 - ' + THtmlLib.GetBoldText(Instrument2.SokidInfo.Symbol) + ' (' + Instrument2.SokidInfo.ContractId.ToString + ')<br>' +
                                 'TickType1='     + Instrument2.TickType1.ToString +
                                 ', Value1='      + Instrument2.PriceValue1.ToString +
                                 ', TickType2='   + Instrument2.TickType2.ToString +
                                 ', Value2='      + Instrument2.PriceValue2.ToString + '<br>' +
                                 'Bypass='        + BoolToStr(FBypass, True));

      FQualifier.State := tsWorking;
      FIsCondition     := True;
      FEnabled         := False;

      if not Assigned(FQualifier.AutoTradesInstance) then
        for var AutoTradeInfo in FAutoTrades do
        begin
          FQualifier.AutoTradesInstance := FAutoTradesInstanceEvent(AutoTradeInfo.RecordId,
                                                                    FQualifier.RecordId,
                                                                    FQualifier.InstanceNum);
          Break;
        end;
      if (FQualifier.PreTime = 0) then
        FQualifier.PreTime := TimeOf(Now);
      FQualifier.DisableAllConditions;
      QualifiersControllerPublisher.UpdateState(FQualifier);
      TIABMarket.CancelMarketData(Instrument1.SokidInfo.ContractId);
      TIABMarket.CancelMarketData(Instrument2.SokidInfo.ContractId);
    end;
  end;
end;

procedure TQualifierCondition.OnConditionTime(Sender: TObject);
var
  IsCondition: Boolean;
  TimeStamp: TDateTime;
begin
  if (Self.Enabled) and (Assigned(FAutoTradesInstanceEvent)) and (Self.TypeCondition in [tcEveryDay, tcSpecificDate]) then
  begin
    TimeStamp := Now;
    if Self.Bypass then
      IsCondition := True
    else
    begin
      if (Self.TypeCondition = tcEveryDay) then
      begin
        var
        StartupDate := TimeOf(Self.StartupDate);
        IsCondition := (StartupDate < TimeOf(TimeStamp)) and (IncMinute(StartupDate, 5) > TimeOf(TimeStamp));
      end
      else if (Self.TypeCondition = tcSpecificDate) then
        IsCondition := (Self.StartupDate < TimeStamp) and (IncMinute(Self.StartupDate, 5) > TimeStamp)
      else
        IsCondition := False;
    end;

    if IsCondition then
    begin
      if Assigned(FTimer) then
        FTimer.Enabled := False;
      TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnConditionTime',
                                     'StartupDate - ' + FormatDateTime('DD.MM.YYYY hh:nn:ss', TimeStamp) + ')<br>' +
                                     'Bypass=' + BoolToStr(FBypass, True));

      FQualifier.State := tsWorking;
      FIsCondition     := True;
      FEnabled         := False;

      if not Assigned(FQualifier.AutoTradesInstance) then
        for var AutoTradeInfo in FAutoTrades do
        begin
          FQualifier.AutoTradesInstance := FAutoTradesInstanceEvent(AutoTradeInfo.RecordId, FQualifier.RecordId, FQualifier.InstanceNum);
          Break;
        end;
      if (FQualifier.PreTime = 0) then
        FQualifier.PreTime := TimeOf(Now);
      FQualifier.DisableAllConditions;
      QualifiersControllerPublisher.UpdateState(FQualifier);
      TIABMarket.CancelMarketData(Instrument1.SokidInfo.ContractId);
      TIABMarket.CancelMarketData(Instrument2.SokidInfo.ContractId);
    end;
  end;
end;

procedure TQualifierCondition.FromXml(const aXmlText: string);
var
  XMLFile: TXMLFile;
begin
  XMLFile := TXMLFile.Create;
  try
    XMLFile.XMLText := aXmlText;
    Self.Instrument1.TickType1     := TIABTickType(XmlFile.ReadInteger(C_SECTION_INSTRUMENT1, 'IBValue', Ord(ttLast)));
    Self.Instrument1.TickType2     := TIABTickType(XmlFile.ReadInteger(C_SECTION_INSTRUMENT1, 'IBValue2', Ord(ttNotSet)));
    Self.Instrument1.TypeOperation := TTypeOperation(XmlFile.ReadInteger(C_SECTION_INSTRUMENT1, 'TypeOperation', Ord(toDivide)));

    Self.Instrument2.TickType1     := TIABTickType(XmlFile.ReadInteger(C_SECTION_INSTRUMENT2, 'IBValue', Ord(ttLast)));
    Self.Instrument2.TickType2     := TIABTickType(XmlFile.ReadInteger(C_SECTION_INSTRUMENT2, 'IBValue2', Ord(ttNotSet)));
    Self.Instrument2.TypeOperation := TTypeOperation(XmlFile.ReadInteger(C_SECTION_INSTRUMENT2, 'TypeOperation', Ord(toDivide)));

    Self.InequalityType := TInequalityType(XmlFile.ReadInteger(C_SECTION, 'Inequality', Ord(TInequalityType.iqAbove)));
  finally
    FreeAndNil(XMLFile);
  end;
end;

function TQualifierCondition.ToString: string;
begin
  case Self.TypeCondition of
    tcRealtime:
      Result := Self.Instrument1.SokidInfo.Symbol + ' ' + Self.InequalityType.ToString + ' ' + Self.Instrument2.SokidInfo.Symbol;
    tcEveryDay:
      Result := FormatDateTime('hh:nn:ss', Self.StartupDate);
    tcSpecificDate:
      Result := FormatDateTime('yyyy.mm.dd hh:nn:ss', Self.StartupDate);
  else
    Result := '';
  end;
end;

function TQualifierCondition.ToValueString: string;
begin
  Result := '';
end;

function TQualifierCondition.ToXml: string;
var
  XmlFile: TXmlFile;
begin
  XmlFile := TXmlFile.Create;
  try
    XmlFile.WriteInteger(C_SECTION_INSTRUMENT1, 'IBValue', Ord(Self.Instrument1.TickType1));
    XmlFile.WriteInteger(C_SECTION_INSTRUMENT1, 'IBValue2', Ord(Self.Instrument1.TickType2));
    XmlFile.WriteInteger(C_SECTION_INSTRUMENT1, 'TypeOperation', Ord(Self.Instrument1.TypeOperation));
    XmlFile.WriteInteger(C_SECTION_INSTRUMENT2, 'IBValue', Ord(Self.Instrument2.TickType1));
    XmlFile.WriteInteger(C_SECTION_INSTRUMENT2, 'IBValue2', Ord(Self.Instrument2.TickType2));
    XmlFile.WriteInteger(C_SECTION_INSTRUMENT2, 'TypeOperation', Ord(Self.Instrument2.TypeOperation));
    XmlFile.WriteInteger(C_SECTION, 'Inequality', Ord(Self.InequalityType));
    Result := XmlFile.XMLText;
  finally
    FreeAndNil(XmlFile);
  end;
end;

{ TQualifier }

procedure TQualifier.FromDB(const aID: Integer);
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT * FROM QUALIFIERS WHERE ID=:ID';
var
  Query: TFDQuery;
begin
  Self.Clear;
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
        Self.Name        := Query.FieldByName('NAME').AsString;
        Self.Description := Query.FieldByName('NOTE').AsString;
        Self.Enabled     := Query.FieldByName('ENABLED').AsInteger = 1;
        Self.CreateTime  := Now;
      end;
      Self.RecordId := aID;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TQualifier.Initialize;
begin
  for var Condition in Self.Conditions do
  begin
   Condition.OnExecuteAutoTradesInstance := FAutoTradesInstanceEvent;
   Condition.Initialize;
  end;
end;

procedure TQualifier.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM QUALIFIERS WHERE ID=:ID';
  C_SQL_UPDATE_TEXT = 'UPDATE QUALIFIERS SET NAME=:NAME, ENABLED=:ENABLED, NOTE=:NOTE, XML_PARAMS=:XML_PARAMS WHERE ID=:ID;';
  C_SQL_INSERT_TEXT = 'INSERT INTO QUALIFIERS(ID, NAME, NOTE, ENABLED, XML_PARAMS) ' + sLineBreak +
                                    ' VALUES(:ID,:NAME,:NOTE,:ENABLED,:XML_PARAMS); ';
var
  Query: TFDQuery;
  IsExists: Boolean;
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
        Query.ParamByName('ID').AsInteger := Self.RecordId;
        Query.Prepare;
        Query.Open;
        IsExists := Query.FieldByName('CNT').AsInteger > 0;
        Query.Close;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'SaveToDB', 'QualifiersTypes', E.Message + TDModUtils.GetQueryInfo(Query));
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
      Self.Name := 'Qualifier nr' + Self.RecordId.ToString;

    Query.ParamByName('ID').AsInteger      := Self.RecordId;
    Query.ParamByName('NAME').AsString     := Self.Name.Substring(0, 100);
    Query.ParamByName('NOTE').AsString     := Self.Description.Substring(0, 500);
    Query.ParamByName('ENABLED').AsInteger := IfThen(Self.Enabled, 1, 0);
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'SaveToDB', 'QualifiersTypes', E.Message + TDModUtils.GetQueryInfo(Query));
    end;
  finally
    FreeAndNil(Query);
    DMod.RefreshQuery(DMod.fbqQualifiers);
  end;
end;

procedure TQualifier.LoadItems;
resourcestring
  C_SQL_SELECT_ITEMS_TEXT = 'SELECT ID FROM QUALIFIERS_CONDITION WHERE QUALIFIER_ID=:ID';
var
  Query: TFDQuery;
  i: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_SELECT_ITEMS_TEXT;
    Query.ParamByName('ID').AsInteger := Self.RecordId;
    Query.Open;
    Query.FetchAll;
    Query.First;
    SetLength(Self.Conditions, Query.RecordCount);
    i := 0;
    while not Query.Eof do
    begin
      if not Assigned(Self.Conditions[i]) then
        Self.Conditions[i] := TQualifierCondition.Create;
      Self.Conditions[i].FromDB(Query.FieldByName('ID').AsInteger);
      Self.Conditions[i].Qualifier := Self;
      Inc(i);
      Query.Next;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TQualifier.AssignFrom(const aQualifier: TQualifier; const aWithItems: Boolean = False);
begin
  Self.Clear;
  Self.RecordId           := aQualifier.RecordId;
  Self.InstanceNum        := aQualifier.InstanceNum;
  Self.Name               := aQualifier.Name;
  Self.Enabled            := aQualifier.Enabled;
  Self.Description        := aQualifier.Description;
  Self.State              := aQualifier.State;
  Self.PreTime            := aQualifier.PreTime;
  Self.PerTime            := aQualifier.PerTime;
  Self.CreateTime         := aQualifier.CreateTime;
  Self.AutoTradesInstance := aQualifier.AutoTradesInstance;
  if aWithItems then
  begin
    SetLength(Self.Conditions, Length(aQualifier.Conditions));
    for var i := Low(aQualifier.Conditions) to High(aQualifier.Conditions) do
    begin
      if not Assigned(Self.Conditions[i]) then
        Self.Conditions[i] := TQualifierCondition.Create;
      Self.Conditions[i].AssignFrom(aQualifier.Conditions[i]);
      Self.Conditions[i].Qualifier := Self;
    end;
  end;
end;

procedure TQualifier.Clear;
begin
  for var i := Low(Self.Conditions) to High(Self.Conditions) do
    if Assigned(Self.Conditions[i]) then
      FreeAndNil(Self.Conditions[i]);
  Self := Default (TQualifier);
end;

procedure TQualifier.DisableAllConditions;
begin
  for var i := Low(Self.Conditions) to High(Self.Conditions) do
    if Assigned(Self.Conditions[i]) then
      Self.Conditions[i].Deinitialize;
end;

{ TQualifierList }

constructor TQualifierList.Create;
begin
  inherited;

end;

function TQualifierList.GetItemByNum(const aInstanceNum: Integer): TQualifier;
var
  i: Integer;
begin
  Result.Clear;
  for i := 0 to Self.Count - 1 do
    if (Self.Items[i].InstanceNum = aInstanceNum) then
      Exit(Self.Items[i]);
end;

function TQualifierList.GetIndexByNum(const aInstanceNum: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Self.Count - 1 do
    if (Self.Items[i].InstanceNum = aInstanceNum) then
      Exit(i);
end;

function TQualifierList.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TQualifierList.Beeps;
begin
  TTask.Create(
    procedure()
    begin
      BeepEx(1040, 150);
      Sleep(10);
      BeepEx(1000, 100);
      Sleep(10);
      BeepEx(1040, 150);
    end).Start;
end;

{ TQualifiersControllerPublisher }

procedure TQualifiersControllerPublisher.DeleteQualifier(const aInstanceNum: Integer);
var
  Item: TObject;
  qc: IQualifiersController;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IQualifiersController, qc) then
        qc.DeleteQualifier(aInstanceNum);
    end;
end;

procedure TQualifiersControllerPublisher.UpdateState(const aQualifier: TQualifier);
var
  Item: TObject;
  qc: IQualifiersController;
begin
  if not Application.Terminated then
    for var i := 0 to Self.Count - 1 do
    begin
      Item := Self.Items[i];
      if Assigned(Item) and Supports(Item, IQualifiersController, qc) then
        qc.UpdateState(aQualifier);
    end;
end;


{ TTypeConditionHelper }

function TTypeConditionHelper.ToString: string;
begin
  Result := TypeConditionString[Self];
end;

{ TQualifierInstrument }

procedure TQualifierInstrument.AssignFrom(const aQualifierInstrument: TQualifierInstrument);
begin
  Self.SokidInfo     := aQualifierInstrument.SokidInfo;
  Self.TickType1     := aQualifierInstrument.TickType1;
  Self.TickType2     := aQualifierInstrument.TickType2;
  Self.PriceValue1   := aQualifierInstrument.PriceValue1;
  Self.PriceValue2   := aQualifierInstrument.PriceValue2;
  Self.TotalValue    := aQualifierInstrument.TotalValue;
  Self.TypeOperation := aQualifierInstrument.TypeOperation;
end;

initialization
  QualifiersControllerPublisher := TQualifiersControllerPublisher.Create;

finalization
  if Assigned(QualifiersControllerPublisher) then
    FreeAndNil(QualifiersControllerPublisher);

end.
