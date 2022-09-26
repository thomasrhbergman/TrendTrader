unit Qualifiers.Types;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, VirtualTrees, IABFunctions,
  IABSocketAPI, System.Generics.Collections, BrokerHelperAbstr, Winapi.msxml, Vcl.Graphics, Entity.Sokid,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const, DebugWriter, HtmlLib, Chart.Trade,
  System.DateUtils, XmlFiles, Data.DB, DaModule, InstrumentList, System.Math, System.Threading,
  Publishers.Interfaces, Generics.Helper, Common.Types, Bleeper, DaModule.Utils, ArrayHelper,
  Global.Types, Publishers, Vcl.Forms, IABFunctions.Helpers, IABFunctions.MarketData, Vcl.ExtCtrls,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet, StrUtils;
{$ENDREGION}

type
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

  (*
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
  *)

  TOnConditionChanged = procedure of object;

  PQualifier = ^TQualifier;
  TQualifier = class(TBaseClass, IUpdateFeeds)
  private const
    C_SECTION = 'Params';
    C_SECTION_INSTRUMENT1 = 'Instrument1';
    C_SECTION_INSTRUMENT2 = 'Instrument2';
    C_SECTION_INSTRUMENT = 'Instrument';
  private
    FBypass: Boolean;
    FCreateTime: TTime;
    FEnabled: Boolean;
    FInequalityCompare: TInequalityType;
    FInequalityValue: TInequalityType;
    FOwnerNode: PVirtualNode;

    FStartupDate: TDateTime;
    FTypeCondition: TTypeCondition;
    FTimer: TTimer;
    FPreTime: TTime;
    FPerTime: TTime;
    FInstanceNum: Integer;

    FComparisonValue: Double;
    FFromTime: TTIme;
    FToTime: TTime;
    FIsCompare: Boolean;
    FIsCompareCondition: Boolean;
    FIsValue: Boolean;
    FIsValueCondition: Boolean;
    FIsTime: Boolean;
    FIsTimeCondition: Boolean;

    FOnConditionChanged: TOnConditionChanged;
    procedure OnConditionTime(Sender: TObject);
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    function GetIsCondition: boolean;
    function GetStatusText: string;
  public
    Instrument1: TQualifierInstrument;
    Instrument2: TQualifierInstrument;
    Instrument: TQualifierInstrument;

    function ToString: string; override;
    function ToValueString: string;
    function ToXml: string;
    procedure AssignFrom(const aQualifierItem: TQualifier);
    procedure FromXml(const aXmlText: string);
    procedure Clear;
    constructor Create; override;
    procedure Deinitialize;
    procedure Initialize;
    destructor Destroy; override;

    procedure FromDB(aID: Integer); override;
    procedure SaveToDB; override;
    class function GetListSQL: string; override;
    class function GetListCaption: string; override;
    class procedure DeleteFromDB(aID: Integer); override;


    property Bypass             : Boolean              read FBypass             write FBypass;
    property CreateTime         : TTime                read FCreateTime         write FCreateTime;
    property Enabled            : Boolean              read FEnabled            write FEnabled;
    property InequalityCompare  : TInequalityType      read FInequalityCompare  write FInequalityCompare;
    property InequalityValue    : TInequalityType      read FInequalityValue    write FInequalityValue;
    property IsCondition        : Boolean              read GetIsCondition;
    property OwnerNode          : PVirtualNode         read FOwnerNode          write FOwnerNode;
    property StartupDate        : TDateTime            read FStartupDate        write FStartupDate;
    property TypeCondition      : TTypeCondition       read FTypeCondition      write FTypeCondition;
    property PreTime            : TTime                read FPreTime            write FPreTime;
    property PerTime            : TTime                read FPerTime            write FPerTime;
    property InstanceNum        : Integer              read FInstanceNum        write FInstanceNum;

    property ComparisonValue    : Double               read FComparisonValue    write FComparisonValue;
    property FromTime           : TTIme                read FFromTime           write FFromTime;
    property ToTime             : TTime                read FToTime             write FToTime;
    property IsCompare          : Boolean              read FIsCompare          write FIsCompare;
    property IsCompareCondition : Boolean              read FIsCompareCondition write FIsCompareCondition;
    property IsValue            : Boolean              read FIsValue            write FIsValue;
    property IsValueCondition   : Boolean              read FIsValueCondition   write FIsValueCondition;
    property IsTime             : Boolean              read FIsTime             write FIsTime;
    property IsTimeCondition    : Boolean              read FIsTimeCondition    write FIsTimeCondition;

    property StatusText         : String               read GetStatusText;
    property OnConditionChanged : TOnConditionChanged  read FOnConditionChanged write FOnConditionChanged;
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

{ TQualifier }

destructor TQualifier.Destroy;
begin
  if Assigned(FTimer) then
    FreeAndNil(FTimer);
  inherited;
end;

procedure TQualifier.AssignFrom(const aQualifierItem: TQualifier);
begin
  Self.Clear;
  Self.TypeCondition      := aQualifierItem.TypeCondition;
  Self.RecordId           := aQualifierItem.RecordId;
  Self.Name               := aQualifierItem.Name;
  Self.Bypass             := aQualifierItem.Bypass;
  Self.InequalityCompare  := aQualifierItem.InequalityCompare;
  Self.InequalityValue    := aQualifierItem.InequalityValue;
  Self.Enabled            := aQualifierItem.Enabled;
  Self.StartupDate        := aQualifierItem.StartupDate;
  Self.OwnerNode          := aQualifierItem.OwnerNode;
  Self.CreateTime         := aQualifierItem.CreateTime;
  Self.ComparisonValue    := aQualifierItem.ComparisonValue;
  Self.FromTime           := aQualifierItem.FromTime;
  Self.ToTime             := aQualifierItem.ToTime;
  Self.IsCompare          := aQualifierItem.IsCompare;
  Self.IsValue            := aQualifierItem.IsValue;
  Self.IsTime             := aQualifierItem.IsTime;

  Self.Instrument1.AssignFrom(aQualifierItem.Instrument1);
  Self.Instrument2.AssignFrom(aQualifierItem.Instrument2);
  Self.Instrument.AssignFrom(aQualifierItem.Instrument);
  (*Self.AutoTrades.Count := aQualifierItem.AutoTrades.Count;
  for var i := Low(aQualifierItem.AutoTrades.Items) to High(aQualifierItem.AutoTrades.Items) do
  begin
    Self.AutoTrades.Items[i].AssignFrom(aQualifierItem.AutoTrades.Items[i]);
    Self.AutoTrades.Items[i].QualifierInstance := aQualifierItem.Qualifier.InstanceNum;
  end;*)
end;

procedure TQualifier.Clear;
begin
  Self.Name               := '';
  Self.RecordId           := -1;
  Self.StartupDate        := 0;
  Self.CreateTime         := 0;
  Self.Enabled            := True;
  Self.Bypass             := False;
  Self.TypeCondition      := Low(TTypeCondition);
  Self.InequalityCompare  := Low(TInequalityType);
  Self.InequalityValue    := Low(TInequalityType);
  Self.Instrument1        := Default(TQualifierInstrument);
  Self.Instrument2        := Default(TQualifierInstrument);
  Self.Instrument         := Default(TQualifierInstrument);
  Self.OwnerNode          := nil;
  Self.ComparisonValue    := 0;
  Self.FromTime           := 0;
  Self.ToTime             := 0;
  Self.IsCompare          := false;
  Self.IsValue            := false;
  Self.IsTime             := false;
  Self.IsCompareCondition := false;
  Self.IsValueCondition   := false;
  Self.IsTimeCondition    := false;
end;

constructor TQualifier.Create;
begin
  inherited;
  ManualFree := true;
  Clear;
end;

procedure TQualifier.FromDB(aID: Integer);
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
        Self.Name             := Query.FieldByName('NAME').AsString;
        Self.TypeCondition    := TTypeCondition(Query.FieldByName('TYPE_CONDITION').AsInteger);
        Self.Enabled          := Query.FieldByName('ENABLED').AsBoolean;
        Self.Bypass           := Query.FieldByName('BYPASS').AsBoolean;
        Self.StartupDate      := Query.FieldByName('STARTUP_DATE').AsDateTime;
        Self.CreateTime       := Now;
        Self.ComparisonValue  := Query.FieldByName('COMPARISON_VALUE').AsFloat;
        Self.FromTime         := Query.FieldByName('FROM_TIME').AsDateTime;
        Self.ToTime           := Query.FieldByName('TO_TIME').AsDateTime;
        Self.IsCompare        := Query.FieldByName('IS_COMPARE').AsBoolean;
        Self.IsValue          := Query.FieldByName('IS_VALUE').AsBoolean;
        Self.IsTime           := Query.FieldByName('IS_TIME').AsBoolean;

        Self.Instrument1.SokidInfo.ContractId := Query.FieldByName('INSTRUMENT1').AsInteger;
        Self.Instrument2.SokidInfo.ContractId := Query.FieldByName('INSTRUMENT2').AsInteger;
        Self.Instrument.SokidInfo.ContractId  := Query.FieldByName('INSTRUMENT').AsInteger;


        Self.FromXml(Query.FieldByName('XML_PARAMS').AsString);

        case Self.TypeCondition of
          tcRealtime:
            begin
              if SokidList.ContainsKey(Self.Instrument1.SokidInfo.ContractId) then
                Self.Instrument1.SokidInfo.AssignFrom(SokidList.Items[Self.Instrument1.SokidInfo.ContractId]);
              if SokidList.ContainsKey(Self.Instrument2.SokidInfo.ContractId) then
                Self.Instrument2.SokidInfo.AssignFrom(SokidList.Items[Self.Instrument2.SokidInfo.ContractId]);
              if SokidList.ContainsKey(Self.Instrument.SokidInfo.ContractId) then
                Self.Instrument.SokidInfo.AssignFrom(SokidList.Items[Self.Instrument.SokidInfo.ContractId]);
            end;
        end;
      end;
      Self.RecordId := aID;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TQualifier.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM QUALIFIERS WHERE ID=:ID';
  C_SQL_UPDATE_TEXT = 'UPDATE QUALIFIERS SET TYPE_CONDITION=:TYPE_CONDITION, NAME=:NAME, ENABLED=:ENABLED,' + sLineBreak +
                                            'BYPASS=:BYPASS, INSTRUMENT1=:INSTRUMENT1, INSTRUMENT2=:INSTRUMENT2, INSTRUMENT=:INSTRUMENT, ' + sLineBreak +
                                            'STARTUP_DATE=:STARTUP_DATE, FROM_TIME=:FROM_TIME, TO_TIME=:TO_TIME, '+ sLineBreak +
                                            'IS_COMPARE=:IS_COMPARE, IS_VALUE=:IS_VALUE, IS_TIME=:IS_TIME, COMPARISON_VALUE=:COMPARISON_VALUE, '+ sLineBreak +
                                            'XML_PARAMS=:XML_PARAMS WHERE ID=:ID';

  C_SQL_INSERT_TEXT = 'INSERT INTO QUALIFIERS ( ID, TYPE_CONDITION, NAME, ENABLED, BYPASS, INSTRUMENT1, INSTRUMENT2, INSTRUMENT,'+
                                               'STARTUP_DATE, FROM_TIME, TO_TIME, IS_COMPARE, IS_VALUE, IS_TIME, COMPARISON_VALUE, XML_PARAMS)' + sLineBreak +
                                     ' VALUES (:ID,:TYPE_CONDITION,:NAME,:ENABLED,:BYPASS,:INSTRUMENT1,:INSTRUMENT2,:INSTRUMENT,'+
                                              ':STARTUP_DATE,:FROM_TIME,:TO_TIME,:IS_COMPARE,:IS_VALUE,:IS_TIME,:COMPARISON_VALUE,:XML_PARAMS)';
var
  Query: TFDQuery;
  IsExists: Boolean;

  procedure CheckInstrumentInDB(AConID: Integer);
  var Q: TFDQuery;
  begin
    Q := TFDQuery.Create(nil);
    try
      Q.Connection := DMod.ConnectionStock;
      Q.SQL.Text := 'SELECT * FROM SOKID_IB WHERE CONID = :CONID';
      Q.ParamByName('CONID').Value := AConID;
      Q.Open;
      if Q.RecordCount = 0 then
        raise Exception.Create('Instrument with ID = '+ AConID.ToString + ' was not found');
    finally
      Q.Free;
    end;
  end;

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
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'SaveToDB', 'Qualifier', E.Message + TDModUtils.GetQueryInfo(Query));
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

    Query.ParamByName('ID').AsInteger             := Self.RecordId;
    Query.ParamByName('TYPE_CONDITION').AsInteger := Ord(Self.TypeCondition);
    Query.ParamByName('NAME').AsString            := Self.Name.Substring(0, 100);
    Query.ParamByName('ENABLED').AsBoolean        := Self.Enabled;
    Query.ParamByName('BYPASS').AsBoolean         := Self.Bypass;
    Query.ParamByName('STARTUP_DATE').AsDateTime  := Self.StartupDate;
    Query.ParamByName('FROM_TIME').AsDateTime     := Self.FromTime;
    Query.ParamByName('TO_TIME').AsDateTime       := Self.ToTime;
    Query.ParamByName('IS_COMPARE').AsBoolean     := Self.IsCompare;
    Query.ParamByName('IS_VALUE').AsBoolean       := Self.IsValue;
    Query.ParamByName('IS_TIME').AsBoolean        := Self.IsTime;
    Query.ParamByName('COMPARISON_VALUE').AsFloat := Self.ComparisonValue;

    Query.ParamByName('XML_PARAMS').AsString      := Self.ToXml;

    if (Self.Instrument1.SokidInfo.ContractId > 0) then
    begin
      CheckInstrumentInDB(Self.Instrument1.SokidInfo.ContractId);
      Query.ParamByName('INSTRUMENT1').AsInteger := Self.Instrument1.SokidInfo.ContractId
    end
    else
      Query.ParamByName('INSTRUMENT1').Clear;
    if (Self.Instrument2.SokidInfo.ContractId > 0) then
    begin
      CheckInstrumentInDB(Self.Instrument2.SokidInfo.ContractId);
      Query.ParamByName('INSTRUMENT2').AsInteger := Self.Instrument2.SokidInfo.ContractId
    end
    else
      Query.ParamByName('INSTRUMENT2').Clear;
    if (Self.Instrument.SokidInfo.ContractId > 0) then
    begin
      CheckInstrumentInDB(Self.Instrument.SokidInfo.ContractId);
      Query.ParamByName('INSTRUMENT').AsInteger := Self.Instrument.SokidInfo.ContractId
    end
    else
      Query.ParamByName('INSTRUMENT').Clear;

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

function TQualifier.GetInstance: TObject;
begin
  Result := Self;
end;

function TQualifier.GetIsCondition: boolean;
begin
  Result :=
    ((IsCompare and IsCompareCondition) or not IsCompare)
    and
    ((IsValue and IsValueCondition) or not IsValue)
    and
    ((IsTime and IsTimeCondition) or not IsTime);
end;

class function TQualifier.GetListCaption: string;
begin
  Result := 'Qualifiers list';
end;

class function TQualifier.GetListSQL: string;
begin
  Result := 'SELECT ID, NAME FROM QUALIFIERS ORDER BY LOWER(NAME)';
end;

function TQualifier.GetStatusText: string;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    if IsCompare then
      L.Add(Format('Compare %s %s %s', [Instrument1.SokidInfo.Name, InequalityCompare.ToString, Instrument2.SokidInfo.Name]));
    if IsValue then
      L.Add(Format('Value %s %s %f', [Instrument.SokidInfo.Name, InequalityValue.ToString, ComparisonValue]));
    if IsTime then
      L.Add(Format('Time %s - %s', [FormatDateTime('hh:nn', FromTime), FormatDateTime('hh:nn', ToTime)]));
    Result := ReplaceText(Trim(L.Text), sLineBreak, '; ');
  finally
    FreeAndNil(L);
  end;
end;

procedure TQualifier.Initialize;
begin
//  if Assigned(FTimer) then
//    FreeAndNil(FTimer);

  if IsCompare then
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

  if IsValue then
  begin
    if (Self.Instrument.SokidInfo.ContractId > 0) then
    begin
      TIABMarket.RequestMarketData(Self.Instrument.SokidInfo.ContractId);
      Self.Instrument.PriceValue1 := TPriceCache.PriceCache.GetLastPrice(Self.Instrument.SokidInfo.ContractId, Self.Instrument.TickType1);
      if (Self.Instrument2.TickType2 < ttNotSet) then
        Self.Instrument.PriceValue2 := TPriceCache.PriceCache.GetLastPrice(Self.Instrument.SokidInfo.ContractId, Self.Instrument.TickType2);
      Self.Instrument.TotalValue := 0;
      TPublishers.FeedPublisher.Subscribe(Self);
    end;
  end;

  if IsTime then
  begin
    if not Assigned(FTimer) then
      FTimer := TTimer.Create(nil);
    FTimer.OnTimer := Self.OnConditionTime;
    FTimer.Enabled := True;
  end;

  TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'TQualifier.Initialize',
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
end;

procedure TQualifier.Deinitialize;
begin
  Self.Enabled := False;
  TPublishers.FeedPublisher.Unsubscribe(Self);
end;

class procedure TQualifier.DeleteFromDB(aID: Integer);
resourcestring
  C_SQL_DELETE = 'DELETE FROM QUALIFIERS WHERE ID=%d';
begin
  //TTreeDocument.DeleteRelations(Data.ID);
  DMod.ExecuteSQL(Format(C_SQL_DELETE, [aID]));
end;

procedure TQualifier.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
var lOldIsCondition: boolean;
begin
  lOldIsCondition := IsCondition;
  if (Self.Enabled) and
     (Value > 0) and
     ((Instrument1.SokidInfo.ContractId = Id) or (Instrument2.SokidInfo.ContractId = Id) or (Instrument.SokidInfo.ContractId = Id)) and
     (Self.TypeCondition = tcRealtime)  then
  begin

    if IsCompare then
    begin
      IsCompareCondition := false;
      //Instrument1
      if (Instrument1.SokidInfo.ContractId = Id) and (Value > 0) then
      begin
        if (Instrument1.TickType1 = TickType) then
        begin
          Instrument1.PriceValue1 := Value;
          {if (Instrument1.TickType2 = ttNotSet) then
            Instrument1.TotalValue := Value
          else if (Instrument1.PriceValue2 > 0) then
            Instrument1.TotalValue := Instrument1.TypeOperation.Calc(Value, Instrument1.PriceValue2); }
        end;
        {if (Self.Instrument1.TickType2 = TickType) then
        begin
          Instrument1.PriceValue2 := Value;
          if (Instrument1.PriceValue1 > 0) then
             Instrument1.TotalValue := Instrument1.TypeOperation.Calc(Instrument1.PriceValue1, Value);
        end; }
      end;

      //Instrument2
      if (Instrument2.SokidInfo.ContractId = Id) and (Value > 0) then
      begin
        if (Instrument2.TickType1 = TickType) then
        begin
          Instrument2.PriceValue1 := Value;
          {if (Instrument2.TickType2 = ttNotSet) then
            Instrument2.TotalValue := Value
          else if (Instrument2.PriceValue2 > 0) then
            Instrument2.TotalValue := Instrument2.TypeOperation.Calc(Value, Instrument2.PriceValue2); }
        end;
        {if (Instrument2.TickType2 = TickType) then
        begin
          Instrument2.PriceValue2 := Value;
          if (Instrument2.PriceValue1 > 0) then
            Instrument2.TotalValue := Instrument2.TypeOperation.Calc(Instrument2.PriceValue1, Value);
        end; }
      end;

      if (Instrument1.TotalValue <> 0) or (Instrument2.TotalValue <> 0) then
      begin
        //State := tsWorking;
        if (PerTime = 0) then
          PerTime := TimeOf(Now);
      end;

      if (Instrument1.PriceValue1 <> 0) and (Instrument2.PriceValue1 <> 0) then
        IsCompareCondition := FInequalityCompare.IsCondition(Instrument1.PriceValue1, Instrument2.PriceValue1);
    end;

    if IsValue then
    begin
      IsValueCondition := false;
      //Instrument
      if (Instrument.SokidInfo.ContractId = Id) and (Value > 0) then
      begin
        if (Instrument.TickType1 = TickType) then
        begin
          Instrument.PriceValue1 := Value;
          {if (Instrument.TickType2 = ttNotSet) then
            Instrument.TotalValue := Value
          else if (Instrument.PriceValue2 > 0) then
            Instrument.TotalValue := Instrument.TypeOperation.Calc(Value, Instrument.PriceValue2); }
        end;
        {if (Instrument.TickType2 = TickType) then
        begin
          Instrument.PriceValue2 := Value;
          if (Instrument.PriceValue1 > 0) then
            Instrument.TotalValue := Instrument.TypeOperation.Calc(Instrument.PriceValue1, Value);
        end;}
      end;

      if (Instrument.PriceValue1 <> 0)  then
        IsValueCondition := FInequalityValue.IsCondition(Instrument.PriceValue1, ComparisonValue);

    end;

    QualifiersControllerPublisher.UpdateState(Self);

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

      //State := tsWorking;
      FEnabled         := False;

      {if not Assigned(FQualifier.AutoTradesInstance) then
        for var AutoTradeInfo in FAutoTrades do
        begin
          FQualifier.AutoTradesInstance := FAutoTradesInstanceEvent(AutoTradeInfo.RecordId,
                                                                    FQualifier.RecordId,
                                                                    FQualifier.InstanceNum);
          Break;
        end; }
      if (PreTime = 0) then
        PreTime := TimeOf(Now);

      QualifiersControllerPublisher.UpdateState(Self);
      TIABMarket.CancelMarketData(Instrument1.SokidInfo.ContractId);
      TIABMarket.CancelMarketData(Instrument2.SokidInfo.ContractId);
      TIABMarket.CancelMarketData(Instrument.SokidInfo.ContractId);
    end;
  end;
  if lOldIsCondition <> IsCondition then
    if Assigned(OnConditionChanged) then
      OnConditionChanged;
end;

procedure TQualifier.OnConditionTime(Sender: TObject);
var
//  IsCondition: Boolean;
//  TimeStamp: TDateTime;
   lOldIsCondition: boolean;
begin
  lOldIsCondition := IsCondition;

  if IsTime then
    IsTimeCondition := (TimeOf(Now) >= TimeOf(FromTime)) and (TimeOf(Now) <= TimeOf(ToTime));

  if lOldIsCondition <> IsCondition then
    if Assigned(OnConditionChanged) then
      OnConditionChanged;
  (*if (Self.Enabled) and (Self.TypeCondition in [tcEveryDay, tcSpecificDate]) then
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

      //State            := tsWorking;
      FEnabled         := False;

      {if not Assigned(FQualifier.AutoTradesInstance) then
        for var AutoTradeInfo in FAutoTrades do
        begin
          FQualifier.AutoTradesInstance := FAutoTradesInstanceEvent(AutoTradeInfo.RecordId, FQualifier.RecordId, FQualifier.InstanceNum);
          Break;
        end;    }
      if (PreTime = 0) then
        PreTime := TimeOf(Now);

      QualifiersControllerPublisher.UpdateState(Self);
      TIABMarket.CancelMarketData(Instrument1.SokidInfo.ContractId);
      TIABMarket.CancelMarketData(Instrument2.SokidInfo.ContractId);
    end;
  end;
  *)
end;

procedure TQualifier.FromXml(const aXmlText: string);
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

    Self.Instrument.TickType1      := TIABTickType(XmlFile.ReadInteger(C_SECTION_INSTRUMENT, 'IBValue', Ord(ttLast)));
    Self.Instrument.TickType2      := TIABTickType(XmlFile.ReadInteger(C_SECTION_INSTRUMENT, 'IBValue2', Ord(ttNotSet)));
    Self.Instrument.TypeOperation  := TTypeOperation(XmlFile.ReadInteger(C_SECTION_INSTRUMENT, 'TypeOperation', Ord(toDivide)));


    Self.InequalityCompare := TInequalityType(XmlFile.ReadInteger(C_SECTION, 'InequalityCompare', Ord(TInequalityType.iqAbove)));
    Self.InequalityValue := TInequalityType(XmlFile.ReadInteger(C_SECTION, 'InequalityValue', Ord(TInequalityType.iqAbove)));
  finally
    FreeAndNil(XMLFile);
  end;
end;

function TQualifier.ToString: string;
begin
  case Self.TypeCondition of
    tcRealtime:
      Result := Self.Instrument1.SokidInfo.Symbol + ' ' + Self.InequalityCompare.ToString + ' ' + Self.Instrument2.SokidInfo.Symbol;
    tcEveryDay:
      Result := FormatDateTime('hh:nn:ss', Self.StartupDate);
    tcSpecificDate:
      Result := FormatDateTime('yyyy.mm.dd hh:nn:ss', Self.StartupDate);
  else
    Result := '';
  end;
end;

function TQualifier.ToValueString: string;
begin
  Result := '';
end;

function TQualifier.ToXml: string;
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
    XmlFile.WriteInteger(C_SECTION_INSTRUMENT, 'IBValue', Ord(Self.Instrument.TickType1));
    XmlFile.WriteInteger(C_SECTION_INSTRUMENT, 'IBValue2', Ord(Self.Instrument.TickType2));
    XmlFile.WriteInteger(C_SECTION_INSTRUMENT, 'TypeOperation', Ord(Self.Instrument.TypeOperation));
    XmlFile.WriteInteger(C_SECTION, 'InequalityCompare', Ord(Self.InequalityCompare));
    XmlFile.WriteInteger(C_SECTION, 'InequalityValue', Ord(Self.InequalityValue));
    Result := XmlFile.XMLText;
  finally
    FreeAndNil(XmlFile);
  end;
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
  Result := nil;
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
