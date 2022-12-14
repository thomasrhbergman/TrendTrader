unit Scanner.Types;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, VirtualTrees, IABFunctions,
  IABSocketAPI, System.Generics.Collections, BrokerHelperAbstr, Winapi.msxml, Vcl.Graphics, Entity.Sokid, Data.DB,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const, DebugWriter, HtmlLib, CustomForms, Document,
  System.DateUtils, Global.Types, System.Math, DaModule, Common.Types, ArrayHelper, AutoTrades.Types,
  IABFunctions.MessageCodes, Publishers, DaModule.Utils, IABFunctions.Helpers, FireDAC.Comp.Client,
  FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TKindAppend = (kaAddAll, kaUpdate, kaReduce, kaIntersection);
  TSourceType = (stFixed, stStaticList, stScannerMarket, stTickColumn, stCalcColumn, stEmbargoColumn);
  TEmbargoType = (etReleaseTime, etHoldTime, etRankingPosition, etRankingSum, etColumnValue, etColumnValueExists, etTimeInterval);
  TEmbargoTypeHelper = record helper for TEmbargoType
  private
    const EmbargoTypeName: array [TEmbargoType] of string = ('Release Time', 'Hold Time', 'Ranking Position', 'Ranking Sum', 'Column', 'Value Exists', 'Time Interval');
  public
    function ToString: string;
  end;

  TFilterOption = record
    Id         : string;
    RangeState : Integer;
    Variables  : string;
  end;
  TFiltersArray = TArray<TFilterOption>;

  TScannerMarket = record
    RecordId         : Integer;
    InstrumentIndex  : Integer;
    InstrumentName   : string;
    AutoRefresh      : Boolean;
    AvailableFilters : string;
    UseExtraFilters  : Boolean;
    MarketLocation   : TIABTagValueArray;
    FiltersArray     : TFiltersArray;
    procedure Clear;
  end;

  TScannerColumn = record
    Name               : string;
    KindAppend         : TKindAppend;
    ScanCriteria       : TIABScanCriteria;
    GlobalScanCriteria : TIABScanCriteria;
    ScanId             : Integer;
    ScannerMarket      : TScannerMarket;
    Step               : Double;
    Weight             : Double;
    MaxRows            : Integer;
    function IsEquals(AScanOptions: TScannerColumn): Boolean;
    function ToList: string;
    procedure Clear;
    procedure FromList(aValue: string);
  end;

  TStaticListColumn = record
    RecordId : Integer;
    Name     : string;
    Weight   : Double;
    function IsEquals(AStaticList: TStaticListColumn): Boolean;
    function ToList: string;
    procedure FromDB(aID: Integer);
    procedure FromList(aInfo: string);
    procedure Clear;
  end;

  TCalculationType = (ctGradientToday, ctGradientLogTerm, ctCorridorWidth);
  TCalculationTypeHelper = record helper for TCalculationType
  private const
    CalculationTypeName: array [TCalculationType] of string = ('Gradient today', 'Gradient log term', 'Corridor width');
  public
    function ToString: string;
  end;

  TCalculationColumn = record
    CalculationType  : TCalculationType;
    Duration         : Integer;
    function Caption: string;
    function IsEquals(ACalculationColumn: TCalculationColumn): Boolean;
    function ToList: string;
    procedure Clear;
    procedure FromList(aInfo: string);
  end;

  TTickColumn = record
    TypeOperation : TTypeOperation;
    Duration      : Integer;
    IBValue1      : TIABTickType;
    IBValue2      : TIABTickType;
    function Caption: string;
    function IsEquals(ATickColumn: TTickColumn): Boolean;
    function ToList: string;
    procedure Clear;
    procedure FromList(aInfo: string);
  end;

  PColumnsInfo = ^TColumnsInfo;
  PExtraColumns = ^TExtraColumns;

  TEmbargoColumn = record
  public
    EmbargoType    : TEmbargoType;
    InequalityType : TInequalityType;
    TimeStamp      : TTime;
    Value1         : Double;
    Value2         : Double;
    ColumnsInfo    : PColumnsInfo;
    RefRecordId    : Integer;
    WorkingDays    : Boolean;
    TimeInterval   : Integer;
    function Caption: string;
    function GetCriteriaColor(aColumnsInfo: PColumnsInfo; aExtraColumns: PExtraColumns): TColor;
    function GetText(aExtraColumns: PExtraColumns): string;
    function IsCriteria(aColumnsInfo: PColumnsInfo; aExtraColumns: PExtraColumns): Boolean;
    function IsEquals(aEmbargoColumn: TEmbargoColumn): Boolean;
    function ToList: string;
    procedure Clear;
    procedure FromList(aInfo: string);
  end;

  TColumnsInfo = record
  private
    FRecordId: Integer;
    function GetRecordId: Integer;
    procedure SetRecordId(const Value: Integer);
  public
    ColumnId          : Integer;
    SourceType        : TSourceType;
    Weight            : Double;
    ColumnPosition    : Integer;
    ColumnWidth       : Integer;
    KindAppend        : TKindAppend;
    TickColumn        : TTickColumn;
    CalcColumn        : TCalculationColumn;
    ScannerColumn     : TScannerColumn;
    StaticColumn      : TStaticListColumn;
    EmbargoColumn     : TEmbargoColumn;
    InitTimeStamp     : TTime;
    constructor Create(ASourceType: TSourceType);
    function Caption: string;
    function IsEquals(AColumnsInfo: TColumnsInfo): Boolean;
    function ToList: string;
    procedure AssignFrom(AColumnsInfo: TColumnsInfo);
    procedure Clear;
    procedure FromList(aInfo: string);
    property RecordId : Integer read GetRecordId write SetRecordId;
  end;

  TExtraColumns = record
  type
    TColumnsItem = record
      IsUnsufficientData : Boolean;
      Price              : Currency;
      Rank               : Double;
      OriginRank         : Double;
      Weight             : Double;
      ColTick            : TColor;
      TimeStamp          : TDateTime;
    end;
  public
    Items          : TDictionary<Integer, TColumnsItem>;
    Position       : Integer;
    RankingSum     : Double;
    RankingColTick : TColor;
    procedure Clear;
    constructor Create(aDummy: Integer);
    function GetText(aColumnsInfo: TColumnsInfo; aIsShowFeedValue: Boolean): string;
  end;

  PInstrumentData = ^TInstrumentData;
  TInstrumentData = record
    Node            : PVirtualNode;
    Id              : Integer;
    RecordId        : Integer;
    BrokerType      : TBrokerType;
    Currency        : string;
    Exchange        : string;
    PrimaryExchange : string;
    Name            : string;
    Group           : string;
    IsIn            : string;
    Value           : Double;
    Sector          : string;
    Symbol          : string;
    LocalSymbol     : string;
    ExtraColumns    : TExtraColumns;
    SecurityType    : TIABSecurityType;
    TradeChart      : TCustomForm;
    IsLocked        : Boolean;
    IsCriteria      : Boolean;
    Description     : string;
    Isolate         : Integer;
    Expiry          : TDateTime;
    Multiplier      : Double;
    InitTimeStamp   : TDateTime;
    TWSMessageItem  : TTWSMessageItem;
    procedure SaveInstrumentToSokidList;
    procedure Assign(aIABScan: TIABScanResultItem); overload;
    procedure Assign(aOrderDoc: TCustomOrderDoc); overload;
    procedure Assign(aFactorDoc: TFactorDoc); overload;
    procedure Clear;
  end;

  TScanTypeColumns = record
    ColId: Integer;
    Name: string;
    Display: Boolean;
    Section: string;
    DisplayType: string;
    ColumnType: string;
  end;

  PScanItem = ^TScanItem;
  TScanItem = record
    ScanId: Integer;
    Count: Integer;
    ScanCriteria: TIABScanCriteria;
    procedure Clear;
  end;

  TComboValue = record
    Code: string;
    DisplayName: string;
    Default: Boolean;
    SyntheticAll: Boolean;
  end;

  TAbstractField = record
    AbstractFieldType: string;
    VarName: string;
    Code: string;
    DisplayName: string;
    DontAllowClearAll: Boolean;
    Abbrev: Boolean;
    AcceptNegatives: Boolean;
    DontAllowNegative: Boolean;
    SkipNotEditableField: Boolean;
    Fraction: Boolean;
    Master: Boolean;
    NarrowField: Boolean;
    RadioButtons: Boolean;
    TypeAhead: Boolean;
    MaxValue: Integer;
    MinValue: Integer;
    Tooltip: string;
    ComboValues: TArray<TComboValue>;
  end;

  PFilterData = ^TFilterData;
  TFilterData = record
    Id: string;
    Category: string;
    Histogram: string;
    Access: string;
    TypeFilter: string; //[RangeFilter, SimpleFilter]
    SkipValidation: Boolean;
    Columns: TArray<TScanTypeColumns>;
    AbstractField: TArray<TAbstractField>;
    Checked: Boolean;
    DisplayName: string;
    procedure Clear;
    procedure Assign(aFilterData: TFilterData);
    function Equal(aFilterData: TFilterData): Boolean;
  end;

  TInstrumentInfo = class
  public
    Node: IXMLDOMNode;
    Name: string;
    InstrType: string;
    SecType: string;
    Filters: string;
    Group: string;
    ShortName: string;
    CloudScanNotSupported: Boolean;
    function ToString: string; override;
    constructor Create(aNode: IXMLDOMNode);
    destructor Destroy; override;
  end;

  PLocationData = ^TLocationData;
  TLocationData = record
    DisplayName: string;
    LocationCode: string;
    Instruments: string;
    RouteExchange: string;
    DelayedOnly: string;
    Access: string;
    procedure Clear;
    function ToString: string;
  end;

  PScanTypeData = ^TScanTypeData;
  TScanTypeData = record
    DisplayName: string;
    ScanCode: string;
    Instruments: string;
    AbsoluteColumns: Boolean;
    Columns: TArray<TScanTypeColumns>;
    procedure Clear;
  end;

  TAutoTradeColumns = TObjectDictionary<Integer, TColumnsInfo>;

  TArrayInstrumentData = TArray<TInstrumentData>;
  PArrayInstrumentData = ^TArrayInstrumentData;
  TArrayFilterData     = TArrayRecord<TFilterData>;
  PArrayFilterData     = ^TArrayFilterData;

  IScannerMarket = interface
    ['{73774C47-5AB5-40A5-AE87-EDDBDF0965F8}']
    function GetAutoTradeInfo: TAutoTradeInfo;
    function GetMainTree: TBaseVirtualTree;
    procedure AddInstrument(const aInstruments: PArrayInstrumentData; aColumnsInfo: TColumnsInfo);
    procedure DecMarketInstances;
    procedure IncMarketInstances;
    procedure SetScanOptions(const aColumnId: Integer; const aScanOptions: TScannerColumn);
  end;

  POrderTemplateData = ^TOrderTemplateData;
  TOrderTemplateData = record
    RecordId: Integer;
    OrderName: string;
    GroupID: Integer;
    OvertakeOrder: Boolean;
    TemplateName: string;
    procedure Clear;
  end;

const
  C_UNSUFFICIENT_SYMB = 'U';

implementation

{ TInstrumentData }

procedure TInstrumentData.Clear;
begin
  ExtraColumns.Clear;
  if Assigned(TradeChart) then
    FreeAndNil(TradeChart);
  Self := Default(TInstrumentData);
end;

procedure TInstrumentData.Assign(aIABScan: TIABScanResultItem);
var
  ColumnsItem: TExtraColumns.TColumnsItem;
begin
  Self.Node          := nil;
  Self.Id            := aIABScan.ContractId;
  Self.RecordId      := -1;
  Self.BrokerType    := TBrokerType.brIB;
  Self.Currency      := aIABScan.Currency;
  Self.Exchange      := aIABScan.Exchange;
  Self.Name          := aIABScan.Symbol;
  Self.SecurityType  := aIABScan.SecurityType;
  Self.LocalSymbol   := aIABScan.LocalSymbol;
  Self.Symbol        := aIABScan.Symbol;
  Self.SecurityType  := aIABScan.SecurityType;
  Self.IsCriteria    := False;
  Self.InitTimeStamp := Now;

  //  ExtraColumns.KindAppend := kaAddAll;
  Self.ExtraColumns.Items.Clear;

  ColumnsItem.Rank := aIABScan.Rank;
  ColumnsItem.Price := 0;
  ColumnsItem.Weight := 0;
  Self.ExtraColumns.Items.Add(-1, ColumnsItem);
  Self.ExtraColumns.RankingSum := 0;
  if SokidList.ContainsKey(aIABScan.ContractId) then
    Self.TWSMessageItem := SokidList.Items[aIABScan.ContractId].TWSMessageItem;
end;

procedure TInstrumentData.Assign(aOrderDoc: TCustomOrderDoc);
begin
  Self.Node            := nil;
  Self.Id              := aOrderDoc.Id;
  Self.RecordId        := -1;
  Self.BrokerType      := TBrokerType.brIB;
  Self.Currency        := aOrderDoc.Currency;
  Self.Exchange        := aOrderDoc.Exchange;
  Self.PrimaryExchange := aOrderDoc.PrimaryExchange;
  Self.Name            := aOrderDoc.InstrumentName;
  Self.IsCriteria      := False;
  Self.Symbol          := aOrderDoc.Symbol;
  Self.Expiry          := aOrderDoc.Expiry;
  Self.Multiplier      := aOrderDoc.Multiplier;
  Self.InitTimeStamp   := Now;
  if (aOrderDoc.BrokerType = TBrokerType.brIB) then
  begin
    Self.LocalSymbol  := TOrderIBDoc(aOrderDoc).LocalSymbol;
    Self.SecurityType := TOrderIBDoc(aOrderDoc).SecurityType;
  end;
  if SokidList.ContainsKey(aOrderDoc.Id) then
    Self.TWSMessageItem := SokidList.Items[aOrderDoc.Id].TWSMessageItem;
end;

procedure TInstrumentData.Assign(aFactorDoc: TFactorDoc);
var
  SokidInfo: TSokidInfo;
begin
  Self.Node            := nil;
  Self.Id              := aFactorDoc.ContractId;
  Self.RecordId        := -1;
  Self.BrokerType      := TBrokerType.brIB;
  Self.Currency        := aFactorDoc.Currency;
  Self.Exchange        := aFactorDoc.Exchange;
  Self.PrimaryExchange := aFactorDoc.PrimaryExchange;
  Self.Name            := aFactorDoc.Symbol;
  Self.IsCriteria      := False;
  Self.Symbol          := aFactorDoc.Symbol;
  Self.Expiry          := aFactorDoc.Expiry;
  Self.LocalSymbol     := aFactorDoc.LocalSymbol;
  Self.SecurityType    := stStock;
  Self.InitTimeStamp   := Now;

  if SokidList.ContainsKey(Self.Id) then
  begin
    SokidInfo := SokidList.Items[Self.Id];
    Self.Multiplier := SokidInfo.Multiplier.ToDouble;
    if (aFactorDoc.BrokerType = TBrokerType.brIB) then
      Self.SecurityType := SokidInfo.GetSecurityType;
  end;
  if SokidList.ContainsKey(aFactorDoc.ContractId) then
    Self.TWSMessageItem := SokidList.Items[aFactorDoc.ContractId].TWSMessageItem;
end;

procedure TInstrumentData.SaveInstrumentToSokidList;
var
  SokidInfo: TSokidInfo;
begin
  if SokidList.ContainsKey(Self.Id) then
    SokidInfo := SokidList.Items[Self.Id]
  else
    SokidInfo := TSokidInfo.Create(Id);
  SokidInfo.Name            := Self.Name;
  SokidInfo.Description     := Self.Description;
  SokidInfo.Symbol          := Self.Symbol;
  SokidInfo.LocalSymbol     := Self.LocalSymbol;
  SokidInfo.ContractId      := Self.Id;
  SokidInfo.Exchange        := Self.Exchange;
  SokidInfo.PrimaryExchange := Self.PrimaryExchange;
  SokidInfo.SecurityType    := Self.SecurityType.ToString;
  SokidInfo.Currency        := Self.Currency;
  SokidInfo.Multiplier      := Self.Multiplier.ToString;
  SokidInfo.Expiry          := Self.Expiry;
  SokidList.SetValue(SokidInfo);
end;

{ TColumnsInfo }

function TColumnsInfo.IsEquals(AColumnsInfo: TColumnsInfo): Boolean;
begin
  Result := (Self.SourceType = AColumnsInfo.SourceType) and (SimpleRoundTo(Self.Weight, -C_DECIMALS) = SimpleRoundTo(AColumnsInfo.Weight, -C_DECIMALS));
  if Result then
    case Self.SourceType of
      stStaticList:
        Result := Self.StaticColumn.IsEquals(AColumnsInfo.StaticColumn);
      stScannerMarket:
        Result := Self.ScannerColumn.IsEquals(AColumnsInfo.ScannerColumn);
      stTickColumn:
        Result := Self.TickColumn.IsEquals(AColumnsInfo.TickColumn);
      stCalcColumn:
        Result := Self.CalcColumn.IsEquals(AColumnsInfo.CalcColumn);
      stEmbargoColumn:
        Result := Self.EmbargoColumn.IsEquals(AColumnsInfo.EmbargoColumn);
    end;
end;

function TColumnsInfo.ToList: string;
begin
  case Self.SourceType of
    stStaticList:
      Result := Self.StaticColumn.ToList;
    stScannerMarket:
      Result := Self.ScannerColumn.ToList;
    stTickColumn:
      Result := Self.TickColumn.ToList;
    stEmbargoColumn:
      Result := Self.EmbargoColumn.ToList;
    stCalcColumn:
      Result := Self.CalcColumn.ToList;
  else
    Result := '';
  end;
end;

procedure TColumnsInfo.AssignFrom(AColumnsInfo: TColumnsInfo);
begin
  Self := Default(TColumnsInfo);
  Self.RecordId       := AColumnsInfo.RecordId;
  Self.SourceType     := AColumnsInfo.SourceType;
  Self.Weight         := AColumnsInfo.Weight;
  Self.ColumnPosition := AColumnsInfo.ColumnPosition;
  Self.ColumnWidth    := AColumnsInfo.ColumnWidth;
  Self.ColumnId       := AColumnsInfo.ColumnId;

  case AColumnsInfo.SourceType of
    stStaticList:
      Self.StaticColumn.FromList(AColumnsInfo.StaticColumn.ToList);
    stTickColumn:
      Self.TickColumn.FromList(AColumnsInfo.TickColumn.ToList);
    stScannerMarket:
      Self.ScannerColumn.FromList(AColumnsInfo.ScannerColumn.ToList);
    stCalcColumn:
      Self.CalcColumn.FromList(AColumnsInfo.CalcColumn.ToList);
  end;
end;

function TColumnsInfo.Caption: string;
begin
  case SourceType of
    stStaticList:
      Result := Self.StaticColumn.Name;
    stScannerMarket:
      Result := Self.ScannerColumn.Name;
    stTickColumn:
      Result := Self.TickColumn.Caption + Format(' (%f)', [Self.Weight]);
    stEmbargoColumn:
      Result := Self.EmbargoColumn.Caption;
    stCalcColumn:
      Result := Self.CalcColumn.Caption;
  end;
//  Result := ColumnId.ToString + ' ' + Result;
end;

procedure TColumnsInfo.Clear;
begin
  case SourceType of
    stStaticList:
      Self.StaticColumn.Clear;
    stScannerMarket:
      Self.ScannerColumn.Clear;
    stTickColumn:
      Self.TickColumn.Clear;
    stEmbargoColumn:
      Self.EmbargoColumn.Clear;
    stCalcColumn:
      Self.CalcColumn.Clear;
  end;
end;

constructor TColumnsInfo.Create(ASourceType: TSourceType);
begin
  Self := Default(TColumnsInfo);
  Self.SourceType := ASourceType;
  Self.InitTimeStamp := Now;
end;

procedure TColumnsInfo.FromList(aInfo: string);
begin
  case SourceType of
    stStaticList:
      Self.StaticColumn.FromList(aInfo);
    stScannerMarket:
      Self.ScannerColumn.FromList(aInfo);
    stTickColumn:
      Self.TickColumn.FromList(aInfo);
    stEmbargoColumn:
      Self.EmbargoColumn.FromList(aInfo);
    stCalcColumn:
      Self.CalcColumn.FromList(aInfo);
  end;
end;

procedure TColumnsInfo.SetRecordId(const Value: Integer);
begin
  if (Value > 0) then
    FRecordId := Value;
end;

function TColumnsInfo.GetRecordId: Integer;
begin
  if (FRecordId = 0) then
    FRecordId := General.GetNextRecordID;
  Result := FRecordId;
end;

{ TFilterData }

procedure TFilterData.Assign(aFilterData: TFilterData);
var
  i: Integer;
begin
  Id             := aFilterData.Id;
  Category       := aFilterData.Category;
  Histogram      := aFilterData.Histogram;
  Access         := aFilterData.Access;
  TypeFilter     := aFilterData.TypeFilter;
  SkipValidation := aFilterData.SkipValidation;
  Checked        := aFilterData.Checked;

  SetLength(Columns, Length(aFilterData.Columns));
  for i := Low(aFilterData.Columns) to High(aFilterData.Columns) do
  begin
    Columns[i].ColId       := aFilterData.Columns[i].ColId;
    Columns[i].Name        := aFilterData.Columns[i].Name;
    Columns[i].Display     := aFilterData.Columns[i].Display;
    Columns[i].Section     := aFilterData.Columns[i].Section;
    Columns[i].DisplayType := aFilterData.Columns[i].DisplayType;
    Columns[i].ColumnType  := aFilterData.Columns[i].ColumnType;
  end;

  SetLength(AbstractField, Length(aFilterData.AbstractField));
  for i := Low(aFilterData.AbstractField) to High(aFilterData.AbstractField) do
  begin
    AbstractField[i].AbstractFieldType    := aFilterData.AbstractField[i].AbstractFieldType;
    AbstractField[i].VarName              := aFilterData.AbstractField[i].VarName;
    AbstractField[i].Code                 := aFilterData.AbstractField[i].Code;
    AbstractField[i].DisplayName          := aFilterData.AbstractField[i].DisplayName;
    AbstractField[i].DontAllowClearAll    := aFilterData.AbstractField[i].DontAllowClearAll;
    AbstractField[i].Abbrev               := aFilterData.AbstractField[i].Abbrev;
    AbstractField[i].AcceptNegatives      := aFilterData.AbstractField[i].AcceptNegatives;
    AbstractField[i].DontAllowNegative    := aFilterData.AbstractField[i].DontAllowNegative;
    AbstractField[i].SkipNotEditableField := aFilterData.AbstractField[i].SkipNotEditableField;
    AbstractField[i].Fraction             := aFilterData.AbstractField[i].Fraction;
    AbstractField[i].Master               := aFilterData.AbstractField[i].Master;
    AbstractField[i].NarrowField          := aFilterData.AbstractField[i].NarrowField;
    AbstractField[i].RadioButtons         := aFilterData.AbstractField[i].RadioButtons;
    AbstractField[i].TypeAhead            := aFilterData.AbstractField[i].TypeAhead;
    AbstractField[i].MaxValue             := aFilterData.AbstractField[i].MaxValue;
    AbstractField[i].MinValue             := aFilterData.AbstractField[i].MinValue;
    AbstractField[i].Tooltip              := aFilterData.AbstractField[i].Tooltip;
  end;

end;

procedure TFilterData.Clear;
begin
  Id := '';
  Category := '';
  Histogram := '';
  Access := '';
  TypeFilter := '';
  SetLength(Columns, 0);
  SetLength(AbstractField, 0);
end;

function TFilterData.Equal(aFilterData: TFilterData): Boolean;
begin
  Result := Self.Id.Equals(aFilterData.Id) and
            Self.Category.Equals(aFilterData.Category) and
            Self.Histogram.Equals(aFilterData.Histogram);
end;

{ TScanTypeData }

procedure TScanTypeData.Clear;
begin
  DisplayName := '';
  ScanCode := '';
  Instruments := '';
  SetLength(Columns, 0);
end;

{ TInstrumentInfo }

constructor TInstrumentInfo.Create(aNode: IXMLDOMNode);
begin
  Node := aNode;
end;

destructor TInstrumentInfo.Destroy;
begin
  Node := nil;
  inherited;
end;

function TInstrumentInfo.ToString: string;
var
  sb: TStringBuilder;
begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('Name=%s ', [Name]).AppendLine
      .AppendFormat('InstrType=%s ', [InstrType]).AppendLine
      .AppendFormat('SecType=%s ', [SecType]).AppendLine
      .AppendFormat('Filters=%s ', [Filters]).AppendLine
      .AppendFormat('Group=%s ', [Group]).AppendLine
      .AppendFormat('ShortName=%s ', [ShortName]).AppendLine
      .AppendFormat('CloudScanNotSupported=%s', [BoolToStr(CloudScanNotSupported, True)]);
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

{ TLocationData }

procedure TLocationData.Clear;
begin
  DisplayName := '';
  LocationCode := '';
  Instruments := '';
  RouteExchange := '';
  DelayedOnly := '';
  Access := '';
end;

function TLocationData.ToString: string;
var
  sb: TStringBuilder;
begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('DisplayName=%s ', [DisplayName]).AppendLine
      .AppendFormat('LocationCode=%s ', [LocationCode]).AppendLine
      .AppendFormat('Instruments=%s ', [Instruments]).AppendLine
      .AppendFormat('RouteExchange=%s ', [RouteExchange]).AppendLine
      .AppendFormat('DelayedOnly=%s ', [DelayedOnly]).AppendLine
      .AppendFormat('Access=%s', [Access]);
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

{ TScannerColumn }

function TScannerColumn.IsEquals(AScanOptions: TScannerColumn): Boolean;
begin
  Result := (Self.ToList = AScanOptions.ToList);
end;

function TScannerColumn.ToList: string;
var
  sb: TStringBuilder;
  i: Integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('Step=%f', [Step]).AppendLine
      .AppendFormat('Weight=%f', [Weight]).AppendLine
      .AppendFormat('MaxRows=%d', [MaxRows]).AppendLine
      .AppendFormat('ScanId=%d', [ScanId]).AppendLine
      .AppendFormat('Name=%s', [Name]).AppendLine;
//      .AppendFormat('RecordId=%d', [RecordId]).AppendLine;

    for i := 0 to IABClient.Scanner.Count - 1 do
      if (IABClient.Scanner.Items[i].ScanId = ScanId) then
      begin
        ScanCriteria := IABClient.Scanner.Items[i].QueryCriteria;
        Break;
      end;

    sb.AppendFormat('ScanCriteria.NumberOfRows=%d', [ScanCriteria.NumberOfRows]).AppendLine
      .AppendFormat('ScanCriteria.Instrument=%s', [ScanCriteria.Instrument]).AppendLine
      .AppendFormat('ScanCriteria.LocationCode=%s', [ScanCriteria.LocationCode]).AppendLine
      .AppendFormat('ScanCriteria.ScanCode=%s', [ScanCriteria.ScanCode]).AppendLine
      .AppendFormat('ScanCriteria.AbovePrice=%f', [ScanCriteria.AbovePrice]).AppendLine
      .AppendFormat('ScanCriteria.BelowPrice=%f', [ScanCriteria.BelowPrice]).AppendLine
      .AppendFormat('ScanCriteria.AboveVolume=%d', [ScanCriteria.AboveVolume]).AppendLine
      .AppendFormat('ScanCriteria.MarketCapAbove=%f', [ScanCriteria.MarketCapAbove]).AppendLine
      .AppendFormat('ScanCriteria.MarketCapBelow=%f', [ScanCriteria.MarketCapBelow]).AppendLine
      .AppendFormat('ScanCriteria.MoodyRatingAbove=%s', [ScanCriteria.MoodyRatingAbove]).AppendLine
      .AppendFormat('ScanCriteria.MoodyRatingBelow=%s', [ScanCriteria.MoodyRatingBelow]).AppendLine
      .AppendFormat('ScanCriteria.SPRatingAbove=%s', [ScanCriteria.SPRatingAbove]).AppendLine
      .AppendFormat('ScanCriteria.SPRatingBelow=%s', [ScanCriteria.SPRatingBelow]).AppendLine
      .AppendFormat('ScanCriteria.MaturityDateAbove=%s', [ScanCriteria.MaturityDateAbove]).AppendLine
      .AppendFormat('ScanCriteria.MaturityDateBelow=%s', [ScanCriteria.MaturityDateBelow]).AppendLine
      .AppendFormat('ScanCriteria.CouponRateAbove=%f', [ScanCriteria.CouponRateAbove]).AppendLine
      .AppendFormat('ScanCriteria.CouponRateBelow=%f', [ScanCriteria.CouponRateBelow]).AppendLine
      .AppendFormat('ScanCriteria.ExcludeConvertible=%d', [ScanCriteria.ExcludeConvertible]).AppendLine
      .AppendFormat('ScanCriteria.AverageOptionVolumeAbove=%d', [ScanCriteria.AverageOptionVolumeAbove]).AppendLine
      .AppendFormat('ScanCriteria.ScannerSettingPairs=%s', [ScanCriteria.ScannerSettingPairs]).AppendLine
      .AppendFormat('ScanCriteria.StockTypeFilter=%s', [ScanCriteria.StockTypeFilter]).AppendLine;

    sb.AppendFormat('ScannerMarket.RecordId=%d', [ScannerMarket.RecordId]).AppendLine
      .AppendFormat('ScannerMarket.InstrumentIndex=%d', [ScannerMarket.InstrumentIndex]).AppendLine
      .AppendFormat('ScannerMarket.InstrumentName=%s', [ScannerMarket.InstrumentName]).AppendLine
      .AppendFormat('ScannerMarket.AutoRefresh=%s', [BoolToStr(ScannerMarket.AutoRefresh, True)]).AppendLine
      .AppendFormat('ScannerMarket.AvailableFilters=%s', [ScannerMarket.AvailableFilters]).AppendLine
      .AppendFormat('ScannerMarket.UseExtraFilters=%s', [BoolToStr(ScannerMarket.UseExtraFilters, True)]).AppendLine;

    for i := 0 to High(ScanCriteria.SubscriptionOptions) do
      sb.AppendFormat('SubscriptionOptions.%s=%s', [ScanCriteria.SubscriptionOptions[i].Tag,
                                                    ScanCriteria.SubscriptionOptions[i].Value]).AppendLine;

    for i := 0 to High(ScanCriteria.FilterOptions) do
      sb.AppendFormat('FilterOptions.%s=%s', [ScanCriteria.FilterOptions[i].Tag,
                                              ScanCriteria.FilterOptions[i].Value]).AppendLine;

    for i := 0 to High(ScannerMarket.FiltersArray) do
      if (not ScannerMarket.FiltersArray[i].Id.IsEmpty) then
        sb.AppendFormat('FiltersArray Id:%s, RangeState:%d, Variables:%s', [ScannerMarket.FiltersArray[i].Id,
                                                                            ScannerMarket.FiltersArray[i].RangeState,
                                                                            ScannerMarket.FiltersArray[i].Variables]).AppendLine;

    for i := 0 to High(ScannerMarket.MarketLocation) do
      if (StrToIntDef(ScannerMarket.MarketLocation[i].Value, 0) = 2) then
        sb.AppendFormat('MarketLocation.%s=%s', [ScannerMarket.MarketLocation[i].Tag,
                                                 ScannerMarket.MarketLocation[i].Value]).AppendLine;

    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

procedure TScannerColumn.Clear;
begin
  Self.ScannerMarket.Clear;
  Self := Default(TScannerColumn);
  Weight  := 1;
  Step    := 1;
  MaxRows := 10;
end;

procedure TScannerColumn.FromList(aValue: string);
var
  StList: TStringList;
  i: Integer;
  Pair: TPair<string, string>;
begin
  if not aValue.IsEmpty then
  begin
    StList := TStringList.Create;
    try
      StList.Text := aValue;
      Step     := StrToFloatDef(StList.Values['Step'], 0);
      MaxRows  := StrToIntDef(StList.Values['MaxRows'], 0);
      Weight   := StrToFloatDef(StList.Values['Weight'], 0);
      ScanId   := StrToIntDef(StList.Values['ScanId'], 0);
      Name     := StList.Values['Name'];

      IABClient.Scanner.InitializeScanCriteria(@ScanCriteria);
      ScanCriteria.NumberOfRows             := StrToIntDef(StList.Values['ScanCriteria.NumberOfRows'], 10);
      ScanCriteria.Instrument               := StList.Values['ScanCriteria.Instrument'];
      ScanCriteria.LocationCode             := StList.Values['ScanCriteria.LocationCode'];
      ScanCriteria.ScanCode                 := StList.Values['ScanCriteria.ScanCode'];
      ScanCriteria.AbovePrice               := StrToFloatDef(StList.Values['ScanCriteria.AbovePrice'], UNSET_DOUBLE);
      ScanCriteria.BelowPrice               := StrToFloatDef(StList.Values['ScanCriteria.BelowPrice'], UNSET_DOUBLE);
      ScanCriteria.AboveVolume              := StrToIntDef(StList.Values['ScanCriteria.AboveVolume'], UNSET_INTEGER);
      ScanCriteria.MarketCapAbove           := StrToFloatDef(StList.Values['ScanCriteria.MarketCapAbove'], UNSET_DOUBLE);
      ScanCriteria.MarketCapBelow           := StrToFloatDef(StList.Values['ScanCriteria.MarketCapBelow'], UNSET_DOUBLE);
      ScanCriteria.MoodyRatingAbove         := StList.Values['ScanCriteria.MoodyRatingAbove'];
      ScanCriteria.MoodyRatingBelow         := StList.Values['ScanCriteria.MoodyRatingBelow'];
      ScanCriteria.SPRatingAbove            := StList.Values['ScanCriteria.SPRatingAbove'];
      ScanCriteria.SPRatingBelow            := StList.Values['ScanCriteria.SPRatingBelow'];
      ScanCriteria.MaturityDateAbove        := StList.Values['ScanCriteria.MaturityDateAbove'];
      ScanCriteria.MaturityDateBelow        := StList.Values['ScanCriteria.MaturityDateBelow'];
      ScanCriteria.CouponRateAbove          := StrToFloatDef(StList.Values['ScanCriteria.CouponRateAbove'], UNSET_DOUBLE);
      ScanCriteria.CouponRateBelow          := StrToFloatDef(StList.Values['ScanCriteria.CouponRateBelow'], UNSET_DOUBLE);
      ScanCriteria.ExcludeConvertible       := StrToIntDef(StList.Values['ScanCriteria.ExcludeConvertible'], 0);
      ScanCriteria.AverageOptionVolumeAbove := StrToIntDef(StList.Values['ScanCriteria.AverageOptionVolumeAbove'], 0);
      ScanCriteria.ScannerSettingPairs      := StList.Values['ScanCriteria.ScannerSettingPairs'];
      ScanCriteria.StockTypeFilter          := StList.Values['ScanCriteria.StockTypeFilter'];

      ScannerMarket.RecordId         := StrToIntDef(StList.Values['ScannerMarket.RecordId'], -1);
      ScannerMarket.InstrumentIndex  := StrToIntDef(StList.Values['ScannerMarket.InstrumentIndex'], -1);
      ScannerMarket.InstrumentName   := StList.Values['ScannerMarket.InstrumentName'];
      ScannerMarket.AutoRefresh      := StrToBoolDef(StList.Values['ScannerMarket.AutoRefresh'], False);
      ScannerMarket.AvailableFilters := StList.Values['ScannerMarket.AvailableFilters'];
      ScannerMarket.UseExtraFilters  := StrToBoolDef(StList.Values['ScannerMarket.UseExtraFilters'], False);

      for i := 0 to StList.Count - 1 do
      begin
        if StList.Names[i].StartsWith('SubscriptionOptions.') then
        begin
          SetLength(ScanCriteria.SubscriptionOptions, Length(ScanCriteria.SubscriptionOptions) + 1);
          ScanCriteria.SubscriptionOptions[High(ScanCriteria.SubscriptionOptions)].Tag   := StList.Names[i].Substring('SubscriptionOptions.'.Length);
          ScanCriteria.SubscriptionOptions[High(ScanCriteria.SubscriptionOptions)].Value := StList.ValueFromIndex[i];
        end
        else if StList.Names[i].StartsWith('FilterOptions.') then
        begin
          SetLength(ScanCriteria.FilterOptions, Length(ScanCriteria.FilterOptions) + 1);
          ScanCriteria.FilterOptions[High(ScanCriteria.FilterOptions)].Tag   := StList.Names[i].Substring('FilterOptions.'.Length);
          ScanCriteria.FilterOptions[High(ScanCriteria.FilterOptions)].Value := StList.ValueFromIndex[i];
        end
        else if StList.Names[i].StartsWith('MarketLocation.') then
        begin
          SetLength(ScannerMarket.MarketLocation, Length(ScannerMarket.MarketLocation) + 1);
          ScannerMarket.MarketLocation[High(ScannerMarket.MarketLocation)].Tag   := StList.Names[i].Substring('MarketLocation.'.Length);
          ScannerMarket.MarketLocation[High(ScannerMarket.MarketLocation)].Value := StList.ValueFromIndex[i];
        end
        else if StList[i].StartsWith('FiltersArray') then
        begin
          var arr := StList[i].Substring('FiltersArray '.Length).Split([',']);
          SetLength(ScannerMarket.FiltersArray, Length(ScannerMarket.FiltersArray) + 1);
          for var str in arr do
          begin
            Pair.Key := str.Split([':'])[0].Trim;
            Pair.Value := str.Split([':'])[1].Trim;
            if Pair.Key.Equals('Id') then
              ScannerMarket.FiltersArray[High(ScannerMarket.FiltersArray)].Id := Pair.Value
            else if Pair.Key.Equals('RangeState') then
              ScannerMarket.FiltersArray[High(ScannerMarket.FiltersArray)].RangeState := StrToIntDef(Pair.Value, 0)
            else if Pair.Key.Equals('Variables') then
              ScannerMarket.FiltersArray[High(ScannerMarket.FiltersArray)].Variables := Pair.Value;
          end;
        end
      end;
    finally
      FreeAndNil(StList);
    end;
  end;
end;

{ TExtraColumns }

procedure TExtraColumns.Clear;
begin
  if Assigned(Items) then
  begin
    Items.Clear;
    FreeAndNil(Items);
  end;
end;

constructor TExtraColumns.Create(aDummy: Integer);
begin
  Items := TDictionary<Integer, TColumnsItem>.Create;
end;

function TExtraColumns.GetText(aColumnsInfo: TColumnsInfo; aIsShowFeedValue: Boolean): string;
var
  ColumnsItem: TColumnsItem;
begin
  Result := '';
  if Items.ContainsKey(aColumnsInfo.ColumnId) then
  begin
    ColumnsItem := Items[aColumnsInfo.ColumnId];
    case aColumnsInfo.SourceType of
      stTickColumn:
        if aIsShowFeedValue then
        begin
          if (aColumnsInfo.TickColumn.IBValue2 <> ttNotSet) then
            Result := FormatFloat('0.0000', ColumnsItem.Price)
          else
            Result := FormatFloat(C_CURRENCY_FORMAT, ColumnsItem.Price);
        end
        else
        begin
          if (aColumnsInfo.TickColumn.IBValue2 <> ttNotSet) then
            Result := FormatFloat('0.0000', ColumnsItem.Price * aColumnsInfo.Weight)
          else
            Result := FormatFloat(C_CURRENCY_FORMAT, ColumnsItem.Price * aColumnsInfo.Weight);
        end;
      stCalcColumn:
        if aIsShowFeedValue then
        begin
          if ColumnsItem.IsUnsufficientData then
            Result := C_UNSUFFICIENT_SYMB
          else
            Result := FormatFloat('0.0000', ColumnsItem.Price);
        end
        else
        begin
          if ColumnsItem.IsUnsufficientData then
            Result := C_UNSUFFICIENT_SYMB
          else
            Result := FormatFloat('0.0000', ColumnsItem.Price * aColumnsInfo.Weight)
        end;
      stScannerMarket:
        if (ColumnsItem.Weight = 0) then
        begin
          if (ColumnsItem.OriginRank - MinDouble < 0.001) then
            Result := '0'
          else
            Result := ColumnsItem.OriginRank.ToString;
        end
        else
          Result := FormatFloat(C_CURRENCY_FORMAT, ColumnsItem.Rank);
      stStaticList:
        Result := FormatFloat(C_CURRENCY_FORMAT, ColumnsItem.Rank * aColumnsInfo.Weight);
      stEmbargoColumn:
        Result := aColumnsInfo.EmbargoColumn.GetText(@Self);
    else
      Result := '0.00';
    end;
  end;
end;

{ TTickColumn }

procedure TTickColumn.Clear;
begin
  Self := Default(TTickColumn);
end;

procedure TTickColumn.FromList(aInfo: string);
var
  StInfo: TStringList;
begin
  if not aInfo.IsEmpty then
  begin
    StInfo := TStringList.Create;
    try
      StInfo.Text   := aInfo;
      TypeOperation := TTypeOperation(StrToIntDef(StInfo.Values['TypeOperation'], 0));
      Duration      := StrToIntDef(StInfo.Values['Duration'], 0);
      IBValue1      := TIABTickType(StrToIntDef(StInfo.Values['IBValue'], 0));
      IBValue2      := TIABTickType(StrToIntDef(StInfo.Values['IBValue2'], 0));
    finally
      FreeAndNil(StInfo);
    end;
  end;
end;

function TTickColumn.IsEquals(ATickColumn: TTickColumn): Boolean;
begin
  Result := (ATickColumn.TypeOperation = Self.TypeOperation) and
            (ATickColumn.Duration = Self.Duration) and
            (ATickColumn.IBValue1 = Self.IBValue1) and
            (ATickColumn.IBValue2 = Self.IBValue2);
end;

function TTickColumn.ToList: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('TypeOperation=%d', [Integer(TypeOperation)]).AppendLine
      .AppendFormat('Duration=%d', [Duration]).AppendLine
      .AppendFormat('IBValue=%d', [Integer(IBValue1)]).AppendLine
      .AppendFormat('IBValue2=%d', [Integer(IBValue2)]).AppendLine;
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

function TTickColumn.Caption: string;
begin
  if (IBValue2 <> ttNotSet) then
    Result := Concat(IBValue1.ToString, ' ', TypeOperation.ToString, ' ', IBValue2.ToString)
  else
    Result := IBValue1.ToString;
end;

{ TStaticListColumn }

procedure TStaticListColumn.Clear;
begin
  Name := '';
end;

procedure TStaticListColumn.FromDB(aID: Integer);
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT NAME, WEIGHT, VISIBLE FROM STATICLISTS WHERE ID=:ID';
var
  Query: TFDQuery;
begin
  Self := Default(TStaticListColumn);
  Self.RecordId := aID;
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_SELECT_TEXT;
    Query.ParamByName('ID').AsInteger := aId;
    try
      Query.Prepare;
      Query.Open;
      if not Query.IsEmpty then
      begin
        Self.Name   := 'List nr ' + aId.ToString + ' ' + Query.FieldByName('NAME').AsString;
        Self.Weight := SimpleRoundTo(Query.FieldByName('WEIGHT').AsFloat, -2);
      end;
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'LoadStaticListColumn', E.Message + TDModUtils.GetQueryInfo(Query));
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TStaticListColumn.FromList(aInfo: string);
var
  StInfo: TStringList;
begin
  if not aInfo.IsEmpty then
  begin
    StInfo := TStringList.Create;
    try
      StInfo.Text   := aInfo;
      Self.RecordId := StrToIntDef(StInfo.Values['Id'], 0);
    finally
      FreeAndNil(StInfo);
    end;
  end;
end;

function TStaticListColumn.IsEquals(AStaticList: TStaticListColumn): Boolean;
begin
  Result := Self.RecordId = AStaticList.RecordId;
end;

function TStaticListColumn.ToList: string;
begin
  Result := 'Id=' + RecordId.ToString;
end;

{ TScanItem }

procedure TScanItem.Clear;
begin
  Self := Default(TScanItem);
end;

{ TEmbargoColumn }

function TEmbargoColumn.IsEquals(aEmbargoColumn: TEmbargoColumn): Boolean;
begin
  Result := (Self.EmbargoType = aEmbargoColumn.EmbargoType) and
            (Self.InequalityType = aEmbargoColumn.InequalityType);
  case Self.EmbargoType of
    etReleaseTime, etHoldTime:
      Result := Result and (Self.WorkingDays = aEmbargoColumn.WorkingDays) and (Self.TimeStamp = aEmbargoColumn.TimeStamp);
    etRankingPosition, etRankingSum:
      Result := Result and (Self.Value1 = aEmbargoColumn.Value1) and
                           (Self.Value2 = aEmbargoColumn.Value2);
    etColumnValue, etColumnValueExists, etTimeInterval:
      Result := Result and (Self.RefRecordId = aEmbargoColumn.RefRecordId);
  end;
end;

function TEmbargoColumn.ToList: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('EmbargoType=%d', [Ord(EmbargoType)]).AppendLine
      .AppendFormat('TimeStamp=%.10f', [TimeStamp]).AppendLine
      .AppendFormat('InequalityType=%d', [Ord(InequalityType)]).AppendLine
      .AppendFormat('Value1=%.10f', [Value1]).AppendLine
      .AppendFormat('Value2=%.10f', [Value2]).AppendLine
      .AppendFormat('WorkingDays=%s', [BoolToStr(WorkingDays)]).AppendLine
      .AppendFormat('TimeInterval=%d', [TimeInterval]).AppendLine;
    if Assigned(ColumnsInfo) then
      sb.AppendFormat('RefRecordId=%d', [ColumnsInfo^.RecordId]).AppendLine
    else
      sb.Append('RefRecordId=-1').AppendLine;
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

procedure TEmbargoColumn.Clear;
begin
  if Assigned(ColumnsInfo) then
    Dispose(ColumnsInfo);
end;

procedure TEmbargoColumn.FromList(aInfo: string);
var
  StList: TStringList;
begin
  Clear;
  if not aInfo.IsEmpty then
  begin
    StList := TStringList.Create;
    try
      StList.Text := aInfo;
      EmbargoType    := TEmbargoType(StrToInt(StList.Values['EmbargoType']));
      InequalityType := TInequalityType(StrToInt(StList.Values['InequalityType']));
      TimeStamp      := StrToFloatDef(StList.Values['TimeStamp'], 0);
      Value1         := StrToFloatDef(StList.Values['Value1'], 0);
      Value2         := StrToFloatDef(StList.Values['Value2'], 0);
      RefRecordId    := StrToIntDef(StList.Values['RefRecordId'], -1);
      WorkingDays    := StrToBoolDef(StList.Values['WorkingDays'], True);
      TimeInterval   := StrToIntDef(StList.Values['TimeInterval'], 0);
    finally
      FreeAndNil(StList);
    end;
  end;
end;

function TEmbargoColumn.GetCriteriaColor(aColumnsInfo: PColumnsInfo; aExtraColumns: PExtraColumns): TColor;
begin
  if IsCriteria(aColumnsInfo, aExtraColumns) then
    Result := clWebTeal
  else
    Result := clWebCrimson;
end;

function TEmbargoColumn.Caption: string;
begin
  case EmbargoType of
    etReleaseTime, etHoldTime:
    begin
      Result := EmbargoType.ToString;
      if WorkingDays then
        Result := Result + ' Wrk';
    end;
    etRankingPosition, etRankingSum:
      Result := EmbargoType.ToString + ' (' + InequalityType.ToString + ')';
    etColumnValue, etColumnValueExists:
      begin
        Result := EmbargoType.ToString;
        if Assigned(Self.ColumnsInfo) then
          Result := Result + ' [' + Self.ColumnsInfo^.Caption + ']';
      end;
    etTimeInterval:
      begin
        Result := EmbargoType.ToString;
        if Assigned(Self.ColumnsInfo) then
          Result := Result + ' [' + Self.ColumnsInfo^.Caption + '] (' + TimeInterval.ToString + ')';
      end;
  end;
end;

function TEmbargoColumn.GetText(aExtraColumns: PExtraColumns): string;
var
  ColumnsItem: TExtraColumns.TColumnsItem;
begin
  case EmbargoType of
    etReleaseTime, etHoldTime:
      Result := TimeToStr(TimeStamp);
    etRankingPosition:
      case InequalityType of
        iqAbove, iqAboveOrEqual:
          Result := FormatFloat('0', Value1);
        iqBelow, iqBelowOrEqual:
          Result := FormatFloat('0', Value1);
        iqBetween:
          Result := FormatFloat('0', Value1) + '..' + FormatFloat('0', Value2);
        iqEqual:
          Result := FormatFloat('0', Value1);
      end;
    etRankingSum:
      case InequalityType of
        iqAbove, iqAboveOrEqual:
          Result := FormatFloat(C_CURRENCY_FORMAT, Value1);
        iqBelow, iqBelowOrEqual:
          Result := FormatFloat(C_CURRENCY_FORMAT, Value1);
        iqBetween:
          Result := FormatFloat(C_CURRENCY_FORMAT, Value1) + '..' + FormatFloat(C_CURRENCY_FORMAT, Value2);
        iqEqual:
          Result := FormatFloat(C_CURRENCY_FORMAT, Value1);
      end;
    etColumnValue, etColumnValueExists:
    begin
      if Assigned(ColumnsInfo) then
        Result := aExtraColumns^.GetText(ColumnsInfo^, True);
      if Result = C_UNSUFFICIENT_SYMB then
        Result := '0';
    end;
    etTimeInterval:
      if Assigned(ColumnsInfo) and aExtraColumns^.Items.ContainsKey(ColumnsInfo.ColumnId) then
      begin
        ColumnsItem := aExtraColumns^.Items[ColumnsInfo.ColumnId];
//        if (ColumnsItem.TimeStamp > 0) then
//          Result := ColumnsInfo.ColumnId.ToString + FormatDateTime(' hh:nn:ss.zzz', ColumnsItem.TimeStamp)
          Result := aExtraColumns^.GetText(ColumnsInfo^, True)
//          Result := MilliSecondOf(ColumnsItem.TimeStamp - ColumnsInfo.InitTimeStamp).ToString
//        else
//          Result := '';
      end
      else
        Result := '';
  end;
end;

function TEmbargoColumn.IsCriteria(aColumnsInfo: PColumnsInfo; aExtraColumns: PExtraColumns): Boolean;
var
  ColumnsItem: TExtraColumns.TColumnsItem;
  Weight: Double;
begin
  Result := False;
  case EmbargoType of
    etReleaseTime:
      if WorkingDays then
        Result := (DayOfWeek(Date) in [2 .. 6]) and (TimeOf(TimeStamp) < TimeOf(Now))
      else
        Result := TimeOf(TimeStamp) < TimeOf(Now);
    etHoldTime:
      if WorkingDays then
        Result := (DayOfWeek(Date) in [2 .. 6]) and (TimeOf(TimeStamp) > TimeOf(Now))
      else
        Result := TimeOf(TimeStamp) > TimeOf(Now);
    etRankingPosition:
      case InequalityType of
        iqBetween:
          Result := (Value1 <= aExtraColumns^.Position) and (Value2 >= aExtraColumns^.Position);
      else
        Result := InequalityType.IsCondition(Value1, aExtraColumns^.Position)
      end;
    etRankingSum:
      case InequalityType of
        iqBetween:
          Result := (Value1 <= aExtraColumns^.RankingSum) and (Value2 >= aExtraColumns^.RankingSum);
      else
        Result := InequalityType.IsCondition(Value1, aExtraColumns^.RankingSum)
      end;
    etColumnValue:
      if Assigned(ColumnsInfo) and aExtraColumns^.Items.ContainsKey(ColumnsInfo.ColumnId) then
      begin
        ColumnsItem := aExtraColumns^.Items[ColumnsInfo.ColumnId];
        var Value := ColumnsItem.Price;
        Weight := IfThen((ColumnsItem.Weight > 0), ColumnsItem.Weight, ColumnsInfo.Weight);
        case ColumnsInfo.SourceType of
          stStaticList:
            Value := ColumnsItem.Rank * Weight;
          stScannerMarket:
            Value := ColumnsItem.Rank;
          stTickColumn:
//            if (ColumnsInfo.TickColumn.IBValue2 <> ttNotSet) then
//              Value := ColumnsItem.Price * Weight * 100
//            else
              Value := ColumnsItem.Price * Weight;
        end;

        case InequalityType of
          iqBetween:
            Result := (Value1 <= Value) and (Value2 >= Value);
        else
          Result := InequalityType.IsCondition(Value, Value1)
        end;

        if (ColumnsInfo.SourceType = stCalcColumn) and ColumnsItem.IsUnsufficientData then
          Result := False;
      end;
    etColumnValueExists:
      if Assigned(ColumnsInfo) and aExtraColumns^.Items.ContainsKey(ColumnsInfo.ColumnId) then
      begin
        ColumnsItem := aExtraColumns^.Items[ColumnsInfo.ColumnId];
        var
        Value := aExtraColumns^.Items[ColumnsInfo.ColumnId].Price;
        Weight := IfThen((ColumnsItem.Weight > 0), ColumnsItem.Weight, ColumnsInfo.Weight);
        case ColumnsInfo.SourceType of
          stStaticList:
            Value := ColumnsItem.Rank * Weight;
          stScannerMarket:
            Value := ColumnsItem.Rank;
          stTickColumn:
            if (ColumnsInfo.TickColumn.IBValue2 <> ttNotSet) then
              Value := ColumnsItem.Price * Weight * 100
            else
              Value := ColumnsItem.Price * Weight;
        end;
        Result := (ColumnsItem.ColTick <> clBlack) and (Value <> 0);
      end;
    etTimeInterval:
      if Assigned(ColumnsInfo) and aExtraColumns^.Items.ContainsKey(ColumnsInfo.ColumnId) then
      begin
        ColumnsItem := aExtraColumns^.Items[ColumnsInfo.ColumnId];
        Result := { Self.TimeInterval } (ColumnsItem.TimeStamp > 0) and (ColumnsItem.Price <> 0);
      end;
  end;
end;

{ TOrderTemplateData }

procedure TOrderTemplateData.Clear;
begin
  OrderName := '';
  OvertakeOrder := False;
  TemplateName := '';
end;

{ TScannerMarket }

procedure TScannerMarket.Clear;
begin
  Self := Default(TScannerMarket);
end;

{ TCalculationTypeHelper }

function TCalculationTypeHelper.ToString: string;
begin
  Result := CalculationTypeName[Self];
end;

{ TEmbargoTypeHelper }

function TEmbargoTypeHelper.ToString: string;
begin
  Result := EmbargoTypeName[Self];
end;

{ TCalculationColumn }

function TCalculationColumn.Caption: string;
begin
  case CalculationType of
    ctGradientToday:
      Result := ctGradientToday.ToString + '[' + Duration.ToString + ' s]';
    ctGradientLogTerm:
      Result := ctGradientLogTerm.ToString + '[' + Duration.ToString + ' w]';
    ctCorridorWidth:
      Result := ctCorridorWidth.ToString;
  end;
end;

procedure TCalculationColumn.Clear;
begin
  Self := Default(TCalculationColumn);
end;

procedure TCalculationColumn.FromList(aInfo: string);
var
  StInfo: TStringList;
begin
  if not aInfo.IsEmpty then
  begin
    StInfo := TStringList.Create;
    try
      StInfo.Text     := aInfo;
      CalculationType := TCalculationType(StrToIntDef(StInfo.Values['CalculationType'], 0));
      Duration        := StrToIntDef(StInfo.Values['Duration'], 0);
    finally
      FreeAndNil(StInfo);
    end;
  end;
end;

function TCalculationColumn.IsEquals(ACalculationColumn: TCalculationColumn): Boolean;
begin
  Result := (ACalculationColumn.CalculationType = Self.CalculationType) and
            (ACalculationColumn.Duration = Self.Duration);
end;

function TCalculationColumn.ToList: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('CalculationType=%d', [Integer(CalculationType)]).AppendLine
      .AppendFormat('Duration=%d', [Duration]).AppendLine;
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

end.
