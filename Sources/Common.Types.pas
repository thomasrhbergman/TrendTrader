unit Common.Types;

interface

{$REGION 'Region uses'}
uses
  Winapi.Messages, System.SysUtils, System.Variants, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Vcl.Graphics,
  System.Math, IABSocketAPI_const;
{$ENDREGION}

type
  TLogListenerType = (ltLogWriter, ltLogView, ltActivityLog);
  TLogListenerTypes = set of TLogListenerType;

  TTypeEvent = (teCheckLastPrice, teCheckInstruments);
  TProgressEvent = procedure(const aPosition: Integer; const aTypeEvent: TTypeEvent) of object;
  TStartProgressEvent = procedure(const aMaxPosition: Integer; const aTypeEvent: TTypeEvent) of object;
  TAbortEvent = function(const aTypeEvent: TTypeEvent): Boolean of object;

  TLockControlEvent = procedure(const aInfo: string) of object;
  TUnlockControlEvent = procedure(const aInfo: string = '') of object;

  TIndexType = (itIndexFutures, itStockFutures);
  TNodeType = (ntNode, ntGroup, ntGroupProfit, ntGroupLoss);

  TTypeUseInAutoorder = (tuMonitor, tuTemplate, tuBaseOrder);
  TTypeUseInAutoorders = set of TTypeUseInAutoorder;
  TTypeUseInAutoorderHelper = record helper for TTypeUseInAutoorder
  private
    const TypeUseInAutoorderString: array [TTypeUseInAutoorder] of string = ('Monitor', 'Template', 'BaseOrder');
  public
    function ToString: string;
  end;

  TCreationType = (ctUser, ctProgramm, ctMirror);
  TCreationTypeHelper = record helper for TCreationType
  private
    const CreationTypeString: array [TCreationType] of string = ('User', 'Programm', 'Mirror');
  public
    function ToString: string;
  end;

  TStringObject = class(TObject)
  public
    Id: Integer;
    StringValue: string;
    constructor Create(const aId: Integer; const aStringValue: string); overload;
  end;

  TAutoTradesCommon = record
    Quantity: Integer;
    QualifierInstance: Integer;
    QualifierID: Integer;
    AutoTradesInstance: Integer;
    AutoTradesID : Integer;
    AllowSendDuplicateOrder: Boolean;
    constructor Create(const aQuantity, aQualifierInstance, aQualifierID, aAutoTradesInstance, aAutoTradesID: Integer; aAllowSendDuplicateOrder: Boolean);
  end;

  TBasePrice = (pbAsk10=110, pbAsk9=109, pbAsk8=108, pbAsk7=107, pbAsk6=106, pbAsk5=105, pbAsk4=104, pbAsk3=103, pbAsk2=103, pbAsk1=101, pbAsk=100,
                pbLast=0, pbMid=1, pbLow=2, pbHigh=3,
                pbBid=-100, pbBid1=-101, pbBid2=-102, pbBid3=-103, pbBid4=-104, pbBid5=-105, pbBid6=-106, pbBid7=-107, pbBid8=-108, pbBid9=-109, pbBid10=-110);
  TBasePriceHelper = record helper for TBasePrice
    function ToString: string;
  end;

  TBasisForPrice = (bpFillPrice, bpTickType);
  TBasisForHelper = record helper for TBasisForPrice
  private
    const BasisForPriceString: array[TBasisForPrice] of string = ('FillPrice', 'TickType');
  public
    function ToString: string;
  end;

  TInequalityType = (iqAbove, iqAboveOrEqual, iqBelow, iqBelowOrEqual, iqEqual, iqCrosses, iqCrossesUp, iqCrossesDown, iqBetween);
  TInequalityTypeHelper = record helper for TInequalityType
  private
    const InequalityFullName: array[TInequalityType] of string = ('Above >', 'Above or equal >=', 'Below <', 'Below or equal <=', 'Equal =', 'Crosses ✕', 'Crosses Up ↑', 'Crosses Down ↓', 'Between');
    const InequalityName: array[TInequalityType] of string = ('>', '>=', '<', '<=', '=', '✕', '↑', '↓', 'in');
  public
    function ToString: string;
    function ToFullName: string;
    function IsCondition(const Value1, Value2: Currency): Boolean;
  end;

  TDocType = (ntQualifier, ntAutoTrade, ntOrderGroupSet, ntOrderGroup, ntOrder, ntCondition, ntAlgos, ntFactor, ntCandidates, ntQuantities, ntOrderTemplate, ntUnknow);
  TDocTypes = set of TDocType;
  TDocTypeHelper = record helper for TDocType
  private
    const DocTypeString: array[TDocType] of string = ('Qualifier', 'AutoTrade', 'Order Groups', 'Order Group', 'Order', 'Condition', 'Algos', 'Factor', 'Candidate process', 'Quantity', 'Order templates', 'Unknow');
  public
    function ToString: string;
  end;

  TOrderKind = (okNormal, okOneCancelAll, okSequentialContinue, okSequentialStop, okModifyOrder);
  TOrderKindHelper = record helper for TOrderKind
  private
    const OrderKindString: array[TOrderKind] of string = ('Normal', 'OneCancelAll', 'SequentialContinue', 'SequentialStop', 'ModifyOrder');
  public
    function ToString: string;
  end;

  TPrecautionarySettingType =(psAlgorithmTotalValueLimit, psNumberOfTicks, psPercentage, psTotalValueLimit, psAlgorithmSizeLimit, psOrderQuantityMax, psMaxAllowedPrice, psMinAllowedPrice);
  TPrecautionarySettingTypes = set of TPrecautionarySettingType;
  TPrecautionarySettingArray = array[TPrecautionarySettingType] of Double;
  TPrecautionarySettingTypeHelper = record helper for TPrecautionarySettingType
  private
    const PrecautionarySettingString: array[TPrecautionarySettingType] of string =('AlgorithmTotalValueLimit', 'NumberOfTicks', 'Percentage', 'TotalValueLimit', 'AlgorithmSizeLimit', 'OrderQuantityMax', 'MaxAllowedPrice', 'MinAllowedPrice');
  public
    function ToString: string;
  end;

  TLogDetailType = (ddEnterMethod, ddExitMethod, ddError, ddText, ddWarning);
  TLogDetailTypes = set of TLogDetailType;
  TLogDetailTypeHelper = record helper for TLogDetailType
  private
    const LogDetailTypesString: array[TLogDetailType] of string = ('Enter Method', 'Exit Method', 'Error', 'Text', 'Warning');
  public
    function ToString: string;
  end;

  TCalculationStage = (csLatestFillPrice, csLimitPrice, csAuxPrice, csTrailStopPrice, csLimitPriceOffset, csAvgPrice, csFillPrice, csAdjCoef,
                       csAuxBasePrice, csLimitBasePrice, csLmtOffsetBasePrice, csTrailStopBasePrice);
  TArrayCalculationStage = array[TCalculationStage] of Double;
  TCalculationStageHelper = record helper for TCalculationStage
  private
    const CalculationStageString: array[TCalculationStage] of string = ('LatestFillPrice', 'LimitPrice', 'AuxPrice', 'TrailStopPrice',
                                                                        'LimitPriceOffset', 'AvgPrice', 'FillPrice', 'AdjCoef',
                                                                        'AuxBasePrice', 'LimitBasePrice', 'LmtOffsetBasePrice', 'TrailStopBasePrice');
  public
    function ToString: string;
  end;

  TRateType = (rtValue, rtPercent);
  TRateTypeHelper = record helper for TRateType
  private
    const RateTypeString: array[TRateType]  of string = ('Price', 'Percent');
  public
    function ToString: string;
  end;

  TSubordination = (suUnknow, suMotherOrder, suChildOrder);
  TSubordinationHelper = record helper for TSubordination
  private
    const SubordinationString: array[TSubordination] of string = ('Unknow', 'MotherOrder', 'ChildOrder');
  public
    function ToString: string;
  end;

  TConditionType = (ctRealtimeValue, ctTimeGap, ctGradient, ctTimeInForce, ctCorridor, ctCorridorPosition, ctTrailBuy, ctTrailSell, ctGradientAndCorridor, ctRealtimeAndTimeGap);
  TConditionTypeHelper = record helper for TConditionType
  private const
    CondTypeString: array[TConditionType] of string = ('Realtime Value', 'Time gap', 'Gradient', 'Time In Force', 'Corridor', 'Corridor position', 'Trail-buy', 'Trail-sell', 'Gradient And Corridor', 'Realtime And Time Gap');
  public
    function ToString: string;
  end;

  TTypeOperation = (toNone, toDivide, toSum, toDiff, toMult, toDividePercent, toSumPercent, toDiffPercent, toMultPercent);
  TTypeOperationHelper = record helper for TTypeOperation
  private const
    TypeOperationString: array[TTypeOperation] of string = (' ', '/', '+', '-', '*', '/ %', '+ %', '- %', '* %');
  public
    function ToString: string;
    function Calc(const Value1, Value2: Currency): Currency;
  end;

  TCompiledValue = record
  public
    TotalValue   : Currency;
    SummValue1   : Currency;
    SummValue2   : Currency;
    IsValueReady : Boolean;
    Factors      : string;
  end;

  PConditionHistory = ^TConditionHistory;
  TConditionHistory = record
  public
    Active: Boolean;
    Enabled: Boolean;
    CondType: TConditionType;
    TimeStamp: TDateTime;
    CalcValue: Double;
    CondLimit: Currency;
    InequalityType: TInequalityType;
    Value1: Double;
    Value2: Double;
    TickType1: TIABTickType;
    TickType2: TIABTickType;
    IsCondition: Boolean;
    TypeOperation: TTypeOperation;
    Factors: string;
    procedure Clear;
  end;

  TBrokerType = (brIB, brNN, brTest);
  TBrokerTypeHelper = record helper for TBrokerType
  private const
    BrokersFullName: array [TBrokerType] of string = ('Interactive Brokers', 'NordNet Brokers', 'Test Brokers');
    BrokersShortName: array [TBrokerType] of string = ('IB', 'NordNet', 'Test');
    BrokersAbbrevName: array [TBrokerType] of string = ('IB', 'NN', 'TB');
  public
    class function FromInteger(aValue: Integer): TBrokerType; static;
    function ToAbbrevName: string;
    function ToFullName: string;
    function ToString: string;
  end;

const
  C_CURRENCY_FORMAT         = ',0.00';
  C_DECIMALS                = 4;
  C_DEFAULT_CURRENCY        = 'SEK';
  C_MONITOR_TREE_FIX_COLUMN = 6;
  C_SECTION_ORDER_STATUS    = 'Status';
  C_WRONG_VALUE_COLOUR     : TColor = clWebSalmon;
  C_ORDER_FILLED_COLOUR    : TColor = $004600;
  C_ORDER_SUBMITTED_COLOUR : TColor = clWebPaleVioletRed;

implementation

{ TAutoTradesCommon }

constructor TAutoTradesCommon.Create(const aQuantity, aQualifierInstance, aQualifierID, aAutoTradesInstance, aAutoTradesID: Integer; aAllowSendDuplicateOrder: Boolean);
begin
  Self := Default(TAutoTradesCommon);
  Self.Quantity                := aQuantity;
  Self.AllowSendDuplicateOrder := aAllowSendDuplicateOrder;
  Self.AutoTradesID            := aAutotradesID;
  Self.AutoTradesInstance      := aAutoTradesInstance;
  Self.QualifierID             := aQualifierID;
  Self.QualifierInstance       := aQualifierInstance;
end;

{ TStringObject }

constructor TStringObject.Create(const aId: Integer; const aStringValue: string);
begin
  Self.Id := aId;
  Self.StringValue := aStringValue;
end;

{ TBasePriceHelper }

function TBasePriceHelper.ToString: string;
begin
  case Self of
    pbLast:
      Result := 'LAST';
    pbMid:
      Result := 'MID';
    pbHigh:
      Result := 'HIGH';
    pbLow:
      Result := 'LOW';
    pbAsk:
      Result := 'ASK';
    pbAsk1 .. pbAsk10:
      Result := 'ASK+' + Abs(Ord(Self) - 100).ToString;
    pbBid:
      Result := 'BID';
    pbBid10 .. pbBid1:
      Result := 'BID-' + Abs(Ord(Self) + 100).ToString;
  end;
end;

{ TBasisForHelper }

function TBasisForHelper.ToString: string;
begin
  Result := BasisForPriceString[Self];
end;

{ TDocTypeHelper }

function TDocTypeHelper.ToString: string;
begin
  Result := DocTypeString[Self];
end;

{ TPrecautionarySettingTypeHelper }

function TPrecautionarySettingTypeHelper.ToString: string;
begin
  Result := PrecautionarySettingString[Self];
end;

{ TLogDetailTypeHelper }

function TLogDetailTypeHelper.ToString: string;
begin
  Result := LogDetailTypesString[Self];
end;

{ TCalculationStageHelper }

function TCalculationStageHelper.ToString: string;
begin
  Result := CalculationStageString[Self];
end;

{ TRateTypeHelper }

function TRateTypeHelper.ToString: string;
begin
  Result := RateTypeString[Self];
end;

{ TSubordinationHelper }

function TSubordinationHelper.ToString: string;
begin
  Result := SubordinationString[Self];
end;

{ TConditionTypeHelper }

function TConditionTypeHelper.ToString: string;
begin
  Result := CondTypeString[Self];
end;

{ TTypeUseInAutoorderHelper }

function TTypeUseInAutoorderHelper.ToString: string;
begin
  Result := TypeUseInAutoorderString[Self];
end;

{ TOrderKindHelper }

function TOrderKindHelper.ToString: string;
begin
  Result := OrderKindString[Self];
end;

{ TTypeOperationHelper }

function TTypeOperationHelper.ToString: string;
begin
  Result := TypeOperationString[Self];
end;

function TTypeOperationHelper.Calc(const Value1, Value2: Currency): Currency;
begin
  Result := 0;
  case Self of
    toDividePercent:
      if (Value2 <> 0) then
        Result := 100 * (Value1 / Value2) / Value2;
    toSumPercent:
      if (Value2 <> 0) then
        Result := 100 * (Value1 + Value2) / Value2;
    toDiffPercent:
      if (Value2 <> 0) then
        Result := 100 * (Value1 - Value2) / Value2;
    toMultPercent:
      if (Value2 <> 0) then
        Result := 100 * (Value1 * Value2) / Value2;
    toDivide:
      if (Value2 <> 0) then
        Result := Value1 / Value2;
    toSum:
      Result := Value1 + Value2;
    toDiff:
      Result := Value1 - Value2;
    toMult:
      Result := Value1 * Value2;
  end;
end;

{ TInequalityTypeHelper }

function TInequalityTypeHelper.IsCondition(const Value1, Value2: Currency): Boolean;
const
  Epsilon = 0.00001;
begin
  Result := False;
  case Self of
    iqAbove:
      Result := Value1 > Value2;
    iqAboveOrEqual:
      Result := Value1 >= Value2;
    iqBelow:
      Result := Value1 < Value2;
    iqBelowOrEqual:
      Result := Value1 <= Value2;
    iqBetween:
      Result := False;
    iqEqual:
      Result := SameValue(Value1, Value2, Epsilon);
    iqCrosses:
      if (Max(Value1, Value2) <> 0) then
        Result := Abs(Value1 - Value2) / Max(Value1, Value2) > 0.05;
    iqCrossesUp:
      if (Value1 <> 0) then
        Result := (Value1 - Value2) / Value1 > 0.05;
    iqCrossesDown:
      if (Value2 <> 0) then
        Result := (Value2 - Value1) / Value2 > 0.05;
  end;
end;

function TInequalityTypeHelper.ToFullName: string;
begin
  Result := InequalityFullName[Self];
end;

function TInequalityTypeHelper.ToString: string;
begin
  Result := InequalityName[Self];
end;


{ TConditionHistory }

procedure TConditionHistory.Clear;
begin
  Self := Default(TConditionHistory);
end;

{ TBrokerTypeHelper }

class function TBrokerTypeHelper.FromInteger(aValue: Integer): TBrokerType;
begin
  if aValue in [0 .. 2] then
    Result := TBrokerType(aValue)
  else
    Result := brIB;
end;

function TBrokerTypeHelper.ToAbbrevName: string;
begin
  Result := BrokersAbbrevName[Self];
end;

function TBrokerTypeHelper.ToFullName: string;
begin
  Result := BrokersFullName[Self];
end;

function TBrokerTypeHelper.ToString: string;
begin
  Result := BrokersShortName[Self];
end;

{ TCreationTypeHelper }

function TCreationTypeHelper.ToString: string;
begin
  Result := CreationTypeString[Self];
end;

end.
