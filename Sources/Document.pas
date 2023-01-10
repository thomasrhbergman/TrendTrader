unit Document;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, System.Generics.Collections, IABfunctions, IABSocketAPI, VirtualTrees, DebugWriter,
  HtmlLib, Global.Types, Bleeper, BrokerHelperAbstr, BrokerHelperNN, NNfunctions, NNfunctions.Types, Utils,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} InstrumentList, CustomForms, IABSocketAPI_const, Data.DB,
  DaModule, Entity.Sokid, IABFunctions.RequestsQueue, Common.Types, System.Math, IABFunctions.MarketData,
  System.StrUtils, DaModule.Utils, XmlFiles, Publishers, IABFunctions.Helpers, System.Threading, Generics.Helper,
  Publishers.Interfaces, System.DateUtils, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TTickValue = array[System.Low(TIABTickType) .. System.High(TIABTickType)] of Double;
  TOrderStatusQueue = class;
  TCustomOrderDoc = class;

  TInstrument = record
    SokidInfo     : TSokidInfo;
    procedure AssignFrom(const aInstrument: TInstrument);
  end;

  TCustomDocument = class
  private
    FAutoTradesID       : Integer;
    FAutoTradesInstance : Integer;
    FEnabled            : Boolean;
    FFreezeValue        : TTickValue;
    FIsFrozen           : Boolean;
    FOwnerNode          : PVirtualNode;
    FQualifierID        : Integer;
    FQualifierInstance  : Integer;
    FRecordId           : Integer;
    FScanSequenceId     : Integer;
    FTickValue          : TTickValue;
    FUseInAutoOrder     : Boolean;
    FXmlParams          : string;
    function GetFreezeValueItem(Index: TIABTickType): Double;
  protected
    function GetValueItem(Index: TIABTickType): Double; virtual;
    function ToValueString: string; virtual;
    procedure AssignFrom(const aSource: TCustomDocument); virtual;
    procedure FromDB(const aRecordId: Integer); virtual; abstract;
    procedure SaveToDB; virtual; abstract;
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure SetValueItem(Index: TIABTickType; const Value: Double); virtual;
  public
    constructor Create;
    procedure Freeze;
    property AutoTradesID                    : Integer      read FAutoTradesID        write FAutoTradesID;
    property AutoTradesInstance              : Integer      read FAutoTradesInstance  write FAutoTradesInstance;
    property Enabled                         : Boolean      read FEnabled             write SetEnabled;
    property FreezeValue[Index: TIABTickType]: Double       read GetFreezeValueItem;
    property IsFrozen                        : Boolean      read FIsFrozen            write FIsFrozen;
    property OwnerNode                       : PVirtualNode read FOwnerNode           write FOwnerNode;
    property QualifierID                     : Integer      read FQualifierID         write FQualifierID;
    property QualifierInstance               : Integer      read FQualifierInstance   write FQualifierInstance;
    property RecordId                        : Integer      read FRecordId            write FRecordId;
    property UseInAutoOrder                  : Boolean      read FUseInAutoOrder      write FUseInAutoOrder;
    property XmlParams                       : string       read FXmlParams           write FXmlParams;
  end;

  TOrderGroupSetDoc = class(TCustomDocument)
  private
    FName    : string;
    FTypeUse : TTypeUseInAutoorder;
  public
    procedure FromDB(const aRecordId: Integer); override;
    procedure SaveToDB; override;
    procedure AssignFrom(const aSource: TCustomDocument); override;
    property TypeUse : TTypeUseInAutoorder read FTypeUse write FTypeUse;
    property Name    : string              read FName    write FName;
  end;

  TOrderGroupDoc = class(TCustomDocument)
  private
    FCheckpointPeriod : Integer;
    FIsAutoOrder      : Boolean;
    FIsRepetitive     : Boolean;
    FKind             : TOrderKind;
    FName             : string;
    FOrderStatusQueue : TOrderStatusQueue;
    procedure SetKind(const Value: TOrderKind);
  public
    constructor Create;
    destructor Destroy; override;

    function ToValueString: string; override;
    procedure AssignFrom(const aSource: TCustomDocument); override;
    procedure FromDB(const aRecordId: Integer); override;
    procedure SaveToDB; override;
    procedure AddOrderToQueue(aOrderDoc: TCustomOrderDoc);

    property IsAutoOrder      : Boolean    read FIsAutoOrder      write FIsAutoOrder;
    property IsRepetitive     : Boolean    read FIsRepetitive     write FIsRepetitive;
    property Kind             : TOrderKind read FKind             write SetKind;
    property Name             : string     read FName             write FName;
    property CheckpointPeriod : Integer    read FCheckpointPeriod write FCheckpointPeriod;
  end;

  TCustomOrderDoc = class(TCustomDocument)
  private type
    TAvg = record
      Quantity: Integer;
      Price: Double;
    end;
  public type
    TExtendedOptions = record
      AuxPriceRelative: Double;
      LimitPriceOffset: Double;
      LimitPriceRelative: Double;
      OpenVolume: Double;
      Subordination: TSubordination;
      BasisForPrice: TBasisForPrice;
      TrailStopPriceRelative: Double;
      VolumePercent: Double;

      AuxBasePrice: TBasePrice;
      LimitBasePrice: TBasePrice;
      LmtOffsetBasePrice: TBasePrice;
      TrailStopBasePrice: TBasePrice;
      procedure AssignFrom(const aSource: TExtendedOptions);
    end;
  private
    FAvgPrice: Currency;
    FBrokerType: TBrokerType;
    FCalculationStage: TArrayCalculationStage;
    FColTick: TColor;
    FCurrency: string;
    FDecimals: Integer;
    FDescription: string;
    FExchange: string;
    FExpiry: TDateTime;
    FExtendedOptions: TExtendedOptions;
    FFilled: Integer;
    FId: Integer;
    FInfo: string;
    FInstrumentName: string;
    FIsActivateChild: Boolean;
    FIsExecuted: Boolean;
    FIsFinal: Boolean;
    FIsRepetitive: Boolean;
    FLastPrice: Currency;
    FLatestFillPrice: Double;
    FLimit: Double;
    FMarketList: string;
    FMultiplier: Double;
    FOrderAction: TIABAction;
    FOrderIBId: Integer;
    FOrderList : TStringList;
    FOrderStatus: TIABOrderState;
    FParentOrder: TCustomOrderDoc;
    FPrices: array of TAvg;
    FPrimaryExchange: string;
    FQuantity: Integer;
    FRateType: TRateType;
    FSymbol: string;
    FTemplateID: Integer;
    FTradeTime: TDateTime;
    FTriggerMethod: TTriggerMethod;
    FVisiblePart: Currency;
    function GetCalculationStageItem(Index: TCalculationStage): Double;
    procedure SetCalculationStageItem(Index: TCalculationStage; const Value: Double);
    procedure SetOrderIBId(const Value: Integer);
  protected
    FOrderType: TIABOrderType;
    function GetOrderStatusText: string; virtual; abstract;
    function IsPossibleToBuyOrder(aNode: PVirtualNode): Boolean; virtual; abstract;
    procedure SetOrderType(const Value: TIABOrderType); virtual;
  public
    Instrument: TInstrument;
    procedure AddPrice(const Price: Double; const Quantity: Integer);
    procedure AssignFrom(const aSource: TCustomDocument); overload; override;
    procedure AssignFrom(const aSource: TSokidInfo); reintroduce; overload;
    procedure Buy; virtual; abstract;
    procedure CancelOrder; virtual; abstract;
    procedure DoBuy; virtual; abstract;
    procedure FromDB(const aRecordId: Integer); override;
    procedure SaveToDB; override;
    procedure UpdateOrder; virtual; abstract;
    function ToString: string; override;
    function ToValueString: string; override;

    constructor Create;
    destructor Destroy; override;
    property AvgPrice        : Currency               read FAvgPrice         write FAvgPrice;
    property BrokerType      : TBrokerType            read FBrokerType       write FBrokerType;
    property CalculationStage[Index: TCalculationStage]: Double read GetCalculationStageItem write SetCalculationStageItem;
    property ColTick         : TColor                 read FColTick          write FColTick;
    property Currency        : string                 read FCurrency         write FCurrency;
    property Decimals        : Integer                read FDecimals         write FDecimals;
    property Description     : string                 read FDescription      write FDescription;
    property Exchange        : string                 read FExchange         write FExchange;
    property Expiry          : TDateTime              read FExpiry           write FExpiry;
    property ExtendedOptions : TExtendedOptions       read FExtendedOptions  write FExtendedOptions;
    property Filled          : Integer                read FFilled           write FFilled;
    property Id              : Integer                read FId               write FId;
    property Info            : string                 read FInfo             write FInfo;
    property InstrumentName  : string                 read FInstrumentName   write FInstrumentName;
    property IsActivateChild : Boolean                read FIsActivateChild  write FIsActivateChild;
    property IsExecuted      : Boolean                read FIsExecuted       write FIsExecuted;
    property IsFinal         : Boolean                read FIsFinal          write FIsFinal;
    property IsRepetitive    : Boolean                read FIsRepetitive     write FIsRepetitive;
    property LastPrice       : Currency               read FLastPrice        write FLastPrice;
    property LatestFillPrice : Double                 read FLatestFillPrice  write FLatestFillPrice;
    property Limit           : Double               read FLimit            write FLimit;
    property MarketList      : string                 read FMarketList       write FMarketList;
    property Multiplier      : Double                 read FMultiplier       write FMultiplier;
    property OrderAction     : TIABAction             read FOrderAction      write FOrderAction;
    property OrderIBId       : Integer                read FOrderIBId        write SetOrderIBId;
    property OrderStatus     : TIABOrderState         read FOrderStatus      write FOrderStatus;
    property OrderStatusText : string                 read GetOrderStatusText;
    property OrderType       : TIABOrderType          read FOrderType        write SetOrderType;
    property ParentOrder     : TCustomOrderDoc        read FParentOrder      write FParentOrder;
    property PrimaryExchange : string                 read FPrimaryExchange  write FPrimaryExchange;
    property Quantity        : Integer                read FQuantity         write FQuantity;
    property RateType        : TRateType              read FRateType         write FRateType;
    property Symbol          : string                 read FSymbol           write FSymbol;
    property TradeTime       : TDateTime              read FTradeTime        write FTradeTime;
    property TriggerMethod   : TTriggerMethod         read FTriggerMethod    write FTriggerMethod;
    property VisiblePart     : currency               read FVisiblePart      write FVisiblePart;
  end;

  TOrderNNDoc = class(TCustomOrderDoc)
  public type
    TCurrentValue = (cvMinPayed, cvMaxPayed, cvLastPayed, cvEntryPrice);
    TValueArray = array [TCurrentValue] of Double;
    TLimitType = (ltRelative, ltFix);
  const
    OrderNNStateString: array [TIABOrderState] of string = ('', '', '', 'Sent', 'Cancelled', 'Filled', 'PartlyFilled', 'Sleeping', 'Error', 'Not Considered');
  private type
    TTrailResult = (trNone, trTrigger, trReached, trStopLoss);
    TConditionResult = record
      Kind: TTrailResult;
      Value: Double;
    end;
  private
    FActivationValue: Double;
    FChildId: Integer;
    FChildIdentifierList: string;
    FChildMarketList: string;
    FChildSymbol: string;
    FConditionReached: Double;
    FConditionStopLoss: Double;
    FFeedFromBroker: Integer;
    FIdentifierList: string;
    FIsActiveReached: Boolean;
    FIsActiveStopLoss: Boolean;
    FIsIn: string;
    FLimitType: TLimitType;
    FOpenVolume: Integer;
    FPrice: Double;
    FReference: string;
    FTrailOrderSendType: TIABOrderType;
    FTrailTriggerLimit: Double;
    FTrailTriggerSendSell: Double;
    FValidUntil: string;
    FValueArray: TValueArray;
    function GetValidUntil: string;
    function GetValueArrayItem(Index: TCurrentValue): Double;
    procedure ClearValueArray;
    procedure SetFeedFromBroker(const Value: Integer);
    procedure SetLastValueArray;
    procedure SetValueArrayItem(Index: TCurrentValue; const Value: Double); overload;
    procedure SetChildId(const Value: Integer);
  protected
    function GetOrderStatusText: string; override;
    function IsPossibleToBuyOrder(aNode: PVirtualNode): Boolean; override;
    procedure SetOrderType(const Value: TIABOrderType); override;
  public
    procedure Buy; override;
    procedure CancelOrder; override;
    procedure DoBuy; override;
    procedure FromDB(const aRecordId: Integer); override;
    procedure SaveToDB; override;
    procedure SetValueArrayFromChildFeed(Index: TIABTickType; const Value: Currency);
    procedure UpdateOrder; override;
    procedure AssignFrom(const aSource: TCustomDocument); override;

    class function ActionStateToOrderState(aAction: string): TIABOrderState;
    constructor Create;
    destructor Destroy; override;

    property ChildId               : Integer          read FChildId              write SetChildId;
    property ChildIdentifierList   : string           read FChildIdentifierList  write FChildIdentifierList;
    property ChildMarketList       : string           read FChildMarketList      write FChildMarketList;
    property ChildSymbol           : string           read FChildSymbol          write FChildSymbol;
    property ConditionReached      : Double           read FConditionReached     write FConditionReached;
    property ConditionStopLoss     : Double           read FConditionStopLoss    write FConditionStopLoss;
    property FeedFromBroker        : Integer          read FFeedFromBroker       write SetFeedFromBroker;
    property IdentifierList        : string           read FIdentifierList       write FIdentifierList;
    property IsActiveReached       : Boolean          read FIsActiveReached      write FIsActiveReached;
    property IsActiveStopLoss      : Boolean          read FIsActiveStopLoss     write FIsActiveStopLoss;
    property IsIn                  : string           read FIsIn                 write FIsIn;
    property LimitType             : TLimitType       read FLimitType            write FLimitType;
    property OpenVolume            : Integer          read FOpenVolume           write FOpenVolume;
    property Price                 : Double           read FPrice                write FPrice;
    property Reference             : string           read FReference            write FReference;
    property TrailOrderSendType    : TIABOrderType    read FTrailOrderSendType   write FTrailOrderSendType;
    property TrailTriggerLimit     : Double           read FTrailTriggerLimit    write FTrailTriggerLimit;
    property TrailTriggerSendSell  : Double           read FTrailTriggerSendSell write FTrailTriggerSendSell;
    property ValidUntil            : string           read GetValidUntil         write FValidUntil;
    property ValueArray[Index: TCurrentValue]: Double read GetValueArrayItem     write SetValueArrayItem;
  end;

  TOrderIBDoc = class(TCustomOrderDoc)
  private
    FAdvancedOrderType: TAdvancedOrderType;
    FAuxPrice: Double;
    FDateStart: TDate;
    FDateStop: TDate;
    FLmtPriceOffset: Double;
    FMaxNumAmount: Integer;
    FMaxNumShares: Integer;
    FOcaGroupNumber: Integer;
    FOrderStartEnabled: Boolean;
    FOrderStopEnabled: Boolean;
    FParentIBId: Integer;
    FScope: Integer;
    FSecurityType: TIABSecurityType;
    FTimeInForce: Integer;
    FTimeStart: TTime;
    FTimeStop: TTime;
    FTrailingPercent: Double;
    FTrailStopPrice: Double;
    FTransmit: Boolean;
    FLocalSymbol: string;
    function GetLocalSymbol: string;
    procedure SetLocalSymbol(const Value: string);
  protected
    function GetOrderStatusText: string; override;
    function IsPossibleToBuyOrder(aNode: PVirtualNode): Boolean; override;
  public const
    C_SCOPE_UNKNOW                  = -1;
    C_SCOPE_ALL_OR_NONE             = 0;
    C_SCOPE_FILL_OR_KILL            = 1;
    C_SCOPE_IMMEDIATE_OR_CANCEL_IOC = 2;
    C_SCOPE_ONE_CANCELS_ALL_OCA     = 3;

    C_SECTION_ORDER       = 'Order';
    C_KEY_ORDER_TYPE      = 'OrderType';
    C_KEY_SCOPE_OF_ORDER  = 'ScopeOfOrder';
    C_KEY_TIME_IN_FORCE   = 'TimeInForce';
    C_KEY_AUXPRICE_BP     = 'AuxBasePrice';
    C_KEY_LIMIT_BP        = 'LimitBasePrice';
    C_KEY_LMTOFFSET_BP    = 'LmtOffsetBasePrice';
    C_KEY_TRAILSTOP_BP    = 'TrailStopBasePrice';
    C_KEY_BASIS_FOR_PRICE = 'BasisForPrice';

    cScopeToString: array [0..3] of string = ('All or none', 'Fill or Kill', 'Immediate Or Cancel IOC', 'Cancels All OCA');
  public
    function CreateIABOrder(const aOrder: TOrderIBDoc): TIABOrder;
    procedure Buy; override;
    procedure CancelOrder; override;
    procedure DoBuy; override;
    procedure FromDB(const aRecordId: Integer); override;
    procedure SaveToDB; override;
    procedure SetSecurityType(const Value: string);
    procedure UpdateOrder; override;
    procedure AssignFrom(const aSource: TCustomDocument); override;
    function ToString: string; override;
    function ToValueString: string; override;

    constructor Create;
    destructor Destroy; override;

    property AdvancedOrderType : TAdvancedOrderType read FAdvancedOrderType write FAdvancedOrderType;
    property AuxPrice          : Double             read FAuxPrice          write FAuxPrice;
    property DateStart         : TDate              read FDateStart         write FDateStart;
    property DateStop          : TDate              read FDateStop          write FDateStop;
    property LmtPriceOffset    : Double             read FLmtPriceOffset    write FLmtPriceOffset;
    property LocalSymbol       : string             read GetLocalSymbol     write SetLocalSymbol;
    property MaxNumAmount      : Integer            read FMaxNumAmount      write FMaxNumAmount;
    property MaxNumShares      : Integer            read FMaxNumShares      write FMaxNumShares;
    property OcaGroupNumber    : Integer            read FOcaGroupNumber    write FOcaGroupNumber;
    property OrderStartEnabled : Boolean            read FOrderStartEnabled write FOrderStartEnabled;
    property OrderStopEnabled  : Boolean            read FOrderStopEnabled  write FOrderStopEnabled;
    property ParentIBId        : Integer            read FParentIBId        write FParentIBId;
    property Scope             : Integer            read FScope             write FScope;
    property SecurityType      : TIABSecurityType   read FSecurityType      write FSecurityType;
    property TimeInForce       : Integer            read FTimeInForce       write FTimeInForce;
    property TimeStart         : TTime              read FTimeStart         write FTimeStart;
    property TimeStop          : TTime              read FTimeStop          write FTimeStop;
    property TrailStopPrice    : Double             read FTrailStopPrice    write FTrailStopPrice;
    property TrailingPercent   : Double             read FTrailingPercent   write FTrailingPercent;
    property Transmit          : Boolean            read FTransmit          write FTransmit;
  end;

  TOrderTestDoc = class(TCustomOrderDoc)
  protected
    function GetOrderStatusText: string; override;
    function IsPossibleToBuyOrder(aNode: PVirtualNode): Boolean; override;
  public
    procedure Buy; override;
    procedure CancelOrder; override;
    procedure DoBuy; override;
    procedure FromDB(const aRecordId: Integer); override;
    procedure SaveToDB; override;
    procedure UpdateOrder; override;
  end;

  TConditionDoc = class(TCustomDocument)
  type
    TPrice = class
    public
      Price: Double;
      constructor Create(aPrice: Double);
    end;
    TGradientPrice = record
    public
      Price: Double;
      TimeStamp: TDateTime;
    end;
    TPriority = (cpNormal, cpPriority, cpVeto);
    TKindCreation = (kcUser, kcTrigger, kcStopLoss, kcReached);
    TValueArray = array [TConditionType] of Double;
    TMinMaxValue = (mmMinPayed, mmMaxPayed, mmLastPayed, mmEntryPrice);
    TMinMaxValueArray = array [TMinMaxValue] of Double;
  const
    C_SECTION_CONDITION = 'Condition';
  protected
    procedure SetValueItem(Index: TIABTickType; const Value: Double); override;
    procedure SetEnabled(const Value: boolean); override;
  private
    FName: String;
    FActivationValue: Double;
    FActive: Boolean;
    FCalcValue: Currency;
    FBypass: Boolean;
    FCompiledValue: TCompiledValue;
    FCondType: TConditionType;
    FCondLimit: Currency;
//    FCondLimitRelative: Currency;
    FCondWidth: Currency;
    FDescription: string;
    FDivisionValue: Currency;
    FDuration: TTime;
    FEndDate: TDate;
    FEndTime: TTime;
    FGradient: Currency;
    FHistory: TList<TConditionHistory>;
    FInequalityCor: TInequalityType;
    FInequalityGr: TInequalityType;
    FInequalityRt: TInequalityType;
    FInitTime: TDateTime;
    FInitValue: Currency;
    FIsBreakUp: Boolean;
    FIsCondition: Boolean;
    FIsLoadedHistoricalData: Boolean;
    FIsValueReady: Boolean;
    FKindCreation: TKindCreation;
    FMinMaxValueArray: TMinMaxValueArray;
    FMonitoring: Integer;
    FPriceArray: TArray<TGradientPrice>;
    FPriority: TPriority;
    FStartDate: TDate;
    FStartTime: TTime;
    FTickType1: TIABTickType;
    FTickType2: TIABTickType;
    FTrailBuy: Double;
    FTrailSell: Double;
    FTypeOperation: TTypeOperation;
    FUpProc: Integer;
    FValueArray: TValueArray;
    FGradientValue: Double;
    FRealTimeType: Integer;
    FTickType1Value: Double;
    FTickType2Value: Double;
    FConditionTime: TDateTime;
    FBasePrice: Double;
    FTimeInForce: Integer;
    FExtendOnLastPriceUp: Boolean;
    FTimeInForceStartTime: TDateTime;
    function GetMinMaxValueArrayItem(Index: TMinMaxValue): Double;
    function GetValueArrayItem(Index: TConditionType): Double;
    procedure AddConditionHistory;
    procedure SetActive(const Value: Boolean);
    procedure SetIsCondition(const Value: Boolean);
    procedure SetMinMaxValueArrayItem(Index: TMinMaxValue; const Value: Double);
    procedure SetValueArrayItem(Index: TConditionType; const Value: Double);
    function GetTimeInForceActive: Boolean;
  public
    Instrument: TInstrument;
    ValueList: TObjectDictionary<TDateTime, TPrice>;
    function ToValueString: string; override;
    function ToString: string; override;
    procedure AddToValueList(aTimeStamp: TDateTime; aPrice, aCoefficient: Double);
    procedure AssignFrom(const aSource: TCustomDocument); override;
    procedure BlankTrailParams;
    procedure FromDB(const aRecordId: Integer); override;
    procedure SaveToDB; override;
    procedure SetLengthPriceArray(aLength: Integer);
    function GetRealTimeTypeAsString: string;
    constructor Create;
    destructor Destroy; override;

    property Name                   : String                   read FName                   write FName;
    property ActivationValue        : Double                   read FActivationValue        write FActivationValue;
    property Active                 : Boolean                  read FActive                 write SetActive;
    property CalcValue              : Currency                 read FCalcValue              write FCalcValue;
    property Bypass                 : Boolean                  read FBypass                 write FBypass;
    property CompiledValue          : TCompiledValue           read FCompiledValue          write FCompiledValue;
    property CondType               : TConditionType           read FCondType               write FCondType;
    property CondLimit              : Currency                 read FCondLimit              write FCondLimit;
//    property CondLimitRelative      : Currency                 read FCondLimitRelative      write FCondLimitRelative;
    property CondWidth              : Currency                 read FCondWidth              write FCondWidth;
    property Description            : string                   read FDescription            write FDescription;
    property DivisionValue          : Currency                 read FDivisionValue          write FDivisionValue;
    property Duration               : TTime                    read FDuration               write FDuration;
    property EndDate                : TDate                    read FEndDate                write FEndDate;
    property EndTime                : TTime                    read FEndTime                write FEndTime;
    property Gradient               : Currency                 read FGradient               write FGradient;
    property InequalityCor          : TInequalityType          read FInequalityCor          write FInequalityCor;
    property InequalityGr           : TInequalityType          read FInequalityGr           write FInequalityGr;
    property InequalityRt           : TInequalityType          read FInequalityRt           write FInequalityRt;
    property InitTime               : TDateTime                read FInitTime               write FInitTime;
    property InitValue              : Currency                 read FInitValue              write FInitValue;
    property IsBreakUp              : Boolean                  read FIsBreakUp              write FIsBreakUp;
    property IsCondition            : Boolean                  read FIsCondition            write SetIsCondition;
    property IsLoadedHistoricalData : Boolean                  read FIsLoadedHistoricalData write FIsLoadedHistoricalData;
    property IsValueReady           : Boolean                  read FIsValueReady           write FIsValueReady;
    property KindCreation           : TKindCreation            read FKindCreation           write FKindCreation;
    property Monitoring             : Integer                  read FMonitoring             write FMonitoring;
    property PriceArray             : TArray<TGradientPrice>   read FPriceArray             write FPriceArray;
    property Priority               : TPriority                read FPriority               write FPriority;
    property StartDate              : TDate                    read FStartDate              write FStartDate;
    property StartTime              : TTime                    read FStartTime              write FStartTime;
    property TickType1              : TIABTickType             read FTickType1              write FTickType1;
    property TickType2              : TIABTickType             read FTickType2              write FTickType2;
    property TickType1Value         : Double                   read FTickType1Value         write FTickType1Value;
    property TickType2Value         : Double                   read FTickType2Value         write FTickType2Value;
    property TrailBuy               : Double                   read FTrailBuy               write FTrailBuy;
    property TrailSell              : Double                   read FTrailSell              write FTrailSell;
    property TypeOperation          : TTypeOperation           read FTypeOperation          write FTypeOperation;
    property UpProc                 : Integer                  read FUpProc                 write FUpProc;
    property History                : TList<TConditionHistory> read FHistory;
    property ValueArray[Index: TConditionType]    : Double     read GetValueArrayItem       write SetValueArrayItem;
    property MinMaxValueArray[Index: TMinMaxValue]: Double     read GetMinMaxValueArrayItem write SetMinMaxValueArrayItem;
    property TickValue[Index: TIABTickType]       : Double     read GetValueItem            write SetValueItem;
    property GradientValue          : Double                   read FGradientValue          write FGradientValue;
    property RealTimeType           : Integer                  read FRealTimeType           write FRealTimeType;
    property ConditionTime          : TDateTime                read FConditionTime;
    property BasePrice              : Double                   read FBasePrice              write FBasePrice;
    property TimeInForce            : Integer                  read FTimeInForce            write FTimeInForce;
    property ExtendOnLastPriceUp    : Boolean                  read FExtendOnLastPriceUp    write FExtendOnLastPriceUp;
    property TimeInForceStartTime   : TDateTime                read FTimeInForceStartTime   write FTimeInForceStartTime;
    property TimeInForceActive      : Boolean                  read GetTimeInForceActive;
  end;

  TAlgosDoc = class(TCustomDocument)
  type
    TPrice = class
    public
      Price: Double;
      constructor Create(aPrice: Double);
    end;
  private
    FName: string;
    FDecimals: Integer;
    FDivisor: Double;
    function GetDivisor: Double;
    procedure SetDivisor(const Value: Double);
  public
    ValueList: TObjectDictionary<TDateTime, TPrice>;
    procedure AddToValueList(aTimeStamp: TDateTime; aPrice, aCoefficient: Double);
    procedure FromDB(const aRecordId: Integer); override;
    procedure SaveToDB; override;
    procedure AssignFrom(const aSource: TCustomDocument); override;
    constructor Create;
    destructor Destroy; override;

    property Name     : string  read FName      write FName;
    property Decimals : Integer read FDecimals  write FDecimals;
    property Divisor  : Double  read GetDivisor write SetDivisor;
    property TickValue[Index: TIABTickType]: Double read GetValueItem write SetValueItem;
  end;

  TFactorDoc = class(TCustomDocument)
  private
    FBrokerType: TBrokerType;
    FChangeProc: Double;
    FContractType: string;
    FCurrency: string;
    FCurrentValue: Currency;
    FDecimals: Integer;
    FDirday: string;
    FExchange: string;
    FExpiry: TDateTime;
    FIBId: Integer;
    FIdentifierList: string;
    FInstrumentName: string;
    FIsIn: string;
    FLocalSymbol: string;
    FMarketList: string;
    FNNId: Integer;
    FOmxs30_w: Double;
    FLastPrice1: Currency;
    FLastPrice2: Currency;
    FPrimaryExchange: string;
    FSymbol: string;
    FTickType1: TIABTickType;
    FTickType2: TIABTickType;
    FUseIBFeeds: Boolean;
    function GetContractId: Integer;
    function GetLocalSymbol: string;
    procedure SetContractId(const Value: Integer);
    procedure SetLocalSymbol(const Value: string);
  public
    function ToValueString: string; override;
    procedure AssignFrom(const aSource: TCustomDocument); override;
    procedure FromDB(const aRecordId: Integer); override;
    procedure SaveToDB; override;
    constructor Create;

    property BrokerType      : TBrokerType  read FBrokerType      write FBrokerType;
    property ChangeProc      : Double       read FChangeProc      write FChangeProc;
    property ContractType    : string       read FContractType    write FContractType;
    property Currency        : string       read FCurrency        write FCurrency;
    property CurrentValue    : Currency     read FCurrentValue    write FCurrentValue;
    property Decimals        : Integer      read FDecimals        write FDecimals;
    property Dirday          : string       read FDirday          write FDirday;
    property Exchange        : string       read FExchange        write FExchange;
    property Expiry          : TDateTime    read FExpiry          write FExpiry;
    property PrimaryExchange : string       read FPrimaryExchange write FPrimaryExchange;
    property IBId            : Integer      read FIBId            write FIBId;
    property NNId            : Integer      read FNNId            write FNNId;
    property IsIn            : string       read FIsIn            write FIsIn;
    property ContractId      : Integer      read GetContractId    write SetContractId;
    property IdentifierList  : string       read FIdentifierList  write FIdentifierList;
    property InstrumentName  : string       read FInstrumentName  write FInstrumentName;
    property MarketList      : string       read FMarketList      write FMarketList;
    property Omxs30_w        : Double       read FOmxs30_w        write FOmxs30_w;
    property LastPrice1      : Currency     read FLastPrice1      write FLastPrice1;
    property LastPrice2      : Currency     read FLastPrice2      write FLastPrice2;
    property Symbol          : string       read FSymbol          write FSymbol;
    property LocalSymbol     : string       read GetLocalSymbol   write SetLocalSymbol;
    property TickType1       : TIABTickType read FTickType1       write FTickType1;
    property TickType2       : TIABTickType read FTickType2       write FTickType2;
    property UseIBFeeds      : Boolean      read FUseIBFeeds      write FUseIBFeeds;
  end;

  TFormDocument = class(TCustomForm)
  private
    FOwnerNode: PVirtualNode;
  public
    Value: array [System.Low(TIABTickType) .. System.High(TIABTickType)] of Double;
    property OwnerNode : PVirtualNode read FOwnerNode write FOwnerNode;
  end;

  TOrderStatusQueue = class(TInterfacedList<TCustomOrderDoc>, IOrderState)
  private
    FTask: ITask;
    FCheckpointPeriod: Integer;
    FOwnerOrderGroupDoc: TOrderGroupDoc;
    //procedure CreateTask;
    //implementation IOrderState
    function GetInstance: TObject;
    procedure OnCloseOrder(const aTempId: Integer);
    procedure OnExecution(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrder(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrderNN(const aOrderList: array of TOrder);
    procedure OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
    procedure OnRebuildFromTWS(Sender: TObject);
  public
    constructor Create(aOwnerOrderGroupDoc: TOrderGroupDoc);
    destructor Destroy; override;
    procedure AddItem(aOrderDoc: TCustomOrderDoc);
    property CheckpointPeriod: Integer read FCheckpointPeriod write FCheckpointPeriod;
  end;

const
  KindCreationString: array[TConditionDoc.TKindCreation]  of string = ('User', 'Trigger', 'StopLoss', 'Reached');
  PriorityString: array[TConditionDoc.TPriority] of string = ('Normal', 'Priority', 'Veto');

implementation

{$R *.dfm}

uses
   Monitor.Interfaces, Monitor.Types;

{ TOrderIBDoc }

constructor TOrderIBDoc.Create;
begin
  inherited;
  AdvancedOrderType := atNone;
  OcaGroupNumber    := -1;
  FParentIBId       := -1;
  Transmit          := False;
  BrokerType        := TBrokerType.brIB;
  OrderType   := TIABOrderType(General.XMLFile.ReadInteger(C_SECTION_ORDER, C_KEY_ORDER_TYPE, Integer(otMarket)));
  Scope       := General.XMLFile.ReadInteger(C_SECTION_ORDER, C_KEY_SCOPE_OF_ORDER, C_SCOPE_UNKNOW);
  TimeInForce := General.XMLFile.ReadInteger(C_SECTION_ORDER, C_KEY_TIME_IN_FORCE, 0);
  if (OrderType = otNoChange) then
    OrderType := otMarket;
end;

procedure TOrderIBDoc.CancelOrder;
begin
  if (BrokerType = TBrokerType.brIB) then
  begin
    if not (OrderStatus in [osFilled, osSleeping, osCancelled]) and (OrderIBId > 0) then
    begin
      IABClient.SendRequest(ibCancelOrder, OrderIBId);
      OrderStatus := osPendCancel;
      TMonitorLists.OrderList.SetLastStatus(OwnerNode, osPendCancel);
      IsExecuted := False;
      TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, Self, 'CancelOrder', Self.Info);
    end
    else if (OrderStatus in [osSleeping]) and (OwnerNode.CheckState = csCheckedNormal) then
    begin
      if Assigned(OwnerNode) then
        TThread.Queue(nil,
          procedure
          var
            Monitor: IMonitor;
          begin
            OrderStatus := osCancelled;
            TMonitorLists.OrderList.SetLastStatus(OwnerNode, osCancelled);
            OwnerNode.CheckState := csUncheckedNormal;
            if Supports(Application.MainForm, IMonitor, Monitor) then
            begin
              Monitor.GetMainTree.InvalidateNode(OwnerNode);
              Monitor.SetChildsFreeze(OwnerNode);
            end;
          end);
    end;
  end;
end;

function TOrderIBDoc.CreateIABOrder(const aOrder: TOrderIBDoc): TIABOrder;
var
  Order: TIABOrder;
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'CreateIABOrder', aOrder.ToString);
  Order := TIABOrder.Create;
  try
    IABClient.ClearOrder(Order);
    Order.SecurityType    := aOrder.SecurityType;
    Order.Action          := aOrder.OrderAction;
    Order.OrderType       := aOrder.OrderType;
    Order.PercentOffset   := 0;
    Order.Exchange        := aOrder.Exchange;
    Order.PrimaryExchange := aOrder.PrimaryExchange;
    Order.Symbol          := aOrder.Symbol;
    Order.Currency        := aOrder.Currency;
    Order.Transmit        := aOrder.Transmit;
    Order.Quantity        := aOrder.Quantity;
    Order.TriggerMethod   := Ord(aOrder.TriggerMethod);
    Order.TimeInForce     := TIABTimeInForce(aOrder.TimeInForce);
    if not SameText(aOrder.PrimaryExchange, 'SFB') then
      if aOrder.VisiblePart < 100 then
        Order.DisplaySize     := Trunc(aOrder.Quantity * aOrder.VisiblePart / 100);
    if (aOrder.Multiplier > 0) then
      Order.Multiplier := aOrder.Multiplier.ToString;

    if (aOrder.Id > 0) then
      Order.ContractId := aOrder.Id;
    if (aOrder.ParentIBId > -1) then
      Order.ParentId := aOrder.ParentIBId;

    case aOrder.SecurityType of
      stOption:
        begin
          Order.Right      := rtCall;
          Order.Multiplier := aOrder.Multiplier.ToString;
        end;
      stFuture:
        begin
          if Length(aOrder.Symbol) < 4 then
            Order.Symbol := aOrder.Symbol
          else
            Order.LocalSymbol := aOrder.Symbol;
        end;
      stCash:
        begin
          Order.Exchange := 'IDEALPRO';
        end;
    end;

    if (aOrder.Scope = C_SCOPE_ONE_CANCELS_ALL_OCA) and (aOrder.OcaGroupNumber > -1) then
    begin
      Order.OcaMethod := ocaCancelWithBlock;
      Order.OcaGroup  := aOrder.OcaGroupNumber.ToString;
    end;

    if aOrder.OrderStartEnabled then
      Order.GoodAfterTime := FormatDateTime('yyyymmdd', aOrder.DateStart) + ' ' + FormatDateTime('hh:nn:ss', aOrder.TimeStart)
    else
      Order.GoodAfterTime := '';

    if aOrder.OrderStopEnabled then
      Order.GoodTillDate := FormatDateTime('yyyymmdd', aOrder.DateStop) +  ' ' + FormatDateTime('hh:nn:ss', aOrder.TimeStop)
    else
      Order.GoodTillDate := '';

    //http://interactivebrokers.github.io/tws-api/basic_orders.html
    case Order.OrderType of
      otMarket:
        begin

        end;
      otMarketOpen:
        begin
          //Order.TimeInForce := tifOPG;
        end;
      otLimit:
        begin
          Order.Price := SimpleRoundTo(aOrder.Limit, -C_DECIMALS);
        end;
      otStop:
        begin
          Order.AuxPrice := SimpleRoundTo(aOrder.AuxPrice, -C_DECIMALS);
        end;
      otStopLimit:
        begin
          Order.AuxPrice := SimpleRoundTo(aOrder.AuxPrice, -C_DECIMALS);
          Order.Price    := SimpleRoundTo(aOrder.Limit, -C_DECIMALS);
        end;
      otTrail:
        begin
          if (aOrder.RateType = rtValue) then
          begin
            Order.AuxPrice        := Abs(SimpleRoundTo(aOrder.AuxPrice, -C_DECIMALS));
            Order.TrailingPercent := UNSET_DOUBLE;
          end
          else
          begin
            Order.AuxPrice        := UNSET_DOUBLE;
            Order.TrailingPercent := aOrder.TrailingPercent;
          end;
          Order.Price := UNSET_DOUBLE;
          if (aOrder.TrailStopPrice > 0) then
            Order.TrailStopPrice := SimpleRoundTo(aOrder.TrailStopPrice, -C_DECIMALS)
          else
            Order.TrailStopPrice := UNSET_DOUBLE;
        end;
      otTrailLimit:
        begin
          if (aOrder.RateType = rtValue) then
          begin
            Order.AuxPrice        := Abs(SimpleRoundTo(aOrder.AuxPrice, -C_DECIMALS));
            Order.TrailingPercent := UNSET_DOUBLE;
          end
          else
          begin
            Order.AuxPrice        := UNSET_DOUBLE;
            Order.TrailingPercent := SimpleRoundTo(aOrder.TrailingPercent, -C_DECIMALS);
          end;
          Order.TrailStopPrice := SimpleRoundTo(aOrder.TrailStopPrice, -C_DECIMALS);
          Order.LmtPriceOffset := Abs(SimpleRoundTo(aOrder.LmtPriceOffset, -C_DECIMALS));
          Order.Price          := UNSET_DOUBLE;
        end;
      otPegMarket:
        begin
          Order.Price    := UNSET_DOUBLE;
          Order.AuxPrice := SimpleRoundTo(aOrder.AuxPrice, -C_DECIMALS);
        end;
      otPegPrimary, otPegMidPt:
        begin
          Order.Price    := SimpleRoundTo(aOrder.Limit, -C_DECIMALS);
          Order.AuxPrice := SimpleRoundTo(aOrder.AuxPrice, -C_DECIMALS);
        end;
    else
      begin
        Order.Price    := SimpleRoundTo(aOrder.Limit, -C_DECIMALS);
        Order.AuxPrice := SimpleRoundTo(aOrder.AuxPrice, -C_DECIMALS);
      end;
    end;
  finally
    Result := Order;
  end;
end;

procedure TOrderIBDoc.AssignFrom(const aSource: TCustomDocument);
var
  Source: TOrderIBDoc absolute aSource;
begin
  inherited AssignFrom(aSource);
  Self.AdvancedOrderType := Source.AdvancedOrderType;
  Self.AuxPrice          := Source.AuxPrice;
  Self.DateStart         := Source.DateStart;
  Self.DateStop          := Source.DateStop;
  Self.ExtendedOptions   := Source.ExtendedOptions;
  Self.LmtPriceOffset    := Source.LmtPriceOffset;
  Self.MaxNumAmount      := Source.MaxNumAmount;
  Self.MaxNumShares      := Source.MaxNumShares;
  Self.Multiplier        := Source.Multiplier;
  Self.OcaGroupNumber    := Source.OcaGroupNumber;
  Self.OrderStartEnabled := Source.OrderStartEnabled;
  Self.OrderStopEnabled  := Source.OrderStopEnabled;
  Self.ParentIBId        := -1;
  Self.PrimaryExchange   := Source.PrimaryExchange;
  Self.RateType          := Source.RateType;
  Self.Scope             := Source.Scope;
  Self.SecurityType      := Source.SecurityType;
  Self.TimeInForce       := Source.TimeInForce;
  Self.TimeStart         := Source.TimeStart;
  Self.TimeStop          := Source.TimeStop;
  Self.TrailingPercent   := Source.TrailingPercent;
  Self.TrailStopPrice    := Source.TrailStopPrice;
end;

procedure TOrderIBDoc.Buy;
var
  Monitor: IMonitor;
  OrderGroupNode: PVirtualNode;
  OrderGroupData: PTreeData;
begin
  if Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    OrderGroupNode := Monitor.GetParentNode(OwnerNode, ntOrderGroup);
    if Assigned(OrderGroupNode) then
    begin
      OrderGroupData := OrderGroupNode^.GetData;
      if (OrderGroupData.OrderGroupDoc.Kind = okModifyOrder) then
//        OrderGroupData.OrderGroupDoc.AddOrderToQueue(Self)
        TTask.Create(
          procedure()
          begin
            Sleep(OrderGroupData.OrderGroupDoc.CheckpointPeriod);
            TThread.Synchronize(nil,
              procedure
              begin
                if Monitor.IsCreateOrModify(OwnerNode, OrderGroupNode) then
                  DoBuy;
              end);
          end).Start
      else
        DoBuy;
    end
    else
      DoBuy;
  end;
end;

procedure TOrderIBDoc.UpdateOrder;
var
  Order : TIABOrder;
  sInfo : string;
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'UpdateOrder');
  if not (OrderStatus in [osFilled, osCancelled, osPendCancel, osNotConsidered]) and (IABClient.Orders.Count > 0) and (OrderIBId > 0) then
  begin
    Order := IABClient.Orders.GetOrder(OrderIBId, IABClient.ClientId);
    if Assigned(Order) then
    begin
      if (Limit = 0) then
        Limit := UNSET_DOUBLE
      else
        Limit := SimpleRoundTo(Limit, -C_DECIMALS);
      if (AuxPrice = 0) then
        AuxPrice := UNSET_DOUBLE
      else
        AuxPrice := SimpleRoundTo(Limit, -C_DECIMALS);
      if IABClient.ModifyOrder(Order.TempId, Order.ClientId, Quantity, otNoChange, Limit, AuxPrice) then
      begin
        sInfo := IABClient.GetTextOrderInfo(osPreSubmit, Order);
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'UpdateOrder', sInfo);
        if Assigned(OwnerNode) then
          TMonitorLists.OrderList.AddStatus(OwnerNode, Order.TempId, 0, 0, sInfo, 'modifyingOrder', -1, osPreSubmit, 0);
      end;
    end;
  end;
  TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'UpdateOrder');
end;

procedure TOrderIBDoc.DoBuy;

  procedure SetCalculationStages;
  begin
    if (CalculationStage[csLatestFillPrice] = 0) then
      CalculationStage[csLatestFillPrice] := LatestFillPrice;
    if (CalculationStage[csLimitPrice] = 0) then
      CalculationStage[csLimitPrice] := Limit;
    if (CalculationStage[csAuxPrice] = 0) then
      CalculationStage[csAuxPrice] := AuxPrice;
    if (CalculationStage[csTrailStopPrice] = 0) then
      CalculationStage[csTrailStopPrice] := TrailStopPrice;
    if (CalculationStage[csAvgPrice] = 0) then
      CalculationStage[csAvgPrice] := AvgPrice;
    if (CalculationStage[csLimitPriceOffset] = 0) then
      CalculationStage[csLimitPriceOffset] := LmtPriceOffset;
  end;

var
  ChildData      : PTreeData;
  ChildList      : TList;
  ChildNode      : PVirtualNode;
  ChildOrder     : TIABOrder;
  ChildOrderIBID : Integer;
  i              : Integer;
  Monitor        : IMonitor;
  Order          : TIABOrder;
  sInfo          : string;
  sLogText       : string;
begin
  SetCalculationStages;
  if not IsPossibleToBuyOrder(OwnerNode) then
  begin
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'DoBuy', THtmlLib.GetColorTag('Parent order is advanced type and not executed', clMaroon));
    Exit;
  end;

  if Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    if (OrderStatus in [osFilled, osCancelled, osPendCancel, osError, osNotConsidered]) then
      Exit;

     if (Scope < C_SCOPE_ALL_OR_NONE) then
     begin
       TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, Self, 'DoBuy', 'Set value of Scope = "ALL OR NONE" (0)');
       Scope := C_SCOPE_ALL_OR_NONE;
     end;

     if (OrderType = otUnknown) then
     begin
       TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, Self, 'DoBuy', 'Set value of OrderType = otMarket');
       OrderType := otMarket;
     end;

     if (TimeInForce < 0) then
     begin
       TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, Self, 'DoBuy', 'Set value of Time in market = Day');
       TimeInForce := Ord(tifDay);
     end;

    TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'DoBuy');
    if Assigned(OwnerNode) then
      TThread.Queue(nil,
        procedure
        begin
          Monitor.SetChildsFreeze(OwnerNode);
          OwnerNode.CheckState := csCheckedNormal;
        end);

    if (not IsExecuted) then
    begin
      if (Self.AdvancedOrderType = atBracket) then
        Self.Transmit := False
      else
        Self.Transmit := True;
      Order := CreateIABOrder(Self);

      ChildOrderIBID := -1;
      if Assigned(Order) then
      begin
        if Self.SecurityType in [stIndex, stFuture, stOption] then
          Order.LocalSymbol := Self.LocalSymbol;
        try
          OrderIBId := IABClient.PlaceOrder(Order);
          IABClient.SetOCAGroup(OrderIBId, Self.OcaGroupNumber.ToString);
          sLogText := 'Place Master OrderID=' + OrderIBId.ToString +
                      ', Symbol=' + Self.Symbol +
                      ', OrderType=' + OrderTypeToStr(OrderType) +
                      ', OCAGroup=' +  Self.OcaGroupNumber.ToString +
                      ', Order scope="' + System.StrUtils.IfThen(Self.Scope >= 0, cScopeToString[Self.Scope], '' ) +
                      ', Quantity=' + Quantity.ToString +
                      ', Transmit=' + BoolToStr(Transmit, True);
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'Do' + Self.OrderAction.ToString.ToUpper, sLogText);
          sLogText := 'OrderID=' + OrderIBId.ToString +
                      ', LatestFillPrice=' + LatestFillPrice.ToString +
                      ', LimitPrice=' + Limit.ToString +
                      ', AuxPrice=' + AuxPrice.ToString +
                      ', TrailStopPrice=' + TrailStopPrice.ToString +
                      ', TrailingPercent=' + TrailingPercent.ToString;
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'Do' + Self.OrderAction.ToString.ToUpper, sLogText);

          IsExecuted  := True; // temporärt skall tilldelas false efter ib konfirmerat
          if (OrderIBId > 0) and Assigned(OwnerNode) then
          begin
            Order.ContractId := Self.Id;
            Order.TempId := OrderIBId;
            sInfo := Concat('OrderName=', Self.Description.Trim, ',', IABClient.GetTextOrderInfo(osPendSubmit, Order));
            TMonitorLists.OrderList.AddStatus(OwnerNode, OrderIBId, 0, 0, sInfo, 'addOrder', -1, osPendSubmit, 0);

            if (AdvancedOrderType <> atNone) then
            begin
              ChildList := TList.Create;
              try
                ChildNode := OwnerNode.FirstChild;
                while Assigned(ChildNode) do
                begin
                  ChildData := ChildNode^.GetData;
                  if Assigned(ChildData) and Assigned(ChildData.OrderDoc) and
                    (ChildData.OrderDoc is TOrderIBDoc) and
                    (ChildData.OrderDoc.OrderStatus = osSleeping) and
                    (not Monitor.ExistsChildConditions(ChildNode)) then
                    ChildList.Add(ChildNode);
                  ChildNode := ChildNode.NextSibling;
                end;

                if (ChildList.Count > 0) then    //send parent order then sleep 50 ms
                begin
                  TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'Do' + Self.OrderAction.ToString.ToUpper, 'Sleep 50 ms');
                  Sleep(50);
                end
                else
                begin
                  TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'Do' + Self.OrderAction.ToString.ToUpper, 'OrderID=' + OrderIBId.ToString + ', ChildCount=0, Transmit=True');
                  Self.Transmit := True;
                end;

                for i := 0 to ChildList.Count - 1 do
                begin
                  ChildNode := ChildList.Items[i];
                  ChildData := ChildNode^.GetData;
                  TOrderIBDoc(ChildData.OrderDoc).ParentIBId := OrderIBId;
                  ChildOrder := CreateIABOrder(TOrderIBDoc(ChildData.OrderDoc));
                  case AdvancedOrderType of
                    atHedging:
                      ChildOrder.HedgeType := htFX;
                    atBracket:
                      ChildOrder.Transmit := i = ChildList.Count - 1;
                  end;
                  ChildOrderIBID := IABClient.PlaceOrder(ChildOrder);
                  IABClient.SetOCAGroup(ChildOrderIBID, TOrderIBDoc(ChildData.OrderDoc).OcaGroupNumber.ToString);
                  if (ChildOrderIBID > 0) then
                  begin
                    sLogText := 'Place Child OrderID=' + ChildOrderIBID.ToString +
                                ', Symbol=' + ChildData.OrderDoc.Symbol +
                                ', Parent Order ID=' + OrderIBId.ToString +
                                ', OrderType=' + OrderTypeToStr(ChildData.OrderDoc.OrderType) +
                                ', OCAGroup=' +  TOrderIBDoc(ChildData.OrderDoc).OcaGroupNumber.ToString +
                                ', Order scope=' + System.StrUtils.IfThen(TOrderIBDoc(ChildData.OrderDoc).Scope >= 0, cScopeToString[TOrderIBDoc(ChildData.OrderDoc).Scope], '') +
                                ', Quantity=' + ChildData.OrderDoc.Quantity.ToString +
                                ', Transmit=' + BoolToStr(ChildOrder.Transmit, True);
                    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'Do' + Self.OrderAction.ToString.ToUpper, sLogText);

                    sLogText := 'OrderID=' + ChildOrderIBID.ToString +
                                ', LatestFillPrice=' + ChildData.OrderDoc.LatestFillPrice.ToString +
                                ', LimitPrice=' + TOrderIBDoc(ChildData.OrderDoc).Limit.ToString +
                                ', AuxPrice=' + TOrderIBDoc(ChildData.OrderDoc).AuxPrice.ToString +
                                ', TrailStopPrice=' + TOrderIBDoc(ChildData.OrderDoc).TrailStopPrice.ToString +
                                ', TrailingProcent=' + TOrderIBDoc(ChildData.OrderDoc).TrailingPercent.ToString +
                                ', OrderType=' + OrderTypeToStr(ChildData.OrderDoc.OrderType) +
                                ', OCAGroup=' +  TOrderIBDoc(ChildData.OrderDoc).OcaGroupNumber.ToString;
                    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'Do' + Self.OrderAction.ToString.ToUpper, sLogText);

                    ChildData.OrderDoc.OrderStatus := osPendSubmit;
                    ChildData.OrderDoc.OrderIBId   := ChildOrderIBID;
                    ChildOrder.ContractId := ChildData.OrderDoc.Id;
                    ChildOrder.TempId := ChildOrderIBID;

                    sInfo := Concat('OrderName=', ChildData.OrderDoc.Description, ',', IABClient.GetTextOrderInfo(osPendSubmit, ChildOrder));
                    TMonitorLists.OrderList.AddStatus(ChildNode, ChildOrderIBID, 0, 0, sInfo, 'addOrder', -1, osPendSubmit, 0);
                    TMonitorLists.OrderList.SetLastStatus(ChildNode, osPendSubmit);
                    ChildOrderIBID := 0;

                    TThread.Queue(nil,
                      procedure
                      begin
                        Monitor.SetChildsFreeze(ChildNode);
                        ChildNode.CheckState := csCheckedNormal;
                      end);
                  end;
                end;
              finally
                FreeAndNil(ChildList);
              end;
            end;
          end;
        finally
          FreeAndNil(Order);
        end;
      end;
    end;

    if (OrderIBId > 0) then
    begin
      TMonitorLists.OrderList.SetLastStatus(OwnerNode, osPendSubmit);
      OrderStatus := osPendSubmit;
      Monitor.SetChildsFreeze(OwnerNode);
      Monitor.OrdersExecuteOneCancelAll(OwnerNode);
      Beep;
    end;
  end;
  TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'DoBuy');
end;

procedure TOrderIBDoc.FromDB(const aRecordId: Integer);
resourcestring
  C_SQL_TEXT = 'SELECT * FROM ORDERS WHERE ID=:RecordId';

  procedure FillExtendedOptions;
  var
    XMLFile: TXMLFile;
  begin
    XMLFile := TXMLFile.Create;
    try
      XMLFile.XMLText := Self.XmlParams;
      FExtendedOptions.AuxBasePrice       := TBasePrice(XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_AUXPRICE_BP, Ord(pbLast)));
      FExtendedOptions.LimitBasePrice     := TBasePrice(XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_LIMIT_BP, Ord(pbLast)));
      FExtendedOptions.LmtOffsetBasePrice := TBasePrice(XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_LMTOFFSET_BP, Ord(pbLast)));
      FExtendedOptions.TrailStopBasePrice := TBasePrice(XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_TRAILSTOP_BP, Ord(pbLast)));
      FExtendedOptions.BasisForPrice      := TBasisForPrice(XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_BASIS_FOR_PRICE, Ord(bpFillPrice)));
    finally
      FreeAndNil(XMLFile);
    end;
  end;

var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_TEXT;
    try
      Query.ParamByName('RecordId').AsInteger := aRecordId;
      Query.Prepare;
      Query.Open;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'FromDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;

    if not Query.IsEmpty then
    begin
      Query.First;
      Self.RecordId := aRecordId;
      if (Query.FieldByName('BROKER_ID').AsInteger = Ord(TBrokerType.brIB)) then
      begin
        Self.AdvancedOrderType := TAdvancedOrderType(Query.FieldByName('ADVANCED_ORDER_TYPE').AsInteger);
        Self.OrderType         := TIABOrderType(Query.FieldByName('ORDER_TYPE').AsInteger);
        Self.Decimals          := Query.FieldByName('DECIMALS').AsInteger;
        Self.InstrumentName    := Query.FieldByName('INSTRUMENT_NAME').AsString;
        Self.BrokerType        := TBrokerType.FromInteger(Query.FieldByName('BROKER_ID').AsInteger);
        Self.Description       := Query.FieldByName('DESCRIPTION').AsString;
        Self.Quantity          := Query.FieldByName('QUANTITY').AsInteger;
        Self.Exchange          := Query.FieldByName('EXCHANGE').AsString;
        Self.Id                := Query.FieldByName('CONTRACT_ID').AsInteger;
        Self.AuxPrice          := Query.FieldByName('AUX_PRICE').AsFloat;
        Self.Scope             := Query.FieldByName('SCOPE').AsInteger;
        Self.TimeInForce       := Query.FieldByName('TIME_IN_FORCE').AsInteger;
        Self.TimeStart         := Query.FieldByName('TIME_START').AsDateTime;
        Self.TimeStop          := Query.FieldByName('TIME_STOP').AsDateTime;
        Self.OcaGroupNumber    := StrToIntDef(Query.FieldByName('OCA_GROUP_NUMBER').AsString, -1);
        Self.LmtPriceOffset    := Query.FieldByName('LMT_PRICE_OFFSET').AsFloat;
        Self.Limit             := Query.FieldByName('LIMIT_PRICE').AsFloat;
        Self.Symbol            := Query.FieldByName('INSTRUMENT_NAME').AsString;
        Self.SecurityType      := TIABSecurityType(Query.FieldByName('SECURITY_TYPE').AsInteger);
        Self.MaxNumAmount      := Query.FieldByName('MAX_NUM_AMOUNT').AsInteger;
        Self.MaxNumShares      := Query.FieldByName('MAX_NUM_SHARES').AsInteger;
        Self.TrailingPercent   := Query.FieldByName('TRAILING_PERCENT').AsFloat;
        Self.TrailStopPrice    := Query.FieldByName('TRAIL_STP_PRICE').AsFloat;
        Self.TriggerMethod     := TTriggerMethod(Query.FieldByName('TRIGGER_METHOD').AsInteger);
        Self.IsActivateChild   := Query.FieldByName('IS_ACTIVATE_CHILD').AsBoolean;
        Self.MarketList        := '';
        Self.RateType          := TRateType(Query.FieldByName('RATE_TYPE').AsInteger);
        Self.XmlParams         := Query.FieldByName('XML_PARAMS').AsString;
        FillExtendedOptions;
        if Query.FieldByName('ORDER_ACTION').AsBoolean then
          Self.OrderAction := iabBuy
        else
          Self.OrderAction := iabSell;
        Self.Instrument.SokidInfo.ContractId := Query.FieldByName('INSTRUMENT').AsInteger;
        if SokidList.ContainsKey(Self.Instrument.SokidInfo.ContractId) then
          Self.Instrument.SokidInfo.AssignFrom(SokidList.Items[Self.Instrument.SokidInfo.ContractId]);
        Self.VisiblePart       := Query.FieldByName('VISIBLE_PART').AsFloat;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
  if SokidList.ContainsKey(Self.Id) then
  begin
    Self.Expiry     := SokidList.Items[Self.Id].Expiry;
    Self.Multiplier := VarToIntDef(SokidList.Items[Self.Id].Multiplier, 0);
  end;
end;

destructor TOrderIBDoc.Destroy;
begin

  inherited;
end;

function TOrderIBDoc.GetOrderStatusText: string;
begin
  Result := OrderStatus.ToString;
end;

function TOrderIBDoc.GetLocalSymbol: string;
begin
  if FLocalSymbol.IsEmpty and SokidList.ContainsKey(Self.Id) then
    FLocalSymbol := SokidList.Items[Self.Id].LocalSymbol;
  Result := FLocalSymbol;
end;

procedure TOrderIBDoc.SetLocalSymbol(const Value: string);
begin
  FLocalSymbol := Value;
end;

function TOrderIBDoc.IsPossibleToBuyOrder(aNode: PVirtualNode): Boolean;
var
  ParentNode: PVirtualNode;
  PrevNode: PVirtualNode;
  Data: PTreeData;
  Monitor: IMonitor;
begin
  Result := True;
  if Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    if Assigned(aNode) then
    begin
      ParentNode := Monitor.GetParentNode(aNode, ntOrder);
      if Assigned(ParentNode) then
      begin
        Data := ParentNode^.GetData;
        if Assigned(Data) and Assigned(Data.OrderDoc) and (Data.OrderDoc is TOrderIBDoc) then
        begin
          if (TOrderIBDoc(Data.OrderDoc).AdvancedOrderType = atNone) then
            Result := True
          else
            Result := (Data.OrderDoc.OrderStatus in [osFilled, osError, osCancelled]);
        end;
      end;

      //Check orders in sequential OrderGroups
      if Result then
      begin
        ParentNode := Monitor.GetParentNode(aNode, ntOrderGroup);
        if Assigned(ParentNode) then
        begin
          Data := ParentNode^.GetData;
          if Assigned(Data.OrderGroupDoc) and (Data.OrderGroupDoc.Kind in [okSequentialStop, okSequentialContinue]) then
          begin
            PrevNode := aNode.PrevSibling;
            while Assigned(PrevNode) do
            begin
              Data := PrevNode^.GetData;
              if (PrevNode.CheckState = csCheckedNormal) and Assigned(Data^.OrderDoc) then
              begin
                case Data^.OrderGroupDoc.Kind of
                  okSequentialContinue:
                    Result := Data^.OrderDoc.OrderStatus in [osFilled, osError, osCancelled, osPendCancel];
                  okSequentialStop:
                    Result := Data^.OrderDoc.OrderStatus in [osFilled];
                end;
                PrevNode := nil;
              end
              else
                PrevNode := PrevNode.PrevSibling;
            end;
          end;
        end;
      end;

    end;
  end;
  if Result then
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'IsPossibleToBuyOrder', THtmlLib.GetColorTag('Is possible to buy order', clNavy))
  else
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'IsPossibleToBuyOrder', THtmlLib.GetColorTag('Is not possible to buy order', clMaroon));
end;

procedure TOrderIBDoc.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM ORDERS WHERE ID=:RecordId';
  C_SQL_UPDATE_TEXT = 'UPDATE ORDERS SET '                       +
                      'ORDER_ACTION        = :OrderAction, '       +
                      'ADVANCED_ORDER_TYPE = :AdvancedOrderType, ' +
                      'AUX_PRICE           = :AuxPrice, '          +
                      'BROKER_ID           = :BrokerType, '        +
                      'CONTRACT_ID         = :ConId, '             +
                      'CURRENCY            = :Currency, '          +
                      'DECIMALS            = :Decimals, '          +
                      'DESCRIPTION         = :Description, '       +
                      'INSTRUMENT_NAME     = :InstrumentName, '    +
                      'EXCHANGE            = :Exchange, '          +
                      'LIMIT_PRICE         = :LimitPrice, '        +
                      'IS_ACTIVATE_CHILD   = :IsActivateChild, '   +
                      'LMT_PRICE_OFFSET    = :LmtPriceOffset, '    +
                      'MAX_NUM_AMOUNT      = :MaxNumAmount, '      +
                      'MAX_NUM_SHARES      = :MaxNumShares, '      +
                      'OCA_GROUP_NUMBER    = :OcaGroupNumber, '    +
                      'TIME_START          = :TimeStart, '         +
                      'TIME_STOP           = :TimeStop, '          +
                      'ORDER_TYPE          = :OrderType, '         +
                      'QUANTITY            = :Quantity, '          +
                      'SCOPE               = :Scope, '             +
                      'SYMBOL              = :Symbol, '            +
                      'LOCAL_SYMBOL        = :LocalSymbol, '       +
                      'XML_PARAMS          = :XmlParams, '         +
                      'TIME_IN_FORCE       = :TimeInForce, '       +
                      'TRAILING_PERCENT    = :TrailingProcent, '   +
                      'EXPIRY              = :Expiry, '            +
                      'RATE_TYPE           = :RateType, '          +
                      'SECURITY_TYPE       = :SecurityType, '      +
                      'TRAIL_STP_PRICE     = :TrailStopPrice, '    +
                      'IS_FINAL            = :IsFinal, '           +
                      'TRIGGER_METHOD      = :TriggerMethod, '     +
                      'INSTRUMENT          = :Instrument, '        +
                      'VISIBLE_PART        = :VisiblePart '        +
                      ' WHERE ID = :RecordId';
  C_SQL_INSERT_TEXT =  'INSERT INTO ORDERS '+
                      '(ID,ADVANCED_ORDER_TYPE,DESCRIPTION,ORDER_ACTION,CONTRACT_ID,DECIMALS,INSTRUMENT_NAME,QUANTITY,'+
                      'LIMIT_PRICE,AUX_PRICE,TRAIL_STP_PRICE,TRAILING_PERCENT,ORDER_TYPE,TIME_IN_FORCE,IS_ACTIVATE_CHILD,'+
                      'SCOPE,XML_PARAMS,OCA_GROUP_NUMBER,TIME_START,TIME_STOP,MAX_NUM_SHARES,MAX_NUM_AMOUNT,TRIGGER_METHOD,'+
                      'BROKER_ID,SYMBOL,LOCAL_SYMBOL,CURRENCY,EXPIRY,LMT_PRICE_OFFSET,RATE_TYPE,SECURITY_TYPE,IS_FINAL,EXCHANGE,'+
                      'INSTRUMENT,VISIBLE_PART)' +
                      ' VALUES(:RecordId, '          +
                      '        :AdvancedOrderType, ' +
                      '        :Description, '       +
                      '        :OrderAction, '       +
                      '        :ConId, '             +
                      '        :Decimals, '          +
                      '        :InstrumentName, '    +
                      '        :Quantity, '          +
                      '        :LimitPrice, '        +
                      '        :AuxPrice, '          +
                      '        :TrailStopPrice, '    +
                      '        :TrailingProcent, '   +
                      '        :OrderType, '         +
                      '        :TimeInForce, '       +
                      '        :IsActivateChild, '   +
                      '        :Scope, '             +
                      '        :XmlParams, '         +
                      '        :OcaGroupNumber, '    +
                      '        :TimeStart, '         +
                      '        :TimeStop, '          +
                      '        :MaxNumShares, '      +
                      '        :MaxNumAmount, '      +
                      '        :TriggerMethod, '     +
                      '        :BrokerType, '        +
                      '        :Symbol, '            +
                      '        :LocalSymbol, '       +
                      '        :Currency, '          +
                      '        :Expiry, '            +
                      '        :LmtPriceOffset, '    +
                      '        :RateType, '          +
                      '        :SecurityType, '      +
                      '        :IsFinal, '           +
                      '        :Exchange,'           +
                      '        :Instrument,'         +
                      '        :VisiblePart)';

  procedure FillXmlParams;
  var
    XMLFile: TXMLFile;
  begin
    XMLFile := TXMLFile.Create;
    try
      XMLFile.XMLText := Self.XmlParams;
      XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_AUXPRICE_BP, Ord(Self.ExtendedOptions.AuxBasePrice));
      XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_LIMIT_BP, Ord(Self.ExtendedOptions.LimitBasePrice));
      XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_LMTOFFSET_BP, Ord(Self.ExtendedOptions.LmtOffsetBasePrice));
      XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_TRAILSTOP_BP, Ord(Self.ExtendedOptions.TrailStopBasePrice));
      XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_BASIS_FOR_PRICE, Ord(Self.ExtendedOptions.BasisForPrice));
      Self.XmlParams := XMLFile.XMLText;
    finally
      FreeAndNil(XMLFile);
    end;
  end;

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
    if (RecordId > 0) then
    begin
      Query.SQL.Text := C_SQL_EXISTS_TEXT;
      try
        Query.ParamByName('RecordId').AsInteger := RecordId;
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
    end
    else
      RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');

    if IsExists then
      Query.SQL.Text := C_SQL_UPDATE_TEXT
    else
      Query.SQL.Text := C_SQL_INSERT_TEXT;

    Query.ParamByName('RecordId').AsInteger          := RecordId;
    Query.ParamByName('AdvancedOrderType').AsInteger := Integer(Self.AdvancedOrderType);
    Query.ParamByName('AuxPrice').AsFloat            := IfThen(Self.AuxPrice = UNSET_DOUBLE, 0, Self.AuxPrice);
    Query.ParamByName('BrokerType').AsInteger        := Ord(Self.BrokerType);
    Query.ParamByName('ConId').AsInteger             := Self.Id;
    Query.ParamByName('Currency').AsString           := Self.Currency.Substring(0, 10);
    Query.ParamByName('Decimals').AsInteger          := Self.Decimals;
    Query.ParamByName('Description').AsString        := Self.Description.Substring(0, 50);
    Query.ParamByName('InstrumentName').AsString     := Self.InstrumentName.Substring(0, 50);
    Query.ParamByName('IsActivateChild').AsBoolean   := Self.IsActivateChild;
    Query.ParamByName('IsFinal').AsBoolean           := Self.IsFinal;
    Query.ParamByName('LimitPrice').AsFloat          := Self.Limit;
    Query.ParamByName('LmtPriceOffset').AsFloat      := IfThen(Self.LmtPriceOffset = UNSET_DOUBLE, 0, Self.LmtPriceOffset);
    Query.ParamByName('LocalSymbol').AsString        := Self.LocalSymbol.Substring(0, 40);
    Query.ParamByName('MaxNumAmount').AsInteger      := Self.MaxNumAmount;
    Query.ParamByName('MaxNumShares').AsInteger      := Self.MaxNumShares;
    Query.ParamByName('OcaGroupNumber').AsInteger    := Self.OcaGroupNumber;
    Query.ParamByName('OrderAction').AsBoolean       := Self.OrderAction = iabBuy;
    Query.ParamByName('OrderType').AsInteger         := Integer(Self.OrderType);
    Query.ParamByName('Quantity').AsInteger          := Self.Quantity;
    Query.ParamByName('RateType').AsInteger          := Ord(Self.RateType);
    Query.ParamByName('Scope').AsInteger             := Self.Scope;
    Query.ParamByName('SecurityType').AsInteger      := Integer(Self.SecurityType);
    Query.ParamByName('Symbol').AsString             := Self.Symbol.Substring(0, 40);
    Query.ParamByName('TimeInForce').AsInteger       := Self.TimeInForce;
    Query.ParamByName('TimeStart').AsDateTime        := Self.TimeStart;
    Query.ParamByName('TimeStop').AsDateTime         := Self.TimeStop;
    Query.ParamByName('TrailingProcent').AsFloat     := IfThen(Self.TrailingPercent = UNSET_DOUBLE, 0, Self.TrailingPercent);
    Query.ParamByName('TrailStopPrice').AsFloat      := IfThen(Self.TrailStopPrice = UNSET_DOUBLE, 0, Self.TrailStopPrice);
    Query.ParamByName('TriggerMethod').AsInteger     := Integer(Self.TriggerMethod);
    if (Self.Instrument.SokidInfo.ContractId > 0) then
      Query.ParamByName('Instrument').AsInteger := Self.Instrument.SokidInfo.ContractId
    else
      Query.ParamByName('Instrument').Clear;
    FillXmlParams;
    Query.ParamByName('XmlParams').AsString          := Self.XmlParams;
    if (Self.Expiry > 0) then
      Query.ParamByName('Expiry').AsDateTime := Self.Expiry
    else
      Query.ParamByName('Expiry').Clear;
    Query.ParamByName('VisiblePart').AsFloat         := Self.VisiblePart;
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveToDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TOrderIBDoc.SetSecurityType(const Value: string);
begin
  Self.SecurityType := TIABSecurityType.FromString(Value);
end;

function TOrderIBDoc.ToString: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.Append(inherited ToString)
      .AppendFormat('LocalSymbol=%s, ', [Self.LocalSymbol])
      .AppendFormat('AuxPrice=%f, ', [Self.AuxPrice])
      .AppendFormat('LmtPriceOffset=%f, ', [Self.LmtPriceOffset])
      .AppendFormat('TrailStopPrice=%f, ', [Self.TrailStopPrice])
      .AppendFormat('TrailingPercent=%f, ', [Self.TrailingPercent])
      .AppendFormat('ParentIBId=%d, ', [Self.ParentIBId])
      .AppendFormat('OcaGroupNumber=%d, ', [Self.OcaGroupNumber])
      .AppendFormat('SecurityType=%s, ', [Self.SecurityType.ToString]);
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

function TOrderIBDoc.ToValueString: string;
var
  sb: TStringBuilder;
  IsTemplate: Boolean;
begin
  sb := TStringBuilder.Create;
  try
    IsTemplate := (Self.Id = 0) or Self.Symbol.IsEmpty;

    sb.AppendFormat('%d / %d, ', [Trunc(Self.Filled), Self.Quantity])
      .AppendFormat('Avg: %f ', [Self.AvgPrice])
      .Append(Self.OrderStatus.ToString).Append(' ')
      .Append(Self.OrderAction.ToString.ToUpper).Append(' ')
      .Append(Self.SecurityType.ToString).Append(' ')
      .Append(Self.OrderType.ToShortString);
    case Self.OrderType of
      otMarket:
        begin

        end;
      otMarketOpen:
        begin
        end;
      otLimit:
        begin
          sb.AppendFormat(' Lmt: %f', [SimpleRoundTo(Self.Limit, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
          sb.Append(' ').Append(Self.ExtendedOptions.LimitBasePrice.ToString);
        end;
      otStop:
        begin
          sb.AppendFormat(' Aux: %f', [SimpleRoundTo(Self.AuxPrice, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
          sb.Append(' ').Append(Self.ExtendedOptions.AuxBasePrice.ToString);
        end;
      otStopLimit:
        begin
          sb.AppendFormat(' Aux: %f', [SimpleRoundTo(Self.AuxPrice, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
          sb.Append(' ').Append(Self.ExtendedOptions.AuxBasePrice.ToString);
          sb.AppendFormat(' Lmt: %f', [SimpleRoundTo(Self.Limit, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
          sb.Append(' ').Append(Self.ExtendedOptions.LimitBasePrice.ToString);
        end;
      otTrail:
        begin
          if (Self.RateType = rtValue) then
          begin
            sb.AppendFormat(' Aux: %f', [SimpleRoundTo(Self.AuxPrice, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
            sb.Append(' ').Append(Self.ExtendedOptions.AuxBasePrice.ToString);
          end
          else
          begin
            sb.AppendFormat(' TrailPerc: %f', [SimpleRoundTo(Self.TrailingPercent, -C_DECIMALS)]);
          end;
          if (Self.TrailStopPrice > 0) then
          begin
            sb.AppendFormat(' TrailStp: %f', [SimpleRoundTo(Self.TrailStopPrice, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
            sb.Append(' ').Append(Self.ExtendedOptions.TrailStopBasePrice.ToString);
          end;
        end;
      otTrailLimit:
        begin
          if (Self.RateType = rtValue) then
          begin
            sb.AppendFormat('Aux: %f', [SimpleRoundTo(Self.AuxPrice, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
            sb.Append(' ').Append(Self.ExtendedOptions.AuxBasePrice.ToString);
          end
          else
          begin
            sb.AppendFormat(' TrailPerc: %f', [SimpleRoundTo(Self.TrailingPercent, -C_DECIMALS)]);
          end;
          sb.AppendFormat(' TrailStp: %f', [SimpleRoundTo(Self.TrailStopPrice, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
          sb.Append(' ').Append(Self.ExtendedOptions.TrailStopBasePrice.ToString);
          sb.AppendFormat(' LmtOffset: %f', [SimpleRoundTo(Self.LmtPriceOffset, -C_DECIMALS)]);
          sb.Append(' ').Append(Self.ExtendedOptions.LmtOffsetBasePrice.ToString);
        end;
      otPegMarket:
        begin
          sb.AppendFormat('Aux: %f', [SimpleRoundTo(Self.AuxPrice, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
          sb.Append(' ').Append(Self.ExtendedOptions.AuxBasePrice.ToString);
        end;
      otPegPrimary, otPegMidPt:
        begin
          sb.AppendFormat(' Lmt: %f', [SimpleRoundTo(Self.Limit, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
          sb.Append(' ').Append(Self.ExtendedOptions.LimitBasePrice.ToString);
          sb.AppendFormat('Aux: %f', [SimpleRoundTo(Self.AuxPrice, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
          sb.Append(' ').Append(Self.ExtendedOptions.AuxBasePrice.ToString);
        end;
    else
      sb.AppendFormat(' Lmt: %f', [SimpleRoundTo(Self.Limit, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
      sb.Append(' ').Append(Self.ExtendedOptions.LimitBasePrice.ToString);
      sb.AppendFormat('Aux: %f', [SimpleRoundTo(Self.AuxPrice, -C_DECIMALS)]).Append(IfThen(IsTemplate, '%', ''));
      sb.Append(' ').Append(Self.ExtendedOptions.AuxBasePrice.ToString);
    end;

    sb.AppendFormat(' TIF:%s', [TimeInForceString[TIABTimeInForce(Self.TimeInForce)]])
      .AppendFormat(' MAX:%s', [Self.MaxNumAmount.ToString + '/' + Self.MaxNumShares.ToString])
      .AppendFormat(' TRIG:%s', [Self.TriggerMethod.ToString]);
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

{ TCustomDocument }

procedure TCustomDocument.AssignFrom(const aSource: TCustomDocument);
begin
  Self.AutoTradesInstance := aSource.AutoTradesInstance;
  Self.QualifierInstance  := aSource.QualifierInstance;
  Self.QualifierID        := aSource.QualifierID;
  Self.AutoTradesID       := aSource.AutoTradesID;
  Self.UseInAutoOrder     := aSource.UseInAutoOrder;
end;

constructor TCustomDocument.Create;
begin
  FOwnerNode := nil;
  FIsFrozen := False;
  FAutoTradesInstance := -1;
  FScanSequenceId := -1;
end;

function TCustomDocument.GetFreezeValueItem(Index: TIABTickType): Double;
begin
  Result := FFreezeValue[Index];
end;

function TCustomDocument.GetValueItem(Index: TIABTickType): Double;
begin
  Result := FTickValue[Index];
end;

procedure TCustomDocument.SetEnabled(const Value: Boolean);
begin
  if (FEnabled <> Value) then
    FEnabled := Value;
end;

procedure TCustomDocument.SetValueItem(Index: TIABTickType; const Value: Double);
begin
  FTickValue[Index] := Value;
end;

function TCustomDocument.ToValueString: string;
begin
  Result := '';
end;

procedure TCustomDocument.Freeze;
var
  TickType: TIABTickType;
begin
  FIsFrozen := True;
  for TickType := System.Low(TIABTickType) to System.High(TIABTickType) do
    FFreezeValue[TickType] := FTickValue[TickType];
end;

{ TConditionDoc }

constructor TConditionDoc.Create;
begin
  inherited;
  FHistory := TList<TConditionHistory>.Create;
  ActivationValue        := 0;
  Active                 := True;
  CondType               := ctRealtimeValue;
  CondLimit              := 0;
  CondWidth              := 0;
  Description            := 'New condition';
  EndDate                := Date;
  EndTime                := Time;
  Gradient               := 0;
  FInitValue             := 0;
  IsBreakUp              := True;
  IsCondition            := False;
  IsLoadedHistoricalData := False;
  IsValueReady           := False;
  Monitoring             := 1800;
  KindCreation           := kcUser;
  Priority               := cpNormal;
  TrailBuy               := 0;
  TrailSell              := 0;
  StartDate              := Date;
  StartTime              := Time;
  UpProc                 := 0;
  TickType1              := ttLast;
  TickType2              := ttNotSet;
  FTypeOperation         := toDivide;//toNone;
  ValueList              := TObjectDictionary<TDateTime, TPrice>.Create([doOwnsValues]);
  InitTime               := Now;
  FDivisionValue         := 1;
  TickType1Value         := 0;
  TickType2Value         := 0;
end;

destructor TConditionDoc.Destroy;
begin
  FreeAndNil(ValueList);
  FreeAndNil(FHistory);
  inherited;
end;

procedure TConditionDoc.FromDB(const aRecordId: Integer);
resourcestring
  C_SQL_TEXT = 'SELECT * FROM CONDITION WHERE ID=:RecordId';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_TEXT;
    Query.ParamByName('RecordId').AsInteger := aRecordId;
    try
      Query.Prepare;
      Query.Open;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'FromDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;

    Query.First;
    if not Query.IsEmpty then
    begin
      Self.Name                := Query.FieldByName('NAME').AsString;
      Self.Active              := Query.FieldByName('COND_ACTIVE').AsBoolean;
      Self.Bypass              := Query.FieldByName('IS_BYPASS').AsBoolean;
      Self.CondType            := TConditionType(Query.FieldByName('COND_TYPE').AsInteger);
      Self.CondLimit           := Query.FieldByName('COND_VALUE').AsFloat;
//      Self.CondLimitRelative   := Query.FieldByName('COND_VALUE_RELATIVE').AsFloat;
      Self.CondWidth           := Query.FieldByName('WIDTH').AsFloat;
      Self.Description         := Query.FieldByName('DESCRIPTION').AsString;
      Self.DivisionValue       := Query.FieldByName('DIVISION_VALUE').AsCurrency;
      Self.Duration            := TimeOf(Query.FieldByName('DURATION').AsDateTime);
      Self.EndDate             := Query.FieldByName('END_DATE').AsDateTime;
      Self.EndTime             := Query.FieldByName('END_TIME').AsDateTime;
      Self.Gradient            := Query.FieldByName('GRADIENT').AsFloat;
      Self.InequalityCor       := TInequalityType(Query.FieldByName('INEQUALITY_COL').AsInteger);
      Self.InequalityGr        := TInequalityType(Query.FieldByName('INEQUALITY_GR').AsInteger);
      Self.InequalityRt        := TInequalityType(Query.FieldByName('INEQUALITY_RT').AsInteger);
      Self.isBreakUp           := Query.FieldByName('IS_BREAKUP').AsBoolean;
      Self.KindCreation        := TKindCreation(Query.FieldByName('KIND_CREATION').AsInteger);
      Self.Monitoring          := Query.FieldByName('MONITORING').AsInteger;
      Self.Priority            := TPriority(Query.FieldByName('PRIORITY').AsInteger);
      Self.StartDate           := Query.FieldByName('START_DATE').AsDateTime;
      Self.StartTime           := Query.FieldByName('START_TIME').AsDateTime;
      Self.TickType1           := TIABTickType(Query.FieldByName('TICK_TYPE1').AsInteger);
      Self.TickType2           := TIABTickType(Query.FieldByName('TICK_TYPE2').AsInteger);
      Self.TrailBuy            := Query.FieldByName('TRAIL_BUY').AsFloat;
      Self.TrailSell           := Query.FieldByName('TRAIL_SELL').AsFloat;
      Self.TypeOperation       := TTypeOperation(Query.FieldByName('TYPE_OPERATION').AsInteger);
      Self.UpProc              := Query.FieldByName('UP_PROC').AsInteger;
      Self.XmlParams           := Query.FieldByName('XML_PARAMS').AsString;
      Self.IsCondition         := Self.Bypass;
      Self.GradientValue       := Query.FieldByName('GRADIENT_VALUE').AsFloat;
      Self.RealTimeType        := Query.FieldByName('REALTIME_TYPE').AsInteger;
      Self.TimeInForce         := Query.FieldByName('TIME_IN_FORCE').AsInteger;
      Self.ExtendOnLastPriceUp := Query.FieldByName('EXTEND_ON_LAST_PRICE_UP').AsBoolean;
      if (Self.EndDate < Today) then
        Self.EndDate := Today;
      if (Self.StartDate < Today) then
        Self.StartDate := Today;
      if (Self.DivisionValue = 0) then
        Self.DivisionValue := 1;
      Self.Instrument.SokidInfo.ContractId := Query.FieldByName('INSTRUMENT').AsInteger;
        if SokidList.ContainsKey(Self.Instrument.SokidInfo.ContractId) then
          Self.Instrument.SokidInfo.AssignFrom(SokidList.Items[Self.Instrument.SokidInfo.ContractId]);
    end;
    Self.RecordId := aRecordId;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TConditionDoc.AddToValueList(aTimeStamp: TDateTime; aPrice, aCoefficient: Double);
begin
  aTimeStamp := RoundToNearest(aTimeStamp, C_ROUND_SEC);
  if ValueList.ContainsKey(aTimeStamp) then
    ValueList.Items[aTimeStamp].Price := ValueList.Items[aTimeStamp].Price + aPrice * aCoefficient
  else
    ValueList.Add(aTimeStamp, TPrice.Create(aPrice * aCoefficient));
//  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'AddValue','aTimeStamp: ' + TimeToStr(aTimeStamp) +', aPrice: ' +aPrice.ToString + ' ,aDivisor: ' + aCoefficient.ToString);
end;

function TConditionDoc.ToString: string;
begin
   {Realtime value-"LAST>=138.55 amount(N)"
    Time gap       "2017-06-08 14:57-2017-06-08 15:57(P)"
    Gradient      "GRAD>4.2-MON=1800(v)"
    corridor      "GRAD>4.2-CORR>10-MON=1800(v)"
    Corridorbreak "GRAD>4.2-CORR>10-BR=UP-MON=1800(v)"
    Corridorposition "GRAD>4.2-CORR>10-POS=75-HOLD UP=500-MON=1800(v)"

    N, P , V FOR normal, priority, veto}

  Result :=  '';
  case Self.CondType of
    ctRealtimeValue:
      Result := IfThen(IsValueReady, FormatFloat('0.00 ', Self.CalcValue), 'n/a') + Self.InequalityRt.ToString + FormatFloat(' 0.00', Self.CondLimit);
    ctTimeGap:
      Result := FormatDateTime('yyyy-MM-dd HH:mm', Trunc(StartDate) + Frac(StartTime)) + FormatDateTime('-yyyy-MM-dd HH:mm', Trunc(EndDate) + Frac(EndTime));
    ctRealtimeAndTimeGap:
      Result := FormatFloat('0.00 ', Self.CalcValue) + Self.InequalityRt.ToString + FormatFloat(' 0.00', Self.CondLimit) + ' / ' +
                FormatDateTime('yyyy-MM-dd HH:mm', Trunc(StartDate) + Frac(StartTime)) + FormatDateTime('-yyyy-MM-dd HH:mm', Trunc(EndDate) + Frac(EndTime));
    ctGradient:
      Result := FormatFloat('0.00 ', Self.Gradient) + Self.InequalityRt.ToString + '-MON=' + Monitoring.ToString;
    ctTimeInForce:
      begin
        Result := '-SEC='+ TimeInForce.ToString;
        if ExtendOnLastPriceUp then
          Result := Result + '-EXTEND';
      end;
    ctCorridor:
      Result := 'GRAD ' + Self.InequalityGr.ToString + FormatFloat('0.00 ', Self.Gradient) +
                '-CORR ' + FormatFloat('0.00 ', Self.CondWidth) + Self.InequalityCor.ToString +
                '-MON=' + Monitoring.ToString;
    ctCorridorPosition:
      Result := 'GRAD ' + Self.InequalityGr.ToString + FormatFloat('0.00 ', Self.Gradient) +
                '-CORR ' + FormatFloat('0.00 ', Self.CondWidth) + Self.InequalityCor.ToString + '-MON=' + Monitoring.ToString +
                '-POS=' + UpProc.ToString;
  end;

  case Priority of
    cpNormal:
      Result := Result + ' (N)';
    cpPriority:
      Result := Result + ' (P)';
    cpVeto:
      Result := Result + ' (V)';
  end;
  if Self.Bypass then
    Result := Result + ' [BYPASS]';
end;

function TConditionDoc.GetMinMaxValueArrayItem(Index: TMinMaxValue): Double;
begin
  Result := FMinMaxValueArray[Index];
end;

function TConditionDoc.GetRealTimeTypeAsString: string;
begin
  case RealTimeType of
    0: Result := ' %';
    1: Result := ' Value';
    2: Result := ' MOAP';
  end;
end;

function TConditionDoc.GetTimeInForceActive: Boolean;
begin
  Result := SecondsBetween(TimeInForceStartTime, Now) > TimeInForce;
end;

procedure TConditionDoc.SetMinMaxValueArrayItem(Index: TMinMaxValue; const Value: Double);
begin
  FMinMaxValueArray[Index] := Value;
end;

procedure TConditionDoc.AssignFrom(const aSource: TCustomDocument);
var
  Source: TConditionDoc absolute aSource;
begin
  inherited AssignFrom(aSource);
  Self.Active        := Source.Active;
  Self.Bypass        := Source.Bypass;
  Self.CondLimit     := Source.CondLimit;
  Self.CondType      := Source.CondType;
  Self.CondWidth     := Source.CondWidth;
  Self.Description   := Source.Description;
  Self.DivisionValue := Source.DivisionValue;
  Self.EndDate       := Source.EndDate;
  Self.EndTime       := Source.EndTime;
  Self.Gradient      := Source.Gradient;
  Self.InequalityCor := Source.InequalityCor;
  Self.InequalityGr  := Source.InequalityGr;
  Self.InequalityRt  := Source.InequalityRt;
  Self.isBreakUp     := Source.isBreakUp;
  Self.Monitoring    := Source.Monitoring;
  Self.Priority      := Source.Priority;
  Self.StartDate     := Source.StartDate;
  Self.StartTime     := Source.StartTime;
  Self.TrailBuy      := Source.TrailBuy;
  Self.TrailSell     := Source.TrailSell;
  Self.UpProc        := Source.UpProc;
  Self.XmlParams     := Source.XmlParams;
  Self.IsCondition   := False;
end;

procedure TConditionDoc.BlankTrailParams;
begin
  FMinMaxValueArray := Default(TMinMaxValueArray);
  IsCondition := False;
end;

function TConditionDoc.GetValueArrayItem(Index: TConditionType): Double;
begin
  Result := FValueArray[Index];
end;

procedure TConditionDoc.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM CONDITION WHERE ID = :RecordId';
  C_SQL_UPDATE_TEXT = 'UPDATE CONDITION SET ' +
                      'NAME                = :Name, '              +
                      'COND_ACTIVE         = :Active, '            +
                      'IS_BREAKUP          = :isBreakUp, '         +
                      'DESCRIPTION         = :Description, '       +
                      'DURATION            = :Duration, '          +
                      'MONITORING          = :Monitoring, '        +
                      'UP_PROC             = :UpProc, '            +
                      'KIND_CREATION       = :KindCreation, '      +
                      'INEQUALITY_RT       = :InequalityRt, '      +
                      'INEQUALITY_GR       = :InequalityGr, '      +
                      'INEQUALITY_COL      = :InequalityCol, '     +
                      'IS_BYPASS           = :Bypass, '            +
                      'PRIORITY            = :Priority, '          +
                      'COND_VALUE          = :CondLimit, '         +
                      'COND_VALUE_RELATIVE = :CondValueRelative, ' +
                      'GRADIENT            = :Gradient, '          +
                      'WIDTH               = :CondWidth, '         +
                      'START_DATE          = :StartDate, '         +
                      'START_TIME          = :StartTime, '         +
                      'END_DATE            = :EndDate, '           +
                      'END_TIME            = :EndTime, '           +
                      'TRAIL_BUY           = :TrailBuy, '          +
                      'TRAIL_SELL          = :TrailSell, '         +
                      'TICK_TYPE1          = :TickType1, '         +
                      'TICK_TYPE2          = :TickType2, '         +
                      'TYPE_OPERATION      = :TypeOperation, '     +
                      'COND_TYPE           = :CondType, '          +
                      'DIVISION_VALUE      = :DivisionValue, '     +
                      'INSTRUMENT          = :Instrument, '        +
                      'GRADIENT_VALUE      = :GradientValue, '     +
                      'REALTIME_TYPE       = :RealTimeType, '      +
                      'TIME_IN_FORCE       = :TimeInForce,'        +
                      'EXTEND_ON_LAST_PRICE_UP = :ExtendOnLastPriceUp, '+
                      'XML_PARAMS          = :XmlParams '          +
                      ' WHERE ID = :RecordId';

  C_SQL_INSERT_TEXT = 'INSERT INTO CONDITION '+
                      '(NAME,COND_TYPE,DESCRIPTION,DURATION,COND_ACTIVE,IS_BREAKUP, ' +
                      'COND_VALUE,GRADIENT,WIDTH,MONITORING,UP_PROC,START_DATE,END_DATE,START_TIME,END_TIME,PRIORITY,' +
                      'KIND_CREATION,TRAIL_BUY,TRAIL_SELL,IS_BYPASS,TICK_TYPE1,TICK_TYPE2,TYPE_OPERATION,COND_VALUE_RELATIVE,' +
                      'INEQUALITY_RT, INEQUALITY_GR,INEQUALITY_COL,DIVISION_VALUE,XML_PARAMS,INSTRUMENT,GRADIENT_VALUE,REALTIME_TYPE,'+
                      'TIME_IN_FORCE, EXTEND_ON_LAST_PRICE_UP, ID)'  +
                      ' VALUES (:Name, '              +
                      '         :CondType, '          +
                      '         :Description, '       +
                      '         :Duration, '          +
                      '         :Active, '            +
                      '         :isBreakUp, '         +
                      '         :CondLimit, '         +
                      '         :Gradient, '          +
                      '         :CondWidth, '         +
                      '         :Monitoring, '        +
                      '         :UpProc, '            +
                      '         :StartDate, '         +
                      '         :EndDate, '           +
                      '         :StartTime, '         +
                      '         :EndTime, '           +
                      '         :Priority, '          +
                      '         :KindCreation, '      +
                      '         :TrailBuy, '          +
                      '         :TrailSell, '         +
                      '         :Bypass, '            +
                      '         :TickType1, '         +
                      '         :TickType2, '         +
                      '         :TypeOperation, '     +
                      '         :CondValueRelative, ' +
                      '         :InequalityRt, '      +
                      '         :InequalityGr, '      +
                      '         :InequalityCol, '     +
                      '         :DivisionValue, '     +
                      '         :XmlParams, '         +
                      '         :Instrument, '        +
                      '         :GradientValue, '     +
                      '         :RealTimeType, '      +
                      '         :TimeInForce, '       +
                      '         :ExtendOnLastPriceUp, '+
                      '         :RecordId)';
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
    if (RecordId > 0) then
    begin
      Query.SQL.Text := C_SQL_EXISTS_TEXT;
      try
        Query.ParamByName('RecordId').AsInteger := RecordId;
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
    end
    else
      RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');

    if IsExists then
      Query.SQL.Text := C_SQL_UPDATE_TEXT
    else
      Query.SQL.Text := C_SQL_INSERT_TEXT;

    Query.ParamByName('Name').AsString             := Self.Name;
    Query.ParamByName('Active').AsBoolean          := Self.Active;
    Query.ParamByName('Bypass').AsBoolean          := Self.Bypass;
    Query.ParamByName('CondType').AsInteger        := Integer(Self.CondType);
    Query.ParamByName('CondLimit').AsCurrency      := Self.CondLimit;
    Query.ParamByName('CondValueRelative').AsFloat := 0;//Self.CondValueRelative;
    Query.ParamByName('CondWidth').AsCurrency      := Self.CondWidth;
    Query.ParamByName('Description').AsString      := Self.Description.Substring(0, 200);
    Query.ParamByName('DivisionValue').AsCurrency  := Self.DivisionValue;
    Query.ParamByName('Duration').AsTime           := Self.Duration;
    Query.ParamByName('EndDate').AsDate            := Self.EndDate;
    Query.ParamByName('EndTime').AsTime            := Self.EndTime;
    Query.ParamByName('Gradient').AsCurrency       := Self.Gradient;
    Query.ParamByName('InequalityCol').AsInteger   := Integer(Self.InequalityCor);
    Query.ParamByName('InequalityGr').AsInteger    := Integer(Self.InequalityGr);
    Query.ParamByName('InequalityRt').AsInteger    := Integer(Self.InequalityRt);
    Query.ParamByName('isBreakUp').AsBoolean       := Self.isBreakUp;
    Query.ParamByName('KindCreation').AsInteger    := Integer(Self.KindCreation);
    Query.ParamByName('Monitoring').AsInteger      := Self.Monitoring;
    Query.ParamByName('Priority').AsInteger        := Integer(Self.Priority);
    Query.ParamByName('RecordId').AsInteger        := RecordId;
    Query.ParamByName('StartDate').AsDate          := Self.StartDate;
    Query.ParamByName('StartTime').AsTime          := Self.StartTime;
    Query.ParamByName('TickType1').AsInteger       := Integer(Self.TickType1);
    Query.ParamByName('TickType2').AsInteger       := Integer(Self.TickType2);
    Query.ParamByName('TrailBuy').AsFloat          := Self.TrailBuy;
    Query.ParamByName('TrailSell').AsFloat         := Self.TrailSell;
    Query.ParamByName('TypeOperation').AsInteger   := Integer(Self.TypeOperation);
    Query.ParamByName('UpProc').AsInteger          := Self.UpProc;
    Query.ParamByName('XmlParams').AsString        := Self.XmlParams;
    if (Self.Instrument.SokidInfo.ContractId > 0) then
      Query.ParamByName('Instrument').AsInteger := Self.Instrument.SokidInfo.ContractId
    else
      Query.ParamByName('Instrument').Clear;
    Query.ParamByName('GradientValue').AsFloat     := Self.GradientValue;
    Query.ParamByName('RealTimeType').AsInteger    := Self.RealTimeType;
    Query.ParamByName('TimeInForce').AsInteger     := Self.TimeInForce;
    Query.ParamByName('ExtendOnLastPriceUp').AsBoolean := Self.ExtendOnLastPriceUp;
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveToDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TConditionDoc.SetActive(const Value: Boolean);
begin
  if (FActive <> Value) then
  begin
    FActive := Value;
    AddConditionHistory;
  end;
end;

procedure TConditionDoc.SetEnabled(const Value: boolean);
var Node: PVirtualNode;
    Data: PTreeData;
begin
  inherited;
  // init base price
  if Enabled then
  begin
    if (CondType = ctRealtimeValue) and (RealTimeType = 3) and Assigned(OwnerNode) then
    begin
      // get condition owner order
      Node := OwnerNode.Parent;
      while Assigned(Node) and Assigned(Node.Parent) {exclude root node} do
      begin
        Data := Node^.GetData;
        if Data.DocType = ntOrder then
          if Assigned(Data.OrderDoc.ParentOrder) then
          begin
            FBasePrice := Data.OrderDoc.ParentOrder.AvgPrice;
            if Data.OrderDoc.ParentOrder.OrderAction = iabBuy then
              InequalityRt := iqBelowOrEqual
            else
              InequalityRt := iqAboveOrEqual;
            break;
          end;
        Node := Node.Parent;
      end;
    end
    else if CondType = ctTimeInForce then
      TimeInForceStartTime := Now;
  end;
end;

procedure TConditionDoc.SetIsCondition(const Value: Boolean);
begin
  if (FIsCondition <> Value) then
  begin
    FIsCondition := Value;
    AddConditionHistory;
    if Value then
      FConditionTime := Now;
  end;
end;

procedure TConditionDoc.AddConditionHistory;
var
  ConditionHistory: TConditionHistory;
begin
  ConditionHistory.Active        := Self.Active;
  ConditionHistory.CondType      := Self.CondType;
  ConditionHistory.TimeStamp     := Now;
  ConditionHistory.CondLimit     := Self.CondLimit;
  ConditionHistory.CalcValue     := Self.CalcValue;
  ConditionHistory.TickType1     := Self.TickType1;
  ConditionHistory.TickType2     := Self.TickType2;
  ConditionHistory.IsCondition   := Self.IsCondition;
  ConditionHistory.TypeOperation := Self.TypeOperation;
  ConditionHistory.Enabled       := Self.Enabled;
  case Self.CondType of
    ctRealtimeValue:
      ConditionHistory.InequalityType := FInequalityRt;
    ctCorridor:
      ConditionHistory.InequalityType := FInequalityCor;
    ctCorridorPosition:
      ConditionHistory.InequalityType := FInequalityCor;
    ctGradientAndCorridor:
      ConditionHistory.InequalityType := FInequalityGr;
    ctGradient:
      ConditionHistory.InequalityType := FInequalityGr;
    ctTrailBuy:
      ;
    ctTrailSell:
      ;
    ctTimeGap:
      ;
  end;
  ConditionHistory.Value1  := FCompiledValue.SummValue1;
  ConditionHistory.Value2  := FCompiledValue.SummValue2;
  ConditionHistory.Factors := FCompiledValue.Factors;
  FHistory.Add(ConditionHistory);
end;

procedure TConditionDoc.SetLengthPriceArray(aLength: Integer);
begin
  SetLength(FPriceArray, aLength);
end;

procedure TConditionDoc.SetValueArrayItem(Index: TConditionType; const Value: Double);
begin
  FValueArray[Index] := Value;
end;

procedure TConditionDoc.SetValueItem(Index: TIABTickType; const Value: Double);
begin
  inherited;
  if (ActivationValue > 0) and (Index = ttLast) and (FMinMaxValueArray[mmLastPayed] <> Value) then
  begin
    FMinMaxValueArray[mmLastPayed] := Value;
    if (FMinMaxValueArray[mmMinPayed] > Value) or (FMinMaxValueArray[mmMinPayed] = 0) then
      FMinMaxValueArray[mmMinPayed] := Value;
    if (FMinMaxValueArray[mmMaxPayed] < Value) or (FMinMaxValueArray[mmMaxPayed] = 0) then
      FMinMaxValueArray[mmMaxPayed] := Value;
  end;
end;

function TConditionDoc.ToValueString: string;
begin
  if IsCondition then
    Result := 'TRUE, '
  else
    Result := 'FALSE, ';

  if Enabled then
    Result := Result + 'ENABLED, '
  else
    Result := Result + 'DISABLED, ';

  if (CondType = ctRealTimeValue) and (RealTimeType = 3) then
    Result := Result + 'BasePrice='+ FBasePrice.ToString + ', ';

  if CondType = ctRealtimeValue then
  begin
    case RealTimeType of
      0 : Result := Result + Format('%s/%s%s%s, ',
                                      [TickType1Value.ToString,
                                       TickType2Value.ToString,
                                       InequalityRt.ToString,
                                       FloatToStr(CondLimit)]);
      1 : Result := Result + Format('%s%s%s, ',
                                      [TickType1Value.ToString,
                                       InequalityRt.ToString,
                                       FloatToStr(CondLimit)]);
      2 : Result := Result + Format('%s/%s%s%s, ',
                                      [TickType1Value.ToString,
                                       TickType2Value.ToString,
                                       InequalityRt.ToString,
                                       FloatToStr(CondLimit)]);
      3 : Result := Result + Format('100*(%s-%s)/%s%s%s, ',
                                      [TickType1Value.ToString,
                                       BasePrice.ToString,
                                       BasePrice.ToString,
                                       InequalityRt.ToString,
                                       FloatToStr(CondLimit)])
    end;
  end
  else if CondType = ctGradient then
    Result := Result + FloatToStr(Gradient) + ', '
  else if (CondType = ctTimeInForce) and Enabled then
    Result := Result + ' Trigger on '+ DateTimeToStr(IncSecond(TimeInForceStartTime, TimeInForce)) + ', ';
  if ConditionTime > 0 then
    Result := Result + 'Time: ' + DateTimeToStr(ConditionTime);
  {Result := 'PRIO: ' + PriorityString[Self.Priority];
  if (Self.CondType = ctRealtimeValue) then
  begin
    if (Self.TickType2 = ttNotSet) then
      Result := Result + ', ' + Self.TickType1.ToString
    else
      Result := Result + ', ' + Self.TickType1.ToString + Self.TypeOperation.ToString + Self.TickType2.ToString;
  end;
  case Self.CondType of
    ctRealtimeValue:
      Result := Result + ': ' + FormatFloat('0.00 ', Self.CalcValue);
    ctRealtimeAndTimeGap:
      Result := Result + ': ' + FormatFloat('0.00 ', Self.CalcValue);
    ctTimeGap:
      ;
    ctGradient:
      Result := Result + ': ' + FormatFloat('0.00 ', Self.Gradient);
    ctCorridor:
      Result := Result + ': ' + FormatFloat('0.00 ', Self.Gradient);
    ctCorridorPosition:
      Result := Result + ': ' + FormatFloat('0.00 ', Self.Gradient);
  end; }
end;

{ TFactorDoc }

procedure TFactorDoc.AssignFrom(const aSource: TCustomDocument);
var
  Source: TFactorDoc absolute aSource;
begin
  inherited AssignFrom(aSource);
  Self.BrokerType      := Source.BrokerType;
  Self.ChangeProc      := Source.ChangeProc;
  Self.ContractType    := Source.ContractType;
  Self.Currency        := Source.Currency;
  Self.CurrentValue    := Source.CurrentValue;
  Self.Decimals        := Source.Decimals;
  Self.DirDay          := Source.DirDay;
  Self.Exchange        := Source.Exchange;
  Self.Expiry          := Source.Expiry;
  Self.PrimaryExchange := Source.PrimaryExchange;
  Self.IBId            := Source.IBId;
  Self.NNId            := Source.NNId;
  Self.IsIn            := Source.IsIn;
  Self.ContractId      := Source.ContractId;
  Self.IdentifierList  := Source.IdentifierList;
  Self.InstrumentName  := Source.InstrumentName;
  Self.MarketList      := Source.MarketList;
  Self.Omxs30_w        := Source.Omxs30_w;
  Self.LastPrice1      := Source.LastPrice1;
  Self.LastPrice2      := Source.LastPrice2;
  Self.Symbol          := Source.Symbol;
  Self.LocalSymbol     := Source.LocalSymbol;
  Self.TickType1       := Source.TickType1;
  Self.TickType2       := Source.TickType2;
  Self.UseIBFeeds      := Source.UseIBFeeds;
end;

constructor TFactorDoc.Create;
begin
  inherited;
  BrokerType   := TBrokerType.brIB;
  UseIBFeeds   := False;
  Decimals     := 2;
  FLocalSymbol := '';
end;

procedure TFactorDoc.FromDB(const aRecordId: Integer);
resourcestring
  C_SQL_TEXT = 'SELECT * FROM FACTOR WHERE ID=:RecordId';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_TEXT;
    Query.ParamByName('RecordId').AsInteger := aRecordId;
    try
      Query.Prepare;
      Query.Open;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'FromDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;

    Query.First;
    if not Query.IsEmpty then
    begin
      Self.BrokerType     := TBrokerType.FromInteger(Query.FieldByName('BROKER_ID').AsInteger);
      Self.ContractId     := Query.FieldByName('CONTRACT_ID').AsInteger;
      Self.ContractType   := Query.FieldByName('CONTRACT_TYPE').AsString;
      Self.Currency       := Query.FieldByName('CURRENCY').AsString;
      Self.Decimals       := Query.FieldByName('DECIMALS').AsInteger;
      Self.Exchange       := Query.FieldByName('EXCHANGE').AsString;
      Self.IdentifierList := Query.FieldByName('IDENTIFIER_LIST').AsString;
      Self.InstrumentName := Query.FieldByName('NAME').AsString;
      Self.IsIn           := Query.FieldByName('ISIN').AsString;
      Self.MarketList     := Query.FieldByName('MARKET_LIST').AsString;
      Self.Symbol         := Query.FieldByName('SYMBOL').AsString;
      Self.LocalSymbol    := Query.FieldByName('LOCAL_SYMBOL').AsString;
      Self.TickType1      := TIABTickType(Query.FieldByName('TICK_TYPE1').AsInteger);
      Self.TickType2      := TIABTickType(Query.FieldByName('TICK_TYPE2').AsInteger);
      Self.Expiry         := Query.FieldByName('EXPIRY').AsDateTime;
      Self.UseInAutoOrder := Query.FieldByName('USE_IN_AUTOORDER').AsBoolean;
    end;
    Self.RecordId := aRecordId;
  finally
    FreeAndNil(Query);
  end;
  if SokidList.ContainsKey(Self.ContractId) then
    Self.Expiry := SokidList.Items[Self.ContractId].Expiry;
end;

function TFactorDoc.GetContractId: Integer;
begin
  case BrokerType of
    brIB:
      Result := IBId;
    brNN:
      Result := NNId;
  else
    Result := 0;
  end
end;

function TFactorDoc.GetLocalSymbol: string;
begin
  if FLocalSymbol.IsEmpty and SokidList.ContainsKey(Self.ContractId) then
    FLocalSymbol := SokidList.Items[Self.ContractId].LocalSymbol;
  Result := FLocalSymbol;
end;

procedure TFactorDoc.SetLocalSymbol(const Value: string);
begin
  if not Value.IsEmpty and not Value.Equals(FLocalSymbol) then
    FLocalSymbol := Value;
end;

function TFactorDoc.ToValueString: string;
begin
  Result := Self.TickType1.ToString + ': ' + Format(Self.TickType1.ToFormat, [TPriceCache.PriceCache.GetLastPrice(Self.ContractId, Self.TickType1)]) + ', ' +
            Self.TickType2.ToString + ': ' + Format(Self.TickType2.ToFormat, [TPriceCache.PriceCache.GetLastPrice(Self.ContractId, Self.TickType2)]);
end;

procedure TFactorDoc.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM FACTOR WHERE ID = :RecordId';

  C_SQL_INSERT_TEXT = 'INSERT INTO FACTOR ' +
                   '(BROKER_ID,CONTRACT_ID,CONTRACT_TYPE,CURRENCY,DECIMALS,EXCHANGE,'     +
                   'TICK_TYPE1,TICK_TYPE2,IDENTIFIER_LIST,NAME,ISIN,MARKET_LIST,EXPIRY,'  +
                   'SYMBOL,LOCAL_SYMBOL,USE_IN_AUTOORDER,ID) '                            +
                   'VALUES (:BrokerType, '     +
                   '        :ConId, '          +
                   '        :ContractType, '   +
                   '        :Currency, '       +
                   '        :Decimals, '       +
                   '        :Exchange, '       +
                   '        :TickType1, '      +
                   '        :TickType2, '      +
                   '        :Identifier, '     +
                   '        :Name, '           +
                   '        :IsIn, '           +
                   '        :MarketList, '     +
                   '        :Expiry, '         +
                   '        :Symbol, '         +
                   '        :LocalSymbol, '    +
                   '        :UseInAutoOrder, ' +
                   '        :RecordId)';
  C_SQL_UPDATE_TEXT = 'UPDATE FACTOR SET ' +
                      'BROKER_ID        = :BrokerType, '     +
                      'CONTRACT_ID      = :ConId, '          +
                      'CONTRACT_TYPE    = :ContractType, '   +
                      'CURRENCY         = :Currency, '       +
                      'DECIMALS         = :Decimals, '       +
                      'EXCHANGE         = :Exchange, '       +
                      'TICK_TYPE1       = :TickType1, '      +
                      'TICK_TYPE2       = :TickType2, '      +
                      'IDENTIFIER_LIST  = :Identifier, '     +
                      'NAME             = :Name, '           +
                      'ISIN             = :IsIn, '           +
                      'MARKET_LIST      = :MarketList, '     +
                      'SYMBOL           = :Symbol, '         +
                      'LOCAL_SYMBOL     = :LocalSymbol, '    +
                      'EXPIRY           = :Expiry, '         +
                      'USE_IN_AUTOORDER = :UseInAutoOrder '  +
                      'WHERE ID         = :RecordId';
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
    if (RecordId > 0) then
    begin
      Query.SQL.Text := C_SQL_EXISTS_TEXT;
      try
        Query.ParamByName('RecordId').AsInteger := RecordId;
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
    end
    else
      RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');

    if IsExists then
      Query.SQL.Text := C_SQL_UPDATE_TEXT
    else
      Query.SQL.Text := C_SQL_INSERT_TEXT;

    Query.ParamByName('BrokerType').AsInteger     := Ord(Self.BrokerType);
    Query.ParamByName('ConId').AsInteger          := Self.ContractId;
    Query.ParamByName('ContractType').AsString    := Self.ContractType.Substring(0, 10);
    Query.ParamByName('Currency').AsString        := Self.Currency.Substring(0, 10);
    Query.ParamByName('Decimals').AsInteger       := Self.Decimals;
    Query.ParamByName('Expiry').AsDateTime        := Self.Expiry;
    Query.ParamByName('Exchange').AsString        := Self.Exchange.Substring(0, 40);
    Query.ParamByName('Identifier').AsString      := Self.IdentifierList.Substring(0, 500);
    Query.ParamByName('IsIn').AsString            := Self.IsIn.Substring(0, 30);
    Query.ParamByName('MarketList').AsString      := Self.MarketList.Substring(0, 100);
    Query.ParamByName('Name').AsString            := Self.InstrumentName.Substring(0, 50);
    Query.ParamByName('Symbol').AsString          := Self.Symbol.Substring(0, 40);
    Query.ParamByName('LocalSymbol').AsString     := Self.LocalSymbol.Substring(0, 40);
    Query.ParamByName('TickType1').AsInteger      := Integer(Self.TickType1);
    Query.ParamByName('TickType2').AsInteger      := Integer(Self.TickType2);
    Query.ParamByName('UseInAutoOrder').AsBoolean := Self.UseInAutoOrder;
    Query.ParamByName('RecordId').AsInteger       := RecordId;
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveToDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TFactorDoc.SetContractId(const Value: Integer);
begin
  case BrokerType of
    brIB:
      IBId := Value;
    brNN:
      NNId := Value;
  end;
end;

{ TAlgosDoc }

constructor TAlgosDoc.Create;
begin
  inherited;
  FDecimals := 2;
  FDivisor  := 1;
  ValueList := TObjectDictionary<TDateTime, TPrice>.Create([doOwnsValues]);
end;

destructor TAlgosDoc.Destroy;
begin
  FreeAndNil(ValueList);
  inherited;
end;

procedure TAlgosDoc.FromDB(const aRecordId: Integer);
resourcestring
  C_SQL_TEXT = 'SELECT * FROM ALGORITMOS WHERE ID=:RecordId';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_TEXT;
    try
      Query.ParamByName('RecordId').AsInteger := aRecordId;
      Query.Prepare;
      Query.Open;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'FromDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
    Query.First;
    if not Query.IsEmpty then
    begin
      Self.Name := Query.FieldByName('NAME').AsString;
//      Self.ConId     := Query.FieldByName('CONTRACT_ID').AsInteger;
      Self.Decimals  := Query.FieldByName('DECIMALS').AsInteger;
      Self.Divisor   := Query.FieldByName('DIVISOR').AsFloat;
    end;
      Self.RecordId  := aRecordId;
  finally
    FreeAndNil(Query);
  end;
end;

function TAlgosDoc.GetDivisor: Double;
begin
  //see Clarifications about forms, charts and historical data
  Result := 1;
end;

procedure TAlgosDoc.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM ALGORITMOS WHERE ID = :RecordId';
  C_SQL_INSERT_TEXT = 'INSERT INTO ALGORITMOS '+
                       '(NAME,DECIMALS,DIVISOR,ID) '+
                       'VALUES(:Name, ' +
                       '       :Decimals, '  +
                       '       :Divisor, '   +
                       '       :RecordId)';

  C_SQL_UPDATE_TEXT = 'UPDATE ALGORITMOS SET ' +
                      'NAME     = :Name, '     +
                      'DECIMALS = :Decimals, ' +
                      'DIVISOR  = :Divisor '   +
                      'WHERE ID = :RecordId';
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
    if (RecordId > 0) then
    begin
      Query.SQL.Text := C_SQL_EXISTS_TEXT;
      try
        Query.ParamByName('RecordId').AsInteger := RecordId;
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
    end
    else
      RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');

    if IsExists then
      Query.SQL.Text := C_SQL_UPDATE_TEXT
    else
      Query.SQL.Text := C_SQL_INSERT_TEXT;

    Query.ParamByName('Name').AsString      := Self.Name.Substring(0, 200);
    Query.ParamByName('Decimals').AsInteger := Self.Decimals;
    Query.ParamByName('Divisor').AsFloat    := Self.Divisor;
    Query.ParamByName('RecordId').AsInteger := RecordId;
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveToDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TAlgosDoc.SetDivisor(const Value: Double);
begin
  FDivisor := Value;
end;

procedure TAlgosDoc.AddToValueList(aTimeStamp: TDateTime; aPrice, aCoefficient: Double);
begin
  aTimeStamp := RoundToNearest(aTimeStamp, C_ROUND_SEC);
  if ValueList.ContainsKey(aTimeStamp) then
    ValueList.Items[aTimeStamp].Price := ValueList.Items[aTimeStamp].Price + aPrice * aCoefficient
  else
    ValueList.Add(aTimeStamp, TPrice.Create(aPrice * aCoefficient));
//  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'AddToValueList', 'aTimeStamp: ' + TimeToStr(aTimeStamp) +', aPrice: ' +aPrice.ToString + ' ,aDivisor: ' + aCoefficient.ToString);
end;

procedure TAlgosDoc.AssignFrom(const aSource: TCustomDocument);
var
  Source: TAlgosDoc absolute aSource;
begin
  inherited AssignFrom(aSource);
  Self.Name     := Source.Name;
//  Self.ConId    := Source.ConId;
  Self.Decimals := Source.Decimals;
  Self.Divisor  := Source.Divisor;
end;

{ TConditionDoc.TPrice }

constructor TConditionDoc.TPrice.Create(aPrice: Double);
begin
  Price := aPrice;
end;

{ TOrderNNDoc }

constructor TOrderNNDoc.Create;
begin
  inherited;
  ClearValueArray;

  FActivationValue      := 0;
  FConditionReached     := 0;
  FConditionStopLoss    := 0;
  FFeedFromBroker       := 0;
  FIdentifierList       := '';
  FIsActiveReached      := False;
  FIsActiveStopLoss     := False;
  FIsIn                 := '';
  FLimit                := 0;
//  FNNId                 := 0;
  FOpenVolume           := 100;
  FOrderType            := otMarket;
  FPrice                := 0;
  FReference            := '';
  FChildSymbol          := '';
  FTrailOrderSendType   := otMarket;
  FTrailTriggerLimit    := 0;
  FTrailTriggerSendSell := 0;
  FValidUntil           := '';
  BrokerType            := TBrokerType.brNN;
end;

destructor TOrderNNDoc.Destroy;
begin

  inherited;
end;

function TOrderNNDoc.IsPossibleToBuyOrder(aNode: PVirtualNode): Boolean;
var
  ParentNode: PVirtualNode;
  PrevNode: PVirtualNode;
  Data: PTreeData;
  Monitor: IMonitor;
begin
  Result := {NordNetBroker.Active and (NordNetBroker.AccountNum > 0) and } not (OrderStatus in [osFilled, osError]);

  //Check orders in sequential OrderGroups
  if Result and Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    ParentNode := Monitor.GetParentNode(aNode, ntOrderGroup);
    if Assigned(ParentNode) then
    begin
      Data := ParentNode^.GetData;
      if Assigned(Data.OrderGroupDoc) and (Data.OrderGroupDoc.Kind in [okSequentialContinue, okSequentialStop]) then
      begin
        PrevNode := aNode.PrevSibling;
        if not Assigned(PrevNode) then
          Result := True
        else
        begin
          Data := PrevNode^.GetData;
          if Assigned(Data.OrderDoc) then
            case Data^.OrderGroupDoc.Kind of
              okSequentialContinue:
                Result := Data^.OrderDoc.OrderStatus in [osFilled, osError, osCancelled, osPendCancel];
              okSequentialStop:
                Result := Data^.OrderDoc.OrderStatus in [osFilled];
            end;
        end;
      end;
    end;
  end;

  if Result then
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'IsPossibleToBuyOrder', THtmlLib.GetColorTag('Is possible to buy order', clNavy))
  else
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'IsPossibleToBuyOrder', THtmlLib.GetColorTag('Is not possible to buy order', clMaroon));
end;

procedure TOrderNNDoc.AssignFrom(const aSource: TCustomDocument);
var
  Source: TOrderNNDoc absolute aSource;
begin
  inherited AssignFrom(aSource);
  Self.ChildIdentifierList   := Source.ChildIdentifierList;
  Self.ChildMarketList       := Source.ChildMarketList;
  Self.ChildId               := Source.ChildId;
  Self.ConditionReached      := Source.ConditionReached;
  Self.ConditionStopLoss     := Source.ConditionStopLoss;
  Self.ExtendedOptions       := Source.ExtendedOptions;
  Self.FeedFromBroker        := Source.FeedFromBroker;
  Self.IdentifierList        := Source.IdentifierList;
  Self.IsActiveReached       := Source.IsActiveReached;
  Self.IsActiveStopLoss      := Source.IsActiveStopLoss;
  Self.IsIn                  := Source.IsIn;
  Self.LimitType             := Source.LimitType;
  Self.MarketList            := Source.MarketList;
  Self.OpenVolume            := Source.OpenVolume;
  Self.PrimaryExchange       := Source.PrimaryExchange;
  Self.Reference             := Source.Reference;
  Self.TrailOrderSendType    := Source.TrailOrderSendType;
  Self.TrailTriggerLimit     := Source.TrailTriggerLimit;
  Self.TrailTriggerSendSell  := Source.TrailTriggerSendSell;
  Self.ValidUntil            := Source.ValidUntil;
end;

procedure TOrderNNDoc.Buy;
begin
//   DoBuy(aModifyingOrder);
  if Assigned(OwnerNode) then
  begin
//    TMonitorLists.OrderList.AddStatus(OwnerNode, 0, 0, 0, 'TimeStamp=' + FormatDateTime('hh:nn:ss.zzz', Now), 'beforeAddOrder', -1, osSleeping, 0);
    TMonitorLists.OrderQueue.AddItem(OwnerNode);
  end;
end;

procedure TOrderNNDoc.UpdateOrder;
var
  ModPrice   : Double;
  OrderReply : TOrderReply;
  sInfo      : string;
begin
  if Assigned(OwnerNode) and (OrderIBId > 0) and (Price > 0) then
  begin
    ModPrice := Price;
    if Self.OrderType in [otLimit, otComboLimit] then
    begin
      case LimitType of
        ltRelative:
          ModPrice := Price + Limit;
        ltFix:
          ModPrice := Limit;
      end;
    end;

    OrderReply := NordNetBroker.UpdateOrder(NordNetBroker.AccountNum, OrderIBId, Quantity, ModPrice, Currency);
    sInfo := NordNetBroker.OrderReplyToStr(OrderReply) + sLineBreak +
             'Modifying parameters: ' + sLineBreak +
             'Quantity=' + Quantity.ToString +
             ', Price=' + Price.ToString +
             ', Currency=' + Currency + sLineBreak + '----------------' + sLineBreak +
             sInfo;
    if OrderReply.action_state.StartsWith('MOD_FAIL') or (OrderReply.order_id <= 0)  then
      TMonitorLists.OrderList.AddStatus(OwnerNode, OrderReply.order_id, 0, 0, sInfo, 'error', -1, ActionStateToOrderState(OrderReply.action_state), 0)
    else
      TMonitorLists.OrderList.AddStatus(OwnerNode, OrderReply.order_id, 0, 0, sInfo, 'modifyingOrder', -1, ActionStateToOrderState(OrderReply.action_state), 0);
  end
end;

procedure TOrderNNDoc.DoBuy;
var
  i                : Integer;
  Identifier       : string;
  loListIdentifier : TStringList;
  loListMarket     : TStringList;
  MarketId         : Integer;
  Order            : TPostOrder;
  OrderReply       : TOrderReply;
  sInfo            : string;
  Monitor          : IMonitor;
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'DoBuy');
  if IsPossibleToBuyOrder(OwnerNode) then
  begin
    IsExecuted := True;
    MarketId   := -1;
    if not MarketList.IsEmpty then
    begin
      loListMarket := TStringList.Create;
      loListIdentifier := TStringList.Create;
      try
        ExtractStrings([';'], [], PChar(IdentifierList), loListIdentifier);
        ExtractStrings([';'], [], PChar(MarketList), loListMarket);
        for i := 0 to loListMarket.Count - 1 do
          if not loListMarket[i].IsEmpty then
          begin
            MarketId := StrToIntDef(loListMarket[i], -1);
            if (MarketId > -1) then
            begin
              if (loListIdentifier.Count = loListMarket.Count) then
                Identifier := loListIdentifier[i];
              Break;
            end;
          end;
      finally
        FreeAndNil(loListIdentifier);
        FreeAndNil(loListMarket);
      end;
    end;

    if Assigned(OwnerNode) then
      TThread.Queue(nil,
        procedure
        begin
          OwnerNode.CheckState := csCheckedNormal;
        end);

    Order := TNordNet.CreatePostOrderRecord(MarketId, Quantity);
    if OrderAction in [iabBuy, iabSell] then
      Order.side := ActionString[OrderAction];

    Order.identifier    := Identifier;
    Order.currency      := Currency;
    Order.reference     := Reference;
    Order.volume        := Quantity;
    Order.valid_until   := ValidUntil;
//    Order.activation_condition : string;
//    Order.trigger_value := 0;
//    Order.trigger_condition    : string;
//    Order.target_value := 0;

    case Self.OrderType of
      otMarket, otComboMarket:
        begin
          Order.order_type := 'NORMAL';
          Order.Price := LastPrice;
        end;
      otLimit, otComboLimit:
        begin
          Order.order_type := 'LIMIT';
//          Order.open_volume := OpenVolume;
          case LimitType of
            ltRelative:
              begin
                if (Limit = 0) then
                  Order.Price := LastPrice
                else
                  Order.Price := LastPrice + Limit;
              end;
            ltFix:
              Order.Price := Limit;
          end;
        end;
    end;
    Self.Price := Order.Price;
    FValueArray[cvEntryPrice] := FValueArray[cvLastPayed];

    OrderReply.order_id := 0;
    try
      OrderReply := NordNetBroker.AddOrder(NordNetBroker.AccountNum, Order);
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoBuy', E.Message);
    end;

    sInfo := 'Start parameters: ' + sLineBreak +
             Format('Last=%f, Limit=%f, EntryPrice=%f,', [LastPrice, Limit, FValueArray[cvEntryPrice]]) +
             NordNetBroker.PostOrderToStr(Order) + sLineBreak +  '----------------' + sLineBreak +
             NordNetBroker.OrderReplyToStr(OrderReply);

    if Assigned(OwnerNode) then
    begin
      if OrderReply.action_state.StartsWith('INS_FAIL') or (OrderReply.order_id <= 0)  then
      begin
        OrderStatus := osError;
        TMonitorLists.OrderList.SetLastStatus(OwnerNode, osError);
        TMonitorLists.OrderList.AddStatus(OwnerNode, OrderReply.order_id, 0, 0, sInfo, 'error', -1, osError, 0);
      end
      else if OrderReply.action_state.StartsWith('INS_PEND') and (OrderReply.order_id > 0) then
      begin
        OrderIBId := OrderReply.order_id;
        TMonitorLists.OrderList.SetLastStatus(OwnerNode, osSubmitted);
        TMonitorLists.OrderList.AddStatus(OwnerNode, OrderReply.order_id, 0, 0, sInfo, 'addOrder', -1, osSleeping, 0);
        OrderStatus := osSubmitted;
        if Supports(Application.MainForm, IMonitor, Monitor) then
          TThread.Queue(nil,
            procedure
            begin
              Monitor.SetChildsFreeze(OwnerNode);
              Beep;
            end);
      end;
    end;
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'DoBuy', sInfo);
  end;
  TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'DoBuy');
end;

procedure TOrderNNDoc.FromDB(const aRecordId: Integer);
resourcestring
  C_SQL_TEXT = 'SELECT * FROM ORDERS WHERE ID=:RecordId';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_TEXT;
    try
      Query.ParamByName('RecordId').AsInteger := aRecordId;
      Query.Prepare;
      Query.Open;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'FromDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;

    if not Query.IsEmpty then
    begin
      Query.First;
      Self.RecordId := aRecordId;
      if (Query.FieldByName('BROKER_ID').AsInteger = Ord(TBrokerType.brNN)) then
      begin
        Self.Id                    := Query.FieldByName('NN_ID').AsInteger;
        Self.BrokerType            := TBrokerType.FromInteger(Query.FieldByName('BROKER_ID').AsInteger);
        Self.ConditionReached      := Query.FieldByName('CONDITION_REACHED').AsFloat;
        Self.ConditionStopLoss     := Query.FieldByName('CONDITION_STOPLOSS').AsFloat;
        Self.Currency              := Query.FieldByName('CURRENCY').AsString;
        Self.Decimals              := Query.FieldByName('DECIMALS').AsInteger;
        Self.Description           := Query.FieldByName('DESCRIPTION').AsString;
        Self.Exchange              := Query.FieldByName('EXCHANGE').AsString;
        Self.IdentifierList        := Query.FieldByName('IDENTIFIER_LIST').AsString;
        Self.InstrumentName        := Query.FieldByName('INSTRUMENT_NAME').AsString;
        Self.IsActiveReached       := Query.FieldByName('IS_ACTIVE_REACHED').AsBoolean;
        Self.IsActiveStopLoss      := Query.FieldByName('IS_ACTIVE_STOPLOSS').AsBoolean;
        Self.IsIn                  := Query.FieldByName('ISIN').AsString;
        Self.FeedFromBroker        := Query.FieldByName('FEED_FROM_BROKER').AsInteger;
        Self.Limit                 := Query.FieldByName('LIMIT').AsFloat;
        Self.LimitType             := TLimitType(Query.FieldByName('LIMIT_TYPE').AsInteger);
        Self.OpenVolume            := Query.FieldByName('OPEN_VOLUME').AsInteger;
        Self.OrderType             := TIABOrderType(Query.FieldByName('ORDER_TYPE').AsInteger);
        Self.Quantity              := Query.FieldByName('QUANTITY').AsInteger;
        Self.Reference             := Query.FieldByName('REFERENCE').AsString;
        Self.Symbol                := Query.FieldByName('SYMBOL').AsString;
        Self.TrailOrderSendType    := TIABOrderType(Query.FieldByName('TRAIL_ORDER_SEND_TYPE').AsInteger);
        Self.TrailTriggerLimit     := Query.FieldByName('TRAIL_TRIGGER_LIMIT').AsFloat;
        Self.TrailTriggerSendSell  := Query.FieldByName('TRAIL_TRIGGER_SEND_SELL').AsFloat;
        Self.TriggerMethod         := TTriggerMethod(Query.FieldByName('TRIGGER_METHOD').AsInteger);
        Self.ValidUntil            := Query.FieldByName('VALID_UNTIL').AsString;
        Self.ChildSymbol           := Query.FieldByName('CHILD_SYMBOL').AsString;
        Self.ChildIdentifierList   := Query.FieldByName('CHILD_IDENTIFIER_LIST').AsString;
        Self.ChildMarketList       := Query.FieldByName('CHILD_MARKET_LIST').AsString;
        Self.ChildId               := Query.FieldByName('CONTRACT_ID').AsInteger;
        Self.XmlParams             := Query.FieldByName('XML_PARAMS').AsString;
        if Query.FieldByName('ORDER_ACTION').AsBoolean then
          Self.OrderAction := iabBuy
        else
          Self.OrderAction := iabSell;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
  if SokidList.ContainsKey(Self.Id) then
    Self.Expiry := SokidList.Items[Self.Id].Expiry;
end;

class function TOrderNNDoc.ActionStateToOrderState(aAction: string): TIABOrderState;
begin
  // Result code: OK,                      Order state: LOCAL,   Action state: INS_PEND
  // Result code: ORDER_MISSING_AGREEMENT, Order state: DELETED, Action state: INS_FAIL
  // Result code: ORDER_DELETED,           Order state: DELETED, Action state: DEL_FAIL, Message: Internal error.

  // order_state (string): The state of the order:
  // DELETED - Order is deleted
  // LOCAL - Order is offline/local and eligible for activation
  // ON_MARKET - Order is active on market

  Result := osError;
  if aAction.StartsWith('DEL_CONF') then      // DEL_CONF - Delete confirmed by market
    Result := osCancelled
  else if aAction.StartsWith('DEL_FAIL') then // DEL_FAIL - Delete request failed and the order is still active on market
    Result := osError
  else if aAction.StartsWith('DEL_PEND') then // DEL_PEND - Delete request in progress and unconfirmed from the market
    Result := osCancelled
  else if aAction.StartsWith('DEL_PUSH') then // DEL_PUSH - Deleted by market
    Result := osFilled
  else if aAction.StartsWith('DEL_WAIT') then // DEL_WAIT - Delete of order on market waiting for market open
    Result := osCancelled
  else if aAction.StartsWith('INS_CONF') then // INS_CONF - Confirmed insert
    Result := osSubmitted
  else if aAction.StartsWith('INS_FAIL') then // INS_FAIL - Insert failed
    Result := osError
  else if aAction.StartsWith('INS_PEND') then // INS_PEND - Pending insert
    Result := osSubmitted
  else if aAction.StartsWith('INS_STOP') then // INS_STOP - The order inserted into the Nordnet system and stopped. This is the state of inactive orders and not triggered stop loss orders
    Result := osCancelled
  else if aAction.StartsWith('INS_WAIT') then // INS_WAIT - Insert waiting for market open
    Result := osPendSubmit
  else if aAction.StartsWith('MOD_CONF') then // MOD_CONF - Modification confirmed by the market
    Result := osSubmitted
  else if aAction.StartsWith('MOD_FAIL') then // MOD_FAIL - Modification failed and the previous order values are still valid
    Result := osError
  else if aAction.StartsWith('MOD_PEND') then // MOD_PEND - Modification in progress and waiting confirmation from market
    Result := osPendSubmit
  else if aAction.StartsWith('MOD_PUSH') then // MOD_PUSH - Modified by market
    Result := osSubmitted
  else if aAction.StartsWith('MOD_WAIT') then // MOD_WAIT - Modification of order on market waiting for market open
    Result := osPendSubmit;
end;

procedure TOrderNNDoc.CancelOrder;
var
  OrderReply : TOrderReply;
  sInfo      : string;
begin
  if NordNetBroker.Active and (NordNetBroker.AccountNum > 0) and (OrderIBId > 0) then
  begin
    try
      OrderReply := NordNetBroker.DeleteOrder(NordNetBroker.AccountNum, OrderIBId);
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'CancelOrder', E.Message);
    end;
    sInfo := NordNetBroker.OrderReplyToStr(OrderReply);
    if OrderReply.order_state.StartsWith('DELETED') then
    begin
      OrderStatus := osCancelled;
      if Assigned(OwnerNode) then
        TMonitorLists.OrderList.SetLastStatus(OwnerNode, osCancelled);
    end;
    ClearValueArray;
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, Self, 'CancelOrder', sInfo);
  end;
end;

function TOrderNNDoc.GetOrderStatusText: string;
begin
  Result := OrderNNStateString[OrderStatus];
end;

function TOrderNNDoc.GetValidUntil: string;
var
  FrmSettings : TFormatSettings;
  ValidDate   : TDate;
begin
  if FValidUntil.IsEmpty then
    Result := FormatDateTime('YYYY-MM-DD', Date)
  else
  begin
    FrmSettings := TFormatSettings.Create;
    FrmSettings.DateSeparator := '-';
    FrmSettings.ShortDateFormat := 'YYYY-MM-DD';
    ValidDate := StrToDateTime(FValidUntil, FrmSettings);
    if (Trunc(ValidDate) < Trunc(Date)) then
      Result := FormatDateTime('YYYY-MM-DD', Date)
    else
      Result := FValidUntil;
  end;
end;

function TOrderNNDoc.GetValueArrayItem(Index: TCurrentValue): Double;
begin
  Result := FValueArray[Index];
end;

procedure TOrderNNDoc.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM ORDERS WHERE ID=:RecordId';
  C_SQL_UPDATE_TEXT = 'UPDATE ORDERS SET '                                    +
                      'ORDER_ACTION                = :OrderAction, '          +
                      'IS_ACTIVATE_CHILD           = :IsActivateChild, '      +
                      'BROKER_ID                   = :BrokerType, '           +
                      'CONDITION_REACHED           = :ConditionReached, '     +
                      'CONDITION_STOPLOSS          = :ConditionStopLoss, '    +
                      'CONTRACT_ID                 = :ConId, '                +
                      'CURRENCY                    = :Currency, '             +
                      'DECIMALS                    = :Decimals, '             +
                      'DESCRIPTION                 = :Description, '          +
                      'FEED_FROM_BROKER            = :FeedFromBroker, '       +
                      'IDENTIFIER_LIST             = :IdentifierList, '       +
                      'INSTRUMENT_NAME             = :InstrumentName, '       +
                      'IS_ACTIVE_REACHED           = :IsActiveReached, '      +
                      'IS_ACTIVE_STOPLOSS          = :IsActiveStopLoss, '     +
                      'ISIN                        = :IsIn, '                 +
                      'EXCHANGE                    = :Exchange, '             +
                      'NN_ID                       = :Id, '                   +
                      'LIMIT                       = :Limit, '                +
                      'LIMIT_TYPE                  = :LimitType, '            +
                      'MARKET_LIST                 = :MarketList, '           +
                      'OPEN_VOLUME                 = :OpenVolume, '           +
                      'ORDER_TYPE                  = :OrderType, '            +
                      'QUANTITY                    = :Quantity, '             +
                      'REFERENCE                   = :Reference, '            +
                      'SYMBOL                      = :Symbol, '               +
                      'XML_PARAMS                  = :XmlParams, '            +
                      'TRAIL_ORDER_SEND_TYPE       = :TrailOrderSendType, '   +
                      'TRAIL_TRIGGER_LIMIT         = :TrailTriggerLimit, '    +
                      'TRAIL_TRIGGER_SEND_SELL     = :TrailTriggerSendSell, ' +
                      'TRIGGER_METHOD              = :TriggerMethod, '        +
                      'VALID_UNTIL                 = :ValidUntil, '           +
                      'CHILD_IDENTIFIER_LIST       = :ChildIdentifier, '      +
                      'EXPIRY                      = :Expiry, '               +
                      'CHILD_SYMBOL                = :ChildSymbol, '          +
                      'IS_FINAL                    = :IsFinal, '              +
                      'CHILD_MARKET_LIST           = :ChildMarketlist '       +
                      ' WHERE ID = :RecordId';
  C_SQL_INSERT_TEXT = 'INSERT INTO ORDERS ' +
                      '(ID,DESCRIPTION,ORDER_ACTION,CONTRACT_ID,DECIMALS,INSTRUMENT_NAME,'          +
                      'QUANTITY,FEED_FROM_BROKER,IS_ACTIVE_STOPLOSS,IS_ACTIVE_REACHED,LIMIT,'       +
                      'LIMIT_TYPE,ORDER_TYPE,REFERENCE,TRAIL_ORDER_SEND_TYPE, XML_PARAMS'           +
                      'VALID_UNTIL,OPEN_VOLUME,TRAIL_TRIGGER_LIMIT,TRAIL_TRIGGER_SEND_SELL,'        +
                      'IS_ACTIVATE_CHILD,MARKET_LIST,CONDITION_REACHED,CONDITION_STOPLOSS,'         +
                      'IDENTIFIER_LIST,ISIN,SYMBOL,CURRENCY,NN_ID,BROKER_ID,CHILD_IDENTIFIER_LIST,' +
                      'EXPIRY,EXCHANGE,CHILD_SYMBOL,CHILD_MARKET_LIST,IS_FINAL,TRIGGER_METHOD)' +
                      ' VALUES(:RecordId, '             +
                      '        :Description, '          +
                      '        :OrderAction, '          +
                      '        :ConId, '                +
                      '        :Decimals, '             +
                      '        :InstrumentName, '       +
                      '        :Quantity, '             +
                      '        :FeedFromBroker, '       +
                      '        :IsActiveStopLoss, '     +
                      '        :IsActiveReached, '      +
                      '        :Limit, '                +
                      '        :LimitType, '            +
                      '        :OrderType, '            +
                      '        :Reference, '            +
                      '        :TrailOrderSendType, '   +
                      '        :XmlParams, '            +
                      '        :ValidUntil, '           +
                      '        :OpenVolume, '           +
                      '        :TrailTriggerLimit, '    +
                      '        :TrailTriggerSendSell, ' +
                      '        :IsActivateChild, '      +
                      '        :MarketList, '           +
                      '        :ConditionReached, '     +
                      '        :ConditionStopLoss, '    +
                      '        :IdentifierList, '       +
                      '        :IsIn, '                 +
                      '        :Symbol, '               +
                      '        :Currency, '             +
                      '        :Id, '                   +
                      '        :BrokerType, '           +
                      '        :ChildIdentifier, '      +
                      '        :Expiry, '               +
                      '        :Exchange, '             +
                      '        :ChildSymbol, '          +
                      '        :ChildMarketlist, '      +
                      '        :IsFinal, '              +
                      '        :TriggerMethod)';
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
    if (RecordId > 0) then
    begin
      Query.SQL.Text := C_SQL_EXISTS_TEXT;
      try
        Query.ParamByName('RecordId').AsInteger := RecordId;
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
    end
    else
      RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');

    if IsExists then
      Query.SQL.Text := C_SQL_UPDATE_TEXT
    else
      Query.SQL.Text := C_SQL_INSERT_TEXT;

    Query.ParamByName('BrokerType').AsInteger         := Ord(Self.BrokerType);
    Query.ParamByName('ChildIdentifier').AsString     := Self.ChildIdentifierList;
    Query.ParamByName('ChildMarketlist').AsString     := Self.ChildMarketList;
    Query.ParamByName('ChildSymbol').AsString         := Self.ChildSymbol;
    Query.ParamByName('ConditionReached').AsFloat     := Self.ConditionReached;
    Query.ParamByName('ConditionStopLoss').AsFloat    := Self.ConditionStopLoss;
    Query.ParamByName('ConId').AsInteger              := Self.ChildId;
    Query.ParamByName('Currency').AsString            := Self.Currency.Substring(0, 10);
    Query.ParamByName('Decimals').AsInteger           := Self.Decimals;
    Query.ParamByName('Description').AsString         := Self.Description.Substring(0, 50);
    Query.ParamByName('FeedFromBroker').AsInteger     := Self.FeedFromBroker;
    Query.ParamByName('Id').AsInteger                 := Self.Id;
    Query.ParamByName('IdentifierList').AsString      := Self.IdentifierList.Substring(0, 500);
    Query.ParamByName('InstrumentName').AsString      := Self.InstrumentName.Substring(0, 50);
    Query.ParamByName('IsActivateChild').AsBoolean    := Self.IsActivateChild;
    Query.ParamByName('IsActiveReached').AsBoolean    := Self.IsActiveReached;
    Query.ParamByName('IsActiveStopLoss').AsBoolean   := Self.IsActiveStopLoss;
    Query.ParamByName('IsFinal').AsBoolean            := Self.IsFinal;
    Query.ParamByName('IsIn').AsString                := Self.IsIn.Substring(0, 30);
    Query.ParamByName('Limit').AsFloat                := Self.Limit;
    Query.ParamByName('LimitType').AsInteger          := Integer(Self.LimitType);
    Query.ParamByName('MarketList').AsString          := Self.MarketList.Substring(0, 100);
    Query.ParamByName('OpenVolume').AsInteger         := Self.OpenVolume;
    Query.ParamByName('OrderAction').AsBoolean        := Self.OrderAction = iabBuy;
    Query.ParamByName('OrderType').AsInteger          := Integer(Self.OrderType);
    Query.ParamByName('Quantity').AsInteger           := Self.Quantity;
    Query.ParamByName('RecordId').AsInteger           := RecordId;
    Query.ParamByName('Reference').AsString           := Self.Reference.Substring(0, 20);
    Query.ParamByName('Symbol').AsString              := Self.Symbol.Substring(0, 40);
    Query.ParamByName('TrailOrderSendType').AsInteger := Integer(Self.TrailOrderSendType);
    Query.ParamByName('TrailTriggerLimit').AsFloat    := Self.TrailTriggerLimit;
    Query.ParamByName('TrailTriggerSendSell').AsFloat := Self.TrailTriggerSendSell;
    Query.ParamByName('TriggerMethod').AsInteger      := Integer(Self.TriggerMethod);
    Query.ParamByName('ValidUntil').AsString          := Self.ValidUntil.Substring(0, 20);
    Query.ParamByName('XmlParams').AsString           := Self.XmlParams;
    if (Self.Expiry > 0) then
      Query.ParamByName('Expiry').AsDateTime := Self.Expiry
    else
      Query.ParamByName('Expiry').Clear;

    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveToDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TOrderNNDoc.SetChildId(const Value: Integer);
var
  Monitor: IMonitor;
begin
  // TPublishers.LogPublisher.Write([ltLogWriter], Self, 'SetId', 'Old ID=' + FChildId.ToString + ', new ID=' + Value.ToString);
  if (FChildId <> Value) then
  begin
    if Assigned(OwnerNode) and Supports(Application.MainForm, IMonitor, Monitor) then
    begin
      if (FChildId > 0) then
      begin
        TMonitorLists.InstrumentChildList.DeleteNode(FChildId, OwnerNode);
        TIABMarket.CancelMarketData(FChildId);
        Monitor.UnsubscribeChildNNFeed(OwnerNode);
      end;

      if (Value > 0) then
      begin
        TMonitorLists.InstrumentChildList.AddNode(Value, OwnerNode);
        if (FeedFromBroker = 0) then
          TIABMarket.RequestMarketData(Value)
        else
          Monitor.SubscribeChildNNFeed(OwnerNode);
      end;
    end;
    FChildId := Value;
    SetLastValueArray;
  end;
end;

procedure TOrderNNDoc.SetFeedFromBroker(const Value: Integer);
begin
//  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'SetFeedFromBroker', 'Old FeedFromBroker=' + FFeedFromBroker.ToString + ', new FeedFromBroker=' + Value.ToString);
  if (FFeedFromBroker <> Value) then
  begin
    FFeedFromBroker := Value;
    SetLastValueArray;
  end;
end;

procedure TOrderNNDoc.SetOrderType(const Value: TIABOrderType);
begin
  if (Value in [otMarket, otLimit, otTrail, otComboMarket, otComboLimit]) then
    FOrderType := Value;
end;

procedure TOrderNNDoc.SetValueArrayItem(Index: TCurrentValue; const Value: Double);
begin
  FValueArray[Index] := Value;
end;

procedure TOrderNNDoc.ClearValueArray;
var
  CurrentValue: TCurrentValue;
begin
  for CurrentValue := Low(TCurrentValue) to High(TCurrentValue) do
    FValueArray[CurrentValue] := 0;
end;

procedure TOrderNNDoc.SetLastValueArray;
var
  CurrentValue: TCurrentValue;
  Price: Double;
begin
  Price := 0;
  FValueArray[cvEntryPrice] := 0;
  if Assigned(NordNetBroker.PriceCache) then
    Price := NordNetBroker.PriceCache.GetLastPrice(FChildId, ttLast);
  for CurrentValue := cvMinPayed to cvLastPayed do
    FValueArray[CurrentValue] := Price;
end;

procedure TOrderNNDoc.SetValueArrayFromChildFeed(Index: TIABTickType; const Value: Currency);
begin
  if (FValueArray[cvEntryPrice] = 0) then
    if (Index = ttLast) and (FValueArray[cvLastPayed] <> Value) then
    begin
      FValueArray[cvLastPayed] := Value;
      if (FValueArray[cvMinPayed] > Value) or (FValueArray[cvMinPayed] = 0) then
        FValueArray[cvMinPayed] := Value;
      if (FValueArray[cvMaxPayed] < Value) or (FValueArray[cvMaxPayed] = 0) then
        FValueArray[cvMaxPayed] := Value;
    end;
end;

{ TCustomOrderDoc }

procedure TCustomOrderDoc.AssignFrom(const aSource: TSokidInfo);
begin
  Self.Symbol          := aSource.Symbol;
  Self.Exchange        := aSource.Exchange;
  Self.PrimaryExchange := aSource.PrimaryExchange;
  Self.Currency        := aSource.Currency;
  Self.Expiry          := aSource.Expiry;
end;

constructor TCustomOrderDoc.Create;
begin
  inherited;
  FOrderList := TStringList.Create;
  FOrderList.Sorted     := True;
  FOrderList.Duplicates := dupIgnore;

  FAvgPrice        := 0;
  FBrokerType      := TBrokerType.brIB;
  FDecimals        := 2;
  FId              := 0;
  FIsExecuted      := False;
  FIsRepetitive    := False;
  FIsFinal         := False;
  LatestFillPrice  := 0;
  FOrderAction     := iabBuy;
  FOrderStatus     := osSleeping;
  FParentOrder     := nil;
  FTemplateID      := 0;
  FMarketList      := '';
  FFilled          := 0;
  FTriggerMethod   := trLast;
  FIsActivateChild := False;
end;

destructor TCustomOrderDoc.Destroy;
begin
  FreeAndNil(FOrderList);
  inherited;
end;

procedure TCustomOrderDoc.FromDB(const aRecordId: Integer);
resourcestring
  C_SQL_TEXT = 'SELECT * FROM ORDERS WHERE ID=:RecordId';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_TEXT;
    try
      Query.ParamByName('RecordId').AsInteger := aRecordId;
      Query.Prepare;
      Query.Open;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'FromDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;

    if not Query.IsEmpty then
    begin
      Query.First;
      Self.RecordId := aRecordId;
      if (Query.FieldByName('BROKER_ID').AsInteger = Ord(TBrokerType.brNN)) then
      begin
        Self.Id              := Query.FieldByName('NN_ID').AsInteger;
        Self.BrokerType      := TBrokerType.FromInteger(Query.FieldByName('BROKER_ID').AsInteger);
        Self.Currency        := Query.FieldByName('CURRENCY').AsString;
        Self.Decimals        := Query.FieldByName('DECIMALS').AsInteger;
        Self.Description     := Query.FieldByName('DESCRIPTION').AsString;
        Self.Exchange        := Query.FieldByName('EXCHANGE').AsString;
        Self.IsActivateChild := Query.FieldByName('IS_ACTIVATE_CHILD').AsBoolean;
        Self.IsFinal         := Query.FieldByName('IS_FINAL').AsBoolean;
        Self.Quantity        := Query.FieldByName('QUANTITY').AsInteger;
        Self.Symbol          := Query.FieldByName('SYMBOL').AsString;
        Self.TriggerMethod   := TTriggerMethod(Query.FieldByName('TRIGGER_METHOD').AsInteger);
        Self.XmlParams       := Query.FieldByName('XML_PARAMS').AsString;

        if Query.FieldByName('ORDER_ACTION').AsBoolean then
          Self.OrderAction := iabBuy
        else
          Self.OrderAction := iabSell;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TCustomOrderDoc.AssignFrom(const aSource: TCustomDocument);
var
  Source: TCustomOrderDoc absolute aSource;
begin
  inherited AssignFrom(aSource);
  Self.BrokerType      := Source.BrokerType;
  Self.Currency        := Source.Currency;
  Self.ColTick         := Source.ColTick;
  Self.Decimals        := Source.Decimals;
  Self.Description     := Source.Description;
  Self.Exchange        := Source.Exchange;
  Self.Id              := Source.Id;
  Self.Limit           := Source.Limit;
  Self.InstrumentName  := Source.InstrumentName;
  Self.IsRepetitive    := Source.IsRepetitive;
  Self.IsActivateChild := Source.IsActivateChild;
  Self.OrderAction     := Source.OrderAction;
  Self.OrderType       := Source.OrderType;
  Self.Quantity        := Source.Quantity;
  Self.Symbol          := Source.Symbol;
  Self.ParentOrder     := Source.ParentOrder;
  Self.TriggerMethod   := Source.TriggerMethod;
  Self.IsFinal         := Source.IsFinal;
  Self.Filled          := 0;
  Self.Info            := '';
  Self.IsExecuted      := False;
  Self.OrderIBId       := 0;
  Self.OrderStatus     := osSleeping;
end;

procedure TCustomOrderDoc.SaveToDB;
begin
  //
end;

function TCustomOrderDoc.GetCalculationStageItem(Index: TCalculationStage): Double;
begin
  Result := FCalculationStage[Index];
end;

procedure TCustomOrderDoc.SetCalculationStageItem(Index: TCalculationStage; const Value: Double);
begin
  FCalculationStage[Index] := SimpleRoundTo(Value, -C_DECIMALS);
end;

procedure TCustomOrderDoc.SetOrderIBId(const Value: Integer);
begin
  FOrderIBId := Value;
  FOrderList.Add(Value.ToString);
end;

procedure TCustomOrderDoc.SetOrderType(const Value: TIABOrderType);
begin
  FOrderType := Value;
end;

function TCustomOrderDoc.ToString: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat('Symbol=%s, ', [Self.Symbol])
      .AppendFormat('BrokerName=%s, ', [Self.BrokerType.ToString])
      .AppendFormat('Id=%d, ', [Self.Id])
      .AppendFormat('Currency=%s, ', [Self.Currency])
      .AppendFormat('Description=%s, ', [Self.Description])
      .AppendFormat('Exchange=%s, ', [Self.Exchange])
      .AppendFormat('PrimaryExchange=%s, ', [Self.PrimaryExchange])
      .AppendFormat('OrderAction=%s, ', [Self.OrderAction.ToString])
      .AppendFormat('Filled=%d, ', [Self.Filled])
      .AppendFormat('InstrumentName=%s, ', [Self.InstrumentName])
      .AppendFormat('LatestFillPrice=%f, ', [Self.LatestFillPrice])
      .AppendFormat('Limit=%f, ', [Self.Limit])
      .AppendFormat('Multiplier=%f, ', [Self.Multiplier])
      .AppendFormat('OrderIBId=%d, ', [Self.OrderIBId])
      .AppendFormat('OrderStatus=%s, ', [Self.OrderStatusText])
      .AppendFormat('OrderType=%s, ', [Self.OrderType.ToString])
      .AppendFormat('Quantity=%d, ', [Self.Quantity])
      .AppendFormat('MarketList=%s, ', [Self.MarketList])
      .AppendFormat('AutoTradesInstance=%d, ', [Self.AutoTradesInstance])
      .AppendFormat('QualifierInstance=%d, ', [Self.QualifierInstance])
      .AppendFormat('QualifierID=%d, ', [Self.QualifierID])
      .AppendFormat('AutoTradesID=%d, ', [Self.AutoTradesID])
      .AppendFormat('OrderIBId=%d, ', [Self.OrderIBId])
      .AppendFormat('IsFinal=%s, ', [BoolToStr(Self.IsFinal, True)]);
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

function TCustomOrderDoc.ToValueString: string;
begin
  Result := Self.OrderAction.ToString + ' ' +
            Self.OrderType.ToString;
end;

procedure TCustomOrderDoc.AddPrice(const Price: Double; const Quantity: Integer);
var
  i   : Integer;
  SumPrice : Double;
  SumQuant: Integer;
begin
  SumPrice := 0;
  SumQuant := 0;
  SetLength(FPrices, Length(FPrices) + 1);
  FPrices[Length(FPrices) - 1].Price    := Price;
  FPrices[Length(FPrices) - 1].Quantity := Quantity;
  for i := Low(FPrices) to High(FPrices) do
  begin
    SumPrice := SumPrice + FPrices[i].Price * FPrices[i].Quantity;
    SumQuant := SumQuant + FPrices[i].Quantity;
  end;
  if (SumQuant > 0) then
  begin
    FAvgPrice := SumPrice / SumQuant;
    FCalculationStage[csAvgPrice] := FAvgPrice;
  end;
end;

{ TCustomOrderDoc.TExtendedOptions }

procedure TCustomOrderDoc.TExtendedOptions.AssignFrom(const aSource: TExtendedOptions);
begin
  Self.AuxPriceRelative       := aSource.AuxPriceRelative;
  Self.LimitPriceOffset       := aSource.LimitPriceOffset;
  Self.LimitPriceRelative     := aSource.LimitPriceRelative;
  Self.OpenVolume             := aSource.OpenVolume;
  Self.Subordination          := aSource.Subordination;
  Self.TrailStopPriceRelative := aSource.TrailStopPriceRelative;
  Self.VolumePercent          := aSource.VolumePercent;
  Self.AuxBasePrice           := aSource.AuxBasePrice;
  Self.LimitBasePrice         := aSource.LimitBasePrice;
  Self.LmtOffsetBasePrice     := aSource.LmtOffsetBasePrice;
  Self.TrailStopBasePrice     := aSource.TrailStopBasePrice;
  Self.BasisForPrice          := aSource.BasisForPrice;
end;

{ TAlgosDoc.TPrice }

constructor TAlgosDoc.TPrice.Create(aPrice: Double);
begin
  Price := aPrice;
end;

{ TOrderGroupDoc }

constructor TOrderGroupDoc.Create;
begin
  FCheckpointPeriod := 100;
  FOrderStatusQueue := TOrderStatusQueue.Create(Self);
end;

destructor TOrderGroupDoc.Destroy;
begin
  TPublishers.OrderStatePublisher.Unsubscribe(Self.FOrderStatusQueue);
  FreeAndNil(FOrderStatusQueue);
  inherited;
end;

procedure TOrderGroupDoc.AddOrderToQueue(aOrderDoc: TCustomOrderDoc);
begin
  Self.FOrderStatusQueue.AddItem(aOrderDoc);
end;

procedure TOrderGroupDoc.AssignFrom(const aSource: TCustomDocument);
var
  Source: TOrderGroupDoc absolute aSource;
begin
  inherited AssignFrom(aSource);
  Self.CheckpointPeriod := Source.CheckpointPeriod;
  Self.IsAutoOrder      := Source.IsAutoOrder;
  Self.IsRepetitive     := Source.IsRepetitive;
  Self.Kind             := Source.Kind;
  Self.Name             := Source.Name;
  Self.RecordId         := Source.RecordId;
end;

procedure TOrderGroupDoc.FromDB(const aRecordId: Integer);
resourcestring
  C_SQL_TEXT = 'SELECT * FROM ORDER_GROUP WHERE ID=:RecordId';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_TEXT;
    try
      Query.ParamByName('RecordId').AsInteger := aRecordId;
      Query.Prepare;
      Query.Open;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'FromDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
    Query.First;
    if not Query.IsEmpty then
    begin
      Self.RecordId     := aRecordId;
      Self.CheckpointPeriod := Query.FieldByName('CHECKPOINT_PERIOD').AsInteger;
      Self.IsRepetitive     := Query.FieldByName('REPETIVE').AsBoolean;
      Self.Kind             := TOrderKind(Query.FieldByName('ORDER_KIND').AsInteger);
      Self.Name             := Query.FieldByName('NAME').AsString;
    end;
    Self.RecordId := aRecordId;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TOrderGroupDoc.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM ORDER_GROUP WHERE ID = :RecordId';
  C_SQL_UPDATE_TEXT = 'UPDATE ORDER_GROUP SET '                 +
                      'NAME              = :OrderGroupName, '   +
                      'REPETIVE          = :IsRepetitive, '     +
                      'CHECKPOINT_PERIOD = :CheckpointPeriod, ' +
                      'ORDER_KIND        = :Kind '              +
                      ' WHERE ID         = :RecordId';

  C_SQL_INSERT_TEXT = 'INSERT INTO ORDER_GROUP ' +
                      '(ID,NAME,REPETIVE,CHECKPOINT_PERIOD,ORDER_KIND)' +
                      ' VALUES(:RecordId, '         +
                      '        :OrderGroupName, '   +
                      '        :IsRepetitive, '     +
                      '        :CheckpointPeriod, ' +
                      '        :Kind)';
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
    if (RecordId > 0) then
    begin
      Query.SQL.Text := C_SQL_EXISTS_TEXT;
      try
        Query.ParamByName('RecordId').AsInteger := RecordId;
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
    end
    else
      RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');

    if IsExists then
      Query.SQL.Text := C_SQL_UPDATE_TEXT
    else
      Query.SQL.Text := C_SQL_INSERT_TEXT;

    Query.ParamByName('CheckpointPeriod').AsInteger := Self.CheckpointPeriod;
    Query.ParamByName('IsRepetitive').AsBoolean     := Self.IsRepetitive;
    Query.ParamByName('Kind').AsInteger             := Integer(Self.Kind);
    Query.ParamByName('OrderGroupName').AsString    := Self.Name.Substring(0, 200);
    Query.ParamByName('RecordId').AsInteger         := RecordId;
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveToDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TOrderGroupDoc.SetKind(const Value: TOrderKind);
begin
  if (FKind <> Value) then
  begin
    FKind := Value;
    if (FKind = okModifyOrder) then
    begin
      TPublishers.OrderStatePublisher.Subscribe(Self.FOrderStatusQueue);
    end;
  end;
end;

function TOrderGroupDoc.ToValueString: string;
begin
  Result := Self.Kind.ToString;
end;

{ TOrderTestDoc }

procedure TOrderTestDoc.FromDB(const aRecordId: Integer);
begin
  inherited;
  Self.RecordId := aRecordId;
end;

procedure TOrderTestDoc.SaveToDB;
begin
  inherited;

end;

procedure TOrderTestDoc.Buy;
begin

end;

procedure TOrderTestDoc.CancelOrder;
begin

end;

procedure TOrderTestDoc.DoBuy;
begin

end;

function TOrderTestDoc.GetOrderStatusText: string;
begin

end;

function TOrderTestDoc.IsPossibleToBuyOrder(aNode: PVirtualNode): Boolean;
begin
  Result := False;
end;

procedure TOrderTestDoc.UpdateOrder;
begin

end;

{ TOrderGroupSetDoc }

procedure TOrderGroupSetDoc.FromDB(const aRecordId: Integer);
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT * FROM ORDER_GROUP_SET WHERE ID=:RecordId';
var
  Query: TFDQuery;
begin
  if (aRecordId > 0) then
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := C_SQL_SELECT_TEXT;
      Query.ParamByName('RecordId').AsInteger := aRecordId;
      Query.Open;
      if not Query.IsEmpty then
      begin
        Self.Name     := Query.FieldByName('NAME').AsString;
        Self.TypeUse  := TTypeUseInAutoorder(Query.FieldByName('TYPE_USE').AsInteger);
      end;
      Self.RecordId := aRecordId;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TOrderGroupSetDoc.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM ORDER_GROUP_SET WHERE ID = :RecordId';
  C_SQL_UPDATE_TEXT = 'UPDATE ORDER_GROUP_SET SET '                +
                      'NAME      = :Name,   ' +
                      'TYPE_USE  = :TypeUse ' +
                      ' WHERE ID = :RecordId';

  C_SQL_INSERT_TEXT = 'INSERT INTO ORDER_GROUP_SET ' +
                      '(ID,TYPE_USE,NAME)' +
                      ' VALUES(:RecordId, ' +
                      '        :TypeUse, '  +
                      '        :Name)';
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
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TOrderGroupSetDoc.SaveToDB', 'MonitorTypes', E.Message + TDModUtils.GetQueryInfo(Query));
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
      Self.Name := 'Order Groups nr' + Self.RecordId.ToString;

    Query.ParamByName('RecordId').AsInteger := Self.RecordId;
    Query.ParamByName('TypeUse').AsInteger  := Integer(Self.TypeUse);
    Query.ParamByName('Name').AsString      := Self.Name.Substring(0, 200);
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TOrderGroupSetDoc.SaveToDB', 'MonitorTypes', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TOrderGroupSetDoc.AssignFrom(const aSource: TCustomDocument);
var
  Source: TOrderGroupSetDoc absolute aSource;
begin
  inherited AssignFrom(aSource);
  Self.Name    := Source.Name;
  Self.TypeUse := Source.TypeUse;
end;

{ TOrderStatusQueue }

constructor TOrderStatusQueue.Create(aOwnerOrderGroupDoc: TOrderGroupDoc);
begin
  inherited Create;
  FOwnerOrderGroupDoc := aOwnerOrderGroupDoc;
end;

destructor TOrderStatusQueue.Destroy;
begin
  if Assigned(FTask) and (FTask.Status = TTaskStatus.Running) then
    FTask.Cancel;
  inherited;
end;

procedure TOrderStatusQueue.AddItem(aOrderDoc: TCustomOrderDoc);
begin
  if (Self.Count = 0) then
  begin
    Self.Add(aOrderDoc);
    aOrderDoc.DoBuy;
  end
  else
  if (Self.IndexOf(aOrderDoc) = -1) then
  begin
    Self.Add(aOrderDoc);
//    if not Assigned(FTask) or (FTask.Status = TTaskStatus.Completed) then
//      CreateTask;
//    if (Self.Count > 0) and (FTask.Status = TTaskStatus.Created) then
//      FTask.Start;
  end;
end;

(* not used
procedure TOrderStatusQueue.CreateTask;
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'CreateTask');
  FTask := TTask.Create(
    procedure()
    var
      Data: PTreeData;
    begin
      Data := nil;
      TThread.NameThreadForDebugging('InstrumentList.TOrderStatusQueue.FTask');
      while Self.Count > 0 do
      begin
        if (FTask.Status = TTaskStatus.Canceled) then
          Break;
//        Data := PVirtualNode(Self.Dequeue).GetData;
        if Assigned(Data) and Assigned(Data.OrderDoc) then
          Data.OrderDoc.DoBuy;
      end;
    end);
  TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'CreateTask');
end;
*)

function TOrderStatusQueue.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TOrderStatusQueue.OnCloseOrder(const aTempId: Integer);
begin

end;

procedure TOrderStatusQueue.OnExecution(Sender: TObject; Order: TIABOrder);
begin

end;

procedure TOrderStatusQueue.OnOpenOrder(Sender: TObject; Order: TIABOrder);
begin

end;

procedure TOrderStatusQueue.OnOpenOrderNN(const aOrderList: array of TOrder);
begin

end;

procedure TOrderStatusQueue.OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
begin

end;

procedure TOrderStatusQueue.OnRebuildFromTWS(Sender: TObject);
begin

end;

{ TInstrument }

procedure TInstrument.AssignFrom(const aInstrument: TInstrument);
begin
  Self.SokidInfo     := aInstrument.SokidInfo;
end;

end.
