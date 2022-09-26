unit IABSocketAPI;

//**************************************************************************
//
//    IABSocketAPI - for use with InteractiveBrokers TradeWorkStation(TWS) and
//                   the IB Gateway, direct order placement API.
//
//    by Ross Hemingway - support@hhssoftware.com - Sep 2022
//
//    https://www.hhssoftware.com/iabsocketapi/
//
//    This version is built to the TWS API spec version 10.18.1
//
//    Note that any API version will work with any TWS version (in the last 5 years).
//    Both ends have version control inbuilt.
//
//    This API will function on Delphi 6 upwards, including all the XE, 10 and 11 versions.
//
//    However, the use of BigDecimals requires XE2 or later: see the end of the
//    IABSocketAPI_const.pas file for details.
//
//    Requires use of companion files IABSocketAPI_const, IABSocketAPI_tcpclient.
//
//
//**************************************************************************
//
//    For help with this API / component set - see the help file at the above site.
//    Also see the readme.txt file attached in this zip package.
//
//    Most of the settings and parameters are common with the C++ API help.  Read this at:
//      https://interactivebrokers.github.io/tws-api/index.html
//
//    This file has Nine classes defined.  Install just the TIABSocket into the library.
//    The other objects compliment the TIABSocket component.
//
//**************************************************************************



{ **********   64 bit    **************

    To compile as 64 bit, make the following change to the package file
    config that the component is installed in:

      remove / comment out the   requires unit designide.dcp

  ***************************  }


{  *******   BCB  ********

    Un-comment the compiler define directive below.
    Also the demo app has a similar compiler directive.}

// {$DEFINE BCBCOMPILE}

{  ***************************  }



{  *******   BigDecimal / bitcoin  ********

    See the comments at end of IABSocketAPI_const file

{  ***************************  }




{  *******   Debugging  ********

   Use this to capture the current raw data from the
   incoming TWS socket.  It will generate three files in
   the current directory named LastStreamIn.txt and CrntStreamIn.txt, and CrntStreamOut.txt.
   Note these files will have many embedded null's.  Use a better text reader or hex viewer.
   Very usefull when the stream blows up.. can trace through the
   data to spot the bogus or invalid data.  }

//  {$DEFINE CAPTURE_TWS_STREAM}

{  ***************************  }



{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
{$IF CompilerVersion < 24.0}  // 24 = XE3
SysUtils, Classes, Windows, Winsock, Math,
{$ELSE}
System.SysUtils, System.Classes, Winapi.Windows, Winapi.WinSock, System.Math,
{$IFEND}
IABSocketAPI_const, IABSocketAPI_tcpclient
{$IFDEF USE_BIGDECIMAL}
, Velthuis.BigDecimals  // see end of IABSocketAPI_const.pas for information on this.
{$ENDIF}
;


type
  TIABSocket = class;  // forward declaration

  TIABScan = class(TPersistent)
  private
    FScanItems: TIABScanResultItemArray;
    FQueryCriteria: TIABScanCriteria;
    FScanId: Integer;
    function GetCount: Integer;
    function GetItems(Index: Integer): {$IFDEF BCBCOMPILE} PTIABScanResultItem; {$ELSE} TIABScanResultItem; {$ENDIF}
    procedure SetItems(Index: Integer; const Value: {$IFDEF BCBCOMPILE} PTIABScanResultItem); {$ELSE} TIABScanResultItem); {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function Add(IABScanResultItem: TIABScanResultItem): Integer;
    procedure Insert(Index: Integer; IABScanResultItem: TIABScanResultItem);
    procedure Delete(Index: Integer);
    function Find(IABScanResultItem: TIABScanResultItem): Integer;
    function QueryCriteria: TIABScanCriteria;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: {$IFDEF BCBCOMPILE} PTIABScanResultItem {$ELSE} TIABScanResultItem {$ENDIF} read GetItems write SetItems; default;
    property ScanId: Integer read FScanId;
  end;

  TIABScanner = class(TPersistent)
  private
    FIABSocket: TIABSocket;
    FList: TList;
    FParameters: string;
    function GetCount: Integer;
    function GetItems(Index: Integer): TIABScan;
  public
    constructor Create(Creator: TIABSocket);
    destructor Destroy; override;
    function Add(IABScan: TIABScan): Integer;
    procedure CancelScan(ScanId: Integer);
    procedure Delete(Index: Integer);
    procedure GetScannerParameters;
    procedure Insert(Index: Integer; IABScan: TIABScan);
    procedure InitializeScanCriteria(PDetails: PTIABScanCriteria);
    function IndexOfScanId(ScanId: Integer): Integer;
    function NewScan(ScanId: Integer; Details: TIABScanCriteria): Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TIABScan read GetItems; default;
    property Parameters: string read FParameters;
  end;

  TIABPortfolio = class(TPersistent)
  private
    FList: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): {$IFDEF BCBCOMPILE} PTIABPortfolioItem; {$ELSE} TIABPortfolioItem; {$ENDIF}
    procedure SetItems(Index: Integer; {$IFDEF BCBCOMPILE} const Value: PTIABPortfolioItem); {$ELSE} const Value: TIABPortfolioItem); {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function Add(IABPortfolioItem: TIABPortfolioItem): Integer;
    procedure Insert(Index: Integer; IABPortfolioItem: TIABPortfolioItem);
    procedure Delete(Index: Integer);
    function Find(IABPortfolioItem: TIABPortfolioItem): Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: {$IFDEF BCBCOMPILE} PTIABPortfolioItem {$ELSE} TIABPortfolioItem {$ENDIF} read GetItems write SetItems; default;
  end;

  TIABInstrumentSpec = class(TPersistent)
  private
    FList: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): {$IFDEF BCBCOMPILE} PTIABInstrumentSpecItem; {$ELSE} TIABInstrumentSpecItem; {$ENDIF}
    procedure SetItems(Index: Integer; {$IFDEF BCBCOMPILE} const Value: PTIABInstrumentSpecItem); {$ELSE} const Value: TIABInstrumentSpecItem); {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function Add(IABInstrumentSpecItem: TIABInstrumentSpecItem): Integer;
    procedure Insert(Index: Integer; IABInstrumentSpecItem: TIABInstrumentSpecItem);
    procedure Delete(Index: Integer);
    function Find(IABInstrumentSpecItem: TIABInstrumentSpecItem): Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: {$IFDEF BCBCOMPILE} PTIABInstrumentSpecItem {$ELSE} TIABInstrumentSpecItem {$ENDIF} read GetItems write SetItems; default;
  end;

  TIABBondSpec = class(TPersistent)
  private
    FList: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): {$IFDEF BCBCOMPILE} PTIABBondSpecItem; {$ELSE} TIABBondSpecItem; {$ENDIF}
    procedure SetItems(Index: Integer; {$IFDEF BCBCOMPILE} const Value: PTIABBondSpecItem); {$ELSE} const Value: TIABBondSpecItem); {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function Add(IABBondSpecItem: TIABBondSpecItem): Integer;
    procedure Insert(Index: Integer; IABBondSpecItem: TIABBondSpecItem);
    procedure Delete(Index: Integer);
    function Find(IABBondSpecItem: TIABBondSpecItem): Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: {$IFDEF BCBCOMPILE} PTIABBondSpecItem {$ELSE} TIABBondSpecItem {$ENDIF} read GetItems write SetItems; default;
  end;


  TIABOrder = class(TPersistent)
  private
    FAccount: string;
    FAction: TIABAction;
    FAuxPrice: Double;
    FCurrency: string;
    FExchange: string;
    FExpiry: string; // now called lastTradeDateOrContractMonth;
    FLocalSymbol: string;
    FOCAgroup: string;
    FOpenClose: Char;
    FOrderOrigin: TIABOrderOrigin;
    FOrderRef: string;
    FOrderType: TIABOrderType;
    FPrice: Double;
    FQuantity: BigDecimal;//Double;
    FRight: TIABRight;
    FSecurityType: TIABSecurityType;
    FStrike: Double;
    FSymbol: string;
    FTimeInForce: TIABTimeInForce;
    FTransmit: Boolean;
    FCompleted: Boolean;
    FTempId: Integer;
    FPermId: Integer;
    FFilled: BigDecimal;//Double;
    FRemaining: BigDecimal;//Double;
    FFillPrice: Double;
    FLatestFillQty: BigDecimal;//Double;
    FLatestFillPrice: Double;
    FChanged: Boolean;
    FParentId: Integer;
    FBlockOrder: Boolean;
    FSweepToFill: Boolean;
    FDisplaySize: Integer;
    FTriggerMethod: Integer;
    FExtendedHours: Boolean;
    FHidden: Boolean;
    FOnFill: TNotifyEvent;
    FClientId: Integer;
    FOnCompleted: TNotifyEvent;
    FExecutions: TIABExecutionArray;
    FComboLegs: TIABComboLegArray;
    //FShareAllocation: string;  dropped 10.10
    FExternalOrder: Boolean;
    FDiscretAmount: Double;
    FGoodAfterTime: string;
    FFAdvGroup: string;
    FFAdvProfile: string;
    FFAdvMethod: string;
    FFAdvPercentage: string;
    FGoodTillDate: string;
    FPrimaryExchange: string;
    FMultiplier: string;
    FShortSaleSlot: Integer;
    FDesignatedLocation: string;
    FOcaMethod: TIABOcaMethod;
    //FRegTradingHoursOnly: Boolean;  dropped 10.10
    FRule80A: TIABRule80A;
    FSettlingFirm: string;
    FAllOrNone: Boolean;
    FMinQuantity: Integer;
    FPercentOffset: Double;
//    FETradeOnly: Boolean;   // dropped in TWS API 10.10
//    FFirmQuoteOnly: Boolean;   // dropped in TWS API 10.10
//    FNBBOPriceCap: Double;   // dropped in TWS API 10.10
    FAuctionStrategy: TIABAuctionStrategy;
    FStartingPrice: Double;
    FStockRefPrice: Double;
    FDelta: Double;
    FStockRangeLower: Double;
    FStockRangeUpper: Double;
    FOverridePercentageConstraints: Boolean;
    FVolatility: Double;
    FVolatilityPeriod: TIABVolatilityPeriod;
    FDeltaNeutralOrderType: TIABOrderType;
    FDeltaNeutralAuxPrice: Double;
    FDeltaNeutralConId: Integer;
    FDeltaNeutralSettlingFirm: string;
    FDeltaNeutralClearingAccount: string;
    FDeltaNeutralClearingIntent: string;
    FDeltaNeutralOpenClose: string;
    FDeltaNeutralShortSale: Boolean;
    FDeltaNeutralShortSaleSlot: Integer;
    FDeltaNeutralDesignatedLocation: string;
    FContinuousUpdate: Boolean;
    FReferencePrice: TIABReferencePrice;
    FTrailStopPrice: Double;
    FTrailingPercent: Double;
    FComboLegsDescrip: string;
    FEFPBasisPoints: Double;
    FEFPBasisType: Integer;
    FWhyHeld: string;
    FScaleInitLevelSize: Integer;
    FScaleSubsLevelSize: Integer;
    FScalePriceIncrement: Double;
    FScalePriceAdjustValue: Double;
    FScalePriceAdjustInterval: Integer;
    FScaleProfitOffset: Double;
    FScaleAutoReset: Boolean;
    FScaleInitPosition: Integer;
    FScaleInitFillQty: Integer;
    FScaleRandomPercent: Boolean;
    FContractId: Integer;
    FClearingAccount: string;
    FClearingIntent: string;
    FVerified: Boolean;
    FQueryResult: TIABOrderQueryResult;
    FDeltaNeutralContractId: Integer;
    FDeltaNeutralContractDelta: Double;
    FDeltaNeutralContractPrice: Double;
    FSecurityID: string;
    FSecurityIDType: string;
    FNotHeld: Boolean;
    FExemptCode: Integer;
    FOptOutSmartRouting: Boolean;
	  FHedgeType: TIABHedgeType;  // 'D' - delta, 'B' - beta, 'F' - FX, 'P' - pair
	  FHedgeParam: string; // 'beta=X' value for beta hedge, 'ratio=Y' for pair hedge
    FAlgoStrategy: string;
    FAlgoParams: TIABTagValueArray;
	  FSmartComboRoutingParams: TIABTagValueArray;
    FComboLegPrice: array of Double;
    FActiveStartTime: string;
    FActiveStopTime: string;
    FAlgoId: string;
    FScaleTable: string;
    FTradingClass: string;
    FSolicited: Boolean;
    FRandomizeSize: Boolean;
    FRandomizePrice: Boolean;
    FModelCode: string;
    FExtOperator: string;
    FOrderMiscOptions: TIABTagValueArray;
    FMktDataOptions: TIABTagValueArray;
  	FReferenceContractId: Integer;
	  FPeggedChangeAmount: Double;
	  FIsPeggedChangeAmountDecrease: Boolean;
	  FReferenceChangeAmount: Double;
	  FReferenceExchangeId: string;
	  FAdjustedOrderType: TIABOrderType;
	  FTriggerPrice: Double;
	  FAdjustedStopPrice: Double;
	  FAdjustedStopLimitPrice: Double;
	  FAdjustedTrailingAmount: Double;
	  FAdjustableTrailingUnit: Integer;
	  FLmtPriceOffset: Double;
    FSoftDollarTier: TIABSoftDollarTier;
    FCashQuantity: Double;
    FMifid2DecisionMaker: string;
    FMifid2DecisionAlgo: string;
    FMifid2ExecutionTrader: string;
    FMifid2ExecutionAlgo: string;
	  FDontUseAutoPriceForHedge: Boolean;
    FIsOmsContainer: Boolean;
    FDiscretionaryUpToLimitPrice: Boolean;
    FAutoCancelDate: string;
    FFilledQuantity: BigDecimal;//Double;
    FRefFuturesConId: Integer;
    FAutoCancelParent: Boolean;
    FShareholder: string;
    FImbalanceOnly: Boolean;
    FRouteMarketableToBbo: Boolean;
    FParentPermId: Int64;
    FUsePriceMgmtAlgo: Integer;
  	FDuration: Integer;
  	FPostToAts: Integer;
    FAdvancedErrorOverride: string;
	  FManualOrderTime: TDateTime;
	  FMinTradeQty: Integer;
	  FMinCompeteSize: Integer;
	  FCompeteAgainstBestOffset: Double;
	  FMidOffsetAtWhole: Double;
	  FMidOffsetAtHalf: Double;
    FCompletedTime: string;
    FCompletedStatus: string;
    FMarketCapPrice: Double;
    procedure SetExpiry(Value: string);
    function GetExecutions(Index: Integer): {$IFDEF BCBCOMPILE} PTIABExecution; {$ELSE} TIABExecution; {$ENDIF}
    procedure SetExecutions(Index: Integer; const Value: {$IFDEF BCBCOMPILE} PTIABExecution); {$ELSE} TIABExecution); {$ENDIF}
    function GetExecutionsCount: Integer;
    function GetComboLegs(Index: Integer): TIABComboLeg;
    function GetComboLegsCount: Integer;
    procedure SetComboLegs(Index: Integer; const Value: TIABComboLeg);
    //procedure SetShareAllocation(Value: string);  dropped 10.10
    //function GetShareAllocation: string;  dropped 10.10
    //procedure SetRegTradingHoursOnly(Value: Boolean);
    //function GetRegTradingHoursOnly: Boolean;
    function GetPropertyDeprecated: Integer;
    procedure SetPropertyDeprecated(Value: Integer);
    function GetAlgoParams(Index: Integer): TIABTagValue;
    procedure SetAlgoParams(Index: Integer; TagValue: TIABTagValue);
    function GetAlgoParamsCount: Integer;
    function GetSmartComboRoutingParams(Index: Integer): TIABTagValue;
    procedure SetSmartComboRoutingParams(Index: Integer; TagValue: TIABTagValue);
    function GetSmartComboRoutingParamsCount: Integer;
    function GetOrderMiscOptions(Index: Integer): TIABTagValue;
    procedure SetOrderMiscOptions(Index: Integer; TagValue: TIABTagValue);
    function GetOrderMiscOptionsCount: Integer;
    function GetMktDataOptions(Index: Integer): TIABTagValue;
    procedure SetMktDataOptions(Index: Integer; TagValue: TIABTagValue);
    function GetMktDataOptionsCount: Integer;
    function GetComboLegPrice(Index: Integer): Double;
    procedure SetComboLegPrice(Index: Integer; Price: Double);
    function GetComboLegPriceCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AddComboLeg(ComboLeg: TIABComboLeg): Integer;
    procedure DeleteComboLeg(Index: Integer);
    function GetQueryResult: {$IFDEF BCBCOMPILE} PTIABOrderQueryResult; {$ELSE} TIABOrderQueryResult; {$ENDIF}
    function AddAlgoParams(TagValue: TIABTagValue): Integer;
    procedure ClearAlgoParams;
    function AddSmartComboRoutingParams(TagValue: TIABTagValue): Integer;
    procedure ClearSmartComboRoutingParams;
    function AddOrderMiscOptions(TagValue: TIABTagValue): Integer;
    procedure ClearOrderMiscOptions;
    function AddMktDataOptions(TagValue: TIABTagValue): Integer;
    procedure ClearMktDataOptions;
    function AddComboLegPrice(Price: Double): Integer;
    procedure ClearComboLegPrice;
    property ClientId: Integer read FClientId write FClientId;
    property Changed: Boolean read FChanged write FChanged;
    property Completed: Boolean read FCompleted write FCompleted;
    property TempId: Integer read FTempId write FTempId;
    property PermId: Integer read FPermId write FPermId;
    property Filled: BigDecimal read FFilled write FFilled;
    property Remaining: BigDecimal read FRemaining write FRemaining;
    property FillPrice: Double read FFillPrice write FFillPrice;
    property LatestFillQty: BigDecimal read FLatestFillQty write FLatestFillQty;
    property LatestFillPrice: Double read FLatestFillPrice write FLatestFillPrice;
    property Executions[Index: Integer]: {$IFDEF BCBCOMPILE} PTIABExecution {$ELSE} TIABExecution {$ENDIF} read GetExecutions write SetExecutions;
    property ExecutionsCount: Integer read GetExecutionsCount;
    property ComboLegs[Index: Integer]: TIABComboLeg read GetComboLegs write SetComboLegs;
    property ComboLegsCount: Integer read GetComboLegsCount;
    //property ShareAllocation: string read GetShareAllocation write SetShareAllocation;   Dropped 10.10
    property DiscretAmount: Double read FDiscretAmount write FDiscretAmount;
    property GoodAfterTime: string read FGoodAfterTime write FGoodAfterTime;
    property FAdvGroup: string read FFAdvGroup write FFAdvGroup;
    property FAdvProfile: string read FFAdvProfile write FFAdvProfile;
    property FAdvMethod: string read FFAdvMethod write FFAdvMethod;
    property FAdvPercentage: string read FFAdvPercentage write FFAdvPercentage;
    property GoodTillDate: string read FGoodTillDate write FGoodTillDate;
    property Multiplier: string read FMultiplier write FMultiplier;
    property ShortSaleSlot: Integer read FShortSaleSlot write FShortSaleSlot;
    property DesignatedLocation: string read FDesignatedLocation write FDesignatedLocation;
    property OcaMethod: TIABOcaMethod read FOcaMethod write FOcaMethod;
   //property RegTradingHoursOnly: Boolean read GetRegTradingHoursOnly write SetRegTradingHoursOnly;   dropped 10.10
    property Rule80A: TIABRule80A read FRule80A write FRule80A;
    property SettlingFirm: string read FSettlingFirm write FSettlingFirm;
    property AllOrNone: Boolean read FAllOrNone write FAllOrNone;
    property MinQuantity: Integer read FMinQuantity write FMinQuantity;
    property PercentOffset: Double read FPercentOffset write FPercentOffset;
//    property ETradeOnly: Boolean read FETradeOnly write FETradeOnly;            // dropped in TWS API 10.10
//    property FirmQuoteOnly: Boolean read FFirmQuoteOnly write FFirmQuoteOnly;   // dropped in TWS API 10.10
//    property NBBOPriceCap: Double read FNBBOPriceCap write FNBBOPriceCap;       // dropped in TWS API 10.10
    property AuctionStrategy: TIABAuctionStrategy read FAuctionStrategy write FAuctionStrategy;
    property StartingPrice: Double read FStartingPrice write FStartingPrice;
    property StockRefPrice: Double read FStockRefPrice write FStockRefPrice;
    property Delta: Double read FDelta write FDelta;
    property StockRangeLower: Double read FStockRangeLower write FStockRangeLower;
    property StockRangeUpper: Double read FStockRangeUpper write FStockRangeUpper;
    property OverridePercentageConstraints: boolean read FOverridePercentageConstraints write FOverridePercentageConstraints;
    property Volatility: Double read FVolatility write FVolatility;
    property VolatilityPeriod: TIABVolatilityPeriod read FVolatilityPeriod write FVolatilityPeriod;
    property DeltaNeutralOrderType: TIABOrderType read FDeltaNeutralOrderType write FDeltaNeutralOrderType;
    property DeltaNeutralAuxPrice: Double read FDeltaNeutralAuxPrice write FDeltaNeutralAuxPrice;
    property DeltaNeutralConId: Integer read FDeltaNeutralConId write FDeltaNeutralConId;
    property DeltaNeutralSettlingFirm: string read FDeltaNeutralSettlingFirm write FDeltaNeutralSettlingFirm;
    property DeltaNeutralClearingAccount: string read FDeltaNeutralClearingAccount write FDeltaNeutralClearingAccount;
    property DeltaNeutralClearingIntent: string read FDeltaNeutralClearingIntent write FDeltaNeutralClearingIntent;
    property DeltaNeutralOpenClose: string read FDeltaNeutralOpenClose write FDeltaNeutralOpenClose;
    property DeltaNeutralShortSale: Boolean read FDeltaNeutralShortSale write FDeltaNeutralShortSale;
    property DeltaNeutralShortSaleSlot: Integer read FDeltaNeutralShortSaleSlot write FDeltaNeutralShortSaleSlot;
    property DeltaNeutralDesignatedLocation: string read FDeltaNeutralDesignatedLocation write FDeltaNeutralDesignatedLocation;
    property ContinuousUpdate: Boolean read FContinuousUpdate write FContinuousUpdate;
    property ReferencePrice: TIABReferencePrice read FReferencePrice write FReferencePrice;
    property TrailStopPrice: Double read FTrailStopPrice write FTrailStopPrice;
    property TrailingPercent: Double read FTrailingPercent write FTrailingPercent;
    property ComboLegsDescrip: string read FComboLegsDescrip;
    property EFPBasisPoints: Double read FEFPBasisPoints;
    property EFPBasisType: Integer read FEFPBasisType;
    property WhyHeld: string read FWhyHeld write FWhyHeld;
    property ScaleNumComponents: Integer read GetPropertyDeprecated write SetPropertyDeprecated; //deprecated in 9.50
    property ScaleComponentSize: Integer read GetPropertyDeprecated write SetPropertyDeprecated; //deprecated in 9.50
    property ScaleInitLevelSize: Integer read FScaleInitLevelSize write FScaleInitLevelSize;
    property ScaleSubsLevelSize: Integer read FScaleSubsLevelSize write FScaleSubsLevelSize;
    property ScalePriceIncrement: Double read FScalePriceIncrement write FScalePriceIncrement;
    property ScalePriceAdjustValue: Double read FScalePriceAdjustValue write FScalePriceAdjustValue;
    property ScalePriceAdjustInterval: Integer read FScalePriceAdjustInterval write FScalePriceAdjustInterval;
    property ScaleProfitOffset: Double read FScaleProfitOffset write FScaleProfitOffset;
    property ScaleAutoReset: Boolean read FScaleAutoReset write FScaleAutoReset;
    property ScaleInitPosition: Integer read FScaleInitPosition write FScaleInitPosition;
    property ScaleInitFillQty: Integer read FScaleInitFillQty write FScaleInitFillQty;
    property ScaleRandomPercent: Boolean read FScaleRandomPercent write FScaleRandomPercent;
    property ContractId: Integer read FContractId write FContractId;
    property ClearingAccount: string read FClearingAccount write FClearingAccount;
    property ClearingIntent: string read FClearingIntent write FClearingIntent;
    property Verified: Boolean read FVerified;
    property DeltaNeutralContractId: Integer read FDeltaNeutralContractId write FDeltaNeutralContractId;
    property DeltaNeutralContractDelta: Double read FDeltaNeutralContractDelta write FDeltaNeutralContractDelta;
    property DeltaNeutralContractPrice: Double read FDeltaNeutralContractPrice write FDeltaNeutralContractPrice;
    property NotHeld: Boolean read FNotHeld write FNotHeld;
    property SecurityID: string read FSecurityID write FSecurityID;
	  property HedgeType: TIABHedgeType read FHedgeType write FHedgeType;
	  property HedgeParam: string read FHedgeParam write FHedgeParam;
    property ExemptCode: Integer read FExemptCode write FExemptCode;
    property OptOutSmartRouting: Boolean read FOptOutSmartRouting write FOptOutSmartRouting;
    property AlgoStrategy: string read FAlgoStrategy write FAlgoStrategy;
    property AlgoParams[Index: Integer]: TIABTagValue read GetAlgoParams write SetAlgoParams;
    property AlgoParamsCount: Integer read GetAlgoParamsCount;
    property SmartComboRoutingParams[Index: Integer]: TIABTagValue read GetSmartComboRoutingParams write SetSmartComboRoutingParams;
    property SmartComboRoutingParamsCount: Integer read GetSmartComboRoutingParamsCount;
    property OrderMiscOptions[Index: Integer]: TIABTagValue read GetOrderMiscOptions write SetOrderMiscOptions;
    property OrderMiscOptionsCount: Integer read GetOrderMiscOptionsCount;
    property MktDataOptions[Index: Integer]: TIABTagValue read GetMktDataOptions write SetMktDataOptions;
    property MktDataOptionsCount: Integer read GetMktDataOptionsCount;
    property ComboLegPrice[Index: Integer]: Double read GetComboLegPrice write SetComboLegPrice;
    property ComboLegPriceCount: Integer read GetComboLegPriceCount;
    property SecurityIDType: string read FSecurityIDType write FSecurityIDType;
    property ActiveStartTime: string read FActiveStartTime write FActiveStartTime;
    property ActiveStopTime: string read FActiveStopTime write FActiveStopTime;
    property ScaleTable: string read FScaleTable write FScaleTable;
    property AlgoId: string read FAlgoId write FAlgoId;
    property TradingClass: string read FTradingClass write FTradingClass;
    property Solicited: Boolean read FSolicited write FSolicited;
    property RandomizeSize: Boolean read FRandomizeSize write FRandomizeSize;
    property RandomizePrice: Boolean read FRandomizePrice write FRandomizePrice;
    property ModelCode: string read FModelCode write FModelCode;
    property ExtOperator: string read FExtOperator write FExtOperator;
    property ReferenceContractId: Integer read FReferenceContractId write FReferenceContractId;
	  property PeggedChangeAmount: Double read FPeggedChangeAmount write FPeggedChangeAmount;
	  property IsPeggedChangeAmountDecrease: Boolean read FIsPeggedChangeAmountDecrease write FIsPeggedChangeAmountDecrease;
	  property ReferenceChangeAmount: Double read FReferenceChangeAmount write FReferenceChangeAmount;
	  property ReferenceExchangeId: string read FReferenceExchangeId write FReferenceExchangeId;
	  property AdjustedOrderType: TIABOrderType read FAdjustedOrderType write FAdjustedOrderType;
	  property TriggerPrice: Double read FTriggerPrice write FTriggerPrice;
	  property AdjustedStopPrice: Double read FAdjustedStopPrice write FAdjustedStopPrice;
	  property AdjustedStopLimitPrice: Double read FAdjustedStopLimitPrice write FAdjustedStopLimitPrice;
	  property AdjustedTrailingAmount: Double read FAdjustedTrailingAmount write FAdjustedTrailingAmount;
	  property AdjustableTrailingUnit: Integer read FAdjustableTrailingUnit write FAdjustableTrailingUnit;
	  property LmtPriceOffset: Double read FLmtPriceOffset write FLmtPriceOffset;
    property SoftDollarTier: TIABSoftDollarTier read FSoftDollarTier write FSoftDollarTier;
    property CashQuantity: Double read FCashQuantity write FCashQuantity;

    property Mifid2DecisionMaker: string read FMifid2DecisionMaker write FMifid2DecisionMaker;
    property Mifid2DecisionAlgo: string read FMifid2DecisionAlgo write FMifid2DecisionAlgo;
    property Mifid2ExecutionTrader: string read FMifid2ExecutionTrader write FMifid2ExecutionTrader;
    property Mifid2ExecutionAlgo: string read FMifid2ExecutionAlgo write FMifid2ExecutionAlgo;
    property DontUseAutoPriceForHedge: Boolean read FDontUseAutoPriceForHedge write FDontUseAutoPriceForHedge;
    property IsOmsContainer: Boolean read FIsOmsContainer write FIsOmsContainer;
    property DiscretionaryUpToLimitPrice: Boolean read FDiscretionaryUpToLimitPrice write FDiscretionaryUpToLimitPrice;
    property AutoCancelDate: string read FAutoCancelDate write FAutoCancelDate;
    property FilledQuantity: BigDecimal read FFilledQuantity write FFilledQuantity;
    property RefFuturesConId: Integer read FRefFuturesConId write FRefFuturesConId;
    property AutoCancelParent: Boolean read FAutoCancelParent write FAutoCancelParent;
    property Shareholder: string read FShareholder write FShareholder;
    property ImbalanceOnly: Boolean read FImbalanceOnly write FImbalanceOnly;
    property RouteMarketableToBbo: Boolean read FRouteMarketableToBbo write FRouteMarketableToBbo;
    property ParentPermId: Int64 read FParentPermId write FParentPermId;
    property UsePriceMgmtAlgo: Integer read FUsePriceMgmtAlgo write FUsePriceMgmtAlgo;
    property MarketCapPrice: Double read FMarketCapPrice write FMarketCapPrice;
    property CompletedTime: string read FCompletedTime write FCompletedTime;
    property CompletedStatus: string read FCompletedStatus write FCompletedStatus;
    property Duration: Integer read FDuration write FDuration;
  	property PostToAts: Integer read FPostToAts write FPostToAts;


  	property AdvancedErrorOverride: string read FAdvancedErrorOverride write FAdvancedErrorOverride;

  	property ManualOrderTime: TDateTime read FManualOrderTime write FManualOrderTime;
  	property MinTradeQty: Integer read FMinTradeQty write FMinTradeQty;
  	property MinCompeteSize: Integer read FMinCompeteSize write FMinCompeteSize;
  	property CompeteAgainstBestOffset: Double read FCompeteAgainstBestOffset write FCompeteAgainstBestOffset;
  	property MidOffsetAtWhole: Double read FMidOffsetAtWhole write FMidOffsetAtWhole;
  	property MidOffsetAtHalf: Double read FMidOffsetAtHalf write FMidOffsetAtHalf;

    property OnFill: TNotifyEvent read FOnFill write FOnFill;
    property OnCompleted: TNotifyEvent read FOnCompleted write FOnCompleted;
  published
    property Account: string read FAccount write FAccount;
    property Action: TIABAction read FAction write FAction;
    property AuxPrice: Double read FAuxPrice write FAuxPrice;
    property Currency: string read FCurrency write FCurrency;
    property Exchange: string read FExchange write FExchange;
    property Expiry: string read FExpiry write SetExpiry;
    property LocalSymbol: string read FLocalSymbol write FLocalSymbol;
    property OCAGroup: string read FOCAgroup write FOCAgroup;
    property OpenClose: Char read FOpenClose write FOpenClose default 'O';
    property OrderRef: string read FOrderRef write FOrderRef;
    property OrderOrigin: TIABOrderOrigin read FOrderOrigin write FOrderOrigin;
    property OrderType: TIABOrderType read FOrderType write FOrderType;
    property Quantity: BigDecimal read FQuantity write FQuantity;
    property Price: Double read FPrice write FPrice;
    property Right: TIABRight read FRight write FRight;
    property SecurityType: TIABSecurityType read FSecurityType write FSecurityType;
    property Strike: Double read FStrike write FStrike;
    property Symbol: string read FSymbol write FSymbol;
    property TimeInForce: TIABTimeInForce read FTimeInForce write FTimeInForce;
    property Transmit: Boolean read FTransmit write FTransmit;
    property ParentId: Integer read FParentId write FParentId;
    property BlockOrder: Boolean read FBlockOrder write FBlockOrder;
    property SweepToFill: Boolean read FSweepToFill write FSweepToFill;
    property DisplaySize: Integer read FDisplaySize write FDisplaySize;
    property TriggerMethod: Integer read FTriggerMethod write FTriggerMethod;
    property ExtendedHours: Boolean read FExtendedHours write FExtendedHours;
    property Hidden: Boolean read FHidden write FHidden;
    property PrimaryExchange: string read FPrimaryExchange write FPrimaryExchange;
  end;

  TIABOrders = class(TPersistent)
  private
    FList: TList;
    FServerVersion: Integer;
    function GetCount: Integer;
    function GetItems(Index: Integer): TIABOrder;
    procedure SetItems(Index: Integer; const Value: TIABOrder);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(TempId, ClientId, PermId: Integer; IABOrder: TIABOrder): Integer;
    procedure Insert(Index, TempId, ClientId, PermId: Integer; IABOrder: TIABOrder);
    procedure Delete(Index: Integer);
    function FindDuplicatePermIds(var Index1, Index2: Integer): Boolean;
    function GetOrder(TempId, ClientId: Integer): TIABOrder;
    function IndexOfTempId(TempId, ClientId: Integer): Integer;
    function IndexOfPermId(PermId: Integer): Integer;
    function TempToPermId(TempId, ClientId: Integer): Integer;
    procedure SetPermId(TempId, ClientId, PermId: Integer);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TIABOrder read GetItems write SetItems; default;
  end;

  TIABAccountTime = procedure(Sender: TObject; TimeStamp: string) of object;
  TIABAccountValue = procedure(Sender: TObject; Index: Integer) of object;
  TIABConnectionState = procedure(Sender: TObject; State: TIABConnection) of object;
  TIABError = procedure(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string) of object;
  TIABOpenOrder = procedure(Sender: TObject; Order: TIABOrder) of object;
  TIABOrderStatus = procedure(Sender: TObject; Order: TIABOrder; Status: TIABOrderState) of object;
  TIABPortfolioUpdate = procedure(Sender: TObject; Index: Integer) of object;
  TIABTickPrice = procedure(Sender: TObject; DataId: Integer; TickType: TIABTickType; Price: Double; TickAttrib: TIABTickAttrib) of object;
  TIABTickSize = procedure(Sender: TObject; DataId: Integer; TickType: TIABTickType; Size: BigDecimal) of object;
  TIABTickPriceAndSize = procedure(Sender: TObject; DataId: Integer; TickType: TIABTickType; Price: Double; Size: BigDecimal) of object;
  TIABTickGeneric = procedure(Sender: TObject; DataId: Integer; TickType: TIABTickType; Value: Double) of object;
  TIABTickString = procedure(Sender: TObject; DataId: Integer; TickType: TIABTickType; Value: string) of object;
  TIABTickEFP = procedure(Sender: TObject; DataId: Integer; TickType: TIABTickType;
                          BasisPoints, TotalDividends, DividendImpact, DividendsToExpiry: Double;
                          HoldDays: Integer; FutureExpiry, BasisPointsString: string) of object;
  TIABInstrumentSpecDetails = procedure(Sender: TObject; Index: Integer) of object;
  TIABBondSpecDetails = procedure(Sender: TObject; Index: Integer) of object;
  TIABExecutionEvent = procedure(Sender: TObject; Order: TIABOrder) of object;
  TIABMarketDepthEvent = procedure(Sender: TObject; DataId, Index, Operation, Side: Integer; Size: BigDecimal; Price: Double) of object;
  TIABMarketLevel2Event = procedure(Sender: TObject; DataId, Index, Operation, Side: Integer; Size: BigDecimal; Price: Double; MMId: string; SmartDepth: Boolean) of object;
  TIABNewsBulletin = procedure(Sender: TObject; MsgID: Integer; Bulletin, NewsSource: string) of object;
  TIABExchangeStatusEvent = procedure(Sender: TObject; MsgID: Integer; Status: TIABExchangeStatus; Bulletin, NewsSource: string) of object;
  TIABManagedAccountsEvent = procedure(Sender: TObject; Details: string) of object;
  TIABReceiveFADetailEvent = procedure(Sender: TObject; FADataType: TIABFADataType; XmlDetails: string) of object;
  TIABHistoricalDataEvent = procedure(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData) of object;
  TIABHistoricalDataUpdateEvent = procedure(Sender: TObject; DataId: Integer; HistoricalChartDataElement: TIABHistoricalChartData) of object;
  TIABScannerParamEvent = procedure(Sender: TObject; Parameters: string) of object;
  TIABScannerDataEvent = procedure(Sender: TObject; Scan: TIABScan) of object;
  // superceeded 9.64 TIABTickOptionComputation = procedure(Sender: TObject; DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, ModelPrice, pvDividend: Double) of object;
  TIABTickOptionComputation = procedure(Sender: TObject; DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice: Double) of object;
  TIABCurrentTimeEvent = procedure(Sender: TObject; DateTime: TDateTime) of object;
  TIABRealTimeDataEvent = procedure(Sender: TObject; DataId: Integer; RealTimeDataElement: TIABRealTimeData) of object;
  TIABVerifiedOrder = procedure(Sender: TObject; Order: TIABOrder) of object;
  TIABFundamentalDataEvent = procedure(Sender: TObject; DataID: Integer; xmlStrData: string) of object;
  TIABInstrumentSpecDetailsReady = procedure(Sender: TObject; DataID: Integer) of object;
  TIABAccountDetailsReady = procedure(Sender: TObject; AccountName: string) of object;
  TIABExecutionDetailsReady = procedure(Sender: TObject; RequestID: Integer) of object;
  TIABSnapShotDataEnd = procedure(Sender: TObject; DataID: Integer) of object;
  TIABDeltaNeutralValidation = procedure(Sender: TObject; DataID: Integer; DeltaNeutralContract: TIABDeltaNeutralContract) of object;
  TIABMarketDataTypeEvent = procedure(Sender: TObject; DataID: Integer; MarketDataType: TIABMarketDataType) of object;
  TIABCommissionReportEvent = procedure(Sender: TObject; CommissionReport: PTIABCommissionReport) of object;
  TIABSecurityDefinitionOptionalParameterEvent = procedure(Sender: TObject; DataId: Integer; Exchange:string; UnderlyingConId: Integer; tradingClass, multiplier, expirations, strikes: string) of object;
  TIABSecurityDefinitionOptionalParameterEventEnd = procedure(Sender: TObject; DataID: Integer) of object;
  TIABSoftDollarTiersEvent = procedure(Sender: TObject; DataID, Item, Count: Integer; Name, Value, DisplayName: string) of object;
  TIABSymbolSampleEvent = procedure(Sender: TObject; DataID, Item, Count: Integer; SymbolDerivative: TIABSymbolDerivativeSpecItem) of object;
  TIABDepthMarketDataDescripItemEvent = procedure(Sender: TObject; Item, Count: Integer; DepthMarketDataDescrip: TIABDepthMarketDataDescripItem) of object;
  TIABNewsTickEvent = procedure(Sender: TObject; TickerID: Integer; TimeStamp: TDatetime; ProviderCode, ArticleId, Headline, ExtraData: string) of object;
  TIABNewsProviderEvent = procedure(Sender: TObject; Item, Count: Integer; ProviderCode, ProviderName: string) of object;
  TIABNewsArticleEvent = procedure(Sender: TObject; DataId, ArticleType: Integer; ArticleText: string) of object;
  TIABHistoricalNewsEvent = procedure(Sender: TObject; DataId: Integer; Time, ProviderCode, ArticleId, Headline: string) of object;
  TIABHistoricalNewsEndEvent = procedure(Sender: TObject; DataId: Integer; HasMore: Boolean) of object;
  TIABMarketRuleEvent = procedure(Sender: TObject; RuleId, Item, Count: Integer; LowEdge, Increament: Double) of object;
  TIABSmartComponentEvent = procedure(Sender: TObject; DataId, Item, Count, BitNumber: Integer; Exchange, ExchangeLetter: string) of object;
  TIABTickReqParamsEvent = procedure(Sender: TObject; TickerId: Integer; MinTick: Double; bboExchange: string; SnapshotPermissions: Integer) of object;
  TIABHeadTimestampEvent = procedure(Sender: TObject; DataId: Integer; HeadTimestamp: string) of object;
  TIABHistogramDataEvent = procedure(Sender: TObject; DataId, Item, Count: Integer; Price: Double; Size: BigDecimal) of object;
  TIABRerouteMktDataDepthReqEvent = procedure(Sender: TObject; DataId, ConId: Integer; Exchange: string) of object;
  TIABProfitLossEvent = procedure(Sender: TObject; DataId: Integer; DailyPnL, UnrealizedPnL, RealizedPnL: Double) of object;
  TIABProfitLossSingleEvent = procedure(Sender: TObject; DataId: Integer; Pos: BigDecimal; DailyPnL, UnrealizedPnL, RealizedPnL, Value: Double) of object;
  TIABCompletedOrdersEvent = procedure(Sender: TObject; Order: TIABOrder) of object;
  TIABHistoricalTickDataEvent = procedure(Sender: TObject; DataID: Integer; TickData: TIABTickData) of object;
  TIABTickByTickDataEvent = procedure(Sender: TObject; DataID: Integer; TickData: TIABTickData) of object;
  TIABReplaceFAEndEvent = procedure(Sender: TObject; DataID: Integer; DataStr: string) of object;
  TIABWsHorizonEventsEvent = procedure(Sender: TObject; DataID: Integer; DataStr: string) of object;
  TIABWsHorizonMetaEvent = procedure(Sender: TObject; DataID: Integer; DataStr: string) of object;
  // new
  TIABHistoricalSessionEvent = procedure(Sender: TObject; DataId, Item, Count: Integer; StartDateTime, EndDateTime, TimeZone: string; HistoricalSessionElement: TIABHistoricalSession) of object;
  TIABUserInfo = procedure(Sender: TObject; DataID: Integer; UserInfo: string) of object;

  TIABSocketThread = class(TThread)
  private
    FIABSocket: TIABSocket;
  protected
    procedure Execute; override;
    procedure SyncdRecieve;
    procedure SyncdClosed;
  public
    constructor Create(Creator: TIABSocket);
  end;

  EIABSocket = class(Exception);
  EIABDeprecatedItem = class(Exception);
  
  TIABSocket = class(TComponent)
  private
    FSocket: TIABTCPClient;
    FSocketThread: TIABSocketThread;
    FConnected: Boolean;
    FConnecting: Boolean;
    FClientID: Integer;
    FNextTempID: Integer;
    FOrders: TIABOrders;
    FVerifiedOrders: TIABOrders;
    FReserveIds: Integer;
    FDefaultOrder: TIABOrder;
    FOutStream: TMemoryStream;
    FInStream: TMemoryStream;
    FOnAccountTime: TIABAccountTime;
    FOnAccountValue: TIABAccountValue;
    FOnConnectionState: TIABConnectionState;
    FOnError: TIABError;
    FOnOpenOrder: TIABOpenOrder;
    FOnOrderStatus: TIABOrderStatus;
    FOnPortfolioUpdate: TIABPortfolioUpdate;
    FOnTickPrice: TIABTickPrice;
    FOnTickSize: TIABTickSize;
    FAccountValues: TStringList;
    FPortfolio: TIABPortfolio;
    FInstrumentSpecs: TIABInstrumentSpec;
    FOnInstrumentSpecDetails: TIABInstrumentSpecDetails;
{$IFNDEF LINUX}
    FMutex: THandle;
{$ENDIF}
    FOnEndOfStreamRead: TNotifyEvent;
    FOnExecution: TIABExecutionEvent;
    FOnMarketDepth: TIABMarketDepthEvent;
    FOnMarketLevel2: TIABMarketLevel2Event;
    FRebuildFromTWS: Boolean;
    FOnRebuildFromTWS: TNotifyEvent;
    FOnNewsBulletin: TIABNewsBulletin;
    FOnExchangeStatus: TIABExchangeStatusEvent;
    FOnManagedAccounts: TIABManagedAccountsEvent;
    FOrderState: TIABOrder;
    FExecuteState: TIABOrder;
    FOnTickPriceAndSize: TIABTickPriceAndSize;
    FOnReceiveFADetail: TIABReceiveFADetailEvent;
    FOnHistoricalData: TIABHistoricalDataEvent;
    FOnHistoricalDataUpdate: TIABHistoricalDataUpdateEvent;
    FConnectAtServerTime: string;
    FServerVersion: Integer;
    FBondSpecs: TIABBondSpec;
    FOnBondSpecDetails: TIABBondSpecDetails;
    FScanner: TIABScanner;
    FOnScannerParam: TIABScannerParamEvent;
    FOnScannerData: TIABScannerDataEvent;
    FOnTickOptionComputation: TIABTickOptionComputation;
    FOnTickGeneric: TIABTickGeneric;
    FOnTickString: TIABTickString;
    FOnIABTickEFP: TIABTickEFP;
    FOnCurrentTime: TIABCurrentTimeEvent;
    FOnRealTimeData: TIABRealTimeDataEvent;
    FOnVerifiedOrder: TIABVerifiedOrder;
    FOnFundamentalData: TIABFundamentalDataEvent;
    FOnInstrumentSpecDetailsReady: TIABInstrumentSpecDetailsReady;
    FAcceptOpenOrdersWindow: DWORD;
    FOnOpenOrderDetailsReady: TNotifyEvent;
    FOnAccountDetailsReady: TIABAccountDetailsReady;
    FOnExecutionDetailsReady: TIABExecutionDetailsReady;
    FOnSnapShotDataEnd: TIABSnapShotDataEnd;
    FOnDeltaNeutralValidation: TIABDeltaNeutralValidation;
    FOnMarketDataType: TIABMarketDataTypeEvent;
    FOnCommissionReport: TIABCommissionReportEvent;
    FOnSecurityDefinitionOptionalParameter: TIABSecurityDefinitionOptionalParameterEvent;
    FOnSecurityDefinitionOptionalParameterEnd: TIABSecurityDefinitionOptionalParameterEventEnd;
    FOnSoftDollarTiers: TIABSoftDollarTiersEvent;
    FOnDepthMarketDataDescripItem: TIABDepthMarketDataDescripItemEvent;
    FClientv100plus: Boolean;
    FClientMaxVerOverride: Integer;
    FConnectionOptions: string;    //connectionOptions is a field reserved for future extensions.  Was used with '+PACEAPI', but not needed from 10.16 - option in TWS config page.
    FOptionalCapabilities: string;
    FOnSymbolSample: TIABSymbolSampleEvent;
    FOnNewsTick: TIABNewsTickEvent;
    FOnNewsProvider: TIABNewsProviderEvent;
    FOnNewsArticle: TIABNewsArticleEvent;
    FOnHistoricalNews: TIABHistoricalNewsEvent;
    FOnHistoricalNewsEnd: TIABHistoricalNewsEndEvent;
    FOnMarketRule: TIABMarketRuleEvent;
    FOnSmartComponent: TIABSmartComponentEvent;
    FOnTickReqParams: TIABTickReqParamsEvent;
    FOnHeadTimestamp: TIABHeadTimestampEvent;
    FOnHistogramData: TIABHistogramDataEvent;
    FOnRerouteMktDataReq: TIABRerouteMktDataDepthReqEvent;
    FOnRerouteMktDepthReq: TIABRerouteMktDataDepthReqEvent;
    FOnProfitLoss: TIABProfitLossEvent;
    FOnProfitLossSingle: TIABProfitLossSingleEvent;
    FSmartDepthMarketData: Boolean;
    FOnCompletedOrders: TIABCompletedOrdersEvent;
    FOnCompletedOrdersEnd: TNotifyEvent;
    FOnHistoricalTickData: TIABHistoricalTickDataEvent;
    FOnTickByTickData: TIABTickByTickDataEvent;
    FOnReplaceFAEnd: TIABReplaceFAEndEvent;
    FOnWsHorizonEvents: TIABWsHorizonEventsEvent;
    FOnWsHorizonMeta: TIABWsHorizonMetaEvent;

    FOnHistoricalSession: TIABHistoricalSessionEvent;
    FOnUserInfo: TIABUserInfo;

    function GetPriorVerMaxVerNo: Integer;
    procedure SetConnected(State: Boolean);
    function GetConnected: Boolean;
    function GetSocketPort: Integer;
    procedure SetSocketPort(const Value: Integer);
    function GetSocketHostAddress: string;
    procedure SetSocketHostAddress(const Value: string);
    procedure SetClientID(const Value: Integer);
    procedure SetReserveIds(const Value: Integer);
    procedure AddToOut(Value: string); overload;
    procedure AddToOut(Value: Integer); overload;
    procedure AddToOut(Value: Double); overload;
    {$IFDEF USE_BIGDECIMAL}
    procedure AddToOut(Value: BigDecimal); overload;
    procedure AddToOutNullValueDef(Value: BigDecimal); overload;
    {$ENDIF}
    procedure AddToOut(Value: Boolean); overload;
    procedure AddToOutNullValueDef(Value: Integer); overload;
    procedure AddToOutNullValueDef(Value: Double); overload;
    procedure ReceiveFromSocket;
{$IFNDEF LINUX}
    function GainSocketMutex: Boolean;
{$ENDIF}
    procedure SendToSocket(Id: Integer);
    procedure SocketConnect(Sender: TObject);
    procedure SocketDisconnect(Sender: TObject);
    procedure SocketError(Sender: TObject; SocketError: Integer);
    procedure Initialize;
    function ProcessConnectAck(pa: PAnsiChar): Boolean;
    procedure StartAPI;
    function DeCodeData: Boolean;
    procedure DoErrorEvent(TempId: Integer; Error: TIABCodeMsgPair; Additional: string = '');
    procedure SendReserveId(Value: Integer);
    function AmendOrderFill(TempId, ClientId, PermId: Integer; Filled, Remaining: BigDecimal; Price, LastPrice: Double; os: TIABOrderState; MktCapPrice: Double): TIABOrder;
    function AmendOrderCancel(TempId, ClientId, PermId: Integer): TIABOrder;
    procedure AttachExecToOrder(OrderIndex: Integer; NewExec: TIABExecution);
    procedure DoPlaceOrModifyOrder(Order: TIABOrder; Id: Integer; Verify: Boolean);
    procedure GetMarketDataGeneric(DataId: Integer; Order: TIABOrder; ExMarketDataString: string; SnapShot: Boolean);
    procedure EncodeContract(Contract: TIABContract; SkipIncExp: Boolean);
    procedure EncodeTagValueList(TagValue: TIABTagValueArray);

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure CancelAccountUpdates;
    procedure CancelOrder(TempID: Integer);
    procedure CancelOrderAtTime(TempID: Integer; CancelDateTime: TDateTime);
    procedure CancelMarketData(DataId: Integer);
    procedure CancelMarketDepth(DataId: Integer);
    procedure GetAccountUpdates(AccountCode: string);
    procedure GetCurrentTWSTime;
    function GetExecutions(Filter: TIABExecutionFilter): Integer;
    procedure GetInstrumentSpecs(DataId: Integer; Order: TIABOrder; IncludeExpired: Boolean = false);
    procedure GetMarketData(DataId: Integer; Order: TIABOrder; ExMarketData: TIABExMktDataSet = []); overload;
    procedure GetMarketData(DataId: Integer; Order: TIABOrder; ExMarketDataString: string); overload;
    procedure GetMarketData(DataId: Integer; Symbol, Local, Exchange, Expiry, Currency, PrimaryExchange: string;
                       SecurityType: TIABSecurityType; Right: TIABRight; Strike: Double; ComboLegs: TIABComboLegArray;
                       TradingClass: string = '';
                       ExMarketData: TIABExMktDataSet = []; Multiplier: string = '');  overload;
    procedure GetMarketSnapShot(DataId: Integer; Symbol, Local, Exchange, Expiry, Currency, PrimaryExchange: string;
                       SecurityType: TIABSecurityType; Right: TIABRight; Strike: Double);
    procedure GetMarketDepth(DataId: Integer; Order: TIABOrder; nRows: Integer); overload;
    procedure GetMarketDepth(DataId: Integer; Symbol, Local, Exchange, PrimaryExchange, Expiry, Currency: string; SecurityType: TIABSecurityType;
              Right: TIABRight; Strike: Double; nRows: Integer; Multiplier: string = ''); overload;
    procedure GetMarketDepth(DataId: Integer; Symbol, Local, Exchange, PrimaryExchange, Expiry, Currency, TradingClass, MktDataOptions, Multiplier: string;
              SecurityType: TIABSecurityType; Right: TIABRight; Strike: Double; nRows, ConId: Integer);  overload;

    function ModifyOrder(TempId, ClientId, Quantity: Integer; OrderType: TIABOrderType; Price, AuxPrice: Double): Boolean;
    function PlaceBracketOrder(ParentOrder: TIABOrder; BracketOrders: array of TIABOrder): Integer;
    function PlaceOrder(Order: TIABOrder): Integer;
    function VerifyOrder(Order: TIABOrder): Integer;
    function PlaceVerifyOrderPreChecks(Order: TIABOrder): Integer;
    procedure RebuildFromTWS;
    procedure RequestCompletedOrders(ApiOnly: Boolean);
    procedure GetNewsBulletins(AllMessages: Boolean);
    procedure CancelNewsBulletins;
    procedure SetServerLogLevel(LogLevel: Integer);
    procedure GetOpenOrdersClient;
    procedure GetOpenOrdersAccount;
    procedure BindTWSOrdersToClient(AutoBind: Boolean);
    procedure GetManagedAccts;
    procedure RequestFA(FADataType: TIABFADataType);
    procedure ReplaceFA(DataId: Integer; FADataType: TIABFADataType; XmlValue: string);
    procedure GetHistoricalData(DataId: Integer; Symbol, Local, Exchange, Expiry, Currency, PrimaryExchange, Multiplier: string;
                                       SecurityType: TIABSecurityType; Right: TIABRight; Strike: Double;
                                       DataEndDateTime: string; DataDuration, DurationTimeUnits: Integer; BarSize: TIABChartBarSize;
                                       DataBasis: TIABHistoricalDataType; ExtendedHours, IncludeExpired, KeepUpdated: Boolean; DateFormat: Integer;
                                       ContractId: Integer; TradingClass: string; ChartOptions: TIABTagValueArray);
    procedure GetRealTimeData(DataId: Integer; Symbol, Local, Exchange, Expiry, Currency, PrimaryExchange, Multiplier: string;
                                       SecurityType: TIABSecurityType; Right: TIABRight; Strike: Double;
                                       BarSize: TIABChartBarSize;
                                       DataBasis: TIABHistoricalDataType; ExtendedHours: Boolean;
                                       ContractId: Integer; TradingClass: string; RealtimeBarOptions: TIABTagValueArray);

    procedure GetHistoricalTicks(DataId: Integer; Contract: TIABContract; StartDateTime, EndDateTime, WhatToShow: string; NumberOfTicks, UseRTH: Integer;
                                      IgnoreSize: Boolean; MiscOptions: TIABTagValueArray);

    procedure GetTickByTickData(DataId: Integer; Contract: TIABContract; TickType: TIABTickDataType; NumberOfTicks: Integer; IgnoreSize: Boolean);
    procedure CancelTickByTickData(DataId: Integer);

    function ExerciseOptions(Order: TIABOrder; OverridePos: Boolean): Integer;
    procedure CancelHistoricalData(DataId: Integer); {Roman} virtual;{add virtual}
    procedure CancelRealTimeData(DataId: Integer);
    procedure GetFundamentalData(DataId: Integer; Order: TIABOrder; ReportType: string);
    procedure CancelFundamentalData(DataId: Integer);
    procedure GetImpliedVolatility(DataID: Integer; Order: TIABOrder; OptionPrice, UnderPrice: Double);
    procedure GetOptionPrice(DataID: Integer; Order: TIABOrder; Volatility, UnderPrice: Double);
    procedure CancelImpliedVolatility(DataId: Integer);
    procedure CancelOptionPrice(DataId: Integer);
    procedure RequestGlobalCancel;
    procedure RequestMarketDataType(DataType: TIABMarketDataType);
    procedure RequestSecDefOptParams(DataId: Integer; UnderlyingSymbol, FutFopExchange, UnderlyingSecType: string; UnderlyingConId: Integer);
    procedure RequestSoftDollarTiers(DataId: Integer);
    procedure GetScannerParameters;
    procedure RequestScan(ScanId: Integer; Criteria: TIABScanCriteria);
    procedure CancelScan(ScanId: Integer);
    procedure RequestMatchingSymbols(DataId: Integer; Pattern:string);
    procedure RequestMktDepthExchanges;
    procedure RequestSmartComponents(DataId: Integer; bboExchange: string);
    procedure RequestNewsProviders;
    procedure RequestNewsArticle(DataId: Integer; ProviderCode, ArticleId, NewsArticleOptions: string);
    procedure RequestHistoricalNews(DataId, conId: Integer; ProviderCodes, StartDateTime, EndDateTime: string;
                               TotalResults: Integer; HistoricalNewsOptions: string);
    procedure RequestHeadTimestamp(TickerId: Integer; InstrumentSpec: TIABInstrumentSpecItem; WhatToShow: string;
                           UseRTH, IncludeExpired, FormatDate: Boolean);
    procedure CancelHeadTimestamp(TickerId: Integer);
    procedure RequestHistogramData(DataId: Integer; InstrumentSpec: TIABInstrumentSpecItem; UseRTH, IncludeExpired: Boolean; TimePeriod: string);
    procedure CancelHistogramData(DataId: Integer);
    procedure RequestMarketRule(MarketRuleId: Integer);
    procedure RequestPnL(DataId: Integer; Account, ModelCode: string);
    procedure CancelPnL(DataId: Integer);
    procedure RequestPnLSingle(DataId: Integer; Account, ModelCode: string; ConId: Integer);
    procedure CancelPnLSingle(DataId: Integer);
    //procedure RequestWSHorizonEventsData(DataID: Integer; ConID: Integer);  // updated to this below in 10.16
    procedure RequestWSHorizonEventsData(DataID: Integer; WSHRequestSpecs: TIABWSHorizonEventData);
    procedure RequestWSHorizonMetaData(DataID: Integer);
    procedure RequestUserInfo(DataId: Integer);

    property Orders: TIABOrders read FOrders write FOrders;
    property VerifiedOrders: TIABOrders read FVerifiedOrders write FVerifiedOrders;
    property AccountValues: TStringList read FAccountValues;
    property InstrumentSpecs: TIABInstrumentSpec read FInstrumentSpecs write FInstrumentSpecs;
    property Portfolio: TIABPortfolio read FPortfolio write FPortfolio;
    property ConnectAtServerTime: string read FConnectAtServerTime;
    property ServerVersion: Integer read FServerVersion;
    property BondSpecs: TIABBondSpec read FBondSpecs write FBondSpecs;
    property Scanner: TIABScanner read FScanner write FScanner;
    property v100plusAPICalls: Boolean read FClientv100plus write FClientv100plus default true;
    property ClientMaxVerOverride: Integer read FClientMaxVerOverride write FClientMaxVerOverride;
    property SmartDepthMarketData: Boolean read FSmartDepthMarketData write FSmartDepthMarketData;
  published
    property Connected: Boolean read GetConnected write SetConnected;
    property ClientID: Integer read FClientID write SetClientID;
    property DefOrder: TIABOrder read FDefaultOrder write FDefaultOrder;
    property ReserveIDs: Integer read FReserveIds write SetReserveIds;
    property TWSHostAddress: string read GetSocketHostAddress write SetSocketHostAddress;
    property TWSPort: Integer read GetSocketPort write SetSocketPort default 7496;
    property OnAccountTime: TIABAccountTime read FOnAccountTime write FOnAccountTime;
    property OnAccountValue: TIABAccountValue read FOnAccountValue write FOnAccountValue;
    property OnConnectionState: TIABConnectionState read FOnConnectionState write FOnConnectionState;
    property OnError: TIABError read FOnError write FOnError;
    property OnInstrumentSpecDetails: TIABInstrumentSpecDetails read FOnInstrumentSpecDetails write FOnInstrumentSpecDetails;
    property OnOpenOrder: TIABOpenOrder read FOnOpenOrder write FOnOpenOrder;
    property OnOrderStatus: TIABOrderStatus read FOnOrderStatus write FOnOrderStatus;
    property OnPortfolioUpdate: TIABPortfolioUpdate read FOnPortfolioUpdate write FOnPortfolioUpdate;
    property OnTickPrice: TIABTickPrice read FOnTickPrice write FOnTickPrice;
    property OnTickSize: TIABTickSize read FOnTickSize write FOnTickSize;
    property OnExecution: TIABExecutionEvent read FOnExecution write FOnExecution;
    property OnMarketDepth: TIABMarketDepthEvent read FOnMarketDepth write FOnMarketDepth;
    property OnMarketLevel2: TIABMarketLevel2Event read FOnMarketLevel2 write FOnMarketLevel2;
    property OnRebuildFromTWS: TNotifyEvent read FOnRebuildFromTWS write FOnRebuildFromTWS;
    property OnNewsBulletin: TIABNewsBulletin read FOnNewsBulletin write FOnNewsBulletin;
    property OnExchangeStatus: TIABExchangeStatusEvent read FOnExchangeStatus write FOnExchangeStatus;
    property OnManagedAccounts: TIABManagedAccountsEvent read FOnManagedAccounts write FOnManagedAccounts;
    property OnTickPriceAndSize: TIABTickPriceAndSize read FOnTickPriceAndSize write FOnTickPriceAndSize;
    property OnReceiveFADetail: TIABReceiveFADetailEvent read FOnReceiveFADetail write FOnReceiveFADetail;
    property OnHistoricalData: TIABHistoricalDataEvent read FOnHistoricalData write FOnHistoricalData;
    property OnHistoricalDataUpdate: TIABHistoricalDataUpdateEvent read FOnHistoricalDataUpdate write FOnHistoricalDataUpdate;
    property OnBondSpecDetails: TIABBondSpecDetails read FOnBondSpecDetails write FOnBondSpecDetails;
    property OnScannerParam: TIABScannerParamEvent read FOnScannerParam write FOnScannerParam;
    property OnScannerData: TIABScannerDataEvent read FOnScannerData write FOnScannerData;
    property OnTickOptionComputation: TIABTickOptionComputation read FOnTickOptionComputation write FOnTickOptionComputation;
    property OnTickGeneric: TIABTickGeneric read FOnTickGeneric write FOnTickGeneric;
    property OnTickString: TIABTickString read FOnTickString write FOnTickString;
    property OnTickEFP: TIABTickEFP read FOnIABTickEFP write FOnIABTickEFP;
    property OnCurrentTime: TIABCurrentTimeEvent read FOnCurrentTime write FOnCurrentTime;
    property OnRealTimeData: TIABRealTimeDataEvent read FOnRealTimeData write FOnRealTimeData;
    property OnVerifiedOrder: TIABVerifiedOrder read FOnVerifiedOrder write FOnVerifiedOrder;
    property OnFundamentalData: TIABFundamentalDataEvent read FOnFundamentalData write FOnFundamentalData;
    property OnOpenOrderDetailsReady: TNotifyEvent read FOnOpenOrderDetailsReady write FOnOpenOrderDetailsReady;
    property OnAccountDetailsReady: TIABAccountDetailsReady read FOnAccountDetailsReady write FOnAccountDetailsReady;
    property OnExecutionDetailsReady: TIABExecutionDetailsReady read FOnExecutionDetailsReady write FOnExecutionDetailsReady;
    property OnSnapShotDataEnd: TIABSnapShotDataEnd read FOnSnapShotDataEnd write FOnSnapShotDataEnd;
    property OnDeltaNeutralValidation: TIABDeltaNeutralValidation read FOnDeltaNeutralValidation write FOnDeltaNeutralValidation;
    property OnMarketDataType: TIABMarketDataTypeEvent read FOnMarketDataType write FOnMarketDataType;
    property OnCommissionReport: TIABCommissionReportEvent read FOnCommissionReport write FOnCommissionReport;
    property OnInstrumentSpecDetailsReady: TIABInstrumentSpecDetailsReady read FOnInstrumentSpecDetailsReady write FOnInstrumentSpecDetailsReady;
    property OnSecurityDefinitionOptionalParameter: TIABSecurityDefinitionOptionalParameterEvent read FOnSecurityDefinitionOptionalParameter write FOnSecurityDefinitionOptionalParameter;
    property OnSecurityDefinitionOptionalParameterEnd: TIABSecurityDefinitionOptionalParameterEventEnd read FOnSecurityDefinitionOptionalParameterEnd write FOnSecurityDefinitionOptionalParameterEnd;
    property OnSoftDollarTiers: TIABSoftDollarTiersEvent read FOnSoftDollarTiers write FOnSoftDollarTiers;
    property OnSymbolSample: TIABSymbolSampleEvent read FOnSymbolSample write FOnSymbolSample;
    property OnDepthMarketDataDescripItem: TIABDepthMarketDataDescripItemEvent read FOnDepthMarketDataDescripItem write FOnDepthMarketDataDescripItem;
    property OnNewsTick: TIABNewsTickEvent read FOnNewsTick write FOnNewsTick;
    property OnNewsProvider: TIABNewsProviderEvent read FOnNewsProvider write FOnNewsProvider;
    property OnNewsArticle: TIABNewsArticleEvent read FOnNewsArticle write FOnNewsArticle;
    property OnHistoricalNews: TIABHistoricalNewsEvent read FOnHistoricalNews write FOnHistoricalNews;
    property OnHistoricalNewsEnd: TIABHistoricalNewsEndEvent read FOnHistoricalNewsEnd write FOnHistoricalNewsEnd;
    property OnMarketRule: TIABMarketRuleEvent read FOnMarketRule write FOnMarketRule;
    property OnSmartComponent: TIABSmartComponentEvent read FOnSmartComponent write FOnSmartComponent;
    property OnTickReqParams: TIABTickReqParamsEvent read FOnTickReqParams write FOnTickReqParams;
    property OnHeadTimestamp: TIABHeadTimestampEvent read FOnHeadTimestamp write FOnHeadTimestamp;
    property OnHistogramData: TIABHistogramDataEvent read FOnHistogramData write FOnHistogramData;
    property OnRerouteMktDataReq: TIABRerouteMktDataDepthReqEvent read FOnRerouteMktDataReq write FOnRerouteMktDataReq;
    property OnRerouteMktDepthReq: TIABRerouteMktDataDepthReqEvent read FOnRerouteMktDepthReq write FOnRerouteMktDepthReq;
    property OnProfitLoss: TIABProfitLossEvent read FOnProfitLoss write FOnProfitLoss;
    property OnProfitLossSingle: TIABProfitLossSingleEvent read FOnProfitLossSingle write FOnProfitLossSingle;
    property OnCompletedOrders: TIABCompletedOrdersEvent read FOnCompletedOrders write FOnCompletedOrders;
    property OnCompletedOrdersEnd: TNotifyEvent read FOnCompletedOrdersEnd write FOnCompletedOrdersEnd;
    property OnHistoricalTickData: TIABHistoricalTickDataEvent read FOnHistoricalTickData write FOnHistoricalTickData;
    property OnTickByTickData: TIABTickByTickDataEvent read FOnTickByTickData write FOnTickByTickData;
    property OnReplaceFAEnd: TIABReplaceFAEndEvent read FOnReplaceFAEnd write FOnReplaceFAEnd;
    property OnWsHorizonEvents: TIABWsHorizonEventsEvent read FOnWsHorizonEvents write FOnWsHorizonEvents;
    property OnWsHorizonMeta: TIABWsHorizonMetaEvent read FOnWsHorizonMeta write FOnWsHorizonMeta;
    // new
    property OnHistoricalSession: TIABHistoricalSessionEvent read FOnHistoricalSession write FOnHistoricalSession;
    property OnUserInfo: TIABUserInfo read FOnUserInfo write FOnUserInfo;

    property OnEndOfStreamRead: TNotifyEvent read FOnEndOfStreamRead write FOnEndOfStreamRead;
  end;



  procedure Register;

  function OrderTypeToStr(IABOrderType: TIABOrderType): string;
  function IABDateTimeStrToDateTime(const Value: string): TDateTime;
  function DateTimeToIABDateTimeStr(Value: TDateTime; Timezone: string = ''): string;
  procedure FillContractFromOrder(Order: TIABOrder; var contract: TIABContract);
  procedure InitIABContract(var contract: TIABContract);

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TIABSocket]);
end;

var
  NewOrderState, OrderStatus: TIABOrder;
  PFUpdate: TIABPortfolioItem;
  InsSpecItem: TIABInstrumentSpecItem;
  SymbolDerivItem: TIABSymbolDerivativeSpecItem;
  DepthMarketDataDescripItem: TIABDepthMarketDataDescripItem;
  BondSpecItem: TIABBondSpecItem;
  NewExec: TIABExecution;
  HistoricalChartData: TIABHistoricalChartData;
  HistoricalSession: TIABHistoricalSession;
  IABScan: TIABScan;
  RealTimeData: TIABRealTimeData;
  S_Ver: Integer = 0;
  DeltaNeutralContract: TIABDeltaNeutralContract;
  CommissionReport: TIABCommissionReport;
  Comboleg: TIABComboLeg;
  TagValue: TIABTagValue;
  ASoftDollarTier: TIABSoftDollarTier;
  TickAttrib: TIABTickAttrib;
  TickData: TIABTickData;


function OrderTypeToStr(IABOrderType: TIABOrderType): string;
begin
  Result := OrderTypeString[IABOrderType];
end;

function IABDateTimeStrToDateTime(const Value: string): TDateTime;
var y, m, d, h, n, c: word; s: string;
begin
  s := Value;
  y := StrToInt(Copy(s,1,4));
  Delete(s,1,4);
  m := StrToInt(Copy(s,1,2));
  Delete(s,1,2);
  d := StrToInt(Copy(s,1,2));
  Delete(s,1,2);
  s := Trim(s);
  h := StrToInt(Copy(s,1,2));
  Delete(s,1,3);
  n := StrToInt(Copy(s,1,2));
  Delete(s,1,3);
  c := StrToInt(Copy(s,1,2));
  Result := EncodeDate(y,m,d) + EncodeTime(h,n,c,0);
end;

function DateTimeToIABDateTimeStr(Value: TDateTime; Timezone: string = ''): string;
var SystemTime: TSystemTime; SystemDateTime: TDateTime;
begin
  // Assumed value is in local time or exchange local time.  Convert to UTC from OS time zone.

  if TimeZone = '' then
    begin
      GetSystemTime(SystemTime);
      SystemDateTime := SystemTimeToDateTime(SystemTime);
      Value := Value - (Now - SystemDateTime);
      TimeZone := 'UTC';
    end;
  DateTimeToString(Result,'yyyymmdd" "hh":"nn":"ss', Value);
  Result := Result + ' ' + Timezone;

  {
  The correct format is yyyymmdd hh:mm:ss xxx where yyyymmdd and xxx are optional. E.g.: 20031126 15:59:00 US/Eastern
  Note that there is a space between the date and time, and between the time and time-zone.
  If no date is specified, current date is assumed. If no time-zone is specified, local time-zone is assumed(deprecated).
  }
end;

function ActionStringType(Action: string): TIABAction;
var a: TIABAction;
begin
  Result := iabIdle;
  for a := iabIdle to High(TIABAction) do
    if Action = ActionString[a] then
      Result := a;
end;

function HedgeTypeStrType(Hedge: string):TIABHedgeType;
var h: TIABHedgeType;
begin
  Result := htUnset;
  for h := htUnset to High(TIABHedgeType) do
    if Hedge = TIABHedgeTypeStr[h] then
      Result := h;
end;

procedure AddInstrumentSpecItemTagValue(TagValueArray: TIABTagValueArray; TagValue: TIABTagValue);
var i: Integer;
begin
  i := Length(TagValueArray);
  SetLength(TagValueArray, i + 1);
  TagValueArray[ i ] := TagValue;
end;

procedure FillContractFromOrder(Order: TIABOrder; var Contract: TIABContract);
begin
  FillChar(Contract, SizeOf(TIABContract), 0);
  Contract.ContractId := Order.ContractId;
  Contract.Symbol := Order.Symbol;
  Contract.SecurityType := Order.SecurityType;
  Contract.LastTradeDateOrContractMonth := Order.Expiry;
  Contract.Strike := Order.Strike;
  Contract.Right := Order.Right;
  Contract.Multiplier := Order.Multiplier;
  Contract.Exchange := Order.Exchange;
  Contract.PrimaryExchange := Order.PrimaryExchange;
  Contract.Currency := Order.Currency;
  Contract.LocalSymbol := Order.LocalSymbol;
  Contract.TradingClass := Order.TradingClass;
  Contract.IncludeExpired := true;
  Contract.SecIdType := '';  //Order.SecIdType;
  Contract.SecId := ''; //Order.SecId;
  Contract.ComboLegsDescrip := Order.ComboLegsDescrip;
//  Contract.ComboLegList := Copy(Order.ComboLegList);
//  Contract.DeltaNeutralContract := Order.DeltaNeutralContract;
end;

procedure InitIABContract(var Contract: TIABContract);
begin
  Contract.ContractId := 0;
  Contract.Strike := 0.0;
  Contract.IncludeExpired := false;
  Contract.ComboLegList := nil;
  Contract.DeltaNeutralContract.ConId := 0;
  Contract.DeltaNeutralContract.Delta := 0.0;
  Contract.DeltaNeutralContract.Price := 0.0;
end;


function DecodeOrderType(ot: string): TIABOrderType;
var i: Integer;
begin
  Result := otUnknown;
  if (ot = 'STP LMT') or (ot = 'STP_LMT') then
    ot := 'STPLMT';
  for i := Ord(Low(TIABOrderType)) to Ord(High(TIABOrderType)) do
    if OrderTypeString[TIABOrderType(i)] = ot then
      begin
        Result := TIABOrderType(i);
        Break;
      end;
end;

function DecodeTIF(tif: string): TIABTimeInForce;
var i: Integer;
begin
  Result := tifUnknown;
  for i := Ord(Low(TIABTimeInForce)) to Ord(High(TIABTimeInForce)) do
    if TimeInForceString[TIABTimeInForce(i)] = tif then
      begin
        Result := TIABTimeInForce(i);
        Break;
      end;
end;

function DecodeRule80A(r80a: string): TIABRule80A;
var i: Integer;
begin
  Result := r80na;
  for i := Ord(Low(TIABRule80A)) to Ord(High(TIABRule80A)) do
    if CompareText(TIABRule80AStr[TIABRule80A(i)],r80A) = 0 then
      begin
        Result := TIABRule80A(i);
        Break;
      end;
end;

function DecodeSecurityType(Sectype: string): TIABSecurityType;
var i: Integer;
begin
  Result := stStock;
  for i := Ord(Low(TIABSecurityType)) to Ord(High(TIABSecurityType)) -1 do
    if SecurityTypeString[TIABSecurityType(i)] = Sectype then
      begin
        Result := TIABSecurityType(i);
        Break;
      end;
end;

function DecodeRight(right: string): TIABRight;
begin
  Result := rtNone;
  if ((right = 'C') or (right = 'CALL')) then
    Result := rtCall
  else if ((right = 'P') or (right = 'PUT')) then
    Result := rtPut;
end;



{ TIABSocket }
procedure TIABSocket.AddToOut(Value: string);
var i: Integer; AnsiStr: AnsiString;
begin
  AnsiStr := AnsiString(Value);   //  required for Unicode, otherwise harmless.
  i := Length(AnsiStr);
  if FClientv100plus and (FOutStream.Position = 0) then
    FOutStream.Write(HEADER_ZERO, HEADER_LEN);
  if i > 0 then
    FOutStream.Write(AnsiStr[1],i);
  FOutStream.Write(TWS_EOV,1);
end;

procedure TIABSocket.AddToOut(Value: Integer);
begin
  AddToOut(IntToStr(Value));
end;

procedure TIABSocket.AddToOut(Value: Double);
var s: string; i: Integer;  DecSep: Char;
begin
  {$IF CompilerVersion >= 22.0}   // XE1
  DecSep := FormatSettings.DecimalSeparator;
  {$ELSE}
  DecSep := DecimalSeparator;
  {$IFEND}
  if IsInfinite(Value) then s := INFINITY_STR
  else if Value >= UNSET_DOUBLE_TEST_VALUE then // = UNSET_DOUBLE then //MaxDouble then
    s := '1.7976931348623157E308' // the java expected version of a maxdouble
  else
    s := FormatFloat('0.00########',Value);
  if DecSep <> '.' then
    begin                           // all this because TWS or Java can't read the systems DecimalSeparator !!
      for i := 1 to Length(s) do
        if s[i] = DecSep then s[i] := '.';
    end;
  AddToOut(s);
end;

{$IFDEF USE_BIGDECIMAL}
procedure TIABSocket.AddToOut(Value: BigDecimal);
var s: string;
begin
  s := Value.ToPlainString;
  AddToOut(s);
end;

procedure TIABSocket.AddToOutNullValueDef(Value: BigDecimal);
begin
  if Value.AsInt32 = UNSET_DECIMAL then AddToOut('') else AddToOut(Value);
end;
{$ENDIF}

procedure TIABSocket.AddToOut(Value: Boolean);
begin
  if Value then
    AddToOut('1')
  else
    AddToOut('0');
end;

procedure TIABSocket.AddToOutNullValueDef(Value: Integer);
begin
  if Value = UNSET_INTEGER then AddToOut('')
  else AddToOut(Value);
end;

procedure TIABSocket.AddToOutNullValueDef(Value: Double);
begin
  if Value >= UNSET_DOUBLE_TEST_VALUE then AddToOut('')
  else AddToOut(Value);
end;

function TIABSocket.AmendOrderCancel(TempId, ClientId, PermId: Integer): TIABOrder;
var i: Integer;
begin
  if PermId > 0 then i := FOrders.IndexOfPermId(PermId)
  else i := -1;
  if (i > -1) then Result := FOrders[i] else Result := FOrders.GetOrder(TempId,ClientId);
  if Result <> nil then
    begin
      Result.FCompleted := true;
      if Assigned(Result.FOnCompleted) then Result.FOnCompleted(Result);
    end;
end;

function TIABSocket.AmendOrderFill(TempId, ClientId, PermId: Integer; Filled, Remaining: BigDecimal; Price, LastPrice: Double; os: TIABOrderState; MktCapPrice: Double): TIABOrder;
{$IFDEF USE_BIGDECIMAL}
var dm: BigDecimal;
{$ENDIF}
begin
  if PermId > 0 then Result := FOrders[FOrders.IndexOfPermId(PermId)] else Result := FOrders.GetOrder(TempId,ClientId);
  if Result <> nil then
    begin

      (************  bug fix  ************  added in 9.50.2
      The TWS will sometimes send duplicate Order status messages of Filled... as in the same message twice.
      Note that under some conditions such as a partial fill, that is the correct thing to do.

      Hence this checks for duplicates, and eats the second (or third) instance,
      and therefore this prevents an extra event from being created.  *)


      {$IFDEF USE_BIGDECIMAL}
      if (BigDecimal.Compare(Filled, Result.FFilled) = 0) and (os = osFilled) then
      {$ELSE}
      if (SameValue(Filled, Result.FFilled)) and (os = osFilled) then
      {$ENDIF}
        begin
          Result := nil;
          Exit;
        end;

      (************  bug fix end  ***********)

      // v 10.10  introduced big decimals.  The quantity and filled went from Integers, to Doubles or to BigDecimals
      //   Comparing Bigdecimals is OK, but not doubles.  Hence the addition of samevalue compares.

      {$IFDEF USE_BIGDECIMAL}
      if (not Filled.IsZero) then       // something happened // amend last action running totals
      {$ELSE}
      if (Filled > 0) then       // something happened // amend last action running totals
      {$ENDIF}
        begin

          {$IFDEF USE_BIGDECIMAL}
          Result.FChanged := BigDecimal.Compare(Filled, Result.FFilled) <> 0;   // determine if execution occured - only way!!!
          {$ELSE}
          Result.FChanged := not SameValue(Filled, Result.FFilled);   // determine if execution occured - only way!!!
          {$ENDIF}

          {$IFDEF USE_BIGDECIMAL}
          if Result.FFilled.IsZero then
          {$ELSE}
          if (Result.FFilled = 0) then
          {$ENDIF}
            begin               // first fill - might be last?
              Result.FLatestFillQty := Filled;
              Result.FLatestFillPrice := Price;
            end

          {$IFDEF USE_BIGDECIMAL}
          else if (BigDecimal.Compare(Filled, Result.FFilled) = 1) then   // if Filled > Result.FFilled
          {$ELSE}
          else if (not SameValue(Filled, Result.FFilled)) and (Filled > Result.FFilled) then
          {$ENDIF}

            begin               // partial fill portion - find actual price and quantity
              Result.FLatestFillQty := Filled - Result.FFilled;
              if LastPrice > 0 then Result.FLatestFillPrice := LastPrice  // Lastprice added in API v7.01
              else
                begin                                                       // else calc last price (is approx)

                  {$IFDEF USE_BIGDECIMAL}
                  if (not Result.FLatestFillQty.IsZero) and (not SameValue(Result.FLatestFillPrice, Price)) then
                    begin
                      dm := (Filled * Price - Result.FFilled * Result.FFillPrice) / Result.FLatestFillQty;
                      Result.FLatestFillPrice := dm.AsDouble;
                    end;
                  {$ELSE}
                  if (Result.FLatestFillQty > 0) and (not SameValue(Result.FLatestFillPrice, Price)) then
                    Result.FLatestFillPrice := (Filled * Price - Result.FFilled * Result.FFillPrice) / Result.FLatestFillQty;
                  {$ENDIF}

                end;
            end
        end;
      Result.FFillPrice := Price;  // amend running totals
      Result.FFilled := Filled;
      Result.FRemaining := Remaining;
      {$IFDEF USE_BIGDECIMAL}
      Result.FCompleted := Remaining.IsZero;  // if done
      {$ELSE}
      Result.FCompleted := Remaining = 0;  // if done
      {$ENDIF}
      Result.FMarketCapPrice := MktCapPrice;
      if Result.FChanged and Assigned(Result.FOnFill) then Result.FOnFill(Result);  // use these only if onorderstatus not used
      if Result.FCompleted and Assigned(Result.FOnCompleted) then Result.FOnCompleted(Result);
    end;
end;

procedure TIABSocket.AttachExecToOrder(OrderIndex: Integer; NewExec: TIABExecution);
var Order: TIABOrder;
begin
  if OrderIndex > -1 then
    begin
      Order := FOrders.Items[OrderIndex];
      SetLength(Order.FExecutions,Length(Order.FExecutions) + 1);
      Order.FExecutions[High(Order.FExecutions)] := NewExec;
    end;
end;

procedure TIABSocket.BindTWSOrdersToClient(AutoBind: Boolean);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  FAcceptOpenOrdersWindow := GetTickCount + OPEN_ORDER_ACCEPT_WINDOW;
  AddToOut(REQ_AUTO_OPEN_ORDERS);
  AddToOut(VERSION);
  AddToOut(AutoBind);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.CancelMarketData(DataId: Integer);
const VERSION = 2;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(CANCEL_MKT_DATA);
  AddToOut(VERSION);
  AddToOut(DataId);
  SendToSocket(DataId);
end;

procedure TIABSocket.CancelMarketDepth(DataId: Integer);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(CANCEL_MKT_DEPTH);
  AddToOut(VERSION);
  AddToOut(DataId);
  if FServerVersion >= MIN_SERVER_VER_SMART_DEPTH then  // 146
    AddToOut(FSmartDepthMarketData);
  SendToSocket(DataId);
end;

procedure TIABSocket.CancelAccountUpdates;
const VERSION = 2;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(REQ_ACCT_DATA);
  AddToOut(VERSION);
  AddToOut( 0 );  // 1 = start 0 = stop
  AddToOut(''); //  // FServerVersion 9
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.CancelFundamentalData(DataId: Integer);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if FServerVersion < MIN_SERVER_VER_FUNDAMENTAL_DATA then
    begin
      DoErrorEvent(DataId, UPDATE_TWS, ' Fundamental data not supported.');
      Exit;
    end;
  AddToOut(CANCEL_FUNDAMENTAL_DATA);
  AddToOut(VERSION);
  AddToOut(DataId);
  SendToSocket(DataId);
end;

procedure TIABSocket.CancelHistoricalData(DataId: Integer);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(CANCEL_HISTORICAL_DATA);
  AddToOut(VERSION);
  AddToOut(DataId);
  SendToSocket(DataId);
end;

procedure TIABSocket.CancelImpliedVolatility(DataId: Integer);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if FServerVersion < MIN_SERVER_VER_CANCEL_CALC_IMPLIED_VOLAT then
    begin
      DoErrorEvent(DataId, UPDATE_TWS, ' calculate implied volatility not supported.');
      Exit;
    end;
  AddToOut(CANCEL_CALC_IMPLIED_VOLAT);
  AddToOut(VERSION);
  AddToOut(DataId);
  SendToSocket(DataId);
end;

procedure TIABSocket.CancelOptionPrice(DataId: Integer);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if FServerVersion < MIN_SERVER_VER_CANCEL_CALC_OPTION_PRICE then
    begin
      DoErrorEvent(DataId, UPDATE_TWS, ' calculate option price not supported.');
      Exit;
    end;
  AddToOut(CANCEL_CALC_OPTION_PRICE);
  AddToOut(VERSION);
  AddToOut(DataId);
  SendToSocket(DataId);
end;

procedure TIABSocket.CancelOrder(TempID: Integer);
const VERSION = 1;
begin
  FRebuildFromTWS := false;
  if not FSocket.Active then
    begin
      DoErrorEvent(TempID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(CANCEL_ORDER);
  AddToOut(VERSION);
  AddToOut(TempID);
  if (FServerVersion >= MIN_SERVER_VER_MANUAL_ORDER_TIME) then  // 169
    AddToOut('');
  SendToSocket(TempID);
end;

procedure TIABSocket.CancelOrderAtTime(TempID: Integer; CancelDateTime: TDateTime);
const VERSION = 1;
var s: string;
begin
  FRebuildFromTWS := false;
  if not FSocket.Active then
    begin
      DoErrorEvent(TempID, NOT_CONNECTED);
      Exit;
    end;
  if FServerVersion < MIN_SERVER_VER_MANUAL_ORDER_TIME then  // 169
    begin
      DoErrorEvent(TempID, UPDATE_TWS, ' CancelOrderAtTime, is not supported.');
      Exit;
    end;
  AddToOut(CANCEL_ORDER);
  AddToOut(VERSION);
  AddToOut(TempID);
  if CancelDateTime <= 1.0 then
    CancelDateTime := CancelDateTime + Date;  // add missing date ( today )
  s := FormatDateTime('yyyymmdd hh":"nn":"ss":"', CancelDateTime);  //   "20220314 19:00:00"
  AddToOut(s);
  SendToSocket(TempID);
end;

procedure TIABSocket.CancelRealTimeData(DataId: Integer);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(CANCEL_REAL_TIME_BARS);
  AddToOut(VERSION);
  AddToOut(DataId);
  SendToSocket(DataId);
end;

procedure TIABSocket.CancelScan(ScanID: Integer);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(ScanID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(CANCEL_SCANNER_SUBSCRIPTION);
  AddToOut(VERSION);
  AddToOut(ScanID);
  SendToSocket(ScanID);
end;

procedure TIABSocket.CancelTickByTickData(DataId: Integer);
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if FServerVersion < MIN_SERVER_VER_TICK_BY_TICK then   // 137
    begin
      DoErrorEvent(DataId, UPDATE_TWS, ' cancel tick-by-tick data not supported.');
      Exit;
    end;
  AddToOut(CANCEL_TICK_BY_TICK_DATA);
  AddToOut(DataId);
  SendToSocket(DataId);
end;


constructor TIABSocket.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FClientv100plus := true;
  FSocket := TIABTCPClient.Create(nil);
  FSocket.OnConnect := SocketConnect;
  FSocket.OnDisconnect := SocketDisconnect;
  FSocket.OnError := SocketError;
  FSocket.RemotePort := '7496';
  FSocket.RemoteHost := '127.0.0.1';
  FDefaultOrder := TIABOrder.Create;
  FOutStream := TMemoryStream.Create;
  FOutStream.SetSize(2048); //  this will auto resize upwards if needed.
  FOutStream.Position := 0;
  FInStream := TMemoryStream.Create;
  FInStream.SetSize(8192);  //  this will auto resize upwards if needed.
  FInStream.Position := 0;
  FOrders := TIABOrders.Create;
  FVerifiedOrders := TIABOrders.Create;
  FAccountValues := TStringList.Create;
  FAccountValues.Add('AccountDescription');
  FAccountValues.Add('CashBalance');
  FAccountValues.Add('Currency');
  FAccountValues.Add('DayTradeEquity');
  FAccountValues.Add('DayTradesRemaining');
  FAccountValues.Add('EquityWithLoanValue');
  FAccountValues.Add('FutureOptionValue');
  FAccountValues.Add('FuturesPNL');
  FAccountValues.Add('GrossPositionValue');
  FAccountValues.Add('InitMarginReq');
  FAccountValues.Add('Leverage');
  FAccountValues.Add('LongOptionValue');
  FAccountValues.Add('MaintMarginReq');
  FAccountValues.Add('NetLiquidation');
  FAccountValues.Add('OptionMarketValue');
  FAccountValues.Add('PreviousDayEquityWithLoanValue');
  FAccountValues.Add('RealizedPnL');
  FAccountValues.Add('ShortOptionValue');
  FAccountValues.Add('StockMarketValue');
  FAccountValues.Add('UnalteredInitMarginReq');
  FAccountValues.Add('UnalteredMaintMarginReq');
  FAccountValues.Add('UnrealizedPnL');
  FPortfolio := TIABPortfolio.Create;
  FInstrumentSpecs := TIABInstrumentSpec.Create;
  FOrderState := TIABOrder.Create;
  FExecuteState := TIABOrder.Create;
  FBondSpecs := TIABBondSpec.Create;
  FScanner := TIABScanner.Create(Self);
  if FConnected then SetConnected(true);
end;

function TIABSocket.DeCodeData: Boolean;
var MsgId, Version, p1, p2, p3, p4, p5, p6, i, j: Integer;
    d, d2, d3, d4, d5, d6, d7, d8: Double;
    {$IFDEF USE_BIGDECIMAL}
    dm1, dm2: BigDecimal;
    {$ELSE}
    dm1, dm2: Double;
    {$ENDIF}
    s1, s2, s3, s4, s5: string;
    os: TIABOrderState;
    pa, BOS, EOS: pAnsiChar;
    msgLen, netLen: u_long;
    time64t, pp1: Int64;
    b1: Boolean;
    AttrMask: LongWord;


  {$IFDEF UNICODE}
  function ReadStream:string;
  var i, j: LongWord;
  begin
    i := StrLen(pa);
    SetLength(Result, i);
    j := i;
    while j > 0 do
      begin
        Result[j] := Char((pa + j-1)^);
        dec(j);
      end;
    inc(i);
    inc(pa,i);
  end;
  {$ELSE}
  function ReadStream:string;
  var i: LongWord;
  begin
    i := StrLen(pa);
    SetString(Result,pa,i);
    inc(i);
    inc(pa,i);
  end;
  {$ENDIF}
  function ReadInt:Integer;
  var s: string;
  begin
    s := ReadStream;
    if s = '' then Result := 0 else
      try
        Result := StrToInt(s);
      except on EConvertError do
        raise EIABSocket.Create('Read Integer fail');
      end;
  end;
  function ReadInt64:Int64;
  var s: string;
  begin
    s := ReadStream;
    if s = '' then Result := 0 else
      try
        Result := StrToInt64(s);
      except on EConvertError do
        raise EIABSocket.Create('Read Integer fail');
      end;
  end;
  function ReadFloat:Double;
  var s: string; DecSep: Char;
  begin
    {$IF CompilerVersion >= 22.0}   // XE1
    DecSep := FormatSettings.DecimalSeparator;
    {$ELSE}
    DecSep := DecimalSeparator;
    {$IFEND}
    s := ReadStream;
    if (s = '') or (s = 'NaN') then
      Result := 0.0
    else if s = INFINITY_STR then
      Result := _INFINITY
    else if (Pos('1.7976931348',s) = 1) and (Pos('308', s) = Length(s) - 2) then
      Result := UNSET_DOUBLE  //MaxDouble  //1.7976931348623157081E+308    // 64 bit java has some other ideas re max double
    else if (Pos('4.9406564584',s) = 1) and (Pos('-324', s) = Length(s) - 3) then  // not used normally
      Result := MinDouble //4.9406564584124654418E-324
    else
      try
        if (DecSep <> '.') and (Pos('.',s) > 0) then s[Pos('.',s)] := DecSep;
        Result := StrToFloat(s);
      except on EConvertError do
        raise EIABSocket.Create('Read Float fail');
      end;
  end;
  function ScanAhead(Nulls: Integer): Boolean;
  var q: pAnsiChar; i: Integer;
  begin             // check ahead for enough #0 to complete decode,
    Result := true;
    if Nulls <= 0 then
      Exit;
    q := pa;
    for i := NativeUInt(pa) to NativeUInt(EOS) -1 do
      begin
        if q^ = #0 then dec(Nulls);
        if Nulls <= 0 then Break;
        inc(q);
      end;
    Result := Nulls = 0;
  end;
  function ReadBool: Boolean;
  var s: string;
  begin
    s := ReadStream;
    Result := not ((s = '') or (s = '0'));
  end;
  function ReadFloatMax: Double;
  begin
    Result := ReadFloat;
    if Result = 0.0 then Result := UNSET_DOUBLE;
  end;
  function ReadIntMax: Integer;
  begin
    Result := ReadInt;
    if Result = 0 then Result := UNSET_INTEGER;
  end;

  function ReadDecimal:BigDecimal;
  {$IFDEF USE_BIGDECIMAL}
  var s: string;
  {$ENDIF}
  begin
    {$IFNDEF USE_BIGDECIMAL}
    Result := ReadFloat;
    {$ELSE}
    s := ReadStream;
    if s = '' then
      s := '0';
    try
      Result := s;
    except on EConvertError do
      raise EIABSocket.Create('Read Decimal fail');
    end;
    {$ENDIF}
  end;
  function ReadDecimalMax:BigDecimal;
  begin
    {$IFNDEF USE_BIGDECIMAL}
    Result := ReadFloatMax;
    {$ELSE}
    Result := ReadDecimal;
    if Result = 0 then Result := UNSET_DECIMAL;
    {$ENDIF}
  end;



label Break_OPEN_ORDER_case;
begin

  Version := 0;
  pa := FInStream.Memory;
  BOS := FInStream.Memory;       // beginning of stream
  EOS := FInStream.Memory;
  inc(EOS,FInStream.Position);  // end of stream

  {$IFDEF CAPTURE_TWS_STREAM}
    DeleteFile('CrntStreamIn.txt');
    PByte(NativeUInt(FInStream.Memory) + NativeUInt(FInStream.Position))^ := $ff;
    PInteger(NativeUInt(FInStream.Memory) + NativeUInt(FInStream.Size) - 4)^ := FInStream.Position;

    FInStream.SaveToFile('CrntStreamIn.txt');
  {$ENDIF}

  repeat                        // for multiple messages in stream

    if FClientv100plus then
      begin
        if (NativeUInt(BOS) + 4) >= NativeUInt(EOS) then
          Break;
        Move(BOS^, netlen, HEADER_LEN);
        msgLen := ntohl(netLen);
        if (msgLen <= 0) or (msgLen > MAX_MSG_LEN) then
          raise EIABSocket.Create('Data stream Fail: MsgLen invalid - reports ' + IntToStr(msgLen) + ' bytes.');
        if (NativeUInt(BOS) + 4 + LongWord(msgLen)) > NativeUInt(EOS) then
          Break;
        inc(pa, HEADER_LEN);
      end
    else
      begin
        if not ScanAhead(1) then Break;
      end;

    MsgId := ReadInt;
    case MsgId of

      -1:begin
        Version := -1;
        BOS := EOS;  // rubs out the whole stream: the -1 value should not occur though??
      end;

      TICK_PRICE:begin
        if not ScanAhead(6) then Break;
        Version := ReadInt;   // no longer used
        p1 := ReadInt; // dataid
        p2 := ReadInt; // ticktype
        d := ReadFloat; // price
        dm1 := ReadDecimal;  //p3 := ReadInt; // size
        p4 := p2;
        if (TIABTickType(p2) in [ttBid,ttAsk,ttLast]) then
          inc(p4)
        else if (TIABTickType(p2) in [ttDelayedBid,ttDelayedAsk,ttDelayedLast]) then
          inc(p4, 3);
        if (p4 > p2) and Assigned(FOnTickPriceAndSize) then FOnTickPriceAndSize(Self,p1,TIABTickType(p4),d,dm1);
        p4 := ReadInt; // CanAutoExecute
        TickAttrib.CanAutoExecute := p4 = 1;

	      if (FServerVersion >= MIN_SERVER_VER_PAST_LIMIT) then // 109
          begin
            TickAttrib.CanAutoExecute := p4 and $01 > 0;
            TickAttrib.PastLimit := p4 and $02 > 0;
    	      if (FServerVersion >= MIN_SERVER_VER_PRE_OPEN_BID_ASK) then // 132
              Tickattrib.PreOpen := p4 and $04 > 0;
          end;
        if Assigned(FOnTickPrice) then FOnTickPrice(Self,p1,TIABTickType(p2),d, TickAttrib);
        BOS := pa;
      end;

      TICK_SIZE:begin
        if not ScanAhead(4) then Break;
        Version := ReadInt;
        p1 := ReadInt; // dataid
        p2 := ReadInt; // ticktype
        dm1 := ReadDecimal;  //p3 := ReadInt; // size
        if Assigned(FOnTickSize) then FOnTickSize(Self,p1,TIABTickType(p2),dm1);
        BOS := pa;
      end;

      TICK_OPTION_COMPUTATION:begin
      // [21;6;100;12;2.8168881223687308;-0.10617530601785129;2.9999999998848343;0.0;0.003299866561903041;0.03795061710001736;-14.867859811456707;643.79]
        if not ScanAhead(5) then Break;
        Version := ReadInt;
        p1 := ReadInt; // dataid
        p2 := ReadInt; // ticktype
        d := ReadFloat; // impliedVol
        d2 := ReadFloat;  // delta
        if d = -1.0 then
          d := UNSET_DOUBLE;  // -1 is the "not computed" indicator
        if (d2 = -2.0) then
          d2 := UNSET_DOUBLE; // -2 is the "not computed" indicator
        d3 := UNSET_DOUBLE;
        d4 := UNSET_DOUBLE;
	      if ((Version >= 6) or (TIABTickType(p2) = ttModelOption) or (TIABTickType(p2) = ttDelayedModelOptionComputation)) then  // introduced in version == 5
          begin
            if not ScanAhead(2) then Break;
            d3 := ReadFloat; //  optPrice
            d4 := ReadFloat; //  pvDividend
            if (d3 = -1.0) then
              d3 := UNSET_DOUBLE;  // -1 is the "not computed" indicator
            if (d4 = -1.0) then
              d4 := UNSET_DOUBLE;  // -1 is the "not computed" indicator
          end;
        d5 := UNSET_DOUBLE;
        d6 := UNSET_DOUBLE;
        d7 := UNSET_DOUBLE;
        d8 := UNSET_DOUBLE;
        if Version >= 6 then
          begin
            if not ScanAhead(4) then Break;
            d5 := ReadFloat;  // gamma
            d6 := ReadFloat;  // vega
            d7 := ReadFloat;  // theta
            d8 := ReadFloat;  // undPrice
	    			if (d5 = -2.0) then // gamma  -2 is the "not yet computed" indicator
						  d5 := UNSET_DOUBLE;
  					if (d6 = -2.0) then // vega   -2 is the "not yet computed" indicator
						  d6 := UNSET_DOUBLE;
	   				if (d7 = -1.0) then // theta  -2 is the "not yet computed" indicator
						  d7 := UNSET_DOUBLE;
  					if (d8 = -1.0) then              // undPrice  -1 is the "not computed" indicator
						  d8 := UNSET_DOUBLE;
          end;
        if Assigned(FOnTickOptionComputation) then FOnTickOptionComputation(Self,p1,TIABTickType(p2),d,d2,d3,d4,d5,d6,d7,d8);
        BOS := pa;
      end;

      TICK_GENERIC:begin
        if not ScanAhead(4) then Break;
        Version := ReadInt;
        p1 := ReadInt; // dataid
        p2 := ReadInt; // ticktype
        d := ReadFloat; // Value
        if Assigned(FOnTickGeneric) then FOnTickGeneric(Self,p1,TIABTickType(p2),d);
        BOS := pa;
      end;

      TICK_STRING:begin
        if not ScanAhead(4) then Break;
        Version := ReadInt;
        p1 := ReadInt; // dataid
        p2 := ReadInt; // ticktype
        s1 := ReadStream; // Value
        if Assigned(FOnTickString) then FOnTickString(Self,p1,TIABTickType(p2),s1);
        BOS := pa;
      end;

      TICK_EFP:begin
        if not ScanAhead(10) then Break;
        Version := ReadInt;
        p1 := ReadInt; // dataid
        p2 := ReadInt; // ticktype
        d := ReadFloat; // basisPoints
        s1 := ReadStream; // formattedBasisPoints
        d2 := ReadFloat;  // impliedFuturesPrice
        p3 := ReadInt;    // holdDays
        s2 := ReadStream; // futureExpiry
        d3 := ReadFloat;  // dividendImpact
        d4 := ReadFloat;  // dividendsToExpiry
        if Assigned(FOnIABTickEFP) then FOnIABTickEFP(Self, p1, TIABTickType(p2), d, d2, d3, d4, p3, s2, s1);
        BOS := pa;
      end;

      ORDER_STATUS:begin
        FRebuildFromTWS := false;

        if (FServerVersion < MIN_SERVER_VER_MARKET_CAP_PRICE) then // 131
          begin
            if not ScanAhead(1) then Break;
            Version := ReadInt;
          end;

        if not ScanAhead(10) then Break;

        p1 := ReadInt;      // tempid
        s1 := ReadStream;   // status
        dm1 := ReadDecimal; //  p2 := ReadInt;      // filled
        dm2 := ReadDecimal; //  p3 := ReadInt;      // remain
        d := ReadFloat;     // avfillprice
        p4 := ReadInt; // permid       v2
        p5 := ReadInt; // parentid     v3
        d2 := ReadFloat; // lastFillPrice  v4
        p6 := ReadInt;  // ClientId        v5
        s2 := ReadStream;   // why held    v6

        d5 := UNSET_DOUBLE;
      	if (ServerVersion >= MIN_SERVER_VER_MARKET_CAP_PRICE) then  // 131
          begin
            if not ScanAhead(1) then Break;
            d5 := ReadFloat;    // mktCapPrice
          end;



        if (p6 = FClientId) and (p1 > FNextTempID) then FNextTempID := p1;

        if p4 > 0 then FOrders.SetPermId(p1,p6,p4);  // first look at permid on order from this client


// ****  bug fix  ****
        if (p1 = 0) and (p6 = 0) then // nothing - required to exclude orders generated in TWS - not able to distiguish
        else                          // them and subsequent overwriting and corruption occurs.  Awiating fix from IAB.
// ****  bug fix  ****


        (*******************************
          API 9.60
          Now the TWS added two more order state values APIPending and APICanceled.
          These would seem to be duplication of what was used prior.
          Here we return them to the original form.
          Note: what does APIPending mean?  submitted, pending, pre-submitted?
          We assume submitted here.

        *******************************)

        if CompareText(s1, 'ApiPending') = 0 then
          s1 := 'Submitted'
        else if CompareText(s1, 'ApiCancelled') = 0 then
          s1 := 'Cancelled';

        (*******************************)


        for i := Ord(High(TIABOrderState)) downto Ord(Low(TIABOrderState)) do
          if (CompareText(s1,OrderStateString[TIABOrderState(i)]) = 0) then
            begin
              os := TIABOrderState(i);
              if os = osCancelled then
                OrderStatus := AmendOrderCancel(p1,p6,p4)
              else
                //OrderStatus := AmendOrderFill(p1,p6,p4,p2,p3,d,d2,os);
                //OrderStatus := AmendOrderFill(p1,p6,p4,d3,d4,d,d2,os,d5);
                OrderStatus := AmendOrderFill(p1,p6,p4,dm1,dm2,d,d2,os,d5);
              if OrderStatus <> nil then
                begin
                  OrderStatus.FParentId := p5;
                  OrderStatus.FWhyHeld := s2;
                  if Assigned(FOnOrderStatus) then FOnOrderStatus(Self,OrderStatus,os);
                end;
              Break;
            end;
        BOS := pa;
      end;

      ERR_MSG:begin
        if not ScanAhead(1) then Break;
        Version := ReadInt;
        if Version < 2 then
          begin
            if not ScanAhead(1) then Break;
            s1 := ReadStream;
            if Assigned(FOnError) then FOnError(Self,NO_VALID_ID, NO_VALID_ERROR_CODE, s1);
          end
        else
          begin
            if not ScanAhead(3 + Integer(FServerVersion >= MIN_SERVER_VER_ADVANCED_ORDER_REJECT)) then Break;
            p1 := ReadInt; // id
            p2 := ReadInt; // errorCode
            s1 := ReadStream;           // msg

           	if (FServerVersion >= MIN_SERVER_VER_ADVANCED_ORDER_REJECT) then
              s2 := ReadStream  // advancedOrderRejectJson
            else
              s2 := '';
            if s2 <> '' then
              s1 := s1 + ' >>> ' + s2;
            if p1 = $7fffffff then p1 := -1;  // fix a type cast problem in TWS.
            if Assigned(FOnError) then FOnError(Self,p1,p2,s1);
          end;
        BOS := pa;
      end;

      OPEN_ORDER:begin
        if not ScanAhead(38 + Integer(FServerVersion < MIN_SERVER_VER_ORDER_CONTAINER)) then Break;

        if (FServerVersion >= MIN_SERVER_VER_ORDER_CONTAINER) then  // 145
          Version := FServerVersion
        else
          Version := ReadInt;

        FOrderState.Free;
        FOrderState := TIABOrder.Create;

        p1 := ReadInt; // order id   //decodeOrderId
        FOrderState.FTempId := p1;

        // Contract portion     //decodeContract
        FOrderState.FContractId := ReadInt; // conId
        FOrderState.FSymbol := ReadStream;  // symbol
        s1 := ReadStream;                  // sectype
        FOrderState.FSecurityType := DecodeSecurityType(s1);
        FOrderState.FExpiry := ReadStream;  // expiry   // lastTradeDateOrContractMonth
        FOrderState.FStrike := ReadFloat;  // strike
        s1 := ReadStream;                   // right
        FOrderState.FRight := DecodeRight(s1);
        if Version >= 32 then
          FOrderState.FMultiplier := ReadStream;  //multiplier
        FOrderState.FExchange := ReadStream;     //  exch
        FOrderState.FCurrency := ReadStream;    // currency
        FOrderState.FLocalSymbol := ReadStream;  // local
        if Version >= 32 then
          FOrderState.FTradingClass := ReadStream;   // tradingClass
        // end Contract portion

        s1 := ReadStream;                  // side
        FOrderState.FAction := ActionStringType(s1);

        {*
        if (FServerVersion >= MIN_SERVER_VER_FRACTIONAL_POSITIONS) then   // 101
          FOrderState.FQuantity := ReadFloat          // totalQuantity
        else
          FOrderState.FQuantity := ReadInt;          // totalQuantity
          *}
        FOrderState.FQuantity := ReadDecimal;          // totalQuantity


        s1 := ReadStream;                           // order type
        FOrderState.FOrderType := DecodeOrderType(s1);

				if Version < 29 then                             // 60
					FOrderState.FPrice := ReadFloat        //LmtPrice
				else
					FOrderState.FPrice := ReadFloatMax;    //LmtPrice
				if Version < 30 then                            //60
					FOrderState.FAuxPrice := ReadFloat        //AuxPrice
				else
					FOrderState.FAuxPrice := ReadFloatMax;     //AuxPrice
        s1 := ReadStream;                  // tif
        FOrderState.FTimeInForce := DecodeTIF(s1);

        FOrderState.FOCAgroup := ReadStream;      // oca
        FOrderState.FAccount := ReadStream;       //  account
        s1 := ReadStream;                           // openclose
        if Length(s1) > 0 then
          FOrderState.FOpenClose := s1[1];
        FOrderState.FOrderOrigin := TIABOrderOrigin(ReadInt);   // origin
        FOrderState.FOrderRef := ReadStream;          // order ref
        p2 := ReadInt;    // clientId
        FOrderState.ClientId := p2;
        if (p2 = FClientId) and (p1 > FNextTempID) then FNextTempID := p1;
        if p2 > -1 then FOrderState.FExternalOrder := p2 <> FClientId;

        FOrderState.PermId := ReadInt;   //permid
        FOrderState.ExtendedHours := ReadBool; //ReadInt > 0;    //rth
        FOrderState.Hidden := ReadBool; //ReadInt > 0;           //hidden
        FOrderState.DiscretAmount := ReadFloat;          // DiscretAmount
        FOrderState.GoodAfterTime := ReadStream;        //goodafter
        //  this item depreceated in 9.40 - should produce a null
        s1 := ReadStream; // dropped 10.10; FOrderState.ShareAllocation := ReadStream;  // ShareAllocation

        //decodeFAParams
        FOrderState.FAdvGroup := ReadStream;   // faGroup
        FOrderState.FAdvMethod := ReadStream;   // faMethod
        FOrderState.FAdvPercentage := ReadStream;    // faPercentage
        FOrderState.FAdvProfile := ReadStream;        // faProfile

        if (FServerVersion >= MIN_SERVER_VER_MODELS_SUPPORT ) then // 103
          FOrderState.ModelCode := ReadStream;                        // ModelCode

        FOrderState.FGoodTillDate := ReadStream;                     //  GoodTillDate
        s1 := ReadStream;        // rule 80
        FOrderState.FRule80A := DecodeRule80A(s1);

        FOrderState.FPercentOffset := ReadFloatMax;      //  PercentOffset
        FOrderState.FSettlingFirm := ReadStream;          // SettlingFirm

        // decodeShortSaleParams
        FOrderState.FShortSaleSlot := ReadInt;            // shortSaleParam
        FOrderState.FDesignatedLocation := ReadStream;    // designatedLocation
        if ((FServerVersion = MIN_SERVER_VER_SSHORTX_OLD) or (Version >= 23)) and not ScanAhead(1) then Break;
			  if FServerVersion = MIN_SERVER_VER_SSHORTX_OLD then      // 51
			    FOrderState.FExemptCode := ReadInt       // exemptCode
    	 	else if Version >= 23 then
          FOrderState.FExemptCode := ReadInt;       // exemptCode


        if not ScanAhead(21) then Break;
        i := ReadInt;                               // auctionStrategy
        if i in [0..Ord(High(TIABAuctionStrategy))] then
          FOrderState.FAuctionStrategy := TIABAuctionStrategy(i);

        // decodeBoxOrderParams
        FOrderState.FStartingPrice := ReadFloatMax;      // startingPrice
        FOrderState.FStockRefPrice := ReadFloatMax;      // stockRefPrice
        FOrderState.FDelta := ReadFloatMax;              // delta

        // decodePegToStkOrVolOrderParams
        FOrderState.FStockRangeLower := ReadFloatMax;       // stockRangeLower
        FOrderState.FStockRangeUpper := ReadFloatMax;       // stockRangeUpper

        FOrderState.FDisplaySize := ReadIntMax;          // displaySize
        FOrderState.FBlockOrder := ReadBool;             // blockOrder
        FOrderState.FSweepToFill := ReadBool;            // sweepToFill
        FOrderState.FAllOrNone := ReadBool;              // allOrNone
        FOrderState.FMinQuantity := ReadIntMax;          // minQty
        i := ReadInt -1;                                // ocaType
        if i in [0..Ord(High(TIABOcaMethod))] then
          FOrderState.FOcaMethod := TIABOcaMethod(i);

        //FOrderState.FETradeOnly := ReadBool;       // dropped in TWS API 10.10
        //FOrderState.FFirmQuoteOnly := ReadBool;        // dropped in TWS API 10.10
        //FOrderState.FNBBOPriceCap := ReadFloatMax;       // dropped in TWS API 10.10
        s1 := ReadStream + ReadStream + ReadStream;

        FOrderState.FParentId := ReadInt;           //parentId
        FOrderState.FTriggerMethod := ReadInt;      //triggerMethod  // 0=Default, 1=Double_Bid_Ask, 2=Last, 3=Double_Last, 4=Bid_Ask, 7=Last_or_Bid_Ask, 8=Mid-point

        // decodeVolOrderParams
        FOrderState.FVolatility := ReadFloatMax;          //volatility
        i := ReadInt;                                     //volatilityType
        if TIABVolatilityPeriod(i) in [low(TIABVolatilityPeriod)..High(TIABVolatilityPeriod)] then
          FOrderState.FVolatilityPeriod := TIABVolatilityPeriod(i) else FOrderState.FVolatilityPeriod := vpUnSet;
        s1 := ReadStream;    // DeltaNeutralOrderType
        FOrderState.FOrderType := DecodeOrderType(s1);

        FOrderState.FDeltaNeutralAuxPrice := ReadFloatMax;    // deltaNeutralAuxPrice
        if (Version >= 27) and (FOrderState.DeltaNeutralOrderType > otNoChange) then
          begin
            if not ScanAhead(4) then Break;
    	      FOrderState.FDeltaNeutralConId := ReadInt;
            if true then                 // decodeOpenOrderAttribs = set to true always.
              begin
     			      FOrderState.FDeltaNeutralSettlingFirm := ReadStream;        // deltaNeutralSettlingFirm
		    	      FOrderState.FDeltaNeutralClearingAccount := ReadStream;     // deltaNeutralClearingAccount
			          FOrderState.FDeltaNeutralClearingIntent := ReadStream;      // deltaNeutralClearingIntent
              end;
          end;
        if (Version >= 31) and (FOrderState.DeltaNeutralOrderType > otNoChange) then
          begin
            if not ScanAhead(4) then Break;
            if true then                // decodeOpenOrderAttribs = set to true always.
            	FOrderState.FDeltaNeutralOpenClose := ReadStream;             // deltaNeutralOpenClose
    				FOrderState.FDeltaNeutralShortSale := ReadBool;                 // deltaNeutralShortSale
			      FOrderState.FDeltaNeutralShortSaleSlot := ReadInt;              // deltaNeutralShortSaleSlot
			      FOrderState.FDeltaNeutralDesignatedLocation := ReadStream;      // deltaNeutralDesignatedLocation
          end;
        if not ScanAhead(1) then Break;
        FOrderState.FContinuousUpdate := ReadBool;          // continuousUpdate
        if not ScanAhead(6) then Break;
        i := ReadInt;                                       // referencePriceType
        if TIABReferencePrice(i) in [low(TIABReferencePrice)..High(TIABReferencePrice)] then
          FOrderState.FReferencePrice := TIABReferencePrice(i) else FOrderState.FReferencePrice := rpUnSet;
        // end decodeVolOrderParams

        // decodeTrailParams
        FOrderState.FTrailStopPrice := ReadFloatMax;             // trailStopPrice
        if Version >= 30 then
					FOrderState.FTrailingPercent := ReadFloatMax;          // trailingPercent

        // decodeBasisPoints
        FOrderState.FEFPBasisPoints := ReadFloatMax;             // basisPoints
        FOrderState.FEFPBasisType := ReadIntMax;                 // basisPointsType

        // decodeComboLegs
        FOrderState.FComboLegsDescrip := ReadStream;             // comboLegsDescrip
				if Version >= 29 then
          begin
            FOrderState.FComboLegs := nil;
            if not ScanAhead(1) then Break;
            p3 := ReadInt;                                  // ComboLegsCount
					  if p3 > 0  then
              begin
                if not ScanAhead(p3 * 8) then Break;
                for i := 0 to p3 -1 do
                  begin
                    ComboLeg.ContractId := ReadInt;               // conID
                    ComboLeg.Ratio := ReadInt;                    // ratio
                    s1 := ReadStream;                             // action
                    ComboLeg.Action := ActionStringType(s1);
                    ComboLeg.Exchange := ReadStream;              // exchange
                    p3 := ReadInt;                                // openClose
                    ComboLeg.OpenClose := TIABLegOpenClose(p3);
                    ComboLeg.ShortSaleSlot := ReadInt;            // shortSaleSlot
                    ComboLeg.DesignatedLocation := ReadStream;    // designatedLocation
                    ComboLeg.ExemptCode := ReadInt;               // exemptCode
                    FOrderState.AddComboLeg(ComboLeg);
                  end;
              end;
            FOrderState.FComboLegPrice := nil;
            if not ScanAhead(1) then Break;
            p3 := ReadInt;                              // OrderComboLegsCount
					  if p3 > 0  then
              begin
                if not ScanAhead(p3) then Break;
                for i := 0 to p3 -1 do
                  begin
                    d := ReadFloatMax;                      // price
                    FOrderState.AddComboLegPrice(d);
                  end;
              end;
          end;
        // end decodeComboLegs

        // decodeSmartComboRoutingParams
			  if Version >= 26 then
          begin
            FOrderState.FSmartComboRoutingParams := nil;
            if not ScanAhead(1) then Break;
            p3 := ReadInt;          // smartComboRoutingParamsCount
					  if p3 > 0  then
              begin
                if not ScanAhead(p3 * 2) then Break;
                for i := 0 to p3 -1 do
                  begin
                    TagValue.Tag := ReadStream;           // tag
                    TagValue.Value := ReadStream;         // value
                    FOrderState.AddSmartComboRoutingParams(TagValue);
                  end;
              end;
          end;

        // decodeScaleOrderParams
        if not ScanAhead(3) then Break;
        if Version >= 20 then
          begin
            FOrderState.FScaleInitLevelSize := ReadIntMax;  // scaleInitLevelSize
            FOrderState.FScaleSubsLevelSize := ReadIntMax;  // scaleSubsLevelSize
          end
        else
          begin
            i := ReadInt; // trash it        // scaleInitLevelSize
            if i = 12345678 then                           // to force compile of readInt;
              FOrderState.FScaleInitLevelSize := 87654321  // to force compile of readInt;
            else
              FOrderState.FScaleInitLevelSize := UNSET_INTEGER;  // scaleInitLevelSize
            FOrderState.FScaleSubsLevelSize := ReadIntMax;
          end;
        FOrderState.FScalePriceIncrement := ReadFloatMax;     //   scalePriceIncrement
				if (Version >= 28) and (FOrderState.FScalePriceIncrement > 0.0) and (FOrderState.FScalePriceIncrement < UNSET_DOUBLE_TEST_VALUE) then  //60
          begin
            if not ScanAhead(7) then Break;
					  FOrderState.FScalePriceAdjustValue := ReadFloatMax;      // scalePriceAdjustValue
					  FOrderState.FScalePriceAdjustInterval := ReadIntMax;     // scalePriceAdjustInterval
					  FOrderState.FScaleProfitOffset := ReadFloatMax;          // scaleProfitOffset
					  FOrderState.FScaleAutoReset := ReadBool;                 // scaleAutoReset
					  FOrderState.FScaleInitPosition := ReadIntMax;            // scaleInitPosition
					  FOrderState.FScaleInitFillQty := ReadIntMax;             // scaleInitFillQty
					  FOrderState.FScaleRandomPercent := ReadBool;             // scaleRandomPercent
          end;
        // end decodeScaleOrderParams

        // decodeHedgeParams
        if not ScanAhead(3) then Break;
				if Version >= 24 then
          begin
            FOrderState.FHedgeType := HedgeTypeStrType(ReadStream);   // hedgeType   // 'D' - delta, 'B' - beta, 'F' - FX, 'P' - pair
            if FOrderState.FHedgeType > htUnset then
			    	  FOrderState.FHedgeParam := ReadStream;                  // hedgeParam // 'beta=X' value for beta hedge, 'ratio=Y' for pair hedge
          end;

				if Version >= 25 then
					FOrderState.FOptOutSmartRouting := ReadBool;            // optOutSmartRouting

        // decodeClearingParams
        if not ScanAhead(3) then Break;
        FOrderState.FClearingAccount := ReadStream;     // clearingAccount
        FOrderState.FClearingIntent := ReadStream;      // clearingIntent

        if Version >= 22 then
            FOrderState.FNotHeld := ReadBool;           // notHeld

        // decodeDeltaNeutral
        if Version >= 20 then
          begin
            if not ScanAhead(1) then Break;
            FOrderState.FDeltaNeutralContractId := 0;
            FOrderState.FDeltaNeutralContractDelta := 0;
            FOrderState.FDeltaNeutralContractPrice := 0.0;
            if ReadBool then                     // deltaNeutralContractPresent
              begin
                if not ScanAhead(3) then Break;
                FOrderState.FDeltaNeutralContractId := ReadInt;         // deltaNeutralContract.conId
                FOrderState.FDeltaNeutralContractDelta := ReadFloat;    // deltaNeutralContract.delta
                FOrderState.FDeltaNeutralContractPrice := ReadFloat;    // deltaNeutralContract.price
              end;
          end;

        // decodeAlgoParams
        if Version >= 21 then
          begin
            if not ScanAhead(1) then Break;
            FOrderState.FAlgoParams := nil;
            FOrderState.FAlgoStrategy := ReadStream;       // algoStrategy
            if Length(FOrderState.FAlgoStrategy) > 0 then
              begin
                if not ScanAhead(1) then Break;
                p3 := ReadInt;                    // algoParamsCount
                if p3 > 0 then
                  begin
                    if not ScanAhead(p3 * 2) then Break;
                    for i := 0 to p3 -1 do
                      begin
                        TagValue.Tag := ReadStream;           // tag
                        TagValue.Value := ReadStream;         // value
                        FOrderState.AddAlgoParams(TagValue);
                      end;
                  end;
              end;
          end;

        if (Version >= 33) then
          FOrderState.Solicited := ReadBool;         // solicited

        // decodeWhatIfInfoAndCommission
        if not ScanAhead(2) then Break;
        FOrderState.FVerified := ReadBool;  // WhatIf
        FOrderState.FQueryResult.Status := ReadStream;
        if (FServerVersion >= MIN_SERVER_VER_WHAT_IF_EXT_FIELDS) then  // 142
          begin
            if not ScanAhead(6) then Break;
            FOrderState.FQueryResult.InitMarginBefore := ReadStream;
            FOrderState.FQueryResult.MaintMarginBefore := ReadStream;
            FOrderState.FQueryResult.EquityWithLoanBefore := ReadStream;
            FOrderState.FQueryResult.InitMarginChange := ReadStream;
            FOrderState.FQueryResult.MaintMarginChange := ReadStream;
            FOrderState.FQueryResult.EquityWithLoanChange := ReadStream;
          end
        else
          begin
            FOrderState.FQueryResult.InitMarginBefore := '';
            FOrderState.FQueryResult.MaintMarginBefore := '';
            FOrderState.FQueryResult.EquityWithLoanBefore := '';
            FOrderState.FQueryResult.InitMarginChange := '';
            FOrderState.FQueryResult.MaintMarginChange := '';
            FOrderState.FQueryResult.EquityWithLoanChange := '';
          end;
        if not ScanAhead(8) then Break;
        FOrderState.FQueryResult.InitMarginAfter := ReadStream;
        FOrderState.FQueryResult.MaintMarginAfter := ReadStream;
        FOrderState.FQueryResult.EquityWithLoanAfter := ReadStream;
        FOrderState.FQueryResult.Commission := ReadFloatMax;
        FOrderState.FQueryResult.MinCommission := ReadFloatMax;
        FOrderState.FQueryResult.MaxCommission := ReadFloatMax;
        FOrderState.FQueryResult.CommissionCurrency := ReadStream;
        FOrderState.FQueryResult.WarningText := ReadStream;

        // decodeVolRandomizeFlags
        if (Version >= 34) then
          begin
            if not ScanAhead(2) then Break;
            FOrderState.RandomizeSize := ReadBool;
            FOrderState.RandomizePrice := ReadBool;
          end;
        // decodePegBenchParams
	      if (FServerVersion >= MIN_SERVER_VER_PEGGED_TO_BENCHMARK) then  // 102
          begin
            if (FOrderState.OrderType = otPegBench) then
              begin
                if not ScanAhead(5) then Break;
		    	      FOrderState.ReferenceContractId := ReadInt;
			          FOrderState.IsPeggedChangeAmountDecrease := ReadBool;
    			      FOrderState.PeggedChangeAmount := ReadFloat;
		    	      FOrderState.ReferenceChangeAmount := ReadFloat;
			          FOrderState.ReferenceExchangeId := ReadStream;
              end;
          end;
        // decodeConditions
	      if (FServerVersion >= MIN_SERVER_VER_PEGGED_TO_BENCHMARK) then  // 102
          begin
            if not ScanAhead(1) then Break;
            p3 := ReadInt;  //conditionsSize;
		        if (p3 > 0) then
              begin
                // conditions not used, but need to remove data
                if not ScanAhead(p3 + 2) then Break;
                s1 := '';
                for i := 0 to p3 -1 do
                  s1 := s1 + ReadStream;   // conditionType
                s1 := s1 + ReadStream;     // conditionsIgnoreRth
                s1 := s1 + ReadStream;     // conditionsCancelOrder
              end;
          end;
        // decodeAdjustedOrderParams
	      if (FServerVersion >= MIN_SERVER_VER_PEGGED_TO_BENCHMARK) then  // 102
          begin
            if not ScanAhead(8) then Break;
		        FOrderState.AdjustedOrderType := DecodeOrderType(ReadStream);
		        FOrderState.TriggerPrice := ReadFloat;
              //decodeStopPriceAndLmtPriceOffset
  		        FOrderState.TrailStopPrice := ReadFloat;
	  	        FOrderState.LmtPriceOffset := ReadFloat;
		        FOrderState.AdjustedStopPrice := ReadFloat;
		        FOrderState.AdjustedStopLimitPrice := ReadFloat;
		        FOrderState.AdjustedTrailingAmount := ReadFloat;
		        FOrderState.AdjustableTrailingUnit := ReadInt;
          end;


        i := 3 * Integer(FServerVersion >= MIN_SERVER_VER_SOFT_DOLLAR_TIER) +
              Integer(FServerVersion >= MIN_SERVER_VER_CASH_QTY) +
              Integer(FServerVersion >= MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE) +
              Integer(FServerVersion >= MIN_SERVER_VER_ORDER_CONTAINER) +
              Integer(FServerVersion >= MIN_SERVER_VER_D_PEG_ORDERS) +
              Integer(FServerVersion >= MIN_SERVER_VER_PRICE_MGMT_ALGO) +
              Integer(FServerVersion >= MIN_SERVER_VER_DURATION) +
              Integer(FServerVersion >= MIN_SERVER_VER_POST_TO_ATS) +
              Integer(FServerVersion >= MIN_SERVER_VER_AUTO_CANCEL_PARENT) +
              5 * Integer(FServerVersion >= MIN_SERVER_VER_PEGBEST_PEGMID_OFFSETS)
              ;
        if (i > 0) and (not ScanAhead(i)) then Break;

        // decodeSoftDollarTier
        if (FServerVersion >= MIN_SERVER_VER_SOFT_DOLLAR_TIER) then  // 106
          begin
        		ASoftDollarTier.Name := ReadStream;
        		ASoftDollarTier.Value := ReadStream;
        		ASoftDollarTier.DisplayName := ReadStream;
            FOrderState.SoftDollarTier := ASoftDollarTier;
        	end;


      	if (FServerVersion >= MIN_SERVER_VER_CASH_QTY) then  // 111

            FOrderState.CashQuantity := ReadFloat;


 	      if FServerVersion >= MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE then // 141

      		FOrderState.FDontUseAutoPriceForHedge := ReadBool;


      	if FServerVersion >= MIN_SERVER_VER_ORDER_CONTAINER then  // 145

          FOrderState.FIsOmsContainer := ReadBool;


      	if FServerVersion >= MIN_SERVER_VER_D_PEG_ORDERS then   // 148

          FOrderState.FDiscretionaryUpToLimitPrice := ReadBool;


      	if FServerVersion >= MIN_SERVER_VER_PRICE_MGMT_ALGO then // 151

          begin

            FOrderState.FUsePriceMgmtAlgo := ReadInt;

            if FOrderState.FUsePriceMgmtAlgo = 2 then

              FOrderState.FUsePriceMgmtAlgo := UNSET_INTEGER;

          end;


      	if FServerVersion >= MIN_SERVER_VER_DURATION then // 158

          FOrderState.FDuration := ReadInt;


      	if FServerVersion >= MIN_SERVER_VER_POST_TO_ATS then // 160

          FOrderState.FPostToAts := ReadInt;


      	if FServerVersion >= MIN_SERVER_VER_AUTO_CANCEL_PARENT then // 162

          FOrderState.FAutoCancelParent := ReadBool;


        // decodePegBestPegMidOrderAttributes

        if (FServerVersion >= MIN_SERVER_VER_PEGBEST_PEGMID_OFFSETS) then

          begin

            FOrderState.MinTradeQty := ReadInt;
            FOrderState.MinCompeteSize := ReadInt;
            FOrderState.CompeteAgainstBestOffset := ReadFloat;
            FOrderState.MidOffsetAtWhole := ReadFloat;
            FOrderState.MidOffsetAtHalf := ReadFloat;

          end;



        // ****  end of streamed data read, now put it into saved objects / classes  *****



        if FOrderState.FVerified then
          begin
            i := FVerifiedOrders.IndexOfTempId(p1,-1);
            if i > -1 then
              with FVerifiedOrders.Items[i] do begin
                FVerified := true;
                FQueryResult.Status := FOrderState.FQueryResult.Status;
                FQueryResult.InitMarginBefore := FOrderState.FQueryResult.InitMarginBefore;
                FQueryResult.MaintMarginBefore := FOrderState.FQueryResult.MaintMarginBefore;
                FQueryResult.EquityWithLoanBefore := FOrderState.FQueryResult.EquityWithLoanBefore;
                FQueryResult.InitMarginChange := FOrderState.FQueryResult.InitMarginChange;
                FQueryResult.MaintMarginChange := FOrderState.FQueryResult.MaintMarginChange;
                FQueryResult.EquityWithLoanChange := FOrderState.FQueryResult.EquityWithLoanChange;
                FQueryResult.InitMarginAfter := FOrderState.FQueryResult.InitMarginAfter;
                FQueryResult.MaintMarginAfter := FOrderState.FQueryResult.MaintMarginAfter;
                FQueryResult.EquityWithLoanAfter := FOrderState.FQueryResult.EquityWithLoanAfter;
                FQueryResult.Commission := FOrderState.FQueryResult.Commission;
                FQueryResult.MinCommission := FOrderState.FQueryResult.MinCommission;
                FQueryResult.MaxCommission := FOrderState.FQueryResult.MaxCommission;
                FQueryResult.CommissionCurrency := FOrderState.FQueryResult.CommissionCurrency;
                FQueryResult.WarningText := FOrderState.FQueryResult.WarningText;
                if Assigned(FOnVerifiedOrder) then FOnVerifiedOrder(Self,FVerifiedOrders[i]);
              end;
            BOS := pa;
            goto Break_OPEN_ORDER_case;
          end;


        (*****************************************************************  added in 9.50.2
          In about API 9.40, IAB decided to make this OPEN_ORDER event fire every time
          a change in order status occured.  A hugely unneccessay thing to do, and it made a
          complete screw up of all the existing code.  They decided to send it blindly,
          for every new logon to the TWS.

          Hence we use this simple timing window, to deliberately throw out OPEN_ORDER events
          when we have not specifically requested them (as in GetExecution, GetOpenOrder, BindToTWS).
          It is set to a 30 second window, to receive OPEN ORDER event only when we ask for them.*)

        if FAcceptOpenOrdersWindow < GetTickCount then
          begin
            BOS := pa;
            goto Break_OPEN_ORDER_case;
          end;

        (*  When requested, these events normally arrive in a single block.
            Hence we can shorten the remaining wait time to match.*)

        FAcceptOpenOrdersWindow := GetTickCount + 4000;

        (******************************************************************)


        //  ??  below no longer required??  since handeled dy ...Max calls above.


        (************   added by APM   *******************
        Nasty little bug in the IABSocket code that showed up when reconnecting to
        TWS with live API orders present.
        Basically, orders coming back from TWS seem to have some fields set to 0
        instead of UNSET as they should, and this fixes it.  The problem seemed
        most visible if attempting to use ModifyOrder.

        if FOrderState.FVolatilityPeriod = vpUnSet then
           FOrderState.FVolatility := UNSET_DOUBLE;
        if FOrderState.FPercentOffset = 0 then
          FOrderState.FPercentOffset := UNSET_DOUBLE;
        if FOrderState.FNBBOPriceCap = 0 then
          FOrderState.FNBBOPriceCap := UNSET_DOUBLE;
        if FOrderState.FStartingPrice = 0 then
          FOrderState.FStartingPrice := UNSET_DOUBLE;
        if FOrderState.FStockRefPrice = 0 then
          FOrderState.FStockRefPrice := UNSET_DOUBLE;
        if FOrderState.FDelta = 0 then
          FOrderState.FDelta := UNSET_DOUBLE;
        if FOrderState.FStockRangeLower = 0 then
          FOrderState.FStockRangeLower := UNSET_DOUBLE;
        if FOrderState.FStockRangeUpper = 0 then
          FOrderState.FStockRangeUpper := UNSET_DOUBLE;
        if FOrderState.FDeltaNeutralOrderType = otNone then
          FOrderState.FDeltaNeutralAuxPrice := UNSET_DOUBLE;
        if FOrderState.FTrailStopPrice = 0 then
          FOrderState.FTrailStopPrice := UNSET_DOUBLE;
        if FOrderState.FEFPBasisPoints = 0 then
          FOrderState.FEFPBasisPoints := UNSET_DOUBLE;
        if FOrderState.FEFPBasisType = 0 then
          FOrderState.FEFPBasisType := UNSET_INTEGER;

        (************  end;  added by APM   *******************)

        (*

        if FOrderState.FMinQuantity = 0 then
          FOrderState.FMinQuantity := UNSET_INTEGER;

        if FOrderState.FQueryResult.Commission = 0 then
          FOrderState.FQueryResult.Commission := UNSET_DOUBLE;
        if FOrderState.FQueryResult.MinCommission = 0 then
          FOrderState.FQueryResult.MinCommission := UNSET_DOUBLE;
        if FOrderState.FQueryResult.MaxCommission = 0 then
          FOrderState.FQueryResult.MaxCommission := UNSET_DOUBLE;

          *)


// ****  bug fix  ****
        if (p1 = 0) and (p2 = 0) and (FOrderState.PermId = -1) then // nothing - required to exclude orders generated in TWS - not able to distiguish
        else                          // them and subsequent overwriting and corruption occurs.  Awiating fix from IAB.
// ****  bug fix  ****

        if ((FOrders.IndexOfTempId(p1,p2) = -1) and (FOrders.IndexOfPermId(FOrderState.PermId) = -1)) then
          begin                                     // when old open orders are regenerated after a restart
            NewOrderState := TIABOrder.Create;      // or strangers appear from other clients / tws
            NewOrderState.Assign(FOrderState);
            FOrders.Add(p1,p2,0,NewOrderState);
            case FRebuildFromTWS of
              false: if Assigned(FOnOpenOrder) then FOnOpenOrder(Self,NewOrderState);
              true:  if Assigned(FOnRebuildFromTWS) then FOnRebuildFromTWS(Self);
            end;
          end
        else
          begin
            i := FOrders.IndexOfPermId(FOrderState.PermId);
            if i = -1 then i := FOrders.IndexOfTempId(p1,p2);
            with FOrders[i] do Assign(FOrderState);
            case FRebuildFromTWS of
              false: if Assigned(FOnOpenOrder) then FOnOpenOrder(Self,FOrders[i]);
              true:  if Assigned(FOnRebuildFromTWS) then FOnRebuildFromTWS(Self);
            end;
          end;
        BOS := pa;
        Break_OPEN_ORDER_case:
      end;

      ACCT_VALUE:begin
        if not ScanAhead(5) then Break;
        Version := ReadInt;
        s1 := ReadStream; // Key
        s2 := ReadStream; // Value
        s3 := ReadStream; // currency
        s4 := ReadStream; // Account name;
        p1 := -1;
        for i := 0 to FAccountValues.Count -1 do
          if Pos(s1,FAccountValues[i]) > 0 then
            begin
              FAccountValues[i] := s1 + ' ' + s2 + ' ' + s3 + ' ' + s4;
              p1 := i;
              Break;
            end;
        if p1 = -1 then p1 := FAccountValues.Add(s1 + ' ' + s2 + ' ' + s3 + ' ' + s4);
        if Assigned(FOnAccountValue) then FOnAccountValue(Self,p1);
        BOS := pa;
      end;

      PORTFOLIO_VALUE:begin
        if not ScanAhead(8) then Break;
        Version := ReadInt;
        PFUpdate.InstrumentId := ReadInt;
        PFUpdate.Symbol := ReadStream;
        s1 := ReadStream;
        PFUpdate.SecurityType := DecodeSecurityType(s1);
        PFUpdate.Expiry := ReadStream;
        PFUpdate.Strike := ReadFloat;
        s1 := ReadStream;
        PFUpdate.Right := DecodeRight(s1);
        if (Version >= 7) then
          begin
            if not ScanAhead(2) then Break;
            PFUpdate.Multiplier := ReadStream;
            PFUpdate.PrimaryExchange := ReadStream;
          end;

        if not ScanAhead(9 + Integer(Version >= 8)) then Break;
        PFUpdate.Currency := ReadStream;
        PFUpdate.Local := ReadStream;
        if Version >= 8 then
          PFUpdate.TradingClass := ReadStream
        else
          PFUpdate.TradingClass := '';

        //if (FServerVersion >= MIN_SERVER_VER_FRACTIONAL_POSITIONS) then
        //  PFUpdate.Position := ReadFloat
        //else
        //  PFUpdate.Position := ReadInt;
        PFUpdate.Position := ReadDecimal;

        PFUpdate.MarketPrice := ReadFloat;
        PFUpdate.MarketValue := ReadFloat;
        PFUpdate.AverageCost := ReadFloat;
        PFUpdate.UnrealizedPNL := ReadFloat;
        PFUpdate.RealizedPNL := ReadFloat;
        PFUpdate.AccountName := ReadStream;

        PFUpdate.PrimaryExchange := '';
        if (Version = 6) and (FServerVersion = 39) then
          begin
            if not ScanAhead(1) then Break;
            PFUpdate.PrimaryExchange := ReadStream;
          end;

        i := FPortfolio.Find(PFUpdate);
        if i > -1 then FPortfolio.Items[i] := {$IFDEF BCBCOMPILE} @PFUpdate {$ELSE} PFUpdate {$ENDIF}
        else i := FPortfolio.Add(PFUpdate);
        if Assigned(FOnPortfolioUpdate) then FOnPortfolioUpdate(Self,i);
        BOS := pa;
      end;

      ACCT_UPDATE_TIME:begin
        if not ScanAhead(2) then Break;
        Version := ReadInt;
        s1 := ReadStream;
        if Assigned(FOnAccountTime) then FOnAccountTime(Self,s1);
        BOS := pa;
      end;

      NEXT_VALID_ID:begin
        if not ScanAhead(2) then Break;
        Version := ReadInt;
        FNextTempID := ReadInt;
        BOS := pa;
      end;

      CONTRACT_DATA:begin
        if not ScanAhead(1) then Break;
        if (FServerVersion < MIN_SERVER_VER_SIZE_RULES) then
          Version := ReadInt
        else
          Version := MaxInt;

        if ((FServerVersion >= MIN_SERVER_VER_SIZE_RULES) and not ScanAhead(39)) then Break
        else if ((FServerVersion >= MIN_SERVER_VER_FRACTIONAL_SIZE_SUPPORT) and (FServerVersion < MIN_SERVER_VER_SIZE_RULES) and not ScanAhead(37)) then Break
        else if ((FServerVersion >= MIN_SERVER_VER_STOCK_TYPE) and not ScanAhead(36)) then Break
        else if ((FServerVersion >= MIN_SERVER_VER_REAL_EXPIRATION_DATE) and not ScanAhead(35)) then Break
        else if ((FServerVersion >= MIN_SERVER_VER_MARKET_RULES) and not ScanAhead(34)) then Break
        else if ((FServerVersion >= MIN_SERVER_VER_UNDERLYING_INFO) and not ScanAhead(32)) then Break
        else if ((FServerVersion >= MIN_SERVER_VER_AGG_GROUP) and not ScanAhead(31)) then Break
        else if ((FServerVersion >= MIN_SERVER_VER_MD_SIZE_MULTIPLIER) and not ScanAhead(30)) then Break
        else if ((Version >= 8) and not ScanAhead(29)) then Break
        else if ((Version >= 6) and not ScanAhead(27)) then Break
        else if ((Version >= 5) and not ScanAhead(20)) then Break
        else if ((Version >= 4) and not ScanAhead(18)) then Break
        else if ((Version >= 3) and not ScanAhead(17)) then Break
        else if ((Version >= 2) and not ScanAhead(16)) then Break
        else if not ScanAhead(15) then Break;

        InsSpecItem.DataID := -1;
        if (Version >= 3) then
          InsSpecItem.DataID := ReadInt;      // reqid
        InsSpecItem.Symbol := ReadStream;     // symbol
        s1 := ReadStream;                     // secType
        InsSpecItem.SecurityType := DecodeSecurityType(s1);
        // InsSpecItem.Expiry := ReadStream;
        s1 := ReadStream;  // lastTradeDateOrContractMonth
        //  decodeLastTradeDate function
        i := Pos(' ', s1);
        InsSpecItem.LastTradeTime := '';
        InsSpecItem.TimeZoneID := '';
        if i = 0 then
          InsSpecItem.Expiry := s1
        else
          begin
            InsSpecItem.Expiry := Copy(s1, 1, i -1);
            Delete(s1, 1, i + 1);
            i := Pos(' ', s1);
            if i = 0 then
              InsSpecItem.LastTradeTime := s1
            else
              begin
                InsSpecItem.LastTradeTime := Copy(s1, 1, i -1);
                Delete(s1, 1, i + 1);
                InsSpecItem.TimeZoneID := s1;
              end;
          end;

        InsSpecItem.Strike := ReadFloat;
        s1 := ReadStream;
        InsSpecItem.Right := DecodeRight(s1);
        InsSpecItem.Exchange := ReadStream;
        InsSpecItem.Currency := ReadStream;
        InsSpecItem.LocalSymbol := ReadStream;
        InsSpecItem.MarketName := ReadStream;
        InsSpecItem.TradingClass := ReadStream;
        InsSpecItem.ContractId := ReadInt;
        InsSpecItem.MinimumTick := ReadFloat;

  	      // InsSpecItem.mdSizeMultiplier := ReadInt;  // dropped in API 10.10
        if (FServerVersion >= MIN_SERVER_VER_MD_SIZE_MULTIPLIER) and (FServerVersion < MIN_SERVER_VER_SIZE_RULES) then
          ReadInt;    // not used anymore
        InsSpecItem.Multiplier := ReadStream;          // multiplier
        if InsSpecItem.Multiplier = '' then InsSpecItem.Multiplier := '0';
        InsSpecItem.OrderTypes := [];
        s1 := ReadStream;     // orderTypes
        s1 := s1 + ',';
        while Length(s1) > 1 do
          begin
            s2 := Copy(s1,1,Pos(',',s1));
            if (s2 = 'STP LMT') or (s2 = 'STP_LMT') then s2 := 'STPLMT';
            Delete(s1,1,Pos(',',s1));
            for i := Ord(Low(TIABOrderType)) + 1 to Ord(High(TIABOrderType)) do
              if s2 = OrderTypeString[TIABOrderType(i)] + ',' then Include(InsSpecItem.OrderTypes,TIABOrderType(i));
          end;
        InsSpecItem.ValidExchanges := ReadStream;     // validExchanges

        InsSpecItem.PriceMagnifier := 0;
        InsSpecItem.UnderConId := 0;
        InsSpecItem.PriceMagnifier := ReadInt;  // PriceMagnifier
        if Version >= 4 then InsSpecItem.UnderConId := ReadInt;   // underConId
				if Version >= 5 then
          begin
            InsSpecItem.LongName := ReadStream;       // longName
            InsSpecItem.PrimaryExchange := ReadStream;  // primaryExchange
          end;

				if Version >= 6 then
          begin
            InsSpecItem.ContractMonth := ReadStream;      // contractMonth
            InsSpecItem.Industry := ReadStream;           // industry
            InsSpecItem.Category := ReadStream;           // category
            InsSpecItem.SubCategory := ReadStream;        // subcategory
            InsSpecItem.TimeZoneID := ReadStream;         // timeZoneId
            InsSpecItem.TradingHours := ReadStream;       // tradingHours
            InsSpecItem.LiquidHours := ReadStream;        // liquidHours
          end;

				if Version >= 8 then
          begin
            InsSpecItem.EVRule := ReadStream;
            InsSpecItem.EVMultiplier := ReadFloat;
          end;

				if Version >= 7 then
          begin
            if not ScanAhead(1) then Break;
            p1 := ReadInt;
            if (p1 > 0) then
              begin
                if not ScanAhead(p1 * 2) then Break;
                for i := 0 to p1 -1 do
                  begin
                    TagValue.Tag := ReadStream;
                    TagValue.Value := ReadStream;
                    AddInstrumentSpecItemTagValue(InsSpecItem.SecIdList, TagValue);
                  end;
              end;
          end;

        i := Integer(FServerVersion >= MIN_SERVER_VER_AGG_GROUP) +
            2 * Integer(FServerVersion >= MIN_SERVER_VER_UNDERLYING_INFO) +
            Integer(FServerVersion >= MIN_SERVER_VER_MARKET_RULES) +
            Integer(FServerVersion >= MIN_SERVER_VER_REAL_EXPIRATION_DATE) +
            Integer(FServerVersion >= MIN_SERVER_VER_STOCK_TYPE) +
            Integer((FServerVersion >= MIN_SERVER_VER_FRACTIONAL_SIZE_SUPPORT) and (FServerVersion < MIN_SERVER_VER_SIZE_RULES)) +
            3 * Integer((FServerVersion >= MIN_SERVER_VER_SIZE_RULES));


        if not ScanAhead(i) then Break;



	      if (FServerVersion >= MIN_SERVER_VER_AGG_GROUP) then // 121
		      InsSpecItem.AggGroup := ReadInt;
      	if (FServerVersion >= MIN_SERVER_VER_UNDERLYING_INFO) then  //122
          begin
        		InsSpecItem.UnderSymbol := ReadStream;
        		InsSpecItem.UnderSecType := ReadStream;
          end;
        if (FServerVersion >= MIN_SERVER_VER_MARKET_RULES) then  //126
    		  InsSpecItem.MarketRuleIds := ReadStream;

        if (FServerVersion >= MIN_SERVER_VER_REAL_EXPIRATION_DATE) then  //134

          InsSpecItem.RealExpirationDate := ReadStream;

	      if (FServerVersion >= MIN_SERVER_VER_STOCK_TYPE) then   // 152

      		InsSpecItem.DerivativeSecTypes := readStream;
	      if (FServerVersion >= MIN_SERVER_VER_FRACTIONAL_SIZE_SUPPORT) and (FServerVersion < MIN_SERVER_VER_SIZE_RULES) then   // 163 164
          s1 := ReadStream;  //  sizeMinTick   // not used anymore
	      if (FServerVersion >= MIN_SERVER_VER_SIZE_RULES) then        // 164
          begin
        		InsSpecItem.MinSize := ReadDecimal;
        		InsSpecItem.SizeIncrement := ReadDecimal;
        		InsSpecItem.SuggestedSizeIncrement := ReadDecimal;
          end;

        // ****  end of streamed data read, now put it into saved objects / classes  *****


        i := FInstrumentSpecs.Find(InsSpecItem);
        if i > -1 then FInstrumentSpecs.Items[i] := {$IFDEF BCBCOMPILE} @InsSpecItem {$ELSE} InsSpecItem {$ENDIF}
        else i := FInstrumentSpecs.Add(InsSpecItem);
        if Assigned(FOnInstrumentSpecDetails) then FOnInstrumentSpecDetails(Self,i);
        BOS := pa;
      end;

      BOND_CONTRACT_DATA:begin
        if not ScanAhead(1) then Break;
        if (FServerVersion < MIN_SERVER_VER_SIZE_RULES) then
          Version := ReadInt
        else
          Version := MaxInt;

        if ((FServerVersion >= MIN_SERVER_VER_SIZE_RULES) and not ScanAhead(33)) then Break
        else if ((FServerVersion >= MIN_SERVER_VER_MARKET_RULES) and not ScanAhead(30)) then Break
        else if ((FServerVersion >= MIN_SERVER_VER_AGG_GROUP) and not ScanAhead(29)) then Break
        else if (FServerVersion >= MIN_SERVER_VER_MD_SIZE_MULTIPLIER) and not ScanAhead(28) then Break
        else if (Version >= 4) and not ScanAhead(27) then Break
        else if (Version >= 3) and not ScanAhead(26) then Break
        else if not ScanAhead(21) then Break;

        BondSpecItem.DataID := -1;
        if (Version >= 3) then  // reqid
          BondSpecItem.DataID := ReadInt;
        BondSpecItem.Symbol := ReadStream;
        s1 := ReadStream;
        BondSpecItem.SecurityType := DecodeSecurityType(s1);
        BondSpecItem.Cusip := ReadStream;
        BondSpecItem.Coupon := ReadFloat;

        // BondSpecItem.Maturity := ReadStream;
        s1 := ReadStream;  // lastTradeDateOrContractMonth
        //  decodeLastTradeDate function
        i := Pos(' ', s1);
        BondSpecItem.LastTradeTime := '';
        BondSpecItem.TimeZoneID := '';
        if i = 0 then
          BondSpecItem.Maturity := s1
        else
          begin
            BondSpecItem.Maturity := Copy(s1, 1, i -1);
            Delete(s1, 1, i + 1);
            i := Pos(' ', s1);
            if i = 0 then
              BondSpecItem.LastTradeTime := s1
            else
              begin
                BondSpecItem.LastTradeTime := Copy(s1, 1, i -1);
                Delete(s1, 1, i + 1);
                BondSpecItem.TimeZoneID := s1;
              end;
          end;


        BondSpecItem.IssueDate := ReadStream;
        BondSpecItem.Ratings := ReadStream;
        BondSpecItem.BondType := ReadStream;
        BondSpecItem.CouponType := ReadStream;
        BondSpecItem.Convertible := ReadInt > 0;
        BondSpecItem.Callable := ReadInt > 0;
        BondSpecItem.Putable := ReadInt > 0;
        BondSpecItem.DescAppend := ReadStream;
        BondSpecItem.Exchange := ReadStream;
        BondSpecItem.Currency := ReadStream;
        BondSpecItem.MarketName := ReadStream;
        BondSpecItem.TradingClass := ReadStream;
        BondSpecItem.ContractId := ReadInt;
        BondSpecItem.MinimumTick := ReadFloat;

 	      if (FServerVersion >= MIN_SERVER_VER_MD_SIZE_MULTIPLIER) and (FServerVersion < MIN_SERVER_VER_SIZE_RULES) then
		      //BondSpecItem.MDSizeMultiplier := ReadInt;  // dropped in API 10.10
          ReadInt;     // not used anymore

        BondSpecItem.OrderTypes := [];
        s1 := ReadStream;
        s1 := s1 + ',';
        while Length(s1) > 1 do
          begin
            s2 := Copy(s1,1,Pos(',',s1));
            if (s2 = 'STP LMT') or (s2 = 'STP_LMT') then s2 := 'STPLMT';
            Delete(s1,1,Pos(',',s1));
            for i := Ord(Low(TIABOrderType)) + 1 to Ord(High(TIABOrderType)) do
              if s2 = OrderTypeString[TIABOrderType(i)] + ',' then Include(BondSpecItem.OrderTypes,TIABOrderType(i));
          end;
        BondSpecItem.ValidExchanges := ReadStream;
        BondSpecItem.NextOptionDate := ReadStream;
        BondSpecItem.NextOptionType := ReadStream;
        BondSpecItem.NextOptionPartial := ReadBool;
        BondSpecItem.Notes := ReadStream;
        if Version >= 4 then
          BondSpecItem.LongName := ReadStream;

				if Version >= 6 then
          begin
            if not ScanAhead(2) then Break;
            BondSpecItem.EVRule := ReadStream;
            BondSpecItem.EVMultiplier := ReadFloat;
          end;

				if Version >= 5 then
          begin
            if not ScanAhead(1) then Break;
            p1 := ReadInt;
            if (p1 > 0) then
              begin
                if not ScanAhead(p1 * 2) then Break;
                for i := 0 to p1 -1 do
                  begin
                    TagValue.Tag := ReadStream;
                    TagValue.Value := ReadStream;
                    AddInstrumentSpecItemTagValue(BondSpecItem.SecIdList, TagValue);
                  end;
              end;
          end;

        i := Integer(FServerVersion >= MIN_SERVER_VER_AGG_GROUP) +
             Integer(FServerVersion >= MIN_SERVER_VER_MARKET_RULES) +

             3 * Integer((FServerVersion >= MIN_SERVER_VER_SIZE_RULES));

        if not ScanAhead(i) then Break;


 	      if (FServerVersion >= MIN_SERVER_VER_AGG_GROUP) then // 121
		      BondSpecItem.AggGroup := ReadInt;
        if (FServerVersion >= MIN_SERVER_VER_MARKET_RULES) then  //126
    		  BondSpecItem.MarketRuleIds := ReadStream;

      	if (FServerVersion >= MIN_SERVER_VER_SIZE_RULES) then   // 164

          begin

            BondSpecItem.MinSize := ReadDecimal;

            BondSpecItem.SizeIncrement := ReadDecimal;

            BondSpecItem.SuggestedSizeIncrement := ReadDecimal;

          end;



        // ****  end of streamed data read, now put it into saved objects / classes  *****

        i := FBondSpecs.Find(BondSpecItem);
        if i > -1 then FBondSpecs.Items[i] := {$IFDEF BCBCOMPILE} @BondSpecItem {$ELSE} BondSpecItem {$ENDIF}
        else i := FBondSpecs.Add(BondSpecItem);
        if Assigned(FOnBondSpecDetails) then FOnBondSpecDetails(Self,i);
        BOS := pa;
      end;

      EXECUTION_DATA:begin
        if (FServerVersion < MIN_SERVER_VER_LAST_LIQUIDITY) then
          begin
            if not ScanAhead(1) then Break;
            Version := ReadInt;
          end
        else
          Version := FServerVersion;
        if Version >= 10 then i := 28
        else if Version >= 9 then i := 27
        else if Version >= 8 then i := 24
        else if Version >= 7 then i := 23
        else if Version >= 6 then i := 22
        else if Version >= 5 then i := 20
        else if Version >= 4 then i := 19
        else if Version >= 3 then i := 18
        else if Version >= 2 then i := 17
        else i := 16;
        if (FServerVersion >= MIN_SERVER_VER_MODELS_SUPPORT) then
          inc(i);
        if not ScanAhead(i) then Break;

        if Version >= 7 then ReadInt;
        // this above is the Request ID sent in the
        // GetExecutions data - not used here;

        p1 := ReadInt;  // tempId
        FExecuteState.FTempId := p1;
        FExecuteState.FContractId := ReadInt;  // conId
        FExecuteState.FSymbol := ReadStream; // symbol
        s1 := ReadStream;                  // sec type
        FExecuteState.FSecurityType := DecodeSecurityType(s1);
        FExecuteState.FExpiry := ReadStream; // expiry
        FExecuteState.FStrike := ReadFloat;  // strike
        s1 := ReadStream;                   // right
        FExecuteState.FRight := DecodeRight(s1);
				if Version >= 9 then FExecuteState.FMultiplier := ReadStream; // multiplier
        FExecuteState.FExchange := ReadStream;  // exchange
        FExecuteState.FCurrency := ReadStream;  // currency
        FExecuteState.FLocalSymbol := ReadStream;  // local
        if Version >= 10 then
          FExecuteState.FTradingClass := ReadStream;
        NewExec.ExecutionId := ReadStream;  // execid
        NewExec.Time := ReadStream;        // time
        NewExec.AcctNumber := ReadStream;  // acctNumber
        NewExec.Exchange := ReadStream;  // exchange
        s1 := ReadStream;              // side
        if (s1 = 'BOT') then NewExec.Side := iabBuy
        else if (s1 = 'SLD') then NewExec.Side := iabSell
        else NewExec.Side := iabidle;       // should not happen

        //if (FServerVersion >= MIN_SERVER_VER_FRACTIONAL_POSITIONS) then
          //NewExec.Volume := ReadFloat  // shares
        //else
          //NewExec.Volume := ReadInt;  // shares
        NewExec.Volume := ReadDecimal;   // shares

        NewExec.Price := ReadFloat;  // price
        NewExec.PermId := ReadInt; // permId
        p3 := NewExec.PermId;
        p2 := ReadInt;  // clientId
        FExecuteState.ClientId := p2;
        if (p2 = FClientId) and (p1 > FNextTempID) then
          FNextTempID := p1;
        NewExec.Liquidation := ReadInt;
        NewExec.CumulativeQty := 0;
        NewExec.AveragePrice := 0.0;
        if Version >= 6 then
          begin
            NewExec.CumulativeQty := ReadDecimal;//ReadInt;
            NewExec.AveragePrice := ReadFloat;
          end;

				if Version >= 8 then
          NewExec.OrderRef := ReadStream;

				if Version >= 9 then
          begin
            NewExec.EVRule := ReadStream;
            NewExec.EVMultiplier := ReadFloat;
          end;

        if (FServerVersion >= MIN_SERVER_VER_MODELS_SUPPORT) then  // 103
          NewExec.ModelCode := ReadStream;
        if (FServerVersion >= MIN_SERVER_VER_LAST_LIQUIDITY) then   // 136
           NewExec.LastLiquidity := ReadInt;


        // end of data reads.  Now assign to objects.


// ****  bug fix  ****
        if (p1 = 0) and (p2 = 0) then // nothing - required to exclude orders generated in TWS - not able to distiguish
        else begin                    // them and subsequent overwriting and corruption occurs.  Awaiting fix from IAB.
// ****  bug fix  ****

        i := FOrders.IndexOfPermId(p3);
        if i = -1 then i := FOrders.IndexOfTempId(p1,p2);
        if i = -1 then     // Make new order for this execution
          begin
            NewOrderState := TIABOrder.Create;
            NewOrderState.Assign(FExecuteState);
            NewOrderState.FAction := NewExec.Side;
            NewOrderState.FQuantity := NewExec.Volume;
            NewOrderState.FPrice := NewExec.Price;
            NewOrderState.FOrderType := otMarket;
            NewOrderState.FCompleted := true;
            NewOrderState.FFilled := NewExec.Volume;
            NewOrderState.FFillPrice := NewExec.Price;
            NewOrderState.FLatestFillQty := NewExec.Volume;
            NewOrderState.FLatestFillPrice := NewExec.Price;
            NewOrderState.FChanged := true;
            NewOrderState.FExternalOrder := true;
            i := FOrders.Add(p1,p2,p3,NewOrderState);
          end
        else if FOrders[i].FExternalOrder then
          with FOrders[i] do
            begin      // top up order with another execution
              if (FQuantity + NewExec.Volume) <> 0 then
              {$IFDEF USE_BIGDECIMAL}
                begin
                  dm1 := (FQuantity * FPrice + NewExec.Volume * NewExec.Price) / (FQuantity + NewExec.Volume);
                  FPrice := dm1.AsDouble;
                end;
              {$ELSE}
                FPrice := (FQuantity * FPrice + NewExec.Volume * NewExec.Price) / (FQuantity + NewExec.Volume);
              {$ENDIF}
              FFillPrice := FPrice;
              FLatestFillPrice := NewExec.Price;
              FQuantity := FQuantity + NewExec.Volume;
              FFilled := FQuantity;
              FLatestFillQty := NewExec.Volume;
            end;
        AttachExecToOrder(i,NewExec);
        if FRebuildFromTWS and Assigned(FOnRebuildFromTWS) then FOnRebuildFromTWS(Self)
        else if Assigned(FOnExecution) then FOnExecution(Self,FOrders[i]);

// ****  bug fix  ****
      end;
// ****  bug fix  ****

        BOS := pa;
      end;

      MARKET_DEPTH:begin
        if not ScanAhead(7) then Break;
        Version := ReadInt;
        p1 := ReadInt; // dataid
        p2 := ReadInt; // position
        p3 := ReadInt; // operation
        p4 := ReadInt; // side
        d := ReadFloat;  // price
        dm1 := ReadDecimal;//i := ReadInt; // size
        //if Assigned(FOnMarketDepth) then FOnMarketDepth(Self,p1,p2,p3,p4,i,d);
        if Assigned(FOnMarketDepth) then FOnMarketDepth(Self,p1,p2,p3,p4,dm1,d);
        BOS := pa;
      end;

      MARKET_DEPTH_L2:begin
        i := 8 + Integer(FServerVersion >= MIN_SERVER_VER_SMART_DEPTH);
        if not ScanAhead(i) then Break;
        Version := ReadInt;
        p1 := ReadInt; // dataid
        p2 := ReadInt; // position
        s1 := ReadStream;  // MM Id
        p3 := ReadInt; // operation
        p4 := ReadInt; // side
        d := ReadFloat;  // price
        dm1 := ReadDecimal;//i := ReadInt; // size
        b1 := false;
      	if (FServerVersion >= MIN_SERVER_VER_SMART_DEPTH) then
          b1 := ReadBool;
        //if Assigned(FOnMarketLevel2) then FOnMarketLevel2(Self,p1,p2,p3,p4,i,d,s1,b1);
        if Assigned(FOnMarketLevel2) then FOnMarketLevel2(Self,p1,p2,p3,p4,dm1,d,s1,b1);
        BOS := pa;
      end;

      NEWS_BULLETINS:begin
        if not ScanAhead(5) then Break;
        Version := ReadInt;
        p1 := ReadInt; // msgId
        p2 := ReadInt; // msgType
        s1 := ReadStream;  // newsMessage
        s2 := ReadStream;  // originatingExch
        case p2 of
          NEWS_MSG:if Assigned(FOnNewsBulletin) then FOnNewsBulletin(Self,p1,s1,s2);
          EXCHANGE_AVAIL_MSG,EXCHANGE_UNAVAIL_MSG:if Assigned(FOnExchangeStatus) then FOnExchangeStatus(Self,p1,TIABExchangeStatus(p2 -1),s1,s2);
        end;
        BOS := pa;
      end;

      MANAGED_ACCTS:begin
        if not ScanAhead(2) then Break;
        Version := ReadInt;
        s1 := ReadStream;  // accountsList
        if Assigned(FOnManagedAccounts) then FOnManagedAccounts(Self,s1);
        BOS := pa;
      end;

      RECEIVE_FA:begin
        if not ScanAhead(3) then Break;
        Version := ReadInt;
        p1 := ReadInt; // FAdatatype
        s1 := ReadStream;  // Xmlstring
        if Assigned(FOnReceiveFADetail) then FOnReceiveFADetail(Self,TIABFADataType( p1 - 1), s1);
        BOS := pa;
      end;

      HISTORICAL_DATA: begin
        if not ScanAhead(3) then Break;

        if (FServerVersion < MIN_SERVER_VER_SYNT_REALTIME_BARS) then //124
          Version := ReadInt
        else
          Version := MaxInt;
        p2 := ReadInt; // req ID
        s2 := 'finished';
        if not ScanAhead(2) then Break;
        s2 := s2 + '-' + ReadStream + '-' + ReadStream;  // startdate, enddate //  example: 'finished-20060406 12:19:16-20060406 12:24:16'
        p1 := ReadInt; // itemcount
        p3 := 9;
        if (FServerVersion >= MIN_SERVER_VER_SYNT_REALTIME_BARS) then // 124
          dec(p3);
        if (p1 > 0) then
          begin
            if not ScanAhead(p1 * p3) then Break;
            for i := 0 to p1 -1 do
              begin
                HistoricalChartData.Date := ReadStream;
                HistoricalChartData.Open := ReadFloat;
                HistoricalChartData.High := ReadFloat;
                HistoricalChartData.Low  := ReadFloat;
                HistoricalChartData.Close := ReadFloat;

                //if (FServerVersion < MIN_SERVER_VER_SYNT_REALTIME_BARS) then  // 124
                  //HistoricalChartData.Volume := ReadInt
                //else
                  //HistoricalChartData.Volume := ReadInt64;
                HistoricalChartData.Volume := ReadDecimal;
                HistoricalChartData.WAP := ReadDecimal;//ReadFloat;
                HistoricalChartData.HasGaps := false;
                if (FServerVersion < MIN_SERVER_VER_SYNT_REALTIME_BARS) then  // 124
                  begin
                    s1 := ReadStream;
                    HistoricalChartData.HasGaps := CompareText('true',s1) = 0;
                  end;
                HistoricalChartData.TradeCount := ReadInt;

                if Assigned(FOnHistoricalData) then FOnHistoricalData(Self,p2, i+1, p1, HistoricalChartData);
              end;
          end;
        // send end of dataset marker
        HistoricalChartData.Date := s2;  //  start/end time string
        HistoricalChartData.Open := -1;
        HistoricalChartData.High := -1;
        HistoricalChartData.Low  := -1;
        HistoricalChartData.Close := -1;
        HistoricalChartData.Volume := -1;
        HistoricalChartData.WAP := -1;
        HistoricalChartData.HasGaps := false;
        HistoricalChartData.TradeCount := -1;
        if Assigned(FOnHistoricalData) then FOnHistoricalData(Self,p2, -1, p1, HistoricalChartData);
        BOS := pa;
      end;

      HISTORICAL_DATA_UPDATE: begin
        if not ScanAhead(9) then Break;
        p2 := ReadInt; // req ID

        Fillchar(HistoricalChartData, sizeof(TIABHistoricalChartData), 0);

        HistoricalChartData.TradeCount := ReadInt;
        HistoricalChartData.Date := ReadStream;
        HistoricalChartData.Open := ReadFloat;
        HistoricalChartData.Close := ReadFloat;
        HistoricalChartData.High := ReadFloat;
        HistoricalChartData.Low  := ReadFloat;
        HistoricalChartData.WAP := ReadDecimal;//ReadFloat;
        HistoricalChartData.Volume := ReadDecimal;//ReadInt64;
        if Assigned(FOnHistoricalDataUpdate) then FOnHistoricalDataUpdate(Self,p2, HistoricalChartData);
        BOS := pa;
      end;

      SCANNER_DATA: begin
        if not ScanAhead(3) then Break;
        Version := ReadInt;
        p2 := ReadInt;  // ticker Id
        p1 := ReadInt;  // n of elements to follow
        if (p1 > 0) and (not ScanAhead(p1 * 16)) then Break;
        p3 := FScanner.IndexOfScanId(p2);   // test if destination exists and use it.
        if p3 > -1 then
          begin
           IABScan := FScanner.Items[p3];
           SetLength(IABScan.FScanItems,p1);
           for i := 0 to p1 -1 do
             with IABScan.FScanItems[i] do
               begin
                 Rank := ReadInt;
                 ContractId := ReadInt;
                 Symbol := ReadStream;
                 s1 := ReadStream;                  // sectype
                 SecurityType := DecodeSecurityType(s1);
                 Expiry := ReadStream;
                 Strike := ReadFloat;
                 s1 := ReadStream;                   // right
                 Right := DecodeRight(s1);
                 Exchange := ReadStream;
                 Currency := ReadStream;
                 LocalSymbol := ReadStream;
                 MarketName := ReadStream;
                 TradingClass := ReadStream;
                 Distance := ReadStream;
                 Benchmark := ReadStream;
                 Projection := ReadStream;
                 LegsString := ReadStream;
               end;
             if Assigned(FOnScannerData) then FOnScannerData(Self, IABScan);
          end
        else   //  scan object not found.. so read data off the feed and send to bitbucket.
          begin
           for i := 0 to p1 -1 do
             begin
               ReadInt;     // rank
               if Version >= 3 then
                 ReadInt;  // conid
               s1 := ReadStream;  // sym
               s1 := s1 + ReadStream;  // sectype
               s1 := s1 + ReadStream;  // exp
               ReadFloat;    // strike
               s1 := s1 + ReadStream;  // right
               s1 := s1 + ReadStream;  // exch
               s1 := s1 + ReadStream;  // curr
               s1 := s1 + ReadStream;  // local
               s1 := s1 + ReadStream;  // mark name
               s1 := s1 + ReadStream;  // tradecall
               s1 := s1 + ReadStream;  // distance
               s1 := s1 + ReadStream;  // benchmark
               s1 := s1 + ReadStream;  // projection
               s1 := s1 + ReadStream;  // legsstring
             end;
          end;
        BOS := pa;
      end;

      SCANNER_PARAMETERS: begin
        if not ScanAhead(2) then Break;
        Version := ReadInt;
        FScanner.FParameters := ReadStream;
        {$IFNDEF LINUX}
        if (Pos(#13#10,FScanner.FParameters) = 0) then
          for i := Length(FScanner.FParameters) downto 1 do
            if (FScanner.FParameters[i] = #10) then
              Insert(#13,FScanner.FParameters,i);
        {$ENDIF}
        if Assigned(FOnScannerParam) then FOnScannerParam(Self,FScanner.FParameters);
        BOS := pa;
      end;

      CURRENT_TIME: begin
        if not ScanAhead(2) then Break;
        Version := ReadInt;
        p2 := ReadInt;
        if Assigned(FOnCurrentTime) then FOnCurrentTime(Self, p2 / 86400 + 25569); // ctime in seconds to TDateTime
        BOS := pa;
      end;

      REAL_TIME_BARS: begin
        if not ScanAhead(10) then Break;
        Version := ReadInt;
        p2 := ReadInt;  //  TickerId
        RealTimeData.DateTime := ReadInt / 86400 + 25569; // ctime in seconds to TDateTime
        RealTimeData.Open := ReadFloat;
        RealTimeData.High := ReadFloat;
        RealTimeData.Low := ReadFloat;
        RealTimeData.Close := ReadFloat;
        RealTimeData.Volume := ReadDecimal;//ReadInt;
        RealTimeData.WAP := ReadDecimal;//ReadFloat;
        RealTimeData.TradeCount := ReadInt;
        if Assigned(FOnRealTimeData) then FOnRealTimeData(Self,p2,RealTimeData);
        BOS := pa;
      end;

      FUNDAMENTAL_DATA: begin
        if not ScanAhead(3) then Break;
        Version := ReadInt;
        p2 := ReadInt;  //  TickerId
        s1 := ReadStream;
        if Assigned(FOnFundamentalData) then FOnFundamentalData(Self,p2,s1);
        BOS := pa;
      end;

      CONTRACT_DATA_END: begin
        if not ScanAhead(2) then Break;
        Version := ReadInt;
        p2 := ReadInt;  //  TickerId
        if Assigned(FOnInstrumentSpecDetailsReady) then
          FOnInstrumentSpecDetailsReady(Self,p2);
        BOS := pa;
      end;

      OPEN_ORDER_END: begin
        if not ScanAhead(1) then Break;
        Version := ReadInt;
        if Assigned(FOnOpenOrderDetailsReady) then
          FOnOpenOrderDetailsReady(Self);
        BOS := pa;
      end;

      ACCT_DOWNLOAD_END: begin
        if not ScanAhead(2) then Break;
        Version := ReadInt;
        s1 := ReadStream;  //  account
        if Assigned(FOnAccountDetailsReady) then
          FOnAccountDetailsReady(Self,s1);
        BOS := pa;
      end;

      EXECUTION_DATA_END: begin
        if not ScanAhead(2) then Break;
        Version := ReadInt;
        p2 := ReadInt;  //  reqID
        if Assigned(FOnExecutionDetailsReady) then
          FOnExecutionDetailsReady(Self,p2);
        BOS := pa;
      end;

			DELTA_NEUTRAL_VALIDATION: begin
        if not ScanAhead(5) then Break;
        Version := ReadInt;
        p2 := ReadInt;  //  reqID
        DeltaNeutralContract.ConId := ReadInt;  // conID
        DeltaNeutralContract.Delta := ReadFloat;  // delta
        DeltaNeutralContract.Price := ReadFloat;  // price
        if Assigned(FOnDeltaNeutralValidation) then
          FOnDeltaNeutralValidation(Self, p2, DeltaNeutralContract);
        BOS := pa;
      end;

			TICK_SNAPSHOT_END: begin
        if not ScanAhead(2) then Break;
        Version := ReadInt;
        p2 := ReadInt;  //  reqID
        if Assigned(FOnSnapShotDataEnd) then
          FOnSnapShotDataEnd(Self, p2);
        BOS := pa;
      end;

			MARKET_DATA_TYPE: begin
        if not ScanAhead(3) then Break;
        Version := ReadInt;
        p2 := ReadInt;  //  reqID
        p1 := ReadInt;  // marketDataType
        if (p1 < 0) or (p1 >= Ord(mdtNone)) then p1 := 0;
        if Assigned(FOnMarketDataType) then
          FOnMarketDataType(Self, p2, TIABMarketDataType(p1));
        BOS := pa;
      end;

		  COMMISSION_REPORT: begin
        if not ScanAhead(7) then Break;
        Version := ReadInt;
        CommissionReport.ExecID := ReadStream;
        CommissionReport.Commission := ReadFloat;
        CommissionReport.Currency := ReadStream;
        CommissionReport.RealizedPNL := ReadFloat;
        CommissionReport.Yield := ReadFloat;
        CommissionReport.YieldRedemptionDate := ReadInt;
        if Assigned(FOnCommissionReport) then
          FOnCommissionReport(Self, @CommissionReport);
        BOS := pa;
      end;


      // the following call backs are ignored - not implemented
      // they apply to Account managers, unspecified, or background colors of the TWS.

    POSITION_DATA: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(13) then Break;
        Version := ReadInt;
        s1 := ReadStream;  //	DECODE_FIELD( account);
				s1 := s1 + ReadStream; // DECODE_FIELD( contract.conId);
				s1 := s1 + ReadStream; // DECODE_FIELD( contract.symbol);
				s1 := s1 + ReadStream; // DECODE_FIELD( contract.secType);
				s1 := s1 + ReadStream; // DECODE_FIELD( contract.expiry);
				s1 := s1 + ReadStream; // DECODE_FIELD( contract.strike);
				s1 := s1 + ReadStream; // DECODE_FIELD( contract.right);
				s1 := s1 + ReadStream; // DECODE_FIELD( contract.multiplier);
				s1 := s1 + ReadStream; // DECODE_FIELD( contract.exchange);
				s1 := s1 + ReadStream; // DECODE_FIELD( contract.currency);
				s1 := s1 + ReadStream; // DECODE_FIELD( contract.localSymbol);
				if Version >= 2 then
					s1 := s1 + ReadStream; // DECODE_FIELD( contract.tradingClass);
            //if (m_serverVersion >= MIN_SERVER_VER_FRACTIONAL_POSITIONS)
            
				s1 := s1 + ReadStream; // DECODE_FIELD( position);
				if Version >= 3 then
					s1 := s1 + ReadStream; // DECODE_FIELD( avgCost);
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;

    POSITION_END: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(1) then Break;
        Version := ReadInt;
        if Version = 12345 then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;

    ACCOUNT_SUMMARY: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(6) then Break;
        Version := ReadInt;
				s1 := ReadStream; // DECODE_FIELD( reqId);
				s1 := s1 + ReadStream; // DECODE_FIELD( account);
				s1 := s1 + ReadStream; // DECODE_FIELD( tag);
				s1 := s1 + ReadStream; // DECODE_FIELD( value);
				s1 := s1 + ReadStream; // DECODE_FIELD( curency);
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;

    ACCOUNT_SUMMARY_END: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(1) then Break;
        Version := ReadInt;
        if Version = 12345 then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;

    VERIFY_MESSAGE_API: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(2) then Break;
        Version := ReadInt;
        s1 := Readstream;
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;

    VERIFY_COMPLETED: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(3) then Break;
        Version := ReadInt;
				s1 := Readstream;  // DECODE_FIELD( isSuccessful);
        s1 := s1 + ReadStream; // DECODE_FIELD( errorText);
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;

    DISPLAY_GROUP_LIST: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(3) then Break;
        Version := ReadInt;
				s1 := Readstream;  // DECODE_FIELD( reqId);
        s1 := s1 + ReadStream; // DECODE_FIELD( groups);
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;

    DISPLAY_GROUP_UPDATED: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(3) then Break;
        Version := ReadInt;
				s1 := Readstream;  // DECODE_FIELD( reqId);
        s1 := s1 + ReadStream; // DECODE_FIELD( contractInfo);
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;

    VERIFY_AND_AUTH_MESSAGE_API: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(3) then Break;
        Version := ReadInt;
				s1 := Readstream;  // DECODE_FIELD( apiData);
        s1 := s1 + ReadStream; // DECODE_FIELD( xyzChallenge);
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;

    VERIFY_AND_AUTH_COMPLETED: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(3) then Break;
        Version := ReadInt;
				s1 := Readstream;  // DECODE_FIELD( isSuccessful));
        s1 := s1 + ReadStream; // DECODE_FIELD( errorText);
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;
    POSITION_MULTI: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(17) then Break;
        Version := ReadInt;
        s1 := '';
        for i := 1 to 16 do
          s1 := s1 + ReadStream;
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;
    POSITION_MULTI_END: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(2) then Break;
        Version := ReadInt;
				s1 := Readstream;  // DECODE_FIELD( reqId);
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;
    ACCOUNT_UPDATE_MULTI: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(7) then Break;
        Version := ReadInt;
        s1 := '';
        for i := 1 to 6 do
          s1 := s1 + ReadStream;
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;
    ACCOUNT_UPDATE_MULTI_END: begin
      // ***  These callbacks not implemented: ***
        if not ScanAhead(2) then Break;
        Version := ReadInt;
				s1 := Readstream;  // DECODE_FIELD( reqId);
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;

    SECURITY_DEFINITION_OPTION_PARAMETER: begin
        if not ScanAhead(6) then Break;
        // Version := ReadInt;  //No more

        p1 := ReadInt; //  reqID
      	s1 := ReadStream; // exchange
    	  p2 := ReadInt; //  underlyingConId);
	      s2 := ReadStream; // tradingClass);
	      s3 := ReadStream; // multiplier);
	      p3 := ReadInt; //  expirationsSize);
        s4 := '';
        if (p3 > 0) and (not ScanAhead(p3)) then Break;
        for i := 0 to p3 -1 do
          s4 := s4 + ReadStream + ';';
        if not ScanAhead(1) then Break;
        p4 := ReadInt; //  StrikeSize);
        s5 := '';
        if (p4 > 0) and (not ScanAhead(p4)) then Break;
        for i := 0 to p4 -1 do
          s5 := s5 + ReadStream + ';';
        if Assigned(FOnSecurityDefinitionOptionalParameter) then
          FOnSecurityDefinitionOptionalParameter(Self, p1, s1, p2, s2, s3, s4, s5);
        BOS := pa;
      end;
    SECURITY_DEFINITION_OPTION_PARAMETER_END: begin
        if not ScanAhead(1) then Break;
        // Version := ReadInt;
        p2 := ReadInt;  //  reqID
        if Assigned(FOnSecurityDefinitionOptionalParameterEnd) then
          FOnSecurityDefinitionOptionalParameterEnd(Self, p2);
        BOS := pa;
      end;
    SOFT_DOLLAR_TIERS: begin
        if not ScanAhead(2) then Break;
        // Version := ReadInt;
        p2 := ReadInt;  //  reqID
        p3 := ReadInt;  //  count tiers
        if p3 > 0 then
          begin
            if not ScanAhead(p3 * 3) then Break;
            for i := 0 to p3 do
              begin
                s1 := ReadStream;
                s2 := ReadStream;
                s3 := ReadStream;
                if Assigned(FOnSoftDollarTiers) then
                  FOnSoftDollarTiers(Self, p2, i + 1, p3, s1, s2, s3);
              end;
          end;
        BOS := pa;
      end;


		  FAMILY_CODES: begin

      // ***  These callbacks not implemented: ***
        if not ScanAhead(1) then Break;
        p1 := ReadInt;  // nFamilyCodes
        s1 := '';
        if (p1 > 0) and not ScanAhead(p1 * 2) then Break;
        for i := 0 to p1 -1 do
          s1 := s1 + ReadStream + ReadStream;
        if s1 = 'qwertyuiop' then Break; //  to force all junk code above to compile.
        BOS := pa;
      end;

		SYMBOL_SAMPLES: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt;  //reqId
        p2 := ReadInt;   //nContractDescriptions
        for i := 0 to p2 -1 do
          begin
             if not ScanAhead(6) then Break;
             SymbolDerivItem.ContractId := ReadInt;   //
             SymbolDerivItem.Symbol := ReadStream;    //
             s1 := ReadStream;                        //
             SymbolDerivItem.SecurityType := DecodeSecurityType(s1);
             SymbolDerivItem.PrimaryExchange := ReadStream;   //
             SymbolDerivItem.Currency := ReadStream;        //
             SymbolDerivItem.DerivativeSecCount := ReadInt;  //nDerivativeSecTypes
             SymbolDerivItem.DerivativeSecTypes := '';
             if not ScanAhead(SymbolDerivItem.DerivativeSecCount) then Break;
             for j := 0 to SymbolDerivItem.DerivativeSecCount -1 do
               SymbolDerivItem.DerivativeSecTypes := SymbolDerivItem.DerivativeSecTypes + Trim(ReadStream) + ' ';
             if Assigned(FOnSymbolSample) then
               FOnSymbolSample(Self, p1, i + 1, p2, SymbolDerivItem);
          end;
        BOS := pa;
      end;
      MKT_DEPTH_EXCHANGES: begin
        if not ScanAhead(2) then Break;
        //p1 := ReadInt;  //reqId error - dropped 10.11.2

        p2 := ReadInt;   //nContractDescriptions
        p3 := p2 * 3;
        if (FServerVersion >= MIN_SERVER_VER_SERVICE_DATA_TYPE) then // 120
          p3 := p2 * 5;
        if (p2 > 0) and (not ScanAhead(p3)) then Break;
        for i := 0 to p2 -1 do
          begin
            if (FServerVersion >= MIN_SERVER_VER_SERVICE_DATA_TYPE) then // 120

              begin
                DepthMarketDataDescripItem.Exchange := ReadStream;  //
                s1 := ReadStream;                        //
                DepthMarketDataDescripItem.SecurityType := DecodeSecurityType(s1);
                DepthMarketDataDescripItem.ListingExchange := ReadStream; //
                DepthMarketDataDescripItem.ServiceDataType := ReadStream; //
                DepthMarketDataDescripItem.AggGroup := ReadInt;  //
              end
            else
              begin
                DepthMarketDataDescripItem.Exchange := ReadStream;  //
                s1 := ReadStream;                        //
                DepthMarketDataDescripItem.SecurityType := DecodeSecurityType(s1);
                if ReadBool then  //
                  DepthMarketDataDescripItem.ServiceDataType := 'Deep2'
                else
                  DepthMarketDataDescripItem.ServiceDataType := 'Deep';
                DepthMarketDataDescripItem.ListingExchange := '';
                DepthMarketDataDescripItem.AggGroup := 0;
              end;
            if Assigned(FOnDepthMarketDataDescripItem) then
              FOnDepthMarketDataDescripItem(Self, i + 1, p2, DepthMarketDataDescripItem);
          end;
        BOS := pa;
      end;
      TICK_NEWS: begin
        if not ScanAhead(5) then Break;
        p1 := ReadInt;  //tickerId
        time64t := ReadInt64;   //timeStamp
        s1 := ReadStream;    //   providerCode);
        s2 := ReadStream;    //   articleId);
        s3 := ReadStream;    //    headline);
        s4 := ReadStream;    //   extraData);
        if Assigned(FOnNewsTick) then
          FOnNewsTick(Self, p1, time64t / 86400 + 25569, s1, s2, s3, s4);
        BOS := pa;
      end;
		  NEWS_PROVIDERS: begin
        if not ScanAhead(1) then Break;
        p1 := ReadInt;  // nNewsProviders
        if (p1 > 0) then
          begin
            if not ScanAhead(p1 * 2) then Break;
            for i := 0 to p1 -1 do
              begin
                s1 := ReadStream;    //   providerCode);
                s2 := ReadStream;    //   providerName);
                if Assigned(FOnNewsProvider) then
                  FOnNewsProvider(Self, i + 1, p1, s1, s2);
              end;
          end;
        BOS := pa;
      end;
		  NEWS_ARTICLE: begin
        if not ScanAhead(3) then Break;
        p1 := ReadInt;  //reqId
        p2 := ReadInt;    //  articleType);
        s1 := ReadStream;    // articleText);
        if Assigned(FOnNewsArticle) then
          FOnNewsArticle(Self, p1, p2, s1);
        BOS := pa;
      end;
		 HISTORICAL_NEWS: begin
        if not ScanAhead(5) then Break;
        p1 := ReadInt;  //reqId
        s1 := ReadStream;  // time);
        s2 := ReadStream;    // providerCode);
        s3 := ReadStream;    // articleId);
        s4 := ReadStream;    // headline);
        if Assigned(FOnHistoricalNews) then
          FOnHistoricalNews(Self, p1, s1, s2, s3, s4);
        BOS := pa;
      end;
     HISTORICAL_NEWS_END: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt;  //reqId
        b1 := ReadBool;  // hasMore);
        if Assigned(FOnHistoricalNewsEnd) then
          FOnHistoricalNewsEnd(Self, p1, b1);
        BOS := pa;
      end;
		  MARKET_RULE: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt;  //marketRuleId);
        p2 := ReadInt;  //nPriceIncrements);
        if (p2 > 0) then
          begin
            if not ScanAhead(p2 * 2) then Break;
            for i := 0 to p2 -1 do
              begin
                d := ReadFloat;    //   lowEdge);
                d2 := ReadFloat;    //   increment);
                if Assigned(FOnMarketRule) then
                  FOnMarketRule(Self, p1, i + 1, p2, d, d2);
              end;
          end;
        BOS := pa;
      end;
 		  SMART_COMPONENTS: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt;  //reqId
        p2 := ReadInt;  // n
        if (p2 > 0) then
          begin
            if not ScanAhead(p2 * 3) then Break;
            for i := 0 to p2 -1 do
              begin
                p3 := ReadInt;  // bitNumber);
                s1 := ReadStream;    // exchange);
                s2 := ReadStream;    // exchangeLetter);
                if Assigned(FOnSmartComponent) then
                  FOnSmartComponent(Self, p1, i + 1, p2, p3, s1, s2);
              end;
          end;
        BOS := pa;
      end;
		  TICK_REQ_PARAMS: begin
        if not ScanAhead(4) then Break;
        p1 := ReadInt;  //tickerId
        d := ReadFloat;  // minTick);
        s1 := ReadStream;    // bboExchange);
        p2 := ReadInt;    // snapshotPermissions);
        if Assigned(FOnTickReqParams) then
          FOnTickReqParams(Self, p1, d, s1, p2);
        BOS := pa;
      end;
		  HEAD_TIMESTAMP: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt;  //reqId
        s1 := ReadStream;  // headTimestamp);
        if Assigned(FOnHeadTimestamp) then
          FOnHeadTimestamp(Self, p1, s1);
        BOS := pa;
      end;
      HISTOGRAM_DATA: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt;   //reqId
        p2 := ReadInt;  // n
        if (p2 > 0) then
          begin
            if not ScanAhead(p2 * 2) then Break;
            for i := 0 to p2 -1 do
              begin
                d := ReadFloat;    //   price);
                dm1 := ReadDecimal;//pp1 := ReadInt64;    //  size);
                if Assigned(FOnHistogramData) then
                  //FOnHistogramData(Self, p1, i + 1, p2, d, pp1);
                  FOnHistogramData(Self, p1, i + 1, p2, d, dm1);
              end;
          end;
        BOS := pa;
      end;
		  REROUTE_MKT_DATA_REQ: begin
        if not ScanAhead(3) then Break;
        p1 := ReadInt;  //reqId
        p2 := ReadInt;  //conId);
        s1 := ReadStream;  //exchange);
        if Assigned(FOnRerouteMktDataReq) then
          FOnRerouteMktDataReq(Self, p1, p2, s1);
        BOS := pa;
      end;
		  REROUTE_MKT_DEPTH_REQ: begin
        if not ScanAhead(3) then Break;
        p1 := ReadInt;  //reqId
        p2 := ReadInt;  //conId);
        s1 := ReadStream;  //exchange);
        if Assigned(FOnRerouteMktDepthReq) then
          FOnRerouteMktDepthReq(Self, p1, p2, s1);
        BOS := pa;
      end;
      PNL: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt;  //reqId
        d := ReadFloat;  //dailyPnL);
        d2 := UNSET_DOUBLE;//0;
        if (FServerVersion >= MIN_SERVER_VER_UNREALIZED_PNL)  then // 129
          d2 := ReadFloat;  //unrealizedPnL)
        d3 := UNSET_DOUBLE;//0;
        if (FServerVersion >= MIN_SERVER_VER_REALIZED_PNL)  then // 135
          d3 := ReadFloat;  //realizedPnL)
        if Assigned(FOnProfitLoss) then
          FOnProfitLoss(Self, p1, d, d2, d3);
        BOS := pa;
      end;
      PNL_SINGLE: begin
        if not ScanAhead(4) then Break;
        p1 := ReadInt;  //reqId
        dm1 := ReadDecimal;// p2 := ReadInt;  //pos
        d := ReadFloat;  //dailyPnL);
        d2 := UNSET_DOUBLE;//0;
        if (FServerVersion >= MIN_SERVER_VER_UNREALIZED_PNL)  then // 129
          d2 := ReadFloat;  //unrealizedPnL)
        d3 := ReadFloat;  //value
        d4 := UNSET_DOUBLE;//0;
        if (FServerVersion >= MIN_SERVER_VER_REALIZED_PNL)  then // 135
          d4 := ReadFloat;  //realizedPnL)
        if Assigned(FOnProfitLossSingle) then
          //FOnProfitLossSingle(Self, p1, p2, d, d2, d4, d3);
          FOnProfitLossSingle(Self, p1, dm1, d, d2, d4, d3);
        BOS := pa;
      end;
      HISTORICAL_TICKS,

      HISTORICAL_TICKS_BID_ASK,
      HISTORICAL_TICKS_LAST: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt;  // dataid
        p2 := ReadInt;  // tickcount

        case MsgID of
          HISTORICAL_TICKS:  begin
             TickData.TickType := tdMidPoint;
             if not ScanAhead(4 * p2 + 1) then Break;
          end;
          HISTORICAL_TICKS_BID_ASK: begin
            TickData.TickType := tdBidAsk;
             if not ScanAhead(6 * p2 + 1) then Break;
          end;
          HISTORICAL_TICKS_LAST: begin
            TickData.TickType := tdLast;
             if not ScanAhead(6 * p2 + 1) then Break;
          end;
        end;


        for i := 0 to p2 -1 do
          begin
            case MsgID of
              HISTORICAL_TICKS: begin
                pp1 := ReadInt64; // ctime
                TickData.Time := 25569 + pp1 / 86400;     //UnixToDateTime(pp1);
                ReadInt; //unused, padding field.
                TickData.Price := ReadFloat;
                TickData.Size := ReadDecimal;//ReadInt;
              end;
              HISTORICAL_TICKS_BID_ASK: begin
                pp1 := ReadInt64; // ctime
                TickData.Time := 25569 + pp1 / 86400;     //UnixToDateTime(pp1);
                AttrMask := ReadInt;
                TickData.AskPastHigh := AttrMask and $01 > 0;
                TickData.BidPastLow := AttrMask and $02 > 0;
                TickData.BidPrice := ReadFloat;
                TickData.AskPrice := ReadFloat;
                TickData.BidSize := ReadDecimal;//ReadInt;
                TickData.AskSize := ReadDecimal;//ReadInt;
              end;
              HISTORICAL_TICKS_LAST: begin
                pp1 := ReadInt64; // ctime
                TickData.Time := 25569 + pp1 / 86400;     //UnixToDateTime(pp1);
                AttrMask := ReadInt;
                TickData.PastLimit := AttrMask and $01 > 0;
                TickData.Unreported := AttrMask and $02 > 0;
                TickData.Price := ReadFloat;
                TickData.Size := ReadDecimal;//ReadInt;
                TickData.Exchange := ReadStream;
                TickData.SpecialConditions := ReadStream;
              end;
            end;
            if Assigned(FOnHistoricalTickData) then
              FOnHistoricalTickData(Self, p1, TickData);
          end;
        ReadInt;  // done
        BOS := pa;
      end;
      TICK_BY_TICK: begin

        if not ScanAhead(3) then Break;
        p1 := ReadInt;  // dataid
        p2 := ReadInt;  // tickType

        TickData.TickType := TIABTickDataType(p2);  // tickType
        pp1 := ReadInt64; // ctime
        TickData.Time := 25569 + pp1 / 86400;     //UnixToDateTime(pp1);
        case TickData.TickType of                      // Last/AllLast
          tdLast, tdAllLast:
            begin
              if not ScanAhead(5) then Break;
              TickData.Price := ReadFloat; // price;
              TickData.Size := ReadDecimal;//ReadInt;  // size
              AttrMask := ReadInt;  // attributes bits
              TickData.PastLimit := AttrMask and $01 > 0;
              TickData.Unreported := AttrMask and $02 > 0;
              TickData.Exchange := ReadStream;
              TickData.SpecialConditions := ReadStream;
            end;
          tdBidAsk:                                  // BidAsk
            begin
              if not ScanAhead(5) then Break;
              TickData.BidPrice := ReadFloat;
              TickData.AskPrice := ReadFloat;
              TickData.BidSize := ReadDecimal;//ReadInt;
              TickData.AskSize := ReadDecimal;//ReadInt;
              AttrMask := ReadInt;
              TickData.BidPastLow := Attrmask and $01 > 0;
              TickData.AskPastHigh := Attrmask and $02 > 0;
            end;
          tdMidPoint:                                // MidPoint
            begin
              if not ScanAhead(1) then Break;
              TickData.MidPoint := ReadFloat;
            end;
        end;
        if Assigned(FOnTickByTickData) then
          FOnTickByTickData(Self, p1, TickData);
        BOS := pa;
      end;
      ORDER_BOUND: begin

        if not ScanAhead(3) then Break;

        //pp1 := ReadInt64;  // orderId

        //p1 := ReadInt;     // apiClientId

        //p2 := ReadInt;    //  apiOrderId


        //  this message not implemented in the API.  Just read off data.

        ReadInt64;  // orderId

        ReadInt;     // apiClientId

        ReadInt;    //  apiOrderId

        BOS := pa;

      end;
      COMPLETED_ORDER: begin


        // This event not to be relied upon.  Instead use the GetExecutions.


        if not ScanAhead(56) then Break;


        Version := MaxInt;


        FOrderState.Free;

        FOrderState := TIABOrder.Create;



        // Contract portion     //decodeContract

        FOrderState.FContractId := ReadInt; // conId
        FOrderState.FSymbol := ReadStream;  // symbol
        s1 := ReadStream;                  // sectype
        FOrderState.FSecurityType := DecodeSecurityType(s1);
        FOrderState.FExpiry := ReadStream;  // expiry   // lastTradeDateOrContractMonth
        FOrderState.FStrike := ReadFloat;  // strike
        s1 := ReadStream;                   // right
        FOrderState.FRight := DecodeRight(s1);
        if Version >= 32 then
          FOrderState.FMultiplier := ReadStream;  //multiplier
        FOrderState.FExchange := ReadStream;     //  exch
        FOrderState.FCurrency := ReadStream;    // currency
        FOrderState.FLocalSymbol := ReadStream;  // local
        if Version >= 32 then
          FOrderState.FTradingClass := ReadStream;   // tradingClass
        // end Contract portion



        s1 := ReadStream;                  // side
        FOrderState.FAction := ActionStringType(s1);
        FOrderState.FQuantity := ReadDecimal;          // totalQuantity
        s1 := ReadStream;                           // order type
        FOrderState.FOrderType := DecodeOrderType(s1);
				if Version < 29 then
					FOrderState.FPrice := ReadFloat        //LmtPrice
				else
					FOrderState.FPrice := ReadFloatMax;    //LmtPrice
				if Version < 30 then
					FOrderState.FAuxPrice := ReadFloat        //AuxPrice
				else
					FOrderState.FAuxPrice := ReadFloatMax;     //AuxPrice
        s1 := ReadStream;                           // tif
        FOrderState.FTimeInForce := DecodeTIF(s1);

        FOrderState.FOCAgroup := ReadStream;      // oca
        FOrderState.FAccount := ReadStream;       //  account
        s1 := ReadStream;                           // openclose
        if Length(s1) > 0 then
          FOrderState.FOpenClose := s1[1];
        FOrderState.FOrderOrigin := TIABOrderOrigin(ReadInt);   // origin
        FOrderState.FOrderRef := ReadStream;          // order ref

        FOrderState.PermId := ReadInt;   //permid
        FOrderState.ExtendedHours := ReadBool; //ReadInt > 0;    //rth
        FOrderState.Hidden := ReadBool; //ReadInt > 0;           //hidden
        FOrderState.DiscretAmount := ReadFloat;          // DiscretAmount
        FOrderState.GoodAfterTime := ReadStream;        //goodafter

        //decodeFAParams
        FOrderState.FAdvGroup := ReadStream;   // faGroup
        FOrderState.FAdvMethod := ReadStream;   // faMethod
        FOrderState.FAdvPercentage := ReadStream;    // faPercentage
        FOrderState.FAdvProfile := ReadStream;        // faProfile

        if (FServerVersion >= MIN_SERVER_VER_MODELS_SUPPORT ) then // 103
          FOrderState.ModelCode := ReadStream;                        // ModelCode

        FOrderState.FGoodTillDate := ReadStream;                     //  GoodTillDate
        s1 := ReadStream;                                       // rule 80
        FOrderState.FRule80A := DecodeRule80A(s1);

        FOrderState.FPercentOffset := ReadFloatMax;      //  PercentOffset
        FOrderState.FSettlingFirm := ReadStream;          // SettlingFirm

        // decodeShortSaleParams
        FOrderState.FShortSaleSlot := ReadInt;            // shortSaleParam
        FOrderState.FDesignatedLocation := ReadStream;    // designatedLocation
        if ((FServerVersion = MIN_SERVER_VER_SSHORTX_OLD) or (Version >= 23)) and not ScanAhead(1) then Break;
			  if FServerVersion = MIN_SERVER_VER_SSHORTX_OLD then      // 51
			    FOrderState.FExemptCode := ReadInt       // exemptCode
    	 	else if Version >= 23 then
          FOrderState.FExemptCode := ReadInt;       // exemptCode

        // decodeBoxOrderParams
        FOrderState.FStartingPrice := ReadFloatMax;      // startingPrice
        FOrderState.FStockRefPrice := ReadFloatMax;      // stockRefPrice
        FOrderState.FDelta := ReadFloatMax;              // delta

        // decodePegToStkOrVolOrderParams
        FOrderState.FStockRangeLower := ReadFloatMax;       // stockRangeLower
        FOrderState.FStockRangeUpper := ReadFloatMax;       // stockRangeUpper

        FOrderState.FDisplaySize := ReadIntMax;          // displaySize
        FOrderState.FSweepToFill := ReadBool;            // sweepToFill
        FOrderState.FAllOrNone := ReadBool;              // allOrNone
        FOrderState.FMinQuantity := ReadIntMax;          // minQty
        i := ReadInt -1;                                // ocaType
        if i in [0..Ord(High(TIABOcaMethod))] then
          FOrderState.FOcaMethod := TIABOcaMethod(i);

        FOrderState.FTriggerMethod := ReadInt;      //triggerMethod  // 0=Default, 1=Double_Bid_Ask, 2=Last, 3=Double_Last, 4=Bid_Ask, 7=Last_or_Bid_Ask, 8=Mid-point

        // decodeVolOrderParams
        FOrderState.FVolatility := ReadFloatMax;          //volatility
        i := ReadInt;                                     //volatilityType
        if TIABVolatilityPeriod(i) in [low(TIABVolatilityPeriod)..High(TIABVolatilityPeriod)] then
          FOrderState.FVolatilityPeriod := TIABVolatilityPeriod(i) else FOrderState.FVolatilityPeriod := vpUnSet;
        s1 := ReadStream;    // DeltaNeutralOrderType
        FOrderState.FDeltaNeutralOrderType := DecodeOrderType(s1);

        FOrderState.FDeltaNeutralAuxPrice := ReadFloatMax;    // deltaNeutralAuxPrice
        if (Version >= 27) and (FOrderState.DeltaNeutralOrderType > otNoChange) then
          begin
            if not ScanAhead(4) then Break;
    	      FOrderState.FDeltaNeutralConId := ReadInt;
            if false then                 // decodeOpenOrderAttribs = set to FALSE always.
              begin
     			      FOrderState.FDeltaNeutralSettlingFirm := ReadStream;        // deltaNeutralSettlingFirm
		    	      FOrderState.FDeltaNeutralClearingAccount := ReadStream;     // deltaNeutralClearingAccount
			          FOrderState.FDeltaNeutralClearingIntent := ReadStream;      // deltaNeutralClearingIntent
              end;
          end;
        if (Version >= 31) and (FOrderState.DeltaNeutralOrderType > otNoChange) then
          begin
            if not ScanAhead(4) then Break;
            if false then                // decodeOpenOrderAttribs = set to FALSE always.
            	FOrderState.FDeltaNeutralOpenClose := ReadStream;             // deltaNeutralOpenClose
    				FOrderState.FDeltaNeutralShortSale := ReadBool;                 // deltaNeutralShortSale
			      FOrderState.FDeltaNeutralShortSaleSlot := ReadInt;              // deltaNeutralShortSaleSlot
			      FOrderState.FDeltaNeutralDesignatedLocation := ReadStream;      // deltaNeutralDesignatedLocation
          end;
        if not ScanAhead(1) then Break;
        FOrderState.FContinuousUpdate := ReadBool;          // continuousUpdate
        if not ScanAhead(6) then Break;
        i := ReadInt;                                       // referencePriceType
        if TIABReferencePrice(i) in [low(TIABReferencePrice)..High(TIABReferencePrice)] then
          FOrderState.FReferencePrice := TIABReferencePrice(i) else FOrderState.FReferencePrice := rpUnSet;
        // end decodeVolOrderParams

        // decodeTrailParams
        FOrderState.FTrailStopPrice := ReadFloatMax;             // trailStopPrice
        if Version >= 30 then
					FOrderState.FTrailingPercent := ReadFloatMax;          // trailingPercent

        // decodeComboLegs
        FOrderState.FComboLegsDescrip := ReadStream;             // comboLegsDescrip
				if Version >= 29 then
          begin
            FOrderState.FComboLegs := nil;
            if not ScanAhead(1) then Break;
            p3 := ReadInt;                                  // ComboLegsCount
					  if p3 > 0  then
              begin
                if not ScanAhead(p3 * 8) then Break;
                for i := 0 to p3 -1 do
                  begin
                    ComboLeg.ContractId := ReadInt;               // conID
                    ComboLeg.Ratio := ReadInt;                    // ratio
                    s1 := ReadStream;                             // action
                    ComboLeg.Action := ActionStringType(s1);
                    ComboLeg.Exchange := ReadStream;              // exchange
                    p3 := ReadInt;                                // openClose
                    ComboLeg.OpenClose := TIABLegOpenClose(p3);
                    ComboLeg.ShortSaleSlot := ReadInt;            // shortSaleSlot
                    ComboLeg.DesignatedLocation := ReadStream;    // designatedLocation
                    ComboLeg.ExemptCode := ReadInt;               // exemptCode
                    FOrderState.AddComboLeg(ComboLeg);
                  end;
              end;
            FOrderState.FComboLegPrice := nil;
            if not ScanAhead(1) then Break;
            p3 := ReadInt;                              // OrderComboLegsCount
					  if p3 > 0  then
              begin
                if not ScanAhead(p3) then Break;
                for i := 0 to p3 -1 do
                  begin
                    d := ReadFloatMax;                      // price
                    FOrderState.AddComboLegPrice(d);
                  end;
              end;
          end;
        // end decodeComboLegs

        // decodeSmartComboRoutingParams
			  if Version >= 26 then
          begin
            FOrderState.FSmartComboRoutingParams := nil;
            if not ScanAhead(1) then Break;
            p3 := ReadInt;          // smartComboRoutingParamsCount
					  if p3 > 0  then
              begin
                if not ScanAhead(p3 * 2) then Break;
                for i := 0 to p3 -1 do
                  begin
                    TagValue.Tag := ReadStream;           // tag
                    TagValue.Value := ReadStream;         // value
                    FOrderState.AddSmartComboRoutingParams(TagValue);
                  end;
              end;
          end;

        // decodeScaleOrderParams
        if not ScanAhead(3) then Break;
        if Version >= 20 then
          begin
            FOrderState.FScaleInitLevelSize := ReadIntMax;  // scaleInitLevelSize
            FOrderState.FScaleSubsLevelSize := ReadIntMax;  // scaleSubsLevelSize
          end
        else
          begin
            ReadInt; // trash it        // scaleInitLevelSize
            FOrderState.FScaleInitLevelSize := UNSET_INTEGER;  // scaleInitLevelSize
            FOrderState.FScaleSubsLevelSize := ReadIntMax;
          end;
        FOrderState.FScalePriceIncrement := ReadFloatMax;     //   scalePriceIncrement
				if (Version >= 28) and (FOrderState.FScalePriceIncrement > 0.0) and (FOrderState.FScalePriceIncrement < UNSET_DOUBLE_TEST_VALUE) then  //60
          begin
            if not ScanAhead(7) then Break;
					  FOrderState.FScalePriceAdjustValue := ReadFloatMax;      // scalePriceAdjustValue
					  FOrderState.FScalePriceAdjustInterval := ReadIntMax;     // scalePriceAdjustInterval
					  FOrderState.FScaleProfitOffset := ReadFloatMax;          // scaleProfitOffset
					  FOrderState.FScaleAutoReset := ReadBool;                 // scaleAutoReset
					  FOrderState.FScaleInitPosition := ReadIntMax;            // scaleInitPosition
					  FOrderState.FScaleInitFillQty := ReadIntMax;             // scaleInitFillQty
					  FOrderState.FScaleRandomPercent := ReadBool;             // scaleRandomPercent
          end;
        // end decodeScaleOrderParams

        // decodeHedgeParams
        if not ScanAhead(3) then Break;
				if Version >= 24 then
          begin
            FOrderState.FHedgeType := HedgeTypeStrType(ReadStream);   // hedgeType   // 'D' - delta, 'B' - beta, 'F' - FX, 'P' - pair
            if FOrderState.FHedgeType > htUnset then
			    	  FOrderState.FHedgeParam := ReadStream;                  // hedgeParam // 'beta=X' value for beta hedge, 'ratio=Y' for pair hedge
          end;

        // decodeClearingParams
        if not ScanAhead(3) then Break;
        FOrderState.FClearingAccount := ReadStream;     // clearingAccount
        FOrderState.FClearingIntent := ReadStream;      // clearingIntent

        if Version >= 22 then
            FOrderState.FNotHeld := ReadBool;           // notHeld

        // decodeDeltaNeutral
        if Version >= 20 then
          begin
            if not ScanAhead(1) then Break;
            FOrderState.FDeltaNeutralContractId := 0;
            FOrderState.FDeltaNeutralContractDelta := 0;
            FOrderState.FDeltaNeutralContractPrice := 0.0;
            if ReadBool then                     // deltaNeutralContractPresent
              begin
                if not ScanAhead(3) then Break;
                FOrderState.FDeltaNeutralContractId := ReadInt;         // deltaNeutralContract.conId
                FOrderState.FDeltaNeutralContractDelta := ReadFloat;    // deltaNeutralContract.delta
                FOrderState.FDeltaNeutralContractPrice := ReadFloat;    // deltaNeutralContract.price
              end;
          end;

        // decodeAlgoParams
        if Version >= 21 then
          begin
            if not ScanAhead(1) then Break;
            FOrderState.FAlgoParams := nil;
            FOrderState.FAlgoStrategy := ReadStream;       // algoStrategy
            if Length(FOrderState.FAlgoStrategy) > 0 then
              begin
                if not ScanAhead(1) then Break;
                p3 := ReadInt;                    // algoParamsCount
                if p3 > 0 then
                  begin
                    if not ScanAhead(p3 * 2) then Break;
                    for i := 0 to p3 -1 do
                      begin
                        TagValue.Tag := ReadStream;           // tag
                        TagValue.Value := ReadStream;         // value
                        FOrderState.AddAlgoParams(TagValue);
                      end;
                  end;
              end;
          end;

        if not ScanAhead(2) then Break;
        if (Version >= 33) then
          FOrderState.Solicited := ReadBool;         // solicited

        // decodeOrderStatus
        ReadStream;  // status - not used in our API

        // decodeVolRandomizeFlags
        if (Version >= 34) then
          begin
            if not ScanAhead(2) then Break;
            FOrderState.RandomizeSize := ReadBool;
            FOrderState.RandomizePrice := ReadBool;
          end;
        // decodePegBenchParams
	      if (FServerVersion >= MIN_SERVER_VER_PEGGED_TO_BENCHMARK) then  // 102
          begin
            if (FOrderState.OrderType = otPegBench) then
              begin
                if not ScanAhead(5) then Break;
		    	      FOrderState.ReferenceContractId := ReadInt;
			          FOrderState.IsPeggedChangeAmountDecrease := ReadBool;
    			      FOrderState.PeggedChangeAmount := ReadFloat;
		    	      FOrderState.ReferenceChangeAmount := ReadFloat;
			          FOrderState.ReferenceExchangeId := ReadStream;
              end;
          end;
        // decodeConditions
	      if (FServerVersion >= MIN_SERVER_VER_PEGGED_TO_BENCHMARK) then  // 102
          begin
            if not ScanAhead(1) then Break;
            p3 := ReadInt;  //conditionsSize;
		        if (p3 > 0) then
              begin
                // conditions not used, but need to remove data
                if not ScanAhead(p3 + 2) then Break;
                s1 := '';
                for i := 0 to p3 -1 do
                  s1 := s1 + ReadStream;   // conditionType
                s1 := s1 + ReadStream;     // conditionsIgnoreRth
                s1 := s1 + ReadStream;     // conditionsCancelOrder
              end;
          end;

        i :=  Integer(FServerVersion >= MIN_SERVER_VER_CASH_QTY) +
              Integer(FServerVersion >= MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE) +
              Integer(FServerVersion >= MIN_SERVER_VER_ORDER_CONTAINER) +
              12;
        if not ScanAhead(i) then Break;

         // decodeStopPriceAndLmtPriceOffset
        FOrderState.FTrailStopPrice := ReadFloat;             // trailStopPrice
				FOrderState.FLmtPriceOffset := ReadFloat;          // lmtPriceOffset

        // decodeCashQty
      	if (FServerVersion >= MIN_SERVER_VER_CASH_QTY) then  // 111

            FOrderState.CashQuantity := ReadFloat;


        // decodeDontUseAutoPriceForHedge

 	      if FServerVersion >= MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE then // 141

      		FOrderState.FDontUseAutoPriceForHedge := ReadBool;


        // decodeIsOmsContainer

      	if FServerVersion >= MIN_SERVER_VER_ORDER_CONTAINER then  // 145

          FOrderState.FIsOmsContainer := ReadBool;


        // decodeAutoCancelDate

        FOrderState.FAutoCancelDate := ReadStream;


        // decodeFilledQuantity

        FOrderState.FFilledQuantity := ReadDecimal;


        //decodeRefFuturesConId

        FOrderState.FRefFuturesConID := ReadInt;


        // decodeAutoCancelParent

        if (FServerVersion >= MIN_CLIENT_VER) then

          FOrderState.FAutoCancelParent := ReadBool;


        // decodeShareholder

        FOrderState.FShareHolder := ReadStream;


        // decodeImbalanceOnly

        FOrderState.FImbalanceOnly := ReadBool;


        // decodeRouteMarketableToBbo

        FOrderState.FRouteMarketableToBbo := ReadBool;


        // decodeParentPermId

        FOrderState.FParentPermId := ReadInt64;


        // decodeCompletedTime

        FOrderState.FCompletedTime := ReadStream;


        // decodeCompletedStatus

        FOrderState.FCompletedStatus := ReadStream;


        // decodePegBestPegMidOrderAttributes

        if (FServerVersion >= MIN_SERVER_VER_PEGBEST_PEGMID_OFFSETS) then

          begin

            FOrderState.MinTradeQty := ReadInt;
            FOrderState.MinCompeteSize := ReadInt;
            FOrderState.CompeteAgainstBestOffset := ReadFloat;
            FOrderState.MidOffsetAtWhole := ReadFloat;
            FOrderState.MidOffsetAtHalf := ReadFloat;

          end;



        // ****  end of streamed data read, now put it into saved objects / classes  *****



        i := FOrders.IndexOfPermId(FOrderState.PermId);

        if (i = -1) then

          begin
            NewOrderState := TIABOrder.Create;
            NewOrderState.Assign(FOrderState);
            i := FOrders.Add(0, FClientId,0,NewOrderState);
          end
        else
          FOrders[i].Assign(FOrderState);

        if Assigned(FOnCompletedOrders) then

          FOnCompletedOrders(Self, FOrders[i]);


        BOS := pa;

      end;
      COMPLETED_ORDERS_END: begin
        if Assigned(FOnCompletedOrdersEnd) then
            FOnCompletedOrdersEnd(Self);
        BOS := pa;
      end;
		  REPLACE_FA_END: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt;  // reqID
        s1 := ReadStream;  // text
        if Assigned(OnReplaceFAEnd) then
          OnReplaceFAEnd(Self, p1, s1);
        BOS := pa;
      end;

    	WSH_META_DATA: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt; // reqID
        s1 := ReadStream; // dataJson
        if Assigned(OnWsHorizonMeta) then
          OnWsHorizonMeta(Self, p1, s1);
        BOS := pa;
      end;

    	WSH_EVENT_DATA: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt; // reqID
        s1 := ReadStream; // dataJson
        if Assigned(OnWsHorizonEvents) then
          OnWsHorizonEvents(Self, p1, s1);
        BOS := pa;
      end;

		  HISTORICAL_SCHEDULE: begin
        if not ScanAhead(5) then Break;
        p1 := ReadInt; // reqID
        s1 := ReadStream; // startDateTime
        s2 := ReadStream; // endDateTime
        s3 := ReadStream; // timeZone
        p2 := ReadInt; // sessionsCount
        if not ScanAhead(p2 * 3) then Break;
        for i := 0 to p2 -1 do
          begin
            HistoricalSession.StartDateTime := ReadStream;
            HistoricalSession.EndDateTime := ReadStream;
            HistoricalSession.RefDate := ReadStream;
            if Assigned(FOnHistoricalSession) then
              FOnHistoricalSession(Self, p1, i + 1, p2, s1, s2, s3, HistoricalSession);
          end;
        BOS := pa;
      end;

      USER_INFO: begin
        if not ScanAhead(2) then Break;
        p1 := ReadInt; // reqID
        s1 := ReadStream; // whiteBrandingId
        if Assigned(FOnUserInfo) then
          FOnUserInfo(Self, p1, s1);
        BOS := pa;
      end;


      // end of known msg's

      else // in case
        begin
          DoErrorEvent(MsgId, UNKNOWN_ID);
          BOS := EOS;
        end;
    end; // case


    if (BOS = EOS) or (Version = -1) then Break;
  until false;                  // loop indefinately until forced break test above

  if (BOS = FInStream.Memory) then
    begin
      Result := false;
      Exit;            // nothing read sucessfully, wait for remainder of chunk
    end;
  Result := true;
  if (BOS = EOS) then FInStream.Position := 0      // complete read:  zero stream to start
  else if (BOS > pAnsiChar(FInStream.Memory)) then     // move unread chunk to front, wait for remainder of chunk
    begin
      p1 := EOS - BOS;
      Move(BOS^,FInStream.Memory^,p1);
      FInStream.Position := p1;
    end;

  {$IFDEF CAPTURE_TWS_STREAM}
    if (BOS = EOS) then
      CopyFile('CrntStreamIn.txt','LastStreamIn.txt',false);
  {$ENDIF}
end;

destructor TIABSocket.Destroy;
begin
  if FSocket.Active or (FSocketThread <> nil) then
    begin
      FOnConnectionState := nil;
      FOnError := nil;
      FSocketThread.Terminate;
      FSocket.Disconnect;
      FSocketThread.WaitFor;
      FSocketThread.Free;
    end;
{$IFNDEF LINUX}
  if FMutex <> 0 then CloseHandle(FMutex);
{$ENDIF}
  FScanner.Free;
  FBondSpecs.Free;
  FOrderState.Free;
  FExecuteState.Free;
  FInstrumentSpecs.Free;
  FPortfolio.Free;
  FAccountValues.Free;
  FVerifiedOrders.Free;
  FOrders.Free;
  FInStream.Free;
  FOutStream.Free;
  FDefaultOrder.Free;
  FSocket.Free;
  inherited Destroy;
end;

procedure TIABSocket.DoErrorEvent(TempId: Integer; Error: TIABCodeMsgPair; Additional: string = '');
begin
  if Assigned(FOnError) then FOnError(Self,TempId, Error.Code, Error.Msg + Additional);
end;

{$IFNDEF LINUX}
function TIABSocket.GainSocketMutex: Boolean;
begin
  case WaitForSingleObject(FMutex,5000) of
    WAIT_FAILED: Result := false;
    WAIT_OBJECT_0: Result := true;
    WAIT_ABANDONED:
      case WaitForSingleObject(FMutex,5000) of
        WAIT_FAILED: Result := false;
        WAIT_OBJECT_0: Result := true;
      else Result := false;
    end;
  else Result := false;
  end;
end;
{$ENDIF}

procedure TIABSocket.EncodeContract(Contract: TIABContract; SkipIncExp: Boolean);
begin
	AddToOut(Contract.ContractId);
  AddToOut(Contract.Symbol);
  AddToOut(SecurityTypeString[Contract.SecurityType]);
  AddToOut(Contract.LastTradeDateOrContractMonth);
  AddToOut(Contract.Strike);
  AddToOut(RightString[Contract.Right]);
  AddToOut(Contract.Multiplier); // FServerVersion 15
  AddToOut(Contract.Exchange);
  AddToOut(Contract.PrimaryExchange); // FServerVersion 14
  AddToOut(Contract.Currency);
  AddToOut(Contract.LocalSymbol); // FServerVersion 2
  AddToOut(Contract.TradingClass);
  if SkipIncExp then
    Exit;
	AddToOut(Contract.IncludeExpired);
end;

procedure TIABSocket.EncodeTagValueList(TagValue: TIABTagValueArray);
var s: string; i: Integer;
begin
  s := '';
  for i := 0 to High(TagValue) do
    s := s + TagValue[i].Tag + '=' + TagValue[i].Value + ';';
  AddToOut(s);
end;

function TIABSocket.ExerciseOptions(Order: TIABOrder; OverridePos: Boolean): Integer;
const VERSION = 2;
var AddOrder: TIABOrder;
begin      // if Order = nil then the default is used
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Result := -1;
      Exit;
    end;
  inc(FNextTempID);
  if Order = nil then Order := FDefaultOrder;
  if not (Order.Action in [iabExercise,iabLapse]) then
    begin
      DoErrorEvent(NO_VALID_ID, INVALID_INPUT_DATA);
      Result := -1;
      Exit;
    end;
  AddToOut(EXERCISE_OPTIONS);
  AddToOut(VERSION);
  AddToOut(FNextTempID);
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then //  68
    AddToOut(Order.ContractId);
  AddToOut(Order.Symbol);
  AddToOut(SecurityTypeString[Order.SecurityType]);
  AddToOut(Order.Expiry);
  AddToOut(Order.Strike);
  AddToOut(RightString[Order.Right]);
  AddToOut(Order.Multiplier);
  AddToOut(Order.Exchange);
  AddToOut(Order.Currency);
  AddToOut(Order.LocalSymbol);
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then //  68
    AddToOut(Order.TradingClass);
  if (Order.Action = iabExercise) then AddToOut(1)
  else if (Order.Action = iabLapse) then AddToOut(2);
  AddToOut(Order.Quantity);
  AddToOut(Order.Account);
  AddToOut(OverridePos);
  SendToSocket(FNextTempId);
  AddOrder := TIABOrder.Create;
  AddOrder.Assign(Order);
  FOrders.Add(FNextTempID,FClientId,0,AddOrder);
  Result := FNextTempID;
end;

procedure TIABSocket.GetAccountUpdates(AccountCode: string);
const VERSION = 2;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(REQ_ACCT_DATA);
  AddToOut(VERSION);
  AddToOut( 1 );  // 1 = start 0 = stop
  AddToOut(AccountCode);   // FServerVersion 9
  SendToSocket(NO_VALID_ID);
end;

function TIABSocket.GetConnected: Boolean;
begin
  if (csDesigning in Self.ComponentState) then Result := FConnected
  else Result := FSocket.Active;
end;

procedure TIABSocket.GetCurrentTWSTime;
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(REQ_CURRENT_TIME);
  AddToOut(VERSION);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.GetInstrumentSpecs(DataId: Integer; Order: TIABOrder; IncludeExpired: Boolean = false);
const VERSION = 8;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
                        // 45
	if (FServerVersion < MIN_SERVER_VER_SEC_ID_TYPE) and ((Order.FSecurityID <> '') or (Order.SecurityIDType <> '')) then
    begin
      DoErrorEvent(DataId, UPDATE_TWS, ' secIdType and secId parameters not supported.');
      Exit;
    end;

  AddToOut(REQ_CONTRACT_DATA);
  AddToOut(VERSION);
  with Order do
    begin
      if FServerVersion >= MIN_SERVER_VER_CONTRACT_DATA_CHAIN then  // 40
  	    AddToOut(DataId);
      if FServerVersion >= MIN_SERVER_VER_CONTRACT_CONID then  // 37
  	  AddToOut(ContractId);
      AddToOut(Symbol);
      AddToOut(SecurityTypeString[SecurityType]);
      AddToOut(Expiry);
      AddToOut(Strike);
      AddToOut(RightString[Right]);
      AddToOut(Multiplier);   // FServerVersion 15

	    if FServerVersion >= MIN_SERVER_VER_PRIMARYEXCH then   // 75
        begin
          AddToOut(Exchange);
          AddToOut(PrimaryExchange);
        end
    	else if FServerVersion >= MIN_SERVER_VER_LINKING then     //70
        begin
      		if (Length(PrimaryExchange) > 0) and ((Exchange = 'BEST') or (Exchange = 'SMART')) then
            AddToOut(Exchange + ':' + PrimaryExchange)
          else
            AddToOut(Exchange);
        end;
      AddToOut(Currency);
      AddToOut(LocalSymbol);
      if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then // 68
        AddToOut(TradingClass);
      AddToOut(IncludeExpired);  // FServerVersion 31
      if FServerVersion >= MIN_SERVER_VER_SEC_ID_TYPE then  // 45
        begin
          AddToOut(Order.FSecurityIDType);
          AddToOut(Order.FSecurityID);
        end;
    end;

  SendToSocket(NO_VALID_ID);
end;

var GetExecutionID: Integer = 0;

function TIABSocket.GetExecutions(Filter: TIABExecutionFilter): Integer;
const VERSION = 3;
begin
  Result := -1;
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  FAcceptOpenOrdersWindow := GetTickCount + OPEN_ORDER_ACCEPT_WINDOW;
  AddToOut(REQ_EXECUTIONS);
  AddToOut(VERSION);
  if FServerVersion >= MIN_SERVER_VER_EXECUTION_DATA_CHAIN then
    begin
      inc(GetExecutionID);
      AddToOut(GetExecutionID);
      Result := GetExecutionID;
      // the above is a unique ID field, this will return in the the
      //  OnExecutionDataReady event
    end;
    // Time format must be 'yyyymmdd-hh:mm:ss' E.g. '20030702-14:55'
  // FServerVersion 9
  AddToOut(Filter.ClientId);
  AddToOut(Filter.AccountCode);
  AddToOut(FormatDateTime('yyyymmdd"-"hh":"nn":"ss',Filter.FromTime));
  AddToOut(Filter.Symbol);
  AddToOut(SecurityTypeString[Filter.SecurityType]);
  AddToOut(Filter.Exchange);
  AddToOut(ActionString[Filter.Action]);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.GetFundamentalData(DataId: Integer; Order: TIABOrder; ReportType: string);
const VERSION = 2;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_FUNDAMENTAL_DATA) then
    begin
      DoErrorEvent(DataId, UPDATE_TWS, ' Fundamental data not supported.');
      Exit;
    end;

  AddToOut(REQ_FUNDAMENTAL_DATA);
  AddToOut(VERSION);
  AddToOut(DataId);
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then //68
    AddToOut(Order.ContractId);
  AddToOut(Order.Symbol);
  AddToOut(SecurityTypeString[Order.SecurityType]);
  AddToOut(Order.Exchange);
  AddToOut(Order.PrimaryExchange);
  AddToOut(Order.Currency);
  AddToOut(Order.LocalSymbol);
  AddToOut(ReportType);
  if FServerVersion >= MIN_SERVER_VER_LINKING then // 70    <<  MISTAKE somewhere.  probably should be  140+ //
    AddToOut(''); // //reserved for future use, must be blank
  SendToSocket(DataId);
end;

procedure TIABSocket.GetHistoricalData(DataId: Integer; Symbol, Local, Exchange, Expiry, Currency, PrimaryExchange, Multiplier: string;
                                       SecurityType: TIABSecurityType; Right: TIABRight; Strike: Double;
                                       DataEndDateTime: string; DataDuration, DurationTimeUnits: Integer; BarSize: TIABChartBarSize;
                                       DataBasis: TIABHistoricalDataType; ExtendedHours, IncludeExpired, KeepUpdated: Boolean; DateFormat: Integer;
                                       ContractId: Integer; TradingClass: string; ChartOptions: TIABTagValueArray);
const VERSION = 6;
var i: Integer; stv: string;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;

  if (FServerVersion < MIN_SERVER_VER_TRADING_CLASS) then
    begin
      if (TradingClass <> '') or (ContractId > 0) then
        begin
           DoErrorEvent(DataId, UPDATE_TWS, ' It does not support conId and tradingClass parameters in reqHistoricalData.');
           Exit;
        end;
    end;
    if (FServerVersion < MIN_SERVER_VER_HISTORICAL_SCHEDULE) then
      begin
        if DataBasis = cdSchedule then
          begin
            DoErrorEvent(DataId, UPDATE_TWS, ' It does not support requesting of historical schedule.');
            Exit;
          end;
      end;

  AddToOut(REQ_HISTORICAL_DATA);
	if (FServerVersion < MIN_SERVER_VER_SYNT_REALTIME_BARS) then  // 124
    AddToOut(VERSION);
  AddToOut(DataId);
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then // 68
    AddToOut(ContractId);
  AddToOut(Symbol);
  AddToOut(SecurityTypeString[SecurityType]);
  AddToOut(Expiry);
  AddToOut(Strike);
  AddToOut(RightString[Right]);
  AddToOut(Multiplier);
  AddToOut(Exchange);
  AddToOut(PrimaryExchange);
  AddToOut(Currency);
  AddToOut(Local);
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then // 68
    AddToOut(TradingClass);
  AddToOut(IncludeExpired);  // FServerVersion 31

  AddToOut(DataEndDateTime);
  AddToOut(ChartBarSizeString[BarSize]);
  case DurationTimeUnits of
    IAB_TIME_UNIT_YEAR: AddToOut(IntToStr(DataDuration) + ' Y');
    IAB_TIME_UNIT_MONTH: AddToOut(IntToStr(DataDuration) + ' M');
    IAB_TIME_UNIT_WEEK: AddToOut(IntToStr(DataDuration) + ' W');
    IAB_TIME_UNIT_DAY: AddToOut(IntToStr(DataDuration) + ' D');
    else AddToOut(IntToStr(DataDuration) + ' S');
  end;
  AddToOut(not ExtendedHours);
  AddToOut(HistoricalDataTypeString[DataBasis]);
  AddToOut(DateFormat);  // FServerVersion 16
  if SecurityType = stBag then
    AddToOut(0); // sets combo leg count to zero; if legs req'd, add more code here.

	if (FServerVersion >= MIN_SERVER_VER_SYNT_REALTIME_BARS) then // 124
    AddToOut(KeepUpdated);

  if FServerVersion >= MIN_SERVER_VER_LINKING then  //  70
    begin
      stv := '';
      for i := 0 to High(ChartOptions) do
        stv := stv + ChartOptions[i].Tag + '=' + ChartOptions[i].Value + ';';
      AddToOut(stv);
    end;
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.GetHistoricalTicks(DataId: Integer; Contract: TIABContract; StartDateTime, EndDateTime, WhatToShow: string;
                                        NumberOfTicks, UseRTH: Integer; IgnoreSize: Boolean; MiscOptions: TIABTagValueArray);
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_HISTORICAL_TICKS) then  // 130
    begin
      DoErrorEvent(DataId, UPDATE_TWS, ' historical ticks request not supported.');
      Exit;
    end;

  AddToOut(REQ_HISTORICAL_TICKS);
  AddToOut(DataId);
  EncodeContract(Contract, false);
  AddToOut(StartDateTime);
  AddToOut(EndDateTime);
  AddToOut(NumberOfTicks);
  AddToOut(WhatToShow);
  AddToOut(UseRth);
  AddToOut(IgnoreSize);
  EncodeTagValueList(MiscOptions);
  SendToSocket(DataId);
end;

procedure TIABSocket.GetImpliedVolatility(DataID: Integer; Order: TIABOrder; OptionPrice, UnderPrice: Double);
const VERSION = 2;
begin

  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_REQ_CALC_IMPLIED_VOLAT) then
    begin
      DoErrorEvent(DataId, UPDATE_TWS, ' implied volatility requests not supported.');
      Exit;
    end;

  AddToOut(REQ_CALC_IMPLIED_VOLAT);
  AddToOut(VERSION);
  AddToOut(DataId);
  AddToOut(Order.ContractID);
  AddToOut(Order.Symbol);
  AddToOut(SecurityTypeString[Order.SecurityType]);
  AddToOut(Order.Expiry);
  AddToOut(Order.Strike);
  AddToOut(RightString[Order.Right]);
  AddToOut(Order.Multiplier);
  AddToOut(Order.Exchange);
  AddToOut(Order.PrimaryExchange);
  AddToOut(Order.Currency);
  AddToOut(Order.LocalSymbol);
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then // 68
    AddToOut(Order.TradingClass);
  AddToOut(OptionPrice);
  AddToOut(UnderPrice);
  if FServerVersion < MIN_SERVER_VER_LINKING then // 70
    AddToOut('');  //reserved for future use, must be blank  // miscOptions
  SendToSocket(DataId);
end;

procedure TIABSocket.GetManagedAccts;
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(REQ_MANAGED_ACCTS);
  AddToOut(VERSION);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.GetMarketData(DataId: Integer; Symbol, Local, Exchange, Expiry, Currency, PrimaryExchange: string;
                       SecurityType: TIABSecurityType; Right: TIABRight; Strike: Double; ComboLegs: TIABComboLegArray;
                       TradingClass: string = '';
                       ExMarketData: TIABExMktDataSet = []; Multiplier: string = '');
var Order: TIABOrder;
begin
  Order := TIABOrder.Create;
  try
    Order.Symbol := Symbol;
    Order.LocalSymbol := Local;
    Order.Exchange := Exchange;
    Order.Expiry := Expiry;
    Order.Currency := Currency;
    Order.PrimaryExchange := PrimaryExchange;
    Order.SecurityType := SecurityType;
    Order.Right := Right;
    Order.Strike := Strike;
    Order.Multiplier := Multiplier;
    Order.TradingClass := TradingClass;
    if High(ComboLegs) > -1 then
      Order.FComboLegs := Copy(ComboLegs);
    GetMarketData(DataId, Order, ExMarketData);
  finally
    Order.Free;
  end;
end;

procedure TIABSocket.GetMarketData(DataId: Integer; Order: TIABOrder; ExMarketData: TIABExMktDataSet = []);
var i: Integer; s: string;
begin
  s := '';
  for i := Ord(Low(TIABExMktData)) to Ord(High(TIABExMktData)) do
    if TIABExMktData(i) in ExMarketData then
      s := s + IntToStr(ExMktDataAsInt[TIABExMktData(i)]) + ',';
  GetMarketDataGeneric(DataId, Order, s, false);
end;

procedure TIABSocket.GetMarketData(DataId: Integer; Order: TIABOrder; ExMarketDataString: string);
begin
  GetMarketDataGeneric(DataId, Order, ExMarketDataString, false);
end;

procedure TIABSocket.GetMarketDataGeneric(DataId: Integer; Order: TIABOrder; ExMarketDataString: string; SnapShot: Boolean);
const VERSION = 11;
var i: Integer; stv: string;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_SNAPSHOT_MKT_DATA) and SnapShot then
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' Snapshot data not supported.');
      Exit;
    end;

  if Order = nil then Order := FDefaultOrder;

  if (FServerVersion < MIN_SERVER_VER_UNDER_COMP) and ((Order.DeltaNeutralContractId > 0) or (Order.DeltaNeutralContractDelta <> 0.0) or (Order.DeltaNeutralContractPrice <> 0.0)) then
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' Combo Delta-neutral not supported.');
      Exit;
    end;

  if (FServerVersion < MIN_SERVER_VER_REQ_MKT_DATA_CONID) and (Order.ContractId > 0) then
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' ConId not supported.');
      Exit;
    end;

  if (FServerVersion < MIN_SERVER_VER_TRADING_CLASS) and (Order.TradingClass <> '') then
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' TradingClass not supported.');
      Exit;
    end;

  AddToOut(REQ_MKT_DATA);
  AddToOut(VERSION);
  AddToOut(DataId);
 	if FServerVersion >= MIN_SERVER_VER_REQ_MKT_DATA_CONID then   // 47
    AddToOut(Order.ContractId);
  AddToOut(Order.Symbol);
  AddToOut(SecurityTypeString[Order.SecurityType]);
  AddToOut(Order.Expiry);
  AddToOut(Order.Strike);
  AddToOut(RightString[Order.Right]);
  AddToOut(Order.Multiplier);    // FServerVersion 14
  AddToOut(Order.Exchange);
  AddToOut(Order.PrimaryExchange);    // FServerVersion 14
  AddToOut(Order.Currency);
  AddToOut(Order.LocalSymbol);  // FServerVersion 2

  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then   // 68
    AddToOut(Order.TradingClass);

  if (Order.SecurityType = stBag) then
    begin
      AddToOut(Order.ComboLegsCount);
      for i := 0 to Order.ComboLegsCount -1 do
        begin
          AddToOut(Order.ComboLegs[i].ContractId);
          AddToOut(Order.ComboLegs[i].Ratio);
          AddToOut(ActionString[Order.ComboLegs[i].Action]);
          AddToOut(Order.ComboLegs[i].Exchange);
        end;
    end;
  if (FServerVersion >= MIN_SERVER_VER_UNDER_COMP) then  // 40
    begin
      if ((Order.DeltaNeutralContractId > 0) or (Order.DeltaNeutralContractDelta <> 0.0) or (Order.DeltaNeutralContractPrice <> 0.0)) then
        begin
          AddToOut( 1 );  // false or true
          AddToOut(Order.DeltaNeutralContractId);
          AddToOut(Order.DeltaNeutralContractDelta);
          AddToOut(Order.DeltaNeutralContractPrice);
        end
      else
        AddToOut( 0 );  // false or true
    end;
  AddToOut(ExMarketDataString);  // FServerVersion 31
  if (FServerVersion >= MIN_SERVER_VER_SNAPSHOT_MKT_DATA) then  // 35
      AddToOut(SnapShot);


  // see: http://interactivebrokers.github.io/tws-api/md_request.html#regulatory_snapshot
  //  Important: Each regulatory snapshot made will incur a fee of 0.01 USD to the account. This applies to both live and paper accounts..
	if (FServerVersion >= MIN_SERVER_VER_REQ_SMART_COMPONENTS) then  //  114
		AddToOut( 0 );  // false or true

    
  if FServerVersion >= MIN_SERVER_VER_LINKING then  //  70
    begin
      stv := '';
      for i := 0 to Order.MktDataOptionsCount -1 do
        stv := stv + Order.MktDataOptions[i].Tag + '=' + Order.MktDataOptions[i].Value + ';';
      AddToOut(stv);
    end;

  SendToSocket(DataId);
end;

procedure TIABSocket.GetMarketSnapShot(DataId: Integer; Symbol, Local, Exchange, Expiry, Currency, PrimaryExchange: string;
                       SecurityType: TIABSecurityType; Right: TIABRight; Strike: Double);
var Order: TIABOrder;
begin
  Order := TIABOrder.Create;
  try
    Order.Symbol := Symbol;
    Order.LocalSymbol := Local;
    Order.Exchange := Exchange;
    Order.Expiry := Expiry;
    Order.Currency := Currency;
    Order.PrimaryExchange := PrimaryExchange;
    Order.SecurityType := SecurityType;
    Order.Right := Right;
    Order.Strike := Strike;
    GetMarketDataGeneric(DataId, Order, '', true);
  finally
    Order.Free;
  end;
end;

procedure TIABSocket.GetMarketDepth(DataId: Integer; Symbol, Local, Exchange, PrimaryExchange, Expiry, Currency, TradingClass, MktDataOptions, Multiplier: string;
          SecurityType: TIABSecurityType; Right: TIABRight; Strike: Double; nRows, ConId: Integer);
const VERSION = 5;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_TRADING_CLASS) and (TradingClass <> '') then
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' TradingClass not supported.');
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_SMART_DEPTH) and (FSmartDepthMarketData) then
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' SMART depth request not supported.');
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_MKT_DEPTH_PRIM_EXCHANGE) and (PrimaryExchange <> '') then
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' PrimaryExchange parameter not needed in GetMktDepth.');
      Exit;
    end;

  AddToOut(REQ_MKT_DEPTH);
  AddToOut(VERSION);
  AddToOut(DataId);
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then // 68
    AddToOut(ConId);
  AddToOut(Symbol);
  AddToOut(SecurityTypeString[SecurityType]);
  AddToOut(Expiry);
  AddToOut(Strike);
  AddToOut(RightString[Right]);
  AddToOut(Multiplier);  // FServerVersion 15
  AddToOut(Exchange);
  if FServerVersion >= MIN_SERVER_VER_MKT_DEPTH_PRIM_EXCHANGE then  // 149
    AddToOut(PrimaryExchange);
  AddToOut(Currency);
  AddToOut(Local);
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then  // 68
    AddToOut(TradingClass);
  AddToOut(nRows);  // FServerVersion 19
  if FServerVersion >= MIN_SERVER_VER_SMART_DEPTH then  // 146
    AddToOut(FSmartDepthMarketData);
  if FServerVersion >= MIN_SERVER_VER_LINKING then  //  70
     AddToOut(MktDataOptions);
  SendToSocket(DataId);
end;

procedure TIABSocket.GetMarketDepth(DataId: Integer; Symbol, Local, Exchange, PrimaryExchange, Expiry, Currency: string;
            SecurityType: TIABSecurityType; Right: TIABRight; Strike: Double; nRows: Integer; Multiplier: string = '');
begin
  GetMarketDepth(DataId, Symbol, Local, Exchange, PrimaryExchange, Expiry, Currency, '', '', '', SecurityType, Right, Strike, nRows, 0);
end;

procedure TIABSocket.GetMarketDepth(DataId: Integer; Order: TIABOrder; nRows: Integer);
var i: Integer; MktDataOptions: string;
begin
  if Order = nil then Order := FDefaultOrder;
  MktDataOptions := '';
  if FServerVersion >= MIN_SERVER_VER_LINKING then  //  70
    for i := 0 to Order.MktDataOptionsCount -1 do
      MktDataOptions := MktDataOptions + Order.MktDataOptions[i].Tag + '=' + Order.MktDataOptions[i].Value + ';';

  GetMarketDepth(DataId, Order.Symbol, Order.LocalSymbol, Order.Exchange, Order.PrimaryExchange, Order.Expiry, Order.Currency,
                Order.TradingClass, MktDataOptions, Order.Multiplier, Order.SecurityType, Order.Right, Order.Strike, nRows,
                Order.ContractId);
end;

procedure TIABSocket.GetOpenOrdersAccount;
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  FAcceptOpenOrdersWindow := GetTickCount + OPEN_ORDER_ACCEPT_WINDOW;
  AddToOut(REQ_ALL_OPEN_ORDERS);
  AddToOut(VERSION);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.GetOpenOrdersClient;
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  FAcceptOpenOrdersWindow := GetTickCount + OPEN_ORDER_ACCEPT_WINDOW;
  AddToOut(REQ_OPEN_ORDERS);
  AddToOut(VERSION);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.GetOptionPrice(DataID: Integer; Order: TIABOrder; Volatility, UnderPrice: Double);
const VERSION = 2;
begin

  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_REQ_CALC_OPTION_PRICE) then
    begin
      DoErrorEvent(DataId, UPDATE_TWS, ' calculate option price requests not supported.');
      Exit;
    end;

  AddToOut(REQ_CALC_OPTION_PRICE);
  AddToOut(VERSION);
  AddToOut(DataId);
  AddToOut(Order.ContractID);
  AddToOut(Order.Symbol);
  AddToOut(SecurityTypeString[Order.SecurityType]);
  AddToOut(Order.Expiry);
  AddToOut(Order.Strike);
  AddToOut(RightString[Order.Right]);
  AddToOut(Order.Multiplier);
  AddToOut(Order.Exchange);
  AddToOut(Order.PrimaryExchange);
  AddToOut(Order.Currency);
  AddToOut(Order.LocalSymbol);
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then  // 68
    AddToOut(Order.TradingClass);
  AddToOut(Volatility);
  AddToOut(UnderPrice);
  SendToSocket(DataId);
end;

procedure TIABSocket.GetRealTimeData(DataId: Integer; Symbol, Local, Exchange, Expiry, Currency, PrimaryExchange, Multiplier: string;
                                       SecurityType: TIABSecurityType; Right: TIABRight; Strike: Double;
                                       BarSize: TIABChartBarSize;
                                       DataBasis: TIABHistoricalDataType; ExtendedHours: Boolean;
                                       ContractId: Integer; TradingClass: string; RealtimeBarOptions: TIABTagValueArray);
const VERSION = 3;
var i: Integer; stv: string;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  if not (DataBasis in [cdTrades..cdBidAsk]) then
    begin
      DoErrorEvent(DataId, FAIL_SEND_REQRTBARS, ' DataBasis paramater out of range for the GetRealTimeData procedure.');
      Exit;
    end;
  AddToOut(REQ_REAL_TIME_BARS);
  AddToOut(VERSION);
  AddToOut(DataId);
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then // 68
    AddToOut(ContractId);
  AddToOut(Symbol);
  AddToOut(SecurityTypeString[SecurityType]);
  AddToOut(Expiry);
  AddToOut(Strike);
  AddToOut(RightString[Right]);
  AddToOut(Multiplier);
  AddToOut(Exchange);
  AddToOut(PrimaryExchange);
  AddToOut(Currency);
  AddToOut(Local);
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then // 68
    AddToOut(TradingClass);
  AddToOut(ChartBarSizeInt[BarSize]);
  AddToOut(HistoricalDataTypeString[DataBasis]);
  AddToOut(not ExtendedHours);
  if FServerVersion >= MIN_SERVER_VER_LINKING then  //  70
    begin
      stv := '';
      for i := 0 to High(RealtimeBarOptions) do
        stv := stv + RealtimeBarOptions[i].Tag + '=' + RealtimeBarOptions[i].Value + ';';
      AddToOut(stv);
    end;

  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.GetScannerParameters;
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(REQ_SCANNER_PARAMETERS);
  AddToOut(VERSION);
  SendToSocket(NO_VALID_ID);
end;

function TIABSocket.GetSocketHostAddress: string;
begin
  {$IF CompilerVersion >= 21.0}   // Delphi 2010
    Result := string(FSocket.RemoteHost);
  {$ELSE}
    Result := FSocket.RemoteHost;
  {$IFEND}
end;

function TIABSocket.GetSocketPort: Integer;
begin
  {$IF CompilerVersion >= 21.0}   // Delphi 2010
    Result := StrToInt(string(FSocket.RemotePort));
  {$ELSE}
    Result := StrToInt(FSocket.RemotePort);
  {$IFEND}
end;

procedure TIABSocket.GetTickByTickData(DataId: Integer; Contract: TIABContract; TickType: TIABTickDataType; NumberOfTicks: Integer; IgnoreSize: Boolean);
begin

  if not FSocket.Active then
    begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_TICK_BY_TICK) then  // 137
    begin
      DoErrorEvent(DataId, UPDATE_TWS, ' tick-by-tick data request not supported.');
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_TICK_BY_TICK_IGNORE_SIZE) then  // 140
    if ((NumberOfTicks <> 0) or IgnoreSize) then
      begin
        DoErrorEvent(DataId, UPDATE_TWS, ' ignoreSize and numberOfTicks parameters not supported.');
        Exit;
      end;

  AddToOut(REQ_TICK_BY_TICK_DATA);
  AddToOut(DataId);
  EncodeContract(Contract, true);
  AddToOut(TickDataTypeString[TickType]);
  if (FServerVersion >= MIN_SERVER_VER_TICK_BY_TICK_IGNORE_SIZE) then  // 140
    begin
      AddToOut(NumberOfTicks);
      AddToOut(IgnoreSize);
    end;
  SendToSocket(DataId);
end;

function TIABSocket.ProcessConnectAck(pa: PAnsiChar): Boolean;
var sa: AnsiString; su: string;  // su is a unicode in 2009, or no change in all lesser versions.
begin
  Result := true;
  sa := pa;       // copy from null to regular 1 byte string
  su := string(sa);   //  promote / copy to a 2 byte string (D2009 + or nothing in all versions prior);
  S_Ver := StrToInt(su);  // check for valid integer as string to read  // hold it temp in case next block fails
  if S_Ver < 0 then
    begin
      // (don't) handle redirects
      pa := 1 + StrEnd(pa);
      DoErrorEvent(NO_VALID_ID, FAIL_CONNECT_TWS_REDIRECT, string(pa));
      Exit;
    end
  else if S_Ver >= 20 then
    begin
      pa := 1 + StrEnd(pa);  // points to server time string - if its there
      if (strlen(pa) < 20) or (strlen(pa) > 60) then Exit; // a rough guess that data exists. // split send - await extras, underlying memory is set to $ff
      sa := pa;
      su := string(sa);
      FConnectAtServerTime := Copy(su,1,Length(su));
    end;


  if FClientv100Plus then
    begin
      if (S_Ver < MIN_CLIENT_VER) or (S_Ver > MAX_CLIENT_VER) or (S_Ver < MIN_SERVER_VER_SUPPORTED) then
        DoErrorEvent(NO_VALID_ID, UNSUPPORTED_VERSION);
    end;
  FServerVersion := S_Ver;
  FOrders.FServerVersion := S_Ver;
  Result := false;
end;

procedure TIABSocket.Initialize;
var pa: PAnsiChar; conFailed: Boolean; netLen, msgLen: u_long;
begin
  conFailed := true;

  if FClientv100plus then
    begin
      pa := FInStream.Memory;
      if FInStream.Position <= 3 then Exit;  // no header info
      if ((pa + 1)^ <> #0) and (pa^ <> #0) then  // old system
        conFailed := true
      else
        begin
          Move(FInStream.Memory^, netLen, HEADER_LEN);
          msgLen := ntohl(netLen);
          if FInStream.Position < msgLen then
            Exit;
          inc(pa, HEADER_LEN);
          conFailed := ProcessConnectAck(pa);
        end;
    end
  else
    begin
      pa := FInStream.Memory;
      if FInStream.Position <= 1 then Exit;  // for when a split send on first contact
      if (FInStream.Position = 2) and ((pa + 1)^ <> #0) then Exit;  // double digits - split send
      if (FInStream.Position >= 2) and (((pa + 1)^ = #0) or ((pa + 2)^ = #0)) then
        conFailed := ProcessConnectAck(pa);
    end;

  if conFailed then
    begin
      DoErrorEvent(NO_VALID_ID, FAIL_CONNECT_TWS);
      if Assigned(FOnConnectionState) then FOnConnectionState(Self,twsFailed);
      SetConnected(false);
      Exit;
    end;
  FInStream.Position := 0;
  //if FServerVersion < MIN_SERVER_VERSION then  // 38 at present  // old system - no longer suppurted
  if FServerVersion < MIN_CLIENT_VER then   // 100
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS);
      if Assigned(FOnConnectionState) then FOnConnectionState(Self,twsFailed);
      SetConnected(false);
      Exit;
    end;
  FConnecting := false;

  // send client version handshake
  StartAPI;

  if Assigned(FOnConnectionState) then FOnConnectionState(Self,twsReady);
end;

procedure TIABSocket.StartAPI;
var VERSION: Integer;
begin
  VERSION := 2;

  if FServerVersion < MIN_SERVER_VER_LINKING then // 70
    begin
      AddToOut(FClientID);
      SendToSocket(NO_VALID_ID);
      if FReserveIds > 0 then SendReserveId(FReserveIds);
    end
  else
    begin
      AddToOut(START_API);
      AddToOut(VERSION);   // version
      AddToOut(FClientID);
      if FServerVersion >= MIN_SERVER_VER_OPTIONAL_CAPABILITIES then
        AddToOut(FOptionalCapabilities);
      SendToSocket(NO_VALID_ID);
    end;
end;


function TIABSocket.ModifyOrder(TempId, ClientId, Quantity: Integer; OrderType: TIABOrderType; Price, AuxPrice: Double): Boolean;
var Order: TIABOrder; i: Integer;
begin
  FRebuildFromTWS := false;
  if not FSocket.Active then
    begin
      DoErrorEvent(TempId, NOT_CONNECTED);
      Result := false;
      Exit;
    end;
  i := FOrders.IndexOfTempId(TempId,ClientId);
  Result := i > -1;
  if not Result then Exit;
  Order := FOrders.Items[i];
  //  these values adjusted only if a value supplied - otherwise original order value prevails.
  if Quantity > 0 then Order.FQuantity := Quantity; // partials now computed by TWS - we specify absolute qty always.
  if OrderType > otNoChange then Order.FOrderType := OrderType;
  if Price > 0.0 then Order.Price := Price;
  if AuxPrice > 0.0 then Order.AuxPrice := AuxPrice;
  DoPlaceOrModifyOrder(Order,TempId,false);
end;

function TIABSocket.PlaceOrder(Order: TIABOrder): Integer;
var AddOrder: TIABOrder;
begin                     // if Order = nil then the default is used
  FRebuildFromTWS := false;
  if Order = nil then Order := FDefaultOrder;
  Result := -1;
  if PlaceVerifyOrderPreChecks(Order) = -1 then
    Exit;

  inc(FNextTempID);
  DoPlaceOrModifyOrder(Order,FNextTempId,false);
  AddOrder := TIABOrder.Create;
  AddOrder.Assign(Order);
  FOrders.Add(FNextTempID,FClientId,0,AddOrder);
  Result := FNextTempID;
end;

function TIABSocket.PlaceBracketOrder(ParentOrder: TIABOrder; BracketOrders: array of TIABOrder): Integer;
var AddOrder, Order: TIABOrder; i, ParentID: Integer;
begin
  FRebuildFromTWS := false;
  Order := nil;
  Result := -1;

  if PlaceVerifyOrderPreChecks(ParentOrder) = -1 then
    Exit;
  for i := 0 to High(BracketOrders) do
    begin
      Order := BracketOrders[i];
      if PlaceVerifyOrderPreChecks(Order) = -1 then
        Exit;
    end;

  inc(FNextTempID);
  ParentID := FNextTempID;
  ParentOrder.FTransmit := false;
  DoPlaceOrModifyOrder(ParentOrder,FNextTempId,false);
  AddOrder := TIABOrder.Create;
  AddOrder.Assign(Order);
  FOrders.Add(FNextTempID,FClientId,0,AddOrder);

  for i := 0 to High(BracketOrders) do
    begin
      inc(FNextTempID);
      Order := BracketOrders[i];
      Order.FParentId := ParentID;
      Order.FTransmit := i = High(BracketOrders);
      DoPlaceOrModifyOrder(Order,FNextTempId,false);
      AddOrder := TIABOrder.Create;
      AddOrder.Assign(Order);
      FOrders.Add(FNextTempID,FClientId,0,AddOrder);
    end;

  Result := ParentID;
end;


function TIABSocket.VerifyOrder(Order: TIABOrder): Integer;
var AddOrder: TIABOrder;
begin                     // if Order = nil then the default is used
  FRebuildFromTWS := false;
  if Order = nil then Order := FDefaultOrder;
  Result := -1;
  if PlaceVerifyOrderPreChecks(Order) = -1 then
    Exit;

  inc(FNextTempID);
  DoPlaceOrModifyOrder(Order,FNextTempId,true);
  AddOrder := TIABOrder.Create;
  AddOrder.Assign(Order);
  FVerifiedOrders.Add(FNextTempID,FClientId,0,AddOrder);
  Result := FNextTempID;
end;

function TIABSocket.PlaceVerifyOrderPreChecks(Order: TIABOrder): Integer;
var i: Integer; err_str: string;
begin
  Result := -1;
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  err_str := '';
  if (FServerVersion < MIN_SERVER_VER_WHAT_IF_ORDERS) then
    err_str := 'what-if and Verify test orders';
  if (FServerVersion < MIN_SERVER_VER_UNDER_COMP) and ((Order.DeltaNeutralContractId > 0) or (Order.DeltaNeutralContractDelta <> 0.0) or (Order.DeltaNeutralContractPrice <> 0.0)) then
    err_str := 'Combo order Delta-neutral';
  if (FServerVersion < MIN_SERVER_VER_SCALE_ORDERS2) and (Order.ScaleSubsLevelSize <> UNSET_INTEGER) then
    err_str := 'Subsequent Level Size for Scale';
  if (FServerVersion < MIN_SERVER_VER_NOT_HELD) and (Order.FNotHeld) then
    err_str := 'NotHeld';
	if (FServerVersion < MIN_SERVER_VER_SEC_ID_TYPE) and ((Order.FSecurityID <> '') or (Order.SecurityIDType <> '')) then
    err_str := 'secIdType and secId';
  if (FServerVersion < MIN_SERVER_VER_PLACE_ORDER_CONID) and (Order.FContractID > 0) then
    err_str := 'ContractID';
  if (FServerVersion < MIN_SERVER_VER_SSHORTX) and (Order.FExemptCode <> -1) then
    err_str := 'ExemptCode';
  if (FServerVersion < MIN_SERVER_VER_HEDGE_ORDERS) and (Order.FHedgeType > htUnset) then
    err_str := 'Hedge';
  if (FServerVersion < MIN_SERVER_VER_OPT_OUT_SMART_ROUTING) and Order.FOptOutSmartRouting then
    err_str := 'OptOutSmartRouting';
  if (FServerVersion < MIN_SERVER_VER_DELTA_NEUTRAL_CONID) and (Order.FDeltaNeutralConId > 0) then
    err_str := 'DeltaNeutralConId';
  if (FServerVersion < MIN_SERVER_VER_DELTA_NEUTRAL_OPEN_CLOSE) and (Length(Order.FDeltaNeutralOpenClose) > 0) then
    err_str := 'DeltaNeutralOpenClose';
  if (FServerVersion < MIN_SERVER_VER_SCALE_ORDERS3) and ((Order.FScalePriceIncrement < UNSET_DOUBLE_TEST_VALUE) or (Order.FScalePriceAdjustValue < UNSET_DOUBLE_TEST_VALUE)) then
    err_str := 'ScalePrice type';
  if (FServerVersion < MIN_SERVER_VER_TRAILING_PERCENT) and (Order.FTrailingPercent < UNSET_DOUBLE_TEST_VALUE) then
    err_str := 'TrailingPercent';
	if (FServerVersion < MIN_SERVER_VER_TRADING_CLASS) and (Order.FTradingClass <> '') then
		err_str := 'TradingClass';
	if (FServerVersion < MIN_SERVER_VER_SCALE_TABLE) and ((Order.FScaleTable <> '') or (Order.FActiveStartTime <> '') or (Order.FActiveStopTime <> '')) then
  	err_str := 'ScaleTable, ActiveStartTime or ActiveStopTime';
	if (FServerVersion < MIN_SERVER_VER_ALGO_ID) and (Order.FAlgoId <> '') then
    err_str := 'AlgoID';

  if (FServerVersion < MIN_SERVER_VER_SSHORT_COMBO_LEGS) and (Order.SecurityType = stBag) and (Order.ComboLegsCount > 0) then

    for i := 0 to Order.ComboLegsCount -1 do
      if (Order.ComboLegs[i].ShortSaleSlot <> 0) or (Order.ComboLegs[i].DesignatedLocation <> '') then
        begin
          err_str := 'SSHORT flag in Combo legs';
          Break;
        end;
  if (FServerVersion < MIN_SERVER_VER_SSHORTX) and (Order.ComboLegsCount > 0) then
    begin
      for i := 0 to Order.ComboLegsCount - 1 do
        if Order.ComboLegs[i].ExemptCode <> -1 then
          begin
            err_str := 'ExemptCode in ComboLegs';
            Break;
          end;
    end;
  if (FServerVersion < MIN_SERVER_VER_ORDER_COMBO_LEGS_PRICE) and (Order.FSecurityType = stBag) then
    begin
      for i := 0 to Order.ComboLegPriceCount - 1 do
        if Order.ComboLegPrice[i] < UNSET_DOUBLE_TEST_VALUE then
          begin
            err_str := 'ComboLegPrice';
            Break;
          end;
    end;
	if (FServerVersion < MIN_SERVER_VER_TRAILING_PERCENT) and (Order.TrailingPercent < UNSET_DOUBLE_TEST_VALUE) then
		err_str := 'Trailing percent';
  if (FServerVersion < MIN_SERVER_VER_TRADING_CLASS) and (Length(Order.TradingClass) > 0) then
    err_str := 'TradingClass';
	if (FServerVersion < MIN_SERVER_VER_SCALE_TABLE) and
    ((Length(Order.ScaleTable) > 0) or (Length(Order.ActiveStartTime) > 0) or (Length(Order.ActiveStopTime) > 0)) then
      err_str := 'scaleTable, activeStartTime and activeStopTime';
	if (FServerVersion < MIN_SERVER_VER_ALGO_ID) and (Length(Order.algoId) > 0) then
    err_str := 'algoId';
	if (FServerVersion < MIN_SERVER_VER_ORDER_SOLICITED) and Order.Solicited then
    err_str := 'Solicited';
	if (FServerVersion < MIN_SERVER_VER_MODELS_SUPPORT) and (Length(Order.ModelCode) > 0) then
    err_str := 'ModelCode';
	if (FServerVersion < MIN_SERVER_VER_EXT_OPERATOR) and (Length(Order.ExtOperator) > 0) then
    err_str := 'ExtOperator';
	if (FServerVersion < MIN_SERVER_VER_SOFT_DOLLAR_TIER) and
    ((Length(Order.SoftDollarTier.Name) > 0) or (Length(Order.SoftDollarTier.Value) > 0) or (Length(Order.SoftDollarTier.DisplayName) > 0)) then
      err_str := 'SoftDollarTier';
	if (FServerVersion < MIN_SERVER_VER_CASH_QTY) and (Order.CashQuantity > 0) then
    err_str := 'cash quantity';
	if (FServerVersion < MIN_SERVER_VER_DECISION_MAKER) and
    ((Length(Order.Mifid2DecisionMaker) > 0) or (Length(Order.Mifid2DecisionAlgo) > 0)) then
      err_str := 'MIFID II decision maker';
	if (FServerVersion < MIN_SERVER_VER_MIFID_EXECUTION) and
    ((Length(Order.Mifid2ExecutionTrader) > 0) or (Length(Order.Mifid2ExecutionAlgo) > 0)) then
      err_str := 'MIFID II execution';
	if (FServerVersion < MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE) and Order.DontUseAutoPriceForHedge then
    err_str := 'auto price for hedge';
	if (FServerVersion < MIN_SERVER_VER_ORDER_CONTAINER) and Order.IsOmsContainer then
    err_str := 'oms container';
	if (FServerVersion < MIN_SERVER_VER_D_PEG_ORDERS) and Order.DiscretionaryUpToLimitPrice then
    err_str := 'D-Peg orders';
	if (FServerVersion < MIN_SERVER_VER_PRICE_MGMT_ALGO) and (Order.UsePriceMgmtAlgo <> IAB_PRICE_MGNT_ALGO_DEFAULT) then
    err_str := 'Use Price Management Algo requests';



	if (FServerVersion <MIN_SERVER_VER_DURATION) and (Order.Duration <> UNSET_INTEGER) then

    err_str := 'Duration attribute';

	if (FServerVersion < MIN_SERVER_VER_POST_TO_ATS) and (Order.postToAts <> UNSET_INTEGER) then

    err_str := 'PostToAts attribute';

	if (FServerVersion < MIN_SERVER_VER_AUTO_CANCEL_PARENT) and (Order.AutoCancelParent) then

    err_str := 'AutoCancelParent parameter';

	if (FServerVersion < MIN_SERVER_VER_ADVANCED_ORDER_REJECT) and (Order.AdvancedErrorOverride <> '') then

    err_str := 'advanced error override attribute';

	if (FServerVersion < MIN_SERVER_VER_MANUAL_ORDER_TIME) and (Order.manualOrderTime <> 0.0) then

    err_str := 'manual order time attribute';

	if (FServerVersion < MIN_SERVER_VER_PEGBEST_PEGMID_OFFSETS) then

    begin

      if (order.minTradeQty <> UNSET_INTEGER) or
          (order.minCompeteSize <> UNSET_INTEGER) or
          (order.competeAgainstBestOffset <> UNSET_DOUBLE) or
          (order.midOffsetAtWhole <> UNSET_DOUBLE) or
          (order.midOffsetAtHalf <> UNSET_DOUBLE) then
        err_str := 'It does not support PEG BEST / PEG MID order parameters: minTradeQty, minCompeteSize, competeAgainstBestOffset, midOffsetAtWhole and midOffsetAtHalf';
    end;



  if Length(err_str) > 0 then
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' Orders with ' + err_str + ' parameter not supported.');
      Exit;
    end;
  Result := 0;
end;


procedure TIABSocket.DoPlaceOrModifyOrder(Order: TIABOrder; Id: Integer; Verify: Boolean);
var i: Integer; Lower, Upper: Double; stv: string;
    VERSION: Integer; SendMidOffsets: Boolean;
begin
  if FServerVersion < MIN_SERVER_VER_NOT_HELD then
    VERSION := 27
  else
    VERSION := 45;

  // code common to both PlaceOrder and ModifyOrder.
  AddToOut(PLACE_ORDER);
  if (FServerVersion < MIN_SERVER_VER_ORDER_CONTAINER) then  // 145
    AddToOut(VERSION);
  AddToOut(Id);
	if FServerVersion >= MIN_SERVER_VER_PLACE_ORDER_CONID then   // 46
		AddToOut(Order.FContractId);
  AddToOut(Order.Symbol);
  AddToOut(SecurityTypeString[Order.SecurityType]);
  AddToOut(Order.Expiry);
  AddToOut(Order.Strike);
  AddToOut(RightString[Order.Right]);
  AddToOut(Order.Multiplier); // FServerVersion 15
  AddToOut(Order.Exchange);
  AddToOut(Order.PrimaryExchange); // FServerVersion 14
  AddToOut(Order.Currency);
  AddToOut(Order.LocalSymbol); // FServerVersion 2
  if FServerVersion >= MIN_SERVER_VER_TRADING_CLASS then // 68
    AddToOut(Order.TradingClass);
	if FServerVersion >= MIN_SERVER_VER_SEC_ID_TYPE then  //  45
    begin
  		AddToOut(Order.FSecurityIDType);
  		AddToOut(Order.FSecurityID);
    end;

  // send main order fields
  AddToOut(ActionString[Order.Action]);

  if (FServerVersion >= MIN_SERVER_VER_FRACTIONAL_POSITIONS) then
    AddToOut(Order.Quantity)
  else
    begin
      i := Round(Order.Quantity);
      AddToOut(i);
    end;

  AddToOut(OrderTypeString[Order.OrderType]);

	if (FServerVersion < MIN_SERVER_VER_ORDER_COMBO_LEGS_PRICE) and (Order.Price >= UNSET_DOUBLE_TEST_VALUE) then  // 61
    AddToOut(0)
	else
    AddToOutNullValueDef(Order.Price);
	if (FServerVersion < MIN_SERVER_VER_TRAILING_PERCENT) and (Order.AuxPrice >= UNSET_DOUBLE_TEST_VALUE) then   // 62
    AddToOut(0)
	else
    AddToOutNullValueDef(Order.AuxPrice);

  // send extended order fields
  AddToOut(TimeInForceString[Order.TimeInForce]);
  AddToOut(Order.OCAGroup);
  AddToOut(Order.Account);
  AddToOut(Order.OpenClose);
  AddToOut(Ord(Order.OrderOrigin));
  AddToOut(Order.OrderRef);
  AddToOut(Order.Transmit);
  AddToOut(Order.ParentId);  // FServerVersion 4
  AddToOut(Order.BlockOrder);  // FServerVersion   5
  AddToOut(Order.SweepToFill);  // FServerVersion    5
  AddToOut(Order.DisplaySize);         // FServerVersion     5
  AddToOut(Order.TriggerMethod);         // FServerVersion    5
  AddToOut(Order.ExtendedHours);   // FServerVersion   5
  AddToOut(Order.Hidden);    // FServerVersion  7
  if (Order.SecurityType = stBag) then     // FServerVersion  8
    begin
      AddToOut(Order.ComboLegsCount);
      for i := 0 to Order.ComboLegsCount -1 do
        begin
          AddToOut(Order.ComboLegs[i].ContractId);
          AddToOut(Order.ComboLegs[i].Ratio);
          AddToOut(ActionString[Order.ComboLegs[i].Action]);
          AddToOut(Order.ComboLegs[i].Exchange);
          AddToOut(Ord(Order.ComboLegs[i].OpenClose));
          AddToOut(Order.ComboLegs[i].ShortSaleSlot);     // FServerVersion 35
          AddToOut(Order.ComboLegs[i].DesignatedLocation);   // FServerVersion 35
  				if FServerVersion >= MIN_SERVER_VER_SSHORTX_OLD then  //51
            AddToOut(Order.ComboLegs[i].ExemptCode);
        end;
    end;

  if (FServerVersion >= MIN_SERVER_VER_ORDER_COMBO_LEGS_PRICE) and (Order.FSecurityType = stBag) then   // 61
    begin
      AddToOut(Order.ComboLegPriceCount);
      if Order.ComboLegPriceCount > 0 then
        for i := 0 to Order.ComboLegPriceCount - 1 do
          AddToOutNullValueDef(Order.ComboLegPrice[i]);
    end;

  if (FServerVersion >= MIN_SERVER_VER_SMART_COMBO_ROUTING_PARAMS) and (Order.FSecurityType = stBag) then  // 57
    begin
      AddToOut(Order.SmartComboRoutingParamsCount);
      if Order.SmartComboRoutingParamsCount > 0 then
        for i := 0 to Order.SmartComboRoutingParamsCount - 1 do
          begin
            AddToOut(Order.SmartComboRoutingParams[i].Tag);
            AddToOut(Order.SmartComboRoutingParams[i].Value);
          end;
    end;

  // send deprecated sharesAllocation field
  AddToOut('');  // FServerVersion 9
  AddToOut(Order.DiscretAmount);     // FServerVersion 10
  AddToOut(Order.GoodAfterTime);    // FServerVersion 11
  AddToOut(Order.GoodTillDate);       // FServerVersion 12
  AddToOut(Order.FAdvGroup);    // FServerVersion    13
  AddToOut(Order.FAdvMethod);     // FServerVersion    13
  AddToOut(Order.FAdvPercentage);     // FServerVersion  13
  AddToOut(Order.FAdvProfile);       // FServerVersion     13

	if (FServerVersion >= MIN_SERVER_VER_MODELS_SUPPORT) then // 103
		AddToOut(Order.ModelCode);

  AddToOut(Order.FShortSaleSlot);    // FServerVersion  18
  AddToOut(Order.DesignatedLocation);     // FServerVersion 18

	if FServerVersion >= MIN_SERVER_VER_SSHORTX_OLD then // 51
		AddToOut(Order.ExemptCode);

  AddToOut(Ord(Order.OcaMethod) + 1);          // FServerVersion  19
  //if FServerVersion < 38 then AddToOut(Order.RegTradingHoursOnly);
  AddToOut(TIABRule80AStr[Order.Rule80A]);
  AddToOut(Order.SettlingFirm);
  AddToOut(Order.AllOrNone);
  AddToOutNullValueDef(Order.MinQuantity);
  AddToOutNullValueDef(Order.PercentOffset);

  //AddToOut(Order.eTradeOnly);               // dropped in TWS API 10.10
  //AddToOut(Order.FirmQuoteOnly);                  // dropped in TWS API 10.10
  //AddToOutNullValueDef(Order.NBBOPriceCap);          // dropped in TWS API 10.10
  AddToOut(false);
  AddToOut(false);
  AddToOutNullValueDef(UNSET_DOUBLE);

  AddToOut(Ord(Order.AuctionStrategy));
  AddToOutNullValueDef(Order.StartingPrice);
  AddToOutNullValueDef(Order.StockRefPrice);
  AddToOutNullValueDef(Order.Delta);
  // Volatility orders had specific watermark price attribs in server version 26
  // .. removed in later builds
  Lower := UNSET_DOUBLE;        // stockRangeLower
  Upper := UNSET_DOUBLE;        // stockRangeUpper
  AddToOutNullValueDef(Lower);
  AddToOutNullValueDef(Upper);

  AddToOut(Order.FOverridePercentageConstraints);   // FServerVersion 22

  AddToOutNullValueDef(Order.Volatility);
  if Order.FVolatilityPeriod <> vpUnSet then
    AddToOut(Integer(Order.FVolatilityPeriod))
  else
    AddToOutNullValueDef(UNSET_INTEGER);
  AddToOut(OrderTypeString[Order.FDeltaNeutralOrderType]);  // FServerVersion 28
  AddToOutNullValueDef(Order.FDeltaNeutralAuxPrice);       // FServerVersion 28

  if (FServerVersion >= MIN_SERVER_VER_DELTA_NEUTRAL_CONID) and (Order.DeltaNeutralOrderType > otNoChange) and (Order.DeltaNeutralOrderType < otNone) then // 58
    begin
      AddToOut(Order.DeltaNeutralConId);
      AddToOut(Order.DeltaNeutralSettlingFirm);
      AddToOut(Order.DeltaNeutralClearingAccount);
      AddToOut(Order.DeltaNeutralClearingIntent);
    end;
	if (FServerVersion >= MIN_SERVER_VER_DELTA_NEUTRAL_OPEN_CLOSE) and (Order.DeltaNeutralOrderType > otNoChange) and (Order.DeltaNeutralOrderType < otNone) then //66
    begin
      AddToOut(Order.DeltaNeutralOpenClose);
      AddToOut(Order.DeltaNeutralShortSale);
      AddToOut(Order.DeltaNeutralShortSaleSlot);
      AddToOut(Order.DeltaNeutralDesignatedLocation);
    end;

  AddToOut(Order.FContinuousUpdate);
  // Volatility orders had specific watermark price attribs in server version 26
  // .. removed in later builds
  if Order.FReferencePrice <> rpUnSet then
    AddToOut(Integer(Order.FReferencePrice))
  else
    AddToOutNullValueDef(UNSET_INTEGER);

  AddToOutNullValueDef(Order.TrailStopPrice);   // FServerVersion 30

	if FServerVersion >= MIN_SERVER_VER_TRAILING_PERCENT then //62
		AddToOutNullValueDef(Order.TrailingPercent);

  if FServerVersion >= MIN_SERVER_VER_SCALE_ORDERS2 then  // 40
    begin
      AddToOutNullValueDef(Order.ScaleInitLevelSize);
      AddToOutNullValueDef(Order.ScaleSubsLevelSize);
    end
  else // if FServerVersion >= MIN_SERVER_VER_SCALE_ORDERS then  // 35
    begin
      AddToOut('');   // for not supported scaleNumComponents
      AddToOutNullValueDef(Order.ScaleInitLevelSize);
    end;

  AddToOutNullValueDef(Order.ScalePriceIncrement);

	if (FServerVersion >= MIN_SERVER_VER_SCALE_ORDERS3) and ((Order.ScalePriceIncrement > 0.0) and (Order.ScalePriceIncrement < UNSET_DOUBLE_TEST_VALUE)) then  //60
    begin
		  AddToOutNullValueDef(Order.ScalePriceAdjustValue);
		  AddToOutNullValueDef(Order.ScalePriceAdjustInterval);
		  AddToOutNullValueDef(Order.ScaleProfitOffset);
		  AddToOut(Order.ScaleAutoReset);
		  AddToOutNullValueDef(Order.ScaleInitPosition);
		  AddToOutNullValueDef(Order.ScaleInitFillQty);
		  AddToOut(Order.ScaleRandomPercent);
    end;

	if FServerVersion >= MIN_SERVER_VER_SCALE_TABLE then  // 69
    begin
		  AddToOut(Order.ScaleTable);
	    AddToOut(Order.ActiveStartTime);
		  AddToOut(Order.ActiveStopTime);
    end;

	if FServerVersion >= MIN_SERVER_VER_HEDGE_ORDERS then  // 54
    begin
		  AddToOut(TIABHedgeTypeStr[Order.HedgeType]);
      if Order.HedgeType > htUnset then
   		  AddToOut(Order.HedgeParam);
     end;

	if FServerVersion >= MIN_SERVER_VER_OPT_OUT_SMART_ROUTING then     // 56
		AddToOut(Order.OptOutSmartRouting);

  if FServerVersion >= MIN_SERVER_VER_PTA_ORDERS then   // 39
    begin
      AddToOut(Order.ClearingAccount);
      AddToOut(Order.ClearingIntent);
    end;

	if FServerVersion >= MIN_SERVER_VER_NOT_HELD then  // 44
		AddToOut(Order.FNotHeld);

  //if (FServerVersion >= MIN_SERVER_VER_UNDER_COMP) then  // 40
  if (FServerVersion >= MIN_SERVER_VER_DELTA_NEUTRAL) then  // 40
    begin
      if ((Order.DeltaNeutralContractId > 0) or (Order.DeltaNeutralContractDelta <> 0.0) or (Order.DeltaNeutralContractPrice <> 0.0)) then
        begin
          AddToOut( 1 );  // false or true
          AddToOut(Order.DeltaNeutralContractId);
          AddToOut(Order.DeltaNeutralContractDelta);
          AddToOut(Order.DeltaNeutralContractPrice);
        end
      else
        AddToOut( 0 );  // false or true
    end;

  if FServerVersion >= MIN_SERVER_VER_ALGO_ORDERS then  // 41
    begin
      AddToOut(Order.AlgoStrategy);
      if Length(Order.AlgoStrategy) > 0 then
        begin
          AddToOut(Order.AlgoParamsCount);
          if Order.AlgoParamsCount > 0 then
            for i := 0 to Order.AlgoParamsCount - 1 do
              begin
                AddToOut(Order.AlgoParams[i].Tag);
                AddToOut(Order.AlgoParams[i].Value);
              end;
        end;
    end;

  if FServerVersion >= MIN_SERVER_VER_ALGO_ID then // 71
    AddToOut(Order.AlgoId);

  AddToOut(Verify);  // FServerVersion 36

  if FServerVersion >= MIN_SERVER_VER_LINKING then  //  70
    begin
      stv := '';
      for i := 0 to High(Order.FOrderMiscOptions) do
        stv := stv + Order.FOrderMiscOptions[i].Tag + '=' + Order.FOrderMiscOptions[i].Value + ';';
      AddToOut(stv);
    end;


	if FServerVersion >= MIN_SERVER_VER_ORDER_SOLICITED then  //  73
		AddToOut(Order.Solicited);
  if FServerVersion >= MIN_SERVER_VER_RANDOMIZE_SIZE_AND_PRICE then  //  76
    begin
      AddToOut(Order.RandomizeSize);
      AddToOut(Order.RandomizePrice);
    end;
	if (FServerVersion >= MIN_SERVER_VER_PEGGED_TO_BENCHMARK) then  //  102
    begin
      if (Order.OrderType = otPegBench) then
        begin
    			AddToOut(Order.ReferenceContractId);
		    	AddToOut(Order.IsPeggedChangeAmountDecrease);
    			AddToOut(Order.PeggedChangeAmount);
		    	AddToOut(Order.ReferenceChangeAmount);
    			AddToOut(Order.ReferenceExchangeId);
        end;

      // Order Conditions not implemented.
      AddToOut(0);
  		//AddToOut(Order.conditions.size());
      //if (order.conditions.size() > 0) {
	  	//	for (ibapi::shared_ptr<OrderCondition> item : order.conditions) {
  		//		ENCODE_FIELD(item->type());
	  	//		item->writeExternal(msg);
  		//	}
      //ENCODE_FIELD(order.conditionsIgnoreRth);
  		//	ENCODE_FIELD(order.conditionsCancelOrder);
	  	//}
  		AddToOut(OrderTypeString[Order.AdjustedOrderType]);
	  	AddToOut(Order.TriggerPrice);
  		AddToOut(Order.LmtPriceOffset);
	  	AddToOut(Order.AdjustedStopPrice);
  		AddToOut(Order.AdjustedStopLimitPrice);
	  	AddToOut(Order.AdjustedTrailingAmount);
  		AddToOut(Order.AdjustableTrailingUnit);
    end;
	if FServerVersion >= MIN_SERVER_VER_EXT_OPERATOR then  //105
		AddToOut(Order.ExtOperator);
	if FServerVersion >= MIN_SERVER_VER_SOFT_DOLLAR_TIER then //106
    begin
  		AddToOut(Order.SoftDollarTier.Name);
  		AddToOut(Order.SoftDollarTier.Value);
	  end;

	if (FServerVersion >= MIN_SERVER_VER_CASH_QTY) then // 111

    begin

      if Order.CashQuantity = 0 then

        AddToOut('')

      else

        AddToOutNullValueDef(Order.CashQuantity);

    end;



  if (FServerVersion >= MIN_SERVER_VER_DECISION_MAKER) then

    begin
      AddToOut(Order.mifid2DecisionMaker);
      AddToOut(Order.mifid2DecisionAlgo);
    end;
  if (FServerVersion >= MIN_SERVER_VER_MIFID_EXECUTION) then
    begin
      AddToOut(Order.mifid2ExecutionTrader);
      AddToOut(Order.mifid2ExecutionAlgo);
    end;
  if (FServerVersion >= MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE) then
    AddToOut(Order.DontUseAutoPriceForHedge);
  if (FServerVersion >= MIN_SERVER_VER_ORDER_CONTAINER) then
    AddToOut(Order.IsOmsContainer);
  if (FServerVersion >= MIN_SERVER_VER_D_PEG_ORDERS) then
    AddToOut(Order.DiscretionaryUpToLimitPrice);
  if (FServerVersion >= MIN_SERVER_VER_PRICE_MGMT_ALGO) then
    AddToOutNullValueDef(Order.UsePriceMgmtAlgo);
  if (FServerVersion >= MIN_SERVER_VER_DURATION) then

    AddToOutNullValueDef(Order.Duration);
  if (FServerVersion >= MIN_SERVER_VER_POST_TO_ATS) then

    AddToOutNullValueDef(Order.PostToATS);

  if (FServerVersion >= MIN_SERVER_VER_AUTO_CANCEL_PARENT) then

    AddToOut(Order.AutoCancelparent);


  if (FServerVersion >= MIN_SERVER_VER_ADVANCED_ORDER_REJECT) then

    AddToOut(Order.AdvancedErrorOverride);
  if (FServerVersion >= MIN_SERVER_VER_MANUAL_ORDER_TIME) then
    begin
      if (Order.manualOrderTime = 0.0) then
        AddToOut('')
      else
        AddToOut(FormatDateTime('yyyymmdd hh":"nn":"ss', Order.manualOrderTime));
    end;
  if (FServerVersion >= MIN_SERVER_VER_PEGBEST_PEGMID_OFFSETS) then
    begin
      if (Uppercase(Order.Exchange) = 'IBKRATS') then
        AddToOutNullValueDef(Order.MinTradeQty);
      SendMidOffsets := false;
      if (Order.OrderType = otPegBest) then
        begin
          AddToOutNullValueDef(Order.minCompeteSize);
          AddToOutNullValueDef(Order.competeAgainstBestOffset);
          //if (Order.CompeteAgainstBestOffset = COMPETE_AGAINST_BEST_OFFSET_UP_TO_MID) then //Infinity
          if IsInfinite(Order.CompeteAgainstBestOffset) then
            SendMidOffsets := true;
        end
      else if (Order.OrderType = otPegMidPt) then
        SendMidOffsets := true;
      if (SendMidOffsets) then
        begin
          AddToOutNullValueDef(Order.midOffsetAtWhole);
          AddToOutNullValueDef(Order.midOffsetAtHalf);
        end;
    end;



  SendToSocket(Id);
end;

procedure TIABSocket.RebuildFromTWS;
var ExecFilter: TIABExecutionFilter;
begin
  FRebuildFromTWS := true;
  FOrders.Free;         // delete all prior details
  FOrders := TIABOrders.Create;
  FPortfolio.Free;       // delete all prior details
  FPortfolio := TIABPortfolio.Create;
  with ExecFilter do
    begin
      ClientId := FClientId;
      AccountCode := '';
      FromTime := Date;
      Symbol := '';
      SecurityType := stAll;
      Exchange := '';
      Action := iabIdle;
    end;
  GetExecutions(ExecFilter);
  GetOpenOrdersClient;
end;

procedure TIABSocket.RequestScan(ScanId: Integer; Criteria: TIABScanCriteria);
const VERSION = 4;
var i: Integer; stv: string;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(ScanId, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(REQ_SCANNER_SUBSCRIPTION);
  if FServerVersion < MIN_SERVER_VER_SCANNER_GENERIC_OPTS then  //  143
    AddToOut(VERSION);
  AddToOut(ScanId);
  with Criteria do
    begin
      AddToOutNullValueDef(numberOfRows);
      AddToOut(Instrument);
      AddToOut(LocationCode);
      AddToOut(ScanCode);
      AddToOutNullValueDef(AbovePrice);
      AddToOutNullValueDef(BelowPrice);
      AddToOutNullValueDef(AboveVolume);
      AddToOutNullValueDef(MarketCapAbove);
      AddToOutNullValueDef(MarketCapBelow);
      AddToOut(MoodyRatingAbove);
      AddToOut(MoodyRatingBelow);
      AddToOut(SPRatingAbove);
      AddToOut(SPRatingBelow);
      AddToOut(MaturityDateAbove);
      AddToOut(MaturityDateBelow);
      AddToOutNullValueDef(CouponRateAbove);
      AddToOutNullValueDef(CouponRateBelow);
      AddToOutNullValueDef(ExcludeConvertible);
      AddToOutNullValueDef(AverageOptionVolumeAbove);  // FServerVersion 25
      AddToOut(ScannerSettingPairs);  // FServerVersion 25
      AddToOut(StockTypeFilter);  // FServerVersion 27

      if FServerVersion >= MIN_SERVER_VER_SCANNER_GENERIC_OPTS then  //  143
        begin
          stv := '';
          for i := 0 to High(FilterOptions) do
            stv := stv + FilterOptions[i].Tag + '=' + FilterOptions[i].Value + ';';
          AddToOut(stv);
        end;

      if FServerVersion >= MIN_SERVER_VER_LINKING then  //  70
        begin
          stv := '';
          for i := 0 to High(SubscriptionOptions) do
            stv := stv + SubscriptionOptions[i].Tag + '=' + SubscriptionOptions[i].Value + ';';
          AddToOut(stv);
        end;
    end;
  SendToSocket(ScanId);
end;

procedure TIABSocket.SendReserveId(Value: Integer);
const VERSION = 1;
begin
  AddToOut(REQ_IDS);
  AddToOut(VERSION);
  AddToOut(Value);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.ReceiveFromSocket;
var DecodeSuccess: Boolean;
begin
  DecodeSuccess := false;
  try
    if FConnecting then Initialize
    else DecodeSuccess := DecodeData;
  except on EIABSocket do
    begin
      DoErrorEvent(NO_VALID_ID, FAIL_STREAM_READ);
      Connected := false;
    end;
  end;
  if DecodeSuccess and Assigned(FOnEndOfStreamRead) then FOnEndOfStreamRead(Self);
end;


{$IFDEF CAPTURE_TWS_STREAM}
procedure AppendStreamOut(FOutStream: TMemoryStream);
var f: Text; st, data: AnsiString;
begin
  st := AnsiString(TimeToStr(Now) + ' : ');
  SetLength(data, FOutStream.Size);
  Move(FOutStream.Memory^, data[1], FOutStream.Size);
  AssignFile(f, 'CrntStreamOut.txt');
  if FileExists('CrntStreamOut.txt') then
    begin
      Reset(f);
      Append(f);
    end
  else
    ReWrite(f);
  WriteLn(f, st + data);
  CloseFile(f);
end;
{$ENDIF}

procedure TIABSocket.SendToSocket(Id: Integer);
type pu_long = ^u_long;
var SockStream: TMemoryStream; pul: pu_long; netlen: u_long;
begin
  if FSocket.Active then
    begin
{$IFNDEF LINUX}
      if GainSocketMutex then
        begin
{$ENDIF}
          if FClientv100plus then
            begin
              pul := FOutStream.Memory;
              if pul^ = 0 then
                begin
                  netlen := htonl(u_long(FOutStream.Position) - HEADER_LEN);
                  pul^ := netlen;
                end;
            end;

          SockStream := TMemoryStream.Create;
          try

            {$IFDEF CAPTURE_TWS_STREAM}
              AppendStreamOut(FOutStream);
            {$ENDIF}

            SockStream.Write(FOutStream.Memory^,FOutStream.Position);
            SockStream.Position := 0;
            if FSocket.SendStream(SockStream) = 0 then FOutStream.Position := 0
            else DoErrorEvent(Id, FAIL_STREAM_WRITE);
          finally
            SockStream.Free;
          end;
{$IFNDEF LINUX}
          ReleaseMutex(FMutex);
        end;
{$ENDIF}
    end
  else DoErrorEvent(Id, NOT_CONNECTED);
end;

procedure TIABSocket.SetClientID(const Value: Integer);
begin
  if not FSocket.Active then FClientID := Value;
end;

function TIABSocket.GetPriorVerMaxVerNo: Integer;
begin  // to demote this API to a lower version. In case newest TWS API is not reliable
  case FClientMaxVerOverride of
    972: Result := 106;
    973: Result := 129;
    976: Result := 151;
    1010: Result := 164;
  else
    Result := MAX_CLIENT_VER;
  end;
end;

procedure TIABSocket.SetConnected(State: Boolean);
var SockError: Boolean; verstr: AnsiString; size, netlen: u_long;
begin
  if csDesigning in ComponentState then
    begin
      FConnected := State;
      Exit;
    end;
  FConnected := false;
  FConnecting := false;
  if State and FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, ALREADY_CONNECTED);
      FSocketThread.Terminate;
      FSocket.Disconnect;
      FSocketThread.WaitFor;
      FSocketThread.Free;
      FSocketThread := nil;
      Exit;
    end;
  if FSocket.Active then FSocket.Disconnect;
  FOutStream.Position := 0;
  FInStream.Position := 0;
  if State then
    begin
      FServerVersion := 1;
      FOrders.FServerVersion := 1;
      FConnectAtServerTime := '';
      FillChar(FInStream.Memory^,FInStream.Size,$FF);  // for testing to come in initialize
      if TWSHostAddress = '' then TWSHostAddress := '127.0.0.1';
      if TWSPort = 0 then TWSPort := 7496;
      FConnecting := true;
      if Assigned(FOnConnectionState) then FOnConnectionState(Self,twsConnecting);
{$IFNDEF LINUX}
      if FMutex = 0 then FMutex := CreateMutex(nil,LongBool(0),pChar(MUTEX_NAME));  // D2009 - no change req'd
{$ENDIF}
      FSocket.Connect;
      FSocket.Select(nil,nil,@SockError,0);
      if SockError then
        begin
          DoErrorEvent(NO_VALID_ID, CONNECT_FAIL);
          FSocket.Active := false;
          if Assigned(FOnConnectionState) then FOnConnectionState(Self,twsFailed);
          Exit;
        end;
      FConnected := FSocket.Connected;
      if FConnected then
        begin
         // the sock thread does the receive only - all send
         // data done in context of this TIABSocket owner thread.
          FSocketThread := TIABSocketThread.Create(Self);
          if FClientv100plus then
            begin
              FOutStream.Clear;
              FOutStream.Write(API_SIGN[0],HEADER_LEN);
              if FClientMaxVerOverride > 0 then
                verstr := AnsiString(Format('v%d..%d', [MIN_CLIENT_VER, GetPriorVerMaxVerNo]))
		          else
                verstr := AnsiString(Format('v%d..%d', [MIN_CLIENT_VER, MAX_CLIENT_VER]));
              if Length(FConnectionOptions) > 0 then
                verstr := verstr + AnsiString(FConnectionOptions);
              size := Length(verstr);
              netlen := htonl(size);
              FOutStream.Write(netlen, HEADER_LEN);
              if size > 0 then FOutStream.Write(verstr[1], size);
            end
          else
            AddToOut(CLIENT_VERSION);   // no longer supported
          SendToSocket(NO_VALID_ID);
        end;
    end;
end;

procedure TIABSocket.SetReserveIds(const Value: Integer);
const VERSION = 1;
begin
  FReserveIds := Value;
  if (csLoading in Self.ComponentState) or (csDesigning in Self.ComponentState) then Exit;
  SendReserveId(Value);
end;

procedure TIABSocket.SetSocketHostAddress(const Value: string);
begin
  if not FSocket.Active then
    FSocket.RemoteHost := Value;
end;

procedure TIABSocket.SetSocketPort(const Value: Integer);
begin
  if not FSocket.Active then
    FSocket.RemotePort := IntToStr(Value);
end;

procedure TIABSocket.SocketConnect(Sender: TObject );
begin
//  for code after sucessful socket creation, but not yet accepted by user in TWS
//  we do the    if Assigned(FOnConnectionState) then FOnConnectionState(Sender,twsReady);
//  after the user accepts connection
end;

procedure TIABSocket.SocketDisconnect(Sender: TObject);
begin
  if Assigned(FOnConnectionState) then FOnConnectionState(Sender,twsClosed);
end;

procedure TIABSocket.SocketError(Sender: TObject; SocketError: Integer);
begin
  if not FConnected then Exit;
  if SocketError <> 10004 {WSAEINTR} then
    if Assigned(FOnConnectionState) then FOnConnectionState(Self,twsFailed);
end;

procedure TIABSocket.CancelNewsBulletins;
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(CANCEL_NEWS_BULLETINS);
  AddToOut(VERSION);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.GetNewsBulletins(AllMessages: Boolean);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(REQ_NEWS_BULLETINS);
  AddToOut(VERSION);
  AddToOut(AllMessages);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.SetServerLogLevel(LogLevel: Integer);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(SET_SERVER_LOGLEVEL);
  AddToOut(VERSION);
  AddToOut(LogLevel);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.ReplaceFA(DataId: Integer; FADataType: TIABFADataType; XmlValue: string);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(REPLACE_FA);
  AddToOut(VERSION);
  AddToOut(Ord(FADataType) + 1);
  AddToOut(XmlValue);
  if FServerVersion >= MIN_SERVER_VER_REPLACE_FA_END then
    AddToOut(DataId);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestCompletedOrders(ApiOnly: Boolean);
begin

      //  ****  warning - BUG in TWS stream - it does not deliver as per the sample code.
      //  the  COMPLETED_ORDER processing fails - se below;
      //  this event not called or used to avoid crash.   Note: GetExecutions does the same job anyway.


  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  if FServerVersion < MIN_SERVER_VER_COMPLETED_ORDERS then  // 150
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' completed orders request not supported.');
      Exit;
    end;
  AddToOut(REQ_COMPLETED_ORDERS);
  AddToOut(ApiOnly);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestFA(FADataType: TIABFADataType);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  AddToOut(REQ_FA);
  AddToOut(VERSION);
  AddToOut(Ord(FADataType) + 1);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestGlobalCancel;
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  if FServerVersion < MIN_SERVER_VER_REQ_GLOBAL_CANCEL then
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS);
      Exit;
    end;
  AddToOut(REQ_GLOBAL_CANCEL);
  AddToOut(VERSION);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestMarketDataType(DataType: TIABMarketDataType);
const VERSION = 1;
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
  if FServerVersion < MIN_SERVER_VER_REQ_MARKET_DATA_TYPE then
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS);
      Exit;
    end;
  AddToOut(REQ_MARKET_DATA_TYPE);
  AddToOut(VERSION);
  AddToOut(Ord(DataType));
  SendToSocket(NO_VALID_ID);
end;


procedure TIABSocket.RequestSecDefOptParams(DataId: Integer; UnderlyingSymbol, FutFopExchange, UnderlyingSecType: string; UnderlyingConId: Integer);
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
	if FServerVersion < MIN_SERVER_VER_SEC_DEF_OPT_PARAMS_REQ then // 104
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' Orders with security definiton option requests not supported.');
      Exit;
    end;
  AddToOut(REQ_SEC_DEF_OPT_PARAMS);
// MISSING the VERSION control.
  AddToOut(DataId);
  AddToOut(UnderlyingSymbol);
  AddToOut(FutFopExchange);
  AddToOut(UnderlyingSecType);
  AddToOut(UnderlyingConId);
  SendToSocket(NO_VALID_ID);
end;
procedure TIABSocket.RequestSoftDollarTiers(DataId: Integer);
begin
  if not FSocket.Active then
    begin
      DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
      Exit;
    end;
	if FServerVersion < MIN_SERVER_VER_SOFT_DOLLAR_TIER then // 106
    begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' SoftDollarTiers requests not supported.');
      Exit;
    end;
	AddToOut(REQ_SOFT_DOLLAR_TIERS);
  AddToOut(DataId);
  SendToSocket(NO_VALID_ID);
end;

//procedure TIABSocket.RequestWSHorizonEventsData(DataID, ConID: Integer);
procedure TIABSocket.RequestWSHorizonEventsData(DataID: Integer; WSHRequestSpecs: TIABWSHorizonEventData);
begin
	if not FSocket.Active then
		begin
		  DoErrorEvent(DataId, NOT_CONNECTED);
		  Exit;
	  end;
	if (FServerVersion < MIN_SERVER_VER_WSHE_CALENDAR) then
	  begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support WSHE Calendar API.');
		  Exit;
	  end;

  if (FServerVersion < MIN_SERVER_VER_WSH_EVENT_DATA_FILTERS) then
    begin
      if ((WSHRequestSpecs.filter <> '') or WSHRequestSpecs.fillWatchlist or WSHRequestSpecs.fillPortfolio or WSHRequestSpecs.fillCompetitors) then
        begin
          DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support WSHorizon event data filters.');
	        Exit;
        end;
    end;
  if (FServerVersion < MIN_SERVER_VER_WSH_EVENT_DATA_FILTERS_DATE) then
    begin
      if ((WSHRequestSpecs.filter <> '') or (WSHRequestSpecs.endDate <> '') or (WSHRequestSpecs.totalLimit <> UNSET_INTEGER)) then
        begin
          DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support WSHorizon event data date filters.');
        Exit;
        end;
    end;
  AddToOut(REQ_WSH_EVENT_DATA);
  AddToOut(DataId);
  AddToOut(WSHRequestSpecs.ConId);

  if (FServerVersion >= MIN_SERVER_VER_WSH_EVENT_DATA_FILTERS) then
    begin
      AddToOut(WSHRequestSpecs.Filter);
      AddToOut(WSHRequestSpecs.FillWatchList);
      AddToOut(WSHRequestSpecs.FillPortfolio);
      AddToOut(WSHRequestSpecs.FillCompetitors);
    end;
  if (FServerVersion >= MIN_SERVER_VER_WSH_EVENT_DATA_FILTERS_DATE) then
    begin
      AddToOut(WSHRequestSpecs.StartDate);
      AddToOut(WSHRequestSpecs.EndDate);
      AddToOut(WSHRequestSpecs.TotalLimit);
    end;

  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestWSHorizonMetaData(DataID: Integer);
begin
	if not FSocket.Active then
		begin
		  DoErrorEvent(DataId, NOT_CONNECTED);
		  Exit;
	  end;
	if (FServerVersion < MIN_SERVER_VER_WSHE_CALENDAR) then
	  begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support WSHE Calendar API.');
		  Exit;
	  end;

  AddToOut(REQ_WSH_META_DATA);
  AddToOut(DataId);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestMatchingSymbols(DataId: Integer; Pattern:string);
begin
	if not FSocket.Active then
		begin
		  DoErrorEvent(DataId, NOT_CONNECTED);
		  Exit;
	  end;

	if (FServerVersion < MIN_SERVER_VER_REQ_MATCHING_SYMBOLS) then
	  begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support matching symbols requests.');
		  Exit;
	  end;

	AddToOut(REQ_MATCHING_SYMBOLS);
	AddToOut(DataId);
	AddToOut(pattern);
	SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestMktDepthExchanges;
begin
	if not FSocket.Active then
		begin
		  DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
		  Exit;
	  end;

	if (FServerVersion < MIN_SERVER_VER_REQ_MKT_DEPTH_EXCHANGES) then 
  	begin
		  DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support market depth exchanges requests.');
	  	Exit;
  	end;
	AddToOut(REQ_MKT_DEPTH_EXCHANGES);
	SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestSmartComponents(DataId: Integer; bboExchange: string);
begin
	if not FSocket.Active then
		begin
		  DoErrorEvent(DataId, NOT_CONNECTED);
		  Exit;
	  end;

	if (FServerVersion < MIN_SERVER_VER_REQ_SMART_COMPONENTS) then
  	begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support smart components request.');
		  Exit;
  	end;

	AddToOut(REQ_SMART_COMPONENTS);
	AddToOut(DataId);
	AddToOut(bboExchange);
	SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestNewsProviders;
begin
	if not FSocket.Active then
		begin
	  	DoErrorEvent(NO_VALID_ID, NOT_CONNECTED);
		  Exit;
	  end;

	if (FServerVersion < MIN_SERVER_VER_REQ_NEWS_PROVIDERS) then 
	  begin
		  DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support news providers requests.');
  		Exit;
  	end;
	AddToOut(REQ_NEWS_PROVIDERS);
	SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestNewsArticle(DataId: Integer; ProviderCode, ArticleId, NewsArticleOptions: string);
begin
	if not FSocket.Active then
		begin
	  	DoErrorEvent(DataId, NOT_CONNECTED);
		  Exit;
	  end;
	if (FServerVersion < MIN_SERVER_VER_REQ_NEWS_ARTICLE) then
  	begin
		  DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support news article requests.');
  		Exit;
  	end;

	AddToOut(REQ_NEWS_ARTICLE);
	AddToOut(DataId);
	AddToOut(ProviderCode);
	AddToOut(ArticleId);

	if (FServerVersion >= MIN_SERVER_VER_NEWS_QUERY_ORIGINS) then
  	begin
      //  string format:   'tag_option=tag_value;tag_option=tag_value;tag_option=tag_value;'
  		AddToOut(NewsArticleOptions);
  	end;
	SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestHistoricalNews(DataId, conId: Integer; ProviderCodes, StartDateTime, EndDateTime: string;
                               TotalResults: Integer; HistoricalNewsOptions: string);
begin
	if not FSocket.Active then
		begin
		  DoErrorEvent(DataId, NOT_CONNECTED);
		  Exit;
	  end;

	if (FServerVersion < MIN_SERVER_VER_REQ_HISTORICAL_NEWS) then
	  begin
		  DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support historical news requests.');
  		Exit;
  	end;

	AddToOut(REQ_HISTORICAL_NEWS);
	AddToOut(DataId);
	AddToOut(conId);
	AddToOut(ProviderCodes);
	AddToOut(StartDateTime);
	AddToOut(EndDateTime);
	AddToOut(TotalResults);

	if (FServerVersion >= MIN_SERVER_VER_NEWS_QUERY_ORIGINS) then
	  begin
      //  string format:   'tag_option=tag_value;tag_option=tag_value;tag_option=tag_value;'
      AddToOut( HistoricalNewsOptions );
  	end;

	SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestHeadTimestamp(TickerId: Integer; InstrumentSpec: TIABInstrumentSpecItem; WhatToShow: string;
                               UseRTH, IncludeExpired, FormatDate: Boolean);
begin
	if not FSocket.Active then
		begin
		DoErrorEvent(TickerId, NOT_CONNECTED);
		Exit;
	end;

	if (FServerVersion < MIN_SERVER_VER_REQ_HEAD_TIMESTAMP) then
	begin
		 DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support head timestamp requests.');
		Exit;
	end;

	AddToOut(REQ_HEAD_TIMESTAMP);
	AddToOut(TickerId);
	AddToOut(InstrumentSpec.ContractId);
	AddToOut(InstrumentSpec.Symbol);
  AddToOut(SecurityTypeString[InstrumentSpec.SecurityType]);
	AddToOut(InstrumentSpec.Expiry);
	AddToOut(InstrumentSpec.Strike);
	AddToOut(RightString[InstrumentSpec.Right]);
	AddToOut(InstrumentSpec.Multiplier);
	AddToOut(InstrumentSpec.Exchange);
	AddToOut(InstrumentSpec.PrimaryExchange);
	AddToOut(InstrumentSpec.Currency);
	AddToOut(InstrumentSpec.LocalSymbol);
	AddToOut(InstrumentSpec.TradingClass);
	AddToOut(IncludeExpired);
	AddToOut(UseRTH);
	AddToOut(WhatToShow);
	AddToOut(FormatDate);
	SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.CancelHeadTimestamp(TickerId: Integer);
begin
	if not FSocket.Active then
		begin
		  DoErrorEvent(TickerId, NOT_CONNECTED);
  		Exit;
	  end;

	if (FServerVersion < MIN_SERVER_VER_CANCEL_HEADTIMESTAMP) then
  	begin
	  	DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support head timestamp requests canceling.');
		  Exit;
  	end;
	AddToOut(CANCEL_HEAD_TIMESTAMP);
	AddToOut(TickerId);
	SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestHistogramData(DataId: Integer; InstrumentSpec: TIABInstrumentSpecItem; UseRTH, IncludeExpired: Boolean; TimePeriod: string);
begin
	if not FSocket.Active then
		begin
		  DoErrorEvent(DataId, NOT_CONNECTED);
		  Exit;
	  end;

	if (FServerVersion < MIN_SERVER_VER_REQ_HISTOGRAM) then 
	  begin
		  DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support histogram requests.');
  		Exit;
  	end;

	AddToOut(REQ_HISTOGRAM_DATA);
	AddToOut(DataId);
	AddToOut(InstrumentSpec.ContractId);
	AddToOut(InstrumentSpec.Symbol);
  AddToOut(SecurityTypeString[InstrumentSpec.SecurityType]);
	AddToOut(InstrumentSpec.Expiry  );
	AddToOut(InstrumentSpec.Strike);
	AddToOut(RightString[InstrumentSpec.Right]);
	AddToOut(InstrumentSpec.Multiplier);
	AddToOut(InstrumentSpec.Exchange);
	AddToOut(InstrumentSpec.PrimaryExchange);
	AddToOut(InstrumentSpec.Currency);
	AddToOut(InstrumentSpec.LocalSymbol);
	AddToOut(InstrumentSpec.TradingClass);
	AddToOut(IncludeExpired);
	AddToOut(UseRTH);
	AddToOut(TimePeriod);

	SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.CancelHistogramData(DataId: Integer);
begin
	if not FSocket.Active then
		begin
  		DoErrorEvent(DataId, NOT_CONNECTED);
	  	Exit;
  	end;

	if (FServerVersion < MIN_SERVER_VER_REQ_HEAD_TIMESTAMP) then
	  begin
		  DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support histogram requests.');
  		Exit;
  	end;

	AddToOut(CANCEL_HISTOGRAM_DATA);
	AddToOut(DataId);
	SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestMarketRule(MarketRuleId: Integer);
begin
	if not FSocket.Active then
		begin
  		DoErrorEvent(MarketRuleId, NOT_CONNECTED);
	  	Exit;
  	end;

	if (FServerVersion < MIN_SERVER_VER_MARKET_RULES) then
	  begin
		  DoErrorEvent(NO_VALID_ID, UPDATE_TWS, '  It does not support market rule requests.');
  		Exit;
  	end;

	AddToOut(REQ_MARKET_RULE);
	AddToOut(MarketRuleId);
	SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestPnL(DataId: Integer; Account, ModelCode: string);
begin
    if not FSocket.Active then
  		begin
        DoErrorEvent(DataId, NOT_CONNECTED);
        Exit;
      end;

    if (FServerVersion < MIN_SERVER_VER_PNL) then
    	begin
          DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' It does not support PnL requests.');
          Exit;
      end;

    AddToOut(REQ_PNL);
    AddToOut(DataId);
    AddToOut(Account);
    AddToOut(ModelCode);
    SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.CancelPnL(DataId: Integer);
begin
    if not FSocket.Active then
  		begin
        DoErrorEvent(DataId, NOT_CONNECTED);
        Exit;
      end;

    if (FServerVersion < MIN_SERVER_VER_PNL) then
    	begin
        DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' It does not support PnL requests.');
        Exit;
      end;

    AddToOut(CANCEL_PNL);
    AddToOut(DataId);
    SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestPnLSingle(DataId: Integer; Account, ModelCode: string; ConId: Integer);
begin
    if not FSocket.Active then
  		begin
        DoErrorEvent(DataId, NOT_CONNECTED);
        Exit;
      end;

    if (FServerVersion < MIN_SERVER_VER_PNL) then
    	begin
        DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' It does not support PnL requests.');
        Exit;
      end;

  AddToOut(REQ_PNL_SINGLE);
  AddToOut(DataId);
  AddToOut(account);
  AddToOut(modelCode);
  AddToOut(conId);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.CancelPnLSingle(DataId: Integer);
begin
  if not FSocket.Active then
		begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_PNL) then
  	begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' It does not support PnL requests.');
      Exit;
    end;
  AddToOut(CANCEL_PNL_SINGLE);
  AddToOut(DataId);
  SendToSocket(NO_VALID_ID);
end;

procedure TIABSocket.RequestUserInfo(DataId: Integer);
begin
  if not FSocket.Active then
		begin
      DoErrorEvent(DataId, NOT_CONNECTED);
      Exit;
    end;
  if (FServerVersion < MIN_SERVER_VER_USER_INFO) then
  	begin
      DoErrorEvent(NO_VALID_ID, UPDATE_TWS, ' It does not support user info requests.');
      Exit;
    end;
  AddToOut(REQ_USER_INFO);
  AddToOut(DataId);
  SendToSocket(NO_VALID_ID);
end;


{ TIABOrder }
function TIABOrder.AddComboLeg(ComboLeg: TIABComboLeg): Integer;
begin
  SetLength(FComboLegs,Length(FComboLegs) + 1);
  Result := High(FComboLegs);
  FComboLegs[Result] := ComboLeg;
end;

procedure TIABOrder.Assign(Source: TPersistent);
begin
  if Source is TIABOrder then
  begin
    FAccount := TIABOrder(Source).FAccount;
    FAction := TIABOrder(Source).FAction;
    FAuxPrice := TIABOrder(Source).FAuxPrice;
    FCurrency := TIABOrder(Source).FCurrency;
    FExchange := TIABOrder(Source).FExchange;
    FExpiry := TIABOrder(Source).FExpiry;
    FLocalSymbol := TIABOrder(Source).FLocalSymbol;
    FOCAgroup := TIABOrder(Source).FOCAgroup;
    FOpenClose := TIABOrder(Source).FOpenClose;
    FOrderRef := TIABOrder(Source).FOrderRef;
    FOrderOrigin := TIABOrder(Source).FOrderOrigin;
    FOrderType := TIABOrder(Source).FOrderType;
    FQuantity := TIABOrder(Source).FQuantity;
    FPrice := TIABOrder(Source).FPrice;
    FRight := TIABOrder(Source).FRight;
    FSecurityType := TIABOrder(Source).FSecurityType;
    FStrike := TIABOrder(Source).FStrike;
    FSymbol := TIABOrder(Source).FSymbol;
    FTimeInForce := TIABOrder(Source).FTimeInForce;
    FTransmit := TIABOrder(Source).FTransmit;
    FCompleted := TIABOrder(Source).FCompleted;
    FTempId := TIABOrder(Source).FTempId;
    FPermId := TIABOrder(Source).FPermId;
    FFilled := TIABOrder(Source).FFilled;
    FRemaining := TIABOrder(Source).FRemaining;
    FFillPrice := TIABOrder(Source).FFillPrice;
    FLatestFillQty := TIABOrder(Source).FLatestFillQty;
    FLatestFillPrice := TIABOrder(Source).FLatestFillPrice;
    FChanged := TIABOrder(Source).FChanged;
    FParentId := TIABOrder(Source).FParentId;
    FBlockOrder := TIABOrder(Source).FBlockOrder;
    FSweepToFill := TIABOrder(Source).FSweepToFill;
    FDisplaySize := TIABOrder(Source).FDisplaySize;
    FTriggerMethod := TIABOrder(Source).FTriggerMethod;
    FExtendedHours := TIABOrder(Source).FExtendedHours;
    FHidden := TIABOrder(Source).FHidden;
    FOnFill := TIABOrder(Source).FOnFill;
    FOnCompleted := TIABOrder(Source).FOnCompleted;
    FExecutions := nil;
    FComboLegs := nil;
    if Length(TIABOrder(Source).FExecutions) > 0 then
      begin
        SetLength(FExecutions,Length(TIABOrder(Source).FExecutions));
        FExecutions := Copy(TIABOrder(Source).FExecutions,0,Length(TIABOrder(Source).FExecutions));
      end;
    if Length(TIABOrder(Source).FComboLegs) > 0 then
      begin
        SetLength(FComboLegs,Length(TIABOrder(Source).FComboLegs));
        FComboLegs := Copy(TIABOrder(Source).FComboLegs,0,Length(TIABOrder(Source).FComboLegs));
      end;
    FClientId := TIABOrder(Source).FClientId;
    //FShareAllocation := TIABOrder(Source).FShareAllocation;   dropped 10.10
    FExternalOrder := TIABOrder(Source).FExternalOrder;
    FDiscretAmount := TIABOrder(Source).FDiscretAmount;
    FGoodAfterTime := TIABOrder(Source).FGoodAfterTime;
    FFAdvGroup := TIABOrder(Source).FFAdvGroup;
    FFAdvProfile := TIABOrder(Source).FFAdvProfile;
    FFAdvMethod := TIABOrder(Source).FFAdvMethod;
    FFAdvPercentage := TIABOrder(Source).FFAdvPercentage;
    FGoodTillDate := TIABOrder(Source).FGoodTillDate;
    FPrimaryExchange := TIABOrder(Source).FPrimaryExchange;
    FMultiplier := TIABOrder(Source).FMultiplier;
    FShortSaleSlot := TIABOrder(Source).FShortSaleSlot;
    FDesignatedLocation := TIABOrder(Source).FDesignatedLocation;
    FOcaMethod := TIABOrder(Source).FOcaMethod;
    //FRegTradingHoursOnly := TIABOrder(Source).FRegTradingHoursOnly;
    FRule80A := TIABOrder(Source).FRule80A;
    FSettlingFirm := TIABOrder(Source).FSettlingFirm;
    FAllOrNone := TIABOrder(Source).FAllOrNone;
    FMinQuantity := TIABOrder(Source).FMinQuantity;
    FPercentOffset := TIABOrder(Source).FPercentOffset;
    //FETradeOnly := TIABOrder(Source).FETradeOnly;        // dropped in TWS API 10.10
    //FFirmQuoteOnly := TIABOrder(Source).FFirmQuoteOnly;  // dropped in TWS API 10.10
    //FNBBOPriceCap := TIABOrder(Source).FNBBOPriceCap;    // dropped in TWS API 10.10
    FAuctionStrategy := TIABOrder(Source).FAuctionStrategy;
    FStartingPrice := TIABOrder(Source).FStartingPrice;
    FStockRefPrice := TIABOrder(Source).FStockRefPrice;
    FDelta := TIABOrder(Source).FDelta;
    FStockRangeLower := TIABOrder(Source).FStockRangeLower;
    FStockRangeUpper := TIABOrder(Source).FStockRangeUpper;
    FOverridePercentageConstraints := TIABOrder(Source).FOverridePercentageConstraints;
    FVolatility := TIABOrder(Source).FVolatility;
    FVolatilityPeriod := TIABOrder(Source).FVolatilityPeriod;
    FDeltaNeutralOrderType := TIABOrder(Source).FDeltaNeutralOrderType;
    FDeltaNeutralAuxPrice := TIABOrder(Source).FDeltaNeutralAuxPrice;
    FContinuousUpdate := TIABOrder(Source).FContinuousUpdate;
    FReferencePrice := TIABOrder(Source).FReferencePrice;
    FTrailStopPrice := TIABOrder(Source).FTrailStopPrice;
    FTrailingPercent := TIABOrder(Source).FTrailingPercent;
    FComboLegsDescrip := TIABOrder(Source).FComboLegsDescrip;
    FEFPBasisPoints := TIABOrder(Source).FEFPBasisPoints;
    FEFPBasisType := TIABOrder(Source).FEFPBasisType;
    FWhyHeld := TIABOrder(Source).FWhyHeld;
//    FScaleNumComponents := TIABOrder(Source).FScaleNumComponents;
//    FScaleComponentSize := TIABOrder(Source).FScaleComponentSize;
    FScaleInitLevelSize := TIABOrder(Source).FScaleInitLevelSize;
    FScaleSubsLevelSize := TIABOrder(Source).FScaleSubsLevelSize;
    FScalePriceIncrement := TIABOrder(Source).FScalePriceIncrement;
    FContractId := TIABOrder(Source).FContractId;
    FClearingAccount := TIABOrder(Source).FClearingAccount;
    FClearingIntent := TIABOrder(Source).FClearingIntent;
    FVerified := TIABOrder(Source).FVerified;
    FQueryResult.Status := TIABOrder(Source).FQueryResult.Status;
    FQueryResult.InitMarginAfter := TIABOrder(Source).FQueryResult.InitMarginAfter;
    FQueryResult.MaintMarginAfter := TIABOrder(Source).FQueryResult.MaintMarginAfter;
    FQueryResult.EquityWithLoanAfter := TIABOrder(Source).FQueryResult.EquityWithLoanAfter;
    FQueryResult.InitMarginChange := TIABOrder(Source).FQueryResult.InitMarginChange;
    FQueryResult.MaintMarginChange := TIABOrder(Source).FQueryResult.MaintMarginChange;
    FQueryResult.EquityWithLoanChange := TIABOrder(Source).FQueryResult.EquityWithLoanChange;
    FQueryResult.InitMarginBefore := TIABOrder(Source).FQueryResult.InitMarginBefore;
    FQueryResult.MaintMarginBefore := TIABOrder(Source).FQueryResult.MaintMarginBefore;
    FQueryResult.EquityWithLoanBefore := TIABOrder(Source).FQueryResult.EquityWithLoanBefore;
    FQueryResult.Commission := TIABOrder(Source).FQueryResult.Commission;
    FQueryResult.MinCommission := TIABOrder(Source).FQueryResult.MinCommission;
    FQueryResult.MaxCommission := TIABOrder(Source).FQueryResult.MaxCommission;
    FQueryResult.CommissionCurrency := TIABOrder(Source).FQueryResult.CommissionCurrency;
    FQueryResult.WarningText := TIABOrder(Source).FQueryResult.WarningText;
    FQueryResult.CompletedTime := TIABOrder(Source).FQueryResult.CompletedTime;
    FQueryResult.CompletedStatus := TIABOrder(Source).FQueryResult.CompletedStatus;
    FDeltaNeutralContractId := TIABOrder(Source).FDeltaNeutralContractId;
    FDeltaNeutralContractDelta := TIABOrder(Source).FDeltaNeutralContractDelta;
    FDeltaNeutralContractPrice := TIABOrder(Source).FDeltaNeutralContractPrice;
    FSecurityID := TIABOrder(Source).FSecurityID;
    FSecurityIDType := TIABOrder(Source).FSecurityIDType;
    FNotHeld := TIABOrder(Source).FNotHeld;
    FDeltaNeutralSettlingFirm := TIABOrder(Source).FDeltaNeutralSettlingFirm;
    FDeltaNeutralClearingAccount := TIABOrder(Source).FDeltaNeutralClearingAccount;
    FDeltaNeutralClearingIntent := TIABOrder(Source).FDeltaNeutralClearingIntent;
    FDeltaNeutralOpenClose := TIABOrder(Source).FDeltaNeutralOpenClose;
    FDeltaNeutralShortSale := TIABOrder(Source).FDeltaNeutralShortSale;
    FDeltaNeutralShortSaleSlot := TIABOrder(Source).FDeltaNeutralShortSaleSlot;
    FDeltaNeutralDesignatedLocation := TIABOrder(Source).FDeltaNeutralDesignatedLocation;
    FTrailingPercent := TIABOrder(Source).FTrailingPercent;
    FScalePriceAdjustValue := TIABOrder(Source).FScalePriceAdjustValue;
    FScalePriceAdjustInterval := TIABOrder(Source).FScalePriceAdjustInterval;
    FScaleProfitOffset := TIABOrder(Source).FScaleProfitOffset;
    FScaleAutoReset := TIABOrder(Source).FScaleAutoReset;
    FScaleInitPosition := TIABOrder(Source).FScaleInitPosition;
    FScaleInitFillQty := TIABOrder(Source).FScaleInitFillQty;
    FScaleRandomPercent := TIABOrder(Source).FScaleRandomPercent;
    FExemptCode := TIABOrder(Source).FExemptCode;
    FOptOutSmartRouting := TIABOrder(Source).FOptOutSmartRouting;
	  FHedgeType := TIABOrder(Source).FHedgeType;
	  FHedgeParam := TIABOrder(Source).FHedgeParam;
    FAlgoStrategy := TIABOrder(Source).FAlgoStrategy;
    FAlgoParams := nil;
	  FSmartComboRoutingParams := nil;
    if Length(TIABOrder(Source).FAlgoParams) > 0 then
      FAlgoParams := Copy(TIABOrder(Source).FAlgoParams);
    if Length(TIABOrder(Source).FSmartComboRoutingParams) > 0 then
      FSmartComboRoutingParams := Copy(TIABOrder(Source).FSmartComboRoutingParams);
    FComboLegPrice := nil;
    if Length(TIABOrder(Source).FComboLegPrice) > 0 then
      FComboLegPrice := Copy(TIABOrder(Source).FComboLegPrice);

    FActiveStartTime := TIABOrder(Source).FActiveStartTime;
    FActiveStopTime := TIABOrder(Source).FActiveStopTime;
    FAlgoId := TIABOrder(Source).FAlgoId;
    FScaleTable := TIABOrder(Source).FScaleTable;
    FTradingClass := TIABOrder(Source).FTradingClass;
    FSolicited := TIABOrder(Source).FSolicited;
    FRandomizeSize := TIABOrder(Source).FRandomizeSize;
    FRandomizePrice := TIABOrder(Source).FRandomizePrice;
    FModelCode := TIABOrder(Source).FModelCode;
    FExtOperator := TIABOrder(Source).FExtOperator;
    FOrderMiscOptions := Copy(TIABOrder(Source).FOrderMiscOptions);
    FMktDataOptions := Copy(TIABOrder(Source).FMktDataOptions);
  	FReferenceContractId := TIABOrder(Source).FReferenceContractId;
	  FPeggedChangeAmount := TIABOrder(Source).FPeggedChangeAmount;
	  FIsPeggedChangeAmountDecrease := TIABOrder(Source).FIsPeggedChangeAmountDecrease;
	  FReferenceChangeAmount := TIABOrder(Source).FReferenceChangeAmount;
	  FReferenceExchangeId := TIABOrder(Source).FReferenceExchangeId;
	  FAdjustedOrderType := TIABOrder(Source).FAdjustedOrderType;
	  FTriggerPrice := TIABOrder(Source).FTriggerPrice;
	  FAdjustedStopPrice := TIABOrder(Source).FAdjustedStopPrice;
	  FAdjustedStopLimitPrice := TIABOrder(Source).FAdjustedStopLimitPrice;
	  FAdjustedTrailingAmount := TIABOrder(Source).FAdjustedTrailingAmount;
	  FAdjustableTrailingUnit := TIABOrder(Source).FAdjustableTrailingUnit;
	  FLmtPriceOffset := TIABOrder(Source).FLmtPriceOffset;
    FSoftDollarTier := TIABOrder(Source).FSoftDollarTier;
    FCashQuantity := TIABOrder(Source).FCashQuantity;
    FMifid2DecisionMaker := TIABOrder(Source).FMifid2DecisionMaker;
    FMifid2DecisionAlgo := TIABOrder(Source).FMifid2DecisionAlgo;
    FMifid2ExecutionTrader := TIABOrder(Source).FMifid2ExecutionTrader;
    FMifid2ExecutionAlgo := TIABOrder(Source).FMifid2ExecutionAlgo;
	  FDontUseAutoPriceForHedge := TIABOrder(Source).FDontUseAutoPriceForHedge;
    FIsOmsContainer := TIABOrder(Source).FIsOmsContainer;
    FDiscretionaryUpToLimitPrice := TIABOrder(Source).FDiscretionaryUpToLimitPrice;
    FAutoCancelDate := TIABOrder(Source).FAutoCancelDate;
    FFilledQuantity := TIABOrder(Source).FFilledQuantity;
    FRefFuturesConId := TIABOrder(Source).FRefFuturesConId;
    FAutoCancelParent := TIABOrder(Source).FAutoCancelParent;
    FShareholder := TIABOrder(Source).FShareholder;
    FImbalanceOnly := TIABOrder(Source).FImbalanceOnly;
    FRouteMarketableToBbo := TIABOrder(Source).FRouteMarketableToBbo;
    FParentPermId := TIABOrder(Source).FParentPermId;
    FUsePriceMgmtAlgo := TIABOrder(Source).FUsePriceMgmtAlgo;
   	FDuration := TIABOrder(Source).FDuration;
    FPostToAts := TIABOrder(Source).FPostToAts;
    FCompletedTime := TIABOrder(Source).FCompletedTime;
    FCompletedStatus := TIABOrder(Source).FCompletedStatus;
    FMarketCapPrice := TIABOrder(Source).FMarketCapPrice;
    Exit;
  end;
  inherited Assign(Source);
end;

constructor TIABOrder.Create;
begin
  inherited Create;
  FTransmit := true;
  FOpenClose := 'O';
  FClientId := -1;
  FMinQuantity := UNSET_INTEGER;
  FPercentOffset := UNSET_DOUBLE;
//  FETradeOnly := true;          // dropped in TWS API 10.10
//  FFirmQuoteOnly := true;       // dropped in TWS API 10.10
//  FNBBOPriceCap := UNSET_DOUBLE;  // dropped in TWS API 10.10
  FStartingPrice := UNSET_DOUBLE;
  FStockRefPrice := UNSET_DOUBLE;
  FDelta := UNSET_DOUBLE;
  FStockRangeLower := UNSET_DOUBLE;
  FStockRangeUpper := UNSET_DOUBLE;
  FVolatility := UNSET_DOUBLE;
  FDeltaNeutralOrderType := otNone;
  FDeltaNeutralAuxPrice := UNSET_DOUBLE;
  FTrailStopPrice := UNSET_DOUBLE;
  FTrailingPercent := UNSET_DOUBLE;
  FEFPBasisPoints := UNSET_DOUBLE;
  FEFPBasisType := UNSET_INTEGER;
  FScaleInitLevelSize := UNSET_INTEGER;
  FScaleSubsLevelSize := UNSET_INTEGER;
  FScalePriceIncrement := UNSET_DOUBLE;
  FQueryResult.Commission := UNSET_DOUBLE;
  FQueryResult.MinCommission := UNSET_DOUBLE;
  FQueryResult.MaxCommission := UNSET_DOUBLE;
  FTrailingPercent := UNSET_DOUBLE;
  FScalePriceAdjustValue := UNSET_DOUBLE;
  FScalePriceAdjustInterval := UNSET_INTEGER;
  FScaleProfitOffset := UNSET_DOUBLE;
  FScaleInitPosition := UNSET_INTEGER;
  FScaleInitFillQty := UNSET_INTEGER;
  FExemptCode := -1;
  FFilledQuantity := UNSET_DECIMAL;//UNSET_DOUBLE;
  FRefFuturesConId := UNSET_INTEGER;
  FParentPermId := UNSET_LONG;
  FUsePriceMgmtAlgo := IAB_PRICE_MGNT_ALGO_DEFAULT;
	FDuration := UNSET_INTEGER;
  FPostToAts := UNSET_INTEGER;
  FReferenceContractId := UNSET_INTEGER;
  FPeggedChangeAmount := UNSET_DOUBLE;
  FReferenceChangeAmount := UNSET_DOUBLE;
  FTriggerPrice := UNSET_DOUBLE;
  FAdjustedStopPrice := UNSET_DOUBLE;
  FAdjustedStopLimitPrice := UNSET_DOUBLE;
  FAdjustableTrailingUnit := UNSET_INTEGER;
  FLmtPriceOffset := UNSET_DOUBLE;

  FMinTradeQty := UNSET_INTEGER;
  FMinCompeteSize := UNSET_INTEGER;
  FCompeteAgainstBestOffset := UNSET_DOUBLE;
  FMidOffsetAtWhole := UNSET_DOUBLE;
  FMidOffsetAtHalf := UNSET_DOUBLE;
end;

procedure TIABOrder.DeleteComboLeg(Index: Integer);
var i: Integer;
begin
  if Index = -1 then FComboLegs := nil
  else if High(FComboLegs) = 0 then FComboLegs := nil
  else if Index = 0 then FComboLegs := Copy(FComboLegs,1,High(FComboLegs))
  else if Index = High(FComboLegs) then FComboLegs := Copy(FComboLegs,0,High(FComboLegs))
  else
    begin
      for i := Index to High(FComboLegs) -1 do FComboLegs[i] := FComboLegs[i + 1];
      FComboLegs := Copy(FComboLegs,0,High(FComboLegs))
    end;
end;

destructor TIABOrder.Destroy;
begin
  FExecutions := nil;
  FComboLegs := nil;
  FAlgoParams := nil;
  FSmartComboRoutingParams := nil;
  FOrderMiscOptions := nil;
  FMktDataOptions := nil;
  FComboLegPrice := nil;
  inherited Destroy;
end;

function TIABOrder.GetComboLegs(Index: Integer): TIABComboLeg;
begin
  Result := FComboLegs[Index];
end;

function TIABOrder.GetComboLegsCount: Integer;
begin
  Result := Length(FComboLegs);
end;

function TIABOrder.GetExecutions(Index: Integer): {$IFDEF BCBCOMPILE} PTIABExecution; {$ELSE} TIABExecution; {$ENDIF}
begin
  {$IFDEF BCBCOMPILE}
  Result := @FExecutions[Index];
  {$ELSE}
  Result := FExecutions[Index];
  {$ENDIF}
end;

function TIABOrder.GetExecutionsCount: Integer;
begin
  Result := Length(FExecutions);
end;

function TIABOrder.GetQueryResult: {$IFDEF BCBCOMPILE} PTIABOrderQueryResult; {$ELSE} TIABOrderQueryResult; {$ENDIF}
begin
  {$IFDEF BCBCOMPILE}
    Result := @FQueryResult;
  {$ELSE}
    Result := FQueryResult;
  {$ENDIF}
end;

procedure TIABOrder.SetComboLegs(Index: Integer; const Value: TIABComboLeg);
begin
  FComboLegs[Index] := Value;
end;

procedure TIABOrder.SetExecutions(Index: Integer; const Value: {$IFDEF BCBCOMPILE} PTIABExecution); {$ELSE} TIABExecution); {$ENDIF}
begin
  {$IFDEF BCBCOMPILE}
  with FExecutions[Index] do begin
    ExecutionId := Value^.ExecutionId;
    Time := Value^.Time;
    AcctNumber := Value^.AcctNumber;
    Exchange := Value^.Exchange;
    Side := Value^.Side;
    Volume := Value^.Volume;
    Price := Value^.Price;
    PermId := Value^.PermId;
    Liquidation := Value^.Liquidation;
    CumulativeQty := Value^.CumulativeQty;
    AveragePrice := Value^.AveragePrice;
    OrderRef := Value^.OrderRef;
    EVRule := Value^.EVRule;
    EVMultiplier := Value^.EVMultiplier;
    ModelCode := Value^.ModelCode;
    LastLiquidity := Value^.LastLiquidity;
  end;
  {$ELSE}
  FExecutions[Index] := Value;
  {$ENDIF}
end;

function TIABOrder.GetAlgoParams(Index: Integer): TIABTagValue;
begin
  Result := FAlgoParams[Index];
end;

procedure TIABOrder.SetAlgoParams(Index: Integer; TagValue: TIABTagValue);
begin
  FAlgoParams[Index] := TagValue;
end;

function TIABOrder.GetAlgoParamsCount: Integer;
begin
  Result := High(FAlgoParams);
end;

function TIABOrder.GetSmartComboRoutingParams(Index: Integer): TIABTagValue;
begin
  Result := FSmartComboRoutingParams[Index];
end;

procedure TIABOrder.SetSmartComboRoutingParams(Index: Integer; TagValue: TIABTagValue);
begin
  FSmartComboRoutingParams[Index] := TagValue;
end;

function TIABOrder.GetSmartComboRoutingParamsCount: Integer;
begin
  Result := High(FSmartComboRoutingParams);
end;

function TIABOrder.GetOrderMiscOptions(Index: Integer): TIABTagValue;
begin
  Result := FOrderMiscOptions[Index];
end;

procedure TIABOrder.SetOrderMiscOptions(Index: Integer; TagValue: TIABTagValue);
begin
  FOrderMiscOptions[Index] := TagValue;
end;

function TIABOrder.GetOrderMiscOptionsCount: Integer;
begin
  Result := High(FOrderMiscOptions);
end;

function TIABOrder.GetMktDataOptions(Index: Integer): TIABTagValue;
begin
  Result := FMktDataOptions[Index];
end;

procedure TIABOrder.SetMktDataOptions(Index: Integer; TagValue: TIABTagValue);
begin
  FMktDataOptions[Index] := TagValue;
end;

function TIABOrder.GetMktDataOptionsCount: Integer;
begin
  Result := High(FMktDataOptions);
end;

function TIABOrder.GetComboLegPrice(Index: Integer): Double;
begin
  Result := FComboLegPrice[Index];
end;

procedure TIABOrder.SetComboLegPrice(Index: Integer; Price: Double);
begin
  FComboLegPrice[Index] := Price;
end;

function TIABOrder.GetComboLegPriceCount: Integer;
begin
  Result := High(FComboLegPrice);
end;

function TIABOrder.AddAlgoParams(TagValue: TIABTagValue): Integer;
begin
  SetLength(FAlgoParams, Length(FAlgoParams) + 1);
  Result := High(FAlgoParams);
  FAlgoParams[Result] := TagValue;
end;

procedure TIABOrder.ClearAlgoParams;
begin
  FAlgoParams := nil;
end;

function TIABOrder.AddSmartComboRoutingParams(TagValue: TIABTagValue): Integer;
begin
  SetLength(FSmartComboRoutingParams, Length(FSmartComboRoutingParams) + 1);
  Result := High(FSmartComboRoutingParams);
  FSmartComboRoutingParams[Result] := TagValue;
end;

procedure TIABOrder.ClearSmartComboRoutingParams;
begin
  FSmartComboRoutingParams := nil;
end;

function TIABOrder.AddOrderMiscOptions(TagValue: TIABTagValue): Integer;
begin
  SetLength(FOrderMiscOptions, Length(FOrderMiscOptions) + 1);
  Result := High(FOrderMiscOptions);
  FOrderMiscOptions[Result] := TagValue;
end;

procedure TIABOrder.ClearOrderMiscOptions;
begin
  FOrderMiscOptions := nil;
end;

function TIABOrder.AddMktDataOptions(TagValue: TIABTagValue): Integer;
begin
  SetLength(FMktDataOptions, Length(FMktDataOptions) + 1);
  Result := High(FMktDataOptions);
  FMktDataOptions[Result] := TagValue;
end;

procedure TIABOrder.ClearMktDataOptions;
begin
  FMktDataOptions := nil;
end;

function TIABOrder.AddComboLegPrice(Price: Double): Integer;
begin
  SetLength(FComboLegPrice, Length(FComboLegPrice) + 1);
  Result := High(FComboLegPrice);
  FComboLegPrice[Result] := Price;
end;

procedure TIABOrder.ClearComboLegPrice;
begin
  FComboLegPrice := nil;
end;

procedure TIABOrder.SetExpiry(Value: string);
begin
  if Length(Value) in [0,6,8] then FExpiry := Value;
end;


{*       all dropped 10.10
procedure TIABOrder.SetShareAllocation(Value: string);
begin
//  if (S_Ver >= 38) and (Value <> '') then
  //  raise EIABDeprecatedItem.Create('property ShareAllocation was Deprecated in API 9.40 onwards.');
  FShareAllocation := Value;
end;

function TIABOrder.GetShareAllocation: string;
begin
//  if S_Ver >= 38 then
  //  raise EIABDeprecatedItem.Create('property ShareAllocation was Deprecated in API 9.40 onwards.');
  Result := FShareAllocation;
end;


procedure TIABOrder.SetRegTradingHoursOnly(Value: Boolean);
begin
  if S_Ver >= 38 then
    raise EIABDeprecatedItem.Create('property RegTradingHoursOnly was Deprecated in API 9.40 onwards  - use property ExtendedHours.');
  FRegTradingHoursOnly := Value;
end;

function TIABOrder.GetRegTradingHoursOnly: Boolean;
begin
  if S_Ver >= 38 then
    raise EIABDeprecatedItem.Create('property RegTradingHoursOnly was Deprecated in API 9.40 onwards - use property ExtendedHours.');
  Result := FRegTradingHoursOnly;
end;
*}

function TIABOrder.GetPropertyDeprecated: Integer;
begin
  raise EIABDeprecatedItem.Create('property was Deprecated in API 9.50 onwards - use alternate properties');
end;

procedure TIABOrder.SetPropertyDeprecated(Value: Integer);
begin
  raise EIABDeprecatedItem.Create('property was Deprecated in API 9.50 onwards - use alternate properties');
end;


{ TIABOrders }

function TIABOrders.Add(TempId, ClientId, PermId: Integer; IABOrder: TIABOrder): Integer;
begin
  IABOrder.FTempId := TempId;
  IABOrder.FClientId := ClientId;
  IABOrder.FPermId := PermId;
  Result := FList.Add(IABOrder);
end;

constructor TIABOrders.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

procedure TIABOrders.Delete(Index: Integer);
begin
  if Index < FList.Count then
    begin
      TIABOrder(FList.Items[Index]).Free;
      FList.Delete(Index);
    end;
end;

destructor TIABOrders.Destroy;
var i: Integer;
begin
  for i := FList.Count -1 downto 0 do TIABOrder(FList.Items[i]).Free;
  FList.Free;
  inherited Destroy;
end;

function TIABOrders.FindDuplicatePermIds(var Index1, Index2: Integer): Boolean;
var i, j: Integer;
begin
  Result := false;
  if GetCount < 2 then Exit;
  for i := 0 to GetCount -1 do
    for j := 0 to GetCount -1 do
      if (i <> j) and (GetItems(i).PermId <> 0) and (GetItems(i).PermId = GetItems(j).PermId) then
        begin
          Index1 := j;  // low
          Index2 := i;  // high
          Result := true;
          Break;
        end;
end;

function TIABOrders.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TIABOrders.GetItems(Index: Integer): TIABOrder;
begin
  if (Index < FList.Count) and (Index > -1) then
    Result := TIABOrder(FList.Items[Index])
  else Result := nil;
end;

function TIABOrders.GetOrder(TempId, ClientId: Integer): TIABOrder;
var i: Integer;
begin
  i := IndexOfTempId(TempId,ClientId);
  if i > -1 then
    Result := TIABOrder(FList.Items[i])
  else Result := nil;
end;

function TIABOrders.IndexOfPermId(PermId: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  for i := FList.Count -1 downto 0 do
    if TIABOrder(FList.Items[i]).FPermId = PermId then
      begin
        Result := i;
        Break;
      end;
end;

function TIABOrders.IndexOfTempId(TempId, ClientId: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  case FServerVersion of
  1..8: for i := FList.Count -1 downto 0 do
          if TIABOrder(FList.Items[i]).TempId = TempId then
            begin
              Result := i;
              Break;
            end;
  else
    for i := FList.Count -1 downto 0 do
      if (TIABOrder(FList.Items[i]).FTempId = TempId) and ((ClientId = -1) or (TIABOrder(FList.Items[i]).FClientId = ClientId)) then
        begin
          Result := i;
          Break;
        end;
  end;
end;

procedure TIABOrders.Insert(Index, TempId, ClientId, PermId: Integer; IABOrder: TIABOrder);
begin
  IABOrder.FTempId := TempId;
  IABOrder.FClientId := ClientId;
  IABOrder.FPermId := PermId;
  FList.Insert(Index,IABOrder);
end;

procedure TIABOrders.SetItems(Index: Integer; const Value: TIABOrder);
var Order: TIABOrder;
begin
  Order := TIABOrder(FList.Items[Index]);
  Order.Assign(Value);
end;

procedure TIABOrders.SetPermId(TempId, ClientId, PermId: Integer);
var i: Integer;
begin
  if IndexOfPermId(PermId) = -1 then
    case FServerVersion of
      1..8:for i := FList.Count -1 downto 0 do
           if TIABOrder(FList.Items[i]).TempId = TempId then
             begin
               TIABOrder(FList.Items[i]).PermId := PermId;
               Break;
             end;
    else
      for i := FList.Count -1 downto 0 do
        if (TIABOrder(FList.Items[i]).TempId = TempId) and (TIABOrder(FList.Items[i]).FClientId = ClientId) then
          begin
            TIABOrder(FList.Items[i]).PermId := PermId;
            Break;
          end;
    end;
end;

function TIABOrders.TempToPermId(TempId, ClientId: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  case FServerVersion of
  1..8: for i := FList.Count -1 downto 0 do
          if TIABOrder(FList.Items[i]).FTempId = TempId then
            begin
              Result := TIABOrder(FList.Items[i]).FPermId;
              Break;
            end;
  else
    for i := FList.Count -1 downto 0 do
      if (TIABOrder(FList.Items[i]).FTempId = TempId) and (TIABOrder(FList.Items[i]).FClientId = ClientId) then
        begin
          Result := TIABOrder(FList.Items[i]).FPermId;
          Break;
        end;
  end;
end;

{ TIABPortfolio }

function TIABPortfolio.Add(IABPortfolioItem: TIABPortfolioItem): Integer;
var PFItem: PTIABPortfolioItem;
begin
  New(PFItem);
  PFItem^ := IABPortfolioItem;
  Result := FList.Add(PFItem);
end;

constructor TIABPortfolio.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

procedure TIABPortfolio.Delete(Index: Integer);
begin
  if Index < FList.Count then
    begin
      Dispose(PTIABPortfolioItem(FList.Items[Index]));
      FList.Delete(Index);
    end;
end;

destructor TIABPortfolio.Destroy;
var i: Integer;
begin
  for i := FList.Count -1 downto 0 do Dispose(PTIABPortfolioItem(FList.Items[i]));
  FList.Free;
  inherited Destroy;
end;

function TIABPortfolio.Find(IABPortfolioItem: TIABPortfolioItem): Integer;
var i: Integer; PFItem: PTIABPortfolioItem;
begin
  Result := -1;
  for i := 0 to FList.Count -1 do
    begin
      PFItem := PTIABPortfolioItem(FList.Items[i]);
      if ((PFItem^.Symbol = IABPortfolioItem.Symbol) and
         (PFItem^.Local = IABPortfolioItem.Local) and
         (PFItem^.SecurityType = IABPortfolioItem.SecurityType) and
//         (PFItem^.Expiry = IABPortfolioItem.Expiry) and
         (Copy(PFItem^.Expiry,1,6) = Copy(IABPortfolioItem.Expiry,1,6)) and
         (PFItem^.Strike = IABPortfolioItem.Strike) and
         (PFItem^.Right = IABPortfolioItem.Right) and
         (PFItem^.Currency = IABPortfolioItem.Currency) and
         (PFItem^.AccountName = IABPortfolioItem.AccountName)) then
           begin
             Result := i;
             Break;
           end;
    end;
end;

function TIABPortfolio.GetCount: Integer;
begin
  Result := FList.Count;
end;

{$IFDEF BCBCOMPILE}
function TIABPortfolio.GetItems(Index: Integer): PTIABPortfolioItem;
begin
  Result := FList.Items[Index];
end;
{$ELSE}
function TIABPortfolio.GetItems(Index: Integer): TIABPortfolioItem;
begin
  Result := PTIABPortfolioItem(FList.Items[Index])^;
end;
{$ENDIF}

procedure TIABPortfolio.Insert(Index: Integer; IABPortfolioItem: TIABPortfolioItem);
var PFItem: PTIABPortfolioItem;
begin
  New(PFItem);
  PFItem^ := IABPortfolioItem;
  FList.Insert(Index, PFItem);
end;

{$IFDEF BCBCOMPILE}
procedure TIABPortfolio.SetItems(Index: Integer; const Value: PTIABPortfolioItem);
begin
  with PTIABPortfolioItem(FList.Items[Index])^ do begin
    Symbol := Value^.Symbol;
    Local := Value^.Local;
    SecurityType := Value^.SecurityType;
    Expiry := Value^.Expiry;
    Strike := Value^.Strike;
    Right := Value^.Right;
    Currency := Value^.Currency;
    Position := Value^.Position;
    MarketPrice := Value^.MarketPrice;
    MarketValue := Value^.MarketValue;
    AverageCost := Value^.AverageCost;
    UnrealizedPNL := Value^.UnrealizedPNL;
    RealizedPNL := Value^.RealizedPNL;
    AccountName := Value^.AccountName;
    Multiplier := Value^.Multiplier;
    PrimaryExchange := Value^.PrimaryExchange;
  end;
end;
{$ELSE}
procedure TIABPortfolio.SetItems(Index: Integer; const Value: TIABPortfolioItem);
begin
  with PTIABPortfolioItem(FList.Items[Index])^ do begin
    Symbol := Value.Symbol;
    Local := Value.Local;
    SecurityType := Value.SecurityType;
    Expiry := Value.Expiry;
    Strike := Value.Strike;
    Right := Value.Right;
    Currency := Value.Currency;
    Position := Value.Position;
    MarketPrice := Value.MarketPrice;
    MarketValue := Value.MarketValue;
    AverageCost := Value.AverageCost;
    UnrealizedPNL := Value.UnrealizedPNL;
    RealizedPNL := Value.RealizedPNL;
    AccountName := Value.AccountName;
    Multiplier := Value.Multiplier;
    PrimaryExchange := Value.PrimaryExchange;
  end;
end;
{$ENDIF}

{ TIABInstrumentSpec }

function TIABInstrumentSpec.Add(IABInstrumentSpecItem: TIABInstrumentSpecItem): Integer;
var ISItem: PTIABInstrumentSpecItem;
begin
  New(ISItem);
  ISItem^ := IABInstrumentSpecItem;
  Result := FList.Add(ISItem);
end;

constructor TIABInstrumentSpec.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

procedure TIABInstrumentSpec.Delete(Index: Integer);
var PInstSpecItem: PTIABInstrumentSpecItem;
begin
  if Index < FList.Count then
    begin
      PInstSpecItem := PTIABInstrumentSpecItem(FList.Items[Index]);
      PInstSpecItem^.SecIdList := nil;
      Dispose(PInstSpecItem);
      FList.Delete(Index);
    end;
end;

destructor TIABInstrumentSpec.Destroy;
var i: Integer;
begin
  for i := FList.Count -1 downto 0 do
    Delete(i);
  FList.Free;
  inherited Destroy;
end;

function TIABInstrumentSpec.Find(IABInstrumentSpecItem: TIABInstrumentSpecItem): Integer;
var i: Integer; ISItem: PTIABInstrumentSpecItem;
begin
  Result := -1;
  for i := 0 to FList.Count -1 do
    begin
      ISItem := PTIABInstrumentSpecItem(FList.Items[i]);
      if ((IABInstrumentSpecItem.DataID > -1) and (ISItem^.DataID = IABInstrumentSpecItem.DataID)) or
         ((IABInstrumentSpecItem.DataID = -1) and (ISItem^.ContractId = IABInstrumentSpecItem.ContractId) and (ISItem^.ContractId <> 0)) then
        begin
          Result := i;
          Break;
        end;
    end;
end;

function TIABInstrumentSpec.GetCount: Integer;
begin
  Result := FList.Count;
end;

{$IFDEF BCBCOMPILE}
function TIABInstrumentSpec.GetItems(Index: Integer): PTIABInstrumentSpecItem;
begin
  Result := FList.Items[Index];
end;
{$ELSE}
function TIABInstrumentSpec.GetItems(Index: Integer): TIABInstrumentSpecItem;
begin
  Result := PTIABInstrumentSpecItem(FList.Items[Index])^;
end;
{$ENDIF}

procedure TIABInstrumentSpec.Insert(Index: Integer; IABInstrumentSpecItem: TIABInstrumentSpecItem);
var ISItem: PTIABInstrumentSpecItem;
begin
  New(ISItem);
  ISItem^ := IABInstrumentSpecItem;
  FList.Insert(Index, ISItem);
end;

{$IFDEF BCBCOMPILE}
procedure TIABInstrumentSpec.SetItems(Index: Integer; const Value: PTIABInstrumentSpecItem);
begin
  with PTIABInstrumentSpecItem(FList.Items[Index])^ do begin
    MarketName := Value^.MarketName;
    TradingClass := Value^.TradingClass;
    ContractId := Value^.ContractId;
    Multiplier := Value^.Multiplier;
    MinimumTick := Value^.MinimumTick;
    OrderTypes := Value^.OrderTypes;
    ValidExchanges := Value^.ValidExchanges;
    Symbol := Value^.Symbol;
    SecurityType := Value^.SecurityType;
    Expiry := Value^.Expiry;
    Strike := Value^.Strike;
    Right := Value^.Right;
    Exchange := Value^.Exchange;
    Currency := Value^.Currency;
    LocalSymbol := Value^.LocalSymbol;
    PriceMagnifier := Value^.PriceMagnifier;
    UnderConId := Value^.UnderConId;
    LongName := Value^.LongName;
    PrimaryExchange := Value^.PrimaryExchange;
    ContractMonth := Value^.ContractMonth;
    Industry := Value^.Industry;
    Category := Value^.Category;
    SubCategory := Value^.SubCategory;
    TimeZoneID := Value^.TimeZoneID;
    TradingHours := Value^.TradingHours;
    LiquidHours := Value^.LiquidHours;
    DataID := Value^.DataID;
    EVRule := Value^.EVRule;
    EVMultiplier := Value^.EVMultiplier;
    SecIdList := Value^.SecIdList;
    AggGroup := Value^.AggGroup;
		UnderSymbol := Value^.UnderSymbol;
		UnderSecType := Value^.UnderSecType;
 		MarketRuleIds := Value^.MarketRuleIds;
    RealExpirationDate := Value^.RealExpirationDate;
    DerivativeSecTypes := Value^.DerivativeSecTypes;
    LastTradeTime := Value^.LastTradeTime;
	  MinSize := Value^.MinSize;
  	SizeIncrement := Value^.SizeIncrement;
  	SuggestedSizeIncrement := Value^.SuggestedSizeIncrement;
  end;
end;
{$ELSE}
procedure TIABInstrumentSpec.SetItems(Index: Integer; const Value: TIABInstrumentSpecItem);
begin
  with PTIABInstrumentSpecItem(FList.Items[Index])^ do begin
    MarketName := Value.MarketName;
    TradingClass := Value.TradingClass;
    ContractId := Value.ContractId;
    Multiplier := Value.Multiplier;
    MinimumTick := Value.MinimumTick;
    OrderTypes := Value.OrderTypes;
    ValidExchanges := Value.ValidExchanges;
    Symbol := Value.Symbol;
    SecurityType := Value.SecurityType;
    Expiry := Value.Expiry;
    Strike := Value.Strike;
    Right := Value.Right;
    Exchange := Value.Exchange;
    Currency := Value.Currency;
    LocalSymbol := Value.LocalSymbol;
    PriceMagnifier := Value.PriceMagnifier;
    UnderConId := Value.UnderConId;
    LongName := Value.LongName;
    PrimaryExchange := Value.PrimaryExchange;
    ContractMonth := Value.ContractMonth;
    Industry := Value.Industry;
    Category := Value.Category;
    SubCategory := Value.SubCategory;
    TimeZoneID := Value.TimeZoneID;
    TradingHours := Value.TradingHours;
    LiquidHours := Value.LiquidHours;
    DataID := Value.DataID;
    EVRule := Value.EVRule;
    EVMultiplier := Value.EVMultiplier;
    SecIdList := Value.SecIdList;
    AggGroup := Value.AggGroup;
		UnderSymbol := Value.UnderSymbol;
		UnderSecType := Value.UnderSecType;
 		MarketRuleIds := Value.MarketRuleIds;
    RealExpirationDate := Value.RealExpirationDate;
    DerivativeSecTypes := Value.DerivativeSecTypes;
    LastTradeTime := Value.LastTradeTime;
	  MinSize := Value.MinSize;
  	SizeIncrement := Value.SizeIncrement;
  	SuggestedSizeIncrement := Value.SuggestedSizeIncrement;
  end;
end;
{$ENDIF}

{ TIABBondSpec }

function TIABBondSpec.Add(IABBondSpecItem: TIABBondSpecItem): Integer;
var BSItem: PTIABBondSpecItem;
begin
  New(BSItem);
  BSItem^ := IABBondSpecItem;
  Result := FList.Add(BSItem);
end;

constructor TIABBondSpec.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

procedure TIABBondSpec.Delete(Index: Integer);
var PBBondSpecItem: PTIABBondSpecItem;
begin
  if Index < FList.Count then
    begin
      PBBondSpecItem := PTIABBondSpecItem(FList.Items[Index]);
      PBBondSpecItem^.SecIdList := nil;
      Dispose(PBBondSpecItem);
      FList.Delete(Index);
    end;
end;

destructor TIABBondSpec.Destroy;
var i: Integer;
begin
  for i := FList.Count -1 downto 0 do
    Delete(i);
  FList.Free;
  inherited Destroy;
end;

function TIABBondSpec.Find(IABBondSpecItem: TIABBondSpecItem): Integer;
var i: Integer; BSItem: PTIABBondSpecItem;
begin
  Result := -1;
  for i := 0 to FList.Count -1 do
    begin
      BSItem := PTIABBondSpecItem(FList.Items[i]);
      if ((IABBondSpecItem.DataID > -1) and (BSItem^.DataID = IABBondSpecItem.DataID)) or
         ((IABBondSpecItem.DataID = -1) and (BSItem^.ContractId = IABBondSpecItem.ContractId) and (BSItem^.ContractId <> 0)) then
        begin
          Result := i;
          Break;
        end;
    end;
end;

function TIABBondSpec.GetCount: Integer;
begin
  Result := FList.Count;
end;

{$IFDEF BCBCOMPILE}
function TIABBondSpec.GetItems(Index: Integer): PTIABBondSpecItem;
begin
  Result := FList.Items[Index];
end;
{$ELSE}
function TIABBondSpec.GetItems(Index: Integer): TIABBondSpecItem;
begin
  Result := PTIABBondSpecItem(FList.Items[Index])^;
end;
{$ENDIF}

procedure TIABBondSpec.Insert(Index: Integer; IABBondSpecItem: TIABBondSpecItem);
var BSItem: PTIABBondSpecItem;
begin
  New(BSItem);
  BSItem^ := IABBondSpecItem;
  FList.Insert(Index, BSItem);
end;

{$IFDEF BCBCOMPILE}
procedure TIABBondSpec.SetItems(Index: Integer; const Value: PTIABBondSpecItem);
begin
  with PTIABBondSpecItem(FList.Items[Index])^ do begin
    MarketName := Value^.MarketName;
    TradingClass := Value^.TradingClass;
    ContractId := Value^.ContractId;
    MinimumTick := Value^.MinimumTick;
    OrderTypes := Value^.OrderTypes;
    ValidExchanges := Value^.ValidExchanges;
    Symbol := Value^.Symbol;
    SecurityType := Value^.SecurityType;
    Cusip := Value^.Cusip;
    Ratings := Value^.Ratings;
    DescAppend := Value^.DescAppend;
    BondType := Value^.BondType;
    CouponType := Value^.CouponType;
    Callable := Value^.Callable;
    Putable := Value^.Putable;
    Coupon := Value^.Coupon;
    Convertible := Value^.Convertible;
    Maturity := Value^.Maturity;
    IssueDate := Value^.IssueDate;
    Exchange := Value^.Exchange;
    Currency := Value^.Currency;
    NextOptionDate := Value^.NextOptionDate;
    NextOptionType := Value^.NextOptionType;
    NextOptionPartial := Value^.NextOptionPartial;
    Notes := Value^.Notes;
    LongName := Value^.LongName;
    DataID := Value^.DataID;
    EVRule := Value^.EVRule;
    EVMultiplier := Value^.EVMultiplier;
    SecIdList := Value^.SecIdList;
    AggGroup := Value^.AggGroup;
		MarketRuleIds := Value^.MarketRuleIds;
    TimeZoneID := Value^.TimeZoneID;
    LastTradeTime := Value^.LastTradeTime;
	  MinSize := Value^.MinSize;
  	SizeIncrement := Value^.SizeIncrement;
  	SuggestedSizeIncrement := Value^.SuggestedSizeIncrement;
  end;
end;
{$ELSE}
procedure TIABBondSpec.SetItems(Index: Integer; const Value: TIABBondSpecItem);
begin
  with PTIABBondSpecItem(FList.Items[Index])^ do begin
    MarketName := Value.MarketName;
    TradingClass := Value.TradingClass;
    ContractId := Value.ContractId;
    MinimumTick := Value.MinimumTick;
    OrderTypes := Value.OrderTypes;
    ValidExchanges := Value.ValidExchanges;
    Symbol := Value.Symbol;
    SecurityType := Value.SecurityType;
    Cusip := Value.Cusip;
    Ratings := Value.Ratings;
    DescAppend := Value.DescAppend;
    BondType := Value.BondType;
    CouponType := Value.CouponType;
    Callable := Value.Callable;
    Putable := Value.Putable;
    Coupon := Value.Coupon;
    Convertible := Value.Convertible;
    Maturity := Value.Maturity;
    IssueDate := Value.IssueDate;
    Exchange := Value.Exchange;
    Currency := Value.Currency;
    NextOptionDate := Value.NextOptionDate;
    NextOptionType := Value.NextOptionType;
    NextOptionPartial := Value.NextOptionPartial;
    Notes := Value.Notes;
    LongName := Value.LongName;
    DataID := Value.DataID;
    EVRule := Value.EVRule;
    EVMultiplier := Value.EVMultiplier;
    SecIdList := Value.SecIdList;
    AggGroup := Value.AggGroup;
		MarketRuleIds := Value.MarketRuleIds;
    TimeZoneID := Value.TimeZoneID;
    LastTradeTime := Value.LastTradeTime;
	  MinSize := Value.MinSize;
  	SizeIncrement := Value.SizeIncrement;
  	SuggestedSizeIncrement := Value.SuggestedSizeIncrement;
  end;
end;
{$ENDIF}


{ TIABScan }

function TIABScan.Add(IABScanResultItem: TIABScanResultItem): Integer;
var i: Integer;
begin
  i := Length(FScanItems);
  SetLength(FScanItems,i + 1);
  FScanItems[i] := IABScanResultItem;
  Result := i;
end;

constructor TIABScan.Create;
begin
  inherited Create;
end;

procedure TIABScan.Delete(Index: Integer);
var i: Integer;
begin
  if Index > High(FScanItems) then Exit;
  if (Index < High(FScanItems)) then
    for i := Index to High(FScanItems) -1 do
      FScanItems[i] := FScanItems[i + 1];
  SetLength(FScanItems,High(FScanItems))
end;

destructor TIABScan.Destroy;
begin
  FScanItems := nil;
  inherited Destroy;
end;

function TIABScan.Find(IABScanResultItem: TIABScanResultItem): Integer;
var i: Integer; ScanItem: TIABScanResultItem;
begin
  Result := -1;
  for i := 0 to Length(FScanItems) -1 do
    begin
      ScanItem := FScanItems[i];
      if ((ScanItem.Symbol = IABScanResultItem.Symbol) and
         (ScanItem.LocalSymbol = IABScanResultItem.LocalSymbol) and
//         (ScanItem.Expiry = IABScanResultItem.Expiry))
         (Copy(ScanItem.Expiry,1,6) = Copy(IABScanResultItem.Expiry,1,6)))
         then
           begin
             Result := i;
             Break;
           end;
    end;
end;

function TIABScan.GetCount: Integer;
begin
  Result := Length(FScanItems);
end;

function TIABScan.QueryCriteria: TIABScanCriteria;
begin
  Result := FQueryCriteria;
end;

function TIABScan.GetItems(Index: Integer): {$IFDEF BCBCOMPILE} PTIABScanResultItem; {$ELSE} TIABScanResultItem; {$ENDIF}
begin
  {$IFDEF BCBCOMPILE}
  Result := @FScanItems[Index];
  {$ELSE}
  Result := FScanItems[Index];
  {$ENDIF}
end;

procedure TIABScan.Insert(Index: Integer; IABScanResultItem: TIABScanResultItem);
var i: Integer;
begin
  if Index > High(FScanItems) then Add(IABScanResultItem)
  else
    begin
      i := Length(FScanItems);
      SetLength(FScanItems,i + 1);
      for i := High(FScanItems) -1 downto Index do
        FScanItems[i] := FScanItems[i + 1];
      FScanItems[Index] := IABScanResultItem;
    end;
end;

procedure TIABScan.SetItems(Index: Integer; const Value: {$IFDEF BCBCOMPILE} PTIABScanResultItem); {$ELSE} TIABScanResultItem); {$ENDIF}
begin
  {$IFDEF BCBCOMPILE}
  with FScanItems[Index] do
    begin
      Rank := Value^.Rank;
      Symbol := Value^.Symbol;
      SecurityType := Value^.SecurityType;
      Expiry := Value^.Expiry;
      Strike := Value^.Strike;
      Right := Value^.Right;
      Exchange := Value^.Exchange;
      Currency := Value^.Currency;
      LocalSymbol := Value^.LocalSymbol;
      MarketName := Value^.MarketName;
      TradingClass := Value^.TradingClass;
      Distance := Value^.Distance;
      Benchmark := Value^.Benchmark;
      Projection := Value^.Projection;
      LegsString := Value^.LegsString;
      ContractId := Value^.ContractId;
    end;
  {$ELSE}
  FScanItems[Index] := Value;
  {$ENDIF}
end;


{ TIABScanner }

function TIABScanner.Add(IABScan: TIABScan): Integer;
begin
  Result := FList.Add(IABScan);
end;

constructor TIABScanner.Create(Creator: TIABSocket);
begin
  inherited Create;
  FIABSocket := Creator;
  FList := TList.Create;
end;

procedure TIABScanner.Delete(Index: Integer);
begin
  if Index < FList.Count then
    begin
      TIABScan(FList.Items[Index]).FQueryCriteria.SubscriptionOptions := nil;
      TIABScan(FList.Items[Index]).FQueryCriteria.FilterOptions := nil;
      TIABScan(FList.Items[Index]).Free;
      FList.Delete(Index);
    end;
end;

destructor TIABScanner.Destroy;
var i: Integer;
begin
  for i := FList.Count -1 downto 0 do
    Delete(i);
  FList.Free;
  inherited Destroy;
end;

function TIABScanner.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TIABScanner.GetItems(Index: Integer): TIABScan;
begin
  if Index < FList.Count then
    Result := TIABScan(FList.Items[Index])
  else Result := nil;
end;

procedure TIABScanner.GetScannerParameters;
begin
  FIABSocket.GetScannerParameters;
end;

function TIABScanner.IndexOfScanId(ScanId: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  for i := FList.Count -1 downto 0 do
    if TIABScan(FList.Items[i]).FScanId = ScanId then
      begin
        Result := i;
        Break;
      end;
end;

procedure TIABScanner.InitializeScanCriteria(PDetails: PTIABScanCriteria);
begin
  if PDetails = nil then Exit;
  with PDetails^ do
    begin
      NumberOfRows := -1;
      AbovePrice := UNSET_DOUBLE;
      BelowPrice := UNSET_DOUBLE;
      AboveVolume := UNSET_INTEGER;
      MarketCapAbove := UNSET_DOUBLE;
      MarketCapBelow := UNSET_DOUBLE;
      CouponRateAbove := UNSET_DOUBLE;
      CouponRateBelow := UNSET_DOUBLE;
      ExcludeConvertible := UNSET_INTEGER; // 10.10
      AverageOptionVolumeAbove := UNSET_INTEGER;   // 10.10

      // extras
      Instrument := '';
      LocationCode := '';
      ScanCode := '';
      MoodyRatingAbove := '';
      MoodyRatingBelow := '';
      SPRatingAbove := '';
      SPRatingBelow := '';
      MaturityDateAbove := '';
      MaturityDateBelow := '';
      ScannerSettingPairs := '';
      StockTypeFilter := 'ALL';
      SubscriptionOptions := nil;
      FilterOptions := nil;
    end;
end;

procedure TIABScanner.Insert(Index: Integer; IABScan: TIABScan);
begin
  FList.Insert(Index,IABScan);
end;

procedure TIABScanner.CancelScan(ScanId: Integer);
begin
  FIABSocket.CancelScan(ScanId);
end;

function TIABScanner.NewScan(ScanId: Integer; Details: TIABScanCriteria): Integer;
var ScanItem: TIABScan;
begin
  ScanItem := TIABScan.Create;
  ScanItem.FQueryCriteria := Details;
  ScanItem.FScanId := ScanId;
  FIABSocket.RequestScan(ScanId, ScanItem.FQueryCriteria);
  Result := Add(ScanItem);
end;


{ TIABSocketThread }

constructor TIABSocketThread.Create(Creator: TIABSocket);
begin
  inherited Create(true);
  FIABSocket := Creator;
  Suspended := false;
end;

procedure TIABSocketThread.Execute;
const ReadBlockSize = 2048;
var SizeRead: Integer; p: Pointer;
begin
  repeat
    repeat
      if FIABSocket.FInStream.Position + ReadBlockSize >= FIABSocket.FInStream.Size then
        FIABSocket.FInStream.SetSize((((FIABSocket.FInStream.Position + ReadBlockSize) div 8192) + 1) * 8192);
      p := Pointer(LongWord(FIABSocket.FInStream.Position) + NativeUInt(FIABSocket.FInStream.Memory));
      SizeRead := FIABSocket.FSocket.ReceiveBuf(p^,ReadBlockSize);   // blocks and waits here
      FIABSocket.FInStream.Position := FIABSocket.FInStream.Position + SizeRead;
    until not FIABSocket.FSocket.WaitForData(0);
    if not FIABSocket.FSocket.Connected or Terminated or (SizeRead = 0) then Break;
    if IsConsole then
      SyncdRecieve
    else
      Synchronize(SyncdRecieve);
  until false;
  if (SizeRead = 0) and not Terminated then Synchronize(SyncdClosed);
end;

procedure TIABSocketThread.SyncdRecieve;
begin
  FIABSocket.ReceiveFromSocket;
end;

procedure TIABSocketThread.SyncdClosed;
begin
  FIABSocket.SetConnected(false);
end;



initialization
  IsMultiThread := true;

finalization

end.

