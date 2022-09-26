unit IABFunctions.Helpers;

interface

{$REGION 'Region uses'}
uses
  System.SysUtils, System.Classes, System.Threading, System.Generics.Collections, System.Generics.Defaults, VCL.Forms,
  System.Diagnostics, IABSocketAPI_const, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  Common.Types, System.DateUtils;
{$ENDREGION}

type
  TAdvancedOrderType = (atNone, atHedging, atBracket);
  TAdvancedOrderTypeHelper = record helper for TAdvancedOrderType
  private const
    AdvancedOrderTypeString: array [TAdvancedOrderType] of string = ('NONE', 'HEDGING', 'BRACKET ORDERS');
  public
    function ToString: string;
  end;

  TIABTickTypeHelper = record helper for TIABTickType
  private const
    TickTypeString: array [TIABTickType] of string = ('BidSize','Bid','Ask','AskSize','Last','LastSize','High','Low','Volume',
      'Close','BidOptionComp','AskOptionComp','LastOptionComp','ModelOption','Open','Low13Week','High13Week','Low26Week','High26Week',
      'Low52Week','High52Week','AvgVolume','OpenInterest','OptionHistoricalVol','OptionImpliedVol','OptionBidExch','OptionAskExch',
      'OptionCallOpenInterest','OptionPutOpenInterest','OptionCallVolume','OptionPutVolume','IndexFuturePremium','BidExch','AskExch',
      'AuctionVolume','AuctionPrice','AuctionImbalance','MarkPrice','BidEFPComp','AskEFPComp','LastEFPComp','OpenEFPComp','HighEFPComp',
      'LowEFPComp','CloseEFPComp','LastTimeStamp','Shortable','FundamentalRatios','RtVolume/vwap','Halted','BidYeild','AskYeild','LastYeild',
      'CustomOptionComp','TradeCount','TradeRate','VolumeRate','LastRTHTrade','RtHistoricalVol','IbDividends','BondFactorMultiplier',
      'RegulatoryImbalance','NewsTick','ShortTermVolume3Min','ShortTermVolume5Min','ShortTermVolume10Min','DelayedBid','DelayedAsk',
      'DelayedLast','DelayedBidSize','DelayedAskSize','DelayedLastSize','DelayedHigh','DelayedLow','DelayedVolume','DelayedClose',
      'DelayedOpen','RtTrdVolume','CreditmanMarkPrice','CreditmanSlowMarkPrice','DelayedBidOptionComputation','DelayedAskOptionComputation',
      'DelayedLastOptionComputation','DelayedModelOptionComputation','LastExch','LastRegTime','FuturesOpenInterest','AvgOptVolume',
      'DelayedLastTimestamp','ShortableShares', 'DelayedHalted', 'Reuters2MutualFunds', 'ETFNavClose', 'ETFNavPrior', 'ETFNavBid',
      'ETFNavAsk', 'ETFNavLast', 'ETFNavFrozenLast', 'ETFNavHigh', 'ETFNavLow', 'NotSet', 'MotherFilledPrice', 'Exchange', 'Market');

    TickTypeFormat: array [TIABTickType] of string = ('%.0f'{ttBidSize},'%0.2f'{ttBid},'%0.2f'{ttAsk},'%.0f'{ttAskSize},'%0.2f'{ttLast},
      '%.0f'{ttLastSize},'%0.2f'{ttHigh},'%0.2f'{ttLow},'%.0f'{ttVolume},'%0.2f'{ttClose},'%0.3f'{ttBidOptionComp},'%0.3f'{ttAskOptionComp},
      '%0.3f'{ttLastOptionComp},'%0.3f'{ttModelOption},'%0.2f'{ttOpen},'%0.2f'{ttLow13Week},'%0.2f'{ttHigh13Week},'%0.2f'{ttLow26Week},
      '%0.2f'{ttHigh26Week},'%0.2f'{ttLow52Week},'%0.2f'{ttHigh52Week},'%.0f'{ttAvgVolume},'%0.2f'{ttOpenInterest},'%.0f'{ttOptionHistoricalVol},
      '%.0f'{ttOptionImpliedVol},'%0.2f'{ttOptionBidExch},'%0.2f'{ttOptionAskExch},'%0.2f'{ttOptionCallOpenInterest},'%0.2f'{ttOptionPutOpenInterest},
      '%.0f'{ttOptionCallVolume},'%.0f'{ttOptionPutVolume},'%0.2f'{ttIndexFuturePremium},'%0.2f'{ttBidExch},'%0.2f'{ttAskExch},'%.0f'{ttAuctionVolume},
      '%0.2f'{ttAuctionPrice},'%0.2f'{ttAuctionImbalance},'%0.2f'{ttMarkPrice},'%0.2f'{ttBidEFPComp},'%0.2f'{ttAskEFPComp},'%0.2f'{ttLastEFPComp},
      '%0.2f'{ttOpenEFPComp},'%0.2f'{ttHighEFPComp},'%0.2f'{ttLowEFPComp},'%0.2f'{ttCloseEFPComp},'%0.2f'{ttLastTimeStamp},'%0.2f'{ttShortable},
      '%0.2f'{ttFundamentalRatios},'%.0f'{ttRtVolume},'%0.2f'{ttHalted},'%0.2f'{ttBidYeild},'%0.2f'{ttAskYeild},'%0.2f'{ttLastYeild},'%0.4f'{ttCustomOptionComp},
      '%0.2f'{ttTradeCount},'%0.2f'{ttTradeRate},'%.0f'{ttVolumeRate},'%0.2f'{ttLastRTHTrade},'%.0f'{ttRtHistoricalVol},'%0.2f'{ttIbDividends},
      '%0.2f'{ttBondFactorMultiplier},'%0.2f'{ttRegulatoryImbalance},'%0.2f'{ttNewsTick},'%.0f'{ttShortTermVolume3Min},'%.0f'{ttShortTermVolume5Min},
      '%.0f'{ttShortTermVolume10Min},'%0.2f'{ttDelayedBid},'%0.2f'{ttDelayedAsk},'%0.2f'{ttDelayedLast},'%.0f'{ttDelayedBidSize},'%.0f'{ttDelayedAskSize},
      '%.0f'{ttDelayedLastSize},'%0.2f'{ttDelayedHigh},'%0.2f'{ttDelayedLow},'%.0f'{ttDelayedVolume},'%0.2f'{ttDelayedClose},'%0.2f'{ttDelayedOpen},
      '%.0f'{ttRtTrdVolume},'%0.2f'{ttCreditmanMarkPrice},'%0.2f'{ttCreditmanSlowMarkPrice},'%0.2f'{ttDelayedBidOptionComputation},
      '%0.2f'{ttDelayedAskOptionComputation},'%0.2f'{ttDelayedLastOptionComputation},'%0.2f'{ttDelayedModelOptionComputation},'%0.2f'{ttLastExch},
      '%0.2f'{ttLastRegTime},'%0.2f'{ttFuturesOpenInterest},'%.0f'{ttAvgOptVolume},'%0.2f'{ttDelayedLastTimestamp},'%0.2f'{ttShortableShares},
      '%0.2f'{ttDelayedHalted}, '%0.2f'{ttReuters2MutualFunds}, '%0.2f'{ttETFNavClose}, '%0.2f'{ttETFNavPrior}, '%0.2f'{ttETFNavBid}, '%0.2f'{ttETFNavAsk},
      '%0.2f'{ttETFNavLast}, '%0.2f'{ttETFNavFrozenLast}, '%0.2f'{ttETFNavHigh}, '%0.2f'{ttETFNavLow},
      '%0.2f'{ttNotSet},'%0.2f'{ttMotherFilledPrice},'%0.2f'{ttExchange},'%0.2f'{ttMarket});
  public
    function ToString: string;
    function ToFormat: string;
  end;

  TIABOrderTypeHelper = record helper for TIABOrderType
  private const
    OrderTypeString: array [TIABOrderType] of string = ('NO CHANGE', 'MKT', 'LMT', 'STP', 'STP LMT', 'PASSSIVE RELATIVE (PASV REL)',
      'VWAP', 'MARKET ON CLOSE (MOC)', 'LIMIT ON CLOSE (LMT)', 'TRAILING STOP (TRAIL)', 'LIMIT ON OPEN (LMT)', 'MARKET ON OPEN (MKT)',
      'ONE CANCEL OTHER', 'BLOCK (LMT)', 'PEGGED TO MARKET (PEG MKT)', 'PEGGED TO STOCK (PEG STK)', 'PEGGED TO MIDPOINT (PEGMID)',
      'PEGGED TO BENCHMARK (PEG BENCH)', 'PEGGED TO PRIMARY (REL)', 'PEGGED TO BEST (PEG BEST)', 'VOLATILITY (VOL)', 'TRAILING STOP LIMIT (TRAIL LIMIT)', 'SCALE',
      'MARKET IF TOUCHED (MIT)', 'LIMIT IF TOUCHED (LIT)', 'MARKET TO LIMIT (MTL)', 'AUCTION (MTL)', 'AUCTION RELATIVE (REL)',
      'AUCTION LIMIT (LMT)', 'AUCTION PEGGED TO STOCK (PEG STK)', 'SWEEP TO FILL (LMT)', 'DISCRETIONARY (LMT)', 'BOX TOP (BOX TOP)',
      'MARKET WITH PROTECTION (MKT PRT)', 'STOP WITH PROTECTION (STP PRT)', 'COMBO LIMIT (LMT)', 'COMBO MARKET (MARKET)',
      'COMBO LIMIT WITH PRICE PER LEG (LMT)', 'RELATIVE LIMIT COMBO (REL + LMT)', 'RELATIVE MARKET COMBO (REL + MKT)', 'NONE', 'UNKNOWN');
    OrderProductString: array [TIABOrderType] of string = (
      '',                                                    //'NO CHANGE'
      'BOND, CFD, EFP, CASH, FUND, FUT, FOP, OPT, STK, WAR', //'MKT'
      'BOND, CFD, CASH, FUT, FOP, OPT, STK, WAR',            //'LMT'
      'CFD, BAG, CASH, FUT, FOP, OPT, STK, WAR',             //'STP'
      'CFD, CASH, FUT, FOP, OPT, STK, WAR',                  //'STP LMT'
      '',                                                    //'PASSSIVE RELATIVE (PASV REL)'
      '',                                                    //'VWAP',
      'CFD, FUT, STK, WAR',                                  //'MARKET ON CLOSE (MOC)'
      'CFD, FUT, STK, WAR',                                  //'LIMIT ON CLOSE (LMT)'
      'CFD, CASH, FOP, FUT, OPT, STK, WAR',                  //'TRAILING STOP (TRAIL)'
      'CFD, STK, OPT, WAR',                                  //'LIMIT ON OPEN (LMT)'
      'CFD, FUT, STK, WAR',                                  //'MARKET ON OPEN (MKT)'
      '',                                                    //'ONE CANCEL OTHER'
      'OPT',                                                 //'BLOCK (LMT)'
      'STK',                                                 //'PEGGED TO MARKET (PEG MKT)'
      'OPT',                                                 //'PEGGED TO STOCK (PEG STK)'
      'STK',                                                 //'PEGGED TO MIDPOINT (PEGMID)'
      'STK, OPT',                                            //'PEGGED TO BENCHMARK (PEG BENCH)'
      'CFD, STK, OPT, FUT',                                  //'PEGGED TO PRIMARY (REL)'
      'CFD, STK, OPT, FUT',                                  //'PEGGED TO BEST (PEG BEST)'
      'FOP, OPT',                                            //'VOLATILITY (VOL)'
      'BOND, CFD, CASH, FUT, FOP, OPT, STK, WAR',            //'TRAILING STOP LIMIT (TRAIL LIMIT)'
      '',                                                    //'SCALE'
      'BOND, CFD, CASH, FUT, FOP, OPT, STK, WAR',            //'MARKET IF TOUCHED (MIT)'
      'BOND, CFD, CASH, FUT, FOP, OPT, STK, WAR',            //'LIMIT IF TOUCHED (LIT)'
      'CFD, FUT, FOP, OPT, STK, WAR',                        //'MARKET TO LIMIT (MTL)'
      'FUT, STK',                                            //'AUCTION (MTL)'
      'OPT (BOX only)',                                      //'AUCTION RELATIVE (REL)'
      'OPT (BOX only)',                                      //'AUCTION LIMIT (LMT)'
      'OPT (BOX only)',                                      //'AUCTION PEGGED TO STOCK (PEG STK)'
      'CFD, STK, WAR (SMART only)',                          //'SWEEP TO FILL (LMT)'
      'STK',                                                 //'DISCRETIONARY (LMT)'
      'OPT (BOX only)',                                      //'BOX TOP (BOX TOP)'
      'FUT, FOP',                                            //'MARKET WITH PROTECTION (MKT PRT)'
      'FUT',                                                 //'STOP WITH PROTECTION (STP PRT)'
      'OPT, STK, FUT',                                       //'COMBO LIMIT (LMT)'
      'OPT, STK, FUT',                                       //'COMBO MARKET (MARKET)'
      'OPT, STK, FUT',                                       //'COMBO LIMIT WITH PRICE PER LEG (LMT)'
      'OPT, STK, FUT',                                       //'RELATIVE LIMIT COMBO (REL + LMT)'
      'OPT, STK, FUT',                                       //'RELATIVE MARKET COMBO (REL + MKT)'
      '',                                                    //'NONE'
      ''                                                     //'UNKNOWN'
      );
  public
    function ToString: string;
    function ToShortString: string;
    function GetOrderProduct: string;
  end;

  TIABTimeInForceHelper = record helper for TIABTimeInForce
  private const
    TimeInForceString: array [TIABTimeInForce] of string = ('DAY (DAY)','GOOD TILL CANCEL (GTC)', 'IMMEDIATE OR CANCEL (IOC)',
      'LIMIT ON OPEN (OPG)', 'GOOD TILL DATE (GTD)', 'AUCTION (AUC)', 'FILL OR KILL (FOK)', 'DAY ''TIL CANCELLED (DTC)',
      'GOOD AFTER TIME (GAT)', '5Mins', 'UNKNOWN');
  public
    function ToString: string;
  end;

  TIABActionHelper = record helper for TIABAction
  private const
    ActionString: array [TIABAction] of string = ('Idle', 'Buy', 'Sell', 'Short', 'Exercise', 'Lapse');
  public
    function ToString: string;
  end;

  TTriggerMethod = (trDefault = 0, trDoubleBidAsk = 1, trLast = 2, trDoubleLast = 3, trBidAsk = 4, trLastOrBidAsk = 7, trMidPoint = 8);
  TTriggerMethodHelper = record helper for TTriggerMethod
  private const
    TriggerMethodString: array [TTriggerMethod] of string = ('Default', 'Double Bid/Ask', 'Last', 'Double Last', 'Bid/Ask', '', '', ''{Last or Bid/Ask not used}, 'Mid-point');
  public
    function ToString: string;
  end;

  TIABSecurityTypeHelper = record helper for TIABSecurityType
  public
    function ToString: string;
    class function FromString(const aSecType: string): TIABSecurityType; static;
  end;

  TIABOrderStateHelper = record helper for TIABOrderState
  public
    function ToString: string;
  end;

  TIABExMktDataHelper = record helper for TIABExMktData
  private const
    ExMktDataString: array [TIABExMktData] of string = ('Option Volume', 'Option Open Interest', 'Historical Volatility',
      'Option Implied Volatility', 'Index Future Premium', 'Miscellaneous', 'Mark Price PnL', 'Mark Price PnL Auction',
      'Average Option Volume', 'RT Volume', 'Shortable', 'Inventory', 'Fundamental Ratios', 'Trade Count', 'Volume Rate',
      'Short Term Volume', 'Realtime Historical Volatility', 'IB Dividends', 'MarkPrice', 'News', 'Trade Rate', 'Last RTH Trade',
      'RT Trade Volume', 'Bond Factor Multiplier', 'ETF Nav Bid/Ask', 'ETF Nav Last', 'ETF Nav Close', 'Futures Open Interest',
      'ETF Nav High/Low', 'Creditman Slow Mark Price', 'ETF Nav Frozen Last');
  public
    function ToString: string;
    function ToInteger: Integer;
  end;

  TIABExMktDataSetHelper = record helper for TIABExMktDataSet
  public
    function ToString: string;
  end;


implementation

{ TAdvancedOrderTypeHelper }

function TAdvancedOrderTypeHelper.ToString: string;
begin
  Result := AdvancedOrderTypeString[Self];
end;

{ TIABTickTypeHelper }

function TIABTickTypeHelper.ToFormat: string;
begin
  Result := TickTypeFormat[Self];
end;

function TIABTickTypeHelper.ToString: string;
begin
  Result := TickTypeString[Self];
end;

{ TIABOrderTypeHelper }

function TIABOrderTypeHelper.GetOrderProduct: string;
begin
  Result := OrderProductString[Self];
end;

function TIABOrderTypeHelper.ToShortString: string;
begin
   Result := OrderTypeString[Self];
end;

function TIABOrderTypeHelper.ToString: string;
begin
  Result := OrderTypeString[Self];
end;

{ TIABTimeInForceHelper }

function TIABTimeInForceHelper.ToString: string;
begin
  Result := TimeInForceString[Self];
end;

{ TIABActionHelper }

function TIABActionHelper.ToString: string;
begin
  Result := ActionString[Self];
end;

{ TTriggerMethodHelper }

function TTriggerMethodHelper.ToString: string;
begin
  Result := TriggerMethodString[Self];
end;

{ TIABSecurityTypeHelper }

class function TIABSecurityTypeHelper.FromString(const aSecType: string): TIABSecurityType;
begin
  Result := stStock;
  for var st := stStock to stAll do
    if (st.ToString = aSecType) then
      Exit(st);
end;

function TIABSecurityTypeHelper.ToString: string;
begin
  Result := SecurityTypeString[Self];
end;

{ TIABOrderStateHelper }

function TIABOrderStateHelper.ToString: string;
begin
  Result := OrderStateString[Self];
end;


{ TIABExMktDataHelper }

function TIABExMktDataHelper.ToInteger: Integer;
begin
  Result := ExMktDataAsInt[Self];
end;

function TIABExMktDataHelper.ToString: string;
begin
  Result := ExMktDataString[Self];
end;

{ TIABExMktDataSetHelper }

function TIABExMktDataSetHelper.ToString: string;
begin
  for var md in Self do
    Result := Result + md.ToInteger.ToString + ';';
end;

end.
