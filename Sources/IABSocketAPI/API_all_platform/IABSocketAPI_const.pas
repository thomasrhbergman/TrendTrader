unit IABSocketAPI_const;

// use with IABSocketAPI v10.18 and later

interface

uses
{$IF CompilerVersion < 24.0}  // XE3
Math
{$ELSE}
System.Math
{$IFEND}
{$IFDEF USE_BIGDECIMAL}
, Velthuis.BigDecimals
{$ENDIF}
;

// all constants and record type defs moved here Jan 2020, to reduce size of main API file.

const
  TWS_EOV: Byte      = 0;
    // outgoing msg id's
  REQ_MKT_DATA       = 1;
  CANCEL_MKT_DATA    = 2;
  PLACE_ORDER        = 3;
  CANCEL_ORDER       = 4;
  REQ_OPEN_ORDERS    = 5;
  REQ_ACCT_DATA      = 6;
  REQ_EXECUTIONS     = 7;
  REQ_IDS            = 8;
  REQ_CONTRACT_DATA  = 9;
  REQ_MKT_DEPTH      = 10;
  CANCEL_MKT_DEPTH   = 11;
  REQ_NEWS_BULLETINS = 12;
  CANCEL_NEWS_BULLETINS = 13;
  SET_SERVER_LOGLEVEL   = 14;
  REQ_AUTO_OPEN_ORDERS  = 15;
  REQ_ALL_OPEN_ORDERS   = 16;
  REQ_MANAGED_ACCTS     = 17;
  REQ_FA                = 18;
  REPLACE_FA            = 19;
  REQ_HISTORICAL_DATA   = 20;
  EXERCISE_OPTIONS      = 21;
  REQ_SCANNER_SUBSCRIPTION    = 22;
  CANCEL_SCANNER_SUBSCRIPTION = 23;
  REQ_SCANNER_PARAMETERS      = 24;
  CANCEL_HISTORICAL_DATA      = 25;
  REQ_CURRENT_TIME            = 49;
  REQ_REAL_TIME_BARS          = 50;
  CANCEL_REAL_TIME_BARS       = 51;
  REQ_FUNDAMENTAL_DATA        = 52;
  CANCEL_FUNDAMENTAL_DATA     = 53;
  REQ_CALC_IMPLIED_VOLAT      = 54;
  REQ_CALC_OPTION_PRICE       = 55;
  CANCEL_CALC_IMPLIED_VOLAT   = 56;
  CANCEL_CALC_OPTION_PRICE    = 57;
  REQ_GLOBAL_CANCEL           = 58;
  REQ_MARKET_DATA_TYPE        = 59;
  // 60 n/a

  {*

  These function not implemented:
  61-64: for Account managers only
  65-66: unknown
  67-70: for background colors of TWS

  REQ_POSITIONS                 = 61;
  REQ_ACCOUNT_SUMMARY           = 62;
  CANCEL_ACCOUNT_SUMMARY        = 63;
  CANCEL_POSITIONS              = 64;
  VERIFY_REQUEST                = 65;
  VERIFY_MESSAGE                = 66;
  QUERY_DISPLAY_GROUPS          = 67;
  SUBSCRIBE_TO_GROUP_EVENTS     = 68;
  UPDATE_DISPLAY_GROUP          = 69;
  UNSUBSCRIBE_FROM_GROUP_EVENTS = 70;
  *}
  START_API                     = 71;

  {

  VERIFY_AND_AUTH_REQUEST       = 72;

  VERIFY_AND_AUTH_MESSAGE       = 73;
  REQ_POSITIONS_MULTI           = 74;
  CANCEL_POSITIONS_MULTI        = 75;
  REQ_ACCOUNT_UPDATES_MULTI     = 76;
  CANCEL_ACCOUNT_UPDATES_MULTI  = 77;
  }
  REQ_SEC_DEF_OPT_PARAMS		    = 78;
  REQ_SOFT_DOLLAR_TIERS			    = 79;

  REQ_FAMILY_CODES              = 80;

  REQ_MATCHING_SYMBOLS          = 81;
  REQ_MKT_DEPTH_EXCHANGES       = 82;
  REQ_SMART_COMPONENTS          = 83;
  REQ_NEWS_ARTICLE              = 84;
  REQ_NEWS_PROVIDERS            = 85;
  REQ_HISTORICAL_NEWS           = 86;
  REQ_HEAD_TIMESTAMP            = 87;
  REQ_HISTOGRAM_DATA            = 88;
  CANCEL_HISTOGRAM_DATA         = 89;
  CANCEL_HEAD_TIMESTAMP         = 90;
  REQ_MARKET_RULE               = 91;
  REQ_PNL                       = 92;
  CANCEL_PNL                    = 93;
  REQ_PNL_SINGLE                = 94;
  CANCEL_PNL_SINGLE             = 95;

  REQ_HISTORICAL_TICKS          = 96;

  REQ_TICK_BY_TICK_DATA         = 97;
  CANCEL_TICK_BY_TICK_DATA      = 98;
  REQ_COMPLETED_ORDERS          = 99;
  REQ_WSH_META_DATA             = 100;
  CANCEL_WSH_META_DATA          = 101;
  REQ_WSH_EVENT_DATA            = 102;
  CANCEL_WSH_EVENT_DATA         = 103;

  REQ_USER_INFO                 = 104;


  // incoming msg id's
  TICK_PRICE         = 1;
  TICK_SIZE          = 2;
  ORDER_STATUS       = 3;
  ERR_MSG            = 4;
  OPEN_ORDER         = 5;
  ACCT_VALUE         = 6;
  PORTFOLIO_VALUE    = 7;
  ACCT_UPDATE_TIME   = 8;
  NEXT_VALID_ID      = 9;
  CONTRACT_DATA      = 10;
  EXECUTION_DATA     = 11;
  MARKET_DEPTH       = 12;
  MARKET_DEPTH_L2    = 13;
  NEWS_BULLETINS     = 14;
  MANAGED_ACCTS      = 15;
  RECEIVE_FA         = 16;
  HISTORICAL_DATA    = 17;
  BOND_CONTRACT_DATA = 18;
  SCANNER_PARAMETERS = 19;
  SCANNER_DATA       = 20;
  TICK_OPTION_COMPUTATION = 21;
  TICK_GENERIC            = 45;
  TICK_STRING             = 46;
  TICK_EFP                = 47;
  CURRENT_TIME            = 49;
  REAL_TIME_BARS          = 50;
  FUNDAMENTAL_DATA        = 51;
  CONTRACT_DATA_END       = 52;
  OPEN_ORDER_END          = 53;
  ACCT_DOWNLOAD_END       = 54;
  EXECUTION_DATA_END      = 55;
  DELTA_NEUTRAL_VALIDATION = 56;
  TICK_SNAPSHOT_END			  = 57;
  MARKET_DATA_TYPE        = 58;
  COMMISSION_REPORT       = 59;
  // 60 n/a

  {*

  These callbacks not implemented:
  61-64: for Account managers only
  65-66: unknown
  67-68: for background colors of TWS
  *}

  POSITION_DATA             = 61;
  POSITION_END              = 62;
  ACCOUNT_SUMMARY           = 63;
  ACCOUNT_SUMMARY_END       = 64;
  VERIFY_MESSAGE_API        = 65;
  VERIFY_COMPLETED          = 66;
  DISPLAY_GROUP_LIST        = 67;
  DISPLAY_GROUP_UPDATED     = 68;

  VERIFY_AND_AUTH_MESSAGE_API = 69;
  VERIFY_AND_AUTH_COMPLETED   = 70;
  POSITION_MULTI              = 71;
  POSITION_MULTI_END          = 72;
  ACCOUNT_UPDATE_MULTI        = 73;
  ACCOUNT_UPDATE_MULTI_END    = 74;
  SECURITY_DEFINITION_OPTION_PARAMETER      = 75;
  SECURITY_DEFINITION_OPTION_PARAMETER_END  = 76;
  SOFT_DOLLAR_TIERS           = 77;

  FAMILY_CODES                              = 78;
  SYMBOL_SAMPLES                            = 79;
  MKT_DEPTH_EXCHANGES                       = 80;
  TICK_REQ_PARAMS                           = 81;
  SMART_COMPONENTS                          = 82;
  NEWS_ARTICLE                              = 83;
  TICK_NEWS                                 = 84;
  NEWS_PROVIDERS                            = 85;
  HISTORICAL_NEWS                           = 86;
  HISTORICAL_NEWS_END                       = 87;
  HEAD_TIMESTAMP                            = 88;
  HISTOGRAM_DATA                            = 89;
  HISTORICAL_DATA_UPDATE                    = 90;
  REROUTE_MKT_DATA_REQ                      = 91;
  REROUTE_MKT_DEPTH_REQ                     = 92;
  MARKET_RULE                               = 93;
  PNL                                       = 94;
  PNL_SINGLE                                = 95;
  HISTORICAL_TICKS                          = 96;
  HISTORICAL_TICKS_BID_ASK                  = 97;
  HISTORICAL_TICKS_LAST                     = 98;
  TICK_BY_TICK                              = 99;
  ORDER_BOUND                               = 100;
  COMPLETED_ORDER                           = 101;
  COMPLETED_ORDERS_END                      = 102;
  REPLACE_FA_END                            = 103;
  WSH_META_DATA                             = 104;
  WSH_EVENT_DATA                            = 105;

  HISTORICAL_SCHEDULE                       = 106;
  USER_INFO                                 = 107;

  HEADER_LEN = 4; // 4 bytes for msg length
  MAX_MSG_LEN = $00FFFFFF; // 16Mb
  API_SIGN: array [0..3] of AnsiChar = ('A', 'P', 'I', #0);  //  API_SIGN[4] = { 'A', 'P', 'I', '\0' }; // "API"
  HEADER_ZERO: Integer = 0;


  //  version controls for features
  MIN_SERVER_VER_REAL_TIME_BARS         = 34;
  MIN_SERVER_VER_SCALE_ORDERS           = 35;
  MIN_SERVER_VER_SNAPSHOT_MKT_DATA      = 35;
  MIN_SERVER_VER_SSHORT_COMBO_LEGS      = 35;
  MIN_SERVER_VER_WHAT_IF_ORDERS         = 36;
  MIN_SERVER_VER_CONTRACT_CONID         = 37;
  MIN_SERVER_VER_PTA_ORDERS             = 39;
  MIN_SERVER_VER_FUNDAMENTAL_DATA       = 40;
  MIN_SERVER_VER_UNDER_COMP             = 40;
  MIN_SERVER_VER_DELTA_NEUTRAL          = MIN_SERVER_VER_UNDER_COMP;
  MIN_SERVER_VER_CONTRACT_DATA_CHAIN    = 40;
  MIN_SERVER_VER_SCALE_ORDERS2          = 40;
  MIN_SERVER_VER_ALGO_ORDERS            = 41;
  MIN_SERVER_VER_EXECUTION_DATA_CHAIN   = 42;
  MIN_SERVER_VER_NOT_HELD               = 44;
  MIN_SERVER_VER_SEC_ID_TYPE            = 45;
  MIN_SERVER_VER_PLACE_ORDER_CONID      = 46;
  MIN_SERVER_VER_REQ_MKT_DATA_CONID     = 47;
  MIN_SERVER_VER_REQ_CALC_IMPLIED_VOLAT = 49;
  MIN_SERVER_VER_REQ_CALC_OPTION_PRICE  = 50;
  MIN_SERVER_VER_CANCEL_CALC_IMPLIED_VOLAT = 50;
  MIN_SERVER_VER_CANCEL_CALC_OPTION_PRICE  = 50;
  MIN_SERVER_VER_SSHORTX_OLD            = 51;
  MIN_SERVER_VER_SSHORTX                = 52;
  MIN_SERVER_VER_REQ_GLOBAL_CANCEL      = 53;
  MIN_SERVER_VER_HEDGE_ORDERS			      = 54;
  MIN_SERVER_VER_REQ_MARKET_DATA_TYPE	= 55;
  MIN_SERVER_VER_OPT_OUT_SMART_ROUTING  = 56;
  MIN_SERVER_VER_SMART_COMBO_ROUTING_PARAMS = 57;
  MIN_SERVER_VER_DELTA_NEUTRAL_CONID    = 58;
  MIN_SERVER_VER_SCALE_ORDERS3          = 60;
  MIN_SERVER_VER_ORDER_COMBO_LEGS_PRICE = 61;
  MIN_SERVER_VER_TRAILING_PERCENT       = 62;
  MIN_SERVER_VER_DELTA_NEUTRAL_OPEN_CLOSE = 66;
  MIN_SERVER_VER_POSITIONS              = 67;
  MIN_SERVER_VER_ACCOUNT_SUMMARY        = 67;
  MIN_SERVER_VER_TRADING_CLASS          = 68;
  MIN_SERVER_VER_SCALE_TABLE            = 69;
  MIN_SERVER_VER_LINKING                = 70;
  MIN_SERVER_VER_ALGO_ID                = 71;
  MIN_SERVER_VER_OPTIONAL_CAPABILITIES  = 72;
  MIN_SERVER_VER_ORDER_SOLICITED        = 73;
  MIN_SERVER_VER_LINKING_AUTH           = 74;
  MIN_SERVER_VER_PRIMARYEXCH            = 75;
  MIN_SERVER_VER_RANDOMIZE_SIZE_AND_PRICE = 76;

  MIN_SERVER_VER_FRACTIONAL_POSITIONS   = 101;
  MIN_SERVER_VER_PEGGED_TO_BENCHMARK    = 102;
  MIN_SERVER_VER_MODELS_SUPPORT         = 103;
  MIN_SERVER_VER_SEC_DEF_OPT_PARAMS_REQ = 104;
  MIN_SERVER_VER_EXT_OPERATOR	          = 105;
  MIN_SERVER_VER_SOFT_DOLLAR_TIER		    = 106;

  MIN_SERVER_VER_REQ_FAMILY_CODES           = 107;
  MIN_SERVER_VER_REQ_MATCHING_SYMBOLS       = 108;
  MIN_SERVER_VER_PAST_LIMIT                 = 109;
  MIN_SERVER_VER_MD_SIZE_MULTIPLIER         = 110;
  MIN_SERVER_VER_CASH_QTY                   = 111;
  MIN_SERVER_VER_REQ_MKT_DEPTH_EXCHANGES    = 112;
  MIN_SERVER_VER_TICK_NEWS                  = 113;
  MIN_SERVER_VER_REQ_SMART_COMPONENTS       = 114;
  MIN_SERVER_VER_REQ_NEWS_PROVIDERS         = 115;
  MIN_SERVER_VER_REQ_NEWS_ARTICLE           = 116;
  MIN_SERVER_VER_REQ_HISTORICAL_NEWS        = 117;
  MIN_SERVER_VER_REQ_HEAD_TIMESTAMP         = 118;
  MIN_SERVER_VER_REQ_HISTOGRAM              = 119;
  MIN_SERVER_VER_SERVICE_DATA_TYPE          = 120;
  MIN_SERVER_VER_AGG_GROUP                  = 121;
  MIN_SERVER_VER_UNDERLYING_INFO            = 122;
  MIN_SERVER_VER_CANCEL_HEADTIMESTAMP       = 123;
  MIN_SERVER_VER_SYNT_REALTIME_BARS	        = 124;
  MIN_SERVER_VER_CFD_REROUTE                = 125;
  MIN_SERVER_VER_MARKET_RULES               = 126;
  MIN_SERVER_VER_DAILY_PNL                  = 127;
  MIN_SERVER_VER_PNL                        = 127;
  MIN_SERVER_VER_NEWS_QUERY_ORIGINS         = 128;
  MIN_SERVER_VER_UNREALIZED_PNL             = 129;
  MIN_SERVER_VER_HISTORICAL_TICKS           = 130;
  MIN_SERVER_VER_MARKET_CAP_PRICE           = 131;
  MIN_SERVER_VER_PRE_OPEN_BID_ASK           = 132;
  MIN_SERVER_VER_REAL_EXPIRATION_DATE       = 134;
  MIN_SERVER_VER_REALIZED_PNL               = 135;
  MIN_SERVER_VER_LAST_LIQUIDITY             = 136;
  MIN_SERVER_VER_TICK_BY_TICK               = 137;
  MIN_SERVER_VER_DECISION_MAKER             = 138;
  MIN_SERVER_VER_MIFID_EXECUTION            = 139;
  MIN_SERVER_VER_TICK_BY_TICK_IGNORE_SIZE   = 140;
  MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE       = 141;
  MIN_SERVER_VER_WHAT_IF_EXT_FIELDS         = 142;
  MIN_SERVER_VER_SCANNER_GENERIC_OPTS       = 143;
  MIN_SERVER_VER_API_BIND_ORDER             = 144;
  MIN_SERVER_VER_ORDER_CONTAINER            = 145;
  MIN_SERVER_VER_SMART_DEPTH                = 146;
  MIN_SERVER_VER_REMOVE_NULL_ALL_CASTING    = 147;
  MIN_SERVER_VER_D_PEG_ORDERS               = 148;
  MIN_SERVER_VER_MKT_DEPTH_PRIM_EXCHANGE    = 149;
  MIN_SERVER_VER_COMPLETED_ORDERS           = 150;
  MIN_SERVER_VER_PRICE_MGMT_ALGO            = 151;
  MIN_SERVER_VER_STOCK_TYPE                 = 152;
  MIN_SERVER_VER_ENCODE_MSG_ASCII7          = 153;
  MIN_SERVER_VER_SEND_ALL_FAMILY_CODES  		= 154;
  MIN_SERVER_VER_NO_DEFAULT_OPEN_CLOSE	  	= 155;
  MIN_SERVER_VER_PRICE_BASED_VOLATILITY     = 156;
  MIN_SERVER_VER_REPLACE_FA_END             = 157;
  MIN_SERVER_VER_DURATION                   = 158;
  MIN_SERVER_VER_MARKET_DATA_IN_SHARES      = 159;
  MIN_SERVER_VER_POST_TO_ATS                = 160;
  MIN_SERVER_VER_WSHE_CALENDAR              = 161;
  MIN_SERVER_VER_AUTO_CANCEL_PARENT         = 162;
  MIN_SERVER_VER_FRACTIONAL_SIZE_SUPPORT    = 163;
  MIN_SERVER_VER_SIZE_RULES                 = 164;

  MIN_SERVER_VER_HISTORICAL_SCHEDULE        = 165;
  MIN_SERVER_VER_ADVANCED_ORDER_REJECT      = 166;
  MIN_SERVER_VER_USER_INFO                  = 167;
  MIN_SERVER_VER_CRYPTO_AGGREGATED_TRADES   = 168;
  MIN_SERVER_VER_MANUAL_ORDER_TIME          = 169;
  MIN_SERVER_VER_PEGBEST_PEGMID_OFFSETS     = 170;
  MIN_SERVER_VER_WSH_EVENT_DATA_FILTERS     = 171;
  MIN_SERVER_VER_IPO_PRICES                 = 172;
  MIN_SERVER_VER_WSH_EVENT_DATA_FILTERS_DATE = 173;
  MIN_SERVER_VER_INSTRUMENT_TIMEZONE         = 174;


  //  This tells TWS how "new" your API client version is. see TIABSocket.SetConnected
  //  Can be overriden backwards to sidestep any new API server version errors,
  //    but WARNING: the code in this client API is not designed for this.  So any
  //    new fields added / changes for the newest server API version, will be missing, and cause errors as well.
  //  See TIABSocket.GetPriorVerMaxVerNo  ( set the ClientMaxVerOverride property )

  CLIENT_REPORTED_VERSION = MIN_SERVER_VER_INSTRUMENT_TIMEZONE;

  // 100+ messaging
  // 100 = enhanced handshake, msg length prefixes
  MIN_CLIENT_VER = 100;
  MAX_CLIENT_VER = CLIENT_REPORTED_VERSION;


  //  OLD system - no longer relevant (or supported in TWS).
  //  client versioning #
  //MIN_SERVER_VERSION = 38;  // about April 2008
  MIN_SERVER_VER_SUPPORTED = 38;
  CLIENT_VERSION = 63;   // OLD pre 100 version.  See v100+ above


//  7 = 7.0
//  8 = 7.01
//  9 = 7.1, 7.2
//  10 = 7.3,
//  11 to 14 were NOT released to public by IAB.
//  15 = 7.6,
//  16 = 8.0, 8.06, 8.1, 8.12,
//  17 = 8.20, 8.21, 8.30
//  18 = 8.40, 8.41
//  19 = 8.50
//  20 = 8.51, 8.52
//  21 = ?  not released to public
//  22 = 8.60, 8.61
//  23 = 8.71
//  24 = 8.80
//  25 = 8.81 ?? not public released
//  26 = 8.82, 8.83
//  27 = 8.84, 8.85
//  28 = 8.90
//  29 = 8.91
//  30 = 9.00
//  31 = 9.10
//  32 = 9.20
//  33 = 9.30
//  34 to 36 - not released
//  37 = 9.40, 9.41
//  38 = 9.50, 9.51
//  39 to 41 - not released
//  42 = 9.60, 9.61
//  43..47 9.63, 9.64.  (9.64 had 2 versions from IAB)
//  48 to 59 = ?? no APi updates done in this period.
//  60 = 9.68
//  61 = 9.69
//  62 = 9.70
//  63 = 9.71
//  then swapped to 100+ system
//  100..106 = 9.72
//  129 = 9.73.?
//  151 = 9.76.01
//  164 = 10.10.4
//  173 = 10.16.1 and 10.17.1
//  174 = 10.18.1



  // TWS New Bulletins constants
  NEWS_MSG             = 1;    // standard IB news bulleting message
  EXCHANGE_AVAIL_MSG   = 2;    // control message specifing that an exchange is available for trading
  EXCHANGE_UNAVAIL_MSG = 3;    // control message specifing that an exchange is unavailable for trading

  NO_VALID_ID         = -1;
  NO_VALID_ERROR_CODE = 0;
  SYSTEM_ERROR        = 600;


type
  TIABAction = (iabIdle,iabBuy,iabSell,iabShort,iabExercise,iabLapse);
  TIABConnection = (twsClosed,twsConnecting,twsReady,twsFailed);
  TIABOrderOrigin = (orCustomer,orFirm,orUnknown);
  TIABOrderState = (osPendSubmit,osPendCancel,osPreSubmit,osSubmitted,osCancelled,osFilled,{ add Roman }osPartlyFilled,osSleeping,osError,osNotConsidered);
  TIABOrderType = (otNoChange,otMarket,otLimit,otStop,otStopLimit,otPassiveRel,otVWAP,otMarketClose,otLimitClose,otTrail,
                    otLimitOpen,otMarketOpen,otOneCancelOther,otISEBlock,
                  	otPegMarket,otPegStock,otPegMidPt,otPegBench,otPegPrimary,otPegBest,otVolatility,otTrailLimit,otScale,
                    otMarketTouch,otLimitTouch,otMarketToLimit,otAuction,otAuctionRel,otAuctionLimit,otAuctionPegStk, otSweepFill,
                    otDiscretionary,otBoxTop,otMarketwProtect,otStopwProtect,
                    otComboLimit,otComboMarket,otComboLimitLeg,otRelLimitCombo,otRelMktCombo,
                  	otNone,otUnknown);
  TIABRight = (rtNone,rtPut,rtCall);
  TIABSecurityType = (stStock,stOption,stFuture,stIndex,stFutOpt,stCash,stBag,stBond,stIOp,stCFD,stFund,stCmdty,stCrypto,stConFut,stFutConFut,stWar,stNews,stAll);

  TIABTickType = (ttBidSize,ttBid,ttAsk,ttAskSize,ttLast,ttLastSize,ttHigh,ttLow,ttVolume,ttClose,ttBidOptionComp,ttAskOptionComp,ttLastOptionComp,ttModelOption,
                  ttOpen,ttLow13Week,ttHigh13Week,ttLow26Week,ttHigh26Week,ttLow52Week,ttHigh52Week,ttAvgVolume,ttOpenInterest,ttOptionHistoricalVol,ttOptionImpliedVol,
                  ttOptionBidExch,ttOptionAskExch,ttOptionCallOpenInterest,ttOptionPutOpenInterest,ttOptionCallVolume,ttOptionPutVolume,ttIndexFuturePremium,ttBidExch,
                  ttAskExch,ttAuctionVolume,ttAuctionPrice,ttAuctionImbalance,ttMarkPrice,
                  ttBidEFPComp,ttAskEFPComp,ttLastEFPComp,ttOpenEFPComp,ttHighEFPComp,ttLowEFPComp,ttCloseEFPComp,
                  ttLastTimeStamp, ttShortable, ttFundamentalRatios,
                  ttRtVolume, ttHalted, ttBidYeild, ttAskYeild, ttLastYeild, ttCustomOptionComp,
                  ttTradeCount, ttTradeRate, ttVolumeRate, ttLastRTHTrade,
                  ttRtHistoricalVol,ttIbDividends,ttBondFactorMultiplier,ttRegulatoryImbalance,ttNewsTick,
                  ttShortTermVolume3Min,ttShortTermVolume5Min,ttShortTermVolume10Min,
                  ttDelayedBid,ttDelayedAsk,ttDelayedLast,ttDelayedBidSize,ttDelayedAskSize,ttDelayedLastSize,ttDelayedHigh,ttDelayedLow,ttDelayedVolume,ttDelayedClose,ttDelayedOpen,
                  ttRtTrdVolume,ttCreditmanMarkPrice,ttCreditmanSlowMarkPrice,
                  ttDelayedBidOptionComputation,ttDelayedAskOptionComputation,ttDelayedLastOptionComputation,ttDelayedModelOptionComputation,
                  ttLastExch,ttLastRegTime,ttFuturesOpenInterest,
                  ttAvgOptVolume,ttDelayedLastTimestamp,ttShortableShares, ttDelayedHalted, ttReuters2MutualFunds,
                  ttETFNavClose, ttETFNavPrior, ttETFNavBid, ttETFNavAsk, ttETFNavLast, ttETFNavFrozenLast, ttETFNavHigh, ttETFNavLow,
                  ttNotSet,
                  {add Roman}ttMotherFilledPrice,ttExchange,ttMarket{end});

  TIABTimeInForce = (tifDay,tifGTC,tifIOC,tifOPG,tifGTD,tifAUC,tifFillKill,tifDTC,tifGAT,tif5Mins,tifUnknown);
  TIABOrderTypesSet = set of TIABOrderType;
  TIABLegOpenClose = (locSamePos,locOpenPos,locClosePos,locUnknownPos);
  TIABExchangeStatus = (esUnknown,esAvailable,esUnAvailable);
  TIABFADataType = (faGroups,faProfiles,faAliases);
  TIABHistoricalDataType = (cdTrades,cdMidPoint,cdBid,cdAsk,cdBidAsk, // RealTimeData accepts these 5 only. The remainder are for Historical requests.
                          cdAdjustedLast,cdHistoricalVolatility,cdOptionImpVolatility,cdRebateRate,cdFeeRate,
                          cdYeildBid,cdYeildAsk,cdYeildBidAsk,cdYeildLast,cdSchedule);
  TIABChartBarSize = (bs1sec,bs5sec,bs15sec,bs30sec,bs1min,bs2min,bs3min,bs5min,bs15min,bs30min,bs1Hour,bs1Day,bs1Week,bs1Month,bs3Month,bs1Year);
  TIABAuctionStrategy = (asUnset,asMatch,asImprovement,asTransparent);
  TIABOcaMethod = (ocaCancelWithBlock,ocaReduceWithBlock,ocaReduceNonBlock);
  TIABRule80A = (r80na,r80aIndividual,r80aAgency,r80aAgentOtherMember,r80aIndividualPTIA,r80aAgencyPTIA,r80aAgentOtherMemberPTIA,r80aIndividualPT,r80aAgencyPT,r80aAgentOtherMemberPT);
  TIABVolatilityPeriod = (vpUnset,vpDaily,vpAnnual);
  TIABReferencePrice = (rpUnset,rpAverage,rpBidOrAsk);
  TIABExMktData = (emdOptionVolume,emdOptionOpenInterest,emdHistoricalVolatility,emdOptionImpliedVolatility,emdIndexFuturePremium,emdMiscellaneous,emdMarkPricePnL,emdMarkPricePnLAuction,
    {add Roman}emdAverageOptionVolume,emdRTVolume,emdShortable,emdInventory,emdFundamentalRatios,emdTradeCount,emdVolumeRate,emdShortTermVolume,emdRealtimeHistoricalVolatility,emdIBDividends,emdMarkPrice,
               emdNews,emdTradeRate,emdLastRTHTrade,emdRTTradeVolume,emdBondFactorMultiplier,emdETFNavBidAsk,emdETFNavLast,emdETFNavClose,emdFuturesOpenInterest,emdETFNavHighLow,emdCreditmanSlowMarkPrice,
               emdETFNavFrozenLast);
  TIABExMktDataSet = set of TIABExMktData;
  TIABMarketDataType = (mdtUnset, mdtRealTime, mdtFrozen, mdtDelayed, mdtDelayedFrozen, mdtNone);
  TIABHedgeType = (htUnset, htDelta, htBeta, htFX, htPair);

  TIABTickDataType = (tdNone, tdLast, tdAllLast, tdBidAsk, tdMidPoint);

  TIABCodeMsgPair = record
    Code: Integer;
    Msg: string;
  end;
  PTIABCodeMsgPair = ^TIABCodeMsgPair;
  TIABCodeMsgPairArray = array of TIABCodeMsgPair;


const
  ALREADY_CONNECTED: TIABCodeMsgPair = (Code: 501; Msg: 'Already connected - socket reset');
  CONNECT_FAIL: TIABCodeMsgPair = (Code: 502; Msg: 'Couldn''t connect to TWS.  Confirm that "Enable ActiveX and Socket Clients" is enabled on the "Settings" menu.  Default live trade port is 7496, paper trade is 7497.');
  UPDATE_TWS: TIABCodeMsgPair = (Code: 503; Msg: 'The TWS is out of date and must be upgraded.');
  NOT_CONNECTED: TIABCodeMsgPair = (Code: 504; Msg: 'Not connected / connection lost');
  UNKNOWN_ID: TIABCodeMsgPair = (Code: 505; Msg: 'Fatal Error: Unknown message id.');
  UNSUPPORTED_VERSION: TIABCodeMsgPair = (Code: 506; Msg: 'Unsupported version');
  NULL_STRING_READ: TIABCodeMsgPair = (Code: 507; Msg: 'Null string read when expecting integer');
  NO_BYTES_READ: TIABCodeMsgPair = (Code: 508; Msg: 'Error: no bytes read or no null terminator found');
  ZERO_BYTE_READ: TIABCodeMsgPair = (Code: 508; Msg: 'Error: no bytes read or no null terminator found');
  SOCKET_EXCEPTION: TIABCodeMsgPair = (Code: 509; Msg: 'Exception caught while reading socket - ');
  FAIL_CREATE_SOCK: TIABCodeMsgPair = (Code: 520; Msg: 'Failed to create socket');
  FAIL_CONNECT_TWS: TIABCodeMsgPair = (Code: 521; Msg: 'Couldn''t connect to TWS.');
  FAIL_SEND_FA_REQUEST: TIABCodeMsgPair = (Code: 522; Msg: 'FA Information Request Sending Error - ');
  FAIL_SEND_FA_REPLACE: TIABCodeMsgPair = (Code: 523; Msg: 'FA Information Replace Sending Error - ');
  FAIL_SEND_REQSCANNER: TIABCodeMsgPair = (Code: 524; Msg: 'Request Scanner Subscription Sending Error - ');
  FAIL_SEND_CANSCANNER: TIABCodeMsgPair = (Code: 525; Msg: 'Cancel Scanner Subscription Sending Error - ');
  FAIL_SEND_REQSCANNERPARAMETERS: TIABCodeMsgPair = (Code: 526; Msg: 'Request Scanner Parameter Sending Error - ');
  FAIL_SEND_REQHISTDATA: TIABCodeMsgPair = (Code: 527; Msg: 'Request Historical Data Sending Error - ');
  FAIL_SEND_CANHISTDATA: TIABCodeMsgPair = (Code: 528; Msg: 'Cancel Historical Data Sending Error - ');
  FAIL_SEND_REQRTBARS: TIABCodeMsgPair = (Code: 529; Msg: 'Request Real-time Bar Data Sending Error - ');
  FAIL_SEND_CANRTBARS: TIABCodeMsgPair = (Code: 530; Msg: 'Cancel Real-time Bar Data Sending Error - ');
  FAIL_SEND_REQCURRTIME: TIABCodeMsgPair = (Code: 531; Msg: 'Request Current Time Sending Error - ');
  INVALID_SYMBOL: TIABCodeMsgPair = (Code: 579; Msg: 'Invalid symbol in string - ');
  FAIL_STREAM_READ: TIABCodeMsgPair = (Code: 950; Msg: 'Error: Stream read error. Consult TWS for positions.');
  FAIL_STREAM_WRITE: TIABCodeMsgPair = (Code: 951; Msg: 'Error: Stream write error.');
  INVALID_INPUT_DATA: TIABCodeMsgPair = (Code: 960; Msg: 'Invalid input data.');
  FAIL_CONNECT_TWS_REDIRECT: TIABCodeMsgPair = (Code: 999; Msg: 'TWS is requesting a TCP redirect to: ');


{$IFNDEF LINUX}
  MUTEX_NAME = 'TWSsocket';
{$ENDIF}


  OPEN_ORDER_ACCEPT_WINDOW = 30000;  // see the OPEN_ORDER event for this.

  TimeInForceString: array [TIABTimeInForce] of string = ('DAY','GTC','IOC','OPG','GTD','AUC','FOK','DTC','GAT','Minutes','UNKNOWN');
  OrderTypeString: array [TIABOrderType] of string = (
            '','MKT','LMT','STP','STP LMT','PASSV REL','VWAP','MOC','LOC','TRAIL',
            'LMT','MKT','OCA','LMT',
            'PEG MKT','PEG STK','PEG MID','PEG BENCH','REL','PEG BEST','VOL','TRAIL LIMIT','SCALE',
            'MIT','LIT','MTL','MTL','REL','LMT','PEG STK','LMT',
            'LMT','BOX TOP','MKT PRT','STP PRT',
            'LMT','MKT','LMT','REL + LMT','REL + MKT',
            '','UNKNOWN');

  OrderTypeText: array [otMarket..otRelMktCombo] of string = (
      'Market','Limit','Stop','StopLimit','PassiveRel','VWAP','MarketClose','LimitClose','Trail',
      'LimitOpen','MarketOpen','OneCancelOther','ISEBlock',
      'PegMarket','PegStock','PegMidPt','PegBench','PegPrimary','PegBest','Volatility','TrailLimit','Scale',
      'MarketTouch','LimitTouch','MarketToLimit','Auction','AuctionRel','AuctionLimit','AuctionPegStk','SweepFill',
      'Discretionary','BoxTop','MarketwProtect','StopwProtect',
      'ComboLimit','ComboMarket','ComboLimitLeg','RelLimitCombo','RelMktCombo');

  SecurityTypeString: array [TIABSecurityType] of string = (
            'STK','OPT','FUT','IND','FOP','CASH','BAG','BOND','IOPT','CFD','FUND',
            'CMDTY','CRYPTO','CONTFUT','FUT+CONTFUT','WAR','NEWS','ALL');

  ActionString: array [TIABAction] of string = ('','BUY','SELL','SSHORT','EXERCISE','LAPSE');
  RightString: array [TIABRight] of string = ('','PUT','CALL');
  OrderStateString: array [TIABOrderState] of string = ('PendingSubmit','PendingCancel','PreSubmitted','Submitted','Cancelled','Filled',{ add Roman }'PartlyFilled','Sleeping','Error','Not Considered');
  HistoricalDataTypeString: array [TIABHistoricalDataType] of string = (
          'TRADES','MIDPOINT','BID','ASK','BID_ASK',
          'AdjustedLast','HistoricalVolatility','OptionImpVolatility','RebateRate','FeeRate',
          'YeildBid','YeildAsk','YeildBidAsk','YeildLast','Schedule');
  ChartBarSizeString: array [TIABChartBarSize] of string = ('1 secs','5 secs','15 secs','30 secs','1 min','2 mins','3 mins','5 mins','15 mins','30 mins',
                                                            '1 hour','1 day','1 week','1 month','3 months','1 year');
  ChartBarSizeInt: array [TIABChartBarSize] of Integer = (1, 5, 15, 30, 60, 120, 180, 300, 15 * 60, 30 * 60, 60 * 60, 60 * 60 * 24, 60 * 60 * 24 * 7, 60 * 60 * 24 * 30, 60 * 60 * 24 * 90, 60 * 60 * 24 * 200);
  ExMktDataAsInt: array [TIABExMktData] of Integer = (100,101,104,106,162,165,221,225,
                                           {add Roman}105,233,236,256,258,293,295,595,411,456,232,292,294,318,375,460,576,577,578,588,614,619,623);

  TickDataTypeString: array [TIABTickDataType] of string = ('', 'Last', 'AllLast', 'BidAsk', 'MidPoint');

  FutureMonthCode: array [1..12] of Char = ('F','G','H','J','K','M','N','Q','U','V','X','Z');

  OPERATION_INSERT = 0;
  OPERATION_UPDATE = 1;
  OPERATION_DELETE = 2;
  SIDE_ASK = 0;
  SIDE_BID = 1;

  _MaxInt64 = $7FFFFFFFFFFFFFFF;
  _MaxUInt64 = $FFFFFFFFFFFFFFFF;
  _MaxDouble = 1.7976931348623157081e+308;

  (*  The problem of MaxDouble:
    In later XE versions  MaxDouble defined as 1.7e+308; in SysUtils, and  1.7976931348623157081e+308; in Math file;
    In early XE versions  confusion exists in the compiler / unit linking order, about which one to use;
    In later D versions   MaxDouble defined as 1.7e+308; in Math file only;
  *)

  UNSET_DOUBLE = _MaxDouble;
  UNSET_DOUBLE_TEST_VALUE = 1.7e+308;
  UNSET_INTEGER = MaxInt;
  UNSET_LONG = _MaxInt64;

  {$IFDEF USE_BIGDECIMAL}
  UNSET_DECIMAL = MaxInt;
  {$ELSE}
  UNSET_DECIMAL = UNSET_DOUBLE;
  {$ENDIF}

  IAB_TIME_UNIT_YEAR = 4;
  IAB_TIME_UNIT_MONTH = 3;
  IAB_TIME_UNIT_WEEK = 2;
  IAB_TIME_UNIT_DAY = 1;
  IAB_TIME_UNIT_SEC = 0;

  TIABFADataTypeStr: array [TIABFADataType] of string = ('GROUPS','PROFILES','ALIASES');
  TIABRule80AStr: array [TIABRule80A] of string = ('','I','A','W','J','U','M','K','Y','N');
  TIABHedgeTypeStr: array [TIABHedgeType] of string = ('', 'D', 'B', 'FX', 'P');

  IAB_PRICE_MGNT_ALGO_DONOTUSE = 0;
  IAB_PRICE_MGNT_ALGO_USE      = 1;
  IAB_PRICE_MGNT_ALGO_DEFAULT = UNSET_INTEGER;

  _INFINITY = Infinity;
  INFINITY_STR = 'Infinity';
  COMPETE_AGAINST_BEST_OFFSET_UP_TO_MID = Infinity; // _INFINITY;

type
  {$IFNDEF USE_BIGDECIMAL}
  BigDecimal = Double;
  {$ENDIF}

  {$IF CompilerVersion < 23.0}  // 23 = XE2 (first 64bit version)
  NativeUInt = LongWord;
  NativeInt = Integer;
  {$IFEND}

  TIABExecutionFilter = record
    ClientId: Integer;
    AccountCode: string;
    FromTime: TDateTime;
    Symbol: string;
    SecurityType: TIABSecurityType;
    Exchange: string;
    Action: TIABAction;
  end;
  PTIABExecutionFilter = ^TIABExecutionFilter;

  TIABComboLeg = record
    ContractId: Integer;
    Action: TIABAction;
    Ratio: Integer;
    Exchange: string;
    OpenClose: TIABLegOpenClose;
    ShortSaleSlot: Integer;
    DesignatedLocation: string;
    ExemptCode: Integer;  // init to -1
  end;
  PTIABComboLeg = ^TIABComboLeg;
  TIABComboLegArray = array of TIABComboLeg;


  TIABTagValue = record
    Tag, Value: string;
  end;
  PTIABTagValue = ^TIABTagValue;
  TIABTagValueArray = array of TIABTagValue;

  TIABPortfolioItem = record
    Symbol: string;
    Local: string;
    SecurityType: TIABSecurityType;
    Expiry: string;
    Strike: Double;
    Right: TIABRight;
    Currency: string;
    Position: BigDecimal;//Double;
    MarketPrice: Double;
    MarketValue: Double;
    AverageCost: Double;
    UnrealizedPNL: Double;
    RealizedPNL: Double;
    AccountName: string;
    InstrumentId: Integer;
    Multiplier: string;
    PrimaryExchange: string;
    TradingClass: string;
  end;
  PTIABPortfolioItem = ^TIABPortfolioItem;

  TIABInstrumentSpecItem = record
    MarketName: string;
    TradingClass: string;  // 9.69 removed from record
    ContractId: Integer;
    Multiplier: string;
    MinimumTick: Double;
    OrderTypes: TIABOrderTypesSet;
    ValidExchanges: string;
    Symbol: string;
    SecurityType: TIABSecurityType;
    Expiry: string;
    Strike: Double;
    Right: TIABRight;
    Exchange: string;
    Currency: string;
    LocalSymbol: string;
    PriceMagnifier: Integer;
    DataID: Integer;
    UnderConId: Integer;
    LongName: string;
    PrimaryExchange: string;
    ContractMonth: string;
    Industry: string;
    Category: string;
    SubCategory: string;
    TimeZoneID: string;
    TradingHours: string;
    LiquidHours: string;
    EVRule: string;
    EVMultiplier: Double;
    SecIdList: TIABTagValueArray;
//    MDSizeMultiplier: Integer;          // dropped in TWS API 10.10
    AggGroup: Integer;
		UnderSymbol: string;
		UnderSecType: string;
 		MarketRuleIds: string;
    RealExpirationDate: string;
    DerivativeSecTypes: string;
    LastTradeTime: string;
	  MinSize: BigDecimal;
  	SizeIncrement: BigDecimal;
  	SuggestedSizeIncrement: BigDecimal;
  end;
  PTIABInstrumentSpecItem = ^TIABInstrumentSpecItem;

  TIABBondSpecItem = record
    MarketName: string;
    TradingClass: string;   // 9.69 removed from record
    ContractId: Integer;
    MinimumTick: Double;
    OrderTypes: TIABOrderTypesSet;
    ValidExchanges: string;
    Symbol: string;
    SecurityType: TIABSecurityType;
    Cusip: string;
    Ratings: string;
    DescAppend: string;
    BondType: string;
    CouponType: string;
    Callable: Boolean;
    Putable: Boolean;
    Coupon: Double;
    Convertible: Boolean;
    Maturity: string;
    IssueDate: string;
    Exchange: string;
    Currency: string;
    NextOptionDate: string;
    NextOptionType: string;
    NextOptionPartial: Boolean;
    Notes: string;
    LongName: string;
    DataID: Integer;
    EVRule: string;
    EVMultiplier: Double;
    SecIdList: TIABTagValueArray;
//    MDSizeMultiplier: Integer;      // dropped in TWS API 10.10
    AggGroup: Integer;
		MarketRuleIds: string;
//    RealExpirationDate: string;    // not part of Bond data
    TimeZoneID: string;
    LastTradeTime: string;
	  MinSize: BigDecimal;
  	SizeIncrement: BigDecimal;
  	SuggestedSizeIncrement: BigDecimal;
  end;
  PTIABBondSpecItem = ^TIABBondSpecItem;

  TIABSymbolDerivativeSpecItem = record
    ContractId: Integer;
    Symbol: string;
    SecurityType: TIABSecurityType;
    Currency: string;
    PrimaryExchange: string;
    DerivativeSecCount: Integer;
    DerivativeSecTypes: string;
  end;
  PTIABSymbolDerivativeSpecItem = ^TIABSymbolDerivativeSpecItem;

  TIABDepthMarketDataDescripItem = record
    Exchange: string;
    SecurityType: TIABSecurityType;
    ListingExchange: string;
    ServiceDataType: string;
    AggGroup: Integer;
  end;
  PTIABDepthMarketDataDescripItem = ^TIABDepthMarketDataDescripItem;

  TIABExecution = record
    ExecutionId: string;
    Time: string;
    AcctNumber: string;
    Exchange: string;
    Side: TIABAction;
    Volume: BigDecimal;//Double;
    Price: Double;
    PermId: Integer;
    Liquidation: Integer;
    CumulativeQty: BigDecimal;//Integer;
    AveragePrice: Double;
    OrderRef: string;
    EVRule: string;
    EVMultiplier: Double;
    ModelCode: string;
    LastLiquidity: Integer;
  end;
  PTIABExecution = ^TIABExecution;
  TIABExecutionArray = array of TIABExecution;

  TIABHistoricalChartData = record
    Date: string;
    Open, High, Low, Close: Double;
    WAP, Volume: BigDecimal;
    TradeCount: Integer;
    HasGaps: Boolean;
  end;
  PTIABHistoricalChartData = ^TIABHistoricalChartData;

  TIABHistoricalSession = record
    StartDateTime: string;
    EndDateTime: string;
    RefDate: string;
  end;
  PTIABHistoricalSession = ^TIABHistoricalSession;

  TIABRealTimeData = record
    DateTime: TDateTime;
    Open, High, Low, Close: Double;
    WAP, Volume: BigDecimal;
    TradeCount: Integer;
  end;
  PTIABRealTimeData = ^TIABRealTimeData;

  TIABScanCriteria = record
    NumberOfRows: Integer;
    Instrument: string;
    LocationCode: string;
    ScanCode: string;
    AbovePrice: Double;
    BelowPrice: Double;
    AboveVolume: Integer;
    MarketCapAbove: Double;
    MarketCapBelow: Double;
    MoodyRatingAbove: string;
    MoodyRatingBelow: string;
    SPRatingAbove: string;
    SPRatingBelow: string;
    MaturityDateAbove: string;
    MaturityDateBelow: string;
    CouponRateAbove: Double;
    CouponRateBelow: Double;
    ExcludeConvertible: Integer;
    AverageOptionVolumeAbove: Integer;
    ScannerSettingPairs: string;
    CandidateSettingPairs: string;
    StockTypeFilter: string;
    SubscriptionOptions: TIABTagValueArray;
    FilterOptions: TIABTagValueArray;
  end;
  PTIABScanCriteria = ^TIABScanCriteria;

  TIABScanResultItem = record
    Rank: Integer;
    Symbol: string;
    SecurityType: TIABSecurityType;
    Expiry: string;
    Strike: Double;
    Right: TIABRight;
    Exchange: string;
    Currency: string;
    LocalSymbol: string;
    MarketName: string;
    TradingClass: string;     // 9.69 removed from record
    Distance: string;
    Benchmark: string;
    Projection: string;
    LegsString: string;
    ContractId: Integer;
  end;
  PTIABScanResultItem = ^TIABScanResultItem;
  TIABScanResultItemArray = array of TIABScanResultItem;

  TIABDeltaNeutralContract = record
    ConId: Integer;
    Delta, Price: Double;
  end;
  PTIABDeltaNeutralContract = ^TIABDeltaNeutralContract;

  TIABOrderQueryResult = record
    Status: string;

    // dropped about 9.72
    //InitMargin: string;
    //MaintMargin: string;
    //EquityWithLoan: string;

    // added about 9.72
    //  these next 3 (After) take the place of the original 3 above, when connected to old Server ver.
    InitMarginAfter: string;
    MaintMarginAfter: string;
    EquityWithLoanAfter: string;
    InitMarginBefore: string;
    MaintMarginBefore: string;
    EquityWithLoanBefore: string;
    InitMarginChange: string;
    MaintMarginChange: string;
    EquityWithLoanChange: string;

    // original
    Commission: Double;
    MinCommission: Double;
    MaxCommission: Double;
    CommissionCurrency: string;
    WarningText: string;

    // added about 9.72
	  CompletedTime: string;
	  CompletedStatus: string;
  end;
  PTIABOrderQueryResult = ^TIABOrderQueryResult;

  TIABCommissionReport = record
    ExecID: string;
    Commission: Double;
    Currency: string;
    RealizedPNL: Double;
    Yield: Double;
    YieldRedemptionDate: Integer;  // YYYYMMDD format
  end;
  PTIABCommissionReport = ^TIABCommissionReport;

  TIABSoftDollarTier = record
    Name, Value, DisplayName: string;
  end;
  PTIABSoftDollarTier = ^TIABSoftDollarTier;

  TIABTickAttrib = record
    CanAutoExecute, PastLimit, PreOpen: Boolean;
  end;
  PTIABTickAttrib = ^TIABTickAttrib;

  TIABContract = record
	  ContractId: Integer;
	  Symbol: string;
	  SecurityType: TIABSecurityType;
	  LastTradeDateOrContractMonth: string;
    Strike: Double;
    Right: TIABRight;
	  Multiplier: string;
	  Exchange: string;
	  PrimaryExchange: string; // pick an actual (ie non-aggregate) exchange that the contract trades on.  DO NOT SET TO SMART.
	  Currency: string;
	  LocalSymbol: string;
	  TradingClass: string;
	  IncludeExpired: Boolean;
	  SecIdType: string;		// CUSIP;SEDOL;ISIN;RIC
	  SecId: string;
  	// COMBOS
	  ComboLegsDescrip: string; // received in open order 14 and up for all combos
  	ComboLegList: TIABComboLegArray;
	  // delta neutral contract
  	DeltaNeutralContract: TIABDeltaNeutralContract;
  end;
  PTIABContract = ^TIABContract;

  TIABTickData = record
    TickType: TIABTickDataType;
    Time: TDateTime;
    // tdLast, tdAllLast
    PastLimit: Boolean;
    Unreported: Boolean;
    Price: Double;
    Size: BigDecimal;//Integer;
    Exchange: string;
    SpecialConditions: string;
    // tdBidAsk
    BidPrice: Double;
    AskPrice: Double;
    BidSize: BigDecimal;//Integer;
    AskSize: BigDecimal;//Integer;
    BidPastLow: Boolean;
  	AskPastHigh: Boolean;
    // tdMidPoint
    MidPoint: Double;
  end;
  PTIABTickData = ^TIABTickData;

  TIABWSHorizonEventData = record
    ConID: Integer;
    Filter: string;
    FillWatchList: Boolean;
    FillPortfolio: Boolean;
    FillCompetitors: Boolean;
    StartDate: string;
    EndDate: string;
    TotalLimit: Integer;
  end;
  PTIABWSHorizonEventData = ^TIABWSHorizonEventData;

implementation

{
    BigDecimals

 In version 10.10, the TWS API added a large decimal math capability.  This became necessary as some
   instruments (namely bitcoins), are able to be traded down to 0.000000001 of a share.  This is beyond the
   reliable range of using floats as decimals.  The TWS chose the Intel® Decimal Floating-Point Math Library,
   but this is only available in C and is too complex to convert to pascal.

 Fortunately a BigDecimal style library is available in pascal, thanks to the work of Rudy Velthuis.  It is
   available here:

     https://github.com/rvelthuis/DelphiBigNumbers

 A backup copy is here:    https://www.hhssoftware.com/iabsocketapi/download/DelphiBigNumbers-master.zip

 This IABSocketAPI can be compiled both with, or without using the DelphiBigNumbers feature.  If you do not
   trade bitcoins or small fractions of shares, then there is little need for BigDecimal support.

 If the DelphiBigNumbers library is not used, then the type BigDecimal in this API is a simple
   redefinition of type Double.  The properties and fields involved are mostly the Size of ticks and
   Filled and Quantity of orders, and share sizes.  In old API's, these were defined as a mix of integer
   and double.  In this API forwards, they are either a type double or type BigDecimal.  Existing application
   code from old API's will need some amending to compile.


   *******************

 To include the DelphiBigNumbers library code into this API ( requires XE2 or higher ) :

   1/  Download the DelphiBigDecimal zip file from the link above, and extract the contents to your favorite
         extra code or third party add-on file location,

   2/  Add the path of the "source" subfolder to your XE enviroment: menu, Tools, Options, Enviroment, Delphi options,
         Library, add to both the library path and browse paths, for 32 and 64 bits,

   3/  Add a USE_BIGDECIMAL define to the project: menu, Project, options, Delphi compiler, Conditional defines,
         for 32 and 64 - all configs,

   4/  Add the unit "Velthuis.BigDecimals" to the uses clause of your app anywhere that type BigDecimals are used.

   5/  With the installed TIABSocket API components file (.dpk), Open, uninstall, add the USE_BIGDECIMAL define to the
         components file, per step 3 above, compile, and reinstall.

   6/  In your application modify / replace the 5 (if used - see below) affected event function / procedure headers
         with the BigDecimal versions.  This can get messy: try to comment out the event declaration and code from
         both the interface and implementation sections. Save the form, confirm delete unused references.  Now create
         new the required event handlers and move your existing code into these.

   7/  Adjust your application code to use BigDecimal routines for conversion and variables.


  See the included pdf help files with the DelphiBigNumbers library.  This includes new routines for conversion of
    BigDecimal to Integers and strings.

  Note from 6/: Using BigDecimals directly affects 5 Event procedures in TIABSocket API.  These are OnTickSize,
    OnTickPriceAndSize, OnMarketDepth, OnMarketLevel2, OnHistogramData, OnProfitLossSingle.  Many other properties
    have changed too, mostly volume, quantity and bid/ask sizes.

}


end.
