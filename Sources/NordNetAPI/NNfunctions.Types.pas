unit NNfunctions.Types;

interface

type
  TStatus = record
    timestamp: Int64;
    valid_version: Boolean;
    system_running: Boolean;
    message: string;
  end;

  TFeed = record
    hostname: string;
    port: Integer;
    encrypted: Boolean;
  end;

  TLogin = record
    environment: string;
    session_key: string;
    expires_in: Integer;
    private_feed: TFeed;
    public_feed: TFeed;
  end;

  TTradable = record
    market_id: Integer;
    display_order: Integer;
    lot_size: Double;
    tick_size_id: Integer;
    identifier: string;
  end;
  TTradables = TArray<TTradable>;

  TUnderlying = record
    instrumment_id: Integer;
    instrument_id: Integer;
    symbol: string;
    isin_code: string;
  end;
  TUnderlyings = TArray<TUnderlying>;

  TInstrument = record
    prospectus_url: string;
    underlyings: TUnderlyings;
    expiration_date: string;
    tradables: TTradables;
    instrument_id: Integer;
    instrument_type: string;
    instrument_group_type: string;
    currency: string;
    number_of_securities: Double;
    multiplier: Double;
    strike_price: Double;
    pawn_percentage: Double;
    symbol: string;
    isin_code: string;
    market_view: string;
    sector: string;
    sector_group: string;
    name: string;
  end;
  TInstruments = TArray<TInstrument>;

  TAccount = record
    accno: Integer;
    type_: string;
    default: Boolean;
    alias: string;
    is_blocked: Boolean;
    blocked_reason: string;
  end;
  TAccounts = TArray<TAccount>;

  TPostOrder = record
    identifier: string;
    market_id: Integer;
    price: Double;
    currency: string;
    volume: Integer;
    side: string;
    order_type: string;
    valid_until: string;
    open_volume: Integer;
    reference: string;
    activation_condition: string;
    trigger_value: Double;
    trigger_condition: string;
    target_value: Double;
  end;

  TAmount = record
    value: Double;
    currency: string;
  end;

  TTradableId = record
    identifier: string;
    market_id: Integer;
  end;

  TActivationCondition = record
    type_: string;
    trailing_value: Double;
    trigger_value: Double;
    trigger_condition: string;
  end;

  TValidity = record
    type_: string;
    valid_until: Int64;
  end;

  TOrder = record
    accno: Integer;
    order_id: Integer;
    price: TAmount;
    volume: Double;
    tradable: TTradableId;
    open_volume: Double;
    traded_volume: Double;
    side: string;
    modified: Int64;
    reference: string;
    activation_condition: TActivationCondition;
    price_condition: string;
    volume_condition: string;
    validity: TValidity;
    action_state: string;
    order_type: string;
    order_state: string;
  end;
  TOrders = TArray<TOrder>;

  TTradeOrder = record
    accno: Integer;
    order_id: Integer;
    trade_id: string;
    tradable: TTradableId;
    price: TAmount;
    volume: Double;
    side: string;
    counterparty: string;
    tradetime: string;
  end;

  TOrderReply = record
    order_id: Integer;
    result_code: string;
    order_state: string;
    action_state: string;
    message: string;
  end;

  TFeedItemType = (fitUnknown, fitError, fitHeartBeat, fitPrice, fitDepth, fitTrade, fitTradingStatus, fitIndex, fitNews, fitOrder, fitTradeOrder);

  TErrorFeedItem = record
    msg: string;
    cmd: string;
  end;

  TPriceFeedItem = record
    i: string;
    m: Integer;
    t: string;
    trade_timestamp: string;
    tick_timestamp: string;
    bid: Double;
    bid_volume: Integer;
    ask: Double;
    ask_volume: Integer;
    close: Double;
    high: Double;
    last: Double;
    last_volume: Integer;
    lot_size: string;
    low: Double;
    open: Double;
    turnover: Double;
    turnover_volume: Integer;
  end;

  TTradeFeedItem = record
    i: string;
    m: Integer;
    t: string;
    trade_timestamp: string;
    price: Double;
    volume: Integer;
    baseprice: Double;
    broker_buying: string;
    broker_selling: string;
    trade_type: string;
    isin: string;
  end;


implementation

end.
