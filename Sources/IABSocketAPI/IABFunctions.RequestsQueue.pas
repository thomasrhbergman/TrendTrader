unit IABFunctions.RequestsQueue;

interface

{$REGION 'Region uses'}
uses
  System.SysUtils, System.Classes, System.Threading, System.Generics.Collections, System.Generics.Defaults, VCL.Forms,
  System.Types, IABSocketAPI, IABSocketAPI_const, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  Global.Types, Utils, System.IOUtils, Common.Types;
{$ENDREGION}

type
  TQueuePriority = (qpLow, qpNormal, qpHigh, hqRealTime);
  TIABCommand = (
                 ibCancelAccountUpdates,
                 ibCancelHistoricalData,
                 ibCancelImpliedVolatility,
                 ibCancelMarketData,
                 ibCancelOrder,
                 ibCancelPnL,
                 ibCancelTickByTickData,
                 ibGetAccountUpdates,
                 ibGetCurrentTWSTime,
                 ibGetHistoricalData,
                 ibGetImpliedVolatility,
                 ibGetInstrumentSpecs,
                 ibGetMarketData,
                 ibGetMarketDataType,
                 ibGetTickByTickData,
                 ibRequestMarketRule,
                 ibRequestMatchingSymbols,
                 ibRequestPnL,
                 ibRequestSecDefOptParams
                 );

  THistoricalDataParams = record
    SubscribeHistData : Boolean;
    DurationTimeUnits : Integer;
    DataDuration      : Integer;
    BarSize           : TIABChartBarSize;
    DataBasis         : TIABHistoricalDataType;
    KeepUpdated       : Boolean;
    constructor Create(aDurationTimeUnits, aDataDuration: Integer; aBarSize: TIABChartBarSize; aDataBasis: TIABHistoricalDataType; aKeepUpdated: Boolean);
  end;

  TIABRequest = record
    Command              : TIABCommand;
    Priority             : TQueuePriority;
    TimeStamp            : TDateTime;
    DataId               : Integer;
    Order                : TIABOrder;
    Details              : string;
    Strike               : Double;
    Option               : Double;
    DataType             : TIABMarketDataType;
    ExMarketData         : TIABExMktDataSet;
    TickDataType         : TIABTickDataType;
    Symbol               : string;
    Exchange             : string;
    SecurityType         : string;
    ContractId           : Integer;
    HistoricalDataParams : THistoricalDataParams;
    procedure Clear;
    constructor Create(aCommand: TIABCommand; aDataId: Integer; aOrder: TIABOrder; aPriority: TQueuePriority);
  end;

  TIABRequestsQueue = class
  private
    FComparer      : IComparer<TIABRequest>;
    FList          : TList<TIABRequest>;
    FQueueLock     : TObject;
    FQueueNotEmpty : TObject;
    FQueueNotFull  : TObject;
    FShutDown      : Boolean;
    FPopTimeout    : Cardinal;
    function GetCount: Integer;
    procedure DoShutDown;
  public
    constructor Create(PopTimeout: Cardinal = INFINITE);
    destructor Destroy; override;
    function PopItem(out AItem: TIABRequest): TWaitResult;
    function PushItem(aCommand: TIABCommand; aContractId: Integer; aOrder: TIABOrder = nil; aPriority: TQueuePriority = qpNormal): TWaitResult; overload;
    function PushItem(aIABRequest: TIABRequest): TWaitResult; overload;
    property Count: Integer read GetCount;
  end;

const
  IABCommandToString: array[TIABCommand] of string = (
                 'CancelAccountUpdates',
                 'CancelHistoricalData',
                 'CancelImpliedVolatility',
                 'CancelMarketData',
                 'CancelOrder',
                 'CancelPnL',
                 'CancelTickByTickData',
                 'GetAccountUpdates',
                 'GetCurrentTWSTime',
                 'GetHistoricalData',
                 'GetImpliedVolatility',
                 'GetInstrumentSpecs',
                 'GetMarketData',
                 'GetMarketDataType',
                 'GetTickByTickData',
                 'RequestMarketRule',
                 'RequestMatchingSymbols',
                 'RequestPnL',
                 'RequestSecDefOptParams'
                 );

implementation

{ TIABRequestsQueue}

constructor TIABRequestsQueue.Create(PopTimeout: Cardinal = INFINITE);
begin
  FList          := TList<TIABRequest>.Create;
  FQueueLock     := TObject.Create;
  FQueueNotEmpty := TObject.Create;
  FQueueNotFull  := TObject.Create;
  FShutDown      := False;
  FPopTimeout    := PopTimeout;
  FComparer := TComparer<TIABRequest>.Construct(
    function(const Left, Right: TIABRequest): Integer
    begin
      Result := TComparer<Integer>.Default.Compare(Ord(Right.Priority), Ord(Left.Priority));
      if (Result = EqualsValue) then
        Result := TComparer<TDateTime>.Default.Compare(Left.TimeStamp, Right.TimeStamp);
    end);
end;

destructor TIABRequestsQueue.Destroy;
begin
  DoShutDown;
  FreeAndNil(FList);
  FreeAndNil(FQueueLock);
  FreeAndNil(FQueueNotEmpty);
  FreeAndNil(FQueueNotFull);
  inherited;
end;

procedure TIABRequestsQueue.DoShutDown;
begin
  System.TMonitor.Enter(FQueueLock);
  try
    FShutDown := True;
  finally
    System.TMonitor.Exit(FQueueLock);
  end;
  System.TMonitor.PulseAll(FQueueNotFull);
  System.TMonitor.PulseAll(FQueueNotEmpty);
end;

function TIABRequestsQueue.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TIABRequestsQueue.PopItem(out AItem: TIABRequest): TWaitResult;
//var
// Item: TIABRequest;
// str: string;
begin
  AItem := Default(TIABRequest);
  System.TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FList.Count = 0) and not FShutDown do
      if not System.TMonitor.Wait(FQueueNotEmpty, FQueueLock, FPopTimeout) then
        Result := wrTimeout;
    if not Assigned(Application) or Application.Terminated then
      Exit
    else if (FShutDown and (FList.Count = 0)) then
      Exit
    else if (Result <> wrSignaled) then
      Exit;

    FList.Sort(FComparer);
    AItem := FList.First;

//    if (FList.Count > 100) and (FList.Count < 200) then
//    begin
//      str := '';
//      for Item in FList do
//      begin
//        str := str + Integer(Item.Priority).ToString + ';' + FormatDateTime('ss.zzz', Item.TimeStamp) + ';' + Integer(Item.Command).ToString + sLineBreak;
//      end;
//      TFile.WriteAllText('d:\temp\1\' + TPath.GetRandomFileName, str);
//    end;

    FList.Delete(0);
  finally
    System.TMonitor.Exit(FQueueLock);
  end;
  System.TMonitor.Pulse(FQueueNotFull);
end;

function TIABRequestsQueue.PushItem(aIABRequest: TIABRequest): TWaitResult;
var
  Order: TIABOrder;
begin
  System.TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
//    while (Result = wrSignaled) and not(FShutDown or Application.Terminated) do
//      if not System.TMonitor.Wait(FQueueNotFull, FQueueLock, INFINITE) then
//        Result := wrTimeout;
    if FShutDown or Application.Terminated then
      Exit;
    aIABRequest.TimeStamp := Now;
    if Assigned(aIABRequest.Order) then
    begin
      Order := TIABOrder.Create;
      Order.Assign(aIABRequest.Order);
      aIABRequest.Order := Order;
    end;
    FList.Add(aIABRequest);
  finally
    System.TMonitor.Exit(FQueueLock);
  end;
  System.TMonitor.Pulse(FQueueNotEmpty);
//  General.LogPublisher.Write([ltLogWriter], ddText, Self, 'PushItem', FList.Count.ToString);
end;

function TIABRequestsQueue.PushItem(aCommand: TIABCommand; aContractId: Integer; aOrder: TIABOrder; aPriority: TQueuePriority): TWaitResult;
begin
  Result := PushItem(TIABRequest.Create(aCommand, aContractId, aOrder, aPriority));
end;

{ TIABRequests }

constructor TIABRequest.Create(aCommand: TIABCommand; aDataId: Integer; aOrder: TIABOrder; aPriority: TQueuePriority);
begin
  Self := Default(TIABRequest);
  Self.Command   := aCommand;
  Self.Priority  := aPriority;
  Self.TimeStamp := Now;
  Self.DataId    := aDataId;
  if Assigned(aOrder) then
  begin
    Self.Order := TIABOrder.Create;
    Self.Order.Assign(aOrder);
  end;
  Self.HistoricalDataParams := THistoricalDataParams.Create(IAB_TIME_UNIT_DAY, 1, bs30sec, cdTrades, False);
end;

procedure TIABRequest.Clear;
begin
  if Assigned(Self.Order) then
    FreeAndNil(Self.Order);
  Self := Default(TIABRequest);
end;

{ THistoricalDataParams }

constructor THistoricalDataParams.Create(aDurationTimeUnits, aDataDuration: Integer; aBarSize: TIABChartBarSize; aDataBasis: TIABHistoricalDataType; aKeepUpdated: Boolean);
begin
  DurationTimeUnits := aDurationTimeUnits; // IAB_TIME_UNIT_DAY
  DataDuration      := aDataDuration;      // 1
  BarSize           := aBarSize;           // bs30sec
  DataBasis         := aDataBasis;         // cdTrades
  KeepUpdated       := aKeepUpdated;       // False
end;

end.
