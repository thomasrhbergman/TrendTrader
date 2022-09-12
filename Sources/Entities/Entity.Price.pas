unit Entity.Price;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  Vcl.Forms, Winapi.Messages, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const,
  Data.DB, HtmlLib, System.IOUtils, System.Threading, System.Types, DaModule.Constants,
  DaModule.Utils, System.Variants, Common.Types, Global.Types, Publishers, System.Math, Vcl.Graphics,
  FireDAC.Comp.Client, IBX.IB, FireDAC.Stan.Param, FireDAC.Comp.DataSet, Utils;
{$ENDREGION}

type
  TStoredPrice = record
    ConId: Integer;
    TimeStamp: TDateTime;
    Value: Double;
    TickType: TIABTickType;
    IsHistorical: Boolean;
    constructor Create(const aConId: Integer; const aTimeStamp: TDateTime; const aValue: Double; const aTickType: TIABTickType; const aIsHistorical: Boolean);
  end;

  TThreadPriceCache = class(TThread)
  private
    FConnectionFeed   : TFDConnection;
    FConnectionSokid  : TFDConnection;
    FQuerySokid       : TFDQuery;
    FQueue            : TThreadedQueue<TStoredPrice>;
    FStoredProc       : TFDStoredProc;
    FTransactionFeed  : TFDTransaction;
    FTransactionSokid : TFDTransaction;
    procedure SaveRecordToDB(const aStoredPrice: TStoredPrice);
    procedure UpdateSokidIB(const aStoredPrice: TStoredPrice);
    procedure CreateFeedConnect;
    procedure CreateStockConnect;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property PriceQueue: TThreadedQueue<TStoredPrice> read FQueue;
  end;

implementation

{ TThreadPriceCache }

constructor TThreadPriceCache.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Priority := tpLowest;
  FQueue := TThreadedQueue<TStoredPrice>.Create(100000, C_POP_TIMEOUT, C_PUSH_TIMEOUT);
  CreateFeedConnect;
  CreateStockConnect;
end;

destructor TThreadPriceCache.Destroy;
begin
  FStoredProc.UnPrepare;
  if FTransactionFeed.Active then
    FTransactionFeed.Commit;
  if FConnectionFeed.Connected then
    FConnectionFeed.Connected := False;
  FreeAndNil(FStoredProc);
  FreeAndNil(FTransactionFeed);
  FreeAndNil(FConnectionFeed);

  FQuerySokid.Unprepare;
  if FTransactionSokid.Active then
    FTransactionSokid.Commit;
  if FConnectionSokid.Connected then
    FConnectionSokid.Connected := False;
  FreeAndNil(FQuerySokid);
  FreeAndNil(FTransactionSokid);
  FreeAndNil(FConnectionSokid);

  FreeAndNil(FQueue);
  inherited;
end;

procedure TThreadPriceCache.CreateStockConnect;
resourcestring
  C_SQL = 'UPDATE SOKID_IB SET LAST_PRICE=:LAST_PRICE WHERE CONID=:CONID';
begin
  FConnectionSokid := TFireBirdConnect.CreateDatabase(TFireBirdConnect.DBNameStock);

  FTransactionSokid := TFDTransaction.Create(FConnectionSokid);
  FConnectionSokid.Transaction := FTransactionSokid;
  FTransactionSokid.Connection := FConnectionSokid;
  FTransactionSokid.Options.AutoCommit := false;

  FQuerySokid := TFDQuery.Create(FConnectionSokid);
  FQuerySokid.SQL.Text    := C_SQL;
  FQuerySokid.Connection  := FConnectionSokid;
  FQuerySokid.Transaction := FTransactionSokid;
end;

procedure TThreadPriceCache.CreateFeedConnect;
resourcestring
  C_PROC_NAME = 'INS_TICK_DATA';
begin
  FConnectionFeed := TFireBirdConnect.CreateDatabase(TFireBirdConnect.DBNameFeed);
  FTransactionFeed := TFDTransaction.Create(FConnectionFeed);
  FConnectionFeed.Transaction := FTransactionFeed;
  FTransactionFeed.Connection := FConnectionFeed;
  FTransactionFeed.Options.AutoCommit := false;

  FStoredProc := TFDStoredProc.Create(FConnectionFeed);
  FStoredProc.StoredProcName := C_PROC_NAME;
  FStoredProc.Connection     := FConnectionFeed;
  FStoredProc.Transaction    := FTransactionFeed;
end;

procedure TThreadPriceCache.Execute;
var
  loPriceList: TStoredPrice;
  WaitResult: TWaitResult;
begin
  inherited;
  TThread.NameThreadForDebugging('Entity.Price.TThreadPriceCache');
  try
    if not FConnectionFeed.Connected then
    begin
      FConnectionFeed.Connected := True;
      FStoredProc.Prepare;
    end;
    if not FConnectionSokid.Connected then
    begin
      FConnectionSokid.Connected := True;
      FQuerySokid.Prepare;
    end;
    while not Terminated do
    begin
      WaitResult := FQueue.PopItem(loPriceList);
      if (WaitResult = TWaitResult.wrSignaled) then
        if (loPriceList.ConId > 0) then
        begin
          SaveRecordToDB(loPriceList);
          UpdateSokidIB(loPriceList);
        end;
    end;
  except
    on E:Exception do
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'Execute', E.Message);
  end;
end;

procedure TThreadPriceCache.UpdateSokidIB(const aStoredPrice: TStoredPrice);
begin
  if (aStoredPrice.TickType in [ttLast, ttClose]) and (aStoredPrice.Value > 0.00001) then
    if (not Terminated) and FConnectionSokid.Connected  then
    begin
      FQuerySokid.Close;
      if not FTransactionSokid.Active then
        FTransactionSokid.StartTransaction;
      try
        FQuerySokid.ParamByName('CONID').AsInteger := aStoredPrice.ConId;
        FQuerySokid.ParamByName('LAST_PRICE').Value := aStoredPrice.Value;
        FQuerySokid.ExecSQL;
        FTransactionSokid.Commit;
      except
        on E: EIBInterBaseError do
        begin
          if FTransactionSokid.Active then
            FTransactionSokid.Rollback;
          if (E.SQLCode <> -803) and (E.SQLCode <> -913) then
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'UpdateSokidIB', E.Message +
                                                                         ', SQLCode: ' + E.SQLCode.ToString +
                                                                         ', IBErrorCode: ' +  E.IBErrorCode.ToString +
                                                                         TDModUtils.GetQueryInfo(FQuerySokid));
          //UpdateSokidIB(aStoredPrice); ????
        end;
        on Er: Exception do
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'UpdateSokidIB', Er.Message + TDModUtils.GetQueryInfo(FQuerySokid));
      end;
    end;
end;

var
  Count: Integer = 0;

procedure TThreadPriceCache.SaveRecordToDB(const aStoredPrice: TStoredPrice);
begin
  if (not Terminated) and FConnectionFeed.Connected and (aStoredPrice.Value > 0.00001) then
  begin
    FStoredProc.Close;
    if not FTransactionFeed.Active then
      FTransactionFeed.StartTransaction;
    try
      FStoredProc.ParamByName('CONID').AsInteger         := aStoredPrice.ConId;
      FStoredProc.ParamByName('TICKDATE').Value          := aStoredPrice.TimeStamp;
      FStoredProc.ParamByName('TICKTYPE').AsInteger      := Integer(aStoredPrice.TickType);
      FStoredProc.ParamByName('TICKVALUE').Value         := aStoredPrice.Value;
      FStoredProc.ParamByName('IS_HISTORICAL').Value     := aStoredPrice.IsHistorical;
      FStoredProc.ExecProc;
      if FTransactionFeed.Active then
        FTransactionFeed.Commit;
//      Inc(Count);
//      if (Count div 1000 = 0) then
//        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'UpdateSokidIB',THtmlLib.GetColorTag( 'Queue:'+ FQueue.QueueSize.ToString + ' count:' + Count.ToString, clBlue));
    except
      on E: EIBInterBaseError do
      begin
        if FTransactionFeed.Active then
          FTransactionFeed.Rollback;
        if (E.SQLCode <> -803) and (E.SQLCode <> -913) then
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'UpdateSokidIB', E.Message +
                                                                       ', SQLCode: ' + E.SQLCode.ToString +
                                                                       ', IBErrorCode: ' +  E.IBErrorCode.ToString +
                                                                       TDModUtils.GetQueryInfo(FStoredProc));
        //SaveRecordToDB(aStoredPrice);  ???
      end;
      on Er: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'UpdateSokidIB', Er.Message + TDModUtils.GetQueryInfo(FStoredProc));
    end;
  end;
end;

{ TStoredPrice }

constructor TStoredPrice.Create(const aConId: Integer; const aTimeStamp: TDateTime; const aValue: Double; const aTickType: TIABTickType; const aIsHistorical: Boolean);
begin
  Self.ConId        := aConId;
  Self.TimeStamp    := aTimeStamp;
  Self.Value        := aValue;
  Self.TickType     := aTickType;
  Self.IsHistorical := aIsHistorical;
end;

end.
