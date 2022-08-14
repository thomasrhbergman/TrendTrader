unit TBfunctions;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.SyncObjs, Data.DB, 
  BrokerHelperAbstr, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} DaModule.Constants,
  FireDAC.Comp.Client;
{$ENDREGION}

type
  TIBThread = class(TThread)
  private
    FCriticalSection: TCriticalSection;
    FConnection: TFDConnection;
    FTransaction: TFDTransaction;
    FOnItemArrived: TUpdatePriceItem;
    FOnUpdateTradeItem: TUpdateTradeItem;
    FQuery: TFDQuery;
    FTag: Integer;
    procedure DoDataArrivedSynchronize;
    procedure DoDataArrived;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFilterDate: TDateTime; AOnItemArrived: TUpdatePriceItem; AUpdateTradeItem: TUpdateTradeItem; AIndex: Byte);
    destructor Destroy; override;
  public
    property Tag: Integer read FTag write FTag;
  end;

  TWorldIndexBroker = class
  private
    FOnPriceArrived: TUpdatePriceItem;
    FOnTradeArrived: TUpdateTradeItem;
    FFilterDate: TDateTime;
    FFeeds: array [0 .. 4] of TIBThread;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginFeed;
    procedure EndFeed;
    property FilterDate: TDateTime read FFilterDate write FFilterDate;
    property OnPriceArrived: TUpdatePriceItem read FOnPriceArrived write FOnPriceArrived;
    property OnTradeArrived: TUpdateTradeItem read FOnTradeArrived write FOnTradeArrived;
  end;

implementation

{ TIBThread }

constructor TIBThread.Create(const AFilterDate: TDateTime; AOnItemArrived: TUpdatePriceItem; AUpdateTradeItem: TUpdateTradeItem; AIndex: Byte);
begin
  inherited Create;
  FreeOnTerminate := True;
  FCriticalSection := TCriticalSection.Create;
  if Assigned(AOnItemArrived) then
    FOnItemArrived := AOnItemArrived;
  if Assigned(AUpdateTradeItem) then
    FOnUpdateTradeItem := AUpdateTradeItem;

  FConnection := TFireBirdConnect.CreateDatabase(TFireBirdConnect.DBNameFeed);

  FTransaction := TFDTransaction.Create(nil);
  FTransaction.Connection := FConnection;
  FConnection.Transaction := FTransaction;
  FTransaction.Options.AutoCommit := false;

  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
  FQuery.Transaction := FTransaction;
  if (AFilterDate = 0) then
    FQuery.SQL.Text := 'SELECT * FROM AKTIEKURS WHERE K_DBNR=' + AIndex.ToString + ' ORDER BY K_TID'
  else
    FQuery.SQL.Text := 'SELECT * FROM AKTIEKURS WHERE K_DBNR=' + AIndex.ToString +
                       ' AND CAST(K_TID AS DATE)=''' + FormatDateTime('YYYY-MM-DD', AFilterDate) + ''' ORDER BY K_TID';
  FQuery.Open;
end;

destructor TIBThread.Destroy;
begin
  if (FQuery.Active) then
    FQuery.Close;
  FreeAndNil(FQuery);
  FTransaction.Commit;
  FreeAndNil(FTransaction);
  FConnection.Connected := False;
  FreeAndNil(FConnection);
  FreeAndNil(FCriticalSection);
  inherited;
end;

procedure TIBThread.Execute;
begin
  inherited;
  while not Terminated do
  begin
    if FQuery.Active and not FQuery.IsEmpty then
      DoDataArrived;
    Sleep(500);
  end;
end;

procedure TIBThread.DoDataArrivedSynchronize;
var
  loPriceItem: TPriceItem;
begin
  if Assigned(FOnItemArrived) then
  begin
    if FQuery.Eof then
      FQuery.First;

    loPriceItem.Identifier := FQuery.FieldByName('K_DBNR').AsString;
    loPriceItem.MarketId := 0;
    loPriceItem.TradeTimestamp := FormatDateTime(FormatSettings.ShortDateFormat, Now);
    loPriceItem.TickTimestamp := FQuery.FieldByName('K_TID').AsString;
    loPriceItem.Bid := 0;
    loPriceItem.BidSize := 0;
    loPriceItem.Ask := 0;
    loPriceItem.AskSize := 0;
    loPriceItem.Close := 0;
    loPriceItem.High := 0;
    loPriceItem.LastSize := 0;
    loPriceItem.LotSize := '';
    loPriceItem.Low := 0;
    loPriceItem.Open := 0;
    loPriceItem.TurnOver := 0;
    loPriceItem.TurnOverSize := 0;
    if (FQuery.FieldByName('K_FIELD').AsString = 'LAST') then
      loPriceItem.Last := FQuery.FieldByName('K_KURSEN').AsFloat;
    FQuery.Next;
    FOnItemArrived(loPriceItem);
  end;
end;

procedure TIBThread.DoDataArrived;
begin
  Synchronize(DoDataArrivedSynchronize);
end;

{ TWorldIndexBroker }

constructor TWorldIndexBroker.Create;
var
  i: Integer;
begin
  for i := Low(FFeeds) to High(FFeeds) do
    FFeeds[i] := nil;
  FOnPriceArrived := nil;
  FOnTradeArrived := nil;
end;

destructor TWorldIndexBroker.Destroy;
begin
  EndFeed;
  FOnPriceArrived := nil;
  FOnTradeArrived := nil;
  inherited;
end;

procedure TWorldIndexBroker.BeginFeed;
var
  i: Integer;
begin
  for i := Low(FFeeds) to High(FFeeds) do
    FFeeds[i] := TIBThread.Create(FilterDate, FOnPriceArrived, FOnTradeArrived, i);
end;

procedure TWorldIndexBroker.EndFeed;
var
  i: Integer;
begin
  for i := Low(FFeeds) to High(FFeeds) do
    if Assigned(FFeeds[i]) and not FFeeds[i].Terminated then
    begin
      FFeeds[i].Terminate;
      WaitForSingleObject(FFeeds[i].Handle, 50);
    end;
end;

end.
