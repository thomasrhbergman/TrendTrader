unit UIABConsole;

interface

uses
  IABSocketAPI, IABSocketAPI_const, SysUtils;

type
  TIABConsole = class(TObject)
    BidSize, AskSize, Volume, LastSize: Integer;
    BidPrice, AskPrice, LastPrice: Double;
    PriceSizedata: Boolean;
    IABSocket: TIABSocket;
    constructor Create;
    destructor Destroy; override;
    procedure ConnectionState(Sender: TObject; State: TIABConnection);
    procedure Error(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
    procedure TickSize(Sender: TObject; DataId: Integer; TickType: TIABTickType; Size: Integer);
    procedure TickPrice(Sender: TObject; DataId: Integer; TickType: TIABTickType; Price: Double; TickAttrib: TIABTickAttrib);
    procedure EndOfStreamRead(Sender: TObject);
  end;

implementation

function GetIndexFutureExpiry: string;
const Months: Integer = 3;
var y,m,d: Word; DT: TDateTime; Next: Boolean; yStr, Expiry: string;
begin
  DecodeDate(Date,y,m,d);
  DT := EncodeDate(y,m,1);
  case DayOfWeek(DT) of
    1:Next := d > 11;
    2:Next := d > 10;
    3:Next := d > 9;
    4:Next := d > 8;
    5:Next := d > 7;
    6:Next := d > 6;
    7:Next := d > 12;
    else Next := false;
  end;
  if Next then m := m + 1;
  if m = 13 then
    begin
      y := y + 1;
      m := 1;
    end;
  while m mod Months > 0 do inc(m);
  yStr := IntToStr(y);
  Expiry := yStr;
  if m < 10 then Expiry := Expiry + '0';
  Expiry := Expiry + IntToStr(m);
  Result := Expiry;
end;

constructor TIABConsole.Create;
begin
  inherited Create;
  IABSocket := TIABSocket.Create(nil);
  IABSocket.OnConnectionState := ConnectionState;
  IABSocket.OnError := Error;
  IABSocket.OnTickPrice := TickPrice;
  IABSocket.OnTickSize := TickSize;
  IABSocket.OnEndOfStreamRead := EndOfStreamRead;

  IABSocket.ClientID := 1234;

  IABSocket.DefOrder.Symbol := 'ES';
  IABSocket.DefOrder.Exchange := 'GLOBEX';
  IABSocket.DefOrder.Expiry := GetIndexFutureExpiry;
  IABSocket.DefOrder.SecurityType := stFuture;
  IABSocket.DefOrder.Currency := 'USD';
end;

destructor TIABConsole.Destroy;
begin
  IABSocket.Free;
  inherited Destroy;
end;

procedure TIABConsole.ConnectionState(Sender: TObject; State: TIABConnection);
var s: string;
begin
  case State of
    twsClosed: s := 'Connection: TWS connection closed';
    twsConnecting: s := 'Connection: Connecting to TWS';
    twsReady: s := 'Connection: TWS connection Ready... Server time: ' + IABSocket.ConnectAtServerTime;
    twsFailed: s := 'Connection: TWS connection Failed';
  end;
  WriteLn(s);
end;

procedure TIABConsole.Error(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
var s: string;
begin
  s := 'ERROR: ' + IntToStr(TempId) + ' ' + IntToStr(ErrorCode) + ' ' + ErrorMsg;
  WriteLn(s);
end;

procedure TIABConsole.TickSize(Sender: TObject; DataId: Integer;
  TickType: TIABTickType; Size: Integer);
begin
  if TickType = ttBidSize then BidSize := Size;
  if TickType = ttAskSize then AskSize := Size;
  if TickType = ttLastSize then LastSize := Size;
  if TickType = ttVolume then Volume := Size;
  PriceSizedata := true;
end;

procedure TIABConsole.TickPrice(Sender: TObject; DataId: Integer; TickType: TIABTickType; Price: Double; TickAttrib: TIABTickAttrib);
begin
  if TickType = ttBid then BidPrice := Price;
  if TickType = ttAsk then ASkPrice := Price;
  if TickType = ttLast then LastPrice := Price;
  PriceSizedata := true;
end;

procedure TIABConsole.EndOfStreamRead(Sender: TObject);
var s: string;
begin
  if not PriceSizedata then Exit;
  s := Format('  BID %.2f/%d  ASK %.2f/%d  LAST %.2f/%d  VOL %d',[BidPrice, BidSize, AskPrice, AskSize, LastPrice, LastSize, Volume]);
  WriteLn(s);
  PriceSizedata := false;
end;

end.
