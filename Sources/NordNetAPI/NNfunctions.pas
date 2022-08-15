unit NNfunctions;

interface

{$REGION 'Region uses'}
uses
  Windows, System.SysUtils, System.AnsiStrings, System.Classes, System.SyncObjs, System.DateUtils,
  System.Generics.Collections, System.StrUtils, System.JSON, System.JSON.Types, System.NetEncoding, Vcl.ExtCtrls,
  BrokerHelperAbstr, IdGlobal, IdTCPConnection, IdTCPClient, IdSSLOpenSSL, Soap.EncdDecd, REST.Utils, REST.Types,
  REST.Client, OpenSSL.RSAUtils, NNfunctions.Types, Common.Types, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  DebugWriter, Global.Types, Publishers;
{$ENDREGION}

type
  TNordNet = class;

  ENordNetException = class(Exception);

  TFeedType = (ftPrivate, ftPublic);
  TLookupType = (ltMarketIdIdentifier, ltIsinCode);

  TErrorItemArrived = procedure(const Sender: TNordNet; const FeedType: TFeedType; const ErrorItem: TErrorFeedItem) of object;
  TFeedDrop = procedure(const Sender: TNordNet; const FeedType: TFeedType) of object;
  TFeedRequest = procedure(const Sender: TNordNet; const FeedType: TFeedType; const JSON: string) of object;
  TGenericError = procedure(const Sender: TNordNet; const Code, Message: string) of object;
  THeartBeatItemArrived = procedure(const Sender: TNordNet; const FeedType: TFeedType) of object;
  TJsonArrived = procedure(const Sender: TNordNet; const FeedType: TFeedType; const JSON: string) of object;
  TNotify = procedure(const Sender: TNordNet) of object;
  TOrderItemArrived = procedure(const Sender: TNordNet; const OrderItem: TOrder) of object;
  TTradeOrderArrived = procedure(const Sender: TNordNet; const TradeOrder: TTradeOrder) of object;
  TPriceItemArrived = procedure(const Sender: TNordNet; const FeedType: TFeedType; const PriceItem: TPriceFeedItem) of object;
  TRequestDone = procedure(const Sender: TNordNet; const Path: string; const HttpCode: Integer; const HttpStatus, HttpBody: string) of object;
  TTradeItemArrived = procedure(const Sender: TNordNet; const FeedType: TFeedType; const TradeItem: TTradeFeedItem) of object;

  TLastError = record
    Code: string;
    Message: string;
  end;

  TFeedThread = class;

  TRawFeedDisconnected = procedure(const Sender: TFeedThread) of object;
  TRawFeedItemArrived = procedure(const Sender: TFeedThread; const FeedItemData: UTF8String) of object;
  TRawFeedRequest = procedure(const Sender: TFeedThread; const Command: UTF8String) of object;

  TFeedThreadReconnect = record
    Attempt: Byte;
    Timer: TTimer;
  end;

  TFeedThread = class(TThread)
  private
    FCriticalSection: TCriticalSection;
    FData: UTF8String;
    FIdTCPClient: TIdTCPClient;
    FManualClosing, FReady: Boolean;
    FOnDisconnected: TRawFeedDisconnected;
    FOnItemArrived: TRawFeedItemArrived;
    FOnRequest: TRawFeedRequest;
    FReconnectInfo: TFeedThreadReconnect;
    FSessionKey: string;
    FTag: Integer;
    procedure ChargeReconnectTimer(const Timeout: Integer);
    procedure CleanupReconnectTimer;
    procedure ClearReconnectInfo;
    procedure DoCheckNewData;
    procedure DoDataArrived;
    procedure DoDataArrivedSync;
    procedure DoNextReconnectAttampt;
    procedure DoOnTerminate(Sender: TObject);
    procedure OnReconnectTimer(Sender: TObject);
    procedure OnSocketConnect(Sender: TObject);
    procedure OnSocketDisconnect(Sender: TObject);
    procedure SetReady(const Value: Boolean);
  protected
    procedure Execute; override;
    property OnTerminate;
  public
    constructor Create(const Host: string; const Port: Integer; const TLS: Boolean; const SessionKey: string; const OnRequest: TRawFeedRequest; const OnItemArrived: TRawFeedItemArrived; const OnDisconnected: TRawFeedDisconnected);
    destructor Destroy; override;
    procedure BeginFeed;
    procedure PostCommand(const Data: UTF8String);
  public
    property Ready: Boolean read FReady write SetReady;
    property Tag: Integer read FTag write FTag;
  end;

  TNordNet = class(TObject)
  private
    FActive: Boolean;
    FBaseURL: string;
    FCloseFeed: TChangeFeedStatus;
    FCriticalSection: TRTLCriticalSection;
    FFeeds: array[TFeedType] of TFeedThread;
    FLastError: TLastError;
    FLoginInfo: TLogin;
    FMutex: THandle;
    FOnDrop: TFeedDrop;
    FOnErrorItem: TErrorItemArrived;
    FOnHeartBeatItemArrived: THeartBeatItemArrived;
    FOnJsonArrived: TJsonArrived;
    FOnLoginError, FOnRequestError: TGenericError;
    FOnLogon, FOnLeave: TNotify;
    FOnPriceItem: TPriceItemArrived;
    FOnRequest: TFeedRequest;
    FOnRespose: TRequestDone;
    FOnTradeItem: TTradeItemArrived;
    FOnTradeOrderItem: TTradeOrderArrived;
    FOnUpdateOrderItem: TOrderItemArrived;
    FOpenFeed: TChangeFeedStatus;
    FPassword: string;
    FPublicKeyFile: string;
    FReconnectInfo: TFeedThreadReconnect;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FStatusInfo: TStatus;
    FUserName: string;
    function CreateAuthBundle(const UserName, Password, PublicKeyFile: string): AnsiString;
    function CreateGlobalLoginFlag(const Login: string): Boolean;
    function ExtractPath: string;
    function FeedTypeBySender(const Sender: TFeedThread): TFeedType;
    function GetFeedActive(const FeedType: TFeedType): Boolean;
    function GetTypeOfFeedItem(const FeedItemData: string; out DataJSON: string): TFeedItemType;
    function ParseAccountsResponse(out Accounts: TAccounts): Boolean;
    function ParseErrorFeedItem(const DataJSON: string; out ErrorFeedItem: TErrorFeedItem): Boolean;
    function ParseErrorResponse: Boolean;
    function ParseInstrumentsResponse(out Instruments: TInstruments): Boolean;
    function ParseLoginResponse: Boolean;
    function ParseOrderFeedItem(const DataJSON: string; out Order: TOrder): Boolean;
    function ParseOrderReplyResponse(out OrderReply: TOrderReply): Boolean;
    function ParseOrdersResponse(const DataJSON: string; out Orders: TOrders): Boolean;
    function ParsePriceFeedItem(const DataJSON: string; out PriceFeedItem: TPriceFeedItem): Boolean;
    function ParseTouchResponse: Boolean;
    function ParseTradeFeedItem(const DataJSON: string; out TradeFeedItem: TTradeFeedItem): Boolean;
    function ParseTradeOrderItem(const DataJSON: string; out TradeOrder: TTradeOrder): Boolean;
    function URL(const Path: string): string;
    procedure AddRequestParams(const AddAuth: Boolean);
    procedure ChargeReconnectTimer(const Timeout: Integer);
    procedure CheckConnect;
    procedure CleanupReconnectTimer;
    procedure ClearLastError;
    procedure ClearLoginInfo;
    procedure ClearReconnectInfo;
    procedure ClearServerInfo;
    procedure DestroyGlobalLoginFlag;
    procedure DoDrop(const FeedType: TFeedType);
    procedure DoErrorItem(const FeedType: TFeedType; const ErrorItem: TErrorFeedItem);
    procedure DoHeartBeatItem(const FeedType: TFeedType);
    procedure DoJsonArrived(const FeedType: TFeedType; const JSON: string);
    procedure DoLeave;
    procedure DoLogin;
    procedure DoLoginError;
    procedure DoNextReconnectAttampt;
    procedure DoOrderItem(const FeedType: TFeedType; const Order: TOrder);
    procedure DoPriceItem(const FeedType: TFeedType; const PriceItem: TPriceFeedItem);
    procedure DoRequest(const FeedType: TFeedType; const JSON: string);
    procedure DoRequestError;
    procedure DoResponse;
    procedure DoTradeItem(const FeedType: TFeedType; const TradeItem: TTradeFeedItem);
    procedure DoTradeOrderItem(const FeedType: TFeedType; const TradeOrder: TTradeOrder);
    procedure OnFeedDisconnect(const Sender: TFeedThread);
    procedure OnFeedItem(const Sender: TFeedThread; const FeedItemData: UTF8String);
    procedure OnFeedRequest(const Sender: TFeedThread; const Command: UTF8String);
    procedure OnReconnectTimer(Sender: TObject);
  protected
    procedure GetServerInfo;
  public
    constructor Create(const BaseURL: string);
    destructor Destroy; override;
    function AddOrder(const accno: Integer; const Order: TPostOrder): TOrderReply;
    function DeleteOrder(const accno, order_id: Integer): TOrderReply;
    function GetAccounts: TAccounts;
    function GetInstruments(const query: string; const limit: Word): TInstruments; overload;
    function GetInstruments(const instrument_id: Integer): TInstruments; overload;
    function GetInstrumentsLookup(const lookup: string; const lookup_type: TLookupType): TInstruments;
    function GetOrders(const accno: Integer; const Deleted: Boolean = False): TOrders;
    function UpdateOrder(const accno, order_id: Integer; const volume: Integer; const price: Double; const currency: string): TOrderReply;
    procedure Touch;
    procedure CloseFeed(const FeedType: TFeedType);
    procedure Leave;
    procedure Login(const UserName, Password, PublicKeyFile: string);
    procedure OpenFeed(const FeedType: TFeedType);
    procedure SubscribeFeedOrderDepth(const FeedType: TFeedType; const i: string; const m: Integer);
    procedure SubscribeFeedPrice(const FeedType: TFeedType; const i: string; const m: Integer);
    procedure SubscribeFeedTrade(const FeedType: TFeedType; const i: string; const m: Integer);
    procedure UnsubscribeFeedOrder(const FeedType: TFeedType; const i: string; const m: Integer);
    procedure UnsubscribeFeedPrice(const FeedType: TFeedType; const i: string; const m: Integer);
    procedure UnsubscribeFeedTrade(const FeedType: TFeedType; const i: string; const m: Integer);
  public
    class function CreatePostOrderRecord(const market_id, volume: Integer): TPostOrder;
    class function GetDefaultAccountIndex(const Accounts: TAccounts): Integer;
    class function OrderReplyToStr(const OrderReply: TOrderReply): string;
    class function OrderToStr(const Order: TOrder): string;
    class function PostOrderToStr(const PostOrder: TPostOrder): string;
    class function TradeOrderToStr(const TradeOrder: TTradeOrder): string;
  public
    property BaseURL: string read FBaseURL;
    property Active: Boolean read FActive;
    property FeedActive[const FeedType: TFeedType]: Boolean read GetFeedActive;
    property StatusInfo: TStatus read FStatusInfo;
    property LoginInfo: TLogin read FLoginInfo;
  public
    property OnCloseFeed            : TChangeFeedStatus     read FCloseFeed              write FCloseFeed;
    property OnDrop                 : TFeedDrop             read FOnDrop                 write FOnDrop;
    property OnErrorItem            : TErrorItemArrived     read FOnErrorItem            write FOnErrorItem;
    property OnHeartBeatItemArrived : THeartBeatItemArrived read FOnHeartBeatItemArrived write FOnHeartBeatItemArrived;
    property OnJsonArrived          : TJsonArrived          read FOnJsonArrived          write FOnJsonArrived;
    property OnLeave                : TNotify               read FOnLeave                write FOnLeave;
    property OnLoginError           : TGenericError         read FOnLoginError           write FOnLoginError;
    property OnLogon                : TNotify               read FOnLogon                write FOnLogon;
    property OnOpenFeed             : TChangeFeedStatus     read FOpenFeed               write FOpenFeed;
    property OnPriceItem            : TPriceItemArrived     read FOnPriceItem            write FOnPriceItem;
    property OnRequest              : TFeedRequest          read FOnRequest              write FOnRequest;
    property OnRequestError         : TGenericError         read FOnRequestError         write FOnRequestError;
    property OnResponse             : TRequestDone          read FOnRespose              write FOnRespose;
    property OnTradeItem            : TTradeItemArrived     read FOnTradeItem            write FOnTradeItem;
    property OnTradeOrder           : TTradeOrderArrived    read FOnTradeOrderItem       write FOnTradeOrderItem;
    property OnUpdateOrder          : TOrderItemArrived     read FOnUpdateOrderItem      write FOnUpdateOrderItem;
  end;

implementation

type
  TPublicBase64Encoding = class(TBase64Encoding)
  public
    property LineSeparator: string read FLineSeparator;
  end;

{ TFeedThread }

procedure TFeedThread.ChargeReconnectTimer(const Timeout: Integer);
begin
  if Assigned(FReconnectInfo.Timer) then
    FReconnectInfo.Timer.Free;

  FReconnectInfo.Timer := TTimer.Create(nil);
  FReconnectInfo.Timer.OnTimer := OnReconnectTimer;
  FReconnectInfo.Timer.Interval := Timeout;
  FReconnectInfo.Timer.Enabled := True;
end;

procedure TFeedThread.CleanupReconnectTimer;
begin
  if Assigned(FReconnectInfo.Timer) then
    FreeAndNil(FReconnectInfo.Timer);
end;

procedure TFeedThread.ClearReconnectInfo;
begin
  if Assigned(FReconnectInfo.Timer) then
    FReconnectInfo.Timer.Enabled := False;
  FReconnectInfo.Attempt := 0;
end;

constructor TFeedThread.Create(const Host: string; const Port: Integer; const TLS: Boolean; const SessionKey: string; const OnRequest: TRawFeedRequest; const OnItemArrived: TRawFeedItemArrived; const OnDisconnected: TRawFeedDisconnected);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  OnTerminate := DoOnTerminate;

  FManualClosing := False;
  FReady := False;
  FSessionKey := SessionKey;
  FData := '';
  FTag := 0;
  FReconnectInfo.Attempt := 0;
  FReconnectInfo.Timer := nil;
  FOnRequest := OnRequest;
  FOnItemArrived := OnItemArrived;
  FOnDisconnected := OnDisconnected;

  FIdTCPClient := TIdTCPClient.Create(nil);
  if TLS then
    FIdTCPClient.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FIdTCPClient);
  FIdTCPClient.ReadTimeout := 250;
  FIdTCPClient.Host := Host;
  FIdTCPClient.Port := Port;
  FIdTCPClient.OnConnected := OnSocketConnect;
  FIdTCPClient.OnDisconnected := OnSocketDisconnect;

  FCriticalSection := TCriticalSection.Create;
end;

destructor TFeedThread.Destroy;
begin
  FManualClosing := True;
  CleanupReconnectTimer;
  FCriticalSection.Enter;
  FIdTCPClient.Free;
  FCriticalSection.Free;
  inherited;
end;

procedure TFeedThread.DoCheckNewData;
var
  Buf: TIdBytes;
  i, Len: Integer;
begin
  FCriticalSection.Enter;
  try
    try
      FIdTCPClient.IOHandler.ReadBytes(Buf, -1, False);
      Len := Length(FData);
      SetLength(FData, Len + Length(Buf));
      for i := 0 to Length(Buf) - 1 do
        FData[Len + i + 1] := AnsiChar(Buf[i]);
    except
      FReady := False;
    end;
  finally
    FCriticalSection.Leave;
  end;

  if not FReady then
    Synchronize(DoNextReconnectAttampt);
end;

procedure TFeedThread.DoDataArrived;
begin
  if Assigned(FOnItemArrived) then
    Synchronize(DoDataArrivedSync)
  else
    FData := '';
end;

procedure TFeedThread.DoDataArrivedSync;
var
  c, Start, Len: Integer;
  Done : Boolean;
begin
  FCriticalSection.Enter;
  try
    repeat
      Start := 1;
      while (Start <= Length(FData)) and (FData[Start] <> '{') do Inc(Start);
      if Start <= Length(FData) then
        begin
          Len := 1;
          c := 1;
          while (Start+Len <= Length(FData)) and (c > 0) do
            begin
              if FData[Start+Len] = '{' then
                Inc(c)
              else if FData[Start+Len] = '}' then
                Dec(c);
              Inc(Len);
            end;

          if c = 0 then
            begin
              FOnItemArrived(Self, Copy(FData, Start, Len));
              FData := Copy(FData, Start+Len, MaxInt);
              Done := (FData = '');
            end
          else
            Done := True;
        end
      else
        Done := True;
    until Done;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TFeedThread.DoNextReconnectAttampt;
begin
  Inc(FReconnectInfo.Attempt);
  case FReconnectInfo.Attempt of
    1:
      try
        FIdTCPClient.Connect;
      except
        DoNextReconnectAttampt;
      end;
    2: ChargeReconnectTimer(5000);
    3: ChargeReconnectTimer(30000);
  else
    Terminate;
  end;
end;

procedure TFeedThread.DoOnTerminate(Sender: TObject);
begin
  if not FManualClosing then
    FOnDisconnected(Self);
end;

procedure TFeedThread.Execute;
begin
  while not Terminated do
    if FReady then
      begin
        CleanupReconnectTimer;
        DoCheckNewData;
        if FData <> '' then DoDataArrived;
      end
    else
      Sleep(1000);
end;

procedure TFeedThread.OnReconnectTimer(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;
  try
    FIdTCPClient.Connect;
  except
    DoNextReconnectAttampt;
  end;
end;

procedure TFeedThread.OnSocketConnect(Sender: TObject);
begin
  if not Terminated then
    begin
      FReady := True;
      ClearReconnectInfo;
      PostCommand(UTF8String(Format('{"cmd":"login", "args":{"session_key":"%s"}}', [FSessionKey])));
    end;
end;

procedure TFeedThread.OnSocketDisconnect(Sender: TObject);
begin
  if not (FManualClosing or Terminated) then
    begin
      FReady := False;
      DoNextReconnectAttampt;
    end;
end;

procedure TFeedThread.PostCommand(const Data: UTF8String);
const
  EOL: AnsiString = #13#10;
var
  Len: Integer;
  Buf: TIdBytes;
begin
  if FReady then
    begin
      FOnRequest(Self, Data);
      Len := Length(Data);
      SetLength(Buf, Len+Length(EOL));
      Move(Data[1], Buf[0], Len);
      Move(EOL[1], Buf[Len], Length(EOL));

      FCriticalSection.Enter;
      try
        try
          FIdTCPClient.IOHandler.Write(Buf);
        except
          FReady := False;
          DoNextReconnectAttampt;
        end;
      finally
        FCriticalSection.Leave;
      end;
    end
  else
    raise ENordNetException.Create('Feed not ready');
end;

procedure TFeedThread.SetReady(const Value: Boolean);
begin
  if Value <> FReady then
    if Value then
      begin
        ClearReconnectInfo;
        try
          FIdTCPClient.Connect;
        except
          DoNextReconnectAttampt;
        end;
      end
    else
      raise ENordNetException.Create('Cannot make feed not ready manually');
end;

procedure TFeedThread.BeginFeed;
begin
  if not FReady then
  begin
    Start;
    try
      FIdTCPClient.Connect;
    except
      DoNextReconnectAttampt;
    end;
  end;
end;

{ TNordNet }

procedure TNordNet.AddRequestParams(const AddAuth: Boolean);
var
  Auth: AnsiString;
begin
  FRESTRequest.Params.Clear;
  FRESTRequest.Params.AddHeader('Accept-Language', 'en');
  if AddAuth then
  begin
    Auth := AnsiString(FLoginInfo.session_key + ':' + FLoginInfo.session_key);
    with FRESTRequest.Params.AddHeader('Authorization', 'Basic') do
    begin
      Options := [poDoNotEncode];
      Value := Value + ' ' + StringReplace(TNetEncoding.Base64.EncodeBytesToString(PAnsiChar(Auth), Length(Auth)), TPublicBase64Encoding(TNetEncoding.Base64).LineSeparator, '', [rfReplaceAll]);
    end;
  end;
end;

procedure TNordNet.ClearLastError;
begin
  FLastError.Code := '';
  FLastError.Message := '';
end;

procedure TNordNet.ClearLoginInfo;
begin
  FLoginInfo.environment := '';
  FLoginInfo.session_key := '';
  FLoginInfo.expires_in := 0;
  FLoginInfo.private_feed.hostname := '';
  FLoginInfo.private_feed.port := 0;
  FLoginInfo.private_feed.encrypted := False;
  FLoginInfo.public_feed.hostname := '';
  FLoginInfo.public_feed.port := 0;
  FLoginInfo.public_feed.encrypted := False;
end;

procedure TNordNet.ClearServerInfo;
begin
  FStatusInfo.timestamp := 0;
  FStatusInfo.valid_version := False;
  FStatusInfo.system_running := False;
  FStatusInfo.message := '';
end;

procedure TNordNet.CloseFeed(const FeedType: TFeedType);
begin
  if Assigned(FFeeds[FeedType]) then
  begin
    FFeeds[FeedType].Tag := 0;
    if FActive then
    begin
      FFeeds[FeedType].Free;
      FFeeds[FeedType] := nil;
    end;
  end;
  if Assigned(OnCloseFeed) then
    OnCloseFeed();
end;

constructor TNordNet.Create(const BaseURL: string);
var
  FT: TFeedType;
begin
  inherited Create;
  FActive := False;
  FBaseURL := BaseURL;
  FMutex := 0;
  for FT := Low(FFeeds) to High(FFeeds) do FFeeds[FT] := nil;

  FOnRespose := nil;
  FOnLogon := nil;
  FOnLeave := nil;
  FOnLoginError := nil;
  FOnRequestError := nil;
  FOnErrorItem := nil;
  FOnPriceItem := nil;
  FOnTradeItem := nil;
  FOnHeartBeatItemArrived := nil;
  FOnJsonArrived := nil;
  FOnRequest := nil;
  FOnDrop := nil;
  FOpenFeed := nil;
  FCloseFeed := nil;

  FReconnectInfo.Attempt := 0;
  FReconnectInfo.Timer := nil;

  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.Accept := 'application/json';
  FRESTClient.AcceptCharset := 'UTF-8,*;q=0.8';
  FRESTClient.HandleRedirects := True;
  FRESTClient.RaiseExceptionOn500 := False;
  FRESTClient.AutoCreateParams := True;

  FRESTResponse := TRESTResponse.Create(nil);
  FRESTResponse.ContentType := 'application/json';

  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.SynchronizedEvents := False;
  InitializeCriticalSection(FCriticalSection);

  ClearServerInfo;
  ClearLoginInfo;
  ClearLastError;
end;

destructor TNordNet.Destroy;
begin
  CleanupReconnectTimer;
  Leave;
  FRESTRequest.Free;
  FRESTResponse.Free;
  FRESTClient.Free;
  FOpenFeed := nil;
  FCloseFeed := nil;
  DeleteCriticalSection(FCriticalSection);
  inherited;
end;

procedure TNordNet.DestroyGlobalLoginFlag;
begin
  if FMutex <> 0 then
    begin
      CloseHandle(FMutex);
      FMutex := 0;
    end;
end;

function TNordNet.CreateAuthBundle(const UserName, Password, PublicKeyFile: string): AnsiString;
var
  Encrypted: AnsiString;
  InStream, OutStream: TBytesStream;
  RSAUtil: TRSAUtil;
begin
  InStream := TBytesStream.Create(BytesOf(EncodeString(UserName)+':'+EncodeString(Password)+':'+EncodeString(IntToStr(FStatusInfo.timestamp))));
  try
    OutStream := TBytesStream.Create;
    try
      RSAUtil := TRSAUtil.Create;
      try
        RSAUtil.PublicKey.LoadFromFile(PublicKeyFile);
        RSAUtil.PublicEncrypt(InStream, OutStream);
        SetLength(Encrypted, OutStream.Size);
        OutStream.Seek(0, soFromBeginning);
        OutStream.Read(PAnsiChar(Encrypted)^, OutStream.Size);
        Result := System.AnsiStrings.StringReplace(EncodeBase64(PAnsiChar(Encrypted), Length(Encrypted)), AnsiString(TPublicBase64Encoding(TNetEncoding.Base64).LineSeparator), '', [rfReplaceAll]);
      finally
        RSAUtil.Free;
      end;
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

function TNordNet.CreateGlobalLoginFlag(const Login: string): Boolean;
begin
  FMutex := CreateMutex(nil, False, PChar(LowerCase(StringReplace(URL(Login), '\', '/', [rfReplaceAll]))));
  if (FMutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
    begin
      DestroyGlobalLoginFlag;
      Result := False;
    end
  else
    Result := True;
end;

class function TNordNet.CreatePostOrderRecord(const market_id, volume: Integer): TPostOrder;
begin
  Result.market_id := market_id;
  Result.volume := volume;
  Result.identifier := '';
  Result.price := 0;
  Result.currency := '';
  Result.side := 'BUY';
  Result.order_type := 'NORMAL';
  Result.valid_until := '';
  Result.open_volume := 0;
  Result.reference := '';
  Result.activation_condition := '';
  Result.trigger_value := 0;
  Result.trigger_condition := '';
  Result.target_value := 0;
end;

function TNordNet.DeleteOrder(const accno, order_id: Integer): TOrderReply;
begin
  DoNextReconnectAttampt;
  if FActive then
  begin
    EnterCriticalSection(FCriticalSection);
    try
      FRESTClient.BaseURL := URL('accounts/{accno}/orders/{order_id}');
      FRESTClient.Params.ParameterByName('accno').Value := IntToStr(accno);
      FRESTClient.Params.ParameterByName('order_id').Value := IntToStr(order_id);
      FRESTClient.ContentType := 'application/json';
      FRESTRequest.Method := rmDELETE;
      FRESTRequest.Body.ClearBody;
      AddRequestParams(True);

      FRESTRequest.Execute;
      DoResponse;
      if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
        begin
          if not ParseOrderReplyResponse(Result) then
            raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content]);
        end
      else
        if FRESTResponse.StatusCode = 403 then
          raise ENordNetException.Create('User is logged in but user or system does not have priviliges to use this endpoint')
        else
          if (FRESTResponse.StatusCode <= 400) or (FRESTResponse.StatusCode <= 401) then
            if ParseErrorResponse then
              DoRequestError
            else
              raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content])
          else
            raise ENordNetException.CreateFmt('API not ready, HTTP status: %d %s', [FRESTResponse.StatusCode, FRESTResponse.StatusText]);
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
  end
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

procedure TNordNet.DoDrop(const FeedType: TFeedType);
begin
  if Assigned(FOnDrop) then
    try
      FOnDrop(Self, FeedType);
    except
      // supress user exception
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoDrop', E.Message);
    end;
end;

procedure TNordNet.DoErrorItem(const FeedType: TFeedType; const ErrorItem: TErrorFeedItem);
begin
  if Assigned(FOnErrorItem) then
    try
      FOnErrorItem(Self, FeedType, ErrorItem);
    except
      // supress user exception
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoErrorItem', E.Message);
    end;
end;

procedure TNordNet.DoHeartBeatItem(const FeedType: TFeedType);
begin
  try
    if Assigned(FOnHeartBeatItemArrived) then
      FOnHeartBeatItemArrived(Self, FeedType);
    if Assigned(FOpenFeed) then
      OnOpenFeed();
  except
    // supress user exception
    on E: Exception do
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoHeartBeatItem', E.Message);
  end;
end;

procedure TNordNet.DoOrderItem(const FeedType: TFeedType; const Order: TOrder);
begin
  try
    if Assigned(FOnUpdateOrderItem) then
      FOnUpdateOrderItem(Self, Order);
  except
    // supress user exception
    on E: Exception do
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoOrderItem', E.Message);
  end;
end;

procedure TNordNet.DoTradeOrderItem(const FeedType: TFeedType; const TradeOrder: TTradeOrder);
begin
  try
    if Assigned(FOnTradeOrderItem) then
      FOnTradeOrderItem(Self, TradeOrder);
  except
    // supress user exception
    on E: Exception do
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoOrderTradeItem', E.Message);
  end;
end;

procedure TNordNet.DoLeave;
begin
  if Assigned(FOnLeave) then
    try
      FOnLeave(Self);
    except
      // supress user exception
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoLeave', E.Message);
    end;
end;

procedure TNordNet.DoLogin;
begin
  if Assigned(FOnLogon) then
    try
      FOnLogon(Self);
    except
      // supress user exception
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoLogin', E.Message);
    end;
end;

procedure TNordNet.DoLoginError;
begin
  if Assigned(FOnLoginError) then
    try
      FOnLoginError(Self, FLastError.Code, FLastError.Message);
    except
      // supress user exception
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoLoginError', E.Message);
    end;
end;

procedure TNordNet.DoPriceItem(const FeedType: TFeedType; const PriceItem: TPriceFeedItem);
begin
  if Assigned(FOnPriceItem) then
    try
      FOnPriceItem(Self, FeedType, PriceItem);
    except
      // supress user exception
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoPriceItem', E.Message);
    end;
end;

procedure TNordNet.DoRequest(const FeedType: TFeedType; const JSON: string);
begin
  if Assigned(FOnRequest) then
    try
      FOnRequest(Self, FeedType, JSON);
    except
      // supress user exception
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoRequest', E.Message);
    end;
end;

procedure TNordNet.DoJsonArrived(const FeedType: TFeedType; const JSON: string);
begin
  if Assigned(FOnJsonArrived) then
    try
      FOnJsonArrived(Self, FeedType, JSON);
    except
      // supress user exception
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoJsonArrived', E.Message);
    end;
end;

procedure TNordNet.DoRequestError;
begin
  if Assigned(FOnRequestError) then
    try
      FOnRequestError(Self, FLastError.Code, FLastError.Message);
    except
      // supress user exception
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoRequestError', E.Message);
    end;
end;

procedure TNordNet.DoResponse;
begin
  if Assigned(FOnRespose) then
    try
      FOnRespose(Self, ExtractPath, FRESTResponse.StatusCode, FRESTResponse.StatusText, FRESTResponse.Content);
    except
      // supress user exception
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoResponse', E.Message);
    end;
end;

procedure TNordNet.DoTradeItem(const FeedType: TFeedType; const TradeItem: TTradeFeedItem);
begin
  if Assigned(FOnTradeItem) then
    try
      FOnTradeItem(Self, FeedType, TradeItem);
    except
      // supress user exception
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DoTradeItem', E.Message);
    end;
end;

function TNordNet.ExtractPath: string;
begin
  Result := Copy(FRESTClient.BaseURL, Length(FBaseURL) + 1, MaxInt);
  if Result = '' then
    Result := '/';
end;

function TNordNet.GetAccounts: TAccounts;
begin
  DoNextReconnectAttampt;
  if FActive then
  begin
    FRESTClient.BaseURL := URL('accounts');
    FRESTRequest.Method := rmGET;
    FRESTRequest.Body.ClearBody;
    AddRequestParams(True);

    FRESTRequest.Execute;
    DoResponse;
    if FRESTResponse.StatusCode = 204 then
      SetLength(Result, 0)
    else
      if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
        begin
          if not ParseAccountsResponse(Result) then
            raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content]);
        end
      else
        if FRESTResponse.StatusCode = 403 then
          raise ENordNetException.Create('User is logged in but user or system does not have priviliges to use this endpoint')
        else
          if (FRESTResponse.StatusCode >= 400) or (FRESTResponse.StatusCode <= 401) then
            if ParseErrorResponse then
              DoRequestError
            else
              raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content])
          else
            raise ENordNetException.CreateFmt('API not ready, HTTP status: %d %s', [FRESTResponse.StatusCode, FRESTResponse.StatusText]);
  end
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

class function TNordNet.GetDefaultAccountIndex(const Accounts: TAccounts): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := 0;
  while i < Length(Accounts) do
    if Accounts[i].default and (not Accounts[i].is_blocked) then
      begin
        Result := i;
        i := High(i);
      end
    else
      Inc(i);
end;

function TNordNet.FeedTypeBySender(const Sender: TFeedThread): TFeedType;
var
  i: Byte;
begin
  Result := Low(Result);
  i := Byte(Result);
  while i <= Byte(High(Result)) do
    if Sender = FFeeds[TFeedType(i)] then
      begin
        Result := TFeedType(i);
        i := High(i);
      end
    else
      Inc(i);
end;

function TNordNet.GetInstruments(const query: string; const limit: Word): TInstruments;
begin
  DoNextReconnectAttampt;
  if FActive then
  begin
    FRESTClient.BaseURL := URL('instruments?query={query}&limit={limit}');
    FRESTClient.Params.ParameterByName('query').Value := query;
    FRESTClient.Params.ParameterByName('limit').Value := IntToStr(limit);
    FRESTRequest.Method := rmGET;
    FRESTRequest.Body.ClearBody;
    AddRequestParams(True);

    FRESTRequest.Execute;
    DoResponse;
    if FRESTResponse.StatusCode = 204 then
      SetLength(Result, 0)
    else
      if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
        begin
          if not ParseInstrumentsResponse(Result) then
            raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content]);
        end
      else
        if (FRESTResponse.StatusCode >= 400) or (FRESTResponse.StatusCode <= 401) then
          if ParseErrorResponse then
            DoRequestError
          else
            raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content])
        else
          raise ENordNetException.CreateFmt('API not ready, HTTP status: %d %s', [FRESTResponse.StatusCode, FRESTResponse.StatusText]);
  end
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

function TNordNet.GetInstruments(const instrument_id: Integer): TInstruments;
begin
  DoNextReconnectAttampt;
  if FActive then
  begin
    FRESTClient.BaseURL := URL('instruments/{instrument_id}');
    FRESTClient.Params.ParameterByName('instrument_id').Value := instrument_id.ToString;
    FRESTRequest.Method := rmGET;
    FRESTRequest.Body.ClearBody;
    AddRequestParams(True);

    FRESTRequest.Execute;
    DoResponse;
    if FRESTResponse.StatusCode = 204 then
      SetLength(Result, 0)
    else
      if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
        begin
          if not ParseInstrumentsResponse(Result) then
            raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content]);
        end
      else
        if (FRESTResponse.StatusCode >= 400) or (FRESTResponse.StatusCode <= 401) then
          if ParseErrorResponse then
            DoRequestError
          else
            raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content])
        else
          raise ENordNetException.CreateFmt('API not ready, HTTP status: %d %s', [FRESTResponse.StatusCode, FRESTResponse.StatusText]);
  end
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

function TNordNet.GetInstrumentsLookup(const lookup: string; const lookup_type: TLookupType): TInstruments;
const
   LookupTypeString: array [TLookupType] of string = ('market_id_identifier', 'isin_code_currency_market_id');
begin
  DoNextReconnectAttampt;
  if FActive then
  begin
    FRESTClient.BaseURL := URL('instruments/lookup/{lookup_type}/{lookup}');
    FRESTClient.Params.ParameterByName('lookup').Value := lookup;
    FRESTClient.Params.ParameterByName('lookup_type').Value := LookupTypeString[lookup_type];
    FRESTRequest.Method := rmGET;
    FRESTRequest.Body.ClearBody;
    AddRequestParams(True);

    FRESTRequest.Execute;
    DoResponse;
    if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
    begin
      if not ParseInstrumentsResponse(Result) then
        raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content]);
    end
    else if (FRESTResponse.StatusCode >= 400) or (FRESTResponse.StatusCode <= 401) then
      if ParseErrorResponse then
        DoRequestError
      else
        raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content])
    else
      raise ENordNetException.CreateFmt('API not ready, HTTP status: %d %s', [FRESTResponse.StatusCode, FRESTResponse.StatusText]);
  end
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

function TNordNet.GetOrders(const accno: Integer; const Deleted: Boolean): TOrders;
begin
  DoNextReconnectAttampt;
  if FActive then
  begin
    FRESTClient.BaseURL := URL('accounts/{accno}/orders');
    FRESTClient.Params.ParameterByName('accno').Value := IntToStr(accno);
    FRESTRequest.Method := rmGET;
    FRESTRequest.Body.ClearBody;
    AddRequestParams(True);
    if Deleted then
      with FRESTRequest.Params.AddItem do
        begin
          Kind := pkGETorPOST;
          Name := 'deleted';
          Value := 'true';
          ContentType := ctNone;
        end;

    FRESTRequest.Execute;
    DoResponse;
    if FRESTResponse.StatusCode = 204 then
      SetLength(Result, 0)
    else
      if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
        begin
          if not ParseOrdersResponse(FRESTResponse.Content, Result) then
            raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content]);
        end
      else
        if FRESTResponse.StatusCode = 403 then
          raise ENordNetException.Create('User is logged in but user or system does not have priviliges to use this endpoint')
        else
          if (FRESTResponse.StatusCode >= 400) or (FRESTResponse.StatusCode <= 401) then
            if ParseErrorResponse then
              DoRequestError
            else
              raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content])
          else
            raise ENordNetException.CreateFmt('API not ready, HTTP status: %d %s', [FRESTResponse.StatusCode, FRESTResponse.StatusText]);
  end
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

function TNordNet.GetFeedActive(const FeedType: TFeedType): Boolean;
begin
  Result := Assigned(FFeeds[FeedType]);
end;

procedure TNordNet.GetServerInfo;
var
  JSON: TJSONObject;
begin
  ClearServerInfo;

  FRESTClient.BaseURL := FBaseURL;
  FRESTRequest.Method := rmGET;
  FRESTRequest.Body.ClearBody;
  AddRequestParams(False);
  FRESTRequest.Execute;
  DoResponse;
  if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
    try
      JSON := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject;
      try
        if JSON.TryGetValue<Int64>('timestamp', FStatusInfo.timestamp) and
           JSON.TryGetValue<Boolean>('valid_version', FStatusInfo.valid_version) and
           JSON.TryGetValue<Boolean>('system_running', FStatusInfo.system_running) and
           JSON.TryGetValue<string>('message', FStatusInfo.message) then
          begin
            if not (FStatusInfo.valid_version and FStatusInfo.system_running) then
              raise ENordNetException.CreateFmt('API not ready: "%s"', [FStatusInfo.message]);
          end
        else
          raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content]);
      finally
        FreeAndNil(JSON);
      end;
    except
      raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content]);
    end
  else
    raise ENordNetException.CreateFmt('API not ready, HTTP status: %d %s', [FRESTResponse.StatusCode, FRESTResponse.StatusText]);
end;

function TNordNet.GetTypeOfFeedItem(const FeedItemData: string; out DataJSON: string): TFeedItemType;
var
  TypeStr: string;
  OrderId: Integer;
  DataObj: TJSONObject;
begin
//  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'GetTypeOfFeedItem', FeedItemData);
  try
    with TJSONObject.ParseJSONValue(FeedItemData) as TJSONObject do
      try
        if TryGetValue<string>('type', TypeStr) then
        begin
          if TypeStr = 'err' then
            Result := fitError
          else if TypeStr = 'heartbeat' then
            Result := fitHeartBeat
          else if TypeStr = 'price' then
            Result := fitPrice
          else if TypeStr = 'order' then
            Result := fitOrder
          else if TypeStr = 'depth' then
            Result := fitDepth
          else if TypeStr = 'trade' then
            Result := fitTrade
          else if TypeStr = 'index' then
            Result := fitIndex
          else if TypeStr = 'news' then
            Result := fitNews
          else if TypeStr = 'trading_status' then
            Result := fitTradingStatus
          else
            Result := fitUnknown;

          if TryGetValue<TJSONObject>('data', DataObj) then
          begin
            DataJSON := DataObj.ToJSON;
            if (Result = fitTrade) then
              with TJSONObject.ParseJSONValue(DataJSON) as TJSONObject do
                if TryGetValue<Integer>('order_id', OrderId) and (OrderId > 0) then
                begin
//                  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'GetTypeOfFeedItem', 'Result := fitOrder');
                  Result := fitTradeOrder;
                end;
          end
          else
            DataJSON := '';
        end
        else
        begin
          DataJSON := '';
          Result := fitUnknown;
        end;
      finally
        Free;
      end;
  except
    DataJSON := '';
    Result := fitUnknown;
  end;
//  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'GetTypeOfFeedItem', DataJSON);
end;

procedure TNordNet.Leave;
begin
//  if FActive then
  begin
    FActive := False;
    CloseFeed(ftPrivate);
    CloseFeed(ftPublic);
    DestroyGlobalLoginFlag;
    DoLeave;
    ClearServerInfo;
    ClearLoginInfo;
    ClearLastError;
    ClearReconnectInfo;
    if Assigned(OnCloseFeed) then
      OnCloseFeed();
  end;
end;

procedure TNordNet.Login(const UserName, Password, PublicKeyFile: string);
begin
  Leave;
  if CreateGlobalLoginFlag(UserName) then
  begin
    GetServerInfo;

    FRESTClient.BaseURL := URL('login');
    FRESTClient.ContentType := 'application/x-www-form-urlencoded';
    FRESTRequest.Method := rmPOST;
    FRESTRequest.Body.ClearBody;
    AddRequestParams(False);

    with FRESTRequest.Params.AddItem do
      begin
        Kind := pkGETorPOST;
        name := 'service';
        Value := 'NEXTAPI';
        ContentType := ctNone;
      end;

    with FRESTRequest.Params.AddItem do
      begin
        Kind := pkGETorPOST;
        name := 'auth';
        Value := string(CreateAuthBundle(UserName, Password, PublicKeyFile));
        ContentType := ctNone;
      end;
    FRESTRequest.Execute;
    DoResponse;
    if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
      if ParseLoginResponse then
        begin
          FActive := True;
          DoLogin;
          FUserName := UserName;
          FPassword := Password;
          FPublicKeyFile := PublicKeyFile;
        end
      else
        raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content])
    else
    begin
      DestroyGlobalLoginFlag;
      if (FRESTResponse.StatusCode <= 400) or (FRESTResponse.StatusCode <= 401) then
        if ParseErrorResponse then
          DoLoginError
        else
          raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content])
      else
        raise ENordNetException.CreateFmt('API not ready, HTTP status: %d %s', [FRESTResponse.StatusCode, FRESTResponse.StatusText]);
    end;
  end
  else
    raise ENordNetException.Create('Only one session with same user name is allowed');
end;

procedure TNordNet.OnFeedDisconnect(const Sender: TFeedThread);
var
  FeedType: TFeedType;
begin
  FeedType := FeedTypeBySender(Sender);
  FFeeds[FeedType] := nil;
  DoDrop(FeedType);
end;

procedure TNordNet.OnFeedItem(const Sender: TFeedThread; const FeedItemData: UTF8String);
var
  JSON, DataJSON: string;
  FeedType: TFeedType;
  ErrorFeedItem: TErrorFeedItem;
  PriceFeedItem: TPriceFeedItem;
  TradeFeedItem: TTradeFeedItem;
  OrderFeedItem: TOrder;
  TradeOrder: TTradeOrder;
begin
  FeedType := FeedTypeBySender(Sender);
  JSON := string(FeedItemData);
  DoJsonArrived(FeedType, JSON);
//  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'OnFeedItem', JSON);
  case GetTypeOfFeedItem(JSON, DataJSON) of
    fitHeartBeat:
      DoHeartBeatItem(FeedType);
    fitOrder:
      if ParseOrderFeedItem(string(DataJSON), OrderFeedItem) then
        DoOrderItem(FeedType, OrderFeedItem);
    fitTradeOrder:
      if ParseTradeOrderItem(string(DataJSON), TradeOrder) then
        DoTradeOrderItem(FeedType, TradeOrder);
    fitError:
      if ParseErrorFeedItem(string(DataJSON), ErrorFeedItem) then
        DoErrorItem(FeedType, ErrorFeedItem);
    fitPrice:
      if ParsePriceFeedItem(string(DataJSON), PriceFeedItem) then
        DoPriceItem(FeedType, PriceFeedItem);
    fitTrade:
      if ParseTradeFeedItem(string(DataJSON), TradeFeedItem) then
        DoTradeItem(FeedType, TradeFeedItem);
  end;
end;

procedure TNordNet.OnFeedRequest(const Sender: TFeedThread; const Command: UTF8String);
begin
  DoRequest(FeedTypeBySender(Sender), string(Command));
end;

procedure TNordNet.OpenFeed(const FeedType: TFeedType);
var
  Options: TFeed;
begin
  if FActive then
    if Assigned(FFeeds[FeedType]) then
      if FFeeds[FeedType].Ready then
        raise ENordNetException.Create('Already opened')
      else
        FFeeds[FeedType].Ready := True
    else
      begin
        case FeedType of
          ftPrivate: Options := FLoginInfo.private_feed;
          ftPublic: Options := FLoginInfo.public_feed;
        else
          Options.hostname := '';
          Options.port := 0;
          Options.encrypted := False;
        end;

        if (Options.hostname <> '') and (Options.port > 0) then
          begin
            FFeeds[FeedType] := TFeedThread.Create(Options.hostname, Options.port, Options.encrypted, FLoginInfo.session_key, OnFeedRequest, OnFeedItem, OnFeedDisconnect);
            try
              FFeeds[FeedType].BeginFeed;
              if Assigned(OnOpenFeed) then
                OnOpenFeed();
            except
              on E: Exception do
                begin
                  FFeeds[FeedType].Free;
                  FFeeds[FeedType] := nil;
                  raise;
                end;
            end;
          end
        else
          raise ENordNetException.Create('Feed parameters not set by server');
      end
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

class function TNordNet.OrderReplyToStr(const OrderReply: TOrderReply): string;
var
  sb: TStringBuilder;
begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat(' OrderId=%d %s,', [OrderReply.order_id, DateTimeToStr(Now)])
      .AppendFormat(' ResultCode=%s,', [OrderReply.result_code])
      .AppendFormat(' OrderState=%s,', [OrderReply.order_state])
      .AppendFormat(' ActionState=%s,', [OrderReply.action_state])
      .AppendFormat(' Message=%s,', [OrderReply.message]);
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

class function TNordNet.TradeOrderToStr(const TradeOrder: TTradeOrder): string;
var
  sb: TStringBuilder;
begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat(' OrderId=%d %s,', [TradeOrder.order_id, DateTimeToStr(Now)])
      .AppendFormat(' Accno=%d,', [TradeOrder.accno])
      .AppendFormat(' TradeId=%s,', [TradeOrder.trade_id])
      .AppendFormat(' Volume=%f,', [TradeOrder.volume])
      .AppendFormat(' Side=%s,', [TradeOrder.side])
      .AppendFormat(' Price=%f,', [TradeOrder.price.value])
      .AppendFormat(' Currency=%s,', [TradeOrder.price.currency])
      .AppendFormat(' Identifier=%s,', [TradeOrder.tradable.identifier])
      .AppendFormat(' MarketId=%d,', [TradeOrder.tradable.market_id])
      .AppendFormat(' Counterparty=%s,', [TradeOrder.counterparty])
      .AppendFormat(' Tradetime=%s', [TradeOrder.tradetime]);
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

class function TNordNet.OrderToStr(const Order: TOrder): string;
var
  sb: TStringBuilder;
begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat(' OrderId=%d %s,', [Order.order_id, DateTimeToStr(Now)])
      .AppendFormat(' ActionState=%s,', [Order.action_state])
      .AppendFormat(' OrderType=%s,', [Order.order_type])
      .AppendFormat(' OrderState=%s,', [Order.order_state])
      .AppendFormat(' Price=%f,', [Order.price.value])
      .AppendFormat(' Currency=%s,', [Order.price.currency])
      .AppendFormat(' Volume=%f,', [Order.volume])
      .AppendFormat(' Identifier=%s,', [Order.tradable.identifier])
      .AppendFormat(' MarketId=%d,', [Order.tradable.market_id])
      .AppendFormat(' OpenVolume=%f,', [Order.open_volume])
      .AppendFormat(' TradedVolume=%f,', [Order.traded_volume])
      .AppendFormat(' Side=%s,', [Order.side])
      .AppendFormat(' Reference=%s,', [Order.reference])
      .AppendFormat(' PriceCondition=%s,', [Order.price_condition])
      .AppendFormat(' VolumeCondition=%s,', [Order.volume_condition])
      .AppendFormat(' ValidityType=%s,', [Order.validity.type_])
      .AppendFormat(' ValidityUntil=%d,', [Order.validity.valid_until])
      .Append(' ActivationCondition: (')
      .AppendFormat('   TrailingValue=%f,', [Order.activation_condition.trailing_value])
      .AppendFormat('   TriggerCondition=%s,', [Order.activation_condition.trigger_condition])
      .AppendFormat('   TriggerValue=%f,', [Order.activation_condition.trigger_value])
      .AppendFormat('   Type=%s)', [Order.activation_condition.type_]);
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

class function TNordNet.PostOrderToStr(const PostOrder: TPostOrder): string;
var
  sb: TStringBuilder;
begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    sb.AppendFormat(' Identifier=%s,', [PostOrder.identifier])
      .AppendFormat(' TimeStamp=%s,', [DateTimeToStr(Now)])
      .AppendFormat(' MarketId=%d,', [PostOrder.market_id])
      .AppendFormat(' Price=%f,', [PostOrder.price])
      .AppendFormat(' Currency=%s,', [PostOrder.currency])
      .AppendFormat(' Volume=%d,', [PostOrder.volume])
      .AppendFormat(' Side=%s,', [PostOrder.side])
      .AppendFormat(' OrderType=%s,', [PostOrder.order_type])
      .AppendFormat(' OpenVolume=%d,', [PostOrder.open_volume])
      .AppendFormat(' ValidUntil=%s,', [PostOrder.valid_until])
      .AppendFormat(' Reference=%s,', [PostOrder.reference])
      .AppendFormat(' ActivationCondition=%s,', [PostOrder.activation_condition])
      .AppendFormat(' TriggerValue=%f,', [PostOrder.trigger_value])
      .AppendFormat(' TriggerCondition=%s,', [PostOrder.trigger_condition])
      .AppendFormat(' target_value=%f,', [PostOrder.target_value]);
    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

function TNordNet.ParseAccountsResponse(out Accounts: TAccounts): Boolean;
var
  JSON: TJSONArray;
  i: Integer;
begin
  try
    JSON := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONArray;
    try
      Result := True;
      SetLength(Accounts, JSON.Count);
      i := 0;
      while (i < JSON.Count) and Result do
        if JSON.Items[i].TryGetValue<Integer>('accno', Accounts[i].accno) and
           JSON.Items[i].TryGetValue<string>('type', Accounts[i].type_) and
           JSON.Items[i].TryGetValue<Boolean>('default', Accounts[i].default) and
           JSON.Items[i].TryGetValue<string>('alias', Accounts[i].alias) then
          begin
            if not JSON.Items[i].TryGetValue<Boolean>('is_blocked', Accounts[i].is_blocked) then
              Accounts[i].is_blocked := False;
            if not JSON.Items[i].TryGetValue<string>('blocked_reason', Accounts[i].blocked_reason) then
              Accounts[i].blocked_reason := '';

            Inc(i);
          end
        else
          Result := False;

      if not Result then
        SetLength(Accounts, 0);
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParseAccountsResponse', E.Message + '<br>' + FRESTResponse.Content);
      Result := False;
    end;
  end;
end;

function TNordNet.ParseOrderReplyResponse(out OrderReply: TOrderReply): Boolean;
begin
  try
    with TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject do
      try
        if TryGetValue<Integer>('order_id', OrderReply.order_id) and
           TryGetValue<string>('result_code', OrderReply.result_code) and
           TryGetValue<string>('action_state', OrderReply.action_state) then
          begin
            if not TryGetValue<string>('order_state', OrderReply.order_state) then
              OrderReply.order_state := '';
            if not TryGetValue<string>('message', OrderReply.message) then
              OrderReply.message := '';
            Result := True;
          end
        else
          Result := False;
      finally
        Free;
      end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParseOrderFeedItem', E.Message + '<br>' + FRESTResponse.Content);
      Result := False;
    end;
  end;
end;

function TNordNet.ParseTradeOrderItem(const DataJSON: string; out TradeOrder: TTradeOrder): Boolean;
var
  AmountJSON   : TJSONObject;
  JSON         : TJSONObject;
  TradableJSON : TJSONObject;
begin
  try
    JSON := TJSONObject.ParseJSONValue(DataJSON) as TJSONObject;
    try
      Result := True;

      if JSON.TryGetValue<Integer>('accno', TradeOrder.accno) and
         JSON.TryGetValue<Integer>('order_id', TradeOrder.order_id) and
         JSON.TryGetValue<string>('trade_id', TradeOrder.trade_id) then
      begin
        if not JSON.TryGetValue<Double>('volume', TradeOrder.volume) then
          TradeOrder.volume := 0;
        if not JSON.TryGetValue<string>('side', TradeOrder.side) then
          TradeOrder.side := '';
        if not JSON.TryGetValue<string>('counterparty', TradeOrder.counterparty) then
          TradeOrder.counterparty := '';
        if not JSON.TryGetValue<string>('tradetime', TradeOrder.tradetime) then
          TradeOrder.tradetime := '';
        if JSON.TryGetValue<TJSONObject>('price', AmountJSON) then
        begin
          if not AmountJSON.TryGetValue<Double>('value', TradeOrder.price.Value) then
            TradeOrder.price.Value := 0;
          if not AmountJSON.TryGetValue<string>('currency', TradeOrder.price.currency) then
            TradeOrder.price.currency := '';
        end;
        if JSON.TryGetValue<TJSONObject>('tradable', TradableJSON) then
        begin
          if not TradableJSON.TryGetValue<string>('identifier', TradeOrder.tradable.identifier) then
            TradeOrder.tradable.identifier := '';
          if not TradableJSON.TryGetValue<Integer>('market_id', TradeOrder.tradable.market_id) then
            TradeOrder.tradable.market_id := 0;
        end;
      end
      else
        Result := False;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParseTradeOrderItem', E.Message + '<br>' + DataJSON);
      Result := False;
    end;
  end;
end;

function TNordNet.ParseOrderFeedItem(const DataJSON: string; out Order: TOrder): Boolean;
var
  AmountJSON   : TJSONObject;
  JSON         : TJSONObject;
  SubJSON      : TJSONObject;
  TradableJSON : TJSONObject;
  ValidityJSON : TJSONObject;
begin
  try
    JSON := TJSONObject.ParseJSONValue(DataJSON) as TJSONObject;
    try
      Result := True;

      if JSON.TryGetValue<Integer>('accno', Order.accno) and
         JSON.TryGetValue<string>('action_state', Order.action_state) and
         JSON.TryGetValue<Int64>('modified', Order.modified) and
         JSON.TryGetValue<Integer>('order_id', Order.order_id) then
      begin
        if not JSON.TryGetValue<string>('side', Order.side) then
          Order.side := '';
        if not JSON.TryGetValue<string>('order_state', Order.order_state) then
          Order.order_state := '';
        if not JSON.TryGetValue<Double>('volume', Order.volume) then
          Order.volume := 0;
        if not JSON.TryGetValue<string>('volume_condition', Order.volume_condition) then
          Order.volume_condition := '';
        if not JSON.TryGetValue<Double>('traded_volume', Order.traded_volume) then
          Order.traded_volume := 0;
        if not JSON.TryGetValue<string>('price_condition', Order.price_condition) then
          Order.price_condition := '';
        if not JSON.TryGetValue<string>('order_type', Order.order_type) then
          Order.order_type := '';
        if not JSON.TryGetValue<string>('reference', Order.reference) then
          Order.reference := '';
        if JSON.TryGetValue<TJSONObject>('price', AmountJSON) then
        begin
          if not AmountJSON.TryGetValue<Double>('value', Order.price.Value) then
            Order.price.Value := 0;
          if not AmountJSON.TryGetValue<string>('currency', Order.price.currency) then
            Order.price.currency := '';
        end;
        if JSON.TryGetValue<TJSONObject>('tradable', TradableJSON) then
        begin
          if not TradableJSON.TryGetValue<string>('identifier', Order.tradable.identifier) then
            Order.tradable.identifier := '';
          if not TradableJSON.TryGetValue<Integer>('market_id', Order.tradable.market_id) then
            Order.tradable.market_id := 0;
        end;
        if JSON.TryGetValue<TJSONObject>('validity', ValidityJSON) then
        begin
          if not ValidityJSON.TryGetValue<string>('type', Order.validity.type_) then
            Order.validity.type_ := '';
          if not ValidityJSON.TryGetValue<Int64>('valid_until', Order.validity.valid_until) then
            Order.validity.valid_until := 0;
        end;
        if JSON.TryGetValue<TJSONObject>('activation_condition', SubJSON) and
           SubJSON.TryGetValue<string>('type', Order.activation_condition.type_) then
        begin
          if not SubJSON.TryGetValue<Double>('trailing_value', Order.activation_condition.trailing_value) then
            Order.activation_condition.trailing_value := 0;
          if not SubJSON.TryGetValue<Double>('trigger_value', Order.activation_condition.trigger_value) then
            Order.activation_condition.trigger_value := 0;
          if not SubJSON.TryGetValue<string>('trigger_condition', Order.activation_condition.trigger_condition) then
            Order.activation_condition.trigger_condition := '';
        end
        else
        begin
          Order.activation_condition.type_ := '';
          Order.activation_condition.trailing_value := 0;
          Order.activation_condition.trigger_value := 0;
          Order.activation_condition.trigger_condition := '';
        end;
      end
      else
        Result := False;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParseOrderFeedItem', E.Message + '<br>' + DataJSON);
      Result := False;
    end;
  end;
end;

function TNordNet.ParseErrorFeedItem(const DataJSON: string; out ErrorFeedItem: TErrorFeedItem): Boolean;
var
  CmdObj: TJSONObject;
begin
  try
    with TJSONObject.ParseJSONValue(DataJSON) as TJSONObject do
      try
        if TryGetValue<string>('msg', ErrorFeedItem.msg) then
          begin
            if TryGetValue<TJSONObject>('cmd', CmdObj) then
              ErrorFeedItem.cmd := CmdObj.ToJSON
            else
              ErrorFeedItem.cmd := '';

            Result := True;
          end
        else
          Result := False;
      finally
        Free;
      end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParseErrorFeedItem', E.Message + '<br>' + DataJSON);
      Result := False;
    end;
  end;
end;

function TNordNet.ParseErrorResponse: Boolean;
begin
  try
    with TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject do
      try
        if TryGetValue<string>('code', FLastError.Code) then
        begin
          if not TryGetValue<string>('message', FLastError.Message) then
            FLastError.Message := '';
          Result := True;
        end
        else
          Result := False;
      finally
        Free;
      end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParseErrorResponse', E.Message + '<br>' + FRESTResponse.Content);
      Result := False;
    end;
  end;
end;

function TNordNet.ParseInstrumentsResponse(out Instruments: TInstruments): Boolean;
var
  JSON, SubJSON: TJSONArray;
  i, j: Integer;
begin
  try
    JSON := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONArray;
    try
      Result := True;
      SetLength(Instruments, JSON.Count);
      i := 0;
      while (i < JSON.Count) and Result do
        if JSON.Items[i].TryGetValue<Integer>('instrument_id', Instruments[i].instrument_id) and
           JSON.Items[i].TryGetValue<string>('currency', Instruments[i].currency) and
           JSON.Items[i].TryGetValue<string>('instrument_type', Instruments[i].instrument_type) and
           JSON.Items[i].TryGetValue<string>('symbol', Instruments[i].symbol) and
           JSON.Items[i].TryGetValue<string>('name', Instruments[i].name) then
          begin
            if not JSON.Items[i].TryGetValue<string>('prospectus_url', Instruments[i].prospectus_url) then
              Instruments[i].prospectus_url := '';
            if not JSON.Items[i].TryGetValue<string>('instrument_group_type', Instruments[i].instrument_group_type) then
              Instruments[i].instrument_group_type := '';
            if not JSON.Items[i].TryGetValue<Double>('number_of_securities', Instruments[i].number_of_securities) then
              Instruments[i].number_of_securities := 0;
            if not JSON.Items[i].TryGetValue<Double>('multiplier', Instruments[i].multiplier) then
              Instruments[i].multiplier :=  0;
            if not JSON.Items[i].TryGetValue<Double>('strike_price', Instruments[i].strike_price) then
              Instruments[i].strike_price := 0;
            if not JSON.Items[i].TryGetValue<Double>('pawn_percentage', Instruments[i].pawn_percentage) then
              Instruments[i].pawn_percentage := 0;
            if not JSON.Items[i].TryGetValue<string>('isin_code', Instruments[i].isin_code) then
              Instruments[i].isin_code := '';
            if not JSON.Items[i].TryGetValue<string>('market_view', Instruments[i].market_view) then
              Instruments[i].market_view :=  '';
            if not JSON.Items[i].TryGetValue<string>('sector', Instruments[i].sector) then
              Instruments[i].sector := '';
            if not JSON.Items[i].TryGetValue<string>('sector_group', Instruments[i].sector_group) then
              Instruments[i].sector_group := '';
            if not JSON.Items[i].TryGetValue<string>('expiration_date', Instruments[i].expiration_date) then
              Instruments[i].expiration_date := '';

            if JSON.Items[i].TryGetValue<TJSONArray>('underlyings', SubJSON) then
              begin
                SetLength(Instruments[i].underlyings, SubJSON.Count);
                for j := 0 to SubJSON.Count-1 do
                  begin
                    if not SubJSON.Items[j].TryGetValue<Integer>('instrumment_id', Instruments[i].underlyings[j].instrumment_id) then
                      Instruments[i].underlyings[j].instrumment_id := 0;
                    if not SubJSON.Items[j].TryGetValue<Integer>('instrument_id', Instruments[i].underlyings[j].instrument_id) then
                      Instruments[i].underlyings[j].instrument_id := 0;
                    if not SubJSON.Items[j].TryGetValue<string>('symbol', Instruments[i].underlyings[j].symbol) then
                      Instruments[i].underlyings[j].symbol := '';
                    if not SubJSON.Items[j].TryGetValue<string>('isin_code', Instruments[i].underlyings[j].isin_code) then
                      Instruments[i].underlyings[j].isin_code := '';
                  end;
              end
            else
              SetLength(Instruments[i].underlyings, 0);

            if JSON.Items[i].TryGetValue<TJSONArray>('tradables', SubJSON) then
              begin
                SetLength(Instruments[i].tradables, SubJSON.Count);
                for j := 0 to SubJSON.Count-1 do
                  begin
                    if not SubJSON.Items[j].TryGetValue<Integer>('market_id', Instruments[i].tradables[j].market_id) then
                      Instruments[i].tradables[j].market_id := 0;
                    if not SubJSON.Items[j].TryGetValue<Integer>('display_order', Instruments[i].tradables[j].display_order) then
                      Instruments[i].tradables[j].display_order := 0;
                    if not SubJSON.Items[j].TryGetValue<Double>('lot_size', Instruments[i].tradables[j].lot_size) then
                      Instruments[i].tradables[j].lot_size := 0;
                    if not SubJSON.Items[j].TryGetValue<Integer>('tick_size_id', Instruments[i].tradables[j].tick_size_id) then
                      Instruments[i].tradables[j].tick_size_id := 0;
                    if not SubJSON.Items[j].TryGetValue<string>('identifier', Instruments[i].tradables[j].identifier) then
                      Instruments[i].tradables[j].identifier := '';
                  end;
              end
            else
              SetLength(Instruments[i].tradables, 0);

            Inc(i);
          end
        else
          Result := False;

      if not Result then
        SetLength(Instruments, 0);
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParseInstrumentsResponse', E.Message + '<br>' + FRESTResponse.Content);
      Result := False;
    end;
  end;
end;

function TNordNet.ParseLoginResponse: Boolean;
var
  JSON, SubJSON: TJSONObject;
begin
  try
    JSON := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject;
    try
      if JSON.TryGetValue<string>('session_key', FLoginInfo.session_key) and (FLoginInfo.session_key <> '') and
         JSON.TryGetValue<Integer>('expires_in', FLoginInfo.expires_in) and (FLoginInfo.expires_in > 0) and
         JSON.TryGetValue<string>('environment', FLoginInfo.environment) then
        begin
          if JSON.TryGetValue<TJSONObject>('private_feed', SubJSON) then
            begin
              if not SubJSON.TryGetValue<string>('hostname', FLoginInfo.private_feed.hostname) then
                FLoginInfo.private_feed.hostname := '';
              if not SubJSON.TryGetValue<Integer>('port', FLoginInfo.private_feed.port) then
                FLoginInfo.private_feed.port := 0;
              if not SubJSON.TryGetValue<Boolean>('encrypted', FLoginInfo.private_feed.encrypted) then
                FLoginInfo.private_feed.encrypted := False;
            end;

          if JSON.TryGetValue<TJSONObject>('public_feed', SubJSON) then
            begin
              if not SubJSON.TryGetValue<string>('hostname', FLoginInfo.public_feed.hostname) then
                FLoginInfo.public_feed.hostname := '';
              if not SubJSON.TryGetValue<Integer>('port', FLoginInfo.public_feed.port) then
                FLoginInfo.public_feed.port := 0;
              if not SubJSON.TryGetValue<Boolean>('encrypted', FLoginInfo.public_feed.encrypted) then
                FLoginInfo.public_feed.encrypted := False;
            end;

          Result := True;
        end
      else
        Result := False;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParseLoginResponse', E.Message + '<br>' + FRESTResponse.Content);
      Result := False;
    end;
  end;
end;

function TNordNet.ParseOrdersResponse(const DataJSON: string; out Orders: TOrders): Boolean;
var
  JSON: TJSONArray;
  AmountJSON, TradableJSON, ValidityJSON, SubJSON: TJSONObject;
  i: Integer;
begin
  try
    JSON := TJSONObject.ParseJSONValue(DataJSON) as TJSONArray;
    try
      Result := True;
      SetLength(Orders, JSON.Count);
      i := 0;
      while (i < JSON.Count) and Result do
        if JSON.Items[i].TryGetValue<Integer>('accno', Orders[i].accno) and
           JSON.Items[i].TryGetValue<Integer>('order_id', Orders[i].order_id) and
           JSON.Items[i].TryGetValue<TJSONObject>('price', AmountJSON) and
           JSON.Items[i].TryGetValue<Double>('volume', Orders[i].volume) and
           JSON.Items[i].TryGetValue<TJSONObject>('tradable', TradableJSON) and
           JSON.Items[i].TryGetValue<Double>('traded_volume', Orders[i].traded_volume) and
           JSON.Items[i].TryGetValue<string>('side', Orders[i].side) and
           JSON.Items[i].TryGetValue<Int64>('modified', Orders[i].modified) and
           JSON.Items[i].TryGetValue<string>('price_condition', Orders[i].price_condition) and
           JSON.Items[i].TryGetValue<string>('volume_condition', Orders[i].volume_condition) and
           JSON.Items[i].TryGetValue<TJSONObject>('validity', ValidityJSON) and
//           JSON.Items[i].TryGetValue<string>('action_state', Orders[i].action_state) and
           JSON.Items[i].TryGetValue<string>('order_type', Orders[i].order_type) and
           JSON.Items[i].TryGetValue<string>('order_state', Orders[i].order_state) then
          begin
            if not JSON.Items[i].TryGetValue<string>('action_state', Orders[i].action_state) then
              Orders[i].action_state := '';

            if not (AmountJSON.TryGetValue<Double>('value', Orders[i].price.value) and
                    AmountJSON.TryGetValue<string>('currency', Orders[i].price.currency)) then
              Result := False;

            if not (TradableJSON.TryGetValue<string>('identifier', Orders[i].tradable.identifier) and
                    TradableJSON.TryGetValue<Integer>('market_id', Orders[i].tradable.market_id)) then
              Result := False;

            if ValidityJSON.TryGetValue<string>('type', Orders[i].validity.type_) then
              begin
                if not ValidityJSON.TryGetValue<Int64>('valid_until', Orders[i].validity.valid_until) then
                  Orders[i].validity.valid_until := 0;
              end
            else
              Result := False;

            if not JSON.Items[i].TryGetValue<string>('reference', Orders[i].reference) then
              Orders[i].reference := '';
            if JSON.Items[i].TryGetValue<TJSONObject>('activation_condition', SubJSON) then
              if SubJSON.TryGetValue<string>('type', Orders[i].activation_condition.type_) then
                begin
                  if not SubJSON.TryGetValue<Double>('trailing_value', Orders[i].activation_condition.trailing_value) then
                    Orders[i].activation_condition.trailing_value := 0;
                  if not SubJSON.TryGetValue<Double>('trigger_value', Orders[i].activation_condition.trigger_value) then
                    Orders[i].activation_condition.trigger_value := 0;
                  if not SubJSON.TryGetValue<string>('trigger_condition', Orders[i].activation_condition.trigger_condition) then
                    Orders[i].activation_condition.trigger_condition := '';
                end
              else
                Result := False
            else
              begin
                Orders[i].activation_condition.type_ := '';
                Orders[i].activation_condition.trailing_value := 0;
                Orders[i].activation_condition.trigger_value := 0;
                Orders[i].activation_condition.trigger_condition := '';
              end;

            Inc(i);
          end
        else
          Result := False;

      if not Result then
        SetLength(Orders, 0);
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParseOrdersResponse', E.Message + '<br>' + DataJSON);
      Result := False;
    end;
  end;
end;

function TNordNet.ParsePriceFeedItem(const DataJSON: string; out PriceFeedItem: TPriceFeedItem): Boolean;
begin
  try
    with TJSONObject.ParseJSONValue(DataJSON) as TJSONObject do
      try
        if not TryGetValue<string>('i', PriceFeedItem.i) then
          PriceFeedItem.i := '';
        if not TryGetValue<Integer>('m', PriceFeedItem.m) then
          PriceFeedItem.m := 0;
        if not TryGetValue<string>('t', PriceFeedItem.t) then
          PriceFeedItem.t := '';
        if not TryGetValue<string>('trade_timestamp', PriceFeedItem.trade_timestamp) then
          PriceFeedItem.trade_timestamp := '';
        if not TryGetValue<string>('tick_timestamp', PriceFeedItem.tick_timestamp) then
          PriceFeedItem.tick_timestamp := '';
        if not TryGetValue<Double>('bid', PriceFeedItem.bid) then
          PriceFeedItem.bid := 0;
        if not TryGetValue<Integer>('bid_volume', PriceFeedItem.bid_volume) then
          PriceFeedItem.bid_volume := 0;
        if not TryGetValue<Double>('ask', PriceFeedItem.ask) then
          PriceFeedItem.ask := 0;
        if not TryGetValue<Integer>('ask_volume', PriceFeedItem.ask_volume) then
          PriceFeedItem.ask_volume := 0;
        if not TryGetValue<Double>('close', PriceFeedItem.close) then
          PriceFeedItem.close := 0;
        if not TryGetValue<Double>('high', PriceFeedItem.high) then
          PriceFeedItem.high := 0;
        if not TryGetValue<Double>('last', PriceFeedItem.last) then
          PriceFeedItem.last := 0;
        if not TryGetValue<Integer>('last_volume', PriceFeedItem.last_volume) then
          PriceFeedItem.last_volume := 0;
        if not TryGetValue<string>('lot_size', PriceFeedItem.lot_size) then
          PriceFeedItem.lot_size := '';
        if not TryGetValue<Double>('low', PriceFeedItem.low) then
          PriceFeedItem.low := 0;
        if not TryGetValue<Double>('open', PriceFeedItem.open) then
          PriceFeedItem.open := 0;
        if not TryGetValue<Double>('turnover', PriceFeedItem.turnover) then
          PriceFeedItem.turnover := 0;
        if not TryGetValue<Integer>('turnover_volume', PriceFeedItem.turnover_volume) then
          PriceFeedItem.turnover_volume := 0;
        Result := True;
      finally
        Free
      end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParsePriceFeedItem', E.Message + '<br>' + DataJSON);
      Result := False;
    end;
  end;
end;

function TNordNet.ParseTouchResponse: Boolean;
var
  JSON: TJSONObject;
  Logged: Boolean;
begin
  try
    JSON := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject;
    try
      if JSON.TryGetValue<Boolean>('logged_in', Logged) then
        begin
          if not Logged then Leave;
          Result := True;
        end
      else
        Result := False;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParseTouchResponse', E.Message + '<br>' + FRESTResponse.Content);
      Result := False;
    end;
  end;
end;

function TNordNet.ParseTradeFeedItem(const DataJSON: string; out TradeFeedItem: TTradeFeedItem): Boolean;
begin
//  TPublishers.LogPublisher.Write([ltLogWriter], Self, 'ParseTradeFeedItem', DataJSON);
  try
    with TJSONObject.ParseJSONValue(DataJSON) as TJSONObject do
      try
        if not TryGetValue<string>('i', TradeFeedItem.i) then
          TradeFeedItem.i := '';
        if not TryGetValue<Integer>('m', TradeFeedItem.m) then
          TradeFeedItem.m := 0;
        if not TryGetValue<string>('t', TradeFeedItem.t) then
          TradeFeedItem.t := '';
        if not TryGetValue<string>('trade_timestamp', TradeFeedItem.trade_timestamp) then
          TradeFeedItem.trade_timestamp := '';
        if not TryGetValue<Double>('price', TradeFeedItem.price) then
          TradeFeedItem.price := 0;
        if not TryGetValue<Integer>('volume', TradeFeedItem.volume) then
          TradeFeedItem.volume := 0;
        if not TryGetValue<Double>('baseprice', TradeFeedItem.baseprice) then
          TradeFeedItem.baseprice := 0;
        if not TryGetValue<string>('broker_buying', TradeFeedItem.broker_buying) then
          TradeFeedItem.broker_buying := '';
        if not TryGetValue<string>('broker_selling', TradeFeedItem.broker_selling) then
          TradeFeedItem.broker_selling := '';
        if not TryGetValue<string>('trade_type', TradeFeedItem.trade_type) then
          TradeFeedItem.trade_type := '';
        if not TryGetValue<string>('isin', TradeFeedItem.isin) then
          TradeFeedItem.isin := '';
        Result := True;
      finally
        Free;
      end;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ParseTradeFeedItem', E.Message + '<br>' + DataJSON);
      Result := False;
    end;
  end;
end;

function TNordNet.AddOrder(const accno: Integer; const Order: TPostOrder): TOrderReply;
var
  FS: TFormatSettings;
begin
  DoNextReconnectAttampt;
  if FActive then
  begin
    EnterCriticalSection(FCriticalSection);
    try
      FS := TFormatSettings.Create;
      FS.DecimalSeparator := '.';
      FS.ThousandSeparator := #0;

      FRESTClient.BaseURL := URL('accounts/{accno}/orders');
      FRESTClient.Params.ParameterByName('accno').Value := IntToStr(accno);
      FRESTClient.ContentType := 'application/json';
      FRESTRequest.Method := rmPOST;
      FRESTRequest.Body.ClearBody;
      AddRequestParams(True);

      with FRESTRequest.Params.AddItem do
        begin
          Kind := pkGETorPOST;
          Name := 'market_id';
          Value := IntToStr(Order.market_id);
          ContentType := ctNone;
        end;
      with FRESTRequest.Params.AddItem do
        begin
          Kind := pkGETorPOST;
          Name := 'volume';
          Value := IntToStr(Order.volume);
          ContentType := ctNone;
        end;
      if Order.identifier <> '' then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'identifier';
            Value := Order.identifier;
            ContentType := ctNone;
          end;
      if Order.price > 0  then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'price';
            Value := FormatFloat(C_CURRENCY_FORMAT, Order.price, FS);
            ContentType := ctNone;
          end;
      if Order.currency <> '' then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'currency';
            Value := Order.currency;
            ContentType := ctNone;
          end;
      if Order.side <> '' then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'side';
            Value := Order.side;
            ContentType := ctNone;
          end;
      if Order.order_type <> '' then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'order_type';
            Value := Order.order_type;
            ContentType := ctNone;
          end;
      if Order.valid_until <> '' then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'valid_until';
            Value := Order.valid_until;
            ContentType := ctNone;
          end;
      if Order.open_volume <> 0 then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'open_volume';
            Value := IntToStr(Order.open_volume);
            ContentType := ctNone;
          end;
      if Order.reference <> '' then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'reference';
            Value := Order.reference;
            ContentType := ctNone;
          end;
      if Order.activation_condition <> '' then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'activation_condition';
            Value := Order.activation_condition;
            ContentType := ctNone;
          end;
      if Order.trigger_value > 0 then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'trigger_value';
            Value := FormatFloat(C_CURRENCY_FORMAT, Order.trigger_value, FS);
            ContentType := ctNone;
          end;
      if Order.trigger_condition <> '' then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'trigger_condition';
            Value := Order.trigger_condition;
            ContentType := ctNone;
          end;
      if Order.target_value > 0 then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'target_value';
            Value := FormatFloat(C_CURRENCY_FORMAT, Order.target_value, FS);
            ContentType := ctNone;
          end;

      try
//        TPublishers.LogPublisher.Write([ltLogWriter], Self, 'AddOrder', 'Before FRESTRequest');
        FRESTRequest.Execute;
//        TPublishers.LogPublisher.Write([ltLogWriter], Self, 'AddOrder', 'After FRESTRequest');
      except
        on E:Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'AddOrder', E.Message);
          raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [E.Message]);
        end;
      end;
      DoResponse;
      if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
        begin
          if not ParseOrderReplyResponse(Result) then
            raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content]);
        end
      else
        if FRESTResponse.StatusCode = 403 then
          raise ENordNetException.Create('User is logged in but user or system does not have priviliges to use this endpoint')
        else
          if (FRESTResponse.StatusCode <= 400) or (FRESTResponse.StatusCode <= 401) then
            if ParseErrorResponse then
              DoRequestError
            else
              raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content])
          else
            raise ENordNetException.CreateFmt('API not ready, HTTP status: %d %s', [FRESTResponse.StatusCode, FRESTResponse.StatusText]);
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
  end
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

procedure TNordNet.SubscribeFeedOrderDepth(const FeedType: TFeedType; const i: string; const m: Integer);
begin
  if FActive then
    if Assigned(FFeeds[FeedType]) then
      if FFeeds[FeedType].Tag < 100 then // TODO: save real subscriptions
      begin
        FFeeds[FeedType].PostCommand(UTF8String(Format('{"cmd":"subscribe", "args":{"t":"depth", "i":"%s", "m":%d}}', [i, m])));
        FFeeds[FeedType].Tag := FFeeds[FeedType].Tag + 1;
      end
      else
        raise ENordNetException.Create('Maximum number of feed is 100 instruments')
    else
      raise ENordNetException.Create('Not opened')
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

procedure TNordNet.SubscribeFeedPrice(const FeedType: TFeedType; const i: string; const m: Integer);
begin
  if FActive then
    if Assigned(FFeeds[FeedType]) then
      if FFeeds[FeedType].Tag < 100 then // TODO: save real subscriptions
        begin
          FFeeds[FeedType].PostCommand(UTF8String(Format('{"cmd":"subscribe", "args":{"t":"price", "i":"%s", "m":%d}}', [i, m])));
          FFeeds[FeedType].Tag := FFeeds[FeedType].Tag + 1;
        end
      else
        raise ENordNetException.Create('Maximum number of feed is 100 instruments')
    else
      raise ENordNetException.Create('Not opened')
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

procedure TNordNet.SubscribeFeedTrade(const FeedType: TFeedType; const i: string; const m: Integer);
begin
  if FActive then
    if Assigned(FFeeds[FeedType]) then
      if FFeeds[FeedType].Tag < 100 then // TODO: save real subscriptions
        begin
          FFeeds[FeedType].PostCommand(UTF8String(Format('{"cmd":"subscribe", "args":{"t":"trade", "i":"%s", "m":%d}}', [i, m])));
          FFeeds[FeedType].Tag := FFeeds[FeedType].Tag + 1;
        end
      else
        raise ENordNetException.Create('Maximum number of feed is 100 instruments')
    else
      raise ENordNetException.Create('Not opened')
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

procedure TNordNet.UnsubscribeFeedOrder(const FeedType: TFeedType; const i: string; const m: Integer);
begin
  if FActive then
    if Assigned(FFeeds[FeedType]) then
    begin
      FFeeds[FeedType].PostCommand(UTF8String(Format('{"cmd":"unsubscribe", "args":{"t":"price", "i":"%s", "m":%d}}', [i, m])));
      if FFeeds[FeedType].Tag > 0 then
        FFeeds[FeedType].Tag := FFeeds[FeedType].Tag - 1;
    end
    else
      raise ENordNetException.Create('Not opened')
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

procedure TNordNet.UnsubscribeFeedPrice(const FeedType: TFeedType; const i: string; const m: Integer);
begin
  if FActive then
    if Assigned(FFeeds[FeedType]) then
    begin
      FFeeds[FeedType].PostCommand(UTF8String(Format('{"cmd":"unsubscribe", "args":{"t":"price", "i":"%s", "m":%d}}', [i, m])));
      if FFeeds[FeedType].Tag > 0 then
        FFeeds[FeedType].Tag := FFeeds[FeedType].Tag - 1;
    end
    else
      raise ENordNetException.Create('Not opened')
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

procedure TNordNet.UnsubscribeFeedTrade(const FeedType: TFeedType; const i: string; const m: Integer);
begin
  if FActive then
    if Assigned(FFeeds[FeedType]) then
    begin
      FFeeds[FeedType].PostCommand(UTF8String(Format('{"cmd":"unsubscribe", "args":{"t":"trade", "i":"%s", "m":%d}}', [i, m])));
      if FFeeds[FeedType].Tag > 0 then
        FFeeds[FeedType].Tag := FFeeds[FeedType].Tag - 1;
    end
    else
      raise ENordNetException.Create('Not opened')
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

function TNordNet.UpdateOrder(const accno, order_id, volume: Integer; const price: Double; const currency: string): TOrderReply;
var
  FS: TFormatSettings;
begin
  DoNextReconnectAttampt;
  if FActive then
  begin
    EnterCriticalSection(FCriticalSection);
    try
      FS := TFormatSettings.Create;
      FS.DecimalSeparator := '.';
      FS.ThousandSeparator := #0;

      FRESTClient.BaseURL := URL('accounts/{accno}/orders/{order_id}');
      FRESTClient.Params.ParameterByName('accno').Value := IntToStr(accno);
      FRESTClient.Params.ParameterByName('order_id').Value := IntToStr(order_id);
      FRESTClient.ContentType := 'application/json';
      FRESTRequest.Method := rmPUT;
      FRESTRequest.Body.ClearBody;
      AddRequestParams(True);

      if volume > 0 then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'volume';
            Value := IntToStr(volume);
            ContentType := ctNone;
          end;
      if price > 0 then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'price';
            Value := FormatFloat(C_CURRENCY_FORMAT, price, FS);
            ContentType := ctNone;
          end;
      if currency <> '' then
        with FRESTRequest.Params.AddItem do
          begin
            Kind := pkGETorPOST;
            Name := 'currency';
            Value := currency;
            ContentType := ctNone;
          end;

      FRESTRequest.Execute;
      DoResponse;
      if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
        begin
          if not ParseOrderReplyResponse(Result) then
            raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content]);
        end
      else
        if FRESTResponse.StatusCode = 403 then
          raise ENordNetException.Create('User is logged in but user or system does not have priviliges to use this endpoint')
        else
          if (FRESTResponse.StatusCode <= 400) or (FRESTResponse.StatusCode <= 401) then
            if ParseErrorResponse then
              DoRequestError
            else
              raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content])
          else
            raise ENordNetException.CreateFmt('API not ready, HTTP status: %d %s', [FRESTResponse.StatusCode, FRESTResponse.StatusText]);
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
  end
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

function TNordNet.URL(const Path: string): string;
begin
  Result := FBaseURL;
  if (Result = '') or (Result[Length(Result)] = '/') then
    Result := Result + Path
  else
    Result := Result + '/' + Path;
end;

procedure TNordNet.Touch;
begin
  if FActive then
  begin
    FRESTClient.BaseURL := URL('login');
    FRESTRequest.Method := rmPUT;
    FRESTRequest.Body.ClearBody;
    AddRequestParams(True);
    try
      FRESTRequest.Execute;
    except
      on E: Exception do
        raise ENordNetException.CreateFmt('Error during execution REST request: %s', [E.Message]);
    end;
    DoResponse;
    if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
    begin
      if not ParseTouchResponse then
        raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content]);
    end
    else if (FRESTResponse.StatusCode <= 400) or (FRESTResponse.StatusCode <= 401) then
    begin
      FActive := False;
      if ParseErrorResponse then
        DoLoginError
      else
        raise ENordNetException.CreateFmt('Unexpected answer:'#13#13'%s', [FRESTResponse.Content])
    end
    else
      raise ENordNetException.CreateFmt('API not ready, HTTP status: %d %s',
        [FRESTResponse.StatusCode, FRESTResponse.StatusText]);
  end
  else
    raise ENordNetException.Create('Not allowed in inactive state');
end;

procedure TNordNet.CheckConnect;
begin
  if (not FUserName.IsEmpty) and (not FPassword.IsEmpty) then
  begin
    try
      Touch;
    except
      on E: Exception do
      begin
        FActive := False;
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'CheckConnect', E.Message);
      end;
    end;

    if not FActive then
      Login(FUserName, FPassword, FPublicKeyFile);
  end;
end;

procedure TNordNet.ChargeReconnectTimer(const Timeout: Integer);
begin
  if Assigned(FReconnectInfo.Timer) then
    FReconnectInfo.Timer.Free;

  FReconnectInfo.Timer := TTimer.Create(nil);
  FReconnectInfo.Timer.OnTimer  := OnReconnectTimer;
  FReconnectInfo.Timer.Interval := Timeout;
  FReconnectInfo.Timer.Enabled  := True;
end;

procedure TNordNet.CleanupReconnectTimer;
begin
  if Assigned(FReconnectInfo.Timer) then
    FreeAndNil(FReconnectInfo.Timer);
end;

procedure TNordNet.ClearReconnectInfo;
begin
  if Assigned(FReconnectInfo.Timer) then
    FReconnectInfo.Timer.Enabled := False;
  FReconnectInfo.Attempt := 0;
end;

procedure TNordNet.OnReconnectTimer(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;
  try
    try
      CheckConnect;
      if not FActive then
        DoNextReconnectAttampt;
    except
      DoNextReconnectAttampt;
    end;
  finally
    (Sender as TTimer).Enabled := True;
  end;
end;

procedure TNordNet.DoNextReconnectAttampt;
begin
  Inc(FReconnectInfo.Attempt);
  case FReconnectInfo.Attempt of
    1:
      try
        CheckConnect;
        if not FActive then
          DoNextReconnectAttampt;
      except
        DoNextReconnectAttampt;
      end;
    2:
      ChargeReconnectTimer(5000);
    3:
      ChargeReconnectTimer(30000);
  else
    FReconnectInfo.Attempt := 0;
  end;
end;

end.
