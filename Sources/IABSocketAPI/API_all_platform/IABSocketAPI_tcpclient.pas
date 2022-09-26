unit IABSocketAPI_tcpclient;

// use with IABSocketAPI v10.16 and later

interface

// August 2022:  shifted all TCP code to this separate file, added winsock2 and IPv6 support.

//  NOTE: This implementation of Winsock2 REQUIRES win 7 or better.

{$IF CompilerVersion >= 24.0}  // 24 = XE3
//  Override the use of winsock2 here;
{$DEFINE USE_WINSOCK2}
{$IFEND}

uses
{$IF CompilerVersion < 24.0}  // 24 = XE3
Windows, SysUtils, Classes, Winsock;
{$ELSE}
Winapi.Windows, System.SysUtils, System.Classes, {$IFDEF USE_WINSOCK2} Winapi.WinSock2 {$ELSE} WinSock {$ENDIF};
{$IFEND}


{  TIABTCPClient  }

{ *************************************************************************** }
{                                                                             }
{   This is a subset of the Sockets.pas file from D6 onwards.  It has been    }
{   stripped of all uneeded components except for the client socket parts.    }
{                                                                             }
{      Due to the ever changing landscape in XE versions and the socket       }
{      code versions, we have decided to include our own simplified version   }
{      that should span all 32/64 bit and the various char sizes over the     }
{      XE builds.                                                             }
{                                                                             }
{      This code is further modified for use with Winsock2 and IPv6           }
{                                                                             }
{ *************************************************************************** }
{                                                                             }
{ Delphi and Kylix Cross-Platform Visual Component Library                    }
{ Internet Application Runtime                                                }
{                                                                             }
{ Copyright (C) 2000, 2001 Borland Software Corporation                       }
{                                                                             }
{ Licensees holding a valid Borland No-Nonsense License for this Software may }
{ use this file in accordance with such license, which appears in the file    }
{ license.txt that came with this Software.                                   }
{                                                                             }
{ *************************************************************************** }


type
{ TIABTCPClient }

  TIABSocketDataEvent = procedure (Sender: TObject; Buf: pAnsiChar; var DataLen: Integer) of object;
  TIABSocketErrorEvent = procedure (Sender: TObject; SocketError: Integer) of object;
  EIABSocketError = class(Exception);
  TServerSocketBlockMode = (bmBlocking, bmNonBlocking, bmThreadBlocking);
  TSocketBlockMode = bmBlocking..bmNonBlocking;
  TSocketIPVersion = (svIPv4, svIPv6, svIPvBoth);

  TIABTCPClient = class(TComponent)
  private
    FActive: Boolean;
    FBlockMode: TSocketBlockMode;
    FBytesReceived: Cardinal;
    FBytesSent: Cardinal;
    FSocket: TSocket;
    FOnCreateHandle: TNotifyEvent;
    FOnDestroyHandle: TNotifyEvent;
    FOnError: TIABSocketErrorEvent;
    FOnReceive: TIABSocketDataEvent;
    FOnSend: TIABSocketDataEvent;
    FLocalHost: string;
    FLocalPort: string;
    FRemoteHost: string;
    FRemotePort: string;
    FConnected: Boolean;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    {$IFDEF USE_WINSOCK2}
    FIPVersion: TSocketIPVersion;
    procedure SetIPVersion(const Value: TSocketIPVersion);
    {$ENDIF}
    procedure SetActive(Value: Boolean);
    procedure SetLocalHost(Value: string);
    procedure SetLocalPort(Value: string);
    procedure SetRemoteHost(Value: string);
    procedure SetRemotePort(Value: string);
    procedure Open;
    procedure Close;
  protected
    procedure DoCreateHandle; dynamic;
    procedure DoDestroyHandle; dynamic;
    procedure DoHandleError; dynamic;
    procedure DoReceive(Buf: pAnsiChar; var DataLen: Integer); virtual;
    procedure DoSend(Buf: pAnsiChar; var DataLen: Integer); virtual;
    function ErrorCheck(rc: Integer): Integer; virtual;
    procedure Loaded; override;
    procedure SetBlockMode(Value: TSocketBlockMode);
    procedure SetBytesReceived(Value: Cardinal);
    procedure SetBytesSent(Value: Cardinal);
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PeekBuf(var Buf; BufSize: Integer): Integer;
    function ReceiveBuf(var Buf; BufSize: Integer; Flags: Integer = 0): Integer;
    function Select(ReadReady, WriteReady, ExceptFlag: PBoolean; TimeOut: Integer = 0): Boolean;
    function SendBuf(var Buf; BufSize: Integer; Flags: Integer = 0): Integer;
    function SendStream(AStream: TStream): Integer;
    function WaitForData(TimeOut: Integer = 0): Boolean;
    property Active: Boolean read FActive write SetActive default False;
    property BlockMode: TSocketBlockMode read FBlockMode write SetBlockMode default bmBlocking;
    property BytesReceived: Cardinal read FBytesReceived;
    property BytesSent: Cardinal read FBytesSent;
    property Handle: TSocket read FSocket;
    property OnCreateHandle: TNotifyEvent read FOnCreateHandle write FOnCreateHandle;
    property OnDestroyHandle: TNotifyEvent read FOnDestroyHandle write FOnDestroyHandle;
    property OnError: TIABSocketErrorEvent read FOnError write FOnError;
    property OnReceive: TIABSocketDataEvent read FOnReceive write FOnReceive;
    property OnSend: TIABSocketDataEvent read FOnSend write FOnSend;
    {$IFDEF USE_WINSOCK2}
    procedure GetSocketAddr(h: string; p: string; var sockaddr: TSockAddrStorage);
    property IPVersion: TSocketIPVersion read FIPVersion write SetIPVersion;
    {$ELSE}
    function GetSocketAddr(h: string; p: string): TSockAddr;
    {$ENDIF}
    function ReceiveFrom(var buf; bufsize: Integer; ToAddr: TSockAddr; var len: Integer; flags: Integer = 0): Integer;
    function SendTo(var buf; bufsize: Integer; ToAddr: TSockAddr; flags: Integer = 0): Integer;
    function LookupHostName(const ipaddr: string): string;
    function LookupHostAddr(const hn: string): string;
    function LookupPort(const sn: string; pn: pAnsiChar = nil): word;
    property LocalHost: string read FLocalHost write SetLocalHost;
    property LocalPort: string read FLocalPort write SetLocalPort;
    property RemoteHost: string read FRemoteHost write SetRemoteHost;
    property RemotePort: string read FRemotePort write SetRemotePort;
    function Connect: Boolean;
    procedure Disconnect;
    property Connected: Boolean read FConnected;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisConnect;
  end;


implementation

{$IFDEF USE_WINSOCK2}

// These types defined localized, as their existence in XE is unreliable.
type
  PAddrinfoA = ^TAddrinfoA;
  TAddrinfoA = record
    ai_flags: Integer;       // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family: Integer;      // PF_xxx
    ai_socktype: Integer;    // SOCK_xxx
    ai_protocol: Integer;    // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen: u_long;//size_t;     // Length of ai_addr
    ai_canonname: PAnsiChar;   // Canonical name for nodename
    ai_addr: PSockAddr;        // Binary address
    ai_next: PAddrinfoA;        // Next structure in linked list
  end;

  PAddrinfoW = ^TAddrinfoW;
  TAddrinfoW = record
    ai_flags: Integer;       // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family: Integer;      // PF_xxx
    ai_socktype: Integer;    // SOCK_xxx
    ai_protocol: Integer;    // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen: u_long;//size_t;     // Length of ai_addr
    ai_canonname: PChar;   // Canonical name for nodename
    ai_addr: PSockAddr;        // Binary address
    ai_next: PAddrinfoW;        // Next structure in linked list
  end;
  TAddrinfo = TAddrinfoW;
  PAddrinfo = ^TAddrinfoW;

  PIn6Addr = ^TIn6Addr;
  TIn6Addr = packed record
    case Size: Byte of
      0 : (Bytes: array [0..15] of u_char);
      1 : (Words: array [0..7] of u_short);
  end;

  PSockAddrIn6 = ^TSockAddrIn6;
  TSockAddrIn6 = record
    sin6_family: u_short;     // AF_INET6.
    sin6_port: u_short;       // Transport level port number.
    sin6_flowinfo: u_long;    // IPv6 flow information.
    sin6_addr: TIn6Addr;       // IPv6 address.
    sin6_scope_id: u_long;     // Set of interfaces for a scope.
  end;

  socklen_t = Integer;

  const
    ws2_32 = 'ws2_32.dll';

    // Flags for getnameinfo()
    NI_NOFQDN      = $01;  {* Only return nodename portion for local hosts *}
    NI_NUMERICHOST = $02;  {* Return numeric form of the host's address *}
    NI_NAMEREQD    = $04;  {* Error if the host's name not in DNS *}
    NI_NUMERICSERV = $08;  {* Return numeric form of the service (port #) *}
    NI_DGRAM       = $10;  {* Service is a datagram service *}
    NI_MAXHOST     = 1025;  {* Max size of a fully-qualified domain name *}
    NI_MAXSERV     = 32;    {* Max size of a service name *}


  // Extra / updated functions from the winsock2 dll, only available on win 7 and later.
  function _connect(s: TSocket; name: PSockAddrStorage; namelen: Integer): Integer; stdcall; external ws2_32 name 'connect';
  function inet_ntopA(Family: Integer; pAddr: Pointer; StringBuf: LPCSTR; StringBufSize: u_long): LPCSTR; stdcall; external ws2_32 name 'inet_ntop';
  function InetNtopW(Family: Integer; pAddr: Pointer; StringBuf: LPCWSTR; StringBufSize: u_long): LPCWSTR; stdcall; external ws2_32 name 'InetNtopW';
  function inet_ntop(Family: Integer; pAddr: Pointer; StringBuf: LPCWSTR; StringBufSize: u_long): LPCWSTR; stdcall; external ws2_32 name 'InetNtopW';
  function inet_ptonA(Family: Integer; pAddrString: LPCSTR; AddrBuf: Pointer): Integer; stdcall; external ws2_32 name 'inet_pton';
  function InetPtonW(Family: Integer; pAddrString: LPCWSTR; AddrBuf: Pointer): Integer; stdcall; external ws2_32 name 'InetPtonW';
  function inet_pton(Family: Integer; pAddrString: LPCWSTR; AddrBuf: Pointer): Integer; stdcall; external ws2_32 name 'InetPtonW';
  function getaddrinfoA( NodeName: LPCSTR; ServiceName: LPCSTR; Hints: PAddrinfoA; var Results: PAddrinfoA): Integer; stdcall; external ws2_32 name 'getaddrinfo';
  function getaddrinfoW( NodeName: LPCWSTR; ServiceName: LPCWSTR; Hints: PAddrinfoW; var Results: PAddrinfoW): Integer; stdcall; external ws2_32 name 'GetAddrInfoW';
  function getaddrinfo( NodeName: LPCWSTR; ServiceName: LPCWSTR; Hints: PAddrinfo; var Results: PAddrinfo): Integer; stdcall; external ws2_32 name 'GetAddrInfoW';
  procedure freeaddrinfoA( Results: PAddrinfoA); stdcall; external ws2_32 name 'freeaddrinfo';
  procedure FreeAddrInfoW( Results: PAddrinfoW); stdcall; external ws2_32 name 'FreeAddrInfoW';
  procedure freeaddrinfo( Results: PAddrinfo); stdcall; external ws2_32 name 'FreeAddrInfoW';
  function getnameinfoA(SockAddr: Pointer; SockaddrLength: socklen_t; pNodeBuffer: LPCSTR; NodeBufferSize: DWORD; pServiceBuffer: LPCSTR;
                      ServiceBufferSize: DWORD; Flags: Integer): Integer; stdcall; external ws2_32 name 'getnameinfo';
  function GetNameInfoW(SockAddr: Pointer; SockaddrLength: socklen_t; pNodeBuffer: LPCWSTR; NodeBufferSize: DWORD; pServiceBuffer: LPCWSTR;
                      ServiceBufferSize: DWORD; Flags: Integer): Integer; stdcall; external ws2_32 name 'GetNameInfoW';
  function getnameinfo(SockAddr: Pointer; SockaddrLength: socklen_t; pNodeBuffer: LPCWSTR; NodeBufferSize: DWORD; pServiceBuffer: LPCWSTR;
                      ServiceBufferSize: DWORD; Flags: Integer): Integer; stdcall; external ws2_32 name 'GetNameInfoW';


  function GetAddressInfo(Inet: Integer; IPorHost: string; PortNameOrNum: string): PAddrinfo;
  var addrinfo_out: PAddrinfo; addrinfo_hints: TAddrinfo;
  begin
    Result := nil;
    FillChar(addrinfo_hints, SizeOf(addrinfo_hints), 0);
    addrinfo_hints.ai_family := Inet;
    addrinfo_hints.ai_socktype := SOCK_STREAM;
    addrinfo_hints.ai_protocol := IPPROTO_TCP;
    if getaddrinfo(pChar(IPorHost), pChar(PortNameOrNum), @addrinfo_hints, addrinfo_out) = 0 then
      Result := addrinfo_out;
  end;

{$ENDIF}


constructor TIABTCPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSocket := INVALID_SOCKET;
  RPR;
end;

destructor TIABTCPClient.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TIABTCPClient.Open;
var afinet: Integer;
  {$IFDEF USE_WINSOCK2} addr_store: TSockAddrStorage; {$ELSE} addr: TSockAddr; {$ENDIF}
begin

  afinet := AF_INET;
  {$IFDEF USE_WINSOCK2}
  if FIPVersion = svIPv6 then
    afinet := AF_INET6;
  {$ENDIF}

  if not FActive then
    begin
      FSocket := ErrorCheck(socket(afinet, SOCK_STREAM, IPPROTO_IP));
      FActive := FSocket <> INVALID_SOCKET;
      if FActive then
        begin
          FBytesReceived := 0;
          FBytesSent := 0;
          DoCreateHandle;
        end;
    end;

  if FActive and not FConnected then
    begin
      {$IFDEF USE_WINSOCK2}
      FillChar(addr_store, SizeOf(TSockAddrStorage), 0);
      addr_store.ss_family := afinet;
      GetSocketAddr(FRemoteHost, FRemotePort, addr_store);
      FConnected := ErrorCheck(_connect(FSocket, @addr_store, sizeof(addr_store))) = 0;
      {$ELSE}
      addr := GetSocketAddr(FRemoteHost, FRemotePort);
      FConnected := ErrorCheck(WinSock.connect(FSocket, addr, sizeof(addr))) = 0;
      {$ENDIF}
      if FConnected then
        DoConnect;
    end;
end;

procedure TIABTCPClient.Close;
begin
  if FConnected then
    begin
      ErrorCheck(shutdown(FSocket, SD_BOTH));
      FConnected := False;
      DoDisconnect;
    end;

  if FActive then
    begin
      ErrorCheck(closesocket(FSocket));
      FSocket := INVALID_SOCKET;
      FActive := False;
      DoDestroyHandle;
    end;
end;

function TIABTCPClient.PeekBuf(var Buf; BufSize: Integer): Integer;
begin
  Result := ErrorCheck(recv(FSocket, buf, bufsize, MSG_PEEK));
end;

function TIABTCPClient.ReceiveBuf(var Buf; BufSize: Integer; Flags: Integer): Integer;
begin
  Result := ErrorCheck(recv(FSocket, Buf, BufSize, Flags));
  if Result <> SOCKET_ERROR then
    DoReceive(pAnsiChar(@Buf), Result);
end;


function TIABTCPClient.Select(ReadReady, WriteReady, ExceptFlag: PBoolean; TimeOut: Integer): Boolean;
type TAllFdSets = record
    ReadFds, WriteFds, ExceptFds: TFDset;
    pReadFd, pWriteFds, pExceptFds: PFDset;
    ptv: PTimeVal;
  end;
var
  AllFDsets: TAllFdSets;
  tv: TTimeVal;
begin
  Result := False;
  if not FActive then
    Exit;

  FillChar(AllFDsets, Sizeof(TAllFdSets), 0);  // does the same job of all the sloppy FD_ZERO() and x := nil code;
  if Assigned(ReadReady) then
    begin
      AllFDsets.pReadFd := @AllFDsets.ReadFds;
      {$IFDEF USE_WINSOCK2} _FD_SET(FSocket, AllFDsets.ReadFds); {$ELSE} FD_SET(FSocket, AllFDsets.ReadFds); {$ENDIF}
    end;
  if Assigned(WriteReady) then
    begin
      AllFDsets.pWriteFds := @AllFDsets.WriteFds;
      {$IFDEF USE_WINSOCK2} _FD_SET(FSocket, AllFDsets.WriteFds); {$ELSE} FD_SET(FSocket, AllFDsets.WriteFds); {$ENDIF}
    end;
  if Assigned(ExceptFlag) then
    begin
      AllFDsets.pExceptFds := @AllFDsets.ExceptFds;
      {$IFDEF USE_WINSOCK2} _FD_SET(FSocket, AllFDsets.ExceptFds); {$ELSE} FD_SET(FSocket, AllFDsets.ExceptFds); {$ENDIF}
    end;
  if TimeOut >= 0 then
    begin
      tv.tv_sec := TimeOut div 1000;
      tv.tv_usec :=  1000 * (TimeOut mod 1000);
      AllFDsets.ptv := @tv;
    end;
  try
    {$IFDEF USE_WINSOCK2}
    Result := ErrorCheck(WinAPI.WinSock2.select(FSocket + 1, AllFDsets.pReadFd, AllFDsets.pWriteFds, AllFDsets.pExceptFds, AllFDsets.ptv)) > 0;
    {$ELSE}
    Result := ErrorCheck(WinSock.select(FSocket + 1, AllFDsets.pReadFd, AllFDsets.pWriteFds, AllFDsets.pExceptFds, AllFDsets.ptv)) > 0;
    {$ENDIF}
  except
    Result := False;
  end;
  if Assigned(ReadReady) then
    ReadReady^ := FD_ISSET(FSocket, AllFDsets.ReadFds);
  if Assigned(WriteReady) then
    WriteReady^ := FD_ISSET(FSocket, AllFDsets.WriteFds);
  if Assigned(ExceptFlag) then
    ExceptFlag^ := FD_ISSET(FSocket, AllFDsets.ExceptFds);
end;


function TIABTCPClient.SendBuf(var Buf; BufSize: Integer; Flags: Integer): Integer;
begin
  DoSend(pAnsiChar(@Buf), BufSize);
  Result := ErrorCheck(send(FSocket, Buf, BufSize, Flags));
  if Result <> SOCKET_ERROR then
    inc(FBytesSent, Result);
end;

function TIABTCPClient.SendStream(AStream: TStream): Integer;
var
  BufLen: Integer;
  Buffer: array[0..1459] of Byte;
begin
  Result := 0;
  if Assigned(AStream) then
    begin
      repeat
        BufLen := AStream.Read(Buffer, SizeOf(Buffer));
      until (BufLen = 0) or (SendBuf(Buffer, BufLen) = SOCKET_ERROR);
    end;
end;

function TIABTCPClient.WaitForData(TimeOut: Integer): Boolean;
var
  ReadReady, ExceptFlag: Boolean;
  c: Char;
begin
  Result := False;
  // Select also returns True when connection is broken.
  if Select(@ReadReady, nil, @ExceptFlag, TimeOut) then
    Result := ReadReady and not ExceptFlag and (PeekBuf(c, sizeof(c)) = 1);
end;

procedure TIABTCPClient.DoHandleError;
var
  SocketError: Integer;
begin
  SocketError := WSAGetLastError;
  if Assigned(FOnError) then
    OnError(Self, SocketError);
end;

procedure TIABTCPClient.DoCreateHandle;
begin
  if FActive and Assigned(FOnCreateHandle) then
    OnCreateHandle(self);
end;

procedure TIABTCPClient.DoDestroyHandle;
begin
  if Assigned(FOnDestroyHandle) then
    OnDestroyHandle(self);
end;

procedure TIABTCPClient.DoReceive(Buf: pAnsiChar; var DataLen: Integer);
begin
  if Assigned(FOnReceive) then
    OnReceive(Self, Buf, DataLen);
  inc(FBytesReceived, DataLen);
end;

procedure TIABTCPClient.DoSend(Buf: pAnsiChar; var DataLen: Integer);
begin
  if Assigned(FOnSend) then
    OnSend(Self, Buf, DataLen);
end;

function TIABTCPClient.ErrorCheck(rc: Integer): Integer;
begin
  Result := rc;
  if rc = SOCKET_ERROR then
    DoHandleError;
end;

procedure TIABTCPClient.Loaded;
begin
  inherited Loaded;
  if FActive and not (csDesigning in ComponentState) then
    begin
      FActive := False;
      Open;
    end;
end;

procedure TIABTCPClient.SetBlockMode(Value: TSocketBlockMode);
var Blocking: u_long;
begin
  if Value <> FBlockMode then
    begin
      Blocking := 0;
      if FBlockMode = bmNonBlocking then
        Blocking := 1;
      {$IFDEF USE_WINSOCK2}
      if (SOCKET_ERROR = ErrorCheck(ioctlsocket(FSocket, Longint(FIONBIO), Blocking))) and FActive then
      {$ELSE}
      if (SOCKET_ERROR = ErrorCheck(ioctlsocket(FSocket, FIONBIO, Blocking))) and FActive then
      {$ENDIF}
        Close;
      FBlockMode := Value;
    end;
end;

procedure TIABTCPClient.SetBytesReceived(Value: Cardinal);
begin
  FBytesReceived := Value;
end;

procedure TIABTCPClient.SetBytesSent(Value: Cardinal);
begin
  FBytesSent := Value
end;

procedure TIABTCPClient.SetActive(Value: Boolean);
begin
  if Value <> FActive then
    begin
      if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
        if Value then
          Open
        else
          Close
      else
        FActive := Value;
  end;
end;

{$IFDEF USE_WINSOCK2}

procedure TIABTCPClient.SetIPVersion(const Value: TSocketIPVersion);
begin
  if FActive then
    Exit;
  FIPVersion := Value;
end;

procedure TIABTCPClient.GetSocketAddr(h: string; p: string; var sockaddr: TSockAddrStorage);
var pSockaddr4: PSockAddrIn;
    pSockaddr6: PSockAddrIn6;
    AddressInfo: PAddrinfo;
    sockaddr_ipv4: PSockAddrIn;
    sockaddr_ipv6: PSockAddrIn6;
begin
  AddressInfo := GetAddressInfo(sockaddr.ss_family, h, p);
  if AddressInfo = nil then
    Exit;

  if sockaddr.ss_family = AF_INET then
    begin
      pSockaddr4 := @sockaddr;
      sockaddr_ipv4 := PSockAddrIn(AddressInfo.ai_addr);
      pSockaddr4.sin_addr.S_addr := sockaddr_ipv4.sin_addr.S_addr;
      pSockaddr4.sin_port := sockaddr_ipv4.sin_port;
    end;
  if sockaddr.ss_family = AF_INET6 then
    begin
      pSockaddr6 := @sockaddr;
      sockaddr_ipv6 := PSockAddrIn6(AddressInfo.ai_addr);
      Move(sockaddr_ipv6.sin6_addr, pSockaddr6.sin6_addr, SizeOf(TIn6Addr));
      pSockaddr6.sin6_port := sockaddr_ipv6.sin6_port;
    end;
  freeaddrinfo(AddressInfo);
end;

function TIABTCPClient.LookupHostAddr(const hn: string): string;
var
  pn: string;
  afinet: Integer;
  AddressInfo: PAddrinfo;
  sockaddr_ipv4: PSockAddrIn;
  sockaddr_ipv6: PSockAddrIn6;
  ipstringbuffer: array [0..45] of Char;
begin
  pn := '0';
  Result := '0.0.0.0';
  afinet := AF_INET;
  if FIPVersion = svIPv6 then
    begin
      Result := '0:0:0:0:0:0:0:0';
      afinet := AF_INET6;
    end
  else if FIPVersion = svIPvBoth then
    afinet := AF_UNSPEC;

  AddressInfo := GetAddressInfo(afinet, hn, pn);
  if AddressInfo = nil then
    Exit;

  case AddressInfo.ai_family of
    AF_UNSPEC: begin
        // nothing;
      end;
    AF_INET:begin
        sockaddr_ipv4 := PSockAddrIn(AddressInfo.ai_addr);
        if nil = inet_ntop(AF_INET, @sockaddr_ipv4.sin_addr, ipstringbuffer, 46) then
          Result := 'inet_ntop failed with ' + IntToStr(WSAGetLastError())
        else
          Result := ipstringbuffer;
      end;
    AF_INET6:begin
        sockaddr_ipv6 := PSockAddrIn6(AddressInfo.ai_addr);
        if nil = inet_ntop(AF_INET6, @sockaddr_ipv6.sin6_addr, ipstringbuffer, 46) then
          Result := 'inet_ntop failed with ' + IntToStr(WSAGetLastError())
        else
          Result := ipstringbuffer;
      end;
    end;
  freeaddrinfo(AddressInfo);
end;

function TIABTCPClient.LookupHostName(const ipaddr: string): string;
var
  SockAddrIn6: TSockAddrIn6; SockAddrIn: TSockAddrIn;
  HostName: array [0..NI_MAXHOST -1] of Char;
  ServiceBuf: array [0..NI_MAXSERV-1] of Char;
  b: Boolean;
begin
  Result := '';
  if Pos(':', ipaddr) > 0 then
    begin
      b := inet_pton(AF_INET6, pChar(ipaddr), @SockAddrIn6.sin6_addr) = 1;
      SockAddrIn6.sin6_family := AF_INET6;
      SockAddrIn6.sin6_port := 0;
      b := b and (getnameinfo(@SockAddrIn6, SizeOf(TSockAddrIn6), HostName, NI_MAXHOST, ServiceBuf, NI_MAXSERV, NI_NUMERICSERV) = 0);
    end
  else
    begin
      b := inet_pton(AF_INET, pChar(ipaddr), @SockAddrIn.sin_addr.S_addr) = 1;
      SockAddrIn.sin_family := AF_INET;
      SockAddrIn.sin_port := 0;
      b := b and (getnameinfo(@SockAddrIn, SizeOf(TSockAddrIn), HostName, NI_MAXHOST, ServiceBuf, NI_MAXSERV, NI_NUMERICSERV) = 0);
    end;
  if b then
    Result := HostName;
end;


{$ELSE}     // old winsock 1.1

function TIABTCPClient.GetSocketAddr(h: string; p: string): TSockAddr;
var Host: AnsiString;
begin
  Host := AnsiString(LookupHostAddr(h));
  Result.sin_family := AF_INET;
  Result.sin_addr.s_addr := inet_addr(pAnsiChar(Host));
  Result.sin_port := htons(LookupPort(p));
end;

function TIABTCPClient.LookupHostAddr(const hn: string): string;
var
  h: PHostEnt;
  HostName: AnsiString;
begin
  Result := '';
  if hn <> '' then
    begin
      HostName := AnsiString(hn);
      if HostName[1] in ['0'..'9'] then
        begin
          if inet_addr(pAnsiChar(HostName)) <> u_long(INADDR_NONE) then
            Result := hn;
        end
      else
        begin
          h := gethostbyname(pAnsiChar(HostName));
          if h <> nil then
            with h^ do
            Result := format('%d.%d.%d.%d', [ord(h_addr^[0]), ord(h_addr^[1]),
          		  ord(h_addr^[2]), ord(h_addr^[3])]);
        end;
    end
  else
    Result := '0.0.0.0';
end;


function TIABTCPClient.LookupHostName(const ipaddr: string): string;
var
  h: PHostEnt;
  addr: TSockAddr;
  ipAddress: AnsiString;
  ResultAnsi: AnsiString;
begin
  Result := '';
  ipAddress := AnsiString(ipaddr);
  addr.sin_addr.s_addr := inet_addr(pAnsiChar(ipAddress));
  if addr.sin_addr.s_addr <> u_long(INADDR_NONE) then
    begin
      h := gethostbyaddr(@addr.sin_addr.s_addr, sizeof(addr), AF_INET);
      if h <> nil then
        begin
          ResultAnsi := h^.h_name;
          Result := string(ResultAnsi);
        end;
    end;

end;

{$ENDIF}   // use_winsock2



function TIABTCPClient.LookupPort(const sn: string; pn: pAnsiChar): word;
var
  se: PServent;
  ServName: AnsiString;
begin
  Result := 0;
  if sn <> '' then
    begin
      ServName := AnsiString(sn);
      se := getservbyname(pAnsiChar(ServName), pn);
      if se <> nil then
        Result := ntohs(se^.s_port)
      else
        Result := StrToInt(sn);
    end;
end;

function TIABTCPClient.ReceiveFrom(var buf; bufsize: Integer; ToAddr: TSockAddr; var len: Integer; flags: Integer): Integer;
begin
  Result := ErrorCheck(recvfrom(FSocket, buf, bufsize, flags, ToAddr, len));
  if Result <> SOCKET_ERROR then
    DoReceive(pAnsiChar(@Buf), Result);
end;

function TIABTCPClient.SendTo(var buf; bufsize: Integer; ToAddr: TSockAddr; flags: Integer): Integer;
begin
  DoSend(pAnsiChar(@Buf), BufSize);
  {$IFDEF USE_WINSOCK2}
  Result := ErrorCheck(WinAPI.WinSock2.sendto(FSocket, buf, bufsize, flags, @ToAddr, sizeof(ToAddr)));
  {$ELSE}
  Result := ErrorCheck(WinSock.sendto(FSocket, buf, bufsize, flags, ToAddr, sizeof(ToAddr)));
  {$ENDIF}
  if Result <> SOCKET_ERROR then
    SetBytesSent(BytesSent + Cardinal(Result));
end;

procedure TIABTCPClient.SetLocalHost(Value : string);
begin
  if Value <> FLocalHost then
    begin
      if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
        Close;
      FLocalHost := Value;
    end;
end;

procedure TIABTCPClient.SetLocalPort(Value: string);
begin
  if Value <> FLocalPort then
    begin
      if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
        Close;
      FLocalPort := Value;
    end;
end;

procedure TIABTCPClient.SetRemoteHost(Value : string);
begin
  if Value <> FRemoteHost then
    begin
      if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
        Close;
      FRemoteHost := Value;
      {$IFDEF USE_WINSOCK2}
      if Pos(':', FRemoteHost) > 0 then
        FIPVersion := svIPv6
      else if (Length(FRemoteHost) > 0) and (Pos('.', FRemoteHost) > 0) and
          (AnsiChar(FRemoteHost[1]) in ['0'..'9']) and (AnsiChar(FRemoteHost[Length(FRemoteHost)]) in ['0'..'9']) then
        FIPVersion := svIPv4;
      {$ENDIF}
    end;
end;

procedure TIABTCPClient.SetRemotePort(Value: string);
begin
  if Value <> FRemotePort then
    begin
      if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
        Close;
      FRemotePort := Value;
    end;
end;

function TIABTCPClient.Connect: Boolean;
begin
  Open;
  Result := FConnected;
end;

procedure TIABTCPClient.Disconnect;
begin
  Close;
end;

procedure TIABTCPClient.DoConnect;
begin
  if Assigned(FOnConnect) then
    OnConnect(self);
end;

procedure TIABTCPClient.DoDisconnect;
begin
  if Assigned(FOnDisconnect) then
    OnDisconnect(self);
end;


//    *****   Winsock startup / cleanup
var
  WSAData: TWSAData;

procedure Startup;
var
  ErrorCode: Integer;
begin
  {$IFDEF USE_WINSOCK2}
  ErrorCode := WSAStartup(WINSOCK_VERSION, WSAData);
  {$ELSE}
  ErrorCode := WSAStartup($0101, WSAData);
  {$ENDIF}
  if ErrorCode <> 0 then
    raise EIABSocketError.Create('WSAStartup');
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise EIABSocketError.Create('WSACleanup');
end;

initialization
  Startup;

finalization
  Cleanup;

end.
