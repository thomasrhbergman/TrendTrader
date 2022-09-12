unit DaModule.Constants;

interface

{$REGION 'Region uses'}
uses
  System.IOUtils, Vcl.Forms, System.SysUtils, System.Classes, Global.Types, System.SyncObjs,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IBX.IBDatabase, DebugWriter, FireDAC.Comp.Client;
{$ENDREGION}

type
  TDBConnectParams = record
    ConnectionString: string;
    AdditionalParameters: string;
  end;

  TIBOnExecuteError = record
  public
    FError     : string;
    FLineIndex : Integer;
    FSQLError  : string;
    FSQLText   : string;
    procedure DoOnExecuteError(Sender: TObject; Error: string; SQLText: string; LineIndex: Integer; var Ignore: Boolean);
  end;

  TInterbaseConnect = record
  const
    C_KEY_PROTOCOL         = 'Protocol';
    C_KEY_SERVER_NAME      = 'ServerName';
    C_KEY_PORT             = 'Port';
    C_KEY_USER_NAME        = 'UserName';
    C_KEY_PASSWORD         = 'Password';
    C_KEY_STOCKXROBOT      = 'ConnectionString.STOCKXROBOT';
    C_KEY_DBFEED           = 'ConnectionString.DB_FEED';
    C_KEY_PATH_STOCKXROBOT = 'Path.STOCKXROBOT';
    C_KEY_PATH_DBFEED      = 'Path.DB_FEED';
    C_KEY_ADD_PARAMETERS   = 'AdditionalParameters';
    C_SECTION_NAME         = 'DBConnect';
  const
    Port        = '3050';
    Host        = 'localhost';
    DBNameStock = 'STOCKXROBOT.IB';
    DBNameFeed  = 'DB_FEED.IB';
    IBUser      = 'SYSDBA';
    IBPassword  = 'masterkey';
  private
    class var CriticalSection : TCriticalSection;
    class function GetConnectParams(const aDBName: string): TDBConnectParams; static;
  public
    class function CreateDatabase(const aDBName: string): TIBDatabase; static;
    class procedure SetConnectParams(var aDatabase: TIBDatabase; aDBName: string); static;
  end;

  TFireBirdConnect = record
  const
    C_KEY_PROTOCOL         = 'Protocol';
    C_KEY_SERVER_NAME      = 'ServerName';
    C_KEY_PORT             = 'Port';
    C_KEY_USER_NAME        = 'UserName';
    C_KEY_PASSWORD         = 'Password';
    C_KEY_STOCKXROBOT      = 'ConnectionString.STOCKXROBOT';
    C_KEY_DBFEED           = 'ConnectionString.DB_FEED';
    C_KEY_PATH_STOCKXROBOT = 'Path.STOCKXROBOT';
    C_KEY_PATH_DBFEED      = 'Path.DB_FEED';
    C_KEY_ADD_PARAMETERS   = 'AdditionalParameters';
    C_SECTION_NAME         = 'FireBirdDBConnect';
    C_KEY_DRIVER_ID        = 'DriverId';
  const
    Port        = '3051';
    Host        = 'localhost';
    DBNameStock = 'STOCKXROBOT.FDB';
    DBNameFeed  = 'DB_FEED.FDB';
    User        = 'SYSDBA';
    Password    = 'masterkey';
    Protocol    = 'local';
    DriverID    = 'FB';
  private
    class var CriticalSection : TCriticalSection;
    class function GetConnectParams(const aDBName: string): TDBConnectParams; static;
  public
    class function CreateDatabase(const aDBName: string): TFDConnection; static;
    class procedure SetConnectParams(var aConnection: TFDConnection; aDBName: string); static;
  end;

implementation

{ TInterbaseConnect }

class function TInterbaseConnect.GetConnectParams(const aDBName: string): TDBConnectParams;
var
  Path: string;
begin
  Path := TPath.GetDirectoryName(Application.ExeName);
  Result := Default(TDBConnectParams);
  Result.ConnectionString     := General.XMLFile.ReadString(C_SECTION_NAME, 'ConnectionString.' + TPath.GetFileNameWithoutExtension(aDBName).ToUpper, TPath.Combine(Path, aDBName));
  Result.AdditionalParameters := 'user_name=' + General.XMLFile.ReadString(C_SECTION_NAME, C_KEY_USER_NAME, IBUser) + sLineBreak +
                                 'password=' + General.XMLFile.ReadString(C_SECTION_NAME, C_KEY_PASSWORD, IBPassword) + sLineBreak +
                                 General.XMLFile.ReadString(C_SECTION_NAME, C_KEY_ADD_PARAMETERS, '');
end;

class procedure TInterbaseConnect.SetConnectParams(var aDatabase: TIBDatabase; aDBName: string);
var
  ConnectParams: TDBConnectParams;
  Params: TStringList;
begin
  CriticalSection.Enter;
  try
    ConnectParams := GetConnectParams(aDBName);
    aDatabase.DatabaseName           := ConnectParams.ConnectionString;
    aDatabase.AllowStreamedConnected := True;
    aDatabase.LoginPrompt            := False;
    aDatabase.ServerType             := 'IBServer';
    Params := TStringList.Create;
    try
      Params.Text := ConnectParams.AdditionalParameters;
      aDatabase.Params.Clear;
      aDatabase.Params.AddStrings(Params);
    finally
      FreeAndNil(Params);
    end;
  finally
    CriticalSection.Leave;
  end;
end;

class function TInterbaseConnect.CreateDatabase(const aDBName: string): TIBDatabase;
begin
  Result := TIBDatabase.Create(nil);
  SetConnectParams(Result, aDBName);
end;

{ TIBOnExecuteError }
procedure TIBOnExecuteError.DoOnExecuteError(Sender: TObject; Error, SQLText: string; LineIndex: Integer; var Ignore: Boolean);
begin
  Ignore     := True;
  FError     := Error;
  FSQLText   := SQLText;
  FLineIndex := LineIndex;
  if (FSQLError <> '') then
    FSQLError := FSQLError + sLineBreak;
  FSQLError := 'Line[' + IntToStr(LineIndex) + '] : ' + Error;
end;

{ TFireBirdConnect }

class function TFireBirdConnect.CreateDatabase(
  const aDBName: string): TFDConnection;
begin
  Result := TFDConnection.Create(nil);
  SetConnectParams(Result, aDBName);
end;

class function TFireBirdConnect.GetConnectParams(
  const aDBName: string): TDBConnectParams;
var
  Path: string;
begin
  Path := TPath.GetDirectoryName(Application.ExeName);
  Result := Default(TDBConnectParams);
  //Result.ConnectionString     := General.XMLFile.ReadString(C_SECTION_NAME, 'ConnectionString.' + TPath.GetFileNameWithoutExtension(aDBName).ToUpper, TPath.Combine(Path, aDBName));
  Result.AdditionalParameters := 'DriverID=' + General.XMLFile.ReadString(C_SECTION_NAME, C_KEY_DRIVER_ID, DriverID) + sLineBreak +
                                 'Protocol='+ General.XMLFile.ReadString(C_SECTION_NAME, C_KEY_Protocol, Protocol) + sLineBreak +
                                 'Database='+ General.XMLFile.ReadString(C_SECTION_NAME, 'ConnectionString.' + TPath.GetFileNameWithoutExtension(aDBName).ToUpper, TPath.Combine(Path, aDBName)) + sLineBreak +
                                 'Port='+ General.XMLFile.ReadString(C_SECTION_NAME, C_KEY_PORT, Port) + sLineBreak +
                                 'User_Name=' + General.XMLFile.ReadString(C_SECTION_NAME, C_KEY_USER_NAME, User) + sLineBreak +
                                 'Password=' + General.XMLFile.ReadString(C_SECTION_NAME, C_KEY_PASSWORD, Password) + sLineBreak +
                                 General.XMLFile.ReadString(C_SECTION_NAME, C_KEY_ADD_PARAMETERS, '');

end;

class procedure TFireBirdConnect.SetConnectParams(var aConnection: TFDConnection;
  aDBName: string);
var
  ConnectParams: TDBConnectParams;
  //Params: TStringList;
begin
  CriticalSection.Enter;
  try
    ConnectParams := GetConnectParams(aDBName);
    aConnection.Params.Text := ConnectParams.AdditionalParameters;
    //aConnection.Params.Database        := ConnectParams.ConnectionString;
    aConnection.LoginPrompt            := False;
    {Params := TStringList.Create;
    try
      Params.Text := ConnectParams.AdditionalParameters;
      aConnection.Params.Clear;
      aConnection.Params.AddStrings(Params);
    finally
      FreeAndNil(Params);
    end; }
  finally
    CriticalSection.Leave;
  end;

end;

initialization
  if not Assigned(TFireBirdConnect.CriticalSection) then
    TFireBirdConnect.CriticalSection := TCriticalSection.Create;

finalization
  if Assigned(TFireBirdConnect.CriticalSection) then
    FreeAndNil(TFireBirdConnect.CriticalSection);

end.
