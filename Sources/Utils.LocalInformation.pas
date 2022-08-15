{*******************************************************************************}
{                                                                               }
{             unit Utils.LocalInformation                                       }
{                  v.3.0.0.20                                                   }
{               created 22/06/2009                                              }
{                                                                               }
{   Procedures and functions that return system Information                     }
{                                                                               }
{                                                                               }
{*******************************************************************************}

unit Utils.LocalInformation;

interface

uses
  //Standart units
  Winapi.Windows, System.WideStrUtils, System.SysUtils, Winapi.SHFolder, System.Win.ComObj,
  Winapi.ActiveX, Winapi.WinSock, System.Win.Registry;

type
  TLocalInformationDialog = class(TObject)
    {
    Description:
      Returns version of Internet Explorer
     }
    class function GetIEVersion: string;
    {
    Description:
      Returns the full path to directories by ID
      \All Users\Application Data
      \Program Files\
      \Program Files\Common\
      \My Pictures\
      ...
    Parameters:
      aFolder: the directory-defining constant described in SHFolder.pas with the prefix CSIDL_
      }
    class function GetSpecialFolderPath(aFolder: integer): string;
    {
    Description:
      Returns system InformationDialog
    Parameters:
      aFlag: constant described in Windows.pas with the prefix LOCALE_
      }
    class function GetLocaleInformationDialog(aFlag: Integer): string;
    {
    Description:
      Returns system code page
     }
    class function GetSystemCodePage: integer;
    {
    Description:
      Returns serial number of logical drive
    Parameters:
      aLetterDrive: drive letter, for example C:, D:
     }
    class function GetDriveSerialNumber(aLetterDrive: PChar): string;
    {
    Description:
      Returns path to System32 system directory
     }
    class function GetDirSystem32: string;
    {
    Description:
      Returns path to Windows directory
     }
    class function GetDirWindows: string;
    {
    Description:
      Returns path to temporary directory
     }
    class function GetDirTemp: string;
    {
    Description:
      Returns full path to temporary file
     }
    class function GetFileTemp(aExtension: string = ''; aPrefix: string = ''): string;
    {
    Description:
      Returns InformationDialog about whether an OLE object is registered, such as 'Excel.Application'
     }
    class function IsOLEObjectInstalled(aName: string): boolean;
    {
    Description:
      Returns Windows version
     }
    class function GetWindowsVersion: string;
    {
    Description:
      Returns current Windows user
     }
    class function GetUserFromWindows: string;
    {
    Description:
      Returns IP address and domain
     }
    class procedure GetLocalIPAddressName(var aIP, aDomain: string);
  end;

implementation

class function TLocalInformationDialog.GetLocaleInformationDialog(aFlag: Integer): string;
var
  pcLCA: array[0..20] of Char;
begin
  if GetLocaleInfo(LOCALE_USER_DEFAULT, aFlag, pcLCA, 19) <= 0 then
    pcLCA[0] := #0;
  Result := pcLCA;
end;

class procedure TLocalInformationDialog.GetLocalIPAddressName(var aIP, aDomain: string);
var
  WSAData : TWSAData;
  p       : PHostEnt;
  sName   : array [0..$FF] of AnsiChar;
begin
  WSAStartup($0101, WSAData);
  GetHostName(@sName, $FF);
  p := GetHostByName(@sName);

  aIP     := string(inet_ntoa(PInAddr(p.h_addr_list^)^));
  aDomain := string(sName);

  WSACleanup;
end;

class function TLocalInformationDialog.GetSpecialFolderPath(aFolder: Integer): string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  path: array[0..MAX_PATH] of Char;
begin
  if SUCCEEDED(SHGetFolderPath(0, aFolder, 0, SHGFP_TYPE_CURRENT, @path[0])) then
    Result := path
  else
    Result := '';
  if (Length(Result) > 0) and (Result[Length(Result)] <> '\') then
    Result := Result + '\';
end;

class function TLocalInformationDialog.GetSystemCodePage: Integer;
begin
  result := StrToInt(GetLocaleInformationDialog(LOCALE_IDEFAULTANSICODEPAGE));
end;

class function TLocalInformationDialog.GetDirWindows: string;
var
  A: array[0..144] of Char;
begin
  GetWindowsDirectory(A, sizeof(A));
  result := StrPas(A) + '\';
end;

class function TLocalInformationDialog.GetDirSystem32: string;
var
  A: array[0..144] of Char;
begin
  GetSystemDirectory(A, sizeof(A));
  result := StrPas(A) + '\';
end;

class function TLocalInformationDialog.GetDirTemp: string;
var
  sTemp: PAnsiChar;
begin
  Result := GetEnvironmentVariable('TEMP');
  if (Result = '') then
    Result := GetEnvironmentVariable('TMP');
  if (Result = '') then
  begin
    GetMem(sTemp, 255);
    GetTempPathA(255, sTemp);
    try
      if (sTemp <> '') then
        Result := string(sTemp);
    finally
      FreeMem(sTemp);
    end;
  end;
  if (Length(Result) > 0) then
    Result := IncludeTrailingPathDelimiter(Result);
end;

class function TLocalInformationDialog.GetFileTemp(aExtension: string = ''; aPrefix: string = ''): string;
begin
  Result := Concat(GetDirTemp, aPrefix, IntToHex(StrToInt(FormatDateTime('hhmmsszzz', Time)), 8), '.tmp');
  if (aExtension <> '') then
  begin
    if (aExtension[1] <> '.') then
      aExtension := Concat('.', aExtension);
    Result := ChangeFileExt(Result, aExtension);
  end;
end;

class function TLocalInformationDialog.GetDriveSerialNumber(aLetterDrive: PChar): string;
var
  DW           : DWord;
  FileSystem   : array[0..$FF] of Char;
  SerialNumber : DWord;
  SysFlags     : DWord;
  VolumeLabel  : array[0..$FF] of Char;
begin
  GetVolumeInformation(aLetterDrive,
                       VolumeLabel,
                       SizeOf(VolumeLabel),
                       @SerialNumber,
                       DW,
                       SysFlags,
                       FileSystem,
                       SizeOf(FileSystem));
  Result := IntToStr(SerialNumber);
end;

class function TLocalInformationDialog.IsOLEObjectInstalled(aName: string): boolean;
var
  ClassID : TCLSID;
  Rez     : HRESULT;
begin
  Rez := CLSIDFromProgID(PWideChar(WideString(aName)), ClassID);
  if Rez = S_OK then
    Result := True
  else
    Result := False;
end;

class function TLocalInformationDialog.GetWindowsVersion: string;
begin
  Result := TOSVersion.ToString;
end;

class function TLocalInformationDialog.GetUserFromWindows: string;
var
  sUserName    : string;
  sUserNameLen : Dword;
begin
  sUserNameLen := 255;
  SetLength(sUserName, sUserNameLen);
  if GetUserName(PChar(sUserName), sUserNameLen) then
    Result := Copy(sUserName,1,sUserNameLen - 1)
  else
    Result := 'Unknown';
end;

class function TLocalInformationDialog.GetIEVersion: string;
var
  loReg: TRegistry;
begin
  loReg := TRegistry.Create;
  try
    loReg.RootKey := HKEY_LOCAL_MACHINE;
    loReg.OpenKey('Software\Microsoft\Internet Explorer', False);
    try
      Result := loReg.ReadString('Version');
    except
      Result := '';
    end;
    loReg.CloseKey;
  finally
    FreeAndNil(loReg);
  end;
end;

end.

