unit Utils.VerInfo;

interface

{$REGION 'Region uses'}
uses
  Windows, Forms, System.SysUtils, StrUtils, Utils.LocalInformation;
{$ENDREGION}

type
  TStringFileInfoType = (sfiCompanyName, sfiFileDescription, sfiFileVersion,
                         sfiInternalName, sfiLegalCopyright, sfiOriginalFilename,
                         sfiProductName, sfiProductVersion);

  TVersionInfo = class
  private
    FCharSet   : string;
    FFileName  : string;
    FFixInfo   : PVSFixedFileInfo;
    FInfoBuf   : Pointer;
    FLang      : Integer;
    FOsVerInfo : TOSVersionInfo;
    function GetStrFileInfo(Idx: TStringFileInfoType): string;
    function GetOSVersion: string;
    function GetFileBuild: string;
    function GetFileVersion: string;
    function GetVersion(aValue: DWORD): string;
  public
    function ProductVersion: string;
    function ModuleDate: TDateTime;
    function ModuleSize: integer;
    function ModuleVersion: string;
    constructor Create(AFileName: string = ''); overload;
    destructor Destroy; override;

    property FileBuild          : string                       read GetFileBuild;
    property FileVersion        : string                       read GetFileVersion;
    property FixedFileInfo      : PVSFixedFileInfo             read FFixInfo;
    property OsVerInfo          : TOSVersionInfo               read FOsVerInfo;
    property OSVersion          : string                       read GetOSVersion;
    property StringFileInfo[Idx : TStringFileInfoType]: string read GetStrFileInfo;
  end;

implementation

const
  TStringFileInfoNames: array[TStringFileInfoType] of string =
    ('CompanyName',
     'FileDescription',
     'FileVersion',
     'InternalName',
     'LegalCopyright',
     'OriginalFilename',
     'ProductName',
     'ProductVersion');

constructor TVersionInfo.Create(AFileName: string = '');
var
  ExeFileName: array[0..255] of Char;
  DWSize, Dummy: DWORD;
  Res: PChar;
begin
  inherited Create;
  if AFileName = '' then
    StrPCopy(ExeFileName, Application.ExeName)
  else
    StrPCopy(ExeFileName, aFileName);

  FFileName := ExeFileName;
  DWSize := GetFileVersionInfoSize(ExeFileName, Dummy);
  GetMem(FInfoBuf, DWSize);
  GetFileVersionInfo(ExeFileName, Dummy, DWSize, FInfoBuf);
  if Assigned(FInfoBuf) then
  begin
    VerQueryValue(FInfoBuf, '\VarFileInfo\Translation', Pointer(Res), Dummy);
    FLang    := Integer(Pointer(Res)^);
    FLang    := Integer((LoWord(FLang) shl 16) + HiWord(FLang));
    FCharSet := '\StringFileInfo\' + IntToHex(FLang, 8) + '\';
    VerQueryValue(FInfoBuf, '\', Pointer(Res), Dummy);
    Pointer(FFixInfo) := Res;
  end;
  FOsVerInfo.dwOSVersionInfoSize := sizeof(TOSVersionInfo);
  GetVersionEx(FOsVerInfo);
end;

destructor TVersionInfo.Destroy;
begin
  FreeMem(FInfoBuf);
  inherited Destroy;
end;

function TVersionInfo.GetStrFileInfo(Idx: TStringFileInfoType): string;
var
  KeyName: array[0..127] of Char;
  DataLen: DWORD;
  Res: PChar;
begin
  Result := '';
  if Assigned(FInfoBuf) then
    begin
      StrPCopy(KeyName, FCharSet + TStringFileInfoNames[Idx]);
      VerQueryValue(FInfoBuf, KeyName, Pointer(Res), DataLen);
      Result := StrPas(Res);
    end;
end;

function TVersionInfo.GetOSVersion: string;
begin
  Result := TLocalInformationDialog.GetWindowsVersion;
end;

function TVersionInfo.GetFileVersion: string;
var
  sMajor   : string;
  sMinor   : string;
  sRelease : string;
begin
  Result := '0.0.0';
  if Assigned(FixedFileInfo) then
  begin
    sMajor   := IntToStr(HiWord(FixedFileInfo.dwFileVersionMS));
    sMinor   := IntToStr(LoWord(FixedFileInfo.dwFileVersionMS));
    sRelease := IntToStr(HiWord(FixedFileInfo.dwFileVersionLS));
    if (sMajor = '') then
      sMajor := '0';
    if (sMinor = '') then
      sMinor := '0';
    if (sRelease = '') then
      sRelease := '0';
    Result := Format('%s.%s.%s', [sMajor, sMinor, sRelease]);
  end;
end;

function TVersionInfo.GetFileBuild: string;
var
  sBuild   : string;
begin
  Result := '0';
  if Assigned(FixedFileInfo) then
  begin
    Result := IntToStr(LoWord(FixedFileInfo.dwFileVersionLS));
    if (sBuild <> '') then
      Result := sBuild;
  end;
end;

function TVersionInfo.GetVersion(aValue: DWORD): string;
var
  sVersion : string;
  sBuild   : string;
begin
  Result := '0.0';
  if (sVersion = '') then
    sVersion := '0';

  if (sBuild  = '') then
    sVersion := '0';

  sBuild   := IntToStr(LoWord(aValue));
  sVersion := IntToStr(HiWord(aValue));
  Result   := Format('%s.%s', [sVersion, sBuild]);
end;

function TVersionInfo.ModuleDate: TDateTime;
var
  tFileDateTime: TDateTime;
begin
  FileAge(FFileName, tFileDateTime);
  Result := tFileDateTime;
end;

function TVersionInfo.ModuleSize: integer;
var
  FindData: TWin32FindData;
  hFind   : THandle;
begin
  Result := -1;
  hFind  := FindFirstFile(PChar(FFileName), FindData);
  if hFind <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(hFind);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      Result := FindData.nFileSizeLow;
  end;
end;

function TVersionInfo.ModuleVersion: string;
begin
  if Assigned(FixedFileInfo) then
    Result := GetVersion(FixedFileInfo.dwFileVersionLS)
  else
    Result := '0.0';
end;

function TVersionInfo.ProductVersion: string;
begin
  if Assigned(FixedFileInfo) then
    Result := GetVersion(FixedFileInfo.dwFileVersionMS)
  else
    Result := '0.0';
end;

end.

