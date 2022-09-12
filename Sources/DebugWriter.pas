unit DebugWriter;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.Classes, Vcl.Forms, System.SysUtils, System.Variants, System.IOUtils, Vcl.Graphics,
  System.DateUtils, System.Threading, Utils.VerInfo, Utils.LocalInformation, HtmlLib, HtmlConsts, XmlFiles,
  System.Types, Common.Types;
{$ENDREGION}

type
  TFileWriter = class
  private
    FCriticalSection : TRTLCriticalSection;
    FFileStream      : TFileStream;
    FFileName        : string;
    FLogPath         : string;
    FStarted         : Boolean;
    function GetSize: Int64;
    procedure SetLogPath(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Finish;
    procedure Start;
    procedure Write(const aInfo: string);
    property FileName : string  read FFileName write FFileName;
    property LogPath  : string  read FLogPath  write SetLogPath;
    property Size     : Int64   read GetSize;
    property Started  : Boolean read FStarted;
  end;

  TLogWriter = class(TComponent)
  private const
    C_FOLDER_LOG     : string = 'log\';
    C_CHAR_ENTER     : Char = #13;
    C_CHAR_LINE_FEED : Char = #10;
  private
    FBaseFileName     : string;
    FCountFiles       : Integer;
    FCountOfDays      : Integer;
    FIsExistHtmlClose : Boolean;
    FIsExistHtmlOpen  : Boolean;
    FLineCount        : Integer;
    FLogFile          : TFileWriter;
    FMaxSize          : Int64;
    function GetActive: Boolean;
    function GetDebugFileName: string;
    function GetLineCount: Integer;
    function GetLog(const aFileName: string = ''): string;
    function GetLogFileName: string;
    function IsStartDebug: Boolean;
    procedure CheckSize;
    procedure DeleteOldFiles;
    procedure Finish;
    procedure RestoreStartParams;
    procedure SetActive(const aValue: Boolean);
    procedure Start;
    procedure WriteFileInfo;
    procedure WriteHtm(const aDetailType: TLogDetailType; aUnit, aClassName, aMethod, aInfo: string); inline;
    procedure WriteText(const aInfo: string);

    property LineCount: Integer read GetLineCount write FLineCount;
  private const
    C_TABLE_TD_TAG     = '<TD class="%s"></TD><TD>%s</TD><TD>%s</TD><TD>%s</TD><TD>%s</TD><TD>%s</TD><TD>%s</TD></TR>';
    C_TABLE_ERROR_TAG  = '<TR class="err">' + C_TABLE_TD_TAG;
    C_TABLE_METHOD_TAG = '<TR class="met">' + C_TABLE_TD_TAG;
    C_TABLE_OBJECT_TAG = '<TR class="obj">' + C_TABLE_TD_TAG;
    C_TABLE_TEXT_TAG   = '<TR class="txt">' + C_TABLE_TD_TAG;
    C_TABLE_WARN_TAG   = '<TR class="warn">' + C_TABLE_TD_TAG;
  public const
    C_IMG_ENTER_HTM = 'img-enter';
    C_IMG_ERROR_HTM = 'img-err';
    C_IMG_EXIT_HTM  = 'img-exit';
    C_DATE_FORMAT   = 'DD.MM.YYYY hh:mm:ss.zzz';

    C_CFG_COUNT_OF_DAYS = 'CountOfDays';
    C_CFG_KEY_IS_START  = 'IsStartDebug';
    C_CFG_KEY_MAX_SIZE  = 'MaxSizeOfLogFile';
    C_CFG_SECTION_DEBUG = 'Debug';
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Write(const aDetailType: TLogDetailType; const aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aMethod, aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aMethod, aUnit, aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aObject: TObject; const aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aObject: TObject; const aMethod, aInfo: string); overload;

    property Active      : Boolean read GetActive    write SetActive;
    property LogFileName : string  read GetLogFileName;
    property CountOfDays : Integer read FCountOfDays write FCountOfDays default 30;    //Number of days during which logs are stored
    property MaxSize     : Int64   read FMaxSize     write FMaxSize     default 0;     //Maximum log file size(MB). When <0 do not control
  end;

var
  LogWriter : TLogWriter;

implementation

{ TLogWriter }

constructor TLogWriter.Create(AOwner: TComponent);
begin
  inherited;
  FCountFiles       := 0;
  FIsExistHtmlOpen  := False;
  FIsExistHtmlClose := False;
  FCountOfDays      := 30;
  FBaseFileName     := '';

  if IsStartDebug then
  begin
    RestoreStartParams;
    Start;
  end;

  if Active then
  begin
    WriteFileInfo;
    DeleteOldFiles;
  end;
end;

procedure TLogWriter.DeleteOldFiles;
begin
  TTask.Create(
    procedure()
    begin
      TThread.NameThreadForDebugging('TLogWriter.DeleteOldFiles');
      for var FileName in TDirectory.GetFiles(GetLog, '*.html', TSearchOption.soAllDirectories) do
      begin
        if (DaysBetween(Now, TFile.GetCreationTime(FileName)) >= CountOfDays) then
          try
            TFile.Delete(FileName);
          except
          end;
      end;
    end).Start;
end;

destructor TLogWriter.Destroy;
begin
  FIsExistHtmlClose := True;
  Finish;
  if Assigned(FLogFile) then
    FreeAndNil(FLogFile);
  inherited;
end;

procedure TLogWriter.WriteFileInfo;
var
  loVersionInfo : TVersionInfo;
  sHostName     : string;
  sIP           : string;
  sText         : string;
  sModuleName   : string;
begin
  sText         := '';
  sModuleName   := Application.ExeName;
  TLocalInformationDialog.GetLocalIPAddressName(sIP, sHostName);
  loVersionInfo := TVersionInfo.Create(sModuleName);
  try
    sText := Concat(C_HTML_LINE + '<pre>',
                    THtmlLib.GetBoldText('Module           : '), sModuleName,                                                C_HTML_BREAK,
                    THtmlLib.GetBoldText('Module version   : '), loVersionInfo.FileVersion, '.', loVersionInfo.FileBuild,    C_HTML_BREAK,
                    THtmlLib.GetBoldText('Module date      : '), DateToStr(loVersionInfo.ModuleDate),                        C_HTML_BREAK,
                    THtmlLib.GetBoldText('Module size      : '), FormatFloat('### ### ### bytes', loVersionInfo.ModuleSize), C_HTML_BREAK,
                    THtmlLib.GetBoldText('Local IP-address : '), sIP, ' (', sHostName, ')',                                  C_HTML_BREAK,
                    THtmlLib.GetBoldText('Windows version  : '), TLocalInformationDialog.GetWindowsVersion,                     C_HTML_BREAK,
                    THtmlLib.GetBoldText('Windows user     : '), TLocalInformationDialog.GetUserFromWindows,                    C_HTML_BREAK,
                    THtmlLib.GetBoldText('Compiler version : '), CompilerVersion.ToString,                                   C_HTML_BREAK,
                    '</pre>' + C_HTML_LINE);
    WriteHtm(ddText, C_HTML_NBSP, C_HTML_NBSP, C_HTML_NBSP, sText);
  finally
    FreeAndNil(loVersionInfo);
  end;
end;

function TLogWriter.IsStartDebug: Boolean;
//var
//  loXmlFile : TXmlFile;
begin
  Result := True;
//  loXmlFile := TXmlFile.Create(GetEnvironmentVariable('USERPROFILE') + '\RoboTrade.xml');
//  try
//    Result := loXmlFile.ReadBool(C_CFG_SECTION_DEBUG, C_CFG_KEY_IS_START, True);
//  finally
//    FreeAndNil(loXmlFile);
//  end;
end;

procedure TLogWriter.RestoreStartParams;
var
  loXmlFile : TXmlFile;
begin
  loXmlFile := TXmlFile.Create(GetEnvironmentVariable('USERPROFILE') + '\RoboTrade.xml');
  try
    loXmlFile.UsedAttributes := [uaCodeType, uaValue, uaComment];
    FMaxSize     := loXmlFile.ReadInt64(C_CFG_SECTION_DEBUG, C_CFG_KEY_MAX_SIZE, 0) * 1024 * 1024;
    FCountOfDays := loXmlFile.ReadInteger(C_CFG_SECTION_DEBUG, C_CFG_COUNT_OF_DAYS, 30);
    if not loXmlFile.ValueExists(C_CFG_SECTION_DEBUG, C_CFG_KEY_MAX_SIZE) then
      loXmlFile.WriteInt64(C_CFG_SECTION_DEBUG, C_CFG_KEY_MAX_SIZE, FMaxSize, 'Max Size Of Log File (Mb)');
    if not loXmlFile.ValueExists(C_CFG_SECTION_DEBUG, C_CFG_COUNT_OF_DAYS) then
      loXmlFile.WriteInteger(C_CFG_SECTION_DEBUG, C_CFG_COUNT_OF_DAYS, FCountOfDays, 'Number of days during which logs are stored');
  finally
    FreeAndNil(loXmlFile);
  end;
end;

function TLogWriter.GetActive: Boolean;
begin
  if Assigned(FLogFile) then
    Result := FLogFile.Started
  else
    Result := False;
end;

procedure TLogWriter.WriteText(const aInfo: string);
begin
  if Active then
  begin
    CheckSize;
    FLogFile.Write(aInfo);
  end;
end;

procedure TLogWriter.Start;
var
  sText : string;
begin
  if not Assigned(FLogFile) then
  begin
    FLogFile := TFileWriter.Create;
    FLogFile.FileName := GetDebugFileName;
    FLogFile.LogPath  := GetLog;
  end;

  if (not FLogFile.Started) then
  begin
    FLogFile.Start;
    if not FIsExistHtmlOpen then
    begin
      FIsExistHtmlOpen := True;
      sText := Concat(C_HTML_OPEN,
                      C_HTML_HEAD_OPEN,
                      C_STYLE,
                      C_HTML_HEAD_CLOSE,
                      THtmlLib.GetTableTag(VarArrayOf([C_HTML_NBSP,
                                                        'Line &#8470;',
                                                        'Time',
                                                        'Unit name',
                                                        'Class name',
                                                        'Method name',
                                                        'Description'])));
    end
    else
      WriteText(THtmlLib.GetColorTag(THtmlLib.GetBoldText('Log session already started'), clNavy));
    if (sText <> '') then
      WriteText(sText);
  end;
end;

procedure TLogWriter.Finish;
var
  sText : string;
begin
  if Assigned(FLogFile) and FLogFile.Started then
  begin
    sText := THtmlLib.GetColorTag(THtmlLib.GetBoldText('Log session finished'), clNavy);
    sText := Concat(C_HTML_TABLE_CLOSE, sText, C_HTML_CLOSE);
  end;
end;

procedure TLogWriter.SetActive(const aValue: Boolean);
begin
  if aValue then
    Start
  else
    Finish;
end;

function TLogWriter.GetDebugFileName: string;
begin
  if FBaseFileName.IsEmpty then
    FBaseFileName := Concat(ChangeFileExt(ExtractFileName(Application.ExeName), '_'), FormatDateTime('yyyy.mm.dd hh.nn.ss', Now)).ToLower;
  if (FCountFiles > 0) then
    Result := Concat(FBaseFileName, FormatFloat('_000', FCountFiles), '.html')
  else
    Result := Concat(FBaseFileName, '.html');
end;

function TLogWriter.GetLineCount: Integer;
begin
  Inc(FLineCount);
  if (FLineCount >= 1000000) then
   FLineCount := 1;
  Result := FLineCount;
end;

function TLogWriter.GetLog(const aFileName: string): string;
var
  sPath: string;
  sRootPath: string;
begin
  sRootPath := ExtractFilePath(Application.ExeName);
  sPath := IncludeTrailingPathDelimiter(sRootPath + C_FOLDER_LOG);
  if (ExtractFileDrive(sPath) <> '') and (not DirectoryExists(sPath)) then
    try
      ForceDirectories(sPath);
    except
      raise Exception.Create(Format('Do not create folder [%s].', [sPath]));
    end;
  Result := Concat(sPath, aFileName);
end;

function TLogWriter.GetLogFileName: string;
begin
  if Assigned(FLogFile) then
    Result := TPath.Combine(FLogFile.LogPath, FLogFile.FileName)
  else
    Result := '';
end;

procedure TLogWriter.Write(const aDetailType: TLogDetailType; const aInfo: string);
begin
  WriteHtm(aDetailType, C_HTML_NBSP, C_HTML_NBSP, C_HTML_NBSP, aInfo);
end;

procedure TLogWriter.Write(const aDetailType: TLogDetailType; const aMethod, aInfo: string);
begin
  WriteHtm(aDetailType, C_HTML_NBSP, C_HTML_NBSP, aMethod, aInfo);
end;

procedure TLogWriter.Write(const aDetailType: TLogDetailType; const aMethod, aUnit, aInfo: string);
begin
  WriteHtm(aDetailType, C_HTML_NBSP, C_HTML_NBSP, aMethod, aInfo);
end;

procedure TLogWriter.Write(const aDetailType: TLogDetailType; const aObject: TObject; const aMethod, aInfo: string);
var
  ClassName: string;
begin
  if Assigned(aObject) then
    ClassName := aObject.ClassName
  else
    ClassName := C_HTML_NBSP;
  WriteHtm(aDetailType, C_HTML_NBSP, ClassName, aMethod, aInfo);
end;

procedure TLogWriter.Write(const aDetailType: TLogDetailType; const aObject: TObject; const aInfo: string);
var
  ClassName: string;
begin
  if Assigned(aObject) then
    ClassName := aObject.ClassName
  else
    ClassName := C_HTML_NBSP;
  WriteHtm(aDetailType, C_HTML_NBSP, ClassName, C_HTML_NBSP, aInfo);
end;

procedure TLogWriter.WriteHtm(const aDetailType: TLogDetailType; aUnit, aClassName, aMethod, aInfo: string);
begin
  if Active then
  begin
    CheckSize;
    if aUnit.IsEmpty then
      aUnit := C_HTML_NBSP;
    if aClassName.IsEmpty then
      aClassName := C_HTML_NBSP;
    if aMethod.IsEmpty then
      aMethod := C_HTML_NBSP;
    if aInfo.IsEmpty then
      aInfo := C_HTML_NBSP;

    if (aDetailType = ddError) then
      aInfo := aInfo.Replace('\n', C_HTML_BREAK);

    aInfo := aInfo.Replace(sLineBreak, C_HTML_BREAK).Replace(C_CHAR_ENTER, C_HTML_BREAK).Replace(C_CHAR_LINE_FEED, C_HTML_BREAK);
    case aDetailType of
      ddEnterMethod:
        FLogFile.Write(Format(C_TABLE_METHOD_TAG, [C_IMG_ENTER_HTM, Format('%.6u', [LineCount]), FormatDateTime(C_DATE_FORMAT, Now), aUnit, aClassName, aMethod, aInfo]));
      ddExitMethod:
        FLogFile.Write(Format(C_TABLE_METHOD_TAG, [C_IMG_EXIT_HTM, Format('%.6u', [LineCount]), FormatDateTime(C_DATE_FORMAT, Now), aUnit, aClassName, aMethod, aInfo]));
      ddError:
        FLogFile.Write(Format(C_TABLE_ERROR_TAG, [C_IMG_ERROR_HTM, Format('%.6u', [LineCount]), FormatDateTime(C_DATE_FORMAT, Now), aUnit, aClassName, aMethod, aInfo]));
      ddText:
        FLogFile.Write(Format(C_TABLE_TEXT_TAG, ['', Format('%.6u', [LineCount]), FormatDateTime(C_DATE_FORMAT, Now), aUnit, aClassName, aMethod, aInfo]));
      ddWarning:
        FLogFile.Write(Format(C_TABLE_WARN_TAG, ['', Format('%.6u', [LineCount]), FormatDateTime(C_DATE_FORMAT, Now), aUnit, aClassName, aMethod, aInfo]));
    end;
  end;
end;

procedure TLogWriter.CheckSize;
const
  C_TAG_LINK = '<br><a href="%s">Next log file</a>';
var
  sNewFileName: string;
begin
  if (MaxSize > 0) and (FLogFile.Size >= MaxSize) then
  begin
    FIsExistHtmlOpen := False;
    Inc(FCountFiles);
    sNewFileName := GetDebugFileName;
    FLogFile.Write(Concat(C_HTML_TABLE_CLOSE, Format(C_TAG_LINK, [sNewFileName]), C_HTML_CLOSE));
    FLogFile.Finish;
    FLogFile.FileName := sNewFileName;
    Start;
    FIsExistHtmlClose := False;
  end;
end;

{ TFileWriter }

constructor TFileWriter.Create;
begin
  inherited;
  FStarted := False;
end;

destructor TFileWriter.Destroy;
begin
  if Assigned(FFileStream) then
    FreeAndNil(FFileStream);
  DeleteCriticalSection(FCriticalSection);
  inherited;
end;

procedure TFileWriter.Finish;
begin
  FStarted := False;
  DeleteCriticalSection(FCriticalSection);
  if Assigned(FFileStream) then
    FreeAndNil(FFileStream);
  inherited;
end;

function TFileWriter.GetSize: Int64;
begin
  if Assigned(FFileStream) then
    Result := FFileStream.Size
  else
    Result := 0;
end;

procedure TFileWriter.SetLogPath(const Value: string);
begin
  if (Value <> '') then
    FLogPath := IncludeTrailingPathDelimiter(Value)
  else
    FLogPath := ExtractFilePath(Application.ExeName);

  if not DirectoryExists(Value) then
    if not CreateDir(Value) then
      FLogPath := '';
end;

procedure TFileWriter.Start;
var
  sFileName : string;
  BOM: TBytes;
begin
  InitializeCriticalSection(FCriticalSection);
  FStarted  := True;
  sFileName := Concat(LogPath, FileName);
  if not Assigned(FFileStream) then
  begin
    if FileExists(sFileName) then
    begin
      FFileStream := TFileStream.Create(sFileName, fmOpenWrite or fmShareDenyWrite);
      FFileStream.Seek(0, soFromEnd);
    end
    else
    begin
      FFileStream := TFileStream.Create(sFileName, fmCreate or fmShareDenyWrite);
      BOM := TEncoding.UTF8.GetPreamble;
      FFileStream.WriteBuffer(Bom[0], Length(BOM));
    end;
  end;
end;

procedure TFileWriter.Write(const aInfo: string);
var
  Buff: TBytes;
begin
  if FStarted then
  begin
    EnterCriticalSection(FCriticalSection);
    try
      Buff := TEncoding.UTF8.GetBytes(aInfo);
      FFileStream.WriteBuffer(Buff, Length(Buff));
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
  end;
end;

initialization
  if not Assigned(LogWriter) and not System.IsLibrary then
    LogWriter := TLogWriter.Create(Application);

finalization
//  if Assigned(LogWriter) then
//    LogWriter.Free;

end.
