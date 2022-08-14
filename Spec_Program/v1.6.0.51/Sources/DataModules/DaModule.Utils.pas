unit DaModule.Utils;

interface

{$REGION 'Region uses'}
uses
  System.SysUtils, System.Classes, Data.DB, IBX.IBCustomDataSet, IBX.IBTable, IBX.IB, IBX.IBQuery, IBX.IBDatabase,
  IBX.IBStoredProc, Vcl.Controls, Winapi.Windows, Vcl.Dialogs, IBX.IBSQL, IBX.IBScript,
  IBX.IBServices, DebugWriter, HtmlLib, System.IOUtils, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  Datasnap.DBClient, System.Variants, FireDAC.Comp.Client, FireDAC.Stan.Param;
{$ENDREGION}
type
  TDModUtils = class
  public
    class function GetQueryInfo(const aQuery: TIBQuery): string; overload; inline;
    class function GetQueryInfo(const aStoredProc: TIBStoredProc): string; overload; inline;
    class function GetQueryInfo(const aQuery: TFDQuery): string; overload; inline;
    class function GetQueryInfo(const aStoredProc: TFDStoredProc): string; overload; inline;
  end;

implementation

class function TDModUtils.GetQueryInfo(const aQuery: TIBQuery): string;
var
  i: Integer;
  Param: string;
begin
  if Assigned(aQuery) then
  begin
    Result := sLineBreak + THtmlLib.SqlToHtml(aQuery.SQL.Text) + sLineBreak;
    for i := 0 to aQuery.Params.Count - 1 do
    begin
      if aQuery.Params[i].Name.ToUpper.Contains('XML') then
        Param := THtmlLib.GetBoldText(aQuery.Params[i].Name) + ': ' + THtmlLib.XmlToHtml(VarToStr(aQuery.Params[i].Value)) + sLineBreak
      else
        Param := THtmlLib.GetBoldText(aQuery.Params[i].Name) + ': ' + VarToStr(aQuery.Params[i].Value) + sLineBreak;
      Result := Result + Param;
    end;
  end;
end;

class function TDModUtils.GetQueryInfo(const aQuery: TFDQuery): string;
var
  i: Integer;
  Param: string;
begin
  if Assigned(aQuery) then
  begin
    Result := sLineBreak + THtmlLib.SqlToHtml(aQuery.SQL.Text) + sLineBreak;
    for i := 0 to aQuery.Params.Count - 1 do
    begin
      if aQuery.Params[i].Name.ToUpper.Contains('XML') then
        Param := THtmlLib.GetBoldText(aQuery.Params[i].Name) + ': ' + THtmlLib.XmlToHtml(VarToStr(aQuery.Params[i].Value)) + sLineBreak
      else
        Param := THtmlLib.GetBoldText(aQuery.Params[i].Name) + ': ' + VarToStr(aQuery.Params[i].Value) + sLineBreak;
      Result := Result + Param;
    end;
  end;
end;

class function TDModUtils.GetQueryInfo(const aStoredProc: TIBStoredProc): string;
var
  i: Integer;
  Param: string;
begin
  if Assigned(aStoredProc) then
  begin
    Result := sLineBreak + aStoredProc.StoredProcName + sLineBreak;
    for i := 0 to aStoredProc.Params.Count - 1 do
    begin
      if aStoredProc.Params[i].Name.ToUpper.Contains('XML') then
        Param := THtmlLib.GetBoldText(aStoredProc.Params[i].Name) + ': ' + THtmlLib.XmlToHtml(VarToStr(aStoredProc.Params[i].Value)) + sLineBreak
      else
        Param := THtmlLib.GetBoldText(aStoredProc.Params[i].Name) + ': ' + VarToStr(aStoredProc.Params[i].Value) + sLineBreak;
      Result := Result + Param;
    end;
  end;
end;

class function TDModUtils.GetQueryInfo(const aStoredProc: TFDStoredProc): string;
var
  i: Integer;
  Param: string;
begin
  if Assigned(aStoredProc) then
  begin
    Result := sLineBreak + aStoredProc.StoredProcName + sLineBreak;
    for i := 0 to aStoredProc.Params.Count - 1 do
    begin
      if aStoredProc.Params[i].Name.ToUpper.Contains('XML') then
        Param := THtmlLib.GetBoldText(aStoredProc.Params[i].Name) + ': ' + THtmlLib.XmlToHtml(VarToStr(aStoredProc.Params[i].Value)) + sLineBreak
      else
        Param := THtmlLib.GetBoldText(aStoredProc.Params[i].Name) + ': ' + VarToStr(aStoredProc.Params[i].Value) + sLineBreak;
      Result := Result + Param;
    end;
  end;
end;


end.
