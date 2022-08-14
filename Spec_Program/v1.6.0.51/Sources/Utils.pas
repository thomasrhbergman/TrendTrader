unit Utils;

interface

{$REGION 'Region uses'}
uses
  System.SysUtils, System.Variants, System.Classes, System.Generics.Defaults, System.Generics.Collections,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Winapi.Windows, System.DateUtils, Common.Types, System.IniFiles,
  Vcl.Controls;
{$ENDREGION}

  function FloatToStrEx(Value: Double): string;
  function IntToStrEx(Value: Integer): string;
  function IsNumber(str: string): Boolean;
  function IsFloat(str: string): Boolean;
  function RoundToNearest(aTime: TDateTime; aRoundInterval: Word): TDateTime;
  function StrToFloatEx(const str: string): Double;
  function StrToIntEx(const str: string): Integer;
  function NullIf(Value, Value1: Integer): Variant; overload;
  function GetDateFromHistoricalChart(aDate: string): TDateTime;
  function GetExpireMonthCode(aYear: SmallInt; aMonth: Byte; aIndexType: TIndexType): string;
  function GetExpiryDate(aDate: string): TDateTime;
  function GetLeftPadString(aText: string; aLength: Integer): string;
  function GetOccurrenceDay(aYear: SmallInt; aMonth, aDayOfWeek, aOccurrence: Byte): TDate;
  function GetRightPadString(aText: string; aLength: Integer): string;
  function GetUniqueList(List: string; Delimiter: Char = ','): string; inline;
  function BoolToChar(const aValue: Boolean): string; inline;
  function LeftStr(const aText: string; aLength: Word): string;
  function NullIf(Value, Value1: Real): Variant; overload;
  function NullIf(Value, Value1: String): Variant; overload;
  function RightStr(const aText: string; aLength: Word): string;
  function SafeStrToFloat(S: String; const ADefault: Single = 0): Extended; inline;
  function VarToBool(const Value: Variant): Boolean; inline;
  function VarToFloat(const V: Variant): Extended; inline;
  function VarToInt64Def(const Value: Variant; DefValue: Int64 = 0): Int64; inline;
  function VarToIntDef(const Value: Variant; DefValue: Integer = 0): Integer; inline;
  procedure SetFocusSafely(const aControl: TWinControl); inline;

  function IsInternetConnected: Boolean;

const
  C_ROUND_SEC = 2;

var
  NoDecimalSeparator: string;

implementation

function InternetGetConnectedState(lpdwFlags: LPDWORD; dwReserved:DWORD):BOOL; stdcall; external 'wininet.dll' name 'InternetGetConnectedState';

function NullIf(Value, Value1: Integer): Variant;
begin
  if Value = Value1 then
    Result := Null
  else
    Result := Value;
end;

function NullIf(Value, Value1: Real): Variant;
begin
  if Value = Value1 then
    Result := Null
  else
    Result := Value;
end;

function NullIf(Value, Value1: String): Variant;
begin
  if Value = Value1 then
    Result := Null
  else
    Result := Value;
end;

function RoundToNearest(aTime: TDateTime; aRoundInterval: Word): TDateTime;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(aTime, Hour, Min, Sec, MSec);
  Sec := (Sec div aRoundInterval) * aRoundInterval;
  Result := Trunc(aTime) + EncodeTime(Hour, Min, Sec, 0);
end;

function StrToIntEx(const str : string) : Integer;
begin
  if str = '' then
    Result := MaxInt
  else
    Result := StrToIntDef(str, 0)
end;

function IntToStrEx(value : Integer) : string;
begin
  if value = Integer.MaxValue then
    Result := ''
  else
    Result := IntToStr(value)
end;

function StrToFloatEx(const str : string): Double;
begin
  if str = '' then
    Result := 0
  else
    Result := StrToFloatDef(StringReplace(str, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]), 0);
end;

{$HINTS OFF}
function IsNumber(str: string): Boolean;
var
  iValue, iCode: Integer;
begin
  str := str.Replace('.', FormatSettings.DecimalSeparator).Replace(',', FormatSettings.DecimalSeparator);
  iCode := 0;
  Val(str, iValue, iCode); //Hint H2077 Value assigned to 'iValue' never used
  Result := iCode = 0;
end;
{$HINTS ON}

function IsFloat(str: string): Boolean;
var
  Value: Double;
begin
  Result := TryStrToFloat(str.Replace('.', FormatSettings.DecimalSeparator).Replace(',', FormatSettings.DecimalSeparator), Value);
end;

function FloatToStrEx(value : Double): string;
begin
  if value >= Double.MaxValue then
    Result := ''
  else
    Result := FloatToStr(value).Replace('.', FormatSettings.DecimalSeparator).Replace(',', FormatSettings.DecimalSeparator);
end;

function VarToFloat(const V: Variant): Extended;
begin
  if VarIsOrdinal(V) or VarIsFloat(V) then
    Result := V
  else
    Result := SafeStrToFloat(VarToStr(V));
end;

function SafeStrToFloat(S: String; const ADefault: Single = 0): Extended;
begin
  S := S.Replace(',', FormatSettings.DecimalSeparator).Replace('.', FormatSettings.DecimalSeparator);
  Result := StrToFloatDef(S, 0);
end;

function GetUniqueList(List: string; Delimiter: Char = ','): string;
var
  stList: THashedStringList;
begin
  Result := List;
  if not List.IsEmpty then
  begin
    stList := THashedStringList.Create;
    try
      stList.Sorted := True;
      stList.Duplicates := dupIgnore;
      stList.Delimiter := Delimiter;
      stList.DelimitedText := List;
      Result := stList.DelimitedText;
    finally
      FreeAndNil(stList);
    end;
  end;
end;

function VarToIntDef(const Value: variant; DefValue: Integer=0): Integer;
begin
  if VarIsOrdinal(Value) or VarIsNumeric(Value) then
    Result := Value
  else
    Result := StrToIntDef(varToStr(Value), DefValue);
end;

function VarToInt64Def(const Value: variant; DefValue: Int64=0): Int64;
begin
  if VarIsOrdinal(Value) or VarIsNumeric(Value) then
    Result := Value
  else
    Result := StrToInt64Def(varToStr(Value), DefValue);
end;

function VarToBool(const Value: Variant): Boolean;
var
  S: string;
begin
  if VarIsType(Value, varBoolean) then
    Result := Value
  else
  begin
    S := VarToStr(Value);
    Result := not(S.IsEmpty or SameText(S, 'false') or SameText(S, '0'));
  end;
end;

function GetDateFromHistoricalChart(aDate: string): TDateTime;
var
  FormatSettings : TFormatSettings;
begin
  if CharInSet(aDate[2], ['0' .. '9']) then
  begin
  {$WARN SYMBOL_PLATFORM OFF}
    FormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);
  {$WARN SYMBOL_PLATFORM ON}
    FormatSettings.ShortDateFormat := 'YYYY-MM-DD';
    FormatSettings.DateSeparator := '-';
    FormatSettings.LongTimeFormat := 'hh:nn:ss';
    FormatSettings.TimeSeparator := ':';
    Result := StrToDateTime(Copy(aDate, 1, 4) + '-' + Copy(aDate, 5, 2) + '-' + Copy(aDate, 7, 2) + ' ' + Copy(aDate, 11, 8), FormatSettings);
  end
  else
    Result := RoundToNearest(Now, C_ROUND_SEC);
end;

function GetExpiryDate(aDate: string): TDateTime;
var
  FormatSettings: TFormatSettings;
begin
{$WARN SYMBOL_PLATFORM OFF}
  FormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);
{$WARN SYMBOL_PLATFORM ON}
  FormatSettings.ShortDateFormat := 'YYYY-MM-DD';
  FormatSettings.DateSeparator := '-';
  FormatSettings.LongTimeFormat := 'hh:nn';
  FormatSettings.TimeSeparator := ':';
  Result := StrToDateTime(Copy(aDate, 1, 4) + '-' + Copy(aDate, 5, 2) + '-' + Copy(aDate, 7, 2) + ' ' + Copy(aDate, 10, 5), FormatSettings);
end;

function GetOccurrenceDay(aYear: SmallInt; aMonth, aDayOfWeek, aOccurrence: Byte): TDate;
var
  DiffDays        : SmallInt;
  FirstDayOfMonth : SmallInt;
  ResultedDay     : SmallInt;
begin
  if (aOccurrence < 1) or (aOccurrence > 5) then
    raise Exception.Create('Occurrence is invalid')
  else if (aMonth < 1) or (aMonth > 12) then
    raise Exception.Create('Month is invalid')
  else if (aDayOfWeek < 1) or (aDayOfWeek > 7) then
    raise Exception.Create('DayOfWeek is invalid');
  FirstDayOfMonth := DayOfTheWeek(StartOfAMonth(aYear, aMonth));
  DiffDays := aDayOfWeek - FirstDayOfMonth;
  if (DiffDays < 0) then
    Inc(DiffDays, 7);
  ResultedDay := (DiffDays + 1) + (7 * (aOccurrence - 1));
  Result := EncodeDate(aYear, aMonth, ResultedDay);
end;

function BoolToChar(const aValue: Boolean): string;
begin
  if aValue then
    Result := 'Y'
  else
    Result := 'N';
end;

function GetExpireMonthCode(aYear: SmallInt; aMonth: Byte; aIndexType: TIndexType): string;
const
  arrIndexFuturesNasdaq: array [1..12] of string = ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L');
//  arrStockFuturesNasdaq: array [1..12] of string = ('M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X');
begin
  if (aMonth < 1) or (aMonth > 12) then
    raise Exception.Create('Month is invalid');
//  case aIndexType of
//    itIndexFutures:
      Result := (aYear Mod 10 ).ToString + arrIndexFuturesNasdaq[aMonth];
//    itStockFutures:
//      Result := (aYear Mod 10 ).ToString + arrStockFuturesNasdaq[aMonth];
//  end;
end;

function GetRightPadString(aText: string; aLength: Integer): string;
begin
  if (aLength > aText.Length) then
    Result := aText + StringOfChar(' ', aLength - aText.Length)
  else
    Result := aText;
end;

function GetLeftPadString(aText: string; aLength: Integer): string;
begin
  if (aLength > aText.Length) then
    Result := StringOfChar(' ', aLength - aText.Length) + aText
  else
    Result := aText;
end;

function RightStr(const aText: string; aLength: Word): string;
var
  len: Byte absolute aText;
begin
  if aLength > len then
    aLength := len;
  RightStr := Copy(aText, len - aLength + 1, aLength)
end;

function LeftStr(const aText: string; aLength: Word): string;
begin
  LeftStr := Copy(aText, 1, aLength)
end;

procedure SetFocusSafely(const aControl: TWinControl);
begin
  if Assigned(aControl) then
    if aControl.CanFocus and aControl.Enabled then
      aControl.SetFocus;
end;

function IsInternetConnected: Boolean;
const
  INTERNET_CONNECTION_MODEM = 1;
  INTERNET_CONNECTION_LAN   = 2;
  INTERNET_CONNECTION_PROXY = 4;
var
  dwConnectionTypes: DWORD;
begin
  dwConnectionTypes := INTERNET_CONNECTION_MODEM or
                       INTERNET_CONNECTION_LAN or
                       INTERNET_CONNECTION_PROXY;
  Result := InternetGetConnectedState(@dwConnectionTypes, 0);
end;

end.
