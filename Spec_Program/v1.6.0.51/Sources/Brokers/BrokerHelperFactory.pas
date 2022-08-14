unit BrokerHelperFactory;

interface

uses
  BrokerHelperAbstr, System.SysUtils, System.Generics.Collections, System.Classes
  {$IFDEF USE_CODE_SITE}, CodeSiteLogging{$ENDIF};

type
  TRegisteredBrokerHelper = TDictionary<string, TBrokerHelperClass>;
  TBrokerHelperFactory = class
    class procedure FillBrokerHelpers(AList: TStrings);
    class function GetBrokerHelperClass(AClassName: string): TBrokerHelperClass;
  end;

  function RegisteredBrokerHelper: TRegisteredBrokerHelper;
  procedure RegBrokerHelper(AName: string; AClass: TBrokerHelperClass);
  procedure UnRegBrokerHelper(AName: string);

implementation

{ TBrokerHelperFactory }

var
  FRegisteredBrokerHelper: TRegisteredBrokerHelper;

function RegisteredBrokerHelper: TRegisteredBrokerHelper;
begin
  if FRegisteredBrokerHelper = nil then
    FRegisteredBrokerHelper := TRegisteredBrokerHelper.Create;
  Result := FRegisteredBrokerHelper;
end;

procedure RegBrokerHelper(AName: string; AClass: TBrokerHelperClass);
begin
  with RegisteredBrokerHelper do
    if not ContainsKey(AName) then
      Add(AName, AClass);
end;

procedure UnRegBrokerHelper(AName: string);
begin
  if FRegisteredBrokerHelper = nil then
    Exit;
  FRegisteredBrokerHelper.Remove(AName);
end;

class procedure TBrokerHelperFactory.FillBrokerHelpers(AList: TStrings);
var
  p:TPair<String, TBrokerHelperClass>;
begin
  if AList = nil then
    Exit;
  for p in RegisteredBrokerHelper do
    AList.Add(p.Key);
end;

class function TBrokerHelperFactory.GetBrokerHelperClass(AClassName: String): TBrokerHelperClass;
var
  p:TPair<String, TBrokerHelperClass>;
begin
  for p in RegisteredBrokerHelper do
    if SameText(AClassName, p.Key) then
      Exit(p.Value);
  if RegisteredBrokerHelper.Count > 0 then
    Result := RegisteredBrokerHelper.Values.ToArray[0]
  else
    Result := nil;
end;

initialization

finalization
  FreeAndNil(FRegisteredBrokerHelper);

end.
