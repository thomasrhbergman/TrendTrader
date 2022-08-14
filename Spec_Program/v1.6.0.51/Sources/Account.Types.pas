unit Account.Types;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.Classes, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}  System.UITypes;
{$ENDREGION}

type
  PAccount = ^TAccount;
  TAccount = record
    AccountName   : string;
    Info          : string;
    ParameterName : string;
    Value         : string;
    procedure Clear;
  end;

implementation

{ TAccount }

procedure TAccount.Clear;
begin
  ParameterName := '';
  Value         := '';
  Info          := '';
  AccountName   := ''
end;

end.
