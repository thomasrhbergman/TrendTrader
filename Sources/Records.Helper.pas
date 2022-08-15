unit Records.Helper;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
 {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Rtti;
{$ENDREGION}

type
  TRecordNameAttribute = class(TCustomAttribute)
  private
    FText: string;
  public
    class function DisplayLabelFor(ATypeInfo: Pointer): string;
    constructor Create(const aText: string);
    property Text: string read FText write FText;
  end;

implementation

{ TRecordNameAttribute }

constructor TRecordNameAttribute.Create(const aText: string);
begin
  FText := aText;
end;

class function TRecordNameAttribute.DisplayLabelFor(ATypeInfo: Pointer): string;
var
  attribute: TCustomAttribute;
  rttiContext: TRttiContext;
  rttiType: TRttiType;
begin
  Result := '';
  rttiContext := TRttiContext.Create;
  try
    rttiType := rttiContext.GetType(ATypeInfo);
    for attribute in rttiType.GetAttributes do
      if attribute is TRecordNameAttribute then
        Exit(TRecordNameAttribute(attribute).Text);
  finally
    rttiContext.Free;
  end;
end;

end.
