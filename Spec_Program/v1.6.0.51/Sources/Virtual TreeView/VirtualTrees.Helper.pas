unit VirtualTrees.Helper;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Types, System.Generics.Collections, System.Generics.Defaults,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}  VirtualTrees, VirtualTrees.Classes, System.Rtti, System.TypInfo;
{$ENDREGION}

type
  TVirtualStringTreeHelper = class helper for TVirtualStringTree
  public
    procedure SetNodeDataType(const Value: Pointer);
  end;

implementation

{ TVirtualStringTreeHelper }

procedure TVirtualStringTreeHelper.SetNodeDataType(const Value: Pointer);
begin
  Self.NodeDataSize := SizeOf(Value);
end;

end.
