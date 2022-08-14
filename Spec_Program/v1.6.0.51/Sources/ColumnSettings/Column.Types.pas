unit Column.Types;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, IABSocketAPI, IABSocketAPI_const,
  System.Generics.Collections, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} DebugWriter;
{$ENDREGION}

type
  PColumnSetting = ^TColumnSetting;
  TColumnSetting = record
    Name     : string;
    Position : Integer;
    Index    : Integer;
    Width    : Integer;
    Visible  : Boolean;
    TickType : TIABTickType;
    Tag      : NativeInt;
    procedure AssignFrom(aColumnSetting: TColumnSetting);
    procedure Clear;
  end;

implementation

{ TColumnSetting }

procedure TColumnSetting.AssignFrom(aColumnSetting: TColumnSetting);
begin
  Self.Name     := aColumnSetting.Name;
  Self.Position := aColumnSetting.Position;
  Self.Index    := aColumnSetting.Index;
  Self.Width    := aColumnSetting.Width;
  Self.Visible  := aColumnSetting.Visible;
  Self.TickType := aColumnSetting.TickType;
  Self.Tag      := aColumnSetting.Tag;
end;

procedure TColumnSetting.Clear;
begin
  Name := '';
end;

end.
