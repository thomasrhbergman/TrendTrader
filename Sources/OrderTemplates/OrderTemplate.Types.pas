unit OrderTemplate.Types;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, VirtualTrees, IABFunctions,
  IABSocketAPI, System.Generics.Collections, BrokerHelperAbstr, Winapi.msxml, Vcl.Graphics, Entity.Sokid,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const, DebugWriter, HtmlLib, Chart.Trade,
  System.DateUtils, XmlFiles, Data.DB, DaModule, InstrumentList, System.Math, System.Threading,
  Publishers.Interfaces, Generics.Helper, Common.Types, AutoTrades.Types, Bleeper, DaModule.Utils, ArrayHelper,
  Global.Types, Publishers, Vcl.Forms, IABFunctions.Helpers, IABFunctions.MarketData, Vcl.ExtCtrls,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  POrderTemplate = ^TOrderTemplate;
  TOrderTemplate = class(TInterfacedPersistent)
  private
    FRecordId: Integer;
    FName: string;
  public
    function ToString: string; override;
    function ToValueString: string;
    procedure AssignFrom(const aOrderTemplate: TOrderTemplate);
    procedure Clear;
    procedure FromDB(const aID: Integer);
    procedure SaveToDB;
    class procedure DeleteFromDB(aID: Integer); static;

    property RecordId         : Integer              read FRecordId         write FRecordId;
    property Name             : string               read FName             write FName;
  end;

implementation

{ TOrderTemplate }

procedure TOrderTemplate.AssignFrom(const aOrderTemplate: TOrderTemplate);
begin
  Self.Clear;
  Self.RecordId         := aOrderTemplate.RecordId;
  Self.Name             := aOrderTemplate.Name;
end;

procedure TOrderTemplate.Clear;
begin
  Self.Name             := '';
  Self.RecordId         := -1;
end;

class procedure TOrderTemplate.DeleteFromDB(aID: Integer);
resourcestring
  C_SQL_DELETE = 'DELETE FROM ORDER_TEMPLATES WHERE ID=%d';
begin
  DMod.ExecuteSQL(Format(C_SQL_DELETE, [aID]));
end;

procedure TOrderTemplate.FromDB(const aID: Integer);
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT * FROM ORDER_TEMPLATES WHERE ID=:ID';
var
  Query: TFDQuery;
begin
  Self.Clear;
  if (aID > 0) then
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := C_SQL_SELECT_TEXT;
      Query.ParamByName('ID').AsInteger := aID;
      Query.Open;
      if not Query.IsEmpty then
      begin
        Self.Name             := Query.FieldByName('NAME').AsString;
      end;
      Self.RecordId := aID;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TOrderTemplate.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM ORDER_TEMPLATES WHERE ID=:ID';
  C_SQL_UPDATE_TEXT = 'UPDATE ORDER_TEMPLATES SET NAME=:NAME' + sLineBreak +
                                             'WHERE ID=:ID';

  C_SQL_INSERT_TEXT = 'INSERT INTO ORDER_TEMPLATES ( ID, NAME)' + sLineBreak +
                                          ' VALUES (:ID,:NAME)';
var
  Query: TFDQuery;
  IsExists: Boolean;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    IsExists := False;
    if (Self.RecordId > 0) then
    begin
      Query.SQL.Text := C_SQL_EXISTS_TEXT;
      try
        Query.ParamByName('ID').AsInteger := Self.RecordId;
        Query.Prepare;
        Query.Open;
        IsExists := Query.FieldByName('CNT').AsInteger > 0;
        Query.Close;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'SaveToDB', 'OrderTemplate', E.Message + TDModUtils.GetQueryInfo(Query));
          raise;
        end;
      end;
    end
    else
      Self.RecordId := DMod.GetNextValue('GEN_ORDER_TEMPLATES_ID');

    if IsExists then
      Query.SQL.Text := C_SQL_UPDATE_TEXT
    else
      Query.SQL.Text := C_SQL_INSERT_TEXT;

    if Self.Name.IsEmpty or SameText(Self.Name, 'Order Template') then
      Self.Name := 'OrderTemplate nr' + Self.RecordId.ToString;

    Query.ParamByName('ID').AsInteger                 := Self.RecordId;
    Query.ParamByName('NAME').AsString                := Self.Name.Substring(0, 100);

    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'SaveToDB', 'OrderTemplate', E.Message + TDModUtils.GetQueryInfo(Query));
    end;
  finally
    FreeAndNil(Query);
    DMod.RefreshQuery(DMod.fbqQualifiers);
  end;

end;

function TOrderTemplate.ToString: string;
begin
  Result := Self.Name;
end;

function TOrderTemplate.ToValueString: string;
begin
  Result := '';
end;

end.