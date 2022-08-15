unit Search.Types;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, VirtualTrees, IABFunctions,
  IABSocketAPI, System.Generics.Collections, Winapi.msxml, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  IABSocketAPI_const, DebugWriter, System.DateUtils, XmlFiles, System.Math, System.Threading, ArrayHelper, Utils,
  Document, Data.DB, DaModule, HtmlLib, DaModule.Utils, Common.Types, Global.Types, Publishers,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TESParameter = record
    TradingClass: string;
    Expirations: string;
    Strikes: string;
    ExpirationsArray: TStringArray;
    StrikesArray: TDoubleArray;
    procedure Clear;
  end;

  TOptionalParameters = record
    Symbol: string;
    LocalSymbol: string;
    DataID: Integer;
    Exchange: string;
    ExpirationsArray: TStringArray;
    StrikesArray: TDoubleArray;

    UnderlyingConId: Integer;
    UnderlyingSecurityType: string;
    UnderlyingSymbol: string;
    UnderlyingExchange: string;
    ESParameters: TArray<TESParameter>;

    SecurityType: TIABSecurityType;
    Currency: string;
    MaxWeeks: Integer;
    StrikePriceInterval: Integer;
    StdDeviations: Double;
    StrikeNum: Integer;
    procedure Clear;
    function IsEqual(aOptionalParameters: TOptionalParameters): Boolean;
   end;

   POption = ^TOption;
   TOption = record
     ContractId      : Integer;
     UnderlyingConId : Integer;
     LocalSymbol     : string;
     Symbol          : string;
     Currency        : string;
     Exchange        : string;
     Expiry          : TDateTime;
     Multiplier      : string;
     Strike          : Double;
     Delta           : Double;
     OptPrice        : Double;
     pvDividend      : Double;
     Gamma           : Double;
     Vega            : Double;
     Theta           : Double;
     undPrice        : Double;
     Right           : TIABRight;
     procedure Clear;
     procedure AssignFrom(aOrder: TIABOrder); overload;
     procedure AssignFrom(aInstrument: TIABInstrumentSpecItem); overload;
   end;

   TSokidGroup = record
     RecordId    : Integer;
     Name        : string;
     Description : string;
     procedure FromDB(aID: Integer);
     procedure SaveToDB;
     procedure Clear;
     class procedure DeleteFromDB(aID: Integer); static;
   end;

   TSokidGroupDetail = record
     class procedure DeleteFromDB(aContractId, aParentId: Integer); static;
     class procedure DeleteAll; static;
     class procedure SaveToDB(aContractId, aParentId, aSortIndex: Integer); static;
   end;

implementation

{ TOptionalParameters }

procedure TOptionalParameters.Clear;
begin
  SetLength(ESParameters, 0);
  DataID                 := 0;
  Exchange               := '';
  MaxWeeks               := 0;
//  Multiplier             := '';
  StdDeviations          := 0;
  StrikeNum              := 0;
  StrikePriceInterval    := 0;
  Symbol                 := '';
  LocalSymbol            := '';
  UnderlyingConId        := 0;
  UnderlyingSymbol       := '';
  UnderlyingExchange     := '';
  UnderlyingSecurityType := '';
  ExpirationsArray.Clear;
  StrikesArray.Clear;
end;

function TOptionalParameters.IsEqual(aOptionalParameters: TOptionalParameters): Boolean;
begin
  Result := (Self.Symbol = aOptionalParameters.Symbol) and
            (Self.DataID = aOptionalParameters.DataID) and
            (Self.Exchange = aOptionalParameters.Exchange) and
            (Self.UnderlyingConId = aOptionalParameters.UnderlyingConId) and
            (Self.Currency = aOptionalParameters.Currency) and
             Self.ExpirationsArray.Compare(aOptionalParameters.ExpirationsArray) and
             Self.StrikesArray.Compare(aOptionalParameters.StrikesArray);
end;

{ TOption }

procedure TOption.AssignFrom(aOrder: TIABOrder);
begin
  Self.Clear;
  Self.ContractId  := aOrder.ContractId;
  Self.Currency    := aOrder.Currency;
  Self.Exchange    := aOrder.Exchange;
  Self.Expiry      := Utils.GetExpiryDate(aOrder.Expiry);
  Self.Multiplier  := aOrder.Multiplier;
  Self.Strike      := aOrder.Strike;
  Self.Symbol      := aOrder.Symbol;
  Self.Right       := aOrder.Right;
  Self.LocalSymbol := aOrder.LocalSymbol;
end;

procedure TOption.AssignFrom(aInstrument: TIABInstrumentSpecItem);
begin
  Self.Clear;
  Self.ContractId  := aInstrument.ContractId;
  Self.Currency    := aInstrument.Currency;
  Self.Exchange    := aInstrument.Exchange;
  Self.Expiry      := Utils.GetExpiryDate(aInstrument.Expiry);
  Self.Multiplier  := aInstrument.Multiplier;
  Self.Strike      := aInstrument.Strike;
  Self.Symbol      := aInstrument.Symbol;
  Self.LocalSymbol := aInstrument.LocalSymbol;
  Self.Right       := aInstrument.Right;
end;
//
//procedure TOption.SetName;
//const
//  IABRightString: array[TIABRight] of string = ('N','P','C');
//  cName = '%d.%s.%s.%.0f';
//begin
//  if (Self.ContractId > 0) then
//    Self.Name := Format(cName, [Self.ContractId, Self.Expiry, IABRightString[Self.Right], Self.Strike])
//  else
//    Self.Name := Format(cName, [Self.UnderlyingConId, Self.Expiry, IABRightString[Self.Right], Self.Strike]);
//end;

procedure TOption.Clear;
begin
  ContractId := 0;
  Currency   := '';
  Exchange   := '';
  Expiry     := 0;
  Multiplier := '';

  Strike     := 0;
  Symbol     := '';
  undPrice   := 0;
  Delta      := 0;
  OptPrice   := 0;
  pvDividend := 0;
  Gamma      := 0;
  Vega       := 0;
  Theta      := 0;
end;

{ TSokidGroup }

procedure TSokidGroup.Clear;
begin
  Self.RecordId := 0;
  Self.Name := '';
  Self.Description := '';
end;

class procedure TSokidGroup.DeleteFromDB(aID: Integer);
resourcestring
  C_SQL_DELETE_TEXT = 'DELETE FROM SOKID_GROUP WHERE ID=:ID';
var
  Query: TFDQuery;
begin
  if (aID > 0) then
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.Transaction := Query.Connection.Transaction;
      Query.SQL.Text := C_SQL_DELETE_TEXT;
      Query.ParamByName('ID').AsInteger := aID;
      try
        Query.Prepare;
        Query.ExecSQL;
        Query.Transaction.CommitRetaining;
      except
        on E: Exception do
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TSokidGroup.DeleteFromDB', 'SearchTypes', E.Message + TDModUtils.GetQueryInfo(Query));
      end;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TSokidGroup.FromDB(aID: Integer);
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT * FROM SOKID_GROUP WHERE ID=:ID';
var
  Query: TFDQuery;
begin
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
        Self.RecordId    := aID;
        Self.Name        := Query.FieldByName('NAME').AsString;
        Self.Description := Query.FieldByName('DESCRIPTION').AsString;
      end;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TSokidGroup.SaveToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT COUNT(*) AS CNT FROM SOKID_GROUP WHERE ID=:RecordId';
  C_SQL_INSERT_TEXT = 'INSERT INTO SOKID_GROUP(ID, NAME, DESCRIPTION) ' + sLineBreak +
                                     ' VALUES(:ID,:NAME,:DESCRIPTION); ';
  C_SQL_UPDATE_TEXT = 'UPDATE SOKID_GROUP SET '   + sLineBreak +
                      'NAME=:NAME,  '             + sLineBreak +
                      'DESCRIPTION=:DESCRIPTION ' + sLineBreak +
                      'WHERE ID=:ID;';
var
  IsExists: Boolean;
  Query: TFDQuery;
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
        Query.ParamByName('RecordId').AsInteger := Self.RecordId;
        Query.Prepare;
        Query.Open;
        IsExists := Query.FieldByName('CNT').AsInteger > 0;
        Query.Close;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TSokidGroup.SaveToDB', 'SearchTypes', E.Message + TDModUtils.GetQueryInfo(Query));
          raise;
        end;
      end;
    end
    else
      Self.RecordId := DMod.GetNextValue('GEN_SOKID_GROUP_ID');

    if IsExists then
      Query.SQL.Text := C_SQL_UPDATE_TEXT
    else
      Query.SQL.Text := C_SQL_INSERT_TEXT;

    if Self.Name.IsEmpty then
      Self.Name := 'Group nr' + Self.RecordId.ToString;

    Query.ParamByName('ID').AsInteger          := Self.RecordId;
    Query.ParamByName('NAME').AsString         := Copy(Self.Name, 0, 200);
    Query.ParamByName('DESCRIPTION').AsString  := Copy(Self.Description, 0, 500);
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TSokidGroup.SaveToDB', 'SearchTypes', E.Message + TDModUtils.GetQueryInfo(Query));
    end;
  finally
    FreeAndNil(Query);
  end;
end;

{ TSokidGroupDetail }

class procedure TSokidGroupDetail.DeleteAll;
resourcestring
  C_SQL_DELETE_TEXT = 'DELETE FROM SOKID_GROUP_DETAIL';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    Query.SQL.Text := C_SQL_DELETE_TEXT;
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TSokidGroupDetail.DeleteAll', 'SearchTypes', E.Message + TDModUtils.GetQueryInfo(Query));
    end;
  finally
    FreeAndNil(Query);
  end;
end;

class procedure TSokidGroupDetail.DeleteFromDB(aContractId, aParentId: Integer);
resourcestring
  C_SQL_DELETE_TEXT = 'DELETE FROM SOKID_GROUP_DETAIL WHERE SOKID_IB=:SOKID_IB AND SOKID_GROUP=:SOKID_GROUP';
var
  Query: TFDQuery;
begin
  if (aContractId > 0) then
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.Transaction := Query.Connection.Transaction;
      Query.SQL.Text := C_SQL_DELETE_TEXT;
      Query.ParamByName('SOKID_IB').AsInteger := aContractId;
      Query.ParamByName('SOKID_GROUP').AsInteger := aParentId;
      try
        Query.Prepare;
        Query.ExecSQL;
        Query.Transaction.CommitRetaining;
      except
        on E: Exception do
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TSokidGroupDetail.DeleteFromDB', 'SearchTypes', E.Message + TDModUtils.GetQueryInfo(Query));
      end;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

class procedure TSokidGroupDetail.SaveToDB(aContractId, aParentId, aSortIndex: Integer);
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT DISTINCT(ID) AS ID FROM SOKID_GROUP_DETAIL WHERE SOKID_GROUP=:SOKID_GROUP AND SOKID_IB=:SOKID_IB';
  C_SQL_INSERT_TEXT = 'INSERT INTO SOKID_GROUP_DETAIL(ID, SOKID_GROUP, SOKID_IB, SORT_INDEX) ' + sLineBreak +
                                            ' VALUES(:ID,:SOKID_GROUP,:SOKID_IB,:SORT_INDEX); ';
  C_SQL_UPDATE_TEXT = 'UPDATE SOKID_GROUP_DETAIL '    + sLineBreak +
                      '   SET ID=:ID, '               + sLineBreak +
                      '       SORT_INDEX=:SORT_INDEX' + sLineBreak +
                      'WHERE SOKID_GROUP=:SOKID_GROUP AND SOKID_IB=:SOKID_IB';
var
  Query: TFDQuery;
  RecordId: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    RecordId := -1;

    Query.SQL.Text := C_SQL_EXISTS_TEXT;
    try
      Query.ParamByName('SOKID_IB').AsInteger    := aContractId;
      Query.ParamByName('SOKID_GROUP').AsInteger := aParentId;
      Query.Prepare;
      Query.Open;
      if not Query.IsEmpty then
        RecordId := Query.FieldByName('ID').AsInteger;
      Query.Close;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TSokidGroupDetail.SaveToDB', 'SearchTypes', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;

    if (RecordId > -1) then
      Query.SQL.Text := C_SQL_UPDATE_TEXT
    else
      Query.SQL.Text := C_SQL_INSERT_TEXT;

    if (RecordId <= 0) then
      RecordId := DMod.GetNextValue('GEN_SOKID_GROUP_DETAIL_ID');

    Query.ParamByName('ID').AsInteger          := RecordId;
    Query.ParamByName('SOKID_GROUP').AsInteger := aParentId;
    Query.ParamByName('SOKID_IB').AsInteger    := aContractId;
    Query.ParamByName('SORT_INDEX').AsInteger  := aSortIndex;
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TSokidGroupDetail.SaveToDB', 'SearchTypes', E.Message + TDModUtils.GetQueryInfo(Query));
    end;
  finally
    FreeAndNil(Query);
  end;
end;

{ TESParameter }

procedure TESParameter.Clear;
begin
  ExpirationsArray.Clear;
  StrikesArray.Clear;
end;

end.
