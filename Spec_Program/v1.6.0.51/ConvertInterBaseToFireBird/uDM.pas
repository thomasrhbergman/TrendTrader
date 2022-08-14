unit uDM;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Phys.FBDef, IBX.IBDatabase, FireDAC.Phys.IBBase,
  FireDAC.Phys.FB, Data.DB, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
  IBX.IBQuery, System.StrUtils, FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util,
  FireDAC.Comp.Script;

type
  TOnMessage = procedure(const AOnMessage: String) of object;

  TDM = class(TDataModule)
    FDConnection: TFDConnection;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    IBDatabase: TIBDatabase;
    IBTransaction: TIBTransaction;
  public
    procedure CopyData(const AIBServer, AIBPort, AIB, AIBUser, AIBPassword, AFBServer, AFBPort, AFB, AFBUser, AFBPassword: String; AOnMessage, AOnShowMessage: TOnMessage);
  end;

var
  DM: TDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TDataModule1 }

procedure TDM.CopyData(const AIBServer, AIBPort, AIB, AIBUser, AIBPassword, AFBServer, AFBPort, AFB, AFBUser, AFBPassword: String; AOnMessage, AOnShowMessage: TOnMessage);
var
  IBQuery: TIBQuery;
  FDQuery: TFDQuery;
  Tables,
  TablesFB,
  TablesNotExists,
  FieldsNotExists,
  FieldsNotExistsFB,
  Generators,
  Indices: TStringList;
  FieldsIB, FieldsFB: TStringList;
  I: Integer;
  J: Integer;
  Cnt: Integer;
  ID: Int64;
  SQLText1: String;
  SQLText2: String;
  SQLText: String;

  procedure CheckTables;
  var
    K: Integer;
    TablesCount: Integer;
  begin
    TablesCount := Tables.Count;

    for K := TablesCount - 1 downto 0 do
    begin
      if TablesFB.IndexOf(Tables[K]) = -1 then
      begin
        TablesNotExists.Add(Tables[K]);
      end;
    end;
  end;

  function GetFieldDDL(const AQuery: TIBQuery): String;
  begin
    Result := '"' + AQuery.FieldByName('RDB$FIELD_NAME').AsString.Trim + '"';

    if AQuery.FieldByName('RDB$COMPUTED_SOURCE').IsNull then
      case AQuery.FieldByName('RDB$FIELD_TYPE').AsInteger of
          7: begin
            case AQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger of
              0: Result := Result + ' SMALLINT';
              1: Result := Result + ' NUMERIC';
              2: Result := Result + ' DECIMAL';
            end;

            if AQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger > 0 then
              Result := Result + '(' + AQuery.FieldByName('RDB$FIELD_PRECISION').AsString + ', ' + IntToStr(Abs(AQuery.FieldByName('RDB$FIELD_SCALE').AsInteger)) + ')';
          end;

          8: begin
            case AQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger of
              0: Result := Result + ' INTEGER';
              1: Result := Result + ' NUMERIC';
              2: Result := Result + ' DECIMAL';
            end;

            if AQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger > 0 then
              Result := Result + '(' + AQuery.FieldByName('RDB$FIELD_PRECISION').AsString + ', ' + IntToStr(Abs(AQuery.FieldByName('RDB$FIELD_SCALE').AsInteger)) + ')';
          end;

         10: Result := Result + ' FLOAT';
         12: Result := Result + ' DATE';
         13: Result := Result + ' TIME';
         14: Result := Result + ' CHAR(' + AQuery.FieldByName('RDB$FIELD_LENGTH').AsString + ')';

         16: begin
            case AQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger of
              0: Result := Result+ ' BIGINT';
              1: Result := Result + ' NUMERIC';
              2: Result := Result + ' DECIMAL';
            end;

            if AQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger > 0 then
              Result := Result + '(' + AQuery.FieldByName('RDB$FIELD_PRECISION').AsString + ', ' + IntToStr(Abs(AQuery.FieldByName('RDB$FIELD_SCALE').AsInteger)) + ')';
         end;

         17: Result := Result+ ' BOOLEAN';
         27: Result := Result + ' DOUBLE PRECISION';
         35: Result := Result + ' TIMESTAMP';
         37: Result := Result + ' VARCHAR(' + AQuery.FieldByName('RDB$FIELD_LENGTH').AsString + ')';
        261: Result := Result + ' BLOB SUB_TYPE ' + AQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsString + ' SEGMENT SIZE ' + AQuery.FieldByName('RDB$SEGMENT_LENGTH').AsString;
      else
        Result := EmptyStr;
      end
    else
      Result := Result+ ' COMPUTED BY ' + AQuery.FieldByName('RDB$COMPUTED_SOURCE').AsString.Trim;

    if not Result.IsEmpty then
    begin
      if not AQuery.FieldByName('RDB$DEFAULT_SOURCE').IsNull then
        Result := Result + ' ' + AQuery.FieldByName('RDB$DEFAULT_SOURCE').AsString.Trim;

      if not AQuery.FieldByName('RDB$NULL_FLAG').IsNull then
        Result := Result + ' ' + ' NOT NULL';
    end;
  end;

  function GenerateTriggerType(AType: Integer): string;
  // multi action triggers should examine bits in value
  // described here https://firebird-support.yahoogroups.narkive.com/sb2xHDMO/values-of-rdb-triggers-rdb-trigger-type
  begin
    case AType of
      1: Result := 'BEFORE INSERT';
      2: Result := 'AFTER INSERT';
      3: Result := 'BEFORE UPDATE';
      4: Result := 'AFTER UPDATE';
      5: Result := 'BEFORE DELETE';
      6: Result := 'AFTER DELETE';
    end;
  end;

  function GetFieldsForIndex(AIndexName: string): string;
  var QTemp: TIBQuery;
  begin
    Result := '';
    QTemp := TIBQuery.Create(nil);
    try
      QTemp.Database := IBDatabase;
      QTemp.UniDirectional := True;
      QTemp.Transaction := IBTransaction;
      QTemp.SQL.Text := 'SELECT * FROM RDB$INDEX_SEGMENTS WHERE RDB$INDEX_NAME = :NAME ORDER BY RDB$FIELD_POSITION';
      QTemp.ParamByName('NAME').Value := AIndexName;
      QTemp.Open;
      while not QTemp.Eof do
      begin
        if not Result.IsEmpty then
          Result := Result + ',';
        Result := Result + '"' + QTemp.FieldByName('RDB$FIELD_NAME').AsString + '"';
        QTemp.Next;
      end;
    finally
      FreeAndNil(QTemp);
    end;
  end;

begin
  IBDatabase.Connected := False;
  IBDatabase.DatabaseName := AIBServer + '/' + AIBPort + ':' + AIB;
  IBDatabase.Params.Clear;
  IBDatabase.Params.Add('user_name=' + AIBUser);
  IBDatabase.Params.Add('password=' + AIBPassword);
  // IBDatabase.Params.Add('lc_ctype=WIN1252');
  IBDatabase.Connected := True;

  FDConnection.Close;
  FDConnection.Params.Clear;
  FDConnection.Params.Add('DriverID=FB');
  FDConnection.Params.Add('Server=' + AFBServer);
  // FDConnection.Params.Add('Database=' + edFB.Text);
  FDConnection.Params.Add('User_Name=' + AFBUser);
  FDConnection.Params.Add('Port=' + AFBPort);
  FDConnection.Params.Add('Password=' + AFBPassword);
  FDConnection.Params.Database := AFB;
  FDConnection.Params.Add('OpenMode=OpenOrCreate');
  // FDConnection.Params.Add('CharacterSet=WIN1252');
  FDConnection.Open;

  IBQuery := TIBQuery.Create(nil);
  FDQuery := TFDQuery.Create(nil);
  Tables := TStringList.Create;
  TablesFB := TStringList.Create;
  TablesNotExists := TStringList.Create;
  FieldsNotExists := TStringList.Create;
  Generators := TStringList.Create;
  Indices := TStringList.Create;
  FieldsIB := TStringList.Create;
  FieldsFB := TStringList.Create;
  FieldsNotExistsFB := TStringList.Create;

  try
    IBQuery.Database := IBDatabase;
    IBQuery.UniDirectional := True;
    IBQuery.Transaction := IBTransaction;
    FDQuery.Connection := FDConnection;

    FDQuery.SQL.Text :=
      'SELECT ix.rdb$index_name FROM rdb$indices ix ' +
      'LEFT JOIN rdb$relation_constraints rc on rc.rdb$index_name = ix.rdb$index_name ' +
      'WHERE (rdb$system_flag is null or rdb$system_flag = 0) ' +
      'AND rc.rdb$constraint_type not IN (''PRIMARY KEY'', ''UNIQUE'') ' +
      'order by ix.rdb$foreign_key nulls first';

    FDQuery.Open;

    while not FDQuery.Eof do
    begin
      Indices.Add(FDQuery.FieldByName('rdb$index_name').AsString);
      FDQuery.Next;
    end;

    for I := 0 to Indices.Count - 1 do
    begin
      FDQuery.Close;
      FDQuery.SQL.Text :=
        'ALTER INDEX ' + Indices[I] + ' INACTIVE';

      FDQuery.ExecSQL;
    end;

    IBQuery.SQL.Text :=
      'select rdb$relation_name ' +
      'from rdb$relations ' +
      'where rdb$view_blr is null ' +
      'and (rdb$system_flag is null or rdb$system_flag = 0)';

    IBQuery.Open;

    while not IBQuery.Eof do
    begin
      Tables.Add(IBQuery.FieldByName('RDB$RELATION_NAME').AsString.Trim);
      IBQuery.Next;
    end;

    FDQuery.Close;

    FDQuery.SQL.Text :=
      'select rdb$relation_name ' +
      'from rdb$relations ' +
      'where rdb$view_blr is null ' +
      'and (rdb$system_flag is null or rdb$system_flag = 0)';

    FDQuery.Open;

    while not FDQuery.Eof do
    begin
      TablesFB.Add(FDQuery.FieldByName('RDB$RELATION_NAME').AsString.Trim);
      FDQuery.Next;
    end;

    FDQuery.Close;
    CheckTables;

    if TablesNotExists.Count > 0 then
    begin
      AOnShowMessage('These tables are missing in FB3 database: ' + sLineBreak + sLineBreak +TablesNotExists.Text +
                 sLineBreak + sLineBreak + 'It will be created, but linked objects will need to be created manually.');

      for I := TablesNotExists.Count - 1 downto 0 do
      begin
        IBQuery.Close;
        IBQuery.SQL.Text :=
          'select R.RDB$FIELD_NAME, F.RDB$FIELD_LENGTH, F.RDB$FIELD_TYPE, F.RDB$FIELD_SCALE, F.RDB$FIELD_SUB_TYPE, ' +
          '       F.RDB$FIELD_PRECISION, CAST(R.RDB$DEFAULT_SOURCE AS VARCHAR(16383)) AS RDB$DEFAULT_SOURCE, CAST(F.RDB$COMPUTED_SOURCE AS VARCHAR(16383)) AS RDB$COMPUTED_SOURCE, R.RDB$NULL_FLAG, F.RDB$SEGMENT_LENGTH ' +
          'from RDB$FIELDS F, RDB$RELATION_FIELDS R ' +
          'where F.RDB$FIELD_NAME = R.RDB$FIELD_SOURCE and R.RDB$SYSTEM_FLAG = 0 and R.RDB$RELATION_NAME = ' + TablesNotExists[I].QuotedString + ' ' +
          'order by R.RDB$FIELD_POSITION';

        IBQuery.Open;
        FieldsNotExists.Clear;

        while not IBQuery.Eof do
        begin
          FieldsNotExists.Add(GetFieldDDL(IBQuery));
          IBQuery.Next;
        end;

        IBQuery.Close;

        FDQuery.Close;
        FDQuery.SQL.Clear;
        FDQuery.SQL.Add('CREATE TABLE ' + TablesNotExists[I] + ' (');

        for J := 0 to FieldsNotExists.Count - 1 do
          if J < FieldsNotExists.Count - 1 then
            FDQuery.SQL.Add(FieldsNotExists[J] + ',')
          else
            FDQuery.SQL.Add(FieldsNotExists[J] + ')');

        FDQuery.ExecSQL;
      end;
    end;

    for I := 0 to Tables.Count - 1 do
    begin
      IBQuery.Close;
      IBQuery.SQL.Text :=
        'select R.RDB$FIELD_NAME ' +
        'from RDB$FIELDS F, RDB$RELATION_FIELDS R ' +
        'where F.RDB$FIELD_NAME = R.RDB$FIELD_SOURCE and R.RDB$SYSTEM_FLAG = 0 and R.RDB$RELATION_NAME = ' + Tables[I].QuotedString + ' ' +
        'order by R.RDB$FIELD_POSITION';

      IBQuery.Open;

      FieldsIB.Clear;

      while not IBQuery.Eof do
      begin
        FieldsIB.Add(IBQuery.FieldByName('RDB$FIELD_NAME').AsString.Trim);
        IBQuery.Next;
      end;

      FDQuery.Close;
      FDQuery.SQL.Text :=
        'select R.RDB$FIELD_NAME ' +
        'from RDB$FIELDS F, RDB$RELATION_FIELDS R ' +
        'where F.RDB$FIELD_NAME = R.RDB$FIELD_SOURCE and R.RDB$SYSTEM_FLAG = 0 and R.RDB$RELATION_NAME = ' + Tables[I].QuotedString + ' ' +
        'order by R.RDB$FIELD_POSITION';

      FDQuery.Open;

      FieldsFB.Clear;

      while not FDQuery.Eof do
      begin
        FieldsFB.Add(FDQuery.FieldByName('RDB$FIELD_NAME').AsString.Trim);
        FDQuery.Next;
      end;

      FieldsNotExistsFB.Clear;

      for J := 0 to FieldsIB.Count - 1 do
        if FieldsFB.IndexOf(FieldsIB[J]) = -1 then
          FieldsNotExistsFB.Add(FieldsIB[J]);

      IBQuery.Close;
      IBQuery.SQL.Text :=
        'select R.RDB$FIELD_NAME, F.RDB$FIELD_LENGTH, F.RDB$FIELD_TYPE, F.RDB$FIELD_SCALE, F.RDB$FIELD_SUB_TYPE, ' +
        '       F.RDB$FIELD_PRECISION, CAST(R.RDB$DEFAULT_SOURCE AS VARCHAR(16383)) AS RDB$DEFAULT_SOURCE, CAST(F.RDB$COMPUTED_SOURCE AS VARCHAR(16383)) AS RDB$COMPUTED_SOURCE, R.RDB$NULL_FLAG, F.RDB$SEGMENT_LENGTH ' +
        'from RDB$FIELDS F, RDB$RELATION_FIELDS R ' +
        'where F.RDB$FIELD_NAME = R.RDB$FIELD_SOURCE and R.RDB$SYSTEM_FLAG = 0 and R.RDB$RELATION_NAME = ' + Tables[I].QuotedString + ' ' +
        'order by R.RDB$FIELD_POSITION';

      IBQuery.Open;

      FieldsNotExists.Clear;

      while not IBQuery.Eof do
      begin
        if FieldsNotExistsFB.IndexOf(IBQuery.FieldByName('RDB$FIELD_NAME').AsString.Trim) <> -1 then
          FieldsNotExists.Add(GetFieldDDL(IBQuery));

        IBQuery.Next;
      end;

      IBQuery.Close;

      FDQuery.Close;

      for J := 0 to FieldsNotExists.Count - 1 do
      begin
        FDQuery.SQL.Text := 'ALTER TABLE ' + Tables[I] + ' ADD ' + FieldsNotExists[J];
        FDQuery.ExecSQL;
      end;

      FDQuery.Close;
      FDQuery.SQL.Text :=
        'DELETE FROM ' + Tables[I];

      FDQuery.ExecSQL;

      IBQuery.Close;
      IBQuery.SQL.Text :=
        'SELECT COUNT(*) FROM ' + Tables[I];

      IBQuery.Open;
      Cnt := IBQuery.Fields[0].AsInteger;
      AOnMessage('Tables ' + (I + 1).ToString + ' / ' + Tables.Count.ToString + ': ' + Tables[I] + ' records ' + '0 / ' + Cnt.ToString);

      IBQuery.Close;
      IBQuery.SQL.Text :=
        'SELECT * FROM ' + Tables[I];

      IBQuery.Open;

      while not IBQuery.Eof do
      begin
        SQLText1 := EmptyStr;
        SQLText2 := EmptyStr;

        for J := 0 to IBQuery.Fields.Count - 1 do
          if not IBQuery.Fields[J].IsNull then
          begin
            SQLText1 := SQLText1 + '"' + IBQuery.Fields[J].FieldName + '", ';
            SQLText2 := SQLText2 + ':' + IBQuery.Fields[J].FieldName + ', ';
          end;

        FDQuery.SQL.Text :=
          'INSERT INTO ' + Tables[I] + '(' + LeftStr(SQLText1, Length(SQLText1) - 2) +
          ') VALUES (' + LeftStr(SQLText2, Length(SQLText2) - 2) + ')';

        for J := 0 to IBQuery.Fields.Count - 1 do
          if not IBQuery.Fields[J].IsNull then
          begin
            FDQuery.ParamByName(IBQuery.Fields[J].FieldName).DataType := IBQuery.Fields[J].DataType;
            FDQuery.ParamByName(IBQuery.Fields[J].FieldName).Value := IBQuery.Fields[J].Value;
          end;

        FDQuery.ExecSQL;
        IBQuery.Next;

        if IBQuery.RecNo mod 1000 = 0 then
          AOnMessage('Tables ' + (I + 1).ToString + ' / ' + Tables.Count.ToString + ': ' + Tables[I] + ' records ' + IBQuery.RecNo.ToString + ' / ' + Cnt.ToString);
      end;

      AOnMessage('Tables ' + (I + 1).ToString + ' / ' + Tables.Count.ToString + ': ' + Tables[I] + ' records ' + IBQuery.RecNo.ToString + ' / ' + Cnt.ToString);
    end;

    for I := 0 to Indices.Count - 1 do
    begin
      FDQuery.Close;
      FDQuery.SQL.Text :=
        'ALTER INDEX ' + Indices[I] + ' ACTIVE';

      FDQuery.ExecSQL;
    end;

    // primary keys
    // it works only for one field per key !!
    IBQuery.Close;

    IBQuery.SQL.Text :=
      'SELECT '+ #13#10 +
      '    IX.RDB$INDEX_NAME AS INDEX_NAME, '+ #13#10 +
      '    SG.RDB$FIELD_NAME AS FIELD_NAME, '+ #13#10 +
      '    RC.RDB$RELATION_NAME AS TABLE_NAME, '+ #13#10 +
      '    RC.RDB$CONSTRAINT_NAME AS KEY_NAME '+ #13#10 +
      'FROM '+ #13#10 +
      '    RDB$INDICES IX '+ #13#10 +
      '    LEFT JOIN RDB$INDEX_SEGMENTS SG ON IX.RDB$INDEX_NAME = SG.RDB$INDEX_NAME '+ #13#10 +
      '    LEFT JOIN RDB$RELATION_CONSTRAINTS RC ON RC.RDB$INDEX_NAME = IX.RDB$INDEX_NAME '+ #13#10 +
      'WHERE '+ #13#10 +
      '    RC.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' AND '+ #13#10 +
      '    SG.RDB$FIELD_NAME NOT LIKE ''%$%''';
    IBQuery.Open;
    while not IBQuery.Eof do
    begin
      FDQuery.Close;
      FDQuery.SQL.Text :=
        'SELECT 1 '+ #13#10 +
        'FROM '+ #13#10 +
        '    RDB$INDICES IX '+ #13#10 +
        '    LEFT JOIN RDB$INDEX_SEGMENTS SG ON IX.RDB$INDEX_NAME = SG.RDB$INDEX_NAME '+ #13#10 +
        '    LEFT JOIN RDB$RELATION_CONSTRAINTS RC ON RC.RDB$INDEX_NAME = IX.RDB$INDEX_NAME '+ #13#10 +
        'WHERE '+ #13#10 +
        '    RC.RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' AND '+ #13#10 +
        '    SG.RDB$FIELD_NAME NOT LIKE ''%$%'' AND '+ #13#10 +
        '    RC.RDB$RELATION_NAME = :TABLE_NAME AND '+ #13#10 +
        '    RC.RDB$CONSTRAINT_NAME = :KEY_NAME';
      FDQuery.ParamByName('TABLE_NAME').Value := IBQuery.FieldByName('TABLE_NAME').AsString;
      FDQuery.ParamByName('KEY_NAME').Value := IBQuery.FieldByName('KEY_NAME').AsString;
      FDQuery.Open;
      if FDQuery.IsEmpty then
      begin
        SQLText :=
          'ALTER TABLE '+ IBQuery.FieldByName('TABLE_NAME').AsString + #13#10 +
          'ADD CONSTRAINT '+ IBQuery.FieldByName('KEY_NAME').AsString + #13#10 +
          'PRIMARY KEY ('+ IBQuery.FieldByName('FIELD_NAME').AsString +')';
        FDQuery.Close;
        FDQuery.SQL.Text := SQLText;
        FDQuery.ExecSQL;
      end;
      IBQuery.Next;
    end;

    // unique keys
    // it works only for one field per key !!
    IBQuery.Close;

    IBQuery.SQL.Text :=
      'SELECT '+ #13#10 +
      '    IX.RDB$INDEX_NAME AS INDEX_NAME, '+ #13#10 +
      '    SG.RDB$FIELD_NAME AS FIELD_NAME, '+ #13#10 +
      '    RC.RDB$RELATION_NAME AS TABLE_NAME, '+ #13#10 +
      '    RC.RDB$CONSTRAINT_NAME AS KEY_NAME '+ #13#10 +
      'FROM '+ #13#10 +
      '    RDB$INDICES IX '+ #13#10 +
      '    LEFT JOIN RDB$INDEX_SEGMENTS SG ON IX.RDB$INDEX_NAME = SG.RDB$INDEX_NAME '+ #13#10 +
      '    LEFT JOIN RDB$RELATION_CONSTRAINTS RC ON RC.RDB$INDEX_NAME = IX.RDB$INDEX_NAME '+ #13#10 +
      'WHERE '+ #13#10 +
      '    RC.RDB$CONSTRAINT_TYPE = ''UNIQUE'' AND '+ #13#10 +
      '    SG.RDB$FIELD_NAME NOT LIKE ''%$%''';
    IBQuery.Open;
    while not IBQuery.Eof do
    begin
      FDQuery.Close;
      FDQuery.SQL.Text :=
        'SELECT 1 '+ #13#10 +
        'FROM '+ #13#10 +
        '    RDB$INDICES IX '+ #13#10 +
        '    LEFT JOIN RDB$INDEX_SEGMENTS SG ON IX.RDB$INDEX_NAME = SG.RDB$INDEX_NAME '+ #13#10 +
        '    LEFT JOIN RDB$RELATION_CONSTRAINTS RC ON RC.RDB$INDEX_NAME = IX.RDB$INDEX_NAME '+ #13#10 +
        'WHERE '+ #13#10 +
        '    RC.RDB$CONSTRAINT_TYPE = ''UNIQUE'' AND '+ #13#10 +
        '    SG.RDB$FIELD_NAME NOT LIKE ''%$%'' AND '+ #13#10 +
        '    RC.RDB$RELATION_NAME = :TABLE_NAME AND '+ #13#10 +
        '    SG.RDB$FIELD_NAME = :FIELD_NAME';
      FDQuery.ParamByName('TABLE_NAME').Value := IBQuery.FieldByName('TABLE_NAME').AsString;
      FDQuery.ParamByName('FIELD_NAME').Value := IBQuery.FieldByName('FIELD_NAME').AsString;
      FDQuery.Open;
      if FDQuery.IsEmpty then
      begin
        SQLText :=
          'ALTER TABLE '+ IBQuery.FieldByName('TABLE_NAME').AsString +
          ' ADD UNIQUE ('+ IBQuery.FieldByName('FIELD_NAME').AsString +')';
        FDQuery.Close;
        FDQuery.SQL.Text := SQLText;
        FDQuery.ExecSQL;
      end;
      IBQuery.Next;
    end;

    // foreign keys
    IBQuery.Close;

    IBQuery.SQL.Text :=
      'SELECT '+ #13#10 +
      '    SG.RDB$FIELD_NAME AS FIELD_NAME, '+ #13#10 +
      '    RC.RDB$RELATION_NAME AS TABLE_NAME, '+ #13#10 +
      '    RT.RDB$RELATION_NAME AS REFERENCE_TABLE_NAME, '+ #13#10 +
      '    RF.REFERENCE_FIELD_NAME AS REFERENCE_FIELD_NAME, '+ #13#10 +
      '    RC.RDB$CONSTRAINT_NAME AS KEY_NAME, '+ #13#10 +
      '    REFC.RDB$UPDATE_RULE AS ON_UPDATE, '+ #13#10 +
      '    REFC.RDB$DELETE_RULE AS ON_DELETE '+ #13#10 +
      'FROM '+ #13#10 +
      '    RDB$INDICES IX '+ #13#10 +
      '    LEFT JOIN RDB$INDEX_SEGMENTS SG ON IX.RDB$INDEX_NAME = SG.RDB$INDEX_NAME '+ #13#10 +
      '    LEFT JOIN RDB$RELATION_CONSTRAINTS RC ON RC.RDB$INDEX_NAME = IX.RDB$INDEX_NAME '+ #13#10 +
      '    LEFT JOIN RDB$INDICES RT ON RT.RDB$INDEX_NAME = IX.RDB$FOREIGN_KEY '+ #13#10 +
      '    LEFT JOIN RDB$REF_CONSTRAINTS REFC ON RC.RDB$CONSTRAINT_NAME = REFC.RDB$CONSTRAINT_NAME '+ #13#10 +
      '    LEFT JOIN ( '+ #13#10 +
      '        SELECT DISTINCT '+ #13#10 +
      '            D1.RDB$DEPENDED_ON_NAME AS TABLE_NAME, '+ #13#10 +
      '            D2.RDB$DEPENDED_ON_NAME AS REFERENCE_TABLE_NAME, '+ #13#10 +
      '            D2.RDB$FIELD_NAME AS REFERENCE_FIELD_NAME '+ #13#10 +
      '        FROM RDB$DEPENDENCIES D1 '+ #13#10 +
      '        INNER JOIN RDB$DEPENDENCIES D2 ON D1.RDB$DEPENDENT_NAME = D2.RDB$DEPENDENT_NAME '+ #13#10 +
      '          AND D1.RDB$DEPENDED_ON_NAME <> D2.RDB$DEPENDED_ON_NAME '+ #13#10 +
      '    ) RF ON RF.TABLE_NAME = RC.RDB$RELATION_NAME AND RF.REFERENCE_TABLE_NAME = RT.RDB$RELATION_NAME '+ #13#10 +
      'WHERE '+ #13#10 +
      '    RC.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' AND '+ #13#10 +
      '    SG.RDB$FIELD_NAME NOT LIKE ''%$%''';
    IBQuery.Open;
    while not IBQuery.Eof do
    begin
      FDQuery.Close;
      FDQuery.SQL.Text :=
        'SELECT 1 '+ #13#10 +
        'FROM '+ #13#10 +
        '    RDB$INDICES IX '+ #13#10 +
        '    LEFT JOIN RDB$INDEX_SEGMENTS SG ON IX.RDB$INDEX_NAME = SG.RDB$INDEX_NAME '+ #13#10 +
        '    LEFT JOIN RDB$RELATION_CONSTRAINTS RC ON RC.RDB$INDEX_NAME = IX.RDB$INDEX_NAME '+ #13#10 +
        '    LEFT JOIN RDB$INDICES RT ON RT.RDB$INDEX_NAME = IX.RDB$FOREIGN_KEY '+ #13#10 +
        'WHERE '+ #13#10 +
        '    RC.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' AND '+ #13#10 +
        '    SG.RDB$FIELD_NAME NOT LIKE ''%$%'' AND '+ #13#10 +
        '    RC.RDB$RELATION_NAME = :TABLE_NAME AND '+ #13#10 +
        '    RC.RDB$CONSTRAINT_NAME = :KEY_NAME';
      FDQuery.ParamByName('TABLE_NAME').Value := IBQuery.FieldByName('TABLE_NAME').AsString;
      FDQuery.ParamByName('KEY_NAME').Value := IBQuery.FieldByName('KEY_NAME').AsString;
      FDQuery.Open;
      if FDQuery.IsEmpty then
      begin
        SQLText :=
          'ALTER TABLE '+ IBQuery.FieldByName('TABLE_NAME').AsString + #13#10 +
          'ADD CONSTRAINT '+ IBQuery.FieldByName('KEY_NAME').AsString + #13#10 +
          'FOREIGN KEY ('+ IBQuery.FieldByName('FIELD_NAME').AsString +')' + #13#10 +
          'REFERENCES '+ IBQuery.FieldByName('REFERENCE_TABLE_NAME').AsString + ' ('+ IBQuery.FieldByName('REFERENCE_FIELD_NAME').AsString + ')' ;
        if not IBQuery.FieldByName('ON_UPDATE').AsString.IsEmpty and not SameText(Trim(IBQuery.FieldByName('ON_UPDATE').AsString), 'RESTRICT') then
          SQLText := SQLText + ' ON UPDATE ' + IBQuery.FieldByName('ON_UPDATE').AsString;
        if not IBQuery.FieldByName('ON_DELETE').AsString.IsEmpty and not SameText(Trim(IBQuery.FieldByName('ON_DELETE').AsString), 'RESTRICT') then
          SQLText := SQLText + ' ON DELETE ' + IBQuery.FieldByName('ON_DELETE').AsString;
        FDQuery.Close;
        FDQuery.SQL.Text := SQLText;
        FDQuery.ExecSQL;
      end;
      IBQuery.Next;
    end;

    // indices
    IBQuery.Close;

    IBQuery.SQL.Text :=
      'SELECT * FROM RDB$INDICES WHERE RDB$INDEX_NAME NOT LIKE ''%$%'' ORDER BY RDB$INDEX_NAME';
    IBQuery.Open;
    while not IBQuery.Eof do
    begin
      FDQuery.Close;
      FDQuery.SQL.Text := 'SELECT * FROM RDB$INDICES WHERE RDB$INDEX_NAME = :NAME AND RDB$RELATION_NAME = :TABLENAME';
      FDQuery.ParamByName('NAME').Value := IBQuery.FieldByName('RDB$INDEX_NAME').AsString;
      FDQuery.ParamByName('TABLENAME').Value := IBQuery.FieldByName('RDB$RELATION_NAME').AsString;
      FDQuery.Open;
      if FDQuery.IsEmpty then
      begin
        SQLText := 'CREATE ';
        if IBQuery.FieldByName('RDB$UNIQUE_FLAG').AsInteger <> 0 then
          SQLText := SQLText + 'UNIQUE ';
        if IBQuery.FieldByName('RDB$INDEX_TYPE').AsInteger = 1 then
          SQLText := SQLText + 'DESC '
        else
          SQLText := SQLText + 'ASC ';
        SQLText := SQLText + 'INDEX '+ IBQuery.FieldByName('RDB$INDEX_NAME').AsString +' ON '+ IBQuery.FieldByName('RDB$RELATION_NAME').AsString + ' ';
        SQLText := SQLText + '(' + GetFieldsForIndex(IBQuery.FieldByName('RDB$INDEX_NAME').AsString) + ')';
        FDQuery.Close;
        FDQuery.SQL.Text := SQLText;
        FDQuery.ExecSQL;
      end;
      IBQuery.Next;
    end;

    // generators

    IBQuery.Close;

    IBQuery.SQL.Text :=
      'SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS WHERE RDB$GENERATOR_NAME NOT LIKE ''%$%'' ORDER BY RDB$GENERATOR_NAME';

    IBQuery.Open;

    while not IBQuery.Eof do
    begin
      FDQuery.Close;
      FDQuery.SQL.Text :=
        'SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS WHERE RDB$GENERATOR_NAME = :NAME';
      FDQuery.ParamByName('NAME').Value := IBQuery.FieldByName('RDB$GENERATOR_NAME').AsString.Trim;
      FDQuery.Open();
      if FDQuery.RecordCount = 0 then
      begin
        FDQuery.Close;
        FDQuery.SQL.Text := 'CREATE GENERATOR '+ IBQuery.FieldByName('RDB$GENERATOR_NAME').AsString.Trim +';';
        FDQuery.ExecSQL;
      end;

      Generators.Add(IBQuery.FieldByName('RDB$GENERATOR_NAME').AsString.Trim);
      IBQuery.Next;
    end;

    for I := 0 to Generators.Count - 1 do
    begin
      FDQuery.Close;
      FDQuery.SQL.Text :=
        'SELECT GEN_ID(' + Generators[I] + ', 0) FROM RDB$DATABASE';

      FDQuery.Open;
      ID := FDQuery.Fields[0].Value;

      FDQuery.Close;
      FDQuery.SQL.Text :=
        'SELECT GEN_ID(' + Generators[I] + ', -' + ID.ToString + ') FROM RDB$DATABASE';

      FDQuery.Open;
      ID := FDQuery.Fields[0].Value;

      IBQuery.Close;
      IBQuery.SQL.Text :=
        'SELECT GEN_ID(' + Generators[I] + ', 0) FROM RDB$DATABASE';

      IBQuery.Open;
      ID := IBQuery.Fields[0].Value;

      FDQuery.Close;
      FDQuery.SQL.Text :=
        'SELECT GEN_ID(' + Generators[I] + ', ' + ID.ToString + ') FROM RDB$DATABASE';

      FDQuery.Open;
    end;

    // triggers
    IBQuery.Close;

    IBQuery.SQL.Text :=
      'SELECT * FROM RDB$TRIGGERS WHERE RDB$TRIGGER_NAME NOT LIKE ''%$%'' ORDER BY RDB$TRIGGER_NAME';
    IBQuery.Open;
    while not IBQuery.Eof do
    begin
      if IBQuery.FieldByName('RDB$TRIGGER_SOURCE').AsString.IsEmpty then
      begin
        IBQuery.Next;
        continue;
      end;
      FDQuery.Close;
      FDQuery.SQL.Text := 'SELECT * FROM RDB$TRIGGERS WHERE RDB$TRIGGER_NAME = :NAME AND RDB$RELATION_NAME = :TABLENAME';
      FDQuery.ParamByName('NAME').Value := IBQuery.FieldByName('RDB$TRIGGER_NAME').AsString;
      FDQuery.ParamByName('TABLENAME').Value := IBQuery.FieldByName('RDB$RELATION_NAME').AsString;
      FDQuery.Open;
      if FDQuery.IsEmpty then
      begin
        SQLText :=
          'CREATE TRIGGER '+ IBQuery.FieldByName('RDB$TRIGGER_NAME').AsString +' FOR '+ IBQuery.FieldByName('RDB$RELATION_NAME').AsString + #13#10;
        if IBQuery.FieldByName('RDB$TRIGGER_INACTIVE').AsInteger = 0 then
          SQLText := SQLText + 'ACTIVE' + #13#10
        else
          SQLText := SQLText + 'INACTIVE' + #13#10;
        SQLText := SQLText + GenerateTriggerType(IBQuery.FieldByName('RDB$TRIGGER_TYPE').AsInteger) + #13#10;
        SQLText := SQLText + 'POSITION '+ IntToStr(IBQuery.FieldByName('RDB$TRIGGER_SEQUENCE').AsInteger) + #13#10;
        SQLText := SQLText + IBQuery.FieldByName('RDB$TRIGGER_SOURCE').AsString;

        FDQuery.Close;
        FDQuery.SQL.Text := SQLText;
        FDQuery.ExecSQL;
      end;

      IBQuery.Next;
    end;

  finally
    IBQuery.Free;
    FDQuery.Free;
    Tables.Free;
    TablesNotExists.Free;
    FieldsNotExists.Free;
    TablesFB.Free;
    Generators.Free;
    FieldsIB.Free;
    FieldsFB.Free;
    FieldsNotExistsFB.Free;
  end;
end;

end.
