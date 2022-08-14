unit DaModule;

interface

{$REGION 'Region uses'}
uses
  System.SysUtils, System.Classes, Data.DB, IBX.IBCustomDataSet, IBX.IBTable, IBX.IB, IBX.IBQuery, IBX.IBDatabase,
  IBX.IBStoredProc, Vcl.Controls, Winapi.Windows, Vcl.Dialogs, IBX.IBSQL, Global.Types, IBX.IBScript,
  IBX.IBServices, DebugWriter, HtmlLib, System.IOUtils, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  System.ImageList, Vcl.ImgList, Vcl.Forms, Utils, IABSocketAPI_const, Datasnap.DBClient, MessageDialog,
  System.Math, DaModule.Constants, DatabaseProperties, DaModule.Resources, SplashScreen, DaModule.Utils,
  System.Variants, Common.Types, Publishers, System.Threading, System.Notification,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Phys.IBBase,
  FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util, FireDAC.Comp.Script,
  FireDAC.Phys.IBWrapper;
{$ENDREGION}

type
  TRelationType = (rtTable, rtProcedure, rtUdf, rtDomain, rtForeignKey);

  TDMod = class(TDataModule)
    dsAccounts: TDataSource;
    dsAktieKurs: TDataSource;
    dsAutoTrades: TDataSource;
    dsInstruments: TDataSource;
    dsOrderGr: TDataSource;
    dsQualifiers: TDataSource;
    NotificationCenter: TNotificationCenter;
    ConnectionStock: TFDConnection;
    ConnectionFeed: TFDConnection;
    TransactionStock: TFDTransaction;
    TransactionFeed: TFDTransaction;
    FBStoredProc: TFDStoredProc;
    FBQuery: TFDQuery;
    fbqOrderGr: TFDQuery;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FBScript: TFDScript;
    fbqAutoTrades: TFDQuery;
    fbqQualifiers: TFDQuery;
    fbtAccounts: TFDTable;
    fbtAccountsACCOUNT_ID: TIntegerField;
    fbtAccountsUSER_ID: TIntegerField;
    fbtAccountsBROKER_ID: TIntegerField;
    fbtAccountsACC_USERNAME: TStringField;
    fbtAccountsACC_PWORD: TStringField;
    fbtAccountsACC_PARAMS: TStringField;
    fbtAccountsQUALIFIERID: TIntegerField;
    fbtAccountsAUTOTRADESID: TIntegerField;
    fbtInstruments: TFDTable;
    fbtInstrumentsCONID: TIntegerField;
    fbtInstrumentsSYMBOL: TStringField;
    fbtInstrumentsOMXS30_W: TSingleField;
    fbtInstrumentsCONTRACTYPE: TStringField;
    fbtInstrumentsCOUNTRY_REGION: TStringField;
    fbtInstrumentsCURRENCY: TStringField;
    fbtInstrumentsASSETID: TStringField;
    fbtInstrumentsNOSHARES: TStringField;
    fbtInstrumentsISIN: TStringField;
    fbtInstrumentsSTOCKTYPE: TStringField;
    fbtInstrumentsSECTOR: TStringField;
    fbtInstrumentsGROUP: TStringField;
    fbtInstrumentsFUTURESTYPE: TStringField;
    fbtInstrumentsLASTTRADINGDATE: TDateField;
    fbtInstrumentsEXPIRATIONDATE: TDateField;
    fbtInstrumentsCONTRACTMONTH: TDateField;
    fbtInstrumentsMULTIPLIER: TIntegerField;
    fbtInstrumentsDECIMALS: TSmallintField;
    fbtInstrumentsPRIMARYINDEX: TStringField;
    fbtInstrumentsMARKET_RULE_IDS: TStringField;
    fbtInstrumentsMARKET_LIST: TStringField;
    fbtInstrumentsDESCRIPTION: TStringField;
    fbtInstrumentsEXCHANGE: TStringField;
    fbtInstrumentsNAME: TStringField;
    fbtInstrumentsBROKER: TIntegerField;
    fbtInstrumentsLAST_PRICE: TSingleField;
    fbtInstrumentsPRIMARY_EXCHANGE: TStringField;
    fbtInstrumentsISOLATE: TIntegerField;
    fbtInstrumentsLOCAL_SYMBOL: TStringField;
    fbtInstrumentsMINIMUM_TICK: TSingleField;
    fbtInstrumentsERROR_CODE: TIntegerField;
    fbtInstrumentsCATEGORY: TStringField;
    fbtInstrumentsSUBCATEGORY: TStringField;
    fbtInstrumentsUNDERLYING_CONID: TIntegerField;
    fbtInstrumentsINDUSTRY: TStringField;
    fbtInstrumentsSTRIKE: TSingleField;
    fbqAktie: TFDQuery;
    fbqAktieA_NAMN: TStringField;
    fbqAktieA_NR: TSmallintField;
    fbqAktieI_INDEX: TStringField;
    fbqAktieKurs: TFDQuery;
    fbqAktieKursK_DBNR: TSmallintField;
    fbqAktieKursK_TID: TSQLTimeStampField;
    fbqAktieKursK_FIELD: TStringField;
    fbqAktieKursK_KURSEN: TCurrencyField;
    FDFBNBackup: TFDFBNBackup;
    procedure ConnectionStockAfterDisconnect(Sender: TObject);
    procedure ConnectionStockBeforeDisconnect(Sender: TObject);
  public
    const
      C_TYPE_BLOB      = 261;
      C_TYPE_BLOB_ID   = 45;
      C_TYPE_BOOLEAN   = 17;
      C_TYPE_CHAR      = 14;
      C_TYPE_CSTRING   = 40;
      C_TYPE_D_FLOAT   = 11;
      C_TYPE_DATE      = 12;
      C_TYPE_DOUBLE    = 27;
      C_TYPE_FLOAT     = 10;
      C_TYPE_INT64     = 16;
      C_TYPE_INTEGER   = 8;
      C_TYPE_QUAD      = 9;
      C_TYPE_SMALLINT  = 7;
      C_TYPE_TIME      = 13;
      C_TYPE_TIMESTAMP = 35;
      C_TYPE_VARCHAR   = 37;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteSQL(aSQLText: string; aConnection: TFDConnection = nil);
    procedure Initialize;

    procedure ChangeFieldType(aTableName, aFieldName: string; aNewType: Integer);
    procedure CreateNewField(aTableName, aFieldName, aFieldType: string; aDefaultValue: string = ''; aConnection: TFDConnection = nil);
    procedure CreateObject(aObjectName, aSqlText: string; aRelationType: TRelationType; aConnection: TFDConnection = nil);
    procedure DropField(aTableName, aFieldName: string; aConnection: TFDConnection = nil);
    procedure DropObject(aObjectName, aSqlText: string; aRelationType: TRelationType; aConnection: TFDConnection = nil);
    procedure RefreshQuery(const aQuery: TFDQuery);
    procedure ResizeField(aTableName, aFieldName: string; aNewSize: Integer; aConnection: TFDConnection = nil);

    //function BackupDatabase(const aDatabase: TIBDatabase): Boolean;
    function BackupDatabase(const aConnection: TFDConnection): Boolean;
    function CheckConnect(aConnection: TFDConnection = nil): Boolean;
    function GetBoolValueFromSQL(aSqlText, aField: string; aConnection: TFDConnection = nil): Boolean;
    function GetDateTimeValueFromSQL(aSqlText, aField: string; aConnection: TFDConnection = nil): TDateTime;
    function GetFloatValueFromSQL(aSqlText, aField: string; aConnection: TFDConnection = nil): Real;
    function GetIntegerValueFromSQL(aSqlText, aField: string; aConnection: TFDConnection = nil): Integer;
    function GetNextValue(aGenName: string; aConnection: TFDConnection = nil): Integer;
    function GetStringValueFromSQL(aSqlText, aField: string; aConnection: TFDConnection = nil): string;
    function GetProcedureValue(aProcName: array of string; const aParams: array of const): Integer;
    function CalculateVolatility(aContractId: Integer; aDateBegin, aDateEnd: TDateTime): Double;
  end;

var
  DMod: TDMod;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

constructor TDMod.Create(AOwner: TComponent);
begin
  inherited;
  //
end;

procedure TDMod.Initialize;
resourcestring
  rsBackupError = 'An error occurred while backup database';
  rsApplicationTerminated = 'Application will be terminated.';
  rsConfigureSettings = 'Please configure connection settings to DB';

  function EncodeVersion(aVersion: string): Integer;
  var
    arrIndex : TArray<string>;
    sArea    : string;
    sResult  : string;
  begin
    arrIndex := aVersion.Split(['.']);
    for sArea in arrIndex do
      sResult := sResult + Format('%.*d', [3, StrToIntDef(sArea, 0)]);
    Result := StrToIntDef(sResult, 0);
  end;

begin
  try
    TFireBirdConnect.SetConnectParams(ConnectionStock, TFireBirdConnect.DBNameStock);
    TFireBirdConnect.SetConnectParams(ConnectionFeed, TFireBirdConnect.DBNameFeed);
    ConnectionStock.Open;
    ConnectionFeed.Open;
  except
    on E: EIBInterBaseError do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'Initialize', E.Message);
      TfrmSplashScreen.HideSplash;
      if (E.SQLCode = -906) then //product INTERBASE is not licensed
      begin
        TMessageDialog.ShowError(E.Message + sLineBreak + sLineBreak + rsApplicationTerminated);
        Application.Terminate;
      end
      else
      begin
        TMessageDialog.ShowError(E.Message + sLineBreak + rsConfigureSettings);
        if (TfrmDatabaseProperties.ShowDocument = mrOk) then
          try
            if not ConnectionStock.Connected then
              TFireBirdConnect.SetConnectParams(ConnectionStock, TFireBirdConnect.DBNameStock);
            if not ConnectionFeed.Connected then
              TFireBirdConnect.SetConnectParams(ConnectionFeed, TFireBirdConnect.DBNameFeed);
            ConnectionStock.Open;
            ConnectionFeed.Open;
          except
            on Er: Exception do
            begin
              TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'Initialize', Er.Message);
              TMessageDialog.ShowError(Er.Message + sLineBreak + sLineBreak + rsApplicationTerminated);
              Application.Terminate;
            end;
          end
        else
        begin
          TMessageDialog.ShowError(rsApplicationTerminated);
          Application.Terminate;
        end;
      end;
    end;
    on En: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'Initialize', En.Message);
      TMessageDialog.ShowError(En.Message + sLineBreak + sLineBreak +rsApplicationTerminated);
      Application.Terminate;
    end;
  end;

  ConnectionStock.Transaction := TransactionStock;
  ConnectionFeed.Transaction  := TransactionFeed;

  General.Connection := ConnectionStock;
  General.Initialize;

  TTask.Create(
    procedure()
    begin
      if not(BackupDatabase(ConnectionStock) and BackupDatabase(ConnectionFeed)) then
        TThread.Synchronize(nil,
          procedure
          begin
            TMessageDialog.ShowError(rsBackupError);
          end);
    end).Start;

//  ChangeFieldType('SOKID_IB', 'EXPIRATIONDATE', C_TYPE_TIMESTAMP);
//  CreateObject('FK_QUALIFIERS_CONDITION_AUTOTR', C_SQL_CREATE_FK_ACCOUNTS_QUALIFIERS, rtForeignKey, IBDatabaseStock);

//  ResizeField('ORDERS', 'EXCHANGE', 20);
  DropField('CONDITION', 'IS_LESS_GRAD');
  DropField('CONDITION', 'IS_LESS_RT');
  DropField('CONDITION', 'IS_LESS_WIDTH');
  DropField('ORDER_GROUP', 'REL_SAMETIME');
  DropField('ORDER_GROUP', 'REL_COND');
  DropField('ORDER_GROUP', 'TYPE_SPEC');
  DropField('ORDER_GROUP', 'GROUP_TYPE');
  DropField('FACTOR', 'TICK_TYPE');
  DropField('QUALIFIERS_CONDITION', 'INEQUALITY_TYPE');

  CreateNewField('AUTOTRADES', 'AUTO_REFRESH', 'BOOLEAN', '0');
  CreateNewField('AUTOTRADES', 'HIST_BAR_SIZE', 'INTEGER');
  CreateNewField('AUTOTRADES', 'HIST_DATA_BASIS', 'INTEGER');
  CreateNewField('AUTOTRADES', 'HIST_DATA_DURATION', 'INTEGER');
  CreateNewField('AUTOTRADES', 'HIST_DATA_KEEP_UPDATED', 'BOOLEAN', '0');
  CreateNewField('AUTOTRADES', 'HIST_DURATION_TIME_UNITS', 'INTEGER');
  CreateNewField('AUTOTRADES', 'SUBSCRIBE_HIST_DATA', 'BOOLEAN', '0');
  CreateNewField('AUTOTRADES', 'TOTAL_ORDER_AMOUNT', 'INTEGER');
  CreateNewField('CONDITION', 'COND_VALUE_RELATIVE', 'FLOAT', '100');
  CreateNewField('CONDITION', 'DIVISION_VALUE', 'FLOAT', '1');
  CreateNewField('CONDITION', 'DURATION', 'TIMESTAMP');
  CreateNewField('CONDITION', 'INEQUALITY_COL', 'INTEGER', '0');
  CreateNewField('CONDITION', 'INEQUALITY_GR', 'INTEGER', '0');
  CreateNewField('CONDITION', 'INEQUALITY_RT', 'INTEGER', '0');
  CreateNewField('CONDITION', 'IS_TEMPLATE', 'BOOLEAN', '0');
  CreateNewField('CONDITION', 'TICK_TYPE1', 'INTEGER', Ord(ttLast).ToString);
  CreateNewField('CONDITION', 'TICK_TYPE2', 'INTEGER', Ord(ttNotSet).ToString);
  CreateNewField('CONDITION', 'TYPE_OPERATION', 'INTEGER', '0');
  CreateNewField('CONDITION', 'XML_PARAMS', 'BLOB SUB_TYPE 1 SEGMENT SIZE 4096');
  CreateNewField('FACTOR', 'TICK_TYPE1', 'INTEGER', Ord(ttLast).ToString);
  CreateNewField('FACTOR', 'TICK_TYPE2', 'INTEGER', Ord(ttNotSet).ToString);
  CreateNewField('MARKET_RULES', 'INCREMENT', 'DECIMAL(6,4)', '');
  CreateNewField('ORDER_GROUP', 'CHECKPOINT_PERIOD', 'INTEGER', '0');
  CreateNewField('ORDER_STATUS', 'NODE_ID', 'INTEGER', '0', ConnectionFeed);
  CreateNewField('ORDER_STATUS', 'PENDSUBMIT_TIME', 'TIMESTAMP', '', ConnectionFeed);
  CreateNewField('ORDER_STATUS', 'QUALIFIER_INSTANCE', 'INTEGER', '0', ConnectionFeed);
  CreateNewField('ORDER_STATUS', 'SUBMITTED_TIME', 'TIMESTAMP', '', ConnectionFeed);
  CreateNewField('ORDERS', 'IS_FINAL', 'BOOLEAN', '0');
  CreateNewField('ORDERS', 'SECURITY_TYPE', 'INTEGER');
  CreateNewField('ORDERS', 'TRIGGER_METHOD', 'INTEGER', '2');
  CreateNewField('STORE_TREE', 'NAME', 'VARCHAR(100)', '');
  CreateNewField('TICK_DATA', 'AVERAGE5', 'FLOAT', '0', ConnectionFeed);
  CreateNewField('TICK_DATA', 'IS_HISTORICAL', 'BOOLEAN', '0', ConnectionFeed);

  CreateObject('TICK_TYPES', C_SQL_CREATE_TICK_TYPES, rtTable, ConnectionFeed);

  CreateObject('GET_VOLATILITY', C_SQL_PROCEDURE_GET_VOLATILITY, rtProcedure, ConnectionFeed);
  CreateObject('GET_GRADIENT', C_SQL_PROCEDURE_GET_GRADIENT, rtProcedure, ConnectionFeed);
  CreateObject('INS_TICK_DATA', C_SQL_PROCEDURE_INS_TICK_DATA, rtProcedure, ConnectionFeed);
  CreateObject('IN_KURS', C_SQL_PROCEDURE_IN_KURS, rtProcedure, ConnectionFeed);
  CreateObject('IN_KURS_OM', C_SQL_PROCEDURE_IN_KURS_OM, rtProcedure, ConnectionFeed);
end;

procedure TDMod.RefreshQuery(const aQuery: TFDQuery);
var
  B: TBookmark;
begin
  B := aQuery.GetBookmark;
  try
    aQuery.Close;
    aQuery.Open;
    aQuery.Last;
  finally
    if aQuery.BookmarkValid(B) then
      aQuery.GotoBookmark(B);
    aQuery.FreeBookmark(B);
  end;
end;

procedure TDMod.ResizeField(aTableName, aFieldName: string; aNewSize: Integer; aConnection: TFDConnection = nil);
resourcestring
  C_SQL_CREATE_FIELD = '';
var
  DomenName: string;
  FieldType: string;
  SubType: Integer;
  Precision : Integer;
  Scale: Integer;
  FieldLength: Integer;
  SegmentLength: Integer;
  lQuery: TFDQuery;
begin
  lQuery := TFDQuery.Create(nil);
  try
    if Assigned(aConnection) then
    begin
      FBQuery.Connection := aConnection;
      lQuery.Connection := aConnection;
    end
    else
    begin
      FBQuery.Connection := ConnectionStock;
      lQuery.Connection := ConnectionStock;
    end;
    lQuery.Transaction := lQuery.Connection.Transaction;
    FBQuery.Transaction := FBQuery.Connection.Transaction;
    FieldType := '';
    try
      FBQuery.Close;
      FBQuery.SQL.Text := C_SQL_GET_DOMEN;
      FBQuery.ParamByName('TableName').AsString := aTableName;
      FBQuery.ParamByName('FieldName').AsString := aFieldName;
      FBQuery.Open;
      if not FBQuery.IsEmpty and (FBQuery.FieldByName('RDB$FIELD_LENGTH').AsInteger <> aNewSize) then
      begin
        DomenName         := FBQuery.FieldByName('RDB$FIELD_NAME').AsString;
        FieldLength       := FBQuery.FieldByName('RDB$FIELD_LENGTH').AsInteger;
        Precision         := FBQuery.FieldByName('RDB$FIELD_PRECISION').AsInteger;
        Scale             := FBQuery.FieldByName('RDB$FIELD_SCALE').AsInteger;
        SubType           := FBQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;
        SegmentLength     := FBQuery.FieldByName('RDB$SEGMENT_LENGTH').AsInteger;
        FieldLength := aNewSize;
        case FBQuery.FieldByName('RDB$FIELD_TYPE').AsInteger of
          C_TYPE_SMALLINT:
            begin
              case SubType of
                1:
                  FieldType := 'NUMERIC(' + Precision.ToString + ', -' + Scale.ToString + ')';
                2:
                  FieldType := 'DECIMAL';
              else
                FieldType := 'SMALLINT';
              end;
            end;
          C_TYPE_INTEGER:
            begin
              case SubType of
                1:
                  FieldType := 'NUMERIC(' + Precision.ToString + ', -' + Scale.ToString + ')';
                2:
                  FieldType := 'DECIMAL';
              else
                FieldType := 'INTEGER';
              end;
            end;
          C_TYPE_QUAD:
            FieldType := 'QUAD';
          C_TYPE_FLOAT:
            FieldType := 'FLOAT';
          C_TYPE_DATE:
            FieldType := 'DATE';
          C_TYPE_TIME:
            FieldType := 'TIME';
          C_TYPE_CHAR:
            FieldType := 'CHAR(' + FieldLength.ToString + ')';
          C_TYPE_INT64:
            begin
              case SubType of
                1:
                  FieldType := 'NUMERIC(' + Precision.ToString + ', -' + Scale.ToString + ')';
                2:
                  FieldType := 'DECIMAL';
              else
                FieldType := 'BIGINT';
              end;
            end;
          C_TYPE_BOOLEAN:
            FieldType := 'BOOLEAN';
          C_TYPE_DOUBLE:
            FieldType := 'DOUBLE';
          C_TYPE_TIMESTAMP:
            FieldType := 'TIMESTAMP';
          C_TYPE_VARCHAR:
            FieldType := 'VARCHAR(' + FieldLength.ToString + ')';
          C_TYPE_CSTRING:
            FieldType := 'CSTRING(' + FieldLength.ToString + ')';
          C_TYPE_BLOB_ID:
            FieldType := 'BLOB_ID';
          C_TYPE_BLOB:
            FieldType := 'BLOB SUB_TYPE ' + SubType.ToString + ' SEGMENT SIZE ' + SegmentLength.ToString;
        end;
      end;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ResizeField', E.Message + TDModUtils.GetQueryInfo(FBQuery));
        ShowMessage(E.Message);
      end;
    end;

    try
      if not FieldType.IsEmpty then
      begin
        lQuery.SQL.Clear;
        lQuery.SQL.Add('ALTER TABLE ' + aTableName + ' ADD ' + aFieldName + '_TMP ' + FieldType + ';');
        lQuery.SQL.Add('UPDATE ' + aTableName + ' SET ' + aFieldName + '_TMP = ' + aFieldName + ';');
        lQuery.SQL.Add('ALTER TABLE ' + aTableName + ' DROP ' + aFieldName + ';');
        lQuery.SQL.Add('ALTER TABLE ' + aTableName + ' ALTER COLUMN ' + aFieldName + '_TMP TO ' + aFieldName + ';');
        lQuery.ExecSQL;
        lQuery.Transaction.Commit;
      end;
    except on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ResizeField', E.Message + '<br>' + THtmlLib.SqlToHtml(lQuery.SQL.Text));
      end;
    end;
  finally
//    if not IBQuery.Transaction.Active then
//      IBQuery.Transaction.StartTransaction;
    FBQuery.Close;
    FreeAndNil(lQuery);
  end;
end;

function TDMod.BackupDatabase(const aConnection: TFDConnection): Boolean;
resourcestring
  rsBackupIsNotActive = 'Database backup is not activated in the parameters';
var
  BackupPath: string;
begin
  Result := False;
  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'BackupDatabase');
  if not General.IsActiveBackup then
  begin
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'BackupDatabase', rsBackupIsNotActive);
    TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'BackupDatabase');
    Exit(True);
  end
  else if Assigned(aConnection) then
    with FDFBNBackup do
    begin

      UserName := TFireBirdConnect.User;
      Password := TFireBirdConnect.Password;
      Host := TFireBirdConnect.Host;
      Port := StrToIntDef(TFireBirdConnect.Port, 0);
      if SameText(TFireBirdConnect.Protocol, 'Local, Default') then
        Protocol := ipLocal
      else
        Protocol := ipTCPIP;
      Level := 0;

      try
        if not General.PathToBackupFolder.IsEmpty and (TDirectory.Exists(General.PathToBackupFolder))then
          BackupPath := General.PathToBackupFolder
        else
        begin
          BackupPath := TPath.Combine(TDirectory.GetCurrentDirectory, 'Backup');
          if not TDirectory.Exists(BackupPath) then
            TDirectory.CreateDirectory(BackupPath);
        end;
        BackupPath   := TPath.Combine(BackupPath, TPath.ChangeExtension(TPath.GetFileName(aConnection.Params.Database), FormatDateTime('YYYY.MM.DD hh.nn.ss', Now) + '.fbk'));
        Database := aConnection.Params.Database;
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'BackupDatabase',
                                                                     'DatabaseName=' + aConnection.Params.Database +
                                                                     '<br>BackupFile=' + BackupPath);
        BackupFile := BackupPath;
        try
          Backup;
          Application.ProcessMessages;
          TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'BackupDatabase', aConnection.Params.Database);
        except
          on E: Exception do
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'BackupDatabase', E.Message);
        end;

        Result := True;
      finally
        TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'BackupDatabase');
      end;
    end;
end;

procedure TDMod.ChangeFieldType(aTableName, aFieldName: string; aNewType: Integer);
var
  DomenName: string;
begin
  try
    try
      FBQuery.Close;
      FBQuery.SQL.Text := C_SQL_GET_DOMEN;
      FBQuery.ParamByName('TableName').AsString := aTableName;
      FBQuery.ParamByName('FieldName').AsString := aFieldName;
      FBQuery.Open;
      if not FBQuery.IsEmpty and (FBQuery.FieldByName('RDB$FIELD_TYPE').AsInteger <> aNewType) then
      begin
        DomenName := FBQuery.FieldByName('RDB$FIELD_NAME').AsString;
        FBQuery.Close;
        FBQuery.SQL.Text := C_SQL_DOMEN_TYPE_EDIT;
        FBQuery.ParamByName('FieldName').AsString := DomenName;
        FBQuery.ParamByName('FieldType').AsInteger := aNewType;
        FBQuery.ExecSQL;
        FBQuery.Transaction.Commit;
      end;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ResizeField', E.Message + TDModUtils.GetQueryInfo(FBQuery));
        ShowMessage(E.Message);
      end;
    end;
  finally
//    if not FBQuery.Transaction.Active then
//      FBQuery.Transaction.StartTransaction;
    FBQuery.Close;
  end;
end;

procedure TDMod.CreateNewField(aTableName, aFieldName, aFieldType: string; aDefaultValue: string = ''; aConnection: TFDConnection = nil);
begin
  if Assigned(aConnection) then
    FBQuery.Connection := aConnection
  else
    FBQuery.Connection := ConnectionStock;
  FBQuery.Transaction := FBQuery.Connection.Transaction;

  try
    try
      FBQuery.Close;
      FBQuery.SQL.Text := C_SQL_IS_EXISTS_FIELD;
      FBQuery.ParamByName('TableName').AsString := aTableName;
      FBQuery.ParamByName('FieldName').AsString := aFieldName;
      FBQuery.Open;

      if FBQuery.IsEmpty then
      begin
        FBQuery.Close;
        FBQuery.SQL.Text := 'ALTER TABLE ' + aTableName + ' ADD ' + aFieldName + ' ' + aFieldType;
        FBQuery.ExecSQL;
        FBQuery.Transaction.CommitRetaining;
        if not aDefaultValue.IsEmpty then
        begin
          FBQuery.Close;
          FBQuery.SQL.Text := 'UPDATE ' + aTableName + ' SET ' + aFieldName + ' = ' + aDefaultValue;
          FBQuery.ExecSQL;
          FBQuery.Transaction.CommitRetaining;
        end;
      end;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'CreateNewField', E.Message + TDModUtils.GetQueryInfo(FBQuery));
        ShowMessage(E.Message);
      end;
    end;
  finally
    FBQuery.Close;
  end;
end;

procedure TDMod.DropField(aTableName, aFieldName: string; aConnection: TFDConnection = nil);
begin
  if Assigned(aConnection) then
    FBQuery.Connection := aConnection
  else
    FBQuery.Connection := ConnectionStock;
  FBQuery.Transaction := FBQuery.Connection.Transaction;

  try
    try
      FBQuery.Close;
      FBQuery.SQL.Text := C_SQL_IS_EXISTS_FIELD;
      FBQuery.ParamByName('TableName').AsString := aTableName;
      FBQuery.ParamByName('FieldName').AsString := aFieldName;
      FBQuery.Open;

      if not FBQuery.IsEmpty then
      begin
        FBQuery.Close;
        FBQuery.SQL.Text := 'ALTER TABLE ' + aTableName + ' DROP '+ aFieldName;
        FBQuery.ExecSQL;
        FBQuery.Transaction.CommitRetaining;
      end;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DropField', E.Message + TDModUtils.GetQueryInfo(FBQuery));
        ShowMessage(E.Message);
      end;
    end;
  finally
    FBQuery.Close;
  end;
end;

procedure TDMod.DropObject(aObjectName, aSqlText: string; aRelationType: TRelationType; aConnection: TFDConnection = nil);
var
  lQuery : TFDQuery;
begin
  try
    try
      FBQuery.Close;
      if Assigned(aConnection) then
      begin
        FBQuery.Connection := aConnection;
        lQuery.Connection := aConnection;
      end
      else
      begin
        FBQuery.Connection := ConnectionStock;
        lQuery.Connection := ConnectionStock;
      end;
      lQuery.Transaction := lQuery.Connection.Transaction;
      FBQuery.Transaction := FBQuery.Connection.Transaction;
      case aRelationType of
        rtTable:
          FBQuery.SQL.Text := C_SQL_IS_EXISTS_TABLE;
        rtProcedure:
          FBQuery.SQL.Text := C_SQL_IS_EXISTS_PROCEDURE;
        rtUdf:
          FBQuery.SQL.Text := C_SQL_IS_EXISTS_FUNCTIONS;
        rtDomain:
          FBQuery.SQL.Text := C_SQL_IS_EXISTS_DOMAIN;
        rtForeignKey:
          FBQuery.SQL.Text := C_SQL_IS_EXISTS_FOREIGN_KEY;
        end;
      FBQuery.ParamByName('ObjectName').AsString := aObjectName.ToUpper;
      FBQuery.Open;
      if not FBQuery.IsEmpty then
      begin
        lQuery.SQL.Text := aSqlText;
        lQuery.ExecSQL;
        lQuery.Transaction.Commit;
      end;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, Self, 'DropObject', E.Message + '<br>' + THtmlLib.SqlToHtml(aSqlText));
        ShowMessage(E.Message);
      end;
    end;
  finally
    FBQuery.Close;
    FreeAndNil(lQuery);
//    if not IBScript.Transaction.Active then
//      IBScript.Transaction.StartTransaction;
  end;
end;

procedure TDMod.CreateObject(aObjectName, aSqlText: string; aRelationType: TRelationType; aConnection: TFDConnection = nil);
var lQuery: TFDQuery;
begin
  lQuery := TFDQuery.Create(nil);
  try
    try
      FBQuery.Close;
      if Assigned(aConnection) then
      begin
        FBQuery.Connection  := aConnection;
        lQuery.Connection := aConnection;
      end
      else
      begin
        FBQuery.Connection  := ConnectionStock;
        lQuery.Connection := ConnectionStock;
      end;
      FBQuery.Transaction  := FBQuery.Connection.Transaction;
      lQuery.Transaction := lQuery.Connection.Transaction;

      case aRelationType of
        rtTable:
          FBQuery.SQL.Text := C_SQL_IS_EXISTS_TABLE;
        rtProcedure:
          FBQuery.SQL.Text := C_SQL_IS_EXISTS_PROCEDURE;
        rtUdf:
          FBQuery.SQL.Text := C_SQL_IS_EXISTS_FUNCTIONS;
        rtDomain:
          FBQuery.SQL.Text := C_SQL_IS_EXISTS_DOMAIN;
        rtForeignKey:
          FBQuery.SQL.Text := C_SQL_IS_EXISTS_FOREIGN_KEY;
        end;
      FBQuery.ParamByName('ObjectName').AsString := aObjectName.ToUpper;
      FBQuery.Open;
      if FBQuery.IsEmpty then
      begin
        lQuery.SQL.Text := aSqlText;
        lQuery.ExecSQL;
        lQuery.Transaction.Commit;
      end;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, Self, 'CreateObject', E.Message + '<br>' + THtmlLib.SqlToHtml(aSqlText));
        ShowMessage(E.Message);
      end;
    end;
  finally
    FBQuery.Close;
    FreeAndNil(lQuery);
//    if not lQuery.Transaction.Active then
//      lQuery.Transaction.StartTransaction;
  end;
end;

function TDMod.CheckConnect(aConnection: TFDConnection = nil): Boolean;
resourcestring
  rsIBDatabaseNotConnected = 'Database %s is not connected!';
var
  Info: string;
begin
  Result := True;
  if not Assigned(aConnection) then
    aConnection := ConnectionStock;

  if not aConnection.Connected then
  begin
    aConnection.Open;
    Result := aConnection.Connected;
    if not Result then
    begin
      Info := Format(rsIBDatabaseNotConnected, [aConnection.Params.Database]);
      TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, Self, 'CheckConnect', Info);
      raise Exception.Create(Info);
    end;
  end;

//  if not aDatabase.DefaultTransaction.Active then
//    aDatabase.DefaultTransaction.StartTransaction;
end;

function TDMod.GetProcedureValue(aProcName: array of string; const aParams: array of const): Integer;
var
  vTyped: array [0 .. $FFF0 div SizeOf(TVarRec)] of TVarRec absolute aParams;
  i: Integer;
begin
  TransactionStock.CommitRetaining;
  FBStoredProc.StoredProcName := aProcName[Low(aProcName)];
  FBStoredProc.Prepare;
  for i := Low(aProcName) + 1 to High(aProcName) - 1 do
    if aProcName[i] <> '' then
      with vTyped[i - 1] do
        with DMod.FBStoredProc.Params do
        begin
          case VType of
            vtInteger:
              ParamByName(aProcName[i]).Value := VInteger;
            vtBoolean:
              if VBoolean then
                ParamByName(aProcName[i]).Value := 1
              else
                ParamByName(aProcName[i]).Value := 0;
            vtExtended:
              ParamByName(aProcName[i]).Value := VExtended^;
            vtString:
              ParamByName(aProcName[i]).Value := VString^;
            vtPWideChar:
              ParamByName(aProcName[i]).Value := VPWideChar^;
            vtUnicodeString:
              ParamByName(aProcName[i]).Value := string(PChar(VUnicodeString));
          end;
        end;
  DMod.FBStoredProc.ExecProc;
  if not aProcName[High(aProcName)].IsEmpty then
    Result := DMod.FBStoredProc.Params.ParamByName(aProcName[High(aProcName)]).AsInteger
  else
    Result := 0;
  DMod.FBStoredProc.UnPrepare;
  DMod.TransactionStock.CommitRetaining;
end;

procedure TDMod.ExecuteSQL(aSQLText: string; aConnection: TFDConnection = nil);
begin
  try
    FBQuery.Close;
    if Assigned(aConnection) then
      FBQuery.Connection := aConnection
    else
      FBQuery.Connection := ConnectionStock;
    FBQuery.Transaction := FBQuery.Connection.Transaction;
    try
      FBQuery.SQL.Text := aSQLText;
      FBQuery.Prepare;
      FBQuery.ExecSQL;
    finally
      FBQuery.Close;
    end;
    FBQuery.Transaction.CommitRetaining;
  except
    on E: Exception do
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ExecuteSQL', E.Message + TDModUtils.GetQueryInfo(FBQuery));
      raise;
    end;
  end;
end;

function TDMod.GetBoolValueFromSQL(aSqlText, aField: string; aConnection: TFDConnection = nil): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  Query := TFDQuery.Create(Self);
  try
    CheckConnect;
    if Assigned(aConnection) then
      Query.Connection := aConnection
    else
      Query.Connection := ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    Query.SQL.Clear;
    Query.SQL.Add(aSqlText); // hämtar om repetetive checked
    try
      Query.Prepare;
      Query.Open;
      Query.First;
      if not Query.Eof then
        Result := Query.FieldByName(aField).AsBoolean;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'GetBoolValueFromSQL', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

function TDMod.GetFloatValueFromSQL(aSqlText, aField: string; aConnection: TFDConnection = nil): Real;
var
  Query: TFDQuery;
begin
  Result := 0.0;
  Query := TFDQuery.Create(Self);
  try
    CheckConnect;
    if Assigned(aConnection) then
      Query.Connection := aConnection
    else
      Query.Connection := ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    Query.SQL.Clear;
    Query.SQL.Add(aSqlText);
    try
      Query.Prepare;
      Query.Open;
      Query.First;
      if not Query.IsEmpty then
        Result := Query.FieldByName(aField).AsFloat;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'GetFloatValueFromSQL', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

function TDMod.GetDateTimeValueFromSQL(aSqlText, aField: string; aConnection: TFDConnection = nil): TDateTime;
var
  Query: TFDQuery;
begin
  Result := 0;
  Query := TFDQuery.Create(nil);
  try
    CheckConnect;
    if Assigned(aConnection) then
      Query.Connection := aConnection
    else
      Query.Connection := ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    Query.SQL.Clear;
    Query.SQL.Add(aSqlText);
    try
      Query.Prepare;
      Query.Open;
      Query.First;
      if not Query.IsEmpty then
        Result := Query.FieldByName(aField).AsDateTime;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'GetDateTimeValueFromSQL', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

function TDMod.GetStringValueFromSQL(aSqlText, aField: string; aConnection: TFDConnection = nil): string;
var
  Query: TFDQuery;
begin
  Result := '';
  Query := TFDQuery.Create(nil);
  try
    CheckConnect;
    if Assigned(aConnection) then
      Query.Connection := aConnection
    else
      Query.Connection := ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    Query.SQL.Text := aSqlText;
    try
      Query.Prepare;
      Query.Open;
      Query.First;
      if not Query.IsEmpty then
        Result := Query.FieldByName(aField).AsString;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'GetStringValueFromSQL', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

function TDMod.GetIntegerValueFromSQL(aSqlText, aField: string; aConnection: TFDConnection = nil): Integer;
begin
  Result := StrToIntDef(Trim(GetStringValueFromSQL(aSqlText, aField, aConnection)), 0);
end;

function TDMod.GetNextValue(aGenName: string; aConnection: TFDConnection = nil): Integer;
begin
  Result := DMod.GetIntegerValueFromSQL('SELECT GEN_ID(' + aGenName + ', 1) FROM RDB$DATABASE', 'GEN_ID', aConnection);
end;

procedure TDMod.ConnectionStockAfterDisconnect(Sender: TObject);
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, ConnectionStock, 'IBDatabase.AfterDisconnect');
end;

procedure TDMod.ConnectionStockBeforeDisconnect(Sender: TObject);
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, ConnectionStock, 'IBDatabase.BeforeDisconnect');
end;

function TDMod.CalculateVolatility(aContractId: Integer; aDateBegin, aDateEnd: TDateTime): Double;
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT VOLATILITY FROM GET_VOLATILITY(:CONID, :DATE_BEGIN, :DATE_END)';
var
  Connection: TFDConnection;
  Query: TFDQuery;
  Transaction: TFDTransaction;
begin
  Result := -1;
  try
    Connection := TFDConnection.Create(nil);
    try
      TFireBirdConnect.SetConnectParams(Connection, TFireBirdConnect.DBNameFeed);
      Transaction := TFDTransaction.Create(Connection);
      try
        Connection.Transaction := Transaction;
        Transaction.Connection := Connection;
        Transaction.Options.AutoCommit := false;
        Query := TFDQuery.Create(Connection);
        try
          Query.SQL.Text := C_SQL_SELECT_TEXT;
          Query.Connection := Connection;
          Query.Transaction := Transaction;
          Query.ParamByName('CONID').AsInteger       := aContractId;
          Query.ParamByName('DATE_BEGIN').AsDateTime := aDateBegin;
          Query.ParamByName('DATE_END').AsDateTime   := aDateEnd;
          try
            Query.Prepare;
            Query.Open;
            if not(Application.Terminated or (csDestroying in TForm(Self).ComponentState)) and not Query.IsEmpty and
               not IsNan(Query.FieldByName('VOLATILITY').AsFloat) then
              Result := Query.FieldByName('VOLATILITY').AsFloat;
          except
            on E: Exception do
              TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'CalculateVolatility', E.Message + TDModUtils.GetQueryInfo(Query));
          end;
        finally
          FreeAndNil(Query);
        end;
      finally
        if Transaction.Active then
          Transaction.Commit;
        FreeAndNil(Transaction);
      end;
    finally
      if Connection.Connected then
        Connection.Connected := False;
      FreeAndNil(Connection);
    end;
  except
    on E: Exception do
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'CalculateVolatility', E.Message);
  end;
end;

end.
