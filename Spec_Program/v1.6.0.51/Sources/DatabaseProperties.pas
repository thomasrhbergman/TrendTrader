unit DatabaseProperties;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Data.DB, System.IOUtils, DebugWriter, CustomForms, MessageDialog, Global.Types, Vcl.StdCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Utils.LocalInformation, DaModule.Constants,
  Vcl.Buttons, Vcl.Dialogs, Vcl.ExtCtrls, DaImages, DaModule.Resources, HtmlLib, System.Actions,
  Vcl.ActnList, DaModule.ExecuteScript, Global.Resources, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.VCLUI.Wait, FireDAC.Comp.Client;
{$ENDREGION}

type
  TIBProtocol = (ibpLocalDefault, ibpLocalLoopback, ibpRemoteTCPIP);

  TfrmDatabaseProperties = class(TCustomForm)
    aCleanDatabases: TAction;
    ActionList: TActionList;
    aRunSQLScript: TAction;
    aTestConnect: TAction;
    btnCancel: TBitBtn;
    btnCleanDatabases: TBitBtn;
    btnOk: TBitBtn;
    btnRunSQLScript: TBitBtn;
    btnTestConnect: TBitBtn;
    cbPort: TComboBox;
    cbProtocol: TComboBox;
    cbServerName: TComboBox;
    edConnectionStringDBFeed: TEdit;
    edConnectionStringStockXRobot: TEdit;
    edPassword: TEdit;
    edPathToDBFeed: TButtonedEdit;
    edPathToStockXRobot: TButtonedEdit;
    edUserName: TEdit;
    lblAddConnectParameters: TLabel;
    lblConnectionStringDBFeed: TLabel;
    lblConnectionStringStockXRobot: TLabel;
    lblPassword: TLabel;
    lblPathToDB_FEED: TLabel;
    lblPathToStockXRobot: TLabel;
    lblPort: TLabel;
    lblProtocol: TLabel;
    lblServerName: TLabel;
    lblUserName: TLabel;
    memAddConnectParameters: TMemo;
    OpenDialog: TFileOpenDialog;
    pnlBottom: TPanel;
    FDConnection: TFDConnection;
    lblDriverID: TLabel;
    edDriverID: TEdit;
    procedure aCleanDatabasesExecute(Sender: TObject);
    procedure aRunSQLScriptExecute(Sender: TObject);
    procedure aTestConnectExecute(Sender: TObject);
    procedure cbProtocolChange(Sender: TObject);
    procedure edPathToDBFeedRightButtonClick(Sender: TObject);
    procedure edPathToStockXRobotRightButtonClick(Sender: TObject);
    procedure OnConnectionStringChange(Sender: TObject);
  private const
    C_IDENTITY_NAME = 'DatabaseProperties';
  private
    FIPAddress: string;
    FDomainName: string;
    function GetConnectString(ADBPath, AProtocol, AServerName, APort: string): string;
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize;
    procedure Deinitialize;
    class function ShowDocument: TModalResult;
  end;

const
  IBProtocolName: array [TIBProtocol] of string = ('Local, Default', 'Local, Loopback', 'Remote, TCP/IP');

implementation

{$R *.dfm}

class function TfrmDatabaseProperties.ShowDocument: TModalResult;
begin
  with TfrmDatabaseProperties.Create(nil) do
    try
      Initialize;
      Result := ShowModal;
      if (Result = mrOk) then
        Deinitialize;
    finally
      Free;
    end;
end;

procedure TfrmDatabaseProperties.Initialize;
var
  Path: string;
begin
  inherited;
  cbProtocol.Items.Clear;
  for var Protocol := Low(TIBProtocol) to High(TIBProtocol) do
    cbProtocol.Items.Add(IBProtocolName[Protocol]);

  TLocalInformationDialog.GetLocalIPAddressName(FIPAddress, FDomainName);
  cbServerName.Items.Clear;
  cbServerName.Items.Add('');
  cbServerName.Items.Add('localhost');
  cbServerName.Items.Add('127.0.0.1');
  cbServerName.Items.Add(FIPAddress);

  cbPort.Items.Clear;
  cbPort.Items.Add('');
  cbPort.Items.Add('3050');
  cbPort.Items.Add('3051');
  //cbPort.Items.Add('gds_db');

  cbProtocol.Text                    := General.XMLFile.ReadString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_PROTOCOL, IBProtocolName[Low(TIBProtocol)]);
  cbServerName.Text                  := General.XMLFile.ReadString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_SERVER_NAME, '');
  cbPort.Text                        := General.XMLFile.ReadString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_PORT, TFireBirdConnect.Port);
  edPathToStockXRobot.Text           := General.XMLFile.ReadString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_PATH_STOCKXROBOT, '');
  edPathToDBFeed.Text                := General.XMLFile.ReadString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_PATH_DBFEED, '');
  edUserName.Text                    := General.XMLFile.ReadString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_USER_NAME, TFireBirdConnect.User);
  edPassword.Text                    := General.XMLFile.ReadString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_PASSWORD, TFireBirdConnect.Password);
  edConnectionStringStockXRobot.Text := General.XMLFile.ReadString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_STOCKXROBOT, '');
  edConnectionStringDBFeed.Text      := General.XMLFile.ReadString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_DBFEED, '');
  edDriverID.Text                    := General.XMLFile.ReadString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_DRIVER_ID, TFireBirdConnect.DriverID);
  if General.XMLFile.IsExistsNode(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_ADD_PARAMETERS) then
    memAddConnectParameters.Text := General.XMLFile.ReadString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_ADD_PARAMETERS, '');

  Path := TPath.GetDirectoryName(Application.ExeName);
  if string(edPathToStockXRobot.Text).IsEmpty or not TFile.Exists(edPathToStockXRobot.Text) then
    edPathToStockXRobot.Text := TPath.Combine(Path, TFireBirdConnect.DBNameStock);
  if string(edPathToDBFeed.Text).IsEmpty or not TFile.Exists(edPathToDBFeed.Text) then
    edPathToDBFeed.Text := TPath.Combine(Path, TFireBirdConnect.DBNameFeed);
  cbProtocolChange(nil);
  OnConnectionStringChange(nil);
end;

procedure TfrmDatabaseProperties.OnConnectionStringChange(Sender: TObject);
begin
  edConnectionStringStockXRobot.Text := GetConnectString(edPathToStockXRobot.Text, cbProtocol.Text, cbServerName.Text, cbPort.Text);
  edConnectionStringDBFeed.Text      := GetConnectString(edPathToDBFeed.Text, cbProtocol.Text, cbServerName.Text, cbPort.Text);
end;

procedure TfrmDatabaseProperties.cbProtocolChange(Sender: TObject);
begin
  if (cbProtocol.Text = IBProtocolName[ibpLocalDefault]) then
  begin
    cbServerName.Enabled := False;
    cbPort.Enabled := False;
  end
  else if (cbProtocol.Text = IBProtocolName[ibpLocalLoopback]) then
  begin
    cbServerName.Enabled := True;
    cbPort.Enabled := True;
    cbServerName.Items.Clear;
    cbServerName.Items.Add('localhost');
    cbServerName.Items.Add('127.0.0.1');
    cbServerName.ItemIndex := 0;
  end
  else if (cbProtocol.Text = IBProtocolName[ibpRemoteTCPIP]) then
  begin
    cbServerName.Enabled := True;
    cbPort.Enabled := True;
    cbServerName.Items.Clear;
    cbServerName.Items.Add('');
    cbServerName.Items.Add('localhost');
    cbServerName.Items.Add(FIPAddress);
    cbServerName.ItemIndex := 2;
  end;
  OnConnectionStringChange(nil);
end;

procedure TfrmDatabaseProperties.Deinitialize;
begin
  inherited;
  try
    General.XMLFile.WriteString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_PROTOCOL, cbProtocol.Text);
    General.XMLFile.WriteString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_SERVER_NAME, cbServerName.Text);
    General.XMLFile.WriteString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_PORT, cbPort.Text);
    General.XMLFile.WriteString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_PATH_STOCKXROBOT, edPathToStockXRobot.Text);
    General.XMLFile.WriteString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_PATH_DBFEED, edPathToDBFeed.Text);
    General.XMLFile.WriteString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_USER_NAME, edUserName.Text);
    General.XMLFile.WriteString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_PASSWORD, edPassword.Text);
    General.XMLFile.WriteString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_STOCKXROBOT, edConnectionStringStockXRobot.Text);
    General.XMLFile.WriteString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_DBFEED, edConnectionStringDBFeed.Text);
    General.XMLFile.WriteString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_ADD_PARAMETERS, string(memAddConnectParameters.Text).Trim);
    General.XMLFile.WriteString(TFireBirdConnect.C_SECTION_NAME, TFireBirdConnect.C_KEY_DRIVER_ID, edDriverID.Text);
  finally
    General.XMLFile.Save;
  end;
end;

function TfrmDatabaseProperties.GetConnectString(ADBPath, AProtocol, AServerName, APort: string): string;
begin
  Result := '';
  if (cbProtocol.Text = IBProtocolName[ibpLocalDefault]) then
  begin
    Result := ADBPath;
  end
  else if (cbProtocol.Text = IBProtocolName[ibpLocalLoopback]) or (cbProtocol.Text = IBProtocolName[ibpRemoteTCPIP]) then
  begin
    if not AServerName.IsEmpty then
      Result := AServerName;
    if not APort.IsEmpty then
      Result := Result + '/' + APort;
    if not Result.IsEmpty then
      Result := Result + ':' + ADBPath
    else
      Result := ADBPath;
  end;
end;

procedure TfrmDatabaseProperties.aCleanDatabasesExecute(Sender: TObject);
resourcestring
  rsQuestion = 'Contents of the following tables will be cleared:' + sLineBreak +
               'ALGORITMOS'                                        + sLineBreak +
               'AUTOTRADES'                                        + sLineBreak +
               'CONDITION'                                         + sLineBreak +
               'DOC_RELATIONS'                                     + sLineBreak +
               'FACTOR'                                            + sLineBreak +
               'MARKET_RULES'                                      + sLineBreak +
               'ORDER_GROUP'                                       + sLineBreak +
               'ORDER_GROUP_SET'                                   + sLineBreak +
               'ORDERS'                                            + sLineBreak +
               'QUALIFIERS'                                        + sLineBreak +
               'SCAN_MARKET'                                       + sLineBreak +
               'QUALIFIERS_CONDITION'                              + sLineBreak + sLineBreak +
               'Do you want to continue?';
var
  Query: TFDQuery;
  Connection: TFDConnection;
  Transaction: TFDTransaction;
begin
  if (TMessageDialog.ShowQuestion(rsQuestion) = mrYes) then
  begin
    Connection := TFireBirdConnect.CreateDatabase(TFireBirdConnect.DBNameStock);
    try
      Connection.Connected := True;
      Transaction := TFDTransaction.Create(Connection);
      try
        Connection.Transaction := Transaction;
        Transaction.Connection := Connection;
        Transaction.Options.AutoCommit := false;
        Transaction.StartTransaction;

        Query := TFDQuery.Create(Connection);
        try
          Query.Connection := Connection;
          Query.Transaction    := Transaction;
          Query.SQL.Text := C_SQL_CLEAN_DATABASE;
          try
            Query.ExecSQL;
            TMessageDialog.ShowInfo(rsSuccessful);
          except on E: Exception do
            TMessageDialog.ShowError(E.Message + '<br>' + THtmlLib.SqlToHtml(Query.SQL.Text));
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
  end;
end;

procedure TfrmDatabaseProperties.aTestConnectExecute(Sender: TObject);

  procedure TestConnect(aConnectionString: string);
  resourcestring
    rsSuccessful = 'Connection to database "%s" is successful';
  begin
    if FDConnection.Connected then
      FDConnection.Connected := False;

    if not aConnectionString.IsEmpty then
    begin
      FDConnection.Params.Clear;
      FDConnection.Params.Database := aConnectionString;
      FDConnection.Params.Add('DriverID=' + edDriverID.Text);
      FDConnection.Params.Add('Database=' + aConnectionString);
      FDConnection.Params.Add('Protocol=' + cbProtocol.Text);
      FDConnection.Params.Add('Port=' + cbPort.Text);
      FDConnection.Params.Add('User_Name=' + edUserName.Text);
      FDConnection.Params.Add('Password=' + edPassword.Text);

      if not string(memAddConnectParameters.Text).Trim.IsEmpty  then
        FDConnection.Params.AddStrings(memAddConnectParameters.Lines);
      try
        FDConnection.Connected := True;
        TMessageDialog.ShowInfo(Format(rsSuccessful, [FDConnection.Params.Database]));
      except
        on E: Exception do
          TMessageDialog.ShowError(FDConnection.Params.Database + sLineBreak + E.Message);
      end;
    end;
  end;

begin
  TestConnect(edConnectionStringStockXRobot.Text);
  TestConnect(edConnectionStringDBFeed.Text);
  if FDConnection.Connected then
    FDConnection.Connected := False;
end;

procedure TfrmDatabaseProperties.aRunSQLScriptExecute(Sender: TObject);
begin
  TfrmExecuteScript.ShowDocument;
end;

function TfrmDatabaseProperties.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmDatabaseProperties.edPathToStockXRobotRightButtonClick(Sender: TObject);
var
  FileTypeItem: TFileTypeItem;
begin
  with OpenDialog do
  begin
    FileName := TFireBirdConnect.DBNameStock;
    FileTypes.Clear;
    FileTypeItem := FileTypes.Add;
    FileTypeItem.DisplayName := TFireBirdConnect.DBNameStock;
    FileTypeItem.FileMask    := TFireBirdConnect.DBNameStock;
    FileTypeItem := FileTypes.Add;
    FileTypeItem.DisplayName := 'All Files';
    FileTypeItem.FileMask    := '*.*';
    Options := [fdoPathMustExist, fdoFileMustExist];
    DefaultFolder := TDirectory.GetCurrentDirectory;
    if Execute then
      edPathToStockXRobot.Text := FileName;
  end;
end;

procedure TfrmDatabaseProperties.edPathToDBFeedRightButtonClick(Sender: TObject);
var
  FileTypeItem: TFileTypeItem;
begin
  with OpenDialog do
  begin
    FileName := TFireBirdConnect.DBNameFeed;
    FileTypes.Clear;
    FileTypeItem := FileTypes.Add;
    FileTypeItem.DisplayName := TFireBirdConnect.DBNameFeed;
    FileTypeItem.FileMask    := TFireBirdConnect.DBNameFeed;
    FileTypeItem := FileTypes.Add;
    FileTypeItem.DisplayName := 'All Files';
    FileTypeItem.FileMask    := '*.*';
    Options := [fdoPathMustExist, fdoFileMustExist];
    DefaultFolder := TDirectory.GetCurrentDirectory;
    if Execute then
      edPathToDBFeed.Text := FileName;
  end;
end;

end.
