unit Column.Settings;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids,
  System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.Mask, Vcl.ImgList, Winapi.ActiveX,
  System.UITypes, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.ImageList, System.Math, Vcl.DBCtrls,
  VirtualTrees, DebugWriter, HtmlLib, Column.Types, IABSocketAPI, IABFunctions, Global.Types, CustomForms,
  DaImages, Monitor.Types, DaModule, XmlFiles, Publishers, Common.Types, DaModule.Utils, IABSocketAPI_const,
  MonitorTree.Helper, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TArrayColumns = TArray<TColumnSetting>;
  PArrayColumns = ^TArrayColumns;

  TfrmColumnSettings = class(TCustomForm)
    aCancel: TAction;
    ActionListMain: TActionList;
    aDeleteColumnSettings: TAction;
    aSave: TAction;
    aSaveColumnSettings: TAction;
    btnAddColumn: TBitBtn;
    btnCancel: TBitBtn;
    btnClearSearchText: TBitBtn;
    btnDeleteColumnSettings: TBitBtn;
    btnSaveColumnSettings: TBitBtn;
    cbColumnSettings: TComboBox;
    edtSearch: TEdit;
    lblAvailableFilters: TLabel;
    lblColumnSettings: TLabel;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    vstColumns: TVirtualStringTree;
    procedure aCancelExecute(Sender: TObject);
    procedure aDeleteColumnSettingsExecute(Sender: TObject);
    procedure aSaveColumnSettingsExecute(Sender: TObject);
    procedure btnClearSearchTextClick(Sender: TObject);
    procedure cbColumnSettingsChange(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure vstColumnsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstColumnsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstColumnsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstColumnsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    FArrayColumns: PArrayColumns;
    FIdentityName: string;
    function GetColumnSettings: string;
    procedure DeleteColumnSettings(const aRecordId: Integer);
    procedure LoadColumnSettings(const aIdentityName: string);
    procedure RestoreColumnSettings(const aXmlParams: string);
    procedure SaveColumnSettings(aName: string; out aRecordId: Integer; const aIdentityName, aXmlParams: string);
  const
    COL_NAME  = 0;
    COL_WIDTH = 1;
    COL_POSITION = 2;

    C_SECTION_COLUMNS = 'Columns';
    C_ATTR_INDEX    = 'Index';
    C_ATTR_NAME     = 'Name';
    C_ATTR_POSITION = 'Position';
    C_ATTR_VISIBLE  = 'Visible';
    C_ATTR_WIDTH    = 'Width';
    C_ATTR_TAG      = 'Tag';
  public
    class function ShowDocument(const aArrayColumns: PArrayColumns; const aIdentityName: string): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmColumnSettings.ShowDocument(const aArrayColumns: PArrayColumns; const aIdentityName: string): TModalResult;
begin
  with TfrmColumnSettings.Create(nil) do
    try
      FArrayColumns := aArrayColumns;
      FIdentityName := aIdentityName;
      Initialize;
      Result := ShowModal;
      if (Result = mrOk) then
        Denitialize;
    finally
      Free;
    end;
end;

procedure TfrmColumnSettings.FormCreate(Sender: TObject);
begin
  inherited;
  vstColumns.NodeDataSize := SizeOf(TColumnSetting);
end;

procedure TfrmColumnSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  for var i := 0 to cbColumnSettings.Items.Count - 1 do
    cbColumnSettings.Items.Objects[i].Free;
  cbColumnSettings.Items.Clear;
end;

procedure TfrmColumnSettings.Initialize;
var
  Data: PColumnSetting;
  Node: PVirtualNode;
begin
  TMonitorTree.Initialize(vstColumns);
  vstColumns.BeginUpdate;
  try
    if (Length(FArrayColumns^) > 0) then
      for var i := Low(FArrayColumns^) to High(FArrayColumns^) do
      begin
        Node := vstColumns.AddChild(nil);
        Node^.CheckType := ctCheckBox;
        Data := Node^.GetData;
        Data^.AssignFrom(FArrayColumns^[i]);
        if Data^.Visible then
          Node^.CheckState := csCheckedNormal
        else
          Node^.CheckState := csUncheckedNormal;
      end;
  finally
    vstColumns.EndUpdate;
  end;
  LoadColumnSettings(FIdentityName);
end;

procedure TfrmColumnSettings.Denitialize;
var
  Data: PColumnSetting;
  Node: PVirtualNode;
  Index: Integer;
begin
  vstColumns.BeginUpdate;
  try
    SetLength(FArrayColumns^, vstColumns.RootNodeCount);
    Index := 0;
    Node  := vstColumns.GetFirst;
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      FArrayColumns^[Index].AssignFrom(Data^);
      Inc(Index);
      Node := Node.NextSibling;
    end;
  finally
    vstColumns.EndUpdate;
  end;
end;

procedure TfrmColumnSettings.edtSearchChange(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PColumnSetting;
begin
  inherited;
  vstColumns.BeginUpdate;
  try
    Node := vstColumns.GetFirst;
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      vstColumns.IsVisible[Node] := string(edtSearch.Text).IsEmpty or (Pos(string(edtSearch.Text).ToUpper, Data^.Name.ToUpper) > 0);
      Node := Node.NextSibling;
    end;
  finally
//    vstColumns.SortTree(vstColumns.Header.SortColumn, vstColumns.Header.SortDirection);
    vstColumns.EndUpdate;
  end;
end;

procedure TfrmColumnSettings.vstColumnsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PColumnSetting;
begin
  Data := Node^.GetData;
  Data^.Visible := Node.CheckState = csCheckedNormal;
end;

procedure TfrmColumnSettings.vstColumnsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PColumnSetting;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    COL_NAME:
      Result := CompareText(Data1^.Name, Data2^.Name);
    COL_WIDTH:
      Result := CompareValue(Data1^.Width, Data2^.Width);
    COL_POSITION:
      Result := CompareValue(Data1^.Position, Data2^.Position);
  end;
end;

procedure TfrmColumnSettings.vstColumnsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PColumnSetting;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmColumnSettings.vstColumnsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PColumnSetting;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    COL_NAME:
      CellText := Data^.Name;
    COL_WIDTH:
      CellText := Data^.Width.ToString;
    COL_POSITION:
      CellText := Data^.Position.ToString;
  else
    CellText := '';
  end;
end;

procedure TfrmColumnSettings.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TfrmColumnSettings.LoadColumnSettings(const aIdentityName: string);
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT * FROM STORE_TREE WHERE IDENTITY=:IDENTITY ORDER BY ID';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    for var i := 0 to cbColumnSettings.Items.Count - 1 do
      cbColumnSettings.Items.Objects[i].Free;
    cbColumnSettings.Items.Clear;

    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_SELECT_TEXT;
    Query.ParamByName('IDENTITY').AsString := aIdentityName;
    Query.Open;
    while not Query.Eof do
    begin
      cbColumnSettings.Items.AddObject(Query.FieldByName('NAME').AsString, TStringObject.Create(Query.FieldByName('ID').AsInteger, Query.FieldByName('XML_PARAMS').AsString));
      Query.Next;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TfrmColumnSettings.RestoreColumnSettings(const aXmlParams: string);
var
  XMLFile: TXMLFile;
  Index: Integer;
  Node: PVirtualNode;
  Data: PColumnSetting;
begin
  if not aXmlParams.IsEmpty then
  begin
    XMLFile := TXMLFile.Create;
    try
      XMLFile.XMLText := aXmlParams;
      XMLFile.CurrentSection := C_SECTION_COLUMNS;
      while not XMLFile.IsLastKey do
      begin
        if XMLFile.ReadAttributes then
        begin
          Index := XMLFile.Attributes.GetAttributeValue(C_ATTR_INDEX, -1);
          Node  := vstColumns.GetFirst;
          Data  := nil;
          while Assigned(Node) do
          begin
            Data := Node^.GetData;
            if (Data^.Index = Index) then
              Break;
            Node := Node.NextSibling;
            Data := nil;
          end;

          if Assigned(Node) and Assigned(Data) then
          begin
            Data^.Position := XMLFile.Attributes.GetAttributeValue(C_ATTR_POSITION, Data^.Position);
            Data^.Name     := XMLFile.Attributes.GetAttributeValue(C_ATTR_NAME, Data^.Name);
            Data^.Width    := XMLFile.Attributes.GetAttributeValue(C_ATTR_WIDTH, Data^.Width);
            Data^.Visible  := XMLFile.Attributes.GetAttributeValue(C_ATTR_VISIBLE, Data^.Visible);
            Data^.TickType := TIABTickType(XMLFile.Attributes.GetAttributeValue(C_ATTR_TAG, Ord(Data^.TickType)));

            if Data^.Visible then
              Node.CheckState := csCheckedNormal
            else
              Node.CheckState := csUncheckedNormal;
            vstColumns.InvalidateNode(Node);
          end;
        end;
        XMLFile.NextKey;
      end;
    finally
      FreeAndNil(XMLFile);
    end;
  end;
end;

function TfrmColumnSettings.GetColumnSettings: string;
var
  XMLFile: TXMLFile;
  Node: PVirtualNode;
  Data: PColumnSetting;
begin
  XMLFile := TXMLFile.Create;
  try
    XMLFile.CurrentSection := C_SECTION_COLUMNS;
    Node  := vstColumns.GetFirst;
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      XMLFile.Attributes.AddNode;
      XMLFile.Attributes.SetAttributeValue(C_ATTR_INDEX, Data^.Index);
      XMLFile.Attributes.SetAttributeValue(C_ATTR_POSITION, Data^.Position);
      XMLFile.Attributes.SetAttributeValue(C_ATTR_NAME, Data^.Name);
      XMLFile.Attributes.SetAttributeValue(C_ATTR_WIDTH, Data^.Width);
      XMLFile.Attributes.SetAttributeValue(C_ATTR_VISIBLE, Data^.Visible);
      XMLFile.Attributes.SetAttributeValue(C_ATTR_TAG, Ord(Data^.TickType));
      XMLFile.WriteAttributes;
      Node := Node.NextSibling;
    end;
    Result := XMLFile.XMLText;
  finally
    FreeAndNil(XMLFile);
  end;
end;

procedure TfrmColumnSettings.SaveColumnSettings(aName: string; out aRecordId: Integer; const aIdentityName, aXmlParams: string);
resourcestring
  C_SQL_INSERT_TEXT = 'INSERT INTO STORE_TREE(ID, NAME, IDENTITY, XML_PARAMS) ' + sLineBreak +
                      'VALUES(:ID, :NAME, :IDENTITY, :XML_PARAMS)';
  C_SQL_UPDATE_TEXT = 'UPDATE STORE_TREE '                                          + sLineBreak +
                      'SET NAME=:NAME, IDENTITY=:IDENTITY, XML_PARAMS=:XML_PARAMS ' + sLineBreak +
                      'WHERE (ID = :ID); ';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    if (aRecordId <= 0) then
    begin
      aRecordId := DMod.GetNextValue('GEN_STORE_TREE_ID');
      Query.SQL.Text := C_SQL_INSERT_TEXT;
    end
    else
      Query.SQL.Text := C_SQL_UPDATE_TEXT;
    if aName.IsEmpty then
      aName := 'ColumnSettings nr ' + aRecordId.ToString;

    Query.ParamByName('ID').AsInteger        := aRecordId;
    Query.ParamByName('XML_PARAMS').AsString := aXmlParams.Substring(0, 16000);
    Query.ParamByName('NAME').AsString       := aName.Substring(0, 100);
    Query.ParamByName('IDENTITY').AsString   := aIdentityName.Substring(0, 100);
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveColumnSettings', E.Message + TDModUtils.GetQueryInfo(Query));
    end;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TfrmColumnSettings.DeleteColumnSettings(const aRecordId: Integer);
resourcestring
  C_SQL_DELETE_TEXT = 'DELETE FROM STORE_TREE ' + sLineBreak +
                      'WHERE (ID = :ID); ';
var
  Query: TFDQuery;
begin
  if (aRecordId > 0) then
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.Transaction := Query.Connection.Transaction;
      Query.SQL.Text := C_SQL_DELETE_TEXT;
      Query.ParamByName('ID').AsInteger := aRecordId;
      try
        Query.Prepare;
        Query.ExecSQL;
        Query.Transaction.CommitRetaining;
      except
        on E: Exception do
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'DeleteColumnSettings', E.Message + TDModUtils.GetQueryInfo(Query));
      end;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TfrmColumnSettings.aDeleteColumnSettingsExecute(Sender: TObject);
var
  str: TStringObject;
begin
  inherited;
  if (cbColumnSettings.ItemIndex > -1) then
  begin
    str := TStringObject(cbColumnSettings.Items.Objects[cbColumnSettings.ItemIndex]);
    if (str.Id > 0) then
    begin
      DeleteColumnSettings(str.Id);
      cbColumnSettings.Items.Objects[cbColumnSettings.ItemIndex].Free;
      cbColumnSettings.Items.Delete(cbColumnSettings.ItemIndex);
      cbColumnSettings.Text := '';
    end;
  end;
end;

procedure TfrmColumnSettings.aSaveColumnSettingsExecute(Sender: TObject);
var
  str: TStringObject;
begin
  if (cbColumnSettings.ItemIndex > -1) then
  begin
    str := TStringObject(cbColumnSettings.Items.Objects[cbColumnSettings.ItemIndex]);
    str.StringValue := GetColumnSettings;
    SaveColumnSettings(cbColumnSettings.Text, str.Id, FIdentityName, str.StringValue);
    cbColumnSettings.Items.Objects[cbColumnSettings.ItemIndex] := str;
  end
  else
  begin
    str := TStringObject.Create;
    str.StringValue := GetColumnSettings;
    SaveColumnSettings(cbColumnSettings.Text, str.Id, FIdentityName, str.StringValue);
    if string(cbColumnSettings.Text).IsEmpty then
      cbColumnSettings.Text := 'ColumnSettings nr ' + str.Id.ToString;
    cbColumnSettings.ItemIndex := cbColumnSettings.Items.AddObject(cbColumnSettings.Text, str);
  end;
end;

procedure TfrmColumnSettings.btnClearSearchTextClick(Sender: TObject);
begin
  edtSearch.Text := '';
end;

procedure TfrmColumnSettings.cbColumnSettingsChange(Sender: TObject);
var
  str: TStringObject;
begin
  if (cbColumnSettings.ItemIndex > -1) then
  begin
    str := TStringObject(cbColumnSettings.Items.Objects[cbColumnSettings.ItemIndex]);
    RestoreColumnSettings(str.StringValue);
  end
end;

end.

