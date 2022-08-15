unit Candidate.DragDropOptions;

interface

{$REGION 'Region uses'}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids, System.Actions,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.Mask, Vcl.ImgList, System.Math, Vcl.DBCtrls,
  System.ImageList,{$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Winapi.ActiveX, System.UITypes, VirtualTrees,
  BrokerHelperAbstr, DebugWriter, HtmlLib, Candidate.Types, CustomForms, DaImages, Monitor.Types, Vcl.Grids, DaModule,
  Global.Types, Utils, Entity.Sokid, VirtualTrees.ExportHelper, Publishers, Common.Types, DaModule.Utils,
  MonitorTree.Helper, MonitorTree.Document, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TCandidateDragDropOptions = record
    SourceType: TSourceType;
    StaticListId: Integer;
    StaticListName: string;
    RememberLastOption: Boolean;
  end;

  TfrmCandidateDragDropOptions = class(TCustomForm)
    btnCancel: TBitBtn;
    btnClearSearchText: TBitBtn;
    btnOk: TBitBtn;
    cbRememberLastOption: TCheckBox;
    edStaticListName: TEdit;
    edtSearchLists: TEdit;
    lblCreateNew: TLabel;
    lblSearchLists: TLabel;
    pnlBottom: TPanel;
    pnlCategoryTop: TPanel;
    pnlStaticList: TPanel;
    pnlStaticLists: TPanel;
    rbAddToExistingStaticList: TRadioButton;
    rbCreateNewStaticList: TRadioButton;
    rbScanColumn: TRadioButton;
    rbStaticList: TRadioButton;
    vstTree: TVirtualStringTree;
    procedure btnClearSearchTextClick(Sender: TObject);
    procedure DoButtonClick(Sender: TObject);
    procedure edtSearchListsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private const
    C_IDENTITY_NAME = 'CandidateDragDropOptions';

    COL_SL_NAME             = 0;
    COL_SL_SYMBOL           = 1;
    COL_SL_LOCAL_SYMBOL     = 2;
    COL_SL_CONID            = 3;
    COL_SL_CURRENCY         = 4;
    COL_SL_EXCHANGE         = 5;
    COL_SL_SECURITY_TYPE    = 6;
    COL_SL_BROKER           = 7;
    COL_SL_EXPIRY           = 8;
    COL_SL_UNDERLYING_CONID = 9;
    COL_SL_DESCRIPTION      = 10;
  private
    FCandidateDragDropOptions: TCandidateDragDropOptions;
    function GetCategoryColumnValue(aColumn: Integer; aData: PSokidInfo): string;
    function GetStaticListId: Integer;
    procedure LoadStaticLists;
    procedure SetFocusedNode(aStaticListId: Integer);
  protected
    function GetIdentityName: string; override;
  public
    class function GetCandidateDragDropOptions: TCandidateDragDropOptions;
    class function ShowDocument: TModalResult;
    class procedure LoadParamsFromXml(var aCandidateDragDropOptions: TCandidateDragDropOptions);
    class procedure SaveParamsToXml(aCandidateDragDropOptions: TCandidateDragDropOptions);
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

{ TfrmCandidateDragDropOptions }

class function TfrmCandidateDragDropOptions.ShowDocument: TModalResult;
begin
  with TfrmCandidateDragDropOptions.Create(nil) do
    try
      Initialize;
      Result := ShowModal;
      Denitialize;
    finally
      Free;
    end;
end;

class function TfrmCandidateDragDropOptions.GetCandidateDragDropOptions: TCandidateDragDropOptions;
begin
  LoadParamsFromXml(Result);
end;

procedure TfrmCandidateDragDropOptions.FormCreate(Sender: TObject);
begin
  vstTree.NodeDataSize := SizeOf(TSokidInfo);
end;

procedure TfrmCandidateDragDropOptions.FormDestroy(Sender: TObject);
begin
  vstTree.Clear;
end;

procedure TfrmCandidateDragDropOptions.Initialize;
begin
  TMonitorTree.Initialize(vstTree);
  TStoreHelper.LoadFromXml(vstTree, C_IDENTITY_NAME + '.' + vstTree.Name);
  LoadStaticLists;
  DMod.CheckConnect;
  LoadParamsFromXml(FCandidateDragDropOptions);
  rbScanColumn.Checked              := FCandidateDragDropOptions.SourceType = stCandidateMarket;
  rbStaticList.Checked              := FCandidateDragDropOptions.SourceType = stStaticList;
  rbCreateNewStaticList.Checked     := FCandidateDragDropOptions.StaticListId <= 0;
  rbAddToExistingStaticList.Checked := FCandidateDragDropOptions.StaticListId > 0;
  edStaticListName.Text             := FCandidateDragDropOptions.StaticListName;
  cbRememberLastOption.Checked      := FCandidateDragDropOptions.RememberLastOption;
  if (FCandidateDragDropOptions.StaticListId > 0) then
    SetFocusedNode(FCandidateDragDropOptions.StaticListId);
  DoButtonClick(nil);
end;

procedure TfrmCandidateDragDropOptions.Denitialize;
begin
  if rbScanColumn.Checked then
    FCandidateDragDropOptions.SourceType := stCandidateMarket
  else
    FCandidateDragDropOptions.SourceType := stStaticList;
  if rbCreateNewStaticList.Checked then
    FCandidateDragDropOptions.StaticListId := -1
  else
    FCandidateDragDropOptions.StaticListId := GetStaticListId;
  FCandidateDragDropOptions.StaticListName := edStaticListName.Text;
  FCandidateDragDropOptions.RememberLastOption := cbRememberLastOption.Checked;
  SaveParamsToXml(FCandidateDragDropOptions);
  TStoreHelper.SaveToXml(vstTree, C_IDENTITY_NAME + '.' + vstTree.Name);
end;

function TfrmCandidateDragDropOptions.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

class procedure TfrmCandidateDragDropOptions.LoadParamsFromXml(var aCandidateDragDropOptions: TCandidateDragDropOptions);
begin
  aCandidateDragDropOptions.RememberLastOption := General.XMLFile.ReadBool(C_IDENTITY_NAME, 'RememberLastOption', False);
  aCandidateDragDropOptions.SourceType         := TSourceType(General.XMLFile.ReadInteger(C_IDENTITY_NAME, 'SourceType', Ord(stCandidateMarket)));
  aCandidateDragDropOptions.StaticListId       := General.XMLFile.ReadInteger(C_IDENTITY_NAME, 'StaticListId', -1);
  aCandidateDragDropOptions.StaticListName     := General.XMLFile.ReadString(C_IDENTITY_NAME, 'StaticListName', '');
end;

class procedure TfrmCandidateDragDropOptions.SaveParamsToXml(aCandidateDragDropOptions: TCandidateDragDropOptions);
begin
  General.XMLFile.WriteBool(C_IDENTITY_NAME, 'RememberLastOption', aCandidateDragDropOptions.RememberLastOption);
  General.XMLFile.WriteInteger(C_IDENTITY_NAME, 'SourceType', Ord(aCandidateDragDropOptions.SourceType));
  General.XMLFile.WriteInteger(C_IDENTITY_NAME, 'StaticListId', aCandidateDragDropOptions.StaticListId);
  General.XMLFile.WriteString(C_IDENTITY_NAME, 'StaticListName', aCandidateDragDropOptions.StaticListName);
end;

procedure TfrmCandidateDragDropOptions.DoButtonClick(Sender: TObject);
begin
  edStaticListName.Enabled          := rbCreateNewStaticList.Checked and rbStaticList.Checked;
  pnlStaticLists.Enabled            := rbAddToExistingStaticList.Checked and rbStaticList.Checked;
  rbCreateNewStaticList.Enabled     := rbStaticList.Checked;
  rbAddToExistingStaticList.Enabled := rbStaticList.Checked;
end;

procedure TfrmCandidateDragDropOptions.LoadStaticLists;
resourcestring
  C_SQL_SELECT_TEXT        = 'SELECT * FROM STATICLISTS WHERE VISIBLE=1';
  C_SQL_SELECT_DETAIL_TEXT = 'SELECT * FROM STATICLISTS_DETAIL WHERE STATICLISTS_ID=:STATICLISTS_ID ORDER BY ID';

  procedure LoadSokidListDetail(aParentId: Integer; aParentNode: PVirtualNode);
  var
    Query: TFDQuery;
    Node: PVirtualNode;
    Data: PSokidInfo;
    ContractId: Integer;
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := C_SQL_SELECT_DETAIL_TEXT;
      try
        Query.ParamByName('STATICLISTS_ID').AsInteger := aParentId;
        Query.Prepare;
        Query.Open;
        Query.First;

        while not Query.Eof do
        begin
          ContractId := Query.FieldByName('CONID').AsInteger;
          if SokidList.ContainsKey(ContractId) then
          begin
            Node := vstTree.AddChild(aParentNode);
            Data := Node^.GetData;
            Data^.AssignFrom(SokidList.Items[ContractId]);
            vstTree.InvalidateNode(Node);
          end;
          Query.Next;
        end;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'LoadStaticLists', E.Message + TDModUtils.GetQueryInfo(Query));
          raise;
        end;
      end;
    finally
      FreeAndNil(Query);
    end;
  end;

var
  Node: PVirtualNode;
  Data: PSokidInfo;
  Query: TFDQuery;
begin
  vstTree.BeginUpdate;
  try
    vstTree.Clear;
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := C_SQL_SELECT_TEXT;
      try
        Query.Prepare;
        Query.Open;
        Query.First;
        while not Query.Eof do
        begin
          Node := vstTree.AddChild(nil);
          Data := Node^.GetData;
          Data^.ContractId  := Query.FieldByName('ID').AsInteger;
          Data^.Name        := Query.FieldByName('NAME').AsString;
          Data^.Description := '';
          Data^.Symbol      := '';
          LoadSokidListDetail(Data^.ContractId, Node);
          Query.Next;
        end;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'LoadStaticLists', E.Message + TDModUtils.GetQueryInfo(Query));
          raise;
        end;
      end;
    finally
      FreeAndNil(Query);
    end;
  finally
    vstTree.FullExpand(nil);
    vstTree.EndUpdate;
  end;
end;

function TfrmCandidateDragDropOptions.GetCategoryColumnValue(aColumn: Integer; aData: PSokidInfo): string;
begin
  if aData.Symbol.IsEmpty then
  begin
    case aColumn of
      COL_SL_NAME:
        Result := aData^.Name;
      COL_SL_DESCRIPTION:
        Result := aData^.Description;
    else
      Result := '';
    end;
  end
  else
    case aColumn of
      COL_SL_NAME:
        Result := aData^.Name;
      COL_SL_CONID:
        Result := aData^.ContractId.ToString;
      COL_SL_SYMBOL:
        Result := aData^.Symbol;
      COL_SL_LOCAL_SYMBOL:
        Result := aData^.LocalSymbol;
      COL_SL_CURRENCY:
        Result := aData^.Currency;
      COL_SL_EXCHANGE:
        Result := aData^.Exchange;
      COL_SL_SECURITY_TYPE:
        Result := aData^.SecurityType;
      COL_SL_BROKER:
        Result := aData^.Broker.ToString;
      COL_SL_EXPIRY:
        if (aData^.Expiry > 0) then
          Result := FormatDateTime('DD.MM.YYYY hh:nn', aData^.Expiry)
        else
          Result := '';
      COL_SL_UNDERLYING_CONID:
        if (aData^.UnderlyingConId > 0) then
          Result := aData^.UnderlyingConId.ToString
        else
          Result := '';
      COL_SL_DESCRIPTION:
        Result := aData^.Description;
    else
      Result := '';
    end;
end;

procedure TfrmCandidateDragDropOptions.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmCandidateDragDropOptions.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    CellText := GetCategoryColumnValue(Column, Data);
end;

procedure TfrmCandidateDragDropOptions.vstTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Data^.Symbol.IsEmpty then
    TargetCanvas.Font.Style := Font.Style + [fsBold];
end;

procedure TfrmCandidateDragDropOptions.SetFocusedNode(aStaticListId: Integer);
var
  Node : PVirtualNode;
  Data: PSokidInfo;
begin
  vstTree.ClearSelection;
  Node := vstTree.GetFirstChild(nil);
  while Assigned(Node) do //and (vstTree.GetNodeLevel(Node) > 0) do
  begin
    Data := Node^.GetData;
    if (Data^.ContractId = aStaticListId) then
    begin
      vstTree.FocusedNode := Node;
      vstTree.Selected[Node] := True;
      Exit;
    end;
    Node := Node^.NextSibling;
  end;
end;

function TfrmCandidateDragDropOptions.GetStaticListId: Integer;
var
  Node: PVirtualNode;
  Data: PSokidInfo;
begin
  Result := -1;
  Node := vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if Data^.Symbol.IsEmpty then
      Result := Data^.ContractId
    else if Assigned(Node^.Parent) then
    begin
      Data := Node^.Parent.GetData;
      if Data^.Symbol.IsEmpty then
        Result := Data^.ContractId
    end;
  end;
end;

procedure TfrmCandidateDragDropOptions.edtSearchListsChange(Sender: TObject);

  function IsChildrenVisible(const aNode: PVirtualNode): Boolean;
  var
    ChildNode: PVirtualNode;
  begin
    Result := False;
    ChildNode := aNode.FirstChild;
    while Assigned(ChildNode) do
      if vstTree.IsVisible[ChildNode] then
        Exit(True)
      else
        ChildNode := ChildNode.NextSibling;
  end;

var
  Node: PVirtualNode;
  Data: PSokidInfo;
  arr: TNodeArray;
begin
  inherited;
  vstTree.BeginUpdate;
  try
    arr := TTreeDocument.GetNodesList(vstTree.RootNode);
    for Node in arr do //Instruments
      if Assigned(Node) and (Node <> vstTree.RootNode) then
      begin
        Data := Node^.GetData;
        if not Data^.Symbol.IsEmpty then
          vstTree.IsVisible[Node] := string(edtSearchLists.Text).IsEmpty or (Pos(string(edtSearchLists.Text).ToUpper, Data^.Name.ToUpper) > 0);
      end;
    for Node in arr do //Groups
      if Assigned(Node) and (Node <> vstTree.RootNode) then
      begin
        Data := Node^.GetData;
        if Data^.Symbol.IsEmpty then
          vstTree.IsVisible[Node] := string(edtSearchLists.Text).IsEmpty or IsChildrenVisible(Node);
      end;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmCandidateDragDropOptions.btnClearSearchTextClick(Sender: TObject);
begin
  edtSearchLists.Text := '';
end;

end.
