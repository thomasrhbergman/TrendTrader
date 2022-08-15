unit Scanner.FilterList;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids, System.Actions,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.Mask, Vcl.ImgList, System.Math, Vcl.DBCtrls, System.ImageList,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Winapi.ActiveX, System.UITypes, VirtualTrees, BrokerHelperAbstr,
  DebugWriter, HtmlLib, Scanner.Types, CustomForms, DaImages, Monitor.Types, MonitorTree.Helper;
{$ENDREGION}

type
  TfrmScannerFilterList = class(TCustomForm)
    aCancel: TAction;
    ActionListMain: TActionList;
    aSave: TAction;
    btnAddColumn: TBitBtn;
    btnCancel: TBitBtn;
    btnClearSearchText: TBitBtn;
    edtAvailableFilters: TEdit;
    lblAvailableFilters: TLabel;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    vstFilter: TVirtualStringTree;
    procedure aCancelExecute(Sender: TObject);
    procedure btnClearSearchTextClick(Sender: TObject);
    procedure edtAvailableFiltersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vstFilterChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFilterCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstFilterFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFilterGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstFilterInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private const
    COL_NAME = 0;
  private
    FFilterList: PArrayFilterData;
    procedure LoadDetailList;
  public
    class function ShowDocument(aArrFilterList: PArrayFilterData): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmScannerFilterList.ShowDocument(aArrFilterList: PArrayFilterData): TModalResult;
begin
  with TfrmScannerFilterList.Create(nil) do
    try
      FFilterList := aArrFilterList;
      Initialize;
      Result := ShowModal;
      if (Result = mrOk) then
        Denitialize;
    finally
      Free;
    end;
end;

procedure TfrmScannerFilterList.FormCreate(Sender: TObject);
begin
  vstFilter.NodeDataSize := SizeOf(TFilterData);
  vstFilter.CheckImageKind := ckCustom;
  vstFilter.CustomCheckImages := DMImage.ilCustomCheckImages;
end;

procedure TfrmScannerFilterList.Initialize;
begin
  TMonitorTree.Initialize(vstFilter);
  if (FFilterList^.Size > 0) then
    LoadDetailList;
end;

procedure TfrmScannerFilterList.Denitialize;
var
  Data: PFilterData;
  Node: PVirtualNode;
  i: Integer;
begin
  vstFilter.BeginUpdate;
  try
    Node := vstFilter.GetFirst;
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      for i := Low(FFilterList^.Items) to High(FFilterList^.Items) do
        if (FFilterList^[i].Id = Data^.Id) then
        begin
          FFilterList^.Items[i].Checked := Data^.Checked;
          Break;
        end;
      Node := Node.NextSibling;
    end;
  finally
    vstFilter.EndUpdate;
  end;
end;

procedure TfrmScannerFilterList.edtAvailableFiltersChange(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PFilterData;
begin
  inherited;
  vstFilter.BeginUpdate;
  try
    Node := vstFilter.GetFirst;
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      Node^.States := Node^.States + [vsFiltered];
      if (edtAvailableFilters.Text = '') or
        (Pos(UpperCase(edtAvailableFilters.Text), UpperCase(Data^.DisplayName)) > 0) then
        Node^.States := Node^.States - [vsFiltered];
      Node := Node.NextSibling;
    end;
  finally
    vstFilter.SortTree(vstFilter.Header.SortColumn, vstFilter.Header.SortDirection);
    vstFilter.EndUpdate;
  end;
end;

procedure TfrmScannerFilterList.vstFilterChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PFilterData;
begin
  Data := Node^.GetData;
  Data^.Checked := Node.CheckState = csCheckedNormal;
end;

procedure TfrmScannerFilterList.vstFilterCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PFilterData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  case Column of
    COL_NAME:
      Result := CompareText(Data1^.DisplayName, Data2^.DisplayName);
  end;
end;

procedure TfrmScannerFilterList.vstFilterFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PFilterData;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmScannerFilterList.vstFilterGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PFilterData;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    COL_NAME:
      CellText := Data^.DisplayName;
  else
    CellText := '';
  end;
end;

procedure TfrmScannerFilterList.vstFilterInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node.CheckType := ctCheckBox;
end;

procedure TfrmScannerFilterList.LoadDetailList;
var
  Data: PFilterData;
  DisplayName : string;
  i, j: Integer;
  Node: PVirtualNode;
begin
  for i := Low(FFilterList^.Items) to High(FFilterList^.Items) do
  begin
    for j := Low(FFilterList^[i].AbstractField) to High(FFilterList^[i].AbstractField) do
    begin
      DisplayName := FFilterList^[i].AbstractField[j].DisplayName.Replace('Above', '').Replace('Below', '').TrimRight;
      if not DisplayName.IsEmpty then
        Break;
    end;
    if DisplayName.IsEmpty then
      for j := Low(FFilterList^[i].Columns) to High(FFilterList^[i].Columns) do
      begin
        DisplayName := FFilterList^[i].Columns[j].Name.Replace('Above', '').Replace('Below', '').TrimRight;
        if not FFilterList^[i].DisplayName.IsEmpty then
          Break;
      end;

    if not ((FFilterList^[i].Id = 'PRICE') or (FFilterList^[i].Id = 'VOLUME')) and (not DisplayName.IsEmpty) then
    begin
      Node := vstFilter.AddChild(nil);
      Data := Node^.GetData;
      Data.Assign(FFilterList^.Items[i]);
      Data^.Checked := FFilterList^.Items[i].Checked;
      Data^.DisplayName := DisplayName;

      if Data^.Checked then
        Node.CheckState := csCheckedNormal
      else
        Node.CheckState := csUncheckedNormal;
    end;
  end;
end;

procedure TfrmScannerFilterList.btnClearSearchTextClick(Sender: TObject);
begin
  edtAvailableFilters.Text := '';
end;

procedure TfrmScannerFilterList.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrClose;
end;

end.
