unit DockForm.MonitorFilter;

interface

{$REGION 'Region uses'}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Generics.Collections, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls, VirtualTrees, IABSocketAPI,
  Vcl.ImgList, Winapi.ActiveX, BrokerHelperAbstr, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.StdCtrls,
{$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const, Document, DaImages, System.ImageList,
  InstrumentList, Utils, IABFunctions, DebugWriter, Global.Types, MessageDialog, Monitor.Interfaces, Common.Types,
  CustomDockForm, System.UITypes, IABFunctions.MarketData, Vcl.ComCtrls, System.Math, Publishers, Monitor.Types,
  Edit.OrderGroup, Edit.Condition, Edit.Algos, Edit.OrderDocument, Edit.Factor, Qualifiers.Types, Qualifiers.Edit,
  AutoTrades.Edit, Edit.OrderGroupSet, InformationDialog, VirtualTrees.ExportHelper, Monitor.Info, IABFunctions.Helpers,
  System.Types, MonitorTree.Helper, MonitorTree.Document, Vcl.CheckLst, Global.Resources,
  MonitorTree.EditDocuments, AutoTrades.Types, HtmlConsts, Publishers.Interfaces, NNfunctions.Types, VirtualTrees.Types;
{$ENDREGION}

type
  TfrmMonitorFilter = class(TfrmCustomDockForm, IUpdateFeeds,
                                                IOrderState,
                                                IError,
                                                IMonitorStructureChange)
    aClearTree: TAction;
    aEditDocument: TAction;
    aExportToHTML: TAction;
    aFilter: TAction;
    aUncheckAll: TAction;
    btnClearTree: TBitBtn;
    btnEditDocument: TBitBtn;
    btnExportToHTML: TBitBtn;
    btnFilter: TBitBtn;
    btnUncheckAll: TBitBtn;
    cbAutomaticUpdate: TCheckBox;
    cbDocumentTypes: TCheckListBox;
    cbOptions: TComboBox;
    lblOptions: TLabel;
    pnlFilter: TPanel;
    timerMonitorUpdate: TTimer;
    procedure aClearTreeExecute(Sender: TObject);
    procedure aEditDocumentExecute(Sender: TObject);
    procedure aExportToHTMLExecute(Sender: TObject);
    procedure aFilterExecute(Sender: TObject);
    procedure aUncheckAllExecute(Sender: TObject);
    procedure cbOptionsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure timerMonitorUpdateTimer(Sender: TObject);
    procedure vstTreeAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstTreeChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDblClick(Sender: TObject);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure vstTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private type
    TGroupOption = (goNone, goDocumentType, goDocumentStatus);
  private const
    C_IDENTITY_NAME = 'DockFormMonitorFilter';
    C_KEY_OPTIONS   = 'OptionsItemIndex';
    C_KEY_DOC_TYPES = 'DocumentTypes';
    GroupOptionName: array [TGroupOption] of string = ('None', 'Documents Type', 'Documents Status');

    COL_ITEMS            = 0;
    COL_VALUE            = 1;
    COL_CALCTYPE         = 2;
    COL_LAST_CLOSE       = 3;
    COL_NODE_ID          = 4;

    C_FIXED_COLUMN_INDEX = 5;
  private
    FDependenceList: TObjectDictionary<PVirtualNode, PVirtualNode>;
    FGroupList: TStringList;
    FGroupOption : TGroupOption;
    FMonitor: IMonitor;
    FMonitorTree: TVirtualStringTree;
    procedure LoadParamsFromXml;
    procedure SaveParamsToXml;
    procedure ProcessForFilter(Sender: TBaseVirtualTree; MonitorNode: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure SyncronizeNode(TempId: Integer);

    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IOrderState
    procedure OnCloseOrder(const aTempId: Integer);
    procedure OnExecution(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrder(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrderNN(const aOrderList: array of TOrder);
    procedure OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
    procedure OnRebuildFromTWS(Sender: TObject);
    //IMonitorStructureChange
    procedure OnMonitorStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
    //implementation IError
    procedure OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure Deinitialize; override;
  end;

var
  frmMonitorFilter: TfrmMonitorFilter;

implementation

{$R *.dfm}

procedure TfrmMonitorFilter.FormCreate(Sender: TObject);
begin
  vstTree.NodeDataSize := SizeOf(TTreeData);
  inherited;
  FMonitorTree := nil;
  FGroupList := TStringList.Create;
  FGroupList.Duplicates := dupIgnore;
  FGroupList.Sorted := True;

  FDependenceList := TObjectDictionary<PVirtualNode, PVirtualNode>.Create;
end;

procedure TfrmMonitorFilter.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FDependenceList);
  FreeAndNil(FGroupList);
  frmMonitorFilter := nil;
end;

function TfrmMonitorFilter.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

function TfrmMonitorFilter.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmMonitorFilter.Initialize;
var
  Column: TVirtualTreeColumn;
begin
  inherited;
  if not Supports(Application.MainForm, IMonitor, FMonitor) then
    raise Exception.Create(rsNotSupportsIMonitor)
  else
    FMonitorTree := FMonitor.GetMainTree;
  Caption := 'Filter';

  cbOptions.Items.Clear;
  for var go := Low(TGroupOption) to High(TGroupOption) do
    cbOptions.Items.Add(GroupOptionName[go]);
  cbOptions.ItemIndex := -1;

  cbDocumentTypes.Items.Clear;
  for var dt := Low(TDocType) to ntFactor do
    cbDocumentTypes.Items.Add(dt.ToString);
  LoadParamsFromXml;

  vstTree.BeginUpdate;
  try
    for var TickType := Low(TIABTickType) to ttETFNavLow do
    begin
      Column := vstTree.Header.Columns.Add;
      Column.Text             := TickType.ToString;
      Column.Tag              := Integer(TickType);
      Column.Options          := Column.Options - [coEditable];
      Column.CaptionAlignment := taCenter;
      Column.Alignment        := taRightJustify;
      Column.Width            := 80;
      Column.Options          := Column.Options - [coVisible]
    end;
    TStoreHelper.LoadFromXml(vstTree, C_IDENTITY_NAME + '.'  + vstTree.Name);
  finally
    vstTree.EndUpdate;
  end;

  TPublishers.ErrorPublisher.Subscribe(Self);
  TPublishers.FeedPublisher.Subscribe(Self);
  TPublishers.OrderStatePublisher.Subscribe(Self);
  TPublishers.MonitorStructureChangePublisher.Subscribe(Self);
end;

procedure TfrmMonitorFilter.Deinitialize;
begin
  inherited;
  SaveParamsToXml;
  TPublishers.ErrorPublisher.Unsubscribe(Self);
  TPublishers.FeedPublisher.Unsubscribe(Self);
  TPublishers.OrderStatePublisher.Unsubscribe(Self);
  TPublishers.MonitorStructureChangePublisher.Unsubscribe(Self);
  TStoreHelper.SaveToXml(vstTree, C_IDENTITY_NAME + '.'  + vstTree.Name);
end;

procedure TfrmMonitorFilter.LoadParamsFromXml;
var
  SelectedItems: string;
begin
  cbOptions.ItemIndex := General.XMLFile.ReadInteger(C_IDENTITY_NAME, C_KEY_OPTIONS, 0);
  if (cbOptions.ItemIndex > -1) then
    FGroupOption := TGroupOption(cbOptions.ItemIndex);
  SelectedItems := General.XMLFile.ReadString(C_IDENTITY_NAME, C_KEY_DOC_TYPES, '');
  for var i := 0 to cbDocumentTypes.Items.Count - 1 do
    cbDocumentTypes.Checked[i] := SelectedItems.Contains(i.ToString);
end;

procedure TfrmMonitorFilter.SaveParamsToXml;
var
  SelectedItems: string;
begin
  General.XMLFile.WriteInteger(C_IDENTITY_NAME, C_KEY_OPTIONS, cbOptions.ItemIndex);

  SelectedItems := '';
  for var i := 0 to cbDocumentTypes.Items.Count - 1 do
    if cbDocumentTypes.Checked[i] then
      SelectedItems := SelectedItems + i.ToString + ',';
  General.XMLFile.WriteString(C_IDENTITY_NAME, C_KEY_DOC_TYPES, SelectedItems);
end;

procedure TfrmMonitorFilter.vstTreeAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
begin
  TMonitorTree.OnAfterCellPaint(Sender, TargetCanvas, Node, Column, CellRect);
end;

procedure TfrmMonitorFilter.vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  inherited;
  TMonitorTree.OnBeforeCellPaint(Sender, TargetCanvas, Node, Column, CellPaintMode, CellRect, ContentRect);
end;

procedure TfrmMonitorFilter.vstTreeChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  MonitorNode: PVirtualNode;
  MonitorData: PTreeData;
  Data: PTreeData;
begin
  inherited;
  if FDependenceList.ContainsKey(Node) then
    if Assigned(FMonitor) then
    begin
      MonitorNode := FDependenceList.Items[Node];
      if Assigned(MonitorNode) and Assigned(FMonitorTree.OnChecking) then
      begin
        FMonitorTree.OnChecking(FMonitorTree, MonitorNode, NewState, Allowed);
        MonitorNode.CheckState := NewState;
        MonitorData := MonitorNode^.GetData;
        Data := Node.GetData;
        Data^.ImageNo := MonitorData^.ImageNo;
      end;
    end;
end;

procedure TfrmMonitorFilter.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  CellText1: string;
  CellText2: string;
begin
  TMonitorTree.OnGetText(Sender, Node1, Column, ttNormal, CellText1);
  TMonitorTree.OnGetText(Sender, Node2, Column, ttNormal, CellText2);
  Result := CompareText(CellText1, CellText2);
end;

procedure TfrmMonitorFilter.vstTreeDblClick(Sender: TObject);
var
  bIsExpanded: Boolean;
begin
  inherited;
  if Assigned(vstTree.FocusedNode) then
  begin
    bIsExpanded := (vsExpanded in vstTree.FocusedNode.States);
    aEditDocumentExecute(nil);
    vstTree.Expanded[vstTree.FocusedNode] := not bIsExpanded;
  end;
end;

procedure TfrmMonitorFilter.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PTreeData;
  TickType: TIABTickType;
  DocType: TDocType;
begin
  inherited;
  if (Column <= COL_CALCTYPE) then
    TMonitorTree.OnDrawText(Sender, TargetCanvas, Node, Column, Text, CellRect, DefaultDraw)
  else if (Column = COL_LAST_CLOSE) then
  begin
    if (StrToFloatDef(vstTree.Text[Node, Column], 0) >= 0) then
      TargetCanvas.Font.Color := clGreen
    else
      TargetCanvas.Font.Color := clRed;
  end
  else if (Column >= C_FIXED_COLUMN_INDEX) then
  begin
    Data := Node^.GetData;
    DocType := TTreeDocument.GetDocType(Sender, Node);
    TickType := TIABTickType(vstTree.Header.Columns[Column].Tag);
    case DocType of
      ntOrder:
        begin
          if (TickType = ttLast) then
            TargetCanvas.Font.Color := Data^.OrderDoc.ColTick
          else
            TargetCanvas.Font.Color := clBlack;
        end;
      ntFactor:
        case TickType of
//          ttLast:
//            TargetCanvas.Font.Color := Data^.FactorDoc.ColTick;
          TIABTickType.ttAsk, ttAskSize:
            TargetCanvas.Font.Color := clMaroon;
          ttBid, ttBidSize:
            TargetCanvas.Font.Color := clNavy;
        else
          TargetCanvas.Font.Color := clBlack;
        end;
    end;
  end;
end;

procedure TfrmMonitorFilter.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeData;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmMonitorFilter.vstTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
  inherited;
  TMonitorTree.OnGetHint(Sender, Node, Column, LineBreakStyle, HintText);
end;

procedure TfrmMonitorFilter.vstTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  inherited;
  TMonitorTree.OnGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);
end;

procedure TfrmMonitorFilter.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

  function GetLastClose(aConId: Integer): string;
  var
    ClosePrice: Double;
  begin
    if (aConId > 0) then
    begin
      ClosePrice := TMonitorLists.PriceCache.GetLastPrice(aConId, ttClose);
      if (ClosePrice <> 0) then
        ClosePrice := (TMonitorLists.PriceCache.GetLastPrice(aConId, ttLast) / ClosePrice - 1) * 100;
      Result := Format('%0.2f', [ClosePrice]);
    end
    else
     Result :='';
  end;

var
  Data: PTreeData;
  TickType: TIABTickType;
  DocType: TDocType;
  Id: Integer;
begin
  inherited;
  if (Column <= COL_CALCTYPE) then
    TMonitorTree.OnGetText(Sender, Node, Column, TextType, CellText)
  else
  begin
    Data := Node^.GetData;
    DocType := TTreeDocument.GetDocType(Sender, Node);
    case DocType of
      ntOrder:
        Id := Data.OrderDoc.Id;
      ntFactor:
        Id := Data.FactorDoc.ContractId;
    else
      Id := 0;
    end;

    if (Column = COL_NODE_ID) then
    begin
      CellText := Data.NodeId.ToString;
      if (DocType = ntOrder) then
        case Data^.OrderDoc.ExtendedOptions.Subordination of
          suMotherOrder:
            CellText := 'm ' + CellText;
          suChildOrder:
            CellText := 'c ' + CellText;
        end;
    end
    else if (Column = COL_LAST_CLOSE) then
      CellText := GetLastClose(Id)
    else if (Column >= C_FIXED_COLUMN_INDEX) and (Id > 0) then
    begin
      TickType := TIABTickType(vstTree.Header.Columns[Column].Tag);
      CellText := Format(TickType.ToFormat, [TMonitorLists.PriceCache.GetLastPrice(Id, TickType)]);
    end
    else
      CellText := '0.00';
  end;
end;

procedure TfrmMonitorFilter.vstTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Node: PVirtualNode;
begin
  if (Key = VK_UP) and (ssShift in Shift) then
  begin
    Node := TVirtualStringTree(Sender).FocusedNode;
    if Assigned(Node) then
      TVirtualStringTree(Sender).FullCollapse(Node);
  end;
end;

procedure TfrmMonitorFilter.aClearTreeExecute(Sender: TObject);
begin
  inherited;
  vstTree.BeginUpdate;
  try
    vstTree.Clear;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmMonitorFilter.aEditDocumentExecute(Sender: TObject);
begin
  inherited;
  TTreeDocumentEdit.Edit(vstTree, [tuMonitor], False);
end;

procedure TfrmMonitorFilter.aExportToHTMLExecute(Sender: TObject);
var
  html: string;
begin
  inherited;
  html := DateTimeToStr(Now) + C_HTML_BREAK + vstTree.ContentToHTML(tstSelected);
  TInformationDialog.ShowMessage(html, GetIdentityName);
end;

procedure TfrmMonitorFilter.ProcessForFilter(Sender: TBaseVirtualTree; MonitorNode: PVirtualNode; Data: Pointer; var Abort: Boolean);

  function GetDocTypes: TDocTypes;
  begin
    Result := [];
    for var i := 0 to cbDocumentTypes.Items.Count - 1 do
      if cbDocumentTypes.Checked[i] then
        Include(Result, TDocType(i));
  end;

  function GetGroupNode(aData: PTreeData): PVirtualNode;
  var
    GroupData: PTreeData;
    CurrData: PTreeData;
    Index: Integer;
    GroupParent: PVirtualNode;
    CurrNode: PVirtualNode;
  begin
    Result := nil;
    case FGroupOption of
      goNone:
        Result := nil;
      goDocumentType:
        begin
          Index := FGroupList.IndexOf(aData^.DocType.ToString);
          if (Index < 0) then
          begin
            Result := vstTree.AddChild(nil);
            GroupData := Result^.GetData;
            GroupData^.Caption := aData.DocType.ToString;
            GroupData^.CreationType := TCreationType.ctMirror;
            GroupData^.ImageNo := -1;

            Index := FGroupList.Add(GroupData^.Caption);
            FGroupList.Objects[Index] := TObject(Result);
          end
          else
            Result := PVirtualNode(FGroupList.Objects[Index]);
        end;
      goDocumentStatus:
        begin
          Index := FGroupList.IndexOf(aData^.DocType.ToString);
          if (Index < 0) then
          begin
            GroupParent := vstTree.AddChild(nil);
            GroupData := GroupParent^.GetData;
            GroupData^.Caption := aData.DocType.ToString;
            GroupData^.CreationType := TCreationType.ctMirror;
            GroupData^.ImageNo := -1;

            Index := FGroupList.Add(GroupData^.Caption);
            FGroupList.Objects[Index] := TObject(GroupParent);
          end
          else
            GroupParent := PVirtualNode(FGroupList.Objects[Index]);

          CurrNode := GroupParent.FirstChild;
          while Assigned(CurrNode) do
          begin
            CurrData := CurrNode^.GetData;
            if (TTreeDocument.GetDocStatus(CurrData) = TTreeDocument.GetDocStatus(aData)) then
            begin
              Result := CurrNode;
              Break;
            end;
            CurrNode := CurrNode.NextSibling;
          end;

          if not Assigned(Result) then
          begin
            Result := vstTree.AddChild(GroupParent);
            GroupData := Result^.GetData;
            GroupData^.Caption := TTreeDocument.GetDocStatus(aData);
            GroupData^.CreationType := TCreationType.ctMirror;
            GroupData^.ImageNo := -1;
          end;
        end;
    end;
  end;

var
  DocTypes: TDocTypes;
  NewNode: PVirtualNode;
  GroupNode: PVirtualNode;
  MonitorData: PTreeData;
  NewData: PTreeData;
begin
  if Assigned(MonitorNode) then
  begin
    DocTypes   := GetDocTypes;
    MonitorData := MonitorNode^.GetData;
    if (MonitorData.DocType in DocTypes) or (DocTypes = []) then
    begin
      GroupNode := GetGroupNode(MonitorData);
      NewNode := vstTree.AddChild(GroupNode);
      NewNode.CheckType  := MonitorNode.CheckType;
      NewNode.CheckState := MonitorNode.CheckState;
      NewData := NewNode.GetData;
      NewData^.AssignFrom(MonitorData^);
      if Assigned(MonitorData^.OrderDoc) then
      begin
        NewData^.OrderDoc.OrderStatus := MonitorData^.OrderDoc.OrderStatus;
        NewData^.OrderDoc.OrderIBId   := MonitorData^.OrderDoc.OrderIBId;
      end;
      NewData^.NodeId := MonitorData^.NodeId;
      FDependenceList.AddOrSetValue(NewNode, MonitorNode);
    end;
  end;
end;

procedure TfrmMonitorFilter.aFilterExecute(Sender: TObject);
begin
  inherited;
  if Assigned(FMonitorTree) then
  begin
    FMonitorTree.BeginUpdate;
    FGroupList.Clear;
    FDependenceList.Clear;
    vstTree.BeginUpdate;
    try
      vstTree.Clear;
      FMonitorTree.IterateSubtree(nil, ProcessForFilter, nil, [], True);
    finally
      vstTree.FullExpand;
      vstTree.SortTree(vstTree.Header.SortColumn, vstTree.Header.SortDirection);
      vstTree.EndUpdate;
      FMonitorTree.EndUpdate;
    end;
  end;
end;

procedure TfrmMonitorFilter.aUncheckAllExecute(Sender: TObject);
begin
  inherited;
  for var i := 0 to cbDocumentTypes.Items.Count - 1 do
    cbDocumentTypes.Checked[i] := False;
end;

procedure TfrmMonitorFilter.cbOptionsChange(Sender: TObject);
begin
  inherited;
  FGroupOption := goNone;
  if (cbOptions.ItemIndex > -1) then
    FGroupOption := TGroupOption(cbOptions.ItemIndex);
end;

procedure TfrmMonitorFilter.OnMonitorStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
begin
  if cbAutomaticUpdate.Checked then
    timerMonitorUpdate.Enabled := True;
end;

procedure TfrmMonitorFilter.timerMonitorUpdateTimer(Sender: TObject);
begin
  inherited;
  timerMonitorUpdate.Enabled := False;
  aFilterExecute(Sender);
end;

procedure TfrmMonitorFilter.SyncronizeNode(TempId: Integer);
var
  MonitorNode: PVirtualNode;
  MonitorData: PTreeData;
  Node: PVirtualNode;
  Data: PTreeData;
begin
  MonitorNode := TMonitorLists.OrderList.GetNodeOrder(TempId);
  if Assigned(MonitorNode) then
    for var pair in FDependenceList do
      if (pair.Value = MonitorNode) then
      begin
        MonitorData := MonitorNode^.GetData;
        Node := pair.Key;
        Node^.CheckType  := MonitorNode^.CheckType;
        Node^.CheckState := MonitorNode^.CheckState;
        Data := Node.GetData;
        Data^.AssignFrom(MonitorData^);
        Data^.OrderDoc.OrderStatus     := MonitorData^.OrderDoc.OrderStatus;
        Data^.OrderDoc.OrderIBId       := MonitorData^.OrderDoc.OrderIBId;
        Data^.OrderDoc.Filled          := MonitorData^.OrderDoc.Filled;
        Data^.OrderDoc.LatestFillPrice := MonitorData^.OrderDoc.LatestFillPrice;
        Data^.OrderDoc.AvgPrice        := MonitorData^.OrderDoc.AvgPrice;
        vstTree.InvalidateNode(Node);
        Break;
      end;
end;

procedure TfrmMonitorFilter.OnCloseOrder(const aTempId: Integer);
begin
  if (aTempId > 0) then
    SyncronizeNode(aTempId);
end;

procedure TfrmMonitorFilter.OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
begin
  if (TempId > 0) then
    SyncronizeNode(TempId);
end;

procedure TfrmMonitorFilter.OnExecution(Sender: TObject; Order: TIABOrder);
begin
  if (Order.TempId > 0) then
    SyncronizeNode(Order.TempId);
end;

procedure TfrmMonitorFilter.OnOpenOrder(Sender: TObject; Order: TIABOrder);
begin
  if (Order.TempId > 0) then
    SyncronizeNode(Order.TempId);
end;

procedure TfrmMonitorFilter.OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
begin
  if (Order.TempId > 0) then
    SyncronizeNode(Order.TempId);
end;

procedure TfrmMonitorFilter.OnOpenOrderNN(const aOrderList: array of TOrder);
begin
  vstTree.Invalidate;
end;

procedure TfrmMonitorFilter.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
begin
  vstTree.Invalidate;
end;

procedure TfrmMonitorFilter.OnRebuildFromTWS(Sender: TObject);
begin

end;

end.
