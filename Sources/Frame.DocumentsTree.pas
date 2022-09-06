unit Frame.DocumentsTree;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, System.StrUtils, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons,
  Vcl.ExtCtrls, System.Threading, System.Generics.Collections, System.Generics.Defaults, System.Actions, Vcl.ActnList,
  System.IniFiles, System.Math, Vcl.Menus, Vcl.ExtDlgs, Vcl.WinXCtrls, Winapi.ActiveX, DebugWriter, CustomForms,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Vcl.Printers, Data.DB, Vcl.Grids,
  Vcl.DBGrids, Global.Types, IABSocketAPI_const, IABSocketAPI, InstrumentList, BrokerHelperAbstr, DaModule, Document,
  MessageDialog, Monitor.Types, DaImages, Edit.OrderGroup, Edit.Condition, Edit.Algos, Edit.OrderDocument, Edit.Factor,
  Qualifiers.Types, Qualifiers.Edit, AutoTrades.Edit, Edit.OrderGroupSet, Vcl.ComCtrls, InformationDialog, Common.Types,
  VirtualTrees.ExportHelper, Monitor.Info, System.Types, MonitorTree.Helper, MonitorTree.Document,
  MonitorTree.EditDocuments, FireDAC.Comp.Client, AutoTrades.Types;
{$ENDREGION}

type
  TframeDocumentsTree = class(TFrame)
    aAddAlgos: TAction;
    aAddCondition: TAction;
    aAddFactor: TAction;
    aAddOrder: TAction;
    aAddOrderGroup: TAction;
    aCollapsSelectedNode: TAction;
    aCollapsTree: TAction;
    aDeleteSelectedNode: TAction;
    aExpandSelectedNode: TAction;
    aExpandTree: TAction;
    aExportToCSV: TAction;
    aExportToXLS: TAction;
    alTree: TActionList;
    aModify: TAction;
    aPrint: TAction;
    aShowInformationDialog: TAction;
    btnClearSearchText: TBitBtn;
    btnExportToCSVTemplate: TBitBtn;
    btnExportToXLS: TBitBtn;
    btnPrintTemplate: TBitBtn;
    edtSearch: TEdit;
    lblSearchFor: TLabel;
    lblViewType: TLabel;
    miAddAlgos: TMenuItem;
    miAddCondition: TMenuItem;
    miAddFactor: TMenuItem;
    miAddOrder: TMenuItem;
    miAddOrderGroup: TMenuItem;
    miCollapsSelectedNode: TMenuItem;
    miCollapsTree: TMenuItem;
    miDeleteSelectedNode: TMenuItem;
    miExpandSelectedNode: TMenuItem;
    miExpandTree: TMenuItem;
    miModify: TMenuItem;
    miSeparator01: TMenuItem;
    miSeparator02: TMenuItem;
    miSeparator03: TMenuItem;
    miShowInformationDialog: TMenuItem;
    pmTree: TPopupMenu;
    pnlDescription: TPanel;
    pnlOrderGroupSet: TPanel;
    pnlOrderGroupSetTop: TPanel;
    rbFlat: TRadioButton;
    rbTree: TRadioButton;
    vstTree: TVirtualStringTree;
    procedure aAddAlgosExecute(Sender: TObject);
    procedure aAddAlgosUpdate(Sender: TObject);
    procedure aAddConditionExecute(Sender: TObject);
    procedure aAddConditionUpdate(Sender: TObject);
    procedure aAddFactorExecute(Sender: TObject);
    procedure aAddFactorUpdate(Sender: TObject);
    procedure aAddOrderExecute(Sender: TObject);
    procedure aAddOrderGroupExecute(Sender: TObject);
    procedure aAddOrderGroupUpdate(Sender: TObject);
    procedure aAddOrderUpdate(Sender: TObject);
    procedure aCollapsSelectedNodeExecute(Sender: TObject);
    procedure aCollapsTreeExecute(Sender: TObject);
    procedure aDeleteSelectedNodeExecute(Sender: TObject);
    procedure aExpandSelectedNodeExecute(Sender: TObject);
    procedure aExpandSelectedNodeUpdate(Sender: TObject);
    procedure aExpandTreeExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToXLSExecute(Sender: TObject);
    procedure aModifyExecute(Sender: TObject);
    procedure aModifyUpdate(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aShowInformationDialogExecute(Sender: TObject);
    procedure btnClearSearchTextClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure pmTreePopup(Sender: TObject);
    procedure rbFlatClick(Sender: TObject);
    procedure vstTreeAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDblClick(Sender: TObject);
    procedure vstTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vstTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure vstTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vstTreeMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
  private type
    TGroupOption = (goNone, goGroupType);
    TGroupView = (gvFlat, gvTree);
  private const
    COL_ITEMS     = 0;
    COL_VALUE     = 1;
    COL_CALCTYPE  = 2;
    COL_RECORD_ID = 3;

    C_IDENTITY_NAME = 'frameTree';
  private
    FDocType      : TDocType;
    FDragAllowed  : Boolean;
    FGroupList    : TStringList;
    FGroupOption  : TGroupOption;
    FGroupSetList : TDictionary<Integer, PVirtualNode>;
    FIDs          : TArray<Integer>;
    FMarkedNode   : TMarkedNode;
    FReadOnly     : Boolean;
    FTypeUse      : TTypeUseInAutoorders;
    function GetDescription: string;
    function GetGroupView: TGroupView;
    function GetSQLText(aIDs: TArray<Integer>; aSQL: string): string;
    procedure AddNodesFromTree(aTargetNode: PVirtualNode; aSourceTree: TVirtualStringTree);
    procedure LoadAutoTrades;
    procedure LoadFromXml(aTree: TVirtualStringTree);
    procedure LoadOrderGroupSet;
    procedure LoadQualifiers;
    procedure Print(aTree: TVirtualStringTree);
    procedure SaveToXml(aTree: TVirtualStringTree);
    procedure SearchForText(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure SetDescription(const Value: string);
    procedure SetFlatTree;
    procedure SetGroupOption(const Value: TGroupOption);
    procedure SetGroupTree(aNode: PVirtualNode);
    procedure SetGroupView(const Value: TGroupView);
    procedure SetMarkedNode(const Value: TMarkedNode);
  public
    function GetHigherNodeId(aDocType: TDocType): TRelationRecord;
    procedure Deinitialize;
    procedure Initialize;
    procedure LoadTree;
    procedure ShowInformationDialog;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Description : string               read GetDescription write SetDescription;
    property DocType     : TDocType             read FDocType       write FDocType;
    property DragAllowed : Boolean              read FDragAllowed   write FDragAllowed;
    property GroupOption : TGroupOption         read FGroupOption   write SetGroupOption;
    property GroupView   : TGroupView           read GetGroupView   write SetGroupView;
    property IDs         : TArray<Integer>      read FIDs           write FIDs;
    property MarkedNode  : TMarkedNode          read FMarkedNode    write SetMarkedNode;
    property ReadOnly    : Boolean              read FReadOnly      write FReadOnly;
    property TypeUse     : TTypeUseInAutoorders read FTypeUse       write FTypeUse;
  end;

implementation

{$R *.dfm}

{ TframeDocumentsTree }

constructor TframeDocumentsTree.Create(AOwner: TComponent);
begin
  inherited;
  Name := C_IDENTITY_NAME;
  vstTree.NodeDataSize := SizeOf(TTreeData);

  FGroupList := TStringList.Create;
  FGroupList.Duplicates := dupIgnore;
  FGroupList.Sorted := True;

  FGroupSetList := TDictionary<Integer, PVirtualNode>.Create;
  FGroupOption := goNone;
  FReadOnly := False;
  FDragAllowed := False;
  GroupView := gvFlat;
end;

destructor TframeDocumentsTree.Destroy;
begin
  FreeAndNil(FGroupList);
  FreeAndNil(FGroupSetList);
  inherited;
end;

procedure TframeDocumentsTree.Initialize;
begin
//  TMonitorTree.Initialize(vstTree);
  LoadFromXml(vstTree);
  case DocType of
    ntQualifier:
      Description := '-Qualifiers Listing-';
    ntAutoTrade:
      Description := '-Auto Trades Listing-';
    ntOrderGroupSet:
      Description := '-OrderGroups Listing--';
    ntOrderGroup:
      Description := '-OrderGroup Listing-';
    ntOrder:
      Description := '-Orders Listing-';
    ntCondition:
      Description := '-Conditions Listing-';
    ntAlgos:
      Description := '-Algos Listing-';
    ntFactor:
      Description := '-Factors Listing-';
  end;
  LoadTree;
end;

procedure TframeDocumentsTree.Deinitialize;
begin
  SaveToXml(vstTree);
end;

procedure TframeDocumentsTree.LoadFromXml(aTree: TVirtualStringTree);
begin
  TStoreHelper.LoadFromXml(aTree, C_IDENTITY_NAME + '.' + aTree.Name);
end;

procedure TframeDocumentsTree.SaveToXml(aTree: TVirtualStringTree);
begin
  TStoreHelper.SaveToXml(aTree, C_IDENTITY_NAME + '.' + aTree.Name);
end;

procedure TframeDocumentsTree.LoadTree;
begin
  case DocType of
    ntQualifier:
      LoadQualifiers;
    ntAutoTrade:
      LoadAutoTrades;
    ntOrderGroupSet:
      LoadOrderGroupSet;
    ntOrderGroup:
      ;
    ntOrder:
      ;
    ntCondition:
      ;
    ntAlgos:
      ;
    ntFactor:
      ;
  end;
  if not vstTree.IsEmpty then
    vstTree.SortTree(vstTree.Header.SortColumn, vstTree.Header.SortDirection);
end;

function TframeDocumentsTree.GetSQLText(aIDs: TArray<Integer>; aSQL: string): string;
var
  IDs: string;
begin
  Result := '';
  IDs := '';
  for var i := Low(aIDs) to High(aIDs) do
    if IDs.IsEmpty then
      IDs := aIDs[i].ToString
    else
      IDs := IDs + ',' + aIDs[i].ToString;

  if not IDs.IsEmpty then
    Result := Format(aSQL, ['D.ID IN(' + IDs + ')'])
  else
    Result := Format(aSQL, ['1=1']);
end;

procedure TframeDocumentsTree.LoadAutoTrades;
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT D.ID AS ID, R.ID AS REL_ID ' +
                      '  FROM AUTOTRADES D ' +
                      'LEFT OUTER JOIN DOC_RELATIONS R ON D.ID = R.RECORD_ID AND R.DOC_TYPE = 2 ' +
                      'WHERE %s ' +
                      'ORDER BY D.NAME';
var
  Query: TFDQuery;
  Data: PTreeData;
  Node: PVirtualNode;
begin
  vstTree.BeginUpdate;
  try
    FGroupSetList.Clear;
    FGroupList.Clear;
    vstTree.Clear;
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := GetSQLText(FIDs, C_SQL_SELECT_TEXT);
      Query.Open;
      while not Query.Eof do
      begin
        Node := TTreeDocument.CreateAutoTrade(nil, vstTree);
        if Assigned(Node) then
        begin
          Data := Node^.GetData;
          Data^.AutoTrade.FromDB(Query.FieldByName('ID').AsInteger);
          TTreeDocument.LoadRelationTree(-1, Query.FieldByName('REL_ID').AsInteger, vstTree, Node, nil, nil);
          FGroupSetList.Add(Data^.AutoTrade.RecordId, Node);
          if (GroupView = gvFlat) then
            vstTree.DeleteChildren(Node);
        end;
        Query.Next;
      end;
    finally
      FreeAndNil(Query);
    end;
  finally
//    if Showing then
//      GroupOption := TGroupOption(cbGroup.ItemIndex);
    vstTree.EndUpdate;
  end;
end;

procedure TframeDocumentsTree.LoadQualifiers;
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT D.ID AS ID, R.ID AS REL_ID ' +
                      '  FROM QUALIFIERS D ' +
                      'LEFT OUTER JOIN DOC_RELATIONS R ON D.ID = R.RECORD_ID AND R.DOC_TYPE = 0 ' +
                      'WHERE %s ' +
                      'ORDER BY D.NAME';
var
  Query: TFDQuery;
  Data: PTreeData;
  Node: PVirtualNode;
begin
  vstTree.BeginUpdate;
  try
    FGroupSetList.Clear;
    FGroupList.Clear;
    vstTree.Clear;
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := GetSQLText(FIDs, C_SQL_SELECT_TEXT);
      Query.Open;
      while not Query.Eof do
      begin
        Node := TTreeDocument.LoadRelationTree(Query.FieldByName('ID').AsInteger, -1, vstTree, nil, nil, nil);
        if Assigned(Node) then
        begin
          Data := Node^.GetData;
          FGroupSetList.Add(Data^.Qualifier.RecordId, Node);
          if (GroupView = gvFlat) then
            vstTree.DeleteChildren(Node);
        end;
        Query.Next;
      end;
    finally
      FreeAndNil(Query);
    end;
  finally
//    if Showing then
//      GroupOption := TGroupOption(cbGroup.ItemIndex);
    vstTree.EndUpdate;
  end;
end;

procedure TframeDocumentsTree.LoadOrderGroupSet;
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT D.ID AS ID, R.ID AS REL_ID' +
                      '  FROM ORDER_GROUP_SET D ' +
                      'LEFT OUTER JOIN DOC_RELATIONS R ON D.ID = R.RECORD_ID AND R.DOC_TYPE = 3 ' +
                      'WHERE TYPE_USE IN (TypeUse) AND %s ' +
                      'ORDER BY D.NAME';
var
  Query: TFDQuery;
  Data: PTreeData;
  Node: PVirtualNode;
  TypeUse: string;
  TypeUseInAutoorder: TTypeUseInAutoorder;
begin
  vstTree.BeginUpdate;
  try
    FGroupSetList.Clear;
    FGroupList.Clear;
    vstTree.Clear;
    TypeUse := '';

    if (FTypeUse = []) then
      TypeUse := '1,2,3'
    else
    for TypeUseInAutoorder := Low(TTypeUseInAutoorder) to High(TTypeUseInAutoorder) do
    begin
      if (TypeUseInAutoorder in FTypeUse) then
      begin
        if not TypeUse.IsEmpty then
          TypeUse := TypeUse + ',';
        TypeUse := TypeUse + Ord(TypeUseInAutoorder).ToString;
      end;
    end;

    if not TypeUse.IsEmpty then
    begin
      Query := TFDQuery.Create(nil);
      try
        DMod.CheckConnect;
        Query.Connection := DMod.ConnectionStock;
        Query.SQL.Text := GetSQLText(FIDs, C_SQL_SELECT_TEXT.Replace('TypeUse', TypeUse));
        Query.Open;
        while not Query.Eof do
        begin
          Node := TTreeDocument.CreateOrderGroupSet(nil, vstTree);
          if Assigned(Node) then
          begin
            Data := Node^.GetData;
            Data^.RecordId := -1;
            Data^.OrderGroupSetDoc.FromDB(Query.FieldByName('ID').AsInteger);
            Data^.RelationId := Query.FieldByName('REL_ID').AsInteger;
            FGroupSetList.Add(Data^.OrderGroupSetDoc.RecordId, Node);
            if (GroupView = gvTree) then
              TTreeDocument.LoadRelationTree(Query.FieldByName('REL_ID').AsInteger, Data^.RelationId, vstTree, Node, nil, nil);
          end;
          Query.Next;
        end;
      finally
        FreeAndNil(Query);
      end;
    end;
  finally
//    if Showing then
//      GroupOption := TGroupOption(cbGroup.ItemIndex);
    vstTree.FullExpand;
    vstTree.EndUpdate;
  end;
end;

function TframeDocumentsTree.GetHigherNodeId(aDocType: TDocType): TRelationRecord;
var
  Node: PVirtualNode;
  Data: PTreeData;
begin
  Result.RecordId   := -1;
  Result.RelationId := -1;

  Node := vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    if (TTreeDocument.GetDocType(vstTree, Node) <> aDocType) then
      Node := TTreeDocument.GetParentNode(vstTree, Node, aDocType);
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      Result.RecordId   := Data^.RecordId;
      Result.RelationId := Data^.RelationId;
      Result.DocType    := aDocType;
    end;
  end;
end;

procedure TframeDocumentsTree.pmTreePopup(Sender: TObject);
var
  DocType: TDocType;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    DocType := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode);
    miShowInformationDialog.Caption := 'Get ' + DocType.ToString + ' Info';
    miShowInformationDialog.Visible := True;
    miSeparator02.Visible := not FReadOnly;
  end;
end;

procedure TframeDocumentsTree.vstTreeAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
begin
  TMonitorTree.OnAfterCellPaint(Sender, TargetCanvas, Node, Column, CellRect);
end;

procedure TframeDocumentsTree.vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PTreeData;

  procedure DrawMarkedNode(aColor: TColor);
  var
    MarkedNodeRect: TRect;
  begin
    MarkedNodeRect.Left   := ContentRect.Left - 20;
    MarkedNodeRect.Right  := ContentRect.Right;
    MarkedNodeRect.Bottom := ContentRect.Bottom;
    MarkedNodeRect.Top    := ContentRect.Top;
    TargetCanvas.Brush.Style := bsSolid;
    TargetCanvas.Pen.Width := 1;
    TargetCanvas.Pen.Color := aColor;
    TargetCanvas.Rectangle(MarkedNodeRect);
  end;

begin
  Data := Node^.GetData;
  if FMarkedNode.Equal(Data^) and (Column = COL_ITEMS) then
    DrawMarkedNode(clNavy);

  TMonitorTree.OnBeforeCellPaint(Sender, TargetCanvas, Node, Column, CellPaintMode, CellRect, ContentRect);
end;

procedure TframeDocumentsTree.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PTreeData;
  Data2: PTreeData;
  DocType1: TDocType;
  DocType2: TDocType;
begin
  if (Column = COL_ITEMS) then
  begin
    DocType1 := TTreeDocument.GetDocType(Sender, Node1);
    DocType2 := TTreeDocument.GetDocType(Sender, Node2);
    if (DocType1 = DocType2) then
    begin
      Data1 := Node1^.GetData;
      Data2 := Node2^.GetData;
      case DocType1 of
        ntQualifier:
          Result := CompareText(Data1^.Qualifier.Name, Data2^.Qualifier.Name);
        ntAutoTrade:
          Result := CompareText(Data1^.AutoTrade.Name, Data2^.AutoTrade.Name);
        ntOrderGroupSet:
          Result := CompareText(Data1^.OrderGroupSetDoc.Name, Data2^.OrderGroupSetDoc.Name);
        ntOrderGroup:
          Result := CompareText(Data1^.OrderGroupDoc.Name, Data2^.OrderGroupDoc.Name);
        ntOrder:
          Result := CompareText(Data1^.OrderDoc.Description, Data2^.OrderDoc.Description);
        ntCondition:
          Result := CompareText(Data1^.ConditionDoc.Description, Data2^.ConditionDoc.Description);
        ntAlgos:
          Result := CompareText(Data1^.AlgosDoc.Name, Data2^.AlgosDoc.Name);
        ntFactor:
          Result := CompareText(Data1^.FactorDoc.InstrumentName, Data2^.FactorDoc.InstrumentName);
      end;
    end;
  end
  else if (Column = COL_RECORD_ID) then
  begin
    Data1 := Node1^.GetData;
    Data2 := Node2^.GetData;
    Result := CompareValue(Data1.RecordId, Data2.RecordId);
  end;
end;

procedure TframeDocumentsTree.vstTreeDblClick(Sender: TObject);
var
  bIsExpanded: Boolean;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    bIsExpanded := (vsExpanded in vstTree.FocusedNode.States);
    aModifyExecute(nil);
    vstTree.Expanded[vstTree.FocusedNode] := not bIsExpanded;
  end;
end;

procedure TframeDocumentsTree.vstTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := DragAllowed;
end;

procedure TframeDocumentsTree.vstTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  SourceNode: PVirtualNode;
  TargetNode: PVirtualNode;
  attMode: TVTNodeAttachMode;
begin
  if DragAllowed and not ReadOnly then
  begin
    SourceNode := nil;
    TargetNode := Sender.DropTargetNode;

    if (Sender = Source) then
    begin
      SourceNode := TVirtualStringTree(Source).FocusedNode;
      if (TTreeDocument.GetDocType(TVirtualStringTree(Source), SourceNode) = ntOrder) and
         (TTreeDocument.GetDocType(Sender, TargetNode) = ntOrder) then
      begin
        if vstTree.GetNodeLevel(TargetNode) <= 1 then
          attMode := amAddChildFirst
        else
          attMode := amInsertAfter;
      end
      else if (TTreeDocument.GetDocType(TVirtualStringTree(Source), SourceNode) = ntFactor) and
              (TTreeDocument.GetDocType(Sender, TargetNode) = ntFactor)
      then
      begin
        if (TargetNode.NextSibling = SourceNode) then
          attMode := amInsertBefore
        else
          attMode := amInsertAfter
      end
      else
        attMode := amAddChildFirst;

      Sender.MoveTo(SourceNode, TargetNode, attMode, False);
    end
    else
    begin
      TargetNode := Sender.DropTargetNode;
      AddNodesFromTree(TargetNode, TVirtualStringTree(Source));
    end;

    if Assigned(SourceNode) then
      TTreeDocument.SetIcon(SourceNode, Sender);
  end;
end;

procedure TframeDocumentsTree.AddNodesFromTree(aTargetNode: PVirtualNode; aSourceTree: TVirtualStringTree);
var
  NewNode: PVirtualNode;
  NewData: PTreeData;
  SourceNode: PVirtualNode;
  SourceData: PTreeData;
begin
  NewNode := nil;
  vstTree.BeginUpdate;
  if FDragAllowed then
  try
    SourceNode := aSourceTree.GetFirstSelected;
    SourceData := SourceNode^.GetData;
    case SourceData^.DocType of
      ntQualifier:
        begin
          NewNode := TTreeDocument.CreateQualifier(aTargetNode, vstTree);
          NewData := NewNode^.GetData;
          NewData^.Qualifier.FromDB(SourceData^.RecordId);
        end;
      ntAutoTrade:
        begin
          NewNode := TTreeDocument.CreateAutoTrade(aTargetNode, vstTree);
          NewData := NewNode^.GetData;
          NewData^.AutoTrade.FromDB(SourceData^.RecordId);
        end;
      ntOrderGroupSet:
        begin
          NewNode := TTreeDocument.CreateOrderGroupSet(aTargetNode, vstTree);
          NewData := NewNode^.GetData;
          NewData^.OrderGroupSetDoc.FromDB(SourceData^.RecordId);
        end;
      ntOrderGroup:
        begin
          NewNode := TTreeDocument.CreateOrderGroup(aTargetNode, vstTree);
          NewData := NewNode^.GetData;
          NewData^.OrderGroupDoc.FromDB(SourceData^.RecordId);
        end;
      ntOrder:
        begin
          NewNode := TTreeDocument.CreateOrder(aTargetNode, vstTree, TTreeDocument.GetOrderBroker(SourceData^.RecordId));
          NewData := NewNode^.GetData;
          NewData^.OrderDoc.FromDB(SourceData^.RecordId);
        end;
      ntCondition:
        begin
          NewNode := TTreeDocument.CreateCondition(aTargetNode, vstTree);
          NewData := NewNode^.GetData;
          NewData^.ConditionDoc.FromDB(SourceData^.RecordId);
        end;
      ntAlgos:
        begin
          NewNode := TTreeDocument.CreateAlgos(aTargetNode, vstTree);
          NewData := NewNode^.GetData;
          NewData^.AlgosDoc.FromDB(SourceData^.RecordId);
        end;
      ntFactor:
        begin
          NewNode := TTreeDocument.CreateFactor(aTargetNode, vstTree);
          NewData := NewNode^.GetData;
          NewData^.FactorDoc.FromDB(SourceData^.RecordId);
        end;
    end;
    if Assigned(NewNode) then
      TTreeDocument.LoadRelationTree(-1, SourceData.RelationId, vstTree, NewNode, nil, nil);
    vstTree.FullExpand(NewNode);
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TframeDocumentsTree.vstTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  inherited;
  if DragAllowed  then
    TMonitorTree.OnDragOver(Sender, Source, Shift, State, Pt, Mode, Effect, Accept);
end;

procedure TframeDocumentsTree.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  TMonitorTree.OnDrawText(Sender, TargetCanvas, Node, Column, Text, CellRect, DefaultDraw);
end;

procedure TframeDocumentsTree.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeData;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TframeDocumentsTree.vstTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
  TMonitorTree.OnGetHint(Sender, Node, Column, LineBreakStyle, HintText);
end;

procedure TframeDocumentsTree.vstTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  TMonitorTree.OnGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);
end;

procedure TframeDocumentsTree.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
 var
  Data: PTreeData;
begin
  inherited;
  if (Column = COL_RECORD_ID) then
  begin
    Data := Node^.GetData;
    CellText := Data.RecordId.ToString;
  end
  else
    TMonitorTree.OnGetText(Sender, Node, Column, TextType, CellText);
end;

procedure TframeDocumentsTree.vstTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TframeDocumentsTree.vstTreeMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  NodeHeight := 20;
end;

procedure TframeDocumentsTree.Print(aTree: TVirtualStringTree);
begin
  Printer.Orientation := poLandscape;
  aTree.Print(Printer, True);
end;

procedure TframeDocumentsTree.rbFlatClick(Sender: TObject);
begin
  LoadTree;
end;

procedure TframeDocumentsTree.aAddOrderGroupUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntOrderGroup, ntAutoTrade, ntUnknow];
end;

procedure TframeDocumentsTree.aAddOrderUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntOrderGroup, ntOrder];
end;

procedure TframeDocumentsTree.aAddConditionUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntOrder, ntCondition];
end;

procedure TframeDocumentsTree.aAddAlgosUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntAlgos, ntCondition];
end;

procedure TframeDocumentsTree.aAddFactorUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntCondition, ntAlgos, ntFactor];
end;

procedure TframeDocumentsTree.SearchForText(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  CellText: string;
begin
  TMonitorTree.OnGetText(Sender, Node, COL_ITEMS, ttNormal, CellText);
  Abort := CellText.ToUpper.Contains(string(Data).ToUpper);
end;

procedure TframeDocumentsTree.edtSearchChange(Sender: TObject);
var
  Node: PVirtualNode;
begin
  inherited;
  vstTree.BeginUpdate;
  try
    Node := vstTree.IterateSubtree(nil, SearchForText, Pointer(edtSearch.Text));
    if Assigned(Node) then
    begin
      vstTree.FocusedNode := Node;
      vstTree.Selected[Node] := True;
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TframeDocumentsTree.btnClearSearchTextClick(Sender: TObject);
begin
  edtSearch.Text := '';
end;

procedure TframeDocumentsTree.SetFlatTree;
var
  Node: PVirtualNode;
  i: Integer;
begin
  for Node in FGroupSetList.Values do
    vstTree.MoveTo(Node, nil, amInsertBefore, False);

  for i := 0 to FGroupList.Count - 1 do
    if Assigned(FGroupList.Objects[i]) then
      vstTree.DeleteNode(PVirtualNode(FGroupList.Objects[i]));
  FGroupList.Clear;
end;

procedure TframeDocumentsTree.SetGroupOption(const Value: TGroupOption);
var
  Node: PVirtualNode;
begin
  FGroupOption := Value;
  vstTree.BeginUpdate;
  try
    SetFlatTree;
    if (GroupOption <> goNone) then
      for Node in FGroupSetList.Values do
        SetGroupTree(Node);
    vstTree.FullExpand(nil);
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TframeDocumentsTree.SetGroupTree(aNode: PVirtualNode);
var
  AttMode: TVTNodeAttachMode;
  Data: PTreeData;
  GroupData: PTreeData;
  GroupNode: PVirtualNode;
  GroupText: string;
  Index: Integer;
begin
  if Assigned(aNode) and (GroupOption <> goNone) then
  begin
    Data := aNode^.GetData;
    case GroupOption of
      goGroupType:
        GroupText := Data^.OrderGroupSetDoc.TypeUse.ToString;
    end;

    Index := FGroupList.IndexOf(GroupText);
    if (Index < 0) then
    begin
      GroupNode := vstTree.AddChild(nil);
      GroupData := GroupNode^.GetData;
      GroupData^.Clear;
      GroupData^.DocType := ntOrderGroupSet;
      GroupData^.ImageNo := -1;
      GroupData^.OrderGroupSetDoc.Name := GroupText;

      Index := FGroupList.Add(GroupText);
      FGroupList.Objects[Index] := TObject(GroupNode);
    end
    else
      GroupNode := PVirtualNode(FGroupList.Objects[Index]);

    if (GroupNode.ChildCount = 0) then
      AttMode := amAddChildFirst
    else
    begin
      AttMode := amInsertAfter;
      GroupNode := GroupNode.LastChild;
    end;
    vstTree.MoveTo(aNode, GroupNode, AttMode, False);
  end;
end;

procedure TframeDocumentsTree.aModifyExecute(Sender: TObject);
begin
  TTreeDocumentEdit.Edit(vstTree, TypeUse, FReadOnly);
end;

procedure TframeDocumentsTree.aModifyUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := not FReadOnly;
  TAction(Sender).Enabled := vstTree.Focused;
end;

procedure TframeDocumentsTree.aDeleteSelectedNodeExecute(Sender: TObject);
resourcestring
  rsQuestion = 'Current node will be deleted. Continue?';
var
  Node: PVirtualNode;
  Data: PTreeData;
begin
  if (TMessageDialog.ShowQuestion(rsQuestion) = mrYes) then
  begin
    vstTree.BeginUpdate;
    try
      Node := vstTree.FocusedNode;
      if Assigned(Node) then
      begin
        Data := Node^.GetData;
        if (Data.RecordId > 0) then
          TTreeDocument.DeleteRelations(Data.RecordId);
        if Assigned(Node) then
          vstTree.DeleteNode(Node);
      end;
    finally
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TframeDocumentsTree.aAddOrderExecute(Sender: TObject);
var
  Data: PTreeData;
  Node, ParentNode: PVirtualNode;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    if (TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) = ntOrder) then
      ParentNode := vstTree.FocusedNode.Parent
    else
      ParentNode := vstTree.FocusedNode;
    if Assigned(ParentNode) then
    begin
      Node := TTreeDocument.CreateOrder(ParentNode, vstTree, TBrokerType.brIB);
      Data := Node^.GetData;
      if (TfrmOrderDocument.ShowDocument(Data.OrderDoc, TTreeDocument.GetOrderSubordination(vstTree, Node)) <> mrOk) then
        vstTree.DeleteNode(Node)
      else
        vstTree.Expanded[ParentNode] := True;
    end;
  end;
end;

procedure TframeDocumentsTree.aAddOrderGroupExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Node := TTreeDocument.CreateOrderGroup(nil, vstTree);
  Data := Node^.GetData;
  if (TfrmEditOrderGroup.ShowDocument(Data.OrderGroupDoc) <> mrOk) then
    vstTree.DeleteNode(Node);
end;

procedure TframeDocumentsTree.aAddConditionExecute(Sender: TObject);
var
  Data: PTreeData;
  Node, ParentNode: PVirtualNode;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    if (TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) = ntCondition) then
      ParentNode := vstTree.FocusedNode.Parent
    else
      ParentNode := vstTree.FocusedNode;
    if Assigned(ParentNode) then
    begin
      Node := TTreeDocument.CreateCondition(ParentNode, vstTree);
      Data := Node^.GetData;
      if (TfrmEditCondition.ShowDocument(Data.ConditionDoc, vstTree) <> mrOk) then
        vstTree.DeleteNode(Node)
      else
        vstTree.Expanded[ParentNode] := True;
    end;
  end;
end;

procedure TframeDocumentsTree.aAddAlgosExecute(Sender: TObject);
var
  Data       : PTreeData;
  Node       : PVirtualNode;
  ParentNode : PVirtualNode;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    ParentNode := vstTree.FocusedNode;
    Node := TTreeDocument.CreateAlgos(ParentNode, vstTree);
    Data := Node^.GetData;
    if (TfrmEditAlgos.ShowDocument(Data.AlgosDoc) <> mrOk) then
      vstTree.DeleteNode(Node)
    else
      vstTree.Expanded[ParentNode] := True;
  end;
end;

procedure TframeDocumentsTree.aAddFactorExecute(Sender: TObject);
var
  Data: PTreeData;
  Node, ParentNode: PVirtualNode;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    vstTree.BeginUpdate;
    try
      if TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) = ntFactor then
        ParentNode := vstTree.FocusedNode.Parent
      else
        ParentNode := vstTree.FocusedNode;

      Node := TTreeDocument.CreateFactor(ParentNode, vstTree);
      Data := Node^.GetData;
      Data.FactorDoc.BrokerType := TBrokerType.brIB;
      Data.FactorDoc.TickType1 := ttLast;
      Data.FactorDoc.TickType2 := ttNotSet;
      vstTree.Expanded[ParentNode] := True;
    finally
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TframeDocumentsTree.aExpandSelectedNodeExecute(Sender: TObject);
begin
  if Assigned(vstTree.FocusedNode) then
    vstTree.FullExpand(vstTree.FocusedNode);
end;

procedure TframeDocumentsTree.aExpandSelectedNodeUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(vstTree.FocusedNode);
end;

procedure TframeDocumentsTree.aExpandTreeExecute(Sender: TObject);
begin
  vstTree.FullExpand;
end;

procedure TframeDocumentsTree.aCollapsSelectedNodeExecute(Sender: TObject);
begin
  if Assigned(vstTree.FocusedNode) then
    vstTree.FullCollapse(vstTree.FocusedNode);
end;

procedure TframeDocumentsTree.aCollapsTreeExecute(Sender: TObject);
begin
  vstTree.FullCollapse;
end;

procedure TframeDocumentsTree.aExportToCSVExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToCSV(vstTree, 'Tree');
end;

procedure TframeDocumentsTree.aExportToXLSExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToExcel(vstTree, 'Tree');
end;

procedure TframeDocumentsTree.aPrintExecute(Sender: TObject);
begin
  Print(vstTree);
end;

procedure TframeDocumentsTree.aShowInformationDialogExecute(Sender: TObject);
begin
  ShowInformationDialog;
end;

procedure TframeDocumentsTree.SetGroupView(const Value: TGroupView);
begin
  rbFlat.Checked := Value = gvFlat;
  rbTree.Checked := Value = gvTree;
end;

procedure TframeDocumentsTree.SetMarkedNode(const Value: TMarkedNode);
begin
  FMarkedNode := Value;
  vstTree.Invalidate;
end;

procedure TframeDocumentsTree.ShowInformationDialog;
var
  Info: string;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    Info := TDocumentInfo.GetNodeInfo(vstTree.FocusedNode);
    TInformationDialog.ShowMessage(Info, 'MonitorTree');
  end;
end;

function TframeDocumentsTree.GetDescription: string;
begin
  Result := pnlDescription.Caption;
end;

procedure TframeDocumentsTree.SetDescription(const Value: string);
begin
  pnlDescription.Caption := Value;
end;

function TframeDocumentsTree.GetGroupView: TGroupView;
begin
  if rbFlat.Checked then
    Result := gvFlat
  else
    Result := gvTree;
end;

end.
