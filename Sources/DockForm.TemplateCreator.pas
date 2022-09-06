unit DockForm.TemplateCreator;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, System.StrUtils, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons,
  Vcl.ExtCtrls, System.Generics.Collections, System.Generics.Defaults, System.Actions, Vcl.ActnList, System.IniFiles,
  System.Math, Vcl.Menus, Vcl.ExtDlgs, Vcl.WinXCtrls, Winapi.ActiveX, Vcl.Grids, Vcl.DBGrids, Global.Types, Document,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} DebugWriter, CustomForms, Data.DB,
  IABSocketAPI_const, IABSocketAPI, InstrumentList, BrokerHelperAbstr, DaModule, MessageDialog, Monitor.Types,
  DaImages, Edit.OrderGroup, Edit.Condition, Edit.Algos, Edit.OrderDocument, Edit.Factor, Qualifiers.Types, ArrayHelper,
  Qualifiers.Edit, Qualifiers, AutoTrades.Edit, Edit.OrderGroupSet, Vcl.ComCtrls, InformationDialog, Frame.DocumentsTree,
  VirtualTrees.ExportHelper, Common.Types, Monitor.Info, System.Threading, CustomDockForm,
  System.Types, Utils, MonitorTree.Helper, MonitorTree.Document, HtmlConsts, AutoTrades.Types;
{$ENDREGION}

type
  TfrmDockFormTemplateCreator = class(TfrmCustomDockForm)
    aAddAutotrade: TAction;
    aAddOrderAsChild: TAction;
    aAddOrderGroupSet: TAction;
    aAddQualifier: TAction;
    aAddQualifierCondition: TAction;
    aClearTree: TAction;
    aCollapsTree: TAction;
    aDeleteQualifier: TAction;
    aDeleteSelectedNode: TAction;
    aEdit: TAction;
    aExpandTree: TAction;
    aOpen: TAction;
    aRefresh: TAction;
    aSave: TAction;
    aSaveAs: TAction;
    aShowInformation: TAction;
    btnAdd: TBitBtn;
    btnDeleteQualifierSet: TBitBtn;
    btnDeleteSelectedNode: TBitBtn;
    btnEditQualifier: TBitBtn;
    btnOpenQualifierSet: TBitBtn;
    btnSave: TBitBtn;
    btnSaveAs: TBitBtn;
    edCurrentQualifierName: TEdit;
    frameQualifierSet: TframeDocumentsTree;
    lblCurrentQualifierName: TLabel;
    miAddAutotrade: TMenuItem;
    miAddNewOrderasChild: TMenuItem;
    miAddOrderGroups: TMenuItem;
    miAddQualifierCondition: TMenuItem;
    miClearTree: TMenuItem;
    miCollapsTree: TMenuItem;
    miDeleteQualifierSet: TMenuItem;
    miDeleteSelectedNode: TMenuItem;
    miEdit: TMenuItem;
    miExpandTree: TMenuItem;
    miOpenQualifierSet: TMenuItem;
    miSep01: TMenuItem;
    miShowInformation: TMenuItem;
    pnlQualifierSet: TPanel;
    pnlQualifierSetLeft: TPanel;
    splQualifierSet: TSplitter;
    btnInfo: TBitBtn;
    aInfo: TAction;
    procedure aAddAlgosExecute(Sender: TObject);
    procedure aAddAlgosUpdate(Sender: TObject);
    procedure aAddAutotradeExecute(Sender: TObject);
    procedure aAddAutotradeUpdate(Sender: TObject);
    procedure aAddConditionExecute(Sender: TObject);
    procedure aAddConditionUpdate(Sender: TObject);
    procedure aAddFactorExecute(Sender: TObject);
    procedure aAddFactorUpdate(Sender: TObject);
    procedure aAddOrderAsChildExecute(Sender: TObject);
    procedure aAddOrderAsChildUpdate(Sender: TObject);
    procedure aAddOrderExecute(Sender: TObject);
    procedure aAddOrderGroupExecute(Sender: TObject);
    procedure aAddOrderGroupSetExecute(Sender: TObject);
    procedure aAddOrderGroupSetUpdate(Sender: TObject);
    procedure aAddOrderGroupTreeExecute(Sender: TObject);
    procedure aAddOrderGroupUpdate(Sender: TObject);
    procedure aAddOrderUpdate(Sender: TObject);
    procedure aAddQualifierConditionExecute(Sender: TObject);
    procedure aAddQualifierConditionUpdate(Sender: TObject);
    procedure aAddQualifierExecute(Sender: TObject);
    procedure aClearTreeExecute(Sender: TObject);
    procedure aCollapsTreeExecute(Sender: TObject);
    procedure aDeleteQualifierExecute(Sender: TObject);
    procedure aDeleteSelectedNodeExecute(Sender: TObject);
    procedure aDeleteSelectedNodeUpdate(Sender: TObject);
    procedure aEditExecute(Sender: TObject);
    procedure aEditUpdate(Sender: TObject);
    procedure aExpandTreeExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aShowInformationDialogExecute(Sender: TObject);
    procedure aShowInformationUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pmTreePopup(Sender: TObject);
    procedure vstTreeAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
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
    procedure aInfoExecute(Sender: TObject);
  private const
    COL_ITEMS = 0;
    COL_VALUE = 1;
    COL_CALCTYPE = 2;

    C_IDENTITY_NAME = 'DockFormTemplateCreator';
  protected
    function GetIdentityName: string; override;
  private
    FCurrentQualifier: TQualifier;
    function CheckData: Boolean;
    procedure AddNewNode(aTargetNode: PVirtualNode; aSourceTree: TVirtualStringTree);
    procedure EditDocument;
    procedure ReloadQualifier(aRecordId: Integer);
    procedure SetCurrentQualifierCaptions;
  public
    procedure Initialize; override;
  end;

var
  frmDockFormTemplateCreator: TfrmDockFormTemplateCreator;

implementation

{$R *.dfm}

{ TfrmDockFormTemplateCreator }

procedure TfrmDockFormTemplateCreator.FormCreate(Sender: TObject);
begin
  vstTree.NodeDataSize := SizeOf(TTreeData);
  inherited;
end;

procedure TfrmDockFormTemplateCreator.FormDestroy(Sender: TObject);
begin
  frmDockFormTemplateCreator := nil;
  inherited;
end;

function TfrmDockFormTemplateCreator.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmDockFormTemplateCreator.Initialize;
begin
  inherited Initialize;
  Caption := 'Template Creator';
  frameQualifierSet.DocType     := ntQualifier;
  frameQualifierSet.ReadOnly    := True;
  frameQualifierSet.DragAllowed := True;
  frameQualifierSet.Initialize;
  frameQualifierSet.Description := '-Templates-';
  SetFocusSafely(frameQualifierSet.edtSearch);
end;

procedure TfrmDockFormTemplateCreator.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frameQualifierSet.Deinitialize;
end;

procedure TfrmDockFormTemplateCreator.pmTreePopup(Sender: TObject);
var
  DocType: TDocType;
begin
  inherited;
  if Assigned(vstTree.FocusedNode) then
  begin
    DocType := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode);
    miShowInformation.Caption := 'Get ' + DocType.ToString + ' Info';
    miShowInformation.Visible := True;
  end
  else
    miShowInformation.Visible := False;
end;

procedure TfrmDockFormTemplateCreator.vstTreeAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
begin
  inherited;
  TMonitorTree.OnAfterCellPaint(Sender, TargetCanvas, Node, Column, CellRect);
end;

procedure TfrmDockFormTemplateCreator.vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  inherited;
  TMonitorTree.OnBeforeCellPaint(Sender, TargetCanvas, Node, Column, CellPaintMode, CellRect, ContentRect);
end;

procedure TfrmDockFormTemplateCreator.vstTreeDblClick(Sender: TObject);
var
  bIsExpanded: Boolean;
begin
  inherited;
  if Assigned(vstTree.FocusedNode) then
  begin
    bIsExpanded := (vsExpanded in vstTree.FocusedNode.States);
    EditDocument;
    vstTree.Expanded[vstTree.FocusedNode] := not bIsExpanded;
  end;
end;

procedure TfrmDockFormTemplateCreator.vstTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  inherited;
  Allowed := (Sender = vstTree) or (Sender = frameQualifierSet.vstTree);
end;

procedure TfrmDockFormTemplateCreator.vstTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  SourceNode: PVirtualNode;
  TargetNode: PVirtualNode;
  attMode: TVTNodeAttachMode;
  Data: PTreeData;
begin
  inherited;
  SourceNode := TVirtualStringTree(Source).FocusedNode;
  TargetNode := Sender.DropTargetNode;
  if (Sender = Source) then
  begin
    if (TTreeDocument.GetDocType(TVirtualStringTree(Source), SourceNode) = ntOrder) and
      (TTreeDocument.GetDocType(Sender, TargetNode) = ntOrder) then
    begin
      if (TargetNode.NextSibling = SourceNode) then
        attMode := amAddChildFirst
      else
        attMode := amAddChildLast;
    end
    else if (TTreeDocument.GetDocType(TVirtualStringTree(Source), SourceNode) = ntFactor) and
      (TTreeDocument.GetDocType(Sender, TargetNode) = ntFactor) then
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
  else if Assigned(SourceNode) then
  begin
    Data := SourceNode^.GetData;
    if (Data^.DocType = ntQualifier) then
      ReloadQualifier(Data^.RecordId)
    else if Assigned(TargetNode) then
      AddNewNode(TargetNode, TVirtualStringTree(Source));
  end;
end;

procedure TfrmDockFormTemplateCreator.vstTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  inherited;
  TMonitorTree.OnDragOver(Sender, Source, Shift, State, Pt, Mode, Effect, Accept);
end;

procedure TfrmDockFormTemplateCreator.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  inherited;
  TMonitorTree.OnDrawText(Sender, TargetCanvas, Node, Column, Text, CellRect, DefaultDraw);
end;

procedure TfrmDockFormTemplateCreator.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeData;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmDockFormTemplateCreator.vstTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
  TMonitorTree.OnGetHint(Sender, Node, Column, LineBreakStyle, HintText);
end;

procedure TfrmDockFormTemplateCreator.vstTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  inherited;
  TMonitorTree.OnGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);
end;

procedure TfrmDockFormTemplateCreator.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;TextType: TVSTTextType; var CellText: string);
var
  Data: PTreeData;
begin
  inherited;
  if (Column = COL_CALCTYPE) and (TTreeDocument.GetDocType(Sender, Node) = ntCondition) then
  begin
    Data := Node^.GetData;
    case Data^.ConditionDoc.CondType of
      ctRealtimeValue:
        CellText := 'xx ' + Data^.ConditionDoc.InequalityRt.ToString + FormatFloat('0.00%', Data^.ConditionDoc.CondLimit);
      ctGradient:
        CellText := FormatFloat('0.00 ', Data^.ConditionDoc.Gradient) + Data^.ConditionDoc.InequalityRt.ToString +
                    '-MON=' + Data^.ConditionDoc.Monitoring.ToString;
      ctCorridor:
        CellText := 'GRAD ' + Data^.ConditionDoc.InequalityGr.ToString + FormatFloat('0.00 ', Data^.ConditionDoc.Gradient) +
                    '-CORR ' + FormatFloat('0.00 ', Data^.ConditionDoc.CondWidth) + Data^.ConditionDoc.InequalityCor.ToString +
                    '-MON=' + Data^.ConditionDoc.Monitoring.ToString;
      ctCorridorPosition:
        CellText := 'GRAD ' + Data^.ConditionDoc.InequalityGr.ToString + FormatFloat('0.00 ', Data^.ConditionDoc.Gradient) +
                    '-CORR ' + FormatFloat('0.00 ', Data^.ConditionDoc.CondWidth) + Data^.ConditionDoc.InequalityCor.ToString +
                    '-MON=' + Data^.ConditionDoc.Monitoring.ToString +
                    '-POS=' + Data^.ConditionDoc.UpProc.ToString;
    end;

    case Data^.ConditionDoc.Priority of
      cpNormal:
        CellText := CellText + ' (N)';
      cpPriority:
        CellText := CellText + ' (P)';
      cpVeto:
        CellText := CellText + ' (V)';
    end;
    if Data^.ConditionDoc.Bypass then
      CellText := CellText + ' [BYPASS]';
  end
  else
    TMonitorTree.OnGetText(Sender, Node, Column, TextType, CellText);
end;

procedure TfrmDockFormTemplateCreator.aShowInformationDialogExecute(Sender: TObject);
var
  Info: string;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    Info := TDocumentInfo.GetNodeInfo(vstTree.FocusedNode);
    TInformationDialog.ShowMessage(Info, 'TemplateCreator');
  end;
end;

procedure TfrmDockFormTemplateCreator.aShowInformationUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := Assigned(vstTree.FocusedNode);
end;

function TfrmDockFormTemplateCreator.CheckData: Boolean;
var
  Msg: string;
begin
  Msg := '';


  Result := Msg.IsEmpty;
  if not Result then
    TMessageDialog.ShowWarning(Msg);
end;

procedure TfrmDockFormTemplateCreator.aRefreshExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Id: Integer;
  Data: PTreeData;
begin
  Node := vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    if (TTreeDocument.GetDocType(vstTree, Node) <> ntQualifier) then
      Node := TTreeDocument.GetParentNode(vstTree, Node, ntQualifier);
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      Id := Data^.Qualifier.RecordId;
      vstTree.BeginUpdate;
      try
        vstTree.Clear;
        TTreeDocument.LoadRelationTree(Id, -1, vstTree, nil, nil, nil);
      finally
        vstTree.EndUpdate;
      end;
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aAddOrderGroupTreeExecute(Sender: TObject);
var
  ParentNode: PVirtualNode;
begin
  vstTree.BeginUpdate;
  try
    ParentNode := TTreeDocument.CreateOrderGroup(nil, vstTree);
    ParentNode := TTreeDocument.CreateOrder(ParentNode, vstTree, TBrokerType.brIB);
    ParentNode := TTreeDocument.CreateCondition(ParentNode, vstTree);
    TTreeDocument.CreateFactor(ParentNode, vstTree);
    vstTree.FullExpand(nil);
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmDockFormTemplateCreator.aAddOrderGroupUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntOrderGroup, ntOrderGroupSet];
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmDockFormTemplateCreator.aAddOrderUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntOrderGroup, ntOrder];
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmDockFormTemplateCreator.aAddOrderAsChildExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
  ParentData: PTreeData;
  ParentNode: PVirtualNode;
  ExtendedOptions : TCustomOrderDoc.TExtendedOptions;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    ParentNode := nil;
    if (TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) = ntOrder) then
      ParentNode := vstTree.FocusedNode;
    if Assigned(ParentNode) then
    begin
      ParentData := vstTree.FocusedNode^.GetData;
//      if (ParentData.OrderDoc.ExtendedOptions.Subordination in [suUnknow, suMotherOrder]) then
      begin
        Node := TTreeDocument.CreateOrder(ParentNode, vstTree, TBrokerType.brIB);
        Data := Node^.GetData;
        Data.OrderDoc.AssignFrom(ParentData.OrderDoc);
        if Data.OrderDoc.OrderAction = iabBuy then
          Data.OrderDoc.OrderAction := iabSell
        else
          Data.OrderDoc.OrderAction := iabBuy;

        ExtendedOptions := Data.OrderDoc.ExtendedOptions;
        ExtendedOptions.Subordination := suChildOrder;
        Data.OrderDoc.ExtendedOptions := ExtendedOptions;

        if (TfrmOrderDocument.ShowDocument(Data.OrderDoc, TTreeDocument.GetOrderSubordination(vstTree, Node)) <> mrOk) then
          vstTree.DeleteNode(Node)
        else
          vstTree.Expanded[ParentNode] := True;
      end;
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aAddOrderAsChildUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntOrder];
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmDockFormTemplateCreator.aAddConditionUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntOrder, ntCondition];
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmDockFormTemplateCreator.aAddAlgosUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntAlgos, ntCondition];
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmDockFormTemplateCreator.aAddAutotradeExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
begin
  inherited;
  if Assigned(vstTree.FocusedNode) then
  begin
    if (TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) = ntAutoTrade) then
      ParentNode := vstTree.FocusedNode.Parent
    else
      ParentNode := vstTree.FocusedNode;

    if Assigned(ParentNode) then
    begin
      Node := TTreeDocument.CreateAutoTrade(ParentNode, vstTree);
      Data := Node^.GetData;
      Data^.AutoTrade.Enabled := True;
      if (TfrmAutoTradesEdit.ShowEditForm(Data^.AutoTrade, dmInsert) <> mrOk) then
        vstTree.DeleteNode(Node)
      else
        vstTree.Expanded[ParentNode] := True;
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aAddAutotradeUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntAutoTrade, ntQualifier];
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmDockFormTemplateCreator.aAddFactorUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntCondition, ntAlgos, ntFactor];
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmDockFormTemplateCreator.aOpenExecute(Sender: TObject);
begin
  ReloadQualifier(frameQualifierSet.GetHigherNodeId(ntQualifier).RecordId);
end;

procedure TfrmDockFormTemplateCreator.aAddQualifierConditionExecute(Sender: TObject);
{var
  Data: PTreeData;
  Node, ParentNode: PVirtualNode;
  QualifierItem: TQualifierCondition;  }
begin
  inherited;
  {if Assigned(vstTree.FocusedNode) then
  begin
    if (TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) = ntQualifierCondition) then
      ParentNode := vstTree.FocusedNode.Parent
    else
      ParentNode := vstTree.FocusedNode;
    if Assigned(ParentNode) then
    begin
      QualifierItem := TQualifierCondition.Create;
      if (TfrmQualifierConditionEdit.ShowDocument(QualifierItem, dmInsert) = mrOk) then
      begin
        Node := TTreeDocument.CreateQualifierCondition(ParentNode, vstTree);
        Data := Node^.GetData;
        Data^.QualifierCondition.AssignFrom(QualifierItem);
        SetLength(FCurrentQualifier.Conditions, Length(FCurrentQualifier.Conditions) + 1);
        FCurrentQualifier.Conditions[High(FCurrentQualifier.Conditions)] := QualifierItem;
        vstTree.InvalidateNode(Node);
        vstTree.Expanded[ParentNode] := True;
      end;
    end;
  end; }
end;

procedure TfrmDockFormTemplateCreator.aAddQualifierConditionUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntQualifier];
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmDockFormTemplateCreator.ReloadQualifier(aRecordId: Integer);
var
  Node: PVirtualNode;
begin
  if (aRecordId > 0) then
  begin
    vstTree.BeginUpdate;
    try
      vstTree.Clear;
      Node := TTreeDocument.LoadRelationTree(aRecordId, -1, vstTree, nil, nil, nil);
      if Assigned(Node) then
      begin
        if not Assigned(FCurrentQualifier) then
          FCurrentQualifier := TQualifier.Create;
        FCurrentQualifier.FromDB(aRecordId);
        FCurrentQualifier.OwnerNode := Node;
        SetCurrentQualifierCaptions;
        frameQualifierSet.LoadTree;
        frameQualifierSet.MarkedNode := TMarkedNode.Create(aRecordId, TDocType.ntQualifier);
      end;
    finally
      DMod.RefreshQuery(DMod.fbqQualifiers);
      vstTree.FullExpand(nil);
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aEditExecute(Sender: TObject);
begin
  EditDocument;
end;

procedure TfrmDockFormTemplateCreator.aEditUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := not vstTree.IsEmpty;
end;

procedure TfrmDockFormTemplateCreator.aDeleteSelectedNodeUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := not vstTree.IsEmpty;
end;

procedure TfrmDockFormTemplateCreator.EditDocument;
var
  Data: PTreeData;
  ChildData: PTreeData;
  Node: PVirtualNode;
begin
  Node := vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    case Data^.DocType of
      ntQualifier:
        if (TfrmQualifierEdit.ShowEditForm(Data^.Qualifier, dmUpdate) = mrOk) then
        begin
          Data^.Qualifier.SaveToDB;
          FCurrentQualifier.FromDB(Data^.RecordId);
          FCurrentQualifier.OwnerNode := Node;
          SetCurrentQualifierCaptions;
          frameQualifierSet.LoadTree;
          frameQualifierSet.MarkedNode := TMarkedNode.Create(Data^.RecordId, TDocType.ntQualifier);
        end;
      ntAutoTrade:
        if (TfrmAutoTradesEdit.ShowEditForm(Data^.AutoTrade, dmUpdate) = mrOk) then
        begin
          if Assigned(Node.FirstChild) then
          begin
            ChildData := Node.FirstChild^.GetData;
            if (ChildData^.DocType = ntOrderGroupSet) then
              Data^.AutoTrade.OrderGroupId := ChildData^.RecordId;
          end;
          Data^.AutoTrade.SaveToDB;
        end;
      ntOrderGroupSet:
        if TfrmEditOrderGroupSet.ShowDocument(Data^.OrderGroupSetDoc, True) = mrOk then
          Data^.OrderGroupSetDoc.SaveToDB;
      ntOrderGroup:
        if TfrmEditOrderGroup.ShowDocument(Data^.OrderGroupDoc, True) = mrOk then
          Data^.OrderGroupDoc.SaveToDB;
      ntCondition:
        if TfrmEditCondition.ShowDocument(Data^.ConditionDoc, vstTree) = mrOk then
          Data^.ConditionDoc.SaveToDB;
      ntAlgos:
        if TfrmEditAlgos.ShowDocument(Data^.AlgosDoc) = mrOk then
          Data^.AlgosDoc.SaveToDB;
      ntOrder:
        if TfrmOrderDocument.ShowDocument(Data^.OrderDoc, TTreeDocument.GetOrderSubordination(vstTree, Node)) = mrOk then
          Data^.OrderDoc.SaveToDB;
      ntFactor:
        if TfrmEditFactor.ShowDocument(Data^.FactorDoc, True) = mrOk then
          Data^.FactorDoc.SaveToDB;
    end;
//    if (Data^.DocType <> ntQualifier) and Assigned(Node) then //the previous version of the node was removed during ReloadQualifier
//      vstTree.InvalidateNode(Node);
  end;
end;

procedure TfrmDockFormTemplateCreator.aExpandTreeExecute(Sender: TObject);
begin
  vstTree.FullExpand;
end;

procedure TfrmDockFormTemplateCreator.aInfoExecute(Sender: TObject);
var
  arrNodes: TNodeArray;
  sb: TStringBuilder;
begin
  inherited;
  if Assigned(FCurrentQualifier.OwnerNode) then
  begin
    arrNodes := TTreeDocument.GetNodesList(FCurrentQualifier.OwnerNode);
    sb := TStringBuilder.Create;
    sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
      .Append(C_HTML_BODY_OPEN).AppendLine.Append(C_HTML_BREAK);
    try
      for var Node in arrNodes do
        sb.AppendLine(TDocumentInfo.GetNodeInfo(Node, False)).Append(C_HTML_LINE);
    finally
      sb.Append(C_HTML_BODY_CLOSE).AppendLine;
      TInformationDialog.ShowMessage(sb.ToString, 'TemplateCreatorNodeInfo');
      FreeAndNil(sb);
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aCollapsTreeExecute(Sender: TObject);
begin
  vstTree.FullCollapse;
end;

procedure TfrmDockFormTemplateCreator.SetCurrentQualifierCaptions;
begin
  edCurrentQualifierName.Text := FCurrentQualifier.Name;
end;

procedure TfrmDockFormTemplateCreator.aAddOrderGroupExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
begin
  inherited;
  if Assigned(vstTree.FocusedNode) then
  begin
    if (TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) = ntOrderGroup) then
      ParentNode := vstTree.FocusedNode.Parent
    else
      ParentNode := vstTree.FocusedNode;
    if Assigned(ParentNode) then
    begin
      Node := TTreeDocument.CreateOrderGroup(ParentNode, vstTree);
      Data := Node^.GetData;
      if (TfrmEditOrderGroup.ShowDocument(Data.OrderGroupDoc, True) <> mrOk) then
        vstTree.DeleteNode(Node)
      else
        vstTree.Expanded[ParentNode] := True;
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aAddOrderGroupSetExecute(Sender: TObject);
resourcestring
  rsContainsOrderGroupSet = 'Autotrade node already contains a child OrderGroupSet';
var
  Data: PTreeData;
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
begin
  inherited;
  if Assigned(vstTree.FocusedNode) then
  begin
    if (TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) = ntOrderGroupSet) then
      ParentNode := vstTree.FocusedNode.Parent
    else
      ParentNode := vstTree.FocusedNode;

    if Assigned(ParentNode) then
    begin
      Node := ParentNode.FirstChild;
      while Assigned(Node) do
      begin
        Data := Node^.GetData;
        if (Data^.DocType = ntOrderGroupSet) then
        begin
          TMessageDialog.ShowWarning(rsContainsOrderGroupSet);
          Exit;
        end;
        Node := ParentNode.NextSibling;
      end;

      Node := TTreeDocument.CreateOrderGroupSet(ParentNode, vstTree);
      Data := Node^.GetData;
      Data^.OrderGroupSetDoc.TypeUse := tuTemplate;
      if (TfrmEditOrderGroupSet.ShowDocument(Data^.OrderGroupSetDoc, True) <> mrOk) then
        vstTree.DeleteNode(Node)
      else
        vstTree.Expanded[ParentNode] := True;
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aAddOrderGroupSetUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntOrderGroupSet, ntAutoTrade];
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmDockFormTemplateCreator.aAddOrderExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
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
      TTreeDocument.SetIcon(Node, vstTree);
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aAddConditionExecute(Sender: TObject);
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
      Data.ConditionDoc.CondLimit := 1;
      if (TfrmEditCondition.ShowDocument(Data.ConditionDoc, vstTree) <> mrOk) then
        vstTree.DeleteNode(Node)
      else
        vstTree.Expanded[ParentNode] := True;
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aAddAlgosExecute(Sender: TObject);
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

procedure TfrmDockFormTemplateCreator.aAddFactorExecute(Sender: TObject);
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

procedure TfrmDockFormTemplateCreator.aDeleteQualifierExecute(Sender: TObject);
var
  Id: Integer;
begin
  Id := frameQualifierSet.GetHigherNodeId(ntQualifier).RecordId;
  if (Id > 0) and (TMessageDialog.ShowQuestion('Current Qualifier will be deleted. Continue?') = mrYes) then
  begin
    try
      TTreeDocument.DeleteRelations(Id);
      if (FCurrentQualifier.RecordId = Id) then
        aClearTreeExecute(Sender);
    finally
      DMod.RefreshQuery(DMod.fbqQualifiers);
      frameQualifierSet.LoadTree;
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aClearTreeExecute(Sender: TObject);
begin
  inherited;
  if (not vstTree.IsEmpty) and (TMessageDialog.ShowQuestion('Current Qualifier will be clear. Continue?') = mrYes) then
  begin
    vstTree.BeginUpdate;
    try
      vstTree.Clear;
      FCurrentQualifier.OwnerNode := nil;
      FCurrentQualifier.Clear;
      SetCurrentQualifierCaptions;
    finally
      frameQualifierSet.MarkedNode := TMarkedNode.Create(-1, TDocType.ntQualifier);
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aDeleteSelectedNodeExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  inherited;
  vstTree.BeginUpdate;
  try
    Node := vstTree.FocusedNode;
    if Assigned(Node) then
    begin
      vstTree.DeleteNode(Node);
      if vstTree.IsEmpty then
      begin
        FCurrentQualifier.Clear;
        SetCurrentQualifierCaptions;
      end;
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmDockFormTemplateCreator.aAddQualifierExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PTreeData;
begin
  vstTree.Clear;
  Node := TTreeDocument.CreateQualifier(nil, vstTree);
  if Assigned(Node) then
    try
      Data := Node^.GetData;
      Data^.Qualifier.Name := '';
      TTreeDocument.SaveRelationTree(vstTree, vstTree.GetFirst, True);
      FCurrentQualifier.AssignFrom(Data^.Qualifier);
      FCurrentQualifier.OwnerNode := Node;
      SetCurrentQualifierCaptions;
    finally
      frameQualifierSet.LoadTree;
      frameQualifierSet.MarkedNode := TMarkedNode.Create(FCurrentQualifier.RecordId, TDocType.ntQualifier);
      DMod.RefreshQuery(DMod.fbqQualifiers);
    end;
end;

procedure TfrmDockFormTemplateCreator.aSaveExecute(Sender: TObject);
var
  Data: PTreeData;
begin
  if not vstTree.IsEmpty and CheckData then
  begin
    vstTree.BeginUpdate;
    try
      if not FCurrentQualifier.Name.Trim.Equals(string(edCurrentQualifierName.Text).Trim) then
        FCurrentQualifier.Name := edCurrentQualifierName.Text;

      if Assigned(FCurrentQualifier.OwnerNode) then
      begin
        Data := FCurrentQualifier.OwnerNode^.GetData;
        Data.Qualifier.Name := FCurrentQualifier.Name;
      end;
      TTreeDocument.DeleteRelations(FCurrentQualifier.RecordId);
      TTreeDocument.SaveRelationTree(vstTree, vstTree.GetFirst);
    finally
      frameQualifierSet.LoadTree;
      frameQualifierSet.MarkedNode := TMarkedNode.Create(FCurrentQualifier.RecordId, TDocType.ntQualifier);
      DMod.RefreshQuery(DMod.fbqQualifiers);
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TfrmDockFormTemplateCreator.aSaveAsExecute(Sender: TObject);
var
  NewName: string;
  Data: PTreeData;
begin
  if not vstTree.IsEmpty then
  begin
    Data := nil;
    NewName := FCurrentQualifier.Name + ' Copy';
    if Vcl.Dialogs.InputQuery('Save as new Qualifier', 'Enter a new name, please', NewName) then
      try
        if Assigned(FCurrentQualifier.OwnerNode) then
        begin
          Data := FCurrentQualifier.OwnerNode^.GetData;
          Data.Qualifier.Name := NewName;
        end;
        TTreeDocument.SaveRelationTree(vstTree, vstTree.GetFirst, True);
        if Assigned(Data) then
        begin
          FCurrentQualifier.AssignFrom(Data^.Qualifier);
          ReloadQualifier(Data^.RecordId);
        end;
      finally
        frameQualifierSet.LoadTree;
        frameQualifierSet.MarkedNode := TMarkedNode.Create(FCurrentQualifier.RecordId, TDocType.ntQualifier);
        DMod.RefreshQuery(DMod.fbqQualifiers);
      end;
  end;
end;

procedure TfrmDockFormTemplateCreator.AddNewNode(aTargetNode: PVirtualNode; aSourceTree: TVirtualStringTree);
var
  AfterLoadProc: TAfterLoadEachDocumentProc;
  NewData: PTreeData;
  NewNode: PVirtualNode;
  SourceData: PTreeData;
  SourceNode: PVirtualNode;
begin
  NewNode := nil;
  AfterLoadProc := procedure(const aNode: PVirtualNode)
    var
      Data: PTreeData;
    begin
      if Assigned(aNode) then
      begin
        Data := aNode^.GetData;
        Data^.CreationType := ctUser;
        case Data^.DocType of
          ntQualifier:
            Data^.Qualifier.RecordId := -1;
          ntAutoTrade:
            Data^.AutoTrade.RecordId := -1;
          ntOrderGroupSet:
            Data^.OrderGroupSetDoc.RecordId := -1;
          ntOrderGroup:
            Data^.OrderGroupDoc.RecordId := -1;
          ntOrder:
            Data^.OrderDoc.RecordId := -1;
          ntCondition:
            Data^.ConditionDoc.RecordId := -1;
          ntAlgos:
            Data^.AlgosDoc.RecordId := -1;
          ntFactor:
            Data^.FactorDoc.RecordId := -1;
        end;
        Data^.RecordId := -1;
        TTreeDocument.SetIcon(aNode, vstTree);
      end;
    end;

  vstTree.BeginUpdate;
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
    begin
      AfterLoadProc(NewNode);
      TTreeDocument.LoadRelationTree(-1, SourceData.RelationId, vstTree, NewNode, nil, AfterLoadProc);
    end;
  finally
    vstTree.FullExpand(NewNode);
    vstTree.EndUpdate;
  end;
end;

procedure TfrmDockFormTemplateCreator.vstTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Node: PVirtualNode;
begin
  inherited;
  if (Key = VK_UP) and (ssShift in Shift) then
  begin
    Node := TVirtualStringTree(Sender).FocusedNode;
    if Assigned(Node) then
      TVirtualStringTree(Sender).FullCollapse(Node);
  end
  else if (Key = VK_DOWN) and (ssCtrl in Shift) then
  begin
    Node := TVirtualStringTree(Sender).FocusedNode;
    if Assigned(Node) and Assigned(Node.FirstChild) then
    begin
      TVirtualStringTree(Sender).FocusedNode := Node^.FirstChild;
      TVirtualStringTree(Sender).Selected[Node^.FirstChild] := True;
      EditDocument;
      Key := 0;
    end;
  end;
end;

end.
