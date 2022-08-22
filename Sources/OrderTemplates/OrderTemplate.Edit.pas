unit OrderTemplate.Edit;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin, System.Math, Vcl.DBCtrls,
  System.Generics.Collections, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABFunctions, IABSocketAPI,
  IABSocketAPI_const, OrderTemplate.Types, Vcl.ComCtrls, MessageDialog, DaModule, System.Actions, Vcl.ActnList,
  Search.Instruments,  VirtualTrees, Entity.Sokid, Data.DB, Scanner.Types, Monitor.Types, Document,
  System.DateUtils, BrokerHelperAbstr, Common.Types, DaImages, Global.Types, Vcl.Imaging.pngimage, Vcl.VirtualImage,
  Global.Resources, IABFunctions.Helpers, Vcl.NumberBox, Publishers.Interfaces, Publishers, InstrumentList,
  IABFunctions.MarketData, Utils, Vcl.Menus, MonitorTree.Document, Edit.OrderDocument, MonitorTree.Helper,
  Edit.Condition, Edit.OrderGroup, HtmlConsts, Monitor.Info, InformationDialog, System.Types;
{$ENDREGION}

type
  TfrmOrderTemplateEdit = class(TCustomForm)
    ActionListMain: TActionList;
    aSave: TAction;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    edtName: TEdit;
    lblName: TLabel;
    pnlBottom: TPanel;
    pnlTypeCondition: TPanel;
    vstTree: TVirtualStringTree;
    ActionList: TActionList;
    aShowInformation: TAction;
    aAddOrderGroup: TAction;
    aAddOrder: TAction;
    aAddCondition: TAction;
    aClearTree: TAction;
    aCollapsTree: TAction;
    aDeleteSelectedNode: TAction;
    aEdit: TAction;
    aExpandTree: TAction;
    aOpen: TAction;
    aRefresh: TAction;
    aSaveAs: TAction;
    Action1: TAction;
    aInfo: TAction;
    pmTree: TPopupMenu;
    miEdit: TMenuItem;
    miDeleteSelectedNode: TMenuItem;
    miClearTree: TMenuItem;
    miShowInformation: TMenuItem;
    miSep01: TMenuItem;
    miAddOrderGroup: TMenuItem;
    miAddOrder: TMenuItem;
    miAddCondition: TMenuItem;
    miSep02: TMenuItem;
    miExpandTree: TMenuItem;
    miCollapsTree: TMenuItem;
    procedure aSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstTreeAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure vstTreeBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const Text: string;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure aAddConditionUpdate(Sender: TObject);
    procedure aAddConditionExecute(Sender: TObject);
    procedure pmTreePopup(Sender: TObject);
    procedure aAddOrderGroupExecute(Sender: TObject);
    procedure aAddOrderGroupUpdate(Sender: TObject);
    procedure aAddOrderUpdate(Sender: TObject);
    procedure aAddOrderExecute(Sender: TObject);
    procedure vstTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
    procedure aEditExecute(Sender: TObject);
    procedure aDeleteSelectedNodeExecute(Sender: TObject);
    procedure aClearTreeExecute(Sender: TObject);
    procedure aCollapsTreeExecute(Sender: TObject);
    procedure aExpandTreeExecute(Sender: TObject);
    procedure aInfoExecute(Sender: TObject);
    procedure aShowInformationExecute(Sender: TObject);
    procedure aShowInformationUpdate(Sender: TObject);
    procedure aDeleteSelectedNodeUpdate(Sender: TObject);
    procedure aEditUpdate(Sender: TObject);
    procedure vstTreeDblClick(Sender: TObject);
  private
    FOrderTemplate: TOrderTemplate;
    function CheckData: Boolean;
    procedure AddFirstOrder;
    procedure EditDocument;
    procedure LoadTree;
  public
    class function ShowDocument(aOrderTemplate: TOrderTemplate; aDialogMode: TDialogMode): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmOrderTemplateEdit.ShowDocument(aOrderTemplate: TOrderTemplate; aDialogMode: TDialogMode): TModalResult;
begin
  with TfrmOrderTemplateEdit.Create(nil) do
  try
    DialogMode := aDialogMode;
    FOrderTemplate.AssignFrom(aOrderTemplate);
    Initialize;
    Result := ShowModal;
    if (Result = mrOk) then
    begin
      Denitialize;
      aOrderTemplate.AssignFrom(FOrderTemplate);
    end;
  finally
    Free;
  end;
end;

procedure TfrmOrderTemplateEdit.vstTreeAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
  inherited;
  TMonitorTree.OnAfterCellPaint(Sender, TargetCanvas, Node, Column, CellRect);
end;

procedure TfrmOrderTemplateEdit.vstTreeBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  inherited;
  TMonitorTree.OnBeforeCellPaint(Sender, TargetCanvas, Node, Column, CellPaintMode, CellRect, ContentRect);
end;

procedure TfrmOrderTemplateEdit.vstTreeDblClick(Sender: TObject);
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

procedure TfrmOrderTemplateEdit.vstTreeDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  inherited;
  TMonitorTree.OnDrawText(Sender, TargetCanvas, Node, Column, Text, CellRect, DefaultDraw);
end;

procedure TfrmOrderTemplateEdit.vstTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
  inherited;
  TMonitorTree.OnGetHint(Sender, Node, Column, LineBreakStyle, HintText);
end;

procedure TfrmOrderTemplateEdit.vstTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  inherited;
  TMonitorTree.OnGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);
end;

procedure TfrmOrderTemplateEdit.vstTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  inherited;
  TMonitorTree.OnGetText(Sender, Node, Column, TextType, CellText);
end;

procedure TfrmOrderTemplateEdit.FormCreate(Sender: TObject);
begin
  FOrderTemplate := TOrderTemplate.Create;
  vstTree.NodeDataSize := SizeOf(TTreeData);
end;

procedure TfrmOrderTemplateEdit.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FOrderTemplate);
end;

procedure TfrmOrderTemplateEdit.Initialize;
resourcestring
  rsCaption = '%s (v.%s)';
begin
  if FOrderTemplate.Name.IsEmpty then
    edtName.Text := 'Order Template'
  else
    edtName.Text := FOrderTemplate.Name;

  case DialogMode of
    dmInsert:
      Self.Caption := Format(rsCaption, ['New Order Template', General.ModuleVersion]);
    dmUpdate:
      Self.Caption := Format(rsCaption, ['Edit Order Template', General.ModuleVersion]);
  end;
  LoadTree;
  AddFirstOrder;
end;

procedure TfrmOrderTemplateEdit.LoadTree;
begin
  if (FOrderTemplate.RecordId > 0) then
  begin
    vstTree.BeginUpdate;
    try
      vstTree.Clear;
      TTreeDocument.LoadOrderTemplateRelationTree(FOrderTemplate.RecordId, -1, vstTree, nil, nil, nil);
    finally
      vstTree.FullExpand(nil);
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TfrmOrderTemplateEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if (ModalResult = mrOk) then
  begin
    Denitialize;
    CanClose := CheckData;
  end;
end;

function TfrmOrderTemplateEdit.CheckData: Boolean;

  function CheckRequired: Boolean;
  var
    Problems: string;
  begin
    Problems := '';
    if FOrderTemplate.Name.IsEmpty then
    begin
      SetFocusSafely(edtName);
      Problems := Format(rcRequiredValue, ['Name']);
    end;

    Result := Problems.IsEmpty;
    if not Result then
      TMessageDialog.ShowWarning(Problems);
  end;

begin
  Result := CheckRequired;
end;

procedure TfrmOrderTemplateEdit.Denitialize;
begin
  FOrderTemplate.Name := edtName.Text;
end;

procedure TfrmOrderTemplateEdit.EditDocument;
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Node := vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    case Data^.DocType of
      ntOrderGroup:
        if TfrmEditOrderGroup.ShowDocument(Data^.OrderGroupDoc, True) = mrOk then
          Data^.OrderGroupDoc.SaveToDB;
      ntCondition:
        if TfrmEditCondition.ShowDocument(Data^.ConditionDoc, vstTree) = mrOk then
          Data^.ConditionDoc.SaveToDB;
      ntOrder:
        if TfrmOrderDocument.ShowDocument(Data^.OrderDoc, TTreeDocument.GetOrderSubordination(vstTree, Node)) = mrOk then
          Data^.OrderDoc.SaveToDB;
    end;
  end;
end;

procedure TfrmOrderTemplateEdit.aAddConditionExecute(Sender: TObject);
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

procedure TfrmOrderTemplateEdit.aAddConditionUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntOrder, ntCondition];
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmOrderTemplateEdit.aAddOrderExecute(Sender: TObject);
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

procedure TfrmOrderTemplateEdit.aAddOrderGroupExecute(Sender: TObject);
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

procedure TfrmOrderTemplateEdit.aAddOrderGroupUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Visible := (TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntOrder])
                              and
                             (vstTree.FocusedNode.Parent = vstTree.RootNode);
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmOrderTemplateEdit.aAddOrderUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstTree, vstTree.FocusedNode) in [ntOrderGroup];
  TAction(Sender).Enabled := TAction(Sender).Visible;
end;

procedure TfrmOrderTemplateEdit.aClearTreeExecute(Sender: TObject);
begin
  inherited;
  if (not vstTree.IsEmpty) and (TMessageDialog.ShowQuestion('Current Order Tepmplate will be cleared. Continue?') = mrYes) then
  begin
    vstTree.BeginUpdate;
    try
      vstTree.Clear;
      AddFirstOrder;
    finally
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TfrmOrderTemplateEdit.aCollapsTreeExecute(Sender: TObject);
begin
  inherited;
  vstTree.FullCollapse;
end;

procedure TfrmOrderTemplateEdit.AddFirstOrder;
var
  //Data: PTreeData;
  Node: PVirtualNode;
begin
  if not Assigned(vstTree.RootNode) or (vstTree.RootNode.ChildCount = 0) then
  begin
    Node := TTreeDocument.CreateOrder(vstTree.RootNode, vstTree, TBrokerType.brIB);
    //Data := Node^.GetData;
    TTreeDocument.SetIcon(Node, vstTree);
  end;
end;

procedure TfrmOrderTemplateEdit.aDeleteSelectedNodeExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  inherited;
  vstTree.BeginUpdate;
  try
    Node := vstTree.FocusedNode;
    if Assigned(Node) then
      vstTree.DeleteNode(Node);
    AddFirstOrder;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmOrderTemplateEdit.aDeleteSelectedNodeUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := not vstTree.IsEmpty;
end;

procedure TfrmOrderTemplateEdit.aEditExecute(Sender: TObject);
begin
  inherited;
  EditDocument;
end;

procedure TfrmOrderTemplateEdit.aEditUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := not vstTree.IsEmpty;
end;

procedure TfrmOrderTemplateEdit.aExpandTreeExecute(Sender: TObject);
begin
  inherited;
  vstTree.FullExpand;
end;

procedure TfrmOrderTemplateEdit.aInfoExecute(Sender: TObject);
var
  arrNodes: TNodeArray;
  sb: TStringBuilder;
begin
  inherited;
  arrNodes := TTreeDocument.GetNodesList(vstTree.RootNode);
  sb := TStringBuilder.Create;
  sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
    .Append(C_HTML_BODY_OPEN).AppendLine.Append(C_HTML_BREAK);
  try
    for var Node in arrNodes do
      sb.AppendLine(TDocumentInfo.GetNodeInfo(Node, False)).Append(C_HTML_LINE);
  finally
    sb.Append(C_HTML_BODY_CLOSE).AppendLine;
    TInformationDialog.ShowMessage(sb.ToString, 'OrderTemplateNodeInfo');
    FreeAndNil(sb);
  end;
end;

procedure TfrmOrderTemplateEdit.aSaveExecute(Sender: TObject);
begin
  FOrderTemplate.SaveToDB;
  TTreeDocument.DeleteOrderTemplateRelations(FOrderTemplate.RecordId);
  TTreeDocument.SaveOrderTemplateRelationTree(FOrderTemplate.RecordId, vstTree, vstTree.RootNode.FirstChild);
  ModalResult := mrOk;
end;

procedure TfrmOrderTemplateEdit.aShowInformationExecute(Sender: TObject);
var
  Info: string;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    Info := TDocumentInfo.GetNodeInfo(vstTree.FocusedNode);
    TInformationDialog.ShowMessage(Info, 'TemplateCreator');
  end;
end;

procedure TfrmOrderTemplateEdit.aShowInformationUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := Assigned(vstTree.FocusedNode);
end;

procedure TfrmOrderTemplateEdit.pmTreePopup(Sender: TObject);
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

end.
