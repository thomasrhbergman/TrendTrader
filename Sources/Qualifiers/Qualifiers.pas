unit Qualifiers;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons,
  Vcl.DBGrids, DaModule, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, Vcl.Menus,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging,{$ENDIF} CustomForms, DebugWriter, HtmlLib, Qualifiers.Types,
  System.Math, Frame.DocumentsTree, Document, Monitor.Types, VirtualTrees, Qualifiers.Edit, MessageDialog,
  AutoTrades.Edit, Common.Types, DaImages, MonitorTree.Helper, MonitorTree.Document;
{$ENDREGION}

type
  TfrmQualifiers = class(TCustomForm)
    aCreate: TAction;
    ActionListMain: TActionList;
    aDeleteQualifier: TAction;
    aEditQualifier: TAction;
    aSelect: TAction;
    btnDelete: TBitBtn;
    btnEdit: TBitBtn;
    btnOpen: TBitBtn;
    frameQualifiers: TframeDocumentsTree;
    miCreate: TMenuItem;
    pnlBottom: TPanel;
    procedure aCreateExecute(Sender: TObject);
    procedure aDeleteQualifierExecute(Sender: TObject);
    procedure aEditQualifierExecute(Sender: TObject);
    procedure aEditQualifierUpdate(Sender: TObject);
    procedure aSelectExecute(Sender: TObject);
  private
    FMarkedNode: TMarkedNode;
  public
    class function ShowDocument(aMarkedNode: TMarkedNode): Integer;
    procedure Initialize;
    procedure Deinitialize;
  end;

implementation

{$R *.dfm}

class function TfrmQualifiers.ShowDocument(aMarkedNode: TMarkedNode): Integer;
begin
  with TfrmQualifiers.Create(nil) do
  try
    FMarkedNode := aMarkedNode;
    Initialize;
    ShowModal;
    if (ModalResult = mrOk) then
    begin
      Result := frameQualifiers.GetHigherNodeId(ntQualifier).RecordId;
      DMod.RefreshQuery(DMod.fbqQualifiers);
    end
    else
      Result := -1;
    Deinitialize;
  finally
    Free;
  end;
end;

procedure TfrmQualifiers.Initialize;
begin
  frameQualifiers.DragAllowed := False;
  frameQualifiers.DocType := ntQualifier;
  frameQualifiers.TypeUse := [tuTemplate];
  frameQualifiers.Initialize;
  frameQualifiers.MarkedNode := FMarkedNode;
  frameQualifiers.GroupView := gvTree;
end;

procedure TfrmQualifiers.Deinitialize;
begin
  frameQualifiers.Deinitialize;
end;

procedure TfrmQualifiers.aCreateExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Node := TTreeDocument.CreateQualifier(nil, frameQualifiers.vstTree);
  Data := Node^.GetData;
  Data^.Qualifier.Name := '';
  Data^.Qualifier.SaveToDB;
  TTreeDocument.SaveRelationTree(frameQualifiers.vstTree, Node);
  if (TfrmQualifierEdit.ShowDocument(Data^.Qualifier, dmInsert) = mrOk) then
  begin
    Node := TTreeDocument.LoadRelationTree(Data^.RecordId, -1, frameQualifiers.vstTree, nil, nil, nil);
    if (frameQualifiers.GroupView = gvFlat) then
      frameQualifiers.vstTree.DeleteChildren(Node);
    DMod.RefreshQuery(DMod.fbqQualifiers);
  end;
end;

procedure TfrmQualifiers.aDeleteQualifierExecute(Sender: TObject);
var
  RecordId: Integer;
begin
  RecordId := frameQualifiers.GetHigherNodeId(ntQualifier).RecordId;
  if (RecordId > 0) and (TMessageDialog.ShowQuestion('Current Qualifier will be deleted. Continue?') = mrYes) then
  begin
    TTreeDocument.DeleteRelations(RecordId);
    DMod.RefreshQuery(DMod.fbqQualifiers);
    frameQualifiers.LoadTree;
  end;
end;

procedure TfrmQualifiers.aEditQualifierExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PTreeData;
begin
  Node := frameQualifiers.vstTree.FocusedNode;
  Data := Node^.GetData;
  if Assigned(Node) then
  begin
    if (TTreeDocument.GetDocType(frameQualifiers.vstTree, Node) <> ntQualifier) then
      Node := TTreeDocument.GetParentNode(frameQualifiers.vstTree, Node, ntQualifier);
    if Assigned(Node) then
      if TfrmQualifierEdit.ShowDocument(Data^.Qualifier, dmUpdate) = mrOk then
      begin
        Data := Node.GetData;
        Data.Qualifier.SaveToDB;
        //Data^.QualifierCondition.SaveToDB;
      end;
  end;
end;

procedure TfrmQualifiers.aEditQualifierUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(frameQualifiers.vstTree.FocusedNode);
end;

procedure TfrmQualifiers.aSelectExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
