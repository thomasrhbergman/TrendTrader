unit OpenDialog.OrderGroupSet;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids,
  DaModule, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, System.StrUtils, Monitor.Types,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Vcl.Menus, Edit.OrderGroupSet, Frame.DocumentsTree,
  Common.Types, DaImages, MessageDialog, VirtualTrees, Vcl.TitleBarCtrls, Document, MonitorTree.Helper, MonitorTree.Document;
{$ENDREGION}

type
  TfrmOrderGroupSet = class(TCustomForm)
    ActionListMain: TActionList;
    aDelete: TAction;
    aEdit: TAction;
    aSelect: TAction;
    btnDelete: TBitBtn;
    btnEdit: TBitBtn;
    btnOpen: TBitBtn;
    frameOrderGroups: TframeDocumentsTree;
    miDelete: TMenuItem;
    miEdit: TMenuItem;
    pnlBottom: TPanel;
    PopupMenu: TPopupMenu;
    procedure aSelectExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aEditExecute(Sender: TObject);
  private
    FMarkedNode: TMarkedNode;
    FTypeUse: TTypeUseInAutoorders;
  public
    procedure Initialize;
    procedure Deinitialize;
    class function ShowDocument(var aOrderGroupSet: TOrderGroupSetDoc; aTypeUse: TTypeUseInAutoorders): TModalResult;
  end;

implementation

{$R *.dfm}

class function TfrmOrderGroupSet.ShowDocument(var aOrderGroupSet: TOrderGroupSetDoc; aTypeUse: TTypeUseInAutoorders): TModalResult;
begin
  with TfrmOrderGroupSet.Create(nil) do
    try
      FTypeUse := aTypeUse;
      FMarkedNode := Default(TMarkedNode);
      FMarkedNode.RecordId := aOrderGroupSet.RecordId;
      FMarkedNode.DocType  := TDocType.ntOrderGroupSet;
      Initialize;
      ShowModal;
      Result := ModalResult;
      if (Result = mrOk) then
      begin
        aOrderGroupSet.RecordId := frameOrderGroups.GetHigherNodeId(ntOrderGroupSet).RecordId;
        if (aOrderGroupSet.RecordId > -1) then
          aOrderGroupSet.FromDB(aOrderGroupSet.RecordId);
        Deinitialize;
      end;
    finally
      Free;
    end;
end;

procedure TfrmOrderGroupSet.Initialize;
begin
  frameOrderGroups.ReadOnly := True;
  frameOrderGroups.DragAllowed := False;
  frameOrderGroups.DocType := ntOrderGroupSet;
  frameOrderGroups.TypeUse := FTypeUse;
  frameOrderGroups.Initialize;
  frameOrderGroups.MarkedNode := FMarkedNode;
end;

procedure TfrmOrderGroupSet.Deinitialize;
begin
  frameOrderGroups.Deinitialize;
end;

procedure TfrmOrderGroupSet.aDeleteExecute(Sender: TObject);
resourcestring
  C_SQL_DELETE = 'DELETE FROM ORDER_GROUP_SET WHERE ID=%d';
var
  RecordId: Integer;
begin
  RecordId := frameOrderGroups.GetHigherNodeId(ntOrderGroupSet).RecordId;
  if (RecordId > 0) and (TMessageDialog.ShowQuestion('Current OrderGroups will be deleted. Continue?') = mrYes) then
  begin
    DMod.ExecuteSQL(Format(C_SQL_DELETE, [RecordId]));
    TTreeDocument.DeleteRelations(RecordId);
    frameOrderGroups.LoadTree;
  end;
end;

procedure TfrmOrderGroupSet.aEditExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Node := frameOrderGroups.vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    if (TTreeDocument.GetDocType(frameOrderGroups.vstTree, Node) <> ntOrderGroupSet) then
      Node := TTreeDocument.GetParentNode(frameOrderGroups.vstTree, Node, ntOrderGroupSet);
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if TfrmEditOrderGroupSet.ShowDocument(Data^.OrderGroupSetDoc) = mrOk then
      begin
        Data^.OrderGroupSetDoc.SaveToDB;
        frameOrderGroups.LoadTree;
      end;
    end;
  end;
end;

procedure TfrmOrderGroupSet.aSelectExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
