unit AutoTrades;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids, DaModule,
  System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, Vcl.Menus, Monitor.Types, Frame.DocumentsTree,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging,{$ENDIF} CustomForms, DebugWriter, HtmlLib, Scanner.Types, MonitorTree.Document,
  MessageDialog, AutoTrades.Edit, Document, VirtualTrees, AutoTrades.Types, Common.Types, DaImages, MonitorTree.Helper;
{$ENDREGION}

type
  TfrmAutoTrades = class(TCustomForm)
    aCancel: TAction;
    aCreate: TAction;
    ActionListMain: TActionList;
    aDelete: TAction;
    aEdit: TAction;
    aSelect: TAction;
    btnDelete: TBitBtn;
    btnEdit: TBitBtn;
    btnOpen: TBitBtn;
    frameAutoTrades: TframeDocumentsTree;
    miCreate: TMenuItem;
    pnlBottom: TPanel;
    procedure aCancelExecute(Sender: TObject);
    procedure aCreateExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aEditExecute(Sender: TObject);
    procedure aEditUpdate(Sender: TObject);
    procedure aSelectExecute(Sender: TObject);
  private
    FMarkedNode: TMarkedNode;
  public
    class function ShowDocument(aMarkedNode: TMarkedNode): Integer;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmAutoTrades.ShowDocument(aMarkedNode: TMarkedNode): Integer;
begin
  Result := -1;
  with TfrmAutoTrades.Create(nil) do
  try
    FMarkedNode := aMarkedNode;
    Initialize;
    if (ShowModal = mrOk) then
      Result := frameAutoTrades.GetHigherNodeId(ntAutoTrade).RecordId;
    Denitialize;
  finally
    Free;
  end;
end;

procedure TfrmAutoTrades.Initialize;
begin
//  frameAutoTrades.ReadOnly := True;
  frameAutoTrades.DragAllowed := False;
  frameAutoTrades.DocType := ntAutoTrade;
  frameAutoTrades.TypeUse := [tuTemplate];
  frameAutoTrades.Initialize;
  frameAutoTrades.MarkedNode := FMarkedNode;
  frameAutoTrades.GroupView := gvTree;
end;

procedure TfrmAutoTrades.Denitialize;
begin
  frameAutoTrades.Deinitialize;
end;

procedure TfrmAutoTrades.aDeleteExecute(Sender: TObject);
var
  Id: Integer;
begin
  Id := frameAutoTrades.GetHigherNodeId(ntAutoTrade).RecordId;
  if (Id > 0) and (TMessageDialog.ShowQuestion('Current AutoTrade will be deleted. Continue?') = mrYes) then
  begin
    TAutoTradeInfo.DeleteFromDB(Id);
    frameAutoTrades.LoadTree;
  end;
end;

procedure TfrmAutoTrades.aCreateExecute(Sender: TObject);
var
  AutoTrade: TAutoTradeInfo;
begin
  AutoTrade.Clear;
  AutoTrade.RecordId                := -1;
  AutoTrade.Active                  := True;
  AutoTrade.AllowSendDuplicateOrder := False;
  AutoTrade.AutoRefresh             := True;
  AutoTrade.Enabled                 := True;
  AutoTrade.MaxNumberOrder          := 1;
  AutoTrade.MaxRows                 := 10;
  AutoTrade.Name                    := 'Auto Trade';
  AutoTrade.OrderAmount             := 1000;
  AutoTrade.OrderCurrency           := C_DEFAULT_CURRENCY;

  if (TfrmAutoTradesEdit.ShowEditForm(AutoTrade, dmInsert) = mrOk) then
  begin
    AutoTrade.SaveToDB;
    DMod.RefreshQuery(DMod.fbqAutoTrades);
    frameAutoTrades.LoadTree;
  end;

end;

procedure TfrmAutoTrades.aEditExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Node := frameAutoTrades.vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    if (TTreeDocument.GetDocType(frameAutoTrades.vstTree, Node) <> ntAutoTrade) then
      Node := TTreeDocument.GetParentNode(frameAutoTrades.vstTree, Node, ntAutoTrade);
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if (TfrmAutoTradesEdit.ShowEditForm(Data^.AutoTrade, dmUpdate) = mrOk) then
      begin
        Data^.AutoTrade.SaveToDB;
        frameAutoTrades.LoadTree;
      end;
    end;
  end;
end;

procedure TfrmAutoTrades.aEditUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(frameAutoTrades.vstTree.FocusedNode);
end;

procedure TfrmAutoTrades.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmAutoTrades.aSelectExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
