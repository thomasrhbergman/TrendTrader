unit ListForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CustomForms, Vcl.ExtCtrls, VirtualTrees,
  Vcl.StdCtrls, Vcl.Buttons, Common.Types, DaModule, FireDAC.Comp.Client, FireDAC.Stan.Param,
  VirtualTrees.Types, Qualifiers.Edit, System.Actions, Vcl.ActnList,
  Qualifiers.Types, MessageDialog, MonitorTree.Document, Candidate.Types,
  Candidate.Main, Quantities.Edit, Quantities.Types, OrderTemplate.Edit,
  OrderTemplate.Types;

type
  TListNodeItem = record
    ID: integer;
    Name: string;
    Active: boolean;
  end;
  PListNodeItem = ^TListNodeItem;

  TfrmListForm = class(TCustomForm)
    vstList: TVirtualStringTree;
    pnlButtons: TPanel;
    btnNew: TBitBtn;
    btnDelete: TBitBtn;
    btnEdit: TBitBtn;
    btnSelect: TBitBtn;
    btnExit: TBitBtn;
    ActionList1: TActionList;
    actNew: TAction;
    actDelete: TAction;
    actEdit: TAction;
    actSelect: TAction;
    actExit: TAction;
    procedure vstListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FormCreate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actSelectExecute(Sender: TObject);
  private
    { Private declarations }
    FDocType: TDocType;
    FSelectedId: Integer;
  public
    { Public declarations }
    procedure PopulateList(aID: integer = 0);
  end;

  function ShowList(aDocType: TDocType): Integer;

implementation

{$R *.dfm}

function ShowList(aDocType: TDocType): Integer;
begin
  Result := -1;
  if not (aDocType  in [ntAutoTrade, ntQualifier, ntCandidates, ntQuantities, ntOrderTemplate]) then
  begin
    TMessageDialog.ShowWarning('List is implemented only for AutoTrades, Qualifiers, Candidate processes, Quantities and Order Templates');
    Exit;
  end;
  with TfrmListForm.Create(nil) do
  try
    FDocType := aDocType;
    case FDocType of
      ntAutoTrade: Caption := 'Autotrades list';
      ntQualifier: Caption := 'Qualifiers list';
      ntCandidates: Caption := 'Candidate processes list';
      ntQuantities: Caption := 'Quantities list';
      ntOrderTemplate: Caption := 'Order templates list';
    end;
    PopulateList;
    ShowModal;
    if (ModalResult = mrOk) then
    begin
      Result := FSelectedId;
      DMod.RefreshQuery(DMod.fbqQualifiers);
    end;
  finally
    Free;
  end;
end;

{ TfrmListForm }

procedure TfrmListForm.actDeleteExecute(Sender: TObject);
var Data: PListNodeItem;
    Node: PVirtualNode;
begin
  inherited;
  Node := vstList.FocusedNode;
  if not Assigned(Node) then Exit;
  Data := Node^.GetData;

  case FDocType of
    ntAutoTrade: ;
    ntQualifier: begin
        if (Data.ID > 0) and (TMessageDialog.ShowQuestion('Current Qualifier will be deleted. Continue?') = mrYes) then
        begin
          TTreeDocument.DeleteRelations(Data.ID);
          DMod.RefreshQuery(DMod.fbqQualifiers);
          PopulateList;
          //TODO delete from table and clear fields in autotrade
        end;
      end;
    ntCandidates: begin
        if (Data.ID > 0) and (TMessageDialog.ShowQuestion('Current Candidate will be deleted. Continue?') = mrYes) then
        begin
          TCandidateInfo.DeleteFromDB(Data.ID);
          PopulateList;
          //TODO delete from table and clear fields in autotrade
        end;
      end;
    ntQuantities: begin
        if (Data.ID > 0) and (TMessageDialog.ShowQuestion('Current Quantity will be deleted. Continue?') = mrYes) then
        begin
          TQuantity.DeleteFromDB(Data.ID);
          PopulateList;
          //TODO delete from table and clear fields in autotrade
        end;
      end;
    ntOrderTemplate: begin
        if (Data.ID > 0) and (TMessageDialog.ShowQuestion('Current Order Template will be deleted. Continue?') = mrYes) then
        begin
          TOrderTemplate.DeleteFromDB(Data.ID);
          PopulateList;
          //TODO delete from table and clear fields in autotrade
        end;
      end;
  end;
end;

procedure TfrmListForm.actEditExecute(Sender: TObject);
var FQualifier: TQualifier;
    FCandidate: TCandidateInfo;
    FQuantity: TQuantity;
    FOrderTemplate: TOrderTemplate;
    Data: PListNodeItem;
    Node: PVirtualNode;
begin
  inherited;
  Node := vstList.FocusedNode;
  if not Assigned(Node) then Exit;
  Data := Node^.GetData;

  case FDocType of
    ntAutoTrade: ;
    ntQualifier: begin
        FQualifier := TQualifier.Create;
        try
          FQualifier.FromDB(Data.ID);
          if (TfrmQualifierEdit.ShowDocument(FQualifier, dmUpdate) = mrOk) then
          begin
            FQualifier.SaveToDB;
            PopulateList(FQualifier.RecordId);
          end;
        finally
          FQualifier.Free;
        end;
      end;
    ntCandidates: begin
        FCandidate.FromDB(Data.ID);
        if (TfrmCandidateMain.ShowDocument(FCandidate, dmUpdate) = mrOk) then
        begin
          FCandidate.SaveToDB;
          PopulateList(FCandidate.RecordId);
        end;
      end;
    ntQuantities: begin
        FQuantity := TQuantity.Create;
        try
          FQuantity.FromDB(Data.ID);
          if (TfrmQuantityEdit.ShowDocument(FQuantity, dmUpdate) = mrOk) then
          begin
            FQuantity.SaveToDB;
            PopulateList(FQuantity.RecordId);
          end;
        finally
          FQuantity.Free;
        end;
      end;
    ntOrderTemplate: begin
        FOrderTemplate := TOrderTemplate.Create;
        try
          FOrderTemplate.FromDB(Data.ID);
          if (TfrmOrderTemplateEdit.ShowDocument(FOrderTemplate, dmUpdate) = mrOk) then
          begin
            FOrderTemplate.SaveToDB;
            PopulateList(FOrderTemplate.RecordId);
          end;
        finally
          FOrderTemplate.Free;
        end;
      end;
  end;
end;

procedure TfrmListForm.actExitExecute(Sender: TObject);
begin
  inherited;
  ModalResult := mrCancel;
end;

procedure TfrmListForm.actNewExecute(Sender: TObject);
var FQualifier: TQualifier;
    FCandidate: TCandidateInfo;
    FQuantity: TQuantity;
    FOrderTemplate: TOrderTemplate;
begin
  inherited;
  case FDocType of
    ntAutoTrade: ;
    ntQualifier: begin
        FQualifier := TQualifier.Create;
        try
          if (TfrmQualifierEdit.ShowDocument(FQualifier, dmInsert) = mrOk) then
          begin
            FQualifier.SaveToDB;
            PopulateList(FQualifier.RecordId);
          end;
        finally
          FQualifier.Free;
        end;
      end;
    ntCandidates: begin
        FCandidate.Clear;
        if (TfrmCandidateMain.ShowDocument(FCandidate, dmInsert) = mrOk) then
        begin
          FCandidate.SaveToDB;
          PopulateList(FCandidate.RecordId);
        end;
      end;
    ntQuantities: begin
        FQuantity := TQuantity.Create;
        try
          if (TfrmQuantityEdit.ShowDocument(FQuantity, dmInsert) = mrOk) then
          begin
            FQuantity.SaveToDB;
            PopulateList(FQuantity.RecordId);
          end;
        finally
          FQuantity.Free;
        end;
      end;
    ntOrderTemplate: begin
        FOrderTemplate := TOrderTemplate.Create;
        try
          if (TfrmOrderTemplateEdit.ShowDocument(FOrderTemplate, dmInsert) = mrOk) then
          begin
            FOrderTemplate.SaveToDB;
            PopulateList(FOrderTemplate.RecordId);
          end;
        finally
          FOrderTemplate.Free;
        end;
      end;
  end;
end;

procedure TfrmListForm.actSelectExecute(Sender: TObject);
var Data: PListNodeItem;
    Node: PVirtualNode;
begin
  inherited;
  Node := vstList.FocusedNode;
  if not Assigned(Node) then
  begin
    TMessageDialog.ShowWarning('Please, select item in the list');
    Exit;
  end;
  Data := Node^.GetData;
  FSelectedId := Data.ID;
  ModalResult := mrOk;
end;

procedure TfrmListForm.FormCreate(Sender: TObject);
begin
  inherited;
  vstList.NodeDataSize := SizeOf(TListNodeItem);
end;

procedure TfrmListForm.PopulateList(aID: integer = 0);
const
  C_SQL_SELECT_AUTOTRADES = 'SELECT ID, NAME, ACTIVE FROM AUTOTRADES ORDER BY LOWER(NAME)';
  C_SQL_SELECT_QUALIFIERS = 'SELECT ID, NAME FROM QUALIFIERS ORDER BY LOWER(NAME)';
  C_SQL_SELECT_CANDIDATES = 'SELECT ID, NAME FROM CANDIDATES ORDER BY LOWER(NAME)';
  C_SQL_SELECT_QUANTITIES = 'SELECT ID, NAME FROM QUANTITIES ORDER BY LOWER(NAME)';
  C_SQL_SELECT_ORDER_TEMPLATES = 'SELECT ID, NAME FROM ORDER_TEMPLATES ORDER BY LOWER(NAME)';

var Q: TFDQuery;
    Data: PListNodeItem;
    Node, FocusNode: PVirtualNode;
begin
  FocusNode := nil;
  vstList.Clear;
  Q := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Q.Connection := DMod.ConnectionStock;
    case FDocType of
      ntAutoTrade: Q.SQL.Text := C_SQL_SELECT_AUTOTRADES;
      ntQualifier: Q.SQL.Text := C_SQL_SELECT_QUALIFIERS;
      ntCandidates: Q.SQL.Text := C_SQL_SELECT_CANDIDATES;
      ntQuantities: Q.SQL.Text := C_SQL_SELECT_QUANTITIES;
      ntOrderTemplate: Q.SQL.Text := C_SQL_SELECT_ORDER_TEMPLATES;
    end;
    Q.Open;
    if Assigned(Q.FindField('ACTIVE')) then
      vstList.Header.Columns.Items[1].Options := vstList.Header.Columns.Items[1].Options + [coVisible]
    else
      vstList.Header.Columns.Items[1].Options := vstList.Header.Columns.Items[1].Options - [coVisible];
    while not Q.Eof do
    begin
      Node := vstList.AddChild(vstList.RootNode);
      Data := Node^.GetData;
      Data.ID := Q.FieldByName('ID').AsInteger;
      Data.Name := Q.FieldByName('NAME').AsString;
      if Assigned(Q.FindField('ACTIVE')) then
        Data.Active := Q.FieldByName('ACTIVE').AsBoolean;
      if Data.ID = aID then
        FocusNode := Node;
      Q.Next;
    end;
  finally
    Q.Free;
  end;
  if Assigned(FocusNode) then
  begin
    vstList.FocusedNode := FocusNode;
    vstList.Selected[FocusNode] := True;
  end;
end;

procedure TfrmListForm.vstListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PListNodeItem;
begin
  inherited;
  if (Column = 0) then
  begin
    Data := Node^.GetData;
    CellText := Data.Name;
  end;
end;

end.
