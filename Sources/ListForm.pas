unit ListForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CustomForms, Vcl.ExtCtrls, VirtualTrees,
  Vcl.StdCtrls, Vcl.Buttons, Common.Types, DaModule, FireDAC.Comp.Client, FireDAC.Stan.Param,
  VirtualTrees.Types, System.Actions, Vcl.ActnList, MessageDialog, DaImages, Global.Types,
  System.Generics.Collections;

type
  TListNodeItem = record
    ID: integer;
    Name: string;
    Active: boolean;
  end;
  PListNodeItem = ^TListNodeItem;

  TListMode = (lmShow, lmSelect);

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
    procedure vstListAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure FormShow(Sender: TObject);
    procedure vstListDblClick(Sender: TObject);
    procedure vstListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    { Private declarations }
    FBaseClass: TCustomBaseClass;
    FFormClass: TCustomFormClass;
    FSelectedId: Integer;
    FListMode: TListMode;
  public
    { Public declarations }
    procedure PopulateList(aID: integer = 0);
  end;

type
  TListFormRecord = record
    BaseClass: TCustomBaseClass;
    FormClass: TCustomFormClass;
  end;

  TListFormFactory = class
  private
    FDictionary: TDictionary<TDocType, TListFormRecord>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterList(const AType: TDocType; ABaseClass: TCustomBaseClass; AFormClass: TCustomFormClass);
    procedure Show(const AType: TDocType);
    function Select(const AType: TDocType; const AId: integer = 0): Integer;
  end;

var
  ListFormFactory: TListFormFactory;

implementation

{$R *.dfm}

{ TfrmListForm }

procedure TfrmListForm.actDeleteExecute(Sender: TObject);
var Data: PListNodeItem;
    Node: PVirtualNode;
begin
  inherited;
  Node := vstList.FocusedNode;
  if not Assigned(Node) then Exit;
  Data := Node^.GetData;
  if (Data.ID > 0) and (TMessageDialog.ShowQuestion('Current record will be deleted. Continue?') = mrYes) then
  begin
    FBaseClass.DeleteFromDB(Data.ID);
    PopulateList;
  end;
end;

procedure TfrmListForm.actEditExecute(Sender: TObject);
var FItem: TBaseClass;
    Data: PListNodeItem;
    Node: PVirtualNode;
begin
  inherited;
  Node := vstList.FocusedNode;
  if not Assigned(Node) then Exit;
  Data := Node^.GetData;
  FItem := FBaseClass.Create;
  try
    FItem.FromDB(Data.ID);
    if (FFormClass.ShowEditForm(FItem, dmUpdate) = mrOk) then
    begin
      FItem.SaveToDB;
      PopulateList(FItem.RecordId);
    end;
  finally
    FItem.Free;
  end;
end;

procedure TfrmListForm.actExitExecute(Sender: TObject);
begin
  inherited;
  ModalResult := mrCancel;
end;

procedure TfrmListForm.actNewExecute(Sender: TObject);
var FItem: TBaseClass;
begin
  inherited;
  FItem := FBaseClass.Create;
  try
    if (FFormClass.ShowEditForm(FItem, dmInsert) = mrOk) then
    begin
      FItem.SaveToDB;
      PopulateList(FItem.RecordId);
    end;
  finally
    FItem.Free;
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

procedure TfrmListForm.FormShow(Sender: TObject);
begin
  inherited;
  actSelect.Enabled := FListMode = lmSelect;
  Caption := FBaseClass.GetListCaption;
  PopulateList(FSelectedId);
end;

procedure TfrmListForm.PopulateList(aID: integer = 0);
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
    Q.SQL.Text := FBaseClass.GetListSQL;
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

procedure TfrmListForm.vstListAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  Data: PListNodeItem;
  Idx: Integer;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
  begin
    if Column = 1 then
    begin
      if Data.Active then
        Idx := 71
      else
        Idx := 72;
      DMImage.vil16.Draw(TargetCanvas, CellRect.Left + (CellRect.Width - 16) div 2, CellRect.Top, Idx);
    end;
  end;
end;

procedure TfrmListForm.vstListDblClick(Sender: TObject);
begin
  inherited;
  if FListMode = lmShow then
    actEdit.Execute
  else
    actSelect.Execute;
end;

procedure TfrmListForm.vstListFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Item: PListNodeItem;
begin
  Item := Node^.GetData;
  Item.Name := '';
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
  end
  else if (Column = 1) then
    CellText := '';
end;

{ TListFormFactory }

constructor TListFormFactory.Create;
begin
  FDictionary := TDictionary<TDocType, TListFormRecord>.Create;
end;

destructor TListFormFactory.Destroy;
begin
  if Assigned(FDictionary) then
    FDictionary.Free;
  inherited;
end;

procedure TListFormFactory.RegisterList(const AType: TDocType;
  ABaseClass: TCustomBaseClass; AFormClass: TCustomFormClass);
var lRecord: TListFormRecord;
begin
  lRecord.BaseClass := ABaseClass;
  lRecord.FormClass := AFormClass;
  FDictionary.AddOrSetValue(AType, lRecord);
end;

function TListFormFactory.Select(const AType: TDocType; const AId: integer = 0): Integer;
var
  lRecord: TListFormRecord;
begin
  Result := -1;
  if FDictionary.TryGetValue(AType, lRecord) then
  begin
    with TfrmListForm.Create(nil) do
    try
      FBaseClass := lRecord.BaseClass;
      FFormClass := lRecord.FormClass;
      FListMode := lmSelect;
      FSelectedId := AId;
      ShowModal;
      if (ModalResult = mrOk) then
        Result := FSelectedId;
    finally
      Free;
    end;
  end;
end;

procedure TListFormFactory.Show(const AType: TDocType);
var
  lRecord: TListFormRecord;
begin
  if FDictionary.TryGetValue(AType, lRecord) then
  begin
    with TfrmListForm.Create(nil) do
    try
      FBaseClass := lRecord.BaseClass;
      FFormClass := lRecord.FormClass;
      FListMode := lmShow;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

initialization
  ListFormFactory := TListFormFactory.Create;

finalization
  if Assigned(ListFormFactory) then
    ListFormFactory.Free;

end.
