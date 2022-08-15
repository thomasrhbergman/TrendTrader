unit Scanner.StaticList;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons,
  Vcl.DBGrids, DaModule, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, Common.Types,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging,{$ENDIF} Vcl.ImgList, System.ImageList, Vcl.Menus, Vcl.Mask,
  System.Math, Vcl.DBCtrls, Winapi.ActiveX, System.UITypes, VirtualTrees, VirtualTrees.Editors,
  BrokerHelperAbstr, Search.Instruments, DebugWriter, HtmlLib, Scanner.Types, CustomForms, MessageDialog,
  Entity.Sokid, IABSocketAPI, IABSocketAPI_const, IABFunctions, IABFunctions.RequestsQueue, DaImages,
  DaModule.Utils, Vcl.NumberBox, Monitor.Types, Global.Types, Publishers, MonitorTree.Helper,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client;
{$ENDREGION}

type
  TfrmScannerStaticList = class(TCustomForm)
    aCancel: TAction;
    ActionListMain: TActionList;
    aDeleteRow: TAction;
    aSave: TAction;
    aShowSearchForm: TAction;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    btnShowSearchForm: TBitBtn;
    dsStaticLists: TDataSource;
    edtName: TEdit;
    edtWeight: TNumberBox;
    lblName: TLabel;
    lblWeight: TLabel;
    miDeleteRow: TMenuItem;
    pmStaticList: TPopupMenu;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    vstTree: TVirtualStringTree;
    fbqStaticLists: TFDQuery;
    fbqStaticListsID: TIntegerField;
    fbqStaticListsWEIGHT: TSingleField;
    fbqStaticListsNAME: TStringField;
    procedure aCancelExecute(Sender: TObject);
    procedure aDeleteListExecute(Sender: TObject);
    procedure aDeleteRowExecute(Sender: TObject);
    procedure aDeleteRowUpdate(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aShowSearchFormExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure vstTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vstTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
  public const
    COL_INSTRUMENT = 0;
    COL_RANK = 1;
  private
    FRecordID: Integer;
    procedure AddInstruments(aTarget: PVirtualNode; const aSource: TVirtualStringTree);
    procedure LoadDetailList(const aRecordID: Integer);
    procedure SaveDetailList(const aRecordID: Integer);
  public
    class function ShowDocument(aRecordId: Integer): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmScannerStaticList.ShowDocument(aRecordId: Integer): TModalResult;
var
  frmScannerStaticList: TfrmScannerStaticList;
begin
  frmScannerStaticList := TfrmScannerStaticList.Create(nil);
  try
    frmScannerStaticList.FRecordID := aRecordId;
    frmScannerStaticList.Initialize;
    Result := frmScannerStaticList.ShowModal;
  finally
    frmScannerStaticList.Denitialize;
    FreeAndNil(frmScannerStaticList);
  end;
end;

procedure TfrmScannerStaticList.FormCreate(Sender: TObject);
begin
  vstTree.NodeDataSize := SizeOf(TInstrumentData);
end;

procedure TfrmScannerStaticList.Initialize;
begin
  TMonitorTree.Initialize(vstTree);
  DMod.CheckConnect;
  if (FRecordID > 0) then
  begin
    if fbqStaticLists.Transaction.Active then
      fbqStaticLists.Transaction.CommitRetaining;
    fbqStaticLists.Open;
    if fbqStaticLists.Locate('ID', FRecordID, []) then
    begin
      edtName.Text   := fbqStaticLists.FieldByName('NAME').AsString;
      edtWeight.ValueFloat := fbqStaticLists.FieldByName('WEIGHT').AsFloat;
    end;
    fbqStaticLists.Close;
    LoadDetailList(FRecordID);
  end;
end;

procedure TfrmScannerStaticList.Denitialize;
begin
  fbqStaticLists.Close;
end;

procedure TfrmScannerStaticList.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PInstrumentData;
  ColumnsItem1, ColumnsItem2: TExtraColumns.TColumnsItem;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    COL_INSTRUMENT:
      Result := CompareText(Data1^.Name, Data2^.Name);
    COL_RANK:
      if (Data1^.ExtraColumns.Items.ContainsKey(-1)) and
         (Data2^.ExtraColumns.Items.ContainsKey(-1)) then
      begin
        ColumnsItem1 := Data1^.ExtraColumns.Items[-1];
        ColumnsItem2 := Data2^.ExtraColumns.Items[-1];
        Result := CompareValue(ColumnsItem1.Rank, ColumnsItem2.Rank);
      end;
  end;
end;

procedure TfrmScannerStaticList.vstTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TfrmScannerStaticList.vstTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  pTarget: PVirtualNode;
  pSource: PVirtualNode;
  attMode: TVTNodeAttachMode;
begin
  if (Sender = Source) then
  begin
    pTarget := Sender.DropTargetNode;
    if Assigned(pTarget) then
    begin
      pSource := TVirtualStringTree(Source).FocusedNode;
      attMode := amAddChildFirst;
      if Assigned(pTarget.NextSibling) then
      begin
        if (pTarget.NextSibling = pSource) then
          attMode := amInsertBefore
        else
          attMode := amInsertAfter;
      end;
      Sender.MoveTo(pSource, pTarget, attMode, False);
    end;
  end
  else
  begin
    pTarget := Sender.DropTargetNode;
    AddInstruments(pTarget, TVirtualStringTree(Source));
  end;
end;

procedure TfrmScannerStaticList.vstTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := (Source is TVirtualStringTree);
end;

procedure TfrmScannerStaticList.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PInstrumentData;
begin
  Data := Node^.GetData;
  if (Data^.TWSMessageItem.ErrorCode > 0) then
    TargetCanvas.Font.Color := clRed;
end;

procedure TfrmScannerStaticList.vstTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TfrmScannerStaticList.vstTreeCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := TFloatEditLink.Create;
  TFloatEditLink(EditLink).Column := COL_RANK;
  TFloatEditLink(EditLink).Decimals := C_DECIMALS;
end;

procedure TfrmScannerStaticList.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PInstrumentData;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmScannerStaticList.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PInstrumentData;
  ColumnsItem: TExtraColumns.TColumnsItem;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    COL_INSTRUMENT:
      if Data^.SecurityType in [stOption, stFuture, stFutOpt] then
        CellText := Data^.LocalSymbol
      else
        CellText := Data^.Name;
    COL_RANK:
      if (Data^.ExtraColumns.Items.ContainsKey(-1)) then
      begin
        ColumnsItem := Data^.ExtraColumns.Items[-1];
        CellText := SimpleRoundTo(ColumnsItem.Rank, -C_DECIMALS).ToString;
      end
      else
        CellText := '';
  end;
end;

procedure TfrmScannerStaticList.vstTreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  Data: PInstrumentData;
  ColumnsItem: TExtraColumns.TColumnsItem;
begin
  if (Column = COL_RANK) then
  begin
    Data := Node^.GetData;
    if (Data^.ExtraColumns.Items.ContainsKey(-1)) then
    begin
      ColumnsItem := Data^.ExtraColumns.Items[-1];
      ColumnsItem.Rank := StrToFloatDef(NewText, 0);
      Data^.ExtraColumns.Items.AddOrSetValue(-1, ColumnsItem);
    end;
  end;
end;

procedure TfrmScannerStaticList.LoadDetailList(const aRecordID: Integer);
resourcestring
  C_SQL_SELECT_STATICLISTS_DETAIL = 'SELECT SD.*, SL.WEIGHT, SL.NAME AS SL_NAME ' +
                                    '  FROM STATICLISTS SL, STATICLISTS_DETAIL SD     ' +
                                    ' WHERE SL.ID = SD.STATICLISTS_ID                 ' +
                                    '   AND STATICLISTS_ID=:STATICLISTS_ID';
var
 ColumnsItem: TExtraColumns.TColumnsItem;
 Data: PInstrumentData;
 Node: PVirtualNode;
 Query : TFDQuery;
 Transaction : TFDTransaction;
 SokidInfo   : TSokidInfo;
begin
  vstTree.BeginUpdate;
  Transaction := TFDTransaction.Create(DMod.ConnectionStock);
  try
    Transaction.Connection := DMod.ConnectionStock;
    Transaction.Options.AutoCommit := false;
    Query := TFDQuery.Create(nil);
    try
      Query.Connection  := DMod.ConnectionStock;
      Query.Transaction := Transaction;
      if not Transaction.Active then
         Transaction.StartTransaction;
      DMod.CheckConnect;
      Query.SQL.Text := C_SQL_SELECT_STATICLISTS_DETAIL;
      Query.Connection := DMod.ConnectionStock;
      Query.ParamByName('STATICLISTS_ID').AsInteger := aRecordID;
      Query.Prepare;
      Query.Open;
      Query.FetchAll;
      while not Query.Eof do
      begin
        if SokidList.ContainsKey(Query.FieldByName('CONID').AsInteger) then
        begin
          Node := vstTree.AddChild(vstTree.RootNode);
          Data := vstTree.GetNodeData(Node);
          SokidInfo := SokidList.Items[Query.FieldByName('CONID').AsInteger];
          Data^.Id             := SokidInfo.ContractId;
          Data^.RecordId       := Query.FieldByName('ID').AsInteger;
          Data^.BrokerType     := SokidInfo.Broker;
          Data^.Currency       := SokidInfo.Currency;
          Data^.Exchange       := SokidInfo.Exchange;
          Data^.Name           := SokidInfo.Name;
          Data^.Group          := SokidInfo.Group;
          Data^.SecurityType   := SokidInfo.GetSecurityType;
          Data^.IsIn           := SokidInfo.IsIn;
          Data^.Symbol         := SokidInfo.Symbol;
          Data^.LocalSymbol    := SokidInfo.LocalSymbol;
          Data^.Sector         := SokidInfo.Sector;
          Data^.TWSMessageItem := SokidInfo.TWSMessageItem;
          Data^.ExtraColumns   := TExtraColumns.Create(0);
          ColumnsItem.Rank     := Query.FieldByName('RANKING').AsFloat;
          Data^.ExtraColumns.Items.AddOrSetValue(-1, ColumnsItem);
        end;
        Query.Next;
      end;
    finally
      FreeAndNil(Query);
    end;
  finally
    FreeAndNil(Transaction);
    vstTree.EndUpdate;
  end;
end;

procedure TfrmScannerStaticList.SaveDetailList(const aRecordID: Integer);
resourcestring
  C_SQL_INSERT_TEXT = 'INSERT INTO STATICLISTS_DETAIL(ID, STATICLISTS_ID, CONID, BROKER_TYPE, RANKING) ' + sLineBreak +
                      'VALUES(:ID, :STATICLISTS_ID, :CONID, :BROKER_TYPE, :RANKING)';
  C_SQL_UPDATE_TEXT = 'UPDATE STATICLISTS_DETAIL '                                                                            + sLineBreak +
                      'SET STATICLISTS_ID = :STATICLISTS_ID, CONID = :CONID, BROKER_TYPE = :BROKER_TYPE, RANKING = :RANKING ' + sLineBreak +
                      'WHERE (ID = :ID); ';
var
  Data: PInstrumentData;
  Node: PVirtualNode;
  Query : TFDQuery;
  ColumnsItem: TExtraColumns.TColumnsItem;
  Order: TIABOrder;
begin
  vstTree.BeginUpdate;
  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection  := DMod.ConnectionStock;
      Query.Transaction := Query.Connection.Transaction;
      DMod.CheckConnect;
      Node := vstTree.GetFirst;
      while Assigned(Node) do
      begin
        Data := Node^.GetData;
        if not SokidList.ContainsKey(Data^.Id) then
        begin
          Data^.SaveInstrumentToSokidList;
          Order := SokidList.GetOrderByConID(Data^.Id);
          try
            IABClient.SendRequest(ibGetInstrumentSpecs, 1, Order);
          finally
            FreeAndNil(Order);
          end;
        end;

        if (Data.RecordId < 0) then
        begin
          Data.RecordId := DMod.GetNextValue('GEN_STATICLISTS_DETAIL_ID');
          Query.SQL.Text := C_SQL_INSERT_TEXT;
        end
        else
          Query.SQL.Text := C_SQL_UPDATE_TEXT;

        Query.ParamByName('ID').AsInteger             := Data^.RecordId;
        Query.ParamByName('STATICLISTS_ID').AsInteger := aRecordID;
        Query.ParamByName('CONID').AsInteger          := Data^.Id;
        Query.ParamByName('BROKER_TYPE').AsInteger    := Ord(Data^.BrokerType);
        if (Data^.ExtraColumns.Items.ContainsKey(-1)) then
        begin
          ColumnsItem := Data^.ExtraColumns.Items[-1];
          Query.ParamByName('RANKING').AsFloat := ColumnsItem.Rank;
        end;

        try
          Query.Prepare;
          Query.ExecSQL;
        except
          on E: Exception do
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveDetailList', E.Message + TDModUtils.GetQueryInfo(Query));
        end;
        Node := vstTree.GetNext(Node);
      end;
    finally
      Query.Transaction.CommitRetaining;
      FreeAndNil(Query);
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmScannerStaticList.aSaveExecute(Sender: TObject);
begin
  if (FRecordID > -1) then
  begin
    DMod.ExecuteSQL('UPDATE STATICLISTS SET NAME =' + QuotedStr(string(edtName.Text).Substring(0, 100)) +
                    ', WEIGHT = ' + QuotedStr(SimpleRoundTo(edtWeight.ValueFloat, -C_DECIMALS).ToString) +
                    ' WHERE ID=' + FRecordID.ToString);
  end
  else
  begin
    FRecordID := DMod.GetNextValue('GEN_STATIC_LISTS_ID');
    DMod.ExecuteSQL('INSERT INTO STATICLISTS (ID, NAME, WEIGHT, VISIBLE) VALUES (' +
                    FRecordID.ToString + ', ' + QuotedStr(edtName.Text) + ', ' + QuotedStr(SimpleRoundTo(edtWeight.ValueFloat, -C_DECIMALS).ToString) + ', 1' +
                    ')');
  end;
  SaveDetailList(FRecordID);
  ModalResult := mrOk;
end;

procedure TfrmScannerStaticList.aDeleteRowExecute(Sender: TObject);
resourcestring
  C_SQL_INSERT_TEXT = 'DELETE FROM STATICLISTS_DETAIL WHERE ID=';
var
  Data: PInstrumentData;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    if ((Sender = miDeleteRow) and (TMessageDialog.ShowQuestion('Current row be deleted. Continue?') = mrYes)) or
        (Sender = aDeleteRow) then
    begin
      Data := vstTree.FocusedNode^.GetData;
      if (Data.RecordId > -1) then
        DMod.ExecuteSQL(C_SQL_INSERT_TEXT + Data.RecordId.ToString);
      vstTree.DeleteNode(vstTree.FocusedNode);
    end;
  end;
end;

procedure TfrmScannerStaticList.aDeleteRowUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstTree.IsEmpty;
end;

procedure TfrmScannerStaticList.AddInstruments(aTarget: PVirtualNode; const aSource: TVirtualStringTree);
var
  Data: PInstrumentData;
  i{, j}: Integer;
  Node: PVirtualNode;
  pSource: PVirtualNode;
  SokidInfo: PSokidInfo;
begin
  vstTree.BeginUpdate;
  try
    pSource := aSource.GetFirstSelected;
    if not Assigned(aTarget) then
      aTarget := vstTree.RootNode;

    // save instruments
    for i := 0 to aSource.SelectedCount - 1 do
    begin
      SokidInfo := aSource.GetNodeData(pSource);
      Node := vstTree.AddChild(vstTree.RootNode);
      Data := vstTree.GetNodeData(Node);
      Data^.BrokerType     := SokidInfo^.Broker;
      Data^.Id             := SokidInfo^.ContractId;
      Data^.Currency       := SokidInfo^.Currency;
      Data^.Exchange       := SokidInfo^.Exchange;
      Data^.SecurityType   := SokidInfo^.GetSecurityType;
      Data^.Symbol         := SokidInfo^.Symbol;
      Data^.LocalSymbol    := SokidInfo^.LocalSymbol;
      Data^.IsIn           := SokidInfo^.IsIn;
      Data^.TWSMessageItem := SokidInfo^.TWSMessageItem;
      Data^.RecordId       := -1;
      Data^.ExtraColumns   := TExtraColumns.Create(0);
      if (SokidInfo^.GetSecurityType in [stOption, stFuture, stFutOpt]) then
        Data^.Name := SokidInfo^.LocalSymbol
      else
        Data^.Name := SokidInfo^.Name;
      vstTree.MoveTo(Node, aTarget, amInsertAfter, False);
      pSource := aSource.GetNextSelected(pSource, False);
    end;
    vstTree.Expanded[aTarget] := True;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmScannerStaticList.aDeleteListExecute(Sender: TObject);
begin
  if (TMessageDialog.ShowQuestion('Current list be deleted. Continue?') = mrYes) then
  begin
    DMod.ExecuteSQL('DELETE FROM STATICLISTS WHERE ID=' + fbqStaticLists.FieldByName('ID').AsString);
    fbqStaticLists.Close;
    fbqStaticLists.Open;
  end;
end;

procedure TfrmScannerStaticList.aShowSearchFormExecute(Sender: TObject);
begin
  TfrmSearchInstruments.ShowDocument(Self);
end;

procedure TfrmScannerStaticList.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrClose;
end;

end.
