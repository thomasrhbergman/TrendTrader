unit Candidate.StaticLists;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids, DaModule, Common.Types,
  System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, VirtualTrees, System.ImageList, Vcl.Menus,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} DaImages, Vcl.ImgList, System.UITypes, CustomForms, Candidate.StaticList,
  Candidate.Types, MessageDialog, Entity.Sokid, Search.Instruments, Monitor.Types, HtmlLib, DebugWriter, Global.Types,
  DaModule.Utils, Publishers, MonitorTree.Helper, MonitorTree.Document,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client;
{$ENDREGION}

type
  TfrmCandidateStaticLists = class(TCustomForm)
    aAddAll: TAction;
    aAddList: TAction;
    aCancel: TAction;
    ActionListMain: TActionList;
    aDeleteList: TAction;
    aEditList: TAction;
    aIntersection: TAction;
    aOk: TAction;
    aReduce: TAction;
    aShowSearchInstruments: TAction;
    aUpdate: TAction;
    bAddAll: TBitBtn;
    bIntersection: TBitBtn;
    bReduce: TBitBtn;
    btnAdd: TBitBtn;
    btnCancel: TBitBtn;
    btnDeleteSelectedNode: TBitBtn;
    btnEditQualifier: TBitBtn;
    btnOk: TBitBtn;
    btnShowSearchForm: TBitBtn;
    bUpdate: TBitBtn;
    dsStaticLists: TDataSource;
    grdStaticLists: TDBGrid;
    miAddAll: TMenuItem;
    miDeleteList: TMenuItem;
    miEditList: TMenuItem;
    miIntersection: TMenuItem;
    miNewList: TMenuItem;
    miReduce: TMenuItem;
    miSep01: TMenuItem;
    miUpdate: TMenuItem;
    pmStaticLists: TPopupMenu;
    pnlBottom: TPanel;
    pnlOptions: TPanel;
    fbqStaticLists: TFDQuery;
    fbqStaticListsID: TIntegerField;
    fbqStaticListsWEIGHT: TSingleField;
    fbqStaticListsNAME: TStringField;
    procedure aAddAllExecute(Sender: TObject);
    procedure aAddAllUpdate(Sender: TObject);
    procedure aAddListExecute(Sender: TObject);
    procedure aAddListUpdate(Sender: TObject);
    procedure aCancelExecute(Sender: TObject);
    procedure aDeleteListExecute(Sender: TObject);
    procedure aEditListExecute(Sender: TObject);
    procedure aEditListUpdate(Sender: TObject);
    procedure aIntersectionExecute(Sender: TObject);
    procedure aOkExecute(Sender: TObject);
    procedure aReduceExecute(Sender: TObject);
    procedure aShowSearchInstrumentsExecute(Sender: TObject);
    procedure aUpdateExecute(Sender: TObject);
    procedure grdStaticListsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure grdStaticListsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  private
    FKindAppend: TKindAppend;
    [weak] FCandidateMarket: ICandidateMarket;
    procedure AddInstruments(aSourceTree: TVirtualStringTree);
    procedure SaveDetailList(const aParentRecordID: Integer; aNodes: TNodeArray);
  public
    class function ShowDocument(aDialogMode: TDialogMode; aKindAppend: TKindAppend; aId: Integer; aCandidateMarket: ICandidateMarket): TModalResult;
    class procedure AddInstrumentToCandidateMain(const ATargetForm: ICandidateMarket; aKindAppend: TKindAppend; aStaticListsId: Integer; aDialogMode: TDialogMode);
    procedure Initialize;
    procedure Denitialize;

    property KindAppend: TKindAppend read FKindAppend write FKindAppend;
  end;

implementation

{$R *.dfm}

class function TfrmCandidateStaticLists.ShowDocument(aDialogMode: TDialogMode; aKindAppend: TKindAppend; aId: Integer; aCandidateMarket: ICandidateMarket): TModalResult;
var
  frmCandidateStaticLists: TfrmCandidateStaticLists;
begin
  frmCandidateStaticLists := TfrmCandidateStaticLists.Create(nil);
  try
    frmCandidateStaticLists.DialogMode := aDialogMode;
    frmCandidateStaticLists.KindAppend := aKindAppend;
    frmCandidateStaticLists.Initialize;
    frmCandidateStaticLists.fbqStaticLists.Locate('ID', aId, []);
    frmCandidateStaticLists.FCandidateMarket := aCandidateMarket;
    Result := frmCandidateStaticLists.ShowModal;
  finally
    frmCandidateStaticLists.Denitialize;
    FreeAndNil(frmCandidateStaticLists);
  end;
end;

procedure TfrmCandidateStaticLists.Initialize;
begin
  DMod.CheckConnect;
  fbqStaticLists.Open;
  if (DialogMode = dmView) then
  begin
    aAddAll.Visible       := False;
    aUpdate.Visible       := False;
    aReduce.Visible       := False;
    aIntersection.Visible := False;
    case KindAppend of
      kaAddAll:
        aAddAll.Visible := True;
      kaUpdate:
        aUpdate.Visible := True;
      kaReduce:
        aReduce.Visible := True;
      kaIntersection:
        aIntersection.Visible := True;
    end;
  end;
end;

procedure TfrmCandidateStaticLists.Denitialize;
begin
  fbqStaticLists.Close;
end;

procedure TfrmCandidateStaticLists.grdStaticListsDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if (Source is TVirtualStringTree) then
    AddInstruments(TVirtualStringTree(Source));
end;

procedure TfrmCandidateStaticLists.AddInstruments(aSourceTree: TVirtualStringTree);
var
  Data: PSokidInfo;
  arrNodes: TNodeArray;
  Node: PVirtualNode;
  RecordId: Integer;
begin
  if Assigned(aSourceTree.FocusedNode) then
  begin
    Node := aSourceTree.FocusedNode;
    Data := Node^.GetData;
    if Data^.Symbol.IsEmpty then
      arrNodes := TTreeDocument.GetNodesList(Node)
    else if Assigned(Node^.Parent) then
    begin
      Data := Node^.Parent.GetData;
      arrNodes := TTreeDocument.GetNodesList(Node^.Parent);
    end;

    if Length(arrNodes) > 0 then
    begin
      RecordId := -1;
      if fbqStaticLists.Locate('NAME', Data^.Name, []) then
        RecordId := fbqStaticLists.FieldByName('ID').AsInteger;
      if (RecordId = -1) then
      begin
        RecordID := DMod.GetNextValue('GEN_STATIC_LISTS_ID');
        DMod.ExecuteSQL('INSERT INTO STATICLISTS (ID, NAME, WEIGHT, VISIBLE) VALUES (' +
                        RecordID.ToString + ', ' + Data^.Name.QuotedString + ',1,1)');
        fbqStaticLists.Close;
        fbqStaticLists.Open;
        fbqStaticLists.Locate('ID', RecordId, []);
      end;
      SaveDetailList(RecordId, arrNodes);
    end;
  end;
end;

procedure TfrmCandidateStaticLists.SaveDetailList(const aParentRecordID: Integer; aNodes: TNodeArray);
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT ID FROM STATICLISTS_DETAIL WHERE CONID=:CONID AND STATICLISTS_ID =:STATICLISTS_ID';
  C_SQL_INSERT_TEXT = 'INSERT INTO STATICLISTS_DETAIL(ID, STATICLISTS_ID, CONID, BROKER_TYPE, RANKING) ' + sLineBreak +
                      'VALUES(:ID, :STATICLISTS_ID, :CONID, :BROKER_TYPE, :RANKING)';
  C_SQL_UPDATE_TEXT = 'UPDATE STATICLISTS_DETAIL '                                                                            + sLineBreak +
                      'SET STATICLISTS_ID = :STATICLISTS_ID, CONID = :CONID, BROKER_TYPE = :BROKER_TYPE, RANKING = :RANKING ' + sLineBreak +
                      'WHERE (ID = :ID); ';
var
  Data: PSokidInfo;
  Node: PVirtualNode;
  Query : TFDQuery;
  IsExists: Boolean;
  RecordId: Integer;
  arrSokidInfo: TArray<PSokidInfo>;
  Index: Integer;
begin
    Query := TFDQuery.Create(nil);
    try
      Query.Connection    := DMod.ConnectionStock;
      Query.Transaction := DMod.ConnectionStock.Transaction;
      DMod.CheckConnect;

      arrSokidInfo := [];
      for Node in aNodes do
        if Assigned(Node) then
        begin
          Data := Node^.GetData;
          if not Data.Symbol.IsEmpty then
          begin
            SetLength(arrSokidInfo, Length(arrSokidInfo) + 1);
            arrSokidInfo[High(arrSokidInfo)] := Data;
          end;
        end;

      Index := Length(arrSokidInfo);
      for Data in arrSokidInfo do
      begin
        Query.SQL.Text := C_SQL_EXISTS_TEXT;
        try
          Query.ParamByName('CONID').AsInteger          := Data^.ContractId;
          Query.ParamByName('STATICLISTS_ID').AsInteger := aParentRecordID;
          Query.Prepare;
          Query.Open;
          RecordId := Query.FieldByName('ID').AsInteger;
          IsExists := RecordId > 0;
          Query.Close;
        except
          on E: Exception do
          begin
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveDetailList', E.Message + TDModUtils.GetQueryInfo(Query));
            raise;
          end;
        end;

        if not IsExists then
        begin
          RecordId := DMod.GetNextValue('GEN_STATICLISTS_DETAIL_ID');
          Query.SQL.Text := C_SQL_INSERT_TEXT;
        end
        else
          Query.SQL.Text := C_SQL_UPDATE_TEXT;

        Query.ParamByName('ID').AsInteger             := RecordId;
        Query.ParamByName('STATICLISTS_ID').AsInteger := aParentRecordID;
        Query.ParamByName('CONID').AsInteger          := Data^.ContractId;
        Query.ParamByName('BROKER_TYPE').AsInteger    := Ord(Data^.Broker);
        Query.ParamByName('RANKING').AsFloat          := Index;
        try
          Query.Prepare;
          Query.ExecSQL;
        except
          on E: Exception do
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveDetailList', E.Message + TDModUtils.GetQueryInfo(Query));
        end;
        Dec(Index);
      end;
    finally
      Query.Transaction.CommitRetaining;
      FreeAndNil(Query);
    end;
end;

procedure TfrmCandidateStaticLists.grdStaticListsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  if (Source is TVirtualStringTree) then
    Accept := TVirtualStringTree(Source).Name = 'vstSokidLists';
end;

procedure TfrmCandidateStaticLists.aAddListExecute(Sender: TObject);
begin
  if (TfrmCandidateStaticList.ShowDocument(-1) = mrOk) then
    DMod.RefreshQuery(fbqStaticLists);
end;

procedure TfrmCandidateStaticLists.aEditListExecute(Sender: TObject);
var
  RecordId: Integer;
begin
  if not fbqStaticLists.IsEmpty then
  begin
    RecordId := fbqStaticLists.FieldByName('ID').AsInteger;
    if (TfrmCandidateStaticList.ShowDocument(RecordId) = mrOk) then
      DMod.RefreshQuery(fbqStaticLists);
  end;
end;

procedure TfrmCandidateStaticLists.aReduceExecute(Sender: TObject);
begin
  if Assigned(FCandidateMarket) then
    AddInstrumentToCandidateMain(FCandidateMarket, kaReduce, fbqStaticLists.FieldByName('ID').AsInteger, DialogMode);
end;

procedure TfrmCandidateStaticLists.aIntersectionExecute(Sender: TObject);
begin
  if Assigned(FCandidateMarket) then
    AddInstrumentToCandidateMain(FCandidateMarket, kaIntersection, fbqStaticLists.FieldByName('ID').AsInteger, DialogMode);
end;

procedure TfrmCandidateStaticLists.aAddAllExecute(Sender: TObject);
begin
  if Assigned(FCandidateMarket) then
    AddInstrumentToCandidateMain(FCandidateMarket, kaAddAll, fbqStaticLists.FieldByName('ID').AsInteger, DialogMode);
end;

procedure TfrmCandidateStaticLists.aUpdateExecute(Sender: TObject);
begin
  if Assigned(FCandidateMarket) then
    AddInstrumentToCandidateMain(FCandidateMarket, kaUpdate, fbqStaticLists.FieldByName('ID').AsInteger, DialogMode);
end;

procedure TfrmCandidateStaticLists.aShowSearchInstrumentsExecute(Sender: TObject);
begin
  TfrmSearchInstruments.ShowDocument(Self);
end;

class procedure TfrmCandidateStaticLists.AddInstrumentToCandidateMain(const ATargetForm: ICandidateMarket; aKindAppend: TKindAppend; aStaticListsId: Integer; aDialogMode: TDialogMode);
resourcestring
  C_SQL_SELECT_STATICLISTS_DETAIL = 'SELECT SD.*, SL.WEIGHT, SL.NAME AS SL_NAME   ' +
                                    '  FROM STATICLISTS SL, STATICLISTS_DETAIL SD ' +
                                    ' WHERE SL.ID = SD.STATICLISTS_ID             ' +
                                    '   AND STATICLISTS_ID=:STATICLISTS_ID        ' +
                                    ' ORDER BY RANKING DESC';
VAR
  ColumnsInfo   : TColumnsInfo;
  ColumnsItem   : TExtraColumns.TColumnsItem;
  Index         : Integer;
  Instruments   : TArrayInstrumentData;
  Query         : TFDQuery;
  CandidateMarket : ICandidateMarket;
  SokidInfo     : TSokidInfo;
begin
  if (aStaticListsId > 0) and (aDialogMode <> dmView) then
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := C_SQL_SELECT_STATICLISTS_DETAIL;
      Query.ParamByName('STATICLISTS_ID').AsInteger := aStaticListsId;
      Query.Prepare;
      Query.Open;
      Query.FetchAll;
      Query.First;
      Index := 0;
      SetLength(Instruments, Query.RecordCount);
      while not Query.Eof do
      begin
        if SokidList.ContainsKey(Query.FieldByName('CONID').AsInteger) then
          with Instruments[Index] do
          begin
            SokidInfo := SokidList.Items[Query.FieldByName('CONID').AsInteger];
            Id             := SokidInfo.ContractId;
            RecordId       := Query.FieldByName('ID').AsInteger;
            BrokerType     := SokidInfo.Broker;
            Currency       := SokidInfo.Currency;
            Exchange       := SokidInfo.Exchange;
            Name           := SokidInfo.Name;
            Group          := SokidInfo.Group;
            IsIn           := SokidInfo.IsIn;
            Symbol         := SokidInfo.Symbol;
            LocalSymbol    := SokidInfo.LocalSymbol;
            Sector         := SokidInfo.Sector;
            SecurityType   := SokidInfo.GetSecurityType;
            Multiplier     := StrToIntDef(SokidInfo.Multiplier, 0);
            TWSMessageItem := SokidInfo.TWSMessageItem;

            ExtraColumns := TExtraColumns.Create(0);
            ColumnsItem.ColTick := clBlack;
            ColumnsItem.Price   := 0;
            ColumnsItem.Rank    := Query.FieldByName('RANKING').AsFloat;
            ColumnsItem.Weight  := 0;
            ExtraColumns.Items.AddOrSetValue(-1, ColumnsItem);
          end;
        Inc(Index);
        Query.Next;
      end;
      Query.First;
      ColumnsInfo := TColumnsInfo.Create(stStaticList);
      ColumnsInfo.StaticColumn.RecordId := aStaticListsId;
      ColumnsInfo.StaticColumn.Name     := Query.FieldByName('SL_NAME').AsString;
      ColumnsInfo.Weight                := Query.FieldByName('WEIGHT').AsFloat;
      ColumnsInfo.KindAppend            := aKindAppend;

      if Assigned(ATargetForm) and Supports(ATargetForm, ICandidateMarket, CandidateMarket) then
        CandidateMarket.AddInstrument(@Instruments, ColumnsInfo)
      else
        raise Exception.Create('TargetForm not supports ICandidateMarket');
    finally
      FreeAndNil(Query);
    end;
  end;
end;

procedure TfrmCandidateStaticLists.aAddAllUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := fbqStaticLists.Active and (fbqStaticLists.RecordCount > 0);
end;

procedure TfrmCandidateStaticLists.aEditListUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := fbqStaticLists.Active and (fbqStaticLists.RecordCount > 0) and (DialogMode <> dmView);
end;

procedure TfrmCandidateStaticLists.aAddListUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (DialogMode <> dmView);
end;

procedure TfrmCandidateStaticLists.aDeleteListExecute(Sender: TObject);
resourcestring
  C_SQL_DELETE = 'DELETE FROM STATICLISTS WHERE ID=';
begin
  if (TMessageDialog.ShowQuestion('Current list be deleted. Continue?') = mrYes) then
  begin
    DMod.ExecuteSQL(C_SQL_DELETE + fbqStaticLists.FieldByName('ID').AsString);
    fbqStaticLists.Close;
    fbqStaticLists.Open;
  end;
end;

procedure TfrmCandidateStaticLists.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmCandidateStaticLists.aOkExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
