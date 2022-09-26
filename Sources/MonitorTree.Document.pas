unit MonitorTree.Document;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, IABSocketAPI_const, IABFunctions, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, HtmlLib, Document, Qualifiers.Types, Scanner.Types, VirtualTrees, BrokerHelperAbstr, DaModule, Data.DB,
  HtmlConsts, AutoTrades.Types, Common.Types, System.StrUtils, Utils, Global.Types,
  InstrumentList, Entity.Sokid, IABFunctions.MarketRules, System.Generics.Defaults, System.Generics.Collections,
  System.DateUtils, DaModule.Utils, ArrayHelper, Publishers, DaImages, IABFunctions.Helpers, Monitor.Types,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TBeforeEachDocumentProc = reference to function(const aDocType: TDocType; const aRelationId, aRecordId, aParentId: Integer): Boolean;
  TAfterLoadEachDocumentProc = reference to procedure(const aNode: PVirtualNode);
  TAfterLoadTreeProc = reference to procedure(const aParentNode: PVirtualNode);

  TTreeDocument = class
  public
    class function CreateAlgos(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree): PVirtualNode; inline;
    class function CreateAutoTrade(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree; const aAutoTrade: TAutoTradeInfo = nil): PVirtualNode; inline;
    class function CreateCondition(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree): PVirtualNode; inline;
    class function CreateFactor(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree): PVirtualNode; inline;
    class function CreateOrder(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree; const aBrokerType: TBrokerType): PVirtualNode; inline;
    class function CreateOrderGroup(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree): PVirtualNode; inline;
    class function CreateOrderGroupSet(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree): PVirtualNode; inline;
    class function CreateQualifier(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree; const aQualifier: TQualifier = nil): PVirtualNode; inline;
    //class function GetAutoTradeItems(aNode: PVirtualNode): TAutoTradesArray; inline;
    class function GetDocStatus(const aData: PTreeData): string; inline;
    class function GetDocType(const aTree: TBaseVirtualTree; const aNode: PVirtualNode): TDocType; inline;
    class function GetNodesList(aNode: PVirtualNode; aIsRecursive: Boolean = True): TNodeArray; inline;
    class function GetNodesListByType(aTree: TBaseVirtualTree; aNode: PVirtualNode; aDocType: TDocType): TNodeArray; inline;
    class function GetOrderBroker(const aRecordId: Integer): TBrokerType;
    class function GetOrderSubordination(const aTree: TBaseVirtualTree; const aNode: PVirtualNode): TSubordination; inline;
    class function GetParentNode(const aTree: TBaseVirtualTree; const aNode: PVirtualNode; const aDocType: TDocType): PVirtualNode; inline;
    class function GetRelationItems(const aRecordId: Integer; const aParentId: Integer = 0): TIntegerArray;
    class function LoadRelationTree(const aRecordId: Integer; const aParentId: Integer; aTree: TBaseVirtualTree; aParentNode: PVirtualNode; aBeforeProc: TBeforeEachDocumentProc = nil; aAfterProc: TAfterLoadEachDocumentProc = nil): PVirtualNode;
    class function CalcCompiledValue(const aTree: TBaseVirtualTree; const aConditionDoc: TConditionDoc): TCompiledValue;
    class procedure CopySubtree(aTree: TBaseVirtualTree; aNode: PVirtualNode; aAfterLoadProc: TAfterLoadEachDocumentProc);
    class procedure DeleteRelations(const aRecordId: Integer);
    class procedure SaveRelationTree(aTree: TBaseVirtualTree; aNode: PVirtualNode; aIsNewGroup: Boolean = False);
    class procedure SetIcon(aNode: PVirtualNode; aTree: TBaseVirtualTree); inline;
    class procedure SetTickTypesForChildFactors(const aTree: TBaseVirtualTree; const aNode: PVirtualNode; const aConditionDoc: TConditionDoc = nil);

    class procedure DeleteOrderTemplateRelations(const aTemplateId: Integer);
    class function LoadOrderTemplateRelationTree(const aTemplateId: Integer; const aParentId: Integer; aTree: TBaseVirtualTree; aParentNode: PVirtualNode; aBeforeProc: TBeforeEachDocumentProc = nil; aAfterProc: TAfterLoadEachDocumentProc = nil): PVirtualNode;
    class procedure SaveOrderTemplateRelationTree(const aTemplateId: integer; aTree: TBaseVirtualTree; aNode: PVirtualNode; aIsNewGroup: Boolean = False);
  end;

const
  ICON_QUALIFIER           = 20;
  ICON_QUALIFIER_CONDITION = 1;
  ICON_AUTOTRADE           = 2;
  ICON_ORDERGROUP_SET      = 3;
  ICON_ORDERGROUP          = 4;
  ICON_ORDER_BUY           = 21;
  ICON_ORDER_SELL          = 22;
  ICON_ORDER_BUY_FILLED    = 21;
  ICON_ORDER_SELL_FILLED   = 22;
  ICON_COND_TRUE           = 23;
  ICON_COND_FALSE          = 23;
  ICON_COND_DISABLED       = 23;
  ICON_ALGOS               = 12;
  ICON_FACTOR              = 13;

implementation

{ TTreeDocument }

class function TTreeDocument.CreateAutoTrade(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree; const aAutoTrade: TAutoTradeInfo = nil): PVirtualNode;
var
  Data: PTreeData;
  ParentNode: PVirtualNode;
begin
  ParentNode := aParentNode;
  if not Assigned(ParentNode) then
    ParentNode := aTree.RootNode;
  Result := aTree.AddChild(ParentNode);
  Data := Result^.GetData;
//  Data^.Clear;
  Data^.DocType        := ntAutoTrade;
  Data^.Enabled        := True;
  Data^.ReValue        := 1.0;
  Data^.CreationType := ctUser;
  Data^.RelationId     := -1;

  if not Assigned(aAutoTrade) then
  begin
    Data^.AutoTrade                         := TAutoTradeInfo.Create;
    Data^.AutoTrade.Active                  := True;
    Data^.AutoTrade.AllowSendDuplicateOrder := False;
    Data^.AutoTrade.AutoRefresh             := True;
    Data^.AutoTrade.Enabled                 := True;
    Data^.AutoTrade.MaxNumberOrder          := 1;
    Data^.AutoTrade.MaxRows                 := 10;
    Data^.AutoTrade.Name                    := 'Auto Trade';
    Data^.AutoTrade.OrderAmount             := 1000;
    Data^.AutoTrade.OrderCurrency           := C_DEFAULT_CURRENCY;
  end
  else
    Data^.AutoTrade := aAutoTrade;

  Data^.AutoTrade.OwnerNode := Result;
  Data^.RecordId := -1;
  SetIcon(Result, aTree);
end;

class function TTreeDocument.CreateCondition(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree): PVirtualNode;
var
  Data: PTreeData;
  ParentNode: PVirtualNode;
begin
  ParentNode := aParentNode;
  if not Assigned(ParentNode) then
    ParentNode := aTree.RootNode;
  Result := aTree.AddChild(ParentNode);
  Data := Result^.GetData;
//  Data^.Clear;
  Data^.DocType      := ntCondition;
  Data^.Enabled      := True;
  Data^.ReValue      := 1.0;
  Data^.CreationType := ctUser;
  Data^.RelationId   := -1;

  Data^.ConditionDoc := TConditionDoc.Create;
  Data^.ConditionDoc.Description := 'Condition';
  Data^.ConditionDoc.IsCondition := False;
  Data^.ConditionDoc.OwnerNode   := Result;
  Data^.RecordId := -1;
  SetIcon(Result, aTree);
end;

class function TTreeDocument.CreateFactor(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree): PVirtualNode;
var
  Data: PTreeData;
  ParentNode: PVirtualNode;
begin
  ParentNode := aParentNode;
  if not Assigned(ParentNode) then
    ParentNode := aTree.RootNode;
  Result := aTree.AddChild(ParentNode);
  Data := Result^.GetData;
//  Data^.Clear;
  Data^.DocType      := ntFactor;
  Data^.Enabled      := True;
  Data^.ReValue      := 1.0;
  Data^.CreationType := ctUser;
  Data^.RelationId   := -1;

  Data^.FactorDoc := TFactorDoc.Create;
  Data^.FactorDoc.UseInAutoOrder := True;
  Data^.FactorDoc.BrokerType     := TBrokerType.brIB;
  Data^.FactorDoc.ContractId     := 0;
  Data^.FactorDoc.IBId           := 0;
  Data^.FactorDoc.InstrumentName := '';
  Data^.FactorDoc.Currency       := '';
  Data^.FactorDoc.Exchange       := '';
  Data^.FactorDoc.ContractType   := '';
  Data^.FactorDoc.OwnerNode      := Result;
  Data^.RecordId := -1;
  SetIcon(Result, aTree);
end;

class function TTreeDocument.GetOrderBroker(const aRecordId: Integer): TBrokerType;
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT BROKER_ID FROM ORDERS WHERE ID =';
begin
  Result := TBrokerType.FromInteger(DMod.GetIntegerValueFromSQL(C_SQL_SELECT_TEXT + aRecordId.ToString, 'BROKER_ID'));
end;

class function TTreeDocument.CreateOrder(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree; const aBrokerType: TBrokerType): PVirtualNode;
var
  Data: PTreeData;
  ParentNode: PVirtualNode;
begin
  ParentNode := aParentNode;
  if not Assigned(ParentNode) then
    ParentNode := aTree.RootNode;
  Result := aTree.AddChild(ParentNode);
  Data := Result^.GetData;
  Data^.DocType      := ntOrder;
  Data^.Enabled      := True;
  Data^.ReValue      := 1.0;
  Data^.CreationType := ctUser;
  Data^.RelationId   := -1;

  case aBrokerType of
    brIB:
      Data^.OrderDoc := TOrderIBDoc.Create;
    brNN:
      Data^.OrderDoc := TOrderNNDoc.Create;
    brTest:
      Data^.OrderDoc := TOrderTestDoc.Create;
  end;

  if Assigned(Data^.OrderDoc) then
  begin
    Data^.OrderDoc.OwnerNode       := Result;
    Data^.OrderDoc.OrderAction     := iabBuy;
    Data^.OrderDoc.Description     := 'Order';
    Data^.OrderDoc.BrokerType      := aBrokerType;
    Data^.OrderDoc.Id              := 0;
    Data^.OrderDoc.InstrumentName  := '';
    Data^.OrderDoc.Symbol          := '';
    Data^.OrderDoc.Exchange        := '';
    Data^.OrderDoc.PrimaryExchange := '';
    Data^.OrderDoc.Currency        := '';
    Data^.OrderDoc.Decimals        := 2;
    Data^.OrderDoc.MarketList      := '';
  end;
  Data^.RecordId := -1;
  SetIcon(Result, aTree);
end;

class function TTreeDocument.CreateAlgos(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree): PVirtualNode;
var
  Data: PTreeData;
  ParentNode: PVirtualNode;
begin
  ParentNode := aParentNode;
  if not Assigned(ParentNode) then
    ParentNode := aTree.RootNode;
  Result := aTree.AddChild(ParentNode);
  Data := Result^.GetData;
//  Data^.Clear;
  Data^.DocType      := ntAlgos;
  Data^.Enabled      := True;
  Data^.ReValue      := 1.0;
  Data^.CreationType := ctUser;
  Data^.RelationId   := -1;

  Data^.AlgosDoc := TAlgosDoc.Create;
  Data^.AlgosDoc.Name := 'Algos';
  Data^.AlgosDoc.OwnerNode := Result;
  Data^.RecordId := -1;
  SetIcon(Result, aTree);
end;

class function TTreeDocument.CreateOrderGroup(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree): PVirtualNode;
var
  Data: PTreeData;
  ParentNode: PVirtualNode;
begin
  ParentNode := aParentNode;
  if not Assigned(ParentNode) then
    ParentNode := aTree.RootNode;
  Result := aTree.AddChild(ParentNode);
  Data := Result^.GetData;
//  Data^.Clear;
  Data^.DocType      := ntOrderGroup;
  Data^.Enabled      := True;
  Data^.ReValue      := 1.0;
  Data^.CreationType := ctUser;
  Data^.RelationId   := -1;

  Data^.OrderGroupDoc := TOrderGroupDoc.Create;
  Data^.OrderGroupDoc.OwnerNode := Result;
  Data^.OrderGroupDoc.Name      := 'Order Group';
  Data^.RecordId := -1;
  SetIcon(Result, aTree);
end;

class function TTreeDocument.CreateOrderGroupSet(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree): PVirtualNode;
var
  Data: PTreeData;
  ParentNode: PVirtualNode;
begin
  ParentNode := aParentNode;
  if not Assigned(ParentNode) then
    ParentNode := aTree.RootNode;
  Result := aTree.AddChild(ParentNode);
  Data := Result^.GetData;
//  Data^.Clear;
  Data^.DocType      := ntOrderGroupSet;
  Data^.Enabled      := True;
  Data^.ReValue      := 1.0;
  Data^.CreationType := ctUser;
  Data^.RelationId   := -1;

  Data^.OrderGroupSetDoc := TOrderGroupSetDoc.Create;
  Data^.OrderGroupSetDoc.Name      := 'Order Groups';
  Data^.OrderGroupSetDoc.TypeUse   := tuMonitor;
  Data^.OrderGroupSetDoc.OwnerNode := Result;
  Data^.RecordId := -1;
  SetIcon(Result, aTree);
end;

class function TTreeDocument.CreateQualifier(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree; const aQualifier: TQualifier = nil): PVirtualNode;
var
  Data: PTreeData;
  ParentNode: PVirtualNode;
begin
  ParentNode := aParentNode;
  if not Assigned(ParentNode) then
    ParentNode := aTree.RootNode;
  Result := aTree.AddChild(ParentNode);
  Data := Result^.GetData;
//  Data^.Clear;
  Data^.DocType      := ntQualifier;
  Data^.Enabled      := True;
  Data^.ReValue      := 1.0;
  Data^.CreationType := ctUser;
  Data^.RelationId   := -1;
  if not Assigned(aQualifier) then
  begin
    Data^.Qualifier           := TQualifier.Create;
    Data^.Qualifier.Name      := 'Qualifier';
    Data^.Qualifier.Enabled   := True;
  end
  else
    Data^.Qualifier := aQualifier;
  Data^.Qualifier.OwnerNode := Result;
  Data^.RecordId := -1;
  SetIcon(Result, aTree);
end;

(*class function TTreeDocument.CreateQualifierCondition(const aParentNode: PVirtualNode; const aTree: TBaseVirtualTree): PVirtualNode;
var
  Data: PTreeData;
  ParentNode: PVirtualNode;
begin
  ParentNode := aParentNode;
  if not Assigned(ParentNode) then
    ParentNode := aTree.RootNode;
  Result := aTree.AddChild(ParentNode);
  Data := Result^.GetData;
  Data^.Clear;
  Data^.DocType      := ntQualifierCondition;
  Data^.Enabled      := True;
  Data^.ReValue      := 1.0;
  Data^.CreationType := ctUser;
  Data^.RelationId   := -1;

  Data^.QualifierCondition := TQualifierCondition.Create;
  Data^.QualifierCondition.Name      := 'Qualifier Condition';
  Data^.QualifierCondition.Enabled   := True;
  Data^.QualifierCondition.OwnerNode := Result;
  Data^.RecordId := -1;
  SetIcon(Result, aTree);
end;*)

class procedure TTreeDocument.DeleteOrderTemplateRelations(
  const aTemplateId: Integer);
resourcestring
  C_SQL_SELECT_TEXT             = 'SELECT * FROM ORDER_TEMPLATE_RELATIONS WHERE ORDER_TEMPLATE_ID=:TemplateId';
  C_SQL_DEL_ORDERGROUP          = 'DELETE FROM ORDER_GROUP WHERE ID=:RecordId';
  C_SQL_DEL_ORDERS              = 'DELETE FROM ORDERS WHERE ID=:RecordId';
  C_SQL_DEL_CONDITION           = 'DELETE FROM CONDITION WHERE ID=:RecordId';
  C_SQL_DEL_RELATIONS           = 'DELETE FROM ORDER_TEMPLATE_RELATIONS WHERE ID=:RecordId';

  procedure DeleteRow(aSQLText: string; aRecordId: Integer);
  var
    Query: TFDQuery;
  begin
    if (aRecordId > 0) then
    begin
      Query := TFDQuery.Create(nil);
      try
        DMod.CheckConnect;
        Query.Connection := DMod.ConnectionStock;
        Query.Transaction := Query.Connection.Transaction;
        Query.SQL.Text := aSQLText;
        Query.ParamByName('RecordId').AsInteger := aRecordId;
        try
          Query.Prepare;
          Query.ExecSQL;
          Query.Transaction.CommitRetaining;
        except
          on E: Exception do
          begin
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TTreeDocument.DeleteOrderTemplateRelations', 'MonitorTypes', E.Message + TDModUtils.GetQueryInfo(Query));
            raise;
          end;
        end;
      finally
        FreeAndNil(Query);
      end;
    end;
  end;
var
  Query: TFDQuery;
  DocType: TDocType;
  RecordId: Integer;
  RelationId: Integer;
begin
  if (aTemplateId > 0) then
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := C_SQL_SELECT_TEXT;
      Query.ParamByName('TemplateId').Value := aTemplateId;
      Query.Open;
      Query.First;
      while not Query.Eof do
      begin
        DocType    := TDocType(Query.FieldByName('DOC_TYPE').AsInteger);
        RecordId   := Query.FieldByName('RECORD_ID').AsInteger;
        RelationId := Query.FieldByName('ID').AsInteger;
        case DocType of
          ntOrderGroup:
            DeleteRow(C_SQL_DEL_ORDERGROUP, RecordId);
          ntOrder:
            DeleteRow(C_SQL_DEL_ORDERS, RecordId);
          ntCondition:
            DeleteRow(C_SQL_DEL_CONDITION, RecordId);
        end;
        DeleteRow(C_SQL_DEL_RELATIONS, RelationId);
        Query.Next;
      end;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

class procedure TTreeDocument.DeleteRelations(const aRecordId: Integer);
resourcestring
  C_SQL_SELECT_TEXT             = 'SELECT * FROM DOC_RELATIONS WHERE ';
  C_SQL_DEL_QUALIFIER           = 'DELETE FROM QUALIFIERS WHERE ID=:RecordId';
  C_SQL_DEL_AUTOTRADES          = 'DELETE FROM AUTOTRADES WHERE ID=:RecordId';
  C_SQL_DEL_ORDER_GROUP_SET     = 'DELETE FROM ORDER_GROUP_SET WHERE ID=:RecordId';
  C_SQL_DEL_ORDERGROUP          = 'DELETE FROM ORDER_GROUP WHERE ID=:RecordId';
  C_SQL_DEL_ORDERS              = 'DELETE FROM ORDERS WHERE ID=:RecordId';
  C_SQL_DEL_CONDITION           = 'DELETE FROM CONDITION WHERE ID=:RecordId';
  C_SQL_DEL_ALGORITMOS          = 'DELETE FROM ALGORITMOS WHERE ID=:RecordId';
  C_SQL_DEL_FACTOR              = 'DELETE FROM FACTOR WHERE ID=:RecordId';
  C_SQL_DEL_DOC_RELATIONS       = 'DELETE FROM DOC_RELATIONS WHERE ID=:RecordId';

  procedure DeleteRow(aSQLText: string; aRecordId: Integer);
  var
    Query: TFDQuery;
  begin
    if (aRecordId > 0) then
    begin
      Query := TFDQuery.Create(nil);
      try
        DMod.CheckConnect;
        Query.Connection := DMod.ConnectionStock;
        Query.Transaction := Query.Connection.Transaction;
        Query.SQL.Text := aSQLText;
        Query.ParamByName('RecordId').AsInteger := aRecordId;
        try
          Query.Prepare;
          Query.ExecSQL;
          Query.Transaction.CommitRetaining;
        except
          on E: Exception do
          begin
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'TTreeDocument.DeleteRelations', 'MonitorTypes', E.Message + TDModUtils.GetQueryInfo(Query));
            raise;
          end;
        end;
      finally
        FreeAndNil(Query);
      end;
    end;
  end;

  function GetWhereClause: string;
  var
    arrItems: TIntegerArray;
  begin
    Result := '';
    arrItems := GetRelationItems(aRecordId);
    arrItems.Unique;
    for var RecordId in arrItems do
      if Result.IsEmpty then
        Result := RecordId.ToString
      else
        Result := Result + ',' + RecordId.ToString;
    if Result.IsEmpty then
      Result := 'ID=' + aRecordId.ToString
    else
      Result := 'ID in (' + Result + ')';
  end;

var
  Query: TFDQuery;
  DocType: TDocType;
  RecordId: Integer;
  RelationId: Integer;
begin
  if (aRecordId > 0) then
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := C_SQL_SELECT_TEXT + GetWhereClause;
      Query.Open;
      Query.First;
      while not Query.Eof do
      begin
        DocType    := TDocType(Query.FieldByName('DOC_TYPE').AsInteger);
        RecordId   := Query.FieldByName('RECORD_ID').AsInteger;
        RelationId := Query.FieldByName('ID').AsInteger;
        case DocType of
          ntQualifier:
            DeleteRow(C_SQL_DEL_QUALIFIER, RecordId);
          ntAutoTrade:
            DeleteRow(C_SQL_DEL_AUTOTRADES, RecordId);
          ntOrderGroupSet:
            DeleteRow(C_SQL_DEL_ORDER_GROUP_SET, RecordId);
          ntOrderGroup:
            DeleteRow(C_SQL_DEL_ORDERGROUP, RecordId);
          ntOrder:
            DeleteRow(C_SQL_DEL_ORDERS, RecordId);
          ntCondition:
            DeleteRow(C_SQL_DEL_CONDITION, RecordId);
          ntAlgos:
            DeleteRow(C_SQL_DEL_ALGORITMOS, RecordId);
          ntFactor:
            DeleteRow(C_SQL_DEL_FACTOR, RecordId);
        end;
        DeleteRow(C_SQL_DEL_DOC_RELATIONS, RelationId);
        Query.Next;
      end;
    finally
      FreeAndNil(Query);
    end;
  end;
end;

class function TTreeDocument.GetNodesListByType(aTree: TBaseVirtualTree; aNode: PVirtualNode; aDocType: TDocType): TNodeArray;
var
  CurrNode : PVirtualNode;
begin
  Result := [];
  if Assigned(aNode) then
  begin
    if (GetDocType(aTree, aNode) = aDocType) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aNode;
    end;

    if (aNode.ChildCount > 0) then
    begin
      CurrNode := aNode.FirstChild;
      while Assigned(CurrNode) do
      begin
        Result := Concat(Result, GetNodesListByType(aTree, CurrNode, aDocType));
        CurrNode := CurrNode.NextSibling;
      end;
    end;
  end;
end;

class function TTreeDocument.GetNodesList(aNode: PVirtualNode; aIsRecursive: Boolean = True): TNodeArray;
var
  CurrNode : PVirtualNode;
begin
  Result := [];
  if Assigned(aNode) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := aNode;
    if (aNode.ChildCount > 0) then
    begin
      CurrNode := aNode.FirstChild;
      while Assigned(CurrNode) do
      begin
        if aIsRecursive then
          Result := Concat(Result, GetNodesList(CurrNode))
        else
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := CurrNode;
        end;
        CurrNode := CurrNode.NextSibling;
      end;
    end;
  end;
end;

{
class function TTreeDocument.GetAutoTradeItems(aNode: PVirtualNode): TAutoTradesArray;
var
  CurrNode : PVirtualNode;
  Data     : PTreeData;
begin
  Result.Clear;
  if Assigned(aNode) and (aNode.ChildCount > 0) then
  begin
    CurrNode := aNode.FirstChild;
    while Assigned(CurrNode) do
    begin
      Data := CurrNode^.GetData;
      if (Data^.DocType  = ntAutoTrade) then
      begin
        Data^.AutoTrade.OwnerNode := CurrNode;
        Result.Add(Data^.AutoTrade);
      end;
      CurrNode := CurrNode.NextSibling;
    end;
  end;
end;
}

class procedure TTreeDocument.CopySubtree(aTree: TBaseVirtualTree; aNode: PVirtualNode; aAfterLoadProc: TAfterLoadEachDocumentProc);

  function CopySubtree(aTree: TBaseVirtualTree; aSourceNode, aParentNode: PVirtualNode; aAfterLoadProc: TAfterLoadEachDocumentProc): PVirtualNode;
  var
    CurrNode   : PVirtualNode;
    SourceData : PTreeData;
    NewData    : PTreeData;
    ParentNode : PVirtualNode;
    NewNode    : PVirtualNode;
  begin
    Result := nil;
    if Assigned(aSourceNode) then
    begin
      SourceData := aSourceNode^.GetData;
      case SourceData^.DocType of
        TDocType.ntQualifier:
          begin
            Result := TTreeDocument.CreateQualifier(aParentNode, aTree);
            NewData := Result^.GetData;
            NewData^.Qualifier.FromDB(SourceData^.RecordId);
            NewData^.Qualifier.OwnerNode := Result;
          end;
        TDocType.ntAutoTrade:
          begin
            Result := TTreeDocument.CreateAutoTrade(aParentNode, aTree);
            NewData := Result^.GetData;
            NewData^.AutoTrade.FromDB(SourceData^.RecordId);
            NewData^.AutoTrade.OwnerNode := Result;
          end;
        TDocType.ntOrderGroupSet:
          begin
            Result := TTreeDocument.CreateOrderGroupSet(aParentNode, aTree);
            NewData := Result^.GetData;
            NewData^.OrderGroupSetDoc.FromDB(SourceData^.RecordId);
            NewData^.OrderGroupSetDoc.OwnerNode := Result;
          end;
        TDocType.ntOrderGroup:
          begin
            Result := TTreeDocument.CreateOrderGroup(aParentNode, aTree);
            NewData := Result^.GetData;
            NewData^.OrderGroupDoc.FromDB(SourceData^.RecordId);
            NewData^.OrderGroupDoc.OwnerNode := Result;
          end;
        TDocType.ntOrder:
          begin
            Result := TTreeDocument.CreateOrder(aParentNode, aTree, GetOrderBroker(SourceData^.RecordId));
            NewData := Result^.GetData;
            NewData^.OrderDoc.FromDB(SourceData^.RecordId);
            case NewData^.OrderDoc.BrokerType of
              brIB:
                TOrderIBDoc(NewData^.OrderDoc).FromDB(SourceData^.RecordId);
              brNN:
                TOrderNNDoc(NewData^.OrderDoc).FromDB(SourceData^.RecordId);
            end;
            NewData^.OrderDoc.OwnerNode := Result;
          end;
        TDocType.ntCondition:
          begin
            Result := TTreeDocument.CreateCondition(aParentNode, aTree);
            NewData := Result^.GetData;
            NewData^.ConditionDoc.FromDB(SourceData^.RecordId);
            NewData^.ConditionDoc.OwnerNode := Result;
          end;
        TDocType.ntAlgos:
          begin
            Result := TTreeDocument.CreateAlgos(aParentNode, aTree);
            NewData := Result^.GetData;
            NewData^.AlgosDoc.FromDB(SourceData^.RecordId);
            NewData^.AlgosDoc.OwnerNode := Result;
          end;
        TDocType.ntFactor:
          begin
            Result := TTreeDocument.CreateFactor(aParentNode, aTree);
            NewData := Result^.GetData;
            NewData^.FactorDoc.FromDB(SourceData^.RecordId);
            NewData^.FactorDoc.OwnerNode := Result;
          end;
      end;
      if Assigned(aAfterLoadProc) then
        aAfterLoadProc(Result);

      CurrNode := aSourceNode.FirstChild;
      while Assigned(CurrNode) do
      begin
        CopySubtree(aTree, CurrNode, Result, aAfterLoadProc);
        CurrNode := CurrNode.NextSibling;
      end;
    end;
  end;

var
  arrChilds : TNodeArray;
begin
  aTree.BeginUpdate;
  try
    arrChilds := TTreeDocument.GetNodesList(aNode, False);
    for var ChildNode in arrChilds do
      if (aNode <> ChildNode) and Assigned(ChildNode) then
        CopySubtree(aTree, ChildNode, aNode, nil);
  finally
    aTree.EndUpdate;
  end;
end;

class function TTreeDocument.GetDocStatus(const aData: PTreeData): string;
begin
  if (aData^.CreationType = ctMirror) then
    Result := aData^.Caption
  else
    case aData^.DocType of
      ntQualifier:
        Result := ''{aData^.Qualifier.State.ToString};
      (*ntQualifierCondition:
        Result := 'IsCondition: ' + BoolToStr(aData^.QualifierCondition.IsCondition, True);*)
      ntAutoTrade:
        Result := aData^.AutoTrade.TradesState.ToString;
      ntOrderGroupSet:
        Result := 'TypeUse: ' + aData^.OrderGroupSetDoc.TypeUse.ToString;
      ntOrderGroup:
        Result := 'Kind: ' + aData^.OrderGroupDoc.Kind.ToString;
      ntOrder:
        Result := 'Status: ' + aData^.OrderDoc.OrderStatusText;
      ntCondition:
        Result := 'IsCondition: ' + BoolToStr(aData^.ConditionDoc.IsCondition, True);
      ntAlgos:
        Result := 'Algos';
      ntFactor:
        Result := 'Symbol: ' + aData^.FactorDoc.Symbol;
      ntUnknow:
        Result := 'Unknow';
    end;
end;

class procedure TTreeDocument.SetTickTypesForChildFactors(const aTree: TBaseVirtualTree; const aNode: PVirtualNode; const aConditionDoc: TConditionDoc = nil);
var
  Childs: TNodeArray;
  Data: PTreeData;
  CondData: PTreeData;
  ConditionDoc: TConditionDoc;
begin
  if Assigned(aNode) then
  begin
    CondData := aNode^.GetData;
    ConditionDoc := nil;
    if Assigned(aConditionDoc) then
      ConditionDoc := aConditionDoc
    else if Assigned(CondData.ConditionDoc) then
      ConditionDoc := CondData.ConditionDoc;
    if Assigned(ConditionDoc) then
    begin
      Childs := TTreeDocument.GetNodesListByType(aTree, aNode, ntFactor);
      for var Node in Childs do
      begin
        Data := Node^.GetData;
        Data^.FactorDoc.TickType1 := ConditionDoc.TickType1;
        Data^.FactorDoc.TickType2 := ConditionDoc.TickType2;
      end;
    end;
  end;
end;

class function TTreeDocument.CalcCompiledValue(const aTree: TBaseVirtualTree; const aConditionDoc: TConditionDoc): TCompiledValue;
resourcestring
  rsFactor = '%d [%s], ';
var
  Data      : PTreeData;
  NodeArray : TNodeArray;
  PriceList : TPriceList;
  Value1    : Currency;
  Value2    : Currency;
begin
  inherited;
  Result := Default(TCompiledValue);
  if Assigned(aConditionDoc) and
     Assigned(aConditionDoc.OwnerNode) and
     (aConditionDoc.CondType in [ctRealtimeValue, ctRealtimeAndTimeGap]) then
  begin
    NodeArray := GetNodesListByType(aTree, aConditionDoc.OwnerNode, TDocType.ntFactor);
    Result.IsValueReady := Length(NodeArray) > 0;
    for var Node in NodeArray do
    begin
      Data := Node^.GetData;
      PriceList := TMonitorLists.PriceCache.GetPriceList(Data^.FactorDoc.ContractId);
      if not Assigned(PriceList) then
        Result.IsValueReady := False
      else if Assigned(Data^.FactorDoc) then
      begin
        Result.Factors := Result.Factors + Format(rsFactor, [Data^.FactorDoc.ContractId, Data^.FactorDoc.Symbol]);
        if (aConditionDoc.TickType2 = ttNotSet) then
        begin
          Result.IsValueReady := Result.IsValueReady and PriceList.ReceivedTick[aConditionDoc.TickType1];
          Value1              := TPriceCache.PriceCache.GetLastPrice(Data^.FactorDoc.ContractId, aConditionDoc.TickType1);
          Result.TotalValue   := Result.TotalValue + Value1;
          Result.SummValue1   := Result.SummValue1 + Value1;
        end
        else
        begin
          Result.IsValueReady := Result.IsValueReady and PriceList.ReceivedTick[aConditionDoc.TickType1] and
                                                         PriceList.ReceivedTick[aConditionDoc.TickType2];
          Value1 := TPriceCache.PriceCache.GetLastPrice(Data^.FactorDoc.ContractId, aConditionDoc.TickType1);
          Value2 := TPriceCache.PriceCache.GetLastPrice(Data^.FactorDoc.ContractId, aConditionDoc.TickType2);
          Result.TotalValue := Result.TotalValue + aConditionDoc.TypeOperation.Calc(Value1, Value2);
          Result.SummValue1 := Result.SummValue1 + Value1;
          Result.SummValue2 := Result.SummValue2 + Value2;
          if (aConditionDoc.TickType1 = ttLast) and
             (aConditionDoc.TickType2 = ttHigh) and
             (aConditionDoc.TypeOperation = toDivide) then
          begin
            Result.TotalValue := 1;
          end;
        end;
      end
    end;
  end;
end;

class function TTreeDocument.GetDocType(const aTree: TBaseVirtualTree; const aNode: PVirtualNode): TDocType;
var
  Data: PTreeData;
begin
  Result := ntUnknow;
  if Assigned(aNode) and (aTree.RootNode <> aNode) then
  begin
    Data := aNode^.GetData;
    if (Data^.CreationType = ctMirror) then
      Result := ntUnknow
    else if Assigned(Data) then
      Result := Data.DocType;
  end;
end;

class function TTreeDocument.GetParentNode(const aTree: TBaseVirtualTree; const aNode: PVirtualNode; const aDocType: TDocType): PVirtualNode;
var
  CurrNode : PVirtualNode;
begin
  Result := nil;
  CurrNode := aNode;
  while Assigned(CurrNode) and (aTree.GetNodeLevel(CurrNode) > 0) do
  begin
    CurrNode := CurrNode.Parent;
    if (TTreeDocument.GetDocType(aTree, CurrNode) = aDocType) then
      Exit(CurrNode);
  end;
end;

class procedure TTreeDocument.SetIcon(aNode: PVirtualNode; aTree: TBaseVirtualTree);
var
  Data: PTreeData;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    case Data.DocType of
      ntQualifier:
        Data.ImageNo := ICON_QUALIFIER;
      ntAutoTrade:
        Data.ImageNo := ICON_AUTOTRADE;
      ntOrderGroupSet:
        Data.ImageNo := ICON_ORDERGROUP_SET;
      ntOrderGroup:
        Data.ImageNo := ICON_ORDERGROUP;
      ntOrder:
        begin
          if (Data.OrderDoc.OrderAction = iabBuy) then
          begin
            if Data.OrderDoc.OrderStatus in [osFilled, osPartlyFilled] then
              Data.ImageNo := ICON_ORDER_BUY_FILLED
            else
              Data.ImageNo := ICON_ORDER_BUY;
          end
          else
          begin
            if Data.OrderDoc.OrderStatus in [osFilled, osPartlyFilled] then
              Data.ImageNo := ICON_ORDER_SELL_FILLED
            else
              Data.ImageNo := ICON_ORDER_SELL;
          end
        end;
      ntCondition:
        if not Data.ConditionDoc.Active then
          Data.ImageNo := ICON_COND_DISABLED
        else
        begin
          if Data.ConditionDoc.IsCondition then
            Data.ImageNo := ICON_COND_TRUE
          else
            Data.ImageNo := ICON_COND_FALSE;
        end;
      ntAlgos:
        Data.ImageNo := ICON_ALGOS;
      ntFactor:
        Data.ImageNo := ICON_FACTOR;
    end;
    aTree.InvalidateNode(aNode);
  end;
end;

class function TTreeDocument.GetRelationItems(const aRecordId: Integer; const aParentId: Integer = 0): TIntegerArray;
resourcestring
  C_SQL_RECORD = 'SELECT DISTINCT * FROM DOC_RELATIONS WHERE RECORD_ID=:ID';
  C_SQL_PARENT = 'SELECT DISTINCT * FROM DOC_RELATIONS WHERE PARENT_ID=:ID';
var
  Query: TFDQuery;
begin
  Result.Clear;
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    if (aParentId > 0) then
    begin
      Query.SQL.Text := C_SQL_PARENT;
      Query.ParamByName('ID').AsInteger := aParentId;
    end
    else
    begin
      Query.SQL.Text := C_SQL_RECORD;
      Query.ParamByName('ID').AsInteger := aRecordId;
    end;
    try
      Query.Prepare;
      Query.Open;
      while not Query.Eof do
      begin
        Result.Add(Query.FieldByName('ID').AsInteger);
        Result.AddRange(GetRelationItems(0, Query.FieldByName('ID').AsInteger));
        Query.Next;
      end;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'GetRelationItems', 'MonitorTypes', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

class function TTreeDocument.GetOrderSubordination(const aTree: TBaseVirtualTree; const aNode: PVirtualNode): TSubordination;
var
  Data: PTreeData;
  ParentNode: PVirtualNode;
begin
  Result := suUnknow;
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data^.OrderDoc) then
    begin
      ParentNode := TTreeDocument.GetParentNode(aTree, aNode, ntOrder);
      if Assigned(ParentNode) then
        Result := suChildOrder
      else
        Result := suMotherOrder;
    end;
  end;
end;

class function TTreeDocument.LoadOrderTemplateRelationTree(const aTemplateId,
  aParentId: Integer; aTree: TBaseVirtualTree; aParentNode: PVirtualNode;
  aBeforeProc: TBeforeEachDocumentProc;
  aAfterProc: TAfterLoadEachDocumentProc): PVirtualNode;
resourcestring
  C_SQL_PARENT = 'SELECT DISTINCT * FROM ORDER_TEMPLATE_RELATIONS WHERE PARENT_ID=:ID';
  C_SQL_RECORD = 'SELECT DISTINCT * FROM ORDER_TEMPLATE_RELATIONS WHERE ORDER_TEMPLATE_ID=:ID AND PARENT_ID=-1';
var
  Data: PTreeData;
  Node: PVirtualNode;
  DocType: TDocType;
  Query: TFDQuery;
  RecordId: Integer;
  RelationId: Integer;
  IsCreate: Boolean;
begin
  Result := nil;
  if not Assigned(aTree) or ((aParentId <= 0) and (aTemplateId <= 0)) then
    Exit(nil);

  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Clear;
    if (aParentId > 0) then
    begin
      Query.SQL.Text := C_SQL_PARENT;
      Query.ParamByName('ID').AsInteger := aParentId;
    end
    else
    begin
      Query.SQL.Text := C_SQL_RECORD;
      Query.ParamByName('ID').AsInteger := aTemplateId;
    end;

    try
      Query.Open;
      Query.First;
      while not Query.Eof do
      begin
        Node := nil;
        DocType    := TDocType(Query.FieldByName('DOC_TYPE').AsInteger);
        RecordId   := Query.FieldByName('RECORD_ID').AsInteger;
        RelationId := Query.FieldByName('ID').AsInteger;

        IsCreate := True;
        if Assigned(aBeforeProc) then
          IsCreate := aBeforeProc(DocType, RelationId, RecordId, aParentId);
        if IsCreate then
          case DocType of
            TDocType.ntOrderGroup:
              begin
                Node := TTreeDocument.CreateOrderGroup(aParentNode, aTree);
                Data := Node^.GetData;
                Data^.OrderGroupDoc.FromDB(RecordId);
                Data^.OrderGroupDoc.OwnerNode := Node;
              end;
            TDocType.ntOrder:
              begin
                Node := TTreeDocument.CreateOrder(aParentNode, aTree, GetOrderBroker(RecordId));
                Data := Node^.GetData;
                Data^.OrderDoc.FromDB(RecordId);
                case Data^.OrderDoc.BrokerType of
                  brIB:
                    TOrderIBDoc(Data^.OrderDoc).FromDB(RecordId);
                  brNN:
                    TOrderNNDoc(Data^.OrderDoc).FromDB(RecordId);
                end;
                Data^.OrderDoc.OwnerNode := Node;
                TTreeDocument.SetIcon(Node, aTree);
              end;
            TDocType.ntCondition:
              begin
                Node := TTreeDocument.CreateCondition(aParentNode, aTree);
                Data := Node^.GetData;
                Data^.ConditionDoc.FromDB(RecordId);
                Data^.ConditionDoc.OwnerNode := Node;
              end;
          end;
        if Assigned(Node) then
        begin
          Result := Node;
          Data := Node^.GetData;
          if Assigned(Data) then
          begin
            Data^.RecordId     := RecordId;
            Data^.RelationId   := Query.FieldByName('ID').AsInteger;
            Data^.CreationType := TCreationType(Query.FieldByName('CREATION_TYPE').AsInteger);
          end;
          if Assigned(aAfterProc) then
            aAfterProc(Node);
          LoadOrderTemplateRelationTree(-1, Data^.RelationId, aTree, Node, aBeforeProc, aAfterProc);
        end;
        Query.Next;
      end;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'LoadOrderTemplateRelationTree', 'OrderTemplate', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

class function TTreeDocument.LoadRelationTree(const aRecordId: Integer; const aParentId: Integer; aTree: TBaseVirtualTree; aParentNode: PVirtualNode; aBeforeProc: TBeforeEachDocumentProc = nil; aAfterProc: TAfterLoadEachDocumentProc = nil): PVirtualNode;
resourcestring
  C_SQL_PARENT = 'SELECT DISTINCT * FROM DOC_RELATIONS WHERE PARENT_ID=:ID';
  C_SQL_RECORD = 'SELECT DISTINCT * FROM DOC_RELATIONS WHERE RECORD_ID=:ID';
var
  Data: PTreeData;
  Node: PVirtualNode;
  DocType: TDocType;
  Query: TFDQuery;
  RecordId: Integer;
  RelationId: Integer;
  IsCreate: Boolean;
begin
  Result := nil;
  if not Assigned(aTree) or ((aParentId <= 0) and (aRecordId <= 0)) then
    Exit(nil);

  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Clear;
    if (aParentId > 0) then
    begin
      Query.SQL.Text := C_SQL_PARENT;
      Query.ParamByName('ID').AsInteger := aParentId;
    end
    else
    begin
      Query.SQL.Text := C_SQL_RECORD;
      Query.ParamByName('ID').AsInteger := aRecordId;
    end;

    try
      Query.Open;
      Query.First;
      while not Query.Eof do
      begin
        Node := nil;
        DocType    := TDocType(Query.FieldByName('DOC_TYPE').AsInteger);
        RecordId   := Query.FieldByName('RECORD_ID').AsInteger;
        RelationId := Query.FieldByName('ID').AsInteger;

        IsCreate := True;
        if Assigned(aBeforeProc) then
          IsCreate := aBeforeProc(DocType, RelationId, RecordId, aParentId);
        if IsCreate then
          case DocType of
            TDocType.ntQualifier:
              begin
                Node := TTreeDocument.CreateQualifier(aParentNode, aTree);
                Data := Node^.GetData;
                Data^.Qualifier.FromDB(RecordId);
                Data^.Qualifier.OwnerNode := Node;
              end;
            TDocType.ntAutoTrade:
              begin
                Node := TTreeDocument.CreateAutoTrade(aParentNode, aTree);
                Data := Node^.GetData;
                Data^.AutoTrade.FromDB(RecordId);
                Data^.AutoTrade.OwnerNode := Node;
              end;
            TDocType.ntOrderGroupSet:
              begin
                Node := TTreeDocument.CreateOrderGroupSet(aParentNode, aTree);
                Data := Node^.GetData;
                Data^.OrderGroupSetDoc.FromDB(RecordId);
                Data^.OrderGroupSetDoc.OwnerNode := Node;
              end;
            TDocType.ntOrderGroup:
              begin
                Node := TTreeDocument.CreateOrderGroup(aParentNode, aTree);
                Data := Node^.GetData;
                Data^.OrderGroupDoc.FromDB(RecordId);
                Data^.OrderGroupDoc.OwnerNode := Node;
              end;
            TDocType.ntOrder:
              begin
                Node := TTreeDocument.CreateOrder(aParentNode, aTree, GetOrderBroker(RecordId));
                Data := Node^.GetData;
                Data^.OrderDoc.FromDB(RecordId);
                case Data^.OrderDoc.BrokerType of
                  brIB:
                    TOrderIBDoc(Data^.OrderDoc).FromDB(RecordId);
                  brNN:
                    TOrderNNDoc(Data^.OrderDoc).FromDB(RecordId);
                end;
                Data^.OrderDoc.OwnerNode := Node;
                TTreeDocument.SetIcon(Node, aTree);
              end;
            TDocType.ntCondition:
              begin
                Node := TTreeDocument.CreateCondition(aParentNode, aTree);
                Data := Node^.GetData;
                Data^.ConditionDoc.FromDB(RecordId);
                Data^.ConditionDoc.OwnerNode := Node;
              end;
            TDocType.ntAlgos:
              begin
                Node := TTreeDocument.CreateAlgos(aParentNode, aTree);
                Data := Node^.GetData;
                Data^.AlgosDoc.FromDB(RecordId);
                Data^.AlgosDoc.OwnerNode := Node;
              end;
            TDocType.ntFactor:
              begin
                Node := TTreeDocument.CreateFactor(aParentNode, aTree);
                Data := Node^.GetData;
                Data^.FactorDoc.FromDB(RecordId);
                Data^.FactorDoc.OwnerNode := Node;
              end;
          end;
        if Assigned(Node) then
        begin
          Result := Node;
          Data := Node^.GetData;
          if Assigned(Data) then
          begin
            Data^.RecordId     := RecordId;
            Data^.RelationId   := Query.FieldByName('ID').AsInteger;
            Data^.CreationType := TCreationType(Query.FieldByName('CREATION_TYPE').AsInteger);
          end;
          if Assigned(aAfterProc) then
            aAfterProc(Node);
          LoadRelationTree(-1, Data^.RelationId, aTree, Node, aBeforeProc, aAfterProc);
        end;
        Query.Next;
      end;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'LoadRelationTree', 'MonitorTypes', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

class procedure TTreeDocument.SaveOrderTemplateRelationTree(
  const aTemplateId: integer; aTree: TBaseVirtualTree; aNode: PVirtualNode; aIsNewGroup: Boolean);
resourcestring
  C_SQL_INSERT_TEXT = 'INSERT INTO ORDER_TEMPLATE_RELATIONS ( ID, PARENT_ID, RECORD_ID, DOC_TYPE, CREATION_TYPE, ORDER_TEMPLATE_ID) ' +
                      '                              VALUES (:ID,:ParentId, :RecordId, :DocType, :CreationType, :OrderTemplateId)';
var
  Data       : PTreeData;
  ParentData : PTreeData;
  ParentNode : PVirtualNode;
  ParentId   : Integer;
  Query      : TFDQuery;
begin
  while Assigned(aNode) do
  begin
    ParentId := -1;
    if (aTree.GetNodeLevel(aNode) > 0) and Assigned(aNode.Parent) then
    begin
      ParentData := aNode.Parent^.GetData;
      ParentId   := ParentData^.RelationId;
    end;
    Data := aNode^.GetData;
    Data^.RelationId := DMod.GetNextValue('GEN_ORDER_TEMPLATE_RELATIONS_ID');

    case Data^.DocType of
      ntOrderGroup:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
          Data^.OrderGroupDoc.SaveToDB;
        end;
      ntOrder:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
          case Data^.OrderDoc.BrokerType of
            brIB:
              if (Data^.OrderDoc is TOrderIBDoc) then
                TOrderIBDoc(Data^.OrderDoc).SaveToDB;
            brNN:
              if (Data^.OrderDoc is TOrderNNDoc) then
                TOrderNNDoc(Data^.OrderDoc).SaveToDB;
          end;
        end;
      ntCondition:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
          Data^.ConditionDoc.SaveToDB;
        end;
    end;

    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.Transaction := Query.Connection.Transaction;
      Query.SQL.Text := C_SQL_INSERT_TEXT;
      Query.ParamByName('CreationType').AsInteger     := Integer(Data^.CreationType);
      Query.ParamByName('DocType').AsInteger          := Integer(Data^.DocType);
      Query.ParamByName('ID').AsInteger               := Data^.RelationId;
      Query.ParamByName('ParentId').AsInteger         := ParentId;
      Query.ParamByName('RecordId').AsInteger         := Data^.RecordId;
      Query.ParamByName('OrderTemplateId').AsInteger  := aTemplateId;
      try
        Query.Prepare;
        Query.ExecSQL;
        Query.Transaction.CommitRetaining;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'SaveOrderTemplateRelationTree', 'OrderTemplate', E.Message + TDModUtils.GetQueryInfo(Query));
          raise;
        end;
      end;
    finally
      FreeAndNil(Query);
    end;
    SaveOrderTemplateRelationTree(aTemplateId, aTree, aNode.FirstChild, aIsNewGroup);
    DMod.TransactionStock.CommitRetaining;
    aNode := aNode.NextSibling;
  end;
end;

class procedure TTreeDocument.SaveRelationTree(aTree: TBaseVirtualTree; aNode: PVirtualNode; aIsNewGroup: Boolean = False);
resourcestring
  C_SQL_INSERT_TEXT = 'INSERT INTO DOC_RELATIONS ( ID, PARENT_ID, RECORD_ID, DOC_TYPE, CREATION_TYPE) ' +
                      '                   VALUES (:ID,:ParentId, :RecordId, :DocType, :CreationType)';
var
  Data       : PTreeData;
  ParentData : PTreeData;
  ParentNode : PVirtualNode;
  ParentId   : Integer;
  Query      : TFDQuery;
begin
  while Assigned(aNode) do
  begin
    ParentId := -1;
    if (aTree.GetNodeLevel(aNode) > 0) and Assigned(aNode.Parent) then
    begin
      ParentData := aNode.Parent^.GetData;
      ParentId   := ParentData^.RelationId;
    end;
    Data := aNode^.GetData;
    Data^.RelationId := DMod.GetNextValue('GEN_RELATIONS_ID');

    case Data^.DocType of
      ntQualifier:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
          begin
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
            Data^.Qualifier.RecordId := Data^.RecordId;
          end;
          Data^.Qualifier.SaveToDB;
        end;
      (*ntQualifierCondition:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
          begin
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
            Data^.QualifierCondition.RecordId := Data^.RecordId;
          end;
          ParentNode := GetParentNode(aTree, aNode, ntQualifier);
          if Assigned(ParentNode) then
          begin
            ParentData := ParentNode^.GetData;
            Data^.QualifierCondition.Qualifier := ParentData^.Qualifier;
          end;
          Data^.QualifierCondition.SaveToDB;
        end; *)
      ntAutoTrade:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
          begin
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
            Data^.AutoTrade.RecordId := Data^.RecordId;
          end;
          Data^.AutoTrade.SaveToDB;
        end;
      ntOrderGroupSet:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
          begin
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
            Data^.OrderGroupSetDoc.RecordId := Data^.RecordId;
          end;
          ParentNode := GetParentNode(aTree, aNode, ntAutoTrade);
          if Assigned(ParentNode) then
          begin
            ParentData := ParentNode^.GetData;
            ParentData^.AutoTrade.OrderGroupId := Data^.RecordId;
            ParentData^.AutoTrade.SaveToDB;
          end;
          Data^.OrderGroupSetDoc.SaveToDB;
        end;
      ntOrderGroup:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
          Data^.OrderGroupDoc.SaveToDB;
        end;
      ntOrder:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
          case Data^.OrderDoc.BrokerType of
            brIB:
              if (Data^.OrderDoc is TOrderIBDoc) then
                TOrderIBDoc(Data^.OrderDoc).SaveToDB;
            brNN:
              if (Data^.OrderDoc is TOrderNNDoc) then
                TOrderNNDoc(Data^.OrderDoc).SaveToDB;
          end;
        end;
      ntCondition:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
          Data^.ConditionDoc.SaveToDB;
        end;
      ntAlgos:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
          Data^.AlgosDoc.SaveToDB;
        end;
      ntFactor:
        begin
          if (Data^.RecordId <= 0) or aIsNewGroup then
            Data^.RecordId := DMod.GetNextValue('GEN_DOCUMENT_ID');
          Data^.FactorDoc.SaveToDB;
        end;
    end;

    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.Transaction := Query.Connection.Transaction;
      Query.SQL.Text := C_SQL_INSERT_TEXT;
      Query.ParamByName('CreationType').AsInteger := Integer(Data^.CreationType);
      Query.ParamByName('DocType').AsInteger      := Integer(Data^.DocType);
      Query.ParamByName('ID').AsInteger           := Data^.RelationId;
      Query.ParamByName('ParentId').AsInteger     := ParentId;
      Query.ParamByName('RecordId').AsInteger     := Data^.RecordId;
      try
        Query.Prepare;
        Query.ExecSQL;
        Query.Transaction.CommitRetaining;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, 'SaveOrderGroupSet', 'MonitorTypes', E.Message + TDModUtils.GetQueryInfo(Query));
          raise;
        end;
      end;
    finally
      FreeAndNil(Query);
    end;
    SaveRelationTree(aTree, aNode.FirstChild, aIsNewGroup);
    DMod.TransactionStock.CommitRetaining;
    aNode := aNode.NextSibling;
  end;
end;

end.
