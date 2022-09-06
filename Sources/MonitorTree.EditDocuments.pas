unit MonitorTree.EditDocuments;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, VirtualTrees, System.StrUtils, Vcl.StdCtrls, Vcl.ExtCtrls, DebugWriter, CustomForms,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}  Global.Types, IABSocketAPI_const, IABSocketAPI, InstrumentList, BrokerHelperAbstr, DaModule, Document,
  MessageDialog, Monitor.Types, Edit.OrderGroup, Edit.Condition, Edit.Algos, Edit.OrderDocument, Edit.Factor,
  Qualifiers.Edit, AutoTrades.Edit, Edit.OrderGroupSet, Edit.OrderIB, Edit.OrderNN, Common.Types,
  MonitorTree.Document;
{$ENDREGION}

type
  TTreeDocumentEdit = class
  public
    class procedure Edit(const aTree: TVirtualStringTree; const aTypeUse: TTypeUseInAutoorders; const aReadOnly: Boolean = False);
  end;

implementation

{ TTreeDocumentEdit }

class procedure TTreeDocumentEdit.Edit(const aTree: TVirtualStringTree; const aTypeUse: TTypeUseInAutoorders; const aReadOnly: Boolean = False);
var
  Data: PTreeData;
  Node: PVirtualNode;
  UseInTemplate: Boolean;
begin
  UseInTemplate := (tuTemplate in aTypeUse) or (tuBaseOrder in aTypeUse);
  Node := aTree.FocusedNode;
  if Assigned(Node) and (not aReadOnly) then
  begin
    Data := Node^.GetData;
    case Data^.DocType of
      ntQualifier:
        if (TfrmQualifierEdit.ShowEditForm(Data^.Qualifier, dmUpdate) = mrOk) then
          Data^.Qualifier.SaveToDB;
      ntAutoTrade:
        if TfrmAutoTradesEdit.ShowEditForm(Data^.AutoTrade, dmUpdate) = mrOk then
          Data^.AutoTrade.SaveToDB;
      ntOrderGroupSet:
        if TfrmEditOrderGroupSet.ShowDocument(Data^.OrderGroupSetDoc, UseInTemplate) = mrOk then
          Data^.OrderGroupSetDoc.SaveToDB;
      ntOrderGroup:
        if TfrmEditOrderGroup.ShowDocument(Data^.OrderGroupDoc, UseInTemplate) = mrOk then
          Data^.OrderGroupDoc.SaveToDB;
      ntCondition:
        if UseInTemplate then
        begin
          if (TfrmEditCondition.ShowDocument(Data^.ConditionDoc, aTree) = mrOk) then
            Data^.ConditionDoc.SaveToDB;
        end
        else
        begin
          if (TfrmEditCondition.ShowDocument(Data^.ConditionDoc) = mrOk) then
            Data^.ConditionDoc.SaveToDB;
        end;
      ntAlgos:
        if TfrmEditAlgos.ShowDocument(Data^.AlgosDoc) = mrOk then
          Data^.AlgosDoc.SaveToDB;
      ntOrder:
        begin
          if UseInTemplate then
          begin
            if TfrmOrderDocument.ShowDocument(Data^.OrderDoc, TTreeDocument.GetOrderSubordination(aTree, Node)) = mrOk then
              Data^.OrderDoc.SaveToDB;
          end
          else
          begin
            if (Data^.OrderDoc.BrokerType = TBrokerType.brIB) and
              (TfrmEditOrderIB.ShowDocument(TOrderIBDoc(Data^.OrderDoc)) = mrOk) then
              Data^.OrderDoc.SaveToDB
            else if (Data^.OrderDoc.BrokerType = TBrokerType.brNN) and
              (TfrmEditOrderNN.ShowDocument(TOrderNNDoc(Data^.OrderDoc), Data^.OrderDoc.ExtendedOptions.Subordination) = mrOk) then
              Data^.OrderDoc.SaveToDB;
          end;
        end;
      ntFactor:
        if TfrmEditFactor.ShowDocument(Data^.FactorDoc) = mrOk then
          Data^.FactorDoc.SaveToDB;
    end;
//    aTree.InvalidateNode(Node);
  end;
end;

end.
