unit Monitor.Types;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, IABSocketAPI_const, IABFunctions, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, HtmlLib, Document, Qualifiers.Types, Scanner.Types, VirtualTrees, BrokerHelperAbstr, DaModule, Data.DB,
  HtmlConsts, AutoTrades.Types, Common.Types, System.StrUtils, Utils, Global.Types,
  InstrumentList, Entity.Sokid, IABFunctions.MarketRules, System.Generics.Defaults, System.Generics.Collections,
  System.DateUtils, DaModule.Utils, ArrayHelper, Publishers, DaImages, IABFunctions.Helpers;
{$ENDREGION}

type
  PTreeData = ^TTreeData;
  TTreeData = record
  private
    FNodeId: Integer;
    FEnabled: Boolean;
    function GetNodeId: Integer;
    function GetRecordId: Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetRecordId(const Value: Integer);
  public
    Caption             : string;
    DocType             : TDocType;
    Qualifier           : TQualifier;
    AutoTrade           : TAutoTradeInfo;
    OrderGroupSetDoc    : TOrderGroupSetDoc;
    OrderGroupDoc       : TOrderGroupDoc;
    OrderDoc            : TCustomOrderDoc;
    ConditionDoc        : TConditionDoc;
    AlgosDoc            : TAlgosDoc;
    FactorDoc           : TFactorDoc;
    TradeChart          : TCustomForm; // TfrmTradeChartForm
    ConditionAlgosChart : TCustomForm; // TfrmConditionAlgosChart
    ConditionChart      : TCustomForm; // TfrmConditionChart
    ReValue             : Double;
    CreationType        : TCreationType;
    ImageNo             : Integer;
    RelationId          : Integer;
    procedure AssignFrom(const aSource: TTreeData);
    property RecordId : Integer read GetRecordId write SetRecordId;
    property Enabled  : Boolean read FEnabled    write SetEnabled;
    property NodeId   : Integer read GetNodeId   write FNodeID;
    procedure Clear;
  end;

  TRelationRecord = record
    RecordId: Integer;
    DocType: TDocType;
    RelationId: Integer;
  end;

implementation

{ TTreeData }

procedure TTreeData.Clear;
begin
  if Assigned(TradeChart) then
    FreeAndNil(TradeChart);
  if (DocType = ntCondition) then
  begin
    if Assigned(ConditionAlgosChart) then
      FreeAndNil(ConditionAlgosChart);
    if Assigned(ConditionChart) then
      FreeAndNil(ConditionChart);
  end;
  if Assigned(Qualifier) then
    FreeAndNil(Qualifier);
  if Assigned(OrderGroupSetDoc) then
    FreeAndNil(OrderGroupSetDoc);
  if Assigned(OrderGroupDoc) then
    FreeAndNil(OrderGroupDoc);
  if Assigned(OrderDoc) then
    FreeAndNil(OrderDoc);
  if Assigned(ConditionDoc) then
    FreeAndNil(ConditionDoc);
  if Assigned(AlgosDoc) then
    FreeAndNil(AlgosDoc);
  if Assigned(FactorDoc) then
    FreeAndNil(FactorDoc);
  if Assigned(TradeChart) then
    FreeAndNil(TradeChart);
  if Assigned(TradeChart) then
    FreeAndNil(TradeChart);

  //Qualifier.Clear;
  AutoTrade.Clear;

  Enabled      := True;
  CreationType := ctUser;
  ReValue      := 1.0;
  Caption      := '';
end;

procedure TTreeData.AssignFrom(const aSource: TTreeData);
begin
  Self.Clear;
  Self.Caption      := aSource.Caption;
  Self.CreationType := aSource.CreationType;
  Self.DocType      := aSource.DocType;
  Self.Enabled      := aSource.Enabled;
  Self.ImageNo      := aSource.ImageNo;
  Self.RelationId   := aSource.RelationId;
  Self.ReValue      := aSource.ReValue;
  case aSource.DocType of
    ntQualifier:
      Self.Qualifier.AssignFrom(aSource.Qualifier);
    ntAutoTrade:
      Self.AutoTrade.AssignFrom(aSource.AutoTrade);
    ntOrderGroupSet:
      begin
        if not Assigned(Self.OrderGroupSetDoc) then
          Self.OrderGroupSetDoc := TOrderGroupSetDoc.Create;
        Self.OrderGroupSetDoc.AssignFrom(aSource.OrderGroupSetDoc);
      end;
    ntOrderGroup:
      begin
        if not Assigned(Self.OrderGroupDoc) then
          Self.OrderGroupDoc := TOrderGroupDoc.Create;
        Self.OrderGroupDoc.AssignFrom(aSource.OrderGroupDoc);
      end;
    ntOrder:
      begin
        if not Assigned(Self.OrderDoc) then
          case aSource.OrderDoc.BrokerType of
            brIB:
              Self.OrderDoc := TOrderIBDoc.Create;
            brNN:
              Self.OrderDoc := TOrderNNDoc.Create;
            brTest:
              Self.OrderDoc := TOrderTestDoc.Create;
            end;
        Self.OrderDoc.AssignFrom(aSource.OrderDoc);
      end;
    ntCondition:
      begin
        if not Assigned(Self.ConditionDoc) then
          Self.ConditionDoc := TConditionDoc.Create;
        Self.ConditionDoc.AssignFrom(aSource.ConditionDoc);
      end;
    ntAlgos:
      begin
        if not Assigned(Self.AlgosDoc) then
          Self.AlgosDoc := TAlgosDoc.Create;
        Self.AlgosDoc.AssignFrom(aSource.AlgosDoc);
      end;
    ntFactor:
      begin
        if not Assigned(Self.FactorDoc) then
          Self.FactorDoc := TFactorDoc.Create;
        Self.FactorDoc.AssignFrom(aSource.FactorDoc);
      end;
  end;
  // Self.TradeChart               Self.TradeChart
  // Self.ConditionAlgosChart      Self.ConditionAlgosChart
  // Self.ConditionChart           Self.ConditionChart
end;

function TTreeData.GetNodeId: Integer;
begin
  if (FNodeID <= 0) then
    FNodeID := General.GetNextNodeID;
  Result := FNodeID;
end;

function TTreeData.GetRecordId: Integer;
begin
  Result := -1;
  case Self.DocType of
    ntQualifier:
      Result := Self.Qualifier.RecordId;
    ntAutoTrade:
      Result := Self.AutoTrade.RecordId;
    ntOrderGroup:
      if Assigned(Self.OrderGroupDoc) then
        Result := Self.OrderGroupDoc.RecordId;
    ntOrderGroupSet:
      Result := Self.OrderGroupSetDoc.RecordId;
    ntOrder:
      if Assigned(Self.OrderDoc) then
        Result := Self.OrderDoc.RecordId;
    ntCondition:
      if Assigned(Self.ConditionDoc) then
        Result := Self.ConditionDoc.RecordId;
    ntAlgos:
      if Assigned(Self.AlgosDoc) then
        Result := Self.AlgosDoc.RecordId;
    ntFactor:
      if Assigned(Self.FactorDoc) then
        Result := Self.FactorDoc.RecordId;
  end;
end;

procedure TTreeData.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  case Self.DocType of
    ntQualifier:
      if Assigned(Self.Qualifier) then
        Self.Qualifier.Enabled := FEnabled;
    ntAutoTrade:
      Self.AutoTrade.Enabled := FEnabled;
    ntOrderGroup:
      if Assigned(Self.OrderGroupDoc) then
        Self.OrderGroupDoc.Enabled := FEnabled;
    ntOrderGroupSet:
      if Assigned(Self.OrderGroupSetDoc) then
        Self.OrderGroupSetDoc.Enabled := FEnabled;
    ntOrder:
      if Assigned(Self.OrderDoc) then
        Self.OrderDoc.Enabled := FEnabled;
    ntCondition:
      if Assigned(Self.ConditionDoc) then
        Self.ConditionDoc.Enabled := FEnabled;
    ntAlgos:
      if Assigned(Self.AlgosDoc) then
        Self.AlgosDoc.Enabled := FEnabled;
    ntFactor:
      if Assigned(Self.FactorDoc) then
        Self.FactorDoc.Enabled := FEnabled;
  end;
end;

procedure TTreeData.SetRecordId(const Value: Integer);
begin
  case Self.DocType of
    ntQualifier:
      Self.Qualifier.RecordId := Value;
    ntAutoTrade:
      Self.AutoTrade.RecordId := Value;
    ntOrderGroup:
      if Assigned(Self.OrderGroupDoc) then
        Self.OrderGroupDoc.RecordId := Value;
    ntOrderGroupSet:
      if Assigned(Self.OrderGroupSetDoc) then
        Self.OrderGroupSetDoc.RecordId := Value;
    ntOrder:
      if Assigned(Self.OrderDoc) then
        Self.OrderDoc.RecordId := Value;
    ntCondition:
      if Assigned(Self.ConditionDoc) then
        Self.ConditionDoc.RecordId := Value;
    ntAlgos:
      if Assigned(Self.AlgosDoc) then
        Self.AlgosDoc.RecordId := Value;
    ntFactor:
      if Assigned(Self.FactorDoc) then
        Self.FactorDoc.RecordId := Value;
  end;
end;

end.
