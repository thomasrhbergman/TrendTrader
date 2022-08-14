unit Frame.OrderStatus;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons, Vcl.ExtCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, Document, System.Generics.Collections,
  System.Generics.Defaults, DebugWriter, CustomForms, Global.Types, System.IniFiles, IABSocketAPI_const, IABSocketAPI,
  System.Math, InstrumentList, Edit.OrderStatus, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ExtDlgs, Common.Types,
  Vcl.Printers, DaModule, Vcl.DBCtrls, Data.DB, XmlFiles, HtmlLib, Monitor.Types,
  Entity.OrderStatus, Vcl.ComCtrls, System.DateUtils, ArrayHelper, VirtualTrees.ExportHelper, DaImages, Frame.Custom,
  MessageDialog, Publishers.Interfaces, DaModule.Utils, Publishers, IABFunctions, IABFunctions.Helpers, System.Types,
  Monitor.Interfaces, Global.Resources, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TOrderStatusGroupOption = (goNone, goQualifier, goAutoTradeNum, goAutoTradeID, goSymbol, goAction);
  TOrderStatusGroupOptionHelper = record helper for TOrderStatusGroupOption
  private const
    OrderStatusGroupOptionString: array [TOrderStatusGroupOption] of string = ('None', 'Qualifier', 'AutoTrade Instance Number', 'AutoTrade Record ID', 'Symbol', 'Action');
  public
    function ToString: string;
  end;

  TframeOrderStatus = class(TframeCustom, IOrderStatus, IMonitorStructureChange)
    alOrderStatus: TActionList;
    aShowOrderStatusDetail: TAction;
    miShowOrderStatusDetail: TMenuItem;
    pmOrderStatus: TPopupMenu;
    timerMonitorUpdate: TTimer;
    procedure aShowOrderStatusDetailExecute(Sender: TObject);
    procedure aShowOrderStatusDetailUpdate(Sender: TObject);
    procedure timerMonitorUpdateTimer(Sender: TObject);
    procedure vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstTreeMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
  private const
    COL_NODE_ID            = 0;
    COL_ORDER_ID           = 1;
    COL_CON_ID             = 2;
    COL_SYMBOL             = 3;
    COL_CURRENCY           = 4;
    COL_ORDER_TYPE         = 5;
    COL_ACTION             = 6;
    COL_SECURITY_TYPE      = 7;
    COL_ORDER_SCOPE        = 8;
    COL_TEMPLATE_ID        = 9;
    COL_OCA                = 10;
    COL_MOTHER_ORDER_ID    = 11;
    COL_ADDED_TIME         = 12;
    COL_PRESUBMITTED       = 13;
    COL_PRESUBMITTED_LP    = 14;
    COL_SUBMITTED          = 15;
    COL_SUBMITTED_LP       = 16;
    COL_FILLED             = 17;
    COL_FILLED_LP          = 18;
    COL_CANCELLED          = 19;
    COL_ERROR              = 20;
    COL_QUANTITY           = 21;
    COL_LATEST_FILL_QTY    = 22;
    COL_REMAINING          = 23;
    COL_FILL_PRICE         = 24;
    COL_AVG_FILL_PRICE     = 25;
    COL_LAST_PRICE         = 26;
    COL_AUX_PRICE          = 27;
    COL_TRAILING_PERCENT   = 28;
    COL_TRAIL_STOP_PRICE   = 29;
    COL_LMT_PRICE_OFFSET   = 30;
    COL_LAST_TIMESTAMP     = 31;
    COL_IN_CALC            = 32;
    COL_SCAN_SEQUENCE      = 33;
    COL_STATUS             = 34;
    COL_QUALIFIER_ID       = 35;
    COL_QUALIFIER_INSTANCE = 36;
    COL_AUTOTRADE_ID       = 37;
    COL_AUTOTRADE_INSTANCE = 38;
    COL_PENDSUBMITTED      = 39;

    C_IDENTITY_NAME = 'frameOrderStatus';
  private
    FAutoTradesCommon : TAutoTradesCommon;
    FGroupList        : TStringList;
    FGroupOption      : TOrderStatusGroupOption;
    FIsShowSleeping   : Boolean;
    FMonitorTree      : TVirtualStringTree;
    FOrderList        : TDictionary<Integer, PVirtualNode>;
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IOrderStatus
    procedure OnStatusUpdate(const aStatus: TIABOrderState; const aOrder: TIABOrder; aNodeOrder: PVirtualNode; const aInfo: string);
    //IMonitorStructureChange
    procedure OnMonitorStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
    procedure ProcessForFilter(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure GetOrdersFromMonitor;

    function GetNodeId(const aOrderIBId: Integer): Integer;
    function GetTextByColumn(aColumnIndex: TColumnIndex; aNode: PVirtualNode): string;
    procedure LoadRecordFromDB(aID: Integer);
    procedure SetFlatTree;
    procedure SetGroupOption(const Value: TOrderStatusGroupOption);
    procedure SetGroupTree(aNode: PVirtualNode);
    procedure SetIsShowSleeping(const Value: Boolean);
  protected
    function GetIdentityName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Deinitialize; override;
    procedure LoadFromDB(const aDateStart, aDateEnd: TDateTime);
    procedure SetFilter(const aAutoTradesCommon: TAutoTradesCommon); override;
    property GroupOption : TOrderStatusGroupOption read FGroupOption write SetGroupOption;
    property IsShowSleeping: Boolean read FIsShowSleeping write SetIsShowSleeping;
  end;

implementation

{$R *.dfm}

{ TframeOrderStatus }

constructor TframeOrderStatus.Create(AOwner: TComponent);
begin
  inherited;
  FMonitorTree := nil;
  FIsShowSleeping := False;
  vstTree.NodeDataSize := SizeOf(TStatusData);
  FOrderList := TDictionary<Integer, PVirtualNode>.Create;
  FGroupOption := goNone;

  FGroupList := TStringList.Create;
  FGroupList.Duplicates := dupIgnore;
  FGroupList.Sorted := True;
  FAutoTradesCommon := Default(TAutoTradesCommon);
end;

destructor TframeOrderStatus.Destroy;
begin
  if Assigned(TPublishers.OrderStatusPublisher) then
    TPublishers.OrderStatusPublisher.Unsubscribe(Self);
  if Assigned(TPublishers.MonitorStructureChangePublisher) then
    TPublishers.MonitorStructureChangePublisher.Unsubscribe(Self);
  FreeAndNil(FOrderList);
  FreeAndNil(FGroupList);
  inherited;
end;

procedure TframeOrderStatus.Initialize;
var
  Monitor: IMonitor;
begin
  inherited Initialize;
  Self.Caption := C_IDENTITY_NAME;
  TPublishers.OrderStatusPublisher.Subscribe(Self);
  TPublishers.MonitorStructureChangePublisher.Subscribe(Self);

  if not Supports(Application.MainForm, IMonitor, Monitor) then
    raise Exception.Create(rsNotSupportsIMonitor)
  else
    FMonitorTree := Monitor.GetMainTree;
end;

procedure TframeOrderStatus.Deinitialize;
begin
  inherited;

end;

function TframeOrderStatus.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

function TframeOrderStatus.GetInstance: TObject;
begin
  Result := Self;
end;

function TframeOrderStatus.GetNodeId(const aOrderIBId: Integer): Integer;
var
  Node: PVirtualNode;
  Data: PStatusData;
begin
  Result := -1;
  for var Id in FOrderList.Keys do
  begin
    Node := FOrderList.Items[Id];
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if Data^.OrderId = aOrderIBId then
        Exit(Data^.NodeId)
    end;
  end;
end;

function TframeOrderStatus.GetTextByColumn(aColumnIndex: TColumnIndex; aNode: PVirtualNode): string;
var
  Data: PStatusData;
begin
  Data := aNode^.GetData;
  case aColumnIndex of
    COL_NODE_ID:
      Result := Data^.NodeId.ToString;
    COL_ORDER_ID:
      Result := Data^.OrderId.ToString;
    COL_CON_ID:
      Result := Data^.ConID.ToString;
    COL_SYMBOL:
      Result := Data^.Symbol;
    COL_LAST_TIMESTAMP:
      Result := FormatDateTime('yyyy.mm.dd hh:nn:ss.zzz', Data^.LastTimestamp);
    COL_LAST_PRICE:
      if (Data^.LastPrice > 0) then
        Result := FormatFloat(C_CURRENCY_FORMAT, Data^.LastPrice)
      else
        Result := '';
    COL_CURRENCY:
      Result := Data^.Currency;
    COL_ACTION:
      Result := Data^.Action.ToString;
    COL_ORDER_TYPE:
      Result := Data^.OrderType.ToString;
    COL_SECURITY_TYPE:
      Result := Data^.SecurityType.ToString;
    COL_ORDER_SCOPE:
      if (Data^.OrderScope >= 0) and (Data^.OrderScope <= 3) then
        Result := TOrderIBDoc.cScopeToString[Data^.OrderScope]
      else
        Result := '';
    COL_TEMPLATE_ID:
      if (Data^.TemplateId > 0) then
        Result := Data^.TemplateId.ToString
      else
        Result := '';
    COL_AUTOTRADE_INSTANCE:
      if (Data^.AutoTradesInstance > 0) then
        Result := Data^.AutoTradesInstance.ToString
      else
        Result := '';
    COL_OCA:
      if (Data^.OCA > 0) then
        Result := Data^.OCA.ToString
      else
        Result := '';
    COL_MOTHER_ORDER_ID:
      if (Data^.MotherOrderId > 0) then
        Result := Data^.MotherOrderId.ToString
      else
        Result := '';
    COL_ADDED_TIME:
      if (Data^.AddedTime > 0) then
        Result := FormatDateTime('hh:mm:ss.zzz', Data^.AddedTime)
      else
        Result := '';
    COL_PENDSUBMITTED:
      if (Data.PendSubmitTime > 0) then
        Result := FormatDateTime('hh:mm:ss.zzz', Data^.PendSubmitTime)
      else
        Result := '';
    COL_PRESUBMITTED:
      if (Data^.PresubmittedTime > 0) then
        Result := FormatDateTime('hh:mm:ss.zzz', Data^.PresubmittedTime)
      else
        Result := '';
    COL_PRESUBMITTED_LP:
      if (Data^.PresubmittedLP > 0) then
        Result := FormatFloat(C_CURRENCY_FORMAT, Data^.PresubmittedLP)
      else
        Result := '';
    COL_SUBMITTED_LP:
      if (Data^.SubmittedLP > 0) then
        Result := FormatFloat(C_CURRENCY_FORMAT, Data^.SubmittedLP)
      else
        Result := '';
    COL_SUBMITTED:
      if (Data.SubmittedTime > 0) then
        Result := FormatDateTime('hh:mm:ss.zzz', Data^.SubmittedTime)
      else
        Result := '';
    COL_FILLED:
      if (Data^.Filled > 0) then
        Result := Data^.Filled.ToString
      else
        Result := '';
    COL_FILLED_LP:
      if (Data^.FilledLP > 0) then
        Result := FormatFloat(C_CURRENCY_FORMAT, Data^.FilledLP)
      else
        Result := '';
    COL_CANCELLED:
      if (Data^.CancelledTime > 0) then
        Result := FormatDateTime('hh:mm:ss.zzz', Data^.CancelledTime)
      else
        Result := '';
    COL_ERROR:
      Result := Data^.Error;
    COL_QUANTITY:
      Result := Data^.Quantity.ToString;
    COL_LATEST_FILL_QTY:
      Result := Data^.LatestFillQty.ToString;
    COL_REMAINING:
      Result := Data^.Remaining.ToString;
    COL_FILL_PRICE:
      if (Data^.FillPrice > 0) then
        Result := FormatFloat(C_CURRENCY_FORMAT, Data^.FillPrice)
      else
        Result := '';
    COL_AVG_FILL_PRICE:
      if (Data^.AvgFillPrice > 0) then
        Result := FormatFloat(C_CURRENCY_FORMAT, Data^.AvgFillPrice)
      else
        Result := '';
    COL_AUX_PRICE:
      if (Data^.AuxPrice > 0) then
        Result := FormatFloat(C_CURRENCY_FORMAT, Data^.AuxPrice)
      else
        Result := '';
    COL_TRAILING_PERCENT:
      if (Data^.TrailingPercent > 0) then
        Result := FormatFloat(C_CURRENCY_FORMAT, Data^.TrailingPercent)
      else
        Result := '';
    COL_TRAIL_STOP_PRICE:
      if (Data^.TrailStopPrice > 0) then
        Result := FormatFloat(C_CURRENCY_FORMAT, Data^.TrailStopPrice)
      else
        Result := '';
    COL_IN_CALC:
      Result := Data^.InCalc;
    COL_LMT_PRICE_OFFSET:
      if (Data^.LmtPriceOffset > 0) then
        Result := FormatFloat(C_CURRENCY_FORMAT, Data^.LmtPriceOffset)
      else
        Result := '';
    COL_SCAN_SEQUENCE:
      if (Data^.ScanSequenceId > 0) then
        Result := Data^.ScanSequenceId.ToString
      else
        Result := '';
    COL_AUTOTRADE_ID:
      if (Data^.AutoTradesId > 0) then
        Result := Data^.AutoTradesId.ToString
      else
        Result := '';
    COL_QUALIFIER_ID:
      if (Data.QualifierId > 0) then
        Result := Data^.QualifierId.ToString
      else
        Result := '';
    COL_QUALIFIER_INSTANCE:
      if (Data^.QualifierInstance > 0) then
        Result := Data^.QualifierInstance.ToString
      else
        Result := '';
    COL_STATUS:
      Result :=  Data^.Status.ToString;
  else
    Result := '';
  end;
end;

procedure TframeOrderStatus.vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PStatusData;
begin
  inherited;
  Data := Node^.GetData;
  if Data^.IsArchived then
  begin
    TargetCanvas.Brush.Color := clWebGhostWhite;
    TargetCanvas.FillRect(CellRect);
  end
  else
  if (Data^.Status = osFilled) then
  begin
    TargetCanvas.Brush.Color := $00EEEEEE;
    TargetCanvas.FillRect(CellRect);
  end
  else  if Data^.Status in [osPartlyFilled, osSubmitted]  then
  begin
    TargetCanvas.Brush.Color := clWebMintcream;
    TargetCanvas.FillRect(CellRect);
  end
end;

procedure TframeOrderStatus.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PStatusData;
begin
  inherited;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  if (Data1^.NodeType = ntGroup) and (Data1^.NodeType = ntGroup) then
    Result := CompareText(Data1^.GroupText, Data2^.GroupText)
  else
    case Column of
      COL_NODE_ID:
        Result := CompareValue(Data1^.NodeId, Data2^.NodeId);
      COL_ORDER_ID:
        Result := CompareValue(Data1^.OrderId, Data2^.OrderId);
      COL_CON_ID:
        Result := CompareValue(Data1^.ConID, Data2^.ConID);
      COL_SYMBOL:
        Result := CompareText(Data1^.Symbol, Data2^.Symbol);
      COL_LAST_TIMESTAMP:
        Result := CompareValue(Data1^.LastTimestamp, Data2^.LastTimestamp);
      COL_LAST_PRICE:
        Result := CompareValue(Data1^.LastPrice, Data2^.LastPrice);
      COL_CURRENCY:
        Result := CompareText(Data1^.Currency, Data2^.Currency);
      COL_ACTION:
        Result := CompareText(Data1^.Action.ToString, Data2^.Action.ToString);
      COL_ORDER_TYPE:
        Result := CompareText(Data1^.OrderType.ToString, Data2^.OrderType.ToString);
      COL_SECURITY_TYPE:
        Result := CompareText(Data1^.SecurityType.ToString, Data2^.SecurityType.ToString);
      COL_ORDER_SCOPE:
        Result := CompareText(TOrderIBDoc.cScopeToString[Data1^.OrderScope], TOrderIBDoc.cScopeToString[Data2^.OrderScope]);
      COL_TEMPLATE_ID:
        Result := CompareValue(Data1^.TemplateId, Data2^.TemplateId);
      COL_AUTOTRADE_INSTANCE:
        Result := CompareValue(Data1^.AutoTradesInstance, Data2^.AutoTradesInstance);
      COL_OCA:
        Result := CompareValue(Data1^.OCA, Data2^.OCA);
      COL_MOTHER_ORDER_ID:
        Result := CompareValue(Data1^.MotherOrderId, Data2^.MotherOrderId);
      COL_ADDED_TIME:
        Result := CompareValue(Data1^.AddedTime, Data2^.AddedTime);
      COL_PRESUBMITTED:
        Result := CompareValue(Data1^.PresubmittedTime, Data2^.PresubmittedTime);
      COL_PENDSUBMITTED:
        Result := CompareValue(Data1^.PendSubmitTime, Data2^.PendSubmitTime);
      COL_PRESUBMITTED_LP:
        Result := CompareValue(Data1^.PresubmittedLP, Data2^.PresubmittedLP);
      COL_SUBMITTED_LP:
        Result := CompareValue(Data1^.SubmittedLP, Data2^.SubmittedLP);
      COL_SUBMITTED:
        Result := CompareValue(Data1^.SubmittedTime, Data2^.SubmittedTime);
      COL_FILLED:
        Result := CompareValue(Data1^.Filled, Data2^.Filled);
      COL_FILLED_LP:
        Result := CompareValue(Data1^.FilledLP, Data2^.FilledLP);
      COL_CANCELLED:
        Result := CompareValue(Data1^.CancelledTime, Data2^.CancelledTime);
      COL_ERROR:
        Result := CompareText(Data1^.Error, Data2^.Error);
      COL_QUANTITY:
        Result := CompareValue(Data1^.Quantity, Data2^.Quantity);
      COL_LATEST_FILL_QTY:
        Result := CompareValue(Data1^.LatestFillQty, Data2^.LatestFillQty);
      COL_REMAINING:
        Result := CompareValue(Data1^.Remaining, Data2^.Remaining);
      COL_FILL_PRICE:
        Result := CompareValue(Data1^.FillPrice, Data2^.FillPrice);
      COL_AVG_FILL_PRICE:
        Result := CompareValue(Data1^.AvgFillPrice, Data2^.AvgFillPrice);
      COL_AUX_PRICE:
        Result := CompareValue(Data1^.AuxPrice, Data2^.AuxPrice);
      COL_TRAILING_PERCENT:
        Result := CompareValue(Data1^.TrailingPercent, Data2^.TrailingPercent);
      COL_TRAIL_STOP_PRICE:
        Result := CompareValue(Data1^.TrailStopPrice, Data2^.TrailStopPrice);
      COL_IN_CALC:
        Result := CompareText(Data1^.InCalc, Data2^.InCalc);
      COL_LMT_PRICE_OFFSET:
        Result := CompareValue(Data1^.LmtPriceOffset, Data2^.LmtPriceOffset);
      COL_SCAN_SEQUENCE:
        Result := CompareValue(Data1^.ScanSequenceId, Data2^.ScanSequenceId);
      COL_AUTOTRADE_ID:
        Result := CompareValue(Data1^.AutoTradesId, Data2^.AutoTradesId);
      COL_QUALIFIER_ID:
        Result := CompareValue(Data1^.QualifierId, Data2^.QualifierId);
      COL_QUALIFIER_INSTANCE:
        Result := CompareValue(Data1^.QualifierInstance, Data2^.QualifierInstance);
      COL_STATUS:
        Result := CompareText(Data1^.Status.ToString, Data2^.Status.ToString);
    end;
end;

procedure TframeOrderStatus.aShowOrderStatusDetailExecute(Sender: TObject);
var
  Data: PStatusData;
  Node: PVirtualNode;
begin
  inherited;
  Node := vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if Assigned(Data) and (Data^.NodeType = ntNode) then
    begin
      if Assigned(Data^.NodeOrder) then
        TfrmEditOrderStatus.ShowDocument(Data^.NodeOrder)
      else
        TfrmEditOrderStatus.ShowDocument(Data^.OrderStatusItems);
    end
    else
      TMessageDialog.ShowWarning('Not assigned');
  end;
end;

procedure TframeOrderStatus.aShowOrderStatusDetailUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := not vstTree.IsEmpty and Assigned(vstTree.FocusedNode);
end;

procedure TframeOrderStatus.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PStatusData;
begin
  inherited;
  Data := Node^.GetData;
  if (Data^.NodeType = ntGroup) then
  begin
    TargetCanvas.Font.Style := [fsBold];
    if (GroupOption = goAction) then
    begin
      if Text.Equals(iabBuy.ToString) then
        TargetCanvas.Font.Color := clBlue
      else
        TargetCanvas.Font.Color := clRed;
    end;
  end
  else
  begin
    if (Data^.Status = osError) then
      TargetCanvas.Font.Color := clRed
    else if (Column = COL_ACTION) then
    begin
      if (Data^.Action = iabBuy) then
        TargetCanvas.Font.Color := clBlue
      else
        TargetCanvas.Font.Color := clRed;
      TargetCanvas.Font.Style := [fsBold];
    end
    else if (Column = COL_FILL_PRICE) then
    begin
      if (Data^.Action = iabSell) then
        TargetCanvas.Font.Color := clGreen;
    end;
  end;
end;

procedure TframeOrderStatus.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PStatusData;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TframeOrderStatus.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PStatusData;
begin
  inherited;
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then
    CellText := ''
  else if (Data.NodeType = ntGroup) then
  begin
    if (Column = COL_ORDER_ID) then
      CellText := Data^.GroupText
    else
      CellText := '';
  end
  else
    CellText := GetTextByColumn(Column, Node);
end;

procedure TframeOrderStatus.vstTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PStatusData;
begin
  inherited;
  Data := Node^.GetData;
  SetLength(Data^.Info, vstTree.Header.Columns.Count);
end;

procedure TframeOrderStatus.vstTreeMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  inherited;
  NodeHeight := 16;
end;

procedure TframeOrderStatus.SetFilter(const aAutoTradesCommon: TAutoTradesCommon);
var
  Data: PStatusData;
begin
  FAutoTradesCommon := aAutoTradesCommon;
  for var Node in FOrderList.Values do
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if (fpFiltered in Self.Parameters) then
        vstTree.IsVisible[Node] := (Data^.AutoTradesInstance > 0) and
                                   (Data^.QualifierID = FAutoTradesCommon.QualifierID) and
                                   (Data^.QualifierInstance = FAutoTradesCommon.QualifierInstance) and
                                   (Data^.AutoTradesID = FAutoTradesCommon.AutoTradesID) and
                                   (Data^.AutoTradesInstance = FAutoTradesCommon.AutoTradesInstance)
      else
        vstTree.IsVisible[Node] := True;

      if vstTree.IsVisible[Node] and (Data.Status = osSleeping) then
        vstTree.IsVisible[Node] := FIsShowSleeping;
    end;
end;

procedure TframeOrderStatus.SetFlatTree;
var
  Node: PVirtualNode;
  i: Integer;
begin
  for Node in FOrderList.Values do
    vstTree.MoveTo(Node, nil, amInsertBefore, False);

  for i := 0 to FGroupList.Count - 1 do
    if Assigned(FGroupList.Objects[i]) then
      vstTree.DeleteNode(PVirtualNode(FGroupList.Objects[i]));
  FGroupList.Clear;
end;

procedure TframeOrderStatus.SetGroupOption(const Value: TOrderStatusGroupOption);
var
  Node: PVirtualNode;
begin
  if (FGroupOption <> Value) then
  begin
    FGroupOption := Value;
    if Assigned(vstTree) then
    begin
      vstTree.BeginUpdate;
      try
        SetFlatTree;
        for Node in FOrderList.Values do
          SetGroupTree(Node);
        vstTree.FullExpand(nil);
      finally
        vstTree.EndUpdate;
      end;
    end;
  end;
end;

procedure TframeOrderStatus.SetGroupTree(aNode: PVirtualNode);
var
  AttMode: TVTNodeAttachMode;
  Data: PStatusData;
  GroupData: PStatusData;
  GroupNode: PVirtualNode;
  GroupText: string;
  Index: Integer;
  MotherNodeId: Integer;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    case GroupOption of
      goSymbol:
        GroupText := Data^.Symbol;
      goQualifier:
        GroupText := 'Qualifier: ' + Data^.QualifierId.ToString;
      goAutoTradeNum:
        if (Data^.AutoTradesInstance <= 0) then
          GroupText := 'AutoTrade Instance Number (Manual): ' + Abs(Data^.AutoTradesInstance).ToString
        else
          GroupText := 'AutoTrade Instance Number: ' + Data^.AutoTradesInstance.ToString;
      goAutoTradeID:
        if (Data^.AutoTradesId <= 0) then
          GroupText := 'AutoTrade Manual'
        else
          GroupText := 'AutoTrade RecordID: ' + Data^.AutoTradesId.ToString;
      goAction:
        GroupText := Data^.Action.ToString;
    else
      GroupText := '';
    end;

    MotherNodeId := GetNodeId(Data^.MotherOrderId);
    if (GroupOption <> goAction) and (Data^.MotherOrderId > 0) and FOrderList.ContainsKey(MotherNodeId) then
      GroupNode := FOrderList.Items[Data^.MotherOrderId]
    else
    begin
      Index := FGroupList.IndexOf(GroupText);
      if (Index < 0) then
      begin
        GroupNode := vstTree.AddChild(nil);
        GroupData := GroupNode^.GetData;
        GroupData^.GroupText := GroupText;
        GroupData^.NodeType := ntGroup;

        Index := FGroupList.Add(GroupText);
        FGroupList.Objects[Index] := TObject(GroupNode);
      end
      else
        GroupNode := PVirtualNode(FGroupList.Objects[Index]);
    end;

    if Assigned(GroupNode) then
    begin
      if (GroupNode.ChildCount = 0) then
        AttMode := amAddChildFirst
      else
      begin
        AttMode := amInsertAfter;
        GroupNode := GroupNode.LastChild;
      end;
      vstTree.MoveTo(aNode, GroupNode, AttMode, False);
    end;
  end;
end;

procedure TframeOrderStatus.SetIsShowSleeping(const Value: Boolean);
var
  Data: PStatusData;
begin
  FIsShowSleeping := Value;
  for var Node in FOrderList.Values do
    if Assigned(Node) then
    begin
      Data := Node.GetData;
      if (Data.Status = osSleeping) then
        vstTree.IsVisible[Node] := FIsShowSleeping;
    end;
end;

procedure TframeOrderStatus.timerMonitorUpdateTimer(Sender: TObject);
begin
  inherited;
  timerMonitorUpdate.Enabled := False;
  GetOrdersFromMonitor;
end;

procedure TframeOrderStatus.OnMonitorStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
var
  MonitorData: PTreeData;
  StatusNode: PVirtualNode;
  StatusData: PStatusData;
begin
  if (Reason = crChildDeleted) and Assigned(Node) then
  begin
    MonitorData := Node.GetData;
    if FOrderList.ContainsKey(MonitorData^.NodeId) then
    begin
      FOrderList.TryGetValue(MonitorData^.NodeId, StatusNode);
      if Assigned(StatusNode) then
      begin
        StatusData := StatusNode.GetData;
        if StatusData.Status = osSleeping then
        begin
          FOrderList.Remove(MonitorData^.NodeId);
          vstTree.DeleteNode(StatusNode);
        end;
      end;
    end;
  end
  else
    timerMonitorUpdate.Enabled := True;
end;

procedure TframeOrderStatus.ProcessForFilter(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  SourceData: PTreeData;
begin
  if Assigned(Node) then
  begin
    SourceData := Node^.GetData;
    if Assigned(SourceData.OrderDoc) then
      OnStatusUpdate(SourceData.OrderDoc.OrderStatus, nil, Node, '');
  end;
end;

procedure TframeOrderStatus.GetOrdersFromMonitor;
begin
  if Assigned(FMonitorTree) then
  begin
    FMonitorTree.BeginUpdate;
    vstTree.BeginUpdate;
    try
      FMonitorTree.IterateSubtree(nil, ProcessForFilter, nil, [], True);
    finally
      vstTree.EndUpdate;
      FMonitorTree.EndUpdate;
    end;
  end;
end;

procedure TframeOrderStatus.OnStatusUpdate(const aStatus: TIABOrderState; const aOrder: TIABOrder; aNodeOrder: PVirtualNode; const aInfo: string);
var
  Data: PStatusData;
  MonitorData: PTreeData;
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
  OrderIBId: Integer;
  MotherNodeId: Integer;
  OrderDoc: TOrderIBDoc;
  NeedRebuild: Boolean;
begin
  if not Assigned(aNodeOrder) then
    aNodeOrder := TMonitorLists.OrderList.GetNodeOrder(aOrder.TempId);

  if Assigned(aNodeOrder) then
  begin
    MonitorData := PVirtualNode(aNodeOrder)^.GetData;
    if Assigned(MonitorData^.OrderDoc) then
    begin
      OrderDoc := TOrderIBDoc(MonitorData^.OrderDoc);
      if Assigned(aOrder) then
      begin
        OrderIBId := aOrder.TempId;
        if (aOrder.TrailingPercent = UNSET_DOUBLE) then
          aOrder.TrailingPercent := 0;
        if (aOrder.TrailStopPrice = UNSET_DOUBLE) then
          aOrder.TrailStopPrice := 0;
        if (aOrder.AuxPrice = UNSET_DOUBLE) then
          aOrder.AuxPrice := 0;
        if (aOrder.LmtPriceOffset = UNSET_DOUBLE) then
          aOrder.LmtPriceOffset := 0;
        if (aOrder.FilledQuantity = UNSET_DOUBLE) then
          aOrder.FilledQuantity := 0;
      end
      else
        OrderIBId := TOrderIBDoc(MonitorData^.OrderDoc).OrderIBId;

      ParentNode := nil;
      if (OrderDoc.ParentIBId > 0) then
      begin
        MotherNodeId := GetNodeId(aOrder.ParentId);
        if FOrderList.ContainsKey(MotherNodeId) then
          ParentNode := FOrderList.Items[MotherNodeId];
      end;

      if not Assigned(ParentNode) and Assigned(OrderDoc.ParentOrder) then
      begin
        MotherNodeId := GetNodeId(TOrderIBDoc(OrderDoc.ParentOrder).OrderIBId);
        if Assigned(OrderDoc.ParentOrder) and FOrderList.ContainsKey(MotherNodeId) then
          ParentNode := FOrderList.Items[MotherNodeId];
      end;

      NeedRebuild := False;
      if not FOrderList.ContainsKey(MonitorData^.NodeId) then
      begin
        Node := vstTree.AddChild(ParentNode);
        FOrderList.Add(MonitorData^.NodeId, Node);
        NeedRebuild := True;
      end
      else
        Node := FOrderList.Items[MonitorData^.NodeId];
      Data := Node^.GetData;

      vstTree.BeginUpdate;
      try
        Data^.NodeId             := MonitorData^.NodeId;
        Data^.NodeOrder          := aNodeOrder;
        Data^.LastTimestamp      := Now;
        Data^.IsArchived         := False;
        Data^.NodeType           := ntNode;
        Data^.OrderId            := OrderIBId;
        Data^.Action             := OrderDoc.OrderAction;
        Data^.QualifierId        := OrderDoc.QualifierID;
        Data^.QualifierInstance  := OrderDoc.QualifierInstance;
        Data^.AutoTradesId       := OrderDoc.AutoTradesId;
        Data^.AutoTradesInstance := OrderDoc.AutoTradesInstance;
        Data^.ConID              := OrderDoc.Id;
        Data^.Currency           := OrderDoc.Currency;
        Data^.MotherOrderId      := OrderDoc.ParentIBId;
        Data^.OCA                := OrderDoc.OcaGroupNumber;
        Data^.OrderScope         := OrderDoc.Scope;
        Data^.OrderType          := OrderDoc.OrderType;
        Data^.Quantity           := OrderDoc.Quantity;
        Data^.SecurityType       := OrderDoc.SecurityType;
        Data^.Symbol             := OrderDoc.Symbol;
        Data^.Status             := aStatus;
        if (Data^.MotherOrderId <= 0) then
          if Assigned(OrderDoc.ParentOrder) then
            Data^.MotherOrderId := OrderDoc.ParentOrder.OrderIBId;

        if (Data^.AddedTime = 0) then
          Data^.AddedTime := Now;

        case aStatus of
          osPendSubmit:
            Data^.PendSubmitTime := Now;
          osPreSubmit:
            begin
              Data^.PresubmittedTime := Now;
              Data^.PresubmittedLP   := TMonitorLists.PriceCache.GetLastPrice(OrderDoc.Id, ttLast)
            end;
          osSubmitted:
            begin
              Data^.SubmittedTime := Now;
              Data^.SubmittedLP   := TMonitorLists.PriceCache.GetLastPrice(OrderDoc.Id, ttLast);
            end;
          osCancelled:
            Data^.CancelledTime := Now;
          osFilled, osPartlyFilled:
            begin
              Data^.FilledLP := TMonitorLists.PriceCache.GetLastPrice(OrderDoc.Id, ttLast);
              if Assigned(aOrder) then
              begin
                if (aOrder.Filled > 0) then
                  Data^.Filled := aOrder.Filled;
                if (aOrder.FillPrice > 0) then
                  Data^.FillPrice := aOrder.FillPrice;
                if (OrderDoc.AvgPrice > 0) then
                  Data^.AvgFillPrice := OrderDoc.AvgPrice;
              end;
            end;
          osError:
            Data^.Error := aInfo;
        end;

        case OrderDoc.OrderType of
          otLimitTouch, otLimit, otLimitClose, otStopLimit:
            begin
              Data^.InCalc := FormatFloat(C_CURRENCY_FORMAT, OrderDoc.ExtendedOptions.LimitPriceRelative) + '% / ' + FormatFloat(C_CURRENCY_FORMAT, OrderDoc.Limit);
              if Assigned(aOrder) then
                Data^.LmtPriceOffset := aOrder.LmtPriceOffset;
            end;
          otStop:
            Data^.InCalc := FormatFloat(C_CURRENCY_FORMAT, OrderDoc.ExtendedOptions.AuxPriceRelative) + '% / ' + FormatFloat(C_CURRENCY_FORMAT, OrderDoc.AuxPrice);
          otTrail:
            begin
              Data^.InCalc := FormatFloat(C_CURRENCY_FORMAT, OrderDoc.ExtendedOptions.TrailStopPriceRelative) + '% / ' + FormatFloat(C_CURRENCY_FORMAT, OrderDoc.TrailStopPrice);
              if Assigned(aOrder) then
              begin
                Data^.TrailingPercent := aOrder.TrailingPercent;
                Data^.TrailStopPrice := aOrder.TrailStopPrice;
              end;
            end;
          otMarket, otMarketClose, otMarketOpen:
            if Assigned(aOrder) then
              Data^.AuxPrice := aOrder.AuxPrice;
        end;

        Data^.LastPrice := TMonitorLists.PriceCache.GetLastPrice(OrderDoc.Id, ttLast);
        if Assigned(aOrder) then
        begin
          if (OrderDoc.Quantity - aOrder.Filled) < Data^.Remaining then
            Data^.Remaining := OrderDoc.Quantity - aOrder.Filled;
          Data^.LatestFillQty := aOrder.LatestFillQty;
          Data^.AuxPrice      := aOrder.AuxPrice;
        end;

        if (fpFiltered in Self.Parameters) then
          vstTree.IsVisible[Node] := (Data^.AutoTradesInstance > 0) and
                                     (Data^.QualifierID = FAutoTradesCommon.QualifierID) and
                                     (Data^.QualifierInstance = FAutoTradesCommon.QualifierInstance) and
                                     (Data^.AutoTradesID = FAutoTradesCommon.AutoTradesID) and
                                     (Data^.AutoTradesInstance = FAutoTradesCommon.AutoTradesInstance)
        else
          vstTree.IsVisible[Node] := True;

        if vstTree.IsVisible[Node] and (Data^.Status = osSleeping) then
          vstTree.IsVisible[Node] := FIsShowSleeping;

        if NeedRebuild then
          SetGroupTree(Node);
        vstTree.InvalidateNode(Node);
        vstTree.FullExpand(ParentNode);
      finally
        vstTree.EndUpdate;
      end;
    end;
  end;
end;

procedure TframeOrderStatus.LoadRecordFromDB(aID: Integer);
var
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
  Data: PStatusData;
  AttMode: TVTNodeAttachMode;
  MotherNodeId: Integer;
begin
  Node := vstTree.AddChild(nil);
  Data := Node^.GetData;
  Data^.FromDB(aID);
  Data^.IsArchived := True;
  Data^.NodeOrder := nil;

  if not FOrderList.ContainsKey(Data^.NodeId) then
    FOrderList.Add(Data^.NodeId, Node);

  ParentNode := nil;
  if (Data^.MotherOrderId > 0) then
  begin
    MotherNodeId := GetNodeId(Data^.MotherOrderId);
    if FOrderList.ContainsKey(MotherNodeId) then
      ParentNode := FOrderList.Items[Data^.MotherOrderId];
  end;

  if Assigned(ParentNode) then
  begin
    if (ParentNode.ChildCount = 0) then
      AttMode := amAddChildFirst
    else
    begin
      AttMode := amInsertAfter;
      ParentNode := ParentNode.LastChild;
    end;
    vstTree.MoveTo(Node, ParentNode, AttMode, False);
  end;

  if (fpFiltered in Self.Parameters) then
    vstTree.IsVisible[Node] := (Data^.AutoTradesInstance > 0) and
                               (Data^.QualifierID = FAutoTradesCommon.QualifierID) and
                               (Data^.QualifierInstance = FAutoTradesCommon.QualifierInstance) and
                               (Data^.AutoTradesID = FAutoTradesCommon.AutoTradesID) and
                               (Data.AutoTradesInstance = FAutoTradesCommon.AutoTradesInstance)
  else
    vstTree.IsVisible[Node] := True;

  vstTree.InvalidateNode(Node);
  SetGroupTree(Node);
end;

procedure TframeOrderStatus.LoadFromDB(const aDateStart, aDateEnd: TDateTime);
resourcestring
  C_SQL_TEXT = 'SELECT ID FROM ORDER_STATUS WHERE (LAST_TIME BETWEEN :DateStart AND :DateEnd) ORDER BY MOTHER_ORDER_ID, ID';

  procedure DeleteArchivedNodes;
  var
    Id: Integer;
    Node: PVirtualNode;
    Data: PStatusData;
    Arr: TIntegerArray;
  begin
    Arr.Clear;
    for Id in FOrderList.Keys do
    begin
      Node := FOrderList.Items[Id];
      if Assigned(Node) then
      begin
        Data := Node^.GetData;
        if Data^.IsArchived then
        begin
          vstTree.DeleteNode(Node);
          Arr.AddUnique(Id);
        end;
      end
      else
        Arr.AddUnique(Id);
    end;
    for Id in Arr do
      FOrderList.Remove(Id);
  end;

var
  Query: TFDQuery;
begin
  vstTree.BeginUpdate;
  try
    DeleteArchivedNodes;
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect(DMod.ConnectionFeed);
      Query.Connection := DMod.ConnectionFeed;
      Query.SQL.Text := C_SQL_TEXT;
      Query.ParamByName('DateStart').AsDateTime := aDateStart;
      Query.ParamByName('DateEnd').AsDateTime   := aDateEnd;
      Query.Transaction := Query.Connection.Transaction;
      Query.Transaction.CommitRetaining;
      try
        Query.Prepare;
        Query.Open;
        while not Query.Eof do
        begin
          LoadRecordFromDB(Query.FieldByName('ID').AsInteger);
          Query.Next;
        end;
      except
        on E: Exception do
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'aLoadFromDBExecute', E.Message + TDModUtils.GetQueryInfo(Query));
      end;
    finally
      FreeAndNil(Query);
    end;
  finally
    vstTree.FullExpand;
    vstTree.EndUpdate;
  end;
end;

{ TOrderStatusGroupOptionHelper }

function TOrderStatusGroupOptionHelper.ToString: string;
begin
  Result := OrderStatusGroupOptionString[Self];
end;

end.
