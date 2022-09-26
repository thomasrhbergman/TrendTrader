unit AutoTrades.Frame;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons, Vcl.ExtCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, System.Generics.Collections, Publishers,
  System.Generics.Defaults, DebugWriter, CustomDockForm, Global.Types, System.IniFiles, IABSocketAPI_const, IABSocketAPI,
  System.Math, InstrumentList, Edit.OrderStatus, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ExtDlgs, BrokerHelperAbstr,
  Vcl.Printers, DaModule, Scanner.Types, MessageDialog, Monitor.Types, Document, VirtualTrees.ExportHelper,
  MonitorTree.Document, IABFunctions.RequestsQueue, Monitor.Interfaces, AutoTrades.Types, Common.Types, DaImages,
  Frame.Custom, MonitorTree.Helper;
{$ENDREGION}

type
  TAutoTradeGroupOption = (goNone, goQualifier, goName);

  TframeAutoTrades = class(TframeCustom, IAutoTradesController)
    procedure vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTreeNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  private const
    COL_INST_NUM           = 0;
    COL_AUTOTRADES_NAME    = 1;
    COL_QUALIFIER          = 2;
    COL_MARKET_SCAN_ID     = 3;
    COL_AUTOTRADES_ID      = 4;
    COL_BUTTON_START_STOP  = 5;
    COL_BUTTON_RESTART     = 6;
    COL_BUTTON_CANCEL      = 7;
    COL_STARTED_TIME       = 8;
    COL_STOPPED_TIME       = 9;
    COL_RESTART_TIME       = 10;
    COL_ORDER_GROUP_ID     = 11;
    COL_STATE              = 12;
    COL_CURRENCY           = 13;
    COL_MAX_NUMBER_ORDER   = 14;
    COL_CURR_NUMBER_ORDER  = 15;
    COL_MAX_ROWS           = 16;
    COL_ORDER_AMOUNT       = 17;
    COL_ORDER_RANKING_SUM  = 18;
    COL_LAST_UPDATE        = 19;
    COL_SCAN_COUNT         = 20;
    COL_INFO               = 21;
    COL_COLUMNS            = 22;
    COL_QUALIFIER_ID       = 23;
    COL_QUALIFIER_INSTANCE = 24;

    C_IDENTITY_NAME = 'frameAutoTrades';
  protected
    function GetIdentityName: string; override;
  private
    FAutoTradesCommon : TAutoTradesCommon;
    FGroupList        : TStringList;
    FGroupOption      : TAutoTradeGroupOption;
    FNodeList         : TDictionary<Integer, PVirtualNode>;
    procedure DeleteOrdersByInstanceNum(aData: PTradesData);
    procedure SetFlatTree;
    procedure SetGroupOption(const Value: TAutoTradeGroupOption);
    procedure SetGroupTree(aNode: PVirtualNode);
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IAutoTradesController
    procedure UpdateState(const aAutoTrade: IAutoTrade);
    procedure SetInfo(const aInstanceNum: Integer; const aInfo: string);
    procedure DeleteAutoTrade(const aInstanceNum: Integer; const aSilenceMode: Boolean = False);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Deinitialize; override;

    procedure Delete;
    procedure DeleteAll;
    procedure SetFilter(const aAutoTradesCommon: TAutoTradesCommon); override;
    procedure SetState(const aState: TTradesState);
    procedure Start;
    procedure Stop;

    property GroupOption: TAutoTradeGroupOption read FGroupOption write SetGroupOption;
  end;

const
  AutoTradeGroupOptionString: array [TAutoTradeGroupOption] of string = ('None', 'Qualifier', 'Name');

implementation

{$R *.dfm}

{ TframeAutoTrades }

constructor TframeAutoTrades.Create(AOwner: TComponent);
begin
  inherited;
  vstTree.NodeDataSize := SizeOf(TTradesData);
  FNodeList := TDictionary<Integer, PVirtualNode>.Create;
  FGroupOption := goNone;

  FGroupList := TStringList.Create;
  FGroupList.Duplicates := dupIgnore;
  FGroupList.Sorted := True;
  FAutoTradesCommon := Default(TAutoTradesCommon);
  AutoTradesControllerPublisher.Subscribe(Self);
end;

destructor TframeAutoTrades.Destroy;
begin
  FreeAndNil(FNodeList);
  FreeAndNil(FGroupList);
  if Assigned(AutoTradesControllerPublisher) then
    AutoTradesControllerPublisher.Unsubscribe(Self);
  inherited;
end;

procedure TframeAutoTrades.Initialize;
begin
  inherited Initialize;
  Self.Caption := C_IDENTITY_NAME;
end;

procedure TframeAutoTrades.Deinitialize;
begin
  inherited;

end;

function TframeAutoTrades.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

function TframeAutoTrades.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TframeAutoTrades.vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PTradesData;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) and (Data^.NodeType = TNodeType.ntNode) then
  begin
    TargetCanvas.Brush.Color := Data^.State.ToColor;
    if (Column in [COL_BUTTON_START_STOP, COL_BUTTON_CANCEL, COL_BUTTON_RESTART]) then
    begin
      CellRect.Left   := CellRect.Left + 2;
      CellRect.Right  := CellRect.Right - 2;
      CellRect.Top    := CellRect.Top + 2;
      CellRect.Bottom := CellRect.Bottom - 2;
      TargetCanvas.Brush.Color := clBtnFace;
    end
    else if (Column = COL_SCAN_COUNT) and (Data^.ScanCount = 0) and (Data^.State in [tsSuspended, tsWorking])  then
    begin
      TargetCanvas.Brush.Color := clWebOrangeRed;
    end;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TframeAutoTrades.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PTradesData;
begin
  inherited;
  if (Sender.FocusedNode <> Node) then
  begin
    Data := Node^.GetData;
    if (Data^.NodeType = TNodeType.ntGroup) then
      TargetCanvas.Font.Style := [fsBold]
    else if (Column = COL_STATE) then
    begin
      case Data^.State of
        tsSuspended:
          TargetCanvas.Font.Color := clWebOrange;
        tsWorking:
          TargetCanvas.Font.Color := clGreen;
        tsCancelled:
          TargetCanvas.Font.Color := clRed;
        tsNotConsidered:
          TargetCanvas.Font.Color := clBlack;
      end;
      TargetCanvas.Font.Style := [fsBold];
    end;
  end;
end;

procedure TframeAutoTrades.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTradesData;
begin
  inherited;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if Assigned(Data) then
      Data^.Clear;
  end;
end;

procedure TframeAutoTrades.vstTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  Data: PTradesData;
begin
  inherited;
  Data := Node^.GetData;
  case Column of
    COL_COLUMNS:
      HintText := Data^.Columns;
    COL_INST_NUM:
      HintText := 'Autotrade>0'
  end;
end;

procedure TframeAutoTrades.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PTradesData;
begin
  inherited;
  if not Assigned(Node) then
    Exit;
  Data := Node^.GetData;

  if (Data.NodeType = TNodeType.ntGroup) then
  begin
    if (Column = COL_INST_NUM) then
      CellText := Data^.GroupText
    else
      CellText := '';
  end
  else
    case Column of
      COL_INST_NUM:
        if (Data^.InstanceNum > 0) then
          CellText := Data^.InstanceNum.ToString
        else
          CellText := Abs(Data^.InstanceNum).ToString + ' (Manual)';
      COL_AUTOTRADES_NAME:
        CellText := Data^.AutotradesName;
      COL_QUALIFIER:
        CellText := Data^.Qualifier;
      COL_MARKET_SCAN_ID:
        CellText := Data^.MarketScannerId;
      COL_AUTOTRADES_ID:
        CellText := Data^.AutoTradesId.ToString;
      COL_BUTTON_START_STOP:
        if (Data^.State = tsSuspended) then
          CellText := 'Start'
        else if (Data^.State = tsWorking) then
          CellText := 'Stop'
        else
          CellText := '';
      COL_BUTTON_RESTART:
        if (Data^.State = tsNotConsidered) then
          CellText := ''
        else
          CellText := 'Restart';
      COL_BUTTON_CANCEL:
        if (Data^.State = tsNotConsidered) then
          CellText := ''
        else
          CellText := 'Cancel';
      COL_STARTED_TIME:
        if Data^.StartedTime > 0 then
          CellText := TimeToStr(Data^.StartedTime)
        else
          CellText := '';
      COL_STOPPED_TIME:
        if Data^.StoppedTime > 0 then
          CellText := TimeToStr(Data^.StoppedTime)
        else
          CellText := '';
      COL_RESTART_TIME:
        if Data^.RestartTime > 0 then
          CellText := TimeToStr(Data^.RestartTime)
        else
          CellText := '';
      COL_ORDER_GROUP_ID:
        CellText := Data^.OrderGroupId.ToString;
      COL_STATE:
        CellText := Data^.State.ToString;
      COL_CURRENCY:
        CellText := Data^.Currency;
      COL_MAX_NUMBER_ORDER:
        CellText := Data^.MaxNumberOrder.ToString;
      COL_CURR_NUMBER_ORDER:
        CellText := Data^.CreatedOrdersCount.ToString;
      COL_MAX_ROWS:
        CellText := Data^.MaxRows.ToString;
      COL_ORDER_AMOUNT:
        CellText := Data^.OrderAmount.ToString;
      COL_ORDER_RANKING_SUM:
        CellText := Data^.OrderRankingSum.ToString;
      COL_LAST_UPDATE:
        if (Data^.LastUpdate > 0) then
          CellText := TimeToStr(Data^.LastUpdate)
        else
          CellText := '';
      COL_SCAN_COUNT:
        CellText := Data^.ScanCount.ToString;
      COL_INFO:
        CellText := Data^.Info;
      COL_COLUMNS:
        CellText := Data^.Columns;
      COL_QUALIFIER_ID:
        if (Data^.QualifierId > 0) then
          CellText := Data^.QualifierId.ToString
        else
          CellText := '';
      COL_QUALIFIER_INSTANCE:
        if (Data^.QualifierInstance > 0) then
          CellText := Data^.QualifierInstance.ToString
        else
          CellText := '';
    end;
end;

procedure TframeAutoTrades.DeleteOrdersByInstanceNum(aData: PTradesData);
resourcestring
  rsInfo = 'AutoTrade Instance "%d" stopped all orders are canceled and removed from the Monitor';
var
  Arr: TArray<PVirtualNode>;
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
  OrderData: PTreeData;
  Monitor: IMonitor;
begin
  if (aData^.InstanceNum <> 0) then
  begin
    Arr := TMonitorLists.OrderList.Keys.ToArray;
    for Node in Arr do
      if Assigned(Node) then
      begin
        OrderData := Node^.GetData;
        if Assigned(OrderData^.OrderDoc) and (OrderData^.OrderDoc.AutoTradesInstance = aData^.InstanceNum) then
          if (OrderData^.OrderDoc.OrderIBId > 0) and (OrderData^.OrderDoc.OrderStatus in [osPendSubmit, osSubmitted, osPreSubmit]) then
          begin
            OrderData^.OrderDoc.CancelOrder;
            TPublishers.LogPublisher.Write([ltLogWriter], ddWarning, Self, 'DeleteOrdersByInstanceNum', 'Cancel Order Id=' + OrderData^.OrderDoc.OrderIBId.ToString);
          end;
      end;

    if Supports(Application.MainForm, IMonitor, Monitor) then
    begin
      for Node in Arr do
        if Assigned(Node) then
        begin
          OrderData := Node^.GetData;
          if Assigned(OrderData.OrderDoc) and (OrderData^.OrderDoc.AutoTradesInstance = aData^.InstanceNum) then
          begin
            ParentNode := TTreeDocument.GetParentNode(Monitor.GetMainTree, Node, TDocType.ntOrderGroup);
            Monitor.DeleteNode(Node, False);
            Monitor.DeleteNode(ParentNode, False);
          end;
        end;
    end;
  end;
end;

procedure TframeAutoTrades.vstTreeNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  Data: PTradesData;
begin
  inherited;
  if (HitInfo.HitColumn in [COL_BUTTON_START_STOP, COL_BUTTON_CANCEL, COL_BUTTON_RESTART]) then
  begin
    Data := HitInfo.HitNode^.GetData;
    if Assigned(Data) and (Data^.State <> tsNotConsidered) and Assigned(Data^.AutoTradesInstance) then
    begin
      case HitInfo.HitColumn of
        COL_BUTTON_START_STOP:
          begin
            if (Data^.State = tsSuspended) then
              Data^.AutoTradesInstance.SetTradesState(tsExecuted)
            else if (Data^.State in [tsExecuted, tsWorking]) then
              Data^.AutoTradesInstance.SetTradesState(tsSuspended);
          end;
        COL_BUTTON_CANCEL:
          begin
            DeleteOrdersByInstanceNum(Data);
            Data^.AutoTradesInstance.CloseAutoTrade;
          end;
        COL_BUTTON_RESTART:
          Data^.AutoTradesInstance.SetTradesState(tsRestarted);
      end;
    end;
  end;
end;

procedure TframeAutoTrades.SetFilter(const aAutoTradesCommon: TAutoTradesCommon);
var
  Data: PTradesData;
begin
  FAutoTradesCommon := aAutoTradesCommon;
  for var Node in FNodeList.Values do
    if Assigned(Node) then
      if (fpFiltered in Self.Parameters) then
      begin
        Data := Node^.GetData;
        vstTree.IsVisible[Node] := (Data^.QualifierInstance > 0) and
                                   (Data^.QualifierId = FAutoTradesCommon.QualifierId) and
                                   (Data^.QualifierInstance = FAutoTradesCommon.QualifierInstance)
      end
      else
        vstTree.IsVisible[Node] := True;
end;

procedure TframeAutoTrades.SetFlatTree;
begin
  for var Node in FNodeList.Values do
    vstTree.MoveTo(Node, nil, amInsertBefore, False);

  for var i := 0 to FGroupList.Count - 1 do
    if Assigned(FGroupList.Objects[i]) then
      vstTree.DeleteNode(PVirtualNode(FGroupList.Objects[i]));
  FGroupList.Clear;
end;

procedure TframeAutoTrades.SetGroupOption(const Value: TAutoTradeGroupOption);
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
        if (GroupOption <> goNone) then
          for Node in FNodeList.Values do
            SetGroupTree(Node);
        vstTree.FullExpand(nil);
      finally
        vstTree.EndUpdate;
      end;
    end;
  end;
end;

procedure TframeAutoTrades.SetGroupTree(aNode: PVirtualNode);
var
  AttMode: TVTNodeAttachMode;
  Data: PTradesData;
  GroupData: PTradesData;
  GroupNode: PVirtualNode;
  GroupText: string;
  Index: Integer;
begin
  if Assigned(aNode) and (GroupOption <> goNone) then
  begin
    Data := aNode^.GetData;
    case GroupOption of
      goQualifier:
        GroupText := Data^.Qualifier;
      goName:
        GroupText := Data^.AutotradesName;
    end;

    Index := FGroupList.IndexOf(GroupText);
    if (Index < 0) then
    begin
      GroupNode := vstTree.AddChild(nil);
      GroupData := GroupNode^.GetData;
      GroupData^.GroupText := GroupText;
      GroupData^.NodeType := TNodeType.ntGroup;

      Index := FGroupList.Add(GroupText);
      FGroupList.Objects[Index] := TObject(GroupNode);
    end
    else
      GroupNode := PVirtualNode(FGroupList.Objects[Index]);

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

procedure TframeAutoTrades.SetInfo(const aInstanceNum: Integer; const aInfo: string);
var
  Data: PTradesData;
  Node: PVirtualNode;
begin
  if FNodeList.ContainsKey(aInstanceNum) then
  begin
    Node := FNodeList.Items[aInstanceNum];
    Data := Node^.GetData;
    Data^.Info := aInfo;
  end;
end;

procedure TframeAutoTrades.UpdateState(const aAutoTrade: IAutoTrade);
var
  Data: PTradesData;
  Node: PVirtualNode;
  AutoTradeInfo: TAutoTradeInfo;
  IsNeedRebuild: Boolean;
  TradesState: TTradesState;
begin
  if Application.Terminated then
    Exit;

  IsNeedRebuild := False;
  if Assigned(aAutoTrade) then
  begin
    AutoTradeInfo := aAutoTrade.GetAutoTradeInfo;
    TradesState   := aAutoTrade.GetTradesState;
    TThread.Queue(nil,
      procedure
      begin
        vstTree.BeginUpdate;
        try
          Node := nil;
          {if FNodeList.ContainsKey(AutoTradeInfo.InstanceNum) and (TradesState = tsNotConsidered) then
          begin
            Node := FNodeList.Items[AutoTradeInfo.InstanceNum];
            Data := Node^.GetData;
            Data^.State := tsNotConsidered;
            vstTree.InvalidateNode(Node);
            Exit;
          end
          else if FNodeList.ContainsKey(AutoTradeInfo.InstanceNum) then
            Node := FNodeList.Items[AutoTradeInfo.InstanceNum]
          else if not FNodeList.ContainsKey(AutoTradeInfo.InstanceNum) and (TradesState <> tsNotConsidered)  then
          begin
            Node := vstTree.AddChild(nil);
            Node.CheckType := ctCheckBox;
            FNodeList.Add(AutoTradeInfo.InstanceNum, Node);
            IsNeedRebuild := True;
          end; }

          if not Assigned(Node) then
            Exit;

          Data := Node^.GetData;
          Data^.AssignFrom(AutoTradeInfo);
          Data^.AutoTradesInstance := aAutoTrade;

          case TradesState of
            tsSuspended:
              begin
                Data^.RestartTime := 0;
                if (Data^.StoppedTime = 0) then
                  Data^.StoppedTime := Now;
              end;
            tsExecuted:
              begin
                Data^.StartedTime := Now;
                Data^.StoppedTime := 0;
              end;
            tsWorking:
              begin
                Data^.StoppedTime := 0;
                if (Data^.StartedTime = 0) then
                  Data^.StartedTime := Now;
              end;
            tsCancelled:
              begin
                if (Data^.StoppedTime = 0) then
                  Data^.StoppedTime := Now;
              end;
          end;
          Data^.State := TradesState;
          if IsNeedRebuild then
            SetGroupTree(Node);

          if (fpFiltered in Self.Parameters) then
            vstTree.IsVisible[Node] := (Data^.QualifierInstance > 0) and
                                       (Data^.QualifierId = FAutoTradesCommon.QualifierId) and
                                       (Data^.QualifierInstance = FAutoTradesCommon.QualifierInstance)
          else
            vstTree.IsVisible[Node] := True;

          vstTree.InvalidateNode(Node);
        finally
          vstTree.EndUpdate;
        end;
      end);
  end;
end;

procedure TframeAutoTrades.SetState(const aState: TTradesState);
resourcestring
  rsInfo = 'State changed manually to ';
var
  Data: PTradesData;
  Node: PVirtualNode;
begin
  Node := vstTree.FocusedNode;
  if Assigned(Node) then
    if ((Node.CheckType = ctCheckBox) and (Node.CheckState = csCheckedNormal)) or
        (Node.CheckType <> ctCheckBox) then
    begin
      Data := Node^.GetData;
      if Assigned(Data) then
        if (Data^.State in [tsWorking, tsSuspended]) and Assigned(Data^.AutoTradesInstance) then
        begin
          Data^.Info := rsInfo + aState.ToString;
          Data^.State := aState;
          Data^.AutoTradesInstance.SetTradesState(aState);
          TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'SetState', Data^.Info + sLineBreak + Data^.AutotradesName + ' (' + Data^.InstanceNum.ToString + ')');
        end;
    end;
end;

procedure TframeAutoTrades.Start;
begin
  SetState(tsWorking);
end;

procedure TframeAutoTrades.Stop;
begin
  SetState(tsSuspended);
end;

procedure TframeAutoTrades.Delete;
resourcestring
  rsQuestion = 'AutoTrade will be stopped,' + sLineBreak +
                'all orders will be canceled and removed from the Monitor.' + sLineBreak +
                'Continue?';
var
  Data: PTradesData;
begin
  if Assigned(vstTree.FocusedNode) then
    if (TMessageDialog.ShowQuestion(rsQuestion) = mrYes) then
    begin
      Data := vstTree.FocusedNode^.GetData;
      AutoTradesControllerPublisher.DeleteAutoTrade(Data^.InstanceNum, True);
    end;
end;

procedure TframeAutoTrades.DeleteAll;
resourcestring
  rsInfo = 'AutoTrade Instance "%d" stopped all orders are canceled and removed from the Monitor';
var
  Arr: TArray<PVirtualNode>;
  Data: PTradesData;
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
  OrderData: PTreeData;
  Monitor: IMonitor;
begin
  Arr := TMonitorLists.OrderList.Keys.ToArray;
  for Node in Arr do
    if Assigned(Node) then
    begin
      OrderData := Node^.GetData;
      if Assigned(OrderData^.OrderDoc) and (OrderData^.OrderDoc.AutoTradesId > 0) then
        if (OrderData^.OrderDoc.OrderIBId > 0) and (OrderData^.OrderDoc.OrderStatus in [osPendSubmit, osSubmitted, osPreSubmit]) then
        begin
          OrderData^.OrderDoc.CancelOrder;
          TPublishers.LogPublisher.Write([ltLogWriter], ddWarning, Self, 'DeleteAutoTrades', 'Cancel Order Id=' + OrderData^.OrderDoc.OrderIBId.ToString);
        end;
    end;

  if Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    for Node in Arr do
      if Assigned(Node) then
      begin
        OrderData := Node^.GetData;
        if Assigned(OrderData.OrderDoc) and (OrderData.OrderDoc.AutoTradesId > 0) then
        begin
          ParentNode := TTreeDocument.GetParentNode(Monitor.GetMainTree, Node, TDocType.ntOrderGroup);
          Monitor.DeleteNode(Node, False);
          Monitor.DeleteNode(ParentNode, False);
          TPublishers.MonitorStructureChangePublisher.OnMonitorStructureChange(nil, nil, crNodeMoved);
        end;
      end;
  end;

  vstTree.BeginUpdate;
  try
    for Node in FNodeList.Values do
      if Assigned(Node) then
      begin
        Data := Node^.GetData;
        if Assigned(Data) {and (Data^.State <> tsInterrupted)} then
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddWarning, Self, 'DeleteAutoTrades', Format(rsInfo, [Data^.InstanceNum]));
          if Assigned(Data^.AutoTradesInstance) then
            Data^.AutoTradesInstance.CloseAutoTrade(True);
        end;
      end;
    vstTree.Clear;
  finally
    vstTree.EndUpdate;
  end;
  FNodeList.Clear;
  FGroupList.Clear;
end;

procedure TframeAutoTrades.DeleteAutoTrade(const aInstanceNum: Integer; const aSilenceMode: Boolean = False);
resourcestring
  rsInfo = 'AutoTrade Instance "%d" stopped all orders are canceled and removed from the Monitor';
var
  Arr: TArray<PVirtualNode>;
  Data: PTradesData;
  GroupNode: PVirtualNode;
  Monitor: IMonitor;
  Node: PVirtualNode;
  OrderData: PTreeData;
begin
  if Application.Terminated then
    Exit;

  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'DeleteAutoTrade', Format(rsInfo, [aInstanceNum]));
  Arr := TMonitorLists.OrderList.Keys.ToArray;
  for Node in Arr do
    if Assigned(Node) then
    begin
      OrderData := Node^.GetData;
      if Assigned(OrderData.OrderDoc) and (OrderData^.OrderDoc.AutoTradesInstance = aInstanceNum) then
      begin
        OrderData^.OrderDoc.CancelOrder;
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'DeleteAutoTrade', 'Cancel Order Id=' + OrderData^.OrderDoc.OrderIBId.ToString);
      end;
    end;

  if Supports(Application.MainForm, IMonitor, Monitor) then
    for Node in Arr do
      if Assigned(Node) then
      begin
        OrderData := Node^.GetData;
        if Assigned(OrderData^.OrderDoc) and (OrderData^.OrderDoc.AutoTradesInstance = aInstanceNum) then
        begin
          GroupNode := TTreeDocument.GetParentNode(Monitor.GetMainTree, Node, TDocType.ntOrderGroup);
          Monitor.DeleteNode(Node, False);
          Monitor.DeleteNode(GroupNode, False);
        end;
      end;

  vstTree.BeginUpdate;
  try
    for Node in FNodeList.Values do
    begin
      Data := Node^.GetData;
      if Assigned(Data) and (Data^.InstanceNum = aInstanceNum) then
      begin
        if Assigned(Data^.AutoTradesInstance) then
          Data^.AutoTradesInstance.CloseAutoTrade(aSilenceMode);
        vstTree.DeleteNode(Node);
      end;
    end;
  finally
    vstTree.EndUpdate;
  end;
  FNodeList.Remove(aInstanceNum);
  GroupOption := GroupOption;
  TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'DeleteAutoTrade', '');
end;

end.

