unit AutoTrades.ComputeOrders;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, System.Math,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CustomDockForm, Vcl.Menus, System.Actions, Vcl.ActnList, VirtualTrees,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Common.Types, System.Generics.Collections, System.Generics.Defaults,
  DebugWriter, Global.Types, IABSocketAPI_const, IABSocketAPI, InstrumentList, Monitor.Types, Utils, Document,
  BrokerHelperAbstr, DaImages, HtmlLib, Account.Types, VirtualTrees.ExportHelper, IABFunctions, Entity.Sokid,
  Monitor.Interfaces, Publishers, IABFunctions.Helpers, MonitorTree.Helper, MonitorTree.Document;
{$ENDREGION}

type
  PСomputeData = ^TСomputeData;
  TСomputeData = record
    NodeType       : TNodeType;
    Name           : string;
    Id             : Integer;
    Broker         : TBrokerType;
    Action         : TIABAction;
    AutotradesName : string;
    AutoTradesId   : Integer;
    OrderType      : TIABOrderType;
    GroupText      : string;
    Currency       : string;
    TotalQuantity  : Integer;
    Sleeping       : Integer;
    PendSubmit     : Integer;
    Submitted      : Integer;
    Filled         : Integer;
    Cancelled      : Integer;
    Error          : Integer;
    FillPrice      : Double;
    TradeTime      : TDateTime;
    LastPrice      : Double;
    procedure Clear;
    procedure ClearNumbers;
  end;

  TfrmDockFormComputeOrders = class(TfrmCustomDockForm)
    aExitAllOrders: TAction;
    aExitAllOrdersIB: TAction;
    aExitAllOrdersNN: TAction;
    aExitAllOrdersX: TAction;
    aExitSelectedOrders: TAction;
    aRefresh: TAction;
    btnExitAllOrders: TBitBtn;
    btnExitAllOrdersIB: TBitBtn;
    btnExitAllOrdersNN: TBitBtn;
    btnExitAllOrdersX: TBitBtn;
    btnExitSelectedOrders: TBitBtn;
    btnRefresh: TBitBtn;
    cbGroup: TComboBox;
    gbDiscardActions: TGroupBox;
    gbSummary: TGroupBox;
    lblOptions: TLabel;
    lblUpdateTime: TLabel;
    pnlBottom: TPanel;
    rgChildOrdersActions: TRadioGroup;
    splTree: TSplitter;
    vstAccount: TVirtualStringTree;
    procedure aExitAllOrdersExecute(Sender: TObject);
    procedure aExitAllOrdersIBExecute(Sender: TObject);
    procedure aExitAllOrdersNNExecute(Sender: TObject);
    procedure aExitAllOrdersXExecute(Sender: TObject);
    procedure aExitSelectedOrdersExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure cbGroupChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstAccountCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstAccountDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstAccountFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstAccountGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private type
    TGroupOption = (goNone, goAction, goAutoTrades, goBroker, goOrderType, goInstrument);
  private const
    GroupOptionString: array [TGroupOption] of string = ('None', 'Action', 'AutoTrades', 'Broker', 'Order Type', 'Instrument');

    C_IDENTITY_NAME         = 'DockFormComputeOrders';
    C_IDENTITY_ACCOUNT_NAME = '.vstAccount';

    COL_NAME           = 0;
    COL_AUTOTRADES_ID  = 1;
    COL_ACTION         = 2;
    COL_CONTRACT_ID    = 3;
    COL_ORDER_TYPE     = 4;
    COL_CURRENCY       = 5;
    COL_TOTAL_QUANTITY = 6;
    COL_SLEEPING       = 7;
    COL_PENDSUBMIT     = 8;
    COL_SUBMITTED      = 9;
    COL_FILLED         = 10;
    COL_CANCELLED      = 11;
    COL_ERROR          = 12;
    COL_FILL_PRICE     = 13;
    COL_LAST_PRICE     = 14;

    COL_ACCOUNT_NAME   = 0;
    COL_ACCOUNT_VALUE  = 1;
    COL_ACCOUNT_INFO   = 2;
  private
    FNodeList           : TDictionary<string, PVirtualNode>;
    FGroupList          : TStringList;
    FGroupOption        : TGroupOption;
    FTotalSubmittedNode : PVirtualNode;
    procedure ExitAllOrders(aBroker: Integer = -1);
    procedure FillAccountList;
    procedure FillNodeList;
    procedure RecalcGroupValues;
    procedure SetFlatTree;
    procedure SetGroupOption(const Value: TGroupOption);
    procedure SetGroupTree(aNode: PVirtualNode);
    property GroupOption: TGroupOption read FGroupOption write SetGroupOption;
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure Deinitialize; override;
  end;

var
  frmDockFormComputeOrders: TfrmDockFormComputeOrders;

implementation

{$R *.dfm}

{ TfrmComputeOrders }

procedure TfrmDockFormComputeOrders.FormCreate(Sender: TObject);
begin
  vstTree.NodeDataSize    := SizeOf(TСomputeData);
  vstAccount.NodeDataSize := SizeOf(TAccount);
  inherited;
  FNodeList := TDictionary<string, PVirtualNode>.Create;
  FGroupList := TStringList.Create;
  FGroupList.Duplicates := dupIgnore;
  FGroupList.Sorted := True;
end;

procedure TfrmDockFormComputeOrders.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FNodeList);
  FreeAndNil(FGroupList);
  inherited;
end;

procedure TfrmDockFormComputeOrders.Initialize;
begin
  inherited Initialize;
  TMonitorTree.Initialize(vstAccount);
  Caption := 'Exit Orders';
  TStoreHelper.LoadFromXml(vstAccount, GetIdentityName + C_IDENTITY_ACCOUNT_NAME);

  cbGroup.Items.Clear;
  for var st in GroupOptionString do
    cbGroup.Items.Add(st);
  cbGroup.ItemIndex := 0;
  FGroupOption := goNone;
end;

procedure TfrmDockFormComputeOrders.Deinitialize;
begin
  inherited;
  TStoreHelper.SaveToXml(vstAccount, GetIdentityName + C_IDENTITY_ACCOUNT_NAME);
end;

function TfrmDockFormComputeOrders.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmDockFormComputeOrders.aRefreshExecute(Sender: TObject);
begin
  inherited;
  FillAccountList;
  FillNodeList;
end;

procedure TfrmDockFormComputeOrders.FillAccountList;
const
  arrAccountName: array [0..5] of string = ('BuyingPower', 'FullAvailableFunds', 'FullExcessLiquidity', 'NetLiquidation', 'TotalCashBalance', 'UnrealizedPnL');
var
  AccountItems: TArray<string>;
  Data: PAccount;
  Node: PVirtualNode;
begin
  vstAccount.BeginUpdate;
  try
    vstAccount.Clear;
    FTotalSubmittedNode := vstAccount.AddChild(nil);
    Data := FTotalSubmittedNode^.GetData;
    Data^.ParameterName := 'TotalSubmittedValue';
    Data^.Info := C_DEFAULT_CURRENCY;

    for var Name in arrAccountName do
      for var Index := 0 to IABClient.AccountValues.Count - 1 do
        if not IABClient.AccountValues[Index].Trim.IsEmpty then
        begin
          AccountItems := IABClient.AccountValues[Index].Split([' ']);
          if (AccountItems[0].Equals(Name)) then
          begin
            Node := vstAccount.AddChild(nil);
            Data := Node^.GetData;
            Data^.ParameterName := AccountItems[0];
            if (Length(AccountItems) >= 3) then
              Data^.Value := AccountItems[1];
            if (Length(AccountItems) >= 4) then
              Data^.Info := AccountItems[2];
          end;
        end;
  finally
    vstAccount.EndUpdate;
  end;
end;

procedure TfrmDockFormComputeOrders.FillNodeList;

  function GetCombineKey(aOrderDoc: TCustomOrderDoc): string; inline;
  begin
    Result := Concat(aOrderDoc.Id.ToString, aOrderDoc.OrderAction.ToString, aOrderDoc.OrderType.ToString);
  end;

  procedure SetTotalSubmittedValue(aValue: Double);
  var
    Data: PAccount;
  begin
    if not Assigned(FTotalSubmittedNode) then
      FTotalSubmittedNode := vstAccount.AddChild(nil);
    Data := FTotalSubmittedNode^.GetData;
    Data^.ParameterName := 'TotalSubmittedValue';
    Data^.Value := Trunc(aValue).ToString;
    Data^.Info  := C_DEFAULT_CURRENCY;
    vstAccount.InvalidateNode(FTotalSubmittedNode);
  end;

var
  Node: PVirtualNode;
  Data: PTreeData;
  ComputeData: PСomputeData;
  ComputeNode: PVirtualNode;
  OrderDoc: TCustomOrderDoc;
  TotalSubmittedValue: Double;
begin
  inherited;
  lblUpdateTime.Caption := 'Update Time: ' + FormatDateTime('hh:mm:ss', Now);
  vstTree.BeginUpdate;
  try
    TotalSubmittedValue := 0;
    FNodeList.Clear;
    FGroupList.Clear;
    vstTree.Clear;
    for Node in TMonitorLists.OrderList.Keys do
      if Assigned(Node) then
      begin
        Data := Node^.GetData;
        ComputeNode := nil;
        OrderDoc := Data^.OrderDoc;
        if Assigned(OrderDoc) then
        begin
          if FNodeList.ContainsKey(GetCombineKey(OrderDoc)) then
            ComputeNode := FNodeList.Items[GetCombineKey(OrderDoc)]
          else
          begin
            ComputeNode := vstTree.AddChild(nil);
            ComputeNode^.CheckType := ctCheckBox;
            FNodeList.Add(GetCombineKey(OrderDoc), ComputeNode);
          end;
        end;

        if Assigned(ComputeNode) then
        begin
          ComputeData := ComputeNode^.GetData;
          if (OrderDoc.BrokerType = TBrokerType.brIB) then
          begin
            if (TOrderIBDoc(OrderDoc).SecurityType in [stOption, stFuture]) then
              ComputeData^.Name := TOrderIBDoc(OrderDoc).LocalSymbol;
          end;

          ComputeData^.NodeType      := ntNode;
          ComputeData^.Name          := OrderDoc.InstrumentName;
          ComputeData^.Id            := OrderDoc.Id;
          ComputeData^.Action        := OrderDoc.OrderAction;
          ComputeData^.Broker        := OrderDoc.BrokerType;
          ComputeData^.OrderType     := OrderDoc.OrderType;
          ComputeData^.AutoTradesId  := OrderDoc.AutoTradesId;
          ComputeData^.Currency      := OrderDoc.Currency;
          ComputeData^.TotalQuantity := ComputeData^.TotalQuantity + OrderDoc.Quantity;
          ComputeData^.LastPrice     := TMonitorLists.PriceCache.GetLastPrice(OrderDoc.Id, ttLast);
          if (ComputeData^.TradeTime < OrderDoc.TradeTime) and (OrderDoc.AvgPrice > 0) then
          begin
            ComputeData^.TradeTime := OrderDoc.TradeTime;
            ComputeData^.FillPrice := OrderDoc.LatestFillPrice;
          end;

          case OrderDoc.OrderStatus of
            osFilled, osPartlyFilled:
              ComputeData^.Filled := ComputeData^.Filled + OrderDoc.Filled;
            osPendSubmit:
              ComputeData^.PendSubmit := ComputeData^.PendSubmit + OrderDoc.Quantity;
            osSleeping:
              ComputeData^.Sleeping := ComputeData^.Sleeping + OrderDoc.Quantity;
            osPreSubmit, osSubmitted:
              begin
                ComputeData^.Submitted := ComputeData^.Submitted + OrderDoc.Quantity;
                TotalSubmittedValue := TotalSubmittedValue + OrderDoc.Quantity * ComputeData^.LastPrice;
              end;
            osCancelled:
              ComputeData^.Cancelled := ComputeData^.Cancelled + OrderDoc.Quantity;
            osError:
              ComputeData^.Error := ComputeData^.Error + OrderDoc.Quantity;
            osPendCancel:
              ;
            osNotConsidered:
              ;
          end;
        end;
      end;
      SetTotalSubmittedValue(TotalSubmittedValue);
  finally
    SetGroupOption(GroupOption);
    vstTree.FullExpand;
    vstTree.EndUpdate;
  end;
end;

procedure TfrmDockFormComputeOrders.cbGroupChange(Sender: TObject);
begin
  inherited;
  if Showing then
    GroupOption := TGroupOption(cbGroup.ItemIndex);
end;

procedure TfrmDockFormComputeOrders.SetGroupOption(const Value: TGroupOption);
var
  Node: PVirtualNode;
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
    finally
      vstTree.FullExpand(nil);
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TfrmDockFormComputeOrders.SetGroupTree(aNode: PVirtualNode);
var
  Data: PСomputeData;
  AttMode: TVTNodeAttachMode;
  GroupData: PСomputeData;
  GroupNode: PVirtualNode;
  GroupText: string;
  Index: Integer;
begin
  if Assigned(aNode) and (GroupOption <> goNone) then
  begin
    Data := aNode^.GetData;
    case GroupOption of
      goAction:
        GroupText := Data^.Action.ToString;
      goAutoTrades:
        GroupText := 'AutoTrades: ' + Data^.AutoTradesId.ToString;
      goBroker:
        GroupText := 'Broker: ' + Data^.Broker.ToString;
      goOrderType:
        GroupText := Data^.OrderType.ToString;
      goInstrument:
        if SokidList.ContainsKey(Data^.Id) then
          GroupText := SokidList.Items[Data^.Id].Name
        else
          GroupText := Data^.Id.ToString;
    end;
    Index := FGroupList.IndexOf(GroupText);
    if (Index < 0) then
    begin
      GroupNode := vstTree.AddChild(nil);
      GroupData := GroupNode^.GetData;
      GroupData^.GroupText := GroupText;
      GroupData^.NodeType  := ntGroup;

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
    RecalcGroupValues;
  end;
end;

procedure TfrmDockFormComputeOrders.RecalcGroupValues;
var
  Index: Integer;
  GroupData: PСomputeData;
  GroupNode: PVirtualNode;
  ChildRun: PVirtualNode;
  ChildData: PСomputeData;
begin
  if (GroupOption <> goNone) then
  begin
    vstTree.BeginUpdate;
    try
      for Index := 0 to FGroupList.Count - 1 do
        if Assigned(FGroupList.Objects[Index]) then
        begin
          GroupNode := PVirtualNode(FGroupList.Objects[Index]);
          GroupData := GroupNode.GetData;
          GroupData^.ClearNumbers;
          ChildRun := GroupNode.FirstChild;
          while Assigned(ChildRun) do
          begin
            ChildData := ChildRun.GetData;
            if Assigned(ChildData) and (ChildData.NodeType = ntNode) then
            begin
              GroupData^.TotalQuantity := GroupData^.TotalQuantity + ChildData^.TotalQuantity;
              GroupData^.Filled        := GroupData^.Filled + ChildData^.Filled;
              GroupData^.PendSubmit    := GroupData^.PendSubmit + ChildData^.PendSubmit;
              GroupData^.Sleeping      := GroupData^.Sleeping + ChildData^.Sleeping;
              GroupData^.Submitted     := GroupData^.Submitted + ChildData^.Submitted;
              GroupData^.Cancelled     := GroupData^.Cancelled + ChildData^.Cancelled;
              GroupData^.Error         := GroupData^.Error + ChildData^.Error;
            end;
            ChildRun := ChildRun.NextSibling;
          end;
        end;
    finally
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TfrmDockFormComputeOrders.SetFlatTree;
var
  Node: PVirtualNode;
  i: Integer;
begin
  for Node in FNodeList.Values do
    vstTree.MoveTo(Node, nil, amInsertBefore, False);
  for i := 0 to FGroupList.Count - 1 do
    if Assigned(FGroupList.Objects[i]) then
      vstTree.DeleteNode(PVirtualNode(FGroupList.Objects[i]));
  FGroupList.Clear;
end;

procedure TfrmDockFormComputeOrders.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PСomputeData;
begin
  inherited;
  Result := 0;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  if (Data1^.NodeType = ntGroup) and (Data1^.NodeType = ntGroup) then
    Result := CompareText(Data1^.GroupText, Data2^.GroupText)
  else
    case Column of
      COL_NAME:
        Result := CompareText(Data1^.Name, Data2^.Name);
      COL_ACTION:
        Result := CompareText(Data1^.Action.ToString, Data2^.Action.ToString);
      COL_ORDER_TYPE:
        Result := CompareText(Data1^.OrderType.ToString, Data2^.OrderType.ToString);
      COL_AUTOTRADES_ID:
        Result := CompareValue(Data1^.AutoTradesId, Data2^.AutoTradesId);
      COL_CURRENCY:
        Result := CompareText(Data1^.Currency, Data2^.Currency);
      COL_CONTRACT_ID:
        Result := CompareValue(Data1^.Id, Data2^.Id);
      COL_TOTAL_QUANTITY:
        Result := CompareValue(Data1^.TotalQuantity, Data2^.TotalQuantity);
      COL_SLEEPING:
        Result := CompareValue(Data1^.Sleeping, Data2^.Sleeping);
      COL_PENDSUBMIT:
        Result := CompareValue(Data1^.PendSubmit, Data2^.PendSubmit);
      COL_SUBMITTED:
        Result := CompareValue(Data1^.Submitted, Data2^.Submitted);
      COL_FILLED:
        Result := CompareValue(Data1^.Filled, Data2^.Filled);
      COL_CANCELLED:
        Result := CompareValue(Data1^.Cancelled, Data2^.Cancelled);
      COL_ERROR:
        Result := CompareValue(Data1^.Error, Data2^.Error);
      COL_FILL_PRICE:
        Result := CompareValue(Data1^.FillPrice, Data2^.FillPrice);
      COL_LAST_PRICE:
        Result := CompareValue(Data1^.LastPrice, Data2^.LastPrice);
    end;
end;

procedure TfrmDockFormComputeOrders.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PСomputeData;
begin
  inherited;
  if (Sender.FocusedNode <> Node) then
  begin
    Data := Node^.GetData;

    if (Data^.NodeType = ntGroup) then
    begin
      TargetCanvas.Font.Style := [fsBold];
      if Data^.GroupText.Equals(iabBuy.ToString) then
        TargetCanvas.Font.Color := clNavy
      else if Data^.GroupText.Equals(iabSell.ToString) then
        TargetCanvas.Font.Color := clRed;
    end
    else
    begin
      case Column of
        COL_ACTION:
          if (Data^.Action = iabBuy) then
            TargetCanvas.Font.Color := clNavy
          else if (Data^.Action = iabSell) then
            TargetCanvas.Font.Color := clRed;
        COL_TOTAL_QUANTITY:
          if (Data^.TotalQuantity = 0) then
            TargetCanvas.Font.Color := clSilver;
        COL_SLEEPING:
          if (Data^.Sleeping = 0) then
            TargetCanvas.Font.Color := clSilver;
        COL_PENDSUBMIT:
          if (Data^.PendSubmit = 0) then
            TargetCanvas.Font.Color := clSilver;
        COL_SUBMITTED:
          if (Data^.Submitted = 0) then
            TargetCanvas.Font.Color := clSilver;
        COL_FILLED:
          if (Data^.Filled = 0) then
            TargetCanvas.Font.Color := clSilver;
        COL_CANCELLED:
          if (Data^.Cancelled = 0) then
            TargetCanvas.Font.Color := clSilver;
        COL_ERROR:
          if (Data^.Error = 0) then
            TargetCanvas.Font.Color := clSilver;
        COL_FILL_PRICE:
          if (Data^.FillPrice = 0) then
            TargetCanvas.Font.Color := clSilver;
        COL_LAST_PRICE:
          if (Data^.LastPrice = 0) then
            TargetCanvas.Font.Color := clSilver;
      end;
    end;
  end;
end;

procedure TfrmDockFormComputeOrders.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PСomputeData;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmDockFormComputeOrders.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PСomputeData;
begin
  inherited;
  if not Assigned(Node) then
    Exit;
  Data := Node^.GetData;

  if (Data^.NodeType = ntGroup) then
    case Column of
      COL_NAME:
        CellText := Data^.GroupText;
      COL_TOTAL_QUANTITY:
        CellText := Data^.TotalQuantity.ToString;
      COL_SLEEPING:
        CellText := Data^.Sleeping.ToString;
      COL_PENDSUBMIT:
        CellText := Data^.PendSubmit.ToString;
      COL_SUBMITTED:
        CellText := Data^.Submitted.ToString;
      COL_FILLED:
        CellText := Data^.Filled.ToString;
      COL_CANCELLED:
        CellText := Data^.Cancelled.ToString;
      COL_ERROR:
        CellText := Data^.Error.ToString;
    else
      CellText := '';
    end
  else if (Data^.NodeType = ntNode) then
    case Column of
      COL_NAME:
        CellText := Data^.Name;
      COL_AUTOTRADES_ID:
        CellText := Data^.AutoTradesId.ToString;
      COL_CURRENCY:
        CellText := Data^.Currency;
      COL_CONTRACT_ID:
        CellText := Data^.Id.ToString;
      COL_ACTION:
        CellText := Data^.Action.ToString;
      COL_ORDER_TYPE:
        CellText := Data^.OrderType.ToString;
      COL_TOTAL_QUANTITY:
        CellText := Data^.TotalQuantity.ToString;
      COL_SLEEPING:
        CellText := Data^.Sleeping.ToString;
      COL_PENDSUBMIT:
        CellText := Data^.PendSubmit.ToString;
      COL_SUBMITTED:
        CellText := Data^.Submitted.ToString;
      COL_FILLED:
        CellText := Data^.Filled.ToString;
      COL_CANCELLED:
        CellText := Data^.Cancelled.ToString;
      COL_ERROR:
        CellText := Data^.Error.ToString;
      COL_FILL_PRICE:
        CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.FillPrice);
      COL_LAST_PRICE:
        CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.LastPrice);
    else
      CellText := '';
    end;
end;

procedure TfrmDockFormComputeOrders.vstAccountCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PAccount;
begin
  inherited;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  case Column of
    COL_ACCOUNT_NAME:
      Result := CompareText(Data1^.ParameterName, Data2^.ParameterName);
    COL_ACCOUNT_VALUE:
      Result := CompareText(Data1^.Value, Data2^.Value);
    COL_ACCOUNT_INFO:
      Result := CompareText(Data1^.Info, Data2^.Info);
  else
    Result := 0;
  end;
end;

procedure TfrmDockFormComputeOrders.vstAccountDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PAccount;
begin
  inherited;
  if (Sender.FocusedNode <> Node) then
  begin
    Data := Node^.GetData;
    if Data^.ParameterName = 'TotalSubmittedValue' then
      TargetCanvas.Font.Style := [fsBold];

    case Column of
      COL_ACCOUNT_VALUE:
        if (StrToFloatDef(Data.Value, 0) < 0) then
          TargetCanvas.Font.Color := clRed
        else
          TargetCanvas.Font.Color := clBlack;
    end;
  end;
end;

procedure TfrmDockFormComputeOrders.vstAccountFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PAccount;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmDockFormComputeOrders.vstAccountGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PAccount;
  FloatValue: Double;
begin
  inherited;
  Data := Node^.GetData;
  case Column of
    COL_ACCOUNT_NAME:
      CellText := Data^.ParameterName;
    COL_ACCOUNT_VALUE:
    begin
      if IsFloat(Data^.Value) then
      begin
        FloatValue := StrToFloatEx(Data^.Value);
        CellText := FormatFloat(',0', Trunc(FloatValue))
      end
      else
        CellText := Data^.Value;
    end;
    COL_ACCOUNT_INFO:
      CellText := Data^.Info;
  else
    CellText := '';
  end;
end;

procedure TfrmDockFormComputeOrders.ExitAllOrders(aBroker: Integer = -1);
var
  Arr: TArray<PVirtualNode>;
  Node: PVirtualNode;
  OrderData: PTreeData;
begin
  Arr := TMonitorLists.OrderList.Keys.ToArray;
  for Node in Arr do
    if Assigned(Node) then
    begin
      OrderData := Node^.GetData;
      if Assigned(OrderData^.OrderDoc) then
        if (aBroker = -1) or (OrderData^.OrderDoc.BrokerType = TBrokerType.brIB) then
        begin
          OrderData^.OrderDoc.CancelOrder;
          TPublishers.LogPublisher.Write([ltLogWriter], ddWarning, Self, 'ExitAllOrders', 'Cancel Order Id=' + OrderData^.OrderDoc.OrderIBId.ToString);
        end;
    end;
  FillNodeList;
end;

procedure TfrmDockFormComputeOrders.aExitAllOrdersExecute(Sender: TObject);
begin
  inherited;
  ExitAllOrders;
end;

procedure TfrmDockFormComputeOrders.aExitAllOrdersIBExecute(Sender: TObject);
begin
  inherited;
  ExitAllOrders(Ord(TBrokerType.brIB));
  IABClient.RequestGlobalCancel;
end;

procedure TfrmDockFormComputeOrders.aExitAllOrdersNNExecute(Sender: TObject);
begin
  inherited;
  ExitAllOrders(Ord(TBrokerType.brNN));
end;

procedure TfrmDockFormComputeOrders.aExitAllOrdersXExecute(Sender: TObject);
var
  Arr: TArray<PVirtualNode>;
  Node: PVirtualNode;
  OrderData: PTreeData;
begin
  inherited;
  Arr := TMonitorLists.OrderList.Keys.ToArray;
  for Node in Arr do
    if Assigned(Node) then
    begin
      OrderData := Node^.GetData;
      if Assigned(OrderData^.OrderDoc) then
        if (OrderData^.OrderDoc.OrderIBId <= 0) and (OrderData^.OrderDoc.OrderStatus in [osSleeping]) then
        begin
          OrderData^.OrderDoc.CancelOrder;
          TPublishers.LogPublisher.Write([ltLogWriter], ddWarning, Self, 'ExitAllOrdersX', 'Cancel Order Id=' + OrderData^.OrderDoc.Id.ToString);
        end;
    end;
  FillNodeList;
end;

procedure TfrmDockFormComputeOrders.aExitSelectedOrdersExecute(Sender: TObject);
var
  ArrMonitor: TArray<PVirtualNode>;
  ArrСompute: TNodeArray;
  OrderData: PTreeData;
  OrderNode: PVirtualNode;
  СomputeData: PСomputeData;
  СomputeNode: PVirtualNode;
  Monitor: IMonitor;
begin
  if Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    ArrСompute := TTreeDocument.GetNodesList(vstTree.RootNode);
    ArrMonitor := TMonitorLists.OrderList.Keys.ToArray;
    for СomputeNode in ArrСompute do
      if (СomputeNode <> vstTree.RootNode) and
         (СomputeNode.CheckState = csCheckedNormal) then
      begin
        СomputeData := СomputeNode^.GetData;
        if (СomputeData^.NodeType = ntNode) then
          for OrderNode in ArrMonitor do
            if Assigned(OrderNode) and (Monitor.GetMainTree.RootNode <> OrderNode) then
            begin
              OrderData := OrderNode^.GetData;
              if Assigned(OrderData^.OrderDoc) then
                if (OrderData^.OrderDoc.Id = СomputeData.Id) and
                  (OrderData^.OrderDoc.OrderType = СomputeData.OrderType) and
                  (OrderData^.OrderDoc.OrderAction = СomputeData.Action) then
                  begin
                    OrderData^.OrderDoc.CancelOrder;
                    TPublishers.LogPublisher.Write([ltLogWriter], ddWarning, Self, 'ExitSelectedOrders', 'Cancel Order Id=' + OrderData^.OrderDoc.Id.ToString);
                  end;
            end;
      end;
    FillNodeList;
  end;
end;

{ TСomputeData }

procedure TСomputeData.Clear;
begin
  Self := Default(TСomputeData);
end;

procedure TСomputeData.ClearNumbers;
begin
  TotalQuantity := 0;
  Sleeping      := 0;
  PendSubmit    := 0;
  Submitted     := 0;
  Filled        := 0;
  Cancelled     := 0;
  Error         := 0;
end;

end.
