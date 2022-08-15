unit Frame.ActiveOrders;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ExtCtrls, System.Generics.Collections, System.UITypes, DebugWriter,
  Global.Types, Document, InstrumentList, Winapi.ActiveX, System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.Buttons, System.Generics.Defaults, Vcl.Menus,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} BrokerHelperAbstr, Monitor.Types, IABSocketAPI, IABFunctions,
  NNfunctions, NNfunctions.Types, BrokerHelperNN, Edit.OrderIB, Edit.OrderNN, IABSocketAPI_const, MessageDialog,
  Common.Types, IABFunctions.RequestsQueue, Monitor.Interfaces, DaImages, Publishers.Interfaces, System.Math,
  Publishers, Frame.Custom, IABFunctions.Helpers;
{$ENDREGION}

type
  TActiveOrdersGroupOption = (goNone, goBroker, goInstrument, goOrderType);
  POrderData = ^TOrderData;
  TOrderData = record
    NodeType          : TNodeType;
    GroupText         : string;
    Node              : PVirtualNode;
    Id                : Integer;
    Action            : string;
    BrokerType        : TBrokerType;
    Broker            : string;
    Instrument        : string;
    OrderType         : string;
    Quantity          : Double;
    Filled            : Double;
    Remaining         : Double;
    Price             : Double;
    FillPrice         : Double;
    LimitPrice        : Double;
    Exchange          : string;
    OCAGroup          : string;
    TimeInForce       : string;
    ParentId          : Integer;
    Info              : string;
    QualifierID       : Integer;
    QualifierInstance : Integer;
    AutoTradesID      : Integer;
    AutoTradeInstance : Integer;
    procedure Clear;
  end;

  TframeActiveOrders = class(TframeCustom, IOrderState)
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
  private const
    COL_ID                 = 0;
    COL_ACTION             = 1;
    COL_BROKER             = 2;
    COL_ORDERTYPE          = 3;
    COL_INSTRUMENT         = 4;
    COL_QUANTITY           = 5;
    COL_REMAINING          = 6;
    COL_FILLED             = 7;
    COL_PRICE              = 8;
    COL_FILLPRICE          = 9;
    COL_LIMIT_PRICE        = 10;
    COL_EXCHANGE           = 11;
    COL_OCA                = 12;
    COL_TIMEINFORCE        = 13;
    COL_PARENTID           = 14;
    COL_INFO               = 15;
    COL_QUALIFIER_ID       = 16;
    COL_QUALIFIER_INSTANCE = 17;
    COL_AUTOTRADE_ID       = 18;
    COL_AUTOTRADE_INSTANCE = 19;

    C_IDENTITY_NAME = 'frameActiveOrders';
  private
    { Private declarations }
    FGroupList: TStringList;
    FGroupOption: TActiveOrdersGroupOption;
    FOrderList: TDictionary<Integer, POrderData>;
    FAutoTradesCommon: TAutoTradesCommon;
    procedure RecalcGroupValues;
    procedure SetFlatTree;
    procedure SetGroupOption(const Value: TActiveOrdersGroupOption);
    procedure SetGroupTree(aData: POrderData);

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IOrderState
    procedure OnCloseOrder(const aTempId: Integer);
    procedure OnExecution(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrder(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrderNN(const aOrderList: array of TOrder);
    procedure OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
    procedure OnRebuildFromTWS(Sender: TObject);
  protected
    function GetIdentityName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Deinitialize; override;

    procedure SetFilter(const aAutoTradesCommon: TAutoTradesCommon); override;
    procedure CancelOrder(aNode: PVirtualNode = nil);
    procedure CancelAllOrders;
    procedure ShowOrder;
    property GroupOption: TActiveOrdersGroupOption read FGroupOption write SetGroupOption;
  end;

 const
   ActiveOrdersGroupOptionName: array [TActiveOrdersGroupOption] of string = ('None', 'Broker', 'Instrument', 'Order type');

implementation

{$R *.dfm}

{ TframeActiveOrders }

constructor TframeActiveOrders.Create(AOwner: TComponent);
begin
  inherited;
  vstTree.NodeDataSize := SizeOf(TOrderData);
  FOrderList := TDictionary<Integer, POrderData>.Create;
  FGroupOption := goNone;

  FGroupList := TStringList.Create;
  FGroupList.Duplicates := dupIgnore;
  FGroupList.Sorted := True;
  FAutoTradesCommon := Default(TAutoTradesCommon);
end;

destructor TframeActiveOrders.Destroy;
begin
  if Assigned(TPublishers.OrderStatePublisher) then
    TPublishers.OrderStatePublisher.Unsubscribe(Self);
  FreeAndNil(FOrderList);
  FreeAndNil(FGroupList);
  inherited;
end;

function TframeActiveOrders.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

function TframeActiveOrders.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TframeActiveOrders.Initialize;
var
  OrdersList: TArray<TOrder>;
begin
  inherited Initialize;
  Self.Caption := C_IDENTITY_NAME;
  if Assigned(NordNetBroker) and NordNetBroker.Active then
  begin
    OrdersList := NordNetBroker.GetOrders(NordNetBroker.AccountNum, False);
    if Length(OrdersList) > 0 then
      OnOpenOrderNN(OrdersList);
  end;
  TPublishers.OrderStatePublisher.Subscribe(Self);
end;

procedure TframeActiveOrders.Deinitialize;
begin
  inherited;

end;

procedure TframeActiveOrders.CancelAllOrders;
begin
  vstTree.BeginUpdate;
  try
    for var Data in FOrderList.Values do
      if Assigned(Data^.Node) then
        CancelOrder(Data^.Node);
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TframeActiveOrders.CancelOrder(aNode: PVirtualNode = nil);
var
  Data: POrderData;
  DataOrder: PTreeData;
  NodeOrder: PVirtualNode;
  OrderReply: TOrderReply;
  sInfo: string;
begin
  if not Assigned(aNode) then
    aNode := vstTree.FocusedNode;

  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data) and (Data.Id > 0) then
    begin
      if (Data.BrokerType = TBrokerType.brIB) then
      begin
        try
          NodeOrder := TMonitorLists.OrderList.GetNodeOrder(Data.Id);
          if Assigned(NodeOrder) then
          begin
            DataOrder := NodeOrder^.GetData;
            if Assigned(DataOrder) and Assigned(DataOrder.OrderDoc) then
              DataOrder.OrderDoc.CancelOrder;
          end
          else
            IABClient.SendRequest(ibCancelOrder, Data.Id);
        except
          on E: Exception do
          begin
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'aCancelOrderExecute', E.Message);
            TMessageDialog.ShowError('The order can not be canceled. ', E.Message);
          end;
        end;
      end
      else if (Data.BrokerType = TBrokerType.brNN) then
      begin
        try
          OrderReply := NordNetBroker.DeleteOrder(NordNetBroker.AccountNum, Data.Id);
          sInfo := NordNetBroker.OrderReplyToStr(OrderReply);
          if (OrderReply.result_code.ToUpper = 'OK') then
            OnCloseOrder(Data.Id)
          else
            TMessageDialog.ShowError('The order can not be canceled.', sInfo);
        except
          on E: Exception do
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'aCancelOrderExecute', E.Message);
        end;
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'aCancelOrderExecute', sInfo);
      end;
    end;
  end
end;

procedure TframeActiveOrders.OnCloseOrder(const aTempId: integer);
var
  Data: POrderData;
begin
  if (aTempId <> 0) and (FOrderList.ContainsKey(aTempId)) then
  begin
    Data := FOrderList.Items[aTempId];
    if Assigned(Data) and Assigned(Data.Node) then
    begin
      TThread.Queue(nil,
        procedure
        begin
          vstTree.BeginUpdate;
          try
            vstTree.DeleteNode(Data.Node);
          finally
            vstTree.EndUpdate;
            FOrderList.Remove(aTempId);
          end;
          RecalcGroupValues;
        end);
    end;
    vstTree.SortTree(vstTree.Header.SortColumn, vstTree.Header.SortDirection);
  end;
end;

procedure TframeActiveOrders.OnRebuildFromTWS(Sender: TObject);
begin
  //nothing
end;

procedure TframeActiveOrders.OnExecution(Sender: TObject; Order: TIABOrder);
var
  Status: TIABOrderState;
begin
  if (Order.TempId > 0) then
  begin
    if Order.Completed then
      Status := osFilled
    else
      Status := osPartlyFilled;
    OnOrderStatus(Sender, Order, Status);
  end;
end;

procedure TframeActiveOrders.OnOpenOrder(Sender: TObject; Order: TIABOrder);
begin
  OnOrderStatus(Sender, Order, osSubmitted);
end;

procedure TframeActiveOrders.OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
var
  Data : POrderData;
  TreeData: PTreeData;
  Node : PVirtualNode;
  TreeNode : PVirtualNode;
  NeedRebuild: Boolean;
begin
  if (Order.TempId <> 0) then
  begin
    if (Status in [osCancelled, osFilled, osError]) then
      OnCloseOrder(Order.TempId)
    else
      TThread.Queue(nil,
        procedure()
        begin
          if (Order.TempId <> 0) then
          begin
            Node := nil;
            NeedRebuild := False;
            if FOrderList.ContainsKey(Order.TempId) then
              Node := FOrderList[Order.TempId].Node;
            if not Assigned(Node) then
            begin
              Node := vstTree.AddChild(nil);
              NeedRebuild := True;
              TreeNode := TMonitorLists.OrderList.GetNodeOrder(Order.TempId);
              if Assigned(TreeNode) then
              begin
                TreeData := TreeNode^.GetData;
                if Assigned(TreeData^.OrderDoc) then
                begin
                  Data := Node^.GetData;
                  Data^.AutoTradesID      := TreeData^.OrderDoc.AutoTradesID;
                  Data^.AutoTradeInstance := TreeData^.OrderDoc.AutoTradesInstance;
                  Data^.QualifierID       := TreeData^.OrderDoc.QualifierID;
                  Data^.QualifierInstance := TreeData^.OrderDoc.QualifierInstance;
                end
                else
                begin
                  Data^.AutoTradesID      := 0;
                  Data^.AutoTradeInstance := 0;
                  Data^.QualifierID       := 0;
                  Data^.QualifierInstance := 0;
                end;
              end;
            end;

            Data := Node^.GetData;
            Data^.Node         := Node;
            Data^.Id           := Order.TempId;
            Data^.Action       := Order.Action.ToString;
            Data^.BrokerType   := TBrokerType.brIB;
            Data^.Broker       := Data^.BrokerType.ToString;
            Data^.TimeInForce  := Order.TimeInForce.ToString;
            Data^.OCAGroup     := Order.OCAGroup;
            Data^.OrderType    := Order.OrderType.ToString;
            Data^.ParentId     := Order.ParentId;
            Data^.NodeType     := ntNode;
            Data^.Instrument   := Order.Symbol;

            if (Order.FilledQuantity > 0) and (Abs(UNSET_DOUBLE - Order.FilledQuantity) > 0.001) then
              Data^.Filled := Order.FilledQuantity;
            Data^.FillPrice := Order.FillPrice;

            if not Order.Exchange.IsEmpty then
              Data^.Exchange := Order.Exchange;
            if Data^.Instrument.IsEmpty and not Order.LocalSymbol.IsEmpty then
              Data^.Instrument := Order.LocalSymbol;

            if (Order.OrderType in [otTrailLimit, otTrail]) then
            begin
              if (Order.TrailStopPrice > 0) and (Abs(UNSET_DOUBLE - Order.TrailStopPrice) > 0.001) then
                Data^.Price := Order.TrailStopPrice;
            end
            else if (Order.OrderType in [otLimit, otLimitClose, otLimitOpen, otLimitTouch]) then
            begin
              if (Order.Price > 0) and (Abs(UNSET_DOUBLE - Order.Price) > 0.001) then
                Data^.LimitPrice := Order.Price;
              if (Order.AuxPrice > 0) and (Abs(UNSET_DOUBLE - Order.AuxPrice) > 0.001) then
                Data^.LimitPrice := Order.AuxPrice;
            end
            else
            begin
              if (Order.Price > 0) and (Abs(UNSET_DOUBLE - Order.Price) > 0.001) then
                Data^.Price := Order.Price;
              if (Order.AuxPrice > 0) and (Abs(UNSET_DOUBLE - Order.AuxPrice) > 0.001) then
                Data^.Price := Order.AuxPrice;
            end;

            if (Order.Quantity > 0) then
              Data^.Quantity := Order.Quantity;
            Data^.Remaining := Order.Quantity - Order.Remaining;

            if (not FOrderList.ContainsKey(Order.TempId)) then
              FOrderList.Add(Order.TempId, Data);

            if (fpFiltered in Self.Parameters) then
              vstTree.IsVisible[Data^.Node] := (Data^.AutoTradeInstance > 0) and
                                               (Data^.QualifierID = FAutoTradesCommon.QualifierID) and
                                               (Data^.QualifierInstance = FAutoTradesCommon.QualifierInstance) and
                                               (Data^.AutoTradesID = FAutoTradesCommon.AutoTradesID) and
                                               (Data^.AutoTradeInstance = FAutoTradesCommon.AutoTradesInstance)
            else
              vstTree.IsVisible[Data^.Node] := True;

            if NeedRebuild then
              SetGroupTree(Data);
            RecalcGroupValues;
          end;
          vstTree.SortTree(vstTree.Header.SortColumn, vstTree.Header.SortDirection);
        end);
  end;
end;

procedure TframeActiveOrders.OnOpenOrderNN(const aOrderList: array of TOrder);
var
  i: Integer;
  j: Integer;
  Data: POrderData;
  Node: PVirtualNode;
  DataOrder: PTreeData;
  NodeOrder: PVirtualNode;
  Order: TOrder;
  Instruments: TInstruments;
  NeedRebuild: Boolean;
begin
  for i := Low(aOrderList) to High(aOrderList) do
  begin
    Order := aOrderList[i];
    if FOrderList.ContainsKey(Order.order_id) and (Order.order_state.StartsWith('DELETED')) then
      OnCloseOrder(Order.order_id)
    else if (Order.order_id > 0) and (not Order.order_state.StartsWith('DELETED')) then
    begin
      Node := nil;
      NeedRebuild := False;
      if FOrderList.ContainsKey(Order.order_id) then
        Node := FOrderList[Order.order_id].Node;
      if not Assigned(Node) then
      begin
        Node := vstTree.AddChild(nil);
        NeedRebuild := True;
      end;

      Data := Node^.GetData;
      Data^.Node := Node;
      // Data^.Exchange := Order.Exchange;
      // Data^.Instrument := Order.LocalSymbol;
      // Data^.ParentId  := Order.ParentId;
      Data^.BrokerType := TBrokerType.brNN;
      Data^.Broker     := Data^.BrokerType.ToString;
      Data^.Id         := Order.order_id;
      Data^.OCAGroup   := '';
      Data^.NodeType   := ntNode;
      if not Order.side.IsEmpty then
        Data^.Action := Order.side;
      if not Order.validity.type_.IsEmpty then
        Data^.TimeInForce := Order.validity.type_;

      if (Order.order_type = 'LIMIT') then
      begin
        if (Order.Price.Value >= 1) then
          Data^.LimitPrice := Order.Price.Value;
      end
      else
      begin
        if (Order.Price.Value >= 1) then
          Data^.Price := Order.Price.Value;
      end;

      if (Order.volume >= 1) then
      begin
        Data^.Quantity := Order.volume;
        Data^.Remaining := Order.volume - Order.traded_volume;
      end;
      if (Order.traded_volume >= 1) then
      begin
        Data^.Filled := Order.traded_volume;
        Data^.FillPrice := Order.price.value;
      end;
      if not Order.order_type.IsEmpty then
        Data^.OrderType := Order.order_type
      else
      begin
        NodeOrder := TMonitorLists.OrderList.GetNodeOrder(Order.order_id);
        if Assigned(NodeOrder) then
        begin
          DataOrder := NodeOrder^.GetData;
          if Assigned(DataOrder) and Assigned(DataOrder.OrderDoc) and (DataOrder.OrderDoc is TOrderNNDoc) then
          begin
            case TOrderNNDoc(DataOrder.OrderDoc).OrderType of
              otMarket, otComboMarket:
                Data^.OrderType := 'NORMAL';
              otLimit, otComboLimit:
                Data^.OrderType := 'LIMIT';
            end;
          end;
          Data^.Instrument := DataOrder.OrderDoc.Symbol;
        end;
      end;

      if Data^.Instrument.IsEmpty then
      begin
        Instruments := NordNetBroker.GetInstrumentsLookup(Order.tradable.market_id.ToString + ':' + Order.tradable.identifier, TLookupType.ltMarketIdIdentifier);
        for j := 0 to Length(Instruments) - 1 do
          if not Instruments[j].symbol.IsEmpty then
          begin
            Data^.Instrument := Instruments[j].symbol;
            Break;
          end;
      end;

      Data^.Info := 'Tradable: ' + Order.tradable.identifier +
                    ', Order type: ' + Order.order_type +
                    ', Order state: ' + Order.order_state +
                    ', Open volume: ' + Order.volume.ToString +
                    ', Traded volume: ' + Order.traded_volume.ToString;
      TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'OnOpenOrderNN', TNordNet.OrderToStr(Order));
      if not FOrderList.ContainsKey(Order.order_id) then
        FOrderList.Add(Order.order_id, Data);

      if (fpFiltered in Self.Parameters) then
        vstTree.IsVisible[Data^.Node] := (Data^.AutoTradeInstance > 0) and
                                         (Data^.QualifierID = FAutoTradesCommon.QualifierID) and
                                         (Data^.QualifierInstance = FAutoTradesCommon.QualifierInstance) and
                                         (Data^.AutoTradesID = FAutoTradesCommon.AutoTradesID) and
                                         (Data^.AutoTradeInstance = FAutoTradesCommon.AutoTradesInstance)
      else
        vstTree.IsVisible[Data^.Node] := True;

      if NeedRebuild then
        SetGroupTree(Data);
    end;
    RecalcGroupValues;
  end;
  vstTree.SortTree(vstTree.Header.SortColumn, vstTree.Header.SortDirection);
end;

procedure TframeActiveOrders.vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: POrderData;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data^.Node) and (Data^.Id > 0) then
  begin
    TargetCanvas.Brush.Color := clWebMintcream;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TframeActiveOrders.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: POrderData;
begin
  inherited;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  case Column of
    COL_ID:
      Result := CompareValue(Data1^.Id, Data2^.Id);
    COL_ACTION:
      Result := CompareText(Data1^.Action, Data2^.Action);
    COL_BROKER:
      Result := CompareText(Data1^.Broker, Data2^.Broker);
    COL_ORDERTYPE:
      Result := CompareText(Data1^.OrderType, Data2^.OrderType);
    COL_INSTRUMENT:
      Result := CompareText(Data1^.Instrument, Data2^.Instrument);
    COL_QUANTITY:
      Result := CompareValue(Data1^.Quantity, Data2^.Quantity);
    COL_REMAINING:
      Result := CompareValue(Data1^.Remaining, Data2^.Remaining);
    COL_FILLPRICE:
      Result := CompareValue(Data1^.FillPrice, Data2^.FillPrice);
    COL_FILLED:
      Result := CompareValue(Data1^.Filled, Data2^.Filled);
    COL_PRICE:
      Result := CompareValue(Data1^.Price, Data2^.Price);
    COL_LIMIT_PRICE:
      Result := CompareValue(Data1^.LimitPrice, Data2^.LimitPrice);
    COL_EXCHANGE:
      Result := CompareText(Data1^.Exchange, Data2^.Exchange);
    COL_OCA:
      Result := CompareText(Data1^.OCAGroup, Data2^.OCAGroup);
    COL_TIMEINFORCE:
      Result := CompareText(Data1^.TimeInForce, Data2^.TimeInForce);
    COL_INFO:
      Result := CompareText(Data1^.Info, Data2^.Info);
    COL_PARENTID:
      Result := CompareValue(Data1^.ParentId, Data2^.ParentId);
    COL_QUALIFIER_ID:
      Result := CompareValue(Data1^.QualifierID, Data2^.QualifierID);
    COL_AUTOTRADE_ID:
      Result := CompareValue(Data1^.AutoTradesID, Data2^.AutoTradesID);
    COL_AUTOTRADE_INSTANCE:
      Result := CompareValue(Data1^.AutoTradeInstance, Data2^.AutoTradeInstance);
    COL_QUALIFIER_INSTANCE:
      Result := CompareValue(Data1^.QualifierInstance, Data2^.QualifierInstance);
  end;
end;

procedure TframeActiveOrders.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: POrderData;
begin
  inherited;
  Data := Node^.GetData;
  if (Data^.NodeType = ntGroup) then
    TargetCanvas.Font.Style := [fsBold]
  else
    case Column of
      COL_ACTION:
        if (Data^.Action = 'BUY') then
          TargetCanvas.Font.Color := clBlue
        else if (Data^.Action = 'SELL') then
          TargetCanvas.Font.Color := clRed;
    end;
end;

procedure TframeActiveOrders.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: POrderData;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TframeActiveOrders.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: POrderData;
begin
  inherited;
  CellText := '';
  Data := Sender.GetNodeData(Node);
  if (Data.NodeType = ntGroup) then
    case Column of
      COL_ID:
        CellText := Data^.GroupText;
    end
  else
    case Column of
      COL_ID:
        CellText := IntToStr(Data^.Id);
      COL_ACTION:
        CellText := Data^.Action;
      COL_BROKER:
        CellText := Data^.Broker;
      COL_ORDERTYPE:
        CellText := Data^.OrderType;
      COL_INSTRUMENT:
        CellText := Data^.Instrument;
      COL_QUANTITY:
        CellText := FloatToStr(Data^.Quantity);
      COL_REMAINING:
        CellText := FloatToStr(Data^.Remaining);
      COL_FILLED:
        CellText := FloatToStr(Data^.Filled);
      COL_PRICE:
        CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.Price);
      COL_FILLPRICE:
        CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.FillPrice);
      COL_LIMIT_PRICE:
        CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.LimitPrice);
      COL_EXCHANGE:
        CellText := Data^.Exchange;
      COL_OCA:
        CellText := Data^.OCAGroup;
      COL_TIMEINFORCE:
        CellText := Data^.TimeInForce;
      COL_PARENTID:
        if (Data^.ParentId > 0) then
          CellText := Data^.ParentId.ToString;
      COL_INFO:
        CellText := Data^.Info;
      COL_QUALIFIER_ID:
        if (Data^.QualifierID > 0) then
          CellText := Data^.QualifierID.ToString;
      COL_QUALIFIER_INSTANCE:
        if (Data^.QualifierInstance > 0) then
          CellText := Data^.QualifierInstance.ToString;
      COL_AUTOTRADE_ID:
        if (Data^.AutoTradesID > 0) then
          CellText := Data^.AutoTradesID.ToString;
      COL_AUTOTRADE_INSTANCE:
        if (Data^.AutoTradeInstance > 0) then
          CellText := Data^.AutoTradeInstance.ToString;
    end
end;

procedure TframeActiveOrders.SetFilter(const aAutoTradesCommon: TAutoTradesCommon);
begin
  FAutoTradesCommon := aAutoTradesCommon;
  for var OrderData in FOrderList.Values do
    if Assigned(OrderData^.Node) then
      if (fpFiltered in Self.Parameters) then
      begin
        vstTree.IsVisible[OrderData^.Node] := (OrderData^.AutoTradeInstance > 0) and
                                              (OrderData^.QualifierID = FAutoTradesCommon.QualifierID) and
                                              (OrderData^.QualifierInstance = FAutoTradesCommon.QualifierInstance) and
                                              (OrderData^.AutoTradesID = FAutoTradesCommon.AutoTradesID) and
                                              (OrderData^.AutoTradeInstance = FAutoTradesCommon.AutoTradesInstance);

      end
      else
        vstTree.IsVisible[OrderData^.Node] := True;
end;

procedure TframeActiveOrders.SetFlatTree;
var
  Data: POrderData;
  i: Integer;
begin
  for Data in FOrderList.Values do
    vstTree.MoveTo(Data.Node, nil, amInsertBefore, False);
  for i := 0 to FGroupList.Count - 1 do
    if Assigned(FGroupList.Objects[i]) then
      vstTree.DeleteNode(PVirtualNode(FGroupList.Objects[i]));
  FGroupList.Clear;
end;

procedure TframeActiveOrders.SetGroupTree(aData: POrderData);
var
  AttMode: TVTNodeAttachMode;
  GroupData: POrderData;
  GroupNode: PVirtualNode;
  GroupText: string;
  Index: Integer;
begin
  if Assigned(aData) and (GroupOption <> goNone) then
  begin
    case GroupOption of
      goBroker:
        GroupText := aData^.Broker;
      goInstrument:
        GroupText := aData^.Instrument;
      goOrderType:
        GroupText := aData^.OrderType;
    end;
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

    if (GroupNode.ChildCount = 0) then
      AttMode := amAddChildFirst
    else
    begin
      AttMode := amInsertAfter;
      GroupNode := GroupNode.LastChild;
    end;

    vstTree.MoveTo(aData.Node, GroupNode, AttMode, False);
    RecalcGroupValues;
  end;
end;

procedure TframeActiveOrders.ShowOrder;
var
  Data: POrderData;
  DataOrder: PTreeData;
  NodeOrder: PVirtualNode;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    Data := vstTree.FocusedNode^.GetData;
    if Assigned(Data) then
    begin
      NodeOrder := TMonitorLists.OrderList.GetNodeOrder(Data.Id);
      if Assigned(NodeOrder) then
      begin
        DataOrder := NodeOrder^.GetData;
        if Assigned(DataOrder) and Assigned(DataOrder^.OrderDoc) then
          case Data^.BrokerType of
            TBrokerType.brIB:
              TfrmEditOrderIB.ShowDocument(TOrderIBDoc(DataOrder^.OrderDoc), True);
            TBrokerType.brNN:
              TfrmEditOrderNN.ShowDocument(TOrderNNDoc(DataOrder^.OrderDoc), DataOrder^.OrderDoc.ExtendedOptions.Subordination);
          end;
      end
    end;
  end;
end;

procedure TframeActiveOrders.RecalcGroupValues;
var
  Index: Integer;
  GroupData: POrderData;
  GroupNode: PVirtualNode;
  ChildRun: PVirtualNode;
  ChildData: POrderData;
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
          GroupData^.Quantity  := 0;
          GroupData^.Filled    := 0;
          GroupData^.Remaining := 0;

          ChildRun := GroupNode.FirstChild;
          while Assigned(ChildRun) do
          begin
            ChildData := ChildRun.GetData;
            if Assigned(ChildData) and (ChildData.NodeType = ntNode) then
            begin
              GroupData^.Quantity  := GroupData^.Quantity + ChildData^.Quantity;
              GroupData^.Filled    := GroupData^.Filled + ChildData^.Filled;
              GroupData^.Remaining := GroupData^.Remaining + ChildData^.Remaining;
            end;
            ChildRun := ChildRun.NextSibling;
          end;
        end;
    finally
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TframeActiveOrders.SetGroupOption(const Value: TActiveOrdersGroupOption);
var
  Data: POrderData;
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
          for Data in FOrderList.Values do
            SetGroupTree(Data);
      finally
        vstTree.EndUpdate;
      end;
    end;
  end;
end;

{TOrderData}

procedure TOrderData.Clear;
begin
  Self := Default(TOrderData);
end;

end.
