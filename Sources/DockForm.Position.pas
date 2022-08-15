unit DockForm.Position;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Generics.Collections, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls, VirtualTrees, IABSocketAPI,
  Vcl.ImgList, Winapi.ActiveX, BrokerHelperAbstr, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.StdCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const, Document, DaImages, System.ImageList,
  InstrumentList, Utils, IABFunctions, DebugWriter, Global.Types, MessageDialog, Monitor.Interfaces, Common.Types,
  CustomDockForm, System.UITypes, IABFunctions.MarketData, System.Math, Publishers, MonitorTree.Document;
{$ENDREGION}

type
  PPosition = ^TPosition;
  TPosition = record
    NodeType       : TNodeType;
    AccountName    : string;
    Broker         : string;
    AverageCost    : Double;
    ConId          : Integer;
    ContractSymbol : string;
    Currency       : string;
    InstrumentID   : Integer;
    ImageNo        : Integer;
    MarketPrice    : Double;
    MarketValue    : Double;
    Position       : Double;
    Price          : Currency;
    RealizedPNL    : Double;
    UnrealizedPNL  : Double;
    SecType        : TIABSecurityType;
    TimeStr        : string;
    procedure Clear;
  end;

  TfrmDockFormPosition = class(TfrmCustomDockForm)
    aCloseAllLinesMarked: TAction;
    aCloseOneSelected: TAction;
    aSelectAllNodes: TAction;
    aSelectLossRealized: TAction;
    aSelectLossUnrealized: TAction;
    aSelectProfitRealized: TAction;
    aSelectProfitUnrealized: TAction;
    btnCloseSelected: TBitBtn;
    btnSellAll: TBitBtn;
    cbOptions: TComboBox;
    lblOptions: TLabel;
    miCloseAllLinesMarked: TMenuItem;
    miCloseOneSelected: TMenuItem;
    miSelectAllNodes: TMenuItem;
    miSelectLossRealized: TMenuItem;
    miSelectLossUnrealized: TMenuItem;
    miSelectProfitRealized: TMenuItem;
    miSelectProfitUnrealized: TMenuItem;
    miSep1: TMenuItem;
    miSep2: TMenuItem;
    miSep3: TMenuItem;
    procedure aCloseAllLinesMarkedExecute(Sender: TObject);
    procedure aCloseOneSelectedExecute(Sender: TObject);
    procedure aCloseOneSelectedUpdate(Sender: TObject);
    procedure aSelectAllNodesExecute(Sender: TObject);
    procedure aSelectLossRealizedExecute(Sender: TObject);
    procedure aSelectLossUnrealizedExecute(Sender: TObject);
    procedure aSelectNodesUpdate(Sender: TObject);
    procedure aSelectProfitRealizedExecute(Sender: TObject);
    procedure aSelectProfitUnrealizedExecute(Sender: TObject);
    procedure btnCloseSelectedDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure btnCloseSelectedDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure cbOptionsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
  private type
    TGroupOption = (goNone, goBroker);
  private const
    C_KEY_LIMIT_ORDER  = 'LimitOrder';
    C_KEY_MARKET_ORDER = 'MarketOrder';
    C_IDENTITY_NAME    = 'DockFormPosition';

    GroupOptionName: array [TGroupOption] of string = ('None', 'Broker');

    COL_CONTRACT      = 0;
    COL_BROKER        = 1;
    COL_POSITION      = 2;
    COL_MARKET_PRICE  = 3;
    COL_MARKET_VALUE  = 4;
    COL_AVERAGE_COST  = 5;
    COL_REALIZED_PL   = 6;
    COL_UNREALIZED_PL = 7;
    COL_ACCOUNT       = 8;
    COL_TIME          = 9;
    COL_CONID         = 10;
  private
    FAccountList: TAccountList;
    FGroupList: TStringList;
    FGroupOption: TGroupOption;
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    procedure RecalcGroupValues;
    procedure SellContract(aNode: PVirtualNode);
    procedure SellSelected;
    procedure SetFlatTree;
    procedure SetGroupOption(const Value: TGroupOption);
    procedure SetGroupTree(aNode: PVirtualNode);
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure UpdateAccountTime(const timeStamp : string);
    procedure UpdatePortfolio(Index: Integer);
    property GroupOption: TGroupOption read FGroupOption write SetGroupOption;
  end;

var
  frmDockFormPosition: TfrmDockFormPosition;

implementation

{$R *.dfm}

procedure TfrmDockFormPosition.FormCreate(Sender: TObject);
begin
  vstTree.NodeDataSize := SizeOf(TPosition);
  inherited;
  FGroupList := TStringList.Create;
  FGroupList.Duplicates := dupIgnore;
  FGroupList.Sorted := True;

  FAccountList := TAccountList.Create([doOwnsValues]);
  FAccountList.DoPriceChange := OnPriceChange;
end;

procedure TfrmDockFormPosition.FormDestroy(Sender: TObject);
begin
  if Assigned(TPublishers.FeedPublisher) then
    TPublishers.FeedPublisher.Unsubscribe(FAccountList);
  FreeAndNil(FGroupList);
  FreeAndNil(FAccountList);
  frmDockFormPosition := nil;
  inherited;
end;

procedure TfrmDockFormPosition.Initialize;
begin
  inherited Initialize;
  Caption := 'Position';
  TPublishers.FeedPublisher.Subscribe(FAccountList);

  cbOptions.Items.Clear;
  for var go := Low(TGroupOption) to High(TGroupOption) do
    cbOptions.Items.Add(GroupOptionName[go]);
  cbOptions.ItemIndex := -1;
end;

function TfrmDockFormPosition.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmDockFormPosition.cbOptionsChange(Sender: TObject);
begin
  if Showing then
    GroupOption := TGroupOption(cbOptions.ItemIndex);
end;

procedure TfrmDockFormPosition.SetGroupTree(aNode: PVirtualNode);
var
  AttMode: TVTNodeAttachMode;
  Data: PPosition;
  GroupData: PPosition;
  GroupNode: PVirtualNode;
  GroupText: string;
  Index: Integer;
  SubGroupLossData: PPosition;
  SubGroupLossNode: PVirtualNode;
  SubGroupProfitData: PPosition;
  SubGroupProfitNode: PVirtualNode;
begin
  if Assigned(aNode) and (GroupOption <> goNone) then
  begin
    Data := aNode.GetData;
    case GroupOption of
      goBroker:
        GroupText := Data^.Broker;
    end;
    Index := FGroupList.IndexOf(GroupText);
    if (Index < 0) then
    begin
      GroupNode := vstTree.AddChild(nil);
      GroupData := GroupNode^.GetData;
      GroupData^.ContractSymbol := GroupText;
      GroupData^.NodeType := ntGroup;
      GroupData^.ImageNo := -1;

      SubGroupProfitNode := vstTree.AddChild(GroupNode);
      SubGroupProfitData := SubGroupProfitNode^.GetData;
      SubGroupProfitData^.ContractSymbol := GroupText + ' profit';
      SubGroupProfitData^.NodeType := ntGroupProfit;
      SubGroupProfitData^.ImageNo := -1;

      SubGroupLossNode := vstTree.AddChild(GroupNode);
      SubGroupLossData := SubGroupLossNode^.GetData;
      SubGroupLossData^.ContractSymbol := GroupText + ' loss';
      SubGroupLossData^.NodeType := ntGroupLoss;
      SubGroupLossData^.ImageNo := -1;

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

procedure TfrmDockFormPosition.RecalcGroupValues;
var
  ChildData: PPosition;
  ChildRun: PVirtualNode;
  Counter: Integer;
  GroupData: PPosition;
  GroupNode: PVirtualNode;
  Index: Integer;
  SubGroupLossData: PPosition;
  SubGroupLossNode: PVirtualNode;
  SubGroupProfitData: PPosition;
  SubGroupProfitNode: PVirtualNode;
begin
  Counter := 0;
  GroupData := nil;
  if (GroupOption <> goNone) then
  begin
    if Assigned(GroupData) then
      GroupData^.AverageCost := 0;
    for Index := 0 to FGroupList.Count - 1 do
      if Assigned(FGroupList.Objects[Index]) then
      begin
        GroupNode := PVirtualNode(FGroupList.Objects[Index]);
        GroupData := GroupNode.GetData;
        GroupData^.AverageCost := 0;
        GroupData^.RealizedPNL := 0;
        GroupData^.UnrealizedPNL := 0;

        ChildRun := GroupNode.FirstChild;
        SubGroupProfitNode := nil;
        SubGroupLossNode := nil;
        SubGroupProfitData := nil;
        SubGroupLossData := nil;

        while Assigned(ChildRun) do
        begin
          ChildData := ChildRun.GetData;
          if (ChildData.NodeType = ntGroupProfit) then
          begin
            SubGroupProfitNode := ChildRun;
            SubGroupProfitData := ChildData;
          end
          else if (ChildData.NodeType = ntGroupLoss) then
          begin
            SubGroupLossNode := ChildRun;
            SubGroupLossData := ChildData;
          end;
          if Assigned(SubGroupLossNode) and Assigned(SubGroupProfitNode) then
            ChildRun := nil
          else
            ChildRun := ChildRun.NextSibling;
        end;

        if Assigned(SubGroupProfitNode) and Assigned(SubGroupProfitData) then
        begin
          SubGroupProfitData^.UnrealizedPNL := 0;
          SubGroupProfitData^.RealizedPNL := 0;
        end;
        if Assigned(SubGroupLossNode) and Assigned(SubGroupLossData) then
        begin
          SubGroupLossData^.UnrealizedPNL := 0;
          SubGroupLossData^.RealizedPNL := 0;
        end;

        ChildRun := GroupNode.FirstChild;
        while Assigned(ChildRun) do
        begin
          ChildData := ChildRun.GetData;
          if Assigned(ChildData) and (ChildData.NodeType = ntNode) then
          begin
            GroupData^.RealizedPNL := GroupData^.RealizedPNL + ChildData^.RealizedPNL;
            GroupData^.UnrealizedPNL := GroupData^.UnrealizedPNL + ChildData^.UnrealizedPNL;
            GroupData^.AverageCost := GroupData^.AverageCost + ChildData^.AverageCost;
            if (ChildData^.Position > 0) then
            begin
              GroupData^.Position := GroupData^.Position + ChildData^.Position;
              Inc(Counter);
            end;

            if Assigned(SubGroupProfitNode) then
            begin
              if (ChildData^.UnrealizedPNL > 0) then
                SubGroupProfitData^.UnrealizedPNL := SubGroupProfitData^.UnrealizedPNL + ChildData^.UnrealizedPNL;
              if (ChildData^.RealizedPNL > 0) then
                SubGroupProfitData^.RealizedPNL := SubGroupProfitData^.RealizedPNL + ChildData^.RealizedPNL;
            end;
            if Assigned(SubGroupLossNode) then
            begin
              if (ChildData^.UnrealizedPNL < 0) then
                SubGroupLossData^.UnrealizedPNL := SubGroupLossData^.UnrealizedPNL + ChildData^.UnrealizedPNL;
              if (ChildData^.RealizedPNL < 0) then
                SubGroupLossData^.RealizedPNL := SubGroupLossData^.RealizedPNL + ChildData^.RealizedPNL;
            end;
          end;
          ChildRun := ChildRun.NextSibling;
        end;
      end;
      if Assigned(GroupData) then
        GroupData^.AverageCost := GroupData^.AverageCost / Counter;
  end;
end;

procedure TfrmDockFormPosition.SetFlatTree;
var
  Node: PVirtualNode;
  i: Integer;
begin
  for Node in FAccountList.Values do
    vstTree.MoveTo(Node, nil, amInsertBefore, False);
  for i := 0 to FGroupList.Count - 1 do
    if Assigned(FGroupList.Objects[i]) then
      vstTree.DeleteNode(PVirtualNode(FGroupList.Objects[i]));
  FGroupList.Clear;
end;

procedure TfrmDockFormPosition.SetGroupOption(const Value: TGroupOption);
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
          for Node in FAccountList.Values do
            SetGroupTree(Node);
      finally
        vstTree.EndUpdate;
      end;
    end;
  end;
end;

procedure TfrmDockFormPosition.UpdateAccountTime(const timeStamp: string);
var
  Node: PVirtualNode;
  Data: PPosition;
begin
  vstTree.BeginUpdate;
  try
    Node := vstTree.GetFirst;
    while Assigned(Node) do
    begin
      Data := vstTree.GetNodeData(Node);
      if Assigned(Data) then
        Data.TimeStr := timeStamp;
      Node := vstTree.GetNext(Node);
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmDockFormPosition.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
var
  Data: PPosition;
  Node: PVirtualNode;
begin
  // Depåinnehav
  for Node in FAccountList.Values do
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if (Data.InstrumentID = Id) then
      begin
        if (Data.SecType = stCash) and (TickType = TIABTickType.ttAsk) then
        begin
          Data.Price := Value;
          Exit;
        end
        else if (Data.SecType <> stCash) and (TickType = ttLast) then
        begin
          Data.Price := Value;
          Exit;
        end;
      end;
    end;
end;

procedure TfrmDockFormPosition.UpdatePortfolio(Index: Integer);
var
  Data: PPosition;
  Node: PVirtualNode;
  PortfolioItem : TIABPortfolioItem;
begin
  Node := nil;
  PortfolioItem := IABClient.Portfolio[Index];
  vstTree.BeginUpdate;
  try
    if FAccountList.ContainsKey(PortfolioItem.Symbol) then
      Node := FAccountList[PortfolioItem.Symbol];
    if not Assigned(Node) then
    begin
      Node := vstTree.AddChild(nil);
      FAccountList.Add(PortfolioItem.Symbol, Node);
    end;
    Data := Node^.GetData;
    Data.Broker         := TBrokerType.brIB.ToString;
    Data.ContractSymbol := PortfolioItem.Symbol;
    Data.AccountName    := PortfolioItem.AccountName;
    Data.AverageCost    := PortfolioItem.AverageCost;
    Data.Currency       := PortfolioItem.Currency;
    Data.InstrumentID   := PortfolioItem.InstrumentID;
    Data.MarketPrice    := PortfolioItem.MarketPrice;
    Data.MarketValue    := PortfolioItem.MarketValue;
    Data.Position       := PortfolioItem.Position;
    Data.RealizedPNL    := PortfolioItem.RealizedPNL;
    Data.SecType        := PortfolioItem.SecurityType;
    Data.UnrealizedPNL  := PortfolioItem.UnrealizedPNL;
    Data.NodeType       := ntNode;
    if (Data.Position < 0) then
      Data.ImageNo := ICON_ORDER_SELL
    else
      Data.ImageNo := ICON_ORDER_BUY;

    TIABMarket.RequestMarketData(PortfolioItem.InstrumentID);
  finally
    vstTree.EndUpdate;
  end;
  TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnUpdatePortfolio',
                                                                   'Symbol=' + PortfolioItem.Symbol +
                                                                   ', ContractID=' + PortfolioItem.InstrumentID.ToString +
                                                                   ', Position=' + PortfolioItem.Position.ToString);
end;

procedure TfrmDockFormPosition.aSelectNodesUpdate(Sender: TObject);
var
  Data: PPosition;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    Data := vstTree.FocusedNode.GetData;
    TAction(Sender).Enabled := Data.NodeType = ntNode;
  end
  else
    TAction(Sender).Enabled := False;
end;

procedure TfrmDockFormPosition.aCloseAllLinesMarkedExecute(Sender: TObject);
begin
  vstTree.SelectAll(True);
  SellSelected;
end;

procedure TfrmDockFormPosition.aCloseOneSelectedExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstTree.FocusedNode;
  if Assigned(Node) and (TMessageDialog.ShowQuestion('Selected position will be sold. Continue?') = mrYes) then
    SellContract(Node);
end;

procedure TfrmDockFormPosition.aCloseOneSelectedUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := vstTree.TotalCount > 0;
end;

procedure TfrmDockFormPosition.SellSelected;
begin
  if (TMessageDialog.ShowQuestion('Selected positions will be sold. Continue?') = mrYes) then
  begin
    vstTree.BeginUpdate;
    try
      for var Node in FAccountList.Values do
        if vstTree.Selected[Node] then
          SellContract(Node);
    finally
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TfrmDockFormPosition.SellContract(aNode: PVirtualNode);
var
  Action : TIABAction;
  Data: PPosition;
  Monitor: IMonitor;
begin
  Action := iabSell;
  if Assigned(aNode) then
  begin
    Data := aNode.GetData;
    if (Data.Position > 0) then
      Action := iabSell
    else if (Data.Position < 0) then
      Action := iabBuy;
    if (Data.Position <> 0) and Supports(Application.MainForm, IMonitor, Monitor) then
      Monitor.SellContractPosition(Data.ContractSymbol,
                                   Data.Currency,
                                   Trunc(Abs(Data.Position)),
                                   Data.InstrumentID,
                                   Data.SecType,
                                   Action);
  end;
end;

procedure TfrmDockFormPosition.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PPosition;
begin
  inherited;
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  if Data1^.NodeType in [ntGroup, ntGroupProfit, ntGroupLoss] then
    Result := 0
  else
    case Column of
      COL_CONTRACT:
        Result := CompareText(Data1^.ContractSymbol, Data2^.ContractSymbol);
      COL_BROKER:
        Result := CompareText(Data1^.Broker, Data2^.Broker);
      COL_POSITION:
        Result := CompareValue(Data1^.Position, Data2^.Position);
      COL_MARKET_PRICE:
        Result := CompareValue(Data1^.MarketPrice, Data2^.MarketPrice);
      COL_MARKET_VALUE:
        Result := CompareValue(Data1^.MarketValue, Data2^.MarketValue);
      COL_AVERAGE_COST:
        Result := CompareValue(Data1^.AverageCost, Data2^.AverageCost);
      COL_UNREALIZED_PL:
        Result := CompareValue(Data1^.UnrealizedPNL, Data2^.UnrealizedPNL);
      COL_REALIZED_PL:
        Result := CompareValue(Data1^.RealizedPNL, Data2^.RealizedPNL);
      COL_ACCOUNT:
        Result := CompareText(Data1^.AccountName, Data2^.AccountName);
      COL_TIME:
        Result := CompareValue(StrToDateTime(Data1^.TimeStr), StrToDateTime(Data2^.TimeStr));
      COL_CONID:
        Result := CompareValue(Data1^.InstrumentID, Data2^.InstrumentID);
    end;
end;

procedure TfrmDockFormPosition.vstTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  inherited;
  Allowed := IABClient.Connected;
end;

procedure TfrmDockFormPosition.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PPosition;
begin
  inherited;
  if Assigned(Node) then
  begin
    Data := vstTree.GetNodeData(Node);
    if (Data^.NodeType in [ntGroup, ntGroupProfit, ntGroupLoss]) then
    begin
      TargetCanvas.Font.Style := [fsBold];
      if (Data^.NodeType = ntGroupProfit) then
        TargetCanvas.Font.Color := clNavy
      else if (Data^.NodeType = ntGroupLoss) then
        TargetCanvas.Font.Color := clRed;
    end
    else
      case Column of
        COL_POSITION:
          if (Data.Position < 0) then
            TargetCanvas.Font.Color := clRed
          else if (Data.Position = 0) then
            TargetCanvas.Font.Color := clBlue
          else
            TargetCanvas.Font.Color := clBlack;
        COL_MARKET_PRICE:
          if (Data.Position = 0) then
            TargetCanvas.Font.Color := clBlue;
        COL_AVERAGE_COST:
          if (Data.Position = 0) then
            TargetCanvas.Font.Color := clBlue;
        COL_MARKET_VALUE:
          if (Data.Position = 0) then
            TargetCanvas.Font.Color := clBlue
          else if (Data.MarketValue < 0) then
            TargetCanvas.Font.Color := clRed
          else
            TargetCanvas.Font.Color := clBlack;
        COL_UNREALIZED_PL:
          if (Data.UnrealizedPNL < 0) then
            TargetCanvas.Font.Color := clRed
          else if (Data.Position = 0) then
            TargetCanvas.Font.Color := clBlue
          else
            TargetCanvas.Font.Color := clBlack;
        COL_REALIZED_PL:
          if (Data.RealizedPNL < 0) then
            TargetCanvas.Font.Color := clRed
          else if (Data.Position = 0) then
            TargetCanvas.Font.Color := clBlue
          else
            TargetCanvas.Font.Color := clBlack;
      end;
  end;
end;

procedure TfrmDockFormPosition.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PPosition;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmDockFormPosition.vstTreeGetImageIndex(Sender: TBaseVirtualTree;Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Data: PPosition;
begin
  inherited;
  if (Kind in [ikNormal, ikSelected]) then
  begin
    Data := vstTree.GetNodeData(Node);
    case Column of
      COL_CONTRACT:
        ImageIndex := Data.ImageNo;
    end;
  end;
end;

procedure TfrmDockFormPosition.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PPosition;
//  s, s2: string;
begin
  Data := Node^.GetData;
//  s := '';
//  s2 := '';
//  if Data.Currency = 'SEK' then
//    s2 := ' kr'
//  else if Data.Currency = 'USD' then
//    s := '$ '
//  else if Data.Currency = 'EUR' then
//    s := '€ '
//  else if Data.Currency = 'GBP' then
//    s := '£ ';

  case Column of
    COL_CONTRACT:
      CellText := Data^.ContractSymbol;
    COL_BROKER:
      CellText := Data^.Broker;
    COL_POSITION:
      CellText := Format('%.0f', [Data^.Position]);
    COL_MARKET_PRICE:
      CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.MarketPrice);
    COL_MARKET_VALUE:
      CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.MarketValue);
    COL_AVERAGE_COST:
      CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.AverageCost);
    COL_UNREALIZED_PL:
      CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.UnrealizedPNL);
    COL_REALIZED_PL:
      CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.RealizedPNL);
    COL_ACCOUNT:
      CellText := Data^.AccountName;
    COL_TIME:
      CellText := Data^.TimeStr;
    COL_CONID:
      CellText := Data^.InstrumentID.ToString;
  end;
end;

procedure TfrmDockFormPosition.vstTreeStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  inherited;
  if (Sender = vstTree) then
  begin
    if (vstTree.SelectedCount > 1) then
      vstTree.DragCursor := crMultiDrag
    else
      vstTree.DragCursor := crDrag;
  end;
end;

procedure TfrmDockFormPosition.btnCloseSelectedDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = vstTree;
end;

procedure TfrmDockFormPosition.btnCloseSelectedDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  btnCloseSelected.DragMode := dmManual;
  SellSelected;
end;

procedure TfrmDockFormPosition.aSelectAllNodesExecute(Sender: TObject);
var
  Data: PPosition;
  Run: PVirtualNode;
begin
  vstTree.BeginUpdate;
  try
    Run := vstTree.GetFirstChild(nil);
    while Assigned(Run) do
    begin
      Data := Run.GetData;
      vstTree.Selected[Run] := (Data.NodeType = ntNode);
      Run := Run.NextSibling;
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmDockFormPosition.aSelectLossRealizedExecute(Sender: TObject);
var
  Data: PPosition;
  Run: PVirtualNode;
begin
  vstTree.BeginUpdate;
  try
    Run := vstTree.GetFirstChild(nil);
    while Assigned(Run) do
    begin
      Data := Run.GetData;
      if Assigned(Data) and (Data.NodeType = ntNode) then
        if (Data.Position = 0) then
          vstTree.Selected[Run] := Data.RealizedPNL < 0
        else
          vstTree.Selected[Run] := False;
      Run := Run.NextSibling;
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmDockFormPosition.aSelectLossUnrealizedExecute(Sender: TObject);
var
  Data: PPosition;
  Run: PVirtualNode;
begin
  vstTree.BeginUpdate;
  try
    Run := vstTree.GetFirstChild(nil);
    while Assigned(Run) do
    begin
      Data := Run.GetData;
      if Assigned(Data) and (Data.NodeType = ntNode) then
        if (Data.Position = 0) then
          vstTree.Selected[Run] := Data.UnrealizedPNL < 0
        else
          vstTree.Selected[Run] := False;
      Run := Run.NextSibling;
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmDockFormPosition.aSelectProfitRealizedExecute(Sender: TObject);
var
  Data: PPosition;
  Run: PVirtualNode;
begin
  vstTree.BeginUpdate;
  try
    Run := vstTree.GetFirstChild(nil);
    while Assigned(Run) do
    begin
      Data := Run.GetData;
      if Assigned(Data) and (Data.NodeType = ntNode) then
        if (Data.Position = 0) then
          vstTree.Selected[Run] := Data.RealizedPNL > 0
        else
          vstTree.Selected[Run] := False;
      Run := Run.NextSibling;
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmDockFormPosition.aSelectProfitUnrealizedExecute(Sender: TObject);
var
  Data: PPosition;
  Run: PVirtualNode;
begin
  vstTree.BeginUpdate;
  try
    Run := vstTree.GetFirstChild(nil);
    while Assigned(Run) do
    begin
      Data := Run.GetData;
      if Assigned(Data) and (Data.NodeType = ntNode) then
        if (Data.Position = 0) then
          vstTree.Selected[Run] := Data.UnrealizedPNL > 0
        else
          vstTree.Selected[Run] := False;
      Run := Run.NextSibling;
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

{ TPosition }

procedure TPosition.Clear;
begin
  AccountName    := '';
  Broker         := '';
  ContractSymbol := '';
  Currency       := '';
  TimeStr        := '';
end;

end.
