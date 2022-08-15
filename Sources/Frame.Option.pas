unit Frame.Option;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, IABFunctions, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons,
  Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, Document, System.Generics.Collections,
  System.Generics.Defaults, DebugWriter, CustomForms, Global.Types, System.IniFiles, IABSocketAPI_const, IABSocketAPI,
  System.Math, InstrumentList, Edit.OrderStatus, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ExtDlgs, BrokerHelperAbstr,
  Vcl.Printers, DaModule, Scanner.Types, MessageDialog, Search.Types, VirtualTrees.ExportHelper, DaImages, Entity.Sokid,
  Publishers.Interfaces, IABFunctions.RequestsQueue, IABFunctions.MarketData, Monitor.Types, Publishers,
  IABFunctions.Helpers, MonitorTree.Helper, VirtualTrees.Types;
{$ENDREGION}

type
  TframeOption = class(TFrame, IUpdateFeeds, ITickOptionComputation, IInstrumentSpecDetails)
    aClearResultList: TAction;
    aColumnSettings: TAction;
    aDeleteItems: TAction;
    aExportToCSV: TAction;
    aExportToExcel: TAction;
    alOptions: TActionList;
    aPrint: TAction;
    aRefresh: TAction;
    btnColumnSettings: TBitBtn;
    btnExportToCSV: TBitBtn;
    btnExportToExcel: TBitBtn;
    btnPrint: TBitBtn;
    btnRefresh: TBitBtn;
    miClearResultList: TMenuItem;
    miDeleteItems: TMenuItem;
    pmOption: TPopupMenu;
    pnlOptions: TPanel;
    vstOption: TVirtualStringTree;
    procedure aClearResultListExecute(Sender: TObject);
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aDeleteItemsExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure vstOptionColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure vstOptionCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstOptionDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstOptionDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstOptionFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstOptionGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstOptionHeaderDragged(Sender: TVTHeader; Column: TColumnIndex; OldPosition: Integer);
  private const
    COL_LOCAL_SYMBOL       = 0;
    COL_CONTRACT_ID        = 1;
    COL_UNDERLYING_CONID   = 2;
    COL_EXPIRY             = 3;
    COL_STRIKE             = 4;
    COL_UNDPRICE           = 5;
    COL_DELTA              = 6;
    COL_OPTPRICE           = 7;
    COL_PVDIVIDEND         = 8;
    COL_GAMMA              = 9;
    COL_VEGA               = 10;
    COL_THETA              = 11;
    C_FIXED_COLUMN_INDEX   = 12;

    C_DATA_ID = 7;

    C_IDENTITY_NAME = 'frameOption';
  private
    FUnderlyingConId: Integer;
    FNodeList: TDictionary<Integer, PVirtualNode>;
    FSymbol: string;
    FParameters: TOptionalParameters;
    function GetOptionValue(aColumn: Integer; aData: POption): string;
    procedure LoadFromXml;
    procedure SaveToXml;
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    //implementation ITickOptionComputation
    procedure OnTickOptionComputation(Sender: TObject; DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice: Double);
    //implementation IInstrumentSpecDetails
    procedure OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
  public
    procedure Initialize;
    procedure Deinitialize;
    procedure Search(aParameters: TOptionalParameters);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Symbol          : string  read FSymbol          write FSymbol;
    property UnderlyingConId : Integer read FUnderlyingConId write FUnderlyingConId;
  end;

implementation

{$R *.dfm}

{ TframeOption }

constructor TframeOption.Create(AOwner: TComponent);
begin
  inherited;
  vstOption.NodeDataSize := SizeOf(TOption);
  FNodeList := TDictionary<Integer, PVirtualNode>.Create;

  TPublishers.FeedPublisher.Subscribe(Self);
  TPublishers.InstrumentSpecDetailsPublisher.Subscribe(Self);
  TPublishers.TickOptionComputationPublisher.Subscribe(Self);
end;

destructor TframeOption.Destroy;
begin
  if Assigned(General) then
  begin
    TIABMarket.CancelMarketData(FParameters.UnderlyingConId);
    TPublishers.FeedPublisher.Unsubscribe(Self);
    TPublishers.InstrumentSpecDetailsPublisher.Unsubscribe(Self);
    TPublishers.TickOptionComputationPublisher.Unsubscribe(Self);
  end;
  FreeAndNil(FNodeList);
  vstOption.Clear;
  inherited;
end;

procedure TframeOption.Initialize;
begin
  TMonitorTree.Initialize(vstOption);
  LoadFromXml;
end;

procedure TframeOption.Deinitialize;
begin
  SaveToXml;
end;

procedure TframeOption.LoadFromXml;
var
  Column   : TVirtualTreeColumn;
  TickType : TIABTickType;
begin
  vstOption.BeginUpdate;
  try
    for TickType := Low(TIABTickType) to ttETFNavLow do
    begin
      Column := vstOption.Header.Columns.Add;
      Column.Text             := TickType.ToString;
      Column.Tag              := Integer(TickType);
      Column.Options          := Column.Options - [coEditable];
      Column.CaptionAlignment := taCenter;
      Column.Alignment        := taRightJustify;
      Column.Options          := Column.Options - [coVisible]
    end;
    TStoreHelper.LoadFromXml(vstOption, C_IDENTITY_NAME);
  finally
    vstOption.EndUpdate;
  end;
end;

procedure TframeOption.SaveToXml;
begin
  TStoreHelper.SaveToXml(vstOption, C_IDENTITY_NAME);
end;

procedure TframeOption.Search(aParameters: TOptionalParameters);
var
  Order: TIABOrder;
begin
  vstOption.BeginUpdate;
  try
    Screen.Cursor := crHourGlass;
    if not FParameters.IsEqual(aParameters) then
      FParameters := aParameters;
    Self.UnderlyingConId := FParameters.UnderlyingConId;
    Self.Symbol          := FParameters.Symbol;
    FNodeList.Clear;
    vstOption.Clear;

    Order := TIABOrder.Create;
    try
      if not IABClient.MarketSubscribeList.IsSubscribed(FParameters.UnderlyingConId) then
      begin
        IABClient.ClearOrder(Order);
        Order.ContractId      := FParameters.UnderlyingConId;
        Order.Currency        := FParameters.Currency;
        Order.Exchange        := FParameters.UnderlyingExchange;
        Order.SecurityType    := TIABSecurityType.FromString(FParameters.UnderlyingSecurityType);
        Order.Symbol          := FParameters.UnderlyingSymbol;
        TIABMarket.RequestMarketData(Order);
      end;

      for var Expiry in FParameters.ExpirationsArray do
        for var Strike in FParameters.StrikesArray do
        begin
          IABClient.ClearOrder(Order);
          Order.Symbol       := FParameters.UnderlyingSymbol;
          Order.Currency     := FParameters.Currency;
          Order.Exchange     := FParameters.Exchange;
          Order.Right        := rtCall;
          Order.SecurityType := FParameters.SecurityType;
          Order.Expiry       := Expiry;
          Order.Strike       := Strike;
          IABClient.SendRequest(ibGetInstrumentSpecs, C_DATA_ID, Order, qpHigh);
        end;
    finally
      FreeAndNil(Order);
    end;
  finally
    Screen.Cursor := crDefault;
    vstOption.EndUpdate;
  end;
end;

procedure TframeOption.OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
var
  SokidInfo: TSokidInfo;
  Node: PVirtualNode;
  Data: POption;
begin
  if (IABClient.InstrumentSpecs.Items[Index].DataID = C_DATA_ID) then
  begin
    vstOption.BeginUpdate;
    try
      if FNodeList.ContainsKey(IABClient.InstrumentSpecs.Items[Index].ContractId) then
        Node := FNodeList.Items[IABClient.InstrumentSpecs.Items[Index].ContractId]
      else
        Node := vstOption.AddChild(nil);
      Data := Node^.GetData;
      Data.AssignFrom(IABClient.InstrumentSpecs.Items[Index]);
      Data^.UnderlyingConId := FParameters.UnderlyingConId;
      FNodeList.AddOrSetValue(Data^.ContractId, Node);
    finally
      vstOption.EndUpdate;
    end;

    if SokidList.ContainsKey(Data^.ContractId) then
    begin
      SokidInfo := SokidList.Items[Data^.ContractId];
      if (SokidInfo.UnderlyingConId <> Data^.UnderlyingConId) then
      begin
        SokidInfo.UnderlyingConId := Data^.UnderlyingConId;
        SokidList.SetValue(SokidInfo);
      end;

      TIABMarket.RequestMarketData(Data^.ContractId);
    end;
    vstOption.InvalidateNode(Node);
  end;
end;

procedure TframeOption.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
var
  Node: PVirtualNode;
begin
  if FNodeList.ContainsKey(Id) then
  begin
    Node := FNodeList.Items[Id];
    if Assigned(Node) then
      vstOption.InvalidateNode(Node);
  end;
end;

procedure TframeOption.OnTickOptionComputation(Sender: TObject; DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice: Double);
var
  Node: PVirtualNode;
  Data: POption;
begin
  if FNodeList.ContainsKey(DataId) then
    Node := FNodeList.Items[DataId]
  else
    Node := nil;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if (undPrice <> UNSET_DOUBLE) then
      Data^.undPrice := undPrice;
    if (Delta <> UNSET_DOUBLE) then
      Data^.Delta := Delta;
    if (OptPrice <> UNSET_DOUBLE) then
      Data^.OptPrice := OptPrice;
    if (pvDividend <> UNSET_DOUBLE) then
      Data^.pvDividend := pvDividend;
    if (Gamma <> UNSET_DOUBLE) then
      Data^.Gamma := Gamma;
    if (Vega <> UNSET_DOUBLE) then
      Data^.Vega := Vega;
    if (Theta <> UNSET_DOUBLE) and (Theta <> -2) then
      Data^.Theta := Theta;

    if (TickType in [ttBidOptionComp, ttAskOptionComp, ttLastOptionComp, ttModelOption, ttCustomOptionComp]) and
      (ImpliedVol <> UNSET_DOUBLE) then
      TMonitorLists.PriceCache.AddPrice(Data^.ContractId, TickType, ImpliedVol, Now);
    vstOption.InvalidateNode(Node);
  end;
end;

procedure TframeOption.vstOptionCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: POption;
begin
  Result := 0;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  if Assigned(Data1) and Assigned(Data2) then
    Result := CompareText(GetOptionValue(Column, Data1), GetOptionValue(Column, Data2));
end;

procedure TframeOption.vstOptionDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TframeOption.vstOptionDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  if (Sender.FocusedNode <> Node) then
  begin
    TargetCanvas.Font.Color := clWindowText;
    case Column of
      COL_STRIKE:
        TargetCanvas.Font.Color := clNavy;
      COL_UNDPRICE:
        TargetCanvas.Font.Color := clNavy;
    end;
  end;
end;

procedure TframeOption.vstOptionFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: POption;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
  begin
    TIABMarket.CancelMarketData(Data^.ContractId);
    Data^.Clear;
  end;
end;

procedure TframeOption.vstOptionGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: POption;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    CellText := GetOptionValue(Column, Data);
end;

procedure TframeOption.vstOptionHeaderDragged(Sender: TVTHeader; Column: TColumnIndex; OldPosition: Integer);
begin
  SaveToXml;
end;

procedure TframeOption.vstOptionColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  SaveToXml;
end;

function TframeOption.GetInstance: TObject;
begin
  Result := Self;
end;

function TframeOption.GetOptionValue(aColumn: Integer; aData: POption): string;
var
  FormatStr: string;
  TickValue: Double;
  tt: TIABTickType;
begin
  case aColumn of
    COL_LOCAL_SYMBOL:
      Result := aData^.LocalSymbol;
    COL_CONTRACT_ID:
      if (aData^.ContractId > 0) then
        Result := aData^.ContractId.ToString
      else
        Result := '';
    COL_UNDERLYING_CONID:
      Result := aData^.UnderlyingConId.ToString;
    COL_EXPIRY:
      Result := FormatDateTime('YYYYMMDD', aData^.Expiry);
    COL_STRIKE:
      Result := Format('%.2f', [aData^.Strike]);
    COL_UNDPRICE:
      if (aData^.undPrice = UNSET_DOUBLE) then
        Result := '∞'
      else
        Result := Format('%.2f', [aData^.undPrice]);
    COL_DELTA:
      if (aData^.Delta = UNSET_DOUBLE) then
        Result := '∞'
      else
        Result := Format('%.3f', [aData^.Delta]);
    COL_OPTPRICE:
      if (aData^.OptPrice = UNSET_DOUBLE) then
        Result := '∞'
      else
        Result := Format('%.3f', [aData^.OptPrice]);
    COL_PVDIVIDEND:
      if (aData^.pvDividend = UNSET_DOUBLE) then
        Result := '∞'
      else
        Result := Format('%.3f', [aData^.pvDividend]);
    COL_GAMMA:
      if (aData^.Gamma = UNSET_DOUBLE) then
        Result := '∞'
      else
        Result := Format('%.3f', [aData^.Gamma]);
    COL_VEGA:
      if (aData^.Vega = UNSET_DOUBLE) then
        Result := '∞'
      else
        Result := Format('%.3f', [aData^.Vega]);
    COL_THETA:
      if (aData^.Theta = UNSET_DOUBLE) then
        Result := '∞'
      else
        Result := Format('%.3f', [aData^.Theta]);
  else
    if (aColumn >= C_FIXED_COLUMN_INDEX) then
    begin
      tt := TIABTickType(aColumn - C_FIXED_COLUMN_INDEX);
      FormatStr := tt.ToFormat;
      TickValue := TMonitorLists.PriceCache.GetLastPrice(aData^.ContractId, tt);
      if (tt = ttLast) and (TickValue = 0) then
        Result := 'b ' + Format(FormatStr, [TMonitorLists.PriceCache.GetLastPrice(aData^.ContractId, ttClose)])
      else if (TickValue = UNSET_DOUBLE) then
        Result := '∞'
      else
        Result := Format(FormatStr, [TickValue]);
    end;
  end;
end;

procedure TframeOption.aClearResultListExecute(Sender: TObject);
begin
  vstOption.Clear;
  FNodeList.Clear;
end;

procedure TframeOption.aRefreshExecute(Sender: TObject);
begin
  Search(FParameters);
end;

procedure TframeOption.aColumnSettingsExecute(Sender: TObject);
begin
  TStoreHelper.ShowColumnSettings(vstOption, C_IDENTITY_NAME, C_FIXED_COLUMN_INDEX);
end;

procedure TframeOption.aDeleteItemsExecute(Sender: TObject);
var
  DelNode: PVirtualNode;
  Data: POption;
  Node: PVirtualNode;
begin
  vstOption.BeginUpdate;
  try
    Node := vstOption.GetFirstSelected(True);
    while Assigned(Node) do
    begin
      DelNode := Node;
      Node := vstOption.GetNextSelected(Node, True);
      Data := DelNode^.GetData;
      FNodeList.Remove(Data^.ContractId);
      vstOption.DeleteNode(DelNode);
    end;
  finally
    vstOption.EndUpdate;
  end;
end;

procedure TframeOption.aExportToCSVExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToCSV(vstOption, 'Option.' + FSymbol);
end;

procedure TframeOption.aExportToExcelExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToExcel(vstOption, 'Option.' + FSymbol);
end;

procedure TframeOption.aPrintExecute(Sender: TObject);
begin
  Printer.Orientation := poLandscape;
  vstOption.Print(Printer, True);
end;

end.
