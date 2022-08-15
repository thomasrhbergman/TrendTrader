unit Frame.Future;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, IABFunctions, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons,
  Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, Document, System.Generics.Collections,
  System.Generics.Defaults, DebugWriter, CustomForms, Global.Types, System.IniFiles, IABSocketAPI_const, IABSocketAPI,
  System.Math, InstrumentList, Edit.OrderStatus, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ExtDlgs, BrokerHelperAbstr,
  Vcl.Printers, DaModule, Scanner.Types, MessageDialog, Column.Settings, Search.Types, VirtualTrees.ExportHelper, DaImages,
  Entity.Sokid, Publishers.Interfaces, IABFunctions.RequestsQueue, IABFunctions.MarketData, Monitor.Types, Publishers,
  IABFunctions.Helpers, MonitorTree.Helper, VirtualTrees.Types;
{$ENDREGION}

type
  TframeFuture = class(TFrame, IUpdateFeeds,
                               IInstrumentSpecDetails)
    aClearResultList: TAction;
    aColumnSettings: TAction;
    aDeleteItems: TAction;
    aExportToCSV: TAction;
    aExportToExcel: TAction;
    alFuture: TActionList;
    aPrint: TAction;
    aRefresh: TAction;
    btnColumnSettings: TBitBtn;
    btnExportToCSV: TBitBtn;
    btnExportToExcel: TBitBtn;
    btnPrint: TBitBtn;
    btnRefresh: TBitBtn;
    miClearResultList: TMenuItem;
    miDeleteItems: TMenuItem;
    pmFuture: TPopupMenu;
    pnlTop: TPanel;
    vstFuture: TVirtualStringTree;
    procedure aClearResultListExecute(Sender: TObject);
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aDeleteItemsExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure vstFutureColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure vstFutureCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstFutureDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstFutureFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFutureGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstFutureHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vstFutureHeaderDragged(Sender: TVTHeader; Column: TColumnIndex; OldPosition: Integer);
  private const
    COL_BROKER           = 0;
    COL_CONTRACT_ID      = 1;
    COL_UNDERLYING_CONID = 2;
    COL_ISIN             = 3;
    COL_NAME             = 4;
    COL_SYMBOL           = 5;
    COL_LOCAL_SYMBOL     = 6;
    COL_TYPE             = 7;
    COL_GROUP            = 8;
    COL_CURRENCY         = 9;
    COL_TRADABLES        = 10;
    COL_SECTOR           = 11;
    COL_EXCHANGE         = 12;
    COL_PRIMARY_EXCHANGE = 13;
    COL_EXPIRY           = 14;
    C_FIXED_COLUMN_INDEX = 15;

    C_IDENTITY_NAME = 'frameFuture';
    C_DATA_ID = 6;
  private
    FUnderlyingConId: Integer;
    FNodeList: TDictionary<Integer, PVirtualNode>;
    FSymbol: string;
    FParameters: TOptionalParameters;
    function GetFutureValue(aColumn: Integer; aData: PSokidInfo): string;
    procedure LoadFromXml;
    procedure SaveToXml;
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
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

{ TframeFuture }

constructor TframeFuture.Create(AOwner: TComponent);
begin
  inherited;
  vstFuture.NodeDataSize := SizeOf(TSokidInfo);
  FNodeList := TDictionary<Integer, PVirtualNode>.Create;

  TPublishers.FeedPublisher.Subscribe(Self);
  TPublishers.InstrumentSpecDetailsPublisher.Subscribe(Self);
  TPublishers.TickOptionComputationPublisher.Subscribe(Self);
end;

destructor TframeFuture.Destroy;
begin
  if Assigned(General) then
  begin
    TPublishers.FeedPublisher.Unsubscribe(Self);
    TPublishers.InstrumentSpecDetailsPublisher.Unsubscribe(Self);
    TPublishers.TickOptionComputationPublisher.Unsubscribe(Self);
  end;
  FreeAndNil(FNodeList);
  vstFuture.Clear;
  inherited;
end;

procedure TframeFuture.Initialize;
begin
  TMonitorTree.Initialize(vstFuture);
  LoadFromXml;
end;

procedure TframeFuture.Deinitialize;
begin
  SaveToXml;
end;

procedure TframeFuture.LoadFromXml;
var
  Column   : TVirtualTreeColumn;
  TickType : TIABTickType;
begin
  vstFuture.BeginUpdate;
  try
    for TickType := Low(TIABTickType) to ttETFNavLow do
    begin
      Column := vstFuture.Header.Columns.Add;
      Column.Text             := TickType.ToString;
      Column.Tag              := Integer(TickType);
      Column.Options          := Column.Options - [coEditable];
      Column.CaptionAlignment := taCenter;
      Column.Alignment        := taRightJustify;
      Column.Options          := Column.Options - [coVisible]
    end;
    TStoreHelper.LoadFromXml(vstFuture, C_IDENTITY_NAME);
  finally
    vstFuture.EndUpdate;
  end;
end;

procedure TframeFuture.SaveToXml;
begin
  TStoreHelper.SaveToXml(vstFuture, C_IDENTITY_NAME);
end;

procedure TframeFuture.Search(aParameters: TOptionalParameters);
var
  Order: TIABOrder;
begin
  if not FParameters.IsEqual(aParameters) then
    FParameters := aParameters;
  Self.UnderlyingConId := aParameters.UnderlyingConId;
  Self.Symbol          := aParameters.Symbol;
  for var Key in FNodeList.Keys do
    TIABMarket.CancelMarketData(Key);
  FNodeList.Clear;
  vstFuture.Clear;

  Order := TIABOrder.Create;
  try
    for var Expiry in aParameters.ExpirationsArray do
    begin
      IABClient.ClearOrder(Order);
      Order.Exchange     := aParameters.Exchange;
      Order.Symbol       := aParameters.Symbol;
      Order.Expiry       := Expiry;
      Order.SecurityType := stFuture;
      IABClient.SendRequest(ibGetInstrumentSpecs, C_DATA_ID, Order);
    end;
  finally
    FreeAndNil(Order);
  end;
end;

procedure TframeFuture.vstFutureColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  SaveToXml;
end;

procedure TframeFuture.vstFutureHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if (Sender.SortColumn <> HitInfo.Column) then
    Sender.SortDirection := sdAscending
  else if (Sender.SortDirection = sdAscending) then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;
  Sender.SortColumn := HitInfo.Column;
  vstFuture.SortTree(HitInfo.Column, Sender.SortDirection, True);
end;

procedure TframeFuture.vstFutureHeaderDragged(Sender: TVTHeader; Column: TColumnIndex; OldPosition: Integer);
begin
  SaveToXml;
end;

procedure TframeFuture.vstFutureCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PSokidInfo;
begin
  Result := 0;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  if Assigned(Data1) and Assigned(Data2) then
    Result := CompareText(GetFutureValue(Column, Data1), GetFutureValue(Column, Data2));
end;

procedure TframeFuture.vstFutureDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TframeFuture.vstFutureFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
  begin
    TIABMarket.CancelMarketData(Data^.ContractId);
    Data^.Clear;
  end;
end;

procedure TframeFuture.vstFutureGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    CellText := GetFutureValue(Column, Data);
end;

function TframeFuture.GetFutureValue(aColumn: Integer; aData: PSokidInfo): string;
var
  FormatStr: string;
  TickValue: Double;
begin
  case aColumn of
    COL_BROKER:
      Result := aData^.Broker.ToString;
    COL_CONTRACT_ID:
      Result := aData^.ContractId.ToString;
    COL_UNDERLYING_CONID:
      if (aData^.UnderlyingConId > 0) then
        Result := aData^.UnderlyingConId.ToString
      else
        Result := '';
    COL_ISIN:
      Result := aData^.IsIn;
    COL_NAME:
      Result := aData^.Name;
    COL_SYMBOL:
      Result := aData^.Symbol;
    COL_LOCAL_SYMBOL:
      Result := aData^.LocalSymbol;
    COL_TYPE:
      Result := aData^.SecurityType;
    COL_GROUP:
      Result := aData^.Group;
    COL_CURRENCY:
      Result := aData^.Currency;
    COL_TRADABLES:
      begin
        Result := '';
        for var i := 0 to Length(aData^.Tradables) - 1 do
          Result := Result + IntToStr(aData^.Tradables[i].MarketId) + ', ';
        if Result <> '' then
          Delete(Result, Length(Result) - 1, 2);
      end;
    COL_SECTOR:
      Result := aData^.Sector;
    COL_EXCHANGE:
      Result := aData^.Exchange;
    COL_PRIMARY_EXCHANGE:
      Result := aData^.PrimaryExchange;
    COL_EXPIRY:
      if (aData^.Expiry > 0) then
        Result := FormatDateTime('DD.MM.YYYY hh:nn', aData^.Expiry)
      else
        Result := '';
  else
    if (aColumn >= C_FIXED_COLUMN_INDEX) then
    begin
      FormatStr := TIABTickType(aColumn - C_FIXED_COLUMN_INDEX).ToFormat;
      TickValue := TMonitorLists.PriceCache.GetLastPrice(aData^.ContractId, TIABTickType(aColumn - C_FIXED_COLUMN_INDEX));
      if (TickValue = UNSET_DOUBLE) then
        Result := '∞'
      else
        Result := Format(FormatStr, [TickValue]);
    end;
  end
end;

function TframeFuture.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TframeFuture.aClearResultListExecute(Sender: TObject);
begin
  vstFuture.Clear;
end;

procedure TframeFuture.aColumnSettingsExecute(Sender: TObject);
begin
  TStoreHelper.ShowColumnSettings(vstFuture, C_IDENTITY_NAME, C_FIXED_COLUMN_INDEX);
end;

procedure TframeFuture.aDeleteItemsExecute(Sender: TObject);
var
  DelNode: PVirtualNode;
  Node: PVirtualNode;
begin
  vstFuture.BeginUpdate;
  try
    Node := vstFuture.GetFirstSelected(True);
    while Assigned(Node) do
    begin
      DelNode := Node;
      Node := vstFuture.GetNextSelected(Node, True);
      vstFuture.DeleteNode(DelNode);
    end;
  finally
    vstFuture.EndUpdate;
  end;
end;

procedure TframeFuture.aExportToCSVExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToCSV(vstFuture, 'Future.' + FSymbol);
end;

procedure TframeFuture.aExportToExcelExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToExcel(vstFuture, 'Future.' + FSymbol);
end;

procedure TframeFuture.aPrintExecute(Sender: TObject);
begin
  Printer.Orientation := poLandscape;
  vstFuture.Print(Printer, True);
end;

procedure TframeFuture.aRefreshExecute(Sender: TObject);
begin
  Search(FParameters);
end;

procedure TframeFuture.OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
var
  SokidInfo: TSokidInfo;
  Node: PVirtualNode;
  Data: PSokidInfo;
begin
  if (IABClient.InstrumentSpecs.Items[Index].DataID = C_DATA_ID) then
  begin
    if SokidList.ContainsKey(IABClient.InstrumentSpecs.Items[Index].ContractId) then
    begin
      SokidInfo := SokidList.Items[IABClient.InstrumentSpecs.Items[Index].ContractId];
      Node := vstFuture.AddChild(nil);
      Data := Node^.GetData;
      Data^.AssignFrom(SokidInfo);
      Data^.UnderlyingConId := Self.UnderlyingConId;
      if (SokidInfo.UnderlyingConId <> Data^.UnderlyingConId) then
      begin
        SokidInfo.UnderlyingConId := Data^.UnderlyingConId;
        SokidList.SetValue(SokidInfo);
      end;
      TIABMarket.RequestMarketData(Data^.ContractId);
      FNodeList.AddOrSetValue(Data^.ContractId, Node);
      vstFuture.InvalidateNode(Node);
    end;
  end;
end;

procedure TframeFuture.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
var
  Node: PVirtualNode;
begin
  if FNodeList.ContainsKey(Id) then
  begin
    Node := FNodeList.Items[Id];
    if Assigned(Node) then
      vstFuture.InvalidateNode(Node);
  end;
end;

end.
