unit Frame.RealtimeFeeds;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, IABFunctions, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons,
  Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, Document, System.Generics.Collections,
  System.Generics.Defaults, DebugWriter, CustomForms, Global.Types, System.IniFiles, IABSocketAPI_const, IABSocketAPI,
  System.Math, InstrumentList, Edit.OrderStatus, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ExtDlgs, BrokerHelperAbstr,
  Vcl.Printers, DaModule, Scanner.Types, MessageDialog, VirtualTrees.ExportHelper, DaImages, Entity.Sokid, Common.Types,
  Publishers.Interfaces, IABFunctions.RequestsQueue, IABFunctions.MarketData, Monitor.Types, Publishers, Utils,
  DaModule.Utils, Global.Resources, HtmlLib, HtmlConsts, Data.DB, InformationDialog, System.DateUtils,
  Frame.Custom, Winapi.ActiveX, Monitor.Info, System.IOUtils, Vcl.ComCtrls, IABFunctions.Helpers, VirtualTrees.Types;
{$ENDREGION}

type
  TframeRealtimeFeeds = class(TframeCustom, IUpdateFeeds)
    aClear: TAction;
    aColumnSettings: TAction;
    aDelete: TAction;
    aExportPriceHistoryToCSV: TAction;
    aExportPriceHistoryToExcel: TAction;
    aExportToCSV: TAction;
    aExportToExcel: TAction;
    alMain: TActionList;
    aPrint: TAction;
    aShowPriceHistory: TAction;
    btnClear: TBitBtn;
    btnColumnSettings: TBitBtn;
    btnExportToCSV: TBitBtn;
    btnExportToExcel: TBitBtn;
    btnPrint: TBitBtn;
    edTimeBegin: TDateTimePicker;
    edTimeEnd: TDateTimePicker;
    lblShowFrom: TLabel;
    lblShowTo: TLabel;
    miDelete: TMenuItem;
    miExportPriceHistory: TMenuItem;
    miExportPriceHistoryToCSV: TMenuItem;
    miExportPriceHistoryToExcel: TMenuItem;
    miShowPriceHistory: TMenuItem;
    pmFeeds: TPopupMenu;
    pnlOptions: TPanel;
    procedure aClearExecute(Sender: TObject);
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aDeleteUpdate(Sender: TObject);
    procedure aExportPriceHistoryToCSVExecute(Sender: TObject);
    procedure aExportPriceHistoryToExcelExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aShowPriceHistoryExecute(Sender: TObject);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vstTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private const
    C_COL_SYMBOL         = 0;
    C_COL_INSTRUMENT     = 1;
    C_COL_CONTRACTID     = 2;
    C_COL_LAST_CLOSE     = 3;
    C_FIXED_COLUMN_INDEX = 4;

    C_IDENTITY_NAME = 'frameRealtimeFeeds';
  private
    FNodeList: TDictionary<Integer, PVirtualNode>;
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure Deinitialize; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TframeRealtimeFeeds }

constructor TframeRealtimeFeeds.Create(AOwner: TComponent);
begin
  inherited;
  vstTree.NodeDataSize := SizeOf(TSokidInfo);
  FNodeList := TDictionary<Integer, PVirtualNode>.Create;
end;

destructor TframeRealtimeFeeds.Destroy;
begin
  vstTree.Clear;
  FreeAndNil(FNodeList);
  inherited;
end;

function TframeRealtimeFeeds.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

function TframeRealtimeFeeds.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TframeRealtimeFeeds.Initialize;
var
  Column: TVirtualTreeColumn;
begin
  vstTree.BeginUpdate;
  try
    for var TickType := Low(TIABTickType) to ttETFNavLow do
    begin
      Column := vstTree.Header.Columns.Add;
      Column.Text             := TickType.ToString;
      Column.Tag              := Integer(TickType);
      Column.Options          := Column.Options - [{coDraggable,} coEditable];
      Column.CaptionAlignment := taCenter;
      Column.Alignment        := taRightJustify;
      Column.Width            := 80;
      Column.Options          := Column.Options - [coVisible]
    end;
    TStoreHelper.LoadFromXml(vstTree, C_IDENTITY_NAME + vstTree.Name);
  finally
    vstTree.EndUpdate;
  end;

  inherited;
  TPublishers.FeedPublisher.Subscribe(Self);
end;

procedure TframeRealtimeFeeds.Deinitialize;
begin
  TPublishers.FeedPublisher.Unsubscribe(Self);
  inherited;
end;

procedure TframeRealtimeFeeds.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
begin
  if FNodeList.ContainsKey(Id) then
    vstTree.InvalidateNode(FNodeList.Items[Id]);
end;

procedure TframeRealtimeFeeds.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSokidInfo;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
  begin
    if (Data^.Broker = TBrokerType.brIB) then
      TIABMarket.CancelMarketData(Data^.ContractId);
    Data^.Clear;
  end;
end;

procedure TframeRealtimeFeeds.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

  function GetLastClose(aConId: Integer): string;
  var
    ClosePrice: Double;
  begin
    ClosePrice := TMonitorLists.PriceCache.GetLastPrice(aConId, ttClose);
    if (ClosePrice <> 0) then
      ClosePrice := (TMonitorLists.PriceCache.GetLastPrice(aConId, ttLast) / ClosePrice - 1) * 100;
    Result := Format('%0.2f', [ClosePrice]);
  end;

var
  Data: PSokidInfo;
  Value: Double;
  formatStr: string;
begin
  Data := Node^.GetData;
  case Column of
    C_COL_SYMBOL:
      CellText := Data^.Symbol;
    C_COL_INSTRUMENT:
      CellText := Data^.Name;
    C_COL_CONTRACTID:
      CellText := Data^.ContractId.ToString;
    C_COL_LAST_CLOSE:
      CellText := GetLastClose(Data^.ContractId);
  else
    Value := TMonitorLists.PriceCache.GetLastPrice(Data^.ContractId, TIABTickType(Column - C_FIXED_COLUMN_INDEX));
    formatStr := TIABTickType(Column - C_FIXED_COLUMN_INDEX).ToFormat;
    CellText := Format(formatStr, [Value]);
  end;
end;

procedure TframeRealtimeFeeds.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PSokidInfo;
begin
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  case Column of
    C_COL_SYMBOL:
      Result := CompareText(Data1^.Symbol, Data2^.Symbol);
    C_COL_INSTRUMENT:
      Result := CompareText(Data1^.Name, Data2^.Name);
    C_COL_CONTRACTID:
      Result := CompareValue(Data1^.ContractId, Data2^.ContractId);
  end;
end;

procedure TframeRealtimeFeeds.vstTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  inherited;
  Allowed := True;
end;

procedure TframeRealtimeFeeds.vstTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  Data: PSokidInfo;
  NewData: PSokidInfo;
  NewNode: PVirtualNode;
  Node: PVirtualNode;
  SourceTree: TBaseVirtualTree;
  SokidInfo: TSokidInfo;
begin
  inherited;
  if (Sender <> Source) then
  begin
    SourceTree := TVirtualStringTree(Source);
    vstTree.BeginUpdate;
    try
      Node := SourceTree.GetFirstSelected;
      while Assigned(Node) do
      begin
        Data := Node^.GetData;
        if not FNodeList.ContainsKey(Data^.ContractId) then
          if SokidList.ContainsKey(Data^.ContractId) then
          begin
            SokidInfo := SokidList.Items[Data^.ContractId];
            NewNode := vstTree.AddChild(nil);
            NewData := NewNode^.GetData;
            NewData^.AssignFrom(SokidInfo);
            vstTree.IsVisible[NewNode] := True;
            vstTree.InvalidateNode(NewNode);
            FNodeList.Add(Data^.ContractId, NewNode);
            if (Data^.Broker = TBrokerType.brIB) then
              TIABMarket.RequestMarketData(Data^.ContractId);
          end;
        Node := SourceTree.GetNextSelected(Node);
      end;
    finally
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TframeRealtimeFeeds.aShowPriceHistoryExecute(Sender: TObject);
var
  Data: PSokidInfo;
begin
  inherited;
  if Assigned(vstTree.FocusedNode) then
  begin
    Data := vstTree.FocusedNode^.GetData;
    TInformationDialog.ShowMessage(TDocumentInfo.GetPriceHistory(Data^.ContractId,
                                                                 function(const aPrice: TPrice): Boolean
                                                                 begin
                                                                   Result := False;
                                                                   if (aPrice.TickType in [ttLast, ttLastSize, ttBid, ttBidSize, ttAsk, ttAskSize, ttClose, ttOpen, ttLow, ttHigh, ttVolume, ttOptionHistoricalVol]) then
                                                                   begin
                                                                     Result := (DateOf(aPrice.TimeStamp) = Today) and
                                                                               (TimeOf(aPrice.TimeStamp) >= edTimeBegin.Time) and (TimeOf(aPrice.TimeStamp) <= edTimeEnd.Time);
                                                                   end;
                                                                 end), 'PriceHistory');
  end;
end;

const
  arrHeaders: TArray<string> = ['TimeStamp',
                                'Last',
                                'LastSize',
                                'Bid',
                                'BidSize',
                                'Ask',
                                'AskSize',
                                'Close',
                                'Open',
                                'Low',
                                'High',
                                'Volume',
                                'Historical Volatility'];

procedure TframeRealtimeFeeds.aExportPriceHistoryToCSVExecute(Sender: TObject);
var
  Data: PSokidInfo;
  PriceList: TPriceList;
  SaveDialog: TFileSaveDialog;
  TypeItem: TFileTypeItem;
  sb: TStringBuilder;
  arrPrices: TArray<TPrice>;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    Data := vstTree.FocusedNode^.GetData;
    PriceList := TMonitorLists.PriceCache.GetPriceList(Data^.ContractID);
    if Assigned(PriceList) then
    begin
      SaveDialog := TFileSaveDialog.Create(nil);
      try
        SaveDialog.FileTypes.Clear;
        SaveDialog.DefaultExtension := '*.csv';
        SaveDialog.FileName := Data^.Symbol + ' PriceHistory.csv';

        TypeItem := SaveDialog.FileTypes.Add;
        TypeItem.DisplayName := 'CSV-file';
        TypeItem.FileMask := '*.csv';

        TypeItem := SaveDialog.FileTypes.Add;
        TypeItem.DisplayName := 'TXT-file';
        TypeItem.FileMask := '*.txt';

        TypeItem := SaveDialog.FileTypes.Add;
        TypeItem.DisplayName := 'All files';
        TypeItem.FileMask := '*.*';
        if SaveDialog.Execute then
        begin
          sb := TStringBuilder.Create;
          try
            for var i := Low(arrHeaders) to High(arrHeaders) do
              sb.Append('"').Append(arrHeaders[i]).Append('",');
            sb.AppendLine;

            arrPrices := PriceList.GetLastPrices(
              function(const aPrice: TPrice): Boolean
              begin
                Result := (aPrice.TickType in [ttLast, ttLastSize, ttBid, ttBidSize, ttAsk, ttAskSize, ttClose, ttOpen, ttLow, ttHigh, ttVolume, ttOptionHistoricalVol]) and
                           SameDate(aPrice.TimeStamp, Today) and
                           TimeInRange(TimeOf(aPrice.TimeStamp), edTimeBegin.Time, edTimeEnd.Time);
              end);

            for var Price in arrPrices do
            begin
              sb.Append('"').Append(FormatDateTime('dd.mm.yyyy hh:nn:ss', Price.TimeStamp)).Append('",');
              case Price.TickType of
                ttLast:
                  sb.AppendFormat('%f,0,0,0,0,0,0,0,0,0,0,0', [Price.Value]).AppendLine;
                ttLastSize:
                  sb.AppendFormat('0,%f,0,0,0,0,0,0,0,0,0,0', [Price.Value]).AppendLine;
                ttBid:
                  sb.AppendFormat('0,0,%f,0,0,0,0,0,0,0,0,0', [Price.Value]).AppendLine;
                ttBidSize:
                  sb.AppendFormat('0,0,0,%f,0,0,0,0,0,0,0,0', [Price.Value]).AppendLine;
                ttAsk:
                  sb.AppendFormat('0,0,0,0,%f,0,0,0,0,0,0,0', [Price.Value]).AppendLine;
                ttAskSize:
                  sb.AppendFormat('0,0,0,0,0,%f,0,0,0,0,0,0', [Price.Value]).AppendLine;
                ttClose:
                  sb.AppendFormat('0,0,0,0,0,0,%f,0,0,0,0,0', [Price.Value]).AppendLine;
                ttOpen:
                  sb.AppendFormat('0,0,0,0,0,0,0,%f,0,0,0,0', [Price.Value]).AppendLine;
                ttLow:
                  sb.AppendFormat('0,0,0,0,0,0,0,0,%f,0,0,0', [Price.Value]).AppendLine;
                ttHigh:
                  sb.AppendFormat('0,0,0,0,0,0,0,0,0,%f,0,0', [Price.Value]).AppendLine;
                ttVolume:
                  sb.AppendFormat('0,0,0,0,0,0,0,0,0,0,%f,0', [Price.Value]).AppendLine;
                ttOptionHistoricalVol:
                  sb.AppendFormat('0,0,0,0,0,0,0,0,0,0,0,%f', [Price.Value]).AppendLine;
              end;
            end;
            TFile.WriteAllText(SaveDialog.FileName, sb.ToString);
          finally
            FreeAndNil(sb);
          end;
        end;
      finally
        FreeAndNil(SaveDialog);
      end;
    end;
  end;
end;

procedure TframeRealtimeFeeds.aExportPriceHistoryToExcelExecute(Sender: TObject);
var
  Data: PSokidInfo;
  PriceList: TPriceList;
  arrPrices : TArray<TPrice>;
  row: Integer;
  SaveDialog: TFileSaveDialog;
  Sheet, Range, WorkBooks: OleVariant;
  TypeItem: TFileTypeItem;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    Data := vstTree.FocusedNode^.GetData;
    PriceList := TMonitorLists.PriceCache.GetPriceList(Data^.ContractID);
    if Assigned(PriceList) then
    begin
      SaveDialog := TFileSaveDialog.Create(nil);
      try
        SaveDialog.FileTypes.Clear;
        SaveDialog.DefaultExtension := '*.xls';
        SaveDialog.FileName := Data^.Symbol + ' PriceHistory.xls';

        TypeItem := SaveDialog.FileTypes.Add;
        TypeItem.DisplayName := 'XLS-file';
        TypeItem.FileMask := '*.xls';

        TypeItem := SaveDialog.FileTypes.Add;
        TypeItem.DisplayName := 'XLSX-file';
        TypeItem.FileMask := '*.xlsx';

        TypeItem := SaveDialog.FileTypes.Add;
        TypeItem.DisplayName := 'All files';
        TypeItem.FileMask := '*.*';
        if SaveDialog.Execute then
          if TExcelExportHelper.RunExcel then
          begin
            WorkBooks := TExcelExportHelper.Excel.WorkBooks.Add;
            Sheet := WorkBooks.WorkSheets[1];

            for var i := Low(arrHeaders) to High(arrHeaders) do
              Sheet.Cells[1, i + 1] := arrHeaders[i];

            arrPrices := PriceList.GetLastPrices(
              function(const aPrice: TPrice): Boolean
              begin
                Result := (aPrice.TickType in [ttLast, ttLastSize, ttBid, ttBidSize, ttAsk, ttAskSize, ttClose, ttOpen, ttLow, ttHigh, ttVolume, ttOptionHistoricalVol]) and
                           SameDate(aPrice.TimeStamp, Today) and
                           TimeInRange(TimeOf(aPrice.TimeStamp), edTimeBegin.Time, edTimeEnd.Time);
              end);

            row := 2;
            for var Price in arrPrices do
            begin
              Inc(row);
              Sheet.Cells[row, 1] := Price.TimeStamp;
              case Price.TickType of
                ttLast:
                  Sheet.Cells[row, 2] := Double(Price.Value);
                ttLastSize:
                  Sheet.Cells[row, 3] := Trunc(Price.Value);
                ttBid:
                  Sheet.Cells[row, 4] := Double(Price.Value);
                ttBidSize:
                  Sheet.Cells[row, 5] := Trunc(Price.Value);
                ttAsk:
                  Sheet.Cells[row, 6] := Double(Price.Value);
                ttAskSize:
                  Sheet.Cells[row, 7] := Trunc(Price.Value);
                ttClose:
                  Sheet.Cells[row, 8] := Double(Price.Value);
                ttOpen:
                  Sheet.Cells[row, 9] := Double(Price.Value);
                ttLow:
                  Sheet.Cells[row, 10] := Double(Price.Value);
                ttHigh:
                  Sheet.Cells[row, 11] := Double(Price.Value);
                ttVolume:
                  Sheet.Cells[row, 12] := Double(Price.Value);
                ttOptionHistoricalVol:
                  Sheet.Cells[row, 13] := Double(Price.Value);
              end;
            end;
            Range := Sheet.Range['A1:M1'];
            Range.EntireColumn.Autofit;
            WorkBooks.SaveAs(SaveDialog.FileName);
            TExcelExportHelper.Excel.Visible := True;
          end;
      finally
        FreeAndNil(SaveDialog);
      end;
    end;
  end;
end;

procedure TframeRealtimeFeeds.aDeleteExecute(Sender: TObject);
var
  Data: PSokidInfo;
begin
  inherited;
  if Assigned(vstTree.FocusedNode) then
  begin
    Data := vstTree.FocusedNode^.GetData;
    FNodeList.Remove(Data^.ContractId);
    vstTree.DeleteNode(vstTree.FocusedNode);
  end;
end;

procedure TframeRealtimeFeeds.aDeleteUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := not vstTree.IsEmpty and Assigned(vstTree.FocusedNode);
end;

procedure TframeRealtimeFeeds.vstTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  inherited;
  Accept := True;
end;

procedure TframeRealtimeFeeds.aClearExecute(Sender: TObject);
begin
  vstTree.Clear;
  FNodeList.Clear;
end;

procedure TframeRealtimeFeeds.aColumnSettingsExecute(Sender: TObject);
begin
  TStoreHelper.ShowColumnSettings(vstTree, C_IDENTITY_NAME, 0);
end;

procedure TframeRealtimeFeeds.aExportToCSVExecute(Sender: TObject);
begin
  ExportToCSV;
end;

procedure TframeRealtimeFeeds.aExportToExcelExecute(Sender: TObject);
begin
  ExportToExcel;
end;

procedure TframeRealtimeFeeds.aPrintExecute(Sender: TObject);
begin
  Print;
end;

end.
