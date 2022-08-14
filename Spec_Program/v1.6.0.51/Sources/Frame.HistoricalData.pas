unit Frame.HistoricalData;

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
  Frame.Custom, MonitorTree.Helper, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  PHistoricalData = ^THistoricalData;
  THistoricalData = record
    DataId: Integer;
    Symbol: string;
    ContractId: Integer;
    BarSize: TIABChartBarSize;
    Duration: Integer;
    DurationTimeUnits: Integer;
    ChartDate: TDateTime;
    DateEnd: TDateTime;
    Open: Double;
    High: Double;
    Low: Double;
    Close: Double;
    Wap: Double;
    Volume: BigDecimal;
    TickType: string;
    ReceivingDate: TDateTime;
    procedure Clear;
  end;

  THistoricalDataParams = record
    Symbol: string;
    ContractId: Integer;
    BarSize: TIABChartBarSize;
    Duration: Integer;
    DurationTimeUnits: Integer;
    DateEnd: TDateTime;
    TickType: string;
    StartTime: TDateTime;
  end;

  TPacingViolations = (pvNone, pvIdenticalPer15Sec, pv6Per2Sec, pv60Per10Min);
  TPacingViolationsHelper = record helper for TPacingViolations
  private
    const PacingViolationsName: array[TPacingViolations] of string = (
      'None',
      'Making identical historical data requests within 15 seconds.',
      'Making six or more historical data requests for the same Contract, Exchange and Tick Type within two seconds.',
      'Making more than 60 requests within any ten minute period.');
  public
    function ToString: string;
  end;

  TframeHistoricalData = class(TframeCustom, IError)
    aClear: TAction;
    aClearHistoricalDB: TAction;
    aColumnSettings: TAction;
    aExportToCSV: TAction;
    aExportToExcel: TAction;
    aInfo: TAction;
    alMain: TActionList;
    aPrint: TAction;
    aSavePriceToDB: TAction;
    btnClear: TBitBtn;
    btnClearHistoricalDB: TBitBtn;
    btnColumnSettings: TBitBtn;
    btnExportToCSV: TBitBtn;
    btnExportToExcel: TBitBtn;
    btnInfo: TBitBtn;
    btnPrint: TBitBtn;
    btnSavePriceToDB: TBitBtn;
    btnSubscribeHistoricalData: TBitBtn;
    pnlOptions: TPanel;
    procedure aClearExecute(Sender: TObject);
    procedure aClearHistoricalDBExecute(Sender: TObject);
    procedure aClearHistoricalDBUpdate(Sender: TObject);
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aInfoExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aSavePriceToDBExecute(Sender: TObject);
    procedure aSavePriceToDBUpdate(Sender: TObject);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private const
    C_COL_SYMBOL        = 0;
    C_COL_BARSIZE       = 1;
    C_COL_DURATION      = 2;
    C_COL_TICKTYPE      = 3;
    C_COL_DATE          = 4;
    C_COL_OPEN          = 5;
    C_COL_HIGH          = 6;
    C_COL_LOW           = 7;
    C_COL_CLOSE         = 8;
    C_COL_WAP           = 9;
    C_COL_VOLUME        = 10;

    C_IDENTITY_NAME = 'frameHistoricalData';
    DurationString: array [0 .. 4] of string = ('S', 'D', 'W', 'M', 'Y');
  private
    FHistoricalDataList: TDictionary<Integer, THistoricalDataParams>;
    FIsLockControls: Boolean;
    FLastDataId: Integer;
    FOnLockControl: TLockControlEvent;
    FOnUnlockControl: TUnlockControlEvent;

    //implementation IError
    procedure OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
    function GetInstance: TObject;
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure Deinitialize; override;
    function CheckPacingViolations(const ASymbol, ATickType: string; const AContractId, ADuration, ADurationTimeUnits: Integer; const ABarSize: TIABChartBarSize; const ADateEnd: TDateTime): TPacingViolations;
    procedure AddHistoricalChartData(const ADataId, AItem, ACount: Integer; const AChartData: TIABHistoricalChartData; const ADateBegin: TDateTime);
    procedure AddHistoricalParameters(const ADataId: Integer; const ASymbol, ATickType: string; const AContractId, ADuration, ADurationTimeUnits: Integer; const ABarSize: TIABChartBarSize; const ADateEnd: TDateTime);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnLockControl  : TLockControlEvent   read FOnLockControl   write FOnLockControl;
    property OnUnlockControl: TUnlockControlEvent read FOnUnlockControl write FOnUnlockControl;
  end;

implementation

{$R *.dfm}

{ TframeHistoricalData }

constructor TframeHistoricalData.Create(AOwner: TComponent);
begin
  inherited;
  vstTree.NodeDataSize := SizeOf(THistoricalData);
  FHistoricalDataList := TDictionary<Integer, THistoricalDataParams>.Create;
  FIsLockControls := False;
  FLastDataId := -1;
end;

destructor TframeHistoricalData.Destroy;
begin
  vstTree.Clear;
  FreeAndNil(FHistoricalDataList);
  inherited;
end;

function TframeHistoricalData.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

function TframeHistoricalData.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TframeHistoricalData.Initialize;
begin
  inherited;
//  TMonitorTree.Initialize(vstTree);
//  TStoreHelper.LoadFromXml(vstTree, GetIdentityName + C_IDENTITY_COLUMNS_NAME);
//  Self.Caption := GetIdentityName;

  TPublishers.ErrorPublisher.Subscribe(Self);
  TMonitorTree.Initialize(vstTree);
end;

procedure TframeHistoricalData.Deinitialize;
begin
  TPublishers.ErrorPublisher.Unsubscribe(Self);
  inherited;
end;

function TframeHistoricalData.CheckPacingViolations(const ASymbol, ATickType: string; const AContractId, ADuration, ADurationTimeUnits: Integer; const ABarSize: TIABChartBarSize; const ADateEnd: TDateTime): TPacingViolations;
var
  HistoricalDataParams: THistoricalDataParams;
  i: Integer;
  CurrentTime: TDateTime;
begin
  Result := pvNone;
  //Making identical historical data requests within 15 seconds.
  if (FLastDataId > 0) then
  begin
    HistoricalDataParams := FHistoricalDataList.Items[FLastDataId];
    if (HistoricalDataParams.Symbol = ASymbol) and
       (HistoricalDataParams.TickType = ATickType) and
       (HistoricalDataParams.ContractId = AContractId) and
       (HistoricalDataParams.Duration = ADuration) and
       (HistoricalDataParams.DurationTimeUnits = ADurationTimeUnits) and
       (HistoricalDataParams.BarSize = ABarSize) and
       (HistoricalDataParams.DateEnd = ADateEnd) and
       (SecondsBetween(HistoricalDataParams.StartTime, Now) <= 15) then
      Exit(pvIdenticalPer15Sec);
  end;

  i := 0;
  CurrentTime := Now;
  //Making six or more historical data requests for the same Contract, Exchange and Tick Type within two seconds.
  for HistoricalDataParams in FHistoricalDataList.Values do
  begin
    if (HistoricalDataParams.ContractId = AContractId) and
       (HistoricalDataParams.TickType = ATickType) and
       (SecondsBetween(HistoricalDataParams.StartTime, CurrentTime) <= 2) then
     Inc(i);
   if (i >= 6) then
     Exit(pv6Per2Sec);
  end;

  i := 0;
  //Making more than 60 requests within any ten minute period.
  for HistoricalDataParams in FHistoricalDataList.Values do
  begin
   if SecondsBetween(HistoricalDataParams.StartTime, CurrentTime) <= 600 then
     Inc(i);
   if (i >= 60) then
     Exit(pv60Per10Min);
  end;
end;

procedure TframeHistoricalData.AddHistoricalChartData(const ADataId, AItem, ACount: Integer; const AChartData: TIABHistoricalChartData; const ADateBegin: TDateTime);
var
  ChartDate: TDateTime;
  Data: PHistoricalData;
  Node: PVirtualNode;
  HistoricalDataParams: THistoricalDataParams;
begin
  Node := nil;
  if FHistoricalDataList.ContainsKey(ADataId) then
  begin
    ChartDate := GetDateFromHistoricalChart(AChartData.Date);
    if (ChartDate >= ADateBegin) then
    begin
      vstTree.BeginUpdate;
      try
        HistoricalDataParams := FHistoricalDataList.Items[ADataId];
        Node := vstTree.AddChild(nil);
        Data := Node^.GetData;
        Data.Symbol            := HistoricalDataParams.Symbol;
        Data.ContractId        := HistoricalDataParams.ContractId;
        Data.BarSize           := HistoricalDataParams.BarSize;
        Data.Duration          := HistoricalDataParams.Duration;
        Data.DurationTimeUnits := HistoricalDataParams.DurationTimeUnits;
        Data.DateEnd           := HistoricalDataParams.DateEnd;
        Data.TickType          := HistoricalDataParams.TickType;
        Data.ChartDate         := ChartDate;
        Data.Open              := AChartData.Open;
        Data.High              := AChartData.High;
        Data.Low               := AChartData.Low;
        Data.Close             := AChartData.Close;
        Data.Wap               := AChartData.Wap;
        Data.Volume            := AChartData.Volume;
        Data.ReceivingDate     := Now;
      finally
        vstTree.EndUpdate;
      end;
    end;

    if (AItem = ACount) then
    begin
      if Assigned(Node) then
      begin
        vstTree.Selected[Node] := True;
        vstTree.FocusedNode    := Node;
        vstTree.ScrollIntoView(Node, True);
      end;
      if Assigned(OnUnlockControl) then
        OnUnlockControl;
    end;
  end;
end;

procedure TframeHistoricalData.AddHistoricalParameters(const ADataId: Integer; const ASymbol, ATickType: string; const AContractId, ADuration, ADurationTimeUnits: Integer; const ABarSize: TIABChartBarSize; const ADateEnd: TDateTime);
var
  HistoricalDataParams: THistoricalDataParams;
begin
  FLastDataId := ADataId;
  if FHistoricalDataList.ContainsKey(ADataId) then
    HistoricalDataParams := FHistoricalDataList.Items[ADataId]
  else
    HistoricalDataParams := Default (THistoricalDataParams);

  HistoricalDataParams.Symbol            := ASymbol;
  HistoricalDataParams.ContractId        := AContractId;
  HistoricalDataParams.BarSize           := ABarSize;
  HistoricalDataParams.Duration          := ADuration;
  HistoricalDataParams.DurationTimeUnits := ADurationTimeUnits;
  HistoricalDataParams.DateEnd           := ADateEnd;
  HistoricalDataParams.TickType          := ATickType;
  HistoricalDataParams.StartTime         := Now;
  FHistoricalDataList.AddOrSetValue(ADataId, HistoricalDataParams);
end;

procedure TframeHistoricalData.OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
begin
  if (ErrorCode = 162) or   //Historical market data Service error message
     (ErrorCode = 165) or   //Historical market Data Service query message
     (ErrorCode = 366) or   //No historical data query found for ticker id
     (ErrorCode = 386) or   //Duplicate ticker ID for API historical data query.
     (ErrorCode = 321) then //Historical data requested duration is invalid
    TThread.Queue(nil,
      procedure
      begin
        FIsLockControls := False;
        if Assigned(OnUnlockControl) then
          OnUnlockControl(ErrorMsg);
      end);
end;

procedure TframeHistoricalData.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PHistoricalData;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TframeHistoricalData.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PHistoricalData;
begin
  Data := Node^.GetData;
  case Column of
    C_COL_SYMBOL:
      CellText := Data^.Symbol;
    C_COL_BARSIZE:
      CellText := ChartBarSizeString[Data^.BarSize];
    C_COL_DURATION:
      CellText := Data.Duration.ToString + ' ' + DurationString[Data^.DurationTimeUnits];
    C_COL_DATE:
      CellText := FormatDateTime('dd.mm.yyyy hh:nn:ss', Data^.ChartDate);
    C_COL_OPEN:
      CellText := Format('%.2f', [Data^.Open]);
    C_COL_HIGH:
      CellText :=  Format('%.2f', [Data^.High]);
    C_COL_LOW:
      CellText :=  Format('%.2f', [Data^.Low]);
    C_COL_CLOSE:
      CellText :=  Format('%.2f', [Data^.Close]);
    C_COL_WAP:
      CellText :=  Format('%.2f', [Data^.Wap]);
    C_COL_VOLUME:
      CellText := Data^.Volume.ToString;
    C_COL_TICKTYPE:
      CellText := Data^.TickType;
  else
    CellText := '';
  end;
end;

procedure TframeHistoricalData.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PHistoricalData;
begin
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  case Column of
    C_COL_SYMBOL:
      Result := CompareText(Data1^.Symbol, Data2^.Symbol);
    C_COL_BARSIZE:
      Result := CompareValue(Ord(Data1^.BarSize), Ord(Data2^.BarSize));
    C_COL_DURATION:
      Result := CompareValue(Data1^.DurationTimeUnits, Data2^.DurationTimeUnits);
    C_COL_DATE:
      Result := CompareValue(Data1^.ChartDate, Data2^.ChartDate);
    C_COL_OPEN:
      Result := CompareValue(Data1^.Open, Data2^.Open);
    C_COL_HIGH:
      Result := CompareValue(Data1^.High, Data2^.High);
    C_COL_LOW:
      Result := CompareValue(Data1^.Low, Data2^.Low);
    C_COL_CLOSE:
      Result := CompareValue(Data1^.Close, Data2^.Close);
    C_COL_WAP:
      Result := CompareValue(Data1^.Wap, Data2^.Wap);
    C_COL_VOLUME:
      Result := CompareValue(Data1^.Volume, Data2^.Volume);
    C_COL_TICKTYPE:
      Result := CompareText(Data1^.TickType, Data2^.TickType);
  end;
end;

procedure TframeHistoricalData.aClearExecute(Sender: TObject);
begin
  vstTree.Clear;
end;

procedure TframeHistoricalData.aColumnSettingsExecute(Sender: TObject);
begin
  TStoreHelper.ShowColumnSettings(vstTree, C_IDENTITY_NAME, 0);
end;

procedure TframeHistoricalData.aExportToCSVExecute(Sender: TObject);
begin
  ExportToCSV;
end;

procedure TframeHistoricalData.aExportToExcelExecute(Sender: TObject);
begin
  ExportToExcel;
end;

procedure TframeHistoricalData.aInfoExecute(Sender: TObject);
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine.Append(C_HTML_BREAK).AppendLine
      .Append(THtmlLib.GetTableTag(VarArrayOf([C_HTML_NBSP, 'Y', 'M', 'W', 'D', 'S']), 'Dependences of parameters of historical data')).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('1M'), 5, 12, 52, 60, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('1W'), 5, 12, 52, 60, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('1 day'), 5, 12, 52, 60, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('8 hours'), C_HTML_NBSP, 1, 4, 34, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('8 hours'), C_HTML_NBSP, 1, 4, 34, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('4 hours'), C_HTML_NBSP, 1, 4, 34, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('3 hours'), C_HTML_NBSP, 1, 4, 34, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('2 hours'), C_HTML_NBSP, 1, 4, 34, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('1 hour'), C_HTML_NBSP, 1, 4, 34, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('30 mins'), C_HTML_NBSP, 1, 4, 34, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('20 mins'), C_HTML_NBSP, C_HTML_NBSP, 3, 27, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('15 mins'), C_HTML_NBSP, C_HTML_NBSP, 2, 20, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('10 mins'), C_HTML_NBSP, C_HTML_NBSP, 1, 13, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('5 mins'), C_HTML_NBSP, C_HTML_NBSP, 1, 10, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('3 mins'), C_HTML_NBSP, C_HTML_NBSP, 1, 10, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('2 mins'), C_HTML_NBSP, C_HTML_NBSP, 1, 10, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('1 min'), C_HTML_NBSP, C_HTML_NBSP, 1, 10, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('30 secs'), C_HTML_NBSP, C_HTML_NBSP, C_HTML_NBSP, 1, 86400]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('15 secs'), C_HTML_NBSP, C_HTML_NBSP, C_HTML_NBSP, C_HTML_NBSP, 30000]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('10 secs'), C_HTML_NBSP, C_HTML_NBSP, C_HTML_NBSP, C_HTML_NBSP, 20000]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('5 secs'), C_HTML_NBSP, C_HTML_NBSP, C_HTML_NBSP, C_HTML_NBSP, 10000]))).AppendLine
      .Append(THtmlLib.GetTableLineTag(VarArrayOf([THtmlLib.GetBoldText('1 sec'), C_HTML_NBSP, C_HTML_NBSP, C_HTML_NBSP, C_HTML_NBSP, 2000]))).AppendLine
      .Append(C_HTML_TABLE_CLOSE).Append(C_HTML_BREAK).AppendLine.Append(C_HTML_BODY_CLOSE).AppendLine;
    TInformationDialog.ShowMessage(sb.ToString, 'HistoricalData');
  finally
    FreeAndNil(sb);
  end;
end;

procedure TframeHistoricalData.aPrintExecute(Sender: TObject);
begin
  Print;
end;

procedure TframeHistoricalData.aSavePriceToDBExecute(Sender: TObject);
resourcestring
  rsSave = 'Saving prices to DB';
var
  Data: PHistoricalData;
  Node: PVirtualNode;
begin
  System.Threading.TTask.Create(
    procedure()
    begin
      TThread.NameThreadForDebugging('Frame.HistoricalData.SavePriceToDB');
      TThread.Queue(nil,
        procedure
        begin
          FIsLockControls := True;
          if Assigned(OnLockControl) then
            OnLockControl(rsSave);
        end);
      try
        Node := vstTree.GetFirst;
        while Assigned(Node) do
        begin
          Data := Node^.GetData;
          TMonitorLists.PriceCache.AddPrice(Data^.ContractId, ttHigh, Data^.High, Data^.ChartDate, True);
          TMonitorLists.PriceCache.AddPrice(Data^.ContractId, ttClose, Data^.Close, Data^.ChartDate, True);
          TMonitorLists.PriceCache.AddPrice(Data^.ContractId, ttLow, Data^.Low, Data^.ChartDate, True);
          TMonitorLists.PriceCache.AddPrice(Data^.ContractId, ttOpen, Data^.Open, Data^.ChartDate, True);
          TMonitorLists.PriceCache.AddPrice(Data^.ContractId, ttVolume, Data^.Volume, Data^.ChartDate, True);
          Node := Node.NextSibling;
        end;
      finally
        TThread.Queue(nil,
          procedure
          begin
            FIsLockControls := False;
            if Assigned(OnUnlockControl) then
              OnUnlockControl;
          end);
      end;
    end).Start;
end;

procedure TframeHistoricalData.aClearHistoricalDBExecute(Sender: TObject);
resourcestring
  rsDelete = 'Delete records from DB';
  rsQuestion = '%d records of %s instrument will be deleted from DB. Continue deleted?';
  C_DELETE_SQL = 'delete from TICK_DATA where IS_HISTORICAL=1 and CONID=:ContractId';
  C_SELECT_SQL = 'select count(*) as cnt from TICK_DATA where IS_HISTORICAL=1 and CONID=';
var
  Data: PHistoricalData;
  Query: TFDQuery;
  cnt: Integer;
begin
  if Assigned(vstTree.FocusedNode) then
  begin
    Data := vstTree.FocusedNode^.GetData;
    cnt := DMod.GetIntegerValueFromSQL(C_SELECT_SQL + Data^.ContractId.ToString, 'cnt', DMod.ConnectionFeed);
    if (TMessageDialog.ShowQuestion(Format(rsQuestion, [cnt, Data^.Symbol])) = mrYes) then
    begin
      TThread.Queue(nil,
        procedure
        begin
          FIsLockControls := True;
          if Assigned(OnLockControl) then
            OnLockControl(rsDelete);
        end);
      Query := TFDQuery.Create(nil);
      try
        DMod.CheckConnect;
        Query.Connection := DMod.ConnectionFeed;
        Query.SQL.Text := C_DELETE_SQL;
        Query.ParamByName('ContractId').AsInteger := Data^.ContractId;
        try
          Query.Prepare;
          Query.ExecSQL;
          Query.Connection.Transaction.CommitRetaining;
          TMessageDialog.ShowInfo(rsSuccessful);
        except
          on E: Exception do
          begin
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ClearHistoricalDB', E.Message + TDModUtils.GetQueryInfo(Query));
            raise;
          end;
        end;
      finally
        FreeAndNil(Query);
        TThread.Queue(nil,
          procedure
          begin
            FIsLockControls := False;
            if Assigned(OnUnlockControl) then
              OnUnlockControl;
          end);
      end;
    end;
  end;
end;

procedure TframeHistoricalData.aClearHistoricalDBUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstTree.IsEmpty and Assigned(vstTree.FocusedNode) and not FIsLockControls;
end;

procedure TframeHistoricalData.aSavePriceToDBUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstTree.IsEmpty and not FIsLockControls;
end;

{ THistoricalData }

procedure THistoricalData.Clear;
begin
  Self := Default(THistoricalData);
end;

{ TPacingViolationsHelper }

function TPacingViolationsHelper.ToString: string;
begin
  Result := PacingViolationsName[Self];
end;

end.
