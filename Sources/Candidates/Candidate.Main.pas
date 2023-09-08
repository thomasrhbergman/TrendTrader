unit Candidate.Main;
{$WARN SYMBOL_PLATFORM OFF}

interface

{$REGION 'Region uses'}
uses
  System.Actions, System.Classes, System.DateUtils, System.Generics.Collections, System.Generics.Defaults, System.ImageList,
  System.Math, System.IOUtils, System.SyncObjs, System.SysUtils, System.Threading, System.UITypes, System.Variants,
  Vcl.ActnList, Vcl.Buttons, Vcl.ComCtrls, Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.ImgList,
  Vcl.Menus, Vcl.Samples.Spin, Vcl.StdCtrls, Vcl.ToolWin, Winapi.ActiveX, Winapi.Messages, Winapi.Windows, 
  IABFunctions, IABSocketAPI, IABSocketAPI_const,
  VirtualTrees, InstrumentList, DockForm.LogView, MessageDialog, Order.Utils, Candidate.TickColumn, Candidate.EmbargoColumn,
  AutoTrades, DaModule.Constants, Candidate.EditWeight, Candidate.Market, Candidate.StaticLists, Utils, DebugWriter,
  CustomForms, HtmlConsts, HtmlLib, InformationDialog, XmlFiles, Entity.Sokid, AutoTrades.Dock, BrokerHelperAbstr,
  DaImages, DaModule, Data.DB, Document, Candidate.Types, Monitor.Types, Global.Types, Monitor.Interfaces, Frame.DocumentsTree,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Publishers.Interfaces, IABFunctions.RequestsQueue, Common.Types,
  IABFunctions.MarketData, AutoTrades.Types, DaModule.Utils, VirtualTrees.Helper, Global.Resources, Vcl.Themes,
  Monitor.Info, Publishers, System.Notification, Chart.Trade, Vcl.NumberBox, IABFunctions.Helpers, System.Types,
  MonitorTree.Helper, Candidate.GradientColumn, VirtualTrees.Types, FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.Comp.DataSet, ListForm, Candidate.PriceChangeColumn,
  Candidate.EmulatePriceChange, MonitorTree.Factory, NNfunctions.Types;
{$ENDREGION}

type TGradientRecord = record
  Gradient: Double;
  Corridor: Double;
  LastPosition: Double;
  Calculated: Boolean;
end;

type
  TfrmCandidateMain = class(TCustomForm, ICandidateMarket,
                                       IScanner,
                                       IOrderState,
                                       IAutoTrade,
                                       IInstrumentSpecDetails,
                                       IUpdateFeeds,
                                       IHistoricalData,
                                       IError)
    aAddEmbargoColumn: TAction;
    aChangeWeigthValue: TAction;
    aChangeWeigthValueColumn: TAction;
    aCopySelectedNodeToMonitor: TAction;
    ActionList: TActionList;
    aDeleteAllNodes: TAction;
    aDeleteColumn: TAction;
    aExecute: TAction;
    aExecuteDuplicateOrder: TAction;
    aExit: TAction;
    aInformationDialog: TAction;
    aInstrumentInfo: TAction;
    aOpenAutoTradeTemplate: TAction;
    aOpenGradientColumn: TAction;
    aOpenIBScanning: TAction;
    aOpenStaticList: TAction;
    aOpenTickColumns: TAction;
    aSaveAutoTradeTemplate: TAction;
    aSaveAutoTradeTemplateAs: TAction;
    aShowColumnDetails: TAction;
    aShowGlobalSettings: TAction;
    aShowPriceHistory: TAction;
    aShowTradeChart: TAction;
    aShowWeighted: TAction;
    BalloonHint: TBalloonHint;
    btnAddEmbargoColumn: TBitBtn;
    btnOpenGradientColumn: TBitBtn;
    btnOpenIBScanning: TBitBtn;
    btnOpenStaticList: TBitBtn;
    btnOpenTickColumns: TBitBtn;
    cbAllowSendDuplicateOrder: TCheckBox;
    cbAutoOrderActive: TCheckBox;
    cbAutoRefresh: TCheckBox;
    cbDurationTimeUnits: TComboBox;
    cbHistDataKeepUpdated: TCheckBox;
    cbOrderCurrency: TComboBox;
    cbOrderCurrencyAdd: TSpeedButton;
    cbSubscribeHistoricalData: TCheckBox;
    cbValidBarSize: TComboBox;
    edAutoTradeTemplate: TEdit;
    edDuration: TNumberBox;
    gbAutoOrder: TGroupBox;
    gbHistoricalOptions: TGroupBox;
    gbIsolate: TGroupBox;
    gbSpecificationAutoTrade: TGroupBox;
    lbIsolate: TListBox;
    lblAutoTradeTemplateCaption: TLabel;
    lblDuration: TLabel;
    lblMaxRows: TLabel;
    lblSingleOrderAmount: TLabel;
    lblTotalOrderAmount: TLabel;
    lblValidBarSize: TLabel;
    miChangeWeigthNodeValue: TMenuItem;
    miCopySelectedNodeToMonitor: TMenuItem;
    miDeleteAllNodes: TMenuItem;
    miDeleteColumn: TMenuItem;
    miExit: TMenuItem;
    miGetInstrumentInfo: TMenuItem;
    miSaveAs: TMenuItem;
    miSep: TMenuItem;
    miShowColumnScanSettings: TMenuItem;
    miShowPriceHistory: TMenuItem;
    miShowTradeChart: TMenuItem;
    pnlAutoOrder: TPanel;
    pnlAutoOrderTop: TPanel;
    pnlMain: TPanel;
    pnlOptions: TPanel;
    PopupMenu: TPopupMenu;
    rgWeightedFeed: TRadioGroup;
    sbMain: TStatusBar;
    seMaxRows: TSpinEdit;
    seSingleOrderAmount: TSpinEdit;
    seTotalOrderAmount: TSpinEdit;
    TimerCalculateGradient: TTimer;
    TimerEmbargo: TTimer;
    vstCandidate: TVirtualStringTree;
    ilBalloonHint: TImageList;
    lblMaxNumberOrder: TLabel;
    seMaxNumberOrder: TSpinEdit;
    pnlContent: TPanel;
    lbColumns: TListBox;
    pnlButtons: TPanel;
    btnSave: TBitBtn;
    btnCancel: TBitBtn;
    btnAddPriceChange: TBitBtn;
    aAddPriceChangeColumn: TAction;
    mmEmulatePriceChange: TMenuItem;
    cbAlwaysMax: TCheckBox;
    cbRepeatInstruments: TCheckBox;
    meLog: TMemo;
    splLog: TSplitter;
    pnlLog: TPanel;
    pnlLogOptions: TPanel;
    rgGradientCalcMode: TRadioGroup;
    procedure aAddEmbargoColumnExecute(Sender: TObject);
    procedure aChangeWeigthValueColumnExecute(Sender: TObject);
    procedure aChangeWeigthValueColumnUpdate(Sender: TObject);
    procedure aChangeWeigthValueExecute(Sender: TObject);
    procedure aChangeWeigthValueUpdate(Sender: TObject);
    procedure aCopySelectedNodeToMonitorExecute(Sender: TObject);
    procedure aCopySelectedNodeToMonitorUpdate(Sender: TObject);
    procedure aDeleteAllNodesExecute(Sender: TObject);
    procedure aDeleteColumnExecute(Sender: TObject);
    procedure aExecuteDuplicateOrderExecute(Sender: TObject);
    procedure aExecuteExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aInformationDialogExecute(Sender: TObject);
    procedure aInstrumentInfoExecute(Sender: TObject);
    procedure aInstrumentInfoUpdate(Sender: TObject);
    procedure aOpenAutoTradeTemplateExecute(Sender: TObject);
    procedure aOpenGradientColumnExecute(Sender: TObject);
    procedure aOpenIBScanningExecute(Sender: TObject);
    procedure aOpenStaticListExecute(Sender: TObject);
    procedure aOpenTickColumnsExecute(Sender: TObject);
    procedure aSaveAutoTradeTemplateAsExecute(Sender: TObject);
    procedure aSaveAutoTradeTemplateExecute(Sender: TObject);
    procedure aShowColumnDetailsExecute(Sender: TObject);
    procedure aShowColumnDetailsUpdate(Sender: TObject);
    procedure aShowGlobalSettingsExecute(Sender: TObject);
    procedure aShowPriceHistoryExecute(Sender: TObject);
    procedure aShowTradeChartExecute(Sender: TObject);
    procedure aShowTradeChartUpdate(Sender: TObject);
    procedure aShowWeightedExecute(Sender: TObject);
    procedure cbAutoOrderActiveClick(Sender: TObject);
    procedure cbAutoRefreshClick(Sender: TObject);
    procedure cbOrderCurrencyAddClick(Sender: TObject);
    procedure cbOrderCurrencyKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnGUIToAutoTradeInfo(Sender: TObject);
    procedure rgWeightedFeedClick(Sender: TObject);
    procedure TimerCalculateGradientTimer(Sender: TObject);
    procedure TimerEmbargoTimer(Sender: TObject);
    procedure vstCandidateAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure vstCandidateBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstCandidateCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstCandidateDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstCandidateDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstCandidateDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstCandidateFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstCandidateGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure vstCandidateGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstCandidateHeaderDblClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vstCandidateHeaderDragging(Sender: TVTHeader; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstCandidateHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure vstCandidateHeaderMouseDown(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vstCandidateInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstCandidatePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure btnSaveClick(Sender: TObject);
    procedure lbColumnsDblClick(Sender: TObject);
    procedure aAddPriceChangeColumnExecute(Sender: TObject);
    procedure mmEmulatePriceChangeClick(Sender: TObject);
  private const
    COL_INSTRUMENT  = 0;
    COL_RANKING_SUM = 1;
    C_FIXED_COLUMN_INDEX = 2;

    C_KEY_AUTO_ORDER_ACTIVE    = 'AutoOrderActive';
    C_KEY_ALLOW_SEND_DUPLICATE = 'AllowSendDuplicateOrder';
    C_KEY_AUTO_REFRESH         = 'AutoRefresh';
    C_KEY_INTERVAL             = 'Interval';
    C_KEY_MAX_ROWS             = 'MaxRows';
    C_KEY_SINGLE_ORDER_AMOUNT  = 'SingleOrderAmount';
    C_KEY_TOTAL_ORDER_AMOUNT   = 'TotalOrderAmount';
    C_KEY_ORDER_CURRENCY       = 'OrderCurrency';
    C_KEY_ORDER_CURRENCY_LIST  = 'OrderCurrencyList';
    C_KEY_ORDER_RANKING_SUM    = 'OrderRankingSum';
    C_KEY_ORDER_MAX_NUMBER     = 'MaxNumberOfOrder';
    C_SECTION_COLUMNS      = 'Columns';
    C_SECTION_MAIN         = 'Main';
    C_SECTION_Candidate_MAIN = 'CandidateMain';
    C_SECTION_TREE_PARAMS  = 'TreeParams';

    C_COLOUR_INDEX_GREEN  = 0;
    C_COLOUR_INDEX_YELLOW = 1;
    C_COLOUR_INDEX_NAVY   = 2;

    C_PANEL_TIME_UPDATE         = 1;
    C_PANEL_CREATED_ORDERS      = 3;
    C_PANEL_TIME_CREATED_ORDERS = 5;
    C_PANEL_TOTAL_NUM_SCANS     = 7;
    C_PANEL_SCAN_COUNT          = 9;
    C_PANEL_AUTO_REFRESH        = 10;
  private
    [weak] FAutoTradesEdit: IAutoTrade;
    FAutoTrade: TAutoTradeInfo;
    FCandidate: TCandidate;
    FCacheNodesWeight: TObjectDictionary<Integer, TExtraColumns>;
    FColumns: TAutoTradeColumns;
    FExplorationMode: Boolean;
    FInstrumentList: TFactorList;
    FIsPriceExists: Boolean;
    FIsLoaded: Boolean;
    FMonitor: IMonitor;
    FSilenceMode: Boolean;
    FSubscribedList: TList<Integer>;
    FActiveNodes: TList<PVirtualNode>;
    function AddOrGetColumn(var aColumnsInfo: TColumnsInfo): Integer;
    function CheckColumn(const aColumnsInfo: TColumnsInfo): Boolean;
    function CheckData: Boolean;
    function CheckRequiredEmbargoColumn: Boolean;
    function ColumnsInfoToXml: string;
    function GetCreatedOrdersCount: Integer;
    procedure AddFixedColumns;
    procedure ApplyAutoOrder;
    procedure AutoTradeInfoToGUI;
    procedure CalculateAllRankingSum;
    function CalcGradient(Id, GradientDuration, GradientMode: Integer): TGradientRecord;
    procedure CalculateGradient(const aNode: PVirtualNode; const aColumnsInfo: TColumnsInfo);
    procedure CalculateGradientLogTerm(const aNode: PVirtualNode; const aColumnsInfo: TColumnsInfo);
    procedure CalculateRankingSum(const aData: PInstrumentData);
    function CalculatePriceChangeWeight(const aNode: PVirtualNode; const aColumnsInfo: TColumnsInfo): currency;
    procedure CalculateValuesForAllNodes;
    procedure CancelScan;
    procedure CheckRankingCriteria(const aData: PInstrumentData);
    procedure CheckRankingCriteries;
    procedure CheckTradesState;
    procedure DeleteAllNodes;
    procedure EditWeigthColumn(const aColumnIndex: TColumnIndex);
    procedure ExecuteActions(aColumnsInfo: TColumnsInfo);
    procedure FillColumnList;
    procedure GUIToAutoTradeInfo;
    procedure PreExecutionEvaluation(const aData: PInstrumentData);
    procedure PreExecutionEvaluationOrders;
    procedure RequestMarketData(const aContractId: Integer);
    procedure ShowColumnDetails(const aColumnID: Integer);
    procedure ShowNotification(const aData: PInstrumentData; const aInfo: string = '');
    procedure ShowCandidateMarket(const aColumnID: Integer; const aScanOptions: TCandidateColumn);
    procedure ShowTradeChart(const aNode: PVirtualNode);
    procedure ShowBaloonHint(const aText: string; const aControl: TWinControl);
    procedure UpdateCaptions;
    procedure UpdateStatus(const aPanelNum: Byte; const aInfo: string); inline;

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IHistoricalData
    procedure OnHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
    //implementation IAutoTrade
    function GetAutoTradeInfo: TAutoTradeInfo;
    function GetTradesState: TTradesState;
    procedure CloseAutoTrade(const aSilenceMode: Boolean = False);
    procedure SetAutoTradeInfo(const aAutoTradeInfo: TAutoTradeInfo);
    procedure SetTradesState(const aValue: TTradesState);

    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    //implementation IScanner
    procedure OnScannerAdd(Sender: TObject; ScanId: Integer);
    procedure OnScannerCancel(Sender: TObject; ScanId: Integer);
    procedure OnScannerData(Sender: TObject; Scan: TIABScan);
    procedure OnScannerParam(Sender: TObject; Parameters: string);
    //implementation ICandidateMarket
    function GetMainTree: TBaseVirtualTree;
    procedure AddInstrument(const aInstruments: PArrayInstrumentData; aColumnsInfo: TColumnsInfo);
    procedure DecMarketInstances;
    procedure IncMarketInstances;
    procedure SetScanOptions(const aColumnId: Integer; const aScanOptions: TCandidateColumn);
    //implementation IInstrumentSpecDetails
    procedure OnInstrumentSpecDetails(Sender: TObject; Index: Integer);

    //implementation IOrderState
    procedure OnCloseOrder(const aTempId: Integer);
    procedure OnExecution(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrder(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrderNN(const aOrderList: array of TOrder);
    procedure OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
    procedure OnRebuildFromTWS(Sender: TObject);

    //implementation IError
    procedure OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);

    procedure CheckAlwaysMax(TempId: Integer; Status: TIABOrderState);

    procedure AddToLog(AText: string);

    procedure DockTo;
    procedure LoadParamsFromXml;
    procedure OpenSequenceRecord;
    procedure SaveParamsToXml;
    procedure SetCreatedOrdersCount(const Value: Integer);
    procedure SetValueFromPriceCache(const aData: PInstrumentData; const aColumnsInfo: TColumnsInfo);
    procedure DisableEditors;
    procedure RefreshColumnValues(const aColumnId: integer);

    property CreatedOrdersCount : Integer      read GetCreatedOrdersCount write SetCreatedOrdersCount;
    property IsLoaded           : Boolean      read FIsLoaded             write FIsLoaded;
    property TradesState        : TTradesState read GetTradesState        write SetTradesState;
    property ActiveNodes        : TList<PVirtualNode> read FActiveNodes write FActiveNodes;
  public
    class function Execute(const aAutoTrade: TAutoTradeInfo): IAutoTrade;
    class function ShowEditForm(aItem: TBaseClass; aDialogMode: TDialogMode): TModalResult; overload; override;
    class procedure ShowDocument; overload;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

{ TfrmCandidateMain }

resourcestring
  rcAutotradingNotActive      = 'Autotrading is not active' + sLineBreak;
  rcCreatedOrdersCountGreater = 'The number of orders created is greater than or equal to "Max Number of Order"' + sLineBreak;
  rsEmbargoRequired           = 'At least one Embargo is required';
  rsInterruptAutotrade        = 'Closing window will interrupt Candidate %s.' + sLineBreak + 'Continue?';
  rsNoInstruments             = 'No instrument have been added to the list';

class procedure TfrmCandidateMain.ShowDocument;
var
  frmCandidateMain: TfrmCandidateMain;
begin
  frmCandidateMain := TfrmCandidateMain.Create(Application);
  frmCandidateMain.IsLoaded := True;
  try
    frmCandidateMain.FExplorationMode := True;
    frmCandidateMain.Initialize;
    frmCandidateMain.TradesState := TTradesState.tsSuspended;
    AutoTradesControllerPublisher.UpdateState(frmCandidateMain);
    frmCandidateMain.Show;
    frmCandidateMain.UpdateCaptions;
  finally
    frmCandidateMain.IsLoaded := False;
  end;
  if (frmCandidateMain.WindowState = wsMinimized) then
    frmCandidateMain.WindowState := wsNormal;
end;

class function TfrmCandidateMain.ShowEditForm(aItem: TBaseClass; aDialogMode: TDialogMode): TModalResult;
var
  frmCandidateMain: TfrmCandidateMain;
begin
  frmCandidateMain := TfrmCandidateMain.Create(Application);
  try
    frmCandidateMain.IsLoaded := True;
    try
      frmCandidateMain.DialogMode := aDialogMode;
      frmCandidateMain.FCandidate := TCandidate(aItem);
      frmCandidateMain.Initialize;
      frmCandidateMain.OpenSequenceRecord;
      frmCandidateMain.AutoTradeInfoToGUI;
      frmCandidateMain.TradesState := TTradesState.tsSuspended;
      frmCandidateMain.UpdateCaptions;
    finally
      frmCandidateMain.IsLoaded := False;
    end;
    Result := frmCandidateMain.ShowModal;
    if (Result = mrOk) then
    begin
      //frmCandidateMain.Denitialize;
      //TCandidate(aItem).AssignFrom(frmCandidateMain.FCandidate);
    end;
  finally
    frmCandidateMain.Free;
  end;
end;

class function TfrmCandidateMain.Execute(const aAutoTrade: TAutoTradeInfo): IAutoTrade;
var
  CandidateMain: TfrmCandidateMain;
begin
  CandidateMain := TfrmCandidateMain.Create(Application);
  Result := CandidateMain;
  with CandidateMain do
  begin
    FAutoTrade := aAutoTrade;
    FCandidate := aAutoTrade.Candidate;
    Initialize;
    DisableEditors;
    TradesState := TTradesState.tsWorking;
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'Execute', 'CandidateMain',
                                       'Name='                    + FCandidate.Name                       + sLineBreak +
                                       'RecordId= '               + FCandidate.RecordId.ToString          + sLineBreak +
                                       'MaxNumberOrder='          + FCandidate.MaxNumberOrder.ToString);
    AutoTradesControllerPublisher.UpdateState(CandidateMain);
    AutoTradeInfoToGUI;
    if General.ViewMarketScanner then
    begin
      DockTo;
      Show;
    end;
    OpenSequenceRecord;
    ApplyAutoOrder;
  end;
end;

function TfrmCandidateMain.GetTradesState: TTradesState;
begin
  //Result := FCandidate.TradesState;
  Result := tsNotConsidered; //!!!!
end;

procedure TfrmCandidateMain.FormCreate(Sender: TObject);
begin
  inherited;
  vstCandidate.NodeDataSize := SizeOf(TInstrumentData);
  FColumns := TObjectDictionary<Integer, TColumnsInfo>.Create;
  FSubscribedList := TList<Integer>.Create;

  FExplorationMode := False;
  FIsLoaded := False;
  FInstrumentList  := TFactorList.Create([doOwnsValues]);
  FCacheNodesWeight := TObjectDictionary<Integer, TExtraColumns>.Create;

  TPublishers.FeedPublisher.Subscribe(Self);
  TPublishers.HistoricalDataPublisher.Subscribe(Self);
  TPublishers.InstrumentSpecDetailsPublisher.Subscribe(Self);
  TPublishers.ScannerPublisher.Subscribe(Self);
  TPublishers.OrderStatePublisher.Subscribe(Self);
  TPublishers.ErrorPublisher.Subscribe(Self);

  if not Supports(Application.MainForm, IMonitor, FMonitor) then
    raise Exception.Create(rsNotSupportsIMonitor);

  FActiveNodes := TList<PVirtualNode>.Create;
end;

procedure TfrmCandidateMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Denitialize;
  Action := caFree;
  Self.ManualDock(nil, nil, alNone);
end;

procedure TfrmCandidateMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if not FSilenceMode then
  begin
    if Showing and (CreatedOrdersCount > 0) then
      CanClose := TMessageDialog.ShowQuestion(Format(rsInterruptAutotrade, [FCandidate.Name])) = mrYes
    else if not FExplorationMode then
      CanClose := CheckRequiredEmbargoColumn
    else if FExplorationMode then
      CanClose := CheckData;
  end;
end;

procedure TfrmCandidateMain.FormDestroy(Sender: TObject);
begin
  TPublishers.FeedPublisher.Unsubscribe(Self);
  TPublishers.HistoricalDataPublisher.Unsubscribe(Self);
  TPublishers.InstrumentSpecDetailsPublisher.Unsubscribe(Self);
  TPublishers.ScannerPublisher.Unsubscribe(Self);
  TPublishers.OrderStatePublisher.Unsubscribe(Self);
  TPublishers.ErrorPublisher.Unsubscribe(Self);
  //FCandidate.TradesState := TTradesState.tsNotConsidered;
  AutoTradesControllerPublisher.UpdateState(Self);
  FreeAndNil(FSubscribedList);
  FreeAndNil(FColumns);
  FreeAndNil(FInstrumentList);
  FreeAndNil(FCacheNodesWeight);
  FreeAndNil(FActiveNodes);
  inherited;
end;

procedure TfrmCandidateMain.FormShow(Sender: TObject);
//var
//  MainForm: TForm;
begin
  inherited;
//  MainForm  := Application.MainForm;
//  Self.Left := MainForm.Left + MainForm.Width;
//  Self.Top  := MainForm.Top;
  AddToLog(DateTimeToStr(Now));
end;

procedure TfrmCandidateMain.Initialize;
begin
  TMonitorTree.Initialize(vstCandidate);
  LoadParamsFromXml;
  TimerCalculateGradient.Enabled := True;

  pnlAutoOrder.Visible             := FExplorationMode;
  aOpenAutoTradeTemplate.Visible   := FExplorationMode;
  aSaveAutoTradeTemplate.Visible   := FExplorationMode;
  aSaveAutoTradeTemplateAs.Visible := FExplorationMode;

  cbValidBarSize.Items.Clear;
  for var ChartBarSize := Low(TIABChartBarSize) to High(TIABChartBarSize) do
    cbValidBarSize.Items.AddObject(ChartBarSizeString[ChartBarSize], TObject(ChartBarSize));
  cbValidBarSize.ItemIndex := Ord(bs30sec);

  cbDurationTimeUnits.Items.Clear;
  cbDurationTimeUnits.Items.AddObject('S Seconds', TObject(IAB_TIME_UNIT_SEC)); //IAB_TIME_UNIT_SEC   = 0;
  cbDurationTimeUnits.Items.AddObject('D Day', TObject(IAB_TIME_UNIT_DAY));     //IAB_TIME_UNIT_DAY   = 1;
  cbDurationTimeUnits.Items.AddObject('W Week', TObject(IAB_TIME_UNIT_WEEK));   //IAB_TIME_UNIT_WEEK  = 2;
  cbDurationTimeUnits.Items.AddObject('M Month', TObject(IAB_TIME_UNIT_MONTH)); //IAB_TIME_UNIT_MONTH = 3;
  cbDurationTimeUnits.Items.AddObject('Y Year', TObject(IAB_TIME_UNIT_YEAR));   //IAB_TIME_UNIT_YEAR  = 4;
  cbDurationTimeUnits.ItemIndex := 0;
end;

procedure TfrmCandidateMain.lbColumnsDblClick(Sender: TObject);
var
  ColumnId: Integer;
  ColumnsInfo: TColumnsInfo;
begin
  if (lbColumns.ItemIndex > -1) then
  begin
    ColumnId := Integer(lbColumns.Items.Objects[lbColumns.ItemIndex]);
    if FColumns.ContainsKey(ColumnId) then
    begin
      ColumnsInfo := FColumns.Items[ColumnId];
      ShowColumnDetails(ColumnsInfo.ColumnId);
    end;
  end;
end;

procedure TfrmCandidateMain.ShowNotification(const aData: PInstrumentData; const aInfo: string = '');
resourcestring
  rsNotification = '%s (%d), Is Criteria: %s, Info: %s';
var
  Notification: TNotification;
begin
  Notification := DMod.NotificationCenter.CreateNotification;
  try
    Notification.Name := 'CandidateMainNotification';
    if Assigned(aData) then
    begin
      Notification.Title     := aData^.Description;
      Notification.AlertBody := Format(rsNotification, [aData^.LocalSymbol,
                                                        aData^.Id,
                                                        BoolToStr(aData^.IsCriteria, True),
                                                        aInfo]);
      TThread.Queue(nil,
        procedure
        begin
          DMod.NotificationCenter.PresentNotification(Notification);
        end)
    end
    else if not aInfo.IsEmpty then
    begin
      Notification.Title := FCandidate.Name;
      Notification.AlertBody := aInfo;
      TThread.Queue(nil,
        procedure
        begin
          DMod.NotificationCenter.PresentNotification(Notification);
        end)
    end;
  finally
    FreeAndNil(Notification);
  end;
end;

procedure TfrmCandidateMain.Denitialize;
var
  Data: PInstrumentData;
  Node: PVirtualNode;
  InstrumentItem: TInstrumentItem;
begin
  SaveParamsToXml;
  TimerCalculateGradient.Enabled := False;
  CancelScan;

  for var DataId in FSubscribedList do
    if FSubscribedList.Contains(DataId) then
      IABClient.CancelHistoricalData(DataId);

  for InstrumentItem in FInstrumentList.Values do
    if Assigned(InstrumentItem) then
      for Node in InstrumentItem.NodeList do
        if Assigned(Node) then
        begin
          Data := Node^.GetData;
          if (Data^.Id > 0) then
            TIABMarket.CancelMarketData(Data^.Id);
        end;

  vstCandidate.Clear;
  if Assigned(FAutoTradesEdit) then
  begin
    FCandidate.ColumnsInfo := ColumnsInfoToXml;
    //FAutoTradesEdit.SetAutoTradeInfo(FCandidate);
  end;
end;

procedure TfrmCandidateMain.DisableEditors;
begin
  edAutoTradeTemplate.ReadOnly := true;
  seMaxNumberOrder.ReadOnly := true;
  cbAlwaysMax.Enabled := false;
  cbRepeatInstruments.Enabled := false;
  pnlButtons.Visible := false;
end;

procedure TfrmCandidateMain.DockTo;
var
  Dock: TfrmAutoTradesDock;
begin
  Dock := TfrmAutoTradesDock.GetDock;
  Dock.Show;
  Self.Caption := FCandidate.Name;
  Self.DragKind := dkDock;
  Self.DragMode := dmAutomatic;
  Self.ManualDock(Dock.pcTrades, Dock.pcTrades, alClient);
end;

function TfrmCandidateMain.CheckData: Boolean;
resourcestring
  C_SQL_CHECK_TEXT = 'SELECT COUNT(*) AS CNT FROM CANDIDATES ' + sLineBreak +
                      'WHERE (NAME = ''%s'') AND (ID <> :ID);';
var
  Msg: string;
begin
  Msg := '';
  if FCandidate.Name.Trim.IsEmpty then
    Msg := Msg + Format(rcRequiredValue, ['Candidate Name']);
  if (DMod.GetIntegerValueFromSQL(Format(C_SQL_CHECK_TEXT, [FCandidate.Name.Trim, FCandidate.RecordId]), 'CNT') > 0) then
    Msg := Msg + rcNameNotUnique;
  if (FCandidate.MaxNumberOrder <= 0) then
    Msg := Msg + Format(rcRequiredValue, ['Max number of order']);
  Result := Msg.IsEmpty;
  if not Result then
    Result := TMessageDialog.ShowQuestion(Msg + sLineBreak + rcQuestionContinueEditing) = mrNo;
end;

procedure TfrmCandidateMain.LoadParamsFromXml;
begin
  if FExplorationMode then
  begin
    cbOrderCurrency.Items.Text             := General.XMLFile.ReadString(C_SECTION_Candidate_MAIN, C_KEY_ORDER_CURRENCY_LIST, C_DEFAULT_CURRENCY);
    cbOrderCurrency.Text                   := General.XMLFile.ReadString(C_SECTION_Candidate_MAIN, C_KEY_ORDER_CURRENCY, C_DEFAULT_CURRENCY);
    FCandidate.MaxNumberOrder          := General.XMLFile.ReadInteger(C_SECTION_Candidate_MAIN, C_KEY_ORDER_MAX_NUMBER, 0);
  end;
  AutoTradeInfoToGUI;
end;

procedure TfrmCandidateMain.mmEmulatePriceChangeClick(Sender: TObject);
var
  Data: PInstrumentData;
begin
  if Assigned(vstCandidate.FocusedNode) then
  begin
    Data := vstCandidate.FocusedNode^.GetData;
    TfrmCandidateEmulatePriceChange.ShowDocument(Data^.Id);
  end;
end;

procedure TfrmCandidateMain.SaveParamsToXml;
begin
  try
    General.XMLFile.WriteInteger(C_SECTION_Candidate_MAIN, C_KEY_ORDER_MAX_NUMBER, seMaxNumberOrder.Value);
  finally
    General.XMLFile.Save;
  end;
end;

procedure TfrmCandidateMain.CheckRankingCriteria(const aData: PInstrumentData);
var
  ColumnsInfo: TColumnsInfo;
  IsExists: Boolean;
begin
  IsExists := False;
  if not Assigned(FColumns) then Exit;
  for ColumnsInfo in FColumns.Values do
  begin
    aData^.IsCriteria := True;
    if (ColumnsInfo.SourceType = stEmbargoColumn) then
    begin
      IsExists := True;
      aData^.IsCriteria := aData^.IsCriteria and ColumnsInfo.EmbargoColumn.IsCriteria(@ColumnsInfo, @aData^.ExtraColumns);
    end;
    if not aData^.IsCriteria then
      Break;
    aData^.IsCriteria := IsExists and aData^.IsCriteria;
  end;
end;

procedure TfrmCandidateMain.CheckRankingCriteries;
var
  Data: PInstrumentData;
  Node: PVirtualNode;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      vstCandidate.SortTree(vstCandidate.Header.Columns[COL_RANKING_SUM].Index, sdDescending);
    end);
  Node := vstCandidate.GetFirstChild(vstCandidate.RootNode);
  while Assigned(Node) do
  begin
    Data := Node^.GetData;
    Data^.ExtraColumns.Position := Node.Index + 1;
    CheckRankingCriteria(Data);
    Node := Node.NextSibling;
  end;
end;

procedure TfrmCandidateMain.TimerEmbargoTimer(Sender: TObject);
begin
  if not Application.Terminated then
    System.Threading.TTask.Create(
      procedure()
      begin
        TThread.NameThreadForDebugging('TfrmCandidateMain.TimerEmbargoTimer');
        CheckRankingCriteries;
        TThread.Queue(nil,
          procedure
          begin
            vstCandidate.Invalidate;
          end);
      end).Start;
end;

procedure TfrmCandidateMain.PreExecutionEvaluationOrders;
var
  Data: PInstrumentData;
  Node: PVirtualNode;
begin
  vstCandidate.BeginUpdate;
  try
    vstCandidate.SortTree(vstCandidate.Header.Columns[COL_RANKING_SUM].Index, sdDescending);
    Node := vstCandidate.GetFirstChild(vstCandidate.RootNode);
    while Assigned(Node) do
    begin
      //FCandidate.TradesState := tsWorking;
      Data := Node^.GetData;
      Data^.ExtraColumns.Position := Node.Index + 1;
      CheckRankingCriteria(Data);
      PreExecutionEvaluation(Data);
      Node := Node.NextSibling;
    end;
  finally
    vstCandidate.EndUpdate;
  end;
end;

procedure TfrmCandidateMain.PreExecutionEvaluation(const aData: PInstrumentData);
var
  LastPrice: Double;
  LastExch: Double;
  Quantity: Integer;
  SingleOrderAmount: Double;
  Currency: string;
  Info: string;
  OrderAmount: Double;
  PrecSettings: TPrecautionarySettingTypes;
begin
  if Assigned(FAutoTrade) and
     aData^.IsCriteria and
     not aData^.IsLocked and
    (ActiveNodes.Count < FCandidate.MaxNumberOrder) then
  begin
//    Info := 'Id=' + aData^.Id.ToString +
//            ', Name=' + aData^.Name +
//            ', Symbol=' + aData^.Symbol +
//            ', LocalSymbol=' + aData^.LocalSymbol +
//            ', Currency=' + aData^.Currency +
//            ', Multiplier=' + aData^.Multiplier.ToString;
//    Info := 'Start: ActiveNodes.Count - '+ ActiveNodes.Count.ToString + '; MaxNumberOrder - '+ FCandidate.MaxNumberOrder.ToString;
//    SaveToLog(Info);
    try
      if SokidList.ActiveInstruments.Contains(aData^.Id) then Exit; // cannot use the same instrument in two active orders
      LastPrice := TMonitorLists.PriceCache.GetLastPrice(aData^.Id, ttLast);
      if (LastPrice = 0) then
      begin
        TIABMarket.RequestMarketData(aData.Id);
        Sleep(50);
        LastPrice := TMonitorLists.PriceCache.GetLastPrice(aData^.Id, ttClose);
      end;
      if (LastPrice = 0) and SokidList.ContainsKey(aData^.Id) then
        LastPrice := SokidList.Items[aData^.Id].LastPrice;
      if (LastPrice = 0) then
      begin
        aData^.Description := 'Not passed - No Feed';
        aData^.IsLocked := True;
        ShowNotification(aData);
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView, ltActivityLog], ddError, 'PreExecutionEvaluation', aData^.Description + ', ' + Info);
      end
      else
      begin
        Currency := aData^.Currency;
        LastExch := 1;
        if Currency.IsEmpty then
          Currency := SokidList.GetItem(aData^.Id).Currency;
        if (Currency <> FAutoTrade.Quantity.Currency) then
          LastExch := TMonitorLists.CurrencyCache.GetLastExchange(FAutoTrade.Quantity.Currency, Currency);
        if (LastExch <= 0) then
        begin
          aData^.Description := 'Not passed - No Exchange Rate ' + Currency;
          aData^.IsLocked := True;
          ShowNotification(aData);
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView, ltActivityLog], ddError, Self, 'PreExecutionEvaluation', aData^.Description + ', ' + Info);
          Exit;
        end;

        //Quantity := 0;
        //OrderAmount := FAutoTrade.Quantity.OrderAmount;
        if (LastPrice > 0) and (LastExch > 0) then
        begin
          if (FAutoTrade.Quantity.TotalOrderAmount < LastPrice) then
          begin
            aData^.Description := 'Not passed - No Total Money';
            aData^.IsLocked := True;
            ShowNotification(aData, 'No Total Money');
            Exit;
          end;

          if (FAutoTrade.Quantity.TotalOrderAmount >= FAutoTrade.Quantity.OrderAmount) then
            OrderAmount := FAutoTrade.Quantity.OrderAmount
          else
            OrderAmount := FAutoTrade.Quantity.TotalOrderAmount;

          if (OrderAmount <= 0) then
          begin
            aData^.Description := 'Not passed - OrderAmount Is 0';
            aData^.IsLocked := True;
            ShowNotification(aData, 'OrderAmount is 0');
            TPublishers.LogPublisher.Write([ltLogWriter], ddWarning, 'PreExecutionEvaluation', 'OrderAmount=0');
          end;

          if (aData^.Multiplier > 0) then
            Quantity := Trunc((OrderAmount * LastExch / (LastPrice * aData^.Multiplier)))
          else
            Quantity := Trunc((OrderAmount * LastExch) / LastPrice);

          SingleOrderAmount := FAutoTrade.Quantity.OrderAmount * LastExch;

          PrecSettings := TOrderUtils.CheckPrecautionarySettings(aData^.SecurityType, Quantity, LastPrice);
          for var PrecSetting in PrecSettings do
            try
              case PrecSetting of
                psAlgorithmTotalValueLimit:
                  ;
                psNumberOfTicks:
                  ;
                psPercentage:
                  ;
                psTotalValueLimit:
                  ;
                psAlgorithmSizeLimit:
                  ;
                psOrderQuantityMax:
                  //Quantity := Trunc(General.PrecautionarySettings[aData^.SecurityType][psOrderQuantityMax]);
                  ;
                psMaxAllowedPrice, psMinAllowedPrice:
                  begin
                    aData^.Description := 'Not passed - ' + PrecSetting.ToString;
                    aData^.IsLocked := True;
                    ShowNotification(aData, PrecSetting.ToString);
                    Exit;
                  end;
              end;
            finally
              TPublishers.LogPublisher.Write([ltLogWriter, ltLogView, ltActivityLog], ddWarning, Self, 'PreExecutionEvaluation', PrecSetting.ToString);
            end;
          FAutoTrade.Quantity.TotalOrderAmount := FAutoTrade.Quantity.TotalOrderAmount - Trunc(Quantity * LastPrice);
        end;

        if (Quantity = 0) then
        begin
          aData^.Description := 'Not passed - Trade limit overdue';
          aData^.IsLocked := True;
          ShowNotification(aData, 'Trade limit overdue');
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView, ltActivityLog], ddError, Self, 'PreExecutionEvaluation', aData^.Description + ', ' + Info);
        end
        else
        begin
          TThread.Synchronize(nil,
            procedure
            var LNode: PVirtualNode;
            begin
              LNode := nil;
              if CreatedOrdersCount < FCandidate.MaxNumberOrder then
                LNode := FMonitor.CreateOrders(FAutoTrade,
                                               aData,
                                               TAutoTradesCommon.Create(Quantity,
                                                                        0{FAutoTrade.Qualifier.QualifierInstance},
                                                                        FCandidate.RecordId,
                                                                        FAutoTrade.InstanceNum,
                                                                        FAutoTrade.RecordId,
                                                                        false,
                                                                        SingleOrderAmount{AllowSendDuplicateOrder}));
              if LNode <> nil then
              begin
                if aData^.IsLocked then
                begin
                  CreatedOrdersCount := CreatedOrdersCount + 1;
                  //SaveToLog('CreatedOrdersCount - '+ CreatedOrdersCount.ToString);
                  ActiveNodes.Add(LNode);
                end;
                aData^.Name := aData^.Name + ': ' + SimpleRoundTo(aData^.ExtraColumns.RankingSum, -2).ToString;
                aData^.Description := 'Created';
                aData^.MotherOrderDoc := TTreeFactory.GetTopOrder(LNode);
                if not SokidList.ActiveInstruments.Contains(aData^.Id) then
                  SokidList.ActiveInstruments.Add(aData^.Id);
              end;
              {if FMonitor.CreateTemplateStructure(FCandidate.OrderGroupId,
                                                  aData,
                                                  TAutoTradesCommon.Create(Quantity,
                                                                           FCandidate.QualifierInstance,
                                                                           FCandidate.QualifierId,
                                                                           FCandidate.InstanceNum,
                                                                           FCandidate.RecordId,
                                                                           FCandidate.AllowSendDuplicateOrder)) <> nil then
              begin
                if aData^.IsLocked then
                  CreatedOrdersCount := CreatedOrdersCount + 1;
                aData^.Name := aData^.Name + ': ' + SimpleRoundTo(aData^.ExtraColumns.RankingSum, -2).ToString;
              end;}
            end);
        end;
      end;
      CheckTradesState;
      AutoTradeInfoToGUI;
    finally
//      Info := 'End: ActiveNodes.Count - '+ ActiveNodes.Count.ToString + '; MaxNumberOrder - '+ FCandidate.MaxNumberOrder.ToString;
//      SaveToLog(Info);
    end;
  end;
end;

procedure TfrmCandidateMain.aChangeWeigthValueColumnUpdate(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
begin
  TAction(Sender).Enabled := (vstCandidate.FocusedColumn >= C_FIXED_COLUMN_INDEX);
  if TAction(Sender).Enabled then
  begin
    if FColumns.ContainsKey(vstCandidate.Header.Columns[vstCandidate.FocusedColumn].Id) then
      ColumnsInfo := FColumns[vstCandidate.Header.Columns[vstCandidate.FocusedColumn].Id];
    TAction(Sender).Enabled := ColumnsInfo.SourceType in [stStaticList, stCandidateMarket, stTickColumn, stPriceChangeColumn];
  end;
end;

procedure TfrmCandidateMain.aChangeWeigthValueUpdate(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
begin
  TAction(Sender).Enabled := not vstCandidate.IsEmpty and (vstCandidate.FocusedColumn >= C_FIXED_COLUMN_INDEX);
  if TAction(Sender).Enabled then
  begin
    if FColumns.ContainsKey(vstCandidate.Header.Columns[vstCandidate.FocusedColumn].Id) then
      ColumnsInfo := FColumns[vstCandidate.Header.Columns[vstCandidate.FocusedColumn].Id];
    TAction(Sender).Enabled := ColumnsInfo.SourceType in [stStaticList, stCandidateMarket, stTickColumn, stPriceChangeColumn];
  end;
end;

procedure TfrmCandidateMain.aCopySelectedNodeToMonitorExecute(Sender: TObject);
var
  Target: PVirtualNode;
begin
  if IABClient.Connected then
  begin
    Target := FMonitor.GetFocusedNode;
    FMonitor.AddInstrumentFromSearch(Target, vstCandidate);
  end;
end;

procedure TfrmCandidateMain.aCopySelectedNodeToMonitorUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstCandidate.IsEmpty and IABClient.Connected;
end;

function TfrmCandidateMain.GetMainTree: TBaseVirtualTree;
begin
  Result := vstCandidate;
end;

procedure TfrmCandidateMain.AddInstrument(const aInstruments: PArrayInstrumentData; aColumnsInfo: TColumnsInfo);
var
  Order: TIABOrder;
  Data: PInstrumentData;
  ExistsItemsList: TList;
  Node: PVirtualNode;
  DelNode: PVirtualNode;
  InstrumentData: TInstrumentData;
  ColumnsInfo: TColumnsInfo;
  ColumnId: Integer;
  NodeArray: TNodeArray;
  ColumnsItem: TExtraColumns.TColumnsItem;

  function GetNodeByConID(aConId: Integer): PVirtualNode;
  var
    Data: PInstrumentData;
    Node: PVirtualNode;
  begin
    Result := nil;
    Node := vstCandidate.GetFirstChild(vstCandidate.RootNode);
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      if (Data^.Id = aConId) then
        Exit(Node);
      Node := Node.NextSibling;
    end;
  end;

  procedure GetInstrumentSpecs(aData: PInstrumentData);
  var
    Order: TIABOrder;
  begin
    Order := TIABOrder.Create;
    try
      Order.ContractId   := aData^.Id;
      Order.SecurityType := aData^.SecurityType;
      Order.Currency     := aData^.Currency;
      Order.Exchange     := aData^.Exchange;
      Order.Symbol       := aData^.Symbol;
      IABClient.SendRequest(ibGetInstrumentSpecs, 1, Order);
    finally
      FreeAndNil(Order);
    end;
  end;

begin
  if (Length(aInstruments^) > 0) then
  begin
    FCandidate.ScanCount := Length(aInstruments^);
    ColumnId := AddOrGetColumn(aColumnsInfo);
    if (ColumnId = 0) then
      Exit;

    if (aColumnsInfo.SourceType = stCandidateMarket) and (aColumnsInfo.CandidateColumn.ScanId = 0) then
      ExecuteActions(aColumnsInfo);

    vstCandidate.BeginUpdate;
    ExistsItemsList := TList.Create;
    try
      for InstrumentData in aInstruments^ do
      begin
        if SokidList.ContainsKey(InstrumentData.Id) and
           (SokidList.Items[InstrumentData.Id].Isolate = 1) then
        begin
          if (lbIsolate.Items.IndexOf(InstrumentData.Id.ToString + ' ' + InstrumentData.Symbol) = -1) then
          begin
            gbIsolate.Visible := True;
            lbIsolate.Items.Add(InstrumentData.Id.ToString + ' ' + InstrumentData.Symbol);
          end;
          Continue;
        end;
        TIABMarket.RequestMarketData(InstrumentData.Id);

        Node := GetNodeByConID(InstrumentData.Id);
        case aColumnsInfo.KindAppend of
          kaAddAll:
            if not Assigned(Node) then
            begin
              Node := vstCandidate.AddChild(vstCandidate.RootNode);
              Data := Node^.GetData;
              Data^.ExtraColumns := TExtraColumns.Create(0);
            end;
          kaReduce:
            if Assigned(Node) then
            begin
              Data := Node^.GetData;
              if not Data^.IsLocked then
                vstCandidate.DeleteNode(Node);
              Node := nil;
            end;
          kaUpdate:
            // The second one will just update the main-list with a new column containing values
            // for instruments which was common in both lists
            ;
          kaIntersection:
            begin
              // The fourth button will add a new column and remove all instruments which was NOT common in both lists.
              // In the main-list there will most likely be empty column-cells.
              // They should be treated as if they contains the value zero.
              if Assigned(Node) then
              begin
                ExistsItemsList.Add(Node);
                Data := Node^.GetData;
                for ColumnsItem in InstrumentData.ExtraColumns.Items.Values do
                  Data^.ExtraColumns.Items.AddOrSetValue(ColumnId, ColumnsItem);
                CalculateRankingSum(Data);
              end;
              Node := nil;
            end;
        end;

        if Assigned(Node) then
        begin
          Data := Node^.GetData;
          Data^.Node           := Node;
          Data^.Id             := InstrumentData.Id;
          Data^.BrokerType     := InstrumentData.BrokerType;
          Data^.Currency       := InstrumentData.Currency;
          Data^.Exchange       := InstrumentData.Exchange;
          Data^.Name           := InstrumentData.Name;
          Data^.Group          := InstrumentData.Group;
          Data^.IsIn           := InstrumentData.IsIn;
          Data^.Sector         := InstrumentData.Sector;
          Data^.Symbol         := InstrumentData.Symbol;
          Data^.LocalSymbol    := InstrumentData.LocalSymbol;
          Data^.SecurityType   := InstrumentData.SecurityType;
          Data^.Multiplier     := InstrumentData.Multiplier;
          Data^.TWSMessageItem := InstrumentData.TWSMessageItem;
          if Data^.SecurityType in [stIndex, stOption, stFuture] then
            Data^.Name := InstrumentData.LocalSymbol;

          if InstrumentData.ExtraColumns.Items.TryGetValue(-1, ColumnsItem) then
            Data^.ExtraColumns.Items.AddOrSetValue(ColumnId, ColumnsItem)
          else
            for ColumnId in InstrumentData.ExtraColumns.Items.Keys do
              if InstrumentData.ExtraColumns.Items.TryGetValue(ColumnId, ColumnsItem) then
                Data^.ExtraColumns.Items.AddOrSetValue(ColumnId, ColumnsItem);

          for ColumnsInfo in FColumns.Values do
          begin
            if not Data^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId) then
            begin
              ColumnsItem.Rank               := 0;
              ColumnsItem.Price              := 0;
              ColumnsItem.ColTick            := clBlack;
              ColumnsItem.IsUnsufficientData := True;

              //restore weight from cache
              if FCacheNodesWeight.ContainsKey(Data^.Id) and
                 FCacheNodesWeight[Data^.Id].Items.ContainsKey(ColumnsInfo.ColumnId) then
                ColumnsItem.Weight := FCacheNodesWeight[Data^.Id].Items[ColumnsInfo.ColumnId].Weight
              else
                ColumnsItem.Weight := 0;
              Data^.ExtraColumns.Items.AddOrSetValue(ColumnsInfo.ColumnId, ColumnsItem);
              SetValueFromPriceCache(Data, ColumnsInfo);
            end;

            ColumnsItem := Data^.ExtraColumns.Items[ColumnsInfo.ColumnId];
            if not (ColumnsInfo.SourceType in [stTickColumn]) then
              if (ColumnsInfo.ColumnId = aColumnsInfo.ColumnId) and (ColumnsItem.TimeStamp = 0) then
              begin
                ColumnsItem.TimeStamp := InstrumentData.InitTimeStamp;
                Data^.ExtraColumns.Items.AddOrSetValue(ColumnsInfo.ColumnId, ColumnsItem);
              end;
          end;

          if Data^.Name.IsEmpty then
          begin
            Data^.Name := Data^.Symbol;
            GetInstrumentSpecs(Data);
          end;
          FInstrumentList.AddNode(InstrumentData.Id, Node);
          CalculateRankingSum(Data);
        end;
      end;

      NodeArray := FInstrumentList.ToNodeArray;
      TArray.Sort<PVirtualNode>(NodeArray, TComparer<PVirtualNode>.Construct(
        function(const Left, Right: PVirtualNode): Integer
        var
          LeftData, RightData: PInstrumentData;
        begin
          LeftData := Left^.GetData;
          RightData := Right^.GetData;
          if (LeftData^.ExtraColumns.RankingSum < RightData^.ExtraColumns.RankingSum) then
            Result := LessThanValue
          else if (LeftData^.ExtraColumns.RankingSum = RightData^.ExtraColumns.RankingSum) then
            Result := EqualsValue
          else
            Result := GreaterThanValue;
        end));

//      for var i := Low(NodeArray) to High(NodeArray) do
//      begin
//        if (i >= FCandidate.MaxRows) then
//        begin
//          if Assigned(NodeArray[i]) then
//            vstCandidate.DeleteNode(NodeArray[i], False);
//          FInstrumentList.DeleteNode(NodeArray[i]);
//        end;
//      end;

      if (aColumnsInfo.KindAppend = kaIntersection) and (ExistsItemsList.Count > 0) then
      begin
        Node := vstCandidate.GetFirstChild(vstCandidate.RootNode);
        while Assigned(Node) do
        begin
          DelNode := Node;
          Node := Node.NextSibling;
          Data := DelNode^.GetData;
          if not Data^.IsLocked and (ExistsItemsList.IndexOf(DelNode) < 0) then
            vstCandidate.DeleteNode(DelNode);
        end;
      end;

//      if FCandidate.HistoricalDataParams.SubscribeHistData then
//      begin
//        Node := vstCandidate.GetFirstChild(vstCandidate.RootNode);
//        while Assigned(Node) do
//        begin
//          Data := Node^.GetData;
//          if not FSubscribedList.Contains(Data^.Id) then
//          begin
//            RequestMarketData(Data^.Id);
//            FSubscribedList.Add(Data^.Id);
//          end;
//          Node := Node.NextSibling;
//        end;
//      end;

      FCandidate.ScanCount := vstCandidate.RootNode.ChildCount;
      PreExecutionEvaluationOrders;
    finally
      FreeAndNil(ExistsItemsList);
      vstCandidate.EndUpdate;
    end;

    for InstrumentData in aInstruments^ do
      InstrumentData.Clear;
  end;
end;

procedure TfrmCandidateMain.DeleteAllNodes;
begin
  vstCandidate.BeginUpdate;
  try
    CancelScan;
    vstCandidate.Clear;
    vstCandidate.Header.Columns.Clear;
    FInstrumentList.Clear;
    FCacheNodesWeight.Clear;
    FColumns.Clear;
    lbColumns.Items.Clear;
    AddFixedColumns;
    TimerEmbargo.Enabled        := False;
    edAutoTradeTemplate.Text    := '';
    CreatedOrdersCount          := 0;
    FCandidate.Name         := '';
    FCandidate.ColumnsInfo  := ColumnsInfoToXml;
    AutoTradeInfoToGUI;
  finally
    vstCandidate.EndUpdate;
  end;
end;

procedure TfrmCandidateMain.aDeleteAllNodesExecute(Sender: TObject);
begin
  if (TMessageDialog.ShowQuestion(rsAllNodesDeletedConfirmation) = mrYes) then
    DeleteAllNodes;
end;

procedure TfrmCandidateMain.aDeleteColumnExecute(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
  ColumnId: Integer;
begin
  if (vstCandidate.FocusedColumn + 1 > C_FIXED_COLUMN_INDEX) and
    (TMessageDialog.ShowQuestion(Format(rsColumnDeletedConfirmation, [vstCandidate.Header.Columns[vstCandidate.FocusedColumn].CaptionText])) = mrYes) then
  begin
    vstCandidate.BeginUpdate;
    try
      ColumnId := vstCandidate.Header.Columns[vstCandidate.FocusedColumn].Id;
      if FColumns.ContainsKey(ColumnId) then
      begin
        ColumnsInfo := FColumns[ColumnId];
        if (ColumnsInfo.SourceType = stCandidateMarket) and IABClient.Connected then
          IABClient.CancelScan(ColumnsInfo.CandidateColumn.ScanId);
        FColumns.Remove(ColumnId);
      end;
      vstCandidate.Header.Columns.Delete(vstCandidate.FocusedColumn);
      TimerEmbargo.Enabled := False;
      for ColumnsInfo in FColumns.Values do
        if (ColumnsInfo.SourceType = stEmbargoColumn) and (ColumnsInfo.EmbargoColumn.EmbargoType in [etReleaseTime, etHoldTime, etTimePeriod]) then
        begin
          TimerEmbargo.Enabled := True;
          Break;
        end;
      CalculateAllRankingSum;
      PreExecutionEvaluationOrders;
    finally
      vstCandidate.EndUpdate;
    end;
  end;
  FillColumnList;
end;

procedure TfrmCandidateMain.aExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmCandidateMain.aInformationDialogExecute(Sender: TObject);
begin
  //TInformationDialog.ShowMessage(TDocumentInfo.GetScanColumnsInfo(FColumns), 'CandidateMain');
end;

procedure TfrmCandidateMain.aInstrumentInfoExecute(Sender: TObject);
var
  Data: PInstrumentData;
  Node: PVirtualNode;
  sb: TStringBuilder;
  TickType: TIABTickType;
begin
  if Assigned(vstCandidate.FocusedNode) then
  begin
    Node := vstCandidate.FocusedNode;
    Data := Node^.GetData;
    sb := TStringBuilder.Create;
    try
      sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
        .Append(C_HTML_BODY_OPEN).AppendLine
        .Append(THtmlLib.GetCenterText(THtmlLib.GetBoldText('Instrument Info'))).Append(C_HTML_BREAK).AppendLine
        .Append(THtmlLib.GetTableTag(VarArrayOf(['Name', 'Value']))).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Id', Data^.Id.ToString]))).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Symbol', Data^.Symbol]))).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf(['LocalSymbol', Data^.LocalSymbol]))).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Name', Data^.Name]))).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Currency', Data^.Currency]))).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Exchange', Data^.Exchange]))).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Sector', Data^.Sector + C_HTML_NBSP]))).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Security Type', Data^.SecurityType.ToString]))).AppendLine
        .Append(THtmlLib.GetTableLineTag(VarArrayOf(['Instrument Group', Data^.Group + C_HTML_NBSP]))).AppendLine
        .Append(C_HTML_TABLE_CLOSE).Append(C_HTML_BREAK).AppendLine
        .Append(THtmlLib.GetCenterText(THtmlLib.GetBoldText('Price Value'))).Append(C_HTML_BREAK).AppendLine
        .Append(THtmlLib.GetTableTag(VarArrayOf(['Name', 'Value']))).AppendLine;
      for TickType := ttBidSize to ttFuturesOpenInterest do
        sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([TickType.ToString, TMonitorLists.PriceCache.GetLastPrice(Data^.Id, TickType)]))).AppendLine;
      sb.Append(C_HTML_TABLE_CLOSE).AppendLine
        .Append(C_HTML_BODY_CLOSE).AppendLine;
      TInformationDialog.ShowMessage(sb.ToString, 'CandidateMainInstrumentInfo');
    finally
      FreeAndNil(sb);
    end;
  end;
end;

procedure TfrmCandidateMain.aInstrumentInfoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstCandidate.IsEmpty;
end;

procedure TfrmCandidateMain.aOpenTickColumnsExecute(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
  RunColumnsInfo: TColumnsInfo;
begin
  if (TfrmCandidateTickColumn.ShowDocument(dmInsert, ColumnsInfo) = mrOk) then
  begin
    for RunColumnsInfo in FColumns.Values do
      if RunColumnsInfo.IsEquals(ColumnsInfo) then
      begin
        TMessageDialog.ShowWarning(Format(rsColumnExists, [ColumnsInfo.Caption]));
        Exit;
      end;
    AddOrGetColumn(ColumnsInfo);
  end;
end;

procedure TfrmCandidateMain.aAddEmbargoColumnExecute(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
  RunColumnsInfo: TColumnsInfo;
begin
  if (TfrmCandidateEmbargoColumn.ShowDocument(dmInsert, FColumns, ColumnsInfo) = mrOk) then
  begin
    for RunColumnsInfo in FColumns.Values do
      if RunColumnsInfo.IsEquals(ColumnsInfo) then
      begin
        TMessageDialog.ShowWarning(Format(rsColumnExists, [ColumnsInfo.Caption]));
        ColumnsInfo.Clear;
        Exit;
      end;
    AddOrGetColumn(ColumnsInfo);
  end;
end;

procedure TfrmCandidateMain.AddFixedColumns;
var
  Column: TVirtualTreeColumn;
begin
  Column := vstCandidate.Header.Columns.Add;
  Column.Width            := 140;
  Column.Text             := 'Symbol';
  Column.CaptionAlignment := taCenter;
  Column.Options          := Column.Options - [coDraggable, coEditable] + [coFixed];

  Column := vstCandidate.Header.Columns.Add;
  Column.Width            := 80;
  Column.Text             := 'Ranking sum';
  Column.CaptionAlignment := taCenter;
  Column.Alignment        := taRightJustify;
  Column.Options          := Column.Options - [coDraggable, coEditable] + [coFixed];
end;

function TfrmCandidateMain.AddOrGetColumn(var aColumnsInfo: TColumnsInfo): Integer;
var
  Column: TVirtualTreeColumn;
  ColumnsItem: TExtraColumns.TColumnsItem;
  Data: PInstrumentData;
  IsExists: Boolean;
  Node: PVirtualNode;
  RunColumnsInfo: TColumnsInfo;
begin
  vstCandidate.BeginUpdate;
  try
    IsExists := False;
    Result := aColumnsInfo.ColumnId;
    for RunColumnsInfo in FColumns.Values do
      if ((aColumnsInfo.ColumnId <= 0) and aColumnsInfo.IsEquals(RunColumnsInfo)) or
        (RunColumnsInfo.ColumnId = aColumnsInfo.ColumnId) then
      begin
        IsExists := True;
        Result := RunColumnsInfo.ColumnId;
        Break;
      end;

    if not IsExists and (aColumnsInfo.SourceType = stStaticList) then
      for RunColumnsInfo in FColumns.Values do
        if (RunColumnsInfo.SourceType = stStaticList) and (aColumnsInfo.StaticColumn.RecordId = RunColumnsInfo.StaticColumn.RecordId) then
        begin
          IsExists := True;
          Result := RunColumnsInfo.ColumnId;
          Break;
        end;

    if not IsExists then
    begin
      case aColumnsInfo.SourceType of
        stStaticList:
          begin
            aColumnsInfo.StaticColumn.FromDB(aColumnsInfo.StaticColumn.RecordId);
            aColumnsInfo.Weight := aColumnsInfo.StaticColumn.Weight;
          end;
        stEmbargoColumn:
          begin
            if aColumnsInfo.EmbargoColumn.EmbargoType in [etReleaseTime, etHoldTime, etTimePeriod] then
              TimerEmbargo.Enabled := True;
          end;
        stTickColumn:
          begin

          end;
      end;

      Column := vstCandidate.Header.Columns.Add;
      if (aColumnsInfo.ColumnWidth > 10) then
        Column.Width := aColumnsInfo.ColumnWidth
      else
        Column.Width := 80;
      Column.Text             := aColumnsInfo.Caption;
      Column.Hint             := 'Column weight: ' + aColumnsInfo.Weight.ToString;
      Column.Alignment        := taRightJustify;
      Column.CaptionAlignment := taCenter;

      aColumnsInfo.ColumnId       := Column.Id;
      aColumnsInfo.ColumnPosition := Column.Position;
      FColumns.AddOrSetValue(Column.Id, aColumnsInfo);
      Result := Column.Id;

      ColumnsItem.Rank               := 0;
      ColumnsItem.Price              := 0;
      ColumnsItem.Weight             := 0;
      ColumnsItem.ColTick            := clBlack;
      ColumnsItem.IsUnsufficientData := True;

      CheckRankingCriteries;
      Node := vstCandidate.GetFirst;
      while Assigned(Node) do
      begin
        Data := Node^.GetData;
        Data^.ExtraColumns.Items.AddOrSetValue(Column.Id, ColumnsItem);
        CalculateGradientLogTerm(Node, aColumnsInfo);
        SetValueFromPriceCache(Data, aColumnsInfo);
        CalculateRankingSum(Data);
        vstCandidate.InvalidateNode(Node);
        Node := vstCandidate.GetNextSibling(Node);
      end;
      FillColumnList;
    end;
  finally
    vstCandidate.EndUpdate;
  end;
end;

procedure TfrmCandidateMain.AddToLog(AText: string);
begin
  meLog.Lines.Add(AText);
end;

procedure TfrmCandidateMain.FillColumnList;
var
  RunColumnsInfo: TColumnsInfo;
  ColumnName: string;
  ArrColumns: TArray<TColumnsInfo>;
begin
  lbColumns.Items.Clear;

  ArrColumns := FColumns.Values.ToArray;
  TArray.Sort<TColumnsInfo>(ArrColumns, TComparer<TColumnsInfo>.Construct(
    function(const Left, Right: TColumnsInfo): Integer
    begin
      if (Left.ColumnPosition > Right.ColumnPosition) then
        Result := GreaterThanValue
      else if (Left.ColumnPosition = Right.ColumnPosition) then
        Result := EqualsValue
      else
        Result := LessThanValue;
    end));

  for RunColumnsInfo in ArrColumns do
  begin
    ColumnName := '';
    case RunColumnsInfo.SourceType of
      stFixed:
        ;
      stStaticList:
        ColumnName := 'Static Column: ' +
                      ' (Id: ' + RunColumnsInfo.StaticColumn.RecordId.ToString + ') ' +
                      RunColumnsInfo.StaticColumn.Name;
      stCandidateMarket:
        ColumnName := 'Candidate Column: ' +
                      ' (Step:' + RunColumnsInfo.CandidateColumn.Step.ToString +
                      ', Weight:' + RunColumnsInfo.CandidateColumn.Weight.ToString + ') ' +
                      RunColumnsInfo.CandidateColumn.Name +
                      ', Locat.:' + RunColumnsInfo.CandidateColumn.ScanCriteria.LocationCode;
      stCalcColumn:
        ColumnName := 'Calc Column: ' + RunColumnsInfo.Caption;
      stTickColumn:
        ColumnName := 'Tick Column: ' + RunColumnsInfo.Caption;
      stEmbargoColumn:
        ColumnName := 'Embargo Column: ' + RunColumnsInfo.EmbargoColumn.Caption;
      stPriceChangeColumn:
        ColumnName := 'Price Change Column: ' + RunColumnsInfo.PriceChangeColumn.Caption;
    end;
    if (lbColumns.Items.IndexOf(ColumnName) < 0) and not ColumnName.IsEmpty then
      lbColumns.Items.AddObject(ColumnName, TObject(RunColumnsInfo.ColumnId));
  end;
  FCandidate.Columns := lbColumns.Items.Text;
  {FCandidate.Note    := lbColumns.Items.Text;
  if Assigned(FAutoTradesEdit) then
  begin
    FCandidate.ColumnsInfo := ColumnsInfoToXml;
    FAutoTradesEdit.SetAutoTradeInfo(FCandidate);
  end; }
end;

procedure TfrmCandidateMain.OpenSequenceRecord;

  procedure LoadColumns;
  var
    Column      : TVirtualTreeColumn;
    ColumnsInfo : TColumnsInfo;
    XMLFile     : TXMLFile;
  begin
    XMLFile := TXMLFile.Create;
    try
      XMLFile.XMLText        := FCandidate.ColumnsInfo;
      XMLFile.CurrentSection := C_SECTION_COLUMNS;
      while not XMLFile.IsLastKey do
      begin
        if XMLFile.ReadAttributes then
        begin
          ColumnsInfo := TColumnsInfo.Create(TSourceType(XMLFile.Attributes.GetAttributeValue('SourceType', stCandidateMarket)), FCandidate.MotherOrderAction);
          ColumnsInfo.KindAppend     := TKindAppend(XMLFile.Attributes.GetAttributeValue('KindAppend', kaAddAll));
          ColumnsInfo.ColumnPosition := XMLFile.Attributes.GetAttributeValue('ColumnPosition', 0);
          ColumnsInfo.ColumnWidth    := XMLFile.Attributes.GetAttributeValue('ColumnWidth', 80);
          ColumnsInfo.Weight         := XMLFile.Attributes.GetAttributeValue('Weight', 1);
          ColumnsInfo.FromList(XMLFile.Attributes.GetAttributeValue('Info', ''));
          ColumnsInfo.CandidateColumn.ScanId := 0;
          if (ColumnsInfo.SourceType <> stFixed) then
            ColumnsInfo.RecordId := XMLFile.Attributes.GetAttributeValue('RecordId', 0);
          if (ColumnsInfo.SourceType = stFixed) and
             (vstCandidate.Header.Columns.Count - 1 >= ColumnsInfo.ColumnPosition) and
             (ColumnsInfo.ColumnWidth > 10) then
          begin
            Column := vstCandidate.Header.Columns[ColumnsInfo.ColumnPosition];
            if Assigned(Column) then
              Column.Width := ColumnsInfo.ColumnWidth;
          end
          else
            AddOrGetColumn(ColumnsInfo);
        end;
        XMLFile.NextKey;
      end;
    finally
      FreeAndNil(XMLFile);
    end;
  end;

  procedure CheckReferenceColumns;
  var
    ColumnsInfo: TColumnsInfo;
    RunColumnsInfo: TColumnsInfo;
    Key: Integer;
    Index: Integer;
  begin
    for Key in FColumns.Keys do
    begin
      ColumnsInfo := FColumns.Items[Key];
      if (ColumnsInfo.SourceType = stEmbargoColumn) and
         (ColumnsInfo.EmbargoColumn.EmbargoType in [etColumnValue, etColumnValueExists, etTimeInterval]) then
        if not Assigned(ColumnsInfo.EmbargoColumn.ColumnsInfo) and (ColumnsInfo.EmbargoColumn.RefRecordId > 0) then
        begin
          for RunColumnsInfo in FColumns.Values do
            if (RunColumnsInfo.RecordId = ColumnsInfo.EmbargoColumn.RefRecordId) then
            begin
              New(ColumnsInfo.EmbargoColumn.ColumnsInfo);
              ColumnsInfo.EmbargoColumn.ColumnsInfo^ := RunColumnsInfo;
              FColumns.Items[Key] := ColumnsInfo;
              for Index := 0 to vstCandidate.Header.Columns.Count - 1 do
                if (vstCandidate.Header.Columns[Index].ID = ColumnsInfo.ColumnId) then
                  vstCandidate.Header.Columns[Index].Text := ColumnsInfo.Caption;
              Break;
            end;
        end;
    end;
  end;

var
  ColumnsInfo: TColumnsInfo;
begin
  if not FCandidate.ColumnsInfo.IsEmpty then
  begin
    vstCandidate.BeginUpdate;
    TimerCalculateGradient.Enabled := False;
    try
      CancelScan;
      lbColumns.Items.Clear;
      FInstrumentList.Clear;
      FColumns.Clear;
      FCacheNodesWeight.Clear;
      vstCandidate.Clear;
      vstCandidate.Header.Columns.Clear;
      AddFixedColumns;
      LoadColumns;
      CheckReferenceColumns;
      UpdateCaptions;
      for ColumnsInfo in FColumns.Values do
        ExecuteActions(ColumnsInfo);
      CalculateValuesForAllNodes;
      CalculateAllRankingSum;
      PreExecutionEvaluationOrders;
    finally
      vstCandidate.EndUpdate;
      TimerCalculateGradient.Enabled := True;
    end;
  end;
end;

procedure TfrmCandidateMain.ExecuteActions(aColumnsInfo: TColumnsInfo);
var
  CandidateColumn: TCandidateColumn;
begin
  case aColumnsInfo.SourceType of
    stStaticList:
      begin
        if (aColumnsInfo.StaticColumn.RecordId > 0) then
          TfrmCandidateStaticLists.AddInstrumentToCandidateMain(Self, aColumnsInfo.KindAppend, aColumnsInfo.StaticColumn.RecordId, dmUpdate);
        if vstCandidate.IsEmpty then
          ShowBaloonHint(rsNoInstruments, pnlOptions);
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self,
                                      'ExecuteActions',
                                      'SourceType=stStaticList, StaticListsId= ' + aColumnsInfo.StaticColumn.RecordId.ToString);
      end;
    stCandidateMarket:
      if (aColumnsInfo.CandidateColumn.ScanId <= 0) then
      begin
        CandidateColumn.FromList(aColumnsInfo.CandidateColumn.ToList);
        if (General.PrecautionarySettings[stAll][psMaxAllowedPrice] > 0) then
        begin
          if (CandidateColumn.ScanCriteria.BelowPrice > General.PrecautionarySettings[stAll][psMaxAllowedPrice]) then
            CandidateColumn.ScanCriteria.BelowPrice := General.PrecautionarySettings[stAll][psMaxAllowedPrice];
        end;

        if (General.PrecautionarySettings[stAll][psMinAllowedPrice] > 0) then
        begin
          if (CandidateColumn.ScanCriteria.AbovePrice = UNSET_DOUBLE) or
             (CandidateColumn.ScanCriteria.AbovePrice < General.PrecautionarySettings[stAll][psMinAllowedPrice]) then
            CandidateColumn.ScanCriteria.AbovePrice := General.PrecautionarySettings[stAll][psMinAllowedPrice];
        end;

        aColumnsInfo.CandidateColumn.ScanId := IABClient.ScanExecute(CandidateColumn.ScanCriteria);
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self,
                                      'ExecuteActions',
                                      'MaxRows= ' + aColumnsInfo.CandidateColumn.MaxRows.ToString + sLineBreak +
                                      'ScanId= ' + aColumnsInfo.CandidateColumn.ScanId.ToString + sLineBreak +
                                      'Step= ' + aColumnsInfo.CandidateColumn.Step.ToString + sLineBreak +
                                      'Weight= ' + aColumnsInfo.CandidateColumn.Weight.ToString + sLineBreak +
                                      'SourceType=stCandidateMarket' + sLineBreak);
        FColumns.AddOrSetValue(aColumnsInfo.ColumnId, aColumnsInfo);
      end;
  end;
end;

procedure TfrmCandidateMain.CheckAlwaysMax(TempId: Integer; Status: TIABOrderState);
var Node, ParentNode: PVirtualNode;
    Data: PTreeData;
    InstrumentData: PInstrumentData;
    InstrumentItem: TInstrumentItem;
begin
  Node := TMonitorLists.OrderList.GetNodeOrder(TempId);
  ParentNode := TTreeFactory.GetTopOrderNode(Node);
  if Assigned(Node) and ActiveNodes.Contains(ParentNode) then
  begin
    Data := Node^.GetData;
    if Assigned(Data^.OrderDoc) and (Data^.OrderDoc.AutoTradesID = FAutoTrade.RecordId) then
    begin
      if (Status in [osCancelled, osError]) then
      begin
        if (Data^.OrderDoc.Filled > 0) then
        begin
          if TTreeFactory.StartChildOrders(Node, Data^.OrderDoc.Filled) then Exit;
        end
        else
          TTreeFactory.CancelChildOrders(Node)
      end
      else if TTreeFactory.HasChildOrders(Node) then
        Exit;
      if SokidList.ActiveInstruments.Contains(Data^.OrderDoc.Instrument.SokidInfo.ContractId) then
        SokidList.ActiveInstruments.Remove(Data^.OrderDoc.Instrument.SokidInfo.ContractId);
      if cbRepeatInstruments.Checked then
      begin
        InstrumentItem := FInstrumentList.GetItem(Data^.OrderDoc.Instrument.SokidInfo.ContractId);
        if Assigned(InstrumentItem) then
          for Node in InstrumentItem.NodeList do
            if Assigned(Node) then
            begin
              InstrumentData := Node^.GetData;
              InstrumentData^.IsLocked := false;
            end;
      end;
      if cbAlwaysMax.Checked then
      begin
        ActiveNodes.Remove(ParentNode);
        CreatedOrdersCount := CreatedOrdersCount - 1;
        //SaveToLog('Remove: ActiveNodes.Count - '+ ActiveNodes.Count.ToString);
      end;
    end;
  end;
end;

function TfrmCandidateMain.CheckColumn(const aColumnsInfo: TColumnsInfo): Boolean;
var
  Column: TVirtualTreeColumn;
  ColumnIndex: Integer;
begin
  Result := False;
  for ColumnIndex := vstCandidate.Header.Columns.GetFirstColumn to vstCandidate.
    Header.Columns.Count - 1 do
  begin
    Column := vstCandidate.Header.Columns.Items[ColumnIndex];
    if (Column.Id = aColumnsInfo.ColumnId) then
    begin
      Column.Options := Column.Options + [coVisible];
      Result := True;
      Break;
    end;
  end;
end;

function TfrmCandidateMain.CheckRequiredEmbargoColumn: Boolean;
var
  ColumnsInfo: TColumnsInfo;
begin
  Result := False;
  for ColumnsInfo in FColumns.Values do
    if (ColumnsInfo.SourceType = stEmbargoColumn) then
    begin
      Result := True;
      Break;
    end;

  if (not Result) then
  begin
    TMessageDialog.ShowWarning(rsEmbargoRequired);
    if (TfrmCandidateEmbargoColumn.ShowDocument(dmInsert, FColumns, ColumnsInfo) = mrOk) then
      AddOrGetColumn(ColumnsInfo);
  end
  else if not CheckColumn(ColumnsInfo) then
    AddOrGetColumn(ColumnsInfo);
end;

function TfrmCandidateMain.ColumnsInfoToXml: string;
var
  ColumnsInfo: TColumnsInfo;
  XmlFile: TXmlFile;
  Column: TVirtualTreeColumn;
  ColumnIndex: TColumnIndex;
begin
  XmlFile := TXmlFile.Create;
  try
    XmlFile.CurrentSection := C_SECTION_COLUMNS;
    Column := nil;
    ColumnIndex := vstCandidate.Header.Columns.GetFirstColumn;
    if (ColumnIndex > -1) then
      Column := vstCandidate.Header.Columns[ColumnIndex];
    while Assigned(Column) do
    begin
      XmlFile.Attributes.AddNode;
      XmlFile.Attributes.SetAttributeValue('ColumnPosition', Column.Position);
      XmlFile.Attributes.SetAttributeValue('ColumnWidth', Column.Width);
      if (Column.Index < C_FIXED_COLUMN_INDEX) then
        XmlFile.Attributes.SetAttributeValue('SourceType', stFixed);

      if FColumns.ContainsKey(Column.Id) then
      begin
        ColumnsInfo := FColumns.Items[Column.Id];
        XmlFile.Attributes.SetAttributeValue('RecordId', ColumnsInfo.RecordId);
        XmlFile.Attributes.SetAttributeValue('SourceType', ColumnsInfo.SourceType);
        XmlFile.Attributes.SetAttributeValue('Weight', ColumnsInfo.Weight);
        XmlFile.Attributes.SetAttributeValue('KindAppend', ColumnsInfo.KindAppend);
        XmlFile.Attributes.SetAttributeValue('Info', ColumnsInfo.ToList);
      end;
      XmlFile.WriteAttributes;
      ColumnIndex := vstCandidate.Header.Columns.GetNextColumn(ColumnIndex);
      if (ColumnIndex > -1)then
        Column := vstCandidate.Header.Columns[ColumnIndex]
      else
        Column := nil;
    end;
    Result := XmlFile.XMLText;
  finally
    FreeAndNil(XmlFile);
  end;
end;

procedure TfrmCandidateMain.aSaveAutoTradeTemplateAsExecute(Sender: TObject);
begin
  if CheckRequiredEmbargoColumn then
  begin
    FCandidate.RecordId := -1;
    FCandidate.ColumnsInfo := ColumnsInfoToXml;
    FCandidate.SaveToDB;
    AutoTradeInfoToGUI;
  end;
end;

procedure TfrmCandidateMain.aSaveAutoTradeTemplateExecute(Sender: TObject);
begin
  if CheckRequiredEmbargoColumn then
  begin
    FCandidate.ColumnsInfo := ColumnsInfoToXml;
    if (FCandidate.Name <> edAutoTradeTemplate.Text) then
      FCandidate.Name := edAutoTradeTemplate.Text;
    FCandidate.SaveToDB;
    AutoTradeInfoToGUI;
  end;
end;

procedure TfrmCandidateMain.aOpenAutoTradeTemplateExecute(Sender: TObject);
var
  Id: Integer;
  MarkedNode: TMarkedNode;
begin
  MarkedNode.DocType := ntAutoTrade;
  MarkedNode.RecordId := FCandidate.RecordId;
  Id := TfrmAutoTrades.ShowDocument(MarkedNode);
  if (Id > 0) then
    try
      IsLoaded := True;
      FCandidate.FromDB(Id);
      edAutoTradeTemplate.Text := FCandidate.Name;
      OpenSequenceRecord;
      AutoTradeInfoToGUI;
    finally
      IsLoaded := False;
    end;
end;

procedure TfrmCandidateMain.aOpenGradientColumnExecute(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
  RunColumnsInfo: TColumnsInfo;
begin
  if (TfrmCandidateGradientColumn.ShowDocument(dmInsert, ColumnsInfo) = mrOk) then
  begin
    for RunColumnsInfo in FColumns.Values do
      if RunColumnsInfo.IsEquals(ColumnsInfo) then
      begin
        TMessageDialog.ShowWarning(Format(rsColumnExists, [ColumnsInfo.Caption]));
        Exit;
      end;
    AddOrGetColumn(ColumnsInfo);
  end;
end;

procedure TfrmCandidateMain.EditWeigthColumn(const aColumnIndex: TColumnIndex);
var
  Weight: Double;
  Coef: Double;
  ColumnsInfo: TColumnsInfo;

  procedure CalculateColumn(aCoef: Double; aColumnID: Integer);
  var
    Data: PInstrumentData;
    Node: PVirtualNode;
    ColumnsItem: TExtraColumns.TColumnsItem;
  begin
    Node := vstCandidate.GetFirstChild(vstCandidate.RootNode);
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      if Data^.ExtraColumns.Items.ContainsKey(aColumnID) then
      begin
        ColumnsItem := Data^.ExtraColumns.Items.Items[aColumnID];
        ColumnsItem.Rank := ColumnsItem.Rank * aCoef;
        Data^.ExtraColumns.Items.AddOrSetValue(aColumnID, ColumnsItem);
        vstCandidate.InvalidateNode(Node);
      end;
      Node := Node.NextSibling;
    end;
  end;

begin
  if (aColumnIndex >= C_FIXED_COLUMN_INDEX) then
  begin
    if FColumns.ContainsKey(vstCandidate.Header.Columns[aColumnIndex].Id) then
      ColumnsInfo := FColumns[vstCandidate.Header.Columns[aColumnIndex].Id];

    if ColumnsInfo.SourceType in [stStaticList, stCandidateMarket, stTickColumn, stPriceChangeColumn] then
    begin
      Weight := ColumnsInfo.Weight;
      if (TfrmCandidateEditWeight.ShowDocument(Weight) = mrOk) then
      begin
        Coef := Weight / ColumnsInfo.Weight;
        ColumnsInfo.Weight := Weight;
        vstCandidate.Header.Columns[aColumnIndex].Hint := 'Weight: ' + Weight.ToString;
        vstCandidate.Header.Columns[aColumnIndex].Text := ColumnsInfo.Caption;

        FColumns.AddOrSetValue(vstCandidate.Header.Columns[aColumnIndex].Id, ColumnsInfo);
        if (ColumnsInfo.SourceType = stCandidateMarket) then
          CalculateColumn(Coef, ColumnsInfo.ColumnId);
//        else
//          FColumns.AddOrSetValue(vstCandidate.Header.Columns[aColumnIndex].Id, ColumnsInfo);
        FillColumnList;
      end;
      CalculateAllRankingSum;
      PreExecutionEvaluationOrders;
    end;
  end;
end;

procedure TfrmCandidateMain.aAddPriceChangeColumnExecute(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
  RunColumnsInfo: TColumnsInfo;
begin
  if (TfrmCandidatePriceChangeColumn.ShowDocument(dmInsert, ColumnsInfo, FCandidate) = mrOk) then
  begin
    for RunColumnsInfo in FColumns.Values do
      if RunColumnsInfo.IsEquals(ColumnsInfo) then
      begin
        TMessageDialog.ShowWarning(Format(rsColumnExists, [ColumnsInfo.Caption]));
        Exit;
      end;
    AddOrGetColumn(ColumnsInfo);
  end;
end;

procedure TfrmCandidateMain.aChangeWeigthValueColumnExecute(Sender: TObject);
begin
  EditWeigthColumn(vstCandidate.FocusedColumn);
end;

procedure TfrmCandidateMain.aChangeWeigthValueExecute(Sender: TObject);
var
  Weight: Double;
  Coef: Double;
  ColumnsInfo: TColumnsInfo;
  Node: PVirtualNode;
  Data: PInstrumentData;
  ColumnsItem: TExtraColumns.TColumnsItem;
  ExtraColumns: TExtraColumns;
begin
  if (vstCandidate.FocusedColumn >= C_FIXED_COLUMN_INDEX) and Assigned(vstCandidate.FocusedNode) then
  begin
    if FColumns.ContainsKey(vstCandidate.Header.Columns[vstCandidate.FocusedColumn].Id) then
      ColumnsInfo := FColumns[vstCandidate.Header.Columns[vstCandidate.FocusedColumn].Id];

    if ColumnsInfo.SourceType in [stStaticList, stCandidateMarket, stTickColumn, stPriceChangeColumn] then
    begin
      Node := vstCandidate.FocusedNode;
      Data := Node^.GetData;
      if Data^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId) then
      begin
        ColumnsItem := Data^.ExtraColumns.Items[ColumnsInfo.ColumnId];
        Weight := ColumnsItem.Weight;
        if (TfrmCandidateEditWeight.ShowDocument(Weight) = mrOk) then
        begin
          if (ColumnsItem.Weight = 0) then
            Coef := 0
          else
            Coef := Weight / ColumnsItem.Weight;

          ColumnsItem.Weight := Weight;
          if (ColumnsInfo.SourceType = stCandidateMarket) then
            ColumnsItem.Rank := ColumnsItem.Rank * Coef;

          //save in cache the changed weight values
          if not FCacheNodesWeight.ContainsKey(Data^.Id) then
            ExtraColumns := TExtraColumns.Create(0)
          else
            ExtraColumns := FCacheNodesWeight[Data^.Id];
          ExtraColumns.Items.AddOrSetValue(ColumnsInfo.ColumnId, ColumnsItem);
          FCacheNodesWeight.AddOrSetValue(Data^.Id, ExtraColumns);
          Data^.ExtraColumns.Items.AddOrSetValue(ColumnsInfo.ColumnId, ColumnsItem);
          CalculateRankingSum(Data);
        end;
      end;
      PreExecutionEvaluationOrders;
    end;
  end;
end;

procedure TfrmCandidateMain.ShowBaloonHint(const aText: string; const aControl: TWinControl);
begin
  BalloonHint.Title := 'Warning';
  BalloonHint.Description := aText;
  BalloonHint.Style := bhsStandard;
  BalloonHint.ImageIndex := 0;
  BalloonHint.HideAfter := 10000;
  BalloonHint.ShowHint(aControl);
end;

procedure TfrmCandidateMain.ShowColumnDetails(const aColumnID: Integer);
var
  ColumnsInfo: TColumnsInfo;
  Column: TVirtualTreeColumn;
begin
  if FColumns.TryGetValue(aColumnID, ColumnsInfo) then
    case ColumnsInfo.SourceType of
      stStaticList:
        TfrmCandidateStaticLists.ShowDocument(dmView, ColumnsInfo.KindAppend, ColumnsInfo.StaticColumn.RecordId, Self);
      stCandidateMarket:
        ShowCandidateMarket(aColumnID, ColumnsInfo.CandidateColumn);
      stTickColumn:
        begin
          TfrmCandidateTickColumn.ShowDocument(dmUpdate, ColumnsInfo);
          Column := TVirtualTreeColumn(vstCandidate.Header.Columns.FindItemID(ColumnsInfo.ColumnId));
          if Assigned(Column) then
            Column.Text := ColumnsInfo.Caption;
          FColumns.AddOrSetValue(aColumnID, ColumnsInfo);
        end;
      stCalcColumn:
        begin
          TfrmCandidateGradientColumn.ShowDocument(dmUpdate, ColumnsInfo);
          Column := TVirtualTreeColumn(vstCandidate.Header.Columns.FindItemID(ColumnsInfo.ColumnId));
          if Assigned(Column) then
            Column.Text := ColumnsInfo.Caption;
          FColumns.AddOrSetValue(aColumnID, ColumnsInfo);
        end;
      stEmbargoColumn:
        begin
          TfrmCandidateEmbargoColumn.ShowDocument(dmUpdate, FColumns, ColumnsInfo);
          Column := TVirtualTreeColumn(vstCandidate.Header.Columns.FindItemID(ColumnsInfo.ColumnId));
          if Assigned(Column) then
            Column.Text := ColumnsInfo.Caption;
          FColumns.AddOrSetValue(aColumnID, ColumnsInfo);
        end;
      stPriceChangeColumn:
        begin
          TfrmCandidatePriceChangeColumn.ShowDocument(dmUpdate, ColumnsInfo, FCandidate);
          Column := TVirtualTreeColumn(vstCandidate.Header.Columns.FindItemID(ColumnsInfo.ColumnId));
          if Assigned(Column) then
            Column.Text := ColumnsInfo.Caption;
          FColumns.AddOrSetValue(aColumnID, ColumnsInfo);
        end;
    end;
  FillColumnList;
  RefreshColumnValues(aColumnID);
end;

procedure TfrmCandidateMain.aShowColumnDetailsExecute(Sender: TObject);
var
  ColumnId: Integer;
begin
  if (vstCandidate.FocusedColumn > C_FIXED_COLUMN_INDEX - 1) then
  begin
    ColumnId := vstCandidate.Header.Columns[vstCandidate.FocusedColumn].Id;
    ShowColumnDetails(ColumnId);
  end;
end;

procedure TfrmCandidateMain.aShowColumnDetailsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstCandidate.IsEmpty;
end;

procedure TfrmCandidateMain.aShowGlobalSettingsExecute(Sender: TObject);
begin
  General.ShowParametersTab(TfrmParameters.C_TAB_PRECAUTIONARY_SETTINGS);
end;

procedure TfrmCandidateMain.aShowPriceHistoryExecute(Sender: TObject);
var
  Data: PInstrumentData;
begin
  if Assigned(vstCandidate.FocusedNode) then
  begin
    Data := vstCandidate.FocusedNode^.GetData;
    TInformationDialog.ShowMessage(TDocumentInfo.GetPriceHistory(Data^.Id, nil), 'PriceHistory');
  end;
end;

procedure TfrmCandidateMain.aShowTradeChartExecute(Sender: TObject);
begin
  if Assigned(vstCandidate.FocusedNode) then
    ShowTradeChart(vstCandidate.FocusedNode);
end;

procedure TfrmCandidateMain.aShowTradeChartUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstCandidate.IsEmpty and Assigned(vstCandidate.FocusedNode);
end;

procedure TfrmCandidateMain.aShowWeightedExecute(Sender: TObject);
begin
  if (rgWeightedFeed.ItemIndex = 0) then
    rgWeightedFeed.ItemIndex := 1
  else
    rgWeightedFeed.ItemIndex := 0;
  vstCandidate.Invalidate;
end;

procedure TfrmCandidateMain.rgWeightedFeedClick(Sender: TObject);
begin
  vstCandidate.Invalidate;
end;

procedure TfrmCandidateMain.CalculateAllRankingSum;
var
  Data: PInstrumentData;
  Node: PVirtualNode;
begin
  Node := vstCandidate.GetFirstChild(vstCandidate.RootNode);
  while Assigned(Node) do
  begin
    Data := Node^.GetData;
    CalculateRankingSum(Data);
    Node := Node.NextSibling;
  end;
end;

procedure TfrmCandidateMain.CalculateRankingSum(const aData: PInstrumentData);
var
  ColumnsItem: TExtraColumns.TColumnsItem;
  RankingSum: Double;
  Weight: Double;
begin
  if Assigned(aData) then
  begin
    RankingSum := 0;
    for var ColumnsInfo in FColumns.Values do
    begin
      if aData^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId) then
      begin
        ColumnsItem := aData^.ExtraColumns.Items[ColumnsInfo.ColumnId];
        Weight := IfThen((ColumnsItem.Weight > 0), ColumnsItem.Weight, ColumnsInfo.Weight);
        //Weight := ColumnsItem.Weight;
        case ColumnsInfo.SourceType of
          stStaticList:
            RankingSum := RankingSum + ColumnsItem.Rank * Weight;
          stCandidateMarket:
            RankingSum := RankingSum + ColumnsItem.Rank * Weight;
          stCalcColumn:
            begin
              if ColumnsInfo.CalcColumn.CalculationType in [ctGradientCalc, ctCorridorWidth, ctlastPosition] then
                if not ColumnsItem.IsUnsufficientData
                    and
                   (ColumnsItem.Price >= ColumnsInfo.CalcColumn.Value1)
                    and
                   (ColumnsItem.Price <= ColumnsInfo.CalcColumn.Value2) then
                  RankingSum := RankingSum + Weight;
            end;
          stTickColumn:
//            if (ColumnsInfo.TickColumn.IBValue2 <> ttNotSet) then
//              RankingSum := RankingSum + ColumnsItem.Price * Weight * 100
//            else
              RankingSum := RankingSum + ColumnsItem.Price * Weight;
          stPriceChangeColumn:
            RankingSum := RankingSum + ColumnsItem.PriceChangeWeight * Weight;
        end;
      end;
    end;
    if IsNan(RankingSum) then
      RankingSum := 0;
    if (RankingSum = 0) then
      aData^.ExtraColumns.RankingColTick := clBlack
    else if (RankingSum >= aData^.ExtraColumns.RankingSum) then
      aData^.ExtraColumns.RankingColTick := clGreen
    else
      aData^.ExtraColumns.RankingColTick := clRed;

    aData^.ExtraColumns.RankingSum := RankingSum;
    TThread.Queue(nil,
      procedure
      begin
        vstCandidate.InvalidateNode(aData^.Node);
      end);
  end;
end;

procedure TfrmCandidateMain.CalculateValuesForAllNodes;
var
  ColumnsInfo: TColumnsInfo;
  Data: PInstrumentData;
  Node: PVirtualNode;
begin
  if not Application.Terminated then
  begin
    vstCandidate.BeginUpdate;
    try
      for ColumnsInfo in FColumns.Values do
        if (ColumnsInfo.SourceType = stCalcColumn) then
        begin
          Node := vstCandidate.GetFirstChild(vstCandidate.RootNode);
          while Assigned(Node) do
          begin
            Data := Node^.GetData;
            case ColumnsInfo.SourceType of
              stCalcColumn:
                if (ColumnsInfo.CalcColumn.CalculationType = ctGradientLogTerm) then
                  CalculateGradientLogTerm(Node, ColumnsInfo);
              stTickColumn:
                SetValueFromPriceCache(Data, ColumnsInfo);
            end;
            Node := Node.NextSibling;
          end;
        end;
    finally
      vstCandidate.EndUpdate;
    end;
  end;
end;

procedure TfrmCandidateMain.CancelScan;
begin
  for var ColumnsInfo in FColumns.Values do
  begin
    if (ColumnsInfo.SourceType = stCandidateMarket) and IABClient.Connected then
      IABClient.CancelScan(ColumnsInfo.CandidateColumn.ScanId);
  end;
  lbIsolate.Items.Clear;
  gbIsolate.Visible := False;
end;

function TfrmCandidateMain.CalcGradient(Id, GradientDuration, GradientMode: Integer): TGradientRecord;
type TDataRecord = record
  Time1: TDateTime;
  Time2: Int64;
  Time3: Double;
  Price: Double;
end;
//new version
var PriceList: TPriceList;
    arrPrices: TArray<TPrice>;
    StartTime: TDateTime;
    I: integer;
    Data: TArray<TDataRecord>;
    LocalLog: string;

    procedure LocalAddToLog(AText: string);
    begin
      if not LocalLog.IsEmpty then
        LocalLog := LocalLog + #13#10;
      LocalLog := LocalLog + AText;
    end;

    function PrepareValue(AValue: Double): Double;
    begin
      if (Abs(AValue) < 0.0000001) or IsNan(AValue) or IsInfinite(AValue) then
        AValue := 0;
      Result := AValue;
    end;

    procedure LinearRegressionLeastSquares(const Data: TArray<TDataRecord>; var A, B: Double);
    var
      n, i: Integer;
      sumX, sumY, sumXY, sumX2: Double;
    begin
      n := Length(Data);

      if n < 2 then
        raise Exception.Create('At least 2 data points are required for linear regression.');

      sumX := 0;
      sumY := 0;
      sumXY := 0;
      sumX2 := 0;

      for i := 0 to n - 1 do
      begin
        if GradientMode = 1 then
        begin
          sumX := sumX + Data[i].Time1;
          sumY := sumY + Data[i].Price;
          sumXY := sumXY + Data[i].Time1 * Data[i].Price;
          sumX2 := sumX2 + Sqr(Data[i].Time1);
        end
        else if GradientMode = 2 then
        begin
          sumX := sumX + Data[i].Time2;
          sumY := sumY + Data[i].Price;
          sumXY := sumXY + Data[i].Time2 * Data[i].Price;
          sumX2 := sumX2 + Sqr(Data[i].Time2);
        end
        else
        begin
          sumX := sumX + Data[i].Time3;
          sumY := sumY + Data[i].Price;
          sumXY := sumXY + Data[i].Time3 * Data[i].Price;
          sumX2 := sumX2 + Sqr(Data[i].Time3);
        end;
      end;

      A := (n * sumXY - sumX * sumY) / (n * sumX2 - Sqr(sumX));
      B := (sumY - A * sumX) / n;
    end;

    procedure CalcGradientValues(Data: TArray<TDataRecord>; var AGradient: TGradientRecord);
    var Slope, Intercept: Double;
        I: integer;
        CalcPrices: TArray<Double>;
        MinResidual, MaxResidual, Residual: Double;
        DeltaT1: TDateTime;
        DeltaT2: Int64;
        DeltaT3: Double;
        DeltaP: Double;
        P1, P2: Double;
    begin
      if Length(Data) = 0 then Exit;
      MinResidual := 0;
      MaxResidual := 0;
      DeltaT1 := 0;
      DeltaT2 := 0;
      DeltaT3 := 0;
      LinearRegressionLeastSquares(Data, Slope, Intercept);
      SetLength(CalcPrices, Length(Data));
      LocalAddToLog('Calculation values');
      for I := 0 to Length(Data) - 1 do
      begin
        if GradientMode = 1 then
        begin
          CalcPrices[I] := Slope * Data[I].Time1 + Intercept;
          LocalAddToLog(Data[I].Time1.ToString + #9 + FloatToStr(Data[I].Price) + #9 + CalcPrices[I].ToString);
        end
        else if GradientMode = 2 then
        begin
          CalcPrices[I] := Slope * Data[I].Time2 + Intercept;
          LocalAddToLog(Data[I].Time2.ToString + #9 + FloatToStr(Data[I].Price) + #9 + CalcPrices[I].ToString);
        end
        else
        begin
          CalcPrices[I] := Slope * Data[I].Time3 + Intercept;
          LocalAddToLog(Data[I].Time3.ToString + #9 + FloatToStr(Data[I].Price) + #9 + CalcPrices[I].ToString);
        end;
        Residual := Data[I].Price - CalcPrices[I];
        if I = 0 then
        begin
          MinResidual := Residual;
          MaxResidual := Residual;
        end
        else
        begin
          if MinResidual > Residual then
            MinResidual := Residual;
          if MaxResidual < Residual then
            MaxResidual := Residual;
        end;
      end;
      if GradientMode = 1 then
        DeltaT1 := Data[Length(Data)-1].Time1 - Data[0].Time1
      else if GradientMode = 2 then
        DeltaT2 := Data[Length(Data)-1].Time2 - Data[0].Time2
      else
        DeltaT3 := Data[Length(Data)-1].Time3 - Data[0].Time3;
      DeltaP := (CalcPrices[Length(CalcPrices) - 1] - Abs(MinResidual))
                -
                (CalcPrices[0] - Abs(MinResidual));

      if GradientMode = 1 then
      begin
        if DeltaT1 <> 0 then
          AGradient.Gradient := PrepareValue(DeltaP / DeltaT1)
        else
          AGradient.Gradient := 0;
        //LocalAddToLog('Coridor calc = '+ (MaxResidual - MinResidual).ToString + ' * ' + DeltaT1.ToString + ' / ' + Sqrt(DeltaT1 * DeltaT1 + DeltaP * DeltaP).ToString);
        AGradient.Corridor := PrepareValue((MaxResidual - MinResidual)
                                           * DeltaT1
                                           / Sqrt(DeltaT1 * DeltaT1 + DeltaP * DeltaP));
      end
      else if GradientMode = 2 then
      begin
        if DeltaT2 <> 0 then
          AGradient.Gradient := PrepareValue(DeltaP / DeltaT2)
        else
          AGradient.Gradient := 0;
        //LocalAddToLog('Coridor calc = '+ (MaxResidual - MinResidual).ToString + ' * ' + DeltaT2.ToString + ' / ' + Sqrt(DeltaT2 * DeltaT2 + DeltaP * DeltaP).ToString);
        AGradient.Corridor := PrepareValue((MaxResidual - MinResidual)
                                           * DeltaT2
                                           / Sqrt(DeltaT2 * DeltaT2 + DeltaP * DeltaP));
      end
      else
      begin
        if DeltaT3 <> 0 then
          AGradient.Gradient := PrepareValue(DeltaP / DeltaT3)
        else
          AGradient.Gradient := 0;
        //LocalAddToLog('Coridor calc = '+ (MaxResidual - MinResidual).ToString + ' * ' + DeltaT3.ToString + ' / ' + Sqrt(DeltaT3 * DeltaT3 + DeltaP * DeltaP).ToString);
        AGradient.Corridor := PrepareValue((MaxResidual - MinResidual)
                                           * DeltaT3
                                           / Sqrt(DeltaT3 * DeltaT3 + DeltaP * DeltaP));
      end;

      P1 := Data[Length(Data) - 1].Price - (CalcPrices[Length(CalcPrices) - 1] - Abs(MinResidual));
      P2 := (CalcPrices[Length(CalcPrices) - 1] + Abs(MaxResidual) - (CalcPrices[Length(CalcPrices) - 1] - Abs(MinResidual)));
//      LocalAddToLog('P1 = '+ P1.ToString);
//      LocalAddToLog('P2 = '+ P2.ToString);
      AGradient.LastPosition := PrepareValue((100 * P1)
                                               /
                                              (100 * P2)
                                              * 100);
    end;

begin
  LocalLog := '';
  try
    Result.Calculated := false;
    Result.Gradient := 0;
    Result.Corridor := 0;
    Result.LastPosition := 0;
    LocalAddToLog('----');
    LocalAddToLog('Monitoring = '+ GradientDuration.ToString);
    PriceList := TMonitorLists.PriceCache.GetPriceList(Id);
    if (PriceList.Count > 0) and (SecondsBetween(PriceList.FirstTimeStamp, Now) < GradientDuration) then
    begin
      LocalAddToLog('No prices');
      Exit;
    end;
    StartTime := IncSecond(Now, -GradientDuration);
    arrPrices := PriceList.GetLastPricesBroken(
                    function(const aPrice: TPrice): Boolean
                    begin
                      //Result := (SecondsBetween(aPrice.TimeStamp, Now) < GradientDuration);
                      Result := aPrice.TimeStamp > StartTime;
                    end);
    if (Length(arrPrices) < 2) then
    begin
      LocalAddToLog('No price array');
      Exit;
    end;

    SetLength(Data, Length(arrPrices));
    for I := 0 to Length(arrPrices) - 1 do
    begin
      Data[I].Time1 := arrPrices[I].TimeStamp;
      Data[I].Time2 := Abs(MilliSecondsBetween(arrPrices[I].TimeStamp, StartTime));
      Data[I].Time3 := Abs(MilliSecondsBetween(arrPrices[I].TimeStamp, StartTime)) / 1000;
      Data[I].Price := arrPrices[I].Value;
    end;

    CalcGradientValues(Data, Result);
    Result.Calculated := true;
    LocalAddToLog('Gradient = '+ Result.Gradient.ToString);
    LocalAddToLog('CorridorWidth = '+ Result.Corridor.ToString);
    LocalAddToLog('LastPosition = '+ Result.LastPosition.ToString);
  finally
    AddToLog(LocalLog);
  end;
end;

procedure TfrmCandidateMain.CalculateGradient(const aNode: PVirtualNode; const aColumnsInfo: TColumnsInfo);
// old version
var
  Data: PInstrumentData;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if (aColumnsInfo.SourceType = stCalcColumn) and
       (aColumnsInfo.CalcColumn.CalculationType in [ctGradientToday, ctCorridorWidth]) and
       (Data^.ExtraColumns.Items.ContainsKey(aColumnsInfo.ColumnId)) then
    begin
      System.Threading.TTask.Create(
        procedure()
        var
          Data: PInstrumentData;
          deltaMax, deltaMin, deltaP, deltaH, deltaT, deltaB: Double;
          sumX, sumY, sumXY, sumSqrX, avgX, avgY, a, b: Double;
          ColumnsItem: TExtraColumns.TColumnsItem;
          corridorValue: Double;
          gradientValue: Double;
          i, n: Integer;
          arrPrices: TArray<TPrice>;
          IsUnsufficientData : Boolean;
          LineDownY1: Double;
          LineUpY1: Double;
          LineUpY2: Double;
          pointValue: Double;
          PrevPrice: Double;
          PriceCount: Integer;
          PriceList: TPriceList;
        begin
          TThread.NameThreadForDebugging('TfrmCandidateMain.CalculateGradient');
          if Assigned(aNode) then
          try
            Data := aNode^.GetData;
            PriceList := TMonitorLists.PriceCache.GetPriceList(Data^.Id);
            corridorValue := 0;
            gradientValue := 0;
            IsUnsufficientData := False;
            if Assigned(PriceList) then
            begin
              if (aColumnsInfo.CalcColumn.CalculationType = ctGradientToday) then
                if (PriceList.Count > 0) and (SecondsBetween(PriceList.FirstTimeStamp, Now) < aColumnsInfo.CalcColumn.Duration) then
                  IsUnsufficientData := True;

              if not IsUnsufficientData then
              begin
                arrPrices := PriceList.GetLastPricesBroken(
                  function(const aPrice: TPrice): Boolean
                  begin
                    Result := (SecondsBetween(aPrice.TimeStamp, Now) < aColumnsInfo.CalcColumn.Duration);
                  end);
                if (Length(arrPrices) = 0) then
                  IsUnsufficientData := True;
              end;

              if not IsUnsufficientData then
              begin
                sumX := 0;
                sumY := 0;
                sumXY := 0;
                sumSqrX := 0;
                for var Price in arrPrices do
                begin
                  sumX := sumX + Price.TimeStamp;
                  sumY := sumY + Price.Value;
                  sumXY := sumXY + Price.TimeStamp * Price.Value;
                  sumSqrX := sumSqrX + Sqr(Price.TimeStamp);
                end;
                PriceCount := Length(arrPrices);
                avgX := sumX / PriceCount;
                avgY := sumY / PriceCount;

                b := (PriceCount * sumXY - sumX * sumY) / (PriceCount * sumSqrX - Sqr(sumX));
                a := avgY - b * avgX;

                deltaMax := 0;
                deltaMin := 0;
                for var Price in arrPrices do
                begin
                  pointValue := (a + b * Price.TimeStamp);
                  if deltaMax < (Price.Value - pointValue) then
                    deltaMax := Price.Value - pointValue;
                  if deltaMin < (pointValue - Price.Value) then
                    deltaMin := pointValue - Price.Value;
                end;

                LineUpY1 := deltaMax + a + b * arrPrices[Low(arrPrices)].TimeStamp;
                LineUpY2 := deltaMax + a + b * arrPrices[High(arrPrices)].TimeStamp;
                LineDownY1 := a + b * arrPrices[Low(arrPrices)].TimeStamp - deltaMin;

                deltaP := LineUpY2 - LineUpY1;
                deltaT := SecondsBetween(arrPrices[High(arrPrices)].TimeStamp, arrPrices[Low(arrPrices)].TimeStamp) / 60 / 60; // in hours
                deltaH := LineUpY1 - LineDownY1;
                deltaB := Sqrt(Sqr(deltaP) + Sqr(deltaT));
                corridorValue := Abs(deltaH * deltaT / deltaB);

                if InRange(deltaP, -0.0000001, 0.0000001) then
                begin
                  if (deltaP * deltaT > 0) then
                    gradientValue := General.MinMaxGradientValue
                  else
                    gradientValue := -General.MinMaxGradientValue;
                end
                else if not IsZero(deltaT, 0.0000001) then
                  gradientValue := deltaP / deltaT
                else
                  gradientValue := 0;
                if IsNan(gradientValue) or IsInfinite(gradientValue) then
                  gradientValue := 0;

                AddToLog('Old gradient = '+ gradientValue.ToString);
                AddToLog('Old corridor = '+ corridorValue.ToString);
              end;
            end;

//            if not(Application.Terminated or (csDestroying in TForm(Self).ComponentState)) and
//              Assigned(Data) and Data^.ExtraColumns.Items.ContainsKey(aColumnsInfo.ColumnId) then
//            begin
//              ColumnsItem := Data^.ExtraColumns.Items[aColumnsInfo.ColumnId];
//              PrevPrice := ColumnsItem.Price;
//              case aColumnsInfo.CalcColumn.CalculationType of
//                ctGradientToday:
//                  ColumnsItem.Price := gradientValue;
//                ctCorridorWidth:
//                  ColumnsItem.Price := corridorValue;
//              end;
//              ColumnsItem.IsUnsufficientData := IsUnsufficientData;
//              if (not IsUnsufficientData) and (ColumnsItem.TimeStamp = 0) then
//                ColumnsItem.TimeStamp := Now;
//
//              if (ColumnsItem.Price = 0) then
//                ColumnsItem.ColTick := clBlack
//              else if (ColumnsItem.Price >= PrevPrice) then
//                ColumnsItem.ColTick := clGreen
//              else
//                ColumnsItem.ColTick := clRed;
//              Data^.ExtraColumns.Items.AddOrSetValue(aColumnsInfo.ColumnId, ColumnsItem);
//              CalculateRankingSum(Data);
//            end;
          except
            on E: Exception do
              TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'CalculateGradient', E.Message);
          end;
        end).Start;
    end;
  end;
end;

procedure TfrmCandidateMain.CalculateGradientLogTerm(const aNode: PVirtualNode; const aColumnsInfo: TColumnsInfo);
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT GRADIENT FROM GET_GRADIENT(:Id, :Weeks)';
begin
  if Assigned(aNode) and
    (aColumnsInfo.SourceType = stCalcColumn) and
    (aColumnsInfo.CalcColumn.CalculationType = ctGradientLogTerm) then
  begin
    System.Threading.TTask.Create(
      procedure()
      var
        ColumnsItem: TExtraColumns.TColumnsItem;
        Data: PInstrumentData;
        Connection: TFDConnection;
        PrevPrice : Double;
        Query: TFDQuery;
        Transaction: TFDTransaction;
      begin
        TThread.NameThreadForDebugging('TfrmCandidateMain.CalculateGradientLogTerm');
        try
          Connection := TFDConnection.Create(nil);
          try
            TFireBirdConnect.SetConnectParams(Connection, TFireBirdConnect.DBNameFeed);
            try
              Connection.Open;
            except
              on E: Exception do
              begin
                TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'CalculateGradientLogTerm',
                                           Connection.Params.Text + '<br>' +
                                           Connection.Params.Database + '<br>' +
                                           E.Message);
                Exit;
              end;
            end;
            Transaction := TFDTransaction.Create(Connection);
            try
              Connection.Transaction := Transaction;
              Transaction.Connection := Connection;
              Transaction.Options.AutoCommit := false;
              Query := TFDQuery.Create(Connection);
              if Assigned(aNode) then
              try
                Data := aNode^.GetData;
                Query.SQL.Text    := C_SQL_SELECT_TEXT;
                Query.Connection  := Connection;
                Query.Transaction := Transaction;
                Query.ParamByName('ID').AsInteger    := Data^.Id;
                Query.ParamByName('WEEKS').AsInteger := aColumnsInfo.CalcColumn.Duration;
                try
                  Query.Prepare;
                  Query.Open;
                  if not (Application.Terminated or (csDestroying in TForm(Self).ComponentState)) and
                     not Query.IsEmpty and not IsNan(Query.FieldByName('GRADIENT').AsFloat) then
                  begin
                    if Data^.ExtraColumns.Items.ContainsKey(aColumnsInfo.ColumnId) then
                    begin
                      ColumnsItem := Data^.ExtraColumns.Items[aColumnsInfo.ColumnId];
                      PrevPrice := ColumnsItem.Price;
                      ColumnsItem.Price := Query.FieldByName('GRADIENT').AsFloat;
                      if (ColumnsItem.Price = 0) then
                        ColumnsItem.ColTick := clBlack
                      else if (ColumnsItem.Price >= PrevPrice) then
                        ColumnsItem.ColTick := clGreen
                      else
                        ColumnsItem.ColTick := clRed;
                        TThread.Queue(nil,
                          procedure
                          var
                            GrData: PInstrumentData;
                          begin
                            if Assigned(aNode) then
                            begin
                              GrData := aNode^.GetData;
                              CalculateRankingSum(GrData);
                              GrData^.ExtraColumns.Items.AddOrSetValue(aColumnsInfo.ColumnId, ColumnsItem);
                              vstCandidate.InvalidateNode(aNode);
                            end;
                          end);
                    end;
                  end;
                except
                  on Er: Exception do
                    TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'CalculateGradientLogTerm', Er.Message + TDModUtils.GetQueryInfo(Query));
                end;
              finally
                FreeAndNil(Query);
              end;
            finally
              if Transaction.Active then
                Transaction.Commit;
              FreeAndNil(Transaction);
            end;
          finally
            if Connection.Connected then
              Connection.Connected := False;
            FreeAndNil(Connection);
          end;
        except
          on En: Exception do
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'CalculateGradientLogTerm', En.Message);
        end;
      end).Start;
  end;
end;

function TfrmCandidateMain.CalculatePriceChangeWeight(
  const aNode: PVirtualNode; const aColumnsInfo: TColumnsInfo): currency;
var Data: PInstrumentData;
    PriceList: TPriceList;
    arrPrices: TArray<TPrice>;
    ColumnsItem: TExtraColumns.TColumnsItem;
    LMaxMinPrice: currency;
    I, LSeconds: Integer;
    LCalc: boolean;
begin
  Result := 0;
  if Assigned(aNode) and
    (aColumnsInfo.SourceType = stPriceChangeColumn) then
  begin
    Data := aNode^.GetData;
    PriceList := TMonitorLists.PriceCache.GetPriceList(Data^.Id);
    if (aColumnsInfo.PriceChangeColumn.LastTickType = lttNone)
       or
       (aColumnsInfo.PriceChangeColumn.LastPriceType = lptNone) then
      Exit;

    ColumnsItem := Data^.ExtraColumns.Items[aColumnsInfo.ColumnId];
    arrPrices := PriceList.GetLastPricesBroken(
                    function(const aPrice: TPrice): Boolean
                    begin
                      Result := (DateOf(aPrice.TimeStamp) = Date);
                    end);
    LCalc := false;
    if Length(arrPrices) >= aColumnsInfo.PriceChangeColumn.LastTickCount then
    begin
      LMaxMinPrice := arrPrices[0].Value;
      if aColumnsInfo.PriceChangeColumn.LastPrice then
      begin
        for I := 1 to Length(arrPrices) - 1 do
          if ((aColumnsInfo.PriceChangeColumn.LastPriceType = lptHigh) and (LMaxMinPrice < arrPrices[I].Value))
             or
             ((aColumnsInfo.PriceChangeColumn.LastPriceType = lptLow) and (LMaxMinPrice > arrPrices[I].Value)) then
            LMaxMinPrice := arrPrices[I].Value;
      end;

      LCalc := true;
      for I := Length(arrPrices) - 1 downto Length(arrPrices) - aColumnsInfo.PriceChangeColumn.LastTickCount + 1 do
      begin
        if aColumnsInfo.PriceChangeColumn.LastTickType = lttUp then
          if arrPrices[I].Value < arrPrices[I-1].Value then
          begin
            LCalc := false;
            break;
          end;
        if aColumnsInfo.PriceChangeColumn.LastTickType = lttDown then
          if arrPrices[I].Value > arrPrices[I-1].Value then
          begin
            LCalc := false;
            break;
          end;
      end;

      if LCalc then
        if aColumnsInfo.PriceChangeColumn.LastPrice then
          LCalc := arrPrices[Length(arrPrices) - 1].Value = LMaxMinPrice;
    end;
    if LCalc then
    begin
      LSeconds := 0;
      if Length(arrPrices) > 0 then
        LSeconds := Abs(SecondsBetween(arrPrices[Length(arrPrices) - 1].TimeStamp, arrPrices[Length(arrPrices) - aColumnsInfo.PriceChangeColumn.LastTickCount].TimeStamp));
      if LCalc and (LSeconds <> 0) then
        Result := aColumnsInfo.PriceChangeColumn.Weight / LSeconds;
    end
    else
      Result := 0;
  end;
end;

function TfrmCandidateMain.GetCreatedOrdersCount: Integer;
begin
  Result := FCandidate.CreatedOrdersCount;
end;

function TfrmCandidateMain.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmCandidateMain.SetAutoTradeInfo(const aAutoTradeInfo: TAutoTradeInfo);
begin
//  IsLoaded := True;
//  try
//    FCandidate.AssignFrom(aAutoTradeInfo);
//    AutoTradeInfoToGUI;
//  finally
//    IsLoaded := False;
//  end;
end;

procedure TfrmCandidateMain.SetCreatedOrdersCount(const Value: Integer);
var lNeedPreExecutionEvaluation: boolean;
begin
  lNeedPreExecutionEvaluation :=
     (CreatedOrdersCount = FCandidate.MaxNumberOrder)
     and
     (Value < FCandidate.MaxNumberOrder);
  FCandidate.CreatedOrdersCount := Value;
  CheckTradesState;
  if (Value > 0) then
    aExecute.ImageIndex := 49 //lightning red
  else
    aExecute.ImageIndex := 12;
  UpdateStatus(C_PANEL_CREATED_ORDERS, Value.ToString);
  UpdateStatus(C_PANEL_TIME_CREATED_ORDERS, FormatDateTime('hh:nn:ss', Now));
  AutoTradesControllerPublisher.UpdateState(Self);
  if lNeedPreExecutionEvaluation then
    PreExecutionEvaluationOrders;
end;

procedure TfrmCandidateMain.SetValueFromPriceCache(const aData: PInstrumentData; const aColumnsInfo: TColumnsInfo);
var
  ColumnsItem: TExtraColumns.TColumnsItem;
begin
  if Assigned(aData) and
    (aData^.ExtraColumns.Items.ContainsKey(aColumnsInfo.ColumnId)) then
  begin
    ColumnsItem := aData^.ExtraColumns.Items[aColumnsInfo.ColumnId];
    if (aColumnsInfo.SourceType = stTickColumn) then
    begin
      if (aColumnsInfo.TickColumn.IBValue2 <> ttNotSet)  and (aColumnsInfo.TickColumn.TypeOperation <> toNone) then
        ColumnsItem.Price := aColumnsInfo.TickColumn.TypeOperation.Calc(TMonitorLists.PriceCache.GetLastPrice(aData^.Id, aColumnsInfo.TickColumn.IBValue1),
                                                                        TMonitorLists.PriceCache.GetLastPrice(aData^.Id, aColumnsInfo.TickColumn.IBValue2))
      else
        ColumnsItem.Price := TMonitorLists.PriceCache.GetLastPrice(aData^.Id, aColumnsInfo.TickColumn.IBValue1);
      if (ColumnsItem.Price <> 0) then
        ColumnsItem.ColTick := clRed;
    end
    else if (aColumnsInfo.SourceType = stEmbargoColumn) and (aColumnsInfo.EmbargoColumn.EmbargoType = etVolumeAmount) then
      ColumnsItem.Price := TMonitorLists.PriceCache.GetLastPrice(aData^.Id, aColumnsInfo.EmbargoColumn.IBValue)
                           *
                           TMonitorLists.PriceCache.GetLastPrice(aData^.Id, ttLast)
    else
      Exit;
    aData^.ExtraColumns.Items.AddOrSetValue(aColumnsInfo.ColumnId, ColumnsItem);
    vstCandidate.InvalidateNode(aData^.Node);
  end;
end;

procedure TfrmCandidateMain.ShowCandidateMarket(const aColumnID: Integer; const aScanOptions: TCandidateColumn);
resourcestring
  rsInstances = 'Limit of Market Candidate subscriptions is reached!';
begin
  if (aOpenIBScanning.Tag < 10) then
    TfrmCandidateMarket.ShowDocument(aColumnID, aScanOptions, Self)
  else
    TMessageDialog.ShowWarning(rsInstances);
end;

procedure TfrmCandidateMain.RefreshColumnValues(const aColumnId: integer);
var
  Data: PInstrumentData;
  Node: PVirtualNode;
  ColumnsInfo: TColumnsInfo;
begin
  vstCandidate.BeginUpdate;
  try
    ColumnsInfo := FColumns.Items[aColumnId];
    if ((ColumnsInfo.SourceType = stEmbargoColumn) and (ColumnsInfo.EmbargoColumn.EmbargoType = etVolumeAmount))
       or
       (ColumnsInfo.SourceType = stTickColumn) then
    begin
      Node := vstCandidate.GetFirstChild(vstCandidate.RootNode);
      while Assigned(Node) do
      begin
        Data := Node^.GetData;
        SetValueFromPriceCache(Data, ColumnsInfo);
        CheckRankingCriteria(Data);
        PreExecutionEvaluation(Data);
        Node := Node.NextSibling;
      end;
    end;
  finally
    vstCandidate.EndUpdate;
  end;
end;

procedure TfrmCandidateMain.RequestMarketData(const aContractId: Integer);
var
  Request: TIABRequest;
  Order: TIABOrder;
begin
  Order := nil;
  if SokidList.ContainsKey(aContractId) then
    Order := SokidList.GetOrderByConID(aContractId);
  if Assigned(Order) then
    try
      if not(Order.SecurityType in [stCash]) then
      begin
        Request := TIABRequest.Create(ibGetHistoricalData,
                                      Order.ContractId,
                                      Order,
                                      qpNormal);
        {Request.HistoricalDataParams := THistoricalDataParams.Create(FCandidate.HistoricalDataParams.DurationTimeUnits,
                                                                     FCandidate.HistoricalDataParams.DataDuration,
                                                                     FCandidate.HistoricalDataParams.BarSize,
                                                                     FCandidate.HistoricalDataParams.DataBasis,
                                                                     FCandidate.HistoricalDataParams.SubscribeHistData); }
        IABClient.SendRequest(Request);
      end;
    finally
      FreeAndNil(Order);
    end;
end;

procedure TfrmCandidateMain.ShowTradeChart(const aNode: PVirtualNode);
var
  Data: PInstrumentData;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if not Assigned(Data.TradeChart) then
    begin
      Data^.TradeChart := TfrmTradeChartForm.Create(nil);
      RequestMarketData(Data^.Id);
      TfrmTradeChartForm(Data^.TradeChart).Initialize(Data^.Id);
    end;
    Data^.TradeChart.Show;
  end;
end;

procedure TfrmCandidateMain.OnHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
var
  Data: PInstrumentData;
  InstrumentItem: TInstrumentItem;
  Node: PVirtualNode;
  PriceList: TPriceList;
  arrPrices: TArray<TPrice>;
  ColumnsItem: TExtraColumns.TColumnsItem;
begin
  if (Item = Count) and FInstrumentList.ContainsKey(DataId) then
  begin
    {if not FCandidate.HistoricalDataParams.KeepUpdated then
      IABClient.CancelHistoricalData(DataId);  }

    InstrumentItem := FInstrumentList.GetItem(DataId);
    if Assigned(InstrumentItem) then
      for Node in InstrumentItem.NodeList do
        if Assigned(Node) then
        begin
          Data := Node^.GetData;
          if Assigned(Data^.TradeChart) then
          begin
            PriceList := TMonitorLists.PriceCache.GetPriceList(DataId);
            arrPrices := PriceList.GetLastPrices(
              function(const aPrice: TPrice): Boolean
              begin
                Result := aPrice.IsHistorical and (Trunc(aPrice.TimeStamp) = Trunc(Date)) and (aPrice.TickType in [ttClose, ttLast]);
              end);
            for var Price in arrPrices do
              TfrmTradeChartForm(Data^.TradeChart).AddValue(Price.Value, Price.TimeStamp);
          end;
          for var ColumnsInfo in FColumns.Values do
            if (ColumnsInfo.SourceType = stCalcColumn) and
               (ColumnsInfo.CalcColumn.CalculationType in [ctGradientToday, ctCorridorWidth]) and
               (Data^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId)) then
            begin
              ColumnsItem := Data^.ExtraColumns.Items[ColumnsInfo.ColumnId];
              CalculateGradient(Node, ColumnsInfo);
            end;
        end;
  end;
end;

procedure TfrmCandidateMain.aOpenIBScanningExecute(Sender: TObject);
var
  ScanOptions: TCandidateColumn;
begin
  ScanOptions.Clear;
  ShowCandidateMarket(-1, ScanOptions);
end;

procedure TfrmCandidateMain.IncMarketInstances;
begin
  aOpenIBScanning.Tag := aOpenIBScanning.Tag + 1;
end;

procedure TfrmCandidateMain.DecMarketInstances;
begin
  aOpenIBScanning.Tag := aOpenIBScanning.Tag - 1;
end;

procedure TfrmCandidateMain.SetScanOptions(const aColumnId: Integer; const aScanOptions: TCandidateColumn);
var
  ColumnsInfo: TColumnsInfo;
begin
    for var Id in FColumns.Keys do
    begin
      ColumnsInfo := FColumns.Items[Id];
      if (ColumnsInfo.SourceType = stCandidateMarket) and (ColumnsInfo.ColumnId = aColumnId) then
      begin
        ColumnsInfo.CandidateColumn := aScanOptions;
        FColumns.AddOrSetValue(Id, ColumnsInfo);

        for var Index := 0 to vstCandidate.Header.Columns.Count - 1 do
          if (vstCandidate.Header.Columns[Index].Id = ColumnsInfo.ColumnId) then
          begin
            vstCandidate.Header.Columns[Index].Hint := 'Weight: ' + aScanOptions.Weight.ToString;
            vstCandidate.Header.Columns[Index].Text := ColumnsInfo.Caption;
            Break;
          end;
        if (ColumnsInfo.CandidateColumn.ScanId > 0) then
        begin
          IABClient.CancelScan(ColumnsInfo.CandidateColumn.ScanId);
          ColumnsInfo.CandidateColumn.ScanId := 0;
        end;
        vstCandidate.Clear;
        FInstrumentList.Clear;
        ExecuteActions(ColumnsInfo);
        FillColumnList;
        Break;
      end;
    end;
end;

procedure TfrmCandidateMain.aOpenStaticListExecute(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
begin
  if (TfrmCandidateStaticLists.ShowDocument(dmInsert, kaAddAll, -1, Self) = mrOk) then
  begin
    for var Id in FColumns.Keys do
    begin
      ColumnsInfo := FColumns.Items[Id];
      if (ColumnsInfo.SourceType = stStaticList) then
      begin
        ColumnsInfo.StaticColumn.FromDB(ColumnsInfo.StaticColumn.RecordId);
        ColumnsInfo.Weight := ColumnsInfo.StaticColumn.Weight;
        FColumns.AddOrSetValue(Id, ColumnsInfo);

        for var Index := 0 to vstCandidate.Header.Columns.Count - 1 do
          if (vstCandidate.Header.Columns[Index].Id = ColumnsInfo.ColumnId) then
          begin
            vstCandidate.Header.Columns[Index].Hint := 'Weight: ' + ColumnsInfo.Weight.ToString;
            vstCandidate.Header.Columns[Index].Text := ColumnsInfo.Caption;
            Break;
          end;
      end;
    end;
    FillColumnList;
    CalculateAllRankingSum;
    PreExecutionEvaluationOrders;
  end;
end;

procedure TfrmCandidateMain.cbAutoOrderActiveClick(Sender: TObject);
begin
  GUIToAutoTradeInfo;
  if not FCandidate.Active then
  begin
    FCandidate.Active := True;
    TradesState := tsSuspended;
  end
  else
  begin
    TradesState := tsWorking;
    //lblStartedTime.Caption := 'Started: ' + TimeToStr(Now);
  end;
  AutoTradesControllerPublisher.UpdateState(Self);
end;

procedure TfrmCandidateMain.cbAutoRefreshClick(Sender: TObject);
begin
  if Showing then
  begin
    GUIToAutoTradeInfo;
    if cbAutoRefresh.Checked then
      for var ColumnsInfo in FColumns.Values do
        if (ColumnsInfo.SourceType = stCandidateMarket) then
          ExecuteActions(ColumnsInfo);
  end;
end;

procedure TfrmCandidateMain.aExecuteExecute(Sender: TObject);

  function CheckRequired: Boolean;
  var
    Problems: string;
  begin
    Problems := '';
    {if (FCandidate.MaxRows = 0) then
    begin
      SetFocusSafely(seMaxRows);
      Problems := Format(rcRequiredValue, ['Max rows']);
    end;
    if (FCandidate.OrderAmount = 0) then
    begin
      SetFocusSafely(seSingleOrderAmount);
      Problems := Problems + Format(rcRequiredValue, ['Single order amount']);
    end;
    if (FCandidate.TotalOrderAmount = 0) then
    begin
      SetFocusSafely(seTotalOrderAmount);
      Problems := Problems + Format(rcRequiredValue, ['Total order amount']);
    end;
    if FCandidate.OrderCurrency.IsEmpty then
    begin
      SetFocusSafely(cbOrderCurrency);
      Problems := Problems + Format(rcRequiredValue, ['Order Currency']);
    end;}
    if (FCandidate.MaxNumberOrder = 0) then
    begin
      SetFocusSafely(seMaxNumberOrder);
      Problems := Problems + Format(rcRequiredValue, ['Max number of order']);
    end
    else if (CreatedOrdersCount >= FCandidate.MaxNumberOrder) then
    begin
      SetFocusSafely(seMaxNumberOrder);
      Problems := Problems + rcCreatedOrdersCountGreater;
    end;

    if not FCandidate.Active then
    begin
      SetFocusSafely(cbAutoOrderActive);
      Problems := Problems + rcAutotradingNotActive;
    end;

    Result := Problems.IsEmpty;
    if not Result then
      TMessageDialog.ShowWarning(Problems);
  end;

begin
  try
    if not FExplorationMode then
    begin
      //FCandidate.TotalOrderAmount := seTotalOrderAmount.Value;
      FCandidate.Active := True;
    end;
    if CheckRequiredEmbargoColumn and CheckRequired then
      ApplyAutoOrder;
  finally
    if not FExplorationMode then
      FCandidate.Active := False;
  end;
end;

procedure TfrmCandidateMain.aExecuteDuplicateOrderExecute(Sender: TObject);
begin
  if cbAllowSendDuplicateOrder.Checked then
  begin
    CreatedOrdersCount := 0;
    PreExecutionEvaluationOrders;
  end;
end;

procedure TfrmCandidateMain.cbOrderCurrencyAddClick(Sender: TObject);
begin
  if (cbOrderCurrency.Items.IndexOf(UpperCase(cbOrderCurrency.Text)) = -1) then
    cbOrderCurrency.Items.Add(UpperCase(cbOrderCurrency.Text));
end;

procedure TfrmCandidateMain.cbOrderCurrencyKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['A' .. 'Z', 'a' .. 'z', #08]) then
    Key := #0;
end;

procedure TfrmCandidateMain.OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
var
  ContractId: Integer;
  Data: PInstrumentData;
  InstrumentItem: TInstrumentItem;
  InstrumentName: string;
  Node: PVirtualNode;
begin
  if (IABClient.InstrumentSpecs.Items[Index].DataID = 1) then
  begin
    ContractId     := IABClient.InstrumentSpecs.Items[Index].ContractId;
    InstrumentName := IABClient.InstrumentSpecs.Items[Index].LongName;
    if not InstrumentName.IsEmpty and FInstrumentList.ContainsKey(ContractId) then
    begin
      InstrumentItem := FInstrumentList.GetItem(ContractId);
      if Assigned(InstrumentItem) then
        for Node in InstrumentItem.NodeList do
          if Assigned(Node) then
          begin
            Data := Node^.GetData;
            if SokidList.ContainsKey(ContractId) then
              Data^.Multiplier := VarToIntDef(SokidList.Items[ContractId].Multiplier);
            if Data^.SecurityType in [stIndex, stOption, stFuture] then
              Data^.Name := IABClient.InstrumentSpecs.Items[Index].LocalSymbol
            else
              Data^.Name := IABClient.InstrumentSpecs.Items[Index].LongName;
          end;
    end;
  end;
end;

procedure TfrmCandidateMain.OnOpenOrder(Sender: TObject; Order: TIABOrder);
begin
end;

procedure TfrmCandidateMain.OnOpenOrderNN(const aOrderList: array of TOrder);
begin
end;

procedure TfrmCandidateMain.OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
begin
  if (Order.Completed or (Status in [osCancelled, osError])) and (Order.TempId > 0) then  //Only TWS API orders
    CheckAlwaysMax(Order.TempId, Status);
end;

procedure TfrmCandidateMain.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
var
  ColumnsInfo: TColumnsInfo;
  Data: PInstrumentData;
  InstrumentItem : TInstrumentItem;
  Node: PVirtualNode;
  ColumnsItem: TExtraColumns.TColumnsItem;
  PrevPrice: Double;
  GradientValues: TDictionary<integer, TGradientRecord>;
  ColorCalculated: boolean;
begin
  GradientValues := TDictionary<integer, TGradientRecord>.Create;
  try
    if (TickType = ttLast) then
      FIsPriceExists := FInstrumentList.ContainsKey(Id);

    if not FInstrumentList.ContainsKey(Id) then Exit;

    if (TickType = ttLast) then
    begin
      AddToLog('');
      AddToLog('PriceChange for '+ SokidList.GetItem(Id).Name);
      for ColumnsInfo in FColumns.Values do
        if (ColumnsInfo.SourceType = stCalcColumn) then
          if ColumnsInfo.CalcColumn.CalculationType in [ctGradientCalc, ctCorridorWidth, ctlastPosition] then
            if not GradientValues.ContainsKey(ColumnsInfo.CalcColumn.Duration) then
              GradientValues.AddOrSetValue(ColumnsInfo.CalcColumn.Duration, CalcGradient(Id, ColumnsInfo.CalcColumn.Duration, rgGradientCalcMode.ItemIndex + 1));
    end;

    for ColumnsInfo in FColumns.Values do
      if ((ColumnsInfo.SourceType = stTickColumn) and ((ColumnsInfo.TickColumn.IBValue1 = TickType) or (ColumnsInfo.TickColumn.IBValue2 = TickType))) or
         ((ColumnsInfo.SourceType = stCalcColumn) and (TickType = ttLast) and (ColumnsInfo.CalcColumn.CalculationType in [ctGradientCalc, ctCorridorWidth, ctLastPosition])) or
         ((ColumnsInfo.SourceType = stEmbargoColumn) and (ColumnsInfo.EmbargoColumn.EmbargoType = etVolumeAmount) and (TickType in [ColumnsInfo.EmbargoColumn.IBValue, ttLast])) or
         (ColumnsInfo.SourceType = stPriceChangeColumn) then
      begin
        InstrumentItem := FInstrumentList.GetItem(Id);
        if Assigned(InstrumentItem) then
        begin
          for Node in InstrumentItem.NodeList do
            if Assigned(Node) then
            begin
              Data := Node^.GetData;
              if Assigned(Data) then
              begin
                if (ColumnsInfo.ColumnId > 0) and (Data^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId)) then
                begin
                  ColorCalculated := false;
                  ColumnsItem := Data^.ExtraColumns.Items[ColumnsInfo.ColumnId];
                  PrevPrice := ColumnsItem.Price;

                  case ColumnsInfo.SourceType of
                    stStaticList:
                      ;
                    stCandidateMarket:
                      ;
                    stTickColumn:
                      begin
                        if (ColumnsInfo.TickColumn.IBValue2 <> ttNotSet) and (ColumnsInfo.TickColumn.TypeOperation <> toNone) then
                          ColumnsItem.Price := ColumnsInfo.TickColumn.TypeOperation.Calc(TMonitorLists.PriceCache.GetLastPrice(Id, ColumnsInfo.TickColumn.IBValue1),
                                                                                         TMonitorLists.PriceCache.GetLastPrice(Id, ColumnsInfo.TickColumn.IBValue2))
                        else
                          ColumnsItem.Price := TMonitorLists.PriceCache.GetLastPrice(Id, ColumnsInfo.TickColumn.IBValue1);
                        if (ColumnsItem.TimeStamp = 0) then
                          ColumnsItem.TimeStamp := TimeStamp;
                      end;
                    stCalcColumn:
                      begin
                        if (TickType = ttLast) then
                        begin
                          //CalculateGradient(Node, ColumnsInfo);
                          ColumnsItem.IsUnsufficientData := false;
                          ColumnsItem.Price := 0;
                          if GradientValues.ContainsKey(ColumnsInfo.CalcColumn.Duration) then
                          begin
                            ColumnsItem.IsUnsufficientData := not GradientValues.Items[ColumnsInfo.CalcColumn.Duration].Calculated;
                            case ColumnsInfo.CalcColumn.CalculationType of
                              ctGradientCalc: ColumnsItem.Price := GradientValues.Items[ColumnsInfo.CalcColumn.Duration].Gradient;
                              ctCorridorWidth: ColumnsItem.Price := GradientValues.Items[ColumnsInfo.CalcColumn.Duration].Corridor;
                              ctlastPosition: ColumnsItem.Price := GradientValues.Items[ColumnsInfo.CalcColumn.Duration].LastPosition;
                            end;
                          end;
                          if (ColumnsItem.Price >= ColumnsInfo.CalcColumn.Value1)
                              and
                             (ColumnsItem.Price <= ColumnsInfo.CalcColumn.Value2) then
                            ColumnsItem.ColTick := clGreen
                          else if not ColumnsItem.IsUnsufficientData then
                            ColumnsItem.ColTick := clRed
                          else
                            ColumnsItem.ColTick := clBlack;
                          ColorCalculated := true;
                        end;
                      end;
                    stPriceChangeColumn:
                      begin
                        // stop calculate if order is filled
                        if Assigned(Data.MotherOrderDoc) and (Data.MotherOrderDoc.Filled > 0) then
                          ColumnsItem.StopCalculatePriceChange := true
                        else
                          ColumnsItem.PriceChangeWeight := CalculatePriceChangeWeight(Node, ColumnsInfo);
                      end;
                    stEmbargoColumn:
                      ColumnsItem.Price := TMonitorLists.PriceCache.GetLastPrice(Id, ColumnsInfo.EmbargoColumn.IBValue)
                                           *
                                           TMonitorLists.PriceCache.GetLastPrice(Id, ttLast);
                  end;

                  if not ColorCalculated then
                  begin
                    if (ColumnsItem.Price = 0) then
                      ColumnsItem.ColTick := clBlack
                    else if (ColumnsItem.Price >= PrevPrice) then
                      ColumnsItem.ColTick := clGreen
                    else
                      ColumnsItem.ColTick := clRed;
                  end;

                  Data^.ExtraColumns.Items.AddOrSetValue(ColumnsInfo.ColumnId, ColumnsItem);
                  CalculateRankingSum(Data);
                  vstCandidate.InvalidateNode(Node);
                end;
              end;
            end;
        end;
        TThread.Queue(nil,
          procedure
          begin
            PreExecutionEvaluationOrders;
          end);
      end;
  finally
    GradientValues.Free;
  end;
end;

procedure TfrmCandidateMain.OnRebuildFromTWS(Sender: TObject);
begin
end;

procedure TfrmCandidateMain.OnScannerData(Sender: TObject; Scan: TIABScan);
var
  i           : Integer;
  Instruments : TArrayInstrumentData;
  ColumnsInfo : TColumnsInfo;
  ColumnsItem : TExtraColumns.TColumnsItem;
  SokidInfo   : TSokidInfo;

  procedure ClearScanResult(AColumnId: Integer);
  var
    ColumnsInfo: TColumnsInfo;
    Data: PInstrumentData;
    Node: PVirtualNode;
    ColumnSum: Integer;
  begin
    vstCandidate.BeginUpdate;
    try
      Node := vstCandidate.GetFirst;
      while Assigned(Node) do
      begin
        Data := Node^.GetData;
        if Data^.ExtraColumns.Items.ContainsKey(AColumnId) and (not Data^.IsLocked) then
        begin
          ColumnSum := 0;
          for ColumnsInfo in FColumns.Values do
            if (ColumnsInfo.SourceType in [stStaticList, stCandidateMarket]) and
              Data^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId) and
              (Data^.ExtraColumns.Items[ColumnsInfo.ColumnId].Rank > 0) then
              Inc(ColumnSum, ColumnsInfo.ColumnId);
          if (ColumnSum = AColumnId) and (not Data^.IsLocked) then
          begin
            ColumnsItem := Data^.ExtraColumns.Items[ColumnsInfo.ColumnId];
            ColumnsItem.Rank := 0;
            Data^.ExtraColumns.Items[ColumnsInfo.ColumnId] := ColumnsItem;
          end;
        end;
        Node := vstCandidate.GetNext(Node);
      end;
    finally
      vstCandidate.EndUpdate;
    end;
  end;

begin
  UpdateStatus(C_PANEL_SCAN_COUNT, Scan.Count.ToString);
  for ColumnsInfo in FColumns.Values do
    if (ColumnsInfo.SourceType = stCandidateMarket) and (ColumnsInfo.CandidateColumn.ScanId = Scan.ScanId) then
    begin
      ClearScanResult(ColumnsInfo.ColumnId);
      SetLength(Instruments, Scan.Count);
      for i := 0 to Scan.Count - 1 do
      begin
        with Instruments[i] do
        begin
          if SokidList.ContainsKey(Scan[i].ContractId) then
          begin
            SokidInfo      := SokidList.Items[Scan[i].ContractId];
            Name           := SokidInfo.Name;
            SecurityType   := SokidInfo.GetSecurityType;
            Symbol         := SokidInfo.Symbol;
            LocalSymbol    := SokidInfo.LocalSymbol;
            Isolate        := SokidInfo.Isolate;
            Multiplier     := VarToIntDef(SokidInfo.Multiplier);
            TWSMessageItem := SokidInfo.TWSMessageItem;
          end
          else
          begin
            Name         := Scan[i].TradingClass;
            SecurityType := Scan[i].SecurityType;
            Symbol       := Scan[i].Symbol;
            LocalSymbol  := Scan[i].LocalSymbol;
            Isolate      := 0;
            Multiplier   := 0;
          end;
          Currency      := Scan[i].Currency;
          Exchange      := Scan[i].Exchange;
          Id            := Scan[i].ContractId;
          BrokerType    := TBrokerType.brIB;
          InitTimeStamp := Now;
          RecordId      := -1;
          if SecurityType in [stIndex, stOption] then
            Name := SokidInfo.LocalSymbol;

          ExtraColumns := TExtraColumns.Create(0);
          ColumnsItem.OriginRank := Scan.Count - Scan.Items[i].Rank;
          ColumnsItem.Rank := ColumnsInfo.CandidateColumn.Weight * (1 + (Scan.Count - Scan.Items[i].Rank - 1) * ColumnsInfo.CandidateColumn.Step);
          if IsNan(ColumnsItem.Rank) then
            ColumnsItem.Rank := 0;
          ColumnsItem.ColTick := clBlack;
          ExtraColumns.Items.AddOrSetValue(ColumnsInfo.ColumnId, ColumnsItem);
        end;
      end;
      FCandidate.LastUpdate := Now;
      AddInstrument(@Instruments, ColumnsInfo);
      TThread.Queue(nil,
        procedure
        begin
          UpdateStatus(C_PANEL_TIME_UPDATE, TimeToStr(FCandidate.LastUpdate));
          {if not FCandidate.AutoRefresh then
          begin
            IABClient.CancelScan(Scan.ScanId);
            TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, Self, 'OnCandidateData', 'Unsubscribed intentionally, to bypass the limit on the number of scans in IB');
          end; }
        end);
      AutoTradesControllerPublisher.UpdateState(Self);
      Break;
    end;
end;

procedure TfrmCandidateMain.OnScannerParam(Sender: TObject; Parameters: string);
begin
  //
end;

procedure TfrmCandidateMain.OnScannerAdd(Sender: TObject; ScanId: Integer);
begin
  UpdateStatus(C_PANEL_TOTAL_NUM_SCANS, IABClient.Scanner.Count.ToString);
end;

procedure TfrmCandidateMain.OnScannerCancel(Sender: TObject; ScanId: Integer);
var
  ColumnsInfo: TColumnsInfo;
begin
  UpdateStatus(C_PANEL_TOTAL_NUM_SCANS, IABClient.Scanner.Count.ToString);
  for var RunColumnsInfo in FColumns.Values do
    if (RunColumnsInfo.SourceType = stCandidateMarket) and (RunColumnsInfo.CandidateColumn.ScanId = ScanId) then
    begin
      ColumnsInfo.AssignFrom(RunColumnsInfo);
      ColumnsInfo.CandidateColumn.ScanId := 0;
      FColumns.AddOrSetValue(ColumnsInfo.ColumnId, ColumnsInfo);
    end
end;

procedure TfrmCandidateMain.OnCloseOrder(const aTempId: Integer);
begin
end;

procedure TfrmCandidateMain.OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
begin
  if (ErrorCode <> 322) and (TempId > 0) then
    CheckAlwaysMax(TempId, osError);
end;

procedure TfrmCandidateMain.OnExecution(Sender: TObject; Order: TIABOrder);
begin
end;

procedure TfrmCandidateMain.OnGUIToAutoTradeInfo(Sender: TObject);
begin
  if Showing then
    GUIToAutoTradeInfo;
end;

procedure TfrmCandidateMain.TimerCalculateGradientTimer(Sender: TObject);
begin
  CalculateValuesForAllNodes;
end;

procedure TfrmCandidateMain.UpdateCaptions;
begin
  {lblAutoTradesName.Caption := Concat('AutoTrade Name: ', FCandidate.Name, ' (', FCandidate.RecordId.ToString, ')').Replace(sLineBreak, '');
  lblInstanceNum.Caption    := Concat('Instance Num: ', Abs(FCandidate.InstanceNum).ToString);
  if (FCandidate.InstanceNum > 0) then
    lblInstanceNum.Caption := Concat(lblInstanceNum.Caption, ' (Autotrade)')
  else
    lblInstanceNum.Caption := Concat(lblInstanceNum.Caption, ' (Manual)');

  FCandidate.Note := lbColumns.Items.Text;
  if (FCandidate.InstanceNum <= 0) then
    AutoTradesControllerPublisher.UpdateState(Self); }
end;

procedure TfrmCandidateMain.UpdateStatus(const aPanelNum: Byte; const aInfo: string);
begin
  sbMain.Panels[aPanelNum].Text := aInfo;
  sbMain.Refresh;
end;

procedure TfrmCandidateMain.GUIToAutoTradeInfo;
begin
  if not IsLoaded then
  begin
    if FExplorationMode then
    begin
      FCandidate.Active                  := cbAutoOrderActive.Checked;
      FCandidate.MaxNumberOrder          := seMaxNumberOrder.Value;
      FCandidate.AlwaysMax               := cbAlwaysMax.Checked;
      FCandidate.RepeatInstruments       := cbRepeatInstruments.Checked;
    end;
    FCandidate.Name := edAutoTradeTemplate.Text;

    if FCandidate.Active and
       (CreatedOrdersCount < FCandidate.MaxNumberOrder) then
      TradesState := tsWorking
    else
      TradesState := tsSuspended;
    CheckTradesState;
    AutoTradesControllerPublisher.UpdateState(Self);
//    if Assigned(FAutoTradesEdit) then
//      FAutoTradesEdit.SetAutoTradeInfo(FCandidate);
  end;
end;

procedure TfrmCandidateMain.AutoTradeInfoToGUI;
begin
  edAutoTradeTemplate.Text          := FCandidate.Name;
  cbAutoOrderActive.Checked         := FCandidate.Active;
  seMaxNumberOrder.Value            := FCandidate.MaxNumberOrder;
  cbAlwaysMax.Checked               := FCandidate.AlwaysMax;
  cbRepeatInstruments.Checked       := FCandidate.RepeatInstruments;
  UpdateCaptions;
//  if Assigned(FAutoTradesEdit) then
//    FAutoTradesEdit.SetAutoTradeInfo(FCandidate);
end;

procedure TfrmCandidateMain.btnSaveClick(Sender: TObject);
begin
  if CheckRequiredEmbargoColumn then
  begin
    FCandidate.ColumnsInfo := ColumnsInfoToXml;
    FCandidate.Name := edAutoTradeTemplate.Text;
    FCandidate.MaxNumberOrder := seMaxNumberOrder.Value;
    FCandidate.AlwaysMax               := cbAlwaysMax.Checked;
    FCandidate.RepeatInstruments       := cbRepeatInstruments.Checked;
    //FCandidate.SaveToDB;
    //AutoTradeInfoToGUI;
    ModalResult := mrOk;
  end;
end;

procedure TfrmCandidateMain.vstCandidateBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PInstrumentData;
begin
  Data := Node^.GetData;
  if (Data^.IsCriteria) then
  begin
    TargetCanvas.Brush.Color := $00EEEEEE;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TfrmCandidateMain.vstCandidateCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PInstrumentData;
  ColumnsItem1, ColumnsItem2: TExtraColumns.TColumnsItem;
  ColumnsInfo: TColumnsInfo;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    COL_INSTRUMENT:
      Result := CompareText(Data1^.Name, Data2^.Name);
    COL_RANKING_SUM:
      Result := CompareValue(Data1^.ExtraColumns.RankingSum, Data2^.ExtraColumns.RankingSum);
  else
    if (Column > -1) and
      FColumns.TryGetValue(vstCandidate.Header.Columns[Column].Id, ColumnsInfo) and
      (Data1^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId)) and
      (Data2^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId)) then
    begin
      ColumnsItem1 := Data1^.ExtraColumns.Items[ColumnsInfo.ColumnId];
      ColumnsItem2 := Data2^.ExtraColumns.Items[ColumnsInfo.ColumnId];
      Result := CompareValue(ColumnsItem1.Price, ColumnsItem2.Price);
    end;
  end;
end;

procedure TfrmCandidateMain.vstCandidateDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TfrmCandidateMain.vstCandidateDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := (Sender <> Source) and (Source is TVirtualStringTree);
end;

procedure TfrmCandidateMain.vstCandidateDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  ColumnsInfo: TColumnsInfo;
  ColumnsItem: TExtraColumns.TColumnsItem;
  Data: PInstrumentData;
begin
  Data := Node^.GetData;
  if (Data^.TWSMessageItem.ErrorCode > 0) then
    TargetCanvas.Font.Color := clRed;

  if (Column = COL_RANKING_SUM) then
  begin
    TargetCanvas.Font.Style := [fsBold];
    TargetCanvas.Font.Color := Data^.ExtraColumns.RankingColTick;
  end
  else if (Column = COL_INSTRUMENT) then
  begin

  end
  else if FColumns.TryGetValue(vstCandidate.Header.Columns[Column].Id, ColumnsInfo) and
          (Data^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId)) then
  begin
    ColumnsItem := Data^.ExtraColumns.Items[ColumnsInfo.ColumnId];
    TargetCanvas.Font.Color := ColumnsItem.ColTick;
    if (ColumnsInfo.SourceType = stEmbargoColumn) then
      TargetCanvas.Font.Color := ColumnsInfo.EmbargoColumn.GetCriteriaColor(@ColumnsInfo, @Data^.ExtraColumns)
    else if (ColumnsInfo.SourceType = stCandidateMarket) and (ColumnsInfo.CandidateColumn.Weight = 0) then
      TargetCanvas.Font.Color := clMedGray
    else if (ColumnsInfo.SourceType = stPriceChangeColumn) and ColumnsItem.StopCalculatePriceChange then
      TargetCanvas.Font.Color := clGray
    else if (ColumnsItem.Weight <> 0) then
    begin
      TargetCanvas.Brush.Color := clSilver;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
end;

procedure TfrmCandidateMain.vstCandidateFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PInstrumentData;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
  begin
//    if not(csDestroying in Self.ComponentState) then
      if Assigned(FSubscribedList) and FSubscribedList.Contains(Data^.Id) then
      begin
//        if FCandidate.HistoricalDataParams.KeepUpdated then
//          FSubscribedList.Extract(Data^.Id);
        IABClient.CancelHistoricalData(Data^.Id);
      end;
    Data^.Clear;
  end;
end;

procedure TfrmCandidateMain.vstCandidateGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PInstrumentData;
  ColumnsInfo: TColumnsInfo;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    COL_INSTRUMENT:
      begin
        if not Data^.Description.IsEmpty then
          CellText := Data^.Name + ' (' + Data^.Description + ')'
        else if Data^.IsLocked then
          CellText := Data^.Name + ' (Locked)'
        else
          CellText := Data^.Name;
      end;
    COL_RANKING_SUM:
      CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.ExtraColumns.RankingSum);
  else
    if FColumns.TryGetValue(vstCandidate.Header.Columns[Column].Id, ColumnsInfo) then
      CellText := Data^.ExtraColumns.GetText(ColumnsInfo, rgWeightedFeed.ItemIndex = 0);
  end;
end;

procedure TfrmCandidateMain.vstCandidateHeaderDblClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var
  ColumnId: Integer;
begin
  if (HitInfo.Column > C_FIXED_COLUMN_INDEX - 1) then
  begin
    ColumnId := vstCandidate.Header.Columns[HitInfo.Column].Id;
    ShowColumnDetails(ColumnId);
  end;
end;

procedure TfrmCandidateMain.vstCandidateHeaderDragging(Sender: TVTHeader;Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column > C_FIXED_COLUMN_INDEX;
end;

procedure TfrmCandidateMain.vstCandidateHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements := [hpeBackground];
end;

procedure TfrmCandidateMain.vstCandidateHeaderMouseDown(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitInfo: THitInfo;
begin
  if (Button = TMouseButton.mbRight) then
  begin
    vstCandidate.GetHitTestInfoAt(X, Y, False, HitInfo);
    if HitInfo.HitColumn > 0 then
      EditWeigthColumn(HitInfo.HitColumn);
  end;
end;

procedure TfrmCandidateMain.vstCandidateAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  if (hpeBackground in Elements) then
  begin
    PaintInfo.TargetCanvas.Brush.Color := clWhite;
    PaintInfo.TargetCanvas.FillRect(PaintInfo.PaintRectangle);
    if Assigned(PaintInfo.Column) then
    begin
      for var Column in FColumns.Values do
        if (Column.SourceType = stEmbargoColumn) and (Column.ColumnId = PaintInfo.Column.Id) then
        begin
          PaintInfo.TargetCanvas.Brush.Color := clWebMintcream;
          PaintInfo.TargetCanvas.FillRect(PaintInfo.PaintRectangle);
          Break;
        end;
    end;
    DrawEdge(PaintInfo.TargetCanvas.Handle, PaintInfo.PaintRectangle, BDR_RAISEDINNER, BF_RIGHT);
  end;
end;

procedure TfrmCandidateMain.vstCandidateGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

  function GetHintString: string;
  var
    Data: PInstrumentData;
    ColumnsInfo: TColumnsInfo;
    ColumnsItem: TExtraColumns.TColumnsItem;
  begin
    Result := '';
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if FColumns.ContainsKey(vstCandidate.Header.Columns[Column].Id) then
      begin
        ColumnsInfo := FColumns[vstCandidate.Header.Columns[Column].Id];
        ColumnsItem := Data^.ExtraColumns.Items[ColumnsInfo.ColumnId];
        if (ColumnsItem.Weight > 0) then
          Result := 'Weight: ' + ColumnsItem.Weight.ToString;
        if (ColumnsItem.TimeStamp > 0) then
          Result := Result + ' InitTime: ' + TimeToStr(ColumnsItem.TimeStamp);
      end;
    end;
  end;

begin
  HintText := GetHintString;
end;

procedure TfrmCandidateMain.vstCandidateInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PInstrumentData;
begin
  Data := Sender.GetNodeData(Node);
  Data^.RecordId := -1;
end;

procedure TfrmCandidateMain.vstCandidatePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  Data: PInstrumentData;
begin
  Data := Node^.GetData;
  if (Column = COL_INSTRUMENT) and Data^.IsLocked then
  begin
    TargetCanvas.Font.Style := [fsBold];
    TargetCanvas.Font.Color := clBlack;
  end;
end;

function TfrmCandidateMain.GetAutoTradeInfo: TAutoTradeInfo;
begin
  //Result := FCandidate;
  Result := nil;
end;

procedure TfrmCandidateMain.CloseAutoTrade(const aSilenceMode: Boolean = False);
begin
  FSilenceMode := aSilenceMode;
  Self.Close;
end;

procedure TfrmCandidateMain.SetTradesState(const aValue: TTradesState);
begin
  {if (FCandidate.TradesState <> aValue) then
  begin
    FCandidate.TradesState := aValue;
    case FCandidate.TradesState of
      tsSuspended:
        FCandidate.Active := False;
      tsExecuted:
        if CheckRequiredEmbargoColumn then
        begin
          FCandidate.Active := True;
          ApplyAutoOrder;
        end;
      tsWorking:
        FCandidate.Active := True;
      tsCancelled:
        begin
          FCandidate.Active := False;
          CancelScan;
        end;
      tsFinished:
        begin
          FCandidate.Active := False;
          CancelScan;
        end;
      tsRestarted:
        begin
          CreatedOrdersCount := 0;
          FCandidate.Active := True;
          OpenSequenceRecord;
          FCandidate.TradesState := tsWorking;
        end;
    end;
    CheckTradesState;
    AutoTradeInfoToGUI;
    AutoTradesControllerPublisher.UpdateState(Self);
  end; }
end;

procedure TfrmCandidateMain.CheckTradesState;
begin
  {if (CreatedOrdersCount >= FCandidate.MaxNumberOrder) then
  begin
    pnlTop.Color := clSilver;
    FCandidate.TradesState := tsFinished;
    FCandidate.Active := False;
    CancelScan;
  end
  else if FCandidate.Active and (FCandidate.OrderGroupId > 0) then
  begin
    pnlTop.Color := clMoneyGreen;
    FCandidate.TradesState := tsWorking;
  end
  else
  begin
    pnlTop.Color := clInfoBk;
    FCandidate.TradesState := tsSuspended;
  end;

  if Assigned(Self.Parent) then
    if (Self.Parent is TTabSheet) then
    begin
      case FCandidate.TradesState of
        tsWorking:
          TTabSheet(Self.Parent).ImageIndex := C_COLOUR_INDEX_GREEN;
        tsSuspended:
          TTabSheet(Self.Parent).ImageIndex := C_COLOUR_INDEX_YELLOW;
        tsFinished:
          TTabSheet(Self.Parent).ImageIndex := C_COLOUR_INDEX_NAVY;
      end;
    end; }
end;

procedure TfrmCandidateMain.ApplyAutoOrder;
resourcestring
  rsTotalOrderAmount = 'Total order amount is 0. AutoTrade is not active.';
var
//  arr: TArray<PVirtualNode>;
//  Data: PInstrumentData;
//  i: Integer;
//  Node: PVirtualNode;
  Info: string;
  WaitForTask: ITask;
begin
  Info := 'AutoTradeInfo.Active='                    + BoolToStr(FCandidate.Active, True) +
          ', AutoTradeInfo.MaxNumberOrder='          + FCandidate.MaxNumberOrder.ToString +
          ', CreatedOrdersCount='                    + CreatedOrdersCount.ToString;

  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, 'ApplyAutoOrder', Info);
  if FCandidate.Active then
  begin
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'ApplyAutoOrder', 'Task WaitForAll');
    FIsPriceExists := False;
//    if (FCandidate.TotalOrderAmount <= 0) then
//      ShowNotification(nil, rsTotalOrderAmount);

    WaitForTask := System.Threading.TTask.Create(
      procedure()
      var
        Counter: Integer;
      begin
        TThread.NameThreadForDebugging('TfrmCandidateMain.ApplyAutoOrder');
        Counter := 0;
        while (not FIsPriceExists) and (Counter < 100) do
        begin
          Sleep(50);
          Inc(Counter, 50);
        end;
      end);
    WaitForTask.Start;
    System.Threading.TTask.WaitForAll([WaitForTask]);

    TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, 'PreExecutionEvaluationOrders');
    try
      PreExecutionEvaluationOrders;
    finally
      TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, 'PreExecutionEvaluationOrders');
    end;

//    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'ApplyAutoOrder', 'Task PreExecutionEvaluation');
//    TTask.Create(
//      procedure()
//      var
//        Node: PVirtualNode;
//      begin
//        TArray.Sort<PVirtualNode>(arr, TComparer<PVirtualNode>.Construct(
//          function(const Left, Right: PVirtualNode): Integer
//          var
//            LeftData, RightData: PInstrumentData;
//          begin
//            LeftData := Left^.GetData;
//            RightData := Right^.GetData;
//            Result := 0;
//            case FCandidate.RankingCriteria of
//              rcSum:
//                if (LeftData^.ExtraColumns.RankingSum < RightData^.ExtraColumns.RankingSum) then
//                  Result := 1
//                else if (LeftData^.ExtraColumns.RankingSum = RightData^.ExtraColumns.RankingSum) then
//                  Result := 0
//                else
//                  Result := -1;
//              rcPosition:
//                if (LeftData^.ExtraColumns.Position > RightData^.ExtraColumns.Position) then
//                  Result := 1
//                else if (LeftData^.ExtraColumns.Position = RightData^.ExtraColumns.Position) then
//                  Result := 0
//                else
//                  Result := -1;
//            end;
//          end));
//
//        for Node in arr do
//        begin
//          Data := Node^.GetData;
//          PreExecutionEvaluation(Data);
//        end;
//      end).Start;
  end;
  TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, 'ApplyAutoOrder');
end;

initialization
  ListFormFactory.RegisterList(ntCandidates, TCandidate, TfrmCandidateMain);

end.
