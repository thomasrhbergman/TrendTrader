unit Scanner.Main;
{$WARN SYMBOL_PLATFORM OFF}

interface

{$REGION 'Region uses'}
uses
  System.Actions, System.Classes, System.DateUtils, System.Generics.Collections, System.Generics.Defaults, System.ImageList,
  System.Math, System.IOUtils, System.SyncObjs, System.SysUtils, System.Threading, System.UITypes, System.Variants,
  Vcl.ActnList, Vcl.Buttons, Vcl.ComCtrls, Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, Vcl.Graphics, Vcl.ImgList,
  Vcl.Menus, Vcl.Samples.Spin, Vcl.StdCtrls, Vcl.ToolWin, Winapi.ActiveX, Winapi.Messages, Winapi.Windows, 
  IABFunctions, IABSocketAPI, IABSocketAPI_const,
  VirtualTrees, InstrumentList, DockForm.LogView, MessageDialog, Order.Utils, Scanner.TickColumn, Scanner.EmbargoColumn,
  AutoTrades, DaModule.Constants, Scanner.EditWeight, Scanner.Market, Scanner.StaticLists, Utils, DebugWriter,
  CustomForms, HtmlConsts, HtmlLib, InformationDialog, XmlFiles, Entity.Sokid, AutoTrades.Dock, BrokerHelperAbstr,
  DaImages, DaModule, Data.DB, Document, Scanner.Types, Monitor.Types, Global.Types, Monitor.Interfaces, Frame.DocumentsTree,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Publishers.Interfaces, IABFunctions.RequestsQueue, Common.Types,
  IABFunctions.MarketData, AutoTrades.Types, DaModule.Utils, VirtualTrees.Helper, Global.Resources, Vcl.Themes,
  Monitor.Info, Publishers, System.Notification, Chart.Trade, Vcl.NumberBox, IABFunctions.Helpers, System.Types,
  MonitorTree.Helper, Scanner.GradientColumn, VirtualTrees.Types, FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TfrmScannerMain = class(TCustomForm, IScannerMarket,
                                       IScanner,
                                       IAutoTrade,
                                       IInstrumentSpecDetails,
                                       IUpdateFeeds,
                                       IHistoricalData)
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
    btnExecute: TBitBtn;
    btnInformationDialog: TBitBtn;
    btnOpenAutoTradeTemplate: TBitBtn;
    btnOpenGradientColumn: TBitBtn;
    btnOpenIBScanning: TBitBtn;
    btnOpenStaticList: TBitBtn;
    btnOpenTickColumns: TBitBtn;
    btnSaveAutoTradeTemplate: TBitBtn;
    btnSaveAutoTradeTemplateAs: TBitBtn;
    btnSendDuplicateOrder: TBitBtn;
    btnShowGlobalSettings: TBitBtn;
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
    frameDocumentsTree: TframeDocumentsTree;
    gbAutoOrder: TGroupBox;
    gbHistoricalOptions: TGroupBox;
    gbIsolate: TGroupBox;
    gbSpecificationAutoTrade: TGroupBox;
    lbColumns: TListBox;
    lbIsolate: TListBox;
    lblAutoTradesName: TLabel;
    lblAutoTradeTemplateCaption: TLabel;
    lblColumns: TLabel;
    lblDuration: TLabel;
    lblInstanceNum: TLabel;
    lblMaxNumberOrder: TLabel;
    lblMaxRows: TLabel;
    lblSingleOrderAmount: TLabel;
    lblStartedTime: TLabel;
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
    pcScans: TPageControl;
    pnlAutoOrder: TPanel;
    pnlAutoOrderTop: TPanel;
    pnlButtons: TPanel;
    pnlMain: TPanel;
    pnlOptions: TPanel;
    pnlTop: TPanel;
    PopupMenu: TPopupMenu;
    rgWeightedFeed: TRadioGroup;
    sbMain: TStatusBar;
    seMaxNumberOrder: TSpinEdit;
    seMaxRows: TSpinEdit;
    seSingleOrderAmount: TSpinEdit;
    seTotalOrderAmount: TSpinEdit;
    TimerCalculateGradient: TTimer;
    TimerEmbargo: TTimer;
    tsOrderGroups: TTabSheet;
    tsScans: TTabSheet;
    vstScanner: TVirtualStringTree;
    ilBalloonHint: TImageList;
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
    procedure lbColumnsDblClick(Sender: TObject);
    procedure OnGUIToAutoTradeInfo(Sender: TObject);
    procedure rgWeightedFeedClick(Sender: TObject);
    procedure TimerCalculateGradientTimer(Sender: TObject);
    procedure TimerEmbargoTimer(Sender: TObject);
    procedure vstScannerAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure vstScannerBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstScannerCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstScannerDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstScannerDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstScannerDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstScannerFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstScannerGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure vstScannerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstScannerHeaderDblClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vstScannerHeaderDragging(Sender: TVTHeader; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstScannerHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure vstScannerHeaderMouseDown(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vstScannerInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstScannerPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
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
    C_SECTION_SCANNER_MAIN = 'ScannerMain';
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
    FAutoTradeInfo: TAutoTradeInfo;
    FCacheNodesWeight: TObjectDictionary<Integer, TExtraColumns>;
    FColumns: TAutoTradeColumns;
    FExplorationMode: Boolean;
    FInstrumentList: TFactorList;
    FIsPriceExists: Boolean;
    FIsLoaded: Boolean;
    FMonitor: IMonitor;
    FSilenceMode: Boolean;
    FSubscribedList: TList<Integer>;
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
    procedure CalculateGradient(const aNode: PVirtualNode; const aColumnsInfo: TColumnsInfo);
    procedure CalculateGradientLogTerm(const aNode: PVirtualNode; const aColumnsInfo: TColumnsInfo);
    procedure CalculateRankingSum(const aData: PInstrumentData);
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
    procedure ShowScannerMarket(const aColumnID: Integer; const aScanOptions: TScannerColumn);
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
    //implementation IScannerMarket
    function GetMainTree: TBaseVirtualTree;
    procedure AddInstrument(const aInstruments: PArrayInstrumentData; aColumnsInfo: TColumnsInfo);
    procedure DecMarketInstances;
    procedure IncMarketInstances;
    procedure SetScanOptions(const aColumnId: Integer; const aScanOptions: TScannerColumn);
    //implementation IInstrumentSpecDetails
    procedure OnInstrumentSpecDetails(Sender: TObject; Index: Integer);

    procedure DockTo;
    procedure LoadParamsFromXml;
    procedure OpenSequenceRecord;
    procedure SaveParamsToXml;
    procedure SetCreatedOrdersCount(const Value: Integer);
    procedure SetValueFromPriceCache(const aData: PInstrumentData; const aColumnsInfo: TColumnsInfo);

    property CreatedOrdersCount : Integer      read GetCreatedOrdersCount write SetCreatedOrdersCount;
    property IsLoaded           : Boolean      read FIsLoaded             write FIsLoaded;
    property TradesState        : TTradesState read GetTradesState        write SetTradesState;
  public
    class function Execute(const aAutoTradeInfo: TAutoTradeInfo): IAutoTrade;
    class function ShowDocument(aParentEdit: IAutoTrade): IAutoTrade; overload;
    class procedure ShowDocument; overload;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

{ TfrmScannerMain }

resourcestring
  rcAutotradingNotActive      = 'Autotrading is not active' + sLineBreak;
  rcCreatedOrdersCountGreater = 'The number of orders created is greater than or equal to "Max Number of Order"' + sLineBreak;
  rsEmbargoRequired           = 'At least one Embargo is required';
  rsInterruptAutotrade        = 'Closing window will interrupt Autotrade %d.' + sLineBreak + 'Continue?';
  rsNoInstruments             = 'No instrument have been added to the list';

class procedure TfrmScannerMain.ShowDocument;
var
  frmScannerMain: TfrmScannerMain;
begin
  frmScannerMain := TfrmScannerMain.Create(Application);
  frmScannerMain.IsLoaded := True;
  try
    frmScannerMain.FExplorationMode := True;
    frmScannerMain.Initialize;
    frmScannerMain.TradesState := TTradesState.tsSuspended;
    AutoTradesControllerPublisher.UpdateState(frmScannerMain);
    frmScannerMain.Show;
    frmScannerMain.UpdateCaptions;
  finally
    frmScannerMain.IsLoaded := False;
  end;
  if (frmScannerMain.WindowState = wsMinimized) then
    frmScannerMain.WindowState := wsNormal;
end;

class function TfrmScannerMain.ShowDocument(aParentEdit: IAutoTrade): IAutoTrade;
var
  frmScannerMain: TfrmScannerMain;
begin
  frmScannerMain := TfrmScannerMain.Create(Application);
  frmScannerMain.FAutoTradesEdit := aParentEdit;
  frmScannerMain.IsLoaded := True;
  try
    frmScannerMain.FAutoTradeInfo.AssignFrom(aParentEdit.GetAutoTradeInfo);
    frmScannerMain.Initialize;
    frmScannerMain.OpenSequenceRecord;
    frmScannerMain.AutoTradeInfoToGUI;
    frmScannerMain.TradesState := TTradesState.tsSuspended;
    frmScannerMain.UpdateCaptions;
  finally
    frmScannerMain.IsLoaded := False;
  end;
  frmScannerMain.Show;
  Result := frmScannerMain;
end;

class function TfrmScannerMain.Execute(const aAutoTradeInfo: TAutoTradeInfo): IAutoTrade;
var
  ScannerMain: TfrmScannerMain;
begin
  ScannerMain := TfrmScannerMain.Create(Application);
  Result := ScannerMain;
  with ScannerMain do
  begin
    FAutoTradeInfo := aAutoTradeInfo;
    Initialize;
    TradesState := TTradesState.tsWorking;
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'Execute', 'ScannerMain',
                                       'Name='                    + FAutoTradeInfo.Name                       + sLineBreak +
                                       'InstanceNum='             + FAutoTradeInfo.InstanceNum.ToString       + sLineBreak +
                                       'RecordId= '               + FAutoTradeInfo.RecordId.ToString          + sLineBreak +
                                       'QualifierId= '            + FAutoTradeInfo.Qualifier.RecordId.ToString       + sLineBreak +
                                       'QualifierInstance= '      + FAutoTradeInfo.QualifierInstance.ToString + sLineBreak +
                                       'OrderGroupId= '           + FAutoTradeInfo.OrderGroupId.ToString      + sLineBreak +
                                       'OrderAmount='             + FAutoTradeInfo.OrderAmount.ToString       + sLineBreak +
                                       'OrderCurrency='           + FAutoTradeInfo.OrderCurrency              + sLineBreak +
                                       'MaxNumberOrder='          + FAutoTradeInfo.MaxNumberOrder.ToString    + sLineBreak +
                                       'MaxRows='                 + FAutoTradeInfo.MaxRows.ToString           + sLineBreak +
                                       'Note='                    + FAutoTradeInfo.Note                       + sLineBreak +
                                       'AllowSendDuplicateOrder=' + BoolToStr(FAutoTradeInfo.AllowSendDuplicateOrder, True));
    AutoTradesControllerPublisher.UpdateState(ScannerMain);
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

function TfrmScannerMain.GetTradesState: TTradesState;
begin
  Result := FAutoTradeInfo.TradesState;
end;

procedure TfrmScannerMain.FormCreate(Sender: TObject);
begin
  inherited;
  vstScanner.NodeDataSize := SizeOf(TInstrumentData);
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

  if not Supports(Application.MainForm, IMonitor, FMonitor) then
    raise Exception.Create(rsNotSupportsIMonitor);
end;

procedure TfrmScannerMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Denitialize;
  Action := caFree;
  Self.ManualDock(nil, nil, alNone);
end;

procedure TfrmScannerMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if not FSilenceMode then
  begin
    if Showing and (CreatedOrdersCount > 0) then
      CanClose := TMessageDialog.ShowQuestion(Format(rsInterruptAutotrade, [FAutoTradeInfo.InstanceNum])) = mrYes
    else if not FExplorationMode then
      CanClose := CheckRequiredEmbargoColumn
    else if FExplorationMode then
      CanClose := CheckData;
  end;
end;

procedure TfrmScannerMain.FormDestroy(Sender: TObject);
begin
  TPublishers.FeedPublisher.Unsubscribe(Self);
  TPublishers.HistoricalDataPublisher.Unsubscribe(Self);
  TPublishers.InstrumentSpecDetailsPublisher.Unsubscribe(Self);
  TPublishers.ScannerPublisher.Unsubscribe(Self);
  FAutoTradeInfo.TradesState := TTradesState.tsNotConsidered;
  AutoTradesControllerPublisher.UpdateState(Self);
  FreeAndNil(FSubscribedList);
  FreeAndNil(FColumns);
  FreeAndNil(FInstrumentList);
  FreeAndNil(FCacheNodesWeight);
  inherited;
end;

procedure TfrmScannerMain.FormShow(Sender: TObject);
var
  MainForm: TForm;
begin
  inherited;
  MainForm  := Application.MainForm;
  Self.Left := MainForm.Left + MainForm.Width;
  Self.Top  := MainForm.Top;
end;

procedure TfrmScannerMain.Initialize;
begin
  TMonitorTree.Initialize(vstScanner);
  if (FAutoTradeInfo.InstanceNum = 0) then
    FAutoTradeInfo.InstanceNum := -General.GetNextInstanceNum;
  LoadParamsFromXml;
  TimerCalculateGradient.Enabled := True;

  frameDocumentsTree.ReadOnly    := True;
  frameDocumentsTree.DragAllowed := False;
  frameDocumentsTree.GroupView   := gvTree;
  frameDocumentsTree.DocType     := ntOrderGroupSet;
  frameDocumentsTree.Description := '';
  frameDocumentsTree.IDs         := [-1];
  frameDocumentsTree.Initialize;

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

procedure TfrmScannerMain.ShowNotification(const aData: PInstrumentData; const aInfo: string = '');
resourcestring
  rsNotification = '%s (%d), Is Criteria: %s, Info: %s';
var
  Notification: TNotification;
begin
  Notification := DMod.NotificationCenter.CreateNotification;
  try
    Notification.Name := 'ScannerMainNotification';
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
      Notification.Title := FAutoTradeInfo.Name;
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

procedure TfrmScannerMain.lbColumnsDblClick(Sender: TObject);
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

procedure TfrmScannerMain.Denitialize;
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

  if Assigned(FAutoTradesEdit) then
  begin
    FAutoTradeInfo.ColumnsInfo := ColumnsInfoToXml;
    FAutoTradesEdit.SetAutoTradeInfo(FAutoTradeInfo);
  end;
end;

procedure TfrmScannerMain.DockTo;
var
  Dock: TfrmAutoTradesDock;
begin
  Dock := TfrmAutoTradesDock.GetDock;
  Dock.Show;
  Self.Caption := FAutoTradeInfo.InstanceNum.ToString + ' (' + FAutoTradeInfo.Name + ')';
  Self.DragKind := dkDock;
  Self.DragMode := dmAutomatic;
  Self.ManualDock(Dock.pcTrades, Dock.pcTrades, alClient);
end;

function TfrmScannerMain.CheckData: Boolean;
resourcestring
  C_SQL_CHECK_TEXT = 'SELECT COUNT(*) AS CNT FROM AUTOTRADES ' + sLineBreak +
                      'WHERE (NAME = ''%s'') AND (ID <> :ID);';
var
  Msg: string;
begin
  Msg := '';
  if FAutoTradeInfo.Name.Trim.IsEmpty then
    Msg := Msg + Format(rcRequiredValue, ['AutoTrade Name']);
  if (DMod.GetIntegerValueFromSQL(Format(C_SQL_CHECK_TEXT, [FAutoTradeInfo.Name.Trim, FAutoTradeInfo.RecordId]), 'CNT') > 0) then
    Msg := Msg + rcNameNotUnique;
  if (FAutoTradeInfo.OrderAmount <= 0) then
    Msg := Msg + Format(rcRequiredValue, ['Single order amount']);
  if (FAutoTradeInfo.TotalOrderAmount <= 0) then
    Msg := Msg + Format(rcRequiredValue, ['Total order amount']);
  if (FAutoTradeInfo.MaxNumberOrder <= 0) then
    Msg := Msg + Format(rcRequiredValue, ['Max number of order']);
  if (FAutoTradeInfo.OrderCurrency.IsEmpty) then
    Msg := Msg + Format(rcRequiredValue, ['Order Currency']);
  if (FAutoTradeInfo.MaxRows <= 0) then
    Msg := Msg + Format(rcRequiredValue, ['Max rows']);
  Result := Msg.IsEmpty;
  if not Result then
    Result := TMessageDialog.ShowQuestion(Msg + sLineBreak + rcQuestionContinueEditing) = mrNo;
end;

procedure TfrmScannerMain.LoadParamsFromXml;
begin
  if FExplorationMode and (FAutoTradeInfo.InstanceNum <= 0) then
  begin
    cbOrderCurrency.Items.Text             := General.XMLFile.ReadString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY_LIST, C_DEFAULT_CURRENCY);
    cbOrderCurrency.Text                   := General.XMLFile.ReadString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY, C_DEFAULT_CURRENCY);
    FAutoTradeInfo.AllowSendDuplicateOrder := General.XMLFile.ReadBool(C_SECTION_SCANNER_MAIN, C_KEY_ALLOW_SEND_DUPLICATE, False);
    FAutoTradeInfo.AutoRefresh             := General.XMLFile.ReadBool(C_SECTION_SCANNER_MAIN, C_KEY_AUTO_REFRESH, True);
    FAutoTradeInfo.MaxNumberOrder          := General.XMLFile.ReadInteger(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_MAX_NUMBER, 0);
    FAutoTradeInfo.MaxRows                 := General.XMLFile.ReadInteger(C_SECTION_SCANNER_MAIN, C_KEY_MAX_ROWS, 50);
    FAutoTradeInfo.TotalOrderAmount        := General.XMLFile.ReadInteger(C_SECTION_SCANNER_MAIN, C_KEY_TOTAL_ORDER_AMOUNT, 0);
    FAutoTradeInfo.OrderAmount             := General.XMLFile.ReadInteger(C_SECTION_SCANNER_MAIN, C_KEY_SINGLE_ORDER_AMOUNT, 0);
    FAutoTradeInfo.OrderCurrency           := General.XMLFile.ReadString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY, '');
  end;
  AutoTradeInfoToGUI;
end;

procedure TfrmScannerMain.SaveParamsToXml;
begin
  if (FAutoTradeInfo.InstanceNum <= 0) then
  try
    General.XMLFile.WriteBool(C_SECTION_SCANNER_MAIN, C_KEY_ALLOW_SEND_DUPLICATE, cbAllowSendDuplicateOrder.Checked);
    General.XMLFile.WriteBool(C_SECTION_SCANNER_MAIN, C_KEY_AUTO_REFRESH, cbAutoRefresh.Checked);
    General.XMLFile.WriteInteger(C_SECTION_SCANNER_MAIN, C_KEY_MAX_ROWS, seMaxRows.Value);
    General.XMLFile.WriteInteger(C_SECTION_SCANNER_MAIN, C_KEY_TOTAL_ORDER_AMOUNT, seTotalOrderAmount.Value);
    General.XMLFile.WriteInteger(C_SECTION_SCANNER_MAIN, C_KEY_SINGLE_ORDER_AMOUNT, seSingleOrderAmount.Value);
    General.XMLFile.WriteInteger(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_MAX_NUMBER, seMaxNumberOrder.Value);
    General.XMLFile.WriteString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY, cbOrderCurrency.Text);
    General.XMLFile.WriteString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY_LIST, cbOrderCurrency.Items.Text);
  finally
    General.XMLFile.Save;
  end;
end;

procedure TfrmScannerMain.CheckRankingCriteria(const aData: PInstrumentData);
var
  ColumnsInfo: TColumnsInfo;
  IsExists: Boolean;
begin
  IsExists := False;
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

procedure TfrmScannerMain.CheckRankingCriteries;
var
  Data: PInstrumentData;
  Node: PVirtualNode;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      vstScanner.SortTree(vstScanner.Header.Columns[COL_RANKING_SUM].Index, sdDescending);
    end);
  Node := vstScanner.GetFirstChild(vstScanner.RootNode);
  while Assigned(Node) do
  begin
    Data := Node^.GetData;
    Data^.ExtraColumns.Position := Node.Index + 1;
    CheckRankingCriteria(Data);
    Node := Node.NextSibling;
  end;
end;

procedure TfrmScannerMain.TimerEmbargoTimer(Sender: TObject);
begin
  if not Application.Terminated then
    System.Threading.TTask.Create(
      procedure()
      begin
        TThread.NameThreadForDebugging('TfrmScannerMain.TimerEmbargoTimer');
        CheckRankingCriteries;
        TThread.Queue(nil,
          procedure
          begin
            vstScanner.Invalidate;
          end);
      end).Start;
end;

procedure TfrmScannerMain.PreExecutionEvaluationOrders;
var
  Data: PInstrumentData;
  Node: PVirtualNode;
begin
  vstScanner.BeginUpdate;
  try
    vstScanner.SortTree(vstScanner.Header.Columns[COL_RANKING_SUM].Index, sdDescending);
    Node := vstScanner.GetFirstChild(vstScanner.RootNode);
    while Assigned(Node) do
    begin
      FAutoTradeInfo.TradesState := tsWorking;
      Data := Node^.GetData;
      Data^.ExtraColumns.Position := Node.Index + 1;
      CheckRankingCriteria(Data);
      PreExecutionEvaluation(Data);
      Node := Node.NextSibling;
    end;
  finally
    vstScanner.EndUpdate;
  end;
end;

procedure TfrmScannerMain.PreExecutionEvaluation(const aData: PInstrumentData);
var
  LastPrice: Double;
  LastExch: Double;
  Quantity: Integer;
  Currency: string;
  Info: string;
  OrderAmount: Double;
  PrecSettings: TPrecautionarySettingTypes;
begin
  if aData^.IsCriteria and
    (FAutoTradeInfo.Active and (not aData^.IsLocked or FAutoTradeInfo.AllowSendDuplicateOrder)) and
    (FAutoTradeInfo.OrderAmount > 0) and
    (FAutoTradeInfo.TotalOrderAmount > 0) and
    (FAutoTradeInfo.OrderGroupId > 0) and
    (CreatedOrdersCount < FAutoTradeInfo.MaxNumberOrder) then
  begin
    Info := 'Id=' + aData^.Id.ToString +
            ', Name=' + aData^.Name +
            ', Symbol=' + aData^.Symbol +
            ', LocalSymbol=' + aData^.LocalSymbol +
            ', Currency=' + aData^.Currency +
            ', Multiplier=' + aData^.Multiplier.ToString;
//    TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'PreExecutionEvaluation', Info);
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
      if (Currency <> FAutoTradeInfo.OrderCurrency) then
        LastExch := TMonitorLists.CurrencyCache.GetLastExchange(FAutoTradeInfo.OrderCurrency, Currency);
      if (LastExch <= 0) then
      begin
        aData^.Description := 'Not passed - No Exchange Rate ' + Currency;
        aData^.IsLocked := True;
        ShowNotification(aData);
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView, ltActivityLog], ddError, Self, 'PreExecutionEvaluation', aData^.Description + ', ' + Info);
        Exit;
      end;

      Quantity := 0;
      if (LastPrice > 0) and (LastExch > 0) then
      begin
        if (FAutoTradeInfo.TotalOrderAmount < LastPrice) then
        begin
          aData^.Description := 'Not passed - No Total Money';
          aData^.IsLocked := True;
          ShowNotification(aData, 'No Total Money');
          Exit;
        end;

        if (FAutoTradeInfo.TotalOrderAmount >= FAutoTradeInfo.OrderAmount) then
          OrderAmount := FAutoTradeInfo.OrderAmount
        else
          OrderAmount := FAutoTradeInfo.TotalOrderAmount;

        {if (OrderAmount <= 0) then
        begin
          aData^.Description := 'Not passed - OrderAmount Is 0';
          aData^.IsLocked := True;
          ShowNotification(aData, 'OrderAmount is 0');
          TPublishers.LogPublisher.Write([ltLogWriter], ddWarning, 'PreExecutionEvaluation', 'OrderAmount=0');
        end;}

        if (aData^.Multiplier > 0) then
          Quantity := Trunc((OrderAmount * LastExch / (LastPrice * aData^.Multiplier)))
        else
          Quantity := Trunc((OrderAmount * LastExch) / LastPrice);

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
                Quantity := Trunc(General.PrecautionarySettings[aData^.SecurityType][psOrderQuantityMax]);
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
        Dec(FAutoTradeInfo.TotalOrderAmount, Trunc(Quantity * LastPrice));
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
          begin
            if FMonitor.CreateTemplateStructure(FAutoTradeInfo.OrderGroupId,
                                                aData,
                                                TAutoTradesCommon.Create(Quantity,
                                                                         FAutoTradeInfo.QualifierInstance,
                                                                         FAutoTradeInfo.Qualifier.RecordId,
                                                                         FAutoTradeInfo.InstanceNum,
                                                                         FAutoTradeInfo.RecordId,
                                                                         FAutoTradeInfo.AllowSendDuplicateOrder)) <> nil then
            begin
              if aData^.IsLocked then
                CreatedOrdersCount := CreatedOrdersCount + 1;
              aData^.Name := aData^.Name + ': ' + SimpleRoundTo(aData^.ExtraColumns.RankingSum, -2).ToString;
            end;
          end);
      end;
    end;
    CheckTradesState;
    AutoTradeInfoToGUI;
  end;
end;

procedure TfrmScannerMain.aChangeWeigthValueColumnUpdate(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
begin
  TAction(Sender).Enabled := (vstScanner.FocusedColumn >= C_FIXED_COLUMN_INDEX);
  if TAction(Sender).Enabled then
  begin
    if FColumns.ContainsKey(vstScanner.Header.Columns[vstScanner.FocusedColumn].Id) then
      ColumnsInfo := FColumns[vstScanner.Header.Columns[vstScanner.FocusedColumn].Id];
    TAction(Sender).Enabled := ColumnsInfo.SourceType in [stStaticList, stScannerMarket, stTickColumn];
  end;
end;

procedure TfrmScannerMain.aChangeWeigthValueUpdate(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
begin
  TAction(Sender).Enabled := not vstScanner.IsEmpty and (vstScanner.FocusedColumn >= C_FIXED_COLUMN_INDEX);
  if TAction(Sender).Enabled then
  begin
    if FColumns.ContainsKey(vstScanner.Header.Columns[vstScanner.FocusedColumn].Id) then
      ColumnsInfo := FColumns[vstScanner.Header.Columns[vstScanner.FocusedColumn].Id];
    TAction(Sender).Enabled := ColumnsInfo.SourceType in [stStaticList, stScannerMarket, stTickColumn];
  end;
end;

procedure TfrmScannerMain.aCopySelectedNodeToMonitorExecute(Sender: TObject);
var
  Target: PVirtualNode;
begin
  if IABClient.Connected then
  begin
    Target := FMonitor.GetFocusedNode;
    FMonitor.AddInstrumentFromSearch(Target, vstScanner);
  end;
end;

procedure TfrmScannerMain.aCopySelectedNodeToMonitorUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstScanner.IsEmpty and IABClient.Connected;
end;

function TfrmScannerMain.GetMainTree: TBaseVirtualTree;
begin
  Result := vstScanner;
end;

procedure TfrmScannerMain.AddInstrument(const aInstruments: PArrayInstrumentData; aColumnsInfo: TColumnsInfo);
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
    Node := vstScanner.GetFirstChild(vstScanner.RootNode);
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
    FAutoTradeInfo.ScanCount := Length(aInstruments^);
    ColumnId := AddOrGetColumn(aColumnsInfo);
    if (ColumnId = 0) then
      Exit;

    if (aColumnsInfo.SourceType = stScannerMarket) and (aColumnsInfo.ScannerColumn.ScanId = 0) then
      ExecuteActions(aColumnsInfo);

    vstScanner.BeginUpdate;
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
              Node := vstScanner.AddChild(vstScanner.RootNode);
              Data := Node^.GetData;
              Data^.ExtraColumns := TExtraColumns.Create(0);
            end;
          kaReduce:
            if Assigned(Node) then
            begin
              Data := Node^.GetData;
              if not Data^.IsLocked then
                vstScanner.DeleteNode(Node);
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

      for var i := Low(NodeArray) to High(NodeArray) do
      begin
        if (i >= FAutoTradeInfo.MaxRows) then
        begin
          if Assigned(NodeArray[i]) then
            vstScanner.DeleteNode(NodeArray[i], False);
          FInstrumentList.DeleteNode(NodeArray[i]);
        end;
      end;

      if (aColumnsInfo.KindAppend = kaIntersection) and (ExistsItemsList.Count > 0) then
      begin
        Node := vstScanner.GetFirstChild(vstScanner.RootNode);
        while Assigned(Node) do
        begin
          DelNode := Node;
          Node := Node.NextSibling;
          Data := DelNode^.GetData;
          if not Data^.IsLocked and (ExistsItemsList.IndexOf(DelNode) < 0) then
            vstScanner.DeleteNode(DelNode);
        end;
      end;

      if FAutoTradeInfo.HistoricalDataParams.SubscribeHistData then
      begin
        Node := vstScanner.GetFirstChild(vstScanner.RootNode);
        while Assigned(Node) do
        begin
          Data := Node^.GetData;
          if not FSubscribedList.Contains(Data^.Id) then
          begin
            RequestMarketData(Data^.Id);
            FSubscribedList.Add(Data^.Id);
          end;
          Node := Node.NextSibling;
        end;
      end;

      FAutoTradeInfo.ScanCount := vstScanner.RootNode.ChildCount;
      PreExecutionEvaluationOrders;
    finally
      FreeAndNil(ExistsItemsList);
      vstScanner.EndUpdate;
    end;

    for InstrumentData in aInstruments^ do
      InstrumentData.Clear;
  end;
end;

procedure TfrmScannerMain.DeleteAllNodes;
begin
  vstScanner.BeginUpdate;
  try
    CancelScan;
    vstScanner.Clear;
    vstScanner.Header.Columns.Clear;
    FInstrumentList.Clear;
    FCacheNodesWeight.Clear;
    FColumns.Clear;
    lbColumns.Items.Clear;
    AddFixedColumns;
    TimerEmbargo.Enabled        := False;
    edAutoTradeTemplate.Text    := '';
    CreatedOrdersCount          := 0;
    FAutoTradeInfo.OrderGroupId := -1;
    FAutoTradeInfo.Name         := '';
    FAutoTradeInfo.ColumnsInfo  := ColumnsInfoToXml;
    AutoTradeInfoToGUI;
  finally
    vstScanner.EndUpdate;
  end;
end;

procedure TfrmScannerMain.aDeleteAllNodesExecute(Sender: TObject);
begin
  if (TMessageDialog.ShowQuestion(rsAllNodesDeletedConfirmation) = mrYes) then
    DeleteAllNodes;
end;

procedure TfrmScannerMain.aDeleteColumnExecute(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
  ColumnId: Integer;
begin
  if (vstScanner.FocusedColumn + 1 > C_FIXED_COLUMN_INDEX) and
    (TMessageDialog.ShowQuestion(Format(rsColumnDeletedConfirmation, [vstScanner.Header.Columns[vstScanner.FocusedColumn].CaptionText])) = mrYes) then
  begin
    vstScanner.BeginUpdate;
    try
      ColumnId := vstScanner.Header.Columns[vstScanner.FocusedColumn].Id;
      if FColumns.ContainsKey(ColumnId) then
      begin
        ColumnsInfo := FColumns[ColumnId];
        if (ColumnsInfo.SourceType = stScannerMarket) and IABClient.Connected then
          IABClient.CancelScan(ColumnsInfo.ScannerColumn.ScanId);
        FColumns.Remove(ColumnId);
      end;
      vstScanner.Header.Columns.Delete(vstScanner.FocusedColumn);
      TimerEmbargo.Enabled := False;
      for ColumnsInfo in FColumns.Values do
        if (ColumnsInfo.SourceType = stEmbargoColumn) and (ColumnsInfo.EmbargoColumn.EmbargoType in [etReleaseTime, etHoldTime]) then
        begin
          TimerEmbargo.Enabled := True;
          Break;
        end;
      CalculateAllRankingSum;
      PreExecutionEvaluationOrders;
    finally
      vstScanner.EndUpdate;
    end;
  end;
  FillColumnList;
end;

procedure TfrmScannerMain.aExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmScannerMain.aInformationDialogExecute(Sender: TObject);
begin
  TInformationDialog.ShowMessage(TDocumentInfo.GetScanColumnsInfo(FColumns), 'ScannerMain');
end;

procedure TfrmScannerMain.aInstrumentInfoExecute(Sender: TObject);
var
  Data: PInstrumentData;
  Node: PVirtualNode;
  sb: TStringBuilder;
  TickType: TIABTickType;
begin
  if Assigned(vstScanner.FocusedNode) then
  begin
    Node := vstScanner.FocusedNode;
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
      TInformationDialog.ShowMessage(sb.ToString, 'ScannerMainInstrumentInfo');
    finally
      FreeAndNil(sb);
    end;
  end;
end;

procedure TfrmScannerMain.aInstrumentInfoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstScanner.IsEmpty;
end;

procedure TfrmScannerMain.aOpenTickColumnsExecute(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
  RunColumnsInfo: TColumnsInfo;
begin
  if (TfrmScannerTickColumn.ShowDocument(dmInsert, ColumnsInfo) = mrOk) then
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

procedure TfrmScannerMain.aAddEmbargoColumnExecute(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
  RunColumnsInfo: TColumnsInfo;
begin
  if (TfrmScannerEmbargoColumn.ShowDocument(dmInsert, FColumns, ColumnsInfo) = mrOk) then
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

procedure TfrmScannerMain.AddFixedColumns;
var
  Column: TVirtualTreeColumn;
begin
  Column := vstScanner.Header.Columns.Add;
  Column.Width            := 140;
  Column.Text             := 'Symbol';
  Column.CaptionAlignment := taCenter;
  Column.Options          := Column.Options - [coDraggable, coEditable] + [coFixed];

  Column := vstScanner.Header.Columns.Add;
  Column.Width            := 80;
  Column.Text             := 'Ranking sum';
  Column.CaptionAlignment := taCenter;
  Column.Alignment        := taRightJustify;
  Column.Options          := Column.Options - [coDraggable, coEditable] + [coFixed];
end;

function TfrmScannerMain.AddOrGetColumn(var aColumnsInfo: TColumnsInfo): Integer;
var
  Column: TVirtualTreeColumn;
  ColumnsItem: TExtraColumns.TColumnsItem;
  Data: PInstrumentData;
  IsExists: Boolean;
  Node: PVirtualNode;
  RunColumnsInfo: TColumnsInfo;
begin
  vstScanner.BeginUpdate;
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
            if aColumnsInfo.EmbargoColumn.EmbargoType in [etReleaseTime, etHoldTime] then
              TimerEmbargo.Enabled := True;
          end;
        stTickColumn:
          begin

          end;
      end;

      Column := vstScanner.Header.Columns.Add;
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
      Node := vstScanner.GetFirst;
      while Assigned(Node) do
      begin
        Data := Node^.GetData;
        Data^.ExtraColumns.Items.AddOrSetValue(Column.Id, ColumnsItem);
        CalculateGradientLogTerm(Node, aColumnsInfo);
        SetValueFromPriceCache(Data, aColumnsInfo);
        CalculateRankingSum(Data);
        vstScanner.InvalidateNode(Node);
        Node := vstScanner.GetNextSibling(Node);
      end;
      FillColumnList;
    end;
  finally
    vstScanner.EndUpdate;
  end;
end;

procedure TfrmScannerMain.FillColumnList;
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
      stScannerMarket:
        ColumnName := 'Scanner Column: ' +
                      ' (Step:' + RunColumnsInfo.ScannerColumn.Step.ToString +
                      ', Weight:' + RunColumnsInfo.ScannerColumn.Weight.ToString + ') ' +
                      RunColumnsInfo.ScannerColumn.Name +
                      ', Locat.:' + RunColumnsInfo.ScannerColumn.ScanCriteria.LocationCode;
      stCalcColumn:
        ColumnName := 'Calc Column: ' + RunColumnsInfo.Caption;
      stTickColumn:
        ColumnName := 'Tick Column: ' + RunColumnsInfo.Caption;
      stEmbargoColumn:
        ColumnName := 'Embargo Column: ' + RunColumnsInfo.EmbargoColumn.Caption;
    end;
    if (lbColumns.Items.IndexOf(ColumnName) < 0) and not ColumnName.IsEmpty then
      lbColumns.Items.AddObject(ColumnName, TObject(RunColumnsInfo.ColumnId));
  end;
  FAutoTradeInfo.Columns := lbColumns.Items.Text;
  FAutoTradeInfo.Note    := lbColumns.Items.Text;
  if Assigned(FAutoTradesEdit) then
  begin
    FAutoTradeInfo.ColumnsInfo := ColumnsInfoToXml;
    FAutoTradesEdit.SetAutoTradeInfo(FAutoTradeInfo);
  end;
end;

procedure TfrmScannerMain.OpenSequenceRecord;

  procedure LoadColumns;
  var
    Column      : TVirtualTreeColumn;
    ColumnsInfo : TColumnsInfo;
    XMLFile     : TXMLFile;
  begin
    XMLFile := TXMLFile.Create;
    try
      XMLFile.XMLText        := FAutoTradeInfo.ColumnsInfo;
      XMLFile.CurrentSection := C_SECTION_COLUMNS;
      while not XMLFile.IsLastKey do
      begin
        if XMLFile.ReadAttributes then
        begin
          ColumnsInfo := TColumnsInfo.Create(TSourceType(XMLFile.Attributes.GetAttributeValue('SourceType', stScannerMarket)));
          ColumnsInfo.KindAppend     := TKindAppend(XMLFile.Attributes.GetAttributeValue('KindAppend', kaAddAll));
          ColumnsInfo.ColumnPosition := XMLFile.Attributes.GetAttributeValue('ColumnPosition', 0);
          ColumnsInfo.ColumnWidth    := XMLFile.Attributes.GetAttributeValue('ColumnWidth', 80);
          ColumnsInfo.Weight         := XMLFile.Attributes.GetAttributeValue('Weight', 1);
          ColumnsInfo.FromList(XMLFile.Attributes.GetAttributeValue('Info', ''));
          ColumnsInfo.ScannerColumn.ScanId := 0;
          if (ColumnsInfo.SourceType <> stFixed) then
            ColumnsInfo.RecordId := XMLFile.Attributes.GetAttributeValue('RecordId', 0);
          if (ColumnsInfo.SourceType = stFixed) and
             (vstScanner.Header.Columns.Count - 1 >= ColumnsInfo.ColumnPosition) and
             (ColumnsInfo.ColumnWidth > 10) then
          begin
            Column := vstScanner.Header.Columns[ColumnsInfo.ColumnPosition];
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
              for Index := 0 to vstScanner.Header.Columns.Count - 1 do
                if (vstScanner.Header.Columns[Index].ID = ColumnsInfo.ColumnId) then
                  vstScanner.Header.Columns[Index].Text := ColumnsInfo.Caption;
              Break;
            end;
        end;
    end;
  end;

  procedure LoadOrderGroupSet;
  begin
    frameDocumentsTree.IDs := [FAutoTradeInfo.OrderGroupId];
    frameDocumentsTree.LoadTree;
  end;

var
  ColumnsInfo: TColumnsInfo;
begin
  if not FAutoTradeInfo.ColumnsInfo.IsEmpty then
  begin
    vstScanner.BeginUpdate;
    TimerCalculateGradient.Enabled := False;
    try
      CancelScan;
      lbColumns.Items.Clear;
      FInstrumentList.Clear;
      FColumns.Clear;
      FCacheNodesWeight.Clear;
      vstScanner.Clear;
      vstScanner.Header.Columns.Clear;
      AddFixedColumns;
      LoadColumns;
      LoadOrderGroupSet;
      CheckReferenceColumns;
      UpdateCaptions;
      for ColumnsInfo in FColumns.Values do
        ExecuteActions(ColumnsInfo);
      CalculateValuesForAllNodes;
      CalculateAllRankingSum;
      PreExecutionEvaluationOrders;
    finally
      vstScanner.EndUpdate;
      TimerCalculateGradient.Enabled := True;
    end;
  end;
end;

procedure TfrmScannerMain.ExecuteActions(aColumnsInfo: TColumnsInfo);
var
  ScannerColumn: TScannerColumn;
begin
  case aColumnsInfo.SourceType of
    stStaticList:
      begin
        if (aColumnsInfo.StaticColumn.RecordId > 0) then
          TfrmScannerStaticLists.AddInstrumentToScannerMain(Self, aColumnsInfo.KindAppend, aColumnsInfo.StaticColumn.RecordId, dmUpdate);
        if vstScanner.IsEmpty then
          ShowBaloonHint(rsNoInstruments, pnlOptions);
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self,
                                      'ExecuteActions',
                                      'SourceType=stStaticList, StaticListsId= ' + aColumnsInfo.StaticColumn.RecordId.ToString);
      end;
    stScannerMarket:
      if (aColumnsInfo.ScannerColumn.ScanId <= 0) then
      begin
        ScannerColumn.FromList(aColumnsInfo.ScannerColumn.ToList);
        if (General.PrecautionarySettings[stAll][psMaxAllowedPrice] > 0) then
        begin
          if (ScannerColumn.ScanCriteria.BelowPrice > General.PrecautionarySettings[stAll][psMaxAllowedPrice]) then
            ScannerColumn.ScanCriteria.BelowPrice := General.PrecautionarySettings[stAll][psMaxAllowedPrice];
        end;

        if (General.PrecautionarySettings[stAll][psMinAllowedPrice] > 0) then
        begin
          if (ScannerColumn.ScanCriteria.AbovePrice = UNSET_DOUBLE) or
             (ScannerColumn.ScanCriteria.AbovePrice < General.PrecautionarySettings[stAll][psMinAllowedPrice]) then
            ScannerColumn.ScanCriteria.AbovePrice := General.PrecautionarySettings[stAll][psMinAllowedPrice];
        end;

        aColumnsInfo.ScannerColumn.ScanId := IABClient.ScanExecute(ScannerColumn.ScanCriteria);
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self,
                                      'ExecuteActions',
                                      'MaxRows= ' + aColumnsInfo.ScannerColumn.MaxRows.ToString + sLineBreak +
                                      'ScanId= ' + aColumnsInfo.ScannerColumn.ScanId.ToString + sLineBreak +
                                      'Step= ' + aColumnsInfo.ScannerColumn.Step.ToString + sLineBreak +
                                      'Weight= ' + aColumnsInfo.ScannerColumn.Weight.ToString + sLineBreak +
                                      'SourceType=stScannerMarket' + sLineBreak);
        FColumns.AddOrSetValue(aColumnsInfo.ColumnId, aColumnsInfo);
      end;
  end;
end;

function TfrmScannerMain.CheckColumn(const aColumnsInfo: TColumnsInfo): Boolean;
var
  Column: TVirtualTreeColumn;
  ColumnIndex: Integer;
begin
  Result := False;
  for ColumnIndex := vstScanner.Header.Columns.GetFirstColumn to vstScanner.
    Header.Columns.Count - 1 do
  begin
    Column := vstScanner.Header.Columns.Items[ColumnIndex];
    if (Column.Id = aColumnsInfo.ColumnId) then
    begin
      Column.Options := Column.Options + [coVisible];
      Result := True;
      Break;
    end;
  end;
end;

function TfrmScannerMain.CheckRequiredEmbargoColumn: Boolean;
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
    if (TfrmScannerEmbargoColumn.ShowDocument(dmInsert, FColumns, ColumnsInfo) = mrOk) then
      AddOrGetColumn(ColumnsInfo);
  end
  else if not CheckColumn(ColumnsInfo) then
    AddOrGetColumn(ColumnsInfo);
end;

function TfrmScannerMain.ColumnsInfoToXml: string;
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
    ColumnIndex := vstScanner.Header.Columns.GetFirstColumn;
    if (ColumnIndex > -1) then
      Column := vstScanner.Header.Columns[ColumnIndex];
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
      ColumnIndex := vstScanner.Header.Columns.GetNextColumn(ColumnIndex);
      if (ColumnIndex > -1)then
        Column := vstScanner.Header.Columns[ColumnIndex]
      else
        Column := nil;
    end;
    Result := XmlFile.XMLText;
  finally
    FreeAndNil(XmlFile);
  end;
end;

procedure TfrmScannerMain.aSaveAutoTradeTemplateAsExecute(Sender: TObject);
begin
  if CheckRequiredEmbargoColumn then
  begin
    FAutoTradeInfo.RecordId := -1;
    FAutoTradeInfo.ColumnsInfo := ColumnsInfoToXml;
    FAutoTradeInfo.SaveToDB;
    AutoTradeInfoToGUI;
  end;
end;

procedure TfrmScannerMain.aSaveAutoTradeTemplateExecute(Sender: TObject);
begin
  if CheckRequiredEmbargoColumn then
  begin
    FAutoTradeInfo.ColumnsInfo := ColumnsInfoToXml;
    if (FAutoTradeInfo.Name <> edAutoTradeTemplate.Text) then
      FAutoTradeInfo.Name := edAutoTradeTemplate.Text;
    FAutoTradeInfo.SaveToDB;
    AutoTradeInfoToGUI;
  end;
end;

procedure TfrmScannerMain.aOpenAutoTradeTemplateExecute(Sender: TObject);
var
  Id: Integer;
  MarkedNode: TMarkedNode;
begin
  MarkedNode.DocType := ntAutoTrade;
  MarkedNode.RecordId := FAutoTradeInfo.RecordId;
  Id := TfrmAutoTrades.ShowDocument(MarkedNode);
  if (Id > 0) then
    try
      IsLoaded := True;
      FAutoTradeInfo.FromDB(Id);
      edAutoTradeTemplate.Text := FAutoTradeInfo.Name;
      OpenSequenceRecord;
      AutoTradeInfoToGUI;
    finally
      IsLoaded := False;
    end;
end;

procedure TfrmScannerMain.aOpenGradientColumnExecute(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
  RunColumnsInfo: TColumnsInfo;
begin
  if (TfrmScannerGradientColumn.ShowDocument(dmInsert, ColumnsInfo) = mrOk) then
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

procedure TfrmScannerMain.EditWeigthColumn(const aColumnIndex: TColumnIndex);
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
    Node := vstScanner.GetFirstChild(vstScanner.RootNode);
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      if Data^.ExtraColumns.Items.ContainsKey(aColumnID) then
      begin
        ColumnsItem := Data^.ExtraColumns.Items.Items[aColumnID];
        ColumnsItem.Rank := ColumnsItem.Rank * aCoef;
        Data^.ExtraColumns.Items.AddOrSetValue(aColumnID, ColumnsItem);
        vstScanner.InvalidateNode(Node);
      end;
      Node := Node.NextSibling;
    end;
  end;

begin
  if (aColumnIndex >= C_FIXED_COLUMN_INDEX) then
  begin
    if FColumns.ContainsKey(vstScanner.Header.Columns[aColumnIndex].Id) then
      ColumnsInfo := FColumns[vstScanner.Header.Columns[aColumnIndex].Id];

    if ColumnsInfo.SourceType in [stStaticList, stScannerMarket, stTickColumn] then
    begin
      Weight := ColumnsInfo.Weight;
      if (TfrmScannerEditWeight.ShowDocument(Weight) = mrOk) then
      begin
        Coef := Weight / ColumnsInfo.Weight;
        ColumnsInfo.Weight := Weight;
        vstScanner.Header.Columns[aColumnIndex].Hint := 'Weight: ' + Weight.ToString;
        vstScanner.Header.Columns[aColumnIndex].Text := ColumnsInfo.Caption;

        if (ColumnsInfo.SourceType = stScannerMarket) then
          CalculateColumn(Coef, ColumnsInfo.ColumnId)
        else
          FColumns.AddOrSetValue(vstScanner.Header.Columns[aColumnIndex].Id, ColumnsInfo);
        FillColumnList;
      end;
      CalculateAllRankingSum;
      PreExecutionEvaluationOrders;
    end;
  end;
end;

procedure TfrmScannerMain.aChangeWeigthValueColumnExecute(Sender: TObject);
begin
  EditWeigthColumn(vstScanner.FocusedColumn);
end;

procedure TfrmScannerMain.aChangeWeigthValueExecute(Sender: TObject);
var
  Weight: Double;
  Coef: Double;
  ColumnsInfo: TColumnsInfo;
  Node: PVirtualNode;
  Data: PInstrumentData;
  ColumnsItem: TExtraColumns.TColumnsItem;
  ExtraColumns: TExtraColumns;
begin
  if (vstScanner.FocusedColumn >= C_FIXED_COLUMN_INDEX) and Assigned(vstScanner.FocusedNode) then
  begin
    if FColumns.ContainsKey(vstScanner.Header.Columns[vstScanner.FocusedColumn].Id) then
      ColumnsInfo := FColumns[vstScanner.Header.Columns[vstScanner.FocusedColumn].Id];

    if ColumnsInfo.SourceType in [stStaticList, stScannerMarket, stTickColumn] then
    begin
      Node := vstScanner.FocusedNode;
      Data := Node^.GetData;
      if Data^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId) then
      begin
        ColumnsItem := Data^.ExtraColumns.Items[ColumnsInfo.ColumnId];
        Weight := ColumnsItem.Weight;
        if (TfrmScannerEditWeight.ShowDocument(Weight) = mrOk) then
        begin
          if (ColumnsItem.Weight = 0) then
            Coef := 0
          else
            Coef := Weight / ColumnsItem.Weight;

          ColumnsItem.Weight := Weight;
          if (ColumnsInfo.SourceType = stScannerMarket) then
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

procedure TfrmScannerMain.ShowBaloonHint(const aText: string; const aControl: TWinControl);
begin
  BalloonHint.Title := 'Warning';
  BalloonHint.Description := aText;
  BalloonHint.Style := bhsStandard;
  BalloonHint.ImageIndex := 0;
  BalloonHint.HideAfter := 10000;
  BalloonHint.ShowHint(aControl);
end;

procedure TfrmScannerMain.ShowColumnDetails(const aColumnID: Integer);
var
  ColumnsInfo: TColumnsInfo;
  Column: TVirtualTreeColumn;
begin
  if FColumns.TryGetValue(aColumnID, ColumnsInfo) then
    case ColumnsInfo.SourceType of
      stStaticList:
        TfrmScannerStaticLists.ShowDocument(dmView, ColumnsInfo.KindAppend, ColumnsInfo.StaticColumn.RecordId, Self);
      stScannerMarket:
        ShowScannerMarket(aColumnID, ColumnsInfo.ScannerColumn);
      stTickColumn:
        begin
          TfrmScannerTickColumn.ShowDocument(dmUpdate, ColumnsInfo);
          Column := TVirtualTreeColumn(vstScanner.Header.Columns.FindItemID(ColumnsInfo.ColumnId));
          if Assigned(Column) then
            Column.Text := ColumnsInfo.Caption;
          FColumns.AddOrSetValue(aColumnID, ColumnsInfo);
        end;
      stCalcColumn:
        begin
          TfrmScannerGradientColumn.ShowDocument(dmUpdate, ColumnsInfo);
          Column := TVirtualTreeColumn(vstScanner.Header.Columns.FindItemID(ColumnsInfo.ColumnId));
          if Assigned(Column) then
            Column.Text := ColumnsInfo.Caption;
          FColumns.AddOrSetValue(aColumnID, ColumnsInfo);
        end;
      stEmbargoColumn:
        begin
          TfrmScannerEmbargoColumn.ShowDocument(dmUpdate, FColumns, ColumnsInfo);
          Column := TVirtualTreeColumn(vstScanner.Header.Columns.FindItemID(ColumnsInfo.ColumnId));
          if Assigned(Column) then
            Column.Text := ColumnsInfo.Caption;
          FColumns.AddOrSetValue(aColumnID, ColumnsInfo);
        end;
    end;
  FillColumnList;
end;

procedure TfrmScannerMain.aShowColumnDetailsExecute(Sender: TObject);
var
  ColumnId: Integer;
begin
  if (vstScanner.FocusedColumn > C_FIXED_COLUMN_INDEX - 1) then
  begin
    ColumnId := vstScanner.Header.Columns[vstScanner.FocusedColumn].Id;
    ShowColumnDetails(ColumnId);
  end;
end;

procedure TfrmScannerMain.aShowColumnDetailsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstScanner.IsEmpty;
end;

procedure TfrmScannerMain.aShowGlobalSettingsExecute(Sender: TObject);
begin
  General.ShowParametersTab(TfrmParameters.C_TAB_PRECAUTIONARY_SETTINGS);
end;

procedure TfrmScannerMain.aShowPriceHistoryExecute(Sender: TObject);
var
  Data: PInstrumentData;
begin
  if Assigned(vstScanner.FocusedNode) then
  begin
    Data := vstScanner.FocusedNode^.GetData;
    TInformationDialog.ShowMessage(TDocumentInfo.GetPriceHistory(Data^.Id, nil), 'PriceHistory');
  end;
end;

procedure TfrmScannerMain.aShowTradeChartExecute(Sender: TObject);
begin
  if Assigned(vstScanner.FocusedNode) then
    ShowTradeChart(vstScanner.FocusedNode);
end;

procedure TfrmScannerMain.aShowTradeChartUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstScanner.IsEmpty and Assigned(vstScanner.FocusedNode);
end;

procedure TfrmScannerMain.aShowWeightedExecute(Sender: TObject);
begin
  if (rgWeightedFeed.ItemIndex = 0) then
    rgWeightedFeed.ItemIndex := 1
  else
    rgWeightedFeed.ItemIndex := 0;
  vstScanner.Invalidate;
end;

procedure TfrmScannerMain.rgWeightedFeedClick(Sender: TObject);
begin
  vstScanner.Invalidate;
end;

procedure TfrmScannerMain.CalculateAllRankingSum;
var
  Data: PInstrumentData;
  Node: PVirtualNode;
begin
  Node := vstScanner.GetFirstChild(vstScanner.RootNode);
  while Assigned(Node) do
  begin
    Data := Node^.GetData;
    CalculateRankingSum(Data);
    Node := Node.NextSibling;
  end;
end;

procedure TfrmScannerMain.CalculateRankingSum(const aData: PInstrumentData);
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
        case ColumnsInfo.SourceType of
          stStaticList:
            RankingSum := RankingSum + ColumnsItem.Rank * Weight;
          stScannerMarket:
            RankingSum := RankingSum + ColumnsItem.Rank;
          stCalcColumn:
            RankingSum := RankingSum + ColumnsItem.Price * Weight;
          stTickColumn:
//            if (ColumnsInfo.TickColumn.IBValue2 <> ttNotSet) then
//              RankingSum := RankingSum + ColumnsItem.Price * Weight * 100
//            else
              RankingSum := RankingSum + ColumnsItem.Price * Weight;
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
        vstScanner.InvalidateNode(aData^.Node);
      end);
  end;
end;

procedure TfrmScannerMain.CalculateValuesForAllNodes;
var
  ColumnsInfo: TColumnsInfo;
  Data: PInstrumentData;
  Node: PVirtualNode;
begin
  if not Application.Terminated then
  begin
    vstScanner.BeginUpdate;
    try
      for ColumnsInfo in FColumns.Values do
        if (ColumnsInfo.SourceType = stCalcColumn) then
        begin
          Node := vstScanner.GetFirstChild(vstScanner.RootNode);
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
      vstScanner.EndUpdate;
    end;
  end;
end;

procedure TfrmScannerMain.CancelScan;
begin
  for var ColumnsInfo in FColumns.Values do
  begin
    if (ColumnsInfo.SourceType = stScannerMarket) and IABClient.Connected then
      IABClient.CancelScan(ColumnsInfo.ScannerColumn.ScanId);
  end;
  lbIsolate.Items.Clear;
  gbIsolate.Visible := False;
end;

procedure TfrmScannerMain.CalculateGradient(const aNode: PVirtualNode; const aColumnsInfo: TColumnsInfo);
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
          TThread.NameThreadForDebugging('TfrmScannerMain.CalculateGradient');
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
              end;
            end;

            if not(Application.Terminated or (csDestroying in TForm(Self).ComponentState)) and
              Assigned(Data) and Data^.ExtraColumns.Items.ContainsKey(aColumnsInfo.ColumnId) then
            begin
              ColumnsItem := Data^.ExtraColumns.Items[aColumnsInfo.ColumnId];
              PrevPrice := ColumnsItem.Price;
              case aColumnsInfo.CalcColumn.CalculationType of
                ctGradientToday:
                  ColumnsItem.Price := gradientValue;
                ctCorridorWidth:
                  ColumnsItem.Price := corridorValue;
              end;
              ColumnsItem.IsUnsufficientData := IsUnsufficientData;
              if (not IsUnsufficientData) and (ColumnsItem.TimeStamp = 0) then
                ColumnsItem.TimeStamp := Now;

              if (ColumnsItem.Price = 0) then
                ColumnsItem.ColTick := clBlack
              else if (ColumnsItem.Price >= PrevPrice) then
                ColumnsItem.ColTick := clGreen
              else
                ColumnsItem.ColTick := clRed;
              Data^.ExtraColumns.Items.AddOrSetValue(aColumnsInfo.ColumnId, ColumnsItem);
              CalculateRankingSum(Data);
            end;
          except
            on E: Exception do
              TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'CalculateGradient', E.Message);
          end;
        end).Start;
    end;
  end;
end;

procedure TfrmScannerMain.CalculateGradientLogTerm(const aNode: PVirtualNode; const aColumnsInfo: TColumnsInfo);
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
        TThread.NameThreadForDebugging('TfrmScannerMain.CalculateGradientLogTerm');
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
                              vstScanner.InvalidateNode(aNode);
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

function TfrmScannerMain.GetCreatedOrdersCount: Integer;
begin
  Result := FAutoTradeInfo.CreatedOrdersCount;
end;

function TfrmScannerMain.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmScannerMain.SetAutoTradeInfo(const aAutoTradeInfo: TAutoTradeInfo);
begin
  IsLoaded := True;
  try
    FAutoTradeInfo.AssignFrom(aAutoTradeInfo);
    AutoTradeInfoToGUI;
  finally
    IsLoaded := False;
  end;
end;

procedure TfrmScannerMain.SetCreatedOrdersCount(const Value: Integer);
begin
  FAutoTradeInfo.CreatedOrdersCount := Value;
  CheckTradesState;
  if (Value > 0) then
    aExecute.ImageIndex := 49 //lightning red
  else
    aExecute.ImageIndex := 12;
  UpdateStatus(C_PANEL_CREATED_ORDERS, Value.ToString);
  UpdateStatus(C_PANEL_TIME_CREATED_ORDERS, FormatDateTime('hh:nn:ss', Now));
  AutoTradesControllerPublisher.UpdateState(Self);
end;

procedure TfrmScannerMain.SetValueFromPriceCache(const aData: PInstrumentData; const aColumnsInfo: TColumnsInfo);
var
  ColumnsItem: TExtraColumns.TColumnsItem;
begin
  if Assigned(aData) and
    (aColumnsInfo.SourceType = stTickColumn) and
    (aData^.ExtraColumns.Items.ContainsKey(aColumnsInfo.ColumnId)) then
  begin
    ColumnsItem := aData^.ExtraColumns.Items[aColumnsInfo.ColumnId];
    if (aColumnsInfo.TickColumn.IBValue2 <> ttNotSet) then
      ColumnsItem.Price := aColumnsInfo.TickColumn.TypeOperation.Calc(TMonitorLists.PriceCache.GetLastPrice(aData^.Id, aColumnsInfo.TickColumn.IBValue1),
                                                                      TMonitorLists.PriceCache.GetLastPrice(aData^.Id, aColumnsInfo.TickColumn.IBValue2))
    else
      ColumnsItem.Price := TMonitorLists.PriceCache.GetLastPrice(aData^.Id, aColumnsInfo.TickColumn.IBValue1);
    if (ColumnsItem.Price <> 0) then
      ColumnsItem.ColTick := clRed;
    aData^.ExtraColumns.Items.AddOrSetValue(aColumnsInfo.ColumnId, ColumnsItem);
    vstScanner.InvalidateNode(aData^.Node);
  end;
end;

procedure TfrmScannerMain.ShowScannerMarket(const aColumnID: Integer; const aScanOptions: TScannerColumn);
resourcestring
  rsInstances = 'Limit of Market scanner subscriptions is reached!';
begin
  if (aOpenIBScanning.Tag < 10) then
    TfrmScannerMarket.ShowDocument(aColumnID, aScanOptions, Self)
  else
    TMessageDialog.ShowWarning(rsInstances);
end;

procedure TfrmScannerMain.RequestMarketData(const aContractId: Integer);
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
        Request.HistoricalDataParams := THistoricalDataParams.Create(FAutoTradeInfo.HistoricalDataParams.DurationTimeUnits,
                                                                     FAutoTradeInfo.HistoricalDataParams.DataDuration,
                                                                     FAutoTradeInfo.HistoricalDataParams.BarSize,
                                                                     FAutoTradeInfo.HistoricalDataParams.DataBasis,
                                                                     FAutoTradeInfo.HistoricalDataParams.SubscribeHistData);
        IABClient.SendRequest(Request);
      end;
    finally
      FreeAndNil(Order);
    end;
end;

procedure TfrmScannerMain.ShowTradeChart(const aNode: PVirtualNode);
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

procedure TfrmScannerMain.OnHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
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
    if not FAutoTradeInfo.HistoricalDataParams.KeepUpdated then
      IABClient.CancelHistoricalData(DataId);

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

procedure TfrmScannerMain.aOpenIBScanningExecute(Sender: TObject);
var
  ScanOptions: TScannerColumn;
begin
  ScanOptions.Clear;
  ShowScannerMarket(-1, ScanOptions);
end;

procedure TfrmScannerMain.IncMarketInstances;
begin
  aOpenIBScanning.Tag := aOpenIBScanning.Tag + 1;
end;

procedure TfrmScannerMain.DecMarketInstances;
begin
  aOpenIBScanning.Tag := aOpenIBScanning.Tag - 1;
end;

procedure TfrmScannerMain.SetScanOptions(const aColumnId: Integer; const aScanOptions: TScannerColumn);
var
  ColumnsInfo: TColumnsInfo;
begin
    for var Id in FColumns.Keys do
    begin
      ColumnsInfo := FColumns.Items[Id];
      if (ColumnsInfo.SourceType = stScannerMarket) and (ColumnsInfo.ColumnId = aColumnId) then
      begin
        ColumnsInfo.ScannerColumn := aScanOptions;
        FColumns.AddOrSetValue(Id, ColumnsInfo);

        for var Index := 0 to vstScanner.Header.Columns.Count - 1 do
          if (vstScanner.Header.Columns[Index].Id = ColumnsInfo.ColumnId) then
          begin
            vstScanner.Header.Columns[Index].Hint := 'Weight: ' + aScanOptions.Weight.ToString;
            vstScanner.Header.Columns[Index].Text := ColumnsInfo.Caption;
            Break;
          end;
        if (ColumnsInfo.ScannerColumn.ScanId > 0) then
        begin
          IABClient.CancelScan(ColumnsInfo.ScannerColumn.ScanId);
          ColumnsInfo.ScannerColumn.ScanId := 0;
        end;
        vstScanner.Clear;
        FInstrumentList.Clear;
        ExecuteActions(ColumnsInfo);
        FillColumnList;
        Break;
      end;
    end;
end;

procedure TfrmScannerMain.aOpenStaticListExecute(Sender: TObject);
var
  ColumnsInfo: TColumnsInfo;
begin
  if (TfrmScannerStaticLists.ShowDocument(dmInsert, kaAddAll, -1, Self) = mrOk) then
  begin
    for var Id in FColumns.Keys do
    begin
      ColumnsInfo := FColumns.Items[Id];
      if (ColumnsInfo.SourceType = stStaticList) then
      begin
        ColumnsInfo.StaticColumn.FromDB(ColumnsInfo.StaticColumn.RecordId);
        ColumnsInfo.Weight := ColumnsInfo.StaticColumn.Weight;
        FColumns.AddOrSetValue(Id, ColumnsInfo);

        for var Index := 0 to vstScanner.Header.Columns.Count - 1 do
          if (vstScanner.Header.Columns[Index].Id = ColumnsInfo.ColumnId) then
          begin
            vstScanner.Header.Columns[Index].Hint := 'Weight: ' + ColumnsInfo.Weight.ToString;
            vstScanner.Header.Columns[Index].Text := ColumnsInfo.Caption;
            Break;
          end;
      end;
    end;
    FillColumnList;
    CalculateAllRankingSum;
    PreExecutionEvaluationOrders;
  end;
end;

procedure TfrmScannerMain.cbAutoOrderActiveClick(Sender: TObject);
begin
  GUIToAutoTradeInfo;
  if not FAutoTradeInfo.Active then
  begin
    FAutoTradeInfo.Active := True;
    TradesState := tsSuspended;
  end
  else
  begin
    TradesState := tsWorking;
    lblStartedTime.Caption := 'Started: ' + TimeToStr(Now);
  end;
  AutoTradesControllerPublisher.UpdateState(Self);
end;

procedure TfrmScannerMain.cbAutoRefreshClick(Sender: TObject);
begin
  if Showing then
  begin
    GUIToAutoTradeInfo;
    if cbAutoRefresh.Checked then
      for var ColumnsInfo in FColumns.Values do
        if (ColumnsInfo.SourceType = stScannerMarket) then
          ExecuteActions(ColumnsInfo);
  end;
end;

procedure TfrmScannerMain.aExecuteExecute(Sender: TObject);

  function CheckRequired: Boolean;
  var
    Problems: string;
  begin
    Problems := '';
    if (FAutoTradeInfo.MaxRows = 0) then
    begin
      SetFocusSafely(seMaxRows);
      Problems := Format(rcRequiredValue, ['Max rows']);
    end;
    if (FAutoTradeInfo.OrderAmount = 0) then
    begin
      SetFocusSafely(seSingleOrderAmount);
      Problems := Problems + Format(rcRequiredValue, ['Single order amount']);
    end;
    if (FAutoTradeInfo.TotalOrderAmount = 0) then
    begin
      SetFocusSafely(seTotalOrderAmount);
      Problems := Problems + Format(rcRequiredValue, ['Total order amount']);
    end;
    if FAutoTradeInfo.OrderCurrency.IsEmpty then
    begin
      SetFocusSafely(cbOrderCurrency);
      Problems := Problems + Format(rcRequiredValue, ['Order Currency']);
    end;
    if (FAutoTradeInfo.MaxNumberOrder = 0) then
    begin
      SetFocusSafely(seMaxNumberOrder);
      Problems := Problems + Format(rcRequiredValue, ['Max number of order']);
    end
    else if (CreatedOrdersCount >= FAutoTradeInfo.MaxNumberOrder) then
    begin
      SetFocusSafely(seMaxNumberOrder);
      Problems := Problems + rcCreatedOrdersCountGreater;
    end;

    if not FAutoTradeInfo.Active then
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
      FAutoTradeInfo.TotalOrderAmount := seTotalOrderAmount.Value;
      FAutoTradeInfo.Active := True;
    end;
    if CheckRequiredEmbargoColumn and CheckRequired then
      ApplyAutoOrder;
  finally
    if not FExplorationMode then
      FAutoTradeInfo.Active := False;
  end;
end;

procedure TfrmScannerMain.aExecuteDuplicateOrderExecute(Sender: TObject);
begin
  if cbAllowSendDuplicateOrder.Checked then
  begin
    CreatedOrdersCount := 0;
    PreExecutionEvaluationOrders;
  end;
end;

procedure TfrmScannerMain.cbOrderCurrencyAddClick(Sender: TObject);
begin
  if (cbOrderCurrency.Items.IndexOf(UpperCase(cbOrderCurrency.Text)) = -1) then
    cbOrderCurrency.Items.Add(UpperCase(cbOrderCurrency.Text));
end;

procedure TfrmScannerMain.cbOrderCurrencyKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['A' .. 'Z', 'a' .. 'z', #08]) then
    Key := #0;
end;

procedure TfrmScannerMain.OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
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

procedure TfrmScannerMain.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
var
  ColumnsInfo: TColumnsInfo;
  Data: PInstrumentData;
  InstrumentItem : TInstrumentItem;
  Node: PVirtualNode;
  ColumnsItem: TExtraColumns.TColumnsItem;
  PrevPrice: Double;
begin
  if (TickType = ttLast) then
    FIsPriceExists := FInstrumentList.ContainsKey(Id);
  for ColumnsInfo in FColumns.Values do
    if ((ColumnsInfo.SourceType = stTickColumn) and ((ColumnsInfo.TickColumn.IBValue1 = TickType) or (ColumnsInfo.TickColumn.IBValue2 = TickType))) or
       ((ColumnsInfo.SourceType = stCalcColumn) and (TickType = ttLast) and (ColumnsInfo.CalcColumn.CalculationType in [ctGradientToday, ctCorridorWidth])) then
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
                ColumnsItem := Data^.ExtraColumns.Items[ColumnsInfo.ColumnId];
                PrevPrice := ColumnsItem.Price;

                case ColumnsInfo.SourceType of
                  stStaticList:
                    ;
                  stScannerMarket:
                    ;
                  stTickColumn:
                    begin
                      if (ColumnsInfo.TickColumn.IBValue2 <> ttNotSet) then
                        ColumnsItem.Price := ColumnsInfo.TickColumn.TypeOperation.Calc(TMonitorLists.PriceCache.GetLastPrice(Id, ColumnsInfo.TickColumn.IBValue1),
                                                                                       TMonitorLists.PriceCache.GetLastPrice(Id, ColumnsInfo.TickColumn.IBValue2))
                      else
                        ColumnsItem.Price := Value;
                      if (ColumnsItem.TimeStamp = 0) then
                        ColumnsItem.TimeStamp := TimeStamp;
                    end;
                  stCalcColumn:
                    CalculateGradient(Node, ColumnsInfo);
                end;

                if (ColumnsItem.Price = 0) then
                  ColumnsItem.ColTick := clBlack
                else if (ColumnsItem.Price >= PrevPrice) then
                  ColumnsItem.ColTick := clGreen
                else
                  ColumnsItem.ColTick := clRed;

                Data^.ExtraColumns.Items.AddOrSetValue(ColumnsInfo.ColumnId, ColumnsItem);
                CalculateRankingSum(Data);
                vstScanner.InvalidateNode(Node);
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
end;

procedure TfrmScannerMain.OnScannerData(Sender: TObject; Scan: TIABScan);
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
    vstScanner.BeginUpdate;
    try
      Node := vstScanner.GetFirst;
      while Assigned(Node) do
      begin
        Data := Node^.GetData;
        if Data^.ExtraColumns.Items.ContainsKey(AColumnId) and (not Data^.IsLocked) then
        begin
          ColumnSum := 0;
          for ColumnsInfo in FColumns.Values do
            if (ColumnsInfo.SourceType in [stStaticList, stScannerMarket]) and
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
        Node := vstScanner.GetNext(Node);
      end;
    finally
      vstScanner.EndUpdate;
    end;
  end;

begin
  UpdateStatus(C_PANEL_SCAN_COUNT, Scan.Count.ToString);
  for ColumnsInfo in FColumns.Values do
    if (ColumnsInfo.SourceType = stScannerMarket) and (ColumnsInfo.ScannerColumn.ScanId = Scan.ScanId) then
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
          ColumnsItem.Rank := ColumnsInfo.ScannerColumn.Weight * (1 + (Scan.Count - Scan.Items[i].Rank - 1) * ColumnsInfo.ScannerColumn.Step);
          if IsNan(ColumnsItem.Rank) then
            ColumnsItem.Rank := 0;
          ColumnsItem.ColTick := clBlack;
          ExtraColumns.Items.AddOrSetValue(ColumnsInfo.ColumnId, ColumnsItem);
        end;
      end;
      FAutoTradeInfo.LastUpdate := Now;
      AddInstrument(@Instruments, ColumnsInfo);
      TThread.Queue(nil,
        procedure
        begin
          UpdateStatus(C_PANEL_TIME_UPDATE, TimeToStr(FAutoTradeInfo.LastUpdate));
          if not FAutoTradeInfo.AutoRefresh then
          begin
            IABClient.CancelScan(Scan.ScanId);
            TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, Self, 'OnScannerData', 'Unsubscribed intentionally, to bypass the limit on the number of scans in IB');
          end;
        end);
      AutoTradesControllerPublisher.UpdateState(Self);
      Break;
    end;
end;

procedure TfrmScannerMain.OnScannerParam(Sender: TObject; Parameters: string);
begin
  //
end;

procedure TfrmScannerMain.OnScannerAdd(Sender: TObject; ScanId: Integer);
begin
  UpdateStatus(C_PANEL_TOTAL_NUM_SCANS, IABClient.Scanner.Count.ToString);
end;

procedure TfrmScannerMain.OnScannerCancel(Sender: TObject; ScanId: Integer);
var
  ColumnsInfo: TColumnsInfo;
begin
  UpdateStatus(C_PANEL_TOTAL_NUM_SCANS, IABClient.Scanner.Count.ToString);
  for var RunColumnsInfo in FColumns.Values do
    if (RunColumnsInfo.SourceType = stScannerMarket) and (RunColumnsInfo.ScannerColumn.ScanId = ScanId) then
    begin
      ColumnsInfo.AssignFrom(RunColumnsInfo);
      ColumnsInfo.ScannerColumn.ScanId := 0;
      FColumns.AddOrSetValue(ColumnsInfo.ColumnId, ColumnsInfo);
    end
end;

procedure TfrmScannerMain.OnGUIToAutoTradeInfo(Sender: TObject);
begin
  if Showing then
    GUIToAutoTradeInfo;
end;

procedure TfrmScannerMain.TimerCalculateGradientTimer(Sender: TObject);
begin
  CalculateValuesForAllNodes;
end;

procedure TfrmScannerMain.UpdateCaptions;
begin
  lblAutoTradesName.Caption := Concat('AutoTrade Name: ', FAutoTradeInfo.Name, ' (', FAutoTradeInfo.RecordId.ToString, ')').Replace(sLineBreak, '');
  lblInstanceNum.Caption    := Concat('Instance Num: ', Abs(FAutoTradeInfo.InstanceNum).ToString);
  if (FAutoTradeInfo.InstanceNum > 0) then
    lblInstanceNum.Caption := Concat(lblInstanceNum.Caption, ' (Autotrade)')
  else
    lblInstanceNum.Caption := Concat(lblInstanceNum.Caption, ' (Manual)');

  FAutoTradeInfo.Note := lbColumns.Items.Text;
  if (FAutoTradeInfo.InstanceNum <= 0) then
    AutoTradesControllerPublisher.UpdateState(Self);
end;

procedure TfrmScannerMain.UpdateStatus(const aPanelNum: Byte; const aInfo: string);
begin
  sbMain.Panels[aPanelNum].Text := aInfo;
  sbMain.Refresh;
end;

procedure TfrmScannerMain.GUIToAutoTradeInfo;
begin
  if not IsLoaded then
  begin
    if FExplorationMode then
    begin
      FAutoTradeInfo.Active                  := cbAutoOrderActive.Checked;
      FAutoTradeInfo.AllowSendDuplicateOrder := cbAllowSendDuplicateOrder.Checked;
      FAutoTradeInfo.AutoRefresh             := cbAutoRefresh.Checked;
      FAutoTradeInfo.MaxNumberOrder          := seMaxNumberOrder.Value;
      FAutoTradeInfo.MaxRows                 := seMaxRows.Value;
      FAutoTradeInfo.OrderAmount             := seSingleOrderAmount.Value;
      FAutoTradeInfo.TotalOrderAmount        := seTotalOrderAmount.Value;
      FAutoTradeInfo.OrderCurrency           := cbOrderCurrency.Text;

      FAutoTradeInfo.HistoricalDataParams.SubscribeHistData := cbSubscribeHistoricalData.Checked;
      FAutoTradeInfo.HistoricalDataParams.KeepUpdated       := cbHistDataKeepUpdated.Checked;
      FAutoTradeInfo.HistoricalDataParams.DataDuration      := edDuration.ValueInt;
      if (cbValidBarSize.ItemIndex > -1) then
        FAutoTradeInfo.HistoricalDataParams.BarSize := TIABChartBarSize(cbValidBarSize.ItemIndex);
      if (cbDurationTimeUnits.ItemIndex > -1) then
        FAutoTradeInfo.HistoricalDataParams.DurationTimeUnits := cbDurationTimeUnits.ItemIndex;
    end;
    FAutoTradeInfo.Name := edAutoTradeTemplate.Text;

    if cbAutoRefresh.Checked or FAutoTradeInfo.AutoRefresh then
      UpdateStatus(C_PANEL_AUTO_REFRESH, 'Auto Refresh')
    else
      UpdateStatus(C_PANEL_AUTO_REFRESH, '');

    if FAutoTradeInfo.Active and
       (FAutoTradeInfo.OrderGroupId > 0) and
       (CreatedOrdersCount < FAutoTradeInfo.MaxNumberOrder) then
      TradesState := tsWorking
    else
      TradesState := tsSuspended;
    CheckTradesState;
    AutoTradesControllerPublisher.UpdateState(Self);
    if Assigned(FAutoTradesEdit) then
      FAutoTradesEdit.SetAutoTradeInfo(FAutoTradeInfo);
  end;
end;

procedure TfrmScannerMain.AutoTradeInfoToGUI;
begin
  edAutoTradeTemplate.Text          := FAutoTradeInfo.Name;
  cbAllowSendDuplicateOrder.Checked := FAutoTradeInfo.AllowSendDuplicateOrder;
  cbAutoOrderActive.Checked         := FAutoTradeInfo.Active;
  cbAutoRefresh.Checked             := FAutoTradeInfo.AutoRefresh;
  cbOrderCurrency.Text              := FAutoTradeInfo.OrderCurrency;
  seMaxNumberOrder.Value            := FAutoTradeInfo.MaxNumberOrder;
  seMaxRows.Value                   := FAutoTradeInfo.MaxRows;
  seSingleOrderAmount.Value         := FAutoTradeInfo.OrderAmount;
  seTotalOrderAmount.Value          := FAutoTradeInfo.TotalOrderAmount;
  cbValidBarSize.ItemIndex          := Ord(FAutoTradeInfo.HistoricalDataParams.BarSize);
  cbSubscribeHistoricalData.Checked := FAutoTradeInfo.HistoricalDataParams.SubscribeHistData;
  cbHistDataKeepUpdated.Checked     := FAutoTradeInfo.HistoricalDataParams.KeepUpdated;
  cbDurationTimeUnits.ItemIndex     := FAutoTradeInfo.HistoricalDataParams.DurationTimeUnits;
  edDuration.ValueInt               := FAutoTradeInfo.HistoricalDataParams.DataDuration;
  if FAutoTradeInfo.AutoRefresh then
    UpdateStatus(C_PANEL_AUTO_REFRESH, 'Auto Refresh')
  else
    UpdateStatus(C_PANEL_AUTO_REFRESH, '');
  UpdateCaptions;
  if Assigned(FAutoTradesEdit) then
    FAutoTradesEdit.SetAutoTradeInfo(FAutoTradeInfo);
end;

procedure TfrmScannerMain.vstScannerBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
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

procedure TfrmScannerMain.vstScannerCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
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
      FColumns.TryGetValue(vstScanner.Header.Columns[Column].Id, ColumnsInfo) and
      (Data1^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId)) and
      (Data2^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId)) then
    begin
      ColumnsItem1 := Data1^.ExtraColumns.Items[ColumnsInfo.ColumnId];
      ColumnsItem2 := Data2^.ExtraColumns.Items[ColumnsInfo.ColumnId];
      Result := CompareValue(ColumnsItem1.Price, ColumnsItem2.Price);
    end;
  end;
end;

procedure TfrmScannerMain.vstScannerDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TfrmScannerMain.vstScannerDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := (Sender <> Source) and (Source is TVirtualStringTree);
end;

procedure TfrmScannerMain.vstScannerDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
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
  else if FColumns.TryGetValue(vstScanner.Header.Columns[Column].Id, ColumnsInfo) and
          (Data^.ExtraColumns.Items.ContainsKey(ColumnsInfo.ColumnId)) then
  begin
    ColumnsItem := Data^.ExtraColumns.Items[ColumnsInfo.ColumnId];
    TargetCanvas.Font.Color := ColumnsItem.ColTick;
    if (ColumnsInfo.SourceType = stEmbargoColumn) then
      TargetCanvas.Font.Color := ColumnsInfo.EmbargoColumn.GetCriteriaColor(@ColumnsInfo, @Data^.ExtraColumns)
    else if (ColumnsInfo.SourceType = stScannerMarket) and (ColumnsInfo.ScannerColumn.Weight = 0) then
      TargetCanvas.Font.Color := clMedGray
    else if (ColumnsItem.Weight <> 0) then
    begin
      TargetCanvas.Brush.Color := clSilver;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
end;

procedure TfrmScannerMain.vstScannerFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PInstrumentData;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
  begin
//    if not(csDestroying in Self.ComponentState) then
      if Assigned(FSubscribedList) and FSubscribedList.Contains(Data^.Id) then
      begin
        if FAutoTradeInfo.HistoricalDataParams.KeepUpdated then
          FSubscribedList.Extract(Data^.Id);
        IABClient.CancelHistoricalData(Data^.Id);
      end;
    Data^.Clear;
  end;
end;

procedure TfrmScannerMain.vstScannerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
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
    if FColumns.TryGetValue(vstScanner.Header.Columns[Column].Id, ColumnsInfo) then
      CellText := Data^.ExtraColumns.GetText(ColumnsInfo, rgWeightedFeed.ItemIndex = 0);
  end;
end;

procedure TfrmScannerMain.vstScannerHeaderDblClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var
  ColumnId: Integer;
begin
  if (HitInfo.Column > C_FIXED_COLUMN_INDEX - 1) then
  begin
    ColumnId := vstScanner.Header.Columns[HitInfo.Column].Id;
    ShowColumnDetails(ColumnId);
  end;
end;

procedure TfrmScannerMain.vstScannerHeaderDragging(Sender: TVTHeader;Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column > C_FIXED_COLUMN_INDEX;
end;

procedure TfrmScannerMain.vstScannerHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements := [hpeBackground];
end;

procedure TfrmScannerMain.vstScannerHeaderMouseDown(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitInfo: THitInfo;
begin
  if (Button = TMouseButton.mbRight) then
  begin
    vstScanner.GetHitTestInfoAt(X, Y, False, HitInfo);
    if HitInfo.HitColumn > 0 then
      EditWeigthColumn(HitInfo.HitColumn);
  end;
end;

procedure TfrmScannerMain.vstScannerAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
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

procedure TfrmScannerMain.vstScannerGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

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
      if FColumns.ContainsKey(vstScanner.Header.Columns[Column].Id) then
      begin
        ColumnsInfo := FColumns[vstScanner.Header.Columns[Column].Id];
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

procedure TfrmScannerMain.vstScannerInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data: PInstrumentData;
begin
  Data := Sender.GetNodeData(Node);
  Data^.RecordId := -1;
end;

procedure TfrmScannerMain.vstScannerPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
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

function TfrmScannerMain.GetAutoTradeInfo: TAutoTradeInfo;
begin
  Result := FAutoTradeInfo;
end;

procedure TfrmScannerMain.CloseAutoTrade(const aSilenceMode: Boolean = False);
begin
  FSilenceMode := aSilenceMode;
  Self.Close;
end;

procedure TfrmScannerMain.SetTradesState(const aValue: TTradesState);
begin
  if (FAutoTradeInfo.TradesState <> aValue) then
  begin
    FAutoTradeInfo.TradesState := aValue;
    case FAutoTradeInfo.TradesState of
      tsSuspended:
        FAutoTradeInfo.Active := False;
      tsExecuted:
        if CheckRequiredEmbargoColumn then
        begin
          FAutoTradeInfo.Active := True;
          ApplyAutoOrder;
        end;
      tsWorking:
        FAutoTradeInfo.Active := True;
      tsCancelled:
        begin
          FAutoTradeInfo.Active := False;
          CancelScan;
        end;
      tsFinished:
        begin
          FAutoTradeInfo.Active := False;
          CancelScan;
        end;
      tsRestarted:
        begin
          CreatedOrdersCount := 0;
          FAutoTradeInfo.Active := True;
          OpenSequenceRecord;
          FAutoTradeInfo.TradesState := tsWorking;
        end;
    end;
    CheckTradesState;
    AutoTradeInfoToGUI;
    AutoTradesControllerPublisher.UpdateState(Self);
  end;
end;

procedure TfrmScannerMain.CheckTradesState;
begin
  if (CreatedOrdersCount >= FAutoTradeInfo.MaxNumberOrder) then
  begin
    pnlTop.Color := clSilver;
    FAutoTradeInfo.TradesState := tsFinished;
    FAutoTradeInfo.Active := False;
    CancelScan;
  end
  else if FAutoTradeInfo.Active and (FAutoTradeInfo.OrderGroupId > 0) then
  begin
    pnlTop.Color := clMoneyGreen;
    FAutoTradeInfo.TradesState := tsWorking;
  end
  else
  begin
    pnlTop.Color := clInfoBk;
    FAutoTradeInfo.TradesState := tsSuspended;
  end;

  if Assigned(Self.Parent) then
    if (Self.Parent is TTabSheet) then
    begin
      case FAutoTradeInfo.TradesState of
        tsWorking:
          TTabSheet(Self.Parent).ImageIndex := C_COLOUR_INDEX_GREEN;
        tsSuspended:
          TTabSheet(Self.Parent).ImageIndex := C_COLOUR_INDEX_YELLOW;
        tsFinished:
          TTabSheet(Self.Parent).ImageIndex := C_COLOUR_INDEX_NAVY;
      end;
    end;
end;

procedure TfrmScannerMain.ApplyAutoOrder;
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
  Info := 'AutoTradeInfo.Active='                    + BoolToStr(FAutoTradeInfo.Active, True) +
          ', AutoTradeInfo.AllowSendDuplicateOrder=' + BoolToStr(FAutoTradeInfo.AllowSendDuplicateOrder, True) +
          ', AutoTradeInfo.OrderAmount='             + FAutoTradeInfo.OrderAmount.ToString +
          ', AutoTradeInfo.TotalOrderAmount='        + FAutoTradeInfo.TotalOrderAmount.ToString +
          ', AutoTradeInfo.OrderGroupId='            + FAutoTradeInfo.OrderGroupId.ToString +
          ', AutoTradeInfo.MaxNumberOrder='          + FAutoTradeInfo.MaxNumberOrder.ToString +
          ', CreatedOrdersCount='                    + CreatedOrdersCount.ToString;

  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, 'ApplyAutoOrder', Info);
  if FAutoTradeInfo.Active then
  begin
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'ApplyAutoOrder', 'Task WaitForAll');
    FIsPriceExists := False;
    if (FAutoTradeInfo.TotalOrderAmount <= 0) then
      ShowNotification(nil, rsTotalOrderAmount);

    WaitForTask := System.Threading.TTask.Create(
      procedure()
      var
        Counter: Integer;
      begin
        TThread.NameThreadForDebugging('TfrmScannerMain.ApplyAutoOrder');
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
//            case FAutoTradeInfo.RankingCriteria of
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

end.
