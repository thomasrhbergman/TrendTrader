unit Monitor;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

{$DEFINE HIDE_OLD}

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Bleeper, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Edit.OrderIB, Vcl.Buttons, Edit.Condition,
  Vcl.AppEvnts, Edit.Factor, Edit.Algos, Relations, Edit.OrderGroup, System.StrUtils, VirtualTrees, Winapi.ActiveX,
  Vcl.ExtCtrls, Vcl.Menus, Vcl.DBCtrls, DaModule, System.ImageList, Vcl.ImgList, Vcl.TabNotBk, System.DateUtils,
  System.SyncObjs, Vcl.Imaging.pngimage, Data.DB, System.Math, LoginNN,
  Winapi.ShellAPI, DaImages, Search.Instruments, BrokerHelperAbstr, LoginIB, System.Generics.Collections, InstrumentList,
  Chart.Trade, System.Actions, Vcl.ActnList, System.IniFiles, System.UITypes, DebugWriter, HtmlLib, Utils, XmlFiles,
  DockForm.Position, IABFunctions, IABSocketAPI, Global.Types, UIABsocket, DockForm.AccountInfo, DockForm.AccountPnL,
  Document, OpenDialog.Algos, Column.OrderSelections, DockForm.ActiveOrders, Search.RequestToIB, Chart.Condition,
  IABSocketAPI_const, Chart.ConditionAlgos, DockForm.LogView, Chart.ConditionRealtime, Edit.OrderNN, NNfunctions.Types,
  BrokerHelperNN, NNfunctions, CustomForms, OrderChange, CheckIBInstruments, Scanner.Types, System.Generics.Defaults,
  Scanner.Main, InformationDialog, Vcl.Printers, Edit.OrderBaseTransform, Order.Utils, Edit.OrderDocument, VirtualTrees.Types,
  System.Threading, Qualifiers, MessageDialog, OpenDialog.OrderGroupSet, Scanner.Market, DockForm.OrderStatus,
  DockForm.AutoTradesController, HtmlConsts, Column.CustomSelections, BrokerHelperFactory, Edit.OrderStatus, System.Types,
  TickByTick.Data, Entity.Sokid, Entity.Price, System.TimeSpan, DockForm.TemplateCreator, Vcl.Samples.Spin, System.IOUtils,
  Qualifiers.Types, DockForm.QualifiersController, Utils.LocalInformation, Monitor.Types, AutoTrades, Vcl.ComCtrls,
  IABFunctions.MessageCodes, Publishers.Interfaces, IABFunctions.RequestsQueue, Monitor.Interfaces, IABFunctions.MarketRules,
  Common.Types, AutoTrades.Types, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} DatabaseProperties, SplashScreen,
  VirtualTrees.ExportHelper, AutoTrades.ComputeOrders, Monitor.Info, IABFunctions.MarketData, DaModule.Utils,
  MonitorTree.Factory, Frame.ActivityLog, Monitor.EventController, Global.Resources, Publishers, DockForm.TotalController,
  System.Notification, Entity.OrderStatus, IABFunctions.Helpers, DockForm.MonitorFilter, MonitorTree.Helper, MonitorTree.Document,

  IdGlobal, IdBaseComponent, IdComponent, IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer, IdContext,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet, ListForm;
{$ENDREGION}

type
  THeaderColInfo = record
    Caption  : string;
    TickType : TIABTickType;
    IsFixed  : Boolean;
    IsFreeze : Boolean;
  end;

  TfrmMonitor = class(TCustomForm, IUpdateFeeds,
                                   IMonitor,
                                   IConnectionState,
                                   IOrderState,
                                   IHistoricalData,
                                   IMotherFilledPriceChange,
                                   IError)
    aActivateAll: TAction;
    aActivateOrders: TAction;
    aAddAlgos: TAction;
    aAddCondition: TAction;
    aAddFactor: TAction;
    aAddOrderGroup: TAction;
    aAutoTradesExecute: TAction;
    aCancelBrokerOrder: TAction;
    aCheckIBInstruments: TAction;
    aCheckLastPrice: TAction;
    aClose: TAction;
    aCollapsTree: TAction;
    aColumnSettings: TAction;
    aConnect: TAction;
    ActionListMain: TActionList;
    aDeleteAllNodes: TAction;
    aDeleteNode: TAction;
    aDisconnect: TAction;
    aDuplicateAlgos: TAction;
    aDuplicateCondition: TAction;
    aDuplicateOrder: TAction;
    aExpandTree: TAction;
    aExportToCSV: TAction;
    aExportToExcel: TAction;
    aHideNode: TAction;
    aInactivateOrders: TAction;
    aLogin: TAction;
    aMakeRepetitiveOrder: TAction;
    aOpenCheckIBInstruments: TAction;
    aOpenGroupSet: TAction;
    aOpenIBCommand: TAction;
    aOpenLogFile: TAction;
    aOpenRelation: TAction;
    ApplicationEvents: TApplicationEvents;
    aPrint: TAction;
    aQualifiersExecute: TAction;
    aRegularRunQualifier: TAction;
    aRenameGroup: TAction;
    aRequestToIBByHistoricalData: TAction;
    aRestart: TAction;
    aSaveGroupSet: TAction;
    aSaveGroupSetAs: TAction;
    aSearch: TAction;
    aShowAutoTrades: TAction;
    aShowCalColumns: TAction;
    aShowCalculationStageInfo: TAction;
    aShowConditionAlgosChart: TAction;
    aShowConditionChart: TAction;
    aShowContractInspector: TAction;
    aShowDatabaseProperties: TAction;
    aShowEditDialog: TAction;
    aShowExchangeRateInfo: TAction;
    aShowGlobalParameters: TAction;
    aShowInformationDialog: TAction;
    aShowInstrumentsWithoutFeedInfo: TAction;
    aShowMonitorConditionChart: TAction;
    aShowOrderStatus: TAction;
    aShowOrdersTimestampInfo: TAction;
    aShowPrecautionarySettingsInfo: TAction;
    aShowPriceHistory: TAction;
    aShowPriceInfo: TAction;
    aShowQualifiers: TAction;
    aShowRuleInformationDialog: TAction;
    aShowSubscribersInfo: TAction;
    aShowSumAlgos: TAction;
    aShowSumCondition: TAction;
    aShowTickByTick: TAction;
    aShowTradeChart: TAction;
    aTransformToBaseOrder: TAction;
    aTransformToOrder: TAction;
    aUnhideAll: TAction;
    aUnhideLastNode: TAction;
    aUseIB: TAction;
    aUseIBfeeds: TAction;
    aUseNN: TAction;
    btnActivateAllOld: TBitBtn;
    btnAutoTradesExecute: TBitBtn;
    btnColumnSettings: TBitBtn;
    btnExportToCSV: TBitBtn;
    btnExportToExcel: TBitBtn;
    btnLoginOld: TBitBtn;
    btnOpenGroup: TBitBtn;
    btnPrint: TBitBtn;
    btnQualifiersExecute: TBitBtn;
    btnRegularRunQualifier: TBitBtn;
    btnRestartOld: TBitBtn;
    btnSaveGroup: TBitBtn;
    btnSaveGroupAs: TBitBtn;
    btnSearch: TBitBtn;
    btnShowAutoTrades: TBitBtn;
    btnShowCalColumns: TBitBtn;
    btnShowContractInspector: TBitBtn;
    btnShowDatabaseProperties: TBitBtn;
    btnShowQualifiers: TBitBtn;
    btnStartHTTPServer: TButton;
    btnTWSTime: TButton;
    cbAccounts: TComboBox;
    cbAutoTrades: TDBLookupComboBox;
    cbQualifiers: TDBLookupComboBox;
    ChB_HideFilled: TCheckBox;
    edCurrentGroupName: TEdit;
    frameActivityLog: TframeActivityLog;
    gbConnectOld: TGroupBox;
    gbExecute: TGroupBox;
    gbExplore: TGroupBox;
    gbService: TGroupBox;
    GroupBox1: TGroupBox;
    HTTPServer: TIdHTTPServer;
    lblAccountBalans: TLabel;
    lblAccountBalansCaption: TLabel;
    lblAcountRealised: TLabel;
    lblAcountRealisedCaption: TLabel;
    lblAcountUnrealised: TLabel;
    lblAcountUnrealisedCaption: TLabel;
    lblAutoTrades: TLabel;
    lblIPAddress: TLabel;
    lblQualifiers: TLabel;
    MainMenu: TMainMenu;
    miAction: TMenuItem;
    miActivateOrders: TMenuItem;
    miAddAlgos: TMenuItem;
    miAddCondition: TMenuItem;
    miAddFactor: TMenuItem;
    miAddOrderGroup: TMenuItem;
    miCancelBrokerOrder: TMenuItem;
    miChart: TMenuItem;
    miCheckIBInstruments: TMenuItem;
    miCheckLastPrice: TMenuItem;
    miClose: TMenuItem;
    miCollapsTree: TMenuItem;
    miColumnSettings: TMenuItem;
    miConditionFactor: TMenuItem;
    miConnect: TMenuItem;
    miData: TMenuItem;
    miDeleteAllNodes: TMenuItem;
    miDeleteNode: TMenuItem;
    miDelimiter1: TMenuItem;
    miDelimiter2: TMenuItem;
    miDelimiter3: TMenuItem;
    miDelimiter4: TMenuItem;
    miDisconnect: TMenuItem;
    miDuplicateAlgos: TMenuItem;
    miDuplicateCondition: TMenuItem;
    miDuplicateOrder: TMenuItem;
    miExpandTree: TMenuItem;
    miGetInfo: TMenuItem;
    miHideNode: TMenuItem;
    miInactivateOrders: TMenuItem;
    miLoadHistoricalData: TMenuItem;
    miMakeRepetitiveOrder: TMenuItem;
    miOpenCheckIBInstruments: TMenuItem;
    miOpenIBCommand: TMenuItem;
    miOpenLogFile: TMenuItem;
    miOpenRelation: TMenuItem;
    miOrderStatus: TMenuItem;
    miRealtimeChart: TMenuItem;
    miReports: TMenuItem;
    miSaveGroupAs1: TMenuItem;
    miSearch: TMenuItem;
    miSep01: TMenuItem;
    miSep02: TMenuItem;
    miSep03: TMenuItem;
    miSep04: TMenuItem;
    miSep05: TMenuItem;
    miSep06: TMenuItem;
    miShowCalculationStageInfo: TMenuItem;
    miShowConditionChart: TMenuItem;
    miShowExchangeRate: TMenuItem;
    miShowGlobalParameters: TMenuItem;
    miShowInstrumentsWithoutFeed: TMenuItem;
    miShowOrdersTimestamp: TMenuItem;
    miShowPrecautionarySettingsInfo: TMenuItem;
    miShowPriceHistory: TMenuItem;
    miShowPriceInformationDialog: TMenuItem;
    miShowRuleInformationDialog: TMenuItem;
    miShowSubscribersInformation: TMenuItem;
    miShowSumAlgos: TMenuItem;
    miShowSumCondition: TMenuItem;
    miShowTickByTick: TMenuItem;
    miShowTradeChart: TMenuItem;
    miTransformToBaseOrder: TMenuItem;
    miTransformToOrder: TMenuItem;
    miTransformToOrders: TMenuItem;
    miTree: TMenuItem;
    miUnhideAll: TMenuItem;
    miUnhideLastNode: TMenuItem;
    miUseIB: TMenuItem;
    miUseIBfeeds: TMenuItem;
    miUseNN: TMenuItem;
    pgcMain: TPageControl;
    pmMainTree: TPopupMenu;
    pnlActivityLog: TPanel;
    pnlMain: TPanel;
    pnlOptions: TPanel;
    pnlTopOld: TPanel;
    sbMain: TStatusBar;
    TimerRunQualifier: TTimer;
    TimerTimeGap: TTimer;
    tsOrders: TTabSheet;
    vstMonitor: TVirtualStringTree;
    pnlTop: TPanel;
    gbConnect: TGroupBox;
    btnLogin: TBitBtn;
    btnRestart: TBitBtn;
    btnActivateAll: TBitBtn;
    btnAutoTrades: TBitBtn;
    btnQualifiers: TBitBtn;
    btnCandidatesProcess: TBitBtn;
    btnQuantities: TBitBtn;
    btnOrderTemplates: TBitBtn;
    procedure aActivateAllExecute(Sender: TObject);
    procedure aActivateOrdersExecute(Sender: TObject);
    procedure aActivateOrdersUpdate(Sender: TObject);
    procedure aAddAlgosExecute(Sender: TObject);
    procedure aAddAlgosUpdate(Sender: TObject);
    procedure aAddConditionExecute(Sender: TObject);
    procedure aAddConditionUpdate(Sender: TObject);
    procedure aAddFactorExecute(Sender: TObject);
    procedure aAddFactorUpdate(Sender: TObject);
    procedure aAddOrderGroupExecute(Sender: TObject);
    procedure aAddOrderGroupUpdate(Sender: TObject);
    procedure aAutoTradesExecuteExecute(Sender: TObject);
    procedure aAutoTradesExecuteUpdate(Sender: TObject);
    procedure aCancelBrokerOrderExecute(Sender: TObject);
    procedure aCancelBrokerOrderUpdate(Sender: TObject);
    procedure aCheckIBInstrumentsExecute(Sender: TObject);
    procedure aCheckLastPriceExecute(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure aCollapsTreeExecute(Sender: TObject);
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aConnectExecute(Sender: TObject);
    procedure aDeleteAllNodesExecute(Sender: TObject);
    procedure aDeleteAllNodesUpdate(Sender: TObject);
    procedure aDeleteNodeExecute(Sender: TObject);
    procedure aDeleteNodeUpdate(Sender: TObject);
    procedure aDisconnectExecute(Sender: TObject);
    procedure aDuplicateAlgosExecute(Sender: TObject);
    procedure aDuplicateAlgosUpdate(Sender: TObject);
    procedure aDuplicateConditionExecute(Sender: TObject);
    procedure aDuplicateConditionUpdate(Sender: TObject);
    procedure aDuplicateOrderExecute(Sender: TObject);
    procedure aDuplicateOrderUpdate(Sender: TObject);
    procedure aExpandTreeExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aHideNodeExecute(Sender: TObject);
    procedure aHideNodeUpdate(Sender: TObject);
    procedure aInactivateOrdersExecute(Sender: TObject);
    procedure aInactivateOrdersUpdate(Sender: TObject);
    procedure aLoginExecute(Sender: TObject);
    procedure aMakeRepetitiveOrderExecute(Sender: TObject);
    procedure aMakeRepetitiveOrderUpdate(Sender: TObject);
    procedure aOpenCheckIBInstrumentsExecute(Sender: TObject);
    procedure aOpenGroupSetExecute(Sender: TObject);
    procedure aOpenGroupSetUpdate(Sender: TObject);
    procedure aOpenIBCommandExecute(Sender: TObject);
    procedure aOpenLogFileExecute(Sender: TObject);
    procedure aOpenLogFileUpdate(Sender: TObject);
    procedure aOpenRelationExecute(Sender: TObject);
    procedure aOpenRelationUpdate(Sender: TObject);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure aPrintExecute(Sender: TObject);
    procedure aQualifiersExecuteExecute(Sender: TObject);
    procedure aQualifiersExecuteUpdate(Sender: TObject);
    procedure aRegularRunQualifierExecute(Sender: TObject);
    procedure aRenameGroupExecute(Sender: TObject);
    procedure aRenameGroupUpdate(Sender: TObject);
    procedure aRequestToIBByHistoricalDataExecute(Sender: TObject);
    procedure aRequestToIBByHistoricalDataUpdate(Sender: TObject);
    procedure aRestartExecute(Sender: TObject);
    procedure aSaveGroupSetAsExecute(Sender: TObject);
    procedure aSaveGroupSetAsUpdate(Sender: TObject);
    procedure aSaveGroupSetExecute(Sender: TObject);
    procedure aSaveGroupSetUpdate(Sender: TObject);
    procedure aSearchExecute(Sender: TObject);
    procedure aSearchUpdate(Sender: TObject);
    procedure aShowAutoTradesExecute(Sender: TObject);
    procedure aShowCalColumnsExecute(Sender: TObject);
    procedure aShowCalculationStageInfoExecute(Sender: TObject);
    procedure aShowConditionAlgosChartExecute(Sender: TObject);
    procedure aShowConditionAlgosChartUpdate(Sender: TObject);
    procedure aShowConditionChartExecute(Sender: TObject);
    procedure aShowConditionChartUpdate(Sender: TObject);
    procedure aShowContractInspectorExecute(Sender: TObject);
    procedure aShowDatabasePropertiesExecute(Sender: TObject);
    procedure aShowEditDialogExecute(Sender: TObject);
    procedure aShowEditDialogUpdate(Sender: TObject);
    procedure aShowExchangeRateInfoExecute(Sender: TObject);
    procedure aShowGlobalParametersExecute(Sender: TObject);
    procedure aShowInformationDialogExecute(Sender: TObject);
    procedure aShowInstrumentsWithoutFeedInfoExecute(Sender: TObject);
    procedure aShowMonitorConditionChartExecute(Sender: TObject);
    procedure aShowMonitorConditionChartUpdate(Sender: TObject);
    procedure aShowOrderStatusExecute(Sender: TObject);
    procedure aShowOrderStatusUpdate(Sender: TObject);
    procedure aShowOrdersTimestampInfoExecute(Sender: TObject);
    procedure aShowPrecautionarySettingsInfoExecute(Sender: TObject);
    procedure aShowPriceHistoryExecute(Sender: TObject);
    procedure aShowPriceHistoryUpdate(Sender: TObject);
    procedure aShowPriceInfoExecute(Sender: TObject);
    procedure aShowQualifiersExecute(Sender: TObject);
    procedure aShowRuleInformationDialogExecute(Sender: TObject);
    procedure aShowRuleInformationDialogUpdate(Sender: TObject);
    procedure aShowSubscribersInfoExecute(Sender: TObject);
    procedure aShowSumAlgosExecute(Sender: TObject);
    procedure aShowSumConditionExecute(Sender: TObject);
    procedure aShowTickByTickExecute(Sender: TObject);
    procedure aShowTradeChartExecute(Sender: TObject);
    procedure aShowTradeChartUpdate(Sender: TObject);
    procedure aTransformToBaseOrderExecute(Sender: TObject);
    procedure aTransformToBaseOrderUpdate(Sender: TObject);
    procedure aTransformToOrderExecute(Sender: TObject);
    procedure aTransformToOrderUpdate(Sender: TObject);
    procedure aUnhideAllExecute(Sender: TObject);
    procedure aUnhideAllUpdate(Sender: TObject);
    procedure aUnhideLastNodeExecute(Sender: TObject);
    procedure aUnhideLastNodeUpdate(Sender: TObject);
    procedure aUseIBExecute(Sender: TObject);
    procedure aUseIBfeedsExecute(Sender: TObject);
    procedure aUseNNExecute(Sender: TObject);
    procedure btnStartHTTPServerClick(Sender: TObject);
    procedure btnTestingStartClick(Sender: TObject);
    procedure btnTWSTimeClick(Sender: TObject);
    procedure cbAccountsChange(Sender: TObject);
    procedure cbAutoTradesCloseUp(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure HTTPServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure OnConditionTimeGap(Sender: TObject);
    procedure pgcMainChange(Sender: TObject);
    procedure pmMainTreePopup(Sender: TObject);
    procedure sbMainClick(Sender: TObject);
    procedure sbMainDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure StringGrid1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TimerRunQualifierTimer(Sender: TObject);
    procedure vstMonitorBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstMonitorChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure vstMonitorCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstMonitorDblClick(Sender: TObject);
    procedure vstMonitorDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstMonitorDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vstMonitorDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstMonitorDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstMonitorFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstMonitorGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure vstMonitorGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: System.UITypes.TImageIndex);
    procedure vstMonitorGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstMonitorInitNode(Sender: TBaseVirtualTree; Parentnode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstMonitorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vstMonitorMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure vstMonitorStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
    procedure btnQualifiersClick(Sender: TObject);
    procedure btnCandidatesProcessClick(Sender: TObject);
    procedure btnQuantitiesClick(Sender: TObject);
    procedure btnOrderTemplatesClick(Sender: TObject);
  private
    const
      C_KEY_SHOW_SUM_ALGOS     = 'ShowSumAlgos';
      C_KEY_SHOW_SUM_CONDITION = 'ShowSumCondition';
      C_KEY_USE_IB_FEEDS       = 'UseIBfeeds';
      C_SECTION_MONITOR        = 'Monitor';

      C_CIRCLE  = Char(9899);
      C_NETWORK = Char(9632);

      COL_ITEMS      = 0;
      COL_NODE_ID    = 1;
      COL_CALCTYPE   = 2;
      COL_CALCFACTOR = 3;
      COL_VALUE      = 4;
      COL_LAST_CLOSE = 5;
  private
    FArrayHiddenNodes: TNodeArray;
    FColumns: TDictionary<Integer, THeaderColInfo>;
    FConditionQueue: TDocumentQueue;
    FConnected: Boolean;
    FConnectedNN: Boolean;
    FControlHistData: TStringList;
    FCriticalSection: TRTLCriticalSection;
    FOrderGroupSet: TOrderGroupSetDoc;
    FQualifierList: TQualifierList;
    FRecieveDataIB: Boolean;
    FRecieveDataNN: Boolean;
    FStorageOrders: TStorageOrders;
    function ConnectToIBBroker: string;
    function ConnectToNNBroker: string;
    function DoAfterLoadEachDocumentProc(const aContractId: Integer; const aPrice: Currency): TAfterLoadEachDocumentProc;
    function DoAfterLoadTreeProc(const aParentNode: PVirtualNode): TAfterLoadTreeProc;
    function DoAutoTradesInstance(const aAutoTradeId, aQualifierId, aQualifierInstance: Integer): IAutoTrade;
    function ExecuteQualifier(const aRecordId: Integer): Integer;
    function GetCurrentOrderGroupSet: PVirtualNode;
    function GetLastPrice(const aId: Integer; const aTickType: TIABTickType = ttLast): Double; inline;
    function GetLevelName(aNode: PVirtualNode): string;
    function GetListOfChilds(aNode: PVirtualNode; aIsRecursive: Boolean; aDocType: TDocType): TList<PVirtualNode>;
    function GetNodeByName(aName: string): PVirtualNode;
    function GetNodeByRecordID(const aNode: PVirtualNode; aRecordID: Integer): PVirtualNode;
    function IsAutoorderGroup(aNode: PVirtualNode): Boolean;
    function IsNodePossibleToRemove(aNode: PVirtualNode): Boolean;
    function OpenGroupSet(const aGroupId: Integer): PVirtualNode;
    procedure CloseQualifier(const aInstanceNum: Integer);
    procedure ActivateAllOrders(aNode: PVirtualNode);
    procedure AddAlgosToConditionAlgosChart(aNode: PVirtualNode);
    procedure AddAlgosToRealtimeChart(aConditionNode, aAlgosNode: PVirtualNode);
    procedure AddConditionToRealtimeChart(aNode: PVirtualNode);
    procedure CheckAllOrders(aNode: PVirtualNode);
    procedure Connect;
    procedure CreateOrderNNStructure(const aParentNode: PVirtualNode; aTradedVolume: Integer);
    procedure CreateTradeChart(aNode: PVirtualNode);
    procedure DeleteAllNodes;
    procedure DeleteChildren(aNode: PVirtualNode; aUpdateAlgos: Boolean = True);
    procedure Disconnect;
    procedure FreeNodeData(aNode: PVirtualNode);
    procedure LoadParamsFromXml;
    procedure PrepareConditionValues(aNode: PVirtualNode);
    procedure ResetChildConditions(const aOrderNode: PVirtualNode);
    procedure SaveParamsToXml;
    procedure SetConnected(const Value: Boolean);
    procedure SetConnectedIB(const Value: Boolean);
    procedure SetConnectedNN(const Value: Boolean);
    procedure SetCurrentGroupCaptions;
    procedure SetHeaderColumns;
    procedure SetNetworkText;
    procedure SetRealTradesText;
    procedure SetRecieveDataIB(const Value: Boolean);
    procedure SetRecieveDataNN(const Value: Boolean);
    procedure ShowNotification(const Order: TIABOrder; const Status: TIABOrderState); overload;
    procedure ShowNotification(const aMessage, aTitle: string); overload;
    procedure SubscribeCurrency;
    procedure SubscribeIBFeed(const aNode: PVirtualNode);
    procedure SubscribeIBFeeds;
    procedure SubscribeNNFeed(const aNode: PVirtualNode);
    procedure SubscribeNNFeeds;
    procedure TestConditions(const aNode: PVirtualNode);
    procedure TestOrder(aNode: PVirtualNode);
    procedure TradeTest(const aNodeOrder: PVirtualNode; const aFillPrice: Double; const aQuantity: Integer);
    procedure TransformToBaseOrder(aNode: PVirtualNode);
    procedure TransformToOrderIB(aNode: PVirtualNode);
    procedure TransformToOrderNN(aNode: PVirtualNode);
    procedure UnsubscribeNNFeed(const aNode: PVirtualNode);
    procedure UpdateAlgos(const aNode: PVirtualNode);
    procedure UpdateCondition(const aNode: PVirtualNode);
    procedure UpdateHistoricalDataForConditionDoc(const aId: Integer);
    procedure UpdatePriceForTB(const PriceItem: TPriceItem);
    procedure OnDocumentQueueNotifyEvent(Sender: TObject; const Item: PVirtualNode; Action: TCollectionNotification);

    //IABClient events
    procedure OnAccountValue(Sender: TObject; Index: Integer);
    procedure OnConnectionState(Sender: TObject; State: TIABConnection);                                                            //implementation IConnectionState
    procedure OnCurrentTime(Sender: TObject; DateTime: TDateTime);
    procedure OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
    procedure OnHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData); //implementation IHistoricalData
    procedure OnInstrumentChildPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    procedure OnInstrumentPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    procedure OnManagedAccounts(Sender: TObject; Details: string);
    procedure OnPortfolioUpdate(Sender: TObject; Index: Integer);
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);             //implementation IUpdateFeeds
    procedure OnProfitLoss(Sender: TObject; DataId: Integer; DailyPnL, UnrealizedPnL, RealizedPnL: Double);
    procedure OnQueueOverflow(Sender: TObject; Count: Integer);

    //implementation IMotherFilledPriceChange
    procedure OnMotherFilledPriceChange(const MotherNode: PVirtualNode);

    //implementation IOrderState
    procedure OnCloseOrder(const aTempId: Integer);
    procedure OnExecution(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrder(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrderNN(const aOrderList: array of TOrder);
    procedure OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
    procedure OnRebuildFromTWS(Sender: TObject);

    //TNordNetBroker events
    procedure OnCloseFeedNN;
    procedure OnOpenFeedNN;
    procedure OnTradeOrder(const Sender: TNordNet; const TradeOrder: TTradeOrder);
    procedure OnUpdateOrder(const Sender: TNordNet; const OrderItem: TOrder);
    procedure OnUpdatePriceForNN(const PriceItem: TPriceItem);
    procedure OnUpdateTradeForNN(const TradeItem: TTradeItem);

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IMonitor
    function ExistsChildConditions(aNode: PVirtualNode): Boolean;
    function GetDockControl: TWinControl;
    function GetDuplicateAlgos(const aNode: PVirtualNode): PVirtualNode;
    function GetDuplicateCondition(const aNode: PVirtualNode): PVirtualNode;
    function GetDuplicateOrderIB(const aNode: PVirtualNode): PVirtualNode;
    function GetDuplicateOrderNN(const aNode: PVirtualNode): PVirtualNode;
    function GetFocusedNode: PVirtualNode;
    function GetIABClient: TIABSocket;
    function GetMainTree: TVirtualStringTree;
    function GetParentNode(aNode: PVirtualNode; aDocType: TDocType): PVirtualNode;
    function IsCreateOrModify(const aNode, aOrderGroupNode: PVirtualNode): Boolean;
    function UseIBFeeds: Boolean;
    procedure AddInstrumentFromSearch(aTargetNode: PVirtualNode; aSourceTree: TVirtualStringTree);
    procedure DeleteNode(aNode: PVirtualNode; aUpdateAlgos: Boolean = True);
    procedure OrdersExecuteOcaGroup(const aOcaGroupNumber: Integer);
    procedure OrdersExecuteOneCancelAll(const aNode: PVirtualNode);
    procedure SellContractPosition(const aSymbol, aCurrency: string; aQuantity, aInstrumentId: Integer; aSecType: TIABSecurityType; aAction: TIABAction);
    procedure SetChildsEnabled(const aNode: PVirtualNode; const aEnabled: Boolean);
    procedure SetChildsFreeze(const aNode: PVirtualNode);
    procedure SetIcon(Node: PVirtualNode);
    procedure ShowAlgosChart(aNode: PVirtualNode);
    procedure ShowConditionAlgosChart(aNode: PVirtualNode);
    procedure ShowConditionChart(aNode: PVirtualNode);
    procedure ShowTradeChart(aNode: PVirtualNode);
    procedure SubscribeChildNNFeed(aNode: PVirtualNode);
    procedure UnsubscribeChildNNFeed(aNode: PVirtualNode);

    function CreateTemplateStructure(const aOrderGroupId: Integer; aInstrumentData: PInstrumentData; const aAutoTradesCommon: TAutoTradesCommon): PVirtualNode;
    function GetCONIDforNN(ASymbol, AType, AExchange, ACurrency: string): Integer;
    function GetConnectedIB: Boolean;
    function UpdateChildAlgos(Node: PVirtualNode; WithUpdateCondition: Boolean = True): Boolean;
    procedure AddInstrumentFromScanner(aTargetNode: PVirtualNode; aSourceTree: TVirtualStringTree);
    procedure DeactivateChildConditions(aNode: PVirtualNode);
    procedure FillDataForChildNodes(const aNode: PVirtualNode; const aFillPrice: Double; const aQuantity: Integer);
    procedure FillPartlyDataForChildNodes(const aNode: PVirtualNode; const aFillPrice: Double; const aQuantity: Integer);
    procedure OrdersExecuteSequential(const aNode: PVirtualNode);
    procedure CancelSiblingsOrders(const aNode: PVirtualNode);
    procedure SaveToDBTreeElement(const aGroupName: string = '');
    procedure WriteHistoricalDataToConditionDoc(const aNode: PVirtualNode);

    property Connected     : Boolean read FConnected     write SetConnected;
    property ConnectedIB   : Boolean read GetConnectedIB write SetConnectedIB;
    property ConnectedNN   : Boolean read FConnectedNN   write SetConnectedNN;
    property RecieveDataIB : Boolean read FRecieveDataIB write SetRecieveDataIB;
    property RecieveDataNN : Boolean read FRecieveDataNN write SetRecieveDataNN;
  public
    procedure ActivateChildOrdersNN(const aNode: PVirtualNode);
    procedure CheckRepetitive(const aNode: PVirtualNode);
    procedure Initialize;
    procedure Deinitialize;
  end;

var
  frmMonitor : TfrmMonitor;

implementation

{$R *.dfm}

procedure TfrmMonitor.FormCreate(Sender: TObject);
begin
  inherited;
  Application.Title := TPath.GetFileNameWithoutExtension(Application.ExeName) + ' v.' + General.ModuleVersion + ', Time ';
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator:= ' ';
  Application.UpdateFormatSettings := True;

  vstMonitor.NodeDataSize := SizeOf(TTreeData);
  vstMonitor.CheckImageKind := ckCustom;
  vstMonitor.CustomCheckImages := DMImage.ilCustomCheckImages;

  FOrderGroupSet := TOrderGroupSetDoc.Create;

  FControlHistData := TStringList.Create;
  FControlHistData.Duplicates := dupIgnore;
  FControlHistData.Sorted := True;

  FColumns       := TDictionary<Integer, THeaderColInfo>.Create;
  FQualifierList := TQualifierList.Create;

  FConditionQueue := TDocumentQueue.Create;

  TMonitorLists.CreateLists;
  TMonitorLists.InstrumentList.DoPriceChange          := OnInstrumentPriceChange;
  TMonitorLists.InstrumentList.DocumentQueue.OnNotify := OnDocumentQueueNotifyEvent;
  TMonitorLists.InstrumentChildList.DoPriceChange     := OnInstrumentChildPriceChange;

  FConnected     := False;
  FConnectedNN   := False;
  FRecieveDataNN := False;
  FRecieveDataIB := False;

  InitializeCriticalSection(FCriticalSection);
  IABFunctions.IABClient := TIABClient.Create(nil);
  IABFunctions.IABClient.OnAccountValue    := OnAccountValue;
  IABFunctions.IABClient.OnCurrentTime     := OnCurrentTime;
  IABFunctions.IABClient.OnManagedAccounts := OnManagedAccounts;
  IABFunctions.IABClient.OnPortfolioUpdate := OnPortfolioUpdate;
  IABFunctions.IABClient.OnProfitLoss      := OnProfitLoss;
  IABFunctions.IABClient.OnQueueOverflow   := OnQueueOverflow;

  {$IFNDEF HIDE_OLD}
  pnlTopOld.Visible := true;
  if not Assigned(frmDockFormTotalController) then
  begin
    frmDockFormTotalController := TfrmDockFormTotalController.Create(Self);
    frmDockFormTotalController.DragKind := dkDock;
    frmDockFormTotalController.DragMode := dmAutomatic;
    frmDockFormTotalController.ManualDock(pgcMain, pgcMain, alClient);
    frmDockFormTotalController.frameQualifiers.QualifierList := FQualifierList;
  end;

  if not Assigned(frmDockFormTemplateCreator) then
  begin
    frmDockFormTemplateCreator := TfrmDockFormTemplateCreator.Create(Self);
    frmDockFormTemplateCreator.DragKind := dkDock;
    frmDockFormTemplateCreator.DragMode := dmAutomatic;
    frmDockFormTemplateCreator.ManualDock(pgcMain, pgcMain, alClient);
  end;

  if not Assigned(frmMonitorFilter) then
  begin
    frmMonitorFilter := TfrmMonitorFilter.Create(Self);
    frmMonitorFilter.DragKind := dkDock;
    frmMonitorFilter.DragMode := dmAutomatic;
    frmMonitorFilter.ManualDock(pgcMain, pgcMain, alClient);
  end;

  if not Assigned(frmDockFormPosition) then
  begin
    frmDockFormPosition := TfrmDockFormPosition.Create(Self);
    frmDockFormPosition.DragKind := dkDock;
    frmDockFormPosition.DragMode := dmAutomatic;
    frmDockFormPosition.ManualDock(pgcMain, pgcMain, alClient);
  end;

  if not Assigned(frmDockFormAccountInfo) then
  begin
    frmDockFormAccountInfo := TfrmDockFormAccountInfo.Create(Self);
    frmDockFormAccountInfo.DragKind := dkDock;
    frmDockFormAccountInfo.DragMode := dmAutomatic;
    frmDockFormAccountInfo.ManualDock(pgcMain, pgcMain, alClient);
  end;

  if not Assigned(frmDockFormAccountPnL) then
  begin
    frmDockFormAccountPnL := TfrmDockFormAccountPnL.Create(Self);
    frmDockFormAccountPnL.DragKind := dkDock;
    frmDockFormAccountPnL.DragMode := dmAutomatic;
    frmDockFormAccountPnL.ManualDock(pgcMain, pgcMain, alClient);
  end;

  if not Assigned(frmDockFormActiveOrders) then
  begin
    frmDockFormActiveOrders := TfrmDockFormActiveOrders.Create(Self);
    frmDockFormActiveOrders.DragKind := dkDock;
    frmDockFormActiveOrders.DragMode := dmAutomatic;
    frmDockFormActiveOrders.ManualDock(pgcMain, pgcMain, alClient);
  end;

  if not Assigned(frmDockFormOrderStatus) then
  begin
    frmDockFormOrderStatus := TfrmDockFormOrderStatus.Create(Self);
    frmDockFormOrderStatus.DragKind := dkDock;
    frmDockFormOrderStatus.DragMode := dmAutomatic;
    frmDockFormOrderStatus.ManualDock(pgcMain, pgcMain, alClient);
  end;

  if not Assigned(frmDockFormAutoTradesController) then
  begin
    frmDockFormAutoTradesController := TfrmDockFormAutoTradesController.Create(Self);
    frmDockFormAutoTradesController.DragKind := dkDock;
    frmDockFormAutoTradesController.DragMode := dmAutomatic;
    frmDockFormAutoTradesController.ManualDock(pgcMain, pgcMain, alClient);
  end;

  if not Assigned(frmDockFormQualifiersController) then
  begin
    frmDockFormQualifiersController := TfrmDockFormQualifiersController.Create(Self);
    frmDockFormQualifiersController.DragKind := dkDock;
    frmDockFormQualifiersController.DragMode := dmAutomatic;
    frmDockFormQualifiersController.ManualDock(pgcMain, pgcMain, alClient);
    frmDockFormQualifiersController.frameQualifiers.QualifierList := FQualifierList;
  end;

  if not Assigned(frmDockFormComputeOrders) then
  begin
    frmDockFormComputeOrders := TfrmDockFormComputeOrders.Create(Self);
    frmDockFormComputeOrders.DragKind := dkDock;
    frmDockFormComputeOrders.DragMode := dmAutomatic;
    frmDockFormComputeOrders.ManualDock(pgcMain, pgcMain, alClient);
  end;

  if not Assigned(frmLogView) then
  begin
    frmLogView := TfrmLogView.Create(Self);
    frmLogView.DragKind := dkDock;
    frmLogView.DragMode := dmAutomatic;
    frmLogView.ManualDock(pgcMain, pgcMain, alClient);
  end;
  {$ENDIF}
end;

procedure TfrmMonitor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TimerTimeGap.Enabled := False;
  Disconnect;
  SaveParamsToXml;
  Deinitialize;
  General.Deinitialize;
  DMod.ConnectionStock.Close;
  DMod.ConnectionFeed.Close;
  FQualifierList.Clear;
  if HTTPServer.Active then
    HTTPServer.Active := False;
end;

procedure TfrmMonitor.FormDestroy(Sender: TObject);
begin
  if Assigned(FStorageOrders) then
    FreeAndNil(FStorageOrders);
  TMonitorLists.DestroyLists;
  FreeAndNil(FConditionQueue);
  FreeAndNil(FQualifierList);
  DeleteCriticalSection(FCriticalSection);
  FreeAndNil(IABFunctions.IABClient);
  FreeAndNil(FControlHistData);
  FreeAndNil(FColumns);
  FreeAndNil(FOrderGroupSet);
  inherited;
end;

procedure TfrmMonitor.Initialize;
var
  i: Integer;
begin
  TPublishers.ConnectionStatePublisher.Subscribe(Self);
  TPublishers.ErrorPublisher.Subscribe(Self);
  TPublishers.FeedPublisher.Subscribe(FQualifierList);
  TPublishers.FeedPublisher.Subscribe(Self);
  TPublishers.FeedPublisher.Subscribe(TMonitorLists.InstrumentChildList);
  TPublishers.FeedPublisher.Subscribe(TMonitorLists.InstrumentList);
  TPublishers.HistoricalDataPublisher.Subscribe(Self);
  TPublishers.MotherFilledPriceChangePublisher.Subscribe(Self);
  TPublishers.InstrumentSpecDetailsPublisher.Subscribe(SokidList);
  TPublishers.LogPublisher.Subscribe(frameActivityLog);
  TPublishers.OrderStatePublisher.Subscribe(Self);
  TPublishers.SecurityDefinitionOptionalParameterPublisher.Subscribe(SokidList);
  TPublishers.TickByTickPublisher.Subscribe(SokidList);
  IABClient.MarketRuleList.LoadFromDB;
  FStorageOrders := TStorageOrders.Create;

  General.ChartStartTime := Date + StrToTime('09:00:00');

  TimerTimeGap.Enabled := True;
  btnLogin.ImageIndex := 53;
  Caption := 'RobotX v.' + General.ModuleVersion + ', Time ' + FormatDateTime('hh:nn:ss', Now);

  Self.LoadParamsFromXml;
  SetHeaderColumns;
  SetRealTradesText;
  TStoreHelper.LoadFromXml(vstMonitor, C_SECTION_MONITOR + '.Columns');
  if DMod.ConnectionStock.Connected then
  begin
    DMod.fbqOrderGr.Active := True;
    DMod.fbqAutoTrades.Active := True;
    DMod.fbqAutoTrades.Last;
    DMod.fbqQualifiers.Active := True;
    DMod.fbqQualifiers.Last;
    DMod.fbtAccounts.Active := True;
    SokidList.LoadFromDB;
  end;

  for i := 0 to ActionListMain.ActionCount - 1 do
    ActionListMain.Actions[i].Tag := ActionListMain.Actions[i].ShortCut;

  aUseNN.Checked := General.XMLFile.ReadBool(TGeneral.C_SECTION_COMMON_PARAMS, TGeneral.C_KEY_USE_NN, True);
  aUseIB.Checked := General.XMLFile.ReadBool(TGeneral.C_SECTION_COMMON_PARAMS, TGeneral.C_KEY_USE_IB, True);

  TMonitorTree.Initialize(vstMonitor);
  {$IFNDEF HIDE_OLD}
  frameActivityLog.Initialize;
  frmDockFormAccountInfo.Initialize;
  frmDockFormAccountPnL.Initialize;
  frmDockFormActiveOrders.Initialize;
  frmDockFormAutoTradesController.Initialize;
  frmDockFormComputeOrders.Initialize;
  frmDockFormOrderStatus.Initialize;
  frmDockFormPosition.Initialize;
  frmDockFormQualifiersController.Initialize;
  frmDockFormTemplateCreator.Initialize;
  frmDockFormTotalController.Initialize;
  frmLogView.Initialize;
  frmMonitorFilter.Initialize;
  {$ENDIF}
end;

procedure TfrmMonitor.Deinitialize;
begin
  TPublishers.ConnectionStatePublisher.Unsubscribe(Self);
  TPublishers.ErrorPublisher.Unsubscribe(Self);
  TPublishers.FeedPublisher.Unsubscribe(FQualifierList);
  TPublishers.FeedPublisher.Unsubscribe(Self);
  TPublishers.FeedPublisher.Unsubscribe(TMonitorLists.InstrumentChildList);
  TPublishers.FeedPublisher.Unsubscribe(TMonitorLists.InstrumentList);
  TPublishers.HistoricalDataPublisher.Unsubscribe(Self);
  TPublishers.MotherFilledPriceChangePublisher.Unsubscribe(Self);
  TPublishers.InstrumentSpecDetailsPublisher.Unsubscribe(SokidList);
  TPublishers.LogPublisher.Unsubscribe(frameActivityLog);
  TPublishers.OrderStatePublisher.Unsubscribe(Self);
  TPublishers.SecurityDefinitionOptionalParameterPublisher.Unsubscribe(SokidList);
  TPublishers.TickByTickPublisher.Unsubscribe(SokidList);

  {$IFNDEF HIDE_OLD}
  frameActivityLog.Deinitialize;
  frmDockFormAccountInfo.Deinitialize;
  frmDockFormAccountPnL.Deinitialize;
  frmDockFormActiveOrders.Deinitialize;
  frmDockFormAutoTradesController.Deinitialize;
  frmDockFormComputeOrders.Deinitialize;
  frmDockFormOrderStatus.Deinitialize;
  frmDockFormPosition.Deinitialize;
  frmDockFormQualifiersController.Deinitialize;
  frmDockFormTemplateCreator.Deinitialize;
  frmDockFormTotalController.Deinitialize;
  frmLogView.Deinitialize;
  frmMonitorFilter.Deinitialize;
  {$ENDIF}
end;

procedure TfrmMonitor.FormShow(Sender: TObject);
begin
  {$IFNDEF HIDE_OLD}
  frmDockFormAccountInfo.Show;
  frmDockFormAccountPnL.Show;
  frmDockFormActiveOrders.Show;
  frmDockFormAutoTradesController.Show;
  frmDockFormComputeOrders.Show;
  frmDockFormOrderStatus.Show;
  frmDockFormPosition.Show;
  frmDockFormQualifiersController.Show;
  frmDockFormTemplateCreator.Show;
  frmDockFormTotalController.Show;
  frmLogView.Show;
  frmMonitorFilter.Show;
  {$ENDIF}
end;

function TfrmMonitor.GetIABClient: TIABSocket;
begin
  Result := IABFunctions.IABClient;
end;

function TfrmMonitor.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmMonitor.SetHeaderColumns;
var
  i: Integer;
  Column: string;
  ColInfo: THeaderColInfo;
  ShowFreezedColumns: Boolean;
begin
  FColumns.Clear;
  for i := 0 to C_MONITOR_TREE_FIX_COLUMN - 1 do
  begin
    ColInfo.Caption  := vstMonitor.Header.Columns[i].Text;
    ColInfo.IsFixed  := True;
    ColInfo.IsFreeze := False;
    ColInfo.TickType := ttNotSet;
    FColumns.Add(i, ColInfo);
  end;

  while (C_MONITOR_TREE_FIX_COLUMN < vstMonitor.Header.Columns.Count) do
    vstMonitor.Header.Columns.Delete(C_MONITOR_TREE_FIX_COLUMN);

  if General.XmlFile.ReadAttributes(General.XmlFile.GetXPath(TGeneral.C_SECTION_COLUMN_SETTINGS)) then
    ShowFreezedColumns := General.XmlFile.Attributes.GetAttributeValue('ShowFreezedColumns', True)
  else
    ShowFreezedColumns := True;

  General.XmlFile.CurrentSection := TGeneral.C_SECTION_COLUMN_SETTINGS;
  while not General.XmlFile.IsLastKey do
  begin
    if General.XmlFile.ReadAttributes then
    begin
      Column := General.XmlFile.Attributes.GetAttributeValue('Column', '');
      if not Column.IsEmpty then
      begin
        ColInfo.IsFixed  := False;
        ColInfo.IsFreeze := False;
        ColInfo.TickType := IABClient.GetTickType(Column);
        ColInfo.Caption  := ColInfo.TickType.ToString;
        with vstMonitor.Header.Columns.Add do
        begin
          Text             := ColInfo.Caption;
          CaptionAlignment := taCenter;
          Alignment        := taRightJustify;
          Options          := Options - [coEditable, coParentColor];
        end;
        FColumns.Add(vstMonitor.Header.Columns.Count - 1, ColInfo);

        if ShowFreezedColumns then
        begin
          ColInfo.IsFixed  := False;
          ColInfo.IsFreeze := True;
          ColInfo.TickType := IABClient.GetTickType(Column);
          ColInfo.Caption  := ColInfo.TickType.ToString + ' F';
          if not (ColInfo.TickType in [ttExchange, ttMarket]) then
          begin
            with vstMonitor.Header.Columns.Add do
            begin
              Text := ColInfo.Caption;
              CaptionAlignment := taCenter;
              Options := Options - [coEditable, coParentColor];
            end;
            FColumns.Add(vstMonitor.Header.Columns.Count - 1, ColInfo);
          end;
        end;
      end;
    end;
    General.XmlFile.NextKey;
  end;
  General.XmlFile.CurrentSection := '';
end;

procedure TfrmMonitor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  function GetControl(AParent: TWinControl): TWinControl;
  var
    i: Integer;
    List: TList;
  begin
    Result := nil;
    List := TList.Create;
    try
      AParent.GetTabOrderList(List);
      for i := 0 to List.Count - 1 do
      begin
        if (TWinControl(List[i]).ControlCount = 0) then
          Result := TWinControl(List[i])
        else
          Result := GetControl(TWinControl(List[i]));
        Break;
      end;
    finally
      FreeAndNil(List);
    end;
  end;

var
  Control: TWinControl;
begin
  if pgcMain.Focused and (Key = VK_DOWN) then
  begin
    Control := GetControl(pgcMain.ActivePage);
    SetFocusSafely(Control);
  end;
end;

procedure TfrmMonitor.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if cbQualifiers.Focused then
    SendMessage(cbQualifiers.Handle, WM_KEYDOWN, VK_DOWN, 0)
  else if cbAutoTrades.Focused then
    SendMessage(cbAutoTrades.Handle, WM_KEYDOWN, VK_DOWN, 0);
end;

procedure TfrmMonitor.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if cbQualifiers.Focused then
    SendMessage(cbQualifiers.Handle, WM_KEYDOWN, VK_UP, 0)
  else if cbAutoTrades.Focused then
    SendMessage(cbAutoTrades.Handle, WM_KEYDOWN, VK_UP, 0);
end;

procedure TfrmMonitor.SaveParamsToXml;
begin
  try
    TStoreHelper.SaveToXml(vstMonitor, C_SECTION_MONITOR + '.Columns');
    General.XmlFile.WriteBool(C_SECTION_MONITOR, C_KEY_SHOW_SUM_ALGOS, aShowSumAlgos.Checked);
    General.XmlFile.WriteBool(C_SECTION_MONITOR, C_KEY_SHOW_SUM_CONDITION, aShowSumCondition.Checked);
    General.XmlFile.WriteBool(C_SECTION_MONITOR, C_KEY_USE_IB_FEEDS, aUseIBfeeds.Checked);
  finally
    General.XmlFile.Save;
  end;
end;

procedure TfrmMonitor.LoadParamsFromXml;
begin
  aShowSumAlgos.Checked     := General.XmlFile.ReadBool(C_SECTION_MONITOR, C_KEY_SHOW_SUM_ALGOS, False);
  aShowSumCondition.Checked := General.XmlFile.ReadBool(C_SECTION_MONITOR, C_KEY_SHOW_SUM_CONDITION, False);
  aUseIBfeeds.Checked       := General.XmlFile.ReadBool(C_SECTION_MONITOR, C_KEY_USE_IB_FEEDS, False);
end;

procedure TfrmMonitor.OnDocumentQueueNotifyEvent(Sender: TObject; const Item: PVirtualNode; Action: TCollectionNotification);
var
  Rect: TRect;
begin
  if not Application.Terminated then
  begin
    Rect.Top := 2;
    Rect.Left := 350;
    Rect.Right := Rect.Left + sbMain.Panels[5].Width - 1;
    Rect.Height := sbMain.Height - 1;
    TThread.Queue(nil,
      procedure()
      begin
        sbMainDrawPanel(sbMain, sbMain.Panels[5], Rect);
      end);
  end;
end;

procedure TfrmMonitor.SetRealTradesText;
var
  Rect: TRect;
begin
  if not Application.Terminated then
  begin
    Rect.Top := 2;
    Rect.Left := 390;
    Rect.Right := Rect.Left + sbMain.Panels[7].Width - 1;
    Rect.Height := sbMain.Height - 1;
    TThread.Queue(nil,
      procedure()
      begin
        sbMainDrawPanel(sbMain, sbMain.Panels[7], Rect);
      end);
  end;
end;

procedure TfrmMonitor.SetNetworkText;
var
  Rect: TRect;
begin
  if not Application.Terminated then
  begin
    Rect.Top := 2;
    Rect.Left := 370;
    Rect.Right := Rect.Left + sbMain.Panels[6].Width - 1;
    Rect.Height := sbMain.Height - 1;
    TThread.Queue(nil,
      procedure()
      begin
        sbMainDrawPanel(sbMain, sbMain.Panels[6], Rect);
      end);
  end;
end;

procedure TfrmMonitor.sbMainClick(Sender: TObject);

  function GetPanelIndex: Integer;
  var
    Pt: TPoint;
    i, w: Integer;
  begin
    Result := -1;
    Pt := SmallPointToPoint(TSmallPoint(DWORD(GetMessagePos)));
    MapWindowPoints(HWND_DESKTOP, sbMain.Handle, Pt, 1);
    w := 0;
    for i := 0 to sbMain.Panels.Count - 1 do
    begin
      w := w + sbMain.Panels[i].Width;
      if (Pt.X < w) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
var
  PanelIndex: Integer;
begin
  PanelIndex := GetPanelIndex;
  if (PanelIndex = 5) then
  begin
    aShowInstrumentsWithoutFeedInfo.Execute;
  end
  else if (PanelIndex = 7) or (PanelIndex = 8) then
  begin

  end;
end;

procedure TfrmMonitor.sbMainDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
resourcestring
 rsIBConnected = 'IB Connected';
 rsIBDisabled  = 'IB Disabled';
 rsOnLine      = 'OnLine';
 rsOffLine     = 'OffLine ' + C_NETWORK;
 rsNNConnected = 'NN Connected';
 rsNNDisabled  = 'NN Disabled';
 rsPaperTrade  = 'Paper Trade';
 rsRealTrade   = 'Real Trade ';
 rsRecieveData = 'Recieve data';

var
  RectForText: TRect;
  IsUseIB: Boolean;
  IsUseNN: Boolean;
begin
  IsUseIB := General.XMLFile.ReadBool(TGeneral.C_SECTION_COMMON_PARAMS, TGeneral.C_KEY_USE_IB, True);
  IsUseNN := General.XMLFile.ReadBool(TGeneral.C_SECTION_COMMON_PARAMS, TGeneral.C_KEY_USE_NN, True);
  if (Panel = StatusBar.Panels[0]) then       //IB Connected
  begin
    if not IsUseIB then
    begin
      Panel.Text := rsIBDisabled;
      StatusBar.Canvas.Font.Color := clBlack;
    end
    else
    begin
      Panel.Text := rsIBConnected;
      if (IABClient.State = twsReady) then
        StatusBar.Canvas.Font.Color := clGreen
      else
        StatusBar.Canvas.Font.Color := clRed;
    end;
    RectForText := Rect;
    StatusBar.Canvas.FillRect(RectForText);
    DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), -1, RectForText, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
  end
  else if (Panel = StatusBar.Panels[1]) then  //Recieve data IB
  begin
    if not IsUseIB then
    begin
      Panel.Text := '';
      StatusBar.Canvas.Font.Color := clBlack;
    end
    else
    begin
      Panel.Text := rsRecieveData;
      if RecieveDataIB then
        StatusBar.Canvas.Font.Color := clGreen
      else
        StatusBar.Canvas.Font.Color := clRed;
    end;

    RectForText := Rect;
    StatusBar.Canvas.FillRect(RectForText);
    DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), -1, RectForText, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
  end
  else if (Panel = StatusBar.Panels[3]) then  //NN Connected
  begin
    if not IsUseNN then
    begin
      Panel.Text := rsNNDisabled;
      StatusBar.Canvas.Font.Color := clBlack;
    end
    else
    begin
      Panel.Text := rsNNConnected;
      if ConnectedNN then
        StatusBar.Canvas.Font.Color := clGreen
      else
        StatusBar.Canvas.Font.Color := clRed;
    end;
    RectForText := Rect;
    StatusBar.Canvas.FillRect(RectForText);
    DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), -1, RectForText, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
  end
  else if (Panel = StatusBar.Panels[4]) then  //Recieve data NN
  begin
    if not IsUseNN then
    begin
      Panel.Text := '';
      StatusBar.Canvas.Font.Color := clBlack;
    end
    else
    begin
      Panel.Text := rsRecieveData;
      if RecieveDataNN then
        StatusBar.Canvas.Font.Color := clGreen
      else
        StatusBar.Canvas.Font.Color := clRed;
    end;
    RectForText := Rect;
    StatusBar.Canvas.FillRect(RectForText);
    DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), -1, RectForText, DT_SINGLELINE or DT_VCENTER);
  end
  else if (Panel = StatusBar.Panels[5]) then  //Nodes Without Feed
  begin
    if not IsUseIB then
    begin
      Panel.Text := '';
    end
    else
    begin
      if (TMonitorLists.InstrumentList.GetCountNodesWithoutFeed = 0) then
        Panel.Text := ''
      else
      begin
        Panel.Text := C_CIRCLE;
        StatusBar.Canvas.Font.Color := clRed;
      end;
    end;
    RectForText := Rect;
    StatusBar.Canvas.FillRect(RectForText);
    DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), -1, RectForText, DT_VCENTER or DT_CENTER);
  end
  else if (Panel = StatusBar.Panels[6]) then  //Internet Connected
  begin
    if not IsInternetConnected then
    begin
      Panel.Text := rsOffLine;
      StatusBar.Canvas.Font.Color := clRed;
    end
    else
    begin
      Panel.Text := rsOnLine;
      StatusBar.Canvas.Font.Color := clGreen;
    end;
    RectForText := Rect;
    StatusBar.Canvas.FillRect(RectForText);
    DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), -1, RectForText, DT_SINGLELINE or DT_VCENTER or DT_CENTER);
  end
  else if (Panel = StatusBar.Panels[7]) then
  begin
    if General.IsRealTrades then
    begin
      if (cbAccounts.ItemIndex > -1) then
        Panel.Text := rsRealTrade + cbAccounts.Items[cbAccounts.ItemIndex]
      else
        Panel.Text := rsRealTrade;
      StatusBar.Canvas.Font.Color := clRed;
    end
    else
    begin
      Panel.Text := rsPaperTrade;
      StatusBar.Canvas.Font.Color := clBlack;
    end;
    RectForText := Rect;
    StatusBar.Canvas.FillRect(RectForText);
    DrawText(StatusBar.Canvas.Handle, PChar(Panel.Text), -1, RectForText, DT_SINGLELINE or DT_VCENTER or DT_CENTER);
  end;
end;

procedure TfrmMonitor.btnCandidatesProcessClick(Sender: TObject);
begin
  inherited;
  ShowList(ntCandidates);
end;

procedure TfrmMonitor.btnOrderTemplatesClick(Sender: TObject);
begin
  inherited;
  ShowList(ntOrderTemplate);
end;

procedure TfrmMonitor.btnQualifiersClick(Sender: TObject);
begin
  inherited;
  ShowList(ntQualifier);
end;

procedure TfrmMonitor.btnQuantitiesClick(Sender: TObject);
begin
  inherited;
  ShowList(ntQuantities);
end;

procedure TfrmMonitor.btnStartHTTPServerClick(Sender: TObject);
var
  IPAddress: string;
  DomainName: string;
begin
  if HTTPServer.Active then
  begin
    HTTPServer.Active := False;
    HTTPServer.Bindings.Clear;
    lblIPAddress.Caption := '';
    btnStartHTTPServer.Caption := 'Start HTTP Server';
  end
  else
  begin
    TLocalInformationDialog.GetLocalIPAddressName(IPAddress, DomainName);
    HTTPServer.Bindings.Add.SetBinding(IPAddress, 8080, Id_IPv4);
    HTTPServer.Bindings.Add.SetBinding('127.0.0.1', 8080, Id_IPv4);
    lblIPAddress.Caption := IPAddress + ':8080' + sLineBreak + '127.0.0.1:8080';
    ShellExecute(Handle, 'open', PChar('http://127.0.0.1:8080'), nil, nil, SW_SHOWNORMAL);
    HTTPServer.DefaultPort      := 8080;
    HTTPServer.MaxConnections   := 1000;
    HTTPServer.AutoStartSession := True;
    HTTPServer.KeepAlive        := False;
    HTTPServer.Active           := True;
    btnStartHTTPServer.Caption  := 'Stop HTTP Server';
  end;
end;

procedure TfrmMonitor.HTTPServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Currency: string;
  sb: TStringBuilder;
  IsMassage: Boolean;
  IsQualifier: Boolean;
begin
  AContext.Connection.IOHandler.DefStringEncoding := IndyTextEncoding_ASCII;
  AResponseInfo.Server       := TGeneral.GetAppVersion;
  AResponseInfo.CacheControl := 'no-cache';
  AResponseInfo.CharSet      := 'windows-1251';
  AResponseInfo.ContentType := 'text/html';
  AResponseInfo.ResponseNo  := 200;
  IsMassage := ARequestInfo.Document = '/message';
  IsQualifier := ARequestInfo.Document = '/qualifier';

  sb := TStringBuilder.Create;
  try
    sb.Append(C_HTML_HEAD_OPEN).Append(C_STYLE).Append(C_HTML_HEAD_CLOSE).AppendLine
      .Append(DateTimeToStr(Now)).Append(C_HTML_BREAK).AppendLine
      .Append(THtmlLib.GetCenterText('Exchange Rate InformationDialog')).Append(C_HTML_BREAK).AppendLine
      .Append(THtmlLib.GetTableTag(VarArrayOf(['Currency', 'Exchange']))).AppendLine;

    for Currency in TMonitorLists.CurrencyCache.Keys do
      sb.Append(THtmlLib.GetTableLineTag(VarArrayOf([Currency, TMonitorLists.CurrencyCache.GetLastExchange(Currency)]))).AppendLine;
    sb.Append(C_HTML_TABLE_CLOSE).Append(C_HTML_BREAK).AppendLine
      .Append('<form><button formaction="message" formmethod="post">Send message to RobotX</button>')
      .Append('      <button formaction="qualifier" formmethod="post">Execute current qualifier</button></form>')
      .Append(C_HTML_BODY_CLOSE).AppendLine;
    AResponseInfo.ContentText := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
  AResponseInfo.WriteContent;
  if IsMassage then
    TMessageDialog.ShowInfo('Received a message from Browser')
  else if IsQualifier then
    TThread.Queue(nil,
      procedure
      begin
        if (DMod.fbtAccounts.FieldByName('QUALIFIERID').AsInteger > 0) then
          aQualifiersExecuteExecute(nil)
        else
          TMessageDialog.ShowWarning('Qualifier not selected')
      end);
end;

procedure TfrmMonitor.btnTWSTimeClick(Sender: TObject);
begin
  IABClient.SendRequest(ibGetCurrentTWSTime, 0);
end;

function TfrmMonitor.DoAfterLoadTreeProc(const aParentNode: PVirtualNode): TAfterLoadTreeProc;
begin
  Result := procedure(const aParentNode: PVirtualNode)
    var
      arrNodes: TNodeArray;
      Data: PTreeData;
      CompiledValue: TCompiledValue;
    begin
      if Assigned(aParentNode) then
      begin
        //Test Conditions
        arrNodes := TTreeDocument.GetNodesListByType(vstMonitor, aParentNode, ntCondition);
        for var Node in arrNodes do
        begin
          Data := Node^.GetData;
          if (Data^.DocType = ntCondition) then
          begin
            case Data^.ConditionDoc.CondType of
              ctRealtimeValue, ctRealtimeAndTimeGap:
                begin
                  CompiledValue := TTreeDocument.CalcCompiledValue(vstMonitor, Data^.ConditionDoc);
                  Data^.ConditionDoc.IsValueReady := CompiledValue.IsValueReady;
                  if CompiledValue.IsValueReady then
                  begin
                    Data^.Enabled := True;
                    Data^.ConditionDoc.CompiledValue := CompiledValue;
                    Data^.ConditionDoc.CalcValue := CompiledValue.TotalValue / Data^.ConditionDoc.DivisionValue;
                    if (Data^.ConditionDoc.InitValue = 0) then
                      Data^.ConditionDoc.InitValue := Data^.ConditionDoc.CalcValue;
                    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'DoAfterLoadTreeProc',
                                                                                'NodeId=' + Data^.NodeId.ToString +
                                                                                ', Factors=' + CompiledValue.Factors +
                                                                                'IsValueReady=' + BoolToStr(CompiledValue.IsValueReady, True) +
                                                                                ', CondLimit=' + CurrToStr(Data^.ConditionDoc.CondLimit) +
                                                                                ', CompiledValue=' + CurrToStr(CompiledValue.TotalValue) +
                                                                                ', DivisionValue=' + CurrToStr(Data^.ConditionDoc.DivisionValue) +
                                                                                ', CalcValue=' + CurrToStr(Data^.ConditionDoc.CondLimit) +
                                                                                ', Sum(' + Data^.ConditionDoc.TickType1.ToString + ')=' + CurrToStr(CompiledValue.SummValue1) +
                                                                                        ' [' + Data^.ConditionDoc.TypeOperation.ToString + '] ' +
                                                                                ', Sum(' + Data^.ConditionDoc.TickType2.ToString + ')=' + CurrToStr(CompiledValue.SummValue2) +
                                                                                ', CalcValue=' + CurrToStr(Data^.ConditionDoc.CalcValue) +
                                                                                ', Enabled=' + BoolToStr(Data^.Enabled, True) +
                                                                                ', Description="' + Data^.ConditionDoc.Description + '"');

                    UpdateChildAlgos(Node, False);
                  end
                  else
                  begin
                    FConditionQueue.AddItem(Node);
                    Data^.Enabled := False;
                    Data^.ConditionDoc.Active := False;
                    TPublishers.LogPublisher.Write([ltLogWriter], ddWarning, Self,
                                                                  'DoAfterLoadTreeProc',
                                                                  'NodeId=' + Data^.NodeID.ToString + sLineBreak +
                                                                  'Price is null, the condition "' + Data^.ConditionDoc.Description + '" is queued');
                  end
                end;
            end;
            AddConditionToRealtimeChart(Node);
            WriteHistoricalDataToConditionDoc(Node);
            UpdateCondition(Node);
          end;
        end;

        //Test Orders
        arrNodes := TTreeDocument.GetNodesListByType(vstMonitor, aParentNode, ntOrder);
        for var Node in arrNodes do
        begin
          TestOrder(Node);
        end;
      end;
    end;
end;

function TfrmMonitor.DoAfterLoadEachDocumentProc(const aContractId: Integer; const aPrice: Currency): TAfterLoadEachDocumentProc;
begin
  Result := procedure(const aNode: PVirtualNode)
    var
      Data: PTreeData;
      SokidInfo: TSokidInfo;
      Order: TIABOrder;
      ExtendedOptions: TCustomOrderDoc.TExtendedOptions;
    begin
      if Assigned(aNode) then
      begin
        Data := aNode^.GetData;
        Data^.CreationType := ctProgramm;
        case Data^.DocType of
          TDocType.ntQualifier:
            begin

            end;
          TDocType.ntAutoTrade:
            begin

            end;
          TDocType.ntOrderGroupSet:
            begin

            end;
          TDocType.ntOrderGroup:
            begin

            end;
          TDocType.ntOrder:
            begin
              if (Data^.OrderDoc.BrokerType = TBrokerType.brIB) then
              begin
                if (aContractId > 0) then
                begin
                  SokidInfo := SokidList.GetItem(aContractId);
                  Data^.OrderDoc.AssignFrom(SokidInfo);
                  with TOrderIBDoc(Data^.OrderDoc) do
                  begin
                    if MarketList.IsEmpty then
                    begin
                      Order := TIABOrder.Create;
                      try
                        IABClient.ClearOrder(Order);
                        Order.ContractId      := aContractId;
                        Order.SecurityType    := SecurityType;
                        Order.Currency        := Currency;
                        Order.Exchange        := Exchange;
                        Order.PrimaryExchange := PrimaryExchange;
                        Order.Multiplier      := '';
                        Order.Symbol          := Symbol;
                        IABClient.SendRequest(ibGetInstrumentSpecs, 1, Order);
                      finally
                        FreeAndNil(Order);
                      end;
                    end;
                  end;
                end;
              end
              else if (Data^.OrderDoc.BrokerType = TBrokerType.brNN) then
              begin
                with TOrderNNDoc(Data^.OrderDoc) do
                  if (ChildId > 0) then
                  begin
                    TMonitorLists.InstrumentChildList.AddNode(ChildId, aNode);
                    SetValueArrayFromChildFeed(ttLast, GetLastPrice(ChildId));
                  end;
              end;
              Data^.OrderDoc.LastPrice := GetLastPrice(aContractId);
              ExtendedOptions := Data^.OrderDoc.ExtendedOptions;
              ExtendedOptions.Subordination := TTreeDocument.GetOrderSubordination(vstMonitor, aNode);
              Data^.OrderDoc.ExtendedOptions := ExtendedOptions;
              Data^.Enabled := Data^.OrderDoc.ExtendedOptions.Subordination in [suMotherOrder, suUnknow];

              TMonitorLists.InstrumentList.AddNode(aContractId, aNode);
              if (Data^.OrderDoc.LastPrice = 0) then
                TIABMarket.RequestMarketData(aContractId)  //Allows to get the latest price if it is not yet available
              else
                TMonitorLists.InstrumentList.DocumentQueue.DeleteItem(aNode);

              TMonitorLists.OrderList.AddOrder(aNode);
            end;
          TDocType.ntCondition:
            begin
              case Data^.ConditionDoc.CondType of
                ctTimeGap, ctRealtimeAndTimeGap:
                  begin
//                    Data^.ConditionDoc.StartDate := Date;
//                    Data^.ConditionDoc.EndDate   := Date;
                  end;
                ctRealtimeValue:
                  begin
                    //implemented in DoAfterLoadTreeProc
                  end;
              end;
            end;
          TDocType.ntAlgos:
            begin
              TMonitorLists.AlgosList.AddItem(aNode);
              AddAlgosToConditionAlgosChart(aNode);
              AddAlgosToRealtimeChart(nil, aNode);
            end;
          TDocType.ntFactor:
            begin
              if (Data^.FactorDoc.BrokerType = TBrokerType.brNN) then
              begin
                if miUseIBfeeds.Checked then
                begin
                  Data^.FactorDoc.UseIBFeeds := True;
                  var ContractId := GetCONIDforNN(Data^.FactorDoc.Symbol, Data^.FactorDoc.ContractType, Data^.FactorDoc.Exchange, Data^.FactorDoc.Currency);
                  Data^.FactorDoc.ContractId := ContractId;
                  Data^.FactorDoc.IBID       := ContractId;
                end
                else
                  SubscribeNNFeed(aNode);
              end
              else if (Data^.FactorDoc.BrokerType = TBrokerType.brIB) then
                TIABMarket.RequestMarketData(aContractId);
              TMonitorLists.InstrumentList.AddNode(aContractId, aNode);
              Data^.FactorDoc.CurrentValue := GetLastPrice(Data^.FactorDoc.ContractId, Data^.FactorDoc.TickType1);
              if (Data^.FactorDoc.CurrentValue > 0) then
                TMonitorLists.InstrumentList.DocumentQueue.DeleteItem(aNode);
//              UpdateCondition(TTreeDocument.GetParentNode(vstMonitor, aNode, ntCondition));
            end;
        end;
        Self.SetIcon(aNode);
      end;
    end;
end;

function TfrmMonitor.CreateTemplateStructure(const aOrderGroupId: Integer; aInstrumentData: PInstrumentData; const aAutoTradesCommon: TAutoTradesCommon): PVirtualNode;
var
  AfterLoadProc: TAfterLoadEachDocumentProc;
  AfterLoadTreeProc: TAfterLoadTreeProc;
  ContractId: Integer;
  Price: Currency;
  arr: TNodeArray;
  ResultData: PTreeData;
begin
  ContractId := aInstrumentData.Id;
  TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'CreateTemplateStructure', 'Begin Create a structure from a template');
  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, 'CreateTemplateStructure');

  Price := GetLastPrice(ContractId);
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'CreateTemplateStructure', 'Price[ttLast]=' + FloatToStr(Price));
  if (Price = 0) then
    TIABMarket.RequestMarketData(ContractId);  //Allows to get the latest price if it is not yet available

  vstMonitor.BeginUpdate;
  try
    Result := TTreeDocument.LoadRelationTree(aOrderGroupId, -1, vstMonitor, GetCurrentOrderGroupSet, nil, nil);
    if Assigned(Result) and Assigned(FOrderGroupSet.OwnerNode) then
    begin
      AfterLoadProc := DoAfterLoadEachDocumentProc(ContractId, Price);
      AfterLoadTreeProc := DoAfterLoadTreeProc(Result);
      TTreeFactory.FillDocuments(Result, aInstrumentData^, 0, aAutoTradesCommon, AfterLoadProc, AfterLoadTreeProc);
      aInstrumentData^.IsLocked := True;
      ResultData := Result^.GetData;
      if (ResultData.DocType = ntOrderGroupSet) then
      begin
        arr := TTreeDocument.GetNodesList(Result, False);
        for var Node in arr do
          vstMonitor.MoveTo(Node, FOrderGroupSet.OwnerNode.LastChild, amInsertAfter, False);
        vstMonitor.DeleteNode(Result);
      end;
    end;
  finally
    vstMonitor.FullExpand(nil);
    vstMonitor.EndUpdate;
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'CreateTemplateStructure', 'End Create a structure from a template');
    TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, 'CreateTemplateStructure');
  end;
end;

procedure TfrmMonitor.cbAutoTradesCloseUp(Sender: TObject);
begin
  if (DMod.fbtAccounts.FieldByName('AUTOTRADESID').AsInteger > 0) and
    (DMod.fbqAutoTrades.Locate('ID', DMod.fbtAccounts.FieldByName('AUTOTRADESID').AsInteger, [])) then
   cbAutoTrades.Hint := DMod.fbqAutoTrades.FieldByName('NOTE').AsString
  else
    cbAutoTrades.Hint := '';
end;

procedure TfrmMonitor.DeleteAllNodes;
var
  Node: PVirtualNode;
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'DeleteAllNodes');
  for Node in TMonitorLists.OrderList.Keys do
    if not IsNodePossibleToRemove(Node) then
    begin
      if (TMessageDialog.ShowQuestion('One or more orders are in a state PendSubmit or PreSubmit. Continue deleted?') = mrNo) then
        Exit;
      Break;
    end;

  vstMonitor.BeginUpdate;
  try
    FOrderGroupSet.OwnerNode := nil;
    FOrderGroupSet.RecordId  := -1;
    FOrderGroupSet.Name      := '';
    vstMonitor.Clear;
    TMonitorLists.InstrumentList.Clear;
    TMonitorLists.InstrumentList.DocumentQueue.Clear;
    TMonitorLists.InstrumentChildList.Clear;
    TMonitorLists.InstrumentChildList.DocumentQueue.Clear;
    TMonitorLists.AlgosList.Clear;
    TMonitorLists.OrderList.Clear;
    FConditionQueue.Clear;
    FControlHistData.Clear;
    SetCurrentGroupCaptions;
    try
      NordNetBroker.Leave;
    finally
      ConnectToNNBroker;
    end;
  finally
    vstMonitor.EndUpdate;
    TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'DeleteAllNodes');
  end;
end;

procedure TfrmMonitor.CreateTradeChart(aNode: PVirtualNode);
var
  Data : PTreeData;
  Id   : Integer;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if not Assigned(Data.TradeChart) then
    begin
      Data^.TradeChart := TfrmTradeChartForm.Create(nil);
      if Assigned(Data.FactorDoc) then
      begin
        Id := Data^.FactorDoc.ContractId;
        TfrmTradeChartForm(Data^.TradeChart).Initialize(Id);
        TfrmTradeChartForm(Data^.TradeChart).InstrumentName := '[Factor] ' + Data.FactorDoc.InstrumentName;
      end
      else if Assigned(Data.OrderDoc) then
      begin
        Id := Data^.OrderDoc.Id;
        TfrmTradeChartForm(Data^.TradeChart).Initialize(Id);
        TfrmTradeChartForm(Data^.TradeChart).InstrumentName := '[Order] ' + Data.OrderDoc.InstrumentName;
      end;
    end;
  end;
end;

procedure TfrmMonitor.ShowTradeChart(aNode: PVirtualNode);
var
  Data: PTreeData;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data.ConditionDoc) then
      ShowConditionChart(aNode)
    else if Assigned(Data.AlgosDoc) then
      ShowAlgosChart(aNode)
    else
    begin
      CreateTradeChart(aNode);
      Data.TradeChart.Show;
    end;
  end;
end;

procedure TfrmMonitor.ShowConditionAlgosChart(aNode: PVirtualNode);
var
  Data : PTreeData;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data) and Assigned(Data.ConditionAlgosChart) then
      Data.ConditionAlgosChart.Show;
  end;
end;

procedure TfrmMonitor.ShowAlgosChart(aNode: PVirtualNode);
var
  Data : PTreeData;
  Pair : TPair<TDateTime, TAlgosDoc.TPrice>;

  procedure PrepareAlgosValues;
  var
    DataFactor : PTreeData;
    FactorList : TList<PVirtualNode>;
    i          : Integer;
    j          : Integer;
    NodeFactor : PVirtualNode;
    PriceList  : TPriceList;
    Price      : TPrice;
  begin
    FactorList := GetListOfChilds(aNode, False, ntFactor);
    try
      for i := 0 to FactorList.Count - 1 do
      begin
        NodeFactor := FactorList[i];
        if Assigned(NodeFactor) then
        begin
          DataFactor := NodeFactor^.GetData;
          if Assigned(DataFactor) and Assigned(DataFactor.FactorDoc) then
            if (DataFactor.FactorDoc.ContractId > 0) then
            begin
              PriceList := TMonitorLists.PriceCache.GetPriceList(DataFactor.FactorDoc.ContractId);
              if Assigned(PriceList) then
                for j := 0 to PriceList.Count - 1 do
                begin
                  Price := PriceList.Items[j];
                  if Price.IsHistorical and (Price.TickType = ttLast) then
                    Data.AlgosDoc.AddToValueList(Price.TimeStamp, Price.Value, DataFactor.ReValue);
                end;
            end;
        end;
      end;
    finally
      FreeAndNil(FactorList);
    end;
  end;

begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data) and Assigned(Data.AlgosDoc) then
    begin
      if not Assigned(Data.TradeChart) then
      begin
        PrepareAlgosValues;
        Data^.TradeChart := TfrmTradeChartForm.Create(nil);
        TfrmTradeChartForm(Data^.TradeChart).InstrumentName := '[Algos] ' + Data^.RecordId.ToString + ' ' + Data.AlgosDoc.Name;
        for Pair in Data^.AlgosDoc.ValueList do
          TfrmTradeChartForm(Data^.TradeChart).AddValue(Pair.Value.Price, Pair.Key);
      end;
      Data.TradeChart.Show;
    end;
  end;
end;

procedure TfrmMonitor.AddAlgosToConditionAlgosChart(aNode: PVirtualNode);
var
  CondData : PTreeData;
  CondNode : PVirtualNode;
  Data     : PTreeData;
begin
  CondNode := nil;
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if (Data^.DocType = ntAlgos) then
      CondNode := GetParentNode(aNode, ntCondition)
    else if (Data^.DocType = ntCondition) then
      CondNode := aNode;
    if Assigned(CondNode) then
    begin
      CondData := CondNode^.GetData;
      if not Assigned(CondData^.ConditionAlgosChart) then
      begin
        CondData^.ConditionAlgosChart := TfrmConditionAlgosChart.Create(nil);
        TfrmConditionAlgosChart(CondData^.ConditionAlgosChart).InstrumentName := '[Condition] ' + CondData^.ConditionDoc.Description;
      end;
      if (Data^.DocType = ntAlgos) and not Assigned(Data^.ConditionAlgosChart) then
      begin
        TfrmConditionAlgosChart(CondData^.ConditionAlgosChart).AddSeries(aNode, Data^.AlgosDoc.Name);
        Data^.ConditionAlgosChart := CondData^.ConditionAlgosChart;
      end;
    end;
  end;
end;

procedure TfrmMonitor.TransformToOrderNN(aNode: PVirtualNode);
var
  Data: PTreeData;
  loOrder: TOrderNNDoc;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if (not Assigned(Data.OrderDoc)) and Assigned(Data.FactorDoc) and (Data.FactorDoc.BrokerType = TBrokerType.brNN) then
    begin
      loOrder := TOrderNNDoc.Create;
      try
        loOrder.OrderAction     := iabBuy;
        loOrder.Description     := '';
        loOrder.BrokerType      := TBrokerType.brNN;
        loOrder.Id              := Data.FactorDoc.NNId;
        loOrder.InstrumentName  := Data.FactorDoc.InstrumentName;
        loOrder.Symbol          := Data.FactorDoc.Symbol;
        loOrder.Exchange        := Data.FactorDoc.Exchange;
        loOrder.PrimaryExchange := Data.FactorDoc.PrimaryExchange;
        loOrder.Currency        := Data.FactorDoc.Currency;
        loOrder.Decimals        := Data.FactorDoc.Decimals;
        loOrder.MarketList      := Data.FactorDoc.MarketList;
        loOrder.IdentifierList  := Data.FactorDoc.IdentifierList;
        loOrder.IsIn            := Data.FactorDoc.IsIn;

        if(TfrmEditOrderNN.ShowDocument(loOrder, loOrder.ExtendedOptions.Subordination) = mrOk) then
        begin
          Data.OrderDoc           := loOrder;
          Data.OrderDoc.OwnerNode := aNode;
          Data.DocType            := ntOrder;
          Data.Enabled            := True;
          loOrder                 := nil;
          Data.ReValue            := 1.0;
          Data.CreationType       := ctUser;
          aNode.CheckType         := ctCheckBox;
          SetIcon(aNode);
          Data.OrderDoc.LastPrice := GetLastPrice(Data^.FactorDoc.ContractId);
          TMonitorLists.OrderList.AddOrder(aNode);
          if (TOrderNNDoc(Data.OrderDoc).FeedFromBroker = 0) and (TOrderNNDoc(Data.OrderDoc).Id > 0) then
          begin
            TMonitorLists.InstrumentList.AddNode(Data.OrderDoc.Id, aNode);
            SubscribeIBFeed(aNode);
          end;
          vstMonitor.MoveTo(aNode, aNode.Parent.FirstChild, amInsertBefore, False);
          TestOrder(aNode);
        end;
      finally
        if Assigned(loOrder) then
          FreeAndNil(loOrder)
        else
          FreeAndNil(Data.FactorDoc);
      end;
    end;
  end;
end;

procedure TfrmMonitor.TransformToBaseOrder(aNode: PVirtualNode);
var
  Data: PTreeData;
  OrderGroupId: Integer;
  InstrumentData: TInstrumentData;
  Quantity: Integer;
  AutoTradesCommon: TAutoTradesCommon;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data^.FactorDoc) then
    begin
      OrderGroupId := TfrmOrderBaseTransform.ShowDocument(Data^.FactorDoc, Quantity);
      if (OrderGroupId > -1) and (Quantity > 0) then
      begin
        InstrumentData.Assign(Data.FactorDoc);
        AutoTradesCommon := Default(TAutoTradesCommon);
        AutoTradesCommon.Quantity := Quantity;
        if Assigned(CreateTemplateStructure(OrderGroupId, @InstrumentData, AutoTradesCommon)) then
          DeleteNode(aNode);
      end;
    end;
  end;
end;

procedure TfrmMonitor.TransformToOrderIB(aNode: PVirtualNode);
var
  Data      : PTreeData;
  Order     : TOrderIBDoc;
  SokidInfo : TSokidInfo;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if (not Assigned(Data^.OrderDoc)) and Assigned(Data^.FactorDoc) and (Data^.FactorDoc.BrokerType = TBrokerType.brIB) then
    begin
      Order := TOrderIBDoc.Create;
      try
        Order.OrderAction     := iabBuy;
        Order.Description     := 'Order';
        Order.BrokerType      := TBrokerType.brIB;
        Order.Id              := Data^.FactorDoc.ContractId;
        Order.InstrumentName  := Data^.FactorDoc.InstrumentName;
        Order.Symbol          := Data^.FactorDoc.Symbol;
        Order.LocalSymbol     := Data^.FactorDoc.LocalSymbol;
        Order.Exchange        := Data^.FactorDoc.Exchange;
        Order.PrimaryExchange := Data^.FactorDoc.PrimaryExchange;
        Order.Currency        := Data^.FactorDoc.Currency;
        Order.Decimals        := Data^.FactorDoc.Decimals;
        Order.MarketList      := Data^.FactorDoc.MarketList;
        Order.SetSecurityType(Data^.FactorDoc.ContractType);

        SokidInfo := SokidList.GetItem(Order.Id);
        if (Order.Symbol.IsEmpty) then
          Order.Symbol := SokidInfo.Symbol;
        if (Order.Exchange.IsEmpty) then
          Order.Exchange := SokidInfo.Exchange;
        if (Order.PrimaryExchange.IsEmpty) then
          Order.PrimaryExchange := SokidInfo.PrimaryExchange;
        if (Order.Currency = '') then
          Order.Currency := SokidInfo.Currency;
        if (Order.MarketList = '') then
          Order.MarketList := SokidInfo.MarketRuleIds;
        Order.Expiry := SokidInfo.Expiry;
        Order.Multiplier := StrToIntDef(SokidInfo.Multiplier, 0);

        if (TfrmEditOrderIB.ShowDocument(Order, False) = mrOk) then
        begin
          Data^.OrderDoc           := Order;
          Data^.OrderDoc.OwnerNode := aNode;
          Data^.DocType            := ntOrder;
          Data^.Enabled            := True;
          Order                    := nil;
          Data^.ReValue            := 1.0;
          Data^.CreationType       := ctUser;
          aNode.CheckType          := ctCheckBox;
          SetIcon(aNode);
          Data^.OrderDoc.LastPrice := GetLastPrice(Data^.FactorDoc.ContractId);

          TMonitorLists.OrderList.AddOrder(aNode);
          vstMonitor.MoveTo(aNode, aNode.Parent.FirstChild, amInsertBefore, False);
          TestOrder(aNode);
        end;
      finally
        if Assigned(Order) then
          FreeAndNil(Order)
        else
          FreeAndNil(Data^.FactorDoc);
      end;
    end;
  end;
end;

procedure TfrmMonitor.StringGrid1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TfrmMonitor.OnConditionTimeGap(Sender: TObject);
var
  Node: PVirtualNode;
  CurData, ParentData: PTreeData;
  ok: Boolean;
  oldCondition: Boolean;
begin
  if not Application.Terminated then
  begin
    General.NoHibernate;
    SetNetworkText;
    Caption := Concat(Application.Title, FormatDateTime('hh:nn:ss', Now));
    if Connected then
    begin
      Node := vstMonitor.GetFirst();
      while Assigned(Node) do
      begin
        CurData := Node^.GetData;
        if (vstMonitor.GetNodeLevel(Node) <> 0) and
           CurData.Enabled and
           Assigned(CurData.ConditionDoc) and
           CurData.ConditionDoc.Active then
        begin
          if Assigned(frmEditCondition) and frmEditCondition.Visible then
            if (TTreeDocument.GetDocType(vstMonitor, Node.Parent) = ntOrder) then
            begin
              ParentData := Node.Parent^.GetData;
              if (frmEditCondition.OwnerNode = Node.Parent) then
                frmEditCondition.Caption := 'Condition [' + ParentData.OrderDoc.InstrumentName + '] ' + Format('%f', [ParentData.OrderDoc.LastPrice]);
            end;

          if (CurData.ConditionDoc.CondType in [ctRealtimeValue, ctRealtimeAndTimeGap]) and
             (TimeOf(CurData.ConditionDoc.Duration) > 0) then
            if (Now > CurData.ConditionDoc.InitTime + TimeOf(CurData.ConditionDoc.Duration)) then
            begin
              oldCondition := CurData.ConditionDoc.IsCondition;
              CurData.ConditionDoc.IsCondition := False;
              CurData.ConditionDoc.Active := False;
              SetIcon(Node);
              // Beep;
              if oldCondition then
                TTask.Create(
                  procedure()
                  begin
                    BeepEx(400, 1000);
                  end).Start;
            end;

          if (CurData.ConditionDoc.CondType in [ctRealtimeAndTimeGap, ctTimeGap]) then
          begin
            ok := False;

            if (CompareDate(Date, CurData.ConditionDoc.StartDate) >= 0) and
               (CompareDate(Date, CurData.ConditionDoc.EndDate) <= 0) and
               (CompareTime(Time, CurData.ConditionDoc.StartTime) >= 0) and
               (CompareTime(Time, CurData.ConditionDoc.EndTime) <= 0) then
            begin
              ok := True;

              if not CurData.ConditionDoc.IsCondition then
              begin
                CurData.ConditionDoc.IsCondition := True;
                SetIcon(Node);
                vstMonitor.InvalidateNode(Node);

                // Beep;
                TTask.Create(
                  procedure()
                  begin
                    BeepEx(1400, 300);
                    Sleep(50);
                    BeepEx(1400, 300);
                  end).Start;

                TestOrder(Node.Parent);
              end;
            end
            else
            begin
              CurData.ConditionDoc.IsCondition := False;
              SetIcon(Node);
            end;

            if CurData.ConditionDoc.IsCondition and (not ok) then
            begin
              vstMonitor.InvalidateNode(Node);
              // Beep;
              TTask.Create(
                procedure()
                begin
                  BeepEx(400, 1000);
                end).Start;
              CurData.ConditionDoc.IsCondition := False;
              SetIcon(Node);
            end;
          end;
        end;
        Node := vstMonitor.GetNext(Node);
      end;
    end;
  end;
end;

// * Om alla conditions ar sanna och minst en ar trigger sa lagg order
procedure TfrmMonitor.TestOrder(aNode: PVirtualNode);
var
  Data: PTreeData;
  Node: PVirtualNode;
  IsExistsPriority: Boolean;
  IsExistsNormal: Boolean;
  Normal: Boolean;
  OrderData: PTreeData;
  Priority: Boolean;
  Veto: Boolean;
begin
  if (TTreeDocument.GetDocType(vstMonitor, aNode) = ntOrder) then
  begin
    OrderData := aNode^.GetData;
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddEnterMethod,
                                                             'TestOrder',
                                                             'NodeId=' + OrderData.NodeID.ToString + sLineBreak +
                                                             'Begin TestOrder [' + OrderData.OrderDoc.Description + ']');

    if (aNode.CheckState <> csCheckedNormal) then
      Exit
    else if not OrderData.Enabled then
      Exit
    else if (Assigned(frmEditOrderIB) and frmEditOrderIB.Visible and (frmEditOrderIB.OwnerNode = aNode)) then
      Exit
    else if (Assigned(frmEditOrderNN) and frmEditOrderNN.Visible and (frmEditOrderNN.OwnerNode = aNode)) then
      Exit;

    IsExistsPriority := False;
    IsExistsNormal := False;
    Normal := True;
    Priority := False;
    Veto := True;

    Node := aNode.FirstChild;
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      if Assigned(Data) and (Data.DocType = ntCondition) then
      begin
        if Assigned(frmEditCondition) and frmEditCondition.Visible then
          Exit;

        if Data.Enabled and Data.ConditionDoc.Active then
        begin
          case Data.ConditionDoc.Priority of
            cpNormal:
              begin
                Normal := Normal and Data.ConditionDoc.IsCondition;
                IsExistsNormal := True;
              end;
            cpPriority:
              begin
                Priority := Priority or Data.ConditionDoc.IsCondition;
                IsExistsPriority := True;
              end;
            cpVeto:
              Veto := Veto and Data.ConditionDoc.IsCondition;
          end;
        end;
      end;
      Node := Node.NextSibling;
    end;
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'TestOrder',
                                   Format('NodeId=%d, Order "%s", OrderIBId=%d, Status=%s, Quantity=%d',
                                          [OrderData.NodeId, OrderData.OrderDoc.Description,OrderData.OrderDoc.OrderIBId, OrderData.OrderDoc.OrderStatusText, OrderData.OrderDoc.Quantity]
                                          ));

    if Veto and ((Normal and IsExistsNormal) or (Priority and IsExistsPriority)) and
      not (OrderData.OrderDoc.OrderStatus in [osError, osNotConsidered, osCancelled]) then
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'TestOrder',
                                     Format('%s<br>Result = <b>Veto</b>[%s] and ((<b>Normal</b>[%s] and <b>IsExistsNormal</b>[%s]) or (<b>Priority</b>[%s] and <b>IsExistsPriority</b>[%s]))',
                                           [THtmlLib.GetColorTag('Order is bought', clBlue), BoolToStr(Veto, True), BoolToStr(Normal, True), BoolToStr(IsExistsNormal, True), BoolToStr(Priority, True), BoolToStr(IsExistsPriority, True)]
                                           ));
      OrderData.OrderDoc.Buy;
    end
    else
      TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'TestOrder',
                                     Format('%s<br>Result = <b>Veto</b>[%s] and ((<b>Normal</b>[%s] and <b>IsExistsNormal</b>[%s]) or (<b>Priority</b>[%s] and <b>IsExistsPriority</b>[%s]))',
                                           [THtmlLib.GetColorTag('Order is not bought', clMaroon), BoolToStr(Veto, True), BoolToStr(Normal, True), BoolToStr(IsExistsNormal, True), BoolToStr(Priority, True), BoolToStr(IsExistsPriority, True)]
                                           ));

    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddExitMethod, 'TestOrder', 'End TestOrder [' + OrderData.OrderDoc.Description + ']');
  end;
end;

procedure TfrmMonitor.TestConditions(const aNode: PVirtualNode);
type
  TGradientInfo = record
    x1 : Double;
    x2 : Double;
    y1 : Double;
    y2 : Double;
  end;
var
//  BreakDown, BreakUp: Boolean;
  LineDown: TGradientInfo;
  LineUp: TGradientInfo;
  MinTime: TDateTime;
  StartIndex: Integer;
  Data: PTreeData;

  function CalculateValues(ACondition: TConditionDoc; var AGradientValue, ACorridorValue, APositionValue: Double): Boolean;
  var
    deltaMax, deltaMin, deltaP, deltaH, deltaT, deltaY, deltaB: Double;
    sumX, sumY, sumXY, sumSqrX, sumSqrY, avgX, avgY, a, b: Double;
    corridorValue: Double;
    gradientValue: Double;
    pointValue: Double;
    positionValue: Double;
    i: Integer;
    Len: Integer;
    FullGradient: Boolean;
    StartTime: TDateTime;
  begin
    Result := False;
    FullGradient := False;

    if (Data.RecordId <= 0) and (Length(ACondition.PriceArray) = 0) and (ACondition.ValueList.Count = 0) then
    begin
      PrepareConditionValues(aNode);
      ACondition.IsLoadedHistoricalData := True;
    end;

    Len := Length(ACondition.PriceArray);
    if (Len > 0) then
    begin
      StartTime := IncSecond(ACondition.PriceArray[Len - 1].TimeStamp, -ACondition.Monitoring);
      i := 0;
      StartIndex := 0;
      MinTime := 0;
      while (StartTime > ACondition.PriceArray[i].TimeStamp) and (i < Len) do
      begin
        FullGradient := True;
        StartIndex := i;
        Inc(i);
      end;
    end;

    if FullGradient then
    begin
      for i := StartIndex to Len - 1 do
      begin
        ACondition.PriceArray[i - StartIndex].Price := ACondition.PriceArray[i].Price;
        ACondition.PriceArray[i - StartIndex].TimeStamp := ACondition.PriceArray[i].TimeStamp;
      end;
      Len := Len - StartIndex;
      ACondition.SetLengthPriceArray(Len);

      sumX    := 0;
      sumY    := 0;
      sumXY   := 0;
      sumSqrX := 0;
      sumSqrY := 0;
      for i := 0 to Len - 1 do
      begin
        sumX    := sumX + ACondition.PriceArray[i].TimeStamp;
        sumY    := sumY + ACondition.PriceArray[i].Price;
        sumXY   := sumXY + ACondition.PriceArray[i].TimeStamp * ACondition.PriceArray[i].Price;
        sumSqrX := sumSqrX + Sqr(ACondition.PriceArray[i].TimeStamp);
        sumSqrY := sumSqrY + Sqr(ACondition.PriceArray[i].Price);
      end;
      avgX := sumX / Len;
      avgY := sumY / Len;

      b := (Len * sumXY - sumX * sumY) / (Len * sumSqrX - Sqr(sumX));
      a := avgY - b * avgX;

      deltaMax := 0;
      deltaMin := 0;
      for i := 0 to Len - 1 do
      begin
        pointValue := (a + b * ACondition.PriceArray[i].TimeStamp);
        if deltaMax < (ACondition.PriceArray[i].Price - pointValue) then
          deltaMax := ACondition.PriceArray[i].Price - pointValue;
        if deltaMin < (pointValue - ACondition.PriceArray[i].Price) then
          deltaMin := pointValue - ACondition.PriceArray[i].Price;
      end;

      LineUp.x1 := ACondition.PriceArray[0].TimeStamp;
      LineUp.y1 := deltaMax + a + b * LineUp.x1;
      LineUp.x2 := ACondition.PriceArray[Len - 1].TimeStamp;
      LineUp.y2 := deltaMax + a + b * LineUp.x2;

      LineDown.x1 := ACondition.PriceArray[0].TimeStamp;
      LineDown.y1 := a + b * LineDown.x1 - deltaMin;
      LineDown.x2 := ACondition.PriceArray[Len - 1].TimeStamp;
      LineDown.y2 := a + b * LineDown.x2 - deltaMin;

      deltaP        := LineUp.y2 - LineUp.y1;
      deltaT        := ACondition.Monitoring / 60 / 60; // in hours
      deltaH        := LineUp.y1 - LineDown.y1;
      deltaY        := ACondition.PriceArray[Len - 1].Price - LineDown.y2;
      positionValue := deltaY / deltaH * 100;
      deltaB        := Sqrt(Sqr(deltaP) + Sqr(deltaT));
      corridorValue := Abs(deltaH * deltaT / deltaB);

      if (deltaP > -0.0000001) and (deltaP < 0.0000001) then
      begin
        if (deltaP * deltaT > 0) then
          gradientValue := General.MinMaxGradientValue
        else
          gradientValue := -General.MinMaxGradientValue;
      end
      else
        gradientValue := deltaP / deltaT;

      ACorridorValue := corridorValue;
      AGradientValue := gradientValue;
      APositionValue := positionValue;

      ACondition.ValueArray[ctCorridor]         := ACorridorValue;
      ACondition.ValueArray[ctCorridorPosition] := APositionValue;
      ACondition.ValueArray[ctGradient]         := AGradientValue;
      Result := True;
    end
    else
    begin
      ACondition.ValueArray[ctCorridor]         := 0;
      ACondition.ValueArray[ctCorridorPosition] := 0;
      ACondition.ValueArray[ctGradient]         := 0;
    end;
  end;

  function CheckCorrWidth(ACondition: TConditionDoc; ACorridorValue: Double): Boolean;
  begin
    Result := ACondition.InequalityCor.IsCondition(ACondition.CondWidth, ACorridorValue);
  end;

  function CheckGradient(ACondition: TConditionDoc; AGradientValue: Double): Boolean;
  begin
    Result := ACondition.InequalityGr.IsCondition(ACondition.Gradient, AGradientValue);
  end;

  function CheckCorridorPosition(ACondition: TConditionDoc; AProcValue: Double): Boolean;
  begin
    Result := AProcValue > ACondition.UpProc;
  end;

  function CheckTrailBuy(ACondition: TConditionDoc): Boolean;
  begin
    Result := (ACondition.ActivationValue > 0) and
              (ACondition.MinMaxValueArray[TConditionDoc.TMinMaxValue.mmLastPayed] - ACondition.MinMaxValueArray[TConditionDoc.TMinMaxValue.mmMinPayed] >= ACondition.TrailBuy);
  end;

  function CheckTrailSell(ACondition: TConditionDoc): Boolean;
  begin
    Result := (ACondition.ActivationValue > 0) and
              (ACondition.MinMaxValueArray[TConditionDoc.TMinMaxValue.mmLastPayed] - ACondition.MinMaxValueArray[TConditionDoc.TMinMaxValue.mmMaxPayed] <= ACondition.TrailSell);
  end;

var
  CorridorValue : Double;
  GradientValue : Double;
  OldRes        : Boolean;
  PositionValue : Double;
  CompiledValue : TCompiledValue;
begin
  if Assigned(aNode) then
  begin
    GradientValue := 0;
    CorridorValue := 0;
    PositionValue := 0;
    Data := aNode^.GetData;
    if Assigned(Data^.ConditionDoc) then
    begin
      CompiledValue := Default(TCompiledValue);
      if FConditionQueue.Contains(aNode) then
      begin
        CompiledValue := TTreeDocument.CalcCompiledValue(vstMonitor, Data^.ConditionDoc);
        Data^.ConditionDoc.IsValueReady := CompiledValue.IsValueReady;
        if CompiledValue.IsValueReady then
        begin
          Data^.Enabled := True;
          Data^.ConditionDoc.Active        := True;
          Data^.ConditionDoc.CalcValue     := CompiledValue.TotalValue / Data^.ConditionDoc.DivisionValue;
          Data^.ConditionDoc.CompiledValue := CompiledValue;
          if (Data^.ConditionDoc.InitValue = 0) then
            Data^.ConditionDoc.InitValue := Data^.ConditionDoc.CalcValue;
          FConditionQueue.DeleteItem(aNode);
          if (Data^.ConditionDoc.CondType in [ctRealtimeValue, ctRealtimeAndTimeGap]) then
            TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'TestConditions',
                                                                        'NodeId=' + Data^.NodeId.ToString +
                                                                        ', Factors=' + CompiledValue.Factors +
                                                                        'IsValueReady=' + BoolToStr(CompiledValue.IsValueReady, True) +
                                                                        ', CondLimit=' + CurrToStr(Data^.ConditionDoc.CondLimit) +
                                                                        ', CompiledValue=' + CurrToStr(CompiledValue.TotalValue) +
                                                                        ', DivisionValue=' + CurrToStr(Data^.ConditionDoc.DivisionValue) +
                                                                        ', CalcValue=' + CurrToStr(Data^.ConditionDoc.CondLimit) +
                                                                        ', Sum(' + Data^.ConditionDoc.TickType1.ToString + ')=' + CurrToStr(CompiledValue.SummValue1) +
                                                                        ' [' + Data^.ConditionDoc.TypeOperation.ToString + '] ' +
                                                                        ', Sum(' + Data^.ConditionDoc.TickType2.ToString + ')=' + CurrToStr(CompiledValue.SummValue2) +
                                                                        ', Remove from Queue, CalcValue=' + CurrToStr(Data^.ConditionDoc.CalcValue) +
                                                                        ', Enabled=' + BoolToStr(Data^.Enabled, True) +
                                                                        ', Description="' + Data^.ConditionDoc.Description + '"');
        end;
      end;

      if (Data^.Enabled and Data^.ConditionDoc.Active) then
      begin
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddEnterMethod, 'TestConditions',
                                                                                'Begin TestConditions [' + Data^.ConditionDoc.Description + ']');
        OldRes := Data^.ConditionDoc.IsCondition;
        if Data^.ConditionDoc.Bypass then
          Data^.ConditionDoc.IsCondition := Data^.ConditionDoc.CompiledValue.IsValueReady
        else
        begin
          case Data^.ConditionDoc.CondType of
            ctRealtimeValue, ctRealtimeAndTimeGap:
              begin
                TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'TestConditions',
                                                                      'NodeId=' + Data.NodeId.ToString +
                                                                      ', Calculated Value=' + FloatToStr(Data^.ConditionDoc.CalcValue) +
                                                                      ' [' + Data^.ConditionDoc.TypeOperation.ToString + '] ' +
                                                                      ', Condition Limit=' + FloatToStr(Data^.ConditionDoc.CondLimit));
                Data^.ConditionDoc.IsCondition := Data^.ConditionDoc.InequalityRt.IsCondition(Data^.ConditionDoc.CalcValue, Data^.ConditionDoc.CondLimit);
                Data^.ConditionDoc.ValueArray[ctRealtimeValue] := Data^.ConditionDoc.CalcValue;
              end;
            ctTrailBuy:
              begin
                Data^.ConditionDoc.IsCondition := CheckTrailBuy(Data^.ConditionDoc);
                if Data^.ConditionDoc.IsCondition then
                begin
                  Data^.ConditionDoc.ValueArray[ctTrailBuy] := Data^.ConditionDoc.CalcValue;
                  Data^.ConditionDoc.MinMaxValueArray[TConditionDoc.TMinMaxValue.mmEntryPrice] := Data^.ConditionDoc.CalcValue;
                end;
              end;
            ctTrailSell:
              begin
                Data^.ConditionDoc.IsCondition := CheckTrailSell(Data^.ConditionDoc);
                if Data^.ConditionDoc.IsCondition then
                begin
                  Data^.ConditionDoc.ValueArray[ctTrailSell] := Data^.ConditionDoc.CalcValue;
                  Data^.ConditionDoc.MinMaxValueArray[TConditionDoc.TMinMaxValue.mmEntryPrice] := Data^.ConditionDoc.CalcValue;
                end;
              end;
            ctGradient:
              begin
                if CalculateValues(Data^.ConditionDoc, GradientValue, CorridorValue, PositionValue) then
                  Data^.ConditionDoc.IsCondition := CheckGradient(Data^.ConditionDoc, GradientValue);
              end;
            ctCorridor:
              begin
                if CalculateValues(Data^.ConditionDoc, GradientValue, CorridorValue, PositionValue) then
                  Data^.ConditionDoc.IsCondition := CheckCorrWidth(Data^.ConditionDoc, CorridorValue);
              end;
            ctGradientAndCorridor:
              begin
                if CalculateValues(Data^.ConditionDoc, GradientValue, CorridorValue, PositionValue) then
                  Data^.ConditionDoc.IsCondition := CheckCorrWidth(Data^.ConditionDoc, CorridorValue) and CheckGradient(Data^.ConditionDoc, GradientValue);
              end;
            ctCorridorPosition:
              begin
                if CalculateValues(Data^.ConditionDoc, GradientValue, CorridorValue, PositionValue) and
                  CheckCorrWidth(Data^.ConditionDoc, CorridorValue) then
                  Data^.ConditionDoc.IsCondition := CheckCorridorPosition(Data^.ConditionDoc, PositionValue);
              end;
          end;
        end;
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'TestConditions',
                                                              'NodeId=' + Data.NodeId.ToString +
                                                              ', IsCondition=' + BoolToStr(Data^.ConditionDoc.IsCondition, True) + ', ' + Data^.ConditionDoc.ToString);
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddExitMethod, 'TestConditions', 'End TestConditions [' + Data^.ConditionDoc.Description + ']');

        if Assigned(Data^.ConditionChart) and (Data^.ConditionDoc.CondType in [ctGradient, ctCorridor, ctCorridorPosition]) then
        begin
          TfrmConditionChart(Data^.ConditionChart).ValueArray[ctGradient]         := GradientValue;
          TfrmConditionChart(Data^.ConditionChart).ValueArray[ctCorridor]         := CorridorValue;
          TfrmConditionChart(Data^.ConditionChart).ValueArray[ctCorridorPosition] := PositionValue;
          TfrmConditionChart(Data^.ConditionChart).SetTopLine(LineUp.x1, LineUp.x2, LineUp.y1, LineUp.y2);
          TfrmConditionChart(Data^.ConditionChart).SetBottomLine(LineDown.x1, LineDown.x2, LineDown.y1, LineDown.y2);
        end;

        if Assigned(frmEditCondition) and frmEditCondition.Showing and (frmEditCondition.OwnerNode = aNode) then
        begin
          for var CondValue := Low(TConditionType) to High(TConditionType) do
            frmEditCondition.ValueArray[CondValue] := Data^.ConditionDoc.ValueArray[CondValue];
//          for var MinMaxValue := Low(TConditionDoc.TMinMaxValue) to High(TConditionDoc.TMinMaxValue) do
//            frmEditCondition.MinMaxValueArray[MinMaxValue] := Data^.ConditionDoc.MinMaxValueArray[MinMaxValue];
        end;

        if (OldRes <> Data^.ConditionDoc.IsCondition) or Data^.ConditionDoc.Bypass then
        begin
          SetIcon(aNode);
          vstMonitor.InvalidateNode((aNode));

          if Data^.ConditionDoc.IsCondition then
          begin
            TTask.Create(
              procedure()
              begin
                BeepEx(1400, 300);
                Sleep(50);
                BeepEx(1400, 300);
              end).Start;

            TestOrder(aNode.Parent);
          end
          else
          begin
            TTask.Create(
              procedure()
              begin
                BeepEx(400, 1000);
              end).Start;
          end;

        end;
        vstMonitor.InvalidateNode(aNode);
      end;
    end;
  end;
end;

procedure TfrmMonitor.UpdateAlgos(const aNode: PVirtualNode);
var
  CurData   : PTreeData;
  CurNode   : PVirtualNode;
  LastValue : Double;
  MainData  : PTreeData;
  MainValue : Double;

  procedure SetTradeChartValue(const aNode: PVirtualNode; const aValue: Double);
  var
    Data: PTreeData;
  begin
    if Assigned(aNode) and TMonitorLists.AlgosList.ContainsKey(aNode) then
    begin
      Data := aNode^.GetData;
      if Assigned(Data^.TradeChart) then
        TfrmTradeChartForm(Data^.TradeChart).AddValue(aValue);
      if Assigned(Data.ConditionAlgosChart) then
        TfrmConditionAlgosChart(Data^.ConditionAlgosChart).AddValue(aNode, aValue);
      if Assigned(frmConditionRealtimeChart) then
        frmConditionRealtimeChart.AddValue(aNode, aValue);
    end;
  end;

begin
  if Assigned(aNode) then
  begin
    MainData := aNode^.GetData;
    MainValue := 0;
    if Assigned(MainData) and Assigned(MainData.AlgosDoc) and MainData.Enabled then
    begin
      CurNode := aNode.FirstChild;
      while Assigned(CurNode) do
      begin
        CurData := CurNode^.GetData;
        if Assigned(CurData) then
        begin
          case TTreeDocument.GetDocType(vstMonitor, CurNode) of
            ntAlgos:
              LastValue := CurData.AlgosDoc.TickValue[ttLast];
            ntFactor:
              LastValue := GetLastPrice(CurData^.FactorDoc.ContractId, CurData^.FactorDoc.TickType1);
          else
            LastValue := 0;
          end;
          MainValue := MainValue + LastValue;
        end;
        CurNode := CurNode.NextSibling;
      end;
      SetTradeChartValue(aNode, MainValue);
      MainData.AlgosDoc.TickValue[ttLast] := MainValue;
    end;

    if (TTreeDocument.GetDocType(vstMonitor, aNode.Parent) = ntAlgos) then
      UpdateAlgos(aNode.Parent)
    else if (TTreeDocument.GetDocType(vstMonitor, aNode.Parent) = ntCondition) then
      UpdateCondition(aNode.Parent);

    TThread.Queue(nil,
      procedure
      begin
        vstMonitor.InvalidateNode(aNode);
        if Assigned(frmEditAlgos) and frmEditAlgos.Showing and (frmEditAlgos.OwnerNode = aNode) then
          frmEditAlgos.MainValue := MainValue;
      end);
  end;
end;

procedure TfrmMonitor.UpdateCondition(const aNode: PVirtualNode);
var
  Data          : PTreeData;
  Item          : TConditionDoc.TGradientPrice;
  CompiledValue : TCompiledValue;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data) and Assigned(Data.ConditionDoc) and (Data.ConditionDoc.Active) then
      if (Data^.ConditionDoc.CondType in [ctRealtimeValue, ctRealtimeAndTimeGap, ctGradient, ctCorridor, ctCorridorPosition]) then
      begin
        CompiledValue := Default(TCompiledValue);
        Data^.ConditionDoc.CalcValue := 0;
        if (Data^.ConditionDoc.CondType <> ctTimeGap) or aShowSumCondition.Checked then
        begin
          CompiledValue := TTreeDocument.CalcCompiledValue(vstMonitor, Data^.ConditionDoc);
          Data^.ConditionDoc.IsValueReady := CompiledValue.IsValueReady;
          Data^.Enabled := CompiledValue.IsValueReady;
          if CompiledValue.IsValueReady then
          begin
            Data^.ConditionDoc.CompiledValue := CompiledValue;
            Data^.ConditionDoc.CalcValue := CompiledValue.TotalValue / Data^.ConditionDoc.DivisionValue;
            Data^.ConditionDoc.TickValue[ttLast] := CompiledValue.TotalValue;
          end;

          if (Data^.ConditionDoc.CondType in [ctRealtimeValue, ctRealtimeAndTimeGap]) then
            TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'UpdateCondition',
                                                                        'NodeId=' + Data^.NodeId.ToString +
                                                                        ', Factors=' + CompiledValue.Factors +
                                                                        'IsValueReady=' + BoolToStr(CompiledValue.IsValueReady, True) +
                                                                        ', Sum(' + Data^.ConditionDoc.TickType1.ToString + ')=' + CurrToStr(CompiledValue.SummValue1) +
                                                                        ', Sum(' + Data^.ConditionDoc.TickType2.ToString + ')=' + CurrToStr(CompiledValue.SummValue2) +
                                                                        ', DivisionValue=' + CurrToStr(Data^.ConditionDoc.DivisionValue) +
                                                                        ', CalcValue=' + CurrToStr(Data^.ConditionDoc.CalcValue) +
                                                                        ', CondLimit=' + CurrToStr(Data^.ConditionDoc.CondLimit) +
                                                                        ', CompiledValue=' + CurrToStr(CompiledValue.TotalValue));
        end;

        if Assigned(Data^.ConditionAlgosChart) then
          TfrmConditionAlgosChart(Data^.ConditionAlgosChart).AddConditionValue(CompiledValue.TotalValue);
        if Assigned(frmConditionRealtimeChart) then
          frmConditionRealtimeChart.AddConditionValue(aNode, CompiledValue.TotalValue);
        if Assigned(Data^.ConditionChart) then
        begin
          TfrmConditionChart(Data^.ConditionChart).AddConditionValue(CompiledValue.TotalValue);
          if (Data^.ConditionDoc.CondType in [ctRealtimeValue, ctRealtimeAndTimeGap]) then
            TfrmConditionChart(Data^.ConditionChart).SetTopLine(Now);
        end;

        if (Data^.ConditionDoc.CondType in [ctGradient, ctCorridor, ctCorridorPosition]) then
        begin
          if Data^.ConditionDoc.IsLoadedHistoricalData then
          begin
            Item.TimeStamp := Utils.RoundToNearest(Now, C_ROUND_SEC);
            var FoundIndex: Integer;
            if TArray.BinarySearch<TConditionDoc.TGradientPrice>(Data^.ConditionDoc.PriceArray, Item, FoundIndex,
                                                  TDelegatedComparer<TConditionDoc.TGradientPrice>.Create(
                                                  function(const Left, Right: TConditionDoc.TGradientPrice): Integer
                                                  begin
                                                    if (Left.TimeStamp < Right.TimeStamp) then
                                                      Result := LessThanValue
                                                    else if (Left.TimeStamp > Right.TimeStamp) then
                                                      Result := GreaterThanValue
                                                    else
                                                      Result := EqualsValue;
                                                  end)) then
            begin
              Data^.ConditionDoc.PriceArray[FoundIndex].Price := CompiledValue.TotalValue;
              Data^.ConditionDoc.PriceArray[FoundIndex].TimeStamp := Item.TimeStamp;
            end
            else
            begin
              FoundIndex := Length(Data^.ConditionDoc.PriceArray);
              Data^.ConditionDoc.SetLengthPriceArray(FoundIndex + 1);
              Data^.ConditionDoc.PriceArray[FoundIndex].Price := CompiledValue.TotalValue;
              Data^.ConditionDoc.PriceArray[FoundIndex].TimeStamp := Item.TimeStamp;
            end;
          end
          else if Data^.ConditionDoc.CondType in [ctTrailBuy, ctTrailSell] then
          begin
            if (TTreeDocument.GetDocType(vstMonitor, aNode.Parent) = ntOrder) and
               (aNode.Parent.CheckState = csCheckedNormal) and
               (Data^.ConditionDoc.ActivationValue = 0) then
              Data^.ConditionDoc.ActivationValue := CompiledValue.TotalValue;
          end;
        end;
        TestConditions(aNode);
      end;
  end;
end;

function TfrmMonitor.UpdateChildAlgos(Node: PVirtualNode; WithUpdateCondition: Boolean = True): Boolean;
var
  CurNode: PVirtualNode;
  MainData, CurData: PTreeData;
begin
  Result := False;
  MainData := Node^.GetData;

  if (Assigned(MainData.ConditionDoc) and (MainData.ConditionDoc.CondType <> ctTimeGap)) or Assigned(MainData.AlgosDoc) then
  begin
    CurNode := Node.FirstChild;
    while Assigned(CurNode) do
    begin
      CurData := CurNode^.GetData;
      if (CurData.DocType = ntAlgos) then
        Result := UpdateChildAlgos(CurNode);
      CurNode := CurNode.NextSibling;
    end;

    if WithUpdateCondition and Assigned(MainData.ConditionDoc) and Result then
      UpdateCondition(Node);
  end;
end;

procedure TfrmMonitor.OnInstrumentChildPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
var
  InstrumentItem : TInstrumentItem;
  InstrumentChildList: TFactorList;
  CurData: PTreeData;
  CurNode: PVirtualNode;
begin
  if Assigned(Sender) and (Sender is TFactorList) then
  begin
    InstrumentChildList := TFactorList(Sender);

     if (TickType = ttLast) then
     begin
       InstrumentItem := InstrumentChildList.GetItem(Id);
       if Assigned(InstrumentItem) then
         for CurNode in InstrumentItem.NodeList do
           if Assigned(CurNode) then
           begin
             CurData := CurNode^.GetData;
             if Assigned(CurData) and
                Assigned(CurData.OrderDoc) and
               (CurData.OrderDoc is TOrderNNDoc) then
               TOrderNNDoc(CurData.OrderDoc).SetValueArrayFromChildFeed(ttLast, Value);
           end;
     end;
  end;
end;

procedure TfrmMonitor.OnMotherFilledPriceChange(const MotherNode: PVirtualNode);
var
  arrFactorNodes: TNodeArray;
  arrOrderNodes: TNodeArray;
  FactorData: PTreeData;
  MotherData: PTreeData;
begin
  if Assigned(MotherNode) then
  begin
    MotherData := MotherNode.GetData;
    arrOrderNodes := TTreeDocument.GetNodesListByType(vstMonitor, MotherNode, ntOrder);
    for var OrderNode in arrOrderNodes do
      if Assigned(OrderNode) then
      begin
        arrFactorNodes := TTreeDocument.GetNodesListByType(vstMonitor, OrderNode, ntFactor);
        for var FactorNode in arrFactorNodes do
          if Assigned(FactorNode) then
          begin
            FactorData := FactorNode^.GetData;
            if Assigned(FactorData) and Assigned(FactorData^.FactorDoc) and (FactorData^.Enabled) then
            begin
              if (FactorData^.FactorDoc.TickType1 = ttMotherFilledPrice) or
                 (FactorData^.FactorDoc.TickType2 = ttMotherFilledPrice) then
                case TTreeDocument.GetDocType(vstMonitor, FactorNode.Parent) of
                  ntAlgos:
                    UpdateAlgos(FactorNode.Parent);
                  ntCondition:
                    begin
                      TPublishers.LogPublisher.Write([ltLogWriter], ddText,
                                                                    'OnMotherFilledPriceChange',
                                                                    'NodeId=' + FactorData^.NodeId.ToString +
                                                                    ', ConId=' + FactorData^.FactorDoc.ContractId.ToString +
                                                                    ', Symbol=' + FactorData^.FactorDoc.Symbol +
                                                                    ', Price Value=' + CurrToStr(MotherData^.OrderDoc.AvgPrice));
                      UpdateCondition(FactorNode.Parent);
                    end;
                end;
            end;
          end;
      end;
  end;
end;

procedure TfrmMonitor.OnInstrumentPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
var
  InstrumentItem: TInstrumentItem;
  FactorList: TFactorList;
  Data: PTreeData;
  Node: PVirtualNode;
begin
  RecieveDataIB := True;
  if Assigned(Sender) and (Sender is TFactorList) then
  begin
    FactorList := TFactorList(Sender);
    InstrumentItem := FactorList.GetItem(Id);
    if Assigned(InstrumentItem) then
    begin
      FactorList.SetValue(Id, TimeStamp, Value, TickType);

      for Node in InstrumentItem.NodeList do
        if Assigned(Node) then
        begin
          Data := Node^.GetData;
          if Assigned(Data) and (TickType < ttNotSet) then
          begin
            //FACTOR
            if Assigned(Data^.FactorDoc) then
            begin
              if (Data^.Enabled or FConditionQueue.Contains(Node)) then
                case TTreeDocument.GetDocType(vstMonitor, Node.Parent) of
                  ntAlgos:
                    if (Data^.FactorDoc.TickType1 = TickType) or (Data^.FactorDoc.TickType2 = TickType) then
                      UpdateAlgos(Node.Parent);
                  ntCondition:
                    if (Data^.FactorDoc.TickType1 = TickType) or (Data^.FactorDoc.TickType2 = TickType) then
                    begin
                      TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'OnInstrumentPriceChange',
                                                                             'NodeId=' + Data^.NodeID.ToString +
                                                                             ', ConId=' + Id.ToString +
                                                                             ', Symbol=' + Data^.FactorDoc.Symbol +
                                                                             ', Price Value=' + CurrToStr(Value));
                      UpdateCondition(Node.Parent);
                    end;
                end;
            end
            //ORDER IB
            else if Assigned(Data^.OrderDoc) and (Data^.OrderDoc is TOrderIBDoc) then
            begin
              if (TickType = ttLast) then
              begin
                Data^.OrderDoc.LastPrice := Value;
                if TMonitorLists.InstrumentList.DocumentQueue.Contains(Node) then
                begin
                  TMonitorLists.InstrumentList.DocumentQueue.DeleteItem(Node);
                  TTreeFactory.FillOrderPrice(Data^.OrderDoc, 0);
                end;
              end;
            end;
          end;
          vstMonitor.InvalidateNode(Node);
        end;
    end;
  end;
end;

procedure TfrmMonitor.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
begin
  if not Application.Terminated then
  begin
    if (TickType = ttMarkPrice) then
      TickType := ttLast;
    TMonitorLists.PriceCache.AddPrice(Id, TickType, Value, TimeStamp);
  end;
end;

procedure TfrmMonitor.vstMonitorBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  TMonitorTree.OnBeforeCellPaint(Sender, TargetCanvas, Node, Column, CellPaintMode, CellRect, ContentRect);
end;

function TfrmMonitor.ExistsChildConditions(aNode: PVirtualNode): Boolean;
var
  CurrNode : PVirtualNode;
begin
  Result := False;
  if Assigned(aNode) then
  begin
    Result := TTreeDocument.GetDocType(vstMonitor, aNode) = ntCondition;
    if (not Result) and (aNode.ChildCount > 0) then
    begin
      CurrNode := aNode.FirstChild;
      while Assigned(CurrNode) do
      begin
        if ExistsChildConditions(CurrNode) then
          Exit(True);
        CurrNode := CurrNode.NextSibling;
      end;
    end;
  end;
end;

function TfrmMonitor.GetParentNode(aNode: PVirtualNode; aDocType: TDocType): PVirtualNode;
begin
  Result := TTreeDocument.GetParentNode(vstMonitor, aNode, aDocType);
end;

procedure TfrmMonitor.DeactivateChildConditions(aNode: PVirtualNode);
var
  CurrNode: PVirtualNode;
  Data: PTreeData;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data.ConditionDoc) then
      Data.ConditionDoc.BlankTrailParams;
    if (aNode.ChildCount > 0) then
    begin
      CurrNode := aNode.FirstChild;
      while Assigned(CurrNode) do
      begin
        DeactivateChildConditions(CurrNode);
        CurrNode := CurrNode.NextSibling;
      end;
    end;
  end;
end;

procedure TfrmMonitor.vstMonitorChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  Data: PTreeData;
begin
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if Assigned(Data) and Assigned(Data.OrderDoc) then
    begin
      case Node.CheckState of
        csUncheckedNormal:
          begin
            if ExistsChildConditions(Node) then
            begin
              Node.CheckState := csCheckedNormal;
              ResetChildConditions(Node);
            end
            else
              Data.OrderDoc.Buy;
            NewState := csCheckedNormal;
          end;
        csCheckedNormal:
          begin
            DeactivateChildConditions(Node);
            if (Data.OrderDoc.OrderStatus in [osSleeping]) then
              NewState := csUncheckedNormal
            else if (Data.OrderDoc.OrderIBId > 0) then
            begin
              NewState := csUncheckedNormal;
              Data.OrderDoc.CancelOrder;
              if General.IsShowCancelBox then
                TMessageDialog.ShowInfo(rsOrderCancellation);
            end
            else
            begin
              NewState := csCheckedNormal;
              Allowed := False;
            end;
          end;
      end;
      SetIcon(Node);
    end;
    TPublishers.MonitorStructureChangePublisher.OnMonitorStructureChange(Sender, Node, crIgnore);
  end;
end;

procedure TfrmMonitor.vstMonitorCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PTreeData;
begin
  Result := 0;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  case Column of
    COL_ITEMS:
      if Assigned(Data1^.FactorDoc) and Assigned(Data2^.FactorDoc) then
        Result := CompareText(Data1^.FactorDoc.InstrumentName, Data2^.FactorDoc.InstrumentName);
    COL_VALUE:
      if Assigned(Data1^.FactorDoc) and Assigned(Data2^.FactorDoc) then
        Result := CompareValue(
          GetLastPrice(Data1^.FactorDoc.ContractId) / GetLastPrice(Data1^.FactorDoc.ContractId, ttClose),
          GetLastPrice(Data2^.FactorDoc.ContractId) / GetLastPrice(Data2^.FactorDoc.ContractId, ttClose))
  end;
end;

procedure TfrmMonitor.vstMonitorDblClick(Sender: TObject);
var
  IsExpanded: Boolean;
  Data: PTreeData;
  Limit: Double;
  Volume: Integer;
begin
  if Assigned(vstMonitor.FocusedNode) then
  begin
    Data := vstMonitor.FocusedNode^.GetData;
    if (vstMonitor.FocusedColumn = COL_VALUE) and Assigned(Data.OrderDoc) and
       (Data.OrderDoc.OrderStatus in [osSleeping, osPreSubmit, osSubmitted, osPendSubmit]) then
    begin
      if (Data.OrderDoc is TOrderNNDoc) then
      begin
        case Data.OrderDoc.OrderType of
          otMarket, otComboMarket:
            Limit := TOrderNNDoc(Data.OrderDoc).Price;
          otLimit, otComboLimit:
            Limit := TOrderNNDoc(Data.OrderDoc).Limit;
        end;
      end
      else if (Data.OrderDoc is TOrderIBDoc) then
      begin
        case Data.OrderDoc.OrderType of
          otMarketTouch, otPegMarket, otAuctionRel, otPassiveRel, otPegMidPt, otStop,
          otStopwProtect, otTrail, otTrailLimit:
            Limit := TOrderIBDoc(Data.OrderDoc).AuxPrice;

          otAuction, otDiscretionary, otPegStock, otPegPrimary, otSweepFill, otAuctionLimit,
          otISEBlock, otLimit, otLimitTouch, otLimitClose, otLimitOpen, otStopLimit, otComboLimit,
          otComboMarket, otComboLimitLeg:
            Limit := TOrderIBDoc(Data.OrderDoc).Limit;
        end;
      end;
      Volume := Data.OrderDoc.Quantity;
      Limit  := SimpleRoundTo(Limit, -2);
      if (TfrmOrderChange.ShowDocument(Limit, Volume) = mrOk) then
      begin
        if (Data.OrderDoc is TOrderNNDoc) then
        begin
          case Data.OrderDoc.OrderType of
            otMarket, otComboMarket:
              TOrderNNDoc(Data.OrderDoc).Price := Limit;
            otLimit, otComboLimit:
              TOrderNNDoc(Data.OrderDoc).Limit := Limit;
          end;
        end
        else if (Data.OrderDoc is TOrderIBDoc) then
        begin
          case Data.OrderDoc.OrderType of
            otMarketTouch, otPegMarket, otAuctionRel, otPassiveRel, otPegMidPt, otStop,
            otStopwProtect, otTrail, otTrailLimit:
              TOrderIBDoc(Data.OrderDoc).AuxPrice := Limit;

            otAuction, otDiscretionary, otPegStock, otPegPrimary, otSweepFill, otAuctionLimit,
            otISEBlock, otLimit, otLimitTouch, otLimitClose, otLimitOpen, otStopLimit, otComboLimit,
            otComboMarket, otComboLimitLeg:
              TOrderIBDoc(Data.OrderDoc).Limit := Limit;
          end;
        end;
        Data.OrderDoc.Quantity := Volume;
        if (Data.OrderDoc.OrderStatus in [osPreSubmit, osSubmitted, osPendSubmit]) then
          Data.OrderDoc.UpdateOrder;
      end;
    end
    else
    begin
      IsExpanded := (vsExpanded in vstMonitor.FocusedNode.States);
      aShowEditDialogExecute(nil);
      vstMonitor.Expanded[vstMonitor.FocusedNode] := not IsExpanded;
    end;
  end;
end;

procedure TfrmMonitor.AddInstrumentFromScanner(aTargetNode: PVirtualNode; aSourceTree: TVirtualStringTree);
var
  i: Integer;
  Node: PVirtualNode;
  Data: PInstrumentData;
  AutoTradeInfo: TAutoTradeInfo;
  AutoTrade: IAutoTrade;
  Quantity: Integer;
  Currency: string;
  LastPrice: Double;
  LastExch: Double;
begin
  if Supports(aSourceTree.Owner, IAutoTrade, AutoTrade) then
  begin
    AutoTradeInfo := AutoTrade.GetAutoTradeInfo;
    if (AutoTradeInfo.OrderGroupId <= 0) then
      AddInstrumentFromSearch(aTargetNode, TVirtualStringTree(aSourceTree))
    else
    begin
      Node := aSourceTree.GetFirstSelected;
      for i := 0 to aSourceTree.SelectedCount - 1 do
      begin
        Data := Node^.GetData;
        Currency := SokidList.GetItem(Data^.Id).Currency;
        if (Currency = C_DEFAULT_CURRENCY) then
          LastExch := 1
        else
          LastExch := TMonitorLists.CurrencyCache.GetLastExchange(C_DEFAULT_CURRENCY + Currency);

        LastPrice := GetLastPrice(Data^.Id);
        if (LastPrice = 0) then
          LastPrice := GetLastPrice(Data^.Id, ttClose);
        Quantity := 1;
        if (LastPrice > 0) and (LastExch > 0) then
          Quantity := Trunc((AutoTradeInfo.OrderAmount / LastExch) / LastPrice);
        CreateTemplateStructure(AutoTradeInfo.OrderGroupId,
                                Data,
                                TAutoTradesCommon.Create(Quantity,
                                                         AutoTradeInfo.QualifierInstance,
                                                         AutoTradeInfo.QualifierId,
                                                         AutoTradeInfo.InstanceNum,
                                                         AutoTradeInfo.RecordId,
                                                         AutoTradeInfo.AllowSendDuplicateOrder));

        Node := aSourceTree.GetNextSelected(Node, False);
      end;
    end;
  end;
end;

procedure TfrmMonitor.AddInstrumentFromSearch(aTargetNode: PVirtualNode; aSourceTree: TVirtualStringTree);

  procedure ConvertToSokidInfo(var Dest: PSokidInfo; Source: PInstrumentData);
  begin
    Dest^.Broker          := Source^.BrokerType;
    Dest^.ContractId      := Source^.Id;
    Dest^.IsIn            := Source^.IsIn;
    Dest^.SecurityType    := Source^.SecurityType.ToString;
    Dest^.Group           := Source^.Group;
    Dest^.Name            := Source^.Name;
    Dest^.Broker          := Source^.BrokerType;
    Dest^.Symbol          := Source^.Symbol;
    Dest^.LocalSymbol     := Source^.LocalSymbol;
    Dest^.Currency        := Source^.Currency;
    Dest^.Exchange        := Source^.Exchange;
    Dest^.PrimaryExchange := Source^.PrimaryExchange;
    Dest^.Sector          := Source^.Sector;
    Dest^.Expiry          := Source^.Expiry;
    Dest^.Multiplier      := Source^.Multiplier.ToString;
  end;

  procedure SaveFactorToSokidList(aFactorDoc: TFactorDoc);
  var
    SokidInfo: TSokidInfo;
  begin
    if Assigned(aFactorDoc) and not SokidList.ContainsKey(aFactorDoc.ContractId) then
    begin
      SokidInfo := TSokidInfo.Create(aFactorDoc.ContractId);
      SokidInfo.SecurityType    := aFactorDoc.ContractType;
      SokidInfo.Currency        := aFactorDoc.Currency;
      SokidInfo.Decimals        := aFactorDoc.Decimals;
      SokidInfo.Description     := aFactorDoc.InstrumentName;
      SokidInfo.Exchange        := aFactorDoc.Exchange;
      SokidInfo.PrimaryExchange := aFactorDoc.PrimaryExchange;
      SokidInfo.MarketRuleIds   := aFactorDoc.MarketList;
      SokidInfo.Name            := aFactorDoc.InstrumentName;
      SokidInfo.Symbol          := aFactorDoc.Symbol;
      SokidInfo.Expiry          := aFactorDoc.Expiry;
      SokidInfo.Broker          := aFactorDoc.BrokerType;
      SokidList.SetValue(SokidInfo);
    end;
  end;

var
  BrokerHelperClass: TBrokerHelperClass;
  CondId: Integer;
  Data: PTreeData;
  i, j: Integer;
  NewNode: PVirtualNode;
  NewOrderGroup: PVirtualNode;
  SourceNode: PVirtualNode;
  SokidInfo: PSokidInfo;
  InstrumentData: PInstrumentData;
  t: TIABTickType;
  SourceIsScanner: Boolean;
begin
  vstMonitor.BeginUpdate;
  NewNode := nil;
  try
    SourceNode := aSourceTree.GetFirstSelected;
    //save instruments
    SourceIsScanner := Supports(aSourceTree.Owner, IAutoTrade);
    for i := 0 to aSourceTree.SelectedCount - 1 do
    begin
      if SourceIsScanner then
      begin
        InstrumentData := SourceNode^.GetData;
        New(SokidInfo);
        ConvertToSokidInfo(SokidInfo, InstrumentData);
      end
      else
        SokidInfo := SourceNode^.GetData;

      if SokidInfo.Symbol.IsEmpty then
      begin
        NewOrderGroup := TTreeDocument.CreateOrderGroup(GetCurrentOrderGroupSet, vstMonitor);
        SetIcon(NewOrderGroup);
        Data := NewOrderGroup^.GetData;
        Data.OrderGroupDoc.Name := SokidInfo.Name;
        vstMonitor.Expanded[NewOrderGroup] := True;
        aTargetNode := NewOrderGroup;
      end;

      if Assigned(aTargetNode) then
      begin
        if IsAutoorderGroup(aTargetNode) and (not General.AllowEnterAutoorder) then
          aTargetNode := nil;
      end
      else
      begin
        NewOrderGroup := TTreeDocument.CreateOrderGroup(GetCurrentOrderGroupSet, vstMonitor);
        SetIcon(NewOrderGroup);
        vstMonitor.Expanded[NewOrderGroup] := True;
        aTargetNode := NewOrderGroup;
      end;

      if not SokidInfo.Symbol.IsEmpty then
      begin
        NewNode := TTreeDocument.CreateFactor(aTargetNode, vstMonitor);
        Data := NewNode^.GetData;
        Data^.FactorDoc.BrokerType      := SokidInfo^.Broker;
        Data^.FactorDoc.TickType1       := ttLast;
        Data^.FactorDoc.TickType2       := ttNotSet;
        Data^.FactorDoc.ContractId      := SokidInfo^.ContractId;
        Data^.FactorDoc.InstrumentName  := SokidInfo^.Name;
        Data^.FactorDoc.Currency        := SokidInfo^.Currency;
        Data^.FactorDoc.Exchange        := SokidInfo^.Exchange;
        Data^.FactorDoc.PrimaryExchange := SokidInfo^.PrimaryExchange;
        Data^.FactorDoc.ContractType    := SokidInfo^.SecurityType;
        Data^.FactorDoc.Symbol          := SokidInfo^.Symbol;
        Data^.FactorDoc.IsIn            := SokidInfo^.IsIn;
        Data^.FactorDoc.Expiry          := SokidInfo^.Expiry;
        Data^.FactorDoc.BrokerType      := SokidInfo^.Broker;
        Data^.FactorDoc.UseInAutoOrder  := False;

        if (SokidInfo^.Tradables <> nil) then
          for j := 0 to Length(SokidInfo^.Tradables) - 1 do
          begin
            if Data^.FactorDoc.IdentifierList.IsEmpty then
            begin
              Data^.FactorDoc.IdentifierList := SokidInfo^.Tradables[j].Identifier;
              Data^.FactorDoc.MarketList     := SokidInfo^.Tradables[j].MarketId.ToString;
            end
            else
            begin
              Data^.FactorDoc.IdentifierList := Data^.FactorDoc.IdentifierList + ';' + SokidInfo^.Tradables[j].Identifier;
              Data^.FactorDoc.MarketList     := Data^.FactorDoc.MarketList + ';' + SokidInfo^.Tradables[j].MarketId.ToString;
            end;
          end;

        Data^.FactorDoc.CurrentValue := GetLastPrice(Data^.FactorDoc.ContractId, Data^.FactorDoc.TickType1);
        if (Data^.FactorDoc.CurrentValue > 0) then
          TMonitorLists.InstrumentList.DocumentQueue.DeleteItem(NewNode);

        case Data^.FactorDoc.BrokerType of
          TBrokerType.brIB:
            begin
              SaveFactorToSokidList(Data^.FactorDoc);
              TIABMarket.RequestMarketData(Data^.FactorDoc.ContractId);
            end;
          TBrokerType.brNN:
            begin
              if miUseIBfeeds.Checked then
              begin
                CondId := GetCONIDforNN(Data^.FactorDoc.Symbol, Data^.FactorDoc.ContractType, Data^.FactorDoc.Exchange, Data^.FactorDoc.Currency);
                Data^.FactorDoc.UseIBFeeds := True;
                Data^.FactorDoc.ContractId      := CondId;
                Data^.FactorDoc.IBID       := CondId;
                TIABMarket.RequestMarketData(Data^.FactorDoc.NNId);
              end
              else
              begin
                BrokerHelperClass := TBrokerHelperFactory.GetBrokerHelperClass(TBrokerType.brNN.ToString);
                for j := 0 to Length(SokidInfo^.Tradables) - 1 do
                  BrokerHelperClass.SubscribeAllFeeds(SokidInfo^.Tradables[j].Identifier, SokidInfo^.Tradables[j].MarketId);
              end;
            end;
        end;

        TMonitorLists.InstrumentList.AddNode(Data^.FactorDoc.ContractId, NewNode);
        SetIcon(NewNode);
        if SourceIsScanner then
          Dispose(SokidInfo);
      end;
      SourceNode := aSourceTree.GetNextSelected(SourceNode, False);
    end;
    vstMonitor.Expanded[aTargetNode] := True;
    UpdateAlgos(GetParentNode(NewNode, ntAlgos));
  finally
    vstMonitor.EndUpdate;
  end;
end;

procedure TfrmMonitor.vstMonitorDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Connected;
end;

procedure TfrmMonitor.vstMonitorDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  SourceNode: PVirtualNode;
  TargetNode: PVirtualNode;
  attMode: TVTNodeAttachMode;
begin
  SourceNode := nil;
  if not Connected then
    Exit;

  if (Sender = Source) then
  begin
    TargetNode := Sender.DropTargetNode;
    SourceNode := TVirtualStringTree(Source).FocusedNode;

    if (TTreeDocument.GetDocType(vstMonitor, SourceNode) = ntOrder) and (TTreeDocument.GetDocType(vstMonitor, TargetNode) = ntOrder) then
    begin
      if (TargetNode.NextSibling = SourceNode) then
        attMode := amAddChildFirst
      else
        attMode := amInsertAfter;
    end
    else if (TTreeDocument.GetDocType(vstMonitor, SourceNode) = ntFactor) and (TTreeDocument.GetDocType(vstMonitor, TargetNode) = ntFactor) then
    begin
      if (TargetNode.NextSibling = SourceNode) then
        attMode := amInsertBefore
      else
        attMode := amInsertAfter
    end
    else
      attMode := amAddChildFirst;

    Sender.MoveTo(SourceNode, TargetNode, attMode, False);
  end
  else if Supports(TBaseVirtualTree(Source).Owner, IAutoTrade) then
  begin
    TargetNode := Sender.DropTargetNode;
    AddInstrumentFromScanner(TargetNode, TVirtualStringTree(Source));
  end
  else
  begin
    TargetNode := Sender.DropTargetNode;
    AddInstrumentFromSearch(TargetNode, TVirtualStringTree(Source));
  end;
  if Assigned(SourceNode) then
    SetIcon(SourceNode);
end;

procedure TfrmMonitor.vstMonitorDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  MoveNode: PVirtualNode;
  ToNode: PVirtualNode;
begin
  ToNode := Sender.GetNodeAt(Pt.X, Pt.Y);
  if (Source = Sender) then
  begin
    MoveNode := TVirtualStringTree(Source).FocusedNode;
    case TTreeDocument.GetDocType(vstMonitor, MoveNode) of
      ntOrderGroup:
        Accept := TTreeDocument.GetDocType(vstMonitor, ToNode) in [ntOrderGroupSet, ntOrder];
      ntFactor:
        Accept := TTreeDocument.GetDocType(vstMonitor, ToNode) in [ntOrder, ntCondition, ntAlgos, ntFactor];
      ntOrder:
        Accept := TTreeDocument.GetDocType(vstMonitor, ToNode) in [ntOrderGroup, ntOrder];
      ntAlgos:
        Accept := TTreeDocument.GetDocType(vstMonitor, ToNode) in [ntCondition, ntAlgos];
      ntCondition:
        Accept := TTreeDocument.GetDocType(vstMonitor, ToNode) in [ntCondition, ntOrder];
    else
      Accept := False;
    end;
  end
  else
    Accept := (Source is TVirtualStringTree) and
      ((ToNode = nil) or (TTreeDocument.GetDocType(vstMonitor, ToNode) in [ntOrderGroup, ntCondition, ntAlgos]));
end;

procedure TfrmMonitor.vstMonitorDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PTreeData;
  ColInfo: THeaderColInfo;
begin
  FColumns.TryGetValue(Column, ColInfo);
  if ColInfo.IsFreeze then
    TargetCanvas.Font.Color := clBlack
  else
  begin
    Data := Node^.GetData;
    case Data^.DocType of
      ntOrder:
        begin
          if (Column = COL_ITEMS) then
          begin
            if (Data^.OrderDoc.OrderAction = iabBuy) then
              TargetCanvas.Font.Color := clGreen
            else if (Data^.OrderDoc.OrderStatus = osSubmitted) then
              TargetCanvas.Font.Color := C_ORDER_SUBMITTED_COLOUR
            else
              TargetCanvas.Font.Color := clMaroon;

            if (Data^.OrderDoc.OrderStatus in [osFilled]) then
            begin
              TargetCanvas.Font.Style := [fsBold];
              if (Data^.OrderDoc.OrderAction = iabBuy) then
                TargetCanvas.Font.Color := C_ORDER_FILLED_COLOUR;
            end
            else if (Data^.OrderDoc.OrderStatus in [osPartlyFilled, osPendSubmit, osPreSubmit, osSubmitted]) then
              TargetCanvas.Font.Style := [fsBold]
            else if (Node.CheckState = csCheckedNormal) then
              TargetCanvas.Font.Style := [fsItalic];
          end
          else if (Column = COL_LAST_CLOSE) then
          begin
            if (StrToFloatDef(vstMonitor.Text[Node, Column], 0) >= 0) then
              TargetCanvas.Font.Color := clGreen
            else
              TargetCanvas.Font.Color := clRed;
          end
          else if (Column >= C_MONITOR_TREE_FIX_COLUMN) and (ColInfo.TickType = ttLast) then
            TargetCanvas.Font.Color := Data^.OrderDoc.ColTick
          else
            TargetCanvas.Font.Color := clBlack;
        end;
      ntCondition:
        ;
      ntAlgos:
        ;
      ntFactor:
        if (Column = COL_LAST_CLOSE) then
        begin
          if (StrToFloatDef(vstMonitor.Text[Node, Column], 0) >= 0) then
            TargetCanvas.Font.Color := clGreen
          else
            TargetCanvas.Font.Color := clRed;
        end
    end;
  end;
end;

procedure TfrmMonitor.vstMonitorFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PTreeData;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
  begin
    TPublishers.MonitorStructureChangePublisher.OnMonitorStructureChange(Sender, Node, crChildDeleted);
    Data^.Clear;
  end;
end;

procedure TfrmMonitor.vstMonitorGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: System.UITypes.TImageIndex);
var
  Data: PTreeData;
begin
  if (Column = COL_ITEMS) and (Kind in [ikNormal, ikSelected]) then
  begin
    Data := Node^.GetData;
    ImageIndex := Data.ImageNo;
  end;
end;

procedure TfrmMonitor.SetIcon(Node: PVirtualNode);
begin
  if Assigned(Node) then
  begin
    TTreeDocument.SetIcon(Node, vstMonitor);
    vstMonitor.InvalidateNode(Node);
  end;
end;

procedure TfrmMonitor.vstMonitorGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
resourcestring
  rsUseInAutoorder = '<Replace from AutoOrder>';
var
  NodeData: PTreeData;
  ColInfo: THeaderColInfo;

  function IsChildOrderActive(aNode: PVirtualNode): Boolean;
  var
    Child: PVirtualNode;
    Data: PTreeData;
  begin
    Result := False;
    if Assigned(aNode) then
    begin
      Data := aNode^.GetData;
      if Assigned(Data) and Assigned(Data.OrderDoc) then
      begin
        Result := (Data.OrderDoc.OrderStatus in [osCancelled, osPartlyFilled, osFilled, osPreSubmit, osSleeping]) and
                  (aNode.CheckState = csCheckedNormal);
        if Result then
          Exit;
      end;

      Child := aNode.FirstChild;
      while Assigned(Child) do
      begin
        Result := IsChildOrderActive(Child);
        if Result then
          Exit;
        Child := Child.NextSibling;
      end;
    end;
  end;

  function GetLastClose(aConId: Integer): string;
  var
    ClosePrice: Double;
  begin
    ClosePrice := GetLastPrice(aConId, ttClose);
    if (ClosePrice <> 0) then
      ClosePrice := (GetLastPrice(aConId, ttLast) / ClosePrice - 1) * 100;
    Result := Format('%0.2f', [ClosePrice]);
  end;

begin
  FColumns.TryGetValue(Column, ColInfo);
  NodeData := Node^.GetData;
  CellText := '';
  if (Column = COL_NODE_ID) then
    CellText := NodeData^.NodeId.ToString;

  case NodeData^.DocType of
    ntOrderGroupSet:
      case Column of
        COL_ITEMS:
          CellText := NodeData^.OrderGroupSetDoc.Name;
      end;
    ntOrderGroup:
      case Column of
        COL_ITEMS:
          begin
            case NodeData^.OrderGroupDoc.Kind of
              okNormal:
                CellText := '[NORM';
              okOneCancelAll:
                CellText := '[OCA';
              okSequentialContinue:
                CellText := '[SEQ';
              okSequentialStop:
                CellText := '[SEQ STOP';
              okModifyOrder:
                CellText := '[MOD';
            end;

            if NodeData^.OrderGroupDoc.IsRepetitive then
              CellText := CellText + ',REP]'
            else
              CellText := CellText + ',ONCE]';
            if NodeData^.OrderGroupDoc.IsAutoOrder then
              CellText := CellText + '[AUTO]';
            CellText := CellText + NodeData^.OrderGroupDoc.Name;
          end;
        COL_VALUE:
          begin
            if IsChildOrderActive(Node) then
              CellText := 'Active ' + NodeData^.OrderGroupDoc.ToValueString
            else
              CellText := 'Sleeping ' + NodeData^.OrderGroupDoc.ToValueString;
          end;
      end;
    ntOrder:
      case Column of
        COL_NODE_ID:
        begin
          CellText := NodeData^.NodeId.ToString;
          case NodeData^.OrderDoc.ExtendedOptions.Subordination of
            suMotherOrder:
              CellText := 'm ' + CellText;
            suChildOrder:
              CellText := 'c ' + CellText;
          end;
        end;
        COL_ITEMS:
          begin
            CellText := GetRightPadString(NodeData^.OrderDoc.InstrumentName, 25).Substring(0, 24);
            if (NodeData^.OrderDoc.BrokerType = TBrokerType.brIB) and
              (TOrderIBDoc(NodeData^.OrderDoc).SecurityType in [stOption, stFuture]) then
              CellText := GetRightPadString(TOrderIBDoc(NodeData^.OrderDoc).LocalSymbol, 20);
            CellText := CellText + ' ' + GetRightPadString(NodeData^.OrderDoc.OrderType.ToString, 12) + GetRightPadString(NodeData^.OrderDoc.OrderAction.ToString, 4);
            if (NodeData^.OrderDoc.BrokerType = TBrokerType.brIB) and (TOrderIBDoc(NodeData^.OrderDoc).Expiry > 0) then
              CellText := CellText + FormatDateTime(' YYYYMMDD', TOrderIBDoc(NodeData^.OrderDoc).Expiry);
          end;
        COL_CALCTYPE:
          CellText := '';
        COL_LAST_CLOSE:
          CellText := GetLastClose(NodeData^.OrderDoc.Id);
        COL_CALCFACTOR:
          CellText := Format('%f / %f', [GetLastPrice(NodeData^.OrderDoc.Id), GetLastPrice(NodeData^.OrderDoc.Id, ttClose)]);
        COL_VALUE:
          CellText := Format('%d / %d, %s, Avg: %f', [Trunc(NodeData^.OrderDoc.Filled), NodeData^.OrderDoc.Quantity, NodeData^.OrderDoc.OrderStatusText, NodeData^.OrderDoc.AvgPrice]);
      else
        case ColInfo.TickType of
          ttExchange, ttMarket:
            CellText := '';
          ttAskSize, ttLastSize, ttBidSize, ttVolume:
            begin
              if ColInfo.IsFreeze then
              begin
                if not NodeData^.Enabled then
                  CellText := '*' + Format('%2.0f', [NodeData^.OrderDoc.FreezeValue[ColInfo.TickType]]);
              end
              else
                CellText := Format('%2.0f', [GetLastPrice(NodeData^.OrderDoc.Id, ColInfo.TickType)]);
            end
        else
          begin
            if ColInfo.IsFreeze then
            begin
              if not NodeData^.Enabled then
                CellText := '*' + Format('%2.f', [NodeData^.OrderDoc.FreezeValue[ColInfo.TickType]]);
            end
            else
              CellText := Format('%2.f', [GetLastPrice(NodeData^.OrderDoc.Id, ColInfo.TickType)]);
          end;
        end;
      end;
    ntCondition:
      case Column of
        COL_ITEMS:
          if (NodeData^.ConditionDoc.CondType in [ctRealtimeValue, ctRealtimeAndTimeGap]) then
            CellText := NodeData^.ConditionDoc.Description + ' (' + FloatToStr(NodeData^.ConditionDoc.CondLimit) + ') / ' + NodeData^.ConditionDoc.CondType.ToString + IfThen(NodeData^.ConditionDoc.Bypass, ' [BYPASS]')
          else
            CellText := NodeData^.ConditionDoc.Description + ' / ' + NodeData^.ConditionDoc.CondType.ToString + IfThen(NodeData^.ConditionDoc.Bypass, ' [BYPASS]');
        COL_CALCTYPE:
          CellText := NodeData^.ConditionDoc.ToString;
        COL_VALUE:
          CellText := NodeData^.ConditionDoc.ToValueString;
      end;
    ntAlgos:
      case Column of
        COL_ITEMS:
          CellText := NodeData^.AlgosDoc.Name + ' ' + IntToStr(NodeData^.RecordId);
        COL_CALCTYPE:
          CellText := '';
        COL_CALCFACTOR:
          CellText := Format('%.5f', [NodeData^.ReValue]);
        COL_VALUE:
          CellText := Format('%2.f', [NodeData^.AlgosDoc.TickValue[ttLast]]);
      end;
    ntFactor:
      case Column of
        COL_ITEMS:
          if NodeData^.FactorDoc.UseInAutoOrder then
            CellText := rsUseInAutoorder
          else
          begin
            CellText := NodeData^.FactorDoc.InstrumentName;
            if (NodeData^.FactorDoc.BrokerType = TBrokerType.brIB) then
              if (NodeData^.FactorDoc.ContractType = stOption.ToString) or
                 (NodeData^.FactorDoc.ContractType = stFuture.ToString) then
                CellText := NodeData^.FactorDoc.LocalSymbol;
          end;
        COL_CALCTYPE:
          CellText := '';
        COL_LAST_CLOSE:
          CellText := GetLastClose(NodeData^.FactorDoc.ContractId);
        COL_CALCFACTOR:
          CellText := Format('%.5f', [NodeData^.ReValue]);
        COL_VALUE:
          CellText := NodeData^.FactorDoc.ToValueString;
      else
        case ColInfo.TickType of
          ttExchange:
            CellText := NodeData^.FactorDoc.Exchange;
          ttMarket:
            CellText := NodeData^.FactorDoc.MarketList;
          ttAskSize, ttLastSize, ttBidSize, ttVolume:
            begin
              if ColInfo.IsFreeze then
              begin
                if not NodeData^.Enabled then
                  CellText := '*' + Format('%2.0f', [NodeData^.FactorDoc.FreezeValue[ColInfo.TickType]]);
              end
              else
                CellText := Format('%2.0f', [GetLastPrice(NodeData^.FactorDoc.ContractId, ColInfo.TickType)]);
            end
        else
          begin
            if ColInfo.IsFreeze then
            begin
              if not NodeData^.Enabled then
                CellText := '*' + Format('%2.f', [NodeData^.FactorDoc.FreezeValue[ColInfo.TickType]]);
            end
            else
              CellText := Format('%2.f', [GetLastPrice(NodeData^.FactorDoc.ContractId, ColInfo.TickType)]);
          end;
        end;
      end;
  end;
end;

procedure TfrmMonitor.vstMonitorInitNode(Sender: TBaseVirtualTree; Parentnode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if TTreeDocument.GetDocType(vstMonitor, Node) = ntOrder then
    Node.CheckType := ctCheckBox;
end;

procedure TfrmMonitor.vstMonitorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Node: PVirtualNode;
begin
  if (Key = VK_UP) and (ssShift in Shift) then
  begin
    Node := vstMonitor.FocusedNode;
    if Assigned(Node) then
      vstMonitor.FullCollapse(Node);
  end;
end;

procedure TfrmMonitor.vstMonitorMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  NodeHeight := 16;
end;

procedure TfrmMonitor.vstMonitorStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
begin
  TPublishers.MonitorStructureChangePublisher.OnMonitorStructureChange(Sender, Node, crAccumulated);
end;

procedure TfrmMonitor.vstMonitorGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

  function GetHintString: string;
  resourcestring
    C_REPETITIVE_HINT = '(Is Repetitive)';
    C_CANCELLED_HINT  = '(Order is going to be cancelled in broker orderbook when unchecking order)';
  var
    Data: PTreeData;
  begin
    Result := '';
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if Assigned(Data) and Assigned(Data^.OrderDoc) then
      begin
        Result := Data^.OrderDoc.Quantity.ToString + ',' + Data^.OrderDoc.Description;
        if not Data^.OrderDoc.IsRepetitive then
          Result := Result + sLineBreak + C_REPETITIVE_HINT;
        if (Data^.OrderDoc.OrderIBId > 0) and (Data^.OrderDoc.OrderStatus in [osPendSubmit, osSubmitted, osPreSubmit]) then
          Result := Result + sLineBreak + C_CANCELLED_HINT;
      end;
    end;
  end;
begin
  HintText := GetHintString;
end;

procedure TfrmMonitor.PrepareConditionValues(aNode: PVirtualNode);
var
  Data       : PTreeData;
  DataFactor : PTreeData;
  FactorList : TList<PVirtualNode>;
  i          : Integer;
  Id         : Integer;
  NodeFactor : PVirtualNode;
  PriceList  : TPriceList;
  arrPrices  : TArray<TPrice>;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data) and Assigned(Data.ConditionDoc) then
    begin
      FactorList := GetListOfChilds(aNode, True, ntFactor);
      try
        for i := 0 to FactorList.Count - 1 do
        begin
          NodeFactor := FactorList[i];
          if Assigned(NodeFactor) then
          begin
            DataFactor := NodeFactor^.GetData;
            if Assigned(DataFactor) and Assigned(DataFactor.FactorDoc) then
            begin
              Id := DataFactor.FactorDoc.ContractId;
              if (Id > 0) then
              begin
                PriceList := TMonitorLists.PriceCache.GetPriceList(Id);
                if Assigned(PriceList) then
                begin
                  arrPrices := PriceList.GetLastPrices(
                    function(const aPrice: TPrice): Boolean
                    begin
                      Result := (DateOf(aPrice.TimeStamp) = DateOf(Date)) and (aPrice.TickType = ttLast);
                    end);
                  for var Price in arrPrices do
                    Data.ConditionDoc.AddToValueList(Price.TimeStamp, Price.Value, DataFactor.ReValue);
                end;
              end;
            end;
          end;
        end;
      finally
        FreeAndNil(FactorList);
      end;
    end;
  end;
end;

procedure TfrmMonitor.ShowConditionChart(aNode: PVirtualNode);
var
  Data      : PTreeData;
  Key       : TDateTime;
  SortArray : TArray<TDateTime>;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data) and Assigned(Data^.ConditionDoc) then
    begin
      if not Assigned(Data^.ConditionChart) then
      begin
        if not Data^.ConditionDoc.IsLoadedHistoricalData then
          PrepareConditionValues(aNode);
        Data^.ConditionChart := TfrmConditionChart.Create(nil);
        TfrmConditionChart(Data^.ConditionChart).InstrumentName := '[Condition] ' + Data^.ConditionDoc.Description;

        SortArray := Data^.ConditionDoc.ValueList.Keys.ToArray;
        TArray.Sort<TDateTime>(SortArray);
        for Key in SortArray do
          if (Data^.ConditionDoc.ValueList[Key].Price > 0) then
          begin
            TfrmConditionChart(Data^.ConditionChart).AddConditionValue(Key, Data^.ConditionDoc.ValueList[Key].Price);
          end;
      end;
      UpdateCondition(aNode);
      if Data^.ConditionDoc.CondType in [ctRealtimeValue, ctRealtimeAndTimeGap] then
        TfrmConditionChart(Data^.ConditionChart).Initialize(Data^.ConditionDoc.CondLimit);
      Data^.ConditionChart.Show;
    end;
  end;
end;

procedure TfrmMonitor.aShowSubscribersInfoExecute(Sender: TObject);
var
  Info: string;
begin
  Info := TDocumentInfo.GetSubscribersInformation;
  TInformationDialog.ShowMessage(Info, 'SubscribersInformation');
end;

procedure TfrmMonitor.WriteHistoricalDataToConditionDoc(const aNode: PVirtualNode);
var
  ConditionDoc   : TConditionDoc;
  Data      : PTreeData;
  Index     : Integer;
  Key       : TDateTime;
  SortArray : TArray<TDateTime>;

  function IsFactorsLoadedHistoricalData: Boolean;
  var
    DataFactor : PTreeData;
    FactorList : TList<PVirtualNode>;
    i          : Integer;
    Id         : Integer;
    NodeFactor : PVirtualNode;
  begin
    Result := True;
    FactorList := GetListOfChilds(aNode, True, ntFactor);
    try
      for i := 0 to FactorList.Count - 1 do
      begin
        NodeFactor := FactorList[i];
        if Assigned(NodeFactor) then
        begin
          DataFactor := NodeFactor^.GetData;
          if Assigned(DataFactor) and Assigned(DataFactor.FactorDoc) and (DataFactor.FactorDoc.BrokerType = TBrokerType.brIB) then
          begin
            Id := DataFactor.FactorDoc.ContractId;
            Result := Result and (FControlHistData.IndexOf(Id.ToString) > -1);
            if not Result then
              Exit(False);
          end;
        end;
      end;
    finally
      FreeAndNil(FactorList);
    end;
  end;

begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data) then
      if Assigned(Data.ConditionDoc) then
        if (not Data.ConditionDoc.IsLoadedHistoricalData) then
        begin
          ConditionDoc := Data.ConditionDoc;
          if IsFactorsLoadedHistoricalData then
          begin
            PrepareConditionValues(aNode);
            ConditionDoc.IsLoadedHistoricalData := True;
            ConditionDoc.SetLengthPriceArray(ConditionDoc.ValueList.Count);
            Index := 0;
            SortArray := ConditionDoc.ValueList.Keys.ToArray;
            TArray.Sort<TDateTime>(SortArray);
            for Key in SortArray do
              if (ConditionDoc.ValueList[Key].Price > 0.01) then
              begin
                Data^.ConditionDoc.PriceArray[Index].TimeStamp := Key;
                Data^.ConditionDoc.PriceArray[Index].Price := ConditionDoc.ValueList[Key].Price;
                Inc(Index);
              end;
          end;
        end;
  end;
end;

function TfrmMonitor.IsAutoorderGroup(aNode: PVirtualNode): Boolean;
resourcestring
  rsOrderGroupName = 'Auto order group';
var
  ParentNode: PVirtualNode;
  OrderGroup: PVirtualNode;
begin
  Result := False;
  if (TTreeDocument.GetDocType(vstMonitor, aNode) = ntOrderGroup) then
    ParentNode := aNode
  else
    ParentNode := GetParentNode(aNode, ntOrderGroup);
  if Assigned(ParentNode) then
  begin
    OrderGroup := GetNodeByName(rsOrderGroupName);
    Result := (ParentNode = OrderGroup);
  end;
end;

procedure TfrmMonitor.SaveToDBTreeElement(const aGroupName: string = '');
var
  Data: PTreeData;
begin
  if not vstMonitor.IsEmpty then
  begin
    Data := nil;
    if not aGroupName.IsEmpty then
      FOrderGroupSet.Name := aGroupName;
    if Assigned(FOrderGroupSet.OwnerNode) then
    begin
      Data := FOrderGroupSet.OwnerNode^.GetData;
      Data^.OrderGroupSetDoc.Name := FOrderGroupSet.Name;
    end;
    TTreeDocument.DeleteRelations(FOrderGroupSet.RecordId);
    TTreeDocument.SaveRelationTree(vstMonitor, vstMonitor.GetFirst, FOrderGroupSet.RecordId <= 0);
    if Assigned(Data) then
      FOrderGroupSet.FromDB(Data^.RecordId);

    vstMonitor.Invalidate;
    SetCurrentGroupCaptions;
  end;
end;

function TfrmMonitor.GetNodeByName(aName: string): PVirtualNode;
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Result := nil;
  Node := vstMonitor.GetFirst();
  while Assigned(Node) do
  begin
    Data := Node^.GetData;
    case Data.DocType of
      ntOrderGroup:
        if Assigned(Data.OrderGroupDoc) and (Data.OrderGroupDoc.Name = aName) then
          Result := Node;
      ntOrder:
        if Assigned(Data.OrderDoc) and (Data.OrderDoc.Description = aName) then
          Result := Node;
      ntCondition:
        if Assigned(Data.ConditionDoc) and (Data.ConditionDoc.Description = aName) then
          Result := Node;
      ntAlgos:
        if Assigned(Data.AlgosDoc) and (Data.AlgosDoc.Name = aName) then
          Result := Node;
      ntFactor:
        if Assigned(Data.FactorDoc) and (Data.FactorDoc.InstrumentName = aName) then
          Result := Node;
    end;
    if Assigned(Result) then
      Exit;
    Node := vstMonitor.GetNext(Node);
  end;
end;

function TfrmMonitor.GetNodeByRecordID(const aNode: PVirtualNode; aRecordID: Integer): PVirtualNode;
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Result := nil;
  if Assigned(aNode) then
  begin
    Node := aNode.FirstChild;
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      if (Data^.RecordId = aRecordID) then
        Result := Node
      else
        Result := GetNodeByRecordID(Node, aRecordID);
      if Assigned(Result) then
        Exit;
      Node := Node.NextSibling;
    end;
  end;
end;

function TfrmMonitor.GetCurrentOrderGroupSet: PVirtualNode;
begin
  if vstMonitor.IsEmpty or not Assigned(FOrderGroupSet.OwnerNode) then
  begin
    FOrderGroupSet.OwnerNode := TTreeDocument.CreateOrderGroupSet(nil, vstMonitor);
    SetCurrentGroupCaptions;
  end;
  Result := FOrderGroupSet.OwnerNode;
end;

function TfrmMonitor.GetLastPrice(const aId: Integer; const aTickType: TIABTickType = ttLast): Double;
begin
  Result := TPriceCache.PriceCache.GetLastPrice(aId, aTickType);
end;

function TfrmMonitor.GetLevelName(aNode: PVirtualNode): string;
var
  Data: PTreeData;
begin
  Result := '';
  if not Assigned(aNode) then
    Exit;
  Data := aNode^.GetData;
  if Assigned(Data) then
  begin
    if Assigned(Data.OrderGroupDoc) then
      Result := 'ORDERGROUP - ' + Data.OrderGroupDoc.Name
    else if Assigned(Data.OrderDoc) then
      Result := 'ORDER - ' + Data.OrderDoc.Description
    else if Assigned(Data.ConditionDoc) then
      Result := 'CONDITION - ' + Data.ConditionDoc.Description
    else if Assigned(Data.AlgosDoc) then
      Result := 'ALGOS - ' + Data.AlgosDoc.Name
    else if Assigned(Data.FactorDoc) then
      Result := 'FACTOR - ' + Data.FactorDoc.InstrumentName;
  end;
end;

function TfrmMonitor.GetListOfChilds(aNode: PVirtualNode; aIsRecursive: Boolean; aDocType: TDocType): TList<PVirtualNode>;

  procedure GetChilds(aNode: PVirtualNode);
  var
    Child: PVirtualNode;
    Data: PTreeData;
  begin
    if Assigned(aNode) then
    begin
      Data := aNode^.GetData;
      if Assigned(Data) and (Data.DocType = aDocType) then
        Result.Add(aNode);

      Child := aNode.FirstChild;
      while Assigned(Child) do
      begin
        Data := Child^.GetData;
        if aIsRecursive then
          GetChilds(Child)
        else if Assigned(Data) and (Data.DocType = aDocType) then
          Result.Add(Child);
        Child := Child.NextSibling;
      end;
    end;
  end;

begin
  Result := TList<PVirtualNode>.Create;
  GetChilds(aNode);
end;

function TfrmMonitor.GetMainTree: TVirtualStringTree;
begin
  Result := vstMonitor;
end;

procedure TfrmMonitor.SetCurrentGroupCaptions;
begin
  if not Assigned(FOrderGroupSet.OwnerNode) then
    edCurrentGroupName.Text := ''
  else
    edCurrentGroupName.Text := FOrderGroupSet.Name;
end;

function TfrmMonitor.GetDuplicateOrderIB(const aNode: PVirtualNode): PVirtualNode;
var
  NewData   : PTreeData;
  NewNode   : PVirtualNode;
  NewOrder  : TOrderIBDoc;
  OldData   : PTreeData;
  OldOrder  : TOrderIBDoc;
  SokidInfo : TSokidInfo;
begin
  Result := nil;
  NewData := nil;
  if Assigned(aNode) then
  begin
    if IsAutoorderGroup(aNode) and not General.AllowEnterAutoorder then
      Exit;
    OldData := aNode^.GetData;
    if Assigned(OldData.OrderDoc) and (OldData.OrderDoc is TOrderIBDoc) then
    begin
      OldOrder := TOrderIBDoc(OldData.OrderDoc);
      NewOrder := TOrderIBDoc.Create;
      try
        NewOrder.AssignFrom(OldOrder);

        SokidInfo := SokidList.GetItem(NewOrder.Id);
        if (NewOrder.Symbol.IsEmpty) then
          NewOrder.Symbol := SokidInfo.Symbol;
        if (NewOrder.Exchange.IsEmpty) then
          NewOrder.Exchange := SokidInfo.Exchange;
        if (NewOrder.PrimaryExchange.IsEmpty) then
          NewOrder.PrimaryExchange := SokidInfo.PrimaryExchange;
        if (NewOrder.Currency = '') then
          NewOrder.Currency := SokidInfo.Currency;
        if (NewOrder.MarketList = '') then
          NewOrder.MarketList := SokidInfo.MarketRuleIds;
        NewOrder.Expiry := SokidInfo.Expiry;
        NewOrder.Multiplier := StrToIntDef(SokidInfo.Multiplier, 0);

        NewNode := vstMonitor.AddChild(aNode.Parent);
        NewOrder.OwnerNode := NewNode;
        NewData := NewNode^.GetData;
        NewData^.Clear;
        NewData.DocType    := ntOrder;
        NewData.OrderDoc   := NewOrder;
        NewNode.CheckType  := ctCheckBox;
        NewOrder.LastPrice := GetLastPrice(NewOrder.Id);
        SetIcon(NewNode);
        TMonitorLists.OrderList.AddOrder(NewNode);
        TMonitorLists.InstrumentList.AddNode(NewOrder.Id, NewNode);
        vstMonitor.MoveTo(NewNode, aNode, amInsertAfter, False);
        Result := NewNode;
        TestOrder(NewNode);
      finally
        if not Assigned(NewData) then
          FreeAndNil(NewOrder);
      end;
    end;
  end;
end;

function TfrmMonitor.GetDuplicateOrderNN(const aNode: PVirtualNode): PVirtualNode;
var
  NewData   : PTreeData;
  NewNode   : PVirtualNode;
  NewOrder  : TOrderNNDoc;
  OldData   : PTreeData;
  OldOrder  : TOrderNNDoc;
  SokidInfo : TSokidInfo;
begin
  Result := nil;
  NewData := nil;
  if Assigned(aNode) then
  begin
    if IsAutoorderGroup(aNode) and not General.AllowEnterAutoorder then
      Exit;
    OldData := aNode^.GetData;
    if Assigned(OldData.OrderDoc) and (OldData.OrderDoc is TOrderNNDoc) then
    begin
      OldOrder := TOrderNNDoc(OldData.OrderDoc);
      NewOrder := TOrderNNDoc.Create;
      try
        NewOrder.AssignFrom(OldOrder);
        if (NewOrder.ChildId > 0) and (NewOrder.FeedFromBroker = 0) then
        begin
          SokidInfo := SokidList.GetItem(NewOrder.ChildId );
          if (NewOrder.Symbol.IsEmpty) then
            NewOrder.Symbol := SokidInfo.Symbol;
          if (NewOrder.Exchange.IsEmpty) then
            NewOrder.Exchange := SokidInfo.Exchange;
          if (NewOrder.PrimaryExchange.IsEmpty) then
            NewOrder.PrimaryExchange := SokidInfo.PrimaryExchange;
          if (NewOrder.Currency.IsEmpty) then
            NewOrder.Currency := SokidInfo.Currency;
          if (NewOrder.MarketList.IsEmpty) then
            NewOrder.MarketList := SokidInfo.MarketRuleIds;
          NewOrder.Expiry := SokidInfo.Expiry;
          NewOrder.Multiplier := StrToIntDef(SokidInfo.Multiplier, 0);
        end;

        NewOrder.LastPrice := GetLastPrice(NewOrder.Id);
        NewNode := vstMonitor.AddChild(aNode.Parent);
        NewOrder.OwnerNode := NewNode;
        NewData := NewNode^.GetData;
        NewData^.Clear;
        NewData^.DocType  := ntOrder;
        NewData^.OrderDoc := NewOrder;
        NewNode.CheckType := ctCheckBox;
        SetIcon(NewNode);
        TMonitorLists.OrderList.AddOrder(NewNode);
        TMonitorLists.InstrumentList.AddNode(NewOrder.Id, NewNode);
        if (NewOrder.ChildId > 0) then
          TMonitorLists.InstrumentChildList.AddNode(NewOrder.ChildId, NewNode);
        vstMonitor.MoveTo(NewNode, aNode, amInsertAfter, False);
        Result := NewNode;
        TestOrder(NewNode);
      finally
        if not Assigned(NewData) then
          FreeAndNil(NewOrder);
      end;
    end;
  end;
end;

function TfrmMonitor.GetFocusedNode: PVirtualNode;
begin
  Result := vstMonitor.FocusedNode;
end;

function TfrmMonitor.GetDockControl: TWinControl;
begin
  Result := pgcMain;
end;

function TfrmMonitor.GetDuplicateAlgos(const aNode: PVirtualNode): PVirtualNode;
var
  NewAlgos : TAlgosDoc;
  NewData  : PTreeData;
  NewNode  : PVirtualNode;
  OldAlgos : TAlgosDoc;
  OldData  : PTreeData;
  TickType : TIABTickType;
begin
  Result := nil;
  NewData := nil;
  if Assigned(aNode) then
  begin
    if IsAutoorderGroup(aNode) and not General.AllowEnterAutoorder then
      Exit;
    OldData := aNode^.GetData;
    if Assigned(OldData.AlgosDoc) then
    begin
      OldAlgos := OldData.AlgosDoc;
      NewAlgos := TAlgosDoc.Create;
      try
        NewAlgos.AssignFrom(OldAlgos);

        NewNode := vstMonitor.AddChild(aNode.Parent);
        NewData := NewNode^.GetData;
        NewData^.Clear;
        NewAlgos.OwnerNode := NewNode;

        for TickType := System.Low(TIABTickType) to System.High(TIABTickType) do
          NewAlgos.TickValue[TickType] := OldAlgos.TickValue[TickType];
        NewData.DocType   := ntAlgos;
        NewData.AlgosDoc  := NewAlgos;
        NewNode.CheckType := ctNone;
        SetIcon(NewNode);
        TMonitorLists.AlgosList.AddItem(NewNode);
        vstMonitor.MoveTo(NewNode, aNode, amInsertAfter, False);
        AddAlgosToConditionAlgosChart(NewNode);
        AddAlgosToRealtimeChart(nil, NewNode);
        Result := NewNode;
        UpdateAlgos(NewNode);
      finally
        if not Assigned(NewData) then
          FreeAndNil(NewAlgos);
      end;
    end;
  end;
end;

function TfrmMonitor.GetDuplicateCondition(const aNode: PVirtualNode): PVirtualNode;
var
  NewData: PTreeData;
  NewNode: PVirtualNode;
  NewCond: TConditionDoc;
  OldData: PTreeData;
  OldCond: TConditionDoc;
  TickType: TIABTickType;
begin
  Result := nil;
  NewData := nil;
  if Assigned(aNode) then
  begin
    if IsAutoorderGroup(aNode) and not General.AllowEnterAutoorder then
      Exit;
    OldData := aNode^.GetData;
    if Assigned(OldData.ConditionDoc) then
    begin
      OldCond := OldData.ConditionDoc;
      NewCond := TConditionDoc.Create;
      try
        NewCond.AssignFrom(OldCond);
        NewCond.IsCondition := OldCond.Bypass;
        NewNode := vstMonitor.AddChild(aNode.Parent);
        NewCond.OwnerNode := NewNode;
        NewData := NewNode^.GetData;
        NewData^.Clear;

        for TickType := System.Low(TIABTickType) to System.High(TIABTickType) do
          NewCond.TickValue[TickType] := OldCond.TickValue[TickType];
        NewData^.DocType      := ntCondition;
        NewData^.ConditionDoc := NewCond;
        NewNode.CheckType     := ctNone;
        SetIcon(NewNode);
        vstMonitor.MoveTo(NewNode, aNode, amInsertAfter, False);
        Result := NewNode;
        AddConditionToRealtimeChart(NewNode);
      finally
        if not Assigned(NewData) then
          FreeAndNil(NewCond);
      end;
    end;
  end;
end;

procedure TfrmMonitor.btnTestingStartClick(Sender: TObject);
var
  Broker: TBrokerHelperClass;
begin
//  if rbTest.Checked then
  begin
    Broker := TBrokerHelperFactory.GetBrokerHelperClass(TBrokerType.brTest.ToString);
    Broker.Leave;
    Broker.SetDate(Date);
    Broker.SetUpdatePrice(UpdatePriceForTB);
  end;
end;

function TfrmMonitor.GetCONIDforNN(ASymbol, AType, AExchange, ACurrency: string): integer;
var
  sqlstr: string;
begin
  sqlstr := 'SELECT CONID FROM MAPPINGNNTOIB M ' +
            'JOIN  SOKID_IB S ON UPPER(M.SymbolNameIB) = UPPER(S.SYMBOL) AND UPPER(M.TypeIB) = UPPER(S.CONTRACTYPE) AND UPPER(M.ExchangeIB) = UPPER(S.EXCHANGE) ' +
            'WHERE UPPER(M.SymbolNameNN) = ' + QuotedStr(UpperCase(ASymbol)) +
            ' and UPPER(M.TypeNN) = ' + QuotedStr(UpperCase(AType)) +
            ' and UPPER(S.CURRENCY) = ' + QuotedStr(UpperCase(ACurrency));

  Result := DMod.GetIntegerValueFromSQL(sqlstr, 'CONID');
end;

procedure TfrmMonitor.SetConnected(const Value: Boolean);
begin
  FConnected := Value;
end;

function TfrmMonitor.GetConnectedIB: Boolean;
begin
  Result := IABClient.Connected;
end;

procedure TfrmMonitor.SetConnectedIB(const Value: Boolean);
begin
  if not(csDestroying in Self.ComponentState) then
  begin
    if not General.XMLFile.ReadBool(TGeneral.C_SECTION_COMMON_PARAMS, TGeneral.C_KEY_USE_IB, Value) then
      IABClient.Connected := False
    else if (IABClient.Connected <> Value) then
      IABClient.Connected := Value;
  end;
end;

procedure TfrmMonitor.SetConnectedNN(const Value: Boolean);
var
  Rect: TRect;
  Connect: Boolean;
begin
  if not (csDestroying in Self.ComponentState) then
  begin
    Connect := Value;
    if not General.XMLFile.ReadBool(TGeneral.C_SECTION_COMMON_PARAMS, TGeneral.C_KEY_USE_NN, Value) then
      Connect := False;
    FConnectedNN := Connect;
    if not Value then
      RecieveDataNN := Connect;
    Rect.Top    := 2;
    Rect.Left   := 182;
    Rect.Right  := Rect.Left + sbMain.Panels[3].Width - 2;
    Rect.Height := sbMain.Height - 1;
    TThread.Queue(nil,
      procedure()
      begin
        sbMainDrawPanel(sbMain, sbMain.Panels[3], Rect)
      end);
  end;
end;

procedure TfrmMonitor.SetRecieveDataIB(const Value: Boolean);
var
  Rect: TRect;
begin
  if not(csDestroying in Self.ComponentState) then
    if (Value <> FRecieveDataIB) then
    begin
      FRecieveDataIB := Value;
      Rect.Top    := 2;
      Rect.Left   := 83;
      Rect.Right  := Rect.Left + sbMain.Panels[1].Width - 2;
      Rect.Height := sbMain.Height - 1;
      TThread.Queue(nil,
        procedure()
        begin
          sbMainDrawPanel(sbMain, sbMain.Panels[1], Rect)
        end);
    end;
end;

procedure TfrmMonitor.SetRecieveDataNN(const Value: Boolean);
var
  Rect: TRect;
begin
  if not(csDestroying in Self.ComponentState) then
//    if (Value <> FRecieveDataNN) then
    begin
      FRecieveDataNN := Value;
      Rect.Top    := 2;
      Rect.Left   := 273;
      Rect.Right  := Rect.Left + sbMain.Panels[4].Width - 2;
      Rect.Height := sbMain.Height - 1;
      TThread.Queue(nil,
        procedure()
        begin
          sbMainDrawPanel(sbMain, sbMain.Panels[4], Rect)
        end);
    end;
end;

procedure TfrmMonitor.OnCloseFeedNN;
begin
  RecieveDataNN := False;
end;

procedure TfrmMonitor.OnOpenFeedNN;
begin
  RecieveDataNN := True;
end;

procedure TfrmMonitor.OnCurrentTime(Sender: TObject; DateTime: TDateTime);
begin
  TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnCurrentTime', 'TWS time -' + FormatDateTime('hh:mm:ss.zzz', DateTime));
end;

procedure TfrmMonitor.OnConnectionState(Sender: TObject; State: TIABConnection);
var
  s: string;
  Rect: TRect;
begin
  case State of
    twsClosed:
      begin
        s := 'TWS connection closed';
        RecieveDataIB := False;
        ConnectedIB := False;
        ShowNotification(s, 'ConnectionState');
      end;
    twsConnecting:
      s := 'Connecting to TWS';
    twsReady:
      begin
        RecieveDataIB := True;
        ConnectedIB := True;
        s := 'TWS connection Ready... Server time: ' + IABClient.ConnectAtServerTime +
             ', TWS API server version: ' + IABClient.ServerVersion.ToString;
        TTask.Create(
          procedure()
          begin
            Sleep(500);
            TThread.Synchronize(nil,
              procedure
              begin
                SubscribeIBFeeds;
                SubscribeCurrency;
                //SokidList.CheckLastPrice;
              end);
          end).Start;
      end;
    twsFailed:
      begin
        s := 'TWS connection Failed';
        RecieveDataIB := False;
        ConnectedIB := False;
        ShowNotification(s, 'ConnectionState');
      end;
  end;
  TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnConnectionState', s);

  Rect.Top    := 2;
  Rect.Left   := 2;
  Rect.Right  := sbMain.Panels[0].Width - 1;
  Rect.Height := sbMain.Height - 1;
  TThread.Queue(nil,
    procedure()
    begin
      sbMainDrawPanel(sbMain, sbMain.Panels[0], Rect);
    end);

  TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'OnConnectionState', s);
end;

procedure TfrmMonitor.cbAccountsChange(Sender: TObject);
begin
  if (cbAccounts.ItemIndex > -1) then
    General.IsRealTrades := not cbAccounts.Items[cbAccounts.ItemIndex].StartsWith('DU')
  else
    General.IsRealTrades := False;
  SetRealTradesText;
end;

procedure TfrmMonitor.OnManagedAccounts(Sender: TObject; Details: string);
var
  DataId: Integer;
  Request: TIABRequest;
begin
  if (cbAccounts.Items.IndexOf(Details) < 0) then
  begin
    cbAccounts.Items.Add(Details);
    Request := Default(TIABRequest);
    Request.Command := ibGetAccountUpdates;
    Request.Details := Details;
    IABClient.SendRequest(Request);
    if (cbAccounts.Items.Count > 0) then
    begin
      cbAccounts.ItemIndex := 0;
      cbAccountsChange(nil)
    end;
  end;

  if Assigned(frmDockFormAccountPnL) then
  begin
    DataId := General.GetNextID;
    Request := Default(TIABRequest);
    Request.Command := ibRequestPnL;
    Request.Details := Details;
    Request.DataId := DataId;
    IABClient.SendRequest(Request);
    frmDockFormAccountPnL.Account := Details;
    frmDockFormAccountPnL.DataId  := DataId;
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'OnManagedAccounts', 'Details=' + Details + ', DataId=' + DataId.ToString);
  end
  else
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, Self, 'OnManagedAccounts', 'Form frmAccountPnL is not assigned');
end;

procedure TfrmMonitor.OnProfitLoss(Sender: TObject; DataId: Integer; DailyPnL, UnrealizedPnL, RealizedPnL: Double);
begin
  if Assigned(frmDockFormAccountPnL) then
    frmDockFormAccountPnL.ProfitLoss(DataId, DailyPnL, UnrealizedPnL);
end;

procedure TfrmMonitor.OnQueueOverflow(Sender: TObject; Count: Integer);
begin
  TThread.Queue(nil,
    procedure
    begin
      sbMain.Panels[9].Text := Count.ToString;
    end);
end;

procedure TfrmMonitor.OnRebuildFromTWS(Sender: TObject);
begin
//
end;

procedure TfrmMonitor.OnTradeOrder(const Sender: TNordNet; const TradeOrder: TTradeOrder);
var
  Data     : PTreeData;
  Node     : PVirtualNode;
  Quantity : Double;
  Info     : string;
begin
  if (TradeOrder.order_id > 0) then
  begin
    Info := NordNetBroker.TradeOrderToStr(TradeOrder);
    Node  := TMonitorLists.OrderList.GetNodeOrder(TradeOrder.order_id);
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if Assigned(Data) and Assigned(Data^.OrderDoc) and (Data^.OrderDoc is TOrderNNDoc) then
      begin
        Quantity := Data^.OrderDoc.Quantity - TradeOrder.volume;
        Data^.OrderDoc.Filled := Trunc(TradeOrder.volume);
        Data^.OrderDoc.TradeTime := StrToDateTimeDef(TradeOrder.tradetime, Now);
        Data^.OrderDoc.LatestFillPrice := TradeOrder.price.value;
        if (TradeOrder.price.value > 0) then
          Data^.OrderDoc.AddPrice(TradeOrder.price.value, Trunc(TradeOrder.volume));
        TMonitorLists.OrderList.AddStatus(TradeOrder.order_id,
                                          TradeOrder.volume,
                                          Quantity,
                                          Info,
                                          'OrderTrade',
                                          -1,                 //ErrorCode
                                          osFilled,
                                          TradeOrder.price.value);

        if (TOrderNNDoc(Data^.OrderDoc).OrderType in [otComboMarket, otComboLimit]) and
           (Data^.OrderDoc.ExtendedOptions.Subordination = suMotherOrder) and (TradeOrder.volume > 0) then
        begin
          if Data^.OrderDoc.IsActivateChild or
             (not Data^.OrderDoc.IsActivateChild and (Data^.OrderDoc.Quantity = TradeOrder.volume)) then
          CreateOrderNNStructure(Node, Trunc(TradeOrder.volume));
        end;
      end;
    end;

    TPublishers.OrderStatePublisher.OnCloseOrder(TradeOrder.order_id);
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'OnTradeOrder', Info.Replace(',', '<br>'));
  end;
end;

procedure TfrmMonitor.OnUpdateOrder(const Sender: TNordNet; const OrderItem: TOrder);
var
  Data       : PTreeData;
  Node       : PVirtualNode;
  OrdersList : TArray<TOrder>;
  Info       : string;
  Status     : TIABOrderState;
begin
  if (OrderItem.order_id > 0) then
  begin
    Status := TOrderNNDoc.ActionStateToOrderState(OrderItem.action_state);
    Info   := NordNetBroker.OrderToStr(OrderItem);
    Node   := TMonitorLists.OrderList.GetNodeOrder(OrderItem.order_id);
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if Assigned(Data) and Assigned(Data.OrderDoc) and (Data.OrderDoc is TOrderNNDoc) then
      begin
        if (Status in [osFilled, osPartlyFilled]) then //and status
          Data.OrderDoc.Filled := Trunc(Data.OrderDoc.Quantity - OrderItem.traded_volume);
        TMonitorLists.OrderList.AddStatus(OrderItem.order_id,
                                          OrderItem.traded_volume,
                                          Data.OrderDoc.Quantity - OrderItem.traded_volume,
                                          Info,
                                          'OrderStatus',
                                          -1,                 //ErrorCode
                                          Status,
                                          OrderItem.price.value);
      end;
    end;

    SetLength(OrdersList, 1);
    OrdersList[0] := OrderItem;
    TPublishers.OrderStatePublisher.OnOpenOrderNN(OrdersList);

    if (Status in [osCancelled, osFilled, osPartlyFilled, osError]) then
      TPublishers.OrderStatePublisher.OnCloseOrder(OrderItem.order_id);
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'OnUpdateOrder', Info.Replace(',', '<br>'));
  end;
end;

procedure TfrmMonitor.SellContractPosition(const aSymbol, aCurrency: string; aQuantity, aInstrumentId: Integer; aSecType: TIABSecurityType; aAction: TIABAction);
resourcestring
  rsOrderGroupName = 'Group contract position';
var
  Data       : PTreeData;
  Order      : TOrderIBDoc;
  OrderGroup : PVirtualNode;
  OrderNode  : PVirtualNode;
  SokidInfo  : TSokidInfo;
begin
  OrderGroup := GetNodeByName(rsOrderGroupName);
  if not Assigned(OrderGroup) then
  begin
    OrderGroup := TTreeDocument.CreateOrderGroup(GetCurrentOrderGroupSet, vstMonitor);
    Data := OrderGroup^.GetData;
    Data.OrderGroupDoc.Name := rsOrderGroupName;
    SetIcon(OrderGroup);
  end;

  OrderNode := vstMonitor.AddChild(OrderGroup);
  OrderNode.CheckType := ctCheckBox;

  Order := TOrderIBDoc.Create;
  try
    Order.BrokerType     := TBrokerType.brIB;
    Order.Currency       := aCurrency;
    Order.Description    := 'Sell position ' + aSymbol;
    Order.OrderAction    := aAction;
    Order.SecurityType   := aSecType;
    Order.TimeInForce    := Ord(tifDay);
    Order.Symbol         := aSymbol;
    Order.Quantity       := aQuantity;
    Order.Id             := aInstrumentId;

    SokidInfo := SokidList.GetItem(aInstrumentId);
    if not (SokidInfo.GetSecurityType in [stFuture]) then
      SokidInfo.MinimumTick := 0;
    Order.Exchange        := SokidInfo.Exchange;
    Order.PrimaryExchange := SokidInfo.PrimaryExchange;
    Order.InstrumentName  := SokidInfo.Name;
    Order.Multiplier      := StrToIntDef(SokidInfo.Multiplier, 0);
    Order.LastPrice       := GetLastPrice(Order.Id);

    if (General.EmergencySettings.OrderType = otMarket) then
      Order.OrderType := otMarket
    else
    begin
      Order.OrderType := otLimit;
      Order.Limit     := GetLastPrice(Order.Id);
      case aAction of
        iabSell:
          Order.Limit := IABClient.MarketRuleList.RoundToMinTick(Order.Limit - Order.Limit * General.EmergencySettings.Percent / 100, Order.MarketList, SokidInfo.MinimumTick);
        iabBuy:
          Order.Limit := IABClient.MarketRuleList.RoundToMinTick(Order.Limit + Order.Limit * General.EmergencySettings.Percent / 100, Order.MarketList, SokidInfo.MinimumTick);
      end;
    end;

    Data := OrderNode^.GetData;
    Data^.Clear;
    Data^.OrderDoc := Order;
    Data^.DocType  := ntOrder;
    Data^.OrderDoc.OwnerNode := OrderNode;

    TMonitorLists.OrderList.AddOrder(OrderNode);
    TMonitorLists.InstrumentList.AddNode(Data^.OrderDoc.Id, OrderNode);
    if (Data^.OrderDoc.LastPrice > 0) then
      TMonitorLists.InstrumentList.DocumentQueue.DeleteItem(OrderNode);

    SetIcon(OrderNode);
    vstMonitor.MoveTo(OrderNode, OrderNode.Parent.LastChild, amInsertAfter, False);
    OrderNode.CheckState := csCheckedNormal;
  finally
    Order.Buy;
  end;
end;

procedure TfrmMonitor.CreateOrderNNStructure(const aParentNode: PVirtualNode; aTradedVolume: Integer);
var
  ChildData      : PTreeData;
  ChildNode      : PVirtualNode;
  ChildOrder     : TOrderNNDoc;
  i              : Integer;
  InstrumentName : string;
  Instruments    : TInstruments;
  ParentData     : PTreeData;
  ParentOrder    : TOrderNNDoc;

  procedure CreateChildOrder;
  begin
    ChildData := nil;
    ChildOrder := TOrderNNDoc.Create;
    try
      ChildOrder.AssignFrom(ParentOrder);
      ChildOrder.IsActivateChild := False;
      ChildOrder.FeedFromBroker  := 1;
      ChildOrder.Info            := '';
      ChildOrder.LimitType       := ltRelative;
      ChildOrder.MarketList      := ParentOrder.MarketList;
      ChildOrder.OpenVolume      := 100;
      ChildOrder.OrderType       := ParentOrder.TrailOrderSendType;
      ChildOrder.Quantity        := aTradedVolume;
      ChildOrder.LastPrice       := GetLastPrice(ChildOrder.Id);

      if (ParentOrder.OrderAction = iabBuy) then
        ChildOrder.OrderAction := iabSell
      else
        ChildOrder.OrderAction := iabBuy;


      ChildNode := vstMonitor.AddChild(aParentNode);
      ChildData := ChildNode^.GetData;
      ChildData^.Clear;
      ChildOrder.OwnerNode := ChildNode;
      ChildData^.DocType   := ntOrder;
      ChildData^.OrderDoc  := ChildOrder;
      ChildNode.CheckType  := ctCheckBox;
      SetIcon(ChildNode);

      ChildNode.CheckState := csCheckedNormal;
      vstMonitor.InvalidateNode(ChildNode);
      TMonitorLists.OrderList.AddOrder(ChildNode);
      TMonitorLists.InstrumentList.AddNode(ChildOrder.Id, ChildNode);
      if (ChildOrder.ChildId > 0) then
        TMonitorLists.InstrumentChildList.AddNode(ChildOrder.ChildId, ChildNode);
    finally
      if not Assigned(ChildData) then
        FreeAndNil(ChildOrder);
    end;
  end;

  procedure CreateChildNodes(aKindCreation: TConditionDoc.TKindCreation);
  var
    DataCond   : PTreeData;
    DataFactor : PTreeData;
    NodeCond   : PVirtualNode;
    NodeFactor : PVirtualNode;
  begin
    NodeCond := TTreeDocument.CreateCondition(ChildNode, vstMonitor);
    DataCond := NodeCond^.GetData;
    DataCond^.ConditionDoc.IsCondition  := False;
    DataCond^.ConditionDoc.Priority     := cpPriority;
    DataCond^.ConditionDoc.CondType     := ctRealtimeValue;
    DataCond^.ConditionDoc.KindCreation := aKindCreation;
    case aKindCreation of
      kcTrigger:
        begin
          DataCond^.ConditionDoc.Description := 'Trail';
          if (ParentOrder.OrderAction = iabSell) then
          begin
            DataCond^.ConditionDoc.CondLimit := ParentOrder.ValueArray[TOrderNNDoc.TCurrentValue.cvMinPayed] + ParentOrder.TrailTriggerSendSell;
//            DataCond^.ConditionDoc.IsLessRT  := False;
          end
          else
          begin
            DataCond^.ConditionDoc.CondLimit := ParentOrder.ValueArray[TOrderNNDoc.TCurrentValue.cvMaxPayed] + ParentOrder.TrailTriggerSendSell;
//            DataCond^.ConditionDoc.IsLessRT  := True;
          end;
        end;
      kcStopLoss:
        begin
          if (ParentOrder.OrderAction = iabBuy) then
          begin
            DataCond^.ConditionDoc.Description := 'Floor';
//            DataCond^.ConditionDoc.IsLessRT    := True;
          end
          else
          begin
            DataCond^.ConditionDoc.Description := 'Roof';
//            DataCond^.ConditionDoc.IsLessRT    := False;
          end;
          DataCond^.ConditionDoc.CondLimit := ParentOrder.ValueArray[TOrderNNDoc.TCurrentValue.cvEntryPrice] + ParentOrder.ConditionStopLoss;
        end;
      kcReached:
        begin
          DataCond^.ConditionDoc.Description := 'Reached';
          DataCond^.ConditionDoc.CondLimit   := ParentOrder.ValueArray[TOrderNNDoc.TCurrentValue.cvEntryPrice] + ParentOrder.ConditionReached;
//          DataCond^.ConditionDoc.IsLessRT    := ParentOrder.OrderAction <> iabBuy;
        end;
    end;
    DataCond^.CreationType := ctProgramm;
    NodeCond.CheckType     := ctNone;
    SetIcon(NodeCond);

    NodeFactor := TTreeDocument.CreateFactor(DataCond^.ConditionDoc.OwnerNode, vstMonitor);
    NodeFactor.CheckType := ctNone;
    DataFactor := NodeFactor^.GetData;
    DataFactor^.FactorDoc.Currency       := ParentOrder.Currency;
    DataFactor^.FactorDoc.Decimals       := ParentOrder.Decimals;
    DataFactor^.FactorDoc.IBID           := ParentOrder.ChildId;
    DataFactor^.FactorDoc.NNId           := ParentOrder.Id;
    DataFactor^.FactorDoc.IsIn           := ParentOrder.IsIn;
    DataFactor^.FactorDoc.OwnerNode      := NodeFactor;
    DataFactor^.FactorDoc.TickType1      := ttLast;
    DataFactor^.FactorDoc.TickType2      := ttNotSet;
    DataFactor^.FactorDoc.InstrumentName := InstrumentName;
    DataFactor^.FactorDoc.IdentifierList := ParentOrder.ChildIdentifierList;
    DataFactor^.FactorDoc.MarketList     := ParentOrder.ChildMarketList;
    DataFactor^.FactorDoc.Symbol         := ParentOrder.ChildSymbol;
    DataFactor^.FactorDoc.ContractId     := ParentOrder.ChildId;
    DataFactor^.FactorDoc.Expiry         := ParentOrder.Expiry;

    if (ParentOrder.ChildId > 0) then
    begin
      if (ParentOrder.FeedFromBroker = 0) then
      begin
        DataFactor^.FactorDoc.BrokerType := TBrokerType.brIB;
        SubscribeIBFeed(NodeFactor);
      end
      else
      begin
        DataFactor^.FactorDoc.BrokerType := TBrokerType.brNN;
        SubscribeNNFeed(NodeFactor);
      end;
    end;
    TMonitorLists.InstrumentList.AddNode(DataFactor^.FactorDoc.ContractId, NodeFactor);
    if (GetLastPrice(DataFactor^.FactorDoc.ContractId, DataFactor^.FactorDoc.TickType1) > 0) then
      TMonitorLists.InstrumentList.DocumentQueue.DeleteItem(NodeFactor);
    SetIcon(NodeFactor);
    vstMonitor.InvalidateNode(NodeFactor);
    UpdateCondition(NodeCond);
  end;

begin
  if Assigned(aParentNode) then
  begin
    ParentData := aParentNode^.GetData;
    if Assigned(ParentData) and Assigned(ParentData.OrderDoc) and (ParentData.OrderDoc is TOrderNNDoc) then
    begin
      ParentOrder := TOrderNNDoc(ParentData.OrderDoc);

      if (ParentOrder.FeedFromBroker = 0) and (ParentOrder.ChildId > 0) then
        InstrumentName := SokidList.GetItem(ParentOrder.ChildId).Name
      else if (ParentOrder.FeedFromBroker = 1) and (ParentOrder.ChildId > 0) then
      begin
        Instruments := NordNetBroker.GetInstruments(ParentOrder.ChildId);
        for i := 0 to Length(Instruments) - 1 do
          if not Instruments[i].Symbol.IsEmpty then
          begin
            InstrumentName := Instruments[i].Name;
            Break;
          end;
      end;
      if InstrumentName.IsEmpty then
        InstrumentName := ParentOrder.ChildSymbol;

      CreateChildOrder;
      case ParentOrder.OrderAction of
        iabBuy:
          begin
            if ParentOrder.IsActiveReached then
              CreateChildNodes(kcReached);
            CreateChildNodes(kcTrigger);
            if ParentOrder.IsActiveStopLoss then
              CreateChildNodes(kcStopLoss);
          end;
        iabSell:
          begin
            if ParentOrder.IsActiveStopLoss then
              CreateChildNodes(kcStopLoss);
            CreateChildNodes(kcTrigger);
            if ParentOrder.IsActiveReached then
              CreateChildNodes(kcReached);
          end;
      end;
    end;
  end;
end;

procedure TfrmMonitor.OnPortfolioUpdate(Sender: TObject; Index: Integer);
begin
  if Assigned(frmDockFormPosition) then
    frmDockFormPosition.UpdatePortfolio(Index);
end;

procedure TfrmMonitor.OnAccountValue(Sender: TObject; Index: Integer);
var
  AccountItems: TArray<string>;
begin
  AccountItems := IABClient.AccountValues[Index].Split([' ']);
  if (Length(AccountItems) >= 4) and AccountItems[2].ToUpper.StartsWith('BASE') then
  begin
    if AccountItems[0].ToUpper.StartsWith('REALIZEDPNL') then
    begin
      lblAcountRealised.Caption := AccountItems[1];
      lblAccountBalans.Caption := FloatToStrEx(SafeStrToFloat(lblAcountRealised.Caption) - SafeStrToFloat(lblAcountUnrealised.Caption));
    end
    else if AccountItems[0].ToUpper.StartsWith('UNREALIZEDPNL') then
    begin
      lblAcountUnrealised.Caption := AccountItems[1];
      lblAccountBalans.Caption := FloatToStrEx(SafeStrToFloat(lblAcountRealised.Caption) - SafeStrToFloat(lblAcountUnrealised.Caption));
    end;
  end;

  if Assigned(frmDockFormAccountInfo) then
    frmDockFormAccountInfo.OnAccountValue(Index);
end;

procedure TfrmMonitor.TradeTest(const aNodeOrder: PVirtualNode; const aFillPrice: Double; const aQuantity: Integer);
var
  DataOrder: PTreeData;
  CondData: PTreeData;
  arrConditions: TNodeArray;
  CompiledValue: TCompiledValue;
begin
  if Assigned(aNodeOrder) then
  begin
    DataOrder := aNodeOrder^.GetData;
    arrConditions := TTreeDocument.GetNodesListByType(vstMonitor, aNodeOrder, ntCondition);
    for var CondNode in arrConditions do
    begin
      CondData := CondNode^.GetData;
      if Assigned(CondData^.ConditionDoc) and (CondData^.ConditionDoc.CondType in [ctRealtimeValue, ctRealtimeAndTimeGap]) then
      begin
        CompiledValue := TTreeDocument.CalcCompiledValue(vstMonitor, CondData^.ConditionDoc);
        CondData^.ConditionDoc.IsValueReady := CompiledValue.IsValueReady;
        if CompiledValue.IsValueReady then
        begin
          CondData^.ConditionDoc.CalcValue := CompiledValue.TotalValue / CondData^.ConditionDoc.DivisionValue;
          if (CondData^.ConditionDoc.InitValue = 0) then
            CondData^.ConditionDoc.InitValue := CondData^.ConditionDoc.CalcValue;
          CondData^.Enabled := True;
          CondData^.ConditionDoc.Active := True;
          CondData^.ConditionDoc.CompiledValue := CompiledValue;
          TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'TradeTest',
                                                                      'NodeId=' + CondData^.NodeId.ToString +
                                                                      ', Factors=' + CompiledValue.Factors +
                                                                      'IsValueReady=' + BoolToStr(CompiledValue.IsValueReady, True) +
                                                                      ', CondLimit=' + CurrToStr(CondData^.ConditionDoc.CondLimit) +
                                                                      ', CompiledValue=' + CurrToStr(CompiledValue.TotalValue) +
                                                                      ', DivisionValue=' + CurrToStr(CondData^.ConditionDoc.DivisionValue) +
                                                                      ', CalcValue=' + CurrToStr(CondData^.ConditionDoc.CondLimit) +
                                                                      ', Sum(' + CondData^.ConditionDoc.TickType1.ToString + ')=' + CurrToStr(CompiledValue.SummValue1) +
                                                                      ' [' + CondData^.ConditionDoc.TypeOperation.ToString + '] ' +
                                                                      ', Sum(' + CondData^.ConditionDoc.TickType2.ToString + ')=' + CurrToStr(CompiledValue.SummValue2) +
                                                                      ', Remove from Queue, CalcValue=' + CurrToStr(CondData^.ConditionDoc.CalcValue) +
                                                                      ', Enabled=' + BoolToStr(CondData^.Enabled, True) +
                                                                      ', Remove from Queue, the condition "' + CondData^.ConditionDoc.Description + '" is enabled');
        end;
        UpdateCondition(CondNode);
      end;
    end;

    if not ExistsChildConditions(aNodeOrder) then
      DataOrder^.OrderDoc.Buy
    else
      TestOrder(aNodeOrder);
  end;
end;

procedure TfrmMonitor.FillPartlyDataForChildNodes(const aNode: PVirtualNode; const aFillPrice: Double; const aQuantity: Integer);
var
  AfterLoadProc: TAfterLoadEachDocumentProc;
  AfterLoadTreeProc: TAfterLoadTreeProc;
  Data: PTreeData;
  InstrumentData: TInstrumentData;
  Node: PVirtualNode;
begin
  if Assigned(aNode) and (aFillPrice > 0) and (aQuantity > 0) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data^.OrderDoc) then
    begin
      vstMonitor.BeginUpdate;
      try
        Node := TTreeDocument.LoadRelationTree(-1, Data^.RelationId, vstMonitor, aNode,
          function(const aDocType: TDocType; const aRelationId, aRecordID, aParentId: Integer): Boolean
          begin
            Result := True;
            if (aParentId = Data^.RelationId) then
              Result := aDocType in [ntOrderGroup, ntOrder];
          end, nil);

        if Assigned(Node) and Assigned(Data^.OrderDoc) then
        begin
          AfterLoadProc := DoAfterLoadEachDocumentProc(Data^.OrderDoc.Id, aFillPrice);
          AfterLoadTreeProc := DoAfterLoadTreeProc(Node);
          InstrumentData.Assign(Data^.OrderDoc);
          TTreeFactory.FillDocuments(Node,
                                     InstrumentData,
                                     Data^.OrderDoc.LatestFillPrice,
                                     TAutoTradesCommon.Create(aQuantity,
                                                              Data^.OrderDoc.QualifierInstance,
                                                              Data^.OrderDoc.QualifierID,
                                                              Data^.OrderDoc.AutoTradesInstance,
                                                              Data^.OrderDoc.AutoTradesID,
                                                              False),
                                     AfterLoadProc,
                                     AfterLoadTreeProc);
          SetChildsEnabled(Node, True);
          TradeTest(Node, aFillPrice, aQuantity);
        end;
      finally
        vstMonitor.EndUpdate;
      end;

      //Cancel first mother order
      TTask.Create(
        procedure()
        var
          WaitingTime: Integer;
        begin
          WaitingTime := General.WaitingTime;  // Default is 3000 ms
          if (MilliSecondsBetween(Now, Data^.OrderDoc.TradeTime) < General.WaitingTime) then
            WaitingTime := MilliSecondsBetween(Now, Data^.OrderDoc.TradeTime);
          TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'FillPartlyDataForChildNodes.TTask',
                                        'NodeId=' + Data^.NodeID.ToString + sLineBreak +
                                        ', WaitingTime=' + WaitingTime.ToString +
                                        ', OrderStatus=' + Data^.OrderDoc.OrderStatus.ToString);
          Sleep(WaitingTime);
          if not(Data^.OrderDoc.OrderStatus in [osCancelled, osFilled, osError]) then
            TThread.Synchronize(nil,
              procedure
              begin
                TPublishers.LogPublisher.Write([ltLogWriter], ddWarning, Self, 'FillPartlyDataForChildNodes.TTask', 'CancelOrder');
                Data^.OrderDoc.CancelOrder;
              end);
        end).Start;
    end;
  end;
end;

procedure TfrmMonitor.FillDataForChildNodes(const aNode: PVirtualNode; const aFillPrice: Double; const aQuantity: Integer);
var
  OrderData: PTreeData;
  OrderDoc: TOrderIBDoc;
  arrOrders: TNodeArray;
begin
  if (aFillPrice > 0) and (aQuantity > 0) then
  begin
    arrOrders := TTreeDocument.GetNodesListByType(vstMonitor, aNode, ntOrder);
    for var Node in arrOrders do
      if Assigned(Node) and (Node <> aNode) then
      begin
        OrderData := Node^.GetData;
        if Assigned(OrderData^.OrderDoc) and
           (not OrderData^.Enabled) and
           (OrderData^.OrderDoc is TOrderIBDoc) and
           (not(OrderData^.OrderDoc.OrderStatus in [osCancelled, osFilled, osPartlyFilled, osError])) and
           (OrderData^.OrderDoc.ExtendedOptions.Subordination = suChildOrder) then
        begin
          SetChildsEnabled(Node, True);
          OrderDoc := TOrderIBDoc(OrderData^.OrderDoc);
          OrderData.OrderDoc.Quantity := aQuantity;
          TTreeFactory.FillOrderPrice(OrderDoc, aFillPrice);
          TradeTest(Node, aFillPrice, aQuantity);
        end;
      end;
  end;
end;

procedure TfrmMonitor.ShowNotification(const Order: TIABOrder; const Status: TIABOrderState);
resourcestring
  rsNotification = '%s [%d] Quantity: %f %s: %f';
begin
  if Status in [osCancelled, osFilled, osError, osPartlyFilled] then
  begin
    TThread.Synchronize(nil,
      procedure
      var
        Notification: TNotification;
      begin
        Notification := DMod.NotificationCenter.CreateNotification;
        try
          Notification.Name := 'OrderStatusNotification';
          Notification.Title := Status.ToString;
          Notification.AlertBody := Format(rsNotification, [Order.Symbol,
                                                            Order.ContractId,
                                                            Order.Quantity,
                                                            Order.Action.ToString,
                                                            Order.Filled]);
          DMod.NotificationCenter.PresentNotification(Notification);
        finally
          FreeAndNil(Notification);
        end;
      end)
  end;
end;

procedure TfrmMonitor.ShowNotification(const aMessage, aTitle: string);
var
  Notification: TNotification;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Notification := DMod.NotificationCenter.CreateNotification;
      try
        Notification.Name := 'Notification';
        Notification.Title := aTitle;
        Notification.AlertBody := aMessage;
        DMod.NotificationCenter.PresentNotification(Notification);
      finally
        FreeAndNil(Notification);
      end;
    end)
end;

procedure TfrmMonitor.OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
var
  Info: string;
  Node: PVirtualNode;
  Data: PTreeData;
begin
  // Use the properties of the Order parameter above, to determine the fill Status of an order.
  // You cannot rely on the Status of an order to determine its fill.
  // The TWS will fill orders under almost any Status! Particularly if its a partial fill.
  // Also new OnExecution in API 7 reports same as the Changed property and the OnFill event.
  // Several ways to get the executions.
  // Not certain if the sequence of Events is reliable?  (Onexecution first, then Onstatus event)
  if (Order.TempId > 0) then  //Only TWS API orders
  begin
    Info := '';
    Node := TMonitorLists.OrderList.GetNodeOrder(Order.TempId);
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if Assigned(Data^.OrderDoc) then
      begin
        Info := Concat('NodeId=', Data^.NodeID.ToString,
                       ', OrderName=', Data^.OrderDoc.Description.Trim, ',');
        Order.OrderType := Data^.OrderDoc.OrderType;
        if not Order.Completed and (Status in [osSubmitted, osPendSubmit]) then
          if (Order.Filled > 0) then
            Status := osPartlyFilled;

        Data^.OrderDoc.Filled                        := Trunc(Order.Filled);
        Data^.OrderDoc.TradeTime                     := Now;
        Data^.OrderDoc.LatestFillPrice               := Order.LatestFillPrice;
        Data^.OrderDoc.CalculationStage[csFillPrice] := Order.LatestFillPrice;
        if Order.Completed and
           (not Data^.OrderDoc.IsActivateChild) and
           (Data^.OrderDoc.ExtendedOptions.Subordination = suMotherOrder) then
          FillDataForChildNodes(Node, Order.FillPrice, Trunc(Order.Filled));

        Info := Concat(Info, IABClient.GetTextOrderInfo(Status, Order));
        TMonitorLists.OrderList.AddStatus(Order,
                                          Info,
                                          'OnOrderStatus',
                                          -1, //ErrorCode
                                          Status);

        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'OnOrderStatus', Info.Replace(',', '<br>'));
        if (Status in [osCancelled, osFilled, osError]) then
          OrdersExecuteSequential(Node);
        if (Status in [osFilled]) then
          CancelSiblingsOrders(Node);

        if (Status in [osCancelled, osFilled, osPartlyFilled]) then
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView, ltActivityLog],
                                          ddText,
                                          Order.TempId,
                                          Trunc(Order.Quantity),
                                          Trunc(Order.Filled),
                                          'OnOrderStatus',
                                          Order.Symbol,
                                          Info,
                                          Status,
                                          Order.Action,
                                          Order.LatestFillPrice);
        ShowNotification(Order, Status);

        if (Status in [osError]) then
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, 'OnOrderStatus', Status.ToString + ' - ' + Info)
        else if (Status in [osCancelled, osSubmitted]) then
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnOrderStatus', Status.ToString + ' - ' + Info)
        else
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnOrderStatus',
                                    'Action='          + Order.Action.ToString +
                                    ', OrderType='     + Order.OrderType.ToString +
                                    ', OrderID='       + Order.TempId.ToString +
                                    ', LatestFillQty=' + Order.LatestFillQty.ToString +
                                    ', Filled='        + Order.Filled.ToString +
                                    ', Remaining='     + Order.Remaining.ToString +
                                    ', LastPrice='     + Order.FillPrice.ToString +
                                    ', '               + Info);
        SetIcon(Node);
        vstMonitor.InvalidateNode(Node);

        if (Status in [osFilled, osPartlyFilled]) and (Data^.OrderDoc.ExtendedOptions.Subordination = suMotherOrder) then
        begin
          TMonitorLists.PriceCache.AddPrice(Data^.OrderDoc.Id, ttMotherFilledPrice, Data^.OrderDoc.AvgPrice, Data^.OrderDoc.TradeTime);
          TPublishers.MotherFilledPriceChangePublisher.OnMotherFilledPriceChange(Node);
        end;
      end;
    end;
  end;
end;

procedure TfrmMonitor.OnExecution(Sender: TObject; Order: TIABOrder);
var
  Data: PTreeData;
  LastExec: TIABExecution;
  Node: PVirtualNode;
  Info: string;
  Status: TIABOrderState;
begin
  Info := '';
  Status := osSubmitted;
  Data := nil;
  if (Order.TempId > 0) then  //Only TWS API orders
  begin
    LastExec := Order.Executions[Order.ExecutionsCount -1];
    Node := TMonitorLists.OrderList.GetNodeOrder(Order.TempId);
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if Assigned(Data) and Assigned(Data^.OrderDoc) then
      begin
        Info := Concat('OrderName=', Data^.OrderDoc.Description.Trim, ', ');
        if (Order.OrderType <> Data.OrderDoc.OrderType) then
        begin
//          Info := Concat(Info, ' Type added from order. Old value - ', Order.OrderType.ToString, ', ');
          Order.OrderType := Data.OrderDoc.OrderType;
        end;

        if Order.Completed then
          Status := osFilled
        else if (Order.Filled > 0) then
          Status := osPartlyFilled;

        if (Data^.CreationType = ctProgramm) and (Data^.OrderDoc.ExtendedOptions.Subordination = suMotherOrder) then
          if Data^.OrderDoc.IsActivateChild then
          begin
            if (Data^.OrderDoc.Filled = 0) then
            begin
              FillDataForChildNodes(Node, LastExec.AveragePrice, Trunc(LastExec.Volume));
              TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnExecution', 'FillPartlyDataForChildNodes, NodeId=' + Data^.NodeID.ToString + ', LastExecVolume=' + LastExec.Volume.ToString);
            end
            else
            begin
              FillPartlyDataForChildNodes(Node, LastExec.AveragePrice, Trunc(LastExec.Volume));
              TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnExecution', 'FillPartlyDataForChildNodes, NodeId=' + Data^.NodeID.ToString + ', LastExecVolume=' + LastExec.Volume.ToString);
            end;
          end;
        Data^.OrderDoc.AvgPrice                      := LastExec.AveragePrice;
        Data^.OrderDoc.Filled                        := Trunc(LastExec.Volume);
        Data^.OrderDoc.TradeTime                     := Now;
        Data^.OrderDoc.LatestFillPrice               := LastExec.AveragePrice;
        Data^.OrderDoc.CalculationStage[csFillPrice] := LastExec.AveragePrice;
        SetIcon(Node);
      end;
    end;

    Info := Concat(Info,
                   IABClient.GetTextOrderInfo(Status, Order), sLineBreak,
                   '----- Execution -----',
                   ',ExecutionId=',  LastExec.ExecutionId,
                   ',Time=',         LastExec.Time,
                   ',Action=',       LastExec.Side.ToString,
                   ',Volume=',       LastExec.Volume.ToString,
                   ',Price=',        LastExec.Price.ToString,
                   ',PermId=',       LastExec.PermId.ToString,
                   ',AveragePrice=', LastExec.AveragePrice.ToString);

    TMonitorLists.OrderList.AddStatus(Order,
                                      Info,
                                      'OnExecution',
                                      -1, //ErrorCode
                                      Status);
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnExecution', Info);
    if Assigned(Data) and Assigned(Data^.OrderDoc) then
      if (Status in [osFilled, osPartlyFilled]) and (Data^.OrderDoc.ExtendedOptions.Subordination = suMotherOrder) then
      begin
        TMonitorLists.PriceCache.AddPrice(Data^.OrderDoc.Id, ttMotherFilledPrice, Data^.OrderDoc.AvgPrice, Now);
        TPublishers.MotherFilledPriceChangePublisher.OnMotherFilledPriceChange(Node);
      end;
  end;
end;

procedure TfrmMonitor.OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
var
  Data: PTreeData;
  Info: string;
  InstName: string;
  TWSMessageItem: TTWSMessageItem;
  MessageText: string;
  Node: PVirtualNode;
  SokidInfo: TSokidInfo;
begin
  Info := '';
  TWSMessageItem := GetTWSMessageItem(ErrorCode);
  if (TWSMessageItem.ErrorCode > -1) then
    MessageText := ', MessageText="' + TWSMessageItem.ErrorMsg + '"';

  if (ErrorCode = 322) then                 //Server error when processing an API client request. Duplicate ticker id
  begin
    Info := 'ErrorCode=' + ErrorCode.ToString + ', ErrorMsg="' + ErrorMsg + '"' + MessageText;
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, 'OnError', Info);
    Exit;
  end;

  if SokidList.ContainsKey(TempId) then
    InstName := ', Instrument=' + SokidList.GetItem(TempId).Symbol + ' (' + SokidList.GetItem(TempId).Name + ')'
  else
    InstName := '';

  Data := nil;
  Node := TMonitorLists.OrderList.GetNodeOrder(TempId);
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if Assigned(Data) and Assigned(Data^.OrderDoc) then
      Info := Concat('NodeId=', Data.NodeId.ToString, ', OrderName=', Data^.OrderDoc.Description, ', ');
  end;

  Info := Concat(Info,
                 'TempId=', TempId.ToString,
                 InstName,
                 ', ErrorCode=', ErrorCode.ToString,
                 ', ErrorMsg="', ErrorMsg, '"',
                 MessageText);
  if (TempId > 0) then
  begin
    if (ErrorCode = 202) then                        //Order cancelled - Reason: An active order on the IB server was cancelled
    begin
      TMonitorLists.OrderList.AddStatus(TempId,
                                        0,
                                        0,
                                        Info,
                                        'Cancelled',
                                        ErrorCode,
                                        osCancelled,
                                        0);
      TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddWarning, 'OnCancelled', Info);
    end
    else
    begin
      TMonitorLists.OrderList.AddStatus(TempId,
                                        0,
                                        0,
                                        Info,
                                        'error',
                                        ErrorCode,
                                        osError,
                                        0);
      if Assigned(Node) and Assigned(Data) and Assigned(Data^.OrderDoc) then
      begin
        Data^.OrderDoc.TradeTime := Now;
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView, ltActivityLog],
                                   ddText,
                                   TempId,
                                   Data^.OrderDoc.Quantity,
                                   Data^.OrderDoc.Filled,
                                   'OnError',
                                   Data^.OrderDoc.Symbol,
                                   Info,
                                   Data^.OrderDoc.OrderStatus,
                                   Data^.OrderDoc.OrderAction,
                                   Data^.OrderDoc.LatestFillPrice);
      end;

      if (ErrorCode = 110) then                           //The price does not conform to the minimum price variation for this contract.
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, 'OnError', Info);
      TPublishers.OrderStatePublisher.OnCloseOrder(TempId);
    end;

    if SokidList.ContainsKey(TempId) then
    begin
      SokidInfo := SokidList.Items[TempId];
      SokidInfo.TWSMessageItem.ErrorCode := ErrorCode;
      SokidInfo.TWSMessageItem.ErrorMsg := ErrorMsg;
      SokidList.SetValue(SokidInfo);
      if Pos(SokidInfo.TWSMessageItem.ErrorCode.ToString, General.PriceScheduleCodeErrors) > 0 then
      begin
        TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddWarning, Self, 'OnError',
                                      'Instrument was deleted from DB' +
                                      ', TWS ErrorCode = ' + SokidInfo.TWSMessageItem.ErrorCode.ToString +
                                      ', ConId='           + SokidInfo.ContractId.ToString +
                                      ', Symbol='          + SokidInfo.Symbol +
                                      ', Description='     + SokidInfo.Description +
                                      ', Exchange='        + SokidInfo.Exchange +
                                      ', PrimaryExchange=' + SokidInfo.PrimaryExchange +
                                      ', MarketRuleIds='   + SokidInfo.MarketRuleIds);
        SokidList.DeleteItem(SokidInfo.ContractId);
      end
      else
        SokidList.SetValue(SokidInfo);
    end;
  end;

  if (TWSMessageItem.ErrorCode > -1) and (TWSMessageItem.ErrorCode <> 504) and     //Not connected.
                                         (TWSMessageItem.ErrorCode <> 202) then    //Order cancelled
  begin
    case TWSMessageItem.TypeCode of
      mcSystem:
        begin
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText, 'OnError', Info);
        end;
      mcWarning:
        begin
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddWarning, 'OnWarning', Info);
        end;
      mcClientError:
        begin
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, 'OnError', Info);
        end;
      mcTWSError:
        begin
          TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddError, 'OnError', Info);
        end;
    end;
  end;
end;

procedure TfrmMonitor.OnOpenOrder(Sender: TObject; Order: TIABOrder);
var
  Info: string;
  Node: PVirtualNode;
  Data: PTreeData;
begin
  //This will later on be used for picking up "orderstate" for updates for an order about
  //margins,loans, commission, commissionCurrency, warningText, completedTime, completedStatus
  if (Order.TempId > 0) and (Order.OrderType <> otNoChange) then  //Only TWS API orders
  begin
    Node := TMonitorLists.OrderList.GetNodeOrder(Order.TempId);
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if Assigned(Data) and Assigned(Data^.OrderDoc) then
      begin
        Info := Concat('NodeId=', Data.NodeId.ToString, ', OrderName=', Data^.OrderDoc.Description);
        if (Order.OrderType <> Data.OrderDoc.OrderType) then
        begin
          Info := Concat(Info, sLineBreak, 'Type added from order. Old value - ', Order.OrderType.ToString);
          Order.OrderType := Data.OrderDoc.OrderType;
        end;

        Info := Concat(Info, sLineBreak, IABClient.GetTextOrderInfo(Data.OrderDoc.OrderStatus, Order));
        TMonitorLists.OrderList.AddStatus(Order.TempId,
                             Order.Filled,
                             Order.Remaining,
                             Info,
                             'OpenOrder',
                             -1, //ErrorCode
                             osPreSubmit,
                             Order.FillPrice);
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'OnOpenOrder', Info.Replace(',', '<br>'));
      end;
    end;
    SetIcon(Node);
  end;
end;

procedure TfrmMonitor.OnOpenOrderNN(const aOrderList: array of TOrder);
begin
  //nothing
  //implementation IOrderState
end;

procedure TfrmMonitor.OnCloseOrder(const aTempId: Integer);
begin
  //nothing
  //implementation IOrderState
end;

procedure TfrmMonitor.UpdateHistoricalDataForConditionDoc(const aId: Integer);
var
  ConditionList: TStringList;
  ConditionNode: PVirtualNode;
  CurData: PTreeData;
  CurNode: PVirtualNode;
  InstrumentItem: TInstrumentItem;
  i: Integer;
begin
  ConditionList := TStringList.Create;
  try
    ConditionList.Duplicates := dupIgnore;
    ConditionList.Sorted := True;

    InstrumentItem := TMonitorLists.InstrumentList.GetItem(aId);
    if Assigned(InstrumentItem) then
    begin
      for CurNode in InstrumentItem.NodeList do
        if Assigned(CurNode) then
        begin
          CurData := CurNode^.GetData;
          if Assigned(CurData) then
          begin
            if Assigned(CurData.FactorDoc) and (CurData.FactorDoc.BrokerType = TBrokerType.brIB) then
            begin
              ConditionNode := GetParentNode(CurNode, ntCondition);
              if Assigned(ConditionNode) then
                ConditionList.AddObject(Integer(ConditionNode).ToString, TObject(ConditionNode));
            end
            else if Assigned(CurData.OrderDoc) and (CurData.OrderDoc.BrokerType = TBrokerType.brIB) then
              CurData.OrderDoc.LastPrice := GetLastPrice(aId);

            if (GetLastPrice(aId) > 0) then
              TMonitorLists.InstrumentList.SetValue(aId, Now, GetLastPrice(aId), ttLast);
          end;
        end;
    end;

    for i := 0 to ConditionList.Count - 1 do
      WriteHistoricalDataToConditionDoc(PVirtualNode(ConditionList.Objects[i]));
  finally
    FreeAndNil(ConditionList);
  end;
end;

procedure TfrmMonitor.OnHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
begin
  if (Item <= Count) then
  begin
    if (FControlHistData.IndexOf(DataId.ToString) = -1) then
    begin
      if (Item < 0) then
      begin
        FControlHistData.Add(DataId.ToString);
        UpdateHistoricalDataForConditionDoc(DataId);
      end
    end;
    TMonitorLists.PriceCache.AddPrice(DataId, HistoricalChartDataElement);
  end;
end;

function TfrmMonitor.OpenGroupSet(const aGroupId: Integer): PVirtualNode;
var
  AfterProc: TAfterLoadEachDocumentProc;
  Data: PTreeData;
  arr: TNodeArray;
begin
  AfterProc := procedure(const aNode: PVirtualNode)
    var
      CondId: Integer;
      Data: PTreeData;
      SokidInfo: TSokidInfo;
      Order: TIABOrder;
      ExtendedOptions: TCustomOrderDoc.TExtendedOptions;
    begin
      if Assigned(aNode) then
      begin
        Data := aNode^.GetData;
        Data^.CreationType := ctUser;
        case Data^.DocType of
          TDocType.ntQualifier:
            begin

            end;
          TDocType.ntAutoTrade:
            begin

            end;
          TDocType.ntOrderGroupSet:
            begin

            end;
          TDocType.ntOrderGroup:
            begin

            end;
          TDocType.ntOrder:
            begin
              CondId := Data^.OrderDoc.Id;
              if (Data^.OrderDoc.BrokerType = TBrokerType.brIB) then
              begin
                if (CondId > 0) then
                begin
                  TIABMarket.RequestMarketData(CondId);
                  SokidInfo := SokidList.GetItem(Data^.OrderDoc.Id);
                  Data^.OrderDoc.AssignFrom(SokidInfo);
                  with TOrderIBDoc(Data^.OrderDoc) do
                  begin
                    SetSecurityType(SokidInfo.SecurityType);
                    if Data^.OrderDoc.MarketList.IsEmpty then
                      Data^.OrderDoc.MarketList := SokidInfo.MarketRuleIds;
                    if MarketList.IsEmpty then
                    begin
                      Order := TIABOrder.Create;
                      try
                        IABClient.ClearOrder(Order);
                        Order.ContractId      := CondId;
                        Order.SecurityType    := SecurityType;
                        Order.Currency        := Currency;
                        Order.Exchange        := Exchange;
                        Order.PrimaryExchange := PrimaryExchange;
                        Order.Multiplier      := '';
                        Order.Symbol          := Symbol;
                        IABClient.SendRequest(ibGetInstrumentSpecs, 1, Order);
                      finally
                        FreeAndNil(Order);
                      end;
                    end;
                  end;
                end;
              end
              else if Data^.OrderDoc.BrokerType = TBrokerType.brNN then
              with TOrderNNDoc(Data^.OrderDoc) do
              begin
                if (ChildId > 0) then
                begin
                  TMonitorLists.InstrumentChildList.AddNode(ChildId, aNode);
                  SetValueArrayFromChildFeed(ttLast, GetLastPrice(ChildId));
                end;
              end;
              ExtendedOptions := Data^.OrderDoc.ExtendedOptions;
              ExtendedOptions.Subordination := TTreeDocument.GetOrderSubordination(vstMonitor, aNode);
              Data^.OrderDoc.ExtendedOptions := ExtendedOptions;
              Data^.Enabled := Data^.OrderDoc.ExtendedOptions.Subordination in [suMotherOrder, suUnknow];
              Data^.OrderDoc.LastPrice := GetLastPrice(CondId);
              TMonitorLists.InstrumentList.AddNode(CondId, aNode);
              if (Data^.OrderDoc.LastPrice > 0) then
                TMonitorLists.InstrumentList.DocumentQueue.DeleteItem(aNode);
              TMonitorLists.OrderList.AddOrder(aNode);
            end;
          TDocType.ntCondition:
            begin
              AddConditionToRealtimeChart(aNode);
              WriteHistoricalDataToConditionDoc(aNode);
              if Assigned(Data^.ConditionDoc) and (Data^.ConditionDoc.CondType <> ctTimeGap) then
                UpdateChildAlgos(aNode, False);
            end;
          TDocType.ntAlgos:
            begin
              TMonitorLists.AlgosList.AddItem(aNode);
              AddAlgosToConditionAlgosChart(aNode);
              AddAlgosToRealtimeChart(nil, aNode);
            end;
          TDocType.ntFactor:
            begin
              SokidInfo := SokidList.GetItem(Data^.FactorDoc.ContractId);
              if (Data^.FactorDoc.Symbol.IsEmpty) then
                Data^.FactorDoc.Symbol := SokidInfo.Symbol;
              if (Data^.FactorDoc.PrimaryExchange.IsEmpty) then
                Data^.FactorDoc.PrimaryExchange := SokidInfo.PrimaryExchange;
              if (Data^.FactorDoc.Exchange.IsEmpty) then
                Data^.FactorDoc.Exchange := SokidInfo.Exchange;
              if (Data^.FactorDoc.Currency = '') then
                Data^.FactorDoc.Currency := SokidInfo.Currency;
              if (Data^.FactorDoc.MarketList = '') then
                Data^.FactorDoc.MarketList := SokidInfo.MarketRuleIds;
              Data^.FactorDoc.Expiry := SokidInfo.Expiry;

              if (Data^.FactorDoc.BrokerType = TBrokerType.brNN) then
              begin
                if miUseIBfeeds.Checked then
                begin
                  Data^.FactorDoc.UseIBFeeds := True;
                  CondId := GetCONIDforNN(Data^.FactorDoc.Symbol, Data^.FactorDoc.ContractType, Data^.FactorDoc.Exchange, Data^.FactorDoc.Currency);
                  Data^.FactorDoc.ContractId := CondId;
                  Data^.FactorDoc.IBID  := CondId;
                end
                else
                  SubscribeNNFeed(aNode);
              end
              else if (Data^.FactorDoc.BrokerType = TBrokerType.brIB) then
                TIABMarket.RequestMarketData(Data^.FactorDoc.ContractId);
              TMonitorLists.InstrumentList.AddNode(Data^.FactorDoc.ContractId, aNode);
              Data^.FactorDoc.CurrentValue := GetLastPrice(Data^.FactorDoc.ContractId, Data^.FactorDoc.TickType1);
              if (Data^.FactorDoc.CurrentValue > 0) then
                TMonitorLists.InstrumentList.DocumentQueue.DeleteItem(aNode);
            end;
        end;
        Self.SetIcon(aNode);
      end;
    end;

  Result := TTreeDocument.LoadRelationTree(aGroupId, -1, vstMonitor, FOrderGroupSet.OwnerNode, nil, AfterProc);
  if Assigned(Result) then
  begin
    if Assigned(FOrderGroupSet.OwnerNode) then
    begin
      arr := TTreeDocument.GetNodesListByType(vstMonitor, Result, ntOrderGroup);
      for var Node in arr do
        vstMonitor.MoveTo(Node, FOrderGroupSet.OwnerNode.LastChild, amInsertAfter, False);
      vstMonitor.DeleteNode(Result);
      Result := FOrderGroupSet.OwnerNode;
    end
    else
    begin
      Data := Result^.GetData;
      FOrderGroupSet.RecordId  := Data^.RecordId;
      FOrderGroupSet.Name      := Data^.OrderGroupSetDoc.Name;
      FOrderGroupSet.OwnerNode := Result;
      SetCurrentGroupCaptions;
    end;
  end;
  vstMonitor.FullExpand;
end;

procedure TfrmMonitor.pgcMainChange(Sender: TObject);
begin
  for var i := 0 to pgcMain.ActivePage.ControlCount - 1 do
    if (pgcMain.ActivePage.Controls[i] is TWinControl) then
    begin
      TWinControl(pgcMain.ActivePage.Controls[i]).SetFocus;
      Break;
    end;
end;

procedure TfrmMonitor.pmMainTreePopup(Sender: TObject);
var
  DocType : TDocType;

begin
  DocType := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode);

  miDelimiter1.Visible := not (DocType = ntUnknow);
  miDelimiter2.Visible := DocType in [ntOrderGroup, ntAlgos, ntFactor];
  miDelimiter3.Visible := not (DocType = ntUnknow);
  miDelimiter4.Visible := not (DocType = ntUnknow);

  miGetInfo.Visible := not (DocType = ntUnknow);
  miAddOrderGroup.Visible := false;
  miTransformToOrders.Visible := false;

  aShowInformationDialog.Caption := 'Get ' + DocType.ToString + ' Info';
end;

function TfrmMonitor.ConnectToIBBroker: string;
var
  ClientID: Integer;
  IPAddress: string;
  Port: Integer;
begin
  if General.XmlFile.ReadAttributes(General.C_SECTION_CONNECTIONS, General.InteractiveParams.C_SECTION) then
  begin
    IPAddress := General.XmlFile.Attributes.GetAttributeValue(General.C_ATTR_LOGIN, General.InteractiveParams.C_IP_ADDRESS);
    ClientID  := General.XmlFile.Attributes.GetAttributeValue(General.C_ATTR_CLIENT_ID, General.InteractiveParams.C_CLIENT_ID);
    Port      := General.XmlFile.Attributes.GetAttributeValue(General.C_ATTR_PORT, General.InteractiveParams.C_PORT);
  end
  else
  begin
    IPAddress := General.InteractiveParams.C_IP_ADDRESS;
    ClientID  := General.InteractiveParams.C_CLIENT_ID;
    Port      := General.InteractiveParams.C_PORT;
  end;

  if General.XMLFile.ReadBool(TGeneral.C_SECTION_COMMON_PARAMS, TGeneral.C_KEY_USE_IB, False) then
  begin
    IABClient.Disconnect;
    IABClient.Connect(IPAddress, Port, ClientID);
    Application.ProcessMessages;
    Sleep(1000);
  end;
end;

function TfrmMonitor.ConnectToNNBroker: string;
var
  UserLogin: string;
  UserPassword: string;
  OrdersList: TArray<TOrder>;
begin
  if General.XmlFile.ReadAttributes(General.C_SECTION_CONNECTIONS, General.NordNetParams.C_SECTION) then
  begin
    UserLogin    := General.XmlFile.Attributes.GetAttributeValue(General.C_ATTR_LOGIN, General.NordNetParams.C_LOGIN);
    UserPassword := General.XmlFile.Attributes.GetAttributeValue(General.C_ATTR_PASSWORD, General.NordNetParams.C_PASSWORD);
  end
  else
  begin
    UserLogin    := General.NordNetParams.C_LOGIN;
    UserPassword := General.NordNetParams.C_PASSWORD;
  end;

  if General.XMLFile.ReadBool(TGeneral.C_SECTION_COMMON_PARAMS, TGeneral.C_KEY_USE_NN, False) then
  begin
    NordNetBroker.Logon(UserLogin, UserPassword);
    if NordNetBroker.Active then
    begin
      OrdersList := NordNetBroker.GetOrders(NordNetBroker.AccountNum, False);
      if Length(OrdersList) > 0 then
        TPublishers.OrderStatePublisher.OnOpenOrderNN(OrdersList);
    end;
  end;
end;

procedure TfrmMonitor.Connect;
begin
  Connected := True;
  btnLogin.ImageIndex := 54;
  btnLogin.Hint := 'Disconnect';

  ConnectToIBBroker;
  SubscribeNNFeeds;
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, THtmlLib.GetColorTag('Connect', clGreen));
end;

procedure TfrmMonitor.SubscribeIBFeeds;
var
  InstrumentItem: TInstrumentItem;
  CurNode: PVirtualNode;
begin
  for InstrumentItem in TMonitorLists.InstrumentList.Values do
    for CurNode in InstrumentItem.NodeList do
     SubscribeIBFeed(CurNode);
end;

procedure TfrmMonitor.SubscribeIBFeed(const aNode: PVirtualNode);
var
  Data: PTreeData;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data) and (Data.DocType in [ntFactor, ntOrder]) then
    begin
      if (Data.DocType = ntFactor) then
        TIABMarket.RequestMarketData(Data.FactorDoc.ContractId)
      else if (Data.DocType = ntOrder) then
      begin
        if (Data.OrderDoc is TOrderIBDoc) then
          TIABMarket.RequestMarketData(Data.OrderDoc.Id)
        else if (Data.OrderDoc is TOrderNNDoc) and
                (TOrderNNDoc(Data.OrderDoc).FeedFromBroker = 0) and
                (TOrderNNDoc(Data.OrderDoc).ChildId > 0) then
          TIABMarket.RequestMarketData(TOrderNNDoc(Data.OrderDoc).ChildId);
      end;
    end;
  end;
end;

procedure TfrmMonitor.SubscribeNNFeeds;
var
  CurNode: PVirtualNode;
  InstrumentItem: TInstrumentItem;
  NNBroker: TBrokerHelperClass;
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'SubscribeNNFeeds');
  if miUseIBfeeds.Checked then
  begin
    NordNetBroker.PriceCache    := TMonitorLists.PriceCache;
    NordNetBroker.OnUpdatePrice := OnUpdatePriceForNN;
    NordNetBroker.OnUpdateTrade := OnUpdateTradeForNN;
    NordNetBroker.OnCloseFeed   := OnCloseFeedNN;
    NordNetBroker.OnUpdateOrder := OnUpdateOrder;
    NordNetBroker.OnTradeOrder  := OnTradeOrder;
    NordNetBroker.OnOpenFeed    := OnOpenFeedNN;

    NNBroker := TBrokerHelperFactory.GetBrokerHelperClass(TBrokerType.brNN.ToString);
    ConnectToNNBroker;
    for InstrumentItem in TMonitorLists.InstrumentList.Values do
      if Assigned(InstrumentItem) then
        for CurNode in InstrumentItem.NodeList do
          if Assigned(CurNode) then
            SubscribeNNFeed(CurNode);
    for InstrumentItem in TMonitorLists.InstrumentChildList.Values do
      if Assigned(InstrumentItem) then
        for CurNode in InstrumentItem.NodeList do
          if Assigned(CurNode) then
            SubscribeChildNNFeed(CurNode);
    ConnectedNN := NNBroker.Active;
  end;
  TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'SubscribeNNFeeds');
end;

procedure TfrmMonitor.UnsubscribeChildNNFeed(aNode: PVirtualNode);
var
  Data: PTreeData;
  i: Integer;
  ListIdentifier: TArray<string>;
  ListMarket: TArray<string>;
  NNBroker: TBrokerHelperClass;
begin
  if Assigned(aNode) then
  begin
    NNBroker := TBrokerHelperFactory.GetBrokerHelperClass(TBrokerType.brNN.ToString);
    Data := aNode^.GetData;
    if Assigned(Data) and
       Assigned(Data.OrderDoc) and
       (Data.OrderDoc is TOrderNNDoc) and
       (TOrderNNDoc(Data.OrderDoc).FeedFromBroker = 1) and
       (TOrderNNDoc(Data.OrderDoc).ChildId > 0)  then
    begin
      ListIdentifier := TOrderNNDoc(Data.OrderDoc).ChildIdentifierList.Split([';']);
      ListMarket := TOrderNNDoc(Data.OrderDoc).ChildMarketList.Split([';']);
      if (Length(ListIdentifier) >= Length(ListMarket)) then
        for i := Low(ListIdentifier) to High(ListIdentifier) do
          NNBroker.UnsubscribeAllFeeds(ListIdentifier[i], StrToInt(ListMarket[i]));
    end;
  end;
end;

procedure TfrmMonitor.UnsubscribeNNFeed(const aNode: PVirtualNode);
var
  Data: PTreeData;
  DataId : Integer;
  i: Integer;
  ListIdentifier: TStringList;
  ListMarket: TStringList;
  NNBrokerClass: TBrokerHelperClass;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data) then
    begin
      DataId := -1;
      ListIdentifier := TStringList.Create;
      ListMarket := TStringList.Create;
      try
        if (Assigned(Data.FactorDoc) and (Data.FactorDoc.BrokerType = TBrokerType.brNN)) then
        begin
          DataId := Data.FactorDoc.NNId;
          ExtractStrings([';'], [], PChar(Data.FactorDoc.IdentifierList), ListIdentifier);
          ExtractStrings([';'], [], PChar(Data.FactorDoc.MarketList), ListMarket);
        end
        else if (Data.OrderDoc is TOrderNNDoc) then
        begin
          DataId := Data.OrderDoc.Id;
          ExtractStrings([';'], [], PChar(TOrderNNDoc(Data.OrderDoc).IdentifierList), ListIdentifier);
          ExtractStrings([';'], [], PChar(TOrderNNDoc(Data.OrderDoc).MarketList), ListMarket);
        end;

        if (DataId > 0) and not TMonitorLists.InstrumentList.IsNodeExists(DataId) then
          if (ListIdentifier.Count > 0) and (ListIdentifier.Count = ListMarket.Count) then
          begin
            NNBrokerClass := TBrokerHelperFactory.GetBrokerHelperClass(TBrokerType.brNN.ToString);
            for i := 0 to ListIdentifier.Count - 1 do
              NNBrokerClass.UnsubscribeAllFeeds(ListIdentifier[i], StrToInt(ListMarket[i]));
          end;
      finally
        FreeAndNil(ListIdentifier);
        FreeAndNil(ListMarket);
      end;
    end;
  end;
end;

procedure TfrmMonitor.SubscribeChildNNFeed(aNode: PVirtualNode);
var
  Data: PTreeData;
  i: Integer;
  ListIdentifier: TArray<string>;
  ListMarket: TArray<string>;
  NNBroker: TBrokerHelperClass;
begin
  if Assigned(aNode) then
  begin
    NNBroker := TBrokerHelperFactory.GetBrokerHelperClass(TBrokerType.brNN.ToString);
    Data := aNode^.GetData;
    if Assigned(Data) and
       Assigned(Data.OrderDoc) and
       (Data.OrderDoc is TOrderNNDoc) and
       (TOrderNNDoc(Data.OrderDoc).FeedFromBroker = 1) and
       (TOrderNNDoc(Data.OrderDoc).ChildId > 0)  then
    begin
      ListIdentifier := TOrderNNDoc(Data.OrderDoc).ChildIdentifierList.Split([';']);
      ListMarket := TOrderNNDoc(Data.OrderDoc).ChildMarketList.Split([';']);
      if (Length(ListIdentifier) >= Length(ListMarket)) then
        for i := Low(ListIdentifier) to High(ListIdentifier) do
          NNBroker.SubscribeAllFeeds(ListIdentifier[i], StrToInt(ListMarket[i]));
    end;
  end;
end;

procedure TfrmMonitor.SubscribeCurrency;
const
  C_SQL = 'SELECT S.CONID, S.LOCAL_SYMBOL ' +
          '  FROM SOKID_IB S ' +
          ' WHERE S.CONTRACTYPE = ''CASH''';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL;
    try
      Query.Prepare;
      Query.Open;
    except
      on E: Exception do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SubscribeCurrency', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;

    while not Query.Eof do
    begin
      if not Query.FieldByName('LOCAL_SYMBOL').IsNull then
      begin
        TIABMarket.RequestMarketData(Query.FieldByName('CONID').AsInteger);
        TMonitorLists.CurrencyCache.AddOrSetValue(Query.FieldByName('LOCAL_SYMBOL').AsString, Query.FieldByName('CONID').AsInteger);
      end;
      Query.Next;
    end;
    Query.Close;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TfrmMonitor.SubscribeNNFeed(const aNode: PVirtualNode);
var
  Data: PTreeData;
  i: Integer;
  ListIdentifier: TStringList;
  ListMarket: TStringList;
  NNBroker: TBrokerHelperClass;
begin
  if Assigned(aNode) then
  begin
    NNBroker := TBrokerHelperFactory.GetBrokerHelperClass(TBrokerType.brNN.ToString);
    Data := aNode^.GetData;
    if NNBroker.Active and Assigned(Data) then
    begin
      ListIdentifier := TStringList.Create;
      ListMarket := TStringList.Create;
      try
        if Assigned(Data.OrderDoc) and (Data.OrderDoc is TOrderNNDoc) then
        begin
          ExtractStrings([';'], [], PChar(TOrderNNDoc(Data.OrderDoc).IdentifierList), ListIdentifier);
          ExtractStrings([';'], [], PChar(TOrderNNDoc(Data.OrderDoc).MarketList), ListMarket);
        end
        else if Assigned(Data.FactorDoc) then
        begin
          ExtractStrings([';'], [], PChar(Data.FactorDoc.IdentifierList), ListIdentifier);
          ExtractStrings([';'], [], PChar(Data.FactorDoc.MarketList), ListMarket);
        end;
        if (ListIdentifier.Count > 0) and (ListIdentifier.Count = ListMarket.Count) then
          for i := 0 to ListIdentifier.Count - 1 do
            NNBroker.SubscribeAllFeeds(ListIdentifier[i], StrToInt(ListMarket[i]));
      finally
        FreeAndNil(ListIdentifier);
        FreeAndNil(ListMarket);
      end;
    end;
  end;
end;

procedure TfrmMonitor.ActivateChildOrdersNN(const aNode: PVirtualNode);
var
  Child: PVirtualNode;
  Data: PTreeData;
begin
  if Assigned(aNode) then
  begin
    Child := aNode.FirstChild;
    while Assigned(Child) do
    begin
      Data := Child^.GetData;
      if Assigned(Data) and Assigned(Data.OrderDoc) then
        Data^.OrderDoc.IsActivateChild := True;
      Child := Child.NextSibling;
    end;
  end;
end;

procedure TfrmMonitor.SetChildsFreeze(const aNode: PVirtualNode);

  procedure SetNodeFreeze(aNode: PVirtualNode);
  var
    Child: PVirtualNode;
    Data: PTreeData;
  begin
    if Assigned(aNode) then
    begin
      Data := aNode^.GetData;
      if Assigned(Data) and Data^.Enabled then
      begin
        case Data^.DocType of
          ntOrderGroup:
            if Assigned(Data^.OrderGroupDoc) then
              Data^.OrderGroupDoc.Freeze;
          ntOrder:
            if Assigned(Data^.OrderDoc) and (Data^.OrderDoc.OrderStatus <> osSleeping) then
              Data^.OrderDoc.Freeze;
          ntCondition:
            if Assigned(Data^.ConditionDoc) then
              Data^.ConditionDoc.Freeze;
          ntAlgos:
            if Assigned(Data^.AlgosDoc) then
              Data^.AlgosDoc.Freeze;
          ntFactor:
            if Assigned(Data^.FactorDoc) then
              Data^.FactorDoc.Freeze;
        end;

        if Assigned(Data^.OrderDoc) then
        begin
          if (Data^.OrderDoc.OrderStatus <> osSleeping) then
            Data^.Enabled := False;
        end
        else
          Data^.Enabled := False;
      end;

      Child := aNode.FirstChild;
      while Assigned(Child) do
      begin
        SetNodeFreeze(Child);
        Child := Child.NextSibling;
      end;
    end;
  end;

begin
  EnterCriticalSection(FCriticalSection);
  try
    SetNodeFreeze(aNode);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TfrmMonitor.SetChildsEnabled(const aNode: PVirtualNode; const aEnabled: Boolean);

  procedure SetNodeEnabled(const aNode: PVirtualNode);
  var
    Child: PVirtualNode;
    Data: PTreeData;
  begin
    if Assigned(aNode) then
    begin
      Data := aNode^.GetData;
      Data^.Enabled := aEnabled;

      Child := aNode.FirstChild;
      while Assigned(Child) do
      begin
        SetNodeEnabled(Child);
        Child := Child.NextSibling;
      end;
    end;
  end;

begin
  EnterCriticalSection(FCriticalSection);
  try
    SetNodeEnabled(aNode);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TfrmMonitor.OrdersExecuteOcaGroup(const aOcaGroupNumber: Integer);
begin
  TTask.Create(
    procedure()
    var
      Data : PTreeData;
      Node : PVirtualNode;
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'OrdersExecuteOcaGroup', 'Oca Group Number: ' + aOcaGroupNumber.ToString);
      for Node in TMonitorLists.OrderList.Keys do
        if Assigned(Node) then
        begin
          Data := Node^.GetData;
          if Assigned(Data) and Assigned(Data.OrderDoc) and
            (Data.OrderDoc is TOrderIBDoc) and
            (Data.OrderDoc.OrderStatus in [osSleeping]) and
            (TOrderIBDoc(Data.OrderDoc).OcaGroupNumber = aOcaGroupNumber) then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                Node.CheckState := csCheckedNormal;
                Data.OrderDoc.Buy;
              end);
          end;
        end;
    end).Start;
end;

function TfrmMonitor.IsCreateOrModify(const aNode, aOrderGroupNode: PVirtualNode): Boolean;

  function IsOrdersEqual(const aOrder1, aOrder2: TCustomOrderDoc): Boolean;
  begin
    Result := (aOrder1.OrderAction = aOrder2.OrderAction) and
              (aOrder1.BrokerType = aOrder2.BrokerType) and
              (aOrder1.OrderType = aOrder2.OrderType) and
              (aOrder1.Id = aOrder2.Id) and
              (aOrder1.Currency = aOrder2.Currency) and
              (aOrder1.OrderType = aOrder2.OrderType);
  end;

var
  RunNode: PVirtualNode;
  CurrentData: PTreeData;
  RunData: PTreeData;
  OrderList: TList<PVirtualNode>;
begin
  TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'IsCreateOrModify');
  Result := True;
  if Assigned(aNode) and Assigned(aOrderGroupNode) then
  begin
    CurrentData := aNode^.GetData;
    if Assigned(CurrentData^.OrderDoc) and (CurrentData^.OrderDoc.OrderStatus = osSleeping) then
    begin
      Result := True;
      OrderList := GetListOfChilds(aOrderGroupNode, False, ntOrder);
      try
        for RunNode in OrderList do
          if (RunNode <> aNode) then
          begin
            RunData := RunNode^.GetData;
            if Assigned(RunData^.OrderDoc) and
              (not (RunData^.OrderDoc.OrderStatus in [osFilled, osCancelled, osPendCancel, osError, osNotConsidered])) and
              (RunData^.OrderDoc.OrderIBId > 0) then
            begin
              if RunData^.OrderDoc.IsFinal then
                Exit(False);

              if IsOrdersEqual(CurrentData^.OrderDoc, RunData^.OrderDoc) then
              begin
                Result := False;
                RunData^.OrderDoc.Limit := CurrentData^.OrderDoc.Limit;
                if (RunData^.OrderDoc.OrderStatus <> osPartlyFilled) then //if Quantity = 0 then execute remaining volume
                  RunData^.OrderDoc.Quantity := RunData^.OrderDoc.Quantity - RunData^.OrderDoc.Filled;  //real remaining volume
                if (RunData^.OrderDoc.BrokerType = brIB) and (CurrentData^.OrderDoc.BrokerType = brIB) then
                  TOrderIBDoc(RunData^.OrderDoc).AuxPrice := TOrderIBDoc(CurrentData^.OrderDoc).AuxPrice;
                CurrentData^.OrderDoc.OrderStatus := osCancelled;
                aNode.CheckState := csUncheckedNormal;
                RunData^.OrderDoc.UpdateOrder;
                SetChildsFreeze(aNode);

                TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText,
                                                                         RunData^.OrderDoc.OrderIBId,
                                                                         RunData^.OrderDoc.Quantity,
                                                                         RunData^.OrderDoc.Filled,
                                                                         'IsCreateOrModify',
                                                                         RunData^.OrderDoc.Symbol,
                                                                         RunData^.OrderDoc.Info,
                                                                         RunData^.OrderDoc.OrderStatus,
                                                                         RunData^.OrderDoc.OrderAction,
                                                                         RunData^.OrderDoc.LastPrice
                                                                         );
              end
              else
              begin
                RunData^.OrderDoc.CancelOrder;
                CurrentData^.OrderDoc.Quantity := RunData^.OrderDoc.Quantity - RunData^.OrderDoc.Filled;  //real remaining volume
                Result := CurrentData^.OrderDoc.Quantity > 0;
                TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddText,
                                                                         CurrentData^.OrderDoc.OrderIBId,
                                                                         CurrentData^.OrderDoc.Quantity,
                                                                         CurrentData^.OrderDoc.Filled,
                                                                         'IsCreateOrModify',
                                                                         CurrentData^.OrderDoc.Symbol,
                                                                         CurrentData^.OrderDoc.Info,
                                                                         CurrentData^.OrderDoc.OrderStatus,
                                                                         CurrentData^.OrderDoc.OrderAction,
                                                                         CurrentData^.OrderDoc.LastPrice
                                                                         );
              end;
              Break;
            end;
          end;
      finally
        FreeAndNil(OrderList);
      end;
    end
    else
      Result := False;
  end;
  TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'IsCreateOrModify');
end;

procedure TfrmMonitor.CancelSiblingsOrders(const aNode: PVirtualNode);
var
  OrderGroupNode: PVirtualNode;
  OrderGroupData: PTreeData;
  OrderList: TList<PVirtualNode>;
  RunData: PTreeData;
begin
  OrderGroupNode := GetParentNode(aNode, ntOrderGroup);
  if Assigned(OrderGroupNode) then
  begin
    OrderGroupData := OrderGroupNode^.GetData;
    if (OrderGroupData.OrderGroupDoc.Kind = okModifyOrder) then
    begin
      OrderList := GetListOfChilds(OrderGroupNode, False, ntOrder);
      try
        for var RunNode in OrderList do
          if (RunNode <> aNode) then
          begin
            RunData := RunNode^.GetData;
            if Assigned(RunData^.OrderDoc) and (RunData^.OrderDoc.OrderStatus in [osSleeping]) then
              RunData^.OrderDoc.CancelOrder;
          end;
      finally
        FreeAndNil(OrderList);
      end;
    end;
  end;
end;

procedure TfrmMonitor.OrdersExecuteOneCancelAll(const aNode: PVirtualNode);
var
  Data: PTreeData;
  OrderGroupNode: PVirtualNode;
  ParentGroupNode: PVirtualNode;
  GroupList: TList<PVirtualNode>;

  procedure UncheckOrderGroup(const aOrderGroupNode: PVirtualNode);
  var
    OrderGroupList: TList<PVirtualNode>;
    CurrNode: PVirtualNode;
    Data: PTreeData;
  begin
    if Assigned(aOrderGroupNode) then
    begin
      Data := aOrderGroupNode^.GetData;
      Data^.OrderGroupDoc.IsFrozen := True;
      OrderGroupList := GetListOfChilds(aOrderGroupNode, False, ntOrder);
      try
        for CurrNode in OrderGroupList do
          if (CurrNode <> aNode) then
          begin
            Data := CurrNode^.GetData;
            if Assigned(Data^.OrderDoc) and
              (Data^.OrderDoc.OrderStatus = osSleeping) then
            begin
              Data^.OrderDoc.OrderStatus := osNotConsidered;
              CurrNode.CheckState        := csUncheckedNormal;
              SetChildsFreeze(CurrNode);
              SetIcon(CurrNode);
              vstMonitor.InvalidateNode(CurrNode);
            end;
          end;
      finally
        FreeAndNil(OrderGroupList);
      end;
    end;
  end;

begin
  if Assigned(aNode) then
  begin
    OrderGroupNode := GetParentNode(aNode, ntOrderGroup);
    if Assigned(OrderGroupNode) then
    begin
      Data := OrderGroupNode^.GetData;
      if Assigned(Data^.OrderGroupDoc) and
        (Data.OrderGroupDoc.Kind = okOneCancelAll) then
        UncheckOrderGroup(OrderGroupNode);

      ParentGroupNode := OrderGroupNode.Parent;
      if Assigned(ParentGroupNode) then
        Data := ParentGroupNode^.GetData
      else
        Data := nil;
      if Assigned(Data) and Assigned(Data^.OrderGroupDoc) and
        (Data.OrderGroupDoc.Kind = okOneCancelAll) then
      begin
        GroupList := GetListOfChilds(ParentGroupNode, False, ntOrderGroup);
        try
          for var Run in GroupList do
            if (Run <> OrderGroupNode) then
            begin
              Data := Run^.GetData;
              if Assigned(Data^.OrderGroupDoc) then
                UncheckOrderGroup(Run);
            end;
        finally
          FreeAndNil(GroupList);
        end;
      end;
    end;
  end
end;

procedure TfrmMonitor.OrdersExecuteSequential(const aNode: PVirtualNode);
var
  ParentNode: PVirtualNode;
  NextNode: PVirtualNode;
  Data: PTreeData;
begin
  //Check orders in sequential OrderGroups
  ParentNode := GetParentNode(aNode, ntOrderGroup);
  if Assigned(ParentNode) then
  begin
    Data := ParentNode^.GetData;
    if Assigned(Data^.OrderGroupDoc) and (Data^.OrderGroupDoc.Kind in [okSequentialContinue, okSequentialStop]) then
    begin
      NextNode := aNode.NextSibling;
      while Assigned(NextNode) do
      begin
        Data := NextNode^.GetData;
        if (NextNode.CheckState = csCheckedNormal) and Assigned(Data^.OrderDoc) and (Data^.OrderDoc.OrderStatus = osSleeping) then
        begin
          Data^.OrderDoc.Buy;
          NextNode := nil;
        end
        else
          NextNode := NextNode.NextSibling;
      end;
    end;
  end;
end;

procedure TfrmMonitor.CheckRepetitive(const aNode: PVirtualNode);
var
  Data: PTreeData;
  OrderGroupData: PTreeData;
  OrderGroupNode: PVirtualNode;
begin
  if Assigned(aNode) then
  begin
    OrderGroupNode := GetParentNode(aNode, ntOrderGroup);
    if Assigned(OrderGroupNode) then
    begin
      OrderGroupData := OrderGroupNode^.GetData;
      if Assigned(OrderGroupData) and Assigned(OrderGroupData.OrderGroupDoc) and OrderGroupData.OrderGroupDoc.IsRepetitive then
      begin
        Data := aNode^.GetData;
        if Assigned(Data) and Assigned(Data.OrderDoc) and (Data.OrderDoc.OrderStatus in [osFilled, osPartlyFilled]) then
         ;
      end;
    end;
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'CheckRepetitive', '');
  end;
end;

procedure TfrmMonitor.Disconnect;
var
  Broker: TBrokerHelperClass;
begin
  Connected := False;
  IABClient.Disconnect;
  Broker := TBrokerHelperFactory.GetBrokerHelperClass(TBrokerType.brTest.ToString);
  if Assigned(Broker) then
  begin
    Broker.SetUpdatePrice(nil);
    Broker.Leave;
  end;

  Broker := TBrokerHelperFactory.GetBrokerHelperClass(TBrokerType.brNN.ToString);
  if Assigned(Broker) then
    Broker.Leave;

  btnLogin.ImageIndex := 53;
  btnLogin.Hint := 'Connect';
  ConnectedIB := False;
  ConnectedNN := False;
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, THtmlLib.GetColorTag('Disconnect', clGreen));
end;

procedure TfrmMonitor.CheckAllOrders(aNode: PVirtualNode);
var
  CurrNode: PVirtualNode;
  Data: PTreeData;
begin
  CurrNode := aNode.FirstChild;
  while Assigned(CurrNode) do
  begin
    CheckAllOrders(CurrNode);
    Data := CurrNode^.GetData;
    if Assigned(Data) and Assigned(Data^.OrderDoc) and (Data^.OrderDoc.OrderStatus = osSleeping) then
    begin
      CurrNode.CheckState := csCheckedNormal;
      SetIcon(CurrNode);
      vstMonitor.InvalidateNode(CurrNode);
    end;
    CurrNode := CurrNode.NextSibling;
  end;
end;

procedure TfrmMonitor.ActivateAllOrders(aNode: PVirtualNode);
var
  CurrNode: PVirtualNode;
  Data: PTreeData;
begin
  CurrNode := aNode.FirstChild;
  while Assigned(CurrNode) do
  begin
    Data := CurrNode^.GetData;
    if Assigned(Data) and
       Assigned(Data^.OrderDoc) and
       (Data^.OrderDoc.OrderStatus = osSleeping) and
      (CurrNode.CheckState = csCheckedNormal) then
    begin
      TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'ActivateAllOrders', Data^.OrderDoc.Description);
      if (not ExistsChildConditions(CurrNode)) then
        Data^.OrderDoc.Buy
      else
        ResetChildConditions(CurrNode);
    end;
    ActivateAllOrders(CurrNode);
    CurrNode := CurrNode.NextSibling;
  end;
end;

procedure TfrmMonitor.ResetChildConditions(const aOrderNode: PVirtualNode);
var
  arr: TNodeArray;
  Data: PTreeData;
begin
  if Assigned(aOrderNode) then
  begin
    arr := TTreeDocument.GetNodesListByType(vstMonitor, aOrderNode, TDocType.ntCondition);
    for var Node in arr do
      if Assigned(Node) then
      begin
        Data := Node^.GetData;
        if Assigned(Data^.ConditionDoc) and Data.Enabled then
        begin
          Data^.ConditionDoc.IsCondition := False;
          UpdateCondition(Node);
        end;
      end;
  end;
end;

procedure TfrmMonitor.aActivateAllExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstMonitor.GetFirst;
  while Assigned(Node) do
  begin
    if (TTreeDocument.GetDocType(vstMonitor, Node) = ntOrder) then
    begin
      if (aActivateAll.ImageIndex = 12) then
      begin
        Node.CheckState := csCheckedNormal;
        ResetChildConditions(Node);
      end
      else
        Node.CheckState := csUncheckedNormal;
      SetIcon(Node);
    end;
    Node := vstMonitor.GetNext(Node);
  end;
  if (aActivateAll.ImageIndex = 12) then
    aActivateAll.ImageIndex := 13
  else
    aActivateAll.ImageIndex := 12;
end;

procedure TfrmMonitor.aActivateOrdersExecute(Sender: TObject);
begin
  if Assigned(vstMonitor.FocusedNode) then
  begin
    CheckAllOrders(vstMonitor.FocusedNode);
    ActivateAllOrders(vstMonitor.FocusedNode);
  end;
end;

procedure TfrmMonitor.aActivateOrdersUpdate(Sender: TObject);
begin
  aActivateOrders.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntOrderGroup];
  aActivateOrders.Enabled := aActivateOrders.Visible and Connected;
end;

procedure TfrmMonitor.aAddAlgosExecute(Sender: TObject);
var
  Data       : PTreeData;
  NewNode    : PVirtualNode;
  ParentNode : PVirtualNode;
begin
  if Assigned(vstMonitor.FocusedNode) then
  begin
    if IsAutoorderGroup(vstMonitor.FocusedNode) and not General.AllowEnterAutoorder then
      Exit;

    ParentNode := vstMonitor.FocusedNode;
    NewNode    := TTreeDocument.CreateAlgos(ParentNode, vstMonitor);
    Data       := NewNode^.GetData;
    SetIcon(NewNode);

    if (TfrmEditAlgos.ShowDocument(Data.AlgosDoc) <> mrOk) then
      vstMonitor.DeleteNode(NewNode)
    else
    begin
      TMonitorLists.AlgosList.AddItem(NewNode);
      AddAlgosToConditionAlgosChart(NewNode);
      AddAlgosToRealtimeChart(ParentNode, NewNode);
      UpdateAlgos(NewNode);
    end;
  end;
end;

procedure TfrmMonitor.aAddAlgosUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntCondition, ntAlgos];
  TAction(Sender).Enabled := TAction(Sender).Visible and Connected;
end;

procedure TfrmMonitor.aAddConditionExecute(Sender: TObject);
var
  Data: PTreeData;
  NewNode, ParentNode: PVirtualNode;
begin
  if Assigned(vstMonitor.FocusedNode) then
  begin
    if IsAutoorderGroup(vstMonitor.FocusedNode) and not General.AllowEnterAutoorder then
      Exit
    else if (TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) = ntCondition) then
      ParentNode := vstMonitor.FocusedNode.Parent
    else
      ParentNode := vstMonitor.FocusedNode;

    NewNode := TTreeDocument.CreateCondition(ParentNode, vstMonitor);
    Data := NewNode^.GetData;
    SetIcon(NewNode);

    if (TfrmEditCondition.ShowDocument(Data.ConditionDoc) <> mrOk) then
      vstMonitor.DeleteNode(NewNode)
    else
    begin
      AddConditionToRealtimeChart(NewNode);
      vstMonitor.Expanded[ParentNode] := True;
    end;
  end;
end;

procedure TfrmMonitor.aAddConditionUpdate(Sender: TObject);
begin
  aAddCondition.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntOrder, ntCondition];
  aAddCondition.Enabled := aAddCondition.Visible and Connected;
end;

procedure TfrmMonitor.aAddFactorExecute(Sender: TObject);
var
  Data: PTreeData;
  NewNode, ParentNode: PVirtualNode;
begin
  if Assigned(vstMonitor.FocusedNode) then
  begin
    if IsAutoorderGroup(vstMonitor.FocusedNode) and not General.AllowEnterAutoorder then
      Exit
    else
    begin
      TfrmSearchInstruments.ShowDocument(Self);
      Exit;
    end;

    vstMonitor.BeginUpdate;
    try
      if TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) = ntFactor then
        ParentNode := vstMonitor.FocusedNode.Parent
      else
        ParentNode := vstMonitor.FocusedNode;

      NewNode := TTreeDocument.CreateFactor(ParentNode, vstMonitor);
      Data    := NewNode^.GetData;
      Data^.FactorDoc.BrokerType     := TBrokerType.brIB;
      Data^.FactorDoc.ContractId     := DMod.fbtInstrumentsCONID.AsInteger;
      Data^.FactorDoc.IBId           := Data^.FactorDoc.ContractId;
      Data^.FactorDoc.InstrumentName := DMod.fbtInstrumentsNAME.AsString;
      Data^.FactorDoc.Currency       := DMod.fbtInstrumentsCURRENCY.AsString;
      Data^.FactorDoc.Exchange       := DMod.fbtInstrumentsEXCHANGE.AsString;
      Data^.FactorDoc.ContractType   := DMod.fbtInstrumentsCONTRACTYPE.AsString;
      Data^.FactorDoc.NNID           := DMod.fbtInstrumentsCONID.AsInteger;
      Data^.FactorDoc.TickType1      := ttLast;
      Data^.FactorDoc.TickType2      := ttNotSet;

      TIABMarket.RequestMarketData(DMod.fbtInstrumentsCONID.AsInteger);
      TMonitorLists.InstrumentList.AddNode(DMod.fbtInstrumentsCONID.AsInteger, NewNode);
      Data^.FactorDoc.CurrentValue := GetLastPrice(Data^.FactorDoc.ContractId, Data^.FactorDoc.TickType1);
      if (Data^.FactorDoc.CurrentValue > 0) then
        TMonitorLists.InstrumentList.DocumentQueue.DeleteItem(NewNode);

      SetIcon(NewNode);
      vstMonitor.Expanded[ParentNode] := True;
      ParentNode := GetParentNode(NewNode, ntAlgos);
      if Assigned(ParentNode) then
        UpdateAlgos(ParentNode);
    finally
      vstMonitor.EndUpdate;
    end;
  end;
end;

procedure TfrmMonitor.aAddFactorUpdate(Sender: TObject);
begin
  aAddFactor.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntOrderGroup, ntCondition, ntAlgos, ntFactor];
  aAddFactor.Enabled := aAddFactor.Visible and Connected;
end;

procedure TfrmMonitor.aAddOrderGroupExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Node := TTreeDocument.CreateOrderGroup(GetCurrentOrderGroupSet, vstMonitor);
  Data := Node^.GetData;
  SetIcon(Node);
  if (TfrmEditOrderGroup.ShowDocument(Data.OrderGroupDoc) <> mrOk) then
    vstMonitor.DeleteNode(Node);
end;

procedure TfrmMonitor.aAddOrderGroupUpdate(Sender: TObject);
begin
  aAddOrderGroup.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntOrderGroup, ntOrderGroupSet, ntUnknow];
  aAddOrderGroup.Enabled := aAddOrderGroup.Visible and Connected;
end;

procedure TfrmMonitor.aCancelBrokerOrderExecute(Sender: TObject);
var
  Data: PTreeData;
begin
  if Assigned(vstMonitor.FocusedNode) then
  begin
    Data := vstMonitor.FocusedNode^.GetData;
    if Assigned(Data) and Assigned(Data^.OrderDoc) then
      case Data^.OrderDoc.OrderStatus of
        osSleeping:
          TMessageDialog.ShowWarning(rsOrderCancelSleeping);
        osFilled, osPartlyFilled:
          TMessageDialog.ShowWarning(rsOrderCancelFilled);
        osCancelled:
          TMessageDialog.ShowWarning(rsOrderCancelCancelled);
      else
        begin
          DeactivateChildConditions(vstMonitor.FocusedNode);
          Data^.OrderDoc.CancelOrder;
          if General.IsShowCancelBox then
            TMessageDialog.ShowInfo(rsOrderCancellation);
        end
      end;
  end;
end;

procedure TfrmMonitor.aCancelBrokerOrderUpdate(Sender: TObject);
var
  Data: PTreeData;
begin
  aCancelBrokerOrder.Visible := False;
  aCancelBrokerOrder.Enabled := Connected;
  if Assigned(vstMonitor.FocusedNode) then
  begin
    Data := vstMonitor.FocusedNode^.GetData;
    if Assigned(Data) and Assigned(Data.OrderDoc) then
      aCancelBrokerOrder.Visible := True;
  end;
end;

procedure TfrmMonitor.aCheckIBInstrumentsExecute(Sender: TObject);
begin
  inherited;
  if (TMessageDialog.ShowQuestion(rsCheckInstruments) = mrYes) then
  begin
    TfrmEventController.ShowDocument(teCheckInstruments);
    SokidList.CheckInstruments;
  end;
end;

procedure TfrmMonitor.aCheckLastPriceExecute(Sender: TObject);
begin
  inherited;
  if (TMessageDialog.ShowQuestion(rsUpdatePrice) = mrYes) then
  begin
    TfrmEventController.ShowDocument(teCheckLastPrice);
    SokidList.CheckLastPrice;
  end;
end;

procedure TfrmMonitor.aCloseExecute(Sender: TObject);
begin
  Application.MainForm.Close;
end;

procedure TfrmMonitor.aDeleteAllNodesExecute(Sender: TObject);
begin
  if (TMessageDialog.ShowQuestion('All nodes be deleted. Continue?') = mrYes) then
    DeleteAllNodes;
end;

procedure TfrmMonitor.aDeleteAllNodesUpdate(Sender: TObject);
begin
  aDeleteAllNodes.Enabled  := Connected;
end;

function TfrmMonitor.IsNodePossibleToRemove(aNode: PVirtualNode): Boolean;
var
  Data: PTreeData;
begin
  Result := False;
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data) then
    begin
      Result := True;// (Data.Re_CreationType = ctUser);
      if Assigned(Data.OrderDoc) then
        Result := not(Data.OrderDoc.OrderStatus in [osPendSubmit, osPreSubmit])
    end;
  end;
end;

procedure TfrmMonitor.aDeleteNodeExecute(Sender: TObject);
begin
  if vstMonitor.Focused and IsNodePossibleToRemove(vstMonitor.FocusedNode) then
  begin
    DeleteNode(vstMonitor.FocusedNode);
    if vstMonitor.IsEmpty then
      DeleteAllNodes;
  end;
end;

procedure TfrmMonitor.aDeleteNodeUpdate(Sender: TObject);
begin
  aDeleteNode.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) <> ntUnknow;
  aDeleteNode.Enabled := aDeleteNode.Visible and Connected and IsNodePossibleToRemove(vstMonitor.FocusedNode);
end;

procedure TfrmMonitor.DeleteNode(aNode: PVirtualNode; aUpdateAlgos: Boolean = True);
var
  ParentNode: PVirtualNode;
  ParentData: PTreeData;
begin
  if Assigned(aNode) then
  begin
    vstMonitor.BeginUpdate;
    try
      ParentNode := aNode.Parent;
      DeleteChildren(aNode, aUpdateAlgos);
      FreeNodeData(aNode);
      vstMonitor.DeleteNode(aNode);
      if aUpdateAlgos then
        while Assigned(ParentNode) and (vstMonitor.GetNodeLevel(ParentNode) > 1) do
        begin
          ParentData := ParentNode^.GetData;
          if Assigned(ParentData) then
          begin
            if Assigned(ParentData.ConditionDoc) then
              UpdateCondition(ParentNode)
            else if Assigned(ParentData.AlgosDoc) then
              UpdateAlgos(ParentNode);
          end;
          ParentNode := ParentNode.Parent;
        end;
    finally
      vstMonitor.EndUpdate;
    end;
  end;
end;

procedure TfrmMonitor.DeleteChildren(aNode: PVirtualNode; aUpdateAlgos: Boolean = True);
var
  Run: PVirtualNode;
  Mark: PVirtualNode;
begin
  if Assigned(aNode) and (aNode.ChildCount > 0) then
  begin
    Run := aNode.LastChild;
    while Assigned(Run) do
    begin
      Mark := Run;
      Run := Run.PrevSibling;
      if Assigned(Run) then
        Run.NextSibling := nil;

      DeleteNode(Mark, aUpdateAlgos);
    end;
    aNode.FirstChild := nil;
    aNode.LastChild  := nil;
  end;
end;

procedure TfrmMonitor.FreeNodeData(aNode: PVirtualNode);
var
  Data: PTreeData;
  loInstrumentItem: TInstrumentItem;
begin
  TMonitorLists.OrderList.DeleteOrder(aNode);
  TMonitorLists.AlgosList.DeleteAlgos(aNode);
  FConditionQueue.DeleteItem(aNode);

  for loInstrumentItem in TMonitorLists.InstrumentList.Values do
  begin
    if (loInstrumentItem.NodeList.Contains(aNode)) then
    begin
      loInstrumentItem.NodeList.Remove(aNode);
      if (loInstrumentItem.NodeList.Count = 0) then
        TIABMarket.CancelMarketData(loInstrumentItem.Id);
    end;
  end;
  TMonitorLists.InstrumentList.DeleteNode(aNode);
  TMonitorLists.InstrumentList.DocumentQueue.DeleteItem(aNode);

  UnsubscribeNNFeed(aNode);
  UnsubscribeChildNNFeed(aNode);

  Data := aNode^.GetData;
  if Assigned(Data) then
  begin
    if Assigned(frmConditionRealtimeChart) then
    begin
      if (Data^.DocType = ntAlgos) then
        frmConditionRealtimeChart.DeleteAlgos(aNode)
      else if (Data^.DocType = ntCondition) then
        frmConditionRealtimeChart.DeleteSeries(aNode);
    end;
    Data^.Clear;
  end;
end;

procedure TfrmMonitor.aShowEditDialogExecute(Sender: TObject);
var
  Data: PTreeData;
begin
  if Assigned(vstMonitor.FocusedNode) then
  begin
    Data := vstMonitor.FocusedNode^.GetData;
    case Data^.DocType of
      ntOrderGroup:
        TfrmEditOrderGroup.ShowDocument(Data^.OrderGroupDoc);
      ntCondition:
        begin
//          UpdateCondition(vstMonitor.FocusedNode);
          if (TfrmEditCondition.ShowDocument(Data^.ConditionDoc) = mrOk) then
          begin
            if (Data^.ConditionDoc.CondType <> ctTimeGap) then
              UpdateChildAlgos(vstMonitor.FocusedNode);
            if (Data^.ConditionDoc.CondType in [ctRealtimeValue, ctRealtimeAndTimeGap]) then
              TTreeDocument.SetTickTypesForChildFactors(vstMonitor, vstMonitor.FocusedNode);

            if (vstMonitor.FocusedNode.ChildCount > 0) then
              UpdateCondition(vstMonitor.FocusedNode)
            else
              Data^.ConditionDoc.IsCondition := Data^.ConditionDoc.Bypass;

            if Assigned(Data^.ConditionChart) and (Data^.ConditionDoc.CondType in [ctRealtimeValue, ctRealtimeAndTimeGap]) then
              TfrmConditionChart(Data^.ConditionChart).Initialize(Data^.ConditionDoc.CondLimit);

            SetIcon(vstMonitor.FocusedNode);
          end;
        end;
      ntAlgos:
        begin
          UpdateAlgos(vstMonitor.FocusedNode);
          if (TfrmEditAlgos.ShowDocument(Data^.AlgosDoc) = mrOk) then
            UpdateAlgos(vstMonitor.FocusedNode);
        end;
      ntOrder:
        if (Data^.OrderDoc is TOrderIBDoc) then
        begin
//          TestOrder(vstMonitor.FocusedNode);
          if ((Data^.OrderDoc is TOrderIBDoc) and (TfrmEditOrderIB.ShowDocument(TOrderIBDoc(Data^.OrderDoc)) = mrOk)) then
            TestOrder(vstMonitor.FocusedNode);
        end
        else if (Data^.OrderDoc is TOrderNNDoc) then
        begin
          if (TfrmEditOrderNN.ShowDocument(TOrderNNDoc(Data^.OrderDoc), Data^.OrderDoc.ExtendedOptions.Subordination) = mrOk) then
            TestOrder(vstMonitor.FocusedNode);
        end;
      ntFactor:
        if (TfrmEditFactor.ShowDocument(Data^.FactorDoc) = mrOk) then
        begin
          UpdateAlgos(TTreeDocument.GetParentNode(vstMonitor, vstMonitor.FocusedNode, ntAlgos));
          UpdateCondition(TTreeDocument.GetParentNode(vstMonitor, vstMonitor.FocusedNode, ntCondition));
        end;
    end;
  end;
end;

procedure TfrmMonitor.aShowEditDialogUpdate(Sender: TObject);
begin
  aShowEditDialog.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntFactor, ntOrder, ntAlgos, ntOrderGroup, ntCondition];
  aShowEditDialog.Enabled := aShowEditDialog.Visible;
end;

procedure TfrmMonitor.aShowGlobalParametersExecute(Sender: TObject);
begin
  General.ShowParametersDialog;
  cbAccountsChange(nil);
end;

procedure TfrmMonitor.AddAlgosToRealtimeChart(aConditionNode, aAlgosNode: PVirtualNode);
begin
  if Assigned(aAlgosNode) and Assigned(frmConditionRealtimeChart) then
  begin
    TTask.Create(
      procedure()
      begin
        TThread.NameThreadForDebugging('TfrmMonitor.AddAlgosToRealtimeChart');
        if not Assigned(aConditionNode) then
          aConditionNode := GetParentNode(aAlgosNode, ntCondition);
        if Assigned(aConditionNode) then
        begin
          frmConditionRealtimeChart.AddAlgos(aConditionNode, aAlgosNode);
          frmConditionRealtimeChart.AddValue(aAlgosNode, 0);
        end;
      end).Start;
  end;
end;

procedure TfrmMonitor.AddConditionToRealtimeChart(aNode: PVirtualNode);
var
  Data: PTreeData;

  procedure AddAlgosToChart(aChildNode: PVirtualNode);
  var
    Child: PVirtualNode;
    DataAlgos: PTreeData;
  begin
    if Assigned(aChildNode) then
    begin
      DataAlgos := aChildNode^.GetData;
      if Assigned(DataAlgos) and Assigned(DataAlgos.AlgosDoc) then
        AddAlgosToRealtimeChart(aNode, aChildNode);
      Child := aChildNode.FirstChild;
      while Assigned(Child) do
      begin
        AddAlgosToChart(Child);
        Child := Child.NextSibling;
      end;
    end;
  end;

begin
  if Assigned(aNode) and Assigned(frmConditionRealtimeChart) then
  begin
    Data := aNode^.GetData;
    if Assigned(Data) and Assigned(Data.ConditionDoc) then
    begin
      frmConditionRealtimeChart.AddSeries(aNode, Data.ConditionDoc.Description);
      AddAlgosToChart(aNode);
    end;
  end;
end;

procedure TfrmMonitor.aShowMonitorConditionChartExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  if not Assigned(frmConditionRealtimeChart) then
    Application.CreateForm(TfrmConditionRealtimeChart, frmConditionRealtimeChart);

  EnterCriticalSection(FCriticalSection);
  try
    Node := vstMonitor.GetFirst;
    while Assigned(Node) do
    begin
      AddConditionToRealtimeChart(Node);
      Node := vstMonitor.GetNext(Node);
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
  frmConditionRealtimeChart.Show;
end;

procedure TfrmMonitor.aShowMonitorConditionChartUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := True;
  TAction(Sender).Enabled := Connected;
end;

procedure TfrmMonitor.aShowOrderStatusExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Node := vstMonitor.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if Assigned(Data) and Assigned(Data.OrderDoc) then
      TfrmEditOrderStatus.ShowDocument(Node)
    else
      TMessageDialog.ShowWarning('Order not assigned');
  end;
end;

procedure TfrmMonitor.aShowOrderStatusUpdate(Sender: TObject);
begin
  aShowOrderStatus.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntOrder];
  aShowOrderStatus.Enabled := aShowOrderStatus.Visible;
end;

procedure TfrmMonitor.aShowOrdersTimestampInfoExecute(Sender: TObject);
begin
  TInformationDialog.ShowMessage(TDocumentInfo.GetOrdersTimestamp, 'OrdersTimestamp');
end;

procedure TfrmMonitor.aOpenLogFileExecute(Sender: TObject);
begin
  if FileExists(LogWriter.LogFileName) then
    ShellExecute(Handle, 'open', PChar(LogWriter.LogFileName), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmMonitor.aTransformToBaseOrderExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Node := vstMonitor.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if Assigned(Data) and Assigned(Data.FactorDoc) then
      TransformToBaseOrder(Node);
  end;
end;

procedure TfrmMonitor.aTransformToBaseOrderUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntFactor];
  TAction(Sender).Enabled :=  TAction(Sender).Visible and Connected;
end;

procedure TfrmMonitor.aTransformToOrderExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Node := vstMonitor.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if Assigned(Data) and Assigned(Data.FactorDoc) then
    begin
      case Data.FactorDoc.BrokerType of
        TBrokerType.brIB:
          TransformToOrderIB(Node);
        TBrokerType.brNN:
          TransformToOrderNN(Node);
      end;
    end;
  end;
end;

procedure TfrmMonitor.aTransformToOrderUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntFactor];
  TAction(Sender).Enabled := TAction(Sender).Visible and Connected;
end;

procedure TfrmMonitor.aSaveGroupSetAsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not vstMonitor.IsEmpty) and (Connected) and (FOrderGroupSet.RecordId > -1);
end;

procedure TfrmMonitor.aRenameGroupUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not vstMonitor.IsEmpty) and (Connected) and (FOrderGroupSet.RecordId > -1);
end;

procedure TfrmMonitor.aOpenLogFileUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FileExists(LogWriter.LogFileName);
end;

procedure TfrmMonitor.aOpenRelationExecute(Sender: TObject);
var
  Data: PTreeData;
begin
  if Assigned(vstMonitor.FocusedNode) then
  begin
    frmRelation := TfrmRelation.Create(nil);
    try
      Data := vstMonitor.FocusedNode^.GetData;
      frmRelation.eCalcFactor.Text := Format('%.5f', [Data.ReValue]);

      if vstMonitor.GetNodeLevel(vstMonitor.FocusedNode) = 0 then
        frmRelation.Lb_ObjName1.Caption := ''
      else
        frmRelation.Lb_ObjName1.Caption := GetLevelName(vstMonitor.FocusedNode.Parent);

      frmRelation.Lb_ObjName2.Caption := GetLevelName(vstMonitor.FocusedNode);
      if (frmRelation.ShowModal = mrOk) then
      begin
        Data.ReValue := StrToFloatDef(frmRelation.eCalcFactor.Text, 0.0);

        case TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode.Parent) of
          ntAlgos:
            UpdateAlgos(vstMonitor.FocusedNode.Parent);
          ntCondition:
            UpdateCondition(vstMonitor.FocusedNode.Parent);
        end;
      end;
    finally
      FreeAndNil(frmRelation);
    end;
  end;
end;

procedure TfrmMonitor.aOpenRelationUpdate(Sender: TObject);
begin
  aOpenRelation.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntAlgos, ntFactor];
  aOpenRelation.Enabled := aOpenRelation.Visible;
end;

procedure TfrmMonitor.ApplicationEventsException(Sender: TObject; E: Exception);
var
  StackTrace: string;
begin
  TfrmSplashScreen.HideSplash;
  StackTrace := E.StackTrace;
  TMessageDialog.ShowError(E.Message, StackTrace);
  TPublishers.LogPublisher.Write([ltLogWriter], ddError, Sender, 'ApplicationEventsException', StackTrace);
end;

procedure TfrmMonitor.ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if not Application.Terminated then
  begin
    General.NoHibernate;
    if Connected then
      if (General.PriceScheduleCheck < TimeOf(Now)) and (IncMinute(General.PriceScheduleCheck, 20) > TimeOf(Now)) then
      begin
        TfrmEventController.ShowDocument(teCheckLastPrice);
        SokidList.CheckLastPrice;
      end;

    if General.IsActiveBackup and not General.IsBackuped then
      if (General.BackupTime < TimeOf(Now)) and (IncMinute(General.BackupTime, 5) > TimeOf(Now)) then
      begin
        General.IsBackuped := True;
        TTask.Create(
          procedure()
          begin
            TThread.NameThreadForDebugging('TfrmMonitor.BackupDatabase');
            DMod.BackupDatabase(DMod.ConnectionStock);
            DMod.BackupDatabase(DMod.ConnectionFeed);
          end).Start;
      end;
  end;
end;

function TfrmMonitor.ExecuteQualifier(const aRecordId: Integer): Integer;
resourcestring
  rsQualifierNoConditions ='"%s" qualifier has no active conditions';
  rsConditionNoAutotrades ='"%s" condition has no autotrades';
var
  Qualifier: TQualifier;
begin
  Qualifier := TQualifier.Create;
  Qualifier.FromDB(DMod.fbtAccounts.FieldByName('QUALIFIERID').AsInteger);
  Qualifier.InstanceNum := General.GetNextInstanceNum;
  Qualifier.State := tsSuspended;
  Qualifier.OnExecuteAutoTradesInstance := DoAutoTradesInstance;
  FQualifierList.Add(Qualifier);
  Qualifier.Initialize;
  Result := Qualifier.InstanceNum;

  {if (Length(Qualifier.Conditions) = 0) then
  begin
    TMessageDialog.ShowWarning(Format(rsQualifierNoConditions, [Qualifier.Name]));
    Exit(0);
  end
  else
    for QualifierCondition in Qualifier.Conditions do
      if (QualifierCondition.AutoTrades.Count = 0) then
      begin
        TMessageDialog.ShowWarning(Format(rsConditionNoAutotrades, [QualifierCondition.Name]));
        TPublishers.LogPublisher.Write([ltLogWriter], ddWarning, Self, 'ExecuteQualifiers', Format(rsConditionNoAutotrades, [QualifierCondition.Name]));
        Exit(0);
      end; }
  QualifiersControllerPublisher.UpdateState(Qualifier);
//    if (QualifierCondition.TypeCondition = tcComparePrice) then
//      for QualifierCondition in Qualifier.Conditions do
//      begin
//        TIABMarket.RequestMarketData(QualifierCondition.Instrument1.SokidInfo.ContractId);
//        TIABMarket.RequestMarketData(QualifierCondition.Instrument2.SokidInfo.ContractId);
//      end;
end;

procedure TfrmMonitor.CloseQualifier(const aInstanceNum: Integer);
var
  Qualifier: TQualifier;
begin
  if (aInstanceNum > 0) then
  begin
    Qualifier := FQualifierList.GetItemByNum(aInstanceNum);
    if Assigned(Qualifier.AutoTradesInstance) then
    begin
      AutoTradesControllerPublisher.DeleteAutoTrade(Qualifier.AutoTradesInstance.GetAutoTradeInfo.InstanceNum, True);
      TPublishers.MonitorStructureChangePublisher.OnMonitorStructureChange(nil, nil, crNodeMoved);
      QualifiersControllerPublisher.DeleteQualifier(aInstanceNum);
    end;
  end;
end;

procedure TfrmMonitor.aQualifiersExecuteExecute(Sender: TObject);
begin
//  if (DMod.fbtAccounts.FieldByName('QUALIFIERID').AsInteger > 0) then
//    ExecuteQualifier(DMod.fbtAccounts.FieldByName('QUALIFIERID').AsInteger);
end;

procedure TfrmMonitor.aQualifiersExecuteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := IABClient.Connected and
                             DMod.fbtAccounts.Active;

end;

procedure TfrmMonitor.aRegularRunQualifierExecute(Sender: TObject);
begin
  if (btnRegularRunQualifier.ImageIndex = 69) then
  begin
    if (DMod.fbtAccounts.FieldByName('QUALIFIERID').AsInteger > 0) then
    begin
      btnRegularRunQualifier.ImageIndex := 70;
      TimerRunQualifierTimer(nil);
      TimerRunQualifier.Enabled := True;
      TimerRunQualifier.Interval := 1000 { ms } * 60 { sec } * 10 { min };
    end;
  end
  else
  begin
    btnRegularRunQualifier.ImageIndex := 69;
    TimerRunQualifier.Enabled := False;
  end;
end;

procedure TfrmMonitor.TimerRunQualifierTimer(Sender: TObject);
var
  InstanceNum: Integer;
begin
  if (DMod.fbtAccounts.FieldByName('QUALIFIERID').AsInteger > 0) then
  begin
    InstanceNum := ExecuteQualifier(DMod.fbtAccounts.FieldByName('QUALIFIERID').AsInteger);
    if (InstanceNum > 0) then
    begin
      TTask.Create(
        procedure()
        const
          total = 1000 { ms } * 60 { sec } * 9 { min };
        var
          timer: Integer;
        begin
          timer := 0;
          while (timer <= total) and not Application.Terminated do
          begin
            Inc(timer, 500);
            Sleep(500);
          end;
          TThread.Queue(nil,
            procedure
            begin
              TThread.NameThreadForDebugging('TfrmMonitor.RunQualifier.' + InstanceNum.ToString);
              CloseQualifier(InstanceNum);
            end)
        end).Start;
    end;
  end;
end;

procedure TfrmMonitor.aShowQualifiersExecute(Sender: TObject);
var
  Id: Integer;
  MarkedNode: TMarkedNode;
begin
  MarkedNode.DocType := TDocType.ntQualifier;
  MarkedNode.RecordId := 0;//DMod.fbtAccounts.FieldByName('QUALIFIERID').AsInteger;
  Id := TfrmQualifiers.ShowDocument(MarkedNode);
  if Id > 0 then
  begin
    DMod.RefreshQuery(DMod.fbqQualifiers);
    DMod.fbtAccounts.Edit;
    DMod.fbtAccounts.FieldByName('QUALIFIERID').AsInteger := Id;
    DMod.fbtAccounts.Post;
  end;
end;

procedure TfrmMonitor.aShowAutoTradesExecute(Sender: TObject);
var
  Id: Integer;
  MarkedNode: TMarkedNode;
begin
  MarkedNode.DocType  := ntAutoTrade;
  MarkedNode.RecordId := DMod.fbtAccounts.FieldByName('AUTOTRADESID').AsInteger;
  Id := TfrmAutoTrades.ShowDocument(MarkedNode);
  if (Id > 0) then
  begin
    DMod.RefreshQuery(DMod.fbqAutoTrades);
    DMod.fbtAccounts.Edit;
    DMod.fbtAccounts.FieldByName('AUTOTRADESID').AsInteger := Id;
    DMod.fbtAccounts.Post;
  end;
end;

function TfrmMonitor.DoAutoTradesInstance(const aAutoTradeId, aQualifierId, aQualifierInstance: Integer): IAutoTrade;
var
  AutoTradeInfo: TAutoTradeInfo;
  Qualifier: TQualifier;
  Index : Integer;
begin
  Result := nil;
  if (aAutoTradeId > 0) then
  begin
    AutoTradeInfo := TAutoTradeInfo.Create(True);
    AutoTradeInfo.FromDB(aAutoTradeId);
    AutoTradeInfo.Active            := True;
    AutoTradeInfo.InstanceNum       := General.GetNextInstanceNum;
    AutoTradeInfo.QualifierId       := aQualifierId;
    AutoTradeInfo.QualifierInstance := aQualifierInstance;
    Result := TfrmScannerMain.Execute(AutoTradeInfo);
    if (aQualifierInstance > 0) then
    begin
      Index := FQualifierList.GetIndexByNum(aQualifierInstance);
      if (Index > -1) then
      begin
        Qualifier := FQualifierList[Index];
        Qualifier.AutoTradesInstance := Result;
        FQualifierList[Index] := Qualifier;
      end;
    end;
  end;
end;

procedure TfrmMonitor.aAutoTradesExecuteExecute(Sender: TObject);
begin
  if (DMod.fbtAccounts.FieldByName('AUTOTRADESID').AsInteger = -1) then
    TfrmScannerMain.ShowDocument
  else if (DMod.fbtAccounts.FieldByName('AUTOTRADESID').AsInteger > 0) then
    DoAutoTradesInstance(DMod.fbtAccounts.FieldByName('AUTOTRADESID').AsInteger, -1, -1);
end;

procedure TfrmMonitor.aAutoTradesExecuteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := DMod.fbtAccounts.Active and
                             ((IABClient.Connected and (DMod.fbtAccounts.FieldByName('AUTOTRADESID').AsInteger > 0)) or
                              (DMod.fbtAccounts.FieldByName('AUTOTRADESID').AsInteger = -1));
end;

procedure TfrmMonitor.aOpenCheckIBInstrumentsExecute(Sender: TObject);
begin
  TfrmCheckIBInstruments.ShowDocument;
end;

procedure TfrmMonitor.aSearchUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Connected;
end;

procedure TfrmMonitor.aConnectExecute(Sender: TObject);
begin
  Connect;
end;

procedure TfrmMonitor.aDisconnectExecute(Sender: TObject);
begin
  Disconnect;
end;

procedure TfrmMonitor.aDuplicateAlgosExecute(Sender: TObject);
begin
  if Assigned(vstMonitor.FocusedNode) then
    GetDuplicateAlgos(vstMonitor.FocusedNode);
end;

procedure TfrmMonitor.aDuplicateAlgosUpdate(Sender: TObject);
begin
  aDuplicateAlgos.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntAlgos];
  aDuplicateAlgos.Enabled := aDuplicateAlgos.Visible and Connected;
end;

procedure TfrmMonitor.aDuplicateConditionExecute(Sender: TObject);
begin
  if Assigned(vstMonitor.FocusedNode) then
    GetDuplicateCondition(vstMonitor.FocusedNode);
end;

procedure TfrmMonitor.aDuplicateConditionUpdate(Sender: TObject);
begin
  aDuplicateCondition.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntCondition];
  aDuplicateCondition.Enabled := aDuplicateCondition.Visible and Connected;
end;

procedure TfrmMonitor.aDuplicateOrderExecute(Sender: TObject);
var
  Data: PTreeData;
  Node: PVirtualNode;
begin
  Node := vstMonitor.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if Assigned(Data.OrderDoc) then
      if (Data.OrderDoc is TOrderIBDoc) then
        GetDuplicateOrderIB(Node)
      else if (Data.OrderDoc is TOrderNNDoc) then
        GetDuplicateOrderNN(Node);
  end;
end;

procedure TfrmMonitor.aDuplicateOrderUpdate(Sender: TObject);
begin
  aDuplicateOrder.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntOrder];
  aDuplicateOrder.Enabled := aDuplicateOrder.Visible and Connected;
end;

procedure TfrmMonitor.aShowExchangeRateInfoExecute(Sender: TObject);
begin
  TInformationDialog.ShowMessage(TDocumentInfo.GetExchangeRate, 'ExchangeRate');
end;

procedure TfrmMonitor.aExpandTreeExecute(Sender: TObject);
begin
  vstMonitor.FullExpand(nil);
end;

procedure TfrmMonitor.aShowPriceInfoExecute(Sender: TObject);
begin
  TInformationDialog.ShowMessage(TDocumentInfo.GetPriceInformation, 'PriceInformation');
end;

procedure TfrmMonitor.aShowRuleInformationDialogExecute(Sender: TObject);
begin
  TInformationDialog.ShowMessage(TDocumentInfo.GetRuleInformation(vstMonitor.FocusedNode), 'RuleInformation');
end;

procedure TfrmMonitor.aShowInstrumentsWithoutFeedInfoExecute(Sender: TObject);
begin
  TInformationDialog.ShowMessage(TDocumentInfo.GetInstrumentsWithoutFeed, 'InstrumentsWithoutFeed');
end;

procedure TfrmMonitor.aShowRuleInformationDialogUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntOrder];
end;

procedure TfrmMonitor.aShowPrecautionarySettingsInfoExecute(Sender: TObject);
begin
  TInformationDialog.ShowMessage(TDocumentInfo.GetPrecautionarySettingsInformation, 'PrecautionarySettings');
end;

procedure TfrmMonitor.aShowPriceHistoryExecute(Sender: TObject);
begin
  TInformationDialog.ShowMessage(TDocumentInfo.GetPriceHistory(vstMonitor.FocusedNode), 'PriceHistory');
end;

procedure TfrmMonitor.aShowPriceHistoryUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntFactor, ntOrder]) and (Connected);
  TAction(Sender).Visible := TAction(Sender).Enabled;
end;

procedure TfrmMonitor.aShowInformationDialogExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Info: string;
begin
  if Assigned(vstMonitor.FocusedNode) then
  begin
    Node := vstMonitor.FocusedNode;
    Info := TDocumentInfo.GetNodeInfo(Node);

    if FConditionQueue.Contains(Node) then
      Info.Replace('<b hidden>', 'The node is waiting for feeds');

    TInformationDialog.ShowMessage(Info, 'NodeInformationDialog');
  end;
end;

procedure TfrmMonitor.aCollapsTreeExecute(Sender: TObject);
begin
  vstMonitor.FullCollapse(nil);
end;

procedure TfrmMonitor.aHideNodeExecute(Sender: TObject);
begin
  if Assigned(vstMonitor.FocusedNode) then
  begin
    SetLength(FArrayHiddenNodes, Length(FArrayHiddenNodes) + 1);
    FArrayHiddenNodes[Length(FArrayHiddenNodes) - 1] := vstMonitor.FocusedNode;
    vstMonitor.FocusedNode.States := vstMonitor.FocusedNode.States - [vsVisible];
    vstMonitor.Invalidate;
  end;
end;

procedure TfrmMonitor.aHideNodeUpdate(Sender: TObject);
begin
  aHideNode.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) <> ntUnknow;
  aHideNode.Enabled := aHideNode.Visible;
end;

procedure TfrmMonitor.aInactivateOrdersExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  if vstMonitor.FocusedNode <> nil then
  begin
    Node := vstMonitor.FocusedNode.FirstChild;
    while Assigned(Node) do
    begin
      if (TTreeDocument.GetDocType(vstMonitor, Node) = ntOrder) then
      begin
        Node.CheckState := csUncheckedNormal;
        SetIcon(Node);
      end;
      Node := Node.NextSibling;
    end;
  end;
end;

procedure TfrmMonitor.aInactivateOrdersUpdate(Sender: TObject);
begin
  aInactivateOrders.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntOrderGroup];
  aInactivateOrders.Enabled := aInactivateOrders.Visible and Connected;
end;

procedure TfrmMonitor.aLoginExecute(Sender: TObject);
begin
  if not Connected then
    Connect
  else
    Disconnect;
end;

procedure TfrmMonitor.aRequestToIBByHistoricalDataExecute(Sender: TObject);
var
  frmRequestToIB: TfrmRequestToIB;
begin
  frmRequestToIB := TfrmRequestToIB.Create(nil);
  try
    frmRequestToIB.Initialize;
    frmRequestToIB.ShowModal;
  finally
    frmRequestToIB.Deinitialize;
    FreeAndNil(frmRequestToIB);
  end;
end;

procedure TfrmMonitor.aRequestToIBByHistoricalDataUpdate(Sender: TObject);
begin
  aRequestToIBByHistoricalData.Enabled := Connected;
end;

procedure TfrmMonitor.aRestartExecute(Sender: TObject);
var
  frmLoginIB: TfrmLoginIB;
  frmLoginNN: TfrmLoginNN;
begin
  if Connected then
    Disconnect;

  Connected := True;
  btnLogin.ImageIndex := 54;
  btnLogin.Hint := 'Disconnect';

  frmLoginNN := TfrmLoginNN.Create(nil);
  try
    frmLoginNN.LoadParamsFromXml;
    if (frmLoginNN.ShowModal = mrOk) then
      frmLoginNN.SaveParamsToXml;
  finally
    FreeAndNil(frmLoginNN);
  end;

  frmLoginIB := TfrmLoginIB.Create(nil);
  try
    frmLoginIB.LoadParamsFromXml;
    if (frmLoginIB.ShowModal = mrOk) then
    begin
      IABClient.Connect(frmLoginIB.IPAddress, frmLoginIB.Port, frmLoginIB.ClientID);
      frmLoginIB.SaveParamsToXml;
    end;
  finally
    FreeAndNil(frmLoginIB);
  end;

  SubscribeNNFeeds;
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'Restart connect');
end;

procedure TfrmMonitor.aMakeRepetitiveOrderExecute(Sender: TObject);
var
  Data: PTreeData;
begin
  if Assigned(vstMonitor.FocusedNode) then
  begin
    Data := vstMonitor.FocusedNode^.GetData;
    if Assigned(Data) and Assigned(Data^.OrderDoc) then
      if not Data^.OrderDoc.IsRepetitive then
        Data^.OrderDoc.IsRepetitive := True;
  end;
end;

procedure TfrmMonitor.aMakeRepetitiveOrderUpdate(Sender: TObject);
begin
  aMakeRepetitiveOrder.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntOrder];
  aMakeRepetitiveOrder.Enabled := aMakeRepetitiveOrder.Visible;
end;

procedure TfrmMonitor.aShowTickByTickExecute(Sender: TObject);
begin
  TfrmTickByTick.ShowDocument;
end;

procedure TfrmMonitor.aShowTradeChartExecute(Sender: TObject);
begin
  if Assigned(vstMonitor.FocusedNode) and (TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntOrder, ntFactor, ntAlgos, ntCondition]) then
    ShowTradeChart(vstMonitor.FocusedNode);
end;

procedure TfrmMonitor.aShowTradeChartUpdate(Sender: TObject);
begin
  aShowTradeChart.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntOrder, ntFactor, ntAlgos, ntCondition];
  aShowTradeChart.Enabled := aShowTradeChart.Visible;
end;

procedure TfrmMonitor.aOpenGroupSetExecute(Sender: TObject);
var
  OrderGroupSet: TOrderGroupSetDoc;
begin
  OrderGroupSet := TOrderGroupSetDoc.Create;
  try
    OrderGroupSet.RecordId := FOrderGroupSet.RecordId;
    if (TfrmOrderGroupSet.ShowDocument(OrderGroupSet, [tuMonitor]) = mrOk) then
      OpenGroupSet(OrderGroupSet.RecordId);
  finally
    FreeAndNil(OrderGroupSet);
  end;
end;

procedure TfrmMonitor.aOpenGroupSetUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Connected;
end;

procedure TfrmMonitor.aOpenIBCommandExecute(Sender: TObject);
begin
  FIABSocket := TFIABSocket.Create(nil);
  try
    FIABSocket.IABSocket1 := IABClient;
    FIABSocket.ShowModal;
  finally
    FreeAndNil(FIABSocket);
  end;
end;

procedure TfrmMonitor.aShowCalColumnsExecute(Sender: TObject);
begin
  if (coVisible in vstMonitor.Header.Columns[1].Options) then
  begin
    vstMonitor.Header.Columns[COL_NODE_ID].Options    := vstMonitor.Header.Columns[COL_NODE_ID].Options - [coVisible];
    vstMonitor.Header.Columns[COL_CALCTYPE].Options   := vstMonitor.Header.Columns[COL_CALCTYPE].Options - [coVisible];
    vstMonitor.Header.Columns[COL_CALCFACTOR].Options := vstMonitor.Header.Columns[COL_CALCFACTOR].Options - [coVisible];
  end
  else
  begin
    vstMonitor.Header.Columns[COL_NODE_ID].Options    := vstMonitor.Header.Columns[COL_NODE_ID].Options + [coVisible];
    vstMonitor.Header.Columns[COL_CALCTYPE].Options   := vstMonitor.Header.Columns[COL_CALCTYPE].Options + [coVisible];
    vstMonitor.Header.Columns[COL_CALCFACTOR].Options := vstMonitor.Header.Columns[COL_CALCFACTOR].Options + [coVisible];
  end;
end;

procedure TfrmMonitor.aShowCalculationStageInfoExecute(Sender: TObject);
var
  Info: string;
begin
  Info := TDocumentInfo.GetCalculationStageInfo(vstMonitor);
  TInformationDialog.ShowMessage(Info, 'CalculationStageInfo');
end;

procedure TfrmMonitor.aShowConditionAlgosChartExecute(Sender: TObject);
begin
  if Assigned(vstMonitor.FocusedNode) and (TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [{ntCondition,} ntAlgos]) then
    ShowConditionAlgosChart(vstMonitor.FocusedNode);
end;

procedure TfrmMonitor.aShowConditionAlgosChartUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntAlgos, ntCondition]) and (Connected);
  TAction(Sender).Visible := TAction(Sender).Enabled;
end;

procedure TfrmMonitor.aShowConditionChartExecute(Sender: TObject);
begin
  if Assigned(vstMonitor.FocusedNode) and (TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) = ntCondition) then
    ShowConditionChart(vstMonitor.FocusedNode);
end;

procedure TfrmMonitor.aShowConditionChartUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) in [ntCondition]) and (Connected);
  TAction(Sender).Visible := TAction(Sender).Enabled;
end;

procedure TfrmMonitor.aShowContractInspectorExecute(Sender: TObject);
resourcestring
  rsFileNotFound = 'File "%s" not found!';
begin
  if TFile.Exists(General.PathToContractInspector) then
    Winapi.ShellAPI.ShellExecute(Handle, nil, PWideChar(General.PathToContractInspector), '', nil, SW_SHOW)
  else
    TMessageDialog.ShowWarning(Format(rsFileNotFound, [General.PathToContractInspector]));
end;

procedure TfrmMonitor.aRenameGroupExecute(Sender: TObject);
var
  sNewName: string;
begin
  sNewName := InputBox('Change group name', 'Enter a new name, please', edCurrentGroupName.Text);
  FOrderGroupSet.Name := sNewName;
  FOrderGroupSet.SaveToDB;
  SetCurrentGroupCaptions;
end;

procedure TfrmMonitor.aSaveGroupSetAsExecute(Sender: TObject);
var
  NewName: string;
begin
  NewName := EdCurrentGroupName.Text + ' Copy';
  if Vcl.Dialogs.InputQuery('Save as new OrderGroupSet', 'Enter a new name, please', NewName) then
  begin
    FOrderGroupSet.RecordId := -1;
    FOrderGroupSet.Name := NewName;
    SetCurrentGroupCaptions;
    SaveToDBTreeElement(NewName);
  end;
  General.XMLFile.Save;
end;

procedure TfrmMonitor.aSaveGroupSetExecute(Sender: TObject);
begin
  SaveToDBTreeElement(edCurrentGroupName.Text);
  General.XMLFile.Save;
end;

procedure TfrmMonitor.aSaveGroupSetUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (not vstMonitor.IsEmpty) and (Connected);
end;

procedure TfrmMonitor.aSearchExecute(Sender: TObject);
begin
  TfrmSearchInstruments.ShowDocument(Self);
end;

procedure TfrmMonitor.aUnhideAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Length(FArrayHiddenNodes) - 1 do
    FArrayHiddenNodes[i].States := FArrayHiddenNodes[i].States + [vsVisible];

  if Length(FArrayHiddenNodes) > 0 then
  begin
    vstMonitor.Invalidate;
    SetLength(FArrayHiddenNodes, 0);
  end;
end;

procedure TfrmMonitor.aUnhideAllUpdate(Sender: TObject);
begin
  aUnhideAll.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) <> ntUnknow;
  aUnhideAll.Enabled := aUnhideAll.Visible;
end;

procedure TfrmMonitor.aUnhideLastNodeExecute(Sender: TObject);
var
  HideCount: Integer;
begin
  HideCount := Length(FArrayHiddenNodes);
  if HideCount > 0 then
  begin
    FArrayHiddenNodes[HideCount - 1].States := FArrayHiddenNodes[HideCount - 1].States + [vsVisible];
    vstMonitor.Invalidate;
    SetLength(FArrayHiddenNodes, HideCount - 1);
  end;
end;

procedure TfrmMonitor.aUnhideLastNodeUpdate(Sender: TObject);
begin
  aUnhideLastNode.Visible := TTreeDocument.GetDocType(vstMonitor, vstMonitor.FocusedNode) <> ntUnknow;
  aUnhideLastNode.Enabled := aUnhideLastNode.Visible;
end;

//For autocheck of menu item
procedure TfrmMonitor.aUseIBfeedsExecute(Sender: TObject);
begin
//
end;

procedure TfrmMonitor.aUseNNExecute(Sender: TObject);
begin
  General.XMLFile.WriteBool(TGeneral.C_SECTION_COMMON_PARAMS, TGeneral.C_KEY_USE_NN, TAction(Sender).Checked);
  if (not TAction(Sender).Checked) then
    ConnectedNN := TAction(Sender).Checked;
end;

procedure TfrmMonitor.aUseIBExecute(Sender: TObject);
begin
  General.XMLFile.WriteBool(TGeneral.C_SECTION_COMMON_PARAMS, TGeneral.C_KEY_USE_IB, TAction(Sender).Checked);
  General.XMLFile.Save;
  if (not TAction(Sender).Checked) then
    ConnectedIB := TAction(Sender).Checked;
end;

procedure TfrmMonitor.aShowSumAlgosExecute(Sender: TObject);
begin
  vstMonitor.Invalidate;
end;

procedure TfrmMonitor.aShowSumConditionExecute(Sender: TObject);
begin
  vstMonitor.Invalidate;
end;

procedure TfrmMonitor.OnUpdatePriceForNN(const PriceItem: TPriceItem);
var
  CurNode: PVirtualNode;
  Data: PTreeData;
  dTime: TDateTime;
  IndexI, IndexM: Integer;
  InstrumentItem: TInstrumentItem;
  ListIdentifier: TStringList;
  ListMarket: TStringList;
  OrderDoc: TOrderNNDoc;
begin
  try
    dTime := UnixToDateTime(StrToInt64Def(PriceItem.TickTimestamp.Substring(0, 10), -1));
  except
    dTime := Utils.RoundToNearest(Now, C_ROUND_SEC);
  end;

  for InstrumentItem in TMonitorLists.InstrumentList.Values do
    for CurNode in InstrumentItem.NodeList do
      if Assigned(CurNode) then
      begin
        Data := CurNode^.GetData;
        if Assigned(Data) and (Data^.DocType in [ntFactor, ntOrder]) then
        begin
          if Assigned(Data^.FactorDoc) {and Data^.Enabled} then
          begin
            if (Pos(UpperCase(PriceItem.Identifier), UpperCase(Data^.FactorDoc.IdentifierList)) > 0) and
               (Pos(IntToStr(PriceItem.MarketId), Data^.FactorDoc.MarketList) > 0) then
            begin
              ListIdentifier := TStringList.Create;
              ListMarket := TStringList.Create;
              try
                ExtractStrings([';'], [], PChar(Data^.FactorDoc.IdentifierList), ListIdentifier);
                ExtractStrings([';'], [], PChar(Data^.FactorDoc.MarketList), ListMarket);

                IndexI := ListIdentifier.IndexOf(PriceItem.Identifier);
                IndexM := ListMarket.IndexOf(IntToStr(PriceItem.MarketId));

                if (IndexI <> -1) and (IndexI = IndexM) then
                begin
                  if (PriceItem.Bid <> 0) then
                  begin
                    TMonitorLists.PriceCache.AddPrice(Data^.FactorDoc.NNID, ttBid, PriceItem.Bid, dTime);
                  end;

                  if (PriceItem.BidSize <> 0) then
                  begin
                    TMonitorLists.PriceCache.AddPrice(Data^.FactorDoc.NNID, ttBidSize, PriceItem.BidSize, dTime);
                  end;

                  if (PriceItem.Ask <> 0) then
                  begin
                    TMonitorLists.PriceCache.AddPrice(Data^.FactorDoc.NNID, TIABTickType.ttAsk, PriceItem.Ask, dTime);
                  end;

                  if (PriceItem.AskSize <> 0) then
                  begin
                    TMonitorLists.PriceCache.AddPrice(Data^.FactorDoc.NNID, ttAskSize, PriceItem.AskSize, dTime);
                  end;

                  if (PriceItem.Close <> 0) then
                  begin
                    TMonitorLists.PriceCache.AddPrice(Data^.FactorDoc.NNID, ttClose, PriceItem.Close, dTime);
                  end;

                  if (PriceItem.High <> 0) then
                  begin
                    TMonitorLists.PriceCache.AddPrice(Data^.FactorDoc.NNID, ttHigh, PriceItem.High, dTime);
                  end;

                  if (PriceItem.Last <> 0) then
                  begin
                    TMonitorLists.InstrumentList.SetValue(Data^.FactorDoc.NNID, dTime, PriceItem.Last, ttLast);
                    TMonitorLists.PriceCache.AddPrice(Data^.FactorDoc.NNID, ttLast, PriceItem.Last, dTime);
                  end;

                  if (PriceItem.LastSize <> 0) then
                  begin
                    TMonitorLists.PriceCache.AddPrice(Data^.FactorDoc.NNID, ttLastSize, PriceItem.LastSize, dTime);
                  end;

                  if (PriceItem.Low <> 0) then
                  begin
                    TMonitorLists.PriceCache.AddPrice(Data^.FactorDoc.NNID, ttLow, PriceItem.Low, dTime);
                  end;

                  if (PriceItem.Open <> 0) then
                  begin
                    TMonitorLists.PriceCache.AddPrice(Data^.FactorDoc.NNID, ttOpen, PriceItem.Open, dTime);
                  end;

                  // Update parent , summerar underliggande varden
                  if (PriceItem.Last <> 0) then
                    case TTreeDocument.GetDocType(vstMonitor, CurNode.Parent) of
                      ntAlgos:
                        UpdateAlgos(CurNode.Parent);
                      ntCondition:
                        UpdateCondition(CurNode.Parent);
                    end;
                  vstMonitor.InvalidateNode(CurNode);
                end;
              finally
                FreeAndNil(ListIdentifier);
                FreeAndNil(ListMarket);
              end;
            end;
          end
          // ****** ORDER *********
          else if Assigned(Data^.OrderDoc) and (Data^.OrderDoc is TOrderNNDoc) then
          begin
            OrderDoc := Data^.OrderDoc as TOrderNNDoc;

            if (Pos(UpperCase(PriceItem.Identifier), UpperCase(OrderDoc.IdentifierList)) > 0) and
              (Pos(IntToStr(PriceItem.MarketId), OrderDoc.MarketList) > 0) then
            begin
              ListIdentifier := TStringList.Create;
              ListMarket := TStringList.Create;
              try
                ExtractStrings([';'], [], PChar(OrderDoc.IdentifierList), ListIdentifier);
                ExtractStrings([';'], [], PChar(OrderDoc.MarketList), ListMarket);

                IndexI := ListIdentifier.IndexOf(PriceItem.Identifier);
                IndexM := ListMarket.IndexOf(IntToStr(PriceItem.MarketId));

                if (IndexI <> -1) and (IndexI = IndexM) then
                begin
                  if (PriceItem.Last <> 0) then
                  begin
                    OrderDoc.LastPrice := PriceItem.Last;
                    TMonitorLists.InstrumentList.SetValue(OrderDoc.Id, dTime, PriceItem.Last, ttLast);
                    TMonitorLists.PriceCache.AddPrice(OrderDoc.Id, ttLast, PriceItem.Last, dTime);
                  end;
                  vstMonitor.InvalidateNode(CurNode);
                end;
              finally
                FreeAndNil(ListIdentifier);
                FreeAndNil(ListMarket);
              end;
            end;
          end;
        end;
      end;

  // ****** CHILD ORDER *********
  for InstrumentItem in TMonitorLists.InstrumentChildList.Values do
    for CurNode in InstrumentItem.NodeList do
      if Assigned(CurNode) then
      begin
        Data := CurNode^.GetData;
        if Assigned(Data) and Assigned(Data^.OrderDoc) and (Data^.OrderDoc is TOrderNNDoc) then
        begin
          OrderDoc := Data^.OrderDoc as TOrderNNDoc;

          if (OrderDoc.FeedFromBroker = 1) and (OrderDoc.ChildId > 0) and
            (Pos(PriceItem.Identifier.ToUpper, OrderDoc.ChildIdentifierList.ToUpper) > 0) and
            (Pos(PriceItem.MarketId.ToString, OrderDoc.ChildMarketList) > 0) then
          begin
            ListIdentifier := TStringList.Create;
            ListMarket := TStringList.Create;
            try
              ExtractStrings([';'], [], PChar(OrderDoc.ChildIdentifierList), ListIdentifier);
              ExtractStrings([';'], [], PChar(OrderDoc.ChildMarketList), ListMarket);

              IndexI := ListIdentifier.IndexOf(PriceItem.Identifier);
              IndexM := ListMarket.IndexOf(IntToStr(PriceItem.MarketId));

              if (IndexI <> -1) and (IndexI = IndexM) and (PriceItem.Last <> 0) then
              begin
                TMonitorLists.PriceCache.AddPrice(OrderDoc.ChildId, ttLast, PriceItem.Last, dTime);
                OrderDoc.SetValueArrayFromChildFeed(ttLast, PriceItem.Last);
              end;
            finally
              FreeAndNil(ListIdentifier);
              FreeAndNil(ListMarket);
            end;
          end;
        end;
      end;
end;

procedure TfrmMonitor.UpdatePriceForTB(const PriceItem: TPriceItem);
var
  i: Integer;
  CurNode: PVirtualNode;
  Data: PTreeData;
  ListIdentifier, ListMarket: TStringList;
  InstrumentItem: TInstrumentItem;
  dTime: TDateTime;
begin
  InstrumentItem := TMonitorLists.InstrumentList.GetItem(StrToIntDef(PriceItem.Identifier, -1));
  if Assigned(InstrumentItem) then
    for i := 0 to InstrumentItem.NodeList.Count - 1 do
    begin
      CurNode := PVirtualNode(InstrumentItem.NodeList.Items[i]);
      if Assigned(CurNode) then
      begin
        Data := CurNode^.GetData;
        if Assigned(Data^.FactorDoc) {and Data^.Enabled} then
        begin
          if (Pos(UpperCase(PriceItem.Identifier), UpperCase(Data^.FactorDoc.Symbol)) > 0) then
          // and(pos(IntToStr(PriceItem.MarketId), Data^.FactorDoc.MarketList) > 0)
          begin
            try
              dTime := UnixToDateTime(StrToInt64Def(PriceItem.TickTimestamp.Substring(0, 10), -1));
            except
              dTime := Utils.RoundToNearest(Now, C_ROUND_SEC);
            end;

            ListIdentifier := TStringList.Create;
            ListMarket := TStringList.Create;
            try
              ExtractStrings([';'], [], PChar(Data^.FactorDoc.IdentifierList), ListIdentifier);
              ExtractStrings([';'], [], PChar(Data^.FactorDoc.MarketList), ListMarket);

              if (PriceItem.Last <> 0) then
              begin
                Data^.FactorDoc.LastPrice1 := PriceItem.Last;
                TMonitorLists.InstrumentList.SetValue(Data^.FactorDoc.NNID, dTime, PriceItem.LAST, ttLast);
              end;

              // Update parent , summerar underliggande varden
              if (PriceItem.Last <> 0) then
                case TTreeDocument.GetDocType(vstMonitor, CurNode.Parent) of
                  ntAlgos:
                    UpdateAlgos(CurNode.Parent);
                  ntCondition:
                    UpdateCondition(CurNode.Parent);
                end;

              vstMonitor.InvalidateNode(CurNode);
            finally
              FreeAndNil(ListIdentifier);
              FreeAndNil(ListMarket);
            end;
          end;
        end;
      end;
    end;
end;

function TfrmMonitor.UseIBFeeds: Boolean;
begin
  Result := miUseIBfeeds.Checked;
end;

procedure TfrmMonitor.OnUpdateTradeForNN(const TradeItem: TTradeItem);
var
  CurNode: PVirtualNode;
  Data: PTreeData;
  IndexI: Integer;
  IndexM: Integer;
  InstrumentItem: TInstrumentItem;
  ListIdentifier: TStringList;
  ListMarket: TStringList;
begin
  for InstrumentItem in TMonitorLists.InstrumentList.Values do
    for CurNode in InstrumentItem.NodeList do
      if Assigned(CurNode) then
      begin
        Data := CurNode^.GetData;
        if Assigned(Data) {and Data^.Enabled} and Assigned(Data^.FactorDoc) then
        begin
          if (Pos(UpperCase(TradeItem.Identifier), UpperCase(Data^.FactorDoc.IdentifierList)) > 0) and
            (Pos(IntToStr(TradeItem.MarketId), Data^.FactorDoc.MarketList) > 0) then
          begin
            ListIdentifier := TStringList.Create;
            ListMarket := TStringList.Create;
            try
              ExtractStrings([';'], [], PChar(Data^.FactorDoc.IdentifierList), ListIdentifier);
              ExtractStrings([';'], [], PChar(Data^.FactorDoc.MarketList), ListMarket);

              IndexI := ListIdentifier.IndexOf(TradeItem.Identifier);
              IndexM := ListMarket.IndexOf(IntToStr(TradeItem.MarketId));

              if (IndexI <> -1) and (IndexI = IndexM) then
              begin
                if TradeItem.Price <> 0 then
                  Data^.FactorDoc.LastPrice1 := TradeItem.Price;

                vstMonitor.InvalidateNode(CurNode);
              end;
            finally
              FreeAndNil(ListIdentifier);
              FreeAndNil(ListMarket);
            end;
          end;
        end;
      end;
end;

procedure TfrmMonitor.aColumnSettingsExecute(Sender: TObject);

  procedure MonitorTreeResize;
  var
    i, w: Integer;
  begin
    w := vstMonitor.Width - vstMonitor.Header.Columns[0].Width;
    if coVisible in vstMonitor.Header.Columns[1].Options then
    begin
      for i := 1 to vstMonitor.Header.Columns.Count - 1 do
        vstMonitor.Header.Columns[i].Width := w div (vstMonitor.Header.Columns.Count - 1);
    end
    else
    begin
      for i := 3 to vstMonitor.Header.Columns.Count - 1 do
        vstMonitor.Header.Columns[i].Width := w div (vstMonitor.Header.Columns.Count - 3);
    end;
  end;

begin
  if (TfrmColumnSelections.ShowDialog = mrOk) then
  begin
    SetHeaderColumns;
    MonitorTreeResize;
  end;
end;

procedure TfrmMonitor.aPrintExecute(Sender: TObject);
begin
  Printer.Orientation := poLandscape;
  vstMonitor.Print(Printer, True);
end;

procedure TfrmMonitor.aShowDatabasePropertiesExecute(Sender: TObject);
begin
  TfrmDatabaseProperties.ShowDocument;
  if DMod.TransactionStock.Active then
  begin
    DMod.TransactionStock.Commit;
    DMod.TransactionStock.StartTransaction;

    DMod.fbqOrderGr.Active := True;
    DMod.fbqAutoTrades.Active := True;
    DMod.fbqAutoTrades.Last;
    DMod.fbqQualifiers.Active := True;
    DMod.fbqQualifiers.Last;
    DMod.fbtAccounts.Active := True;
  end;
end;

procedure TfrmMonitor.aExportToCSVExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToCSV(vstMonitor, 'Monitor');
end;

procedure TfrmMonitor.aExportToExcelExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToExcel(vstMonitor, 'Monitor');
end;

end.
