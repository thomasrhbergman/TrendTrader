unit Candidate.Market;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Generics.Collections, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls, VirtualTrees, System.UITypes,
  IABSocketAPI, System.ImageList, Vcl.ImgList, BrokerHelperAbstr, Vcl.Menus, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Winapi.msxml, Vcl.ComCtrls, System.Math, Vcl.Samples.Spin, IdGlobalProtocols, IABSocketAPI_const,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Document, DaModule, Global.Types, InstrumentList, IABFunctions,
  DebugWriter, CustomForms, XmlFiles, Candidate.Types, Candidate.Filters, Candidate.AllFilters, VirtualTrees.Editors,
  Chart.Trade, Data.DB, HtmlLib, Candidate.MarketOpenSequence, Vcl.Printers,
  Vcl.ExtDlgs, System.Threading, Utils, MessageDialog, InformationDialog, Entity.Sokid, DaImages, Column.Settings,
  Monitor.Types, IABFunctions.MarketData, VirtualTrees.ExportHelper, Winapi.ActiveX, Publishers.Interfaces,
  IABFunctions.RequestsQueue, DaModule.Utils, Common.Types, Vcl.NumberBox, Publishers, Global.Resources,
  IABFunctions.MessageCodes, Candidate.DragDropOptions, System.Bindings.Expression, System.Bindings.ExpressionDefaults,
  IABFunctions.Helpers, MonitorTree.Helper, MonitorTree.Document, VirtualTrees.Types,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TfrmCandidateMarket = class(TCustomForm, IScanner,
                                         IInstrumentSpecDetails,
                                         IError)
    aAddAll: TAction;
    aAddFilter: TAction;
    aCancelScan: TAction;
    aClearAllFilters: TAction;
    aClearAvailableFilters: TAction;
    aClearIsolateSearchText: TAction;
    aColumnSettings: TAction;
    ActionList: TActionList;
    aExportToCSV: TAction;
    aExportToExcel: TAction;
    aExtraFilter: TAction;
    aInformationDialog: TAction;
    aIntersection: TAction;
    aIsolateColumnSettings: TAction;
    aIsolateDelete: TAction;
    aNewScan: TAction;
    aOpen: TAction;
    aPrint: TAction;
    aReduce: TAction;
    aRemoveAll: TAction;
    aSave: TAction;
    aSaveAs: TAction;
    aShowDragDropOptions: TAction;
    aShowTradeChart: TAction;
    aUpdate: TAction;
    btnAddAll: TBitBtn;
    btnAddFilter: TBitBtn;
    btnCancelScan: TBitBtn;
    btnClearAllFilters: TBitBtn;
    btnClearAvailableFilters: TBitBtn;
    btnClearIsolateSearchText: TBitBtn;
    btnColumnSettings: TBitBtn;
    btnExport: TBitBtn;
    btnExportToExcel: TBitBtn;
    btnExtraFilter: TBitBtn;
    btnInformationDialog: TBitBtn;
    btnIntersection: TBitBtn;
    btnIsolateColumnSettings: TBitBtn;
    btnIsolateDelete: TBitBtn;
    btnNewScan: TBitBtn;
    btnOpen: TBitBtn;
    btnPrint: TBitBtn;
    btnReduce: TBitBtn;
    btnRemoveAll: TBitBtn;
    btnSave: TBitBtn;
    btnSaveAs: TBitBtn;
    btnShowDragDropOptions: TBitBtn;
    btnUpdate: TBitBtn;
    cbAutoRefresh: TCheckBox;
    cbUseExtraFilter: TCheckBox;
    edCurrentSequenceName: TEdit;
    edtAvailableFilters: TEdit;
    edtIsolateSearch: TEdit;
    edtStep: TNumberBox;
    edtWeight: TNumberBox;
    lbInstruments: TListBox;
    lblAutoTradesName: TLabel;
    lblAvailableFilters: TLabel;
    lblCurrentSequenceName: TLabel;
    lblExcludedInstruments: TLabel;
    lblInstanceNum: TLabel;
    lblMaxRows: TLabel;
    lblStep: TLabel;
    lblWeight: TLabel;
    miShowTradeChart: TMenuItem;
    pcResult: TPageControl;
    pmInstruments: TPopupMenu;
    pmMain: TPopupMenu;
    pnlAvailableFilters: TPanel;
    pnlFilterBottom: TPanel;
    pnlFilterMain: TPanel;
    pnlFilterTop: TPanel;
    pnlIsolate: TPanel;
    pnlIsolateTop: TPanel;
    pnlLocation: TPanel;
    pnlMain: TPanel;
    pnlMainFilters: TPanel;
    pnlOptions: TPanel;
    pnlParameter: TPanel;
    pnlResult: TPanel;
    pnlResultTop: TPanel;
    pnlRight: TPanel;
    pnlTop: TPanel;
    pnlTopScanList: TPanel;
    sbFilter: TScrollBox;
    sbMain: TStatusBar;
    seMaxRows: TSpinEdit;
    splFilterMain: TSplitter;
    splIsolate: TSplitter;
    splLocation: TSplitter;
    splParameter: TSplitter;
    splResult: TSplitter;
    tsResult: TTabSheet;
    tsScanList: TTabSheet;
    vstInstruments: TVirtualStringTree;
    vstIsolate: TVirtualStringTree;
    vstLocation: TVirtualStringTree;
    vstScanList: TVirtualStringTree;
    vstScanType: TVirtualStringTree;
    procedure aAddAllExecute(Sender: TObject);
    procedure aAddAllUpdate(Sender: TObject);
    procedure aAddFilterExecute(Sender: TObject);
    procedure aCancelScanExecute(Sender: TObject);
    procedure aClearAllFiltersExecute(Sender: TObject);
    procedure aClearAvailableFiltersExecute(Sender: TObject);
    procedure aClearIsolateSearchTextExecute(Sender: TObject);
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aExtraFilterExecute(Sender: TObject);
    procedure aGetCandidateParametersUpdate(Sender: TObject);
    procedure aInformationDialogExecute(Sender: TObject);
    procedure aIntersectionExecute(Sender: TObject);
    procedure aIsolateColumnSettingsExecute(Sender: TObject);
    procedure aIsolateDeleteExecute(Sender: TObject);
    procedure aNewScanExecute(Sender: TObject);
    procedure aNewScanUpdate(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aReduceExecute(Sender: TObject);
    procedure aRemoveAllExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aSaveAsUpdate(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aShowDragDropOptionsExecute(Sender: TObject);
    procedure aShowTradeChartExecute(Sender: TObject);
    procedure aShowTradeChartUpdate(Sender: TObject);
    procedure aUpdateExecute(Sender: TObject);
    procedure cbUseExtraFilterClick(Sender: TObject);
    procedure edtAvailableFiltersChange(Sender: TObject);
    procedure edtIsolateSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbInstrumentsClick(Sender: TObject);
    procedure seMaxRowsChange(Sender: TObject);
    procedure vstInstrumentsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstInstrumentsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure vstInstrumentsDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstInstrumentsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstInstrumentsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstInstrumentsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstInstrumentsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure vstInstrumentsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstInstrumentsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstIsolateCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstIsolateDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstIsolateDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vstIsolateDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstIsolateFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstIsolateGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstLocationChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstLocationFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstLocationGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstScanListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstScanListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstScanListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vstScanListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstScanTypeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstScanTypeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstScanTypeDblClick(Sender: TObject);
    procedure vstScanTypeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstScanTypeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private const
    COL_LOCATION  = 0;
    COL_PARAMETER = 0;
    COL_NAME      = 0;

    C_SECTION_COLUMN_SETTINGS = 'ScannerMarket.';
    C_IDENTITY_NAME = 'ScannerMarket';

    C_DATA_ID_SYMBOL = 9;
    C_SEPARATOR = ';';

    COL_POSITION         = 0;
    COL_INSTRUMENT       = 1;
    COL_SYMBOL           = 2;
    COL_RANKING          = 3;
    COL_LAST_CLOSE       = 4;
    C_FIXED_COLUMN_INDEX = 5;

    COL_SCAN_ID        = 0;
    COL_COUNT          = 1;
    COL_INST           = 2;
    COL_LOCATION_CODE  = 3;
    COL_SCAN_CODE      = 4;
    COL_NUMBER_OF_ROWS = 5;

    COL_EXCLUDE_NAME        = 0;
    COL_EXCLUDE_SYMBOL      = 1;
    COL_EXCLUDE_LOCALSYMBOL = 2;
    COL_EXCLUDE_CONID       = 3;
    COL_EXCLUDE_CURRENCY    = 4;
    COL_EXCLUDE_EXCHANGE    = 5;
    COL_EXCLUDE_EXPIRY      = 6;
    COL_EXCLUDE_DESCRIPTION = 7;

    C_PANEL_STATUS      = 1;
    C_PANEL_COUNT       = 3;
    C_PANEL_SCAN_ID     = 5;
    C_PANEL_UPDATE_SCAN = 7;
  private
    [weak] FCandidateMarket: ICandidateMarket;
    FColumnId: Integer;
    FFilterList: TArrayFilterData;
    FFiltersController: TFiltersController;
    FInstrumentList: TFactorList;
    FLocationInstruments: TStringList;
    FScanList: TDictionary<Integer, PVirtualNode>;
    FCandidateDataList: TDictionary<Integer, PInstrumentData>;
    FCandidateParametersFile: string;
    FScanOptions: TCandidateColumn;
    FUseExtraFilters: Boolean;
    FXmlFile: TXMLFile;
    function CheckData: Boolean;
    function GetIsolateColumnValue(aColumn: Integer; aData: PSokidInfo): string;
    function GetLocationCode(const aNode: PVirtualNode): string;
    function GetRootInstrumentIndex(aRootInstrumentName: string): Integer;
    function GetScanCriteria(const aScanCode: string): TIABScanCriteria;
    function GetUseExtraFilters: Boolean;
    function ParseLocationInfo(aXmlNode: IXMLDOMNode; aParentNode: PVirtualNode): PVirtualNode;
    procedure AddInstrumentToCandidateMain(const aKindAppend: TKindAppend);
    procedure AddInstrumentToStaticList(aDragDropOptions: TCandidateDragDropOptions);
    procedure CancelScan;
    procedure CancelSubscribe;
    procedure ClearInstruments;
    procedure FillIsolate;
    procedure GetInstrumentSpecs(aData: PInstrumentData);
    procedure GetCandidateParameters;
    procedure GUIToScanOptions;
    procedure LoadParamsFromXml;
    procedure NewScan;
    procedure Open(aSequenceRecordId: Integer);
    procedure ParseFilter(aXmlNode: IXMLDOMNode);
    procedure ParseFilterList;
    procedure ParseInstrumentInfo(const aInstrument: TInstrumentInfo);
    procedure ParseLocation(aXmlNode: IXMLDOMNode; aParentNode: PVirtualNode);
    procedure ParseScanType(aXmlNode: IXMLDOMNode; aParseScanType: string);
    procedure ParseScanTypeList(const aInstruments: string);
    procedure ParseXmlParams(const aXmlText: string = '');
    procedure RankingSum;
    procedure ReadExtraFilters;
    procedure ReadFilters;
    procedure RecalcSortIndex;
    procedure ReloadFilterList(aFilters: string);
    procedure Save;
    procedure SaveParamsToXml;
    procedure ScanOptionsToGUI;
    procedure SetInstruments(const aIndex: Integer);
    procedure SetRootInstrument;
    procedure SetUseExtraFilters(const Value: Boolean);
    procedure ShowTradeChart(aNode: PVirtualNode);
    procedure SubscribeMarketIABData(const aConId: Integer; const aNode: PVirtualNode);
    procedure UpdateStatus(const aPanelNum: Byte; const aInfo: string);
    procedure WriteExtraFilters;
    procedure WriteFilters;

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IError
    procedure OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);

    //implementation IScanner
    procedure OnScannerData(Sender: TObject; Scan: TIABScan);
    procedure OnScannerParam(Sender: TObject; Parameters: string);
    procedure OnScannerAdd(Sender: TObject; ScanId: Integer);
    procedure OnScannerCancel(Sender: TObject; ScanId: Integer);

    //implementation IInstrumentSpecDetails
    procedure OnInstrumentSpecDetails(Sender: TObject; Index: Integer);

    property UseExtraFilters: Boolean read GetUseExtraFilters write SetUseExtraFilters;
  protected
    function GetIdentityName: string; override;
  public
    class function ShowDocument(const aColumnId: Integer; const aScanOptions: TCandidateColumn; const aCandidateMarket: ICandidateMarket): TModalResult;
    procedure Initialize;
    procedure Deinitialize;
  end;

implementation

{$R *.dfm}

resourcestring
  rsColumnName = 'Seq %d (%s)';
  rsSave = 'Please save Market Candidate Sequence';
  rsWeight = 'Weight';

class function TfrmCandidateMarket.ShowDocument(const aColumnId: Integer; const aScanOptions: TCandidateColumn; const aCandidateMarket: ICandidateMarket): TModalResult;
begin
  Result := mrOk;
  with TfrmCandidateMarket.Create(nil) do
  begin
    FColumnId := aColumnId;
    FScanOptions := aScanOptions;
    if Assigned(aCandidateMarket) then
    begin
      FCandidateMarket := aCandidateMarket;
      FCandidateMarket.IncMarketInstances;
    end;
    Initialize;
    CancelScan;
    CancelSubscribe;
    GetCandidateParameters;
    if not FScanOptions.ScanCriteria.Instrument.IsEmpty then
    begin
      SetInstruments(GetRootInstrumentIndex(FScanOptions.CandidateMarket.InstrumentName));
      FScanOptions.ScanId := IABClient.ScanExecute(FScanOptions.ScanCriteria);
    end;
    Open(-1);
    ScanOptionsToGUI;
    Show;
  end;
end;

procedure TfrmCandidateMarket.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  inherited;
  FColumnId := -1;
  vstInstruments.NodeDataSize := SizeOf(TInstrumentData);
  vstIsolate.NodeDataSize     := SizeOf(TSokidInfo);
  vstLocation.NodeDataSize    := SizeOf(TLocationData);
  vstScanType.NodeDataSize    := SizeOf(TScanTypeData);

  FFiltersController := TFiltersController.Create;
  FFiltersController.Owner := sbFilter;

  FLocationInstruments := TStringList.Create;
  FLocationInstruments.Duplicates := dupIgnore;
  FLocationInstruments.Sorted := True;

  FInstrumentList := TFactorList.Create([doOwnsValues]);
  FInstrumentList.DoPriceChange := OnPriceChange;
  TPublishers.FeedPublisher.Subscribe(FInstrumentList);
  TPublishers.ScannerPublisher.Subscribe(Self);
  TPublishers.ErrorPublisher.Subscribe(Self);
  TPublishers.InstrumentSpecDetailsPublisher.Subscribe(Self);

  FScanList := TDictionary<Integer, PVirtualNode>.Create;
  FCandidateDataList := TDictionary<Integer, PInstrumentData>.Create;

  FScanOptions.Clear;
  FXmlFile := TXMLFile.Create;
  FCandidateParametersFile := ExtractFilePath(Application.ExeName) + 'ScannerParameters.xml';

  for i := 0 to IABClient.Scanner.Count - 1 do
    OnScannerAdd(Self, IABClient.Scanner.Items[i].ScanId);
end;

procedure TfrmCandidateMarket.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Deinitialize;
  TPublishers.FeedPublisher.Unsubscribe(FInstrumentList);
  TPublishers.ScannerPublisher.Unsubscribe(Self);
  TPublishers.ErrorPublisher.Unsubscribe(Self);
  TPublishers.InstrumentSpecDetailsPublisher.Unsubscribe(Self);
  IABClient.OnScannerParam := nil;
  Action := caFree;
end;

procedure TfrmCandidateMarket.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
resourcestring
  rsQuestion = 'Do you want to change the column in AutoTrade?';
begin
  if Assigned(FCandidateMarket) and (FColumnId > 0) then
    if (TMessageDialog.ShowQuestion(rsQuestion) = mrYes) then
      FCandidateMarket.SetScanOptions(FColumnId, FScanOptions);
end;

procedure TfrmCandidateMarket.FormDestroy(Sender: TObject);
begin
  if Assigned(FCandidateMarket) then
    FCandidateMarket.DecMarketInstances;

  FreeAndNil(FInstrumentList);
  FreeAndNil(FXmlFile);
  FreeAndNil(FCandidateDataList);
  FreeAndNil(FScanList);
  FreeAndNil(FLocationInstruments);
  FFiltersController.RemoveAllFilters;
  FreeAndNil(FFiltersController);
  inherited;
end;

procedure TfrmCandidateMarket.Initialize;
var
  Column: TVirtualTreeColumn;
  InstrumentInfo: TInstrumentInfo;
  LocationData: PLocationData;
  ScanTypeData: PScanTypeData;
  arr: TNodeArray;
  Node: PVirtualNode;
begin
  TMonitorTree.Initialize(vstInstruments);
  TMonitorTree.Initialize(vstIsolate);
  TMonitorTree.Initialize(vstLocation);
  TMonitorTree.Initialize(vstScanList);
  TMonitorTree.Initialize(vstScanType);
  FillIsolate;
  LoadParamsFromXml;
  ParseXmlParams;

  TStoreHelper.LoadFromXml(vstIsolate, C_SECTION_COLUMN_SETTINGS + vstIsolate.Name);
  vstInstruments.BeginUpdate;
  try
    for var TickType := Low(TIABTickType) to ttETFNavLow do
    begin
      Column := vstInstruments.Header.Columns.Add;
      Column.Text             := TickType.ToString;
      Column.Tag              := Integer(TickType);
      Column.Options          := Column.Options - [{coDraggable,} coEditable];
      Column.CaptionAlignment := taCenter;
      Column.Alignment        := taRightJustify;
      Column.Width            := 80;
      Column.Options          := Column.Options - [coVisible]
    end;
    TStoreHelper.LoadFromXml(vstInstruments, C_SECTION_COLUMN_SETTINGS + vstInstruments.Name);
  finally
    vstInstruments.EndUpdate;
  end;
  UseExtraFilters := False;
  IABClient.Scanner.InitializeScanCriteria(@FScanOptions.GlobalScanCriteria);

  //Set default values
  for var i := 0 to lbInstruments.Items.Count - 1 do
    if Assigned(lbInstruments.Items.Objects[i]) then
    begin
      InstrumentInfo := TInstrumentInfo(lbInstruments.Items.Objects[i]);
      if (InstrumentInfo.InstrType.ToUpper = 'STOCK.EU') then
      begin
        SetInstruments(i);
        lbInstruments.Selected[i] := True;

        arr := TTreeDocument.GetNodesList(vstLocation.RootNode);
        for var j := Low(arr) to High(arr) do
        begin
          Node := arr[j];
          if Assigned(Node) and (Node <> vstLocation.RootNode) then
          begin
            LocationData := Node^.GetData;
            if (LocationData^.LocationCode.ToUpper = 'STK.EU.SFB') then
            begin
              vstLocation.FocusedNode    := Node;
              vstLocation.Selected[Node] := True;
              Node.CheckState := csCheckedNormal;
              Break;
            end;
          end;
        end;

        edtAvailableFilters.Text := 'Top ';
        edtAvailableFiltersChange(nil);
        arr := TTreeDocument.GetNodesList(vstScanType.RootNode);
        for var j := Low(arr) to High(arr) do
        begin
          Node := arr[j];
          if Assigned(Node) and (Node <> vstScanType.RootNode) then
          begin
            ScanTypeData := Node^.GetData;
            if (ScanTypeData^.ScanCode.ToUpper = 'TOP_PERC_GAIN') then
            begin
              vstScanType.FocusedNode    := Node;
              vstScanType.Selected[Node] := True;
              Break;
            end;
          end;
        end;

        seMaxRows.Value := 10;
        Break;
      end;
    end;
end;

procedure TfrmCandidateMarket.Deinitialize;
begin
  CancelSubscribe;
  SaveParamsToXml;
  FCandidateDataList.Clear;
  FScanList.Clear;
  CancelScan;
end;

procedure TfrmCandidateMarket.LoadParamsFromXml;
begin

end;

procedure TfrmCandidateMarket.SaveParamsToXml;
begin
  TStoreHelper.SaveToXml(vstInstruments, C_SECTION_COLUMN_SETTINGS + vstInstruments.Name);
  TStoreHelper.SaveToXml(vstIsolate, C_SECTION_COLUMN_SETTINGS + vstIsolate.Name);
end;

procedure TfrmCandidateMarket.edtAvailableFiltersChange(Sender: TObject);
var
  Data: PScanTypeData;
  Node: PVirtualNode;
begin
  inherited;
  vstScanType.BeginUpdate;
  try
    Node := vstScanType.GetFirst;
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      vstScanType.IsVisible[Node] := (edtAvailableFilters.Text = '') or (Pos(UpperCase(edtAvailableFilters.Text), Data^.DisplayName.ToUpper) > 0);
      Node := Node.NextSibling;
    end;
  finally
    vstScanType.SortTree(vstScanType.Header.SortColumn, vstScanType.Header.SortDirection);
    vstScanType.EndUpdate;
  end;
end;

procedure TfrmCandidateMarket.edtIsolateSearchChange(Sender: TObject);
var
  Data: PSokidInfo;
  Node: PVirtualNode;
begin
  inherited;
  vstIsolate.BeginUpdate;
  try
    Node := vstIsolate.GetFirst;
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      vstIsolate.IsVisible[Node] := (edtIsolateSearch.Text = '') or (Pos(UpperCase(edtIsolateSearch.Text), Data^.Name.ToUpper) > 0);
      Node := Node.NextSibling;
    end;
  finally
    vstIsolate.SortTree(vstIsolate.Header.SortColumn, vstIsolate.Header.SortDirection);
    vstIsolate.EndUpdate;
  end;
end;

procedure TfrmCandidateMarket.lbInstrumentsClick(Sender: TObject);
var
  i: Integer;
  InstrumentInfo: TInstrumentInfo;
begin
  if (lbInstruments.Count > 0) then
  begin
    for i := 0 to lbInstruments.Items.Count - 1 do
      if lbInstruments.Selected[i] and Assigned(lbInstruments.Items.Objects[i]) then
      begin
        InstrumentInfo := TInstrumentInfo(lbInstruments.Items.Objects[i]);
        ParseInstrumentInfo(InstrumentInfo);
        ParseScanTypeList(InstrumentInfo.InstrType);
        ReloadFilterList(InstrumentInfo.Filters);
        FCandidateDataList.Clear;
        FScanOptions.CandidateMarket.InstrumentIndex := i;
        FScanOptions.CandidateMarket.InstrumentName  := InstrumentInfo.Name;
        Break;
      end;
  end;
end;

procedure TfrmCandidateMarket.SetRootInstrument;
begin
  for var i := 0 to lbInstruments.Items.Count - 1 do
    if lbInstruments.Selected[i] and Assigned(lbInstruments.Items.Objects[i]) then
    begin
      FScanOptions.CandidateMarket.InstrumentName := TInstrumentInfo(lbInstruments.Items.Objects[i]).Name;
      FScanOptions.CandidateMarket.InstrumentIndex := i;
      Break;
    end;
end;

procedure TfrmCandidateMarket.ParseInstrumentInfo(const aInstrument: TInstrumentInfo);
var
  ChildNode: IXMLDOMNode;
  CurrentNode: IXMLDOMNode;
  i: Integer;
  LocationTreeNode: IXMLDOMNode;
begin
  CurrentNode := nil;
  LocationTreeNode := nil;
  FLocationInstruments.Clear;
  vstLocation.BeginUpdate;
  try
    vstLocation.Clear;

    FXmlFile.RootNode := 'ScanParameterResponse';
    FXmlFile.CurrentSection := FXmlFile.RootNode;
    while not FXmlFile.IsLastKey do
    begin
      if FXmlFile.ReadAttributes and
       (UpperCase(FXmlFile.Attributes.GetAttributeValue('varName', '')) = 'LOCATIONTREE') then
      begin
        LocationTreeNode := FXmlFile.CurrentNode;
        FXmlFile.CurrentSection := '';
      end;
      if Assigned(CurrentNode) then
        Break;
      FXmlFile.NextKey;
    end;

    if Assigned(LocationTreeNode) and LocationTreeNode.hasChildNodes then
    begin
      for i := 0 to LocationTreeNode.childNodes.length - 1 do
      begin
        ChildNode := LocationTreeNode.childNodes.item[i];
        if (ChildNode.nodeName = 'Location') and ChildNode.hasChildNodes then
        begin
          if ((ChildNode.childNodes.item[0].nodeName = 'displayName') and
            (UpperCase(ChildNode.childNodes.item[0].Text) = aInstrument.Name.ToUpper)) or
            ((ChildNode.childNodes.item[0].nodeName = 'locationCode') and
            (UpperCase(ChildNode.childNodes.item[0].Text) = aInstrument.InstrType.ToUpper)) then
          begin
            ParseLocation(ChildNode, nil);
            Break;
          end;
        end;
      end;
    end;
  finally
    vstLocation.FullExpand;
    vstLocation.EndUpdate;
  end;
end;

procedure TfrmCandidateMarket.ParseLocation(aXmlNode: IXMLDOMNode; aParentNode: PVirtualNode);
var
  ChildNode: IXMLDOMNode;
  i: Integer;
  ParentNode: PVirtualNode;
begin
  if Assigned(aXmlNode) then
  begin
    ParentNode := ParseLocationInfo(aXmlNode, aParentNode);
    for i := 0 to aXmlNode.childNodes.length - 1 do
    begin
      ChildNode := aXmlNode.childNodes.item[i];
      ParseLocation(ChildNode, ParentNode);
    end;
  end;
end;

procedure TfrmCandidateMarket.ParseScanType(aXmlNode: IXMLDOMNode; aParseScanType: string);
var
  i: Integer;
  ChildNode: IXMLDOMNode;
  Data: PScanTypeData;
  Node: PVirtualNode;

  procedure ParseColumns;
  var
    j, k: Integer;
    ColumnSetNode: IXMLDOMNode;
  begin
    SetLength(Data^.Columns, ChildNode.childNodes.length);

    for j := 0 to ChildNode.childNodes.length - 1 do
    begin
      ColumnSetNode := ChildNode.childNodes.item[j];
      Data^.Columns[j].ColumnType := ColumnSetNode.nodeName;
      for k := 0 to ColumnSetNode.childNodes.length - 1 do
      begin
        if (ColumnSetNode.childNodes.item[k].nodeName = 'colId') then
          Data^.Columns[j].ColId := StrToIntDef(ColumnSetNode.childNodes.item[k].nodeTypedValue, 0)
        else if (ColumnSetNode.childNodes.item[k].nodeName = 'name') then
          Data^.Columns[j].Name := Trim(ColumnSetNode.childNodes.item[k].nodeTypedValue)
        else if (ColumnSetNode.childNodes.item[k].nodeName = 'display') then
          Data^.Columns[j].Display := StrToBool(ColumnSetNode.childNodes.item[k].nodeTypedValue)
        else if (ColumnSetNode.childNodes.item[k].nodeName = 'displayType') then
          Data^.Columns[j].DisplayType := Trim(ColumnSetNode.childNodes.item[k].nodeTypedValue);
      end;
    end;
  end;

begin
  Node := nil;
  if Assigned(aXmlNode) and (aXmlNode.nodeName = 'ScanType') and aXmlNode.hasChildNodes then
  begin
    for i := 0 to aXmlNode.childNodes.Length - 1 do
      if (aXmlNode.childNodes.item[i].nodeName = 'instruments') and
        (Pos(aParseScanType, Trim(aXmlNode.childNodes.item[i].nodeTypedValue)) > 0) then
      begin
        Node := vstScanType.AddChild(nil);
        Data := Node^.GetData;
        Break;
      end;

    if Assigned(Node) then
    begin
      for i := 0 to aXmlNode.childNodes.length - 1 do
      begin
        ChildNode := aXmlNode.childNodes.item[i];
        if (ChildNode.nodeName = 'displayName') then
          Data^.DisplayName := Trim(ChildNode.nodeTypedValue)
        else if (ChildNode.nodeName = 'scanCode') then
          Data^.ScanCode := Trim(ChildNode.nodeTypedValue)
        else if (ChildNode.nodeName = 'instruments') then
          Data^.Instruments := Trim(ChildNode.nodeTypedValue)
        else if (ChildNode.nodeName = 'absoluteColumns') then
          Data^.AbsoluteColumns := StrToBool(ChildNode.nodeTypedValue)
        else if (ChildNode.nodeName = 'Columns') then
          ParseColumns;
      end;
      if Data^.DisplayName.IsEmpty then
        Node.States := Node.States - [vsVisible];
    end;
  end;
end;

procedure TfrmCandidateMarket.ParseScanTypeList(const aInstruments: string);
var
  i: Integer;
  ScanTypeListNode: IXMLDOMNode;
  ChildNode: IXMLDOMNode;
begin
  ScanTypeListNode := nil;
  vstScanType.BeginUpdate;
  try
    vstScanType.Clear;

    FXmlFile.RootNode := 'ScanParameterResponse';
    FXmlFile.CurrentSection := FXmlFile.RootNode;
    while not FXmlFile.IsLastKey do
    begin
      if FXmlFile.ReadAttributes and
       (UpperCase(FXmlFile.Attributes.GetAttributeValue('varName', '')) = 'SCANTYPELIST') then
      begin
        ScanTypeListNode := FXmlFile.CurrentNode;
        FXmlFile.CurrentSection := '';
      end;
      if Assigned(ScanTypeListNode) then
        Break;
      FXmlFile.NextKey;
    end;

    if Assigned(ScanTypeListNode) and ScanTypeListNode.hasChildNodes then
    begin
      for i := 0 to ScanTypeListNode.childNodes.length - 1 do
      begin
        ChildNode := ScanTypeListNode.childNodes.item[i];
        if (ChildNode.nodeName = 'ScanType') and ChildNode.hasChildNodes then
          ParseScanType(ChildNode, aInstruments);
      end;
    end;

  finally
    vstScanType.SortTree(vstScanType.Header.SortColumn, vstScanType.Header.SortDirection);
    vstScanType.EndUpdate;
  end;
end;

procedure TfrmCandidateMarket.ParseFilterList;
var
  ChildNode: IXMLDOMNode;
  CurrentNode: IXMLDOMNode;
  FilterListNode: IXMLDOMNode;
  i: Integer;
begin
  CurrentNode := nil;
  FilterListNode := nil;

  FXmlFile.RootNode := 'ScanParameterResponse';
  FXmlFile.CurrentSection := FXmlFile.RootNode;
  while not FXmlFile.IsLastKey do
  begin
    if FXmlFile.ReadAttributes and (UpperCase(FXmlFile.Attributes.GetAttributeValue('varName', '')) = 'FILTERLIST') then
    begin
      FilterListNode := FXmlFile.CurrentNode;
      FXmlFile.CurrentSection := '';
    end;
    if Assigned(CurrentNode) then
      Break;
    FXmlFile.NextKey;
  end;

  if Assigned(FilterListNode) and FilterListNode.hasChildNodes then
  begin
    FFilterList.Clear;
    for i := 0 to FilterListNode.childNodes.Length - 1 do
    begin
      ChildNode := FilterListNode.childNodes.item[i];
      if ((ChildNode.nodeName = 'RangeFilter') or (ChildNode.nodeName = 'SimpleFilter')) and ChildNode.hasChildNodes then
        ParseFilter(ChildNode);
    end;
  end;
end;

procedure TfrmCandidateMarket.ParseFilter(aXmlNode: IXMLDOMNode);
var
  i: Integer;
  ChildNode: IXMLDOMNode;
  Data: TFilterData;

  procedure ParseColumns;
  var
    j, k: Integer;
    ColumnSetNode: IXMLDOMNode;
  begin
    SetLength(Data.Columns, ChildNode.childNodes.length);

    for j := 0 to ChildNode.childNodes.length - 1 do
    begin
      ColumnSetNode := ChildNode.childNodes.item[j];
      Data.Columns[j].ColumnType := ColumnSetNode.nodeName;
      for k := 0 to ColumnSetNode.childNodes.length - 1 do
      begin
        if (ColumnSetNode.childNodes.item[k].nodeName = 'colId') then
          Data.Columns[j].ColId := StrToIntDef(ColumnSetNode.childNodes.item[k].nodeTypedValue, 0)
        else if (ColumnSetNode.childNodes.item[k].nodeName = 'name') then
          Data.Columns[j].Name := Trim(ColumnSetNode.childNodes.item[k].nodeTypedValue)
        else if (ColumnSetNode.childNodes.item[k].nodeName = 'display') then
          Data.Columns[j].Display := StrToBool(ColumnSetNode.childNodes.item[k].nodeTypedValue)
        else if (ColumnSetNode.childNodes.item[k].nodeName = 'displayType') then
          Data.Columns[j].DisplayType := Trim(ColumnSetNode.childNodes.item[k].nodeTypedValue);
      end;
    end;
  end;

  procedure ParseComboValues(aComboValuesNode: IXMLDOMNode; var AbstractField: TAbstractField);
  var
    j, k: Integer;
    ComboValue: IXMLDOMNode;
  begin
    if (aComboValuesNode.nodeName = 'ComboValues') then
    begin
      SetLength(AbstractField.ComboValues, aComboValuesNode.childNodes.Length);
      for j := 0 to aComboValuesNode.childNodes.Length - 1 do
        for k := 0 to aComboValuesNode.childNodes[j].childNodes.Length - 1 do
        begin
          ComboValue := aComboValuesNode.childNodes[j].childNodes[k];
          if (ComboValue.nodeName = 'code') then
            AbstractField.ComboValues[j].Code := Trim(ComboValue.nodeTypedValue)
          else if (ComboValue.nodeName = 'displayName') then
            AbstractField.ComboValues[j].DisplayName := Trim(ComboValue.nodeTypedValue)
          else if (ComboValue.nodeName = 'default') then
            AbstractField.ComboValues[j].Default := StrToBool(ComboValue.nodeTypedValue)
          else if (ComboValue.nodeName = 'dontAllowClearAll') then
            AbstractField.ComboValues[j].SyntheticAll := StrToBool(ComboValue.nodeTypedValue);
        end;
    end;
  end;

  procedure ParseAbstractField;
  var
    Index, j: Integer;
    AbstractFieldNode: IXMLDOMNode;
  begin
    SetLength(Data.AbstractField, Length(Data.AbstractField) + 1);
    Index := Length(Data.AbstractField) - 1;

    if Assigned(ChildNode.attributes.getNamedItem('type')) then
      Data.AbstractField[Index].AbstractFieldType := ChildNode.attributes.getNamedItem('type').nodeTypedValue;
    if Assigned(ChildNode.attributes.getNamedItem('varName')) then
      Data.AbstractField[Index].VarName := ChildNode.attributes.getNamedItem('varName').nodeTypedValue;

    for j := 0 to ChildNode.childNodes.Length - 1 do
    begin
      AbstractFieldNode := ChildNode.childNodes.item[j];
      if (AbstractFieldNode.nodeName = 'code') then
        Data.AbstractField[Index].Code := Trim(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'displayName') then
        Data.AbstractField[Index].DisplayName := Trim(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'varName') then
        Data.AbstractField[Index].VarName := Trim(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'tooltip') then
        Data.AbstractField[Index].Tooltip := Trim(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'dontAllowClearAll') then
        Data.AbstractField[Index].DontAllowClearAll := StrToBool(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'abbrev') then
        Data.AbstractField[Index].Abbrev := StrToBool(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'acceptNegatives') then
        Data.AbstractField[Index].AcceptNegatives := StrToBool(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'dontAllowNegative') then
        Data.AbstractField[Index].DontAllowNegative := StrToBool(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'skipNotEditableField') then
        Data.AbstractField[Index].SkipNotEditableField := StrToBool(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'fraction') then
        Data.AbstractField[Index].Fraction := StrToBool(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'master') then
        Data.AbstractField[Index].Master := StrToBool(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'narrowField') then
        Data.AbstractField[Index].NarrowField := StrToBool(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'radioButtons') then
        Data.AbstractField[Index].RadioButtons := StrToBool(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'typeAhead') then
        Data.AbstractField[Index].TypeAhead := StrToBool(AbstractFieldNode.nodeTypedValue)
      else if (AbstractFieldNode.nodeName = 'maxValue') then
        Data.AbstractField[Index].MaxValue := StrToIntDef(AbstractFieldNode.nodeTypedValue, MaxInt)
      else if (AbstractFieldNode.nodeName = 'minValue') then
        Data.AbstractField[Index].MinValue := StrToIntDef(AbstractFieldNode.nodeTypedValue, -MaxInt)
      else if (AbstractFieldNode.nodeName = 'ComboValues') then
        ParseComboValues(AbstractFieldNode, Data.AbstractField[Index]);
    end;
  end;

begin
  if Assigned(aXmlNode) and ((aXmlNode.nodeName = 'RangeFilter') or (aXmlNode.nodeName = 'SimpleFilter')) and aXmlNode.hasChildNodes then
  begin
    Data.TypeFilter := Trim(aXmlNode.nodeName);
    for i := 0 to aXmlNode.childNodes.Length - 1 do
    begin
      ChildNode := aXmlNode.childNodes.item[i];
      if (ChildNode.nodeName = 'id') then
        Data.Id := Trim(ChildNode.nodeTypedValue)
      else if (ChildNode.nodeName = 'category') then
        Data.Category := Trim(ChildNode.nodeTypedValue)
      else if (ChildNode.nodeName = 'histogram') then
        Data.Histogram := Trim(ChildNode.nodeTypedValue)
      else if (ChildNode.nodeName = 'access') then
        Data.Access := Trim(ChildNode.nodeTypedValue)
      else if (ChildNode.nodeName = 'Columns') then
        ParseColumns
      else if (ChildNode.nodeName = 'AbstractField') then
        ParseAbstractField;
    end;
    FFilterList.Add(Data);
  end;
end;

function TfrmCandidateMarket.ParseLocationInfo(aXmlNode: IXMLDOMNode; aParentNode: PVirtualNode): PVirtualNode;
var
  i: Integer;
  ChildNode: IXMLDOMNode;
  Data: PLocationData;
begin
  Result := aParentNode;
  if Assigned(aXmlNode) and (aXmlNode.nodeName = 'Location') and aXmlNode.hasChildNodes then
  begin
    if Assigned(aParentNode) then
      Result := vstLocation.AddChild(aParentNode)
    else
      Result := vstLocation.AddChild(vstLocation.RootNode);

    Data := Result^.GetData;
    for i := 0 to aXmlNode.childNodes.length - 1 do
    begin
      ChildNode := aXmlNode.childNodes.item[i];
      if (ChildNode.nodeName = 'displayName') then
        Data^.DisplayName := Trim(ChildNode.nodeTypedValue)
      else if (ChildNode.nodeName = 'locationCode') then
        Data^.LocationCode := Trim(ChildNode.nodeTypedValue)
      else if (ChildNode.nodeName = 'instruments') then
        Data^.Instruments := Trim(ChildNode.nodeTypedValue)
      else if (ChildNode.nodeName = 'routeExchange') then
        Data^.RouteExchange := Trim(ChildNode.nodeTypedValue)
      else if (ChildNode.nodeName = 'delayedOnly') then
        Data^.DelayedOnly := Trim(ChildNode.nodeTypedValue)
      else if (ChildNode.nodeName = 'access') then
        Data^.Access := Trim(ChildNode.nodeTypedValue);
    end;
    if Assigned(aParentNode) then
      aParentNode^.CheckType := ctTriStateCheckBox;
    Result^.CheckType  := ctCheckBox;
    Result^.CheckState := csUncheckedNormal;
    for var item in FScanOptions.CandidateMarket.MarketLocation do
      if (item.Tag.ToUpper = Data^.LocationCode) then
      begin
        Result^.CheckState := TCheckState(StrToIntDef(item.Value, Ord(csUncheckedNormal)));
        Break;
      end;
    FLocationInstruments.Add(Data^.Instruments);
  end;
end;

procedure TfrmCandidateMarket.RankingSum;
var
  Data: PInstrumentData;
  Node: PVirtualNode;
  ColumnItems: TExtraColumns.TColumnsItem;
  i: Integer;
begin
  i := 0;
  Node := vstInstruments.GetLast;
  while Assigned(Node) do
  begin
    Data := Node^.GetData;
    if Data^.ExtraColumns.Items.ContainsKey(-1) then
    begin
      ColumnItems := Data^.ExtraColumns.Items[-1];
      Data^.ExtraColumns.RankingSum := FScanOptions.Weight * (1 + (ColumnItems.Rank - 1) * FScanOptions.Step);
    end;
    Data^.ExtraColumns.Position := Integer(vstInstruments.RootNodeCount) - i;
    vstInstruments.InvalidateNode(Node);
    Node := Node.PrevSibling;
    Inc(i);
  end;
end;

procedure TfrmCandidateMarket.RecalcSortIndex;
var
  Data: PInstrumentData;
  Node: PVirtualNode;
  i: Integer;
begin
  i := 0;
  Node := vstInstruments.GetFirst;
  while Assigned(Node) do
  begin
    Inc(i);
    Data := Node^.GetData;
    Data^.ExtraColumns.Position := i;
    Node := Node.NextSibling;
  end;
end;

procedure TfrmCandidateMarket.OnScannerData(Sender: TObject; Scan: TIABScan);
//data from the server arrives here
var
  i: Integer;
  Data: PInstrumentData;
  Node: PVirtualNode;
  DeletedNode: PVirtualNode;
  Exists: Boolean;
  TickType: TIABTickType;
  ColumnItems: TExtraColumns.TColumnsItem;
begin
  OnScannerAdd(Self, Scan.ScanId);
  if (Scan.ScanId = FScanOptions.ScanId) then
  begin
    UpdateStatus(C_PANEL_COUNT, Scan.Count.ToString);
    UpdateStatus(C_PANEL_UPDATE_SCAN, FormatDateTime('hh:nn:ss', Now));
    vstInstruments.BeginUpdate;
    try
      Node := vstInstruments.GetLast;
      while Assigned(Node) do
      begin
        Exists := False;
        DeletedNode := nil;
        Data := Node^.GetData;
        for i := 0 to Scan.Count - 1 do
          if (Data^.Id = Scan.Items[i].ContractId) then
          begin
            Exists := True;
            Break;
          end;

        if not Exists then
          DeletedNode := Node;

        Node := Node.PrevSibling;
        if not Exists and Assigned(DeletedNode) then
        begin
          TIABMarket.CancelMarketData(Data^.Id);
          FCandidateDataList.Remove(Data^.Id);
          FInstrumentList.DeleteNode(Data^.Id, DeletedNode);
          vstInstruments.DeleteNode(DeletedNode);
        end;
      end;

      for i := 0 to Scan.Count - 1 do
      begin
        if (FCandidateDataList.ContainsKey(Scan.Items[i].ContractId)) then
          Data := FCandidateDataList.Items[Scan.Items[i].ContractId]
        else
        begin
          Node := vstInstruments.AddChild(nil);
          Data := Node^.GetData;
          Data^.Node := Node;
          Data^.Exchange := Scan.Items[i].Exchange;
          Data^.ExtraColumns := TExtraColumns.Create(0);
          Data^.ExtraColumns.Position := i + 1;
          if SokidList.ContainsKey(Scan.Items[i].ContractId) then
            Data^.Isolate := SokidList.Items[Scan.Items[i].ContractId].Isolate;
          FCandidateDataList.Add(Scan.Items[i].ContractId, Data);
        end;

        if Assigned(Data) then
        begin
          Data^.Assign(Scan.Items[i]);
          if Data^.ExtraColumns.Items.ContainsKey(-1) then
          begin
            ColumnItems := Data^.ExtraColumns.Items[-1];
            ColumnItems.Rank := Scan.Count - Scan.Items[i].Rank;
            Data^.ExtraColumns.Items.AddOrSetValue(-1, ColumnItems);
          end;

          for TickType := Low(TIABTickType) to ttNotSet do
          begin
            Data^.ExtraColumns.Items.TryGetValue(Integer(TickType), ColumnItems);
            ColumnItems.Price := TMonitorLists.PriceCache.GetLastPrice(Data^.Id, TickType);
            Data^.ExtraColumns.Items.AddOrSetValue(Integer(TickType), ColumnItems);
          end;

          if Data^.SecurityType in [stIndex, stOption] then
            Data^.Name := Data^.LocalSymbol
          else
          begin
            if SokidList.ContainsKey(Scan.Items[i].ContractId) then
              Data^.Name := SokidList.Items[Scan.Items[i].ContractId].Name
            else
              GetInstrumentSpecs(Data);
          end;
          if not FInstrumentList.ContainsKey(Scan.Items[i].ContractId) then
          begin
            SubscribeMarketIABData(Scan.Items[i].ContractId, Node);
            FInstrumentList.AddNode(Scan.Items[i].ContractId, Node);
          end;
        end;
      end;
      RankingSum;
    finally
      vstInstruments.SortTree(vstInstruments.Header.Columns[COL_POSITION].Index, sdAscending);
      vstInstruments.EndUpdate;
      vstInstruments.Invalidate;
    end;
    UpdateStatus(C_PANEL_STATUS, '');
    if not cbAutoRefresh.Checked then
      CancelScan;
  end;
end;

procedure TfrmCandidateMarket.OnScannerParam(Sender: TObject; Parameters: string);
begin
  inherited;
  Screen.Cursor := crHourGlass;
  try
    ParseXmlParams(Parameters);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmCandidateMarket.UpdateStatus(const aPanelNum: Byte; const aInfo: string);
begin
  sbMain.Panels[aPanelNum].Text := aInfo;
  sbMain.Refresh;
end;

procedure TfrmCandidateMarket.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
var
  InstrumentItem: TInstrumentItem;
  Data: PInstrumentData;
  Node: PVirtualNode;
  ColumnItems: TExtraColumns.TColumnsItem;
begin
  InstrumentItem := FInstrumentList.GetItem(Id);
  if Assigned(InstrumentItem) then
    for Node in InstrumentItem.NodeList do
      if Assigned(Node) then
      begin
        Data := Node^.GetData;
        if Assigned(Data) and (TickType < ttNotSet) then
        begin
          ColumnItems.Price := Value;
          Data.ExtraColumns.Items.AddOrSetValue(Integer(TickType), ColumnItems);
          vstInstruments.InvalidateNode(Node);
        end;
      end;
end;

procedure TfrmCandidateMarket.OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
resourcestring
  rsMessage = 'Error. Code: %d, TemplId: %d, Msg "%s"';
begin
  if (GetTWSMessageItem(ErrorCode).TypeCode = mcClientError) then
    UpdateStatus(C_PANEL_STATUS, Format(rsMessage, [ErrorCode, TempId, ErrorMsg]));
end;

procedure TfrmCandidateMarket.OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
var
  SokidInfo: TSokidInfo;
  Data: PInstrumentData;
begin
  if (IABClient.InstrumentSpecs.Items[Index].DataID = C_DATA_ID_SYMBOL) then
  begin
    SokidInfo.ContractId  := IABClient.InstrumentSpecs.Items[Index].ContractId;
    SokidInfo.Name        := IABClient.InstrumentSpecs.Items[Index].LongName;
    if FCandidateDataList.ContainsKey(SokidInfo.ContractId) then
    begin
      Data := FCandidateDataList.Items[SokidInfo.ContractId];
      Data^.Name := SokidInfo.Name;
      if Assigned(Data^.Node) then
        vstInstruments.InvalidateNode(Data^.Node);
    end;
  end;
end;

function TfrmCandidateMarket.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

function TfrmCandidateMarket.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmCandidateMarket.GetInstrumentSpecs(aData: PInstrumentData);
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
    IABClient.SendRequest(ibGetInstrumentSpecs, C_DATA_ID_SYMBOL, Order);
  finally
    FreeAndNil(Order);
  end;
end;

procedure TfrmCandidateMarket.ParseXmlParams(const aXmlText: string = '');
var
  ChildNode: IXMLDOMNode;
  CurrentNode: IXMLDOMNode;
  i: Integer;
  InstrumentInfo: TInstrumentInfo;
begin
  if not aXmlText.IsEmpty then
    FXmlFile.XMLText := aXmlText
  else if FileExists(FCandidateParametersFile) then
    FXmlFile.LoadFromFile(FCandidateParametersFile);

  ClearInstruments;
  ParseFilterList;
  CurrentNode := nil;
  FXmlFile.RootNode       := 'ScanParameterResponse';
  FXmlFile.CurrentSection := FXmlFile.RootNode;
  while not FXmlFile.IsLastKey do
  begin
    if FXmlFile.ReadAttributes and
     (UpperCase(FXmlFile.Attributes.GetAttributeValue('varName', '')) = 'FULLINSTRUMENTLIST') then
    begin
      CurrentNode := FXmlFile.CurrentNode;
      FXmlFile.CurrentSection := '';
    end;
    FXmlFile.NextKey;
  end;

  if Assigned(CurrentNode) then
  begin
    FXmlFile.SectionNode := CurrentNode;
    FXmlFile.CurrentNode := FXmlFile.SectionNode.firstChild;
    while not FXmlFile.IsLastKey do
    begin
      if FXmlFile.CurrentNode.hasChildNodes then
      begin
        InstrumentInfo := TInstrumentInfo.Create(FXmlFile.CurrentNode);
        for i := 0 to FXmlFile.CurrentNode.childNodes.length - 1 do
        begin
          ChildNode := FXmlFile.CurrentNode.childNodes.item[i];
          if (ChildNode.nodeName = 'name') then
            InstrumentInfo.Name := Trim(ChildNode.nodeTypedValue)
          else if (ChildNode.nodeName = 'type') then
            InstrumentInfo.InstrType := Trim(ChildNode.nodeTypedValue)
          else if (ChildNode.nodeName = 'secType') then
            InstrumentInfo.SecType := Trim(ChildNode.nodeTypedValue)
          else if (ChildNode.nodeName = 'filters') then
            InstrumentInfo.Filters := Trim(ChildNode.nodeTypedValue)
          else if (ChildNode.nodeName = 'group') then
            InstrumentInfo.Group := Trim(ChildNode.nodeTypedValue)
          else if (ChildNode.nodeName = 'shortName') then
            InstrumentInfo.ShortName := Trim(ChildNode.nodeTypedValue)
          else if (ChildNode.nodeName = 'cloudScanNotSupported') then
            InstrumentInfo.CloudScanNotSupported := ChildNode.nodeTypedValue;
        end;
        lbInstruments.Items.AddObject(InstrumentInfo.Name, InstrumentInfo);
      end;
      FXmlFile.NextKey;
    end;
  end;

  FXmlFile.CurrentSection := '';
  FXmlFile.SaveToFile(FCandidateParametersFile);
end;

procedure TfrmCandidateMarket.aAddAllExecute(Sender: TObject);
begin
  AddInstrumentToCandidateMain(kaAddAll);
end;

procedure TfrmCandidateMarket.aInformationDialogExecute(Sender: TObject);
begin
  TInformationDialog.ShowMessage(FScanOptions.ToList.Replace(sLineBreak, '<br>'), 'CandidateMarket');
end;

procedure TfrmCandidateMarket.aIntersectionExecute(Sender: TObject);
begin
  AddInstrumentToCandidateMain(kaIntersection);
end;

procedure TfrmCandidateMarket.aReduceExecute(Sender: TObject);
begin
  AddInstrumentToCandidateMain(kaReduce);
end;

procedure TfrmCandidateMarket.aUpdateExecute(Sender: TObject);
begin
  AddInstrumentToCandidateMain(kaUpdate);
end;

procedure TfrmCandidateMarket.CancelScan;
begin
  if IABClient.Connected and (FScanOptions.ScanId > 0) then
    IABClient.CancelScan(FScanOptions.ScanId);
end;

procedure TfrmCandidateMarket.CancelSubscribe;
begin
  for var DataId in FInstrumentList.Keys do
    TIABMarket.CancelMarketData(DataId);
end;

procedure TfrmCandidateMarket.cbUseExtraFilterClick(Sender: TObject);
begin
  FUseExtraFilters := cbUseExtraFilter.Checked;
end;

function TfrmCandidateMarket.GetScanCriteria(const aScanCode: string): TIABScanCriteria;
var
  Data          : PLocationData;
  FilterControl : TFilterControl;
  ScanCriteria  : TIABScanCriteria;
  LocationNode: PVirtualNode;
begin
  IABClient.Scanner.InitializeScanCriteria(@ScanCriteria);
  LocationNode := vstLocation.RootNode.FirstChild;
  if Assigned(LocationNode) then
  begin
    Data := LocationNode^.GetData;
    ScanCriteria.Instrument   := Data^.Instruments;
    ScanCriteria.LocationCode := GetLocationCode(LocationNode);
    ScanCriteria.ScanCode     := aScanCode;
    ScanCriteria.NumberOfRows := FScanOptions.MaxRows;

    if UseExtraFilters then
    begin
      ScanCriteria.AbovePrice               := FScanOptions.GlobalScanCriteria.AbovePrice;
      ScanCriteria.BelowPrice               := FScanOptions.GlobalScanCriteria.BelowPrice;
      ScanCriteria.AboveVolume              := FScanOptions.GlobalScanCriteria.AboveVolume;
      ScanCriteria.MarketCapAbove           := FScanOptions.GlobalScanCriteria.MarketCapAbove;
      ScanCriteria.MarketCapBelow           := FScanOptions.GlobalScanCriteria.MarketCapBelow;
      ScanCriteria.MoodyRatingAbove         := FScanOptions.GlobalScanCriteria.MoodyRatingAbove;
      ScanCriteria.MoodyRatingBelow         := FScanOptions.GlobalScanCriteria.MoodyRatingBelow;
      ScanCriteria.SPRatingAbove            := FScanOptions.GlobalScanCriteria.SPRatingAbove;
      ScanCriteria.SPRatingBelow            := FScanOptions.GlobalScanCriteria.SPRatingBelow;
      ScanCriteria.MaturityDateAbove        := FScanOptions.GlobalScanCriteria.MaturityDateAbove;
      ScanCriteria.MaturityDateBelow        := FScanOptions.GlobalScanCriteria.MaturityDateBelow;
      ScanCriteria.CouponRateAbove          := FScanOptions.GlobalScanCriteria.CouponRateAbove;
      ScanCriteria.CouponRateBelow          := FScanOptions.GlobalScanCriteria.CouponRateBelow;
      ScanCriteria.ExcludeConvertible       := FScanOptions.GlobalScanCriteria.ExcludeConvertible;
      ScanCriteria.AverageOptionVolumeAbove := FScanOptions.GlobalScanCriteria.AverageOptionVolumeAbove;
      ScanCriteria.CandidateSettingPairs      := FScanOptions.GlobalScanCriteria.CandidateSettingPairs;
      ScanCriteria.StockTypeFilter          := FScanOptions.GlobalScanCriteria.StockTypeFilter;
    end;

    if FFiltersController.ContainsKey('PRICE') then
    begin
      FilterControl := FFiltersController.Items['PRICE'];
      if Assigned(FilterControl) then
      begin
        if FilterControl.VariableList.ContainsKey('priceAbove') then
          ScanCriteria.AbovePrice := StrToFloatDef(FilterControl.VariableList.Items['priceAbove'], UNSET_DOUBLE);

        if (FilterControl.RangeState = TRangeState.rsRange) and
          FilterControl.VariableList.ContainsKey('priceBelow') then
          ScanCriteria.BelowPrice := StrToFloatDef(FilterControl.VariableList.Items['priceBelow'], UNSET_DOUBLE);
      end;
    end;

    if FFiltersController.ContainsKey('VOLUME') then
    begin
      FilterControl := FFiltersController.Items['VOLUME'];
      if Assigned(FilterControl) and FilterControl.VariableList.ContainsKey('volumeAbove') then
        ScanCriteria.AboveVolume := StrToIntDef(FilterControl.VariableList.Items['volumeAbove'], UNSET_INTEGER);
    end;

    if FFiltersController.ContainsKey('MOODY') then
    begin
      FilterControl := FFiltersController.Items['MOODY'];
      if Assigned(FilterControl) then
      begin
        if FilterControl.VariableList.ContainsKey('moodyRatingAbove') then
          ScanCriteria.MoodyRatingAbove := FilterControl.VariableList.Items['moodyRatingAbove'];

        if (FilterControl.RangeState = TRangeState.rsRange) and
          FilterControl.VariableList.ContainsKey('moodyRatingBelow') then
          ScanCriteria.MoodyRatingBelow := FilterControl.VariableList.Items['moodyRatingBelow'];
      end;
    end;

    if FFiltersController.ContainsKey('SP') then
    begin
      FilterControl := FFiltersController.Items['SP'];
      if Assigned(FilterControl) then
      begin
        if FilterControl.VariableList.ContainsKey('spRatingAbove') then
          ScanCriteria.SPRatingAbove := FilterControl.VariableList.Items['spRatingAbove'];

        if (FilterControl.RangeState = TRangeState.rsRange) and
          FilterControl.VariableList.ContainsKey('spRatingBelow') then
          ScanCriteria.SPRatingBelow := FilterControl.VariableList.Items['spRatingBelow'];
      end;
    end;

    if FFiltersController.ContainsKey('MATDATE') then
    begin
      FilterControl := FFiltersController.Items['MATDATE'];
      if Assigned(FilterControl) then
      begin
        if FilterControl.VariableList.ContainsKey('maturityDateAbove') then
          ScanCriteria.MaturityDateAbove := FilterControl.VariableList.Items['maturityDateAbove'];

        if (FilterControl.RangeState = TRangeState.rsRange) and
          FilterControl.VariableList.ContainsKey('maturityDateBelow') then
          ScanCriteria.MaturityDateBelow := FilterControl.VariableList.Items['maturityDateBelow'];
      end;
    end;

    if FFiltersController.ContainsKey('CPNRATE') then
    begin
      FilterControl := FFiltersController.Items['CPNRATE'];
      if Assigned(FilterControl) then
      begin
        if FilterControl.VariableList.ContainsKey('couponRateAbove') then
          ScanCriteria.CouponRateAbove := StrToIntDef(FilterControl.VariableList.Items['couponRateAbove'], UNSET_INTEGER);

        if (FilterControl.RangeState = TRangeState.rsRange) and
          FilterControl.VariableList.ContainsKey('couponRateBelow') then
          ScanCriteria.CouponRateBelow := StrToIntDef(FilterControl.VariableList.Items['couponRateBelow'], UNSET_INTEGER);
      end;
    end;

    if FFiltersController.ContainsKey('CONVOPT') then
    begin
      FilterControl := FFiltersController.Items['CONVOPT'];
      if Assigned(FilterControl) and FilterControl.VariableList.ContainsKey('excludeConvertible') then
        ScanCriteria.ExcludeConvertible := StrToIntDef(FilterControl.VariableList.Items['excludeConvertible'], UNSET_INTEGER);
    end;

    for var PairFilter in FFiltersController do
      if (Pos(PairFilter.Key, 'CONVOPT,CPNRATE,MATDATE,SP,MOODY,VOLUME,PRICE') = 0) then
      begin
        FilterControl := FFiltersController.Items[PairFilter.Key];
        if Assigned(FilterControl) then
          for var PairVariable in FilterControl.VariableList do
          begin
            SetLength(ScanCriteria.FilterOptions, Length(ScanCriteria.FilterOptions) + 1);
            ScanCriteria.FilterOptions[Length(ScanCriteria.FilterOptions) - 1].Tag   := PairVariable.Key;
            ScanCriteria.FilterOptions[Length(ScanCriteria.FilterOptions) - 1].Value := PairVariable.Value;
          end;
      end;
  end;
  Result := ScanCriteria;
end;

procedure TfrmCandidateMarket.NewScan;
resourcestring
  C_INSTRUMENT = 'Instrument';
var
  FilterControl : TFilterControl;
  ScanTypeData  : PScanTypeData;
begin
  if not Assigned(vstScanType.FocusedNode) then
  begin
    SetFocusSafely(vstScanType);
    TMessageDialog.ShowWarning('Please, select an parameter.');
  end
  else if Assigned(vstLocation.RootNode) and Assigned(vstLocation.RootNode.FirstChild) then
  begin
    if Assigned(vstScanType.FocusedNode) then
    begin
      ScanTypeData      := vstScanType.FocusedNode^.GetData;
      FScanOptions.Name := ScanTypeData^.DisplayName;

      if FFiltersController.ContainsKey('HASOPTIONS') then
      begin
        FilterControl := FFiltersController.Items['HASOPTIONS'];
        if Assigned(FilterControl) and FilterControl.VariableList.ContainsKey('hasOptionsIs') then
        begin
          if StrToBoolDef(FilterControl.VariableList.Items['hasOptionsIs'], False) then
            vstInstruments.Header.Columns[COL_INSTRUMENT].Text := ScanTypeData^.DisplayName + ' (Has option)'
          else
            vstInstruments.Header.Columns[COL_INSTRUMENT].Text := ScanTypeData^.DisplayName + ' (Any)';
        end;
      end
      else
        vstInstruments.Header.Columns[COL_INSTRUMENT].Text := ScanTypeData^.DisplayName;
      CancelSubscribe;
      WriteFilters;
      FScanOptions.ScanCriteria := GetScanCriteria(ScanTypeData^.ScanCode);
      FScanOptions.ScanId       := IABClient.ScanExecute(FScanOptions.ScanCriteria);
      UpdateStatus(C_PANEL_SCAN_ID, FScanOptions.ScanId.ToString);
      UpdateStatus(C_PANEL_COUNT, '0');
      UpdateStatus(C_PANEL_STATUS, 'New Scan');
    end;
  end;
end;

function TfrmCandidateMarket.GetLocationCode(const aNode: PVirtualNode): string;
var
  CurrNode: PVirtualNode;
  Data: PLocationData;
begin
  Result := '';
  if Assigned(aNode) then
  begin
    if (aNode.ChildCount > 0) then
    begin
      CurrNode := aNode.firstChild;
      while Assigned(CurrNode) do
      begin
        Result := Result + GetLocationCode(CurrNode);
        CurrNode := CurrNode.NextSibling;
      end;
    end
    else
    begin
      Data := aNode^.GetData;
      if (aNode.CheckState in [csCheckedNormal, csMixedPressed]) and (not Data^.LocationCode.IsEmpty) then
        Result := Data^.LocationCode + ',';
    end;
  end;
end;

procedure TfrmCandidateMarket.GUIToScanOptions;
begin
  FScanOptions.MaxRows := seMaxRows.Value;
  FScanOptions.Name    := edCurrentSequenceName.Text;
  FScanOptions.Step    := edtStep.ValueFloat;
  FScanOptions.Weight  := edtWeight.ValueFloat;
  FScanOptions.CandidateMarket.AutoRefresh      := cbAutoRefresh.Checked;
  FScanOptions.CandidateMarket.AvailableFilters := edtAvailableFilters.Text;
  FScanOptions.CandidateMarket.UseExtraFilters  := UseExtraFilters;
end;

procedure TfrmCandidateMarket.ScanOptionsToGUI;
begin
  cbAutoRefresh.Checked      := FScanOptions.CandidateMarket.AutoRefresh;
  edCurrentSequenceName.Text := FScanOptions.Name;
  edtAvailableFilters.Text   := FScanOptions.CandidateMarket.AvailableFilters;
  edtStep.ValueFloat         := FScanOptions.Step;
  edtWeight.ValueFloat       := FScanOptions.Weight;
  seMaxRows.Value            := FScanOptions.MaxRows;
  UseExtraFilters            := FScanOptions.CandidateMarket.UseExtraFilters;
  if Assigned(FCandidateMarket) then
  begin
    lblAutoTradesName.Caption := 'Parent AutoTrades Name: ' + FCandidateMarket.GetAutoTradeInfo.Name;
    lblInstanceNum.Caption := Concat('Parent Instance Num: ', Abs(FCandidateMarket.GetAutoTradeInfo.InstanceNum).ToString);
    if (FCandidateMarket.GetAutoTradeInfo.InstanceNum > 0) then
      lblInstanceNum.Caption := Concat(lblInstanceNum.Caption, ' (Autotrade)')
    else
      lblInstanceNum.Caption := Concat(lblInstanceNum.Caption, ' (Manual)');
  end;
  edtAvailableFiltersChange(nil);
end;

procedure TfrmCandidateMarket.seMaxRowsChange(Sender: TObject);
begin
  if Showing then
  begin
    FScanOptions.MaxRows := seMaxRows.Value;
    FScanOptions.ScanCriteria.NumberOfRows := seMaxRows.Value;
  end;
end;

procedure TfrmCandidateMarket.aNewScanExecute(Sender: TObject);
begin
  if IABClient.Connected and CheckData then
  begin
    CancelScan;
    FInstrumentList.Clear;
    FCandidateDataList.Clear;
    vstInstruments.Clear;
    GUIToScanOptions;
    NewScan;
  end;
end;

procedure TfrmCandidateMarket.aNewScanUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := IABClient.Connected;
end;

procedure TfrmCandidateMarket.aAddAllUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstInstruments.IsEmpty;
end;

procedure TfrmCandidateMarket.ReloadFilterList(aFilters: string);
var
  i: Integer;
  InstrumentFiltersList: TStringList;
begin
  FFiltersController.RemoveAllFilters;
  FFiltersController.FilterList.Clear;

  InstrumentFiltersList := TStringList.Create;
  try
    InstrumentFiltersList.Delimiter       := ',';
    InstrumentFiltersList.StrictDelimiter := True;
    InstrumentFiltersList.Duplicates      := dupIgnore;
    InstrumentFiltersList.DelimitedText   := aFilters;

    for i := Low(FFilterList.Items) to High(FFilterList.Items) do
      if (InstrumentFiltersList.IndexOf(FFilterList[i].Id) > -1) then
        FFiltersController.FilterList.AddIfNotContains(FFilterList[i]);

    for i := Low(FFiltersController.AnchorValues) to High(FFiltersController.AnchorValues) do
      FFiltersController.AddFilter(FFiltersController.AnchorValues[i]);
  finally
    FreeAndNil(InstrumentFiltersList);
  end;
end;

procedure TfrmCandidateMarket.aAddFilterExecute(Sender: TObject);
begin
  FFiltersController.AddOrReplaceFilters;
end;

procedure TfrmCandidateMarket.aClearAllFiltersExecute(Sender: TObject);
begin
  FFiltersController.ClearAllFilters;
end;

procedure TfrmCandidateMarket.aClearAvailableFiltersExecute(Sender: TObject);
begin
  edtAvailableFilters.Text := '';
  edtAvailableFiltersChange(nil);
end;

procedure TfrmCandidateMarket.aClearIsolateSearchTextExecute(Sender: TObject);
begin
  edtIsolateSearch.Text := '';
end;

procedure TfrmCandidateMarket.aColumnSettingsExecute(Sender: TObject);
begin
  TStoreHelper.ShowColumnSettings(vstInstruments, GetIdentityName + '.vstInstruments', C_FIXED_COLUMN_INDEX);
end;

procedure TfrmCandidateMarket.aRemoveAllExecute(Sender: TObject);
begin
  FFiltersController.RemoveAllFilters;
end;

procedure TfrmCandidateMarket.ShowTradeChart(aNode: PVirtualNode);
var
  Data: PInstrumentData;
  i: Integer;
  PriceList: TPriceList;
  Price: TPrice;
begin
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if not Assigned(Data.TradeChart) then
    begin
      Data^.TradeChart := TfrmTradeChartForm.Create(nil);
      TfrmTradeChartForm(Data^.TradeChart).Initialize(Data^.Id);
      if (Data^.Id > 0) then
      begin
        PriceList := TMonitorLists.PriceCache.GetPriceList(Data^.Id);
        if Assigned(PriceList) then
          for i := 0 to PriceList.Count - 1 do
          begin
            Price := PriceList.Items[i];
            if Price.IsHistorical and (Price.TickType = ttLast) then
              TfrmTradeChartForm(Data.TradeChart).AddValue(Price.Value, Price.TimeStamp);
          end;
      end;
    end;
    Data.TradeChart.Show;
  end;
end;

procedure TfrmCandidateMarket.aOpenExecute(Sender: TObject);
var
  RecordId: Integer;
begin
  RecordId := TfrmCandidateMarketOpenSequence.GetScanRecordID;
  if (RecordId > 0) then
    Open(RecordId);
end;

procedure TfrmCandidateMarket.SetInstruments(const aIndex: Integer);
var
  Node: PVirtualNode;
  ScanCode: string;
  ScanTypeData: PScanTypeData;
begin
  if (aIndex > -1) and (lbInstruments.Count > aIndex) then
  begin
    vstScanType.BeginUpdate;
    try
      lbInstruments.Selected[aIndex] := True;
      lbInstrumentsClick(nil);
      edtAvailableFiltersChange(nil);
      ScanCode := FScanOptions.ScanCriteria.ScanCode.ToUpper;
      if not ScanCode.IsEmpty then
      begin
        Node := vstScanType.GetFirst;
        while Assigned(Node) do
        begin
          ScanTypeData := Node^.GetData;
          if not(vsFiltered in Node^.States) and (ScanTypeData^.ScanCode.ToUpper = ScanCode) then
          begin
            vstScanType.FocusedNode    := Node;
            vstScanType.Selected[Node] := True;
            Break;
          end;
          Node := Node.NextSibling;
        end;
      end;
    finally
      vstScanType.EndUpdate;
    end;
  end;
end;

procedure TfrmCandidateMarket.ReadFilters;
var
  Pair: TPair<string, string>;
  ArrVar: TArray<string>;
  Id: string;
begin
  FFiltersController.RemoveAllFilters;
  for var i := Low(FScanOptions.CandidateMarket.FiltersArray) to High(FScanOptions.CandidateMarket.FiltersArray) do
  begin
    Id := FScanOptions.CandidateMarket.FiltersArray[i].Id;
    if not Id.IsEmpty then
    begin
      ArrVar := FScanOptions.CandidateMarket.FiltersArray[i].Variables.Split([C_SEPARATOR]);
      if FFiltersController.AddFilter(Id) then
      begin
        if InRange(FScanOptions.CandidateMarket.FiltersArray[i].RangeState, Ord(Low(TRangeState)), Ord(High(TRangeState))) then
          FFiltersController.Items[Id].RangeState := TRangeState(FScanOptions.CandidateMarket.FiltersArray[i].RangeState)
        else
          FFiltersController.Items[Id].RangeState := rsUnknown;
        for var Variables in ArrVar do
          if not Variables.IsEmpty then
          begin
            Pair.Key   := Variables.Split(['='])[0];
            Pair.Value := Variables.Split(['='])[1];
            FFiltersController.Items[Id].VariableList.AddOrSetValue(Pair.Key, Pair.Value);
            FFiltersController.Items[Id].SetVariableValue(Pair.Key, Pair.Value);
          end;
      end;
    end;
  end;
end;

procedure TfrmCandidateMarket.WriteFilters;
var
  Pair: TPair<string, string>;
  FilterControl: TFilterControl;
  Variables: string;
  i: Integer;
begin
  FScanOptions.CandidateMarket.FiltersArray := [];
  i := 0;
  SetLength(FScanOptions.CandidateMarket.FiltersArray, FFiltersController.Count);
  for FilterControl in FFiltersController.Values do
    if Assigned(FilterControl) then
    begin
      FScanOptions.CandidateMarket.FiltersArray[i].Id         := FilterControl.Id;
      FScanOptions.CandidateMarket.FiltersArray[i].RangeState := Ord(FilterControl.RangeState);
      Variables := '';
      for Pair in FilterControl.VariableList do
        Variables := Variables + Pair.Key + '=' + Pair.Value + C_SEPARATOR;
      FScanOptions.CandidateMarket.FiltersArray[i].Variables := Variables;
      Inc(i);
    end;
end;

procedure TfrmCandidateMarket.ReadExtraFilters;
  function GetValue(AValue: Double): Double;
  begin
    if (AValue = 0) then
      Result := MaxDouble
    else
      Result := AValue;
  end;
begin
  FScanOptions.GlobalScanCriteria.AbovePrice               := GetValue(FScanOptions.ScanCriteria.AbovePrice);
  FScanOptions.GlobalScanCriteria.BelowPrice               := GetValue(FScanOptions.ScanCriteria.BelowPrice);
  FScanOptions.GlobalScanCriteria.CouponRateAbove          := GetValue(FScanOptions.ScanCriteria.CouponRateAbove);
  FScanOptions.GlobalScanCriteria.CouponRateBelow          := GetValue(FScanOptions.ScanCriteria.CouponRateBelow);
  FScanOptions.GlobalScanCriteria.MarketCapAbove           := GetValue(FScanOptions.ScanCriteria.MarketCapAbove);
  FScanOptions.GlobalScanCriteria.MarketCapBelow           := GetValue(FScanOptions.ScanCriteria.MarketCapBelow);
  FScanOptions.GlobalScanCriteria.AboveVolume              := FScanOptions.ScanCriteria.AboveVolume;
  FScanOptions.GlobalScanCriteria.AverageOptionVolumeAbove := FScanOptions.ScanCriteria.AverageOptionVolumeAbove;
  FScanOptions.GlobalScanCriteria.ExcludeConvertible       := FScanOptions.ScanCriteria.ExcludeConvertible;
  FScanOptions.GlobalScanCriteria.MaturityDateAbove        := FScanOptions.ScanCriteria.MaturityDateAbove;
  FScanOptions.GlobalScanCriteria.MaturityDateBelow        := FScanOptions.ScanCriteria.MaturityDateBelow;
  FScanOptions.GlobalScanCriteria.MoodyRatingAbove         := FScanOptions.ScanCriteria.MoodyRatingAbove;
  FScanOptions.GlobalScanCriteria.MoodyRatingBelow         := FScanOptions.ScanCriteria.MoodyRatingBelow;
  FScanOptions.GlobalScanCriteria.CandidateSettingPairs      := FScanOptions.ScanCriteria.CandidateSettingPairs;
  FScanOptions.GlobalScanCriteria.SPRatingAbove            := FScanOptions.ScanCriteria.SPRatingAbove;
  FScanOptions.GlobalScanCriteria.SPRatingBelow            := FScanOptions.ScanCriteria.SPRatingBelow;
  FScanOptions.GlobalScanCriteria.StockTypeFilter          := FScanOptions.ScanCriteria.StockTypeFilter;
end;

procedure TfrmCandidateMarket.WriteExtraFilters;

  function GetValueDouble(AValue: Double): Double;
  begin
    if (AValue = UNSET_DOUBLE) then
      Result := 0
    else
      Result := AValue;
  end;

  function GetValueInteger(AValue: Integer): Integer;
  begin
    if (AValue = UNSET_INTEGER) then
      Result := 0
    else
      Result := AValue;
  end;

begin
//  if UseExtraFilters then
  begin
    FScanOptions.ScanCriteria.AbovePrice               := GetValueDouble(FScanOptions.GlobalScanCriteria.AbovePrice);
    FScanOptions.ScanCriteria.BelowPrice               := GetValueDouble(FScanOptions.GlobalScanCriteria.BelowPrice);
    FScanOptions.ScanCriteria.CouponRateAbove          := GetValueDouble(FScanOptions.GlobalScanCriteria.CouponRateAbove);
    FScanOptions.ScanCriteria.CouponRateBelow          := GetValueDouble(FScanOptions.GlobalScanCriteria.CouponRateBelow);
    FScanOptions.ScanCriteria.MarketCapAbove           := GetValueDouble(FScanOptions.GlobalScanCriteria.MarketCapAbove);
    FScanOptions.ScanCriteria.MarketCapBelow           := GetValueDouble(FScanOptions.GlobalScanCriteria.MarketCapBelow);
    FScanOptions.ScanCriteria.AboveVolume              := GetValueInteger(FScanOptions.GlobalScanCriteria.AboveVolume);
    FScanOptions.ScanCriteria.AverageOptionVolumeAbove := GetValueInteger(FScanOptions.GlobalScanCriteria.AverageOptionVolumeAbove);
    FScanOptions.ScanCriteria.ExcludeConvertible       := GetValueInteger(FScanOptions.GlobalScanCriteria.ExcludeConvertible);
    FScanOptions.ScanCriteria.MaturityDateAbove        := FScanOptions.GlobalScanCriteria.MaturityDateAbove;
    FScanOptions.ScanCriteria.MaturityDateBelow        := FScanOptions.GlobalScanCriteria.MaturityDateBelow;
    FScanOptions.ScanCriteria.MoodyRatingAbove         := FScanOptions.GlobalScanCriteria.MoodyRatingAbove;
    FScanOptions.ScanCriteria.MoodyRatingBelow         := FScanOptions.GlobalScanCriteria.MoodyRatingBelow;
    FScanOptions.ScanCriteria.CandidateSettingPairs      := FScanOptions.GlobalScanCriteria.CandidateSettingPairs;
    FScanOptions.ScanCriteria.SPRatingAbove            := FScanOptions.GlobalScanCriteria.SPRatingAbove;
    FScanOptions.ScanCriteria.SPRatingBelow            := FScanOptions.GlobalScanCriteria.SPRatingBelow;
    FScanOptions.ScanCriteria.StockTypeFilter          := FScanOptions.GlobalScanCriteria.StockTypeFilter;
  end;
end;

procedure TfrmCandidateMarket.Open(aSequenceRecordId: Integer);

  procedure LoadDataFromDB;
  resourcestring
    C_SQL_SELECT_TEXT = 'SELECT * FROM SCAN_MARKET WHERE ID=:ID';
  var
    Query: TFDQuery;
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := C_SQL_SELECT_TEXT;
      Query.ParamByName('ID').AsInteger := aSequenceRecordId;
      try
        Query.Prepare;
        Query.Open;
        if not Query.IsEmpty then
        begin
          FScanOptions.Clear;
          FScanOptions.FromList(Query.FieldByName('OPTIONS').AsString);
          FScanOptions.Name                   := Query.FieldByName('NAME').AsString;
          FScanOptions.CandidateMarket.RecordId := aSequenceRecordId;
          edCurrentSequenceName.Text          := FScanOptions.Name;
        end;
      except
        on E: Exception do
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'LoadDataFromDB', E.Message + TDModUtils.GetQueryInfo(Query));
      end;
    finally
      FreeAndNil(Query);
    end;
  end;

var
  Index: Integer;
begin
  if (aSequenceRecordId > 0) then
    LoadDataFromDB;
  ScanOptionsToGUI;
  SetInstruments(FScanOptions.CandidateMarket.InstrumentIndex);
  ReadFilters;
  ReadExtraFilters;
  Index := lbInstruments.ItemIndex;
  try
    GetCandidateParameters;
  finally
    lbInstruments.ItemIndex := Index;
  end;
end;

function TfrmCandidateMarket.GetRootInstrumentIndex(aRootInstrumentName: string): Integer;
begin
  Result := -1;
  for var i := 0 to lbInstruments.Items.Count - 1 do
    if Assigned(lbInstruments.Items.Objects[i]) and
      (TInstrumentInfo(lbInstruments.Items.Objects[i]).Name.ToUpper = aRootInstrumentName.ToUpper) then
      Exit(i);
end;

procedure TfrmCandidateMarket.aSaveAsExecute(Sender: TObject);
var
  NewName: string;
begin
  NewName := FScanOptions.Name + ' Copy';
  if Vcl.Dialogs.InputQuery('Save as new Sequence', 'Enter a new name, please', NewName) then
  begin
    edCurrentSequenceName.Text := NewName;
    FScanOptions.Name := NewName;
    FScanOptions.CandidateMarket.RecordId := -1;
    Save;
  end;
end;

procedure TfrmCandidateMarket.aSaveAsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FScanOptions.CandidateMarket.RecordId > 0;
end;

procedure TfrmCandidateMarket.aSaveExecute(Sender: TObject);
begin
  if CheckData then
    Save;
end;

procedure TfrmCandidateMarket.Save;
resourcestring
  C_SQL_INSERT_TEXT = 'INSERT INTO SCAN_MARKET(ID, NAME, OPTIONS) ' + sLineBreak +
                      'VALUES(:ID, :NAME, :OPTIONS)';
  C_SQL_UPDATE_TEXT = 'UPDATE SCAN_MARKET '               + sLineBreak +
                      'SET NAME=:NAME, OPTIONS=:OPTIONS ' + sLineBreak +
                      'WHERE (ID = :ID); ';
  C_SQL_CHECK_TEXT = 'SELECT COUNT(*) AS CNT FROM SCAN_MARKET ' + sLineBreak +
                      'WHERE (NAME = ''%s'') and ID<>%d;';
var
  Query: TFDQuery;
  Data: PScanTypeData;
begin
  GUIToScanOptions;
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.Transaction := Query.Connection.Transaction;
    if (FScanOptions.CandidateMarket.RecordId <= 0) then
    begin
      FScanOptions.CandidateMarket.RecordId := DMod.GetNextValue('GEN_SCAN_MARKET_ID');
      Query.SQL.Text := C_SQL_INSERT_TEXT;
    end
    else
      Query.SQL.Text := C_SQL_UPDATE_TEXT;

    if FScanOptions.Name.IsEmpty then
      if Assigned(vstScanType.FocusedNode) then
      begin
        Data := vstScanType.FocusedNode^.GetData;
        FScanOptions.Name := Data^.DisplayName;
      end;

    WriteFilters;
    if FScanOptions.Name.IsEmpty then
      FScanOptions.Name := 'Candidate Market sequence nr' + FScanOptions.CandidateMarket.RecordId.ToString;
    FScanOptions.ScanCriteria := GetScanCriteria(FScanOptions.ScanCriteria.ScanCode);
    Query.ParamByName('ID').AsInteger     := FScanOptions.CandidateMarket.RecordId;
    Query.ParamByName('NAME').AsString    := FScanOptions.Name.Substring(0, 100);
    Query.ParamByName('OPTIONS').AsString := FScanOptions.ToList;
    try
      Query.Prepare;
      Query.ExecSQL;
      Query.Transaction.CommitRetaining;
    except
      on E: Exception do
        TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'Save', E.Message + TDModUtils.GetQueryInfo(Query));
    end;
    edCurrentSequenceName.Text := FScanOptions.Name;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TfrmCandidateMarket.aShowDragDropOptionsExecute(Sender: TObject);
begin
  TfrmCandidateDragDropOptions.ShowDocument;
end;

procedure TfrmCandidateMarket.aShowTradeChartExecute(Sender: TObject);
begin
  if Assigned(vstInstruments.FocusedNode) then
    ShowTradeChart(vstInstruments.FocusedNode);
end;

procedure TfrmCandidateMarket.aShowTradeChartUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := IABClient.Connected and not vstInstruments.IsEmpty;
end;

procedure TfrmCandidateMarket.AddInstrumentToStaticList(aDragDropOptions: TCandidateDragDropOptions);
var
  ColumnItems: TExtraColumns.TColumnsItem;
  ColumnsInfo: TColumnsInfo;
  Data: PInstrumentData;
  Index: Integer;
  Instruments: TArrayInstrumentData;
  Node: PVirtualNode;

  {function GetStaticListName(aTemplate: string): string;
  var
    Expression: TBindingExpression;
  begin
    Expression := TBindingExpressionDefault.Create;
    Expression.Source := aTemplate;
    Expression.Compile([TBindingAssociation.Create(Instruments[0], 'inst')]);
    Result := Expression.Evaluate.GetValue.ToString;
    Expression.Free;
  end; }

  function AddStaticLists(aName: string): Integer;
  begin
    Result := DMod.GetNextValue('GEN_STATIC_LISTS_ID');
    DMod.ExecuteSQL('INSERT INTO STATICLISTS (ID, NAME, WEIGHT, VISIBLE) VALUES (' + Result.ToString + ', ' + aName.QuotedString + ',1,1)');
  end;

  procedure AddInstrumentsToStaticList;
  resourcestring
    C_SQL_EXISTS_TEXT = 'SELECT ID FROM STATICLISTS_DETAIL WHERE CONID=:CONID AND STATICLISTS_ID =:STATICLISTS_ID';
    C_SQL_INSERT_TEXT = 'INSERT INTO STATICLISTS_DETAIL(ID, STATICLISTS_ID, CONID, BROKER_TYPE, RANKING) ' + sLineBreak +
                        'VALUES(:ID, :STATICLISTS_ID, :CONID, :BROKER_TYPE, :RANKING)';
  var
    Query : TFDQuery;
    IsExists: Boolean;
    RecordId: Integer;
  begin
    Query := TFDQuery.Create(nil);
    try
      Query.Connection  := DMod.ConnectionStock;
      Query.Transaction := DMod.ConnectionStock.Transaction;
      DMod.CheckConnect;

      for var Instrument in Instruments do
      begin
        Query.SQL.Text := C_SQL_EXISTS_TEXT;
        try
          Query.ParamByName('CONID').AsInteger          := Instrument.Id;
          Query.ParamByName('STATICLISTS_ID').AsInteger := aDragDropOptions.StaticListId;
          Query.Prepare;
          Query.Open;
          RecordId := Query.FieldByName('ID').AsInteger;
          IsExists := RecordId > 0;
          Query.Close;
        except
          on E: Exception do
          begin
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveDetailList', E.Message + TDModUtils.GetQueryInfo(Query));
            raise;
          end;
        end;

        if not IsExists then
        begin
          RecordId := DMod.GetNextValue('GEN_STATICLISTS_DETAIL_ID');
          Query.SQL.Text := C_SQL_INSERT_TEXT;
          Query.ParamByName('ID').AsInteger             := RecordId;
          Query.ParamByName('STATICLISTS_ID').AsInteger := aDragDropOptions.StaticListId;
          Query.ParamByName('CONID').AsInteger          := Instrument.Id;
          Query.ParamByName('BROKER_TYPE').AsInteger    := Ord(Instrument.BrokerType);
          Query.ParamByName('RANKING').AsFloat          := 0;
          try
            Query.Prepare;
            Query.ExecSQL;
          except
            on E: Exception do
              TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'SaveDetailList', E.Message + TDModUtils.GetQueryInfo(Query));
          end;
        end;
      end;
    finally
      Query.Transaction.CommitRetaining;
      FreeAndNil(Query);
    end;
  end;

begin
  if (FScanOptions.CandidateMarket.RecordId <= 0) then
    TMessageDialog.ShowInfo(rsSave)
  else if (edtWeight.ValueFloat <= 0) then
    TMessageDialog.ShowWarning(Format(rcRequiredValue, ['Weight']))
  else if (edtStep.ValueFloat <= 0) then
    TMessageDialog.ShowWarning(Format(rcRequiredValue, ['Step']))
  else if Assigned(FCandidateMarket) and (DialogMode <> dmView) and (vstInstruments.SelectedCount > 0) then
  begin
    Index := 0;
    SetLength(Instruments, vstInstruments.SelectedCount);
    Node := vstInstruments.GetFirst;
    while Assigned(Node) do
    begin
      if vstInstruments.Selected[Node] then
      begin
        Data := Node^.GetData;
        if (Data.Isolate <> 1)  then
        begin
          with Instruments[Index] do
          begin
            RecordId     := -1;
            Id           := Data^.Id;
            BrokerType   := TBrokerType.brIB;
            Currency     := Data^.Currency;
            Exchange     := Data^.Exchange;
            Name         := Data^.Name;
            Group        := Data^.Group;
            SecurityType := Data^.SecurityType;
            IsIn         := Data^.IsIn;
            Symbol       := Data^.Symbol;
            LocalSymbol  := Data^.LocalSymbol;
            Sector       := Data^.Sector;
            ExtraColumns := TExtraColumns.Create(0);

            ColumnItems := Data^.ExtraColumns.Items[-1];
            ColumnItems.ColTick := clBlack;
            ColumnItems.Price   := 0;
            ColumnItems.Weight  := 0;
            ColumnItems.Rank    := 0;
            ExtraColumns.Items.AddOrSetValue(-1, ColumnItems);
          end;
          Inc(Index);
        end;
      end;
      Node := Node.NextSibling;
    end;
    ColumnsInfo := TColumnsInfo.Create(stStaticList);
    if (aDragDropOptions.StaticListId <= 0) then
    begin
      if aDragDropOptions.StaticListName.IsEmpty then
        aDragDropOptions.StaticListName := Instruments[0].Name;
      aDragDropOptions.StaticListId := AddStaticLists(aDragDropOptions.StaticListName);
    end;
    AddInstrumentsToStaticList;
    ColumnsInfo.StaticColumn.FromDB(aDragDropOptions.StaticListId);
    ColumnsInfo.Weight := FScanOptions.Weight;
    SetRootInstrument;
    ColumnsInfo.KindAppend := kaAddAll;
    FCandidateMarket.AddInstrument(@Instruments, ColumnsInfo);
  end;
end;

procedure TfrmCandidateMarket.AddInstrumentToCandidateMain(const aKindAppend: TKindAppend);
var
  ColumnItems: TExtraColumns.TColumnsItem;
  ColumnsInfo: TColumnsInfo;
  Data: PInstrumentData;
  Index: Integer;
  Instruments: TArrayInstrumentData;
  Node: PVirtualNode;
begin
  if (FScanOptions.CandidateMarket.RecordId <= 0) then
    TMessageDialog.ShowInfo(rsSave)
//  else if (edtWeight.ValueFloat <= 0) then
//    TMessageDialog.ShowWarning(Format(rcRequiredValue, ['Weight']))
//  else if (edtStep.ValueFloat <= 0) then
//    TMessageDialog.ShowWarning(Format(rcRequiredValue, ['Step']))
  else if Assigned(FCandidateMarket) and (DialogMode <> dmView) and (not vstInstruments.IsEmpty) then
  begin
    GUIToScanOptions;
    Index := 0;
    SetLength(Instruments, vstInstruments.RootNodeCount);
    Node := vstInstruments.GetFirst;
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      with Instruments[Index] do
      begin
        RecordId     := -1;
        Id           := Data^.Id;
        BrokerType   := TBrokerType.brIB;
        Currency     := Data^.Currency;
        Exchange     := Data^.Exchange;
        Name         := Data^.Name;
        Group        := Data^.Group;
        SecurityType := Data^.SecurityType;
        IsIn         := Data^.IsIn;
        Symbol       := Data^.Symbol;
        LocalSymbol  := Data^.LocalSymbol;
        Sector       := Data^.Sector;
        ExtraColumns := TExtraColumns.Create(0);

        ColumnItems := Data^.ExtraColumns.Items[-1];
        ColumnItems.ColTick    := clBlack;
        ColumnItems.Price      := 0;
        ColumnItems.Weight     := 0;
        ColumnItems.OriginRank := ColumnItems.Rank;
        ColumnItems.Rank       := FScanOptions.Weight * (1 + (ColumnItems.Rank - 1) * FScanOptions.Step);
        ExtraColumns.Items.AddOrSetValue(-1, ColumnItems);
      end;
      Inc(Index);
      Node := Node.NextSibling;
    end;
    ColumnsInfo := TColumnsInfo.Create(stCandidateMarket);
    ColumnsInfo.Weight := FScanOptions.Weight;
    SetRootInstrument;
    ColumnsInfo.CandidateColumn.FromList(FScanOptions.ToList);
    ColumnsInfo.CandidateColumn.Name := Format(rsColumnName, [FScanOptions.CandidateMarket.RecordId, FScanOptions.Name]);
    ColumnsInfo.KindAppend := aKindAppend;
    FCandidateMarket.AddInstrument(@Instruments, ColumnsInfo);
  end;
end;

procedure TfrmCandidateMarket.SubscribeMarketIABData(const aConId: Integer; const aNode: PVirtualNode);
var
  Order: TIABOrder;
  Data: PInstrumentData;
  MarketData: string;
begin
  MarketData := '';
  if IABClient.Connected and Assigned(aNode) then
  begin
    Order := TIABOrder.Create;
    try
      IABClient.ClearOrder(Order);
      Data := aNode^.GetData;
      Order.ContractId   := aConId;
      Order.SecurityType := Data^.SecurityType;
      Order.Currency     := Data^.Currency;
      Order.Exchange     := Data^.Exchange;
      Order.Symbol       := Data^.Symbol;
      TIABMarket.RequestMarketData(Order);
    finally
      FreeAndNil(Order);
    end;
  end;
end;

procedure TfrmCandidateMarket.aExtraFilterExecute(Sender: TObject);
begin
  if (TfrmCandidateAllFilters.ShowDocument(@FScanOptions.GlobalScanCriteria) = mrOk) then
    WriteExtraFilters;
end;

procedure TfrmCandidateMarket.GetCandidateParameters;
const
  cFileSize = 1000000;
begin
  if FileExists(FCandidateParametersFile) and (FileSizeByName(FCandidateParametersFile) > cFileSize) then
    ParseXmlParams
  else if IABClient.Connected then
    IABClient.Scanner.GetScannerParameters;
end;

function TfrmCandidateMarket.GetUseExtraFilters: Boolean;
begin
  Result := FUseExtraFilters;
end;

procedure TfrmCandidateMarket.SetUseExtraFilters(const Value: Boolean);
begin
  FUseExtraFilters := Value;
  cbUseExtraFilter.Checked := Value;
end;

procedure TfrmCandidateMarket.aGetCandidateParametersUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := IABClient.Connected;
end;

function TfrmCandidateMarket.CheckData: Boolean;
var
  Msg: string;
  ScanTypeData: PScanTypeData;
begin
  Msg := '';
  if GetLocationCode(vstLocation.RootNode.firstChild).IsEmpty then
    Msg := Msg + 'Location not selected! ' + sLineBreak;

  if not Assigned(vstScanType.FocusedNode) then
    Msg := Msg + 'Parameter not selected! ' + sLineBreak
  else
  begin
    ScanTypeData := vstScanType.FocusedNode^.GetData;
    if ScanTypeData^.ScanCode.IsEmpty then
      Msg := Msg + 'Parameter not selected! ' + sLineBreak;
  end;

  Result := Msg.IsEmpty;
  if not Result then
    TMessageDialog.ShowWarning(Msg);
end;

procedure TfrmCandidateMarket.ClearInstruments;
var
  i: Integer;
begin
  for i := 0 to lbInstruments.Items.Count - 1 do
    lbInstruments.Items.Objects[i] := nil;
  lbInstruments.Clear;
end;

procedure TfrmCandidateMarket.vstLocationChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PLocationData;
  IsExists: Boolean;
begin
  if Showing and (Node.CheckState in [csCheckedNormal, csUncheckedNormal]) then
  begin
    Data := Node^.GetData;
    IsExists := False;
    for var i := Low(FScanOptions.CandidateMarket.MarketLocation) to High(FScanOptions.CandidateMarket.MarketLocation) do
      if (FScanOptions.CandidateMarket.MarketLocation[i].Tag.ToUpper = Data^.LocationCode) then
      begin
        IsExists := True;
        FScanOptions.CandidateMarket.MarketLocation[i].Tag   := Data^.LocationCode;
        FScanOptions.CandidateMarket.MarketLocation[i].Value := Ord(Node.CheckState).ToString;
        Break;
      end;
      if not IsExists then
      begin
        SetLength(FScanOptions.CandidateMarket.MarketLocation, Length(FScanOptions.CandidateMarket.MarketLocation) + 1);
        FScanOptions.CandidateMarket.MarketLocation[Length(FScanOptions.CandidateMarket.MarketLocation) - 1].Tag   := Data^.LocationCode;
        FScanOptions.CandidateMarket.MarketLocation[Length(FScanOptions.CandidateMarket.MarketLocation) - 1].Value := Ord(Node.CheckState).ToString;
      end;
  end;
end;

procedure TfrmCandidateMarket.vstLocationFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PLocationData;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmCandidateMarket.vstLocationGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLocationData;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    COL_LOCATION:
      CellText := Data^.DisplayName;
  else
    CellText := '';
  end;
end;

procedure TfrmCandidateMarket.vstScanTypeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PScanTypeData;
begin
  if Showing and Assigned(Node) then
  begin
    Data := Node^.GetData;
    FScanOptions.ScanCriteria.ScanCode := Data^.ScanCode;
  end;
end;

procedure TfrmCandidateMarket.vstScanTypeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PScanTypeData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    COL_PARAMETER:
      Result := CompareText(Data1^.DisplayName, Data2^.DisplayName);
  end;
end;

procedure TfrmCandidateMarket.vstScanTypeDblClick(Sender: TObject);
begin
  aNewScanExecute(nil);
end;

procedure TfrmCandidateMarket.vstScanTypeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PScanTypeData;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmCandidateMarket.vstScanListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PScanItem;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    COL_SCAN_ID:
      Result := CompareValue(Data1^.ScanId, Data2^.ScanId);
    COL_COUNT:
      Result := CompareValue(Data1^.Count, Data2^.Count);
    COL_INST:
      Result := CompareText(Data1^.ScanCriteria.Instrument, Data2^.ScanCriteria.Instrument);
    COL_LOCATION_CODE:
      Result := CompareText(Data1^.ScanCriteria.LocationCode, Data2^.ScanCriteria.LocationCode);
    COL_SCAN_CODE:
      Result := CompareText(Data1^.ScanCriteria.ScanCode, Data2^.ScanCriteria.ScanCode);
    COL_NUMBER_OF_ROWS:
      Result := CompareValue(Data1^.ScanCriteria.NumberOfRows, Data2^.ScanCriteria.NumberOfRows);
  end;
end;

procedure TfrmCandidateMarket.vstScanListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PScanItem;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmCandidateMarket.vstScanListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TScanItem);
end;

procedure TfrmCandidateMarket.vstScanListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PScanItem;
begin
  Data := Node^.GetData;
  case Column of
    COL_SCAN_ID:
      CellText := Data^.ScanId.ToString;
    COL_COUNT:
      CellText := Data^.Count.ToString;
    COL_INST:
      CellText := Data^.ScanCriteria.Instrument;
    COL_LOCATION_CODE:
      CellText := Data^.ScanCriteria.LocationCode;
    COL_SCAN_CODE:
      CellText := Data^.ScanCriteria.ScanCode;
    COL_NUMBER_OF_ROWS:
      CellText := Data^.ScanCriteria.NumberOfRows.ToString;
  else
    CellText := '';
  end;
end;

procedure TfrmCandidateMarket.OnScannerAdd(Sender: TObject; ScanId: Integer);
var
  Data: PScanItem;
  Node: PVirtualNode;
  i: Integer;
begin
  if (ScanId > 0) then
  begin
    vstScanList.BeginUpdate;
    try
      if FScanList.ContainsKey(ScanId) then
        Node := FScanList[ScanId]
      else
        Node := vstScanList.AddChild(nil);

      Data := Node^.GetData;
      i := IABClient.Scanner.IndexOfScanId(ScanId);
      if (i > -1) then
      begin
        Data^.ScanId       := IABClient.Scanner.Items[i].ScanId;
        Data^.ScanCriteria := IABClient.Scanner.Items[i].QueryCriteria;
        Data^.Count        := IABClient.Scanner.Items[i].Count;
        FScanList.AddOrSetValue(ScanId, Node);
      end;
    finally
      vstScanList.EndUpdate;
    end;
  end;
end;

procedure TfrmCandidateMarket.OnScannerCancel(Sender: TObject; ScanId: Integer);
var
  Node: PVirtualNode;
begin
  vstScanList.BeginUpdate;
  try
    if FScanList.ContainsKey(ScanId) then
    begin
      Node := FScanList[ScanId];
      if Assigned(Node) then
        vstScanList.DeleteNode(Node);
      FScanList.Remove(ScanId);
      UpdateStatus(C_PANEL_STATUS, 'Canceled Scan ' + FScanOptions.ScanId.ToString);
    end;
  finally
    vstScanList.EndUpdate;
  end;
end;

procedure TfrmCandidateMarket.aCancelScanExecute(Sender: TObject);
var
  Data: PScanItem;
  Node: PVirtualNode;
begin
  Node := vstScanList.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    IABClient.CancelScan(Data^.ScanId);
  end;
end;

procedure TfrmCandidateMarket.vstScanTypeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PScanTypeData;
begin
  Data := Node^.GetData;
  case Column of
    COL_NAME:
      CellText := Data^.DisplayName;
  else
    CellText := '';
  end;
end;

procedure TfrmCandidateMarket.vstInstrumentsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PInstrumentData;
  ColumnsItem1, ColumnsItem2: TExtraColumns.TColumnsItem;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    COL_INSTRUMENT:
      Result := CompareText(Data1^.Name, Data2^.Name);
    COL_SYMBOL:
      Result := CompareText(Data1^.Symbol, Data2^.Symbol);
    COL_RANKING:
      if (Data1^.ExtraColumns.Items.ContainsKey(-1)) and (Data2^.ExtraColumns.Items.ContainsKey(-1)) then
      begin
        ColumnsItem1 := Data1^.ExtraColumns.Items[-1];
        ColumnsItem2 := Data2^.ExtraColumns.Items[-1];
        Result := CompareValue(ColumnsItem1.Rank, ColumnsItem2.Rank);
      end
      else
        Result := 0;
    COL_POSITION:
      Result := CompareValue(Data1^.ExtraColumns.Position, Data2^.ExtraColumns.Position);
  end;
end;

procedure TfrmCandidateMarket.vstInstrumentsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := TFloatEditLink.Create;
  TFloatEditLink(EditLink).Column   := COL_RANKING;
  TFloatEditLink(EditLink).Decimals := C_DECIMALS;
end;

procedure TfrmCandidateMarket.vstInstrumentsDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := vstInstruments.SelectedCount > 0;
end;

procedure TfrmCandidateMarket.vstInstrumentsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PInstrumentData;
begin
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if (Data^.Isolate = 1) then
    begin
      TargetCanvas.Font.Color := clRed;
    end;
    if (Column = COL_LAST_CLOSE) then
    begin
     if (StrToFloatDef(vstInstruments.Text[Node, Column], 0) >= 0) then
       TargetCanvas.Font.Color := clGreen
     else
       TargetCanvas.Font.Color := clRed;
    end
  end;
end;

procedure TfrmCandidateMarket.vstInstrumentsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  if Assigned(Node) then
  begin
    vstInstruments.SortTree(vstInstruments.Header.Columns[COL_RANKING].Index, sdDescending);
    RecalcSortIndex;
  end;
end;

procedure TfrmCandidateMarket.vstInstrumentsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TfrmCandidateMarket.vstInstrumentsEndDrag(Sender, Target: TObject; X, Y: Integer);
var
  DragDropOptions: TCandidateDragDropOptions;
begin
  if Assigned(FCandidateMarket) and (Target = FCandidateMarket.GetMainTree) then
  begin
    DragDropOptions := TfrmCandidateDragDropOptions.GetCandidateDragDropOptions;
    if not DragDropOptions.RememberLastOption then
    begin
      if (TfrmCandidateDragDropOptions.ShowDocument = mrOk) then
        TfrmCandidateDragDropOptions.LoadParamsFromXml(DragDropOptions)
      else
        Exit;
    end;
    if (DragDropOptions.SourceType = stCandidateMarket) then
      AddInstrumentToCandidateMain(kaAddAll)
    else
      AddInstrumentToStaticList(DragDropOptions);
  end;
end;

procedure TfrmCandidateMarket.vstInstrumentsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PInstrumentData;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
  begin
    TIABMarket.CancelMarketData(Data^.Id);
    Data^.Clear;
  end;
end;

procedure TfrmCandidateMarket.vstInstrumentsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

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
  ColumnsItem : TExtraColumns.TColumnsItem;
  Data        : PInstrumentData;
  formatStr   : string;
begin
  Data := Node^.GetData;
  case Column of
    COL_INSTRUMENT:
      CellText := Data^.Name;
    COL_SYMBOL:
      CellText := Data^.Symbol;
    COL_RANKING:
      if Data^.ExtraColumns.Items.ContainsKey(-1) then
      begin
        ColumnsItem := Data^.ExtraColumns.Items[-1];
        CellText := SimpleRoundTo(ColumnsItem.Rank, -C_DECIMALS).ToString;
      end;
    COL_POSITION:
      CellText := Data^.ExtraColumns.Position.ToString;
    COL_LAST_CLOSE:
      CellText := GetLastClose(Data^.Id);
  else
    if Data^.ExtraColumns.Items.ContainsKey(Column - C_FIXED_COLUMN_INDEX) then
    begin
      ColumnsItem := Data^.ExtraColumns.Items[Column - C_FIXED_COLUMN_INDEX];
      formatStr := TIABTickType(Column - C_FIXED_COLUMN_INDEX).ToFormat;
      CellText := Format(formatStr, [ColumnsItem.Price]);
    end
    else
      CellText := '0';
  end;
end;

procedure TfrmCandidateMarket.aPrintExecute(Sender: TObject);
begin
  Printer.Orientation := poLandscape;
  vstInstruments.Print(Printer, True);
end;

procedure TfrmCandidateMarket.aExportToCSVExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToCSV(vstInstruments, Self.Caption);
end;

procedure TfrmCandidateMarket.aExportToExcelExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToExcel(vstInstruments, Self.Caption);
end;

function TfrmCandidateMarket.GetIsolateColumnValue(aColumn: Integer; aData: PSokidInfo): string;
begin
  case aColumn of
    COL_EXCLUDE_NAME:
      Result := aData^.Name;
    COL_EXCLUDE_CONID:
      Result := aData^.ContractId.ToString;
    COL_EXCLUDE_SYMBOL:
      Result := aData^.Symbol;
    COL_EXCLUDE_LOCALSYMBOL:
      Result := aData.LocalSymbol;
    COL_EXCLUDE_CURRENCY:
      Result := aData.Currency;
    COL_EXCLUDE_EXCHANGE:
      Result := aData.Exchange;
    COL_EXCLUDE_EXPIRY:
      if (aData.Expiry > 0) then
        Result := FormatDateTime('DD.MM.YYYY hh:nn', aData.Expiry)
      else
        Result := '';
    COL_EXCLUDE_DESCRIPTION:
      Result := aData.Description;
  else
    Result := '';
  end;
end;

procedure TfrmCandidateMarket.vstIsolateCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PSokidInfo;
begin
  Result := 0;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  if Assigned(Data1) and Assigned(Data2) then
    Result := CompareText(GetIsolateColumnValue(Column, Data1), GetIsolateColumnValue(Column, Data2));
end;

procedure TfrmCandidateMarket.vstIsolateDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TfrmCandidateMarket.vstIsolateDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  NewData: PSokidInfo;
  NewNode: PVirtualNode;
  SokidInfo: TSokidInfo;
  SourceData: PInstrumentData;
  SourceNode: PVirtualNode;
  SourceTree: TBaseVirtualTree;
begin
  if (Sender <> Source) then
  begin
    vstIsolate.BeginUpdate;
    try
      SourceTree := TVirtualStringTree(Source);
      SourceNode := SourceTree.GetFirstSelected;
      while Assigned(SourceNode) do
      begin
        SourceData := SourceNode^.GetData;
        if SokidList.ContainsKey(SourceData^.Id) then
        begin
          SokidInfo := SokidList.Items[SourceData^.Id];
          NewNode := vstIsolate.AddChild(nil);
          NewData := NewNode^.GetData;
          NewData^.AssignFrom(SokidInfo);
          vstIsolate.IsVisible[NewNode] := True;
          vstIsolate.InvalidateNode(NewNode);
          SokidInfo.Isolate := 1;
          SokidList.SetValue(SokidInfo);
          if FCandidateDataList.ContainsKey(NewData^.ContractId) then
            FCandidateDataList.Items[NewData^.ContractId].Isolate := 1;
        end;
        SourceNode := SourceTree.GetNextSelected(SourceNode);
      end;
    finally
      vstIsolate.EndUpdate;
    end;
  end;
end;

procedure TfrmCandidateMarket.vstIsolateDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := Source = vstInstruments;
end;

procedure TfrmCandidateMarket.vstIsolateFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmCandidateMarket.vstIsolateGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    CellText := GetIsolateColumnValue(Column, Data);
end;

procedure TfrmCandidateMarket.aIsolateColumnSettingsExecute(Sender: TObject);
begin
  TStoreHelper.ShowColumnSettings(vstIsolate, GetIdentityName, 0);
end;

procedure TfrmCandidateMarket.aIsolateDeleteExecute(Sender: TObject);
var
  SokidInfo: TSokidInfo;
  Data: PSokidInfo;
  Node: PVirtualNode;
begin
  Node := vstIsolate.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if SokidList.ContainsKey(Data^.ContractId) then
    begin
      SokidInfo := SokidList.Items[Data^.ContractId];
      SokidInfo.Isolate := 0;
      SokidList.SetValue(SokidInfo);
      vstIsolate.DeleteNode(Node);

      if FCandidateDataList.ContainsKey(Data^.ContractId) then
        FCandidateDataList.Items[Data^.ContractId].Isolate := 0;
    end;
  end;
end;

procedure TfrmCandidateMarket.FillIsolate;
var
  SokidInfo: TSokidInfo;
  Data: PSokidInfo;
  Node: PVirtualNode;
begin
  vstIsolate.BeginUpdate;
  try
    for SokidInfo in SokidList.Values do
      if (SokidInfo.Isolate = 1) then
      begin
        Node := vstIsolate.AddChild(nil);
        Data := Node^.GetData;
        Data^.AssignFrom(SokidInfo);
        vstIsolate.IsVisible[Node] := True;
        vstIsolate.InvalidateNode(Node);
      end;
  finally
    vstIsolate.EndUpdate;
  end;
end;

end.
