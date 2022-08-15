unit Search.Instruments;
{Checklist settings
 Unchecking the setting in TWS Global Configuration at API -> Settings -> "Expose entire trading schedule to the API"  }

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.TabNotBk, VirtualTrees, Vcl.Menus,
  BrokerHelperAbstr, Global.Types, IABFunctions, IABSocketAPI, DebugWriter, Vcl.CheckLst, Scanner.Types, Entity.Sokid,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABSocketAPI_const, Document, System.DateUtils,
  InstrumentList, Utils, DefinitionOptionalParameter, DaImages, System.Actions, Vcl.ActnList, Winapi.ShellAPI,
  Winapi.ActiveX, Vcl.Printers, VirtualTrees.ExportHelper, System.Generics.Collections, Search.Types, Frame.Option,
  Frame.Custom, Frame.Future, Data.DB, DaModule, HtmlLib, System.UITypes, Edit.SokidList,
  Frame.RealtimeFeeds, MessageDialog, Monitor.Types, Edit.Instrument, System.Math, System.Threading, Publishers.Interfaces,
  Common.Types, IABFunctions.RequestsQueue, BrokerHelperFactory, Monitor.Interfaces, System.IOUtils, ArrayHelper, DaModule.Utils,
  Publishers, IABFunctions.Helpers, MonitorTree.Helper, MonitorTree.Document, System.Win.ComObj,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TfrmSearchInstruments = class(TCustomForm, IInstrumentSpecDetails)
    aAddInstrumentToDB: TAction;
    aClear: TAction;
    aColumnSettings: TAction;
    aColumnSettingsLists: TAction;
    ActionList: TActionList;
    aDeleteList: TAction;
    aEditInstruments: TAction;
    aEditList: TAction;
    aInstExportToCSV: TAction;
    aInstExportToExcel: TAction;
    aInstPrint: TAction;
    aInstrumentsClear: TAction;
    aInstrumentsDeleteItem: TAction;
    aInstrumentsMoveToMonitor: TAction;
    aNewList: TAction;
    aOpenResultInANewWindow: TAction;
    aSaveAsNewList: TAction;
    aSearch: TAction;
    aSearchFuture: TAction;
    aSearchOption: TAction;
    aShowContractInspector: TAction;
    btnClear: TBitBtn;
    btnClearSearchText: TBitBtn;
    btnColumnSettings: TBitBtn;
    btnColumnSettingsLists: TBitBtn;
    btnDeleteList: TBitBtn;
    btnEditInstruments: TBitBtn;
    btnEditList: TBitBtn;
    btnExportToExcel: TBitBtn;
    btnInstExportToCSV: TBitBtn;
    btnInstPrint: TBitBtn;
    btnNewList: TBitBtn;
    btnSaveAsNewList: TBitBtn;
    btnSearch: TBitBtn;
    btnShowContractInspector: TBitBtn;
    cbCurrencyFilter: TComboBox;
    cbExtCurrency: TComboBox;
    cbExtExchange: TComboBox;
    cbExtSecurityType: TComboBox;
    cbExtSymbol: TComboBox;
    cbInstrList: TComboBox;
    cbInstrumentGroupFilter: TComboBox;
    cbInstrumentTypeFilter: TComboBox;
    cbMarketFilter: TComboBox;
    cbSearchFunction: TComboBox;
    cbSectorFilter: TComboBox;
    cbSecurityType: TCheckListBox;
    cbTradablesFilter: TComboBox;
    cbUseSecurityType: TCheckBox;
    edExtExpiryMonth: TEdit;
    edMatchingSymbol: TComboBox;
    edSearch: TEdit;
    edtSearchLists: TEdit;
    eSearchStringTestBroker: TEdit;
    eSymbolFilter: TEdit;
    frameRealtimeFeeds: TframeRealtimeFeeds;
    gbContractDetails: TGroupBox;
    lblCurrency: TLabel;
    lblExtCurrency: TLabel;
    lblExtExchange: TLabel;
    lblExtExpiryMonth: TLabel;
    lblExtSecurityTypeCaption: TLabel;
    lblExtSymbol: TLabel;
    lblInstrumentGroup: TLabel;
    lblInstrumentType: TLabel;
    lblMarket: TLabel;
    lblSearchFunction: TLabel;
    lblSearchLists: TLabel;
    lblSearchString: TLabel;
    lblSearchStringTestBroker: TLabel;
    lblSector: TLabel;
    lblSecurityType: TLabel;
    lblSymbolName: TLabel;
    lblTradablesMarkets: TLabel;
    miAddInstrumentToDB: TMenuItem;
    miClearResultList: TMenuItem;
    miDeleteItem: TMenuItem;
    miMoveToMonitor: TMenuItem;
    miOpenResultInANewWindow: TMenuItem;
    miOpenSearchConfiguration: TMenuItem;
    miSaveSearchConfiguration: TMenuItem;
    miSearchChain: TMenuItem;
    miSearchFuture: TMenuItem;
    miSearchOption: TMenuItem;
    miSeparator01: TMenuItem;
    pcBrokers: TPageControl;
    pcDerivatives: TPageControl;
    pmInstruments: TPopupMenu;
    pmSearchParam: TPopupMenu;
    pnlCategory: TPanel;
    pnlCategoryTop: TPanel;
    pnlCentral: TPanel;
    pnlContractDetails: TPanel;
    pnlIBSearch: TPanel;
    pnlInstruments: TPanel;
    pnlInstrumentsTop: TPanel;
    pnlInteractiveBroker: TPanel;
    pnlRealtimeFeeds: TGroupBox;
    pnlSearchBottom: TPanel;
    pnlSearchParam: TPanel;
    rbFuturesCalendar: TRadioButton;
    rbNextRolling: TRadioButton;
    rbOnlySecurityType: TRadioButton;
    rbTypeSearch: TRadioGroup;
    rgActions: TRadioGroup;
    rgSearchType: TRadioGroup;
    sbMain: TStatusBar;
    sdInstruments: TSaveDialog;
    splCategory: TSplitter;
    splContractDetails: TSplitter;
    splRealtimeFeeds: TSplitter;
    tsInteractiveBroker: TTabSheet;
    tsNordNetBroker: TTabSheet;
    tsTestBroker: TTabSheet;
    vstInstruments: TVirtualStringTree;
    vstSokidLists: TVirtualStringTree;
    procedure aAddInstrumentToDBExecute(Sender: TObject);
    procedure aClearExecute(Sender: TObject);
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aColumnSettingsListsExecute(Sender: TObject);
    procedure aDeleteListExecute(Sender: TObject);
    procedure aEditInstrumentsExecute(Sender: TObject);
    procedure aEditListExecute(Sender: TObject);
    procedure aInstExportToCSVExecute(Sender: TObject);
    procedure aInstExportToExcelExecute(Sender: TObject);
    procedure aInstPrintExecute(Sender: TObject);
    procedure aInstrumentsClearExecute(Sender: TObject);
    procedure aInstrumentsDeleteItemExecute(Sender: TObject);
    procedure aInstrumentsMoveToMonitorExecute(Sender: TObject);
    procedure aInstrumentsMoveToMonitorUpdate(Sender: TObject);
    procedure aNewListExecute(Sender: TObject);
    procedure aOpenResultInANewWindowExecute(Sender: TObject);
    procedure aSaveAsNewListExecute(Sender: TObject);
    procedure aSearchExecute(Sender: TObject);
    procedure aSearchFutureExecute(Sender: TObject);
    procedure aSearchOptionExecute(Sender: TObject);
    procedure aShowContractInspectorExecute(Sender: TObject);
    procedure btnClearSearchTextClick(Sender: TObject);
    procedure cbExtSecurityTypeChange(Sender: TObject);
    procedure cbSearchFunctionChange(Sender: TObject);
    procedure cbUseSecurityTypeClick(Sender: TObject);
    procedure DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtSearchListsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miOpenSearchConfigurationClick(Sender: TObject);
    procedure miSaveSearchConfigurationClick(Sender: TObject);
    procedure pcBrokersChange(Sender: TObject);
    procedure pmInstrumentsPopup(Sender: TObject);
    procedure rgSearchTypeClick(Sender: TObject);
    procedure vstInstrumentsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstInstrumentsDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstInstrumentsDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vstInstrumentsDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstInstrumentsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstInstrumentsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstInstrumentsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstSokidListsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstSokidListsDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vstSokidListsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstSokidListsIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string; var Result: Integer);
    procedure vstSokidListsNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstSokidListsNodeMoving(Sender: TBaseVirtualTree; Node, Target: PVirtualNode; var Allowed: Boolean);
    procedure vstSokidListsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private const
    C_SEARCH_SECTION_NAME = 'Search';
    C_SEPARATOR = ';';
    C_DATA_ID_COMMON           = 2;
    C_DATA_ID_SYMBOL           = 4;
    C_DATA_ID_CONTRACT_DETAILS = 11;
    C_IDENTITY_NAME = 'SearchInstruments';

    COL_BROKER           = 0;
    COL_ID               = 1;
    COL_ISIN             = 2;
    COL_NAME             = 3;
    COL_SYMBOL           = 4;
    COL_LOCAL_SYMBOL     = 5;
    COL_SECURITY_TYPE    = 6;
    COL_GROUP            = 7;
    COL_CURRENCY         = 8;
    COL_TRADABLES        = 9;
    COL_SECTOR           = 10;
    COL_EXCHANGE         = 11;
    COL_PRIMARY_EXCHANGE = 12;
    COL_MARKET           = 13;
    COL_EXPIRY           = 14;
    COL_LAST_PRICE       = 15;
    COL_ERROR_CODE       = 16;
    COL_ERROR_MSG        = 17;

    COL_SL_NAME             = 0;
    COL_SL_SYMBOL           = 1;
    COL_SL_LOCAL_SYMBOL     = 2;
    COL_SL_CONID            = 3;
    COL_SL_CURRENCY         = 4;
    COL_SL_EXCHANGE         = 5;
    COL_SL_SECURITY_TYPE    = 6;
    COL_SL_BROKER           = 7;
    COL_SL_EXPIRY           = 8;
    COL_SL_UNDERLYING_CONID = 9;
    COL_SL_DESCRIPTION      = 10;
  private
    { Private declarations }
    FFutureFrameList: TObjectDictionary<Integer, TframeFuture>;
    FInstruments: TArray<TSokidInfo>;
    FOptionFrameList: TObjectDictionary<Integer, TframeOption>;
    FStickForm: TForm;
    function GetCategoryColumnValue(aColumn: Integer; aData: PSokidInfo): string;
    function GetColumnValue(aColumn: Integer; aData: PSokidInfo): string;
    procedure AdditionalFilters(const ABrokerType: TBrokerType; var AInstruments: TArray<TSokidInfo>);
    procedure DoSymbolSample(Sender: TObject; DataID, Item, Count: Integer; SymbolDerivative: TIABSymbolDerivativeSpecItem);
    procedure GetInstruments(const ABrokerType: TBrokerType; const AFilter: string; var AInstruments: TArray<TSokidInfo>);
    procedure LoadSokidLists;
    procedure SearchChain(aSecurityType: TIABSecurityType);
    procedure ShowResultInstrument(AInstruments: TArray<TSokidInfo>);

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IInstrumentSpecDetails
    procedure OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
  protected
    function GetIdentityName: string; override;
    function GetSecurityTypeText: string;
    procedure LoadParamsFromXml; virtual;
    procedure SaveParamsToXml; virtual;
  public
    class procedure ShowDocument(const aStickForm: TForm);
    property StickForm: TForm read FStickForm write FStickForm;
  end;

var
  frmSearchInstruments: TfrmSearchInstruments;

implementation

{$R *.dfm}

procedure TfrmSearchInstruments.FormCreate(Sender: TObject);
var
  st: TIABSecurityType;
begin
  inherited;
  FStickForm := nil;
  cbSecurityType.Items.Clear;
  for st := Low(TIABSecurityType) to High(TIABSecurityType) do
  begin
    if st in [stStock, stOption, stFuture, stIndex, stCFD, stCmdty, stCash] then
      cbSecurityType.Items.Add(st.ToString);
  end;

   cbExtSecurityType.Items.Clear;
  for st := Low(TIABSecurityType) to High(TIABSecurityType) do
  begin
    if st in [stStock, stOption, stFuture, stIndex, stCFD, stCmdty, stCash] then
      cbExtSecurityType.Items.Add(st.ToString);
  end;

  vstInstruments.NodeDataSize := SizeOf(TSokidInfo);
  vstSokidLists.NodeDataSize := SizeOf(TSokidInfo);

  LoadParamsFromXml;
  IABClient.OnSymbolSample := DoSymbolSample;
  FOptionFrameList := TObjectDictionary<Integer, TframeOption>.Create;
  FFutureFrameList := TObjectDictionary<Integer, TframeFuture>.Create;
  TPublishers.InstrumentSpecDetailsPublisher.Subscribe(Self);
  LoadSokidLists;
  TMonitorTree.Initialize(vstInstruments);
  TMonitorTree.Initialize(vstSokidLists);
  frameRealtimeFeeds.Initialize;
end;

procedure TfrmSearchInstruments.FormDestroy(Sender: TObject);
begin
  TPublishers.InstrumentSpecDetailsPublisher.Unsubscribe(Self);
  IABClient.OnSymbolSample := nil;

  vstInstruments.Clear;
  vstSokidLists.Clear;
  SaveParamsToXml;
  FreeAndNil(FOptionFrameList);
  FreeAndNil(FFutureFrameList);
  inherited;
end;

procedure TfrmSearchInstruments.FormShow(Sender: TObject);
var
  MainForm: TForm;
begin
  inherited;
  if Assigned(FStickForm) then
    MainForm := FStickForm
  else
    MainForm := Application.MainForm;
  Self.Left := MainForm.Left + MainForm.Width;
  Self.Top := MainForm.Top;
  SetFocusSafely(edMatchingSymbol);
  vstInstruments.Height := pnlInstruments.Height div 3 * 2;
end;

class procedure TfrmSearchInstruments.ShowDocument(const aStickForm: TForm);
begin
  if Assigned(frmSearchInstruments) then
    FreeAndNil(frmSearchInstruments);
  frmSearchInstruments := TfrmSearchInstruments.Create(Application);
  frmSearchInstruments.StickForm := aStickForm;
  frmSearchInstruments.Show;
end;

function TfrmSearchInstruments.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

function TfrmSearchInstruments.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmSearchInstruments.GetInstruments(const ABrokerType: TBrokerType; const AFilter: string; var AInstruments: TArray<TSokidInfo>);
var
  LoginError: string;
  FBrokerHelperClass: TBrokerHelperClass;
begin
  FBrokerHelperClass := TBrokerHelperFactory.GetBrokerHelperClass(ABrokerType.ToAbbrevName);
  if Assigned(FBrokerHelperClass) then
  begin
    if (FBrokerHelperClass.HelperName = TBrokerType.brNN.ToAbbrevName) then
    begin
      LoginError := FBrokerHelperClass.Login(TNordNetParams.C_LOGIN, TNordNetParams.C_PASSWORD);
      if LoginError.IsEmpty then
        AInstruments := FBrokerHelperClass.GetSokidBySymbol(AFilter)
      else
        TMessageDialog.ShowError(LoginError);
    end
    else if (FBrokerHelperClass.HelperName = TBrokerType.brIB.ToAbbrevName) then
      case rbTypeSearch.ItemIndex of
        0:
          AInstruments := FBrokerHelperClass.GetSokidBySymbol(AFilter);
        1:
          AInstruments := FBrokerHelperClass.GetSokidByName(AFilter);
      end;
  end;
end;

procedure TfrmSearchInstruments.aInstrumentsMoveToMonitorExecute(Sender: TObject);
var
  Monitor: IMonitor;
begin
  if Supports(Application.MainForm, IMonitor, Monitor) then
    Monitor.AddInstrumentFromSearch(nil, vstInstruments);
end;

procedure TfrmSearchInstruments.aInstrumentsMoveToMonitorUpdate(Sender: TObject);
var
  Data: PSokidInfo;
begin
  if Assigned(vstInstruments.FocusedNode) then
  begin
    Data := vstInstruments.FocusedNode^.GetData;
    TAction(Sender).Enabled := Data^.TWSMessageItem.ErrorCode <= 0;
  end
end;

procedure TfrmSearchInstruments.aInstrumentsDeleteItemExecute(Sender: TObject);
begin
  if Assigned(vstInstruments.FocusedNode) then
    vstInstruments.DeleteNode(vstInstruments.FocusedNode);
end;

procedure TfrmSearchInstruments.aOpenResultInANewWindowExecute(Sender: TObject);
var
  Search: TfrmSearchInstruments;
begin
  Search := TfrmSearchInstruments.Create(Self);
  Search.Position := PoDefault;
  Search.Top := Top + 10;
  Search.Left := Left + 10;
  Search.ShowResultInstrument(FInstruments);
  Search.Show;
end;

procedure TfrmSearchInstruments.aAddInstrumentToDBExecute(Sender: TObject);
var
  Data: PSokidInfo;
  Node: PVirtualNode;
begin
  Node := vstInstruments.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if Assigned(Data) and not SokidList.ContainsKey(Data^.ContractId) then
      SokidList.SetValue(Data^);
  end;
end;

procedure TfrmSearchInstruments.aColumnSettingsListsExecute(Sender: TObject);
begin
  TStoreHelper.ShowColumnSettings(vstSokidLists, GetIdentityName + '.vstInstruments', 0);
end;

procedure TfrmSearchInstruments.aClearExecute(Sender: TObject);
begin
  vstInstruments.Clear;
  SetLength(FInstruments, 0);
end;

procedure TfrmSearchInstruments.aColumnSettingsExecute(Sender: TObject);
begin
  TStoreHelper.ShowColumnSettings(vstInstruments, GetIdentityName + '.vstSokidLists', 0);
end;

procedure TfrmSearchInstruments.AdditionalFilters(const ABrokerType: TBrokerType; var AInstruments: TArray<TSokidInfo>);
var
  CurrencyFilter: string;
  DeletedInst: string;
  FindTradables: Boolean;
  i: Integer;
  j: Integer;
  GroupFilter: string;
  TypeFilter: string;
  MarketFilter: string;
  SectorFilter: string;
  SecuryTypeFilter: string;
  st: TIABSecurityType;
  SymbolFilter: string;
  TradablesFilter: string;
begin
  if (ABrokerType = TBrokerType.brNN) then
  begin
    SymbolFilter := UpperCase(Trim(eSymbolFilter.Text));

    if cbInstrumentGroupFilter.ItemIndex > 0 then
    begin
      GroupFilter := cbInstrumentGroupFilter.Items[cbInstrumentGroupFilter.ItemIndex];
      GroupFilter := copy(GroupFilter, 1, pos(',', GroupFilter) - 1);
    end
    else
      GroupFilter := '';

    if cbCurrencyFilter.ItemIndex > 0 then
      CurrencyFilter := cbCurrencyFilter.Items[cbCurrencyFilter.ItemIndex]
    else
      CurrencyFilter := '';

    if cbTradablesFilter.ItemIndex > 0 then
    begin
      TradablesFilter := cbTradablesFilter.Items[cbTradablesFilter.ItemIndex];
      TradablesFilter := copy(TradablesFilter, 1, pos(',', TradablesFilter) - 1);
    end
    else
      TradablesFilter := '';

    if cbInstrumentTypeFilter.ItemIndex > 0 then
    begin
      TypeFilter := cbInstrumentTypeFilter.Items[cbInstrumentTypeFilter.ItemIndex];
      TypeFilter := copy(TypeFilter, 1, pos(',', TypeFilter) - 1);
    end
    else
      TypeFilter := '';

    if cbMarketFilter.ItemIndex > 0 then
    begin
      MarketFilter := cbMarketFilter.Items[cbMarketFilter.ItemIndex];
      MarketFilter := copy(MarketFilter, 1, pos(',', MarketFilter) - 1);
    end
    else
      MarketFilter := '';

    if cbSectorFilter.ItemIndex > 0 then
      SectorFilter := cbSectorFilter.Items[cbSectorFilter.ItemIndex]
    else
      SectorFilter := '';

    if (SymbolFilter <> '') or (GroupFilter <> '') or (CurrencyFilter <> '') or (TradablesFilter <> '') or
      (TypeFilter <> '') or (MarketFilter <> '') or (SectorFilter <> '') then
    begin
      DeletedInst := C_SEPARATOR;
      for i := 0 to Length(AInstruments) - 1 do
      begin
        if (SymbolFilter <> '') and (pos(UpperCase(SymbolFilter), AInstruments[i].Symbol) = 0) then
        begin
          DeletedInst := DeletedInst + IntToStr(i) + C_SEPARATOR;
          Continue;
        end;

        if (GroupFilter <> '') and not SameText(GroupFilter, AInstruments[i].Group) then
        begin
          DeletedInst := DeletedInst + IntToStr(i) + C_SEPARATOR;
          Continue;
        end;

        if (CurrencyFilter <> '') and not SameText(CurrencyFilter, AInstruments[i].Currency) then
        begin
          DeletedInst := DeletedInst + IntToStr(i) + C_SEPARATOR;
          Continue;
        end;

        if (TradablesFilter <> '') then
        begin
          FindTradables := false;
          for j := 0 to Length(AInstruments[i].Tradables) do
            if SameText(TradablesFilter, IntToStr(AInstruments[i].Tradables[j].MarketId)) then
            begin
              FindTradables := true;
              Break;
            end;

          if not FindTradables then
          begin
            DeletedInst := DeletedInst + IntToStr(i) + C_SEPARATOR;
            Continue;
          end;
        end;

        if (TypeFilter <> '') and not SameText(TypeFilter, AInstruments[i].SecurityType) then
        begin
          DeletedInst := DeletedInst + IntToStr(i) + C_SEPARATOR;
          Continue;
        end;

        if (SectorFilter <> '') and not SameText(SectorFilter, AInstruments[i].Sector) then
        begin
          DeletedInst := DeletedInst + IntToStr(i) + C_SEPARATOR;
          Continue;
        end;
      end;

      j := 0;
      for i := 0 to Length(AInstruments) - 1 do
        if pos(C_SEPARATOR + IntToStr(i) + C_SEPARATOR, DeletedInst) = 0 then
        begin
          AInstruments[j].AssignFrom(AInstruments[i]);
          inc(j);
        end;

      SetLength(AInstruments, j);
    end;
  end
  else if (ABrokerType = TBrokerType.brIB) then
  begin
    if cbUseSecurityType.Checked then
    begin
      for i := 0 to cbSecurityType.Items.Count - 1 do
        if cbSecurityType.Checked[i] then
          SecuryTypeFilter := SecuryTypeFilter + C_SEPARATOR + cbSecurityType.Items[i].ToUpper;

      if (SecuryTypeFilter = '') then
        for st := Low(TIABSecurityType) to High(TIABSecurityType) do
          if st in [stStock, stOption, stFuture, stIndex, stCmdty] then
            SecuryTypeFilter := SecuryTypeFilter + C_SEPARATOR + st.ToString;

      DeletedInst := C_SEPARATOR;
      for i := 0 to Length(AInstruments) - 1 do
        if (Pos(AInstruments[i].SecurityType, SecuryTypeFilter) = 0) then
          DeletedInst := DeletedInst + i.ToString + C_SEPARATOR;
    end;

    j := 0;
    for i := 0 to Length(AInstruments) - 1 do
      if (Pos(C_SEPARATOR + IntToStr(i) + C_SEPARATOR, DeletedInst) = 0) then
      begin
        AInstruments[j].AssignFrom(AInstruments[i]);
        Inc(j);
      end;
    SetLength(AInstruments, j);
  end;
end;

procedure TfrmSearchInstruments.ShowResultInstrument(AInstruments: TArray<TSokidInfo>);
var
  i: Integer;
  Node: PVirtualNode;
  Data: PSokidInfo;
  FindInstument: Boolean;
begin
  vstInstruments.BeginUpdate;
  try
    case rgActions.ItemIndex of
      0, 1:
        begin
          if (rgActions.ItemIndex = 0) then
            vstInstruments.Clear;

          for i := 0 to Length(AInstruments) - 1 do
            if not AInstruments[i].Symbol.IsEmpty then
            begin
              Node := vstInstruments.AddChild(nil);
              Data := Node^.GetData;
              Data^.AssignFrom(AInstruments[i]);
            end;
        end;
      2:
        begin
          Node := vstInstruments.GetFirst;
          while Assigned(Node) do
          begin
            Data := Node^.GetData;
            for i := 0 to Length(AInstruments) - 1 do
              if Data^.Equal(AInstruments[i]) then
              begin
                vstInstruments.DeleteNode(Node);
                Break;
              end;

            Node := vstInstruments.GetNextSibling(Node);
          end;
        end;
      3:
        begin
          Node := vstInstruments.GetFirst;
          while Assigned(Node) do
          begin
            Data := Node^.GetData;
            FindInstument := false;
            for i := 0 to Length(AInstruments) - 1 do
              if Data^.Equal(AInstruments[i]) then
              begin
                FindInstument := true;
                Break;
              end;
            if not FindInstument then
              vstInstruments.DeleteNode(Node);
            Node := vstInstruments.GetNextSibling(Node);
          end;
        end;
    end;
  finally
    vstInstruments.EndUpdate;
  end;
end;

function TfrmSearchInstruments.GetColumnValue(aColumn: Integer; aData: PSokidInfo): string;
begin
  case aColumn of
    COL_BROKER:
      Result := aData^.Broker.ToString;
    COL_ID:
      Result := IntToStr(aData^.ContractId);
    COL_ISIN:
      Result := aData^.IsIn;
    COL_NAME:
      Result := aData^.Name;
    COL_SYMBOL:
      Result := aData^.Symbol;
    COL_LOCAL_SYMBOL:
      Result := aData^.LocalSymbol;
    COL_SECURITY_TYPE:
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
    COL_MARKET:
      Result := ''; // aData^.Market;
    COL_EXPIRY:
      if (aData^.Expiry > 0) then
        Result := FormatDateTime('DD.MM.YYYY hh:nn', aData^.Expiry)
      else
        Result := '';
    COL_LAST_PRICE:
      if (aData^.LastPrice > 0) then
        Result := Format('%2.f', [aData^.LastPrice])
      else
        Result := '';
    COL_ERROR_CODE:
      if (aData^.TWSMessageItem.ErrorCode > 0) then
        Result := aData^.TWSMessageItem.ErrorCode.ToString
      else
        Result := '';
    COL_ERROR_MSG:
      if not aData^.TWSMessageItem.ErrorMsg.IsEmpty then
        Result := aData^.TWSMessageItem.ErrorMsg
      else
        Result := '';
  else
    Result := '';
  end;
end;

procedure TfrmSearchInstruments.LoadParamsFromXml;
var
  SecurityArray: TArray<string>;
  i: Integer;
  Items: TStringList;

  procedure JoinList(aDest: TComboBox; aSource: TStringList);
  begin
    for var j := 0 to aSource.Count - 1 do
      if (aDest.Items.IndexOf(aSource[j]) = -1) then
        aDest.Items.Add(aSource[j]);
  end;

  function GetMonth: string;
  var
    AYear, AMonth, ADay: Word;
  begin
    DecodeDate(IncMonth(Date), AYear, AMonth, ADay);
    Result := Concat(AYear.ToString, FormatFloat('00', AMonth));
  end;

begin
  cbCurrencyFilter.ItemIndex        := General.XMLFile.ReadInteger(C_SEARCH_SECTION_NAME, 'Currency', 0);
  cbInstrList.ItemIndex             := General.XMLFile.ReadInteger(C_SEARCH_SECTION_NAME, 'Instrument', 0);
  cbInstrumentGroupFilter.ItemIndex := General.XMLFile.ReadInteger(C_SEARCH_SECTION_NAME, 'InstrumentGroup', 0);
  cbInstrumentTypeFilter.ItemIndex  := General.XMLFile.ReadInteger(C_SEARCH_SECTION_NAME, 'InstrumentType', 0);
  cbMarketFilter.ItemIndex          := General.XMLFile.ReadInteger(C_SEARCH_SECTION_NAME, 'Market', 0);
  cbSearchFunction.ItemIndex        := General.XMLFile.ReadInteger(C_SEARCH_SECTION_NAME, 'SearchFunction', 0);
  cbSectorFilter.ItemIndex          := General.XMLFile.ReadInteger(C_SEARCH_SECTION_NAME, 'Sector', 0);
  cbTradablesFilter.ItemIndex       := General.XMLFile.ReadInteger(C_SEARCH_SECTION_NAME, 'Tradables', 0);
  cbUseSecurityType.Checked         := General.XMLFile.ReadBool(C_SEARCH_SECTION_NAME, 'UseSecurityType', false);
  edSearch.Text                     := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME, 'Filter', '');
  edMatchingSymbol.Text             := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME, 'MatchingSymbolsText', '');
  edMatchingSymbol.Items.Text       := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME, 'MatchingSymbols', '');
  eSymbolFilter.Text                := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME, 'Symbol', '');
  rgSearchType.ItemIndex            := General.XMLFile.ReadInteger(C_SEARCH_SECTION_NAME, 'SearchType', 0);

  SecurityArray := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME, 'SecurityType', '').Split([C_SEPARATOR]);
  for i := Low(SecurityArray) to High(SecurityArray) do
    if (StrToIntDef(SecurityArray[i], -1) < cbSecurityType.Count) and (StrToIntDef(SecurityArray[i], -1) > -1) then
      cbSecurityType.Checked[SecurityArray[i].ToInteger] := True;

  TStoreHelper.LoadFromXml(vstInstruments, C_SEARCH_SECTION_NAME + '.' + vstInstruments.Name);
  TStoreHelper.LoadFromXml(vstSokidLists, C_SEARCH_SECTION_NAME + '.' + vstSokidLists.Name);

  cbExtSymbol.Text            := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME, 'InstrumentSymbol', '');
  cbExtExchange.Text          := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME, 'InstrumentExchange', '');
  cbExtCurrency.Text          := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME, 'InstrumentCurrency', '');
//  edExtExpiryMonth.Text       := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME, 'InstrumentExpiry', GetMonth);
  cbExtSecurityType.ItemIndex := General.XMLFile.ReadInteger(C_SEARCH_SECTION_NAME, 'InstrumentSecurityType', -1);

  Items := TStringList.Create;
  try
    Items.Text := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME,'InstrumentSymbol.Items', '');
    JoinList(cbExtSymbol, Items);

    Items.Text := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME, 'InstrumentExchange.Items', '');
    JoinList(cbExtExchange, Items);

    Items.Text := General.XMLFile.ReadString(C_SEARCH_SECTION_NAME,'InstrumentCurrency.Items', '');
    JoinList(cbExtCurrency, Items);
  finally
    FreeAndNil(Items);
  end;

  cbUseSecurityTypeClick(nil);
  cbSearchFunctionChange(nil);
  cbExtSecurityTypeChange(nil);
end;

procedure TfrmSearchInstruments.SaveParamsToXml;
var
  SecurityType: string;
  i: Integer;
begin
  try
    for i := 0 to cbSecurityType.Count - 1 do
      if cbSecurityType.Checked[i] then
        SecurityType := SecurityType + i.ToString + C_SEPARATOR;

    General.XMLFile.WriteBool(C_SEARCH_SECTION_NAME, 'UseSecurityType', cbUseSecurityType.Checked);
    General.XMLFile.WriteInteger(C_SEARCH_SECTION_NAME, 'Currency', cbCurrencyFilter.ItemIndex);
    General.XMLFile.WriteInteger(C_SEARCH_SECTION_NAME, 'Instrument', cbInstrList.ItemIndex);
    General.XMLFile.WriteInteger(C_SEARCH_SECTION_NAME, 'InstrumentGroup', cbInstrumentGroupFilter.ItemIndex);
    General.XMLFile.WriteInteger(C_SEARCH_SECTION_NAME, 'InstrumentType', cbInstrumentTypeFilter.ItemIndex);
    General.XMLFile.WriteInteger(C_SEARCH_SECTION_NAME, 'Market', cbMarketFilter.ItemIndex);
    General.XMLFile.WriteInteger(C_SEARCH_SECTION_NAME, 'SearchFunction', cbSearchFunction.ItemIndex);
    General.XMLFile.WriteInteger(C_SEARCH_SECTION_NAME, 'SearchType', rgSearchType.ItemIndex);
    General.XMLFile.WriteInteger(C_SEARCH_SECTION_NAME, 'Sector', cbSectorFilter.ItemIndex);
    General.XMLFile.WriteInteger(C_SEARCH_SECTION_NAME, 'Tradables', cbTradablesFilter.ItemIndex);
    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'Filter', edSearch.Text);
    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'MatchingSymbols', edMatchingSymbol.Items.Text);
    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'MatchingSymbolsText', edMatchingSymbol.Text);
    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'SecurityType', SecurityType);
    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'Symbol', eSymbolFilter.Text);

    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'InstrumentSymbol', cbExtSymbol.Text);
    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'InstrumentExchange',  cbExtExchange.Text);
    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'InstrumentCurrency', cbExtCurrency.Text);
    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'InstrumentExpiry', edExtExpiryMonth.Text);
    General.XMLFile.WriteInteger(C_SEARCH_SECTION_NAME, 'InstrumentSecurityType', cbExtSecurityType.ItemIndex);

    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'InstrumentSymbol.Items', cbExtSymbol.Items.Text);
    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'InstrumentExchange.Items', cbExtExchange.Items.Text);
    General.XMLFile.WriteString(C_SEARCH_SECTION_NAME, 'InstrumentCurrency.Items', cbExtCurrency.Items.Text);

    TStoreHelper.SaveToXml(vstInstruments, C_SEARCH_SECTION_NAME + '.' + vstInstruments.Name);
    TStoreHelper.SaveToXml(vstSokidLists, C_SEARCH_SECTION_NAME + '.' + vstSokidLists.Name);
  finally
    General.XMLFile.Save;
  end;

  for var Frm in FOptionFrameList.Values do
    Frm.Deinitialize;
  for var Frm in FFutureFrameList.Values do
    Frm.Deinitialize;
  frameRealtimeFeeds.Deinitialize;
end;

procedure TfrmSearchInstruments.miSaveSearchConfigurationClick(Sender: TObject);
begin
  SaveParamsToXml;
end;

procedure TfrmSearchInstruments.OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
var
  SokidInfo: TSokidInfo;
begin
  if (IABClient.InstrumentSpecs.Items[Index].DataID = C_DATA_ID_CONTRACT_DETAILS) and
    SokidList.ContainsKey(IABClient.InstrumentSpecs.Items[Index].ContractId) then
  begin
    SokidInfo := SokidList.Items[IABClient.InstrumentSpecs.Items[Index].ContractId];
    if not General.Exchange.ShowQBALGO  and SokidInfo.Exchange.StartsWith('QBALGO') then
      Exit;
    SokidInfo.Broker := TBrokerType.brIB;
    SetLength(FInstruments, Length(FInstruments) + 1);
    FInstruments[High(FInstruments)].AssignFrom(SokidInfo);
    ShowResultInstrument(FInstruments);
  end
  else if (IABClient.InstrumentSpecs.Items[Index].DataID = C_DATA_ID_COMMON) and
    SokidList.ContainsKey(IABClient.InstrumentSpecs.Items[Index].ContractId) then
  begin
    SokidInfo := SokidList.Items[IABClient.InstrumentSpecs.Items[Index].ContractId];
    SokidInfo.Broker := TBrokerType.brIB;
    SetLength(FInstruments, Length(FInstruments) + 1);
    FInstruments[High(FInstruments)].AssignFrom(SokidInfo);
    ShowResultInstrument(FInstruments);
  end;
end;

procedure TfrmSearchInstruments.aInstExportToCSVExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToCSV(vstInstruments, 'SearchInstruments');
end;

procedure TfrmSearchInstruments.aInstExportToExcelExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToExcel(vstInstruments, 'SearchInstruments');
end;

procedure TfrmSearchInstruments.aInstrumentsClearExecute(Sender: TObject);
begin
  vstInstruments.Clear;
end;

procedure TfrmSearchInstruments.aInstPrintExecute(Sender: TObject);
begin
  Printer.Orientation := poLandscape;
  vstInstruments.Print(Printer, true);
end;

procedure TfrmSearchInstruments.aSearchFutureExecute(Sender: TObject);
begin
  SearchChain(stFuture);
end;

procedure TfrmSearchInstruments.aSearchOptionExecute(Sender: TObject);
begin
  SearchChain(stOption);
end;

procedure TfrmSearchInstruments.aShowContractInspectorExecute(Sender: TObject);
resourcestring
  rsFileNotFound = 'File "%s" not found!';
begin
  if TFile.Exists(General.PathToContractInspector) then
    Winapi.ShellAPI.ShellExecute(Handle, nil, PWideChar(General.PathToContractInspector), '', nil, SW_SHOW)
  else
    TMessageDialog.ShowWarning(Format(rsFileNotFound, [General.PathToContractInspector]));
end;

procedure TfrmSearchInstruments.SearchChain(aSecurityType: TIABSecurityType);
var
  Parameters: TOptionalParameters;
  FrameOption: TframeOption;
  FrameFuture: TframeFuture;
  TabSheet: TTabSheet;
  Data: PSokidInfo;
begin
  Parameters.Clear;
  Parameters.SecurityType := aSecurityType;
  if Assigned(vstInstruments.FocusedNode) then
  begin
    Data := vstInstruments.FocusedNode^.GetData;
    Parameters.UnderlyingSecurityType := Data^.SecurityType;
    Parameters.UnderlyingSymbol       := Data^.Symbol;
    Parameters.UnderlyingExchange     := Data^.Exchange;
  end;

  if (TfrmDefOptionalParameter.ShowDocument(Parameters) = mrOk) and (Parameters.UnderlyingConId > 0) then
  begin
    case aSecurityType of
      stFuture:
        begin
          if FFutureFrameList.ContainsKey(Parameters.UnderlyingConId) then
            FrameFuture := FFutureFrameList.Items[Parameters.UnderlyingConId]
          else
          begin
            TabSheet := TTabSheet.Create(pcDerivatives);
            TabSheet.Caption     := 'Future ' + Parameters.Symbol;
            TabSheet.PageControl := pcDerivatives;
            FrameFuture := TframeFuture.Create(TabSheet);
            FrameFuture.Parent          := TabSheet;
            FrameFuture.Align           := alClient;
            FrameFuture.Symbol          := Parameters.Symbol;
            FrameFuture.UnderlyingConId := Parameters.UnderlyingConId;
            FrameFuture.Initialize;
            FFutureFrameList.Add(Parameters.UnderlyingConId, FrameFuture);
          end;
          if Assigned(FrameFuture) then
          begin
            pcDerivatives.ActivePage := TTabSheet(FrameFuture.Parent);
            FrameFuture.Search(Parameters);
          end;
        end;
      stOption:
        begin
          if FOptionFrameList.ContainsKey(Parameters.UnderlyingConId) then
            FrameOption := FOptionFrameList.Items[Parameters.UnderlyingConId]
          else
          begin
            TabSheet := TTabSheet.Create(pcDerivatives);
            TabSheet.Caption     := 'Option ' + Parameters.Symbol;
            TabSheet.PageControl := pcDerivatives;
            FrameOption := TframeOption.Create(TabSheet);
            FrameOption.Parent          := TabSheet;
            FrameOption.Align           := alClient;
            FrameOption.Symbol          := Parameters.Symbol;
            FrameOption.UnderlyingConId := Parameters.UnderlyingConId;
            FrameOption.Initialize;
            FOptionFrameList.Add(Parameters.UnderlyingConId, FrameOption);
          end;
          if Assigned(FrameOption) then
          begin
            pcDerivatives.ActivePage := TTabSheet(FrameOption.Parent);
            FrameOption.Search(Parameters);
          end;
        end;
    end;
    pcBrokersChange(nil);
  end;
end;

procedure TfrmSearchInstruments.pcBrokersChange(Sender: TObject);
begin
  pnlContractDetails.Visible := (pcBrokers.ActivePage = tsInteractiveBroker) and (FOptionFrameList.Count + FFutureFrameList.Count > 0);
  splContractDetails.Visible := pnlContractDetails.Visible;
end;

procedure TfrmSearchInstruments.pmInstrumentsPopup(Sender: TObject);
var
  Data: PSokidInfo;
  Node: PVirtualNode;
begin
  Node := vstInstruments.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if Assigned(Data) and SokidList.ContainsKey(Data^.ContractId) then
      miAddInstrumentToDB.Caption := 'Update Instrument in DB'
    else
      miAddInstrumentToDB.Caption := 'Save Instrument in a DB'
  end;
end;

procedure TfrmSearchInstruments.miOpenSearchConfigurationClick(Sender: TObject);
begin
  LoadParamsFromXml;
end;

procedure TfrmSearchInstruments.vstInstrumentsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PSokidInfo;
begin
  Result := 0;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  if Assigned(Data1) and Assigned(Data2) then
  begin
    case Column of
      COL_ID:
        Result := CompareValue(Data1^.ContractId, Data2^.ContractId);
      COL_EXPIRY:
        Result := CompareValue(Data1^.Expiry, Data2^.Expiry);
      COL_LAST_PRICE:
        Result := CompareValue(Data1^.LastPrice, Data2^.LastPrice);
      COL_ERROR_CODE:
        Result := CompareValue(Data1^.TWSMessageItem.ErrorCode, Data2^.TWSMessageItem.ErrorCode);
      COL_ERROR_MSG:
        Result := CompareText(Data1^.TWSMessageItem.ErrorMsg, Data2^.TWSMessageItem.ErrorMsg);
    else
      Result := CompareText(GetColumnValue(Column, Data1), GetColumnValue(Column, Data2));
    end;
  end;
end;

procedure EnumDataFormats(const DataObj: IDataObject; const FormatList: TStrings);
var
  Enum: IEnumFormatEtc;
  FormatEtc: TFormatEtc;
  Buffer: array [0 .. 255] of Char;
begin
  if DataObj.EnumFormatEtc(DATADIR_GET, Enum) = S_OK then
    while Enum.Next(1, FormatEtc, nil) = S_OK do
    begin
      case FormatEtc.cfFormat of
        CF_TEXT:
          FormatList.Add('CF_TEXT');
        CF_BITMAP:
          FormatList.Add('CF_BITMAP');
        CF_METAFILEPICT:
          FormatList.Add('CF_METAFILEPICT');
        CF_SYLK:
          FormatList.Add('CF_SYLK');
        CF_DIF:
          FormatList.Add('CF_DIF');
        CF_TIFF:
          FormatList.Add('CF_TIFF');
        CF_OEMTEXT:
          FormatList.Add('CF_TIFF');
        CF_DIB:
          FormatList.Add('CF_DIB');
        CF_PALETTE:
          FormatList.Add('CF_PALETTE');
        CF_PENDATA:
          FormatList.Add('CF_PENDATA');
        CF_RIFF:
          FormatList.Add('CF_RIFF');
        CF_WAVE:
          FormatList.Add('CF_WAVE');
        CF_UNICODETEXT:
          FormatList.Add('CF_UNICODETEXT');
        CF_ENHMETAFILE:
          FormatList.Add('CF_ENHMETAFILE');
        CF_HDROP:
          FormatList.Add('CF_HDROP');
        CF_LOCALE:
          FormatList.Add('CF_LOCALE');
      else
        if GetClipBoardFormatName(FormatEtc.cfFormat, Buffer, 255) > 0 then
          FormatList.Add(Buffer);
      end;
    end;
end;

procedure TfrmSearchInstruments.vstInstrumentsDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  Component : TComponent;
  Data: PSokidInfo;
  DataOption: POption;
  i: Integer;
  IsOption: Boolean;
  Node: PVirtualNode;
  SourceTree: TBaseVirtualTree;

  FmtEtc: TFormatEtc;
  StgMed: TStgMedium;
//  pData: PChar;
begin
  if (Sender <> Source) then
  begin
    IsOption := False;
    if (Source is TComponent) then
    begin
      Component := Source as TComponent;
      while Assigned(Component) do
      begin
        if Component is TframeOption then
        begin
          IsOption := True;
          Break;
        end;
        Component := Component.Owner;
      end;
    end;

    if Assigned(Source) and (Source is TVirtualStringTree) then
    begin
      i := 0;
      SourceTree := TVirtualStringTree(Source);
      Node := SourceTree.GetFirstSelected;
      if IsOption then
      begin
        SetLength(FInstruments, SourceTree.SelectedCount);
        while Assigned(Node) do
        begin
          DataOption := Node^.GetData;
          if (DataOption^.ContractId > 0) and SokidList.ContainsKey(DataOption^.ContractId) then
          begin
            SetLength(FInstruments, i + 1);
            FInstruments[i].AssignFrom(SokidList.Items[DataOption^.ContractId]);
            Inc(i);
          end;
          Node := SourceTree.GetNextSelected(Node);
        end;
      end
      else
      begin
        SetLength(FInstruments, SourceTree.SelectedCount);
        while Assigned(Node) do
        begin
          Data := Node^.GetData;
          if not Data.Symbol.IsEmpty then
          begin
            FInstruments[i].AssignFrom(Data^);
            Inc(i);
          end;
          Node := SourceTree.GetNextSelected(Node);
        end;
      end;
    end
    else if Assigned(DataObject) then
    begin
      FmtEtc.cfFormat := CF_UNICODETEXT;
      FmtEtc.ptd := nil;
      FmtEtc.dwAspect := DVASPECT_CONTENT;
      FmtEtc.lindex := -1;
      FmtEtc.tymed := TYMED_ISTREAM;
      OleCheck(DataObject.GetData(FmtEtc, StgMed));
      try
//         Memo1.Lines.Clear;
//        EnumDataFormats(DataObject,Memo1.Lines);
//        pData := GlobalLock(StgMed.hGlobal);
//        Memo1.Text := pData;
      finally
        GlobalUnlock(StgMed.hGlobal);
        ReleaseStgMedium(StgMed);
      end;
    end;
    ShowResultInstrument(FInstruments);
  end;
end;

procedure TfrmSearchInstruments.vstInstrumentsDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TfrmSearchInstruments.vstInstrumentsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if (Data^.TWSMessageItem.ErrorCode > 0) then
    TargetCanvas.Font.Color := clRed;
end;

procedure TfrmSearchInstruments.vstInstrumentsDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
//var
//  Data: PSokidInfo;
begin
  if Assigned(Node) then
  begin
//    Data := Node^.GetData;
    Allowed := True;// (Data^.TWSMessageItem.ErrorCode <= 0);
  end
  else
    Allowed := False;
end;

procedure TfrmSearchInstruments.vstInstrumentsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmSearchInstruments.vstInstrumentsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    CellText := GetColumnValue(Column, Data);
end;

procedure TfrmSearchInstruments.aSearchExecute(Sender: TObject);
var
  BrokerType: TBrokerType;
  Filter: string;
  Request: TIABRequest;
  Order: TIABOrder;
begin
  BrokerType := TBrokerType.brIB;
  case pcBrokers.ActivePageIndex of
    0: // tsInteractiveBroker
      begin
        BrokerType := TBrokerType.brIB;
        Filter := edMatchingSymbol.Text;
      end;
    1: // tsNordNetBroker
      begin
        BrokerType := TBrokerType.brNN;
        if (rgSearchType.ItemIndex = 0) then
          Filter := edSearch.Text
        else
          Filter := cbInstrList.Text;
      end;
    2: // tsTestBroker
      begin
        BrokerType := TBrokerType.brTest;
        Filter := '';
      end;
  end;

  Screen.Cursor := crHourGlass;
  try
    if (BrokerType = TBrokerType.brIB) then
    begin
      if cbSearchFunction.ItemIndex in [1 .. 2] then
      begin
        if not IABClient.Connected then
        begin
          TMessageDialog.ShowWarning('IABClient is not connected!');
          Exit;
        end
      end
      else if Filter.Trim.IsEmpty then
      begin
        TMessageDialog.ShowWarning('Search string is empty!');
        Exit;
      end;

      if (rgActions.ItemIndex = 0) then
        vstInstruments.Clear;
      SetLength(FInstruments, 0);

      case cbSearchFunction.ItemIndex of
        0:
          begin
            GetInstruments(BrokerType, Filter, FInstruments);
            AdditionalFilters(BrokerType, FInstruments);
            ShowResultInstrument(FInstruments);
          end;
        1:
          begin
            Request := Default (TIABRequest);
            Request.Command := ibRequestMatchingSymbols;
            Request.Details := Filter;
            Request.DataID  := C_DATA_ID_SYMBOL;
            IABClient.SendRequest(Request);
          end;
        2:
          if (cbExtSecurityType.ItemIndex >= Ord(Low(TIABSecurityType))) and
             (cbExtSecurityType.ItemIndex <= Ord(High(TIABSecurityType))) then
          begin
            Order := TIABOrder.Create;
            try
              IABClient.ClearOrder(Order);
              Order.Symbol       := cbExtSymbol.Text;
              Order.Exchange     := cbExtExchange.Text;
              Order.Currency     := cbExtCurrency.Text;
              Order.Expiry       := edExtExpiryMonth.Text;
              Order.SecurityType := TIABSecurityType(cbExtSecurityType.ItemIndex);

              Request := Default (TIABRequest);
              Request.Command := ibGetInstrumentSpecs;
              Request.DataID  := C_DATA_ID_CONTRACT_DETAILS;
              Request.Order   := Order;
              if (Order.SecurityType in [stFuture]) then
              begin
                if rbNextRolling.Checked then
                  Request.Details := 'CONTFUT'
                else if rbFuturesCalendar.Checked then
                  Request.Details := 'FUT+CONTFUT';
              end;
              IABClient.SendRequest(Request);
              TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'Search',
                                                      'Symbol='             + Order.Symbol +
                                                      ', Exchange='         + Order.Exchange +
                                                      ', SecurityType='     + Order.SecurityType.ToString +
                                                      ', Currency='         + Order.Currency +
                                                      ', Expiry='           + Order.Expiry +
                                                      ', Futures Calendar=' + Request.Details
                                                      );
            finally
              FreeAndNil(Order);
            end;
          end;
      end;

      if (not string(cbExtSymbol.Text).IsEmpty) and (cbExtSymbol.Items.IndexOf(cbExtSymbol.Text) = -1) then
        cbExtSymbol.Items.Add(cbExtSymbol.Text);
      if (not string(cbExtExchange.Text).IsEmpty) and (cbExtExchange.Items.IndexOf(cbExtExchange.Text) = -1) then
        cbExtExchange.Items.Add(cbExtExchange.Text);
      if (not string(cbExtCurrency.Text).IsEmpty) and (cbExtCurrency.Items.IndexOf(cbExtCurrency.Text) = -1) then
        cbExtCurrency.Items.Add(cbExtCurrency.Text);
    end
    else
    begin
      GetInstruments(BrokerType, Filter, FInstruments);
      AdditionalFilters(BrokerType, FInstruments);
      ShowResultInstrument(FInstruments);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmSearchInstruments.DoSymbolSample(Sender: TObject; DataID, Item, Count: Integer; SymbolDerivative: TIABSymbolDerivativeSpecItem);
var
  sInfo: string;
begin
  if (DataID = C_DATA_ID_SYMBOL) then
  begin
    if (Item = 1) then
      SetLength(FInstruments, Count);
    FInstruments[Item - 1].Broker            := TBrokerType.brIB;
    FInstruments[Item - 1].Currency          := SymbolDerivative.Currency;
    FInstruments[Item - 1].Exchange          := SymbolDerivative.PrimaryExchange;
    FInstruments[Item - 1].ContractId        := SymbolDerivative.ContractId;
    FInstruments[Item - 1].SecurityType      := SymbolDerivative.SecurityType.ToString;
    FInstruments[Item - 1].Name              := SymbolDerivative.Symbol;
    FInstruments[Item - 1].Symbol            := SymbolDerivative.Symbol;
    FInstruments[Item - 1].PrimaryExchange   := SymbolDerivative.PrimaryExchange;
    FInstruments[Item - 1].AdditionalFeature := [afMatchingSymbol];
    FInstruments[Item - 1].LocalSymbol       := SymbolDerivative.Symbol;

    sInfo := 'ContractId='     + SymbolDerivative.ContractId.ToString +
             ', Symbol='       + SymbolDerivative.Symbol +
             ', Currency='     + SymbolDerivative.Currency +
             ', SecurityType=' + SymbolDerivative.SecurityType.ToString +
             ', Exchange='     + SymbolDerivative.PrimaryExchange;
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'DoSymbolSample', sInfo);

    if (Item = Count) then
    begin
//      AdditionalFilters(IBBrokerType, FInstruments);
      ShowResultInstrument(FInstruments);
    end;
  end;
end;

function TfrmSearchInstruments.GetSecurityTypeText: string;
var
  st: TIABSecurityType;
  i: Integer;
begin
  for i := 0 to cbSecurityType.Items.Count - 1 do
    if cbSecurityType.Checked[i] then
    begin
      if Result.IsEmpty then
        Result := cbSecurityType.Items[i].ToUpper
      else
        Result := Result + ',' + cbSecurityType.Items[i].ToUpper;
    end;

  if Result.IsEmpty then
    for st := Low(TIABSecurityType) to High(TIABSecurityType) do
      if st in [stStock, stOption, stFuture, stIndex, stCmdty] then
      begin
        if Result.IsEmpty then
          Result := st.ToString
        else
          Result := Result + ',' + st.ToString;
      end;
end;

procedure TfrmSearchInstruments.cbExtSecurityTypeChange(Sender: TObject);
begin
  rbNextRolling.Enabled     := cbExtSecurityType.ItemIndex = Ord(stFuture);
//  rbFuturesCalendar.Enabled := cbExtSecurityType.ItemIndex = Ord(stFuture);
  edExtExpiryMonth.Enabled  := cbExtSecurityType.ItemIndex = Ord(stFuture);
  if (cbExtSecurityType.ItemIndex <> Ord(stFuture)) and rbNextRolling.Checked then
    rbOnlySecurityType.Checked := True;
end;

procedure TfrmSearchInstruments.cbSearchFunctionChange(Sender: TObject);
begin
  rbTypeSearch.Enabled      := cbSearchFunction.ItemIndex = 0;
  cbUseSecurityType.Enabled := cbSearchFunction.ItemIndex = 0 ;
  cbSecurityType.Enabled    := (cbSearchFunction.ItemIndex = 0) and cbUseSecurityType.Checked;
  gbContractDetails.Visible := cbSearchFunction.ItemIndex = 2;
  if (cbSearchFunction.ItemIndex = 2) then
    edMatchingSymbol.Text := '';
end;

procedure TfrmSearchInstruments.cbUseSecurityTypeClick(Sender: TObject);
begin
  cbSecurityType.Enabled := cbUseSecurityType.Checked;
end;

procedure TfrmSearchInstruments.DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
  begin
    aSearchExecute(btnSearch);
    if (Sender = edMatchingSymbol) then
    begin
      if edMatchingSymbol.Items.IndexOf(edMatchingSymbol.Text) = -1 then
        edMatchingSymbol.Items.Add(edMatchingSymbol.Text);
      edMatchingSymbol.Text := '';
    end;
  end;
end;

procedure TfrmSearchInstruments.rgSearchTypeClick(Sender: TObject);
begin
  edSearch.Visible := (rgSearchType.ItemIndex = 0);
  cbInstrList.Visible := not edSearch.Visible;
end;

procedure TfrmSearchInstruments.aNewListExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PSokidInfo;
  SokidGroup: TSokidGroup;
begin
  SokidGroup.Clear;
  if TfrmEditSokidList.ShowDocument(SokidGroup) then
  begin
    SokidGroup.SaveToDB;
    Node := vstSokidLists.AddChild(nil);
    Data := Node^.GetData;
    Data^.ContractId := SokidGroup.RecordId;
    Data^.Name := SokidGroup.Name;
    Data^.Description := SokidGroup.Description;
    Data^.Symbol := '';
  end;
end;

procedure TfrmSearchInstruments.aSaveAsNewListExecute(Sender: TObject);
var
  Node: PVirtualNode;
  GroupNode: PVirtualNode;
  Data: PSokidInfo;
  CurrentNode: PVirtualNode;
  NewNode: PVirtualNode;
  NewData: PSokidInfo;
  SokidGroup: TSokidGroup;
  arr: TNodeArray;
begin
  CurrentNode := vstSokidLists.FocusedNode;
  if Assigned(CurrentNode) then
  begin
    arr := TTreeDocument.GetNodesList(CurrentNode);
    SokidGroup.Clear;
    if TfrmEditSokidList.ShowDocument(SokidGroup) then
    begin
      SokidGroup.SaveToDB;
      GroupNode := vstSokidLists.AddChild(nil);
      Data := GroupNode^.GetData;
      Data^.ContractId  := SokidGroup.RecordId;
      Data^.Name        := SokidGroup.Name;
      Data^.Description := SokidGroup.Description;
      Data^.Symbol      := '';

      for Node in arr do
        if (Node <> CurrentNode) then
        begin
          Data := Node^.GetData;
          NewNode := vstSokidLists.AddChild(GroupNode);
          NewData := NewNode^.GetData;
          NewData^.AssignFrom(Data^);
          vstSokidLists.IsVisible[NewNode] := True;
          vstSokidLists.InvalidateNode(NewNode);
          TSokidGroupDetail.SaveToDB(NewData^.ContractId, SokidGroup.RecordId, NewNode^.Index);
        end;
      vstSokidLists.FullExpand(GroupNode);
    end;
  end;
end;

procedure TfrmSearchInstruments.aDeleteListExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PSokidInfo;
  ParentData: PSokidInfo;
begin
  if Assigned(vstSokidLists.FocusedNode) then
  begin
    Node := vstSokidLists.FocusedNode;
    Data := Node^.GetData;
    if Data^.Symbol.IsEmpty and (TMessageDialog.ShowQuestion('All nodes be deleted. Continue?') = mrYes)  then
    begin
      TSokidGroup.DeleteFromDB(Data^.ContractId);
      vstSokidLists.DeleteNode(Node);
    end
    else if not Data^.Symbol.IsEmpty then
    begin
      ParentData := Node^.Parent.GetData;
      TSokidGroupDetail.DeleteFromDB(Data^.ContractId, ParentData.ContractId);
      vstSokidLists.DeleteNode(Node);
    end;
  end;
end;

procedure TfrmSearchInstruments.aEditInstrumentsExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PSokidInfo;
begin
  if Assigned(vstInstruments.FocusedNode) then
  begin
    Node := vstInstruments.FocusedNode;
    Data := Node^.GetData;
    if (TfrmEditInstrument.ShowDocument(Data^.ContractId) = mrOk) then
      Data^.AssignFrom(SokidList.Items[Data^.ContractId]);
  end;
end;

procedure TfrmSearchInstruments.aEditListExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PSokidInfo;
  SokidGroup: TSokidGroup;
begin
  if Assigned(vstSokidLists.FocusedNode) then
  begin
    Node := vstSokidLists.FocusedNode;
    Data := Node^.GetData;
    if Data^.Symbol.IsEmpty then
    begin
      SokidGroup.FromDB(Data^.ContractId);
      if TfrmEditSokidList.ShowDocument(SokidGroup) then
      begin
        SokidGroup.SaveToDB;
        Data^.Name := SokidGroup.Name;
        Data^.Description := SokidGroup.Description;
        Data^.Symbol := '';
        vstSokidLists.InvalidateNode(Node);
      end;
    end
    else if (TfrmEditInstrument.ShowDocument(Data^.ContractId) = mrOk) then
      Data^.AssignFrom(SokidList.Items[Data^.ContractId]);
  end;
end;

procedure TfrmSearchInstruments.edtSearchListsChange(Sender: TObject);

  function IsChildrenVisible(const aNode: PVirtualNode): Boolean;
  var
    ChildNode: PVirtualNode;
  begin
    Result := False;
    ChildNode := aNode.FirstChild;
    while Assigned(ChildNode) do
      if vstSokidLists.IsVisible[ChildNode] then
        Exit(True)
      else
        ChildNode := ChildNode.NextSibling;
  end;

var
  Node: PVirtualNode;
  Data: PSokidInfo;
  arr: TNodeArray;
begin
  inherited;
  vstSokidLists.BeginUpdate;
  try
    arr := TTreeDocument.GetNodesList(vstSokidLists.RootNode);
    for Node in arr do //Instruments
      if Assigned(Node) and (Node <> vstSokidLists.RootNode) then
      begin
        Data := Node^.GetData;
        if not Data^.Symbol.IsEmpty then
          vstSokidLists.IsVisible[Node] := string(edtSearchLists.Text).IsEmpty or (Pos(string(edtSearchLists.Text).ToUpper, Data^.Name.ToUpper) > 0);
      end;
    for Node in arr do //Groups
      if Assigned(Node) and (Node <> vstSokidLists.RootNode) then
      begin
        Data := Node^.GetData;
        if Data^.Symbol.IsEmpty then
          vstSokidLists.IsVisible[Node] := string(edtSearchLists.Text).IsEmpty or IsChildrenVisible(Node);
      end;
  finally
    vstSokidLists.EndUpdate;
  end;
end;

procedure TfrmSearchInstruments.btnClearSearchTextClick(Sender: TObject);
begin
  edtSearchLists.Text := '';
end;

procedure TfrmSearchInstruments.LoadSokidLists;
resourcestring
  C_SQL_SELECT_TEXT        = 'SELECT * FROM SOKID_GROUP ORDER BY NAME';
  C_SQL_SELECT_DETAIL_TEXT = 'SELECT * FROM SOKID_GROUP_DETAIL WHERE SOKID_GROUP=:SOKID_GROUP ORDER BY SORT_INDEX';

  procedure LoadSokidListDetail(aParentId: Integer; aParentNode: PVirtualNode);
  var
    Query: TFDQuery;
    Node: PVirtualNode;
    Data: PSokidInfo;
    ContractId: Integer;
  begin
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := C_SQL_SELECT_DETAIL_TEXT;
      try
        Query.ParamByName('SOKID_GROUP').AsInteger := aParentId;
        Query.Prepare;
        Query.Open;
        Query.First;

        while not Query.Eof do
        begin
          ContractId := Query.FieldByName('SOKID_IB').AsInteger;
          if SokidList.ContainsKey(ContractId) then
          begin
            Node := vstSokidLists.AddChild(aParentNode);
            Data := Node^.GetData;
            Data^.AssignFrom(SokidList.Items[ContractId]);
            vstSokidLists.InvalidateNode(Node);
          end;
          Query.Next;
        end;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'FillCategoryTree', E.Message + TDModUtils.GetQueryInfo(Query));
          raise;
        end;
      end;
    finally
      FreeAndNil(Query);
    end;
  end;

var
  Node: PVirtualNode;
  Data: PSokidInfo;
  Query: TFDQuery;
begin
  vstSokidLists.BeginUpdate;
  try
    vstSokidLists.Clear;
    Query := TFDQuery.Create(nil);
    try
      DMod.CheckConnect;
      Query.Connection := DMod.ConnectionStock;
      Query.SQL.Text := C_SQL_SELECT_TEXT;
      try
        Query.Prepare;
        Query.Open;
        Query.First;

        while not Query.Eof do
        begin
          Node := vstSokidLists.AddChild(nil);
          Data := Node^.GetData;
          Data^.ContractId  := Query.FieldByName('ID').AsInteger;
          Data^.Name        := Query.FieldByName('NAME').AsString;
          Data^.Description := Query.FieldByName('DESCRIPTION').AsString;
          Data^.Symbol      := '';
          LoadSokidListDetail(Data^.ContractId, Node);
          Query.Next;
        end;
      except
        on E: Exception do
        begin
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'FillCategoryTree', E.Message + TDModUtils.GetQueryInfo(Query));
          raise;
        end;
      end;
    finally
      FreeAndNil(Query);
    end;
  finally
    vstSokidLists.FullExpand(nil);
    vstSokidLists.EndUpdate;
  end;
end;

procedure TfrmSearchInstruments.vstSokidListsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PSokidInfo;
begin
  Result := 0;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  if Assigned(Data1) and Assigned(Data2) then
    Result := CompareText(GetCategoryColumnValue(Column, Data1), GetCategoryColumnValue(Column, Data2));
end;

procedure TfrmSearchInstruments.vstSokidListsDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  ChildData: PSokidInfo;
  ChildNode: PVirtualNode;
  Component : TComponent;
  DataOption: POption;
  Data: PSokidInfo;
  IsExists: Boolean;
  IsOption: Boolean;
  NewData: PSokidInfo;
  NewNode: PVirtualNode;
  Node: PVirtualNode;
  SourceTree: TBaseVirtualTree;
  TargetData: PSokidInfo;
  TargetNode: PVirtualNode;
  SokidInfo: TSokidInfo;
  attMode: TVTNodeAttachMode;
begin
  if (Sender = Source) then
  begin
    SourceTree := TVirtualStringTree(Source);
    TargetNode := Sender.DropTargetNode;

    if Assigned(TargetNode) then
    begin
      Data := TargetNode^.GetData;

      if Data^.Symbol.IsEmpty then
      begin
        if (TargetNode^.ChildCount = 0) then
          attMode := amAddChildFirst
        else
          attMode := amAddChildLast;
      end
      else if Assigned(TVirtualStringTree(Source).FocusedNode) then
      begin
        if (TargetNode^.Index < TVirtualStringTree(Source).FocusedNode^.Index) then
          attMode := amInsertBefore
        else
          attMode := amInsertAfter;
      end;

      vstSokidLists.BeginUpdate;
      try
        var NodeArray := SourceTree.GetSortedSelection(False);
        for Node in NodeArray do
          if Assigned(Node) then
          begin
            Data := Node^.GetData;
            if not Data^.Symbol.IsEmpty then
            begin
              // find duplicates
              IsExists := False;
              ChildNode := TargetNode.FirstChild;
              while Assigned(ChildNode) do
              begin
                ChildData := ChildNode^.GetData;
                if (ChildData^.ContractId = Data.ContractId) then
                begin
                  IsExists := True;
                  Break;
                end
                else
                  ChildNode := ChildNode.NextSibling;
              end;

              if not IsExists then
                Sender.MoveTo(Node, TargetNode, attMode, False);
            end;
          end;
      finally
        vstSokidLists.FullExpand(TargetNode);
        vstSokidLists.EndUpdate;
      end;
    end;

  end
  else if (Sender <> Source) then
  begin
    IsOption := False;
    if (Source is TComponent) then
    begin
      Component := Source as TComponent;
      while Assigned(Component) do
      begin
        if Component is TframeOption then
        begin
          IsOption := True;
          Break;
        end;
        Component := Component.Owner;
      end;
    end;

    SourceTree := TVirtualStringTree(Source);
    TargetNode := Sender.DropTargetNode;
    if Assigned(TargetNode) then
    begin
      TargetData := TargetNode^.GetData;
      if not TargetData^.Symbol.IsEmpty then
        TargetNode := TargetNode.Parent;

      vstSokidLists.BeginUpdate;
      try
        Node := SourceTree.GetFirstSelected;
        while Assigned(Node) do
        begin
          SokidInfo.Clear;
          if IsOption then
          begin
            DataOption := Node^.GetData;
            if (DataOption^.ContractId > 0) and SokidList.ContainsKey(DataOption^.ContractId) then
              SokidInfo := SokidList.Items[DataOption^.ContractId];
          end
          else
          begin
            Data := Node^.GetData;
            if (Data^.ContractId > 0) then
            begin
              if not SokidList.ContainsKey(Data^.ContractId) then
                SokidList.SetValue(Data^);
              SokidInfo := SokidList.Items[Data^.ContractId];
            end;
          end;

          if (SokidInfo.ContractId <= 0) then
          begin
            Node := SourceTree.GetNextSelected(Node);
            Continue;
          end;

          // find duplicates
          IsExists := False;
          ChildNode := TargetNode.FirstChild;
          while Assigned(ChildNode) do
          begin
            ChildData := ChildNode^.GetData;
            if (ChildData^.ContractId = SokidInfo.ContractId) then
            begin
              IsExists := True;
              Break;
            end
            else
              ChildNode := ChildNode.NextSibling;
          end;

          if not IsExists then
          begin
            NewNode := vstSokidLists.AddChild(TargetNode);
            NewData := NewNode^.GetData;
            NewData^.AssignFrom(SokidInfo);
            vstSokidLists.IsVisible[NewNode] := True;
            vstSokidLists.InvalidateNode(NewNode);
            TSokidGroupDetail.SaveToDB(NewData^.ContractId, TargetData^.ContractId, NewNode^.Index);
          end;
          Node := SourceTree.GetNextSelected(Node);
        end;
      finally
        vstSokidLists.FullExpand(TargetNode);
        vstSokidLists.EndUpdate;
      end;
    end;
  end;
end;

procedure TfrmSearchInstruments.vstSokidListsNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSokidInfo;
  ParentData: PSokidInfo;
  arrNodes: TNodeArray;
begin
  if Assigned(Node^.Parent) and (Node^.Parent <> vstSokidLists.RootNode) then
  begin
    ParentData := Node^.Parent.GetData;
    arrNodes := TTreeDocument.GetNodesList(Node^.Parent);
    for Node in arrNodes do
      if (Node <> vstSokidLists.RootNode) then
      begin
        Data := Node^.GetData;
        if not Data^.Symbol.IsEmpty then
          TSokidGroupDetail.SaveToDB(Data^.ContractId, ParentData.ContractId, Node^.Index);
      end;
  end;
end;

procedure TfrmSearchInstruments.vstSokidListsNodeMoving(Sender: TBaseVirtualTree; Node, Target: PVirtualNode; var Allowed: Boolean);
var
  Data: PSokidInfo;
  ParentData: PSokidInfo;
begin
  if Assigned(Node^.Parent) and (Node^.Parent <> vstSokidLists.RootNode) then
  begin
    Data := Node^.GetData;
    ParentData := Node^.Parent.GetData;
    TSokidGroupDetail.DeleteFromDB(Data^.ContractId, ParentData^.ContractId);
  end;
end;

procedure TfrmSearchInstruments.vstSokidListsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    CellText := GetCategoryColumnValue(Column, Data);
end;

procedure TfrmSearchInstruments.vstSokidListsIncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string; var Result: Integer);
var
  Data: PSokidInfo;
  ColumnIndex: TColumnIndex;
  TextVal: string;
begin
  Data := Node^.GetData;
  if (vstSokidLists.GetNodeLevel(Node) = 1) then
  begin
    ColumnIndex := vstSokidLists.FocusedColumn;
    TextVal := GetCategoryColumnValue(ColumnIndex, Data)
  end
  else
    TextVal := Data^.Name;
  Result := StrLIComp(PChar(SearchText), PChar(TextVal), Min(Length(SearchText), Length(TextVal)));
end;

procedure TfrmSearchInstruments.vstSokidListsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Data^.Symbol.IsEmpty then
    TargetCanvas.Font.Style := Font.Style + [fsBold];
end;

function TfrmSearchInstruments.GetCategoryColumnValue(aColumn: Integer; aData: PSokidInfo): string;
begin
  if aData.Symbol.IsEmpty then
  begin
    case aColumn of
      COL_SL_NAME:
        Result := aData^.Name;
      COL_SL_DESCRIPTION:
        Result := aData^.Description;
    else
      Result := '';
    end;
  end
  else
    case aColumn of
      COL_SL_NAME:
        Result := aData^.Name;
      COL_SL_CONID:
        Result := aData^.ContractId.ToString;
      COL_SL_SYMBOL:
        Result := aData^.Symbol;
      COL_SL_LOCAL_SYMBOL:
        Result := aData^.LocalSymbol;
      COL_SL_CURRENCY:
        Result := aData^.Currency;
      COL_SL_EXCHANGE:
        Result := aData^.Exchange;
      COL_SL_SECURITY_TYPE:
        Result := aData^.SecurityType;
      COL_SL_BROKER:
        Result := aData^.Broker.ToString;
      COL_SL_EXPIRY:
        if (aData^.Expiry > 0) then
          Result := FormatDateTime('DD.MM.YYYY hh:nn', aData^.Expiry)
        else
          Result := '';
      COL_SL_UNDERLYING_CONID:
        if (aData^.UnderlyingConId > 0) then
          Result := aData^.UnderlyingConId.ToString
        else
          Result := '';
      COL_SL_DESCRIPTION:
        Result := aData^.Description;
    else
      Result := '';
    end;
end;

end.
