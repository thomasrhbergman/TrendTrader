unit Edit.OrderIB;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  System.Threading, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Edit.Factor, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.Imaging.pngimage, IABfunctions, IABSocketAPI, Vcl.Samples.Spin, System.Math, System.Actions, Vcl.ActnList,
  DebugWriter, HtmlLib, Utils, System.UITypes, Global.Types, Monitor.Types, Document, MessageDialog, Order.Utils,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const, Monitor.Interfaces, Common.Types, InstrumentList,
  Entity.Sokid, IABFunctions.MarketRules, Publishers.Interfaces, ParametersStore, DaModule, DaImages, Vcl.NumberBox,
  Global.Resources, XmlFiles, VirtualTrees, Publishers, NNfunctions.Types, IABFunctions.Helpers, MonitorTree.Document,
  IABFunctions.MarketData, System.Types, Vcl.Menus;
{$ENDREGION}

type
  TfrmEditOrderIB = class(TFormDocument, IUpdateFeeds, IOrderState)
    aAddCondition: TAction;
    aAddConditionAndFactor: TAction;
    aAddFactor: TAction;
    aAuxPriceCalc: TAction;
    aBuy: TAction;
    aCancelOrder: TAction;
    ActionListOrder: TActionList;
    aLimitPriceCalc: TAction;
    aLmtPriceOffsetCalc: TAction;
    aMakeDefault: TAction;
    aOrderStatus: TAction;
    aSave: TAction;
    aShowGlobalSettings: TAction;
    aTrailStopPriceCalc: TAction;
    btnAddNode: TBitBtn;
    btnAuxPriceCalc: TButton;
    btnBuy: TBitBtn;
    btnCancel: TBitBtn;
    btnCancelOrder: TBitBtn;
    btnLimitPriceCalc: TButton;
    btnLmtPriceOffsetCalc: TButton;
    btnMakeDefault: TBitBtn;
    btnOk: TBitBtn;
    btnReleaseAllOCAOrders: TButton;
    btnShowGlobalSettings: TBitBtn;
    btnStatus: TBitBtn;
    btnTrailStopPriceCalc: TButton;
    cbAdvancedOrderType: TComboBox;
    cbAuxBasePrice: TComboBox;
    cbLimitBasePrice: TComboBox;
    cbLmtOffsetBasePrice: TComboBox;
    cbOrderType: TComboBox;
    cbScope: TComboBox;
    cbTimeInForce: TComboBox;
    cbTrailStopBasePrice: TComboBox;
    cbTriggerMethod: TComboBox;
    chbOrderIsFinal: TCheckBox;
    chbOrderStart: TCheckBox;
    chbOrderStop: TCheckBox;
    dtpDateStart: TDateTimePicker;
    dtpDateStop: TDateTimePicker;
    dtpTimeStart: TDateTimePicker;
    dtpTimeStop: TDateTimePicker;
    edAuxPrice: TNumberBox;
    edAuxPricePerc: TNumberBox;
    edDescription: TEdit;
    edLimit: TNumberBox;
    edLimitPerc: TNumberBox;
    edLmtPriceOffset: TNumberBox;
    edLmtPriceOffsetPerc: TNumberBox;
    edQuantity: TNumberBox;
    edTrailStopPrice: TNumberBox;
    edTrailStopPricePerc: TNumberBox;
    grpRepetitive: TGroupBox;
    lblAdvancedOrderType: TLabel;
    lblAuxPrice: TLabel;
    lblAuxPriceCalc: TLabel;
    lblBuy: TLabel;
    lblConId: TLabel;
    lblConIdCaption: TLabel;
    lblCurrency: TLabel;
    lblCurrencyCaption: TLabel;
    lblDescription: TLabel;
    lblExchange: TLabel;
    lblExchangeCaption: TLabel;
    lblFilled: TLabel;
    lblFilledCaption: TLabel;
    lblLastPrice: TLabel;
    lblLastPriceCaption: TLabel;
    lblLimitPriceCalc: TLabel;
    lblLmtOffset: TLabel;
    lblLmtPrice: TLabel;
    lblLmtPriceOffsetCalc: TLabel;
    lblMaxNumAmount: TLabel;
    lblMaxNumShares: TLabel;
    lblMultiplier: TLabel;
    lblMultiplierCaption: TLabel;
    lblOcaName: TLabel;
    lblOrderIBId: TLabel;
    lblOrderIBIdCaption: TLabel;
    lblOrderType: TLabel;
    lblOrderValue: TLabel;
    lblOrderValueCaption: TLabel;
    lblQuantity: TLabel;
    lblScope: TLabel;
    lblSecureType: TLabel;
    lblSell: TLabel;
    lblStatus: TLabel;
    lblTickSize: TLabel;
    lblTickSizeCaption: TLabel;
    lblTimeInForce: TLabel;
    lblTrailStopPrice: TLabel;
    lblTrailStopPriceCalc: TLabel;
    lblTriggerMethod: TLabel;
    lblType: TLabel;
    lblTypeCaption: TLabel;
    memoInfo: TMemo;
    miAddCondition: TMenuItem;
    miAddConditionandFactor: TMenuItem;
    miAddFactor: TMenuItem;
    pcInfo: TPageControl;
    pmAddNode: TPopupMenu;
    pnlAdvanced: TPanel;
    pnlAuxPrice: TPanel;
    pnlAuxPriceCalc: TPanel;
    pnlBottom: TPanel;
    pnlLimitPrice: TPanel;
    pnlLimitPriceCalc: TPanel;
    pnlLmtOffset: TPanel;
    pnlLmtOffsetCalc: TPanel;
    pnlMain: TPanel;
    pnlOrderOptions: TPanel;
    pnlOrderStart: TPanel;
    pnlOrderStop: TPanel;
    pnlQuantity: TPanel;
    pnlQuantityMult: TPanel;
    pnlTrailStopPrice: TPanel;
    pnlTrailStopPriceCalc: TPanel;
    rbBuy: TRadioButton;
    rbSell: TRadioButton;
    seMaxNumAmount: TSpinEdit;
    seMaxNumShares: TSpinEdit;
    seOcaGroupNumber: TSpinEdit;
    tsOrderInfo: TTabSheet;
    procedure aAuxPriceCalcExecute(Sender: TObject);
    procedure aBuyExecute(Sender: TObject);
    procedure aBuyUpdate(Sender: TObject);
    procedure aCancelOrderExecute(Sender: TObject);
    procedure aCancelOrderUpdate(Sender: TObject);
    procedure aLimitPriceCalcExecute(Sender: TObject);
    procedure aLmtPriceOffsetCalcExecute(Sender: TObject);
    procedure aMakeDefaultExecute(Sender: TObject);
    procedure aOrderStatusExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
    procedure aShowGlobalSettingsExecute(Sender: TObject);
    procedure aTrailStopPriceCalcExecute(Sender: TObject);
    procedure btnReleaseAllOCAOrdersClick(Sender: TObject);
    procedure cbOrderTypeChange(Sender: TObject);
    procedure cbScopeChange(Sender: TObject);
    procedure cbTimeInForceChange(Sender: TObject);
    procedure chbOrderStartClick(Sender: TObject);
    procedure chbOrderStopClick(Sender: TObject);
    procedure edQuantityChangeValue(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbBuyClick(Sender: TObject);
    procedure rbSellClick(Sender: TObject);

    procedure OnBasePriceChange(Sender: TObject);
    procedure OnBasePriceDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure OnPercentChange(Sender: TObject);
    procedure SetModyfied(Sender: TObject);
    procedure aAddFactorExecute(Sender: TObject);
    procedure btnAddNodeClick(Sender: TObject);
    procedure aAddConditionExecute(Sender: TObject);
    procedure aAddConditionAndFactorExecute(Sender: TObject);
  protected
    procedure DoAfterShow(Sender: TObject);
  private
    FAction: TIABAction;
    FColDay: TColor;
    FColTick: TColor;
    FDecimals: Integer;
    FExtendedOptions: TCustomOrderDoc.TExtendedOptions;
    FId: Integer;
    FInfo: string;
    FInstrumentName: string;
    FIsExecuted: Boolean;
    FIsRepetitive: Boolean;
    FLastPrice: Currency;
    FMarketList: string;
    FModyfied : Boolean;
    FMultiplier: Double;
    FOrderIBId: Integer;
    FOrderStatus: TIABOrderState;
    FParametersStore: TParametersStore;
    FParentIBId: Integer;
    FRateType: TRateType;
    FSecurityType: TIABSecurityType;
    FSokidInfo: TSokidInfo;
    FSymbol: string;
    FTickSize: Double;
    FTrailingPercent: Double;
    FTransmit: Boolean;
    function AddCondition(const aParentNode: PVirtualNode): PVirtualNode;
    function AddFactor(const aParentNode: PVirtualNode): PVirtualNode;
    function GetAdvancedOrderType: TAdvancedOrderType;
    function GetAuxPrice: Double;
    function GetAuxPricePerc: Double;
    function GetBasePriceValue(const aBasePrice: TBasePrice): Currency;
    function GetBrokerType: TBrokerType;
    function GetCurrency: string;
    function GetDateStart: TDate;
    function GetDateStop: TDate;
    function GetDescription: string;
    function GetExchange: string;
    function GetExtendedOptions: TCustomOrderDoc.TExtendedOptions;
    function GetFilled: Integer;
    function GetInstrumentName: string;
    function GetIsExecuted: Boolean;
    function GetIsFinal: Boolean;
    function GetLimitPerc: Double;
    function GetLimitPrice: Double;
    function GetLmtPriceOffset: Double;
    function GetLmtPriceOffsetPerc: Double;
    function GetMaxNumAmount: Integer;
    function GetMaxNumShares: Integer;
    function GetModyfied: Boolean;
    function GetOcaGroupNumber: Integer;
    function GetOrderAction: TIABAction;
    function GetOrderStartEnabled: Boolean;
    function GetOrderStatusText: string;
    function GetOrderStopEnabled: Boolean;
    function GetOrderType: TIABOrderType;
    function GetQuantity: Integer;
    function GetScope: Integer;
    function GetTimeInForce: Integer;
    function GetTimeStart: TTime;
    function GetTimeStop: TTime;
    function GetTrailStopPrice: Double;
    function GetTrailStopPricePerc: Double;
    function GetTriggerMethod: TTriggerMethod;
    function GetXmlParams: string;
    procedure AssignToDocMainParams(var aDocument: TOrderIBDoc);
    procedure Buy(aModifyingOrder: Boolean = False);
    procedure ChangeOrderType;
    procedure CheckSecureType;
    procedure NormalizationOrder;
    procedure SetAdvancedOrderType(const Value: TAdvancedOrderType);
    procedure SetAuxPrice(const Value: Double);
    procedure SetAuxPricePerc(const Value: Double);
    procedure SetCurrency(const Value: string);
    procedure SetDateStart(const Value: TDate);
    procedure SetDateStop(const Value: TDate);
    procedure SetDecimals(const Value: Integer);
    procedure SetDescription(const Value: string);
    procedure SetExchange(const Value: string);
    procedure SetExtendedOptions(const Value: TCustomOrderDoc.TExtendedOptions);
    procedure SetFilled(const Value: Integer);
    procedure SetId(const Value: Integer);
    procedure SetInfo(const Value: string);
    procedure SetInstrumentName(const Value: string);
    procedure SetIsFinal(const Value: Boolean);
    procedure SetLimitPerc(const Value: Double);
    procedure SetLimitPrice(const Value: Double);
    procedure SetLmtPriceOffset(const Value: Double);
    procedure SetLmtPriceOffsetPerc(const Value: Double);
    procedure SetMarketList(const Value: string);
    procedure SetMaxNumAmount(const Value: Integer);
    procedure SetMaxNumShares(const Value: Integer);
    procedure SetMultiplier(const Value: Double);
    procedure SetOcaGroupNumber(const Value: Integer);
    procedure SetOrderAction(const Value: TIABAction);
    procedure SetOrderIBId(const Value: Integer);
    procedure SetOrderStartEnabled(const Value: Boolean);
    procedure SetOrderStatus(const Value: TIABOrderState);
    procedure SetOrderStopEnabled(const Value: Boolean);
    procedure SetOrderType(const Value: TIABOrderType);
    procedure SetOrderValue;
    procedure SetQuantity(const Value: Integer);
    procedure SetScope(const Value: Integer);
    procedure SetSecurityType(const Value: TIABSecurityType); overload;
    procedure SetTickSize(const Value: Double);
    procedure SetTimeInForce(const Value: Integer);
    procedure SetTimeStart(const Value: TTime);
    procedure SetTimeStop(const Value: TTime);
    procedure SetTrailStopPrice(const Value: Double);
    procedure SetTrailStopPricePerc(const Value: Double);
    procedure SetTriggerMethod(const Value: TTriggerMethod);
    procedure SetXmlParams(const Value: string);

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    //implementation IOrderState
    procedure OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
    procedure OnOpenOrder(Sender: TObject; Order: TIABOrder);
    procedure OnExecution(Sender: TObject; Order: TIABOrder);
    procedure OnCloseOrder(const aTempId: Integer);
    procedure OnRebuildFromTWS(Sender: TObject);
    procedure OnOpenOrderNN(const aOrderList: array of TOrder);

    procedure LoadParamsFromXml;
    procedure SaveParamsToXml;

    property Modyfied: Boolean read GetModyfied  write FModyfied;
  public
    procedure Initialize;
    procedure Deinitialize;
    procedure DisableButtons;
    procedure AssignFromDoc(const aDocument: TOrderIBDoc);
    procedure AssignToDoc(var aDocument: TOrderIBDoc);
    class function ShowDocument(aDocument: TOrderIBDoc; aEnabledButton: Boolean = True): TModalResult;

    property AdvancedOrderType  : TAdvancedOrderType  read GetAdvancedOrderType  write SetAdvancedOrderType;
    property AuxPrice           : Double              read GetAuxPrice           write SetAuxPrice;
    property AuxPricePerc       : Double              read GetAuxPricePerc       write SetAuxPricePerc;
    property BrokerType         : TBrokerType         read GetBrokerType;
    property ColDay             : TColor              read FColDay               write FColDay;
    property ColTick            : TColor              read FColTick              write FColTick;
    property Currency           : string              read GetCurrency           write SetCurrency;
    property DateStart          : TDate               read GetDateStart          write SetDateStart;
    property DateStop           : TDate               read GetDateStop           write SetDateStop;
    property Decimals           : Integer             read FDecimals             write SetDecimals;
    property Description        : string              read GetDescription        write SetDescription;
    property Exchange           : string              read GetExchange           write SetExchange;
    property Filled             : Integer             read GetFilled             write SetFilled;
    property Id                 : Integer             read FId                   write SetId;
    property Info               : string              read FInfo                 write SetInfo;
    property InstrumentName     : string              read GetInstrumentName     write SetInstrumentName;
    property IsExecuted         : Boolean             read GetIsExecuted         write FIsExecuted;
    property IsRepetitive       : Boolean             read FIsRepetitive         write FIsRepetitive;
    property IsFinal            : Boolean             read GetIsFinal            write SetIsFinal;
    property LastPrice          : Currency            read FLastPrice            write FLastPrice;
    property Limit              : Double              read GetLimitPrice         write SetLimitPrice;
    property LimitPerc          : Double              read GetLimitPerc          write SetLimitPerc;
    property LmtPriceOffset     : Double              read GetLmtPriceOffset     write SetLmtPriceOffset;
    property LmtPriceOffsetPerc : Double              read GetLmtPriceOffsetPerc write SetLmtPriceOffsetPerc;
    property MarketList         : string              read FMarketList           write SetMarketList;
    property MaxNumAmount       : Integer             read GetMaxNumAmount       write SetMaxNumAmount;
    property MaxNumShares       : Integer             read GetMaxNumShares       write SetMaxNumShares;
    property Multiplier         : Double              read FMultiplier           write SetMultiplier;
    property OcaGroupNumber     : Integer             read GetOcaGroupNumber     write SetOcaGroupNumber;
    property OrderAction        : TIABAction          read GetOrderAction        write SetOrderAction;
    property OrderIBId          : Integer             read FOrderIBId            write SetOrderIBId;
    property OrderStartEnabled  : Boolean             read GetOrderStartEnabled  write SetOrderStartEnabled;
    property OrderStatus        : TIABOrderState      read FOrderStatus          write SetOrderStatus;
    property OrderStatusText    : string              read GetOrderStatusText;
    property OrderStopEnabled   : Boolean             read GetOrderStopEnabled   write SetOrderStopEnabled;
    property OrderType          : TIABOrderType       read GetOrderType          write SetOrderType;
    property ParentIBId         : Integer             read FParentIBId           write FParentIBId;
    property Quantity           : Integer             read GetQuantity           write SetQuantity;
    property RateType           : TRateType           read FRateType             write FRateType;
    property Scope              : Integer             read GetScope              write SetScope;
    property SecurityType       : TIABSecurityType    read FSecurityType         write SetSecurityType;
    property Symbol             : string              read FSymbol               write FSymbol;
    property TickSize           : Double              read FTickSize             write SetTickSize;
    property TimeInForce        : Integer             read GetTimeInForce        write SetTimeInForce;
    property TimeStart          : TTime               read GetTimeStart          write SetTimeStart;
    property TimeStop           : TTime               read GetTimeStop           write SetTimeStop;
    property TrailingPercent    : Double              read FTrailingPercent      write FTrailingPercent;
    property TrailStopPrice     : Double              read GetTrailStopPrice     write SetTrailStopPrice;
    property TrailStopPricePerc : Double              read GetTrailStopPricePerc write SetTrailStopPricePerc;
    property Transmit           : Boolean             read FTransmit             write FTransmit;
    property TriggerMethod      : TTriggerMethod      read GetTriggerMethod      write SetTriggerMethod;
    property XmlParams          : string              read GetXmlParams          write SetXmlParams;

    property ExtendedOptions : TCustomOrderDoc.TExtendedOptions read GetExtendedOptions write SetExtendedOptions;
  end;

var
  frmEditOrderIB: TfrmEditOrderIB;

implementation

uses
  Edit.OrderStatus;

{$R *.dfm}

class function TfrmEditOrderIB.ShowDocument(aDocument: TOrderIBDoc; aEnabledButton: Boolean = True): TModalResult;
var
  Monitor: IMonitor;
begin
  Result := mrCancel;
  if Assigned(aDocument) then
  begin
    frmEditOrderIB := TfrmEditOrderIB.Create(nil);
    if not aEnabledButton then
      frmEditOrderIB.DisableButtons;
    try
      frmEditOrderIB.LoadParamsFromXml;
      frmEditOrderIB.AssignFromDoc(aDocument);
      frmEditOrderIB.Initialize;
      if (frmEditOrderIB.ShowModal = mrOk) then
      begin
        Result := mrOk;
        frmEditOrderIB.AssignToDoc(aDocument);
        frmEditOrderIB.Deinitialize;
        if Supports(Application.MainForm, IMonitor, Monitor) then
          TTreeDocument.SetIcon(aDocument.OwnerNode, Monitor.GetMainTree);
      end;
    finally
      FreeAndNil(frmEditOrderIB);
    end;
  end;
end;

procedure TfrmEditOrderIB.Initialize;
begin
//  General.LogPublisher.Write([ltLogWriter, ltLogView], ddEnterMethod, Self, 'Initialize');
  lblOrderIBId.Caption := OrderIBId.ToString;
  ChangeOrderType;
  cbTimeInForceChange(nil);
  if (OrderStatus in [osFilled, osCancelled, osPendCancel]) then
    aSave.Caption := 'Duplicate Order'
  else if IsExecuted then
    aSave.Caption := 'Send modified';
  edQuantityChangeValue(nil);
  OnBasePriceChange(cbAuxBasePrice);
  OnBasePriceChange(cbLimitBasePrice);
  OnBasePriceChange(cbLmtOffsetBasePrice);
  OnBasePriceChange(cbTrailStopBasePrice);
//  General.LogPublisher.Write([ltLogWriter, ltLogView], ddExitMethod, Self, 'Initialize');
end;

procedure TfrmEditOrderIB.Deinitialize;
begin
  FParametersStore.IdentityName := 'EditOrderIB-' + OrderType.ToString + '-' + OrderAction.ToString;
  FParametersStore.Store;
end;

procedure TfrmEditOrderIB.FormCreate(Sender: TObject);
var
  ItemIndex: Integer;
begin
  inherited;
//  General.LogPublisher.Write([ltLogWriter, ltLogView], ddEnterMethod, Self, 'FormCreate');
  Self.OnAfterShow := DoAfterShow;
  TPublishers.FeedPublisher.Subscribe(Self);
  TPublishers.OrderStatePublisher.Subscribe(Self);

  for var ot := otMarket to otRelMktCombo do
    cbOrderType.Items.Add(ot.ToString);

  for var tf := System.Low(TIABTimeInForce) to System.High(TIABTimeInForce) do
    cbTimeInForce.Items.Add(tf.ToString);

  for var at := System.Low(TAdvancedOrderType) to System.High(TAdvancedOrderType) do
    cbAdvancedOrderType.Items.Add(at.ToString);

  for var tr := System.Low(TTriggerMethod) to System.High(TTriggerMethod) do
    if not tr.ToString.IsEmpty then
      cbTriggerMethod.Items.AddObject(tr.ToString, TObject(tr));

  ItemIndex := -1;
  cbAuxBasePrice.Items.BeginUpdate;
  cbLimitBasePrice.Items.BeginUpdate;
  cbLmtOffsetBasePrice.Items.BeginUpdate;
  cbTrailStopBasePrice.Items.BeginUpdate;
  try
    cbAuxBasePrice.Clear;
    cbLimitBasePrice.Clear;
    cbLmtOffsetBasePrice.Clear;
    cbTrailStopBasePrice.Clear;
    for var BasePrice := High(TBasePrice) downto Low(TBasePrice) do
      case BasePrice of
        pbLast:
          begin
            ItemIndex := cbAuxBasePrice.Items.AddObject('LAST', TObject(Ord(BasePrice)));
            cbLimitBasePrice.Items.AddObject('LAST', TObject(Ord(BasePrice)));
            cbLmtOffsetBasePrice.Items.AddObject('LAST', TObject(Ord(BasePrice)));
            cbTrailStopBasePrice.Items.AddObject('LAST', TObject(Ord(BasePrice)));
          end;
        pbMid:
          begin
            cbAuxBasePrice.Items.AddObject('MID', TObject(Ord(BasePrice)));
            cbLimitBasePrice.Items.AddObject('MID', TObject(Ord(BasePrice)));
            cbLmtOffsetBasePrice.Items.AddObject('MID', TObject(Ord(BasePrice)));
            cbTrailStopBasePrice.Items.AddObject('MID', TObject(Ord(BasePrice)));
          end;
        pbHigh:
          begin
            cbAuxBasePrice.Items.AddObject('HIGH', TObject(Ord(BasePrice)));
            cbLimitBasePrice.Items.AddObject('HIGH', TObject(Ord(BasePrice)));
            cbLmtOffsetBasePrice.Items.AddObject('HIGH', TObject(Ord(BasePrice)));
            cbTrailStopBasePrice.Items.AddObject('HIGH', TObject(Ord(BasePrice)));
          end;
        pbLow:
          begin
            cbAuxBasePrice.Items.AddObject('LOW', TObject(Ord(BasePrice)));
            cbLimitBasePrice.Items.AddObject('LOW', TObject(Ord(BasePrice)));
            cbLmtOffsetBasePrice.Items.AddObject('LOW', TObject(Ord(BasePrice)));
            cbTrailStopBasePrice.Items.AddObject('LOW', TObject(Ord(BasePrice)));
          end;
        pbAsk:
          begin
            cbAuxBasePrice.Items.AddObject('ASK', TObject(Ord(BasePrice)));
            cbLimitBasePrice.Items.AddObject('ASK', TObject(Ord(BasePrice)));
            cbLmtOffsetBasePrice.Items.AddObject('ASK', TObject(Ord(BasePrice)));
            cbTrailStopBasePrice.Items.AddObject('ASK', TObject(Ord(BasePrice)));
          end;
        pbAsk1 .. pbAsk10:
          begin
            cbAuxBasePrice.Items.AddObject('ASK+' + Abs(Ord(BasePrice) - 100).ToString, TObject(Ord(BasePrice)));
            cbLimitBasePrice.Items.AddObject('ASK+' + Abs(Ord(BasePrice) - 100).ToString, TObject(Ord(BasePrice)));
            cbLmtOffsetBasePrice.Items.AddObject('ASK+' + Abs(Ord(BasePrice) - 100).ToString, TObject(Ord(BasePrice)));
            cbTrailStopBasePrice.Items.AddObject('ASK+' + Abs(Ord(BasePrice) - 100).ToString, TObject(Ord(BasePrice)));
          end;
        pbBid:
          begin
            cbAuxBasePrice.Items.AddObject('BID', TObject(Ord(BasePrice)));
            cbLimitBasePrice.Items.AddObject('BID', TObject(Ord(BasePrice)));
            cbLmtOffsetBasePrice.Items.AddObject('BID', TObject(Ord(BasePrice)));
            cbTrailStopBasePrice.Items.AddObject('BID', TObject(Ord(BasePrice)));
          end;
        pbBid10 .. pbBid1:
          begin
            cbAuxBasePrice.Items.AddObject('BID-' + Abs(Ord(BasePrice) + 100).ToString, TObject(Ord(BasePrice)));
            cbLimitBasePrice.Items.AddObject('BID-' + Abs(Ord(BasePrice) + 100).ToString, TObject(Ord(BasePrice)));
            cbLmtOffsetBasePrice.Items.AddObject('BID-' + Abs(Ord(BasePrice) + 100).ToString, TObject(Ord(BasePrice)));
            cbTrailStopBasePrice.Items.AddObject('BID-' + Abs(Ord(BasePrice) + 100).ToString, TObject(Ord(BasePrice)));
          end;
      end;
    cbAuxBasePrice.ItemIndex       := ItemIndex;
    cbLimitBasePrice.ItemIndex     := ItemIndex;
    cbLmtOffsetBasePrice.ItemIndex := ItemIndex;
    cbTrailStopBasePrice.ItemIndex := ItemIndex;
  finally
    cbAuxBasePrice.Items.EndUpdate;
    cbLimitBasePrice.Items.EndUpdate;
    cbLmtOffsetBasePrice.Items.EndUpdate;
    cbTrailStopBasePrice.Items.EndUpdate;
  end;

  IsExecuted             := False;
  ColDay                 := clBlack;
  seOcaGroupNumber.Value := -1;
  pnlOrderStart.Visible  := False;

  Decimals          := 2;
  FModyfied         := False;
  IsRepetitive      := False;
  OrderAction       := iabBuy;
  OrderStatus       := osSleeping;
  ParentIBId        := -1;
  AdvancedOrderType := atNone;
  OrderType         := otMarket;
  TimeInForce       := Integer(System.Low(TIABTimeInForce));
  Transmit          := False;
  Scope             := TOrderIBDoc.C_SCOPE_UNKNOW;
  FSokidInfo        := Default(TSokidInfo);

  FParametersStore := TParametersStore.Create;
  DMod.CheckConnect;
  FParametersStore.Connection := DMod.ConnectionStock;
  FParametersStore.StoreComponent := Self;
  FParametersStore.PropertiesList.Add('AuxPricePerc');
  FParametersStore.PropertiesList.Add('LimitPerc');
  FParametersStore.PropertiesList.Add('LmtPriceOffsetPerc');
  FParametersStore.PropertiesList.Add('TrailStopPricePerc');
//  General.LogPublisher.Write([ltLogWriter, ltLogView], ddExitMethod, Self, 'FormCreate');
end;

procedure TfrmEditOrderIB.FormDestroy(Sender: TObject);
begin
  TPublishers.FeedPublisher.Unsubscribe(Self);
  TPublishers.OrderStatePublisher.Unsubscribe(Self);
  FreeAndNil(FParametersStore);
  inherited;
end;

procedure TfrmEditOrderIB.FormShow(Sender: TObject);
var
  Data: PTreeData;
begin
  pnlAuxPrice.Top       := 200;
  pnlLmtOffset.Top      := 200;
  pnlTrailStopPrice.Top := 200;
  pnlLimitPrice.Top     := 200;
  inherited;

  if Assigned(OwnerNode) then
  begin
    Data := OwnerNode.GetData;
    Self.Caption := Self.Caption + ' NodeID: ' + Data.NodeId.ToString;
  end;
end;

procedure TfrmEditOrderIB.DoAfterShow(Sender: TObject);
begin
  if (FSokidInfo.TWSMessageItem.ErrorCode > 0) then
    TMessageDialog.ShowWarning(Format(rsInstrumentValidationError, [FSokidInfo.TWSMessageItem.ErrorMsg]));
end;

procedure TfrmEditOrderIB.rbBuyClick(Sender: TObject);
begin
  OrderAction := iabBuy;
  OnPercentChange(nil);
end;

procedure TfrmEditOrderIB.rbSellClick(Sender: TObject);
begin
  OrderAction := iabSell;
  OnPercentChange(nil);
end;

procedure TfrmEditOrderIB.LoadParamsFromXml;
begin
  cbOrderType.ItemIndex   := General.XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_ORDER_TYPE, -1);
  cbTimeInForce.ItemIndex := General.XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_TIME_IN_FORCE, -1);
  cbScope.ItemIndex       := General.XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_SCOPE_OF_ORDER, TOrderIBDoc.C_SCOPE_UNKNOW);
end;

procedure TfrmEditOrderIB.SaveParamsToXml;
begin
  try
    General.XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_ORDER_TYPE, Integer(OrderType));
    General.XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_TIME_IN_FORCE, cbTimeInForce.ItemIndex);
    General.XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_SCOPE_OF_ORDER, cbScope.ItemIndex);
  finally
    General.XMLFile.Save;
  end;
end;

function TfrmEditOrderIB.GetXmlParams: string;
var
  XMLFile: TXMLFile;
begin
  XMLFile := TXMLFile.Create;
  try
    XMLFile.WriteFloat(TOrderIBDoc.C_SECTION_ORDER, 'AuxPricePerc', AuxPricePerc);
    XMLFile.WriteFloat(TOrderIBDoc.C_SECTION_ORDER, 'LimitPerc', LimitPerc);
    XMLFile.WriteFloat(TOrderIBDoc.C_SECTION_ORDER, 'TrailStopPricePerc', TrailStopPricePerc);
    XMLFile.WriteFloat(TOrderIBDoc.C_SECTION_ORDER, 'LmtPriceOffsetPerc', LmtPriceOffsetPerc);
    Result := XMLFile.XMLText;
  finally
    FreeAndNil(XMLFile);
  end;
end;

procedure TfrmEditOrderIB.SetXmlParams(const Value: string);
var
  XMLFile: TXMLFile;
  Perc: Double;
begin
//  General.LogPublisher.Write([ltLogWriter, ltLogView], ddEnterMethod, Self, 'SetXmlParams');
  if not Value.IsEmpty then
  begin
    XMLFile := TXMLFile.Create;
    try
      XMLFile.XMLText := Value;
      Perc := XMLFile.ReadFloat(TOrderIBDoc.C_SECTION_ORDER, 'AuxPricePerc', 0);
      if (Perc <> 0) then
        AuxPricePerc := Perc;
      Perc := XMLFile.ReadFloat(TOrderIBDoc.C_SECTION_ORDER, 'LimitPerc', 0);
      if (Perc <> 0) then
        LimitPerc := Perc;
      Perc := XMLFile.ReadFloat(TOrderIBDoc.C_SECTION_ORDER, 'TrailStopPricePerc', 0);
      if (Perc <> 0) then
        TrailStopPricePerc := Perc;
      Perc := XMLFile.ReadFloat(TOrderIBDoc.C_SECTION_ORDER, 'LmtPriceOffsetPerc', 0);
      if (Perc <> 0) then
        LmtPriceOffsetPerc := Perc;
    finally
      FreeAndNil(XMLFile);
    end;
  end;
//  General.LogPublisher.Write([ltLogWriter, ltLogView], ddExitMethod, Self, 'SetXmlParams');
end;

procedure TfrmEditOrderIB.aBuyExecute(Sender: TObject);
begin
  inherited;
  Buy;
end;

procedure TfrmEditOrderIB.aBuyUpdate(Sender: TObject);
begin
  inherited;
  case OrderAction of
    iabSell:
      begin
        TAction(Sender).Caption := 'Sell Now';
        btnBuy.Font.Color := clMaroon;
      end;
    iabBuy:
      begin
        TAction(Sender).Caption := 'Buy Now';
        btnBuy.Font.Color := clGreen;
      end;
  end;
  TAction(Sender).Visible := not IsExecuted;
end;

procedure TfrmEditOrderIB.aCancelOrderExecute(Sender: TObject);
var
  Data: PTreeData;
begin
  inherited;
  if Assigned(OwnerNode) then
  begin
    Data := OwnerNode^.GetData;
    if Assigned(Data) and Assigned(Data.OrderDoc) then
    begin
      Data.OrderDoc.CancelOrder;
      OrderStatus := osPendCancel;
    end;
  end;
end;

procedure TfrmEditOrderIB.aCancelOrderUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not (OrderStatus in [osFilled]);
  TAction(Sender).Visible := IsExecuted;
end;

procedure TfrmEditOrderIB.aAddConditionAndFactorExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  inherited;
  Node := AddCondition(Self.OwnerNode);
  if Assigned(Node) then
    AddFactor(Node);
end;

procedure TfrmEditOrderIB.aAddConditionExecute(Sender: TObject);
begin
  inherited;
  AddCondition(Self.OwnerNode);
end;

procedure TfrmEditOrderIB.aAddFactorExecute(Sender: TObject);
begin
  inherited;
  AddFactor(Self.OwnerNode);
end;

function TfrmEditOrderIB.AddFactor(const aParentNode: PVirtualNode): PVirtualNode;
var
  Monitor: IMonitor;
  Data: PTreeData;
  Tree: TVirtualStringTree;
  NewNode: PVirtualNode;
  SokidInfo: TSokidInfo;
begin
  Result := nil;
  if Assigned(aParentNode) and Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    SokidInfo := SokidList.GetItem(Self.Id);
    if not SokidInfo.Symbol.IsEmpty then
    begin
      Tree := Monitor.GetMainTree;
      NewNode := TTreeDocument.CreateFactor(aParentNode, Tree);
      Data := NewNode^.GetData;
      Data^.FactorDoc.TickType1       := ttLast;
      Data^.FactorDoc.TickType2       := ttNotSet;
      Data^.FactorDoc.ContractId      := SokidInfo.ContractId;
      Data^.FactorDoc.InstrumentName  := SokidInfo.Name;
      Data^.FactorDoc.Currency        := SokidInfo.Currency;
      Data^.FactorDoc.Exchange        := SokidInfo.Exchange;
      Data^.FactorDoc.PrimaryExchange := SokidInfo.PrimaryExchange;
      Data^.FactorDoc.ContractType    := SokidInfo.SecurityType;
      Data^.FactorDoc.Symbol          := SokidInfo.Symbol;
      Data^.FactorDoc.IsIn            := SokidInfo.IsIn;
      Data^.FactorDoc.Expiry          := SokidInfo.Expiry;
      Data^.FactorDoc.BrokerType      := SokidInfo.Broker;
      Data^.FactorDoc.UseInAutoOrder  := False;
      Data^.FactorDoc.CurrentValue    := TPriceCache.PriceCache.GetLastPrice(Data^.FactorDoc.ContractId, ttLast);
      TIABMarket.RequestMarketData(Data^.FactorDoc.ContractId);
      TMonitorLists.InstrumentList.AddNode(Data^.FactorDoc.ContractId, NewNode);
      TTreeDocument.SetIcon(NewNode, Tree);
      Tree.InvalidateNode(NewNode);
      Result := NewNode;
    end;
  end;
end;

function TfrmEditOrderIB.AddCondition(const aParentNode: PVirtualNode): PVirtualNode;
var
  Monitor: IMonitor;
  Data: PTreeData;
  Tree: TVirtualStringTree;
  NewNode: PVirtualNode;
begin
  Result := nil;
  if Assigned(aParentNode) and Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    Tree := Monitor.GetMainTree;
    NewNode := TTreeDocument.CreateCondition(aParentNode, Tree);
    Data := NewNode^.GetData;
    Data.ConditionDoc.CondType := ctRealtimeValue;
    TTreeDocument.SetIcon(NewNode, Tree);
    Tree.InvalidateNode(NewNode);
    Result := NewNode;
  end;
end;

procedure TfrmEditOrderIB.aAuxPriceCalcExecute(Sender: TObject);
var
  Price: Double;
begin
  inherited;
  Price := StrToFloatDef(lblAuxPriceCalc.Caption, FLastPrice);
  case OrderType of
    otTrailLimit, otTrail:
      AuxPrice := IABClient.MarketRuleList.RoundToMinTick(Abs(AuxPricePerc / 100) * Price, FSokidInfo.MarketRuleIds, FSokidInfo.MinimumTick);
    else
      AuxPrice := IABClient.MarketRuleList.RoundToMinTick((1 + AuxPricePerc / 100) * Price, FSokidInfo.MarketRuleIds, FSokidInfo.MinimumTick);
  end;
end;

procedure TfrmEditOrderIB.aLmtPriceOffsetCalcExecute(Sender: TObject);
var
  Price: Double;
begin
  inherited;
  Price := StrToFloatDef(lblLmtPriceOffsetCalc.Caption, FLastPrice);
  LmtPriceOffset := IABClient.MarketRuleList.RoundToMinTick(Abs(LmtPriceOffsetPerc / 100) * Price, FSokidInfo.MarketRuleIds, FSokidInfo.MinimumTick);
end;

procedure TfrmEditOrderIB.aLimitPriceCalcExecute(Sender: TObject);
var
  Price: Double;
begin
  inherited;
  Price := StrToFloatDef(lblLimitPriceCalc.Caption, FLastPrice);
  Limit := IABClient.MarketRuleList.RoundToMinTick((1 + LimitPerc / 100) * Price, FSokidInfo.MarketRuleIds, FSokidInfo.MinimumTick);
end;

procedure TfrmEditOrderIB.aTrailStopPriceCalcExecute(Sender: TObject);
var
  Price: Double;
begin
  inherited;
  Price := StrToFloatDef(lblTrailStopPriceCalc.Caption, FLastPrice);
  case OrderType of
    otTrailLimit, otTrail:
      TrailStopPrice := IABClient.MarketRuleList.RoundToMinTick((1 + TrailStopPricePerc / 100) * Price, FSokidInfo.MarketRuleIds, FSokidInfo.MinimumTick);
    else
      TrailStopPrice := IABClient.MarketRuleList.RoundToMinTick((TrailStopPricePerc / 100) * Price, FSokidInfo.MarketRuleIds, FSokidInfo.MinimumTick);
  end;
end;

procedure TfrmEditOrderIB.aMakeDefaultExecute(Sender: TObject);
begin
  inherited;
  SaveParamsToXml;
end;

procedure TfrmEditOrderIB.aOrderStatusExecute(Sender: TObject);
begin
  inherited;
  if Assigned(OwnerNode) then
    TfrmEditOrderStatus.ShowDocument(OwnerNode)
  else
    TMessageDialog.ShowWarning('Order not assigned');
end;

procedure TfrmEditOrderIB.aSaveExecute(Sender: TObject);
var
  NewNode: PVirtualNode;
  Data: PTreeData;
  Monitor: IMonitor;
  PrecSettings: TPrecautionarySettingTypes;
begin
  inherited;
  if Modyfied then
  begin
    PrecSettings := TOrderUtils.CheckPrecautionarySettings(SecurityType, Quantity, FLastPrice);
    for var PrecSetting in PrecSettings do
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
          if TMessageDialog.ShowQuestion(Format(rsPrecautionaryQuantity, [General.PrecautionarySettings[SecurityType][psOrderQuantityMax]])) = mrYes then
            Quantity := Trunc(General.PrecautionarySettings[SecurityType][psOrderQuantityMax]);
        psMaxAllowedPrice:
          TMessageDialog.ShowWarning(Format(rsPrecautionaryMaxAllowedPrice, [General.PrecautionarySettings[SecurityType][psMaxAllowedPrice]]));
        psMinAllowedPrice:
          TMessageDialog.ShowWarning(Format(rsPrecautionaryMinAllowedPrice, [General.PrecautionarySettings[SecurityType][psMinAllowedPrice]]));
      end;

    if (OrderStatus in [osFilled, osCancelled, osPendCancel]) then
    begin
      if (TMessageDialog.ShowQuestion('Order has been disabled. Duplicate a order?') = mrYes) and
          Supports(Application.MainForm, IMonitor, Monitor) then
      begin
        NewNode := Monitor.GetDuplicateOrderIB(OwnerNode);
        if Assigned(NewNode) then
        begin
          Data := NewNode^.GetData;
          if Assigned(Data) then
            AssignToDocMainParams(TOrderIBDoc(Data.OrderDoc));
        end;
      end;
    end
    else
    begin
      if IsExecuted then
      begin
        IsExecuted := False;
        Buy(Modyfied);
      end
      else if (OrderStatus in [osSleeping]) then
        ModalResult := mrOk;
    end;
  end;
  Modyfied := False;
end;

procedure TfrmEditOrderIB.aSaveUpdate(Sender: TObject);
var
  Data: PTreeData;
begin
  inherited;
  if Assigned(OwnerNode) then
  begin
    Data := OwnerNode^.GetData;
    TAction(Sender).Enabled := (Data.CreationType = ctUser) and (Quantity > 0);
  end
  else
    TAction(Sender).Enabled := (OrderStatus in [osSleeping]) and (Quantity > 0);
end;

procedure TfrmEditOrderIB.aShowGlobalSettingsExecute(Sender: TObject);
begin
  inherited;
  General.ShowParametersTab(TfrmParameters.C_TAB_PRECAUTIONARY_SETTINGS);
end;

procedure TfrmEditOrderIB.OnBasePriceDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ComboBox: TComboBox absolute Control;
  Canvas: TCanvas;
  BasePrice: TBasePrice;
begin
//  General.LogPublisher.Write([ltLogWriter, ltLogView], ddEnterMethod, Self, 'OnBasePricesDrawItem');
  inherited;
  Canvas := ComboBox.Canvas;
  Canvas.Brush.Color := clWhite;
  Canvas.Font.Color  := clBlack;

  BasePrice := TBasePrice(ComboBox.Items.Objects[Index]);
  if (BasePrice = pbAsk) then
  begin
    Canvas.Font.Color := clMaroon;
    Canvas.Font.Style := [TFontStyle.fsBold];
  end
  else if (BasePrice = pbBid) then
  begin
    Canvas.Font.Color := clNavy;
    Canvas.Font.Style := [TFontStyle.fsBold];
  end
  else if (BasePrice = pbMid) then
  begin
    Canvas.Font.Color := clGray;
    Canvas.Font.Style := [TFontStyle.fsBold];
  end
  else if (BasePrice in [pbLast, pbHigh, pbLow]) then
  begin
    Canvas.Font.Color := clBlack;
    Canvas.Font.Style := [TFontStyle.fsBold];
  end;

  Canvas.FillRect(Rect);
  Canvas.TextOut(Rect.Left, Rect.Top, ComboBox.Items[Index]);
  if odFocused in State then
    Canvas.DrawFocusRect(Rect);
//  General.LogPublisher.Write([ltLogWriter, ltLogView], ddExitMethod, Self, 'OnBasePricesDrawItem');
end;

procedure TfrmEditOrderIB.cbOrderTypeChange(Sender: TObject);
begin
  if Showing then
  begin
    CheckSecureType;
    ChangeOrderType;
  end;
end;

procedure TfrmEditOrderIB.ChangeOrderType;

  procedure EnableControl(aControl: TWinControl; aValue: Boolean);
  var
    i: Integer;
  begin
    aControl.Enabled := aValue;
    for i := 0 to aControl.ControlCount - 1 do
      if (aControl.Controls[i] is TWinControl) then
        EnableControl(TWinControl(aControl.Controls[i]), aValue);
  end;

begin
  pnlLimitPrice.Visible     := OrderType in [otLimit, otStopLimit, otTrailLimit];
  pnlAuxPrice.Visible       := OrderType in [otTrail, otTrailLimit, otStop, otStopLimit];
  pnlTrailStopPrice.Visible := OrderType in [otTrail, otTrailLimit];
  pnlLmtOffset.Visible      := OrderType in [otTrailLimit];

  seOcaGroupNumber.Enabled       := (cbScope.ItemIndex = TOrderIBDoc.C_SCOPE_ONE_CANCELS_ALL_OCA);
  btnReleaseAllOCAOrders.Enabled := seOcaGroupNumber.Enabled;
  lblSecureType.Caption          := 'SecureType:   ' + OrderType.GetOrderProduct;
  pnlLimitPrice.Enabled          := True;
  EnableControl(pnlLimitPrice, True);

  case OrderType of
    otStop, otStopLimit:
      begin
        lblAuxPrice.Caption := 'Stop Price:';
      end;
    otMarketOpen:
      begin
        TimeInForce := Ord(tifOPG);
      end;
    otPegPrimary:
      begin
        lblAuxPrice.Caption := 'Offset Amount:';
        lblLmtPrice.Caption := 'Price Cap:';
      end;
    otLimit:
      begin

      end;
    otTrailLimit, otTrail:
      begin
        btnAuxPriceCalc.Caption       := 'Calc Amount';
        btnTrailStopPriceCalc.Caption := 'Calc Price';
        lblAuxPrice.Caption           := 'Trail Amount:';
        lblTrailStopPrice.Caption     := 'Trail Stop Price:';
        EnableControl(pnlLimitPrice, False);
      end;
    otPegMidPt:
      begin

      end;
  else
    begin
      btnAuxPriceCalc.Caption       := 'Calc Price';
      btnTrailStopPriceCalc.Caption := 'Calc Amount';
      lblAuxPrice.Caption           := 'Price:';
      lblLmtPrice.Caption           := 'Limit Price:';
    end;
  end;
  pnlAuxPrice.Top       := 200;
  pnlLmtOffset.Top      := 200;
  pnlTrailStopPrice.Top := 200;
  pnlLimitPrice.Top     := 200;

  FParametersStore.IdentityName := 'EditOrderIB-' + OrderType.ToString + '-' + OrderAction.ToString;
  FParametersStore.Restore;
  OnPercentChange(nil);
  Modyfied := True;
end;

procedure TfrmEditOrderIB.cbScopeChange(Sender: TObject);
begin
  if Showing then
  begin
    btnReleaseAllOCAOrders.Enabled := cbScope.ItemIndex = TOrderIBDoc.C_SCOPE_ONE_CANCELS_ALL_OCA;
    seOcaGroupNumber.Enabled       := cbScope.ItemIndex = TOrderIBDoc.C_SCOPE_ONE_CANCELS_ALL_OCA;
    Modyfied := True;
  end;
end;

procedure TfrmEditOrderIB.CheckSecureType;
resourcestring
  rsNotSupported = 'Ordertype not supported for this securitype';
begin
  if not OrderType.GetOrderProduct.Contains(SecurityType.ToString) then
    TMessageDialog.ShowWarning(rsNotSupported);
end;

procedure TfrmEditOrderIB.cbTimeInForceChange(Sender: TObject);
begin
  chbOrderStart.Visible := (cbTimeInForce.ItemIndex = 4);
  chbOrderStop.Visible  := (cbTimeInForce.ItemIndex = 4);
  pnlOrderStart.Visible := chbOrderStart.Visible;
  pnlOrderStart.Visible := chbOrderStop.Visible;
  if Showing and (cbTimeInForce.ItemIndex = Ord(TIABTimeInForce.tifIOC)) then
    cbScope.ItemIndex := TOrderIBDoc.C_SCOPE_FILL_OR_KILL;
  Modyfied := True;
end;

procedure TfrmEditOrderIB.chbOrderStartClick(Sender: TObject);
begin
  pnlOrderStart.Visible := chbOrderStart.Checked;
end;

procedure TfrmEditOrderIB.chbOrderStopClick(Sender: TObject);
begin
  pnlOrderStop.Visible := chbOrderStop.Checked;
end;

procedure TfrmEditOrderIB.Buy(aModifyingOrder: Boolean = False);
resourcestring
  rsSelectOrdertype = 'Please select ordertype';
var
  Data: PTreeData;
  Monitor: IMonitor;
begin
  if Modyfied then
    Modyfied := False;

  if (cbOrderType.ItemIndex < 0) then
  begin
    TMessageDialog.ShowWarning(rsSelectOrdertype);
    Exit;
  end;

  if (OrderStatus in [osFilled, osCancelled, osPendCancel, osPendSubmit]) then
    Exit;

//  if (not IsExecuted) then
  begin
//    General.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'Buy');
    if Supports(Application.MainForm, IMonitor, Monitor) then
      Monitor.SetChildsFreeze(OwnerNode);
    Self.Caption := Self.Caption + ' [Running]';
    if Assigned(OwnerNode) then
    begin
      Data := OwnerNode^.GetData;
      if Assigned(Data) and Assigned(Data.OrderDoc) then
      begin
        NormalizationOrder;
        AssignToDocMainParams(TOrderIBDoc(Data.OrderDoc));
        if aModifyingOrder then
          Data.OrderDoc.UpdateOrder
        else
          Data.OrderDoc.Buy;
        if (Data.OrderDoc.OrderIBId > 0) then
          ModalResult := mrClose;
      end;
    end;
  end;
//  General.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'Buy');
end;

procedure TfrmEditOrderIB.btnAddNodeClick(Sender: TObject);
begin
  with btnAddNode.ClientToScreen(Point(0, btnAddNode.Height)) do
    pmAddNode.Popup(X, Y);
end;


procedure TfrmEditOrderIB.btnReleaseAllOCAOrdersClick(Sender: TObject);
resourcestring
  rsQuestion = 'All the orders of OCA group number %d will be released. Continue?';
var
  Monitor:  IMonitor;
begin
  inherited;
  if (cbScope.ItemIndex = TOrderIBDoc.C_SCOPE_ONE_CANCELS_ALL_OCA) and (OcaGroupNumber > -1) and
     Supports(Application.MainForm, IMonitor, Monitor) and
    (TMessageDialog.ShowQuestion(Format(rsQuestion, [OcaGroupNumber])) = mrYes) then
    Monitor.OrdersExecuteOcaGroup(OcaGroupNumber);
end;

function TfrmEditOrderIB.GetBrokerType: TBrokerType;
begin
  Result := TBrokerType.brIB;
end;

function TfrmEditOrderIB.GetCurrency: string;
begin
  Result := lblCurrency.Caption;
end;

procedure TfrmEditOrderIB.SetCurrency(const Value: string);
begin
  lblCurrency.Caption := Value;
end;

procedure TfrmEditOrderIB.SetDateStart(const Value: TDate);
begin
  dtpDateStart.Date := Value;
end;

function TfrmEditOrderIB.GetDateStart: TDate;
begin
  Result := dtpDateStart.Date;
end;

procedure TfrmEditOrderIB.SetDateStop(const Value: TDate);
begin
  dtpDateStop.Date := Value;
end;

function TfrmEditOrderIB.GetDateStop: TDate;
begin
  Result := dtpDateStop.Date;
end;

function TfrmEditOrderIB.GetDescription: string;
begin
  Result := Trim(edDescription.Text);
end;

function TfrmEditOrderIB.GetExchange: string;
begin
  Result := lblExchange.Caption;
end;

function TfrmEditOrderIB.GetFilled: Integer;
begin
  Result := VarToIntDef(lblFilled.Caption);
end;

procedure TfrmEditOrderIB.SetDecimals(const Value: Integer);
begin
  FDecimals := Value;
  edAuxPrice.Decimal       := FDecimals;
  edLimit.Decimal          := FDecimals;
  edTrailStopPrice.Decimal := FDecimals;
  edLmtPriceOffset.Decimal := FDecimals;
end;

procedure TfrmEditOrderIB.SetDescription(const Value: string);
begin
  edDescription.Text := Value;
end;

procedure TfrmEditOrderIB.DisableButtons;
begin
  aBuy.Enabled      := False;
  btnStatus.Enabled := False;
end;

procedure TfrmEditOrderIB.OnPercentChange(Sender: TObject);
begin
  inherited;
  if not Assigned(Sender) or (Sender = edAuxPricePerc) then
  begin
    edAuxPricePerc.Color := clWindow;
    if OrderType in [otTrail, otTrailLimit] then
    begin
      if (AuxPricePerc <= 0 )then
        edAuxPricePerc.Color := C_WRONG_VALUE_COLOUR;
    end
    else if OrderType in [otStopLimit] then
      case OrderAction of
        iabBuy:
          begin
            if (AuxPricePerc <= 0 )then
              edAuxPricePerc.Color := C_WRONG_VALUE_COLOUR;
          end;
        iabSell:
          begin
            if (AuxPricePerc >= 0) then
              edAuxPricePerc.Color := C_WRONG_VALUE_COLOUR;
          end;
      end
    else
      case OrderAction of
        iabBuy:
          begin
            if (AuxPricePerc <= 0) then
              edAuxPricePerc.Color := C_WRONG_VALUE_COLOUR;
          end;
        iabSell:
          begin
            if (AuxPricePerc >= 0) then
              edAuxPricePerc.Color := C_WRONG_VALUE_COLOUR;
          end;
      end;
  end;

  if not Assigned(Sender) or (Sender = edLimitPerc) then
  begin
    edLimitPerc.Color := clWindow;
    if OrderType in [otStopLimit, otTrailLimit] then
      case OrderAction of
        iabBuy:
          begin
            if (LimitPerc <= 0) then
              edLimitPerc.Color := C_WRONG_VALUE_COLOUR;
          end;
        iabSell:
          begin
            if (LimitPerc >= 0) then
              edLimitPerc.Color := C_WRONG_VALUE_COLOUR;
          end;
      end
    else
      case OrderAction of
        iabBuy:
          begin
            if (LimitPerc >= 0) then
              edLimitPerc.Color := C_WRONG_VALUE_COLOUR;
          end;
        iabSell:
          begin
            if (LimitPerc <= 0) then
              edLimitPerc.Color := C_WRONG_VALUE_COLOUR;
          end;
      end;
  end;

  if not Assigned(Sender) or (Sender = edTrailStopPricePerc) then
  begin
    edTrailStopPricePerc.Color := clWindow;
    case OrderAction of
      iabBuy:
        begin
          if (TrailStopPricePerc <= 0) then
            edTrailStopPricePerc.Color := C_WRONG_VALUE_COLOUR;
        end;
      iabSell:
        begin
          if (TrailStopPricePerc >= 0) then
            edTrailStopPricePerc.Color := C_WRONG_VALUE_COLOUR;
        end;
    end;
  end;

  if not Assigned(Sender) or (Sender = edLmtPriceOffsetPerc) then
  begin
    edLmtPriceOffsetPerc.Color := clWindow;
    if OrderType in [otTrailLimit] then
      if (LmtPriceOffsetPerc <= 0) then
        edLmtPriceOffsetPerc.Color := C_WRONG_VALUE_COLOUR;
  end;
end;

procedure TfrmEditOrderIB.SetExchange(const Value: string);
begin
  lblExchange.Caption := Value;
end;

procedure TfrmEditOrderIB.SetExtendedOptions(const Value: TCustomOrderDoc.TExtendedOptions);

  function GetItemIndex(const aBasePrice: TBasePrice): Integer;
  var
    Index: Integer;
  begin
    Result := -1;
    Index := -1;
    for var BasePrice := High(TBasePrice) downto Low(TBasePrice) do
      case BasePrice of
        pbBid10 .. pbBid, pbAsk .. pbAsk10, pbLast .. pbHigh:
          begin
            Inc(Index);
            if (BasePrice = aBasePrice) then
              Exit(Index);
            if (BasePrice in [pbLast .. pbHigh]) then
              Result := Index;
          end;
      end;
  end;

var
  Index: Integer;
begin
  FExtendedOptions := Value;
  Index := GetItemIndex(FExtendedOptions.AuxBasePrice);
  if (Index > -1) then
    cbAuxBasePrice.ItemIndex := Index;
  Index := GetItemIndex(FExtendedOptions.LimitBasePrice);
  if (Index > -1) then
    cbLimitBasePrice.ItemIndex := Index;
  Index := GetItemIndex(FExtendedOptions.LmtOffsetBasePrice);
  if (Index > -1) then
    cbLmtOffsetBasePrice.ItemIndex := Index;
  Index := GetItemIndex(FExtendedOptions.TrailStopBasePrice);
  if (Index > -1) then
    cbTrailStopBasePrice.ItemIndex := Index;

  if FExtendedOptions.AuxPriceRelative <> 0 then
    edAuxPricePerc.ValueFloat := FExtendedOptions.AuxPriceRelative;
  if FExtendedOptions.LimitPriceRelative <> 0 then
    edLimitPerc.ValueFloat := FExtendedOptions.LimitPriceRelative;
  if FExtendedOptions.LimitPriceOffset <> 0 then
    edLmtPriceOffsetPerc.ValueFloat := FExtendedOptions.LimitPriceOffset;
  if FExtendedOptions.TrailStopPriceRelative <> 0 then
    edTrailStopPricePerc.ValueFloat := FExtendedOptions.TrailStopPriceRelative;
end;

function TfrmEditOrderIB.GetExtendedOptions: TCustomOrderDoc.TExtendedOptions;
begin
  if (cbAuxBasePrice.ItemIndex > -1) then
    FExtendedOptions.AuxBasePrice := TBasePrice(cbAuxBasePrice.Items.Objects[cbAuxBasePrice.ItemIndex]);
  if (cbLimitBasePrice.ItemIndex > -1) then
    FExtendedOptions.LimitBasePrice := TBasePrice(cbLimitBasePrice.Items.Objects[cbLimitBasePrice.ItemIndex]);
  if (cbLmtOffsetBasePrice.ItemIndex > -1) then
    FExtendedOptions.LmtOffsetBasePrice := TBasePrice(cbLmtOffsetBasePrice.Items.Objects[cbLmtOffsetBasePrice.ItemIndex]);
  if (cbTrailStopBasePrice.ItemIndex > -1) then
    FExtendedOptions.TrailStopBasePrice := TBasePrice(cbTrailStopBasePrice.Items.Objects[cbTrailStopBasePrice.ItemIndex]);
  Result := FExtendedOptions;
end;

procedure TfrmEditOrderIB.SetFilled(const Value: Integer);
begin
  lblFilled.Caption := Value.ToString;
end;

function TfrmEditOrderIB.GetInstance: TObject;
begin
  Result := Self;
end;

function TfrmEditOrderIB.GetInstrumentName: string;
begin
  Result := FInstrumentName;
end;

function TfrmEditOrderIB.GetIsExecuted: Boolean;
begin
  Result := FIsExecuted or (OrderIBId > 0) or (OrderStatus <> osSleeping);
end;

procedure TfrmEditOrderIB.SetId(const Value: Integer);
begin
  FId := Value;
  lblConId.Caption := Value.ToString;
  FLastPrice := TMonitorLists.PriceCache.GetLastPrice(Value, ttLast);
  lblLastPrice.Caption := FormatFloat(C_CURRENCY_FORMAT, FLastPrice);
  if SokidList.ContainsKey(Value) then
  begin
    FSokidInfo := SokidList.Items[Value];
    if not (FSokidInfo.GetSecurityType in [stFuture]) then
      FSokidInfo.MinimumTick := 0;
  end;
end;

procedure TfrmEditOrderIB.SetInfo(const Value: string);
begin
  FInfo := Value;
  memoInfo.Lines.Clear;
  memoInfo.Lines.Text := Value.Replace(',', sLineBreak);
end;

procedure TfrmEditOrderIB.SetInstrumentName(const Value: string);
begin
  FInstrumentName := Value;
  Self.Caption := 'Instrument ' + Value;
end;

function TfrmEditOrderIB.GetIsFinal: Boolean;
begin
  Result := chbOrderIsFinal.Checked;
end;

procedure TfrmEditOrderIB.SetIsFinal(const Value: Boolean);
begin
  chbOrderIsFinal.Checked := Value;
end;

function TfrmEditOrderIB.GetLimitPrice: Double;
begin
  Result := edLimit.ValueFloat;
end;

procedure TfrmEditOrderIB.SetLimitPerc(const Value: Double);
begin
  edLimitPerc.ValueFloat := Value;
end;

function TfrmEditOrderIB.GetLimitPerc: Double;
begin
  Result := edLimitPerc.ValueFloat;
end;

procedure TfrmEditOrderIB.SetLimitPrice(const Value: Double);
begin
  edLimit.ValueFloat := Value;
end;

procedure TfrmEditOrderIB.SetAuxPrice(const Value: Double);
begin
  edAuxPrice.ValueFloat := Value;
end;

function TfrmEditOrderIB.GetAuxPrice: Double;
begin
  Result := edAuxPrice.ValueFloat;
end;

function TfrmEditOrderIB.GetAuxPricePerc: Double;
begin
  Result := edAuxPricePerc.ValueFloat;
end;

procedure TfrmEditOrderIB.SetAuxPricePerc(const Value: Double);
begin
  edAuxPricePerc.ValueFloat := Value;
end;

function TfrmEditOrderIB.GetLmtPriceOffset: Double;
begin
  Result := edLmtPriceOffset.ValueFloat;
end;

procedure TfrmEditOrderIB.SetLmtPriceOffset(const Value: Double);
begin
  edLmtPriceOffset.ValueFloat := Value;
end;

procedure TfrmEditOrderIB.SetLmtPriceOffsetPerc(const Value: Double);
begin
  edLmtPriceOffsetPerc.ValueFloat := Value;
end;

function TfrmEditOrderIB.GetLmtPriceOffsetPerc: Double;
begin
  Result := edLmtPriceOffsetPerc.ValueFloat;
end;

procedure TfrmEditOrderIB.SetOrderAction(const Value: TIABAction);
begin
  if Value in [iabBuy, iabSell] then
  begin
    FAction := Value;
    case Value of
      iabBuy:
        begin
          rbBuy.Checked  := True;
          rbSell.Checked := False;
        end;
      iabSell:
        begin
          rbBuy.Checked  := False;
          rbSell.Checked := True;
        end;
    end;
  end;
end;

function TfrmEditOrderIB.GetOrderAction: TIABAction;
begin
  Result := FAction;
end;

function TfrmEditOrderIB.GetAdvancedOrderType: TAdvancedOrderType;
begin
  Result := TAdvancedOrderType(cbAdvancedOrderType.ItemIndex);
end;

procedure TfrmEditOrderIB.SetAdvancedOrderType(const Value: TAdvancedOrderType);
begin
  cbAdvancedOrderType.ItemIndex := Integer(Value);
end;

procedure TfrmEditOrderIB.SetModyfied(Sender: TObject);
begin
  if Showing then
    Modyfied := True;
end;

procedure TfrmEditOrderIB.SetMultiplier(const Value: Double);
begin
  FMultiplier := Value;
end;

function TfrmEditOrderIB.GetModyfied: Boolean;
begin
  Result := FModyfied or IsExecuted;
end;

function TfrmEditOrderIB.GetMaxNumAmount: Integer;
begin
  Result := seMaxNumAmount.Value;
end;

function TfrmEditOrderIB.GetMaxNumShares: Integer;
begin
  Result := seMaxNumShares.Value;
end;

procedure TfrmEditOrderIB.SetMarketList(const Value: string);
begin
  FMarketList := Value;
  TickSize := IABClient.MarketRuleList.GetMinTick(FLastPrice, Value, FSokidInfo.MinimumTick);
end;

procedure TfrmEditOrderIB.SetMaxNumAmount(const Value: Integer);
begin
  seMaxNumAmount.Value := Value;
end;

procedure TfrmEditOrderIB.SetMaxNumShares(const Value: Integer);
begin
  seMaxNumShares.Value := Value;
end;

function TfrmEditOrderIB.GetQuantity: Integer;
begin
  Result := edQuantity.ValueInt;
end;

function TfrmEditOrderIB.GetScope: Integer;
begin
  Result := cbScope.ItemIndex;
end;

procedure TfrmEditOrderIB.SetScope(const Value: Integer);
begin
  cbScope.ItemIndex := Value;
end;

procedure TfrmEditOrderIB.SetSecurityType(const Value: TIABSecurityType);
begin
  FSecurityType   := Value;
  lblType.Caption := Value.ToString;
end;

procedure TfrmEditOrderIB.SetQuantity(const Value: Integer);
begin
  edQuantity.ValueInt := Value;
  if (Multiplier > 1) then
    lblOrderValue.Caption := FormatFloat(C_CURRENCY_FORMAT, FLastPrice * Multiplier * Value)
  else
    lblOrderValue.Caption := FormatFloat(C_CURRENCY_FORMAT, FLastPrice * Value);
end;

procedure TfrmEditOrderIB.edQuantityChangeValue(Sender: TObject);
begin
  inherited;
  SetOrderValue;
  Modyfied := True;
end;

procedure TfrmEditOrderIB.SetTickSize(const Value: Double);
begin
  if (FTickSize <> Value) then
  begin
    FTickSize := Value;
    lblTickSize.Caption := FTickSize.ToString;
  end;
end;

procedure TfrmEditOrderIB.SetTimeInForce(const Value: Integer);
begin
  cbTimeInForce.ItemIndex := Value;
end;

function TfrmEditOrderIB.GetTimeInForce: Integer;
begin
  Result := cbTimeInForce.ItemIndex;
end;

procedure TfrmEditOrderIB.SetTimeStart(const Value: TTime);
begin
  dtpTimeStart.Time := Value;
end;

function TfrmEditOrderIB.GetTimeStart: TTime;
begin
  Result := dtpTimeStart.Time;
end;

procedure TfrmEditOrderIB.SetTimeStop(const Value: TTime);
begin
  dtpTimeStop.Time := Value;
end;

function TfrmEditOrderIB.GetTimeStop: TTime;
begin
  Result := dtpTimeStop.Time;
end;

procedure TfrmEditOrderIB.SetTrailStopPrice(const Value: Double);
begin
  edTrailStopPrice.ValueFloat := Value;
end;

function TfrmEditOrderIB.GetTrailStopPrice: Double;
begin
  Result := edTrailStopPrice.ValueFloat;
end;

function TfrmEditOrderIB.GetTrailStopPricePerc: Double;
begin
  Result := edTrailStopPricePerc.ValueFloat;
end;

function TfrmEditOrderIB.GetTriggerMethod: TTriggerMethod;
begin
  Result := Low(TTriggerMethod);
  if (cbTriggerMethod.ItemIndex > -1) then
    Result := TTriggerMethod(cbTriggerMethod.Items.Objects[cbTriggerMethod.ItemIndex]);
end;

procedure TfrmEditOrderIB.SetTriggerMethod(const Value: TTriggerMethod);
begin
  cbTriggerMethod.ItemIndex := cbTriggerMethod.Items.IndexOf(Value.ToString);
end;

procedure TfrmEditOrderIB.SetTrailStopPricePerc(const Value: Double);
begin
  edTrailStopPricePerc.ValueFloat := Value;
end;

procedure TfrmEditOrderIB.SetOrderIBId(const Value: Integer);
begin
  FOrderIBId := Value;
  lblOrderIBId.Caption := FOrderIBId.ToString;
end;

procedure TfrmEditOrderIB.SetOrderStatus(const Value: TIABOrderState);
var
  Data: PTreeData;
  Monitor: IMonitor;
begin
  FOrderStatus         := Value;
  lblStatus.Caption    := Value.ToString;
  lblStatus.Font.Color := clBlack;
  if (Value = osError) then
  begin
    lblStatus.Caption    := 'Error';
    lblStatus.Font.Color := clRed;
  end;

  if Assigned(OwnerNode) and Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    TTreeDocument.SetIcon(OwnerNode, Monitor.GetMainTree);
    Data := OwnerNode^.GetData;
    if Assigned(Data) then
      Data.Enabled := not (Value in [osFilled, osCancelled, osPendCancel, osPendSubmit]);
  end;
  btnCancelOrder.Enabled := not (OrderStatus in [osFilled, osCancelled, osPendCancel]);
end;

procedure TfrmEditOrderIB.SetOrderStartEnabled(const Value: Boolean);
begin
  chbOrderStart.Checked := Value;
end;

function TfrmEditOrderIB.GetOrderStartEnabled: Boolean;
begin
  Result := chbOrderStart.Checked;
end;

procedure TfrmEditOrderIB.SetOrderStopEnabled(const Value: Boolean);
begin
  chbOrderStop.Checked := Value;
end;

function TfrmEditOrderIB.GetOrderStopEnabled: Boolean;
begin
  Result := chbOrderStop.Checked;
end;

procedure TfrmEditOrderIB.SetOrderType(const Value: TIABOrderType);
begin
  if (OrderType <> Value) then
    cbOrderType.ItemIndex := Ord(Value) - 1;
end;

procedure TfrmEditOrderIB.OnBasePriceChange(Sender: TObject);
var
  Price: Double;
begin
  inherited;
//  General.LogPublisher.Write([ltLogWriter, ltLogView], ddEnterMethod, Self, 'OnBasePricesChange');
  if Sender is TComboBox then
    with TComboBox(Sender) do
      if (ItemIndex > -1) then
      begin
        Price := GetBasePriceValue(TBasePrice(Items.Objects[ItemIndex]));
        if (Sender = cbAuxBasePrice) then
          lblAuxPriceCalc.Caption := FormatFloat(C_CURRENCY_FORMAT, Price)
        else if (Sender = cbLimitBasePrice) then
          lblLimitPriceCalc.Caption := FormatFloat(C_CURRENCY_FORMAT, Price)
        else if (Sender = cbLmtOffsetBasePrice) then
          lblLmtPriceOffsetCalc.Caption := FormatFloat(C_CURRENCY_FORMAT, Price)
        else if (Sender = cbTrailStopBasePrice) then
          lblTrailStopPriceCalc.Caption := FormatFloat(C_CURRENCY_FORMAT, Price);
      end;
//  General.LogPublisher.Write([ltLogWriter, ltLogView], ddExitMethod, Self, 'OnBasePricesChange');
end;

function TfrmEditOrderIB.GetBasePriceValue(const aBasePrice: TBasePrice): Currency;
var
  AskPrice: Double;
  BidPrice: Double;
begin
  Result := FLastPrice;
  case aBasePrice of
    pbAsk .. pbAsk10:
      begin
        AskPrice := TMonitorLists.PriceCache.GetLastPrice(Id, ttAsk);
        if (AskPrice = 0) then
          AskPrice := FLastPrice;
        Result := AskPrice + TickSize * Abs(Ord(aBasePrice) - 100);
      end;
    pbBid10 .. pbBid:
      begin
        BidPrice := TMonitorLists.PriceCache.GetLastPrice(Id, ttBid);
        if (BidPrice = 0) then
          BidPrice := FLastPrice;
        Result := BidPrice - TickSize * Abs(Ord(aBasePrice) + 100);
      end;
    pbMid:
      begin
        BidPrice := TMonitorLists.PriceCache.GetLastPrice(Id, ttBid);
        if (BidPrice = 0) then
          BidPrice := FLastPrice;
        AskPrice := TMonitorLists.PriceCache.GetLastPrice(Id, ttAsk);
        if (AskPrice = 0) then
          AskPrice := FLastPrice;
        Result := BidPrice + (AskPrice - BidPrice) / 2;
      end;
  end;
end;

procedure TfrmEditOrderIB.SetOrderValue;
begin
  if (Multiplier > 1) then
    lblOrderValue.Caption := FormatFloat(C_CURRENCY_FORMAT, FLastPrice * Multiplier * Quantity)
  else
    lblOrderValue.Caption := FormatFloat(C_CURRENCY_FORMAT, FLastPrice * Quantity);
end;

function TfrmEditOrderIB.GetOrderType: TIABOrderType;
begin
  if (cbOrderType.ItemIndex > -1) then
    Result := TIABOrderType(cbOrderType.ItemIndex + 1)
  else
    Result := otUnknown;
end;

function TfrmEditOrderIB.GetOcaGroupNumber: Integer;
begin
  Result := seOcaGroupNumber.Value;
end;

procedure TfrmEditOrderIB.SetOcaGroupNumber(const Value: Integer);
begin
  seOcaGroupNumber.Value := Value;
end;

function TfrmEditOrderIB.GetOrderStatusText: string;
begin
  Result := lblStatus.Caption;
end;

procedure TfrmEditOrderIB.AssignFromDoc(const aDocument: TOrderIBDoc);
begin
  if Assigned(aDocument) then
  begin
    Self.LastPrice         := aDocument.LastPrice;
    Self.AdvancedOrderType := aDocument.AdvancedOrderType;
    Self.AuxPrice          := aDocument.AuxPrice;
    Self.ColTick           := aDocument.ColTick;
    Self.Currency          := aDocument.Currency;
    Self.DateStart         := aDocument.DateStart;
    Self.DateStop          := aDocument.DateStop;
    Self.Decimals          := aDocument.Decimals;
    Self.Description       := aDocument.Description;
    Self.Exchange          := aDocument.Exchange;
    Self.Id                := aDocument.Id;
    Self.Info              := aDocument.Info;
    Self.InstrumentName    := aDocument.InstrumentName;
    Self.IsExecuted        := aDocument.IsExecuted;
    Self.IsRepetitive      := aDocument.IsRepetitive;
    Self.IsFinal           := aDocument.IsFinal;
    Self.Limit             := aDocument.Limit;
    Self.LmtPriceOffset    := aDocument.LmtPriceOffset;
    Self.MaxNumAmount      := aDocument.MaxNumAmount;
    Self.MaxNumShares      := aDocument.MaxNumShares;
    Self.Multiplier        := aDocument.Multiplier;
    Self.OcaGroupNumber    := aDocument.OcaGroupNumber;
    Self.OrderAction       := aDocument.OrderAction;
    Self.OrderIBId         := aDocument.OrderIBId;
    Self.OrderStartEnabled := aDocument.OrderStartEnabled;
    Self.OrderStatus       := aDocument.OrderStatus;
    Self.OrderStopEnabled  := aDocument.OrderStopEnabled;
    Self.OrderType         := aDocument.OrderType;
    Self.OwnerNode         := aDocument.OwnerNode;
    Self.ParentIBId        := aDocument.ParentIBId;
    Self.Quantity          := aDocument.Quantity;
    Self.RateType          := aDocument.RateType;
    Self.Scope             := aDocument.Scope;
    Self.SecurityType      := aDocument.SecurityType;
    Self.Symbol            := aDocument.Symbol;
    Self.TimeInForce       := aDocument.TimeInForce;
    Self.TimeStart         := aDocument.TimeStart;
    Self.TimeStop          := aDocument.TimeStop;
    Self.TrailingPercent   := aDocument.TrailingPercent;
    Self.TrailStopPrice    := aDocument.TrailStopPrice;
    Self.Transmit          := aDocument.Transmit;
    Self.TriggerMethod     := aDocument.TriggerMethod;
    Self.Filled            := aDocument.Filled;
    Self.MarketList        := aDocument.MarketList;
    Self.XmlParams         := aDocument.XmlParams;
    Self.ExtendedOptions   := aDocument.ExtendedOptions;
  end;
//  General.LogPublisher.Write([ltLogWriter, ltLogView], ddExitMethod, Self, 'AssignFromDoc');
end;

procedure TfrmEditOrderIB.AssignToDocMainParams(var aDocument: TOrderIBDoc);
begin
  if Assigned(aDocument) then
  begin
    aDocument.AdvancedOrderType := Self.AdvancedOrderType;
    aDocument.AuxPrice          := Self.AuxPrice;
    aDocument.BrokerType        := Self.BrokerType;
    aDocument.ColTick           := Self.ColTick;
    aDocument.Currency          := Self.Currency;
    aDocument.DateStart         := Self.DateStart;
    aDocument.DateStop          := Self.DateStop;
    aDocument.Decimals          := Self.Decimals;
    aDocument.Description       := Self.Description;
    aDocument.Exchange          := Self.Exchange;
    aDocument.InstrumentName    := Self.InstrumentName;
    aDocument.IsRepetitive      := Self.IsRepetitive;
    aDocument.IsFinal           := Self.IsFinal;
    aDocument.Limit             := Self.Limit;
    aDocument.LmtPriceOffset    := Self.LmtPriceOffset;
    aDocument.MaxNumAmount      := Self.MaxNumAmount;
    aDocument.MaxNumShares      := Self.MaxNumShares;
    aDocument.OcaGroupNumber    := Self.OcaGroupNumber;
    aDocument.OrderAction       := Self.OrderAction;
    aDocument.OrderStartEnabled := Self.OrderStartEnabled;
    aDocument.OrderStopEnabled  := Self.OrderStopEnabled;
    aDocument.OrderType         := Self.OrderType;
    aDocument.ParentIBId        := Self.ParentIBId;
    aDocument.Quantity          := Self.Quantity;
    aDocument.Scope             := Self.Scope;
    aDocument.SecurityType      := Self.SecurityType;
    aDocument.Symbol            := Self.Symbol;
    aDocument.TimeInForce       := Self.TimeInForce;
    aDocument.TimeStart         := Self.TimeStart;
    aDocument.TimeStop          := Self.TimeStop;
    aDocument.TrailingPercent   := Self.TrailingPercent;
    aDocument.TrailStopPrice    := Self.TrailStopPrice;
    aDocument.Transmit          := Self.Transmit;
    aDocument.TriggerMethod     := Self.TriggerMethod;
    aDocument.RateType          := Self.RateType;
    aDocument.XmlParams         := Self.XmlParams;
    aDocument.ExtendedOptions   := Self.ExtendedOptions;
  end;
end;

procedure TfrmEditOrderIB.NormalizationOrder;
begin
    case Self.OrderType of
      otMarket, otMarketOpen:
        begin
          Self.AuxPrice        := 0;
          Self.Limit           := 0;
          Self.LmtPriceOffset  := 0;
          Self.TrailingPercent := 0;
          Self.TrailStopPrice  := 0;
        end;
      otLimit:
        begin
          Self.AuxPrice        := 0;
          Self.LmtPriceOffset  := 0;
          Self.TrailingPercent := 0;
          Self.TrailStopPrice  := 0;
        end;
      otStop, otPegMarket, otPegPrimary, otPegMidPt:
        begin
          Self.Limit           := 0;
          Self.LmtPriceOffset  := 0;
          Self.TrailingPercent := 0;
          Self.TrailStopPrice  := 0;
        end;
      otStopLimit:
        begin
          Self.LmtPriceOffset  := 0;
          Self.TrailingPercent := 0;
          Self.TrailStopPrice  := 0;
        end;
      otTrail:
        begin
//          Self.AuxPrice        := 0;
          Self.Limit           := 0;
          Self.LmtPriceOffset  := 0;
          Self.TrailingPercent := 0;
//          Self.TrailStopPrice  := 0;
        end;
      otTrailLimit:
        begin
          Self.TrailingPercent := 0;
          Self.Limit           := 0;
        end;
    end;
end;

procedure TfrmEditOrderIB.OnExecution(Sender: TObject; Order: TIABOrder);
begin
  //nothing
end;

procedure TfrmEditOrderIB.OnOpenOrder(Sender: TObject; Order: TIABOrder);
begin
  //nothing
end;

procedure TfrmEditOrderIB.OnRebuildFromTWS(Sender: TObject);
begin
  //nothing
end;

procedure TfrmEditOrderIB.OnOpenOrderNN(const aOrderList: array of TOrder);
begin
  //nothing
end;

procedure TfrmEditOrderIB.OnCloseOrder(const aTempId: Integer);
begin
  //nothing
end;

procedure TfrmEditOrderIB.OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
var
  Data: PTreeData;
begin
  if (Order.TempId > 0) and (Order.TempId = OrderIBId) then
  begin
    Self.OrderStatus := Status;
    if Assigned(OwnerNode) then
    begin
      Data := PVirtualNode(OwnerNode)^.GetData;
      Self.Info := Data.OrderDoc.Info;
    end;
  end;
end;

procedure TfrmEditOrderIB.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
begin
  if (FId = Id) and (TickType = ttLast) then
  begin
    FLastPrice := Value;
    lblLastPrice.Caption := FormatFloat(C_CURRENCY_FORMAT, Value);
    TickSize := IABClient.MarketRuleList.GetMinTick(FLastPrice, FMarketList, FSokidInfo.MinimumTick);
    SetOrderValue;

    OnBasePriceChange(cbAuxBasePrice);
    OnBasePriceChange(cbLimitBasePrice);
    OnBasePriceChange(cbLmtOffsetBasePrice);
    OnBasePriceChange(cbTrailStopBasePrice);
  end;
end;

procedure TfrmEditOrderIB.AssignToDoc(var aDocument: TOrderIBDoc);
begin
  if Assigned(aDocument) then
  begin
    NormalizationOrder;
    AssignToDocMainParams(aDocument);
    aDocument.OrderIBId   := Self.OrderIBId;
    aDocument.IsExecuted  := Self.IsExecuted;
    aDocument.OrderStatus := Self.OrderStatus;
  end;
end;

end.
