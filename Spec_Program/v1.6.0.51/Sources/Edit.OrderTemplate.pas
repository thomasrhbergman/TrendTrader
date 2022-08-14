unit Edit.OrderTemplate;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Edit.Factor, Vcl.ExtCtrls, Vcl.ComCtrls, VirtualTrees,
  Vcl.Imaging.pngimage, IABfunctions, IABSocketAPI, Vcl.Samples.Spin, System.Math, System.Actions, Vcl.ActnList,
  DebugWriter, HtmlLib, Utils, System.UITypes, Global.Types, Monitor.Types, Document, MessageDialog,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const, Common.Types, InstrumentList, XmlFiles,
  Entity.Sokid, IABFunctions.MarketRules, ParametersStore, DaModule, DaImages, BrokerHelperAbstr, Vcl.WinXPanels,
  Vcl.NumberBox, Vcl.ControlList, Vcl.VirtualImage, Global.Resources, IABFunctions.Helpers;
{$ENDREGION}

type
  TfrmOrderTemplate = class(TFormDocument)
    aCancel: TAction;
    ActionListOrder: TActionList;
    aSave: TAction;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    cbActivateChildOrder: TCheckBox;
    cbAdvancedOrderType: TComboBox;
    cbAuxBasePrice: TComboBox;
    cbBroker: TComboBox;
    cbLimitBasePrice: TComboBox;
    cbLmtOffsetBasePrice: TComboBox;
    cbOrderType: TComboBox;
    cbScope: TComboBox;
    cbSecureType: TComboBox;
    cbTimeInForce: TComboBox;
    cbTrailStopBasePrice: TComboBox;
    cbTriggerMethod: TComboBox;
    chbOrderStart: TCheckBox;
    chbOrderStop: TCheckBox;
    cpMain: TCardPanel;
    crdIB: TCard;
    crdNN: TCard;
    crdTest: TCard;
    dtpDateStart: TDateTimePicker;
    dtpDateStop: TDateTimePicker;
    dtpTimeStart: TDateTimePicker;
    dtpTimeStop: TDateTimePicker;
    edAuxPrice: TNumberBox;
    edDescription: TEdit;
    edLimit: TNumberBox;
    edLmtPriceOffset: TNumberBox;
    edTrailStopPrice: TNumberBox;
    grpRepetitive: TGroupBox;
    imgWarning: TVirtualImage;
    lblAdvancedOrderType: TLabel;
    lblAuxPrice: TLabel;
    lblAvailableSecureType: TLabel;
    lblBroker: TLabel;
    lblBuy: TLabel;
    lblDescription: TLabel;
    lblInfo: TLabel;
    lblLmtOffset: TLabel;
    lblLmtPrice: TLabel;
    lblMaxNumAmount: TLabel;
    lblMaxNumShares: TLabel;
    lblOcaName: TLabel;
    lblOrderType: TLabel;
    lblScope: TLabel;
    lblSecureType: TLabel;
    lblSell: TLabel;
    lblTimeInForce: TLabel;
    lblTrailStopPrice: TLabel;
    lblTriggerMethod: TLabel;
    pnlActivateChild: TPanel;
    pnlAdvanced: TPanel;
    pnlAuxPrice: TPanel;
    pnlBottom: TPanel;
    pnlIB: TPanel;
    pnlInfo: TPanel;
    pnlLimitPrice: TPanel;
    pnlLmtOffset: TPanel;
    pnlNN: TPanel;
    pnlOrderOptions: TPanel;
    pnlOrderStart: TPanel;
    pnlOrderStop: TPanel;
    pnlTest: TPanel;
    pnlTop: TPanel;
    pnlTrailStopPrice: TPanel;
    rbBuy: TRadioButton;
    rbChildOrderPriceBased: TRadioGroup;
    rbSell: TRadioButton;
    seMaxNumAmount: TSpinEdit;
    seMaxNumShares: TSpinEdit;
    seOcaGroupNumber: TSpinEdit;
    chbOrderIsFinal: TCheckBox;
    procedure aSaveExecute(Sender: TObject);
    procedure cbBrokerChange(Sender: TObject);
    procedure cbOrderTypeChange(Sender: TObject);
    procedure cbScopeChange(Sender: TObject);
    procedure cbSecureTypeChange(Sender: TObject);
    procedure cbTimeInForceChange(Sender: TObject);
    procedure chbOrderStartClick(Sender: TObject);
    procedure chbOrderStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnBasePricesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure OnChildOrderPriceBasedChange(Sender: TObject);
    procedure OnPercentChange(Sender: TObject);
    procedure rbBuyClick(Sender: TObject);
    procedure rbSellClick(Sender: TObject);
  private
    FAction: TIABAction;
    FExtendedOptions: TCustomOrderDoc.TExtendedOptions;
    FId: Integer;
    FParametersStore: TParametersStore;
    FSecurityType: TIABSecurityType;
    FSokidInfo: TSokidInfo;
    FSubordination: TSubordination;
    FSymbol: string;
    FTrailingPercent: Double;
    function GetAdvancedOrderType: TAdvancedOrderType;
    function GetAuxPrice: Double;
    function GetBrokerType: TBrokerType;
    function GetDateStart: TDate;
    function GetDateStop: TDate;
    function GetDescription: string;
    function GetExchange: string;
    function GetExtendedOptions: TCustomOrderDoc.TExtendedOptions;
    function GetIsActivateChild: Boolean;
    function GetLimitPrice: Double;
    function GetLmtPriceOffset: Double;
    function GetMaxNumAmount: Integer;
    function GetMaxNumShares: Integer;
    function GetOcaGroupNumber: Integer;
    function GetOrderAction: TIABAction;
    function GetOrderStartEnabled: Boolean;
    function GetOrderStopEnabled: Boolean;
    function GetOrderType: TIABOrderType;
    function GetScope: Integer;
    function GetTimeInForce: Integer;
    function GetTimeStart: TTime;
    function GetTimeStop: TTime;
    function GetTrailStopPrice: Double;
    function GetTriggerMethod: TTriggerMethod;
    procedure ChangeOrderType;
    procedure CheckSecureType;
    procedure NormalizationOrder;
    procedure SetAdvancedOrderType(const Value: TAdvancedOrderType);
    procedure SetAuxPrice(const Value: Double);
    procedure SetBrokerType(const Value: TBrokerType);
    procedure SetDateStart(const Value: TDate);
    procedure SetDateStop(const Value: TDate);
    procedure SetDescription(const Value: string);
    procedure SetExchange(const Value: string);
    procedure SetExtendedOptions(const Value: TCustomOrderDoc.TExtendedOptions);
    procedure SetId(const Value: Integer);
    procedure SetIsActivateChild(const Value: Boolean);
    procedure SetLimitPrice(const Value: Double);
    procedure SetLmtPriceOffset(const Value: Double);
    procedure SetMaxNumAmount(const Value: Integer);
    procedure SetMaxNumShares(const Value: Integer);
    procedure SetOcaGroupNumber(const Value: Integer);
    procedure SetOrderAction(const Value: TIABAction);
    procedure SetOrderStartEnabled(const Value: Boolean);
    procedure SetOrderStopEnabled(const Value: Boolean);
    procedure SetOrderType(const Value: TIABOrderType);
    procedure SetScope(const Value: Integer);
    procedure SetSecurityType(const Value: TIABSecurityType); overload;
    procedure SetTimeInForce(const Value: Integer);
    procedure SetTimeStart(const Value: TTime);
    procedure SetTimeStop(const Value: TTime);
    procedure SetTrailStopPrice(const Value: Double);
    procedure SetTriggerMethod(const Value: TTriggerMethod);
    function GetIsFinal: Boolean;
    procedure SetIsFinal(const Value: Boolean);
  public
    procedure Initialize;
    procedure Deinitialize;
    procedure LoadParamsFromXml;
    procedure SaveParamsToXml;
    procedure AssignFromDoc(const aDocument: TCustomOrderDoc);
    procedure AssignToDoc(var aDocument: TCustomOrderDoc);
    class function ShowDocument(aDocument: TCustomOrderDoc; const aSubordination: TSubordination): TModalResult;

    property AdvancedOrderType : TAdvancedOrderType  read GetAdvancedOrderType write SetAdvancedOrderType;
    property AuxPrice          : Double              read GetAuxPrice          write SetAuxPrice;
    property BrokerType        : TBrokerType         read GetBrokerType        write SetBrokerType;
    property DateStart         : TDate               read GetDateStart         write SetDateStart;
    property DateStop          : TDate               read GetDateStop          write SetDateStop;
    property Description       : string              read GetDescription       write SetDescription;
    property Exchange          : string              read GetExchange          write SetExchange;
    property Id                : Integer             read FId                  write SetId;
    property IsFinal           : Boolean             read GetIsFinal           write SetIsFinal;
    property IsActivateChild   : Boolean             read GetIsActivateChild   write SetIsActivateChild;
    property Limit             : Double              read GetLimitPrice        write SetLimitPrice;
    property LmtPriceOffset    : Double              read GetLmtPriceOffset    write SetLmtPriceOffset;
    property MaxNumAmount      : Integer             read GetMaxNumAmount      write SetMaxNumAmount;
    property MaxNumShares      : Integer             read GetMaxNumShares      write SetMaxNumShares;
    property OcaGroupNumber    : Integer             read GetOcaGroupNumber    write SetOcaGroupNumber;
    property OrderAction       : TIABAction          read GetOrderAction       write SetOrderAction;
    property OrderStartEnabled : Boolean             read GetOrderStartEnabled write SetOrderStartEnabled;
    property OrderStopEnabled  : Boolean             read GetOrderStopEnabled  write SetOrderStopEnabled;
    property OrderType         : TIABOrderType       read GetOrderType         write SetOrderType;
    property Scope             : Integer             read GetScope             write SetScope;
    property SecurityType      : TIABSecurityType    read FSecurityType        write SetSecurityType;
    property Symbol            : string              read FSymbol              write FSymbol;
    property TimeInForce       : Integer             read GetTimeInForce       write SetTimeInForce;
    property TimeStart         : TTime               read GetTimeStart         write SetTimeStart;
    property TimeStop          : TTime               read GetTimeStop          write SetTimeStop;
    property TrailingPercent   : Double              read FTrailingPercent     write FTrailingPercent;
    property TrailStopPrice    : Double              read GetTrailStopPrice    write SetTrailStopPrice;
    property TriggerMethod     : TTriggerMethod      read GetTriggerMethod     write SetTriggerMethod;

    property ExtendedOptions : TCustomOrderDoc.TExtendedOptions read GetExtendedOptions write SetExtendedOptions;
  end;

implementation

{$R *.dfm}

class function TfrmOrderTemplate.ShowDocument(aDocument: TCustomOrderDoc; const aSubordination: TSubordination): TModalResult;
begin
  Result := mrCancel;
  if Assigned(aDocument) then
  begin
    with TfrmOrderTemplate.Create(nil) do
    try
      FSubordination := aSubordination;
      AssignFromDoc(aDocument);
      Initialize;
      if (ShowModal = mrOk) then
      begin
        Result := mrOk;
        AssignToDoc(aDocument);
        Deinitialize;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmOrderTemplate.Initialize;
begin
  ChangeOrderType;
  cbTimeInForceChange(nil);
  pnlActivateChild.Visible       := FSubordination = suMotherOrder;
  rbChildOrderPriceBased.Visible := FSubordination = suChildOrder;
  lblInfo.Caption := rsChangingDocument;
  OnChildOrderPriceBasedChange(nil);
end;

procedure TfrmOrderTemplate.Deinitialize;
begin
  FParametersStore.IdentityName := 'EditOrderIB-' + OrderType.ToString + '-' + OrderAction.ToString;
  FParametersStore.Store;
end;

procedure TfrmOrderTemplate.FormCreate(Sender: TObject);
var
  ItemIndex: Integer;
begin
  inherited;
  for var ot := otMarket to otRelMktCombo do
    cbOrderType.Items.Add(ot.ToString);

  for var tf := System.Low(TIABTimeInForce) to System.High(TIABTimeInForce) do
    cbTimeInForce.Items.Add(tf.ToString);

  for var at := System.Low(TAdvancedOrderType) to System.High(TAdvancedOrderType) do
    cbAdvancedOrderType.Items.Add(at.ToString);

  for var br := System.Low(TBrokerType) to System.High(TBrokerType) do
    cbBroker.Items.Add(br.ToString);

  for var st := System.Low(TIABSecurityType) to System.High(TIABSecurityType) do
    cbSecureType.Items.Add(st.ToString);

  for var tr := System.Low(TTriggerMethod) to System.High(TTriggerMethod) do
    if not tr.ToString.IsEmpty then
      cbTriggerMethod.Items.AddObject(tr.ToString, TObject(tr));

  seOcaGroupNumber.Value := -1;
  OrderAction       := iabBuy;
  AdvancedOrderType := atNone;
  OrderType         := otMarket;
  TimeInForce       := Integer(System.Low(TIABTimeInForce));
  Scope             := TOrderIBDoc.C_SCOPE_UNKNOW;
  FSokidInfo        := Default(TSokidInfo);
  BrokerType        := TBrokerType.brIB;

  FParametersStore := TParametersStore.Create;
  DMod.CheckConnect;
  FParametersStore.Connection := DMod.ConnectionStock;
  FParametersStore.StoreComponent := Self;

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
end;

procedure TfrmOrderTemplate.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FParametersStore);
  inherited;
end;

procedure TfrmOrderTemplate.FormShow(Sender: TObject);
begin
  inherited;
  pnlAuxPrice.Top            := 200;
  pnlLmtOffset.Top           := 200;
  pnlTrailStopPrice.Top      := 200;
  pnlLimitPrice.Top          := 200;
  rbChildOrderPriceBased.Top := 200;
end;

procedure TfrmOrderTemplate.rbBuyClick(Sender: TObject);
begin
  OrderAction := iabBuy;
  OnPercentChange(nil);
end;

procedure TfrmOrderTemplate.OnChildOrderPriceBasedChange(Sender: TObject);
begin
  inherited;
  if (FSubordination = suChildOrder) then
  begin
    cbAuxBasePrice.Enabled       := rbChildOrderPriceBased.Visible and (rbChildOrderPriceBased.ItemIndex = Ord(bpTickType));
    cbLimitBasePrice.Enabled     := rbChildOrderPriceBased.Visible and (rbChildOrderPriceBased.ItemIndex = Ord(bpTickType));
    cbTrailStopBasePrice.Enabled := rbChildOrderPriceBased.Visible and (rbChildOrderPriceBased.ItemIndex = Ord(bpTickType));
    cbLmtOffsetBasePrice.Enabled := rbChildOrderPriceBased.Visible and (rbChildOrderPriceBased.ItemIndex = Ord(bpTickType));
  end;
end;

procedure TfrmOrderTemplate.rbSellClick(Sender: TObject);
begin
  OrderAction := iabSell;
  OnPercentChange(nil);
end;

procedure TfrmOrderTemplate.LoadParamsFromXml;
begin
  cbOrderType.ItemIndex   := General.XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_ORDER_TYPE, -1);
  cbTimeInForce.ItemIndex := General.XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_TIME_IN_FORCE, -1);
  cbScope.ItemIndex       := General.XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_SCOPE_OF_ORDER, TOrderIBDoc.C_SCOPE_UNKNOW);
end;

procedure TfrmOrderTemplate.SaveParamsToXml;
begin
  try
    General.XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_ORDER_TYPE, Integer(OrderType));
    General.XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_TIME_IN_FORCE, cbTimeInForce.ItemIndex);
    General.XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_SCOPE_OF_ORDER, cbScope.ItemIndex);
  finally
    General.XMLFile.Save;
  end;
end;

procedure TfrmOrderTemplate.aSaveExecute(Sender: TObject);
begin
  inherited;
  ModalResult := mrOk;
end;

procedure TfrmOrderTemplate.cbOrderTypeChange(Sender: TObject);
begin
  if Showing then
  begin
    CheckSecureType;
    ChangeOrderType;
  end;
end;

procedure TfrmOrderTemplate.ChangeOrderType;

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
  pnlLimitPrice.Visible     := OrderType in [otLimit, otStopLimit];
  pnlAuxPrice.Visible       := OrderType in [otTrail, otTrailLimit, otStop, otStopLimit];
  pnlTrailStopPrice.Visible := OrderType in [otTrail, otTrailLimit];
  pnlLmtOffset.Visible      := OrderType in [otTrailLimit];

  seOcaGroupNumber.Enabled       := (cbScope.ItemIndex = TOrderIBDoc.C_SCOPE_ONE_CANCELS_ALL_OCA);
  lblAvailableSecureType.Caption := 'Available Security Types:   ' + OrderType.GetOrderProduct;
  pnlLimitPrice.Enabled          := True;
  edAuxPrice.MinValue            := -100;

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
        lblAuxPrice.Caption       := 'Trail Amount:';
        edAuxPrice.MinValue       := 0;
        lblTrailStopPrice.Caption := 'Trail Stop Price:';
      end;
    otPegMidPt:
      begin

      end;
  else
    begin
      lblAuxPrice.Caption := 'Price:';
      lblLmtPrice.Caption := 'Limit Price:';
    end;
  end;
  pnlAuxPrice.Top            := 200;
  pnlLmtOffset.Top           := 200;
  pnlTrailStopPrice.Top      := 200;
  pnlLimitPrice.Top          := 200;
  rbChildOrderPriceBased.Top := 200;

  FParametersStore.IdentityName := 'OrderBaseCustom-' + OrderType.ToString + '-' + OrderAction.ToString;
  FParametersStore.Restore;
  OnPercentChange(nil);
end;

procedure TfrmOrderTemplate.cbScopeChange(Sender: TObject);
begin
  if Showing then
    seOcaGroupNumber.Enabled := cbScope.ItemIndex = TOrderIBDoc.C_SCOPE_ONE_CANCELS_ALL_OCA;
end;

procedure TfrmOrderTemplate.cbSecureTypeChange(Sender: TObject);
begin
  inherited;
  if (cbSecureType.ItemIndex > -1) then
    FSecurityType := TIABSecurityType(cbSecureType.ItemIndex)
  else
    FSecurityType := stStock;
  CheckSecureType;
end;

procedure TfrmOrderTemplate.CheckSecureType;
resourcestring
  rsNotSupported = 'Ordertype not supported for this securitype';
begin
  if not OrderType.GetOrderProduct.Contains(SecurityType.ToString) then
    TMessageDialog.ShowWarning(rsNotSupported);
end;

procedure TfrmOrderTemplate.cbTimeInForceChange(Sender: TObject);
begin
  chbOrderStart.Enabled := (cbTimeInForce.ItemIndex = 4);
  chbOrderStop.Enabled  := (cbTimeInForce.ItemIndex = 4);
  if Showing and (cbTimeInForce.ItemIndex = Ord(TIABTimeInForce.tifIOC)) then
    cbScope.ItemIndex := TOrderIBDoc.C_SCOPE_FILL_OR_KILL;
end;

procedure TfrmOrderTemplate.chbOrderStartClick(Sender: TObject);
begin
  pnlOrderStart.Enabled := chbOrderStart.Checked;
end;

procedure TfrmOrderTemplate.chbOrderStopClick(Sender: TObject);
begin
  pnlOrderStop.Enabled := chbOrderStop.Checked;
end;

function TfrmOrderTemplate.GetBrokerType: TBrokerType;
begin
  if (cbBroker.ItemIndex > -1) then
    Result := TBrokerType.FromInteger(cbBroker.ItemIndex)
  else
    Result := TBrokerType.brIB;
end;

procedure TfrmOrderTemplate.SetBrokerType(const Value: TBrokerType);
begin
  if (cbBroker.ItemIndex <> Ord(Value)) then
  begin
    cbBroker.ItemIndex := Ord(Value);
    cpMain.ActiveCardIndex := Ord(Value);
  end;
end;

procedure TfrmOrderTemplate.cbBrokerChange(Sender: TObject);
begin
  inherited;
  if Showing then
    cpMain.ActiveCardIndex := cbBroker.ItemIndex;
end;

procedure TfrmOrderTemplate.SetDateStart(const Value: TDate);
begin
  dtpDateStart.Date := Value;
end;

function TfrmOrderTemplate.GetDateStart: TDate;
begin
  Result := dtpDateStart.Date;
end;

procedure TfrmOrderTemplate.SetDateStop(const Value: TDate);
begin
  dtpDateStop.Date := Value;
end;

function TfrmOrderTemplate.GetDateStop: TDate;
begin
  Result := dtpDateStop.Date;
end;

function TfrmOrderTemplate.GetDescription: string;
begin
  Result := string(edDescription.Text).Trim;
end;

procedure TfrmOrderTemplate.SetDescription(const Value: string);
begin
  edDescription.Text := Value;
end;

procedure TfrmOrderTemplate.SetExchange(const Value: string);
begin

end;

function TfrmOrderTemplate.GetExchange: string;
begin

end;

procedure TfrmOrderTemplate.SetExtendedOptions(const Value: TCustomOrderDoc.TExtendedOptions);

  function GetItemIndex(const aBasePrice: TBasePrice): Integer;
  var
    Index: Integer;
  begin
    Result := -1;
    Index := -1;
    for var BasePrice := High(TBasePrice) downto Low(TBasePrice) do
      case BasePrice of
        pbBid10 .. pbBid, pbAsk .. pbAsk10, pbLast .. pbLow:
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

  rbChildOrderPriceBased.ItemIndex := Ord(FExtendedOptions.BasisForPrice);
end;

function TfrmOrderTemplate.GetExtendedOptions: TCustomOrderDoc.TExtendedOptions;
begin
  if (cbAuxBasePrice.ItemIndex > -1) then
    FExtendedOptions.AuxBasePrice := TBasePrice(cbAuxBasePrice.Items.Objects[cbAuxBasePrice.ItemIndex]);
  if (cbLimitBasePrice.ItemIndex > -1) then
    FExtendedOptions.LimitBasePrice := TBasePrice(cbLimitBasePrice.Items.Objects[cbLimitBasePrice.ItemIndex]);
  if (cbLmtOffsetBasePrice.ItemIndex > -1) then
    FExtendedOptions.LmtOffsetBasePrice := TBasePrice(cbLmtOffsetBasePrice.Items.Objects[cbLmtOffsetBasePrice.ItemIndex]);
  if (cbTrailStopBasePrice.ItemIndex > -1) then
    FExtendedOptions.TrailStopBasePrice := TBasePrice(cbTrailStopBasePrice.Items.Objects[cbTrailStopBasePrice.ItemIndex]);

  if (rbChildOrderPriceBased.ItemIndex = 0) then
    FExtendedOptions.BasisForPrice := bpFillPrice
  else
    FExtendedOptions.BasisForPrice := bpTickType;
  Result := FExtendedOptions;
end;

procedure TfrmOrderTemplate.OnPercentChange(Sender: TObject);
begin
  inherited;
  if not Assigned(Sender) or (Sender = edAuxPrice) then
  begin
    edAuxPrice.Color := clWindow;
    if OrderType in [otTrail, otTrailLimit] then
    begin
      if (AuxPrice <= 0 )then
        edAuxPrice.Color := C_WRONG_VALUE_COLOUR;
    end
    else if OrderType in [otStopLimit] then
      case OrderAction of
        iabBuy:
          begin
            if (AuxPrice <= 0 )then
              edAuxPrice.Color := C_WRONG_VALUE_COLOUR;
          end;
        iabSell:
          begin
            if (AuxPrice >= 0) then
              edAuxPrice.Color := C_WRONG_VALUE_COLOUR;
          end;
      end
    else
      case OrderAction of
        iabBuy:
          begin
            if (AuxPrice <= 0) then
              edAuxPrice.Color := C_WRONG_VALUE_COLOUR;
          end;
        iabSell:
          begin
            if (AuxPrice >= 0) then
              edAuxPrice.Color := C_WRONG_VALUE_COLOUR;
          end;
      end;
  end;

  if not Assigned(Sender) or (Sender = edLimit) then
  begin
    edLimit.Color := clWindow;
    if OrderType in [otStopLimit, otTrailLimit] then
      case OrderAction of
        iabBuy:
          begin
            if (Limit <= 0) then
              edLimit.Color := C_WRONG_VALUE_COLOUR;
          end;
        iabSell:
          begin
            if (Limit >= 0) then
              edLimit.Color := C_WRONG_VALUE_COLOUR;
          end;
      end
    else
      case OrderAction of
        iabBuy:
          begin
            if (Limit >= 0) then
              edLimit.Color := C_WRONG_VALUE_COLOUR;
          end;
        iabSell:
          begin
            if (Limit <= 0) then
              edLimit.Color := C_WRONG_VALUE_COLOUR;
          end;
      end;
  end;

  if not Assigned(Sender) or (Sender = edTrailStopPrice) then
  begin
    edTrailStopPrice.Color := clWindow;
    case OrderAction of
      iabBuy:
        begin
          if (TrailStopPrice <= 0) then
            edTrailStopPrice.Color := C_WRONG_VALUE_COLOUR;
        end;
      iabSell:
        begin
          if (TrailStopPrice >= 0) then
            edTrailStopPrice.Color := C_WRONG_VALUE_COLOUR;
        end;
    end;
  end;

  if not Assigned(Sender) or (Sender = edLmtPriceOffset) then
  begin
    edLmtPriceOffset.Color := clWindow;
    if OrderType in [otTrailLimit] then
      if (LmtPriceOffset <= 0) then
        edLmtPriceOffset.Color := C_WRONG_VALUE_COLOUR;
  end;
end;

procedure TfrmOrderTemplate.OnBasePricesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ComboBox: TComboBox absolute Control;
  Canvas: TCanvas;
  BasePrice: TBasePrice;
begin
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
end;

procedure TfrmOrderTemplate.SetId(const Value: Integer);
begin
  FId := Value;
  if SokidList.ContainsKey(Value) then
    FSokidInfo := SokidList.Items[Value];
end;

procedure TfrmOrderTemplate.SetIsActivateChild(const Value: Boolean);
begin
  cbActivateChildOrder.Checked := Value;
end;

procedure TfrmOrderTemplate.SetIsFinal(const Value: Boolean);
begin
  chbOrderIsFinal.Checked := Value;
end;

function TfrmOrderTemplate.GetIsFinal: Boolean;
begin
  Result := chbOrderIsFinal.Checked;
end;

function TfrmOrderTemplate.GetIsActivateChild: Boolean;
begin
  Result := cbActivateChildOrder.Checked;
end;

function TfrmOrderTemplate.GetLimitPrice: Double;
begin
  Result := edLimit.ValueFloat;
end;

procedure TfrmOrderTemplate.SetLimitPrice(const Value: Double);
begin
  edLimit.ValueFloat := Value;
end;

procedure TfrmOrderTemplate.SetAuxPrice(const Value: Double);
begin
  edAuxPrice.ValueFloat := Value;
end;

function TfrmOrderTemplate.GetAuxPrice: Double;
begin
  Result := edAuxPrice.ValueFloat;
end;

function TfrmOrderTemplate.GetLmtPriceOffset: Double;
begin
  Result := edLmtPriceOffset.ValueFloat;
end;

procedure TfrmOrderTemplate.SetLmtPriceOffset(const Value: Double);
begin
  edLmtPriceOffset.ValueFloat := Value;
end;

procedure TfrmOrderTemplate.SetOrderAction(const Value: TIABAction);
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

function TfrmOrderTemplate.GetOrderAction: TIABAction;
begin
  Result := FAction;
end;

function TfrmOrderTemplate.GetAdvancedOrderType: TAdvancedOrderType;
begin
  Result := TAdvancedOrderType(cbAdvancedOrderType.ItemIndex);
end;

procedure TfrmOrderTemplate.SetAdvancedOrderType(const Value: TAdvancedOrderType);
begin
  cbAdvancedOrderType.ItemIndex := Integer(Value);
end;

function TfrmOrderTemplate.GetMaxNumAmount: Integer;
begin
  Result := seMaxNumAmount.Value;
end;

function TfrmOrderTemplate.GetMaxNumShares: Integer;
begin
  Result := seMaxNumShares.Value;
end;

procedure TfrmOrderTemplate.SetMaxNumAmount(const Value: Integer);
begin
  seMaxNumAmount.Value := Value;
end;

procedure TfrmOrderTemplate.SetMaxNumShares(const Value: Integer);
begin
  seMaxNumShares.Value := Value;
end;

function TfrmOrderTemplate.GetScope: Integer;
begin
  Result := cbScope.ItemIndex;
end;

procedure TfrmOrderTemplate.SetScope(const Value: Integer);
begin
  cbScope.ItemIndex := Value;
end;

procedure TfrmOrderTemplate.SetSecurityType(const Value: TIABSecurityType);
begin
  FSecurityType := Value;
  cbSecureType.ItemIndex := cbSecureType.Items.IndexOf(Value.ToString);
end;

procedure TfrmOrderTemplate.SetTimeInForce(const Value: Integer);
begin
  cbTimeInForce.ItemIndex := Value;
end;

function TfrmOrderTemplate.GetTimeInForce: Integer;
begin
  Result := cbTimeInForce.ItemIndex;
end;

procedure TfrmOrderTemplate.SetTimeStart(const Value: TTime);
begin
  dtpTimeStart.Time := Value;
end;

function TfrmOrderTemplate.GetTimeStart: TTime;
begin
  Result := dtpTimeStart.Time;
end;

procedure TfrmOrderTemplate.SetTimeStop(const Value: TTime);
begin
  dtpTimeStop.Time := Value;
end;

function TfrmOrderTemplate.GetTimeStop: TTime;
begin
  Result := dtpTimeStop.Time;
end;

procedure TfrmOrderTemplate.SetTrailStopPrice(const Value: Double);
begin
  edTrailStopPrice.ValueFloat := Value;
end;

function TfrmOrderTemplate.GetTriggerMethod: TTriggerMethod;
begin
  Result := Low(TTriggerMethod);
  if (cbTriggerMethod.ItemIndex > -1) then
    Result := TTriggerMethod(cbTriggerMethod.Items.Objects[cbTriggerMethod.ItemIndex]);
end;

procedure TfrmOrderTemplate.SetTriggerMethod(const Value: TTriggerMethod);
begin
  cbTriggerMethod.ItemIndex := cbTriggerMethod.Items.IndexOf(Value.ToString);
end;

function TfrmOrderTemplate.GetTrailStopPrice: Double;
begin
  Result := edTrailStopPrice.ValueFloat;
end;

procedure TfrmOrderTemplate.SetOrderStartEnabled(const Value: Boolean);
begin
  chbOrderStart.Checked := Value;
end;

function TfrmOrderTemplate.GetOrderStartEnabled: Boolean;
begin
  Result := chbOrderStart.Checked;
end;

procedure TfrmOrderTemplate.SetOrderStopEnabled(const Value: Boolean);
begin
  chbOrderStop.Checked := Value;
end;

function TfrmOrderTemplate.GetOrderStopEnabled: Boolean;
begin
  Result := chbOrderStop.Checked;
end;

procedure TfrmOrderTemplate.SetOrderType(const Value: TIABOrderType);
begin
  if (OrderType <> Value) then
    cbOrderType.ItemIndex := Ord(Value) - 1;
end;

function TfrmOrderTemplate.GetOrderType: TIABOrderType;
begin
  if (cbOrderType.ItemIndex > -1) then
    Result := TIABOrderType(cbOrderType.ItemIndex + 1)
  else
    Result := otUnknown;
end;

function TfrmOrderTemplate.GetOcaGroupNumber: Integer;
begin
  Result := seOcaGroupNumber.Value;
end;

procedure TfrmOrderTemplate.SetOcaGroupNumber(const Value: Integer);
begin
  seOcaGroupNumber.Value := Value;
end;

procedure TfrmOrderTemplate.AssignFromDoc(const aDocument: TCustomOrderDoc);
var
  OrderIB: TOrderIBDoc absolute aDocument;
  OrderNN: TOrderNNDoc absolute aDocument;
begin
  if Assigned(aDocument) then
  begin
    Self.BrokerType      := aDocument.BrokerType;
    Self.Description     := aDocument.Description;
    Self.OrderType       := aDocument.OrderType;
    Self.Symbol          := aDocument.Symbol;
    Self.Id              := aDocument.Id;
    Self.IsActivateChild := aDocument.IsActivateChild;
    Self.Exchange        := aDocument.Exchange;
    Self.Limit           := aDocument.Limit;
    Self.OrderAction     := aDocument.OrderAction;
    Self.ExtendedOptions := aDocument.ExtendedOptions;
    Self.TriggerMethod   := aDocument.TriggerMethod;
    Self.IsFinal         := aDocument.IsFinal;
//    Self.ColDay         := aDocument.ColDay;
//    Self.ColTick        := aDocument.ColTick;
//    Self.Currency       := aDocument.Currency;
//    Self.Decimals       := aDocument.Decimals;
//    Self.DirDay         := aDocument.DirDay;
//    Self.Filled         := 0;
//    Self.Info           := '';
//    Self.InstrumentName := aDocument.InstrumentName;
//    Self.IsExecuted     := False;
//    Self.IsRepetitive   := aDocument.IsRepetitive;
//    Self.OrderIBId      := 0;
//    Self.OrderStatus    := osSleeping;
//    Self.ParentOrder    := aDocument.ParentOrder;
//    Self.Quantity       := aDocument.Quantity;
//    Self.SignDay        := aDocument.SignDay;
//    Self.Symbol         := aDocument.Symbol;

    if (aDocument is TOrderIBDoc) then
    begin
      Self.AdvancedOrderType := OrderIB.AdvancedOrderType;
      Self.AuxPrice          := OrderIB.AuxPrice;
      Self.DateStart         := OrderIB.DateStart;
      Self.DateStop          := OrderIB.DateStop;
      Self.LmtPriceOffset    := OrderIB.LmtPriceOffset;
      Self.MaxNumAmount      := OrderIB.MaxNumAmount;
      Self.MaxNumShares      := OrderIB.MaxNumShares;
      Self.OcaGroupNumber    := OrderIB.OcaGroupNumber;
      Self.OrderAction       := OrderIB.OrderAction;
      Self.OrderStartEnabled := OrderIB.OrderStartEnabled;
      Self.OrderStopEnabled  := OrderIB.OrderStopEnabled;
      Self.OrderType         := OrderIB.OrderType;
      Self.OwnerNode         := OrderIB.OwnerNode;
      Self.Scope             := OrderIB.Scope;
      Self.SecurityType      := OrderIB.SecurityType;
      Self.Symbol            := OrderIB.Symbol;
      Self.TimeInForce       := OrderIB.TimeInForce;
      Self.TimeStart         := OrderIB.TimeStart;
      Self.TimeStop          := OrderIB.TimeStop;
      Self.TrailingPercent   := OrderIB.TrailingPercent;
      Self.TrailStopPrice    := OrderIB.TrailStopPrice;
    end
    else if (aDocument is TOrderNNDoc) then
    begin;

    end;
  end;
end;

procedure TfrmOrderTemplate.AssignToDoc(var aDocument: TCustomOrderDoc);
var
  OrderIB: TOrderIBDoc absolute aDocument;
  OrderNN: TOrderNNDoc absolute aDocument;
begin
  if Assigned(aDocument) then
  begin
    if (Self.BrokerType <> aDocument.BrokerType) then
    begin
      FreeAndNil(aDocument);
      case Self.BrokerType of
        brIB:
          aDocument := TOrderIBDoc.Create;
        brNN:
          aDocument := TOrderNNDoc.Create;
        brTest:
          aDocument := TOrderTestDoc.Create;
      end;
    end;
    NormalizationOrder;
    aDocument.BrokerType      := Self.BrokerType;
    aDocument.Description     := Self.Description;
    aDocument.Exchange        := Self.Exchange;
    aDocument.Id              := Self.Id;
    aDocument.Limit           := Self.Limit;
    aDocument.OrderAction     := Self.OrderAction;
    aDocument.OrderType       := Self.OrderType;
    aDocument.IsActivateChild := Self.IsActivateChild;
    aDocument.IsFinal         := Self.IsFinal;
    aDocument.Symbol          := Self.Symbol;
    aDocument.ExtendedOptions := Self.ExtendedOptions;
    aDocument.TriggerMethod   := Self.TriggerMethod;
    if (aDocument is TOrderIBDoc) then
    begin
      OrderIB.AdvancedOrderType := Self.AdvancedOrderType;
      OrderIB.AuxPrice          := Self.AuxPrice;
      OrderIB.DateStart         := Self.DateStart;
      OrderIB.DateStop          := Self.DateStop;
      OrderIB.LmtPriceOffset    := Self.LmtPriceOffset;
      OrderIB.MaxNumAmount      := Self.MaxNumAmount;
      OrderIB.MaxNumShares      := Self.MaxNumShares;
      OrderIB.OcaGroupNumber    := Self.OcaGroupNumber;
      OrderIB.OrderStartEnabled := Self.OrderStartEnabled;
      OrderIB.OrderStopEnabled  := Self.OrderStopEnabled;
      OrderIB.Scope             := Self.Scope;
      OrderIB.SecurityType      := Self.SecurityType;
      OrderIB.TimeInForce       := Self.TimeInForce;
      OrderIB.TimeStart         := Self.TimeStart;
      OrderIB.TimeStop          := Self.TimeStop;
      OrderIB.TrailingPercent   := Self.TrailingPercent;
      OrderIB.TrailStopPrice    := Self.TrailStopPrice;
    end
    else if (aDocument is TOrderNNDoc) then
    begin;

    end;
  end;
end;

procedure TfrmOrderTemplate.NormalizationOrder;
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
          Self.Limit           := 0;
          Self.LmtPriceOffset  := 0;
          Self.TrailingPercent := 0;
        end;
      otTrailLimit:
        begin
          Self.TrailingPercent := 0;
          Self.Limit           := 0;
        end;
    end;
end;

end.
