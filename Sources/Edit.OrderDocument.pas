unit Edit.OrderDocument;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Edit.Factor, Vcl.ExtCtrls, Vcl.ComCtrls, VirtualTrees,
  Vcl.Imaging.pngimage, IABfunctions, IABSocketAPI, Vcl.Samples.Spin, System.Math, System.Actions, Vcl.ActnList,
  DebugWriter, HtmlLib, Utils, System.UITypes, Global.Types, Monitor.Types, Document, MessageDialog,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const, Common.Types, InstrumentList, XmlFiles,
  Entity.Sokid, IABFunctions.MarketRules, ParametersStore, DaModule, DaImages, BrokerHelperAbstr, Vcl.WinXPanels,
  Vcl.NumberBox, Vcl.ControlList, Vcl.VirtualImage, Global.Resources, IABFunctions.Helpers, Search.Instruments,
  IABFunctions.MarketData;
{$ENDREGION}

type
  TfrmOrderDocument = class(TFormDocument)
    aCancel: TAction;
    ActionListOrder: TActionList;
    aSave: TAction;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    cbBroker: TComboBox;
    cpMain: TCardPanel;
    crdIB: TCard;
    crdNN: TCard;
    crdTest: TCard;
    edDescription: TEdit;
    lblBroker: TLabel;
    lblBuy: TLabel;
    lblDescription: TLabel;
    lblSell: TLabel;
    pnlBottom: TPanel;
    pnlIB: TPanel;
    pnlNN: TPanel;
    pnlTest: TPanel;
    pnlTop: TPanel;
    rbBuy: TRadioButton;
    rbSell: TRadioButton;
    lblOrderType: TLabel;
    cbOrderType: TComboBox;
    pnlLimitPrice: TPanel;
    lblLmtPrice: TLabel;
    edLimit: TNumberBox;
    cbLimitBasePrice: TComboBox;
    lblScope: TLabel;
    cbScope: TComboBox;
    edVisiblePart: TNumberBox;
    lblTrailStopPrice: TLabel;
    btnShowSearchForm: TBitBtn;
    aShowSearchInstruments: TAction;
    lblInstrument: TLabel;
    lblInstrumentName: TLabel;
    aClear: TAction;
    procedure aSaveExecute(Sender: TObject);
    procedure cbBrokerChange(Sender: TObject);
    procedure cbOrderTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnBasePricesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure OnPercentChange(Sender: TObject);
    procedure rbBuyClick(Sender: TObject);
    procedure rbSellClick(Sender: TObject);
    procedure aShowSearchInstrumentsExecute(Sender: TObject);
    procedure pnlIBDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pnlIBDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    FAction: TIABAction;
    FId: Integer;
    FParametersStore: TParametersStore;
    FSokidInfo: TSokidInfo;
    FSubordination: TSubordination;
    function GetBrokerType: TBrokerType;
    function GetDescription: string;
    function GetLimitPrice: Double;
    function GetOrderAction: TIABAction;
    function GetOrderType: TIABOrderType;
    function GetScope: Integer;

    procedure ChangeOrderType;
    procedure NormalizationOrder;

    procedure SetBrokerType(const Value: TBrokerType);
    procedure SetDescription(const Value: string);
    procedure SetId(const Value: Integer);
    procedure SetLimitPrice(const Value: Double);
    procedure SetOrderAction(const Value: TIABAction);
    procedure SetOrderType(const Value: TIABOrderType);
    procedure SetScope(const Value: Integer);
    function GetVisiblePart: Currency;
    procedure SetVisiblePart(const Value: Currency);
  public
    Instrument: TInstrument;
    procedure Initialize;
    procedure Deinitialize;
    procedure LoadParamsFromXml;
    procedure SaveParamsToXml;
    procedure AssignFromDoc(const aDocument: TCustomOrderDoc);
    procedure AssignToDoc(var aDocument: TCustomOrderDoc);
    procedure SetSokidInfo(aSokidInfo: TSokidInfo);
    class function ShowDocument(aDocument: TCustomOrderDoc; const aSubordination: TSubordination): TModalResult;

    property BrokerType        : TBrokerType         read GetBrokerType        write SetBrokerType;
    property Description       : string              read GetDescription       write SetDescription;
    property Id                : Integer             read FId                  write SetId;
    property Limit             : Double              read GetLimitPrice        write SetLimitPrice;
    property OrderAction       : TIABAction          read GetOrderAction       write SetOrderAction;
    property OrderType         : TIABOrderType       read GetOrderType         write SetOrderType;
    property Scope             : Integer             read GetScope             write SetScope;
    property VisiblePart       : Currency            read GetVisiblePart       write SetVisiblePart;
  end;

implementation

{$R *.dfm}

class function TfrmOrderDocument.ShowDocument(aDocument: TCustomOrderDoc; const aSubordination: TSubordination): TModalResult;
begin
  Result := mrCancel;
  if Assigned(aDocument) then
  begin
    with TfrmOrderDocument.Create(nil) do
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

procedure TfrmOrderDocument.SetSokidInfo(aSokidInfo: TSokidInfo);
begin
  Instrument.SokidInfo := aSokidInfo;
  if Instrument.SokidInfo.ContractId > 0 then
    lblInstrumentName.Caption := aSokidInfo.Name
  else
    lblInstrumentName.Caption := '<Replace from AutoOrder>';
end;

procedure TfrmOrderDocument.SetVisiblePart(const Value: Currency);
begin
  edVisiblePart.ValueFloat := Value;
end;

procedure TfrmOrderDocument.Initialize;
begin
  ChangeOrderType;
end;

procedure TfrmOrderDocument.Deinitialize;
begin
  FParametersStore.IdentityName := 'EditOrderIB-' + OrderType.ToString + '-' + OrderAction.ToString;
  FParametersStore.Store;
end;

procedure TfrmOrderDocument.FormCreate(Sender: TObject);
var
  ItemIndex: Integer;
begin
  inherited;
  for var ot := otMarket to otRelMktCombo do
    if ot in [otMarket,otLimit] then
      cbOrderType.Items.Add(ot.ToString);

  for var br := System.Low(TBrokerType) to System.High(TBrokerType) do
    cbBroker.Items.Add(br.ToString);

  OrderAction       := iabBuy;
  OrderType         := otMarket;
  Scope             := TOrderIBDoc.C_SCOPE_UNKNOW;
  FSokidInfo        := Default(TSokidInfo);
  BrokerType        := TBrokerType.brIB;

  FParametersStore := TParametersStore.Create;
  DMod.CheckConnect;
  FParametersStore.Connection := DMod.ConnectionStock;
  FParametersStore.StoreComponent := Self;

  ItemIndex := -1;
  cbLimitBasePrice.Items.BeginUpdate;
  try
    cbLimitBasePrice.Clear;
    for var BasePrice := High(TBasePrice) downto Low(TBasePrice) do
      case BasePrice of
        pbLast: ItemIndex := cbLimitBasePrice.Items.AddObject('LAST', TObject(Ord(BasePrice)));
        pbMid: cbLimitBasePrice.Items.AddObject('MID', TObject(Ord(BasePrice)));
        pbHigh: cbLimitBasePrice.Items.AddObject('HIGH', TObject(Ord(BasePrice)));
        pbLow: cbLimitBasePrice.Items.AddObject('LOW', TObject(Ord(BasePrice)));
        pbAsk: cbLimitBasePrice.Items.AddObject('ASK', TObject(Ord(BasePrice)));
        pbAsk1 .. pbAsk10: cbLimitBasePrice.Items.AddObject('ASK+' + Abs(Ord(BasePrice) - 100).ToString, TObject(Ord(BasePrice)));
        pbBid: cbLimitBasePrice.Items.AddObject('BID', TObject(Ord(BasePrice)));
        pbBid10 .. pbBid1: cbLimitBasePrice.Items.AddObject('BID-' + Abs(Ord(BasePrice) + 100).ToString, TObject(Ord(BasePrice)));
      end;
    cbLimitBasePrice.ItemIndex     := ItemIndex;
  finally
    cbLimitBasePrice.Items.EndUpdate;
  end;
end;

procedure TfrmOrderDocument.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FParametersStore);
  inherited;
end;

procedure TfrmOrderDocument.FormShow(Sender: TObject);
begin
  inherited;
  pnlLimitPrice.Top          := 200;
end;

procedure TfrmOrderDocument.rbBuyClick(Sender: TObject);
begin
  OrderAction := iabBuy;
  OnPercentChange(nil);
end;

procedure TfrmOrderDocument.rbSellClick(Sender: TObject);
begin
  OrderAction := iabSell;
  OnPercentChange(nil);
end;

procedure TfrmOrderDocument.LoadParamsFromXml;
begin
  cbOrderType.ItemIndex   := General.XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_ORDER_TYPE, -1);
  cbScope.ItemIndex       := General.XMLFile.ReadInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_SCOPE_OF_ORDER, TOrderIBDoc.C_SCOPE_UNKNOW);
end;

procedure TfrmOrderDocument.SaveParamsToXml;
begin
  try
    General.XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_ORDER_TYPE, Integer(OrderType));
    General.XMLFile.WriteInteger(TOrderIBDoc.C_SECTION_ORDER, TOrderIBDoc.C_KEY_SCOPE_OF_ORDER, cbScope.ItemIndex);
  finally
    General.XMLFile.Save;
  end;
end;

procedure TfrmOrderDocument.aSaveExecute(Sender: TObject);
begin
  inherited;
  ModalResult := mrOk;
end;

procedure TfrmOrderDocument.cbOrderTypeChange(Sender: TObject);
begin
  if Showing then
  begin
    ChangeOrderType;
  end;
end;

procedure TfrmOrderDocument.ChangeOrderType;

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
  pnlLimitPrice.Enabled     := True;

  pnlLimitPrice.Top          := 200;

  FParametersStore.IdentityName := 'OrderBaseCustom-' + OrderType.ToString + '-' + OrderAction.ToString;
  FParametersStore.Restore;
  OnPercentChange(nil);
end;

function TfrmOrderDocument.GetBrokerType: TBrokerType;
begin
  if (cbBroker.ItemIndex > -1) then
    Result := TBrokerType.FromInteger(cbBroker.ItemIndex)
  else
    Result := TBrokerType.brIB;
end;

procedure TfrmOrderDocument.SetBrokerType(const Value: TBrokerType);
begin
  if (cbBroker.ItemIndex <> Ord(Value)) then
  begin
    cbBroker.ItemIndex := Ord(Value);
    cpMain.ActiveCardIndex := Ord(Value);
  end;
end;

procedure TfrmOrderDocument.cbBrokerChange(Sender: TObject);
begin
  inherited;
  if Showing then
    cpMain.ActiveCardIndex := cbBroker.ItemIndex;
end;

function TfrmOrderDocument.GetDescription: string;
begin
  Result := string(edDescription.Text).Trim;
end;

procedure TfrmOrderDocument.SetDescription(const Value: string);
begin
  edDescription.Text := Value;
end;

procedure TfrmOrderDocument.OnPercentChange(Sender: TObject);
begin
  inherited;
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
end;

procedure TfrmOrderDocument.pnlIBDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  SourceNode: PVirtualNode;
  Data: PSokidInfo;
begin
  if Assigned(Source) and (Source is TVirtualStringTree) then
  begin
    SourceNode := TVirtualStringTree(Source).GetFirstSelected;
    if Assigned(SourceNode) then
    begin
      Data := SourceNode^.GetData;
      if (Sender = pnlIB) then
      begin
        if (Instrument.SokidInfo.ContractId > 0) then
          TIABMarket.CancelMarketData(Instrument.SokidInfo.ContractId);
      end;
      TIABMarket.RequestMarketData(Data^.ContractId);
      SetSokidInfo(Data^);
    end;
  end;
end;

procedure TfrmOrderDocument.pnlIBDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited;
  Accept := Assigned(Source) and (Source is TVirtualStringTree);
end;

procedure TfrmOrderDocument.OnBasePricesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
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

procedure TfrmOrderDocument.SetId(const Value: Integer);
begin
  FId := Value;
end;

function TfrmOrderDocument.GetLimitPrice: Double;
begin
  Result := edLimit.ValueFloat;
end;

procedure TfrmOrderDocument.SetLimitPrice(const Value: Double);
begin
  edLimit.ValueFloat := Value;
end;

procedure TfrmOrderDocument.SetOrderAction(const Value: TIABAction);
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

function TfrmOrderDocument.GetOrderAction: TIABAction;
begin
  Result := FAction;
end;

function TfrmOrderDocument.GetScope: Integer;
begin
  Result := cbScope.ItemIndex;
end;

function TfrmOrderDocument.GetVisiblePart: Currency;
begin
  Result := edVisiblePart.ValueFloat;
end;

procedure TfrmOrderDocument.SetScope(const Value: Integer);
begin
  cbScope.ItemIndex := Value;
end;

procedure TfrmOrderDocument.SetOrderType(const Value: TIABOrderType);
begin
  if (OrderType <> Value) then
    cbOrderType.ItemIndex := Ord(Value) - 1;
end;

function TfrmOrderDocument.GetOrderType: TIABOrderType;
begin
  if (cbOrderType.ItemIndex > -1) then
    Result := TIABOrderType(cbOrderType.ItemIndex + 1)
  else
    Result := otUnknown;
end;

procedure TfrmOrderDocument.aShowSearchInstrumentsExecute(Sender: TObject);
begin
  inherited;
  TfrmSearchInstruments.ShowDocument(Self);
end;

procedure TfrmOrderDocument.AssignFromDoc(const aDocument: TCustomOrderDoc);
var
  OrderIB: TOrderIBDoc absolute aDocument;
  OrderNN: TOrderNNDoc absolute aDocument;
begin
  if Assigned(aDocument) then
  begin
    Self.BrokerType      := aDocument.BrokerType;
    Self.Description     := aDocument.Description;
    Self.OrderType       := aDocument.OrderType;
    Self.Id              := aDocument.Id;
    Self.Limit           := aDocument.Limit;
    Self.OrderAction     := aDocument.OrderAction;
    Self.VisiblePart     := aDocument.VisiblePart;
    SetSokidInfo(aDocument.Instrument.SokidInfo);
    if (aDocument is TOrderIBDoc) then
    begin
      Self.OrderAction       := OrderIB.OrderAction;
      Self.OrderType         := OrderIB.OrderType;
      Self.OwnerNode         := OrderIB.OwnerNode;
      Self.Scope             := OrderIB.Scope;
    end
    else if (aDocument is TOrderNNDoc) then
    begin;

    end;
  end;
end;

procedure TfrmOrderDocument.AssignToDoc(var aDocument: TCustomOrderDoc);
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
    aDocument.Id              := Self.Id;
    aDocument.Limit           := Self.Limit;
    aDocument.OrderAction     := Self.OrderAction;
    aDocument.OrderType       := Self.OrderType;
    aDocument.VisiblePart     := Self.VisiblePart;
    aDocument.Instrument.AssignFrom(Self.Instrument);
    if (aDocument is TOrderIBDoc) then
    begin
      OrderIB.Scope             := Self.Scope;
    end
    else if (aDocument is TOrderNNDoc) then
    begin;

    end;
  end;
end;

procedure TfrmOrderDocument.NormalizationOrder;
begin
    case Self.OrderType of
      otMarket, otMarketOpen:
        begin
          Self.Limit           := 0;
        end;
      otLimit:
        begin
        end;
      otStop, otPegMarket, otPegPrimary, otPegMidPt:
        begin
          Self.Limit           := 0;
        end;
      otStopLimit:
        begin
        end;
      otTrail:
        begin
          Self.Limit           := 0;
        end;
      otTrailLimit:
        begin
          Self.Limit           := 0;
        end;
    end;
end;

end.
