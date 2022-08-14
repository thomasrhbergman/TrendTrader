{ Nordnet External API - REST services Revision 1.16
  https://api.test.nordnet.se/projects/api/wiki/REST_API_documentation
  https://api.test.nordnet.se/api-docs/index.html
}
unit Edit.OrderNN;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Edit.Factor, Vcl.ExtCtrls, Vcl.ComCtrls, VirtualTrees,
  Vcl.Imaging.pngimage, Vcl.Samples.Spin, System.Math, System.Actions, Vcl.ActnList, BrokerHelperAbstr, DebugWriter,
  HtmlLib, Utils, System.UITypes, Document, Global.Types, IABSocketAPI, Search.InstrumentsNN, DaModule,
  Monitor.Types, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABSocketAPI_const, MessageDialog, InstrumentList,
  Monitor.Interfaces, Common.Types, DaImages, Publishers, MonitorTree.Document, System.Types;
{$ENDREGION}

type
  TfrmEditOrderNN = class(TFormDocument)
    aBuy: TAction;
    aCancelOrder: TAction;
    ActionListOrder: TActionList;
    aSave: TAction;
    aSelectInstrument: TAction;
    btnBuy: TBitBtn;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    btnSelectInstrument: TButton;
    cbActivateChildForRecievedPart: TCheckBox;
    cbActiveReached: TCheckBox;
    cbActiveStopLoss: TCheckBox;
    cbFeedFrom: TComboBox;
    dtValidUntil: TDateTimePicker;
    edLimit: TEdit;
    edtConditionReached: TEdit;
    edtConditionStopLoss: TEdit;
    edtDescription: TEdit;
    edtReference: TEdit;
    edtTrailTriggerLimit: TEdit;
    edtTrailTriggerSendSell: TEdit;
    gbChildOrder: TGroupBox;
    gbCurrentValues: TGroupBox;
    gbIceberg: TGroupBox;
    gbLimitType: TGroupBox;
    gbOrderTypes: TGroupBox;
    gbReached: TGroupBox;
    gbReference: TGroupBox;
    gbStopLoss: TGroupBox;
    gbTimeInForce: TGroupBox;
    gbTrailTrigger: TGroupBox;
    gbTypeOfSendOrder: TGroupBox;
    lblBuy: TLabel;
    lblChildCondFeed: TLabel;
    lblChildConditionParams: TLabel;
    lblChildOrderType: TLabel;
    lblDescriptionCaption: TLabel;
    lblEntryPrice: TLabel;
    lblEntryPriceCaption: TLabel;
    lblFeedFrom: TLabel;
    lblFromLastPayed: TLabel;
    lblChildInstrumentName: TLabel;
    lblInstrumentCaption: TLabel;
    lblInstrumentName: TLabel;
    lblInstrumentSymbol: TLabel;
    lblLastPayed: TLabel;
    lblLastPayedCaption: TLabel;
    lblMaxPayed: TLabel;
    lblMaxPayedCaption: TLabel;
    lblMinPayed: TLabel;
    lblMinPayedCaption: TLabel;
    lblPartOfOrderCaption: TLabel;
    lblSell: TLabel;
    lblSingleTrailOrderParams: TLabel;
    lblStatus: TLabel;
    lblStatusCaption: TLabel;
    lblTrailSendSellCaption: TLabel;
    lblTrailTriggerLimitCaption: TLabel;
    lblVolumeCaption: TLabel;
    memoInfo: TMemo;
    pnlBottom: TPanel;
    pnlCenterCurrentValues: TPanel;
    pnlCenterOrderType: TPanel;
    pnlChildConditionParams: TPanel;
    pnlChildOrderType: TPanel;
    pnlChildParams: TPanel;
    pnlFeedFrom: TPanel;
    pnlIceberg: TPanel;
    pnlLimit: TPanel;
    pnlSingleTrailOrderParams: TPanel;
    pnlTop: TPanel;
    pnlTrail: TPanel;
    rbBuy: TRadioButton;
    rbCombinationTrailOrderLMT: TRadioButton;
    rbCombinationTrailOrderMRK: TRadioButton;
    rbLimitOrder: TRadioButton;
    rbLimitTypeFix: TRadioButton;
    rbLimitTypeRelative: TRadioButton;
    rbMarketOrder: TRadioButton;
    rbSell: TRadioButton;
    rbTimeInForceToday: TRadioButton;
    rbTimeInForceUntil: TRadioButton;
    rbTrailTriggerLimitOrder: TRadioButton;
    rbTrailTriggerMarketOrder: TRadioButton;
    seOpenVolume: TSpinEdit;
    seVolume: TSpinEdit;
    lblAvgPrice: TLabel;
    procedure aBuyExecute(Sender: TObject);
    procedure aBuyUpdate(Sender: TObject);
    procedure aCancelOrderExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
    procedure aSelectInstrumentExecute(Sender: TObject);
    procedure aSelectInstrumentUpdate(Sender: TObject);
    procedure cbActiveReachedClick(Sender: TObject);
    procedure cbActiveStopLossClick(Sender: TObject);
    procedure cbFeedFromChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
    procedure OnKeyPressLessZero(Sender: TObject; var Key: Char);
    procedure OnKeyPressOverZero(Sender: TObject; var Key: Char);
    procedure OnLimitTypeChange(Sender: TObject);
    procedure OnOrderTypeChange(Sender: TObject);
    procedure rbBuyClick(Sender: TObject);
    procedure rbTimeInForceUntilClick(Sender: TObject);
    procedure rbTrailTriggerLimitOrderClick(Sender: TObject);
  private const
    C_DECIMALS = 2;
  private
    FAvgPrice: Double;
    FChildId: Integer;
    FChildIdentifierList: string;
    FChildMarketList: string;
    FCurrency: string;
    FDecimals: Integer;
    FChildExchange: string;
    FFilled: Integer;
    FId: Integer;
    FIdentifierList: string;
    FInfo: string;
    FIsExecuted: Boolean;
    FIsRepetitive: Boolean;
    FMarketList: string;
    FOrderId: Integer;
    FOrderStatus: TIABOrderState;
    FOrderType: TIABOrderType;
    FPrice: Double;
    FStatus: string;
    FValueArray: TOrderNNDoc.TValueArray;
    FSubordination: TSubordination;
    function GetActivateChild: Boolean;
    function GetBrokerType: Integer;
    function GetConditionReached: Double;
    function GetConditionStopLoss: Double;
    function GetDescription: string;
    function GetFeedFromBroker: Integer;
    function GetInstrumentName: string;
    function GetIsActiveReached: Boolean;
    function GetIsActiveStopLoss: Boolean;
    function GetLimit: Double;
    function GetLimitType: TOrderNNDoc.TLimitType;
    function GetOpenVolume: Integer;
    function GetOrderAction: TIABAction;
    function GetOrderType: TIABOrderType;
    function GetQuantity: Integer;
    function GetReference: string;
    function GetSymbol: string;
    function GetChildSymbol: string;
    function GetTrailOrderSendType: TIABOrderType;
    function GetTrailTriggerLimit: Double;
    function GetTrailTriggerSendSell: Double;
    function GetValidUntil: string;
    function GetValueArrayItem(Index: TOrderNNDoc.TCurrentValue): Double;
    procedure AssignToDocMainParams(var aDocument: TOrderNNDoc);
    procedure ClearCurrentValue;
    procedure LoadParamsFromXml;
    procedure SaveParamsToXml;
    procedure SetActivateChild(const Value: Boolean);
    procedure SetAvgPrice(const Value: Double);
    procedure SetConditionReached(const Value: Double);
    procedure SetConditionStopLoss(const Value: Double);
    procedure SetDescription(const Value: string);
    procedure SetFeedFromBroker(const Value: Integer);
    procedure SetFilled(const Value: Integer);
    procedure SetInfo(const Value: string);
    procedure SetInstrumentName(const Value: string);
    procedure SetIsActiveReached(const Value: Boolean);
    procedure SetIsActiveStopLoss(const Value: Boolean);
    procedure SetLimit(const Value: Double);
    procedure SetLimitType(const Value: TOrderNNDoc.TLimitType);
    procedure SetOpenVolume(const Value: Integer);
    procedure SetOrderAction(const Value: TIABAction);
    procedure SetOrderStatus(const Value: TIABOrderState);
    procedure SetOrderType(const Value: TIABOrderType);
    procedure SetQuantity(const Value: Integer);
    procedure SetReference(const Value: string);
    procedure SetSymbol(const Value: string);
    procedure SetChildSymbol(const Value: string);
    procedure SetTrailOrderSendType(const Value: TIABOrderType);
    procedure SetTrailTriggerLimit(const Value: Double);
    procedure SetTrailTriggerSendSell(const Value: Double);
    procedure SetValidUntil(const Value: string);
    procedure SetValueArrayItem(Index: TOrderNNDoc.TCurrentValue; const Value: Double);
  public
    procedure AssignFromDoc(const aDocument: TOrderNNDoc);
    procedure AssignToDoc(var aDocument: TOrderNNDoc);
    class function ShowDocument(aDocument: TOrderNNDoc; const aSubordination: TSubordination): TModalResult;

    property IsActivateChild      : Boolean                read GetActivateChild        write SetActivateChild;
    property AvgPrice             : Double                 read FAvgPrice               write SetAvgPrice;
    property BrokerType           : Integer                read GetBrokerType;
    property ChildExchange        : string                 read FChildExchange          write FChildExchange;
    property ChildId              : Integer                read FChildId                write FChildId;
    property ChildIdentifierList  : string                 read FChildIdentifierList    write FChildIdentifierList;
    property ChildMarketList      : string                 read FChildMarketList        write FChildMarketList;
    property ChildSymbol          : string                 read GetChildSymbol          write SetChildSymbol;
    property ConditionReached     : Double                 read GetConditionReached     write SetConditionReached;
    property ConditionStopLoss    : Double                 read GetConditionStopLoss    write SetConditionStopLoss;
    property Currency             : string                 read FCurrency               write FCurrency;
    property Decimals             : Integer                read FDecimals               write FDecimals;
    property Description          : string                 read GetDescription          write SetDescription;
    property FeedFromBroker       : Integer                read GetFeedFromBroker       write SetFeedFromBroker;
    property Filled               : Integer                read FFilled                 write SetFilled;
    property Id                   : Integer                read FId                     write FId;
    property IdentifierList       : string                 read FIdentifierList         write FIdentifierList;
    property Info                 : string                 read FInfo                   write SetInfo;
    property InstrumentName       : string                 read GetInstrumentName       write SetInstrumentName;
    property IsActiveReached      : Boolean                read GetIsActiveReached      write SetIsActiveReached;
    property IsActiveStopLoss     : Boolean                read GetIsActiveStopLoss     write SetIsActiveStopLoss;
    property IsExecuted           : Boolean                read FIsExecuted             write FIsExecuted;
    property IsRepetitive         : Boolean                read FIsRepetitive           write FIsRepetitive;
    property Limit                : Double                 read GetLimit                write SetLimit;
    property LimitType            : TOrderNNDoc.TLimitType read GetLimitType            write SetLimitType;
    property MarketList           : string                 read FMarketList             write FMarketList;
    property OpenVolume           : Integer                read GetOpenVolume           write SetOpenVolume;
    property OrderAction          : TIABAction             read GetOrderAction          write SetOrderAction;
    property OrderIBId            : Integer                read FOrderId                write FOrderId;
    property OrderStatus          : TIABOrderState         read FOrderStatus            write SetOrderStatus;
    property OrderType            : TIABOrderType          read GetOrderType            write SetOrderType;
    property Price                : Double                 read FPrice                  write FPrice;
    property Quantity             : Integer                read GetQuantity             write SetQuantity;
    property Reference            : string                 read GetReference            write SetReference;
    property Symbol               : string                 read GetSymbol               write SetSymbol;
    property TrailOrderSendType   : TIABOrderType          read GetTrailOrderSendType   write SetTrailOrderSendType;
    property TrailTriggerLimit    : Double                 read GetTrailTriggerLimit    write SetTrailTriggerLimit;
    property TrailTriggerSendSell : Double                 read GetTrailTriggerSendSell write SetTrailTriggerSendSell;
    property ValidUntil           : string                 read GetValidUntil           write SetValidUntil;
    property ValueArray[Index: TOrderNNDoc.TCurrentValue]: Double read GetValueArrayItem write SetValueArrayItem;
  end;

var
  frmEditOrderNN: TfrmEditOrderNN;

implementation

{$R *.dfm}

resourcestring
  C_ENTRY_PRICE_CAPTION_AVG         = 'Avg mother price:';
  C_ENTRY_PRICE_CAPTION_ENTRY       = 'Entry price:';
  C_GBTRAIL_CAPTION                 = '%s (Fixed value, stop loss)';
  C_INFO_CAPTION_LIMIT_ORDER        = 'Limit-order parameters';
  C_INFO_CAPTION_MARKET_ORDER       = 'Market-order parameters';
  C_INFO_CAPTION_SINGLE_TRAIL_ORDER = 'Single trail-order parameters';
  C_INFO_CAPTION_TRAIL_ORDER        = 'Child trail-order parameters';

  C_TRIGGER_SEND_CAPTION = 'Send %s order when last payed is from %s payed:';
  C_REACHED_SEND_CAPTION = '%s when reached from entry price:';



class function TfrmEditOrderNN.ShowDocument(aDocument: TOrderNNDoc; const aSubordination: TSubordination): TModalResult;
var
  Monitor: IMonitor;
begin
  Result := mrCancel;
  if Assigned(aDocument) then
  begin
    frmEditOrderNN := TfrmEditOrderNN.Create(nil);
    try
      frmEditOrderNN.FSubordination := aSubordination;
      frmEditOrderNN.AssignFromDoc(aDocument);
      if (frmEditOrderNN.ShowModal = mrOk) then
      begin
        Result := mrOk;
        frmEditOrderNN.SaveParamsToXml;
        frmEditOrderNN.AssignToDoc(aDocument);
        if Supports(Application.MainForm, IMonitor, Monitor) then
          TTreeDocument.SetIcon(aDocument.OwnerNode, Monitor.GetMainTree);
      end;
    finally
      FreeAndNil(frmEditOrderNN);
    end;
  end;
end;

procedure TfrmEditOrderNN.FormCreate(Sender: TObject);
begin
  inherited;
  FOrderStatus  := osSleeping;
  FOrderType    := otMarket;

  cbFeedFrom.Items.Clear;
  cbFeedFrom.Items.Add(TBrokerType.brIB.ToString);
  cbFeedFrom.Items.Add(TBrokerType.brNN.ToString);
  cbFeedFrom.ItemIndex := 0;

  dtValidUntil.Date := Date;

  LoadParamsFromXml;
  OnOrderTypeChange(rbMarketOrder);
end;

procedure TfrmEditOrderNN.FormShow(Sender: TObject);
begin
  Left := Application.MainForm.Left + (Application.MainForm.Width div 2) - (Width div 2);
  Top  := Application.MainForm.Top + (Application.MainForm.Height div 2) - (Height div 2);
end;

procedure TfrmEditOrderNN.OnOrderTypeChange(Sender: TObject);
begin
  inherited;
  pnlLimit.Visible := False;
  pnlChildParams.Visible := False;

  if (Sender = rbMarketOrder) then
  begin
    pnlLimit.Visible := False;
    gbCurrentValues.Visible := False;
  end
  else if (Sender = rbLimitOrder) then
  begin
    pnlLimit.Visible := True;
    gbCurrentValues.Visible := False;
  end
  else if (Sender = rbCombinationTrailOrderMRK) then
  begin
    gbCurrentValues.Visible := True;
    gbChildOrder.Visible := FSubordination = suMotherOrder;
    pnlChildParams.Visible := True;
  end
  else if (Sender = rbCombinationTrailOrderLMT) then
  begin
    gbCurrentValues.Visible := True;
    gbChildOrder.Visible := FSubordination = suMotherOrder;
    pnlChildParams.Visible := True;
    pnlLimit.Visible := True;
  end;

  if (FSubordination = suChildOrder) then
  begin
    lblEntryPriceCaption.Caption := C_ENTRY_PRICE_CAPTION_AVG;
    rbCombinationTrailOrderLMT.Enabled := False;
    rbCombinationTrailOrderMRK.Enabled := False;
    if not(OrderType in [otMarket, otLimit]) then
      OrderType := otMarket;
  end
  else
  begin
    lblEntryPriceCaption.Caption := C_ENTRY_PRICE_CAPTION_ENTRY;
    rbCombinationTrailOrderLMT.Enabled := True;
    rbCombinationTrailOrderMRK.Enabled := True;
  end;
end;

procedure TfrmEditOrderNN.OnKeyPress(Sender: TObject; var Key: Char);
begin
  if not(CharInSet(Key, ['0' .. '9', '+', '-', #08, FormatSettings.DecimalSeparator])) then
    Key := #0;
end;

procedure TfrmEditOrderNN.OnKeyPressLessZero(Sender: TObject; var Key: Char);
var
  CharSet: TSysCharSet;
  LKey: AnsiChar;
begin
  LKey := #0;
  CharSet := ['0' .. '9', #08, FormatSettings.DecimalSeparator];
  case OrderAction of
    iabBuy:
      LKey := '-';
    iabSell:
      LKey := '+';
  end;
  Include(CharSet, LKey);
  if not(CharInSet(Key, CharSet)) then
    Key := #0;
end;

procedure TfrmEditOrderNN.OnKeyPressOverZero(Sender: TObject; var Key: Char);
var
  CharSet: TSysCharSet;
  LKey: AnsiChar;
begin
  LKey := #0;
  CharSet := ['0' .. '9', #08, FormatSettings.DecimalSeparator];
  case OrderAction of
    iabBuy:
      LKey := '+';
    iabSell:
      LKey := '-';
  end;
  Include(CharSet, LKey);
  if not(CharInSet(Key, CharSet)) then
    Key := #0;
end;


procedure TfrmEditOrderNN.LoadParamsFromXml;
begin
  // cbOrderType.ItemIndex   := Global.XMLFile.ReadInteger(TOrderDoc.C_SECTION_ORDER, TOrderDoc.C_KEY_ORDER_TYPE, -1);
  // cbTimeInForce.ItemIndex := Global.XMLFile.ReadInteger(TOrderDoc.C_SECTION_ORDER, TOrderDoc.C_KEY_TIME_IN_FORCE, -1);
  // cbScope.ItemIndex       := Global.XMLFile.ReadInteger(TOrderDoc.C_SECTION_ORDER, TOrderDoc.C_KEY_SCOPE_OF_ORDER, TOrderDoc.C_SCOPE_UNKNOW);
end;

procedure TfrmEditOrderNN.SaveParamsToXml;
begin
  // if cbMakeThisOrderDefault.Checked then
  // try
  // Global.XMLFile.WriteInteger(TOrderDoc.C_SECTION_ORDER, TOrderDoc.C_KEY_ORDER_TYPE, cbOrderType.ItemIndex);
  // Global.XMLFile.WriteInteger(TOrderDoc.C_SECTION_ORDER, TOrderDoc.C_KEY_TIME_IN_FORCE, cbTimeInForce.ItemIndex);
  // Global.XMLFile.WriteInteger(TOrderDoc.C_SECTION_ORDER, TOrderDoc.C_KEY_SCOPE_OF_ORDER, cbScope.ItemIndex);
  // finally
  // Global.XMLFile.Save;
  // end;
end;

procedure TfrmEditOrderNN.SetOrderStatus(const Value: TIABOrderState);
var
  Data: PTreeData;
  Monitor: IMonitor;
begin
  FOrderStatus := Value;

  case Value of
    osError:
      lblStatus.Font.Color := clRed;
    osSleeping:
      lblStatus.Font.Color := clMaroon;
    osSubmitted:
      lblStatus.Font.Color := clGreen;
  else
    lblStatus.Font.Color := clBlack;
  end;
  if (Value = osSleeping) and IsExecuted then
    lblStatus.Caption := 'Executed'
  else
    lblStatus.Caption := TOrderNNDoc.OrderNNStateString[Value];
  FStatus := '%d/%d ' + lblStatus.Caption;

  if Assigned(OwnerNode) and Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    TTreeDocument.SetIcon(OwnerNode, Monitor.GetMainTree);
    Data := OwnerNode^.GetData;
    if Assigned(Data) then
      Data.Enabled := not(Value in [osFilled, osCancelled, osSubmitted]);
  end;
end;

procedure TfrmEditOrderNN.SetOrderType(const Value: TIABOrderType);
var
  RadioButton: TRadioButton;
begin
  RadioButton := nil;
  if (Value in [otMarket, otLimit, otComboMarket, otComboLimit]) then
  begin
    FOrderType := Value;
    case FOrderType of
      otMarket:
        RadioButton := rbMarketOrder;
      otLimit:
        RadioButton := rbLimitOrder;
      otComboMarket:
        RadioButton := rbCombinationTrailOrderMRK;
      otComboLimit:
        RadioButton := rbCombinationTrailOrderLMT;
    end;
    if Assigned(RadioButton) then
    begin
      RadioButton.Checked := True;
      OnOrderTypeChange(RadioButton);
    end;
  end;
end;

function TfrmEditOrderNN.GetOrderType: TIABOrderType;
begin
  if rbMarketOrder.Checked then
    FOrderType := otMarket
  else if rbLimitOrder.Checked then
    FOrderType := otLimit
  else if rbCombinationTrailOrderMRK.Checked then
    FOrderType := otComboMarket
  else if rbCombinationTrailOrderLMT.Checked then
    FOrderType := otComboLimit;
  Result := FOrderType;
end;

function TfrmEditOrderNN.GetActivateChild: Boolean;
begin
  Result := cbActivateChildForRecievedPart.Checked;
end;

function TfrmEditOrderNN.GetBrokerType: Integer;
begin
  Result := 1;
end;

procedure TfrmEditOrderNN.SetActivateChild(const Value: Boolean);
begin
  cbActivateChildForRecievedPart.Checked := Value;
end;

procedure TfrmEditOrderNN.SetAvgPrice(const Value: Double);
begin
  FAvgPrice := Value;
  lblAvgPrice.Caption := Format('Avg: %f', [Value]);
end;

function TfrmEditOrderNN.GetDescription: string;
begin
  Result := edtDescription.Text;
end;

function TfrmEditOrderNN.GetFeedFromBroker: Integer;
begin
  Result := -1;
  if (cbFeedFrom.ItemIndex > -1) then
    Result := cbFeedFrom.ItemIndex;
end;

procedure TfrmEditOrderNN.SetFeedFromBroker(const Value: Integer);
begin
  cbFeedFrom.ItemIndex := Value;
end;

procedure TfrmEditOrderNN.SetFilled(const Value: Integer);
begin
  FFilled := Value;
  lblStatus.Caption := Format(FStatus, [Value, Quantity]);
end;

function TfrmEditOrderNN.GetIsActiveReached: Boolean;
begin
  Result := cbActiveReached.Checked;
end;

procedure TfrmEditOrderNN.SetIsActiveReached(const Value: Boolean);
begin
  cbActiveReached.Checked := Value;
  edtConditionReached.Enabled := Value;
end;

function TfrmEditOrderNN.GetIsActiveStopLoss: Boolean;
begin
  Result := cbActiveStopLoss.Checked;
end;

procedure TfrmEditOrderNN.SetIsActiveStopLoss(const Value: Boolean);
begin
  cbActiveStopLoss.Checked := Value;
  edtConditionStopLoss.Enabled := Value;
end;

procedure TfrmEditOrderNN.SetDescription(const Value: string);
begin
  edtDescription.Text := Value;
end;

function TfrmEditOrderNN.GetReference: string;
begin
  Result := edtReference.Text;
end;

function TfrmEditOrderNN.GetInstrumentName: string;
begin
  Result := lblInstrumentName.Caption;
end;

procedure TfrmEditOrderNN.SetInfo(const Value: string);
begin
  FInfo := Value;
  memoInfo.Lines.Add(Value);
end;

procedure TfrmEditOrderNN.SetInstrumentName(const Value: string);
begin
  lblInstrumentName.Caption := Value;
end;

function TfrmEditOrderNN.GetSymbol: string;
begin
  Result := lblInstrumentSymbol.Caption;
end;

procedure TfrmEditOrderNN.SetSymbol(const Value: string);
begin
  lblInstrumentSymbol.Caption := Value;
end;

function TfrmEditOrderNN.GetChildSymbol: string;
begin
  Result := lblChildInstrumentName.Caption;
end;

procedure TfrmEditOrderNN.SetChildSymbol(const Value: string);
begin
  lblChildInstrumentName.Caption := Value;
end;

function TfrmEditOrderNN.GetTrailTriggerLimit: Double;
begin
  Result := SimpleRoundTo(StrToFloatDef(edtTrailTriggerLimit.Text, 0), -C_DECIMALS);
end;

procedure TfrmEditOrderNN.SetTrailTriggerLimit(const Value: Double);
begin
  edtTrailTriggerLimit.Text := SimpleRoundTo(Abs(Value), -C_DECIMALS).ToString;
  if (Value > 0) then
    edtTrailTriggerLimit.Text := '+' + edtTrailTriggerLimit.Text
  else if (Value < 0) then
    edtTrailTriggerLimit.Text := '-' + edtTrailTriggerLimit.Text;
end;

function TfrmEditOrderNN.GetTrailOrderSendType: TIABOrderType;
begin
  if rbTrailTriggerMarketOrder.Checked then
    Result := otMarket
  else
    Result := otLimit;
end;

procedure TfrmEditOrderNN.SetTrailOrderSendType(const Value: TIABOrderType);
begin
  case Value of
    otMarket:
      begin
        rbTrailTriggerMarketOrder.Checked := True;
        edtTrailTriggerLimit.Enabled := False;
      end;
    otLimit:
      begin
        rbTrailTriggerLimitOrder.Checked := True;
        edtTrailTriggerLimit.Enabled := True;
      end;
  end;
end;

function TfrmEditOrderNN.GetTrailTriggerSendSell: Double;
begin
  Result := 0;
  case OrderAction of
    iabBuy:
      Result := -Abs(SimpleRoundTo(StrToFloatDef(edtTrailTriggerSendSell.Text, 0), -C_DECIMALS));
    iabSell:
      Result := Abs(SimpleRoundTo(StrToFloatDef(edtTrailTriggerSendSell.Text, 0), -C_DECIMALS));
  end;
end;

procedure TfrmEditOrderNN.SetTrailTriggerSendSell(const Value: Double);
begin
  edtTrailTriggerSendSell.Text := SimpleRoundTo(Abs(Value), -C_DECIMALS).ToString;
  case OrderAction of
    iabBuy:
      edtTrailTriggerSendSell.Text := '-' + edtTrailTriggerSendSell.Text;
    iabSell:
      edtTrailTriggerSendSell.Text := '+' + edtTrailTriggerSendSell.Text;
  end;
end;

function TfrmEditOrderNN.GetConditionReached: Double;
begin
  Result := 0;
  case OrderAction of
    iabBuy:
      Result := Abs(SimpleRoundTo(StrToFloatDef(edtConditionReached.Text, 0), -C_DECIMALS));
    iabSell:
      Result := -Abs(SimpleRoundTo(StrToFloatDef(edtConditionReached.Text, 0), -C_DECIMALS));
  end;
end;

procedure TfrmEditOrderNN.SetConditionReached(const Value: Double);
begin
  edtConditionReached.Text := SimpleRoundTo(Abs(Value), -C_DECIMALS).ToString;
  case OrderAction of
    iabBuy:
      edtConditionReached.Text := '+' + edtConditionReached.Text;
    iabSell:
      edtConditionReached.Text := '-' + edtConditionReached.Text;
  end;
end;

function TfrmEditOrderNN.GetConditionStopLoss: Double;
begin
  Result := 0;
  case OrderAction of
    iabBuy:
      Result := -Abs(SimpleRoundTo(StrToFloatDef(edtConditionStopLoss.Text, 0), -C_DECIMALS));
    iabSell:
      Result := Abs(SimpleRoundTo(StrToFloatDef(edtConditionStopLoss.Text, 0), -C_DECIMALS));
  end;
end;

procedure TfrmEditOrderNN.SetConditionStopLoss(const Value: Double);
begin
  edtConditionStopLoss.Text := SimpleRoundTo(Abs(Value), -C_DECIMALS).ToString;
  case OrderAction of
    iabBuy:
      edtConditionStopLoss.Text := '-' + edtConditionStopLoss.Text;
    iabSell:
      edtConditionStopLoss.Text := '+' + edtConditionStopLoss.Text;
  end;
end;

procedure TfrmEditOrderNN.SetReference(const Value: string);
begin
  edtReference.Text := Value;
end;

function TfrmEditOrderNN.GetLimit: Double;
begin
  Result := SimpleRoundTo(StrToFloatDef(edLimit.Text, 0), -C_DECIMALS)
end;

function TfrmEditOrderNN.GetOrderAction: TIABAction;
begin
  if rbBuy.Checked then
    Result := iabBuy
  else
    Result := iabSell;
end;

procedure TfrmEditOrderNN.SetOrderAction(const Value: TIABAction);
var
  Action: string;
  MaxMin: string;
begin
  if Value in [iabBuy, iabSell] then
  begin
    case Value of
      iabBuy:
        begin
          rbBuy.Checked := True;
          rbSell.Checked := False;
          gbStopLoss.Caption := Format(C_GBTRAIL_CAPTION, ['Floor']);
          Action := 'Sell';
          MaxMin := 'max';
          btnBuy.Caption    := 'Buy Now';
          btnBuy.Font.Color := clGreen;
        end;
      iabSell:
        begin
          rbBuy.Checked := False;
          rbSell.Checked := True;
          gbStopLoss.Caption := Format(C_GBTRAIL_CAPTION, ['Roof']);
          Action := 'Buy';
          MaxMin := 'min';
          btnBuy.Caption    := 'Sell Now';
          btnBuy.Font.Color := clMaroon;
        end;
    end;
    cbActiveReached.Caption         := Format(C_REACHED_SEND_CAPTION, [Action]);
    cbActiveStopLoss.Caption        := Format(C_REACHED_SEND_CAPTION, [Action]);
    lblTrailSendSellCaption.Caption := Format(C_TRIGGER_SEND_CAPTION, [Action.ToLower, MaxMin.ToLower]);

    SetConditionReached(ConditionReached);
    SetConditionStopLoss(ConditionStopLoss);
    SetTrailTriggerSendSell(TrailTriggerSendSell);
  end;
end;

procedure TfrmEditOrderNN.SetLimit(const Value: Double);
begin
  edLimit.Text := SimpleRoundTo(Value, -C_DECIMALS).ToString;
end;

function TfrmEditOrderNN.GetLimitType: TOrderNNDoc.TLimitType;
begin
  if rbLimitTypeRelative.Checked then
    Result := ltRelative
  else
    Result := ltFix;
end;

procedure TfrmEditOrderNN.SetLimitType(const Value: TOrderNNDoc.TLimitType);
begin
  case Value of
    ltRelative:
      rbLimitTypeRelative.Checked := True;
    ltFix:
      rbLimitTypeFix.Checked := True;
  end;
end;

function TfrmEditOrderNN.GetValidUntil: string;
begin
  if rbTimeInForceToday.Checked then
    Result := FormatDateTime('YYYY-MM-DD', Date)
  else
    Result := FormatDateTime('YYYY-MM-DD', dtValidUntil.Date);
end;

procedure TfrmEditOrderNN.SetValidUntil(const Value: string);
var
  ValueDate: TDate;
  FrmSettings: TFormatSettings;
begin
  if not Value.IsEmpty then
  begin
    FrmSettings := TFormatSettings.Create;
    FrmSettings.DateSeparator   := '-';
    FrmSettings.ShortDateFormat := 'YYYY-MM-DD';
    ValueDate := StrToDateTime(Value, FrmSettings);
    if (Trunc(ValueDate) <> Trunc(Date)) then
    begin
      dtValidUntil.Date := ValueDate;
      dtValidUntil.Enabled := True;
      rbTimeInForceUntil.Checked := True;
    end
    else
      rbTimeInForceToday.Checked := True;
  end;
end;

function TfrmEditOrderNN.GetValueArrayItem(Index: TOrderNNDoc.TCurrentValue): Double;
begin
  Result := FValueArray[Index];
end;

procedure TfrmEditOrderNN.SetValueArrayItem(Index: TOrderNNDoc.TCurrentValue; const Value: Double);
begin
  FValueArray[Index] := Value;
  case Index of
    cvMinPayed:
      lblMinPayed.Caption := SimpleRoundTo(Value, -C_DECIMALS).ToString;
    cvMaxPayed:
      lblMaxPayed.Caption := SimpleRoundTo(Value, -C_DECIMALS).ToString;
    cvLastPayed:
      lblLastPayed.Caption := SimpleRoundTo(Value, -C_DECIMALS).ToString;
    cvEntryPrice:
      lblEntryPrice.Caption := SimpleRoundTo(Value, -C_DECIMALS).ToString;
  end;
end;

function TfrmEditOrderNN.GetOpenVolume: Integer;
begin
  Result := seOpenVolume.Value;
end;

procedure TfrmEditOrderNN.SetOpenVolume(const Value: Integer);
begin
  seOpenVolume.Value := Value;
end;

procedure TfrmEditOrderNN.cbActiveReachedClick(Sender: TObject);
begin
  inherited;
  edtConditionReached.Enabled := IsActiveReached;
end;

procedure TfrmEditOrderNN.cbActiveStopLossClick(Sender: TObject);
begin
  inherited;
  edtConditionStopLoss.Enabled := IsActiveStopLoss;
end;

procedure TfrmEditOrderNN.cbFeedFromChange(Sender: TObject);
begin
  inherited;
  ChildId := 0;
  ChildSymbol := '';
  ChildExchange := '';
  ChildIdentifierList := '';
  ChildMarketList := '';
  ClearCurrentValue;
end;

procedure TfrmEditOrderNN.ClearCurrentValue;
var
  CurrentValue: TOrderNNDoc.TCurrentValue;
begin

  for CurrentValue := System.Low(TOrderNNDoc.TCurrentValue) to System.High(TOrderNNDoc.TCurrentValue) do
    Self.ValueArray[CurrentValue] := 0;
end;

function TfrmEditOrderNN.GetQuantity: Integer;
begin
  Result := seVolume.Value;
end;

procedure TfrmEditOrderNN.SetQuantity(const Value: Integer);
begin
  seVolume.Value := Value;
end;

procedure TfrmEditOrderNN.rbBuyClick(Sender: TObject);
begin
  inherited;
  if (Sender = rbBuy) or (Sender = lblBuy) then
    OrderAction := iabBuy
  else if (Sender = rbSell) or (Sender = lblSell) then
    OrderAction := iabSell;
end;

procedure TfrmEditOrderNN.OnLimitTypeChange(Sender: TObject);
begin
  inherited;
//
end;

procedure TfrmEditOrderNN.rbTimeInForceUntilClick(Sender: TObject);
begin
  inherited;
  dtValidUntil.Enabled := Sender = rbTimeInForceUntil;
end;

procedure TfrmEditOrderNN.rbTrailTriggerLimitOrderClick(Sender: TObject);
begin
  inherited;
  edtTrailTriggerLimit.Enabled := Sender = rbTrailTriggerLimitOrder;
end;

procedure TfrmEditOrderNN.aBuyExecute(Sender: TObject);
var
  Data: PTreeData;
  Monitor: IMonitor;
begin
  inherited;
  if Assigned(OwnerNode) and Supports(Application.MainForm, IMonitor, Monitor) then
    if (not Monitor.ExistsChildConditions(OwnerNode)) then
    begin
      Data := OwnerNode^.GetData;
      if Assigned(Data) and Assigned(Data.OrderDoc) and (Data.OrderDoc is TOrderNNDoc) then
      begin
        AssignToDoc(TOrderNNDoc(Data.OrderDoc));
        TOrderNNDoc(Data.OrderDoc).Buy;
        ModalResult := mrCancel;
      end;
    end;
end;

procedure TfrmEditOrderNN.aBuyUpdate(Sender: TObject);
var
  Data: PTreeData;
begin
  inherited;
  if Assigned(OwnerNode) then
  begin
    Data := OwnerNode^.GetData;
    TAction(Sender).Enabled := Assigned(Data) and Assigned(Data.OrderDoc) and
                               (Data.OrderDoc is TOrderNNDoc) and
                               (Data.OrderDoc.OrderStatus in [osSleeping]) and
                               (Quantity > 0);
  end
  else
    TAction(Sender).Enabled := False;
end;

procedure TfrmEditOrderNN.aCancelOrderExecute(Sender: TObject);
begin
  inherited;
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'aCancelOrder', 'Execute');
end;

procedure TfrmEditOrderNN.aSaveExecute(Sender: TObject);
var
  NewNode: PVirtualNode;
  Data: PTreeData;
  Monitor: IMonitor;
begin
  inherited;
  if IsExecuted or (OrderStatus in [osFilled, osCancelled, osPendCancel, osPendSubmit]) then
  begin
    if (TMessageDialog.ShowQuestion('Order has been changed. Duplicate a order?') = mrYes) and
      Supports(Application.MainForm, IMonitor, Monitor) then
    begin
      NewNode := Monitor.GetDuplicateOrderNN(OwnerNode);
      if Assigned(NewNode) then
      begin
        Data := NewNode^.GetData;
        if Assigned(Data) then
          AssignToDocMainParams(TOrderNNDoc(Data.OrderDoc));
      end;
    end;
    ModalResult := mrCancel;
  end
  else
    ModalResult := mrOk;
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'aSave', 'Execute');
end;

procedure TfrmEditOrderNN.aSaveUpdate(Sender: TObject);
var
  Data: PTreeData;
begin
  inherited;
  if Assigned(OwnerNode) then
  begin
    Data := OwnerNode^.GetData;
    TAction(Sender).Enabled := Assigned(Data) and (Data.CreationType = ctUser) and (Quantity > 0);
  end
  else
    TAction(Sender).Enabled := Quantity > 0;
end;

procedure TfrmEditOrderNN.aSelectInstrumentExecute(Sender: TObject);
var
  LExchange: string;
  LId: Integer;
  LSymbol: string;
  LIdentifierList: string;
  LMarketList: string;
begin
  inherited;
  if //(Global.SelectInstrOrderNN = 0) and
     (TfrmSearchInstrumentsNN.ShowDocument(FeedFromBroker, LId, LSymbol, LExchange, LIdentifierList, LMarketList) = mrOk) then
  begin
    ChildId             := LId;
    ChildSymbol         := LSymbol;
    ChildExchange       := LExchange;
    ChildIdentifierList := LIdentifierList;
    ChildMarketList     := LMarketList;
  end;
  ClearCurrentValue;
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'aSelectInstrument.Execute', 'New value: Id=' + ChildId.ToString +
                                                             ', SymbolIB=' + ChildSymbol +
                                                             ', Exchange=' + ChildExchange);
end;

procedure TfrmEditOrderNN.aSelectInstrumentUpdate(Sender: TObject);
begin
  inherited;
//  TAction(Sender).Enabled := FeedFromBroker = 0;
//  lblIBInstrumentName.Visible := TAction(Sender).Enabled;
//  btnSelectInstrument.Enabled := TAction(Sender).Enabled;
end;

procedure TfrmEditOrderNN.AssignFromDoc(const aDocument: TOrderNNDoc);
var
  CurrentValue: TOrderNNDoc.TCurrentValue;
begin
  if Assigned(aDocument) then
  begin
    for CurrentValue := System.Low(TOrderNNDoc.TCurrentValue) to System.High(TOrderNNDoc.TCurrentValue) do
      Self.ValueArray[CurrentValue] := aDocument.ValueArray[CurrentValue];

    Self.Info                 := TMonitorLists.OrderList.GetStatusText(aDocument.OwnerNode);
    Self.OrderAction          := aDocument.OrderAction;
    Self.IsActivateChild      := aDocument.IsActivateChild;
    Self.ConditionReached     := aDocument.ConditionReached;
    Self.ConditionStopLoss    := aDocument.ConditionStopLoss;
    Self.Currency             := aDocument.Currency;
    Self.Decimals             := aDocument.Decimals;
    Self.Description          := aDocument.Description;
    Self.ChildExchange        := aDocument.Exchange;
    Self.FeedFromBroker       := aDocument.FeedFromBroker;
    Self.Id                   := aDocument.Id;
    Self.IdentifierList       := aDocument.IdentifierList;
    Self.InstrumentName       := aDocument.InstrumentName;
    Self.IsActiveReached      := aDocument.IsActiveReached;
    Self.IsActiveStopLoss     := aDocument.IsActiveStopLoss;
    Self.IsExecuted           := aDocument.IsExecuted;
    Self.IsRepetitive         := aDocument.IsRepetitive;
    Self.Limit                := aDocument.Limit;
    Self.LimitType            := aDocument.LimitType;
    Self.MarketList           := aDocument.MarketList;
    Self.OpenVolume           := aDocument.OpenVolume;
    Self.OrderIBId            := aDocument.OrderIBId;
    Self.OrderStatus          := aDocument.OrderStatus;
    Self.OrderType            := aDocument.OrderType;
    Self.OwnerNode            := aDocument.OwnerNode;
    Self.Price                := aDocument.Price;
    Self.Quantity             := aDocument.Quantity;
    Self.Reference            := aDocument.Reference;
    Self.Symbol               := aDocument.Symbol;
    Self.TrailOrderSendType   := aDocument.TrailOrderSendType;
    Self.TrailTriggerLimit    := aDocument.TrailTriggerLimit;
    Self.TrailTriggerSendSell := aDocument.TrailTriggerSendSell;
    Self.ValidUntil           := aDocument.ValidUntil;
    Self.Filled               := aDocument.Filled;
    Self.AvgPrice             := aDocument.AvgPrice;
    Self.ChildSymbol          := aDocument.ChildSymbol;
    Self.ChildIdentifierList  := aDocument.ChildIdentifierList;
    Self.ChildMarketList      := aDocument.ChildMarketList;
    Self.ChildId              := aDocument.ChildId;
  end;
end;

procedure TfrmEditOrderNN.AssignToDocMainParams(var aDocument: TOrderNNDoc);
begin
  if Assigned(aDocument) then
  begin
    aDocument.IsActivateChild      := Self.IsActivateChild;
    aDocument.ConditionReached     := Self.ConditionReached;
    aDocument.ConditionStopLoss    := Self.ConditionStopLoss;
    aDocument.Currency             := Self.Currency;
    aDocument.Decimals             := Self.Decimals;
    aDocument.Description          := Self.Description;
    aDocument.Exchange             := Self.ChildExchange;
    aDocument.InstrumentName       := Self.InstrumentName;
    aDocument.IsActiveReached      := Self.IsActiveReached;
    aDocument.IsActiveStopLoss     := Self.IsActiveStopLoss;
    aDocument.IsRepetitive         := Self.IsRepetitive;
    aDocument.Limit                := Self.Limit;
    aDocument.LimitType            := Self.LimitType;
    aDocument.OpenVolume           := Self.OpenVolume;
    aDocument.OrderAction          := Self.OrderAction;
    aDocument.OrderIBId            := Self.OrderIBId;
    aDocument.OrderType            := Self.OrderType;
    aDocument.Quantity             := Self.Quantity;
    aDocument.Price                := Self.Price;
    aDocument.ChildSymbol          := Self.ChildSymbol;
    aDocument.Reference            := Self.Reference;
    aDocument.TrailOrderSendType   := Self.TrailOrderSendType;
    aDocument.TrailTriggerLimit    := Self.TrailTriggerLimit;
    aDocument.TrailTriggerSendSell := Self.TrailTriggerSendSell;
    aDocument.ValidUntil           := Self.ValidUntil;
    aDocument.FeedFromBroker       := Self.FeedFromBroker;
    aDocument.ChildIdentifierList  := Self.ChildIdentifierList;
    aDocument.ChildMarketList      := Self.ChildMarketList;
    aDocument.ChildId              := Self.ChildId;
  end;
end;

procedure TfrmEditOrderNN.AssignToDoc(var aDocument: TOrderNNDoc);
begin
  if Assigned(aDocument) then
  begin
    AssignToDocMainParams(aDocument);
    aDocument.IsExecuted := Self.IsExecuted;
  end;
end;

end.
