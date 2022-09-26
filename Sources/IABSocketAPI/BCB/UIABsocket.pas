unit UIABsocket;

//  For compling this demo onto BCB, remove comment marks to make compiler defines below effective.
 // {$DEFINE BCBCOMPILE}

interface

uses
{$IF CompilerVersion < 24.0}  // 24 = XE3
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Shellapi, Menus, ComCtrls, UTextForm,
{$ELSE}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes, Vcl.Graphics,
     Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, UTextForm, ShellAPI,
{$IFEND}
IABSocketAPI, IABSocketAPI_const
{$IFDEF USE_BIGDECIMAL}
, Velthuis.BigDecimals   // see top of IABSocketAPI_const.pas for information on this.
{$ENDIF}
;

const
  UM_DOIT = WM_USER + 1;  // this gives an example below, of using messages to sidestep any nested API command loops errors.

type
  TFIABSocket = class(TForm)
    ButtonConnection: TButton;
    ButtonPlaceOrder: TButton;
    IABSocket1: TIABSocket;
    EditP: TEdit;
    MemoAcct: TMemo;
    ButtonExecutions: TButton;
    ButtonAcctPortfolio: TButton;
    ButtonTickData: TButton;
    labelbid: TLabel;
    labelbidsize: TLabel;
    labelask: TLabel;
    labelasksize: TLabel;
    MemoError: TMemo;
    ButtonOpenOrders: TButton;
    ListBoxOrderType: TListBox;
    RBBuy: TRadioButton;
    RBSell: TRadioButton;
    EditV: TEdit;
    MemoOrderStat: TMemo;
    LabelVol: TLabel;
    LabelLast: TLabel;
    LabelSize: TLabel;
    EditA: TEdit;
    EditSym: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EditExch: TEdit;
    ComboSecType: TComboBox;
    ButtonLevel2Depth: TButton;
    ButtonCancelOrder: TButton;
    EditExp: TEdit;
    ButtonNewsBulletins: TButton;
    StatusBar1: TStatusBar;
    EditCur: TEdit;
    Bevel2: TBevel;
    ButtonScanner: TButton;
    ButtonSnap: TButton;
    ButtonTWStime: TButton;
    ButtonModifyOrder: TButton;
    ButtonHistory: TButton;
    ButtonContractInfo: TButton;
    ButtonRealTimebars: TButton;
    ButtonVerifyOrder: TButton;
    LabelOptTick: TLabel;
    ComboBoxLogLevel: TComboBox;
    Label2: TLabel;
    ButtonTimeSales: TButton;
    ButtonCxTS: TButton;
    gbOrder: TGroupBox;
    gbAbout: TGroupBox;
    Bevel3: TBevel;
    gbData: TGroupBox;
    pTop: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    ButtonSearch: TButton;
    ButtonWSHorizon: TButton;
    procedure ButtonConnectionClick(Sender: TObject);
    procedure ButtonPlaceOrderClick(Sender: TObject);
    procedure IABSocket1Error(Sender: TObject; TempId, ErrorCode: Integer;  ErrorMsg: String);
    procedure ButtonExecutionsClick(Sender: TObject);
    procedure ButtonAcctPortfolioClick(Sender: TObject);
    procedure ButtonTickDataClick(Sender: TObject);
    procedure ButtonOpenOrdersClick(Sender: TObject);
    procedure IABSocket1OpenOrder(Sender: TObject; Order: TIABOrder);
    procedure IABSocket1AccountTime(Sender: TObject; TimeStamp: String);
    procedure IABSocket1AccountValue(Sender: TObject; Index: Integer);
    procedure IABSocket1PortfolioUpdate(Sender: TObject; Index: Integer);
    procedure FormCreate(Sender: TObject);
    procedure ButtonCancelOrderClick(Sender: TObject);
    procedure IABSocket1OrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
    procedure Label4Click(Sender: TObject);
    procedure IABSocket1ConnectionState(Sender: TObject; State: TIABConnection);
    procedure ButtonContractInfoClick(Sender: TObject);
    procedure IABSocket1InstrumentSpecDetails(Sender: TObject; Index: Integer);
    procedure ButtonLevel2DepthClick(Sender: TObject);
    procedure IABSocket1Execution(Sender: TObject; Order: TIABOrder);
    procedure IABSocket1NewsBulletin(Sender: TObject; MsgID: Integer; Bulletin, NewsSource: String);
    procedure IABSocket1ExchangeStatus(Sender: TObject; MsgID: Integer; Status: TIABExchangeStatus; Bulletin, NewsSource: String);
    procedure ButtonNewsBulletinsClick(Sender: TObject);
    procedure IABSocket1ManagedAccounts(Sender: TObject; Details: String);
    procedure AppOnHint(Sender: TObject);
    procedure IABSocket1TickPrice(Sender: TObject; DataId: Integer; TickType: TIABTickType; Price: Double; TickAttrib: TIABTickAttrib);
    procedure ButtonHistoryClick(Sender: TObject);
    procedure IABSocket1HistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
    procedure IABSocket1BondSpecDetails(Sender: TObject; Index: Integer);
    procedure ButtonConnectionKeyPress(Sender: TObject; var Key: Char);
    procedure ButtonScannerClick(Sender: TObject);
    procedure IABSocket1ScannerParam(Sender: TObject; Parameters: String);
    procedure IABSocket1ScannerData(Sender: TObject; Scan: TIABScan);
    procedure ButtonModifyOrderClick(Sender: TObject);
    procedure ButtonSnapClick(Sender: TObject);
    procedure ButtonTWStimeClick(Sender: TObject);
    procedure IABSocket1CurrentTime(Sender: TObject; DateTime: TDateTime);
    procedure ButtonRealTimebarsClick(Sender: TObject);
    procedure IABSocket1RealTimeData(Sender: TObject; DataId: Integer; RealTimeDataElement: TIABRealTimeData);
    procedure ButtonVerifyOrderClick(Sender: TObject);
    procedure IABSocket1VerifiedOrder(Sender: TObject; Order: TIABOrder);
    procedure IABSocket1EndOfStreamRead(Sender: TObject);
    procedure IABSocket1FundamentalData(Sender: TObject; DataID: Integer;xmlStrData: String);
    procedure ComboBoxLogLevelChange(Sender: TObject);
    procedure IABSocket1TickOptionComputation(Sender: TObject;
      DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice,
      pvDividend, Gamma, Vega, Theta, undPrice: Double);

    {$IFDEF USE_BIGDECIMAL}
    procedure IABSocket1TickSize(Sender: TObject; DataId: Integer; TickType: TIABTickType; Size: BigDecimal);
    procedure IABSocket1MarketDepth(Sender: TObject; DataId, Index, Operation, Side: Integer; Size: BigDecimal; Price: Double);
    procedure IABSocket1MarketLevel2(Sender: TObject; DataId, Index, Operation, Side: Integer; Size: BigDecimal; Price: Double; MMId: string; SmartDepth: Boolean);
    {$ELSE}
    procedure IABSocket1TickSize(Sender: TObject; DataId: Integer; TickType: TIABTickType; Size: Double);
    procedure IABSocket1MarketDepth(Sender: TObject; DataId, Index, Operation, Side: Integer; Size, Price: Double);
    procedure IABSocket1MarketLevel2(Sender: TObject; DataId, Index, Operation, Side: Integer; Size, Price: Double; MMId: string; SmartDepth: Boolean);
    {$ENDIF}

    procedure IABSocket1HistoricalTickData(Sender: TObject; DataID: Integer; TickData: TIABTickData);
    procedure ButtonCxTSClick(Sender: TObject);
    procedure IABSocket1SymbolSample(Sender: TObject; DataID, Item, Count: Integer; SymbolDerivative: TIABSymbolDerivativeSpecItem);
    procedure ButtonSearchClick(Sender: TObject);
    procedure IABSocket1WsHorizonMeta(Sender: TObject; DataID: Integer; DataStr: string);
    procedure ButtonWSHorizonClick(Sender: TObject);
    procedure IABSocket1NewsProvider(Sender: TObject; Item, Count: Integer; ProviderCode, ProviderName: string);
    procedure IABSocket1DepthMarketDataDescripItem(Sender: TObject; Item, Count: Integer; DepthMarketDataDescrip: TIABDepthMarketDataDescripItem);

   private
    RealtimeBars: Integer;
    function SetOrderPricing: Boolean;
    function SetOrderParams: Boolean;

    // procedure DoIt(var Msg: TMessage); message UM_DOIT;   Sample code below.
  public
    MDataId, CDataId, TDataId, HTDataId: Integer;
  end;

  function GetIndexFutureExpiry: string;
  function ExpiryDateToContractName(YearMonth: string): string;




var
  FIABSocket: TFIABSocket;



implementation

uses UDepth, UCombo, UXcution, UScanForm, UHistoricalData, UHostport;

{$R *.DFM}

procedure TFIABSocket.ButtonConnectionClick(Sender: TObject);
//var port: string;
begin
  if ButtonConnection.Tag = 0 then
    begin
      FHostPort.Left := Left + 20;
      FHostPort.Top := Top + 20;
      FHostPort.ShowModal;
      if FHostPort.ModalResult = mrOk then
        begin
          IABSocket1.TWSHostAddress := FHostPort.ComboBoxIPHost.Text;
          IABSocket1.TWSPort := StrTointDef(FHostPort.ComboBoxPort.Text, 7497);
          IABSocket1.Connected := true;
        end;
    end
  else
    IABSocket1.Connected := false;//not Boolean(ButtonConnection.Tag);
end;

function TFIABSocket.SetOrderPricing: Boolean;
begin
  Result := true;
  IABSocket1.DefOrder.OrderType := TIABOrderType(ListBoxOrderType.ItemIndex + 1);
  if RBBuy.Checked then
    IABSocket1.DefOrder.Action := iabBuy
  else if RBSell.Checked then
    IABSocket1.DefOrder.Action := iabSell
  else
    begin
      {$IF CompilerVersion >= 24.0}  // XE3
      MessageDlgPos('Select a Buy or Sell order.', mtInformation, [mbOK], 0, Left + 30, Top + 80, mbOK);
      {$ELSE}
      MessageDlgPos('Select a Buy or Sell order.', mtInformation, [mbOK], 0, Left + 30, Top + 80);
      {$IFEND}
      Result := false;
      Exit;
    end;

{
  TIABOrderType = (otNoChange,otMarket,otLimit,otStop,otStopLimit,otPassiveRel,otVWAP,otMarketClose,otLimitClose,otTrail,
                    otLimitOpen,otMarketOpen,otOneCancelOther,otISEBlock,
                  	otPegMarket,otPegStock,otPegMidPt,otPegBench,otPegPrimary,otPegBest,otVolatility,otTrailLimit,otScale,
                    otMarketTouch,otLimitTouch,otMarketToLimit,otAuction,otAuctionRel,otAuctionLimit,otAuctionPegStk, otSweepFill,
                    otDiscretionary,otBoxTop,otMarketwProtect,otStopwProtect,
                    otComboLimit,otComboMarket,otComboLimitLeg,otRelLimitCombo,otRelMktCombo,
                  	otNone,otUnknown);
}

  case IABSocket1.DefOrder.OrderType of
    otMarket: begin
        IABSocket1.DefOrder.Quantity := {$IFDEF USE_BIGDECIMAL}EditV.Text {$ELSE} StrToFloat(EditV.Text){$ENDIF};
      end;
    otLimit: begin
        IABSocket1.DefOrder.Quantity := {$IFDEF USE_BIGDECIMAL}EditV.Text {$ELSE} StrToFloat(EditV.Text){$ENDIF};
        IABSocket1.DefOrder.Price := StrToFloat(EditP.Text);
      end;
    otStop: begin
        IABSocket1.DefOrder.Quantity := {$IFDEF USE_BIGDECIMAL}EditV.Text {$ELSE} StrToFloat(EditV.Text){$ENDIF};
        IABSocket1.DefOrder.Price := StrToFloat(EditP.Text);
      end;
    otStopLimit: begin
        IABSocket1.DefOrder.Quantity := {$IFDEF USE_BIGDECIMAL}EditV.Text {$ELSE} StrToFloat(EditV.Text){$ENDIF};
        IABSocket1.DefOrder.Price := StrToFloat(EditP.Text);
        if EditA.Text <> 'Aux/Strike' then
          IABSocket1.DefOrder.AuxPrice := StrToFloat(EditA.Text);
      end;
    else
      begin
        {$IF CompilerVersion >= 24.0}  // XE3
        MessageDlgPos('Only the basic MKT, LMT, STP, STPLMT order types are utilized in this demo app.  This API makes available all 40+ order types for your coding pleasure.', mtInformation, [mbOK], 0, Left + 30, Top + 80, mbOK);
        {$ELSE}
        MessageDlgPos('Only the basic MKT, LMT, STP, STPLMT order types are utilized in this demo app.  This API makes available all 40+ order types for your coding pleasure.', mtInformation, [mbOK], 0, Left + 30, Top + 80);
        {$IFEND}
        Result := false;
      end;
  end;
end;

function TFIABSocket.SetOrderParams: Boolean;
begin
  Result := true;

  IABSocket1.DefOrder.LocalSymbol := '';
  IABSocket1.DefOrder.Exchange := '';
  IABSocket1.DefOrder.PrimaryExchange := '';
  IABSocket1.DefOrder.Symbol := '';
  IABSocket1.DefOrder.Expiry := '';
  IABSocket1.DefOrder.Currency := '';
  IABSocket1.DefOrder.Strike := 0.0;
  IABSocket1.DefOrder.AuxPrice := 0.0;
  IABSocket1.DefOrder.Right := rtNone;
  IABSocket1.DefOrder.Price := 0.0;
  IABSocket1.DefOrder.Quantity := 0.0;
  IABSocket1.DefOrder.Action := iabIdle;
  IABSocket1.DefOrder.Multiplier := '0';
  IABSocket1.DefOrder.DeleteComboLeg(-1);
  IABSocket1.DefOrder.TimeInForce := tifDay;




  // A BIG note about exchanges....
  //
  //  The TWS is very picky about what combinations of Exchange, Local Exchange, local symbol, expiry formats etc.
  //  Some items they want them all, and some need the bare minimun.
  //  They change the rules in the TWS a lot.  You will need to experiment to find the right combination.
  //
  //   ***   many good examples and formats here:  ***
  //
  //   Visit   https://interactivebrokers.github.io/tws-api/orders.html#gsc.tab=0
  //
  //   Another good set of examples are in the C++ API sample code, in the
  //     TestCppClient application and its ContractSamples.cpp file.
  //
  //


  IABSocket1.DefOrder.SecurityType := TIABSecurityType(ComboSecType.ItemIndex);

  //    TIABSecurityType = (stStock,stOption,stFuture,stIndex,stFutOpt,stCash,stBag,stBond,stIOp,stCFD,stFund,stCmdty,stCrypto,stConFut,stFutConFut,stWar,stNews,stAll);

  case IABSocket1.DefOrder.SecurityType of
    stStock: begin
        IABSocket1.DefOrder.Symbol := EditSym.Text;
        IABSocket1.DefOrder.Currency := EditCur.Text;
        if EditExch.Text = 'SMART' then
          begin
            IABSocket1.DefOrder.Exchange := EditExch.Text;
            IABSocket1.DefOrder.PrimaryExchange := 'ISLAND';
          end
        else if EditExch.Text = 'NASDAQ' then
          IABSocket1.DefOrder.Exchange := 'ISLAND'
        else
          IABSocket1.DefOrder.Exchange := EditExch.Text;
      end;

    stOption: begin
        IABSocket1.DefOrder.Symbol := EditSym.Text;
        IABSocket1.DefOrder.Exchange := EditExch.Text;
        IABSocket1.DefOrder.Currency := EditCur.Text;
        IABSocket1.DefOrder.Expiry := EditExp.Text;
        IABSocket1.DefOrder.Strike := StrToFloat(EditA.Text);
        IABSocket1.DefOrder.Right := rtCall;
        IABSocket1.DefOrder.Multiplier := '100';
      end;
    stFuture: begin
        if Length(EditSym.Text) < 4 then
          IABSocket1.DefOrder.Symbol := EditSym.Text
        else
          IABSocket1.DefOrder.LocalSymbol := EditSym.Text;
        IABSocket1.DefOrder.Exchange := EditExch.Text;
        IABSocket1.DefOrder.Currency := EditCur.Text;
        IABSocket1.DefOrder.Expiry := EditExp.Text;
      end;
    stIndex: begin
        IABSocket1.DefOrder.Symbol := EditSym.Text;
        IABSocket1.DefOrder.Exchange := EditExch.Text;
        IABSocket1.DefOrder.Currency := EditCur.Text;
      end;
    stCash: begin
        IABSocket1.DefOrder.Symbol := EditSym.Text;
        IABSocket1.DefOrder.Exchange := 'IDEALPRO';
        IABSocket1.DefOrder.Currency := EditCur.Text;
      end;

    stCrypto: begin
        IABSocket1.DefOrder.Symbol := EditSym.Text;
        IABSocket1.DefOrder.Exchange := EditExch.Text;
        IABSocket1.DefOrder.Currency := EditCur.Text;
        if IABSocket1.DefOrder.OrderType = otMarket then
          IABSocket1.DefOrder.TimeInForce := tifIOC
        else
          IABSocket1.DefOrder.TimeInForce := tif5Mins;

      end;



    else
      begin
        {$IF CompilerVersion >= 24.0}  // XE3
        MessageDlgPos('Only the basic stStock, stOption, stFuture, stIndex, stCash and stCrypto Security types are utilized in this demo app.  This API does provide all 18+ security types for your coding pleasure.', mtInformation, [mbOK], 0, Left + 30, Top + 80, mbOK);
        {$ELSE}
        MessageDlgPos('Only the basic stStock, stOption, stFuture, stIndex, stCash and stCrypto Security types are utilized in this demo app.  This API does provide all 18+ security types for your coding pleasure.', mtInformation, [mbOK], 0, Left + 30, Top + 80);
        {$IFEND}
        Result := false;
      end;


    end;
end;


procedure TFIABSocket.ButtonPlaceOrderClick(Sender: TObject);
begin
  if SetOrderParams and SetOrderPricing then
    IABSocket1.PlaceOrder(IABSocket1.DefOrder);

//  Here we sent the DefOrder, but you could of created a new TIABOrder and sent it.

end;

procedure TFIABSocket.IABSocket1Error(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: String);
begin
  MemoError.Lines.Insert(0,IntToStr(TempId) + ' ' + IntToStr(ErrorCode) + ' ' + ErrorMsg);
  if ErrorCode = 317 then FDepth.EmptyGrid;
end;

procedure TFIABSocket.ButtonExecutionsClick(Sender: TObject);
begin
  FExecution.Left := Left + 20;
  FExecution.Top := Top + 50;
  FExecution.ShowModal;
  if FExecution.ModalResult = mrOk then IABSocket1.GetExecutions(FExecution.ExecFilter);
end;

procedure TFIABSocket.ButtonAcctPortfolioClick(Sender: TObject);
var s: string;
begin
  case ButtonAcctPortfolio.Tag of
  0:begin
      s := '';
      if not InputQuery('Account query string','FA clients only, otherwise blank.',s) then Exit;
      IABSocket1.GetAccountUpdates(s);
      ButtonAcctPortfolio.Tag := 1;
      ButtonAcctPortfolio.Caption := 'Cx Acct/PF';
    end;
  1:begin
      IABSocket1.CancelAccountUpdates;
      ButtonAcctPortfolio.Tag := 0;
      ButtonAcctPortfolio.Caption := 'Acct / PF';
    end;
  end;
end;

procedure TFIABSocket.IABSocket1AccountValue(Sender: TObject; Index: Integer);
begin
  MemoAcct.Lines.Insert(0,IABSocket1.AccountValues[Index]);
end;

procedure TFIABSocket.ButtonTickDataClick(Sender: TObject);
begin

  if MDataId > 500 then IABSocket1.CancelMarketData(MDataId -1);

  // Effective API 9.00.  The GetMarketData now includes Extended data requests param "ExMarketData"
  // Use this only to obtain extra data.  Normal data (price / size / volume / trade) will flow using
  // a closed set (the default) per below.

  IABSocket1.RequestMarketDataType(mdtDelayed);

  if SetOrderParams then
    IABSocket1.GetMarketData(MDataId,IABSocket1.DefOrder);
  inc(MDataId);
end;



procedure TFIABSocket.IABSocket1TickPrice(Sender: TObject; DataId: Integer;
  TickType: TIABTickType; Price: Double; TickAttrib: TIABTickAttrib);
begin
  if TickType in [ttBid,ttDelayedBid] then LabelBid.caption := FormatFloat('0.00',Price);
  if TickType in [ttAsk,ttDelayedAsk] then LabelAsk.caption := FormatFloat('0.00',Price);
  if TickType in [ttLast,ttDelayedLast] then LabelLast.caption := FormatFloat('0.00',Price);
end;

{$IFDEF USE_BIGDECIMAL}

procedure TFIABSocket.IABSocket1TickSize(Sender: TObject; DataId: Integer; TickType: TIABTickType; Size: BigDecimal);
begin
  if TickType in [ttBidSize,ttDelayedBidSize] then Labelbidsize.caption := Size.ToPlainString;
  if TickType in [ttAskSize,ttDelayedAskSize] then Labelasksize.caption := Size.ToPlainString;
  if TickType in [ttLastSize,ttDelayedLastSize] then LabelSize.caption := Size.ToPlainString;
  if TickType in [ttVolume,ttDelayedVolume] then LabelVol.caption := Size.ToPlainString;
end;

{$ELSE}

procedure TFIABSocket.IABSocket1TickSize(Sender: TObject; DataId: Integer; TickType: TIABTickType; Size: Double);
begin
  if TickType in [ttBidSize,ttDelayedBidSize] then Labelbidsize.caption := FormatFloat('0.#',Size); // IntToStr(Size);
  if TickType in [ttAskSize,ttDelayedAskSize] then Labelasksize.caption := FormatFloat('0.#',Size); // IntToStr(Size);
  if TickType in [ttLastSize,ttDelayedLastSize] then LabelSize.caption := FormatFloat('0.#',Size); // IntToStr(Size);
  if TickType in [ttVolume,ttDelayedVolume] then LabelVol.caption := FormatFloat('0.#',Size); // IntToStr(Size);
end;

{$ENDIF}

procedure TFIABSocket.ButtonOpenOrdersClick(Sender: TObject);
begin
  {
  case MessageDlgPos('Select Order origins:' + #13#10#13#10 + 'Yes: for all orders from all clients.' + #13#10 + 'No: for all orders from this client only.',mtConfirmation,[mbYes,mbNo],0,Left + 10, Top + 50) of
    mrYes: IABSocket1.GetOpenOrdersAccount;
    mrNo: IABSocket1.GetOpenOrdersClient;
  end;
  }
  IABSocket1.GetOpenOrdersClient;
end;

procedure TFIABSocket.IABSocket1OpenOrder(Sender: TObject; Order: TIABOrder);
begin
  MemoAcct.Lines.Insert(0,'OpenOrder ' + Order.Symbol + ' ' + {$IFDEF USE_BIGDECIMAL}Order.Quantity.ToPlainString {$ELSE} FormatFloat('0.###', Order.Quantity){$ENDIF} );
end;

procedure TFIABSocket.IABSocket1AccountTime(Sender: TObject; TimeStamp: String);
begin
  MemoAcct.Lines.Insert(0,'TimeStamp ' + TimeStamp);
end;

procedure TFIABSocket.IABSocket1PortfolioUpdate(Sender: TObject; Index: Integer);
begin
  MemoAcct.Lines.Insert(0,IABSocket1.Portfolio[Index].Symbol + ' ' + {$IFDEF USE_BIGDECIMAL}IABSocket1.Portfolio[Index].Position.ToPlainString {$ELSE} FloatToStr(IABSocket1.Portfolio[Index].Position){$ENDIF} );
end;

procedure TFIABSocket.FormCreate(Sender: TObject);
var i: TIABOrderType;  j: Integer;
begin
  for i := Low(OrderTypeText) to High(OrderTypeText) do
    ListBoxOrderType.Items.Add(OrderTypeText[i]);

  for j := Ord(Low(TIABSecurityType)) to Ord(High(TIABSecurityType)) - 1 do
    ComboSecType.Items.Add(SecurityTypeString[TIABSecurityType(j)]);

  ListBoxOrderType.ItemIndex := 1;
  ComboSecType.ItemIndex := 2;

  {$IF CompilerVersion >= 22.0}   // XE1
  EditP.Text := '0' + FormatSettings.DecimalSeparator + '00';
  {$ELSE}
  EditP.Text := '0' + DecimalSeparator + '00';
  {$IFEND}

  //EditExp.Text := GetIndexFutureExpiry;
  EditSym.Text := 'ES' + ExpiryDateToContractName(GetIndexFutureExpiry);

  MDataId := 500;
  CDataId := 1000;
  TDataId := 1500;
  HTDataId := 2000;
  Application.OnHint := AppOnHint;


  // NOT recommended below.  Goes back to the 2016 and prior version of the API.  No longer works
  // IABSocket1.v100plusAPICalls := false;

  //  Used to go back an API version or two... in case the latest beta from IAB has bugs
  //  972, 973, etc.
  //IABSocket1.ClientMaxVerOverride := 1010;
end;

procedure TFIABSocket.ButtonCancelOrderClick(Sender: TObject);
begin
  if IABSocket1.Orders.Count > 0 then IABSocket1.CancelOrder(IABSocket1.Orders[IABSocket1.Orders.Count -1].TempId);
end;

procedure TFIABSocket.IABSocket1OrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
var s, s2: string;
begin
  case Status of
   osPendSubmit: s := 'pendsub';
   osPendCancel: s := 'pendcnl';
   osPreSubmit: s := 'presub';
   osSubmitted: s := 'submtd';
   osCancelled: s := 'cncld';
   osFilled: s := 'filld';
  end;
  s2 := 'tId=' + IntToStr(Order.TempId) + ' pId=' + IntToStr(Order.permid) + ' f=';
  {$IFDEF USE_BIGDECIMAL}
  s2 := s2 + Order.Filled.ToPlainString + ' r=' + Order.Remaining.ToPlainString;
  {$ELSE}
  s2 := s2 + FormatFloat('0.###', Order.Filled) + ' r=' + FormatFloat('0.###', Order.Remaining);
  {$ENDIF}
  s2 := s2 + ' stat=' + s + ' p=' + FloatToStr(Order.FillPrice);
  MemoOrderStat.Lines.Insert(0, s2);
  MemoOrderStat.Lines.Objects[0] := TObject(Order.TempId);

//    + ' ' + IntToStr(Order.LatestFillQty) + ' ' + IntToStr(Ord(Order.Changed)) + ' ' +  FloatToStr(Order.LatestFillPrice));


//  ** NOTE **
//  Use the properties of the Order parameter above, to determine the fill status of an order.
//  You cannot rely on the status of an order to determine its fill.
//  The TWS will fill orders under almost any status!  Particularly if its a partial fill.
//
//  Also new OnExecution in API 7 reports same as the Changed property and the OnFill event.
//  Several ways to get the executions.
//  Not certain if the sequence of Events is reliable?  (Onexecution first,  then Onstatus event)
end;

procedure TFIABSocket.IABSocket1ConnectionState(Sender: TObject; State: TIABConnection);
begin
  case State of
    twsClosed: MemoError.Lines.Insert(0,'TWS connection closed');
    twsConnecting: MemoError.Lines.Insert(0,'Connecting to TWS, host ' + IABSocket1.TWSHostAddress + ', port ' + IntTostr(IABSocket1.TWSPort));
    twsReady: begin
                MemoError.Lines.Insert(0,'TWS connection Ready... Server time: ' + IABSocket1.ConnectAtServerTime);
                MemoError.Lines.Insert(0,'TWS API server version: ' + IntToStr(IABSocket1.ServerVersion));
              end;
    twsFailed: MemoError.Lines.Insert(0,'TWS connection Failed');
  end;
  if State = twsReady then
    begin
      ButtonConnection.Caption := 'Disconnect';
      ButtonConnection.Tag := 1;
    end
  else
    begin
      ButtonConnection.Caption := 'Connect';
      ButtonConnection.Tag := 0;
    end;
end;

function ShellExecAndWait32(Wait: Boolean;Directory, FileName, CmdParams:string; Visibility: Word; AppHwnd: HWND):Boolean;
var Hinst: Longword;
begin
  Hinst := ShellExecute(AppHwnd,'open',PChar(FileName),PChar(CmdParams),PChar(Directory),Visibility);
  Result := Hinst > 32;
  if Result and Wait then WaitforSingleObject(Hinst,INFINITE);
end;

procedure TFIABSocket.Label4Click(Sender: TObject);
begin
  ShellExecAndWait32(false,'',Label4.Caption,'',SW_SHOWNORMAL,Application.Handle);
end;

procedure TFIABSocket.ButtonContractInfoClick(Sender: TObject);
begin
  if SetOrderParams then
    IABSocket1.GetInstrumentSpecs(TDataId, IABSocket1.DefOrder);
  inc(TDataId);
end;

procedure TFIABSocket.ButtonCxTSClick(Sender: TObject);
begin
  if HTDataId > 2000 then
    IABSocket1.CancelTickByTickData(HTDataId);
end;

procedure TFIABSocket.IABSocket1InstrumentSpecDetails(Sender: TObject; Index: Integer);
var s: string; ot: TIABOrderType;
begin
{$IFDEF BCBCOMPILE}
  with PTIABInstrumentSpecItem(IABSocket1.InstrumentSpecs[Index])^ do
{$ELSE}
  with IABSocket1.InstrumentSpecs[Index] do
{$ENDIF}
    begin
      s := MarketName + ' ' + TradingClass + ' ' + IntToStr(ContractId) + ' ' + Multiplier + ' ' + FloatToStr(MinimumTick);
      for ot := Low(TIABOrderType) to High(TIABOrderType) do
        if (ot in OrderTypes) then s := s + ' ' + OrderTypeTostr(ot);
      s := s + ' ' + ValidExchanges;
    end;
  MemoAcct.Lines.Insert(0,s);
end;

procedure TFIABSocket.ButtonLevel2DepthClick(Sender: TObject);
begin
  FDepth.Top := Top;
  FDepth.Left := Left + Width;
  FDepth.Show;
end;

procedure TFIABSocket.IABSocket1Execution(Sender: TObject; Order: TIABOrder);
var LastExec: {$IFDEF BCBCOMPILE} PTIABExecution; {$ELSE} TIABExecution; {$ENDIF} s: string;
begin
{$IFDEF BCBCOMPILE}
  LastExec := Order.Executions[Order.ExecutionsCount -1];
  with LastExec^ do
    s := 'e=' + ExecutionId + ' pId=' + IntToStr(PermId) + ' t=' + Time + ' act=' + AcctNumber + ' ' + Exchange + ' ' +
          IntToStr(Ord(Side)) + ' ' + FloatToStr(Volume) + ' ' + FloatToStr(Price);
{$ELSE}
  LastExec := Order.Executions[Order.ExecutionsCount -1];
  with LastExec do
    s := 'e=' + ExecutionId + ' pId=' + IntToStr(PermId) + ' t=' + Time + ' act=' + AcctNumber + ' ' + Exchange + ' ' +
          IntToStr(Ord(Side)) + ' ' + {$IFDEF USE_BIGDECIMAL}Volume.ToPlainString {$ELSE} FloatToStr(Volume){$ENDIF} + ' ' + FloatToStr(Price);
{$ENDIF}

  MemoOrderStat.Lines.Insert(0,s);
end;

function GetIndexFutureExpiry: string;
const Months: Integer = 3;
var y,m,d: Word; DT: TDateTime; Next: Boolean; yStr, Expiry: string;
begin
  DecodeDate(Date,y,m,d);
  DT := EncodeDate(y,m,1);
  case DayOfWeek(DT) of
    1:Next := d > 11;
    2:Next := d > 10;
    3:Next := d > 9;
    4:Next := d > 8;
    5:Next := d > 7;
    6:Next := d > 6;
    7:Next := d > 12;
    else Next := false;
  end;
  if Next then m := m + 1;
  if m = 13 then
    begin
      y := y + 1;
      m := 1;
    end;
  while m mod Months > 0 do inc(m);
  yStr := IntToStr(y);
  Expiry := yStr;
  if m < 10 then Expiry := Expiry + '0';
  Expiry := Expiry + IntToStr(m);
  Result := Expiry;
end;

function ExpiryDateToContractName(YearMonth: string): string;
var i: Integer; y: char;
begin
  Result := '';
  if Length(YearMonth) <> 6 then Exit;
  for i := 1 to 6 do
  {$IFDEF UNICODE}
    if not CharInSet(YearMonth[i], ['0'..'9']) then Exit;
  {$ELSE}
    if not (YearMonth[i] in ['0'..'9']) then Exit;
  {$ENDIF}
  y := YearMonth[4];
  Delete(YearMonth,1,4);
  i := StrToInt(YearMonth);
  if not i in [1..12] then Exit;
  Result := FutureMonthCode[i] + y;
end;

procedure TFIABSocket.IABSocket1NewsBulletin(Sender: TObject; MsgID: Integer; Bulletin, NewsSource: String);
var s: string;
begin
  s := NewsSource + ': ' + Bulletin + ' (' + IntToStr(MsgID) + ')';
  Application.MessageBox(pChar(s),pChar('TWS Flash Bulletin'),MB_OK or MB_ICONINFORMATION);
end;

procedure TFIABSocket.IABSocket1NewsProvider(Sender: TObject; Item, Count: Integer; ProviderCode, ProviderName: string);
var s: string;
begin
  s := IntToStr(Item) + ' ' + ProviderCode + ' ' + ProviderName;
  MemoAcct.Lines.Insert(0, s);
end;

procedure TFIABSocket.IABSocket1ExchangeStatus(Sender: TObject; MsgID: Integer; Status: TIABExchangeStatus; Bulletin, NewsSource: String);
var s: string;
begin
  s := NewsSource + ': ' + Bulletin + ' (' + IntToStr(MsgID) + ')';
  if Status = esAvailable then Application.MessageBox(pChar(s),pChar('Exchange Avialable'),MB_OK or MB_ICONINFORMATION)
  else Application.MessageBox(pChar(s),pChar('Exchange NOT Avialable'),MB_OK or MB_ICONWARNING);
end;

procedure TFIABSocket.ButtonNewsBulletinsClick(Sender: TObject);
begin
  IABSocket1.GetNewsBulletins(false);
end;

procedure TFIABSocket.IABSocket1ManagedAccounts(Sender: TObject; Details: String);
begin
  MemoAcct.Lines.Insert(0,'Managed Acct''s: ' + Details);
end;

{$IFDEF USE_BIGDECIMAL}
procedure TFIABSocket.IABSocket1MarketDepth(Sender: TObject; DataId, Index, Operation, Side: Integer; Size: BigDecimal; Price: Double);
{$ELSE}
procedure TFIABSocket.IABSocket1MarketDepth(Sender: TObject; DataId, Index, Operation, Side: Integer; Size, Price: Double);
{$ENDIF}
begin
  FDepth.AmendGrid(Index,Operation,Side,Size,Price,'');
end;

{$IFDEF USE_BIGDECIMAL}
procedure TFIABSocket.IABSocket1MarketLevel2(Sender: TObject; DataId, Index, Operation, Side: Integer; Size: BigDecimal; Price: Double; MMId: string; SmartDepth: Boolean);
{$ELSE}
procedure TFIABSocket.IABSocket1MarketLevel2(Sender: TObject; DataId, Index, Operation, Side: Integer; Size, Price: Double;  MMId: string; SmartDepth: Boolean);
{$ENDIF}
begin
  FDepth.AmendGrid(Index,Operation,Side,Size,Price,MMId);
end;

procedure TFIABSocket.AppOnHint(Sender: TObject);
begin
  StatusBar1.SimpleText := GetLongHint(Application.Hint);
end;

procedure TFIABSocket.ButtonHistoryClick(Sender: TObject);
var Contract: TIABContract;
begin
  if Sender = ButtonHistory then
    FHistoricalData.RadioButton1.Checked := true
  else
    FHistoricalData.RadioButton2.Checked := true;

  FHistoricalData.Label1.Caption := EditSym.Text + ' ' + EditExch.Text + ' ' + EditExp.Text;
  FHistoricalData.Left := left + 20;
  FHistoricalData.Top := Top + 30;
  FHistoricalData.ShowModal;
  if FHistoricalData.ModalResult <> mrOK then
    Exit;

  // MessageDlgPos('Chart available on Live accounts only.' + #13#10#13#10 + 'This chart request is preset to: 1 day, 5 min bars, all trading hours.  The API allows you full control over these parameters.',mtInformation,[mbOK],0,Left -10, Top + 50);

  if FHistoricalData.RadioButton2.Checked then // tick
    begin
      if HTDataId > 2000 then
        IABSocket1.CancelTickByTickData(HTDataId);
      inc(HTDataId);
      SetOrderParams;
      FillContractFromOrder(IABSocket1.DefOrder, Contract);
      IABSocket1.GetTickByTickData(HTDataId, Contract, FHistoricalData.TickType, FHistoricalData.TickCount, false);
    end
  else
    begin                   // bar data
      if SetOrderParams then
        with IABSocket1.DefOrder do
            IABSocket1.GetHistoricalData(CDataId, Symbol,LocalSymbol,Exchange,Expiry,Currency,Exchange,'',//Multiplier,
                                 SecurityType,Right,Strike,
                                 DateTimeToIABDateTimeStr(Now), 1, IAB_TIME_UNIT_DAY, FHistoricalData.BarPeriod,
                                 FHistoricalData.BarData, true, false, false, 1, 0, '', nil);
      inc(CDataId);
    end;


  // history formats
                    //             DateTimeToIABDateTimeStr(Now), 1, IAB_TIME_UNIT_DAY, bs5Min,   - today, 5 min bars
                    //             DateTimeToIABDateTimeStr(Now), 1, IAB_TIME_UNIT_DAY, bs1Min,   - today, 1 min bars
                    //             DateTimeToIABDateTimeStr(Now), 2, IAB_TIME_UNIT_DAY, bs1Min,  -  several days, 1 min bars
                    //  History dates now take a time zone, or UTC suffix.  see  DateTimeToIABDateTimeStr(Value: TDateTime; Timezone: string = '');
end;

procedure TFIABSocket.IABSocket1HistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
var i: Integer; DataForm: TFTextForm; Memo: TMemo; s: string;
const ForT: array [Boolean] of string = ('false','true');
begin
  DataForm := nil;
  for i := 0 to Screen.FormCount -1 do
    if (TFTextForm(Screen.Forms[i]).Tag = DataId) and (DataId > 0) then DataForm := TFTextForm(Screen.Forms[i]);

  if DataForm = nil then
    begin
      DataForm := TFTextForm.Create(Application);
      DataForm.Tag := DataId;
      DataForm.Caption := 'Bar data - DataId: ' + IntToStr(DataId);
      Memo := DataForm.Memo;
      Memo.Lines.Add('Format: Date/Time, Open, High, Low, Close, WAP, Volume, nTrades, HasGaps');
    end;

  //  ###  WARNING: DO NOT do anything complicated in this event - it blocks the stream and will bog down your app
  //  Instead just grab the data provided - save it to somewhere - and Post yourself a message.  This will allow this
  //  message event to complete, and the app live.

  Memo := DataForm.Memo;
  with HistoricalChartDataElement do
    begin
      s := Date + ', ' + FloatToStr(Open) + ', ' + FloatToStr(High) + ', ' + FloatToStr(Low) + ', ' + FloatToStr(Close) + ', ';
      {$IFDEF USE_BIGDECIMAL}
      s := s + WAP.ToPlainString + ', ' + Volume.ToPlainString;
      {$ELSE}
      s := s + FloatToStr(WAP) + ', ' + FormatFloat('0.#', Volume);
      {$ENDIF}
      s := s + ', ' + IntToStr(TradeCount) + ', ' + ForT[Hasgaps];
      Memo.Lines.Add(s);
    end;
  DataForm.Show;
end;

procedure TFIABSocket.IABSocket1HistoricalTickData(Sender: TObject; DataID: Integer; TickData: TIABTickData);
var i: Integer; DataForm: TFTextForm; Memo: TMemo; s: string;
const ForT: array [Boolean] of string = ('false','true');
begin
  DataForm := nil;
  for i := 0 to Screen.FormCount -1 do
    if (TFTextForm(Screen.Forms[i]).Tag = DataId) and (DataId > 0) then DataForm := TFTextForm(Screen.Forms[i]);

  if DataForm = nil then
    begin
      DataForm := TFTextForm.Create(Application);
      DataForm.Tag := DataId;
      DataForm.Caption := 'Tick data - DataId: ' + IntToStr(DataId);
      Memo := DataForm.Memo;
      case TickData.TickType of
        tdLast, tdAllLast:
          Memo.Lines.Add('Format: Date/Time, Price, Size, PastLimit, Unreported, Exchange, Notes');
      tdBidAsk:
          Memo.Lines.Add('Format: Date/Time, BidPrice, BidSize, AskSize, AskPrice, BidPastLow, AskPastHigh');
      tdMidPoint:
          Memo.Lines.Add('Format: Date/Time, Midpoint');
      end;
    end;

  Memo := DataForm.Memo;
  s := 'no data';
  with TickData do
    case TickType of
    {$IFDEF USE_BIGDECIMAL}
      tdLast, tdAllLast:
        s := DateTimeTostr(Time) + ', ' + FormatFloat('0.00#',Price) + ', ' + Size.ToPlainString + ', ' + ForT[PastLimit] + ', ' + ForT[Unreported] + ', ' + Exchange + ', ' + SpecialConditions;
      tdBidAsk:
        s := DateTimeTostr(Time) + ', ' + FormatFloat('0.00#',BidPrice) + ', ' + BidSize.ToPlainString + ', ' + AskSize.ToPlainString + ', ' + FormatFloat('0.00#',AskPrice) + ', ' + ForT[BidPastLow] + ', ' + ForT[AskPastHigh];
      tdMidPoint:
        s := DateTimeTostr(Time) + ', ' + FormatFloat('0.00#',MidPoint);
    {$ELSE}
      tdLast, tdAllLast:
        s := DateTimeTostr(Time) + ', ' + FormatFloat('0.00#',Price) + ', ' + FormatFloat('0.#',Size) + ', ' + ForT[PastLimit] + ', ' + ForT[Unreported] + ', ' + Exchange + ', ' + SpecialConditions;
      tdBidAsk:
        s := DateTimeTostr(Time) + ', ' + FormatFloat('0.00#',BidPrice) + ', ' + FormatFloat('0.#',BidSize) + ', ' + FormatFloat('0.#',AskSize) + ', ' + FormatFloat('0.00#',AskPrice) + ', ' + ForT[BidPastLow] + ', ' + ForT[AskPastHigh];
      tdMidPoint:
        s := DateTimeTostr(Time) + ', ' + FormatFloat('0.00#',MidPoint);
    {$ENDIF}
    end;

  Memo.Lines.Add(s);
  DataForm.Show;
end;

procedure TFIABSocket.IABSocket1BondSpecDetails(Sender: TObject;  Index: Integer);
var s: string; ot: TIABOrderType;
begin
{$IFDEF BCBCOMPILE}
  with PTIABBondSpecItem(IABSocket1.BondSpecs[Index])^ do
{$ELSE}
  with IABSocket1.BondSpecs[Index] do
{$ENDIF}
    begin
      s := MarketName + ' ' + TradingClass + ' ' + IntToStr(ContractId) + ' ' + Cusip + ' ' + Ratings + ' ' + BondType + ' ' + CouponType;
      for ot := Low(TIABOrderType) to High(TIABOrderType) do
        if (ot in OrderTypes) then s := s + ' ' + OrderTypeTostr(ot);
      s := s + ' ' + ValidExchanges;
    end;
  MemoAcct.Lines.Insert(0,s);
end;

procedure TFIABSocket.ButtonConnectionKeyPress(Sender: TObject; var Key: Char);
var s: string;
begin     // connect , disconnect
  if ButtonConnection.Tag <> 0 then Exit;
  if Key = 'a' then
    begin
      s := IABSocket1.TWSHostAddress;
      if InputQuery('Set TWS host IP','IP numbers', s) then
        IABSocket1.TWSHostAddress := s;
    end;
  if Key = 'p' then
    begin
      s := IntToStr(IABSocket1.TWSPort);
      if InputQuery('Set TWS Port','Port numbers', s) then
        IABSocket1.TWSPort := StrToInt(s);
    end;
  if Key = 'i' then
    begin
      s := IntToStr(IABSocket1.ClientId);
      if InputQuery('Set TWS ClientId','ClientID number', s) then
        IABSocket1.ClientId := StrToInt(s);
    end;
end;

procedure TFIABSocket.ButtonScannerClick(Sender: TObject);
begin
  FScanForm.Left := Left + 20;
  FScanForm.Top := Top + 50;

  //  NOTE - we initialize the struct here.. sets a lot of default values for TWS later.
  IABSocket1.Scanner.InitializeScanCriteria(@FScanForm.ScanCriteria);

  FScanForm.ShowModal;
  case FScanForm.Tag of
    1: IABSocket1.Scanner.GetScannerParameters;
    2: IABSocket1.Scanner.CancelScan(FScanForm.ScanId);
    3: IABSocket1.Scanner.NewScan(FScanForm.ScanId, FScanForm.ScanCriteria);
   end;
end;

procedure TFIABSocket.IABSocket1ScannerParam(Sender: TObject;  Parameters: String);
var i: Integer; DataForm: TFTextForm;
begin
  DataForm := nil;
  for i := 0 to Screen.FormCount -1 do
    if (TFTextForm(Screen.Forms[i]).Caption = 'Scanner parameters') then DataForm := TFTextForm(Screen.Forms[i]);

  if DataForm = nil then
    begin
      DataForm := TFTextForm.Create(Application);
      DataForm.Caption := 'Scanner parameters';
    end;
  DataForm.Memo.Clear;
  DataForm.Memo.Lines.Add(Parameters);
  DataForm.Show;
end;

procedure TFIABSocket.IABSocket1SymbolSample(Sender: TObject; DataID, Item, Count: Integer; SymbolDerivative: TIABSymbolDerivativeSpecItem);
var s: string;
begin
  with SymbolDerivative do
    begin
      s := IntToStr(DataID) + ': ' + IntToStr(Item) + ' ' + IntTostr(ContractId) + ' ' + Symbol + ' ' + SecurityTypeString[SecurityType] + ' ' + Currency + ' ' + PrimaryExchange;
      if DerivativeSecCount > 0 then
        s := s + ' Derivatives: ' + DerivativeSecTypes;
    end;
  MemoAcct.Lines.Insert(0, s);
end;

procedure TFIABSocket.IABSocket1ScannerData(Sender: TObject;  Scan: TIABScan);
var i: Integer; DataForm: TFTextForm; s: string;
begin
  DataForm := nil;
  for i := 0 to Screen.FormCount -1 do
    if (TFTextForm(Screen.Forms[i]).Tag = Scan.ScanId) and (Scan.ScanId > 0) then DataForm := TFTextForm(Screen.Forms[i]);

  if DataForm = nil then
    begin
      DataForm := TFTextForm.Create(Application);
      DataForm.Tag := Scan.ScanId;
      DataForm.Caption := 'ScanId: ' + IntToStr(Scan.ScanId);
    end;

  //  ###  WARNING: DO NOT do anything complicated in this event - it blocks the stream and will bog down your app
  //  Instead just grab the data provided - save it to somewhaere - and Post yourself a message.  This will allow this
  //  message event to continue, and the app live.

  DataForm.Memo.Clear;
  DataForm.Memo.Lines.Add('Format: Rank, Symbol, Expiry, Strike, Exch, Local, MarketName, TradingClass, Distance, Benchmark, Projection');
  for i := 0 to Scan.Count -1 do
    {$IFDEF BCBCOMPILE}
    with Scan.Items[i]^ do
    {$ELSE}
    with Scan.Items[i] do
    {$ENDIF}
      begin
        s := IntToStr(Rank) + ', ' + Symbol + ', ' + Expiry + ', ' + FloatToStr(Strike) + ', ' + Exchange + ', ' + LocalSymbol + ', ' + MarketName + ', ' + TradingClass + ', ' + Distance + ', ' + Benchmark + ', ' + Projection + ', ' + LegsString;
        DataForm.Memo.Lines.Add(s);
      end;
  DataForm.Show;
end;

//Sample Modify Order code : price or size
procedure TFIABSocket.ButtonModifyOrderClick(Sender: TObject);
var Order: TIABOrder;
begin
     // grab latest order - assuming its valid and the one to mod.
  if IABSocket1.Orders.Count = 0 then Exit;
  Order := IABSocket1.Orders[IABSocket1.Orders.Count -1];
  IABSocket1.ModifyOrder(Order.TempId,Order.ClientId,StrToInt(EditV.Text),otNoChange,StrToFloat(EditP.Text),0);
end;

procedure TFIABSocket.ButtonSearchClick(Sender: TObject);
var Inst: string;
begin
  Inst := InputBox('Instrument search', 'Enter a symbol to search for', 'AAPL');
  if Inst <> '' then
    IABSocket1.RequestMatchingSymbols(TDataId, Inst);
  inc(TDataId);
end;

procedure TFIABSocket.ButtonSnapClick(Sender: TObject);
begin
  if TIABSecurityType(ComboSecType.ItemIndex) = stBag then
    Exit;


  if SetOrderParams then
    with IABSocket1.DefOrder do
      IABSocket1.GetMarketSnapShot(CDataId, Symbol, LocalSymbol, Exchange, Expiry, Currency, PrimaryExchange, SecurityType, Right, Strike);

  inc(CDataId);
end;

procedure TFIABSocket.ButtonTWStimeClick(Sender: TObject);
begin
  IABSocket1.GetCurrentTWSTime;
end;

procedure TFIABSocket.IABSocket1CurrentTime(Sender: TObject; DateTime: TDateTime);
begin
  MemoAcct.Lines.Insert(0, 'TWS time: ' + DateTimeToStr(DateTime));
end;

procedure TFIABSocket.IABSocket1DepthMarketDataDescripItem(Sender: TObject; Item, Count: Integer;
  DepthMarketDataDescrip: TIABDepthMarketDataDescripItem);
var s: string;
begin
  with DepthMarketDataDescrip do
    begin
      s := IntToStr(Item) + ' ' + Exchange + ' ' + SecurityTypeString[SecurityType] + ' ' + ListingExchange + ' ' + ServiceDataType + ' ';
      if UNSET_INTEGER = AggGroup then
        s := s + '0'
      else
        s := s + IntToStr(AggGroup);
    end;
  MemoAcct.Lines.Insert(0, s);
end;

procedure TFIABSocket.ButtonRealTimebarsClick(Sender: TObject);
begin
  if SetOrderParams then
   with IABSocket1.DefOrder do
    IABSocket1.GetRealTimeData(CDataId, Symbol,LocalSymbol,Exchange,Expiry,Currency,Exchange, '',
                                 SecurityType,Right,Strike,
                                 bs5Sec,
                                 cdTrades, true, 0, '', nil);
  inc(CDataId);
end;



procedure TFIABSocket.IABSocket1RealTimeData(Sender: TObject; DataId: Integer; RealTimeDataElement: TIABRealTimeData);
var i: Integer; DataForm: TFTextForm; Memo: TMemo; s: string; IsNewForm: Boolean;
begin
  DataForm := nil;
  IsNewForm := false;

  for i := 0 to Screen.FormCount -1 do
    if (TFTextForm(Screen.Forms[i]).Tag = DataId) and (DataId > 0) then DataForm := TFTextForm(Screen.Forms[i]);

  if DataForm = nil then
    begin
      DataForm := TFTextForm.Create(Application);
      DataForm.Tag := DataId;
      DataForm.Caption := 'DataId: ' + IntToStr(DataId);
      Memo := DataForm.Memo;
      Memo.Lines.Add('Format: Time, Open, High, Low, Close, WAP, Volume, nTrades');
      IsNewForm := true;
    end;

  //  ###  WARNING: DO NOT do anything complicated in this event - it blocks the stream and will bog down your app
  //  Instead just grab the data provided - save it to somewhere - and Post yourself a message.  This will allow this
  //  message event to complete, and the app live.

  Memo := DataForm.Memo;
  with RealTimeDataElement do
    begin
      s := TimeToStr(DateTime) + ', ' + FloatToStr(Open) + ', ' + FloatToStr(High) + ', ' + FloatToStr(Low) + ', ' + FloatToStr(Close) + ', ';
      {$IFDEF USE_BIGDECIMAL}
      s := s + WAP.ToPlainString + ', ' + Volume.ToPlainString + ', ';
      {$ELSE}
      s := s + FloatToStr(WAP) + ', ' + FormatFloat('0.#', Volume) + ', ';
      {$ENDIF}
      s := s + IntToStr(TradeCount);
      Memo.Lines.Add(s);
      Inc(RealtimeBars);
      if RealtimeBars mod 15 = 0 then
        Memo.Lines.Add('Format: Time, Open, High, Low, Close, WAP, Volume, nTrades');
    end;
  if IsNewForm then
    DataForm.Show;
end;


procedure TFIABSocket.ButtonVerifyOrderClick(Sender: TObject);
begin

 //  A verify order tells TWS to check the order details and returns the commission details, etc, through the
 //  OnVerifyOrder event, and adds this order to the VerifiedOrders property.
 //  A verified order will not affect the account state or execute.

 //  To further PlaceOrder this verified order do this:
  //   IABSocket1.PlaceOrder(IABSocket1.VerifiedOrders[1]);

  if SetOrderParams and SetOrderPricing then
    IABSocket1.VerifyOrder(IABSocket1.DefOrder);
end;

procedure TFIABSocket.ButtonWSHorizonClick(Sender: TObject);
begin
  IABSocket1.RequestWSHorizonMetaData(TDataId);
  inc(TDataId);
end;

procedure TFIABSocket.IABSocket1VerifiedOrder(Sender: TObject; Order: TIABOrder);
var s: string;
begin
  s := IntToStr(Order.TempId) + ' ';
  MemoAcct.Lines.Insert(0,s +  'Status=' + Order.GetQueryResult.Status);

  if Order.GetQueryResult.MaintMarginChange = '' then  // pre 9.72
    MemoAcct.Lines.Insert(0,s +  'InitMarg=' + Order.GetQueryResult.InitMarginAfter + ' Maint=' + Order.GetQueryResult.MaintMarginAfter)
  else
    begin
      MemoAcct.Lines.Insert(0,s +  'Before: InitMarg=' + Order.GetQueryResult.InitMarginBefore + ' Maint=' + Order.GetQueryResult.MaintMarginBefore);
      MemoAcct.Lines.Insert(0,s +  'Change: InitMarg=' + Order.GetQueryResult.InitMarginChange + ' Maint=' + Order.GetQueryResult.MaintMarginChange);
      MemoAcct.Lines.Insert(0,s +  'After: InitMarg=' + Order.GetQueryResult.InitMarginAfter + ' Maint=' + Order.GetQueryResult.MaintMarginAfter);
    end;
  MemoAcct.Lines.Insert(0,s +  'Comm=' + FormatFloat('0.00',Order.GetQueryResult.Commission) + ' Currency=' + Order.GetQueryResult.CommissionCurrency);
  if Order.GetQueryResult.WarningText <> '' then
    MemoAcct.Lines.Insert(0,s +  'WARNING = ' + Order.GetQueryResult.WarningText);
end;

procedure TFIABSocket.IABSocket1WsHorizonMeta(Sender: TObject; DataID: Integer; DataStr: string);
var s: string;
begin
  s := IntToStr(DataID) + ' ' + DataStr;
  MemoAcct.Lines.Insert(0,s);
end;

//procedure TFIABSocket.IABSocket1InstrumentSpecDetailsReady(Sender: TObject;  DataID: Integer);
//begin
//  this even not really required...  only added to match IAB events.
//  use the  OnInstrumentSpecDetails event instead.


//end;

procedure TFIABSocket.IABSocket1EndOfStreamRead(Sender: TObject);
begin
//  Use this to do all your processing at the end of each data chunk.
//  The data stream from the TWS will have several price and tick events all in one chunk.
//  Hence just save all the single event item to temp locations, then process the data in one chunk.
//  NOTE  This event will still block the TWS thread...

{*
  Do something like this:
    * In each tick and price event, just capture the raw data and save it to a global var.
    * Then in this EndOfStreamRead event, use the saved tick / price data and do all your updating of data in your
      tracking and update any screen data.
    * You can also do your trading logic update here, but only if its quick.  If its slow or accesses other data,
      then see the next comment.
*}


{*
    If your code logic is slow, or generates new orders automatically based on bid/ask and price changes,
      or automatically placing closing orders after a fill, then you CANNOT do those new orders here.
      Instead you must use this function to save the required actions to local variables, and post yourself
      a message.  In that message handler, make the new orders. The purpose here is to allow the data
      thread and message queues from the TWS to finish and complete its tasks, and return to idle state,
      before issuing the TWS with new tasks.  You cannot have code that compounds instructions within
      instruction results to the TWS - it will lock up.  Example code:

    if Time_to_make_a_new_order then
      begin
        Save the intended changes to local var, or some state flag to indicate an action is required;
        PostMessage(Handle, UM_DOIT, 0, 1);
        See the next function DoIt.
      end;
*}
end;


{*
procedure TFIABSocket.DoIt(var Msg: TMessage);
begin
  if Msg.LParam = 1 then
    Do new order per saved details  ( create new orders / commands to the TWS )
end;
*}

procedure TFIABSocket.IABSocket1FundamentalData(Sender: TObject;  DataID: Integer; xmlStrData: String);
begin
  //  This requires a subscription to Reuters.
  //  Data is in the form of XML in the xmlStrData.
end;

procedure TFIABSocket.ComboBoxLogLevelChange(Sender: TObject);
begin
  IABSocket1.SetServerLogLevel(ComboBoxLogLevel.ItemIndex + 1);

  // error level tracking.  Set this to 5, and then read the
  //   help file about how to decode the saved log from TWS
  // Also see the {$DEFINE CAPTURE_TWS_STREAM}  at the top of the
  //  IABSocketAPI.pas file.
end;

procedure TFIABSocket.IABSocket1TickOptionComputation(Sender: TObject;
  DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice,
  pvDividend, Gamma, Vega, Theta, undPrice: Double);
begin
  LabelOptTick.Tag := LabelOptTick.Tag + 1;
  LabelOptTick.Caption := IntToStr(LabelOptTick.Tag);
end;

end.

