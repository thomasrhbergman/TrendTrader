unit DefinitionOptionalParameter;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, DaModule, Vcl.ComCtrls, IABFunctions,
  IABSocketAPI, Vcl.ExtCtrls, Vcl.Samples.Spin, Document, System.Actions, Vcl.ActnList, BrokerHelperAbstr, ArrayHelper,
  IABSocketAPI_const, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Monitor.Types, Vcl.CheckLst,
  Utils, Global.Types, InstrumentList, Entity.Sokid, DebugWriter, Search.Types, Publishers.Interfaces,
  IABFunctions.RequestsQueue, IABFunctions.MarketData, DaImages, Common.Types, Publishers, IABFunctions.Helpers;
{$ENDREGION}

type
  TfrmDefOptionalParameter = class(TCustomForm, IInstrumentSpecDetails,
                                                ITickOptionComputation,
                                                IUpdateFeeds,
                                                ISecurityDefinitionOptionalParameter)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    btnRequestContractDetails: TButton;
    cbContractId: TComboBox;
    cbExchange: TComboBox;
    cbExpirationsSelectAll: TCheckBox;
    cbPrimaryExchange: TComboBox;
    cbSecurityTypeInstr: TComboBox;
    cbStrikesSelectAll: TCheckBox;
    cbTradingClass: TCheckListBox;
    cbUnderlyingSymbol: TComboBox;
    gbMultiSelect: TGroupBox;
    gbOptionCharacteristics: TGroupBox;
    gbOptions: TGroupBox;
    gbSearchDerivatives: TGroupBox;
    gbStdDeviations: TGroupBox;
    gbStrikes: TGroupBox;
    lbExpirations: TCheckListBox;
    lblContractID: TLabel;
    lblContractIDCaption: TLabel;
    lblCurrency: TLabel;
    lblCurrencyCaption: TLabel;
    lblExchange: TLabel;
    lblExchangeC: TLabel;
    lblExchangeCaption: TLabel;
    lblLocalSymbol: TLabel;
    lblLocalSymbolCaption: TLabel;
    lblMaxWeeks: TLabel;
    lblPrimaryExchange: TLabel;
    lblSecurityType: TLabel;
    lblSecurityTypeCaption: TLabel;
    lblSecurityTypeInstr: TLabel;
    lblStrikePriceInterval: TLabel;
    lblSymbol: TLabel;
    lblSymbolCaption: TLabel;
    lblUnderlyingContractID: TLabel;
    lblUnderlyingSymbol: TLabel;
    lbStrikes: TCheckListBox;
    pnlBottom: TPanel;
    pnlExpirations: TPanel;
    pnlExpirationsBottom: TPanel;
    pnlExpirationsTop: TPanel;
    pnlLeft: TPanel;
    pnlStrikes: TPanel;
    pnlStrikesBottom: TPanel;
    pnlStrikesTop: TPanel;
    rbDev1_0: TRadioButton;
    rbDev1_5: TRadioButton;
    rbDev2_0: TRadioButton;
    rbDev2_5: TRadioButton;
    rbDevCustom: TRadioButton;
    rbStrike4: TRadioButton;
    rbStrike6: TRadioButton;
    rbStrike8: TRadioButton;
    rbStrike10: TRadioButton;
    rbStrikeAll: TRadioButton;
    rbStrikeCustom: TRadioButton;
    rgTradingClass: TRadioGroup;
    seDevCustom: TSpinEdit;
    seMaxWeeks: TSpinEdit;
    seStrikeCustom: TSpinEdit;
    seStrikePriceInterval: TSpinEdit;
    lblLastPrice: TLabel;
    procedure btnRequestContractDetailsClick(Sender: TObject);
    procedure cbExpirationsSelectAllClick(Sender: TObject);
    procedure cbStrikesSelectAllClick(Sender: TObject);
    procedure cbTradingClassClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnStrikeClick(Sender: TObject);
    procedure rgTradingClassClick(Sender: TObject);
  private const
    C_IDENTITY_NAME = 'Search';
    C_DATA_ID = 9;
    C_SEC_DEF_OPT = 1;
  private
    { Private declarations }
    FParameters: TOptionalParameters;
    FLastPrice: Currency;
    procedure LoadParamsFromXml;
    procedure ParametersToGUI;
    procedure ReFilterStrike(aNum: Integer);
    procedure SaveParamsToXml;
    procedure SetLastPrice(const Value: Currency);

    property LastPrice: Currency read FLastPrice write SetLastPrice;
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation ISecurityDefinitionOptionalParameter
    procedure OnSecurityDefinitionOptionalParameter(Sender: TObject; DataID: Integer; Exchange: string; UnderlyingConId: Integer; TradingClass, Multiplier, Expirations, Strikes: string);
    //implementation IInstrumentSpecDetails
    procedure OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
    //implementation ITickOptionComputation
    procedure OnTickOptionComputation(Sender: TObject; DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice: Double);
  public
    procedure Initialize;
    procedure Deinitialize;
    class function ShowDocument(var aParameters: TOptionalParameters): TModalResult;
  end;

implementation

{$R *.dfm}

{ TfrmDefOptionalParameter }

class function TfrmDefOptionalParameter.ShowDocument(var aParameters: TOptionalParameters): TModalResult;
begin
  Result := mrCancel;
  with TfrmDefOptionalParameter.Create(nil) do
    try
      FParameters := aParameters;
      Initialize;
      ShowModal;
      if (ModalResult = mrOk) then
      begin
        Deinitialize;
        aParameters := FParameters;
        Result := ModalResult;
      end;
    finally
      Free;
    end;
end;

procedure TfrmDefOptionalParameter.FormCreate(Sender: TObject);
var
  Request: TIABRequest;
begin
  Request := Default(TIABRequest);
  Request.DataType := mdtRealTime;
  Request.Command := ibGetMarketDataType;
  IABClient.SendRequest(Request);

  TPublishers.InstrumentSpecDetailsPublisher.Subscribe(Self);
  TPublishers.TickOptionComputationPublisher.Subscribe(Self);
  TPublishers.SecurityDefinitionOptionalParameterPublisher.Subscribe(Self);
  TPublishers.FeedPublisher.Subscribe(Self);
  FLastPrice := 0;
end;

procedure TfrmDefOptionalParameter.FormDestroy(Sender: TObject);
begin
  TPublishers.InstrumentSpecDetailsPublisher.Unsubscribe(Self);
  TPublishers.TickOptionComputationPublisher.Unsubscribe(Self);
  TPublishers.SecurityDefinitionOptionalParameterPublisher.Unsubscribe(Self);
  TPublishers.FeedPublisher.Unsubscribe(Self);
end;

function TfrmDefOptionalParameter.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmDefOptionalParameter.Initialize;
begin
  cbSecurityTypeInstr.Items.Clear;
  for var st := Low(TIABSecurityType) to High(TIABSecurityType) do
    cbSecurityTypeInstr.Items.Add(st.ToString);

  LoadParamsFromXml;
  ParametersToGUI;

  cbUnderlyingSymbol.Text  := FParameters.UnderlyingSymbol;
  cbExchange.Text          := FParameters.UnderlyingExchange;
  cbSecurityTypeInstr.Text := FParameters.UnderlyingSecurityType;
end;

procedure TfrmDefOptionalParameter.Deinitialize;
var
  i: Integer;
begin
  SaveParamsToXml;
  for i := Low(FParameters.ESParameters) to High(FParameters.ESParameters) do
    FParameters.StrikesArray.Clear;
  for i := 0 to lbStrikes.Count - 1 do
    if lbStrikes.Checked[i] then
      FParameters.StrikesArray.AddUnique(StrToFloatEx(lbStrikes.Items[i]));

  FParameters.ExpirationsArray.Clear;
  for i := 0 to lbExpirations.Count - 1 do
    if lbExpirations.Checked[i] then
      FParameters.ExpirationsArray.AddUnique(lbExpirations.Items[i].Replace('.', ''));

  if rbDev1_0.Checked then
    FParameters.StdDeviations := 1
  else if rbDev1_5.Checked then
    FParameters.StdDeviations := 1.5
  else if rbDev2_0.Checked then
    FParameters.StdDeviations := 2
  else if rbDev2_5.Checked then
    FParameters.StdDeviations := 2.5
  else
    FParameters.StdDeviations := seDevCustom.Value;

  FParameters.MaxWeeks            := seMaxWeeks.Value;
  FParameters.StrikePriceInterval := seStrikePriceInterval.Value;
end;

procedure TfrmDefOptionalParameter.LoadParamsFromXml;
var
  Items: TStringList;

  procedure JoinList(aDest: TComboBox; aSource: TStringList);
  begin
    for var j := 0 to aSource.Count - 1 do
      if (aDest.Items.IndexOf(aSource[j]) = -1) then
        aDest.Items.Add(aSource[j]);
  end;

begin
  cbUnderlyingSymbol.Text  := General.XMLFile.ReadString(C_IDENTITY_NAME, 'InstrumentSymbol', '');
  cbExchange.Text          := General.XMLFile.ReadString(C_IDENTITY_NAME, 'InstrumentExchange', '');
  cbPrimaryExchange.Text   := General.XMLFile.ReadString(C_IDENTITY_NAME, 'InstrumentPrimaryExchange', '');
  cbSecurityTypeInstr.Text := General.XMLFile.ReadString(C_IDENTITY_NAME, 'InstrumentSecurityType', '');
  cbContractId.Text        := General.XMLFile.ReadString(C_IDENTITY_NAME, 'InstrumentContractId', '');

  Items := TStringList.Create;
  try
    Items.Text := General.XMLFile.ReadString(C_IDENTITY_NAME, 'InstrumentSymbol.Items', '');
    JoinList(cbUnderlyingSymbol, Items);

    Items.Text := General.XMLFile.ReadString(C_IDENTITY_NAME, 'InstrumentExchange.Items', '');
    JoinList(cbExchange, Items);

    Items.Text := General.XMLFile.ReadString(C_IDENTITY_NAME, 'InstrumentPrimaryExchange.Items', '');
    JoinList(cbPrimaryExchange, Items);

    Items.Text := General.XMLFile.ReadString(C_IDENTITY_NAME, 'InstrumentContractId.Items', '');
    JoinList(cbContractId, Items);
  finally
    FreeAndNil(Items);
  end;
end;

procedure TfrmDefOptionalParameter.SaveParamsToXml;
begin
  try
    General.XMLFile.WriteString(C_IDENTITY_NAME, 'InstrumentSymbol', cbUnderlyingSymbol.Text);
    General.XMLFile.WriteString(C_IDENTITY_NAME, 'InstrumentExchange', cbExchange.Text);
    General.XMLFile.WriteString(C_IDENTITY_NAME, 'InstrumentPrimaryExchange', cbPrimaryExchange.Text);
    General.XMLFile.WriteString(C_IDENTITY_NAME, 'InstrumentSecurityType', cbSecurityTypeInstr.Text);
    General.XMLFile.WriteString(C_IDENTITY_NAME, 'InstrumentContractId', cbContractId.Text);

    General.XMLFile.WriteString(C_IDENTITY_NAME, 'InstrumentContractId.Items', cbContractId.Items.Text);
    General.XMLFile.WriteString(C_IDENTITY_NAME, 'InstrumentExchange.Items', cbExchange.Items.Text);
    General.XMLFile.WriteString(C_IDENTITY_NAME, 'InstrumentPrimaryExchange.Items', cbPrimaryExchange.Items.Text);
    General.XMLFile.WriteString(C_IDENTITY_NAME, 'InstrumentSymbol.Items', cbUnderlyingSymbol.Items.Text);
  finally
    General.XMLFile.Save;
  end;
end;

procedure TfrmDefOptionalParameter.SetLastPrice(const Value: Currency);
begin
  FLastPrice := Value;
  lblLastPrice.Caption := 'Last Price: ' + FormatFloat(C_CURRENCY_FORMAT, FLastPrice);
end;

procedure TfrmDefOptionalParameter.btnRequestContractDetailsClick(Sender: TObject);
var
  Order: TIABOrder;
begin
  FParameters.Clear;
  lbExpirations.Items.Clear;
  lbStrikes.Items.Clear;
  rgTradingClass.Items.Clear;
  cbTradingClass.Items.Clear;
  Order := TIABOrder.Create;
  try
    IABClient.ClearOrder(Order);
    Order.Symbol          := cbUnderlyingSymbol.Text;
    Order.Exchange        := cbExchange.Text;
    Order.PrimaryExchange := cbPrimaryExchange.Text;
    Order.SecurityType    := TIABSecurityType.FromString(cbSecurityTypeInstr.Text);

    if (cbContractId.Text <> '') then
      Order.ContractId := StrToIntDef(cbContractId.Text, 0);
    IABClient.SendRequest(ibGetInstrumentSpecs, C_DATA_ID, Order, qpHigh);
  finally
    FreeAndNil(Order);
  end;
  if (not string(cbUnderlyingSymbol.Text).IsEmpty) and (cbUnderlyingSymbol.Items.IndexOf(cbUnderlyingSymbol.Text) = -1) then
    cbUnderlyingSymbol.Items.Add(cbUnderlyingSymbol.Text);
  if (not string(cbExchange.Text).IsEmpty) and (cbExchange.Items.IndexOf(cbExchange.Text) = -1) then
    cbExchange.Items.Add(cbExchange.Text);
  if (not string(cbPrimaryExchange.Text).IsEmpty) and (cbPrimaryExchange.Items.IndexOf(cbPrimaryExchange.Text) = -1) then
    cbPrimaryExchange.Items.Add(cbPrimaryExchange.Text);
  if (not string(cbContractId.Text).IsEmpty) and (cbContractId.Items.IndexOf(cbContractId.Text) = -1) then
    cbContractId.Items.Add(cbContractId.Text);
end;

procedure TfrmDefOptionalParameter.OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
var
  Request: TIABRequest;
begin
  if (IABClient.InstrumentSpecs.Items[Index].DataID = C_DATA_ID) then
  begin
    FParameters.Symbol           := IABClient.InstrumentSpecs.Items[Index].Symbol;
    FParameters.LocalSymbol      := IABClient.InstrumentSpecs.Items[Index].LocalSymbol;
    FParameters.Exchange         := IABClient.InstrumentSpecs.Items[Index].Exchange;
    FParameters.UnderlyingConId  := IABClient.InstrumentSpecs.Items[Index].ContractId;
    FParameters.Currency         := IABClient.InstrumentSpecs.Items[Index].Currency;
    FParameters.UnderlyingSymbol := IABClient.InstrumentSpecs.Items[Index].Symbol;

    Request := Default(TIABRequest);
    Request.Command      := ibRequestSecDefOptParams;
    Request.Symbol       := FParameters.Symbol;
    Request.Exchange     := '';
    Request.SecurityType := cbSecurityTypeInstr.Text;
    Request.ContractId   := FParameters.UnderlyingConId;
    Request.DataID       := C_SEC_DEF_OPT;
    Request.Priority     := qpHigh;
    IABClient.SendRequest(Request);
    ParametersToGUI;
  end;
end;

procedure TfrmDefOptionalParameter.OnSecurityDefinitionOptionalParameter(Sender: TObject; DataID: Integer; Exchange: string; UnderlyingConId: Integer; TradingClass, Multiplier, Expirations, Strikes: string);
var
  Order: TIABOrder;
  ESParameter: TESParameter;
  arr: TArray<string>;
begin
  if (DataID = C_SEC_DEF_OPT) then
  begin
    TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self,
                                  'OnSecurityDefinitionOptionalParameter',
                                  'DataId='          + DataID.ToString + sLineBreak +
                                  'Exchange='        + Exchange + sLineBreak +
                                  'UnderlyingConId=' + UnderlyingConId.ToString + sLineBreak +
                                  'TradingClass='    + TradingClass + sLineBreak +
                                  'Multiplier='      + Multiplier + sLineBreak +
                                  'Expirations='     + Expirations + sLineBreak +
                                  'Strikes='         + Strikes);
    if (VarToIntDef(Multiplier, 100) = 100) then
    begin
      ESParameter.Expirations  := Expirations;
      ESParameter.Strikes      := Strikes;
      ESParameter.TradingClass := TradingClass;

      ESParameter.StrikesArray.Clear;
      if not Strikes.IsEmpty then
      begin
        arr := Strikes.Split([';']);
        for var i := Low(arr) to High(arr) do
          if not arr[i].IsEmpty then
            ESParameter.StrikesArray.AddUnique(StrToFloatEx(arr[i]));
         ESParameter.StrikesArray.Sort;
      end;

      ESParameter.ExpirationsArray.Clear;
      if not Expirations.IsEmpty then
      begin
        arr := Expirations.Split([';']);
        for var i := Low(arr) to High(arr) do
          if not arr[i].IsEmpty then
            ESParameter.ExpirationsArray.AddUnique(arr[i]);
        ESParameter.ExpirationsArray.Sort;
      end;
      SetLength(FParameters.ESParameters, Length(FParameters.ESParameters) + 1);
      FParameters.ESParameters[High(FParameters.ESParameters)] := ESParameter;

      FParameters.DataID                 := DataID;
      FParameters.Exchange               := Exchange;
      FParameters.Symbol                 := TradingClass;
      FParameters.UnderlyingConId        := UnderlyingConId;
      FParameters.UnderlyingExchange     := cbExchange.Text;
      FParameters.UnderlyingSecurityType := cbSecurityTypeInstr.Text;
      FParameters.UnderlyingSymbol       := cbUnderlyingSymbol.Text;

      if (LastPrice = 0) then
        LastPrice := TMonitorLists.PriceCache.GetLastPrice(UnderlyingConId, ttLast);
      if (LastPrice = 0) then
        LastPrice := SokidList.GetValueFromFeed(UnderlyingConId, ttLast);
      if (LastPrice = 0) then
      begin
        Order := TIABOrder.Create;
        try
          IABClient.ClearOrder(Order);
          Order.ContractId      := UnderlyingConId;
          Order.Currency        := FParameters.Currency;
          Order.Exchange        := FParameters.Exchange;
          Order.SecurityType    := FParameters.SecurityType;
          Order.Symbol          := FParameters.Symbol;
          TIABMarket.RequestMarketData(Order);
        finally
          FreeAndNil(Order);
        end;
      end;
      ParametersToGUI;
    end;
  end;
end;

procedure TfrmDefOptionalParameter.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
begin
  if (FParameters.UnderlyingConId = Id) and (TickType = ttLast) then
  begin
    TIABMarket.CancelMarketData(FParameters.UnderlyingConId);
    TIABMarket.CancelMarketData(Id);
    FLastPrice := Value;
    ReFilterStrike(FParameters.StrikeNum);
  end;
end;

procedure TfrmDefOptionalParameter.OnTickOptionComputation(Sender: TObject; DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice: Double);
begin

end;

procedure TfrmDefOptionalParameter.cbTradingClassClick(Sender: TObject);
var
  i: Integer;
  Strike: Double;
  Expire: string;
begin
  lbStrikes.Clear;
  if (FParameters.SecurityType = stOption) then
  begin
    FParameters.StrikesArray.Clear;
    for i := 0 to cbTradingClass.Items.Count - 1 do
      if cbTradingClass.Checked[i] then
        for Strike in FParameters.ESParameters[i].StrikesArray do
          FParameters.StrikesArray.AddUnique(Strike);

    FParameters.StrikesArray.Sort;
    OnStrikeClick(nil);
    if (FParameters.StrikeNum <> Integer.MaxValue) then
      ReFilterStrike(FParameters.StrikeNum);
  end
  else
    lbStrikes.Enabled := False;

  FParameters.ExpirationsArray.Clear;
  for i := 0 to cbTradingClass.Items.Count - 1 do
    if cbTradingClass.Checked[i] then
      for Expire in FParameters.ESParameters[i].ExpirationsArray do
        FParameters.ExpirationsArray.AddUnique(Expire);
  FParameters.ExpirationsArray.Sort;
  lbExpirations.Items.BeginUpdate;
  try
    lbExpirations.Clear;
    for Expire in FParameters.ExpirationsArray do
      lbExpirations.Items.Add(Expire);
  finally
    lbExpirations.Items.EndUpdate;
  end;
end;

procedure TfrmDefOptionalParameter.ParametersToGUI;
var
  i: Integer;
begin
  lblSymbol.Caption       := FParameters.Symbol;
  lblLocalSymbol.Caption  := FParameters.LocalSymbol;
  lblExchange.Caption     := FParameters.Exchange;
  lblCurrency.Caption     := FParameters.Currency;
  lblSecurityType.Caption := FParameters.SecurityType.ToString;

  if (FParameters.UnderlyingConId > 0) then
    lblContractID.Caption := FParameters.UnderlyingConId.ToString;

  rgTradingClass.Items.Clear;
  cbTradingClass.Items.Clear;
  for i := Low(FParameters.ESParameters) to High(FParameters.ESParameters) do
  begin
    rgTradingClass.Items.Add(FParameters.ESParameters[i].TradingClass);
    cbTradingClass.Items.Add(FParameters.ESParameters[i].TradingClass);
  end;
  rgTradingClass.ItemIndex := 0;
  rgTradingClassClick(nil);
end;

procedure TfrmDefOptionalParameter.ReFilterStrike(aNum: Integer);
var
  Index: Integer;
  StartIndex: Integer;
  EndIndex: Integer;
  i: Integer;
begin
  if (FParameters.StrikesArray.Size > 0) and (LastPrice > 0) then
  begin
    Index := 0;
    for i := Low(FParameters.StrikesArray.Items) to High(FParameters.StrikesArray.Items) do
      if (FParameters.StrikesArray[i] >= LastPrice) then
      begin
        Index := i;
        Break;
      end;
    StartIndex := Index - aNum div 2;
    EndIndex   := Index + aNum div 2 - 1;
    if (StartIndex < 0) then
      StartIndex := 0;
    if (EndIndex > High(FParameters.StrikesArray.Items)) then
      EndIndex := High(FParameters.StrikesArray.Items);

    lbStrikes.Items.BeginUpdate;
    try
      lbStrikes.Clear;
      for i := StartIndex to EndIndex do
        lbStrikes.Items.Add(FParameters.StrikesArray[i].ToString);
    finally
      lbStrikes.Items.EndUpdate;
    end;
    cbStrikesSelectAll.Checked := False;
  end;
end;

procedure TfrmDefOptionalParameter.rgTradingClassClick(Sender: TObject);
begin
  if (rgTradingClass.ItemIndex > -1) then
  begin
    lblSymbol.Caption  := FParameters.ESParameters[rgTradingClass.ItemIndex].TradingClass;
    FParameters.Symbol := FParameters.ESParameters[rgTradingClass.ItemIndex].TradingClass;
  end;
end;

procedure TfrmDefOptionalParameter.OnStrikeClick(Sender: TObject);
begin
  seStrikeCustom.Enabled := rbStrikeCustom.Checked;
  if rbStrike4.Checked then
    FParameters.StrikeNum := 4
  else if rbStrike6.Checked then
    FParameters.StrikeNum := 6
  else if rbStrike8.Checked then
    FParameters.StrikeNum := 8
  else if rbStrike10.Checked then
    FParameters.StrikeNum := 10
  else if rbStrikeCustom.Checked then
    FParameters.StrikeNum := seStrikeCustom.Value
  else
    FParameters.StrikeNum := Integer.MaxValue;
  ReFilterStrike(FParameters.StrikeNum);
end;

procedure TfrmDefOptionalParameter.cbExpirationsSelectAllClick(Sender: TObject);
begin
  if cbExpirationsSelectAll.Checked then
    lbExpirations.CheckAll(cbChecked, False, True)
  else
    lbExpirations.CheckAll(cbUnchecked, True, False);
end;

procedure TfrmDefOptionalParameter.cbStrikesSelectAllClick(Sender: TObject);
begin
  if cbStrikesSelectAll.Checked then
    lbStrikes.CheckAll(cbChecked, False, True)
  else
    lbStrikes.CheckAll(cbUnchecked, True, False);
end;

end.
