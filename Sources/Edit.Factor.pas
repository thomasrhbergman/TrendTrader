unit Edit.Factor;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, DaModule, Vcl.ComCtrls, IABFunctions, IABSocketAPI,
  Vcl.ExtCtrls, Vcl.Samples.Spin, Document, System.Actions, Vcl.ActnList, VirtualTrees, BrokerHelperAbstr, Monitor.Types,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABSocketAPI_const, Monitor.Interfaces, Common.Types,
  DaImages, Publishers.Interfaces, Global.Types, Publishers, IABFunctions.Helpers, InstrumentList;
{$ENDREGION}

type
  TfrmEditFactor = class(TFormDocument, IUpdateFeeds)
    aCancel: TAction;
    ActionListMain: TActionList;
    aGraph: TAction;
    aSave: TAction;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    btnShowTradeChart: TBitBtn;
    cbTickType1: TComboBox;
    cbTickType2: TComboBox;
    cbUseInAutoOrder: TCheckBox;
    lblAndOr: TLabel;
    lblConId: TLabel;
    lblConIdCaption: TLabel;
    lblContractType: TLabel;
    lblCurrency: TLabel;
    lblCurrencyCaption: TLabel;
    lblDecimals: TLabel;
    lblExchange: TLabel;
    lblExchangeCaption: TLabel;
    lblIBid: TLabel;
    lblIBidCaption: TLabel;
    lblName: TLabel;
    lblNameCaption: TLabel;
    lblReWeight: TLabel;
    lblReWeightCaption: TLabel;
    lblTickTypeCaption: TLabel;
    lblTypeCaption: TLabel;
    lblValue1: TLabel;
    lblValue2: TLabel;
    lblValueCaption: TLabel;
    pnlBottom: TPanel;
    seDecimals: TSpinEdit;
    procedure aCancelExecute(Sender: TObject);
    procedure aGraphExecute(Sender: TObject);
    procedure aGraphUpdate(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
    procedure cbTickType1Change(Sender: TObject);
    procedure cbTickType2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    const C_FORM_CAPTION = 'Condition factor [%s]';
  private
    { Private declarations }
    FBrokerType: TBrokerType;
    FChangeProc: Double;
    FDirday: string;
    FIBId: Integer;
    FId: Integer;
    FIdentifierList: string;
    FInstrumentName: string;
    FMarketList: string;
    FNNId: Integer;
    FOmxs30_w: Double;
    FLastPrice1: Double;
    FLastPrice2: Double;
    FSymbol: string;
    FUseIBFeeds: Boolean;
    function GetContractId: Integer;
    function GetContractType: string;
    function GetCurrency: string;
    function GetDecimals: Integer;
    function GetExchange: string;
    function GetTickType1: TIABTickType;
    function GetTickType2: TIABTickType;
    function GetUseInAutoOrder: Boolean;
    procedure SetBrokerType(const Value: TBrokerType);
    procedure SetContractId(const Value: Integer);
    procedure SetContractType(const Value: string);
    procedure SetCurrency(const Value: string);
    procedure SetDecimals(const Value: Integer);
    procedure SetExchange(const Value: string);
    procedure SetIBId(const Value: Integer);
    procedure SetInstrumentName(const Value: string);
    procedure SetTickType1(const Value: TIABTickType);
    procedure SetTickType2(const Value: TIABTickType);
    procedure SetUseInAutoOrder(const Value: Boolean);

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
  public
    class function ShowDocument(aDocument: TFactorDoc; aShowTemplate: Boolean = False): TModalResult;
    procedure AssignFromDoc(const aDocument: TFactorDoc);
    procedure AssignToDoc(var aDocument: TFactorDoc);
    procedure Initialize;
    procedure Deinitialize;

    property BrokerType     : TBrokerType  read FBrokerType       write SetBrokerType;
    property ChangeProc     : Double       read FChangeProc       write FChangeProc;
    property ContractId     : Integer      read GetContractId     write SetContractId;
    property ContractType   : string       read GetContractType   write SetContractType;
    property Currency       : string       read GetCurrency       write SetCurrency;
    property Decimals       : Integer      read GetDecimals       write SetDecimals;
    property Dirday         : string       read FDirday           write FDirday;
    property Exchange       : string       read GetExchange       write SetExchange;
    property IBId           : Integer      read FIBId             write SetIBId;
    property IdentifierList : string       read FIdentifierList   write FIdentifierList;
    property InstrumentName : string       read FInstrumentName   write SetInstrumentName;
    property LastPrice1     : Double       read FLastPrice1       write FLastPrice1;
    property LastPrice2     : Double       read FLastPrice2       write FLastPrice2;
    property MarketList     : string       read FMarketList       write FMarketList;
    property NNId           : Integer      read FNNId             write FNNId;
    property Omxs30_w       : Double       read FOmxs30_w         write FOmxs30_w;
    property Symbol         : string       read FSymbol           write FSymbol;
    property TickType1      : TIABTickType read GetTickType1      write SetTickType1;
    property TickType2      : TIABTickType read GetTickType2      write SetTickType2;
    property UseIBFeeds     : Boolean      read FUseIBFeeds       write FUseIBFeeds;
    property UseInAutoOrder : Boolean      read GetUseInAutoOrder write SetUseInAutoOrder;
  end;

implementation

{$R *.dfm}

class function TfrmEditFactor.ShowDocument(aDocument: TFactorDoc; aShowTemplate: Boolean = False): TModalResult;
begin
  Result := mrCancel;
  if Assigned(aDocument) then
  begin
    with TfrmEditFactor.Create(nil) do
    try
      Initialize;
      AssignFromDoc(aDocument);
      cbUseInAutoOrder.Visible := aShowTemplate;
      lblValue1.Caption := CurrToStr(TMonitorLists.PriceCache.GetLastPrice(ContractId, TickType1));
      lblValue2.Caption := CurrToStr(TMonitorLists.PriceCache.GetLastPrice(ContractId, TickType2));
      if (ShowModal = mrOk) then
      begin
        Result := mrOk;
        AssignToDoc(aDocument);
      end;
      Deinitialize;
    finally
      Free;
    end;
  end;
end;

procedure TfrmEditFactor.Initialize;
begin
  BrokerType := TBrokerType.brIB;
  UseIBFeeds := False;
  Decimals   := 2;
  lblReWeight.Caption := FloatToStrF(Omxs30_w, ffGeneral, 4, 4);

  cbTickType1.Items.Clear;
  cbTickType2.Items.Clear;
  for var TickType := ttBidSize to ttMotherFilledPrice do
  begin
    cbTickType1.Items.Add(TickType.ToString);
    cbTickType2.Items.Add(TickType.ToString);
  end;
end;

procedure TfrmEditFactor.Deinitialize;
begin

end;

procedure TfrmEditFactor.FormCreate(Sender: TObject);
begin
  inherited;
  TPublishers.FeedPublisher.Subscribe(Self);
end;

procedure TfrmEditFactor.FormDestroy(Sender: TObject);
begin
  TPublishers.FeedPublisher.Unsubscribe(Self);
  inherited;
end;

function TfrmEditFactor.GetContractId: Integer;
begin
  Result := -1;
  case BrokerType of
    TBrokerType.brIB:
      Result := IBId;
    TBrokerType.brNN:
      begin
        if UseIBFeeds then
          Result := IBId
        else
          Result := NNId;
      end;
    TBrokerType.brTest:
      Result := FId;
  end;
end;

procedure TfrmEditFactor.SetBrokerType(const Value: TBrokerType);
begin
  FBrokerType := Value;
  Self.Caption := Format(C_FORM_CAPTION, [Value.ToAbbrevName]);
end;

procedure TfrmEditFactor.SetCurrency(const Value: string);
begin
  lblCurrency.Caption := Value;
end;

function TfrmEditFactor.GetCurrency: string;
begin
  Result := lblCurrency.Caption;
end;

function TfrmEditFactor.GetExchange: string;
begin
  Result := lblExchange.Caption;
end;

function TfrmEditFactor.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmEditFactor.SetExchange(const Value: string);
begin
  lblExchange.Caption := Value;
end;

procedure TfrmEditFactor.SetDecimals(const Value: Integer);
begin
  seDecimals.Value := Value;
end;

function TfrmEditFactor.GetContractType: string;
begin
  Result := lblContractType.Caption;
end;

procedure TfrmEditFactor.SetContractType(const Value: string);
begin
  lblContractType.Caption := Value;
end;

function TfrmEditFactor.GetDecimals: Integer;
begin
  Result := seDecimals.Value;
end;

procedure TfrmEditFactor.SetIBId(const Value: Integer);
begin
  FIBId := Value;
  lblIBid.Caption := Value.ToString;
end;

procedure TfrmEditFactor.SetContractId(const Value: Integer);
begin
  lblConId.Caption := Value.ToString;
  case BrokerType of
    TBrokerType.brIB:
      IBId := Value;
    TBrokerType.brNN:
      NNId := Value;
    TBrokerType.brTest:
      FId := Value;
  end;
end;

procedure TfrmEditFactor.SetInstrumentName(const Value: string);
begin
  FInstrumentName := Value;
  lblName.Caption := Value;
end;

procedure TfrmEditFactor.SetTickType1(const Value: TIABTickType);
begin
  cbTickType1.ItemIndex := Integer(Value);
end;

procedure TfrmEditFactor.SetTickType2(const Value: TIABTickType);
begin
  cbTickType2.ItemIndex := Integer(Value);
end;

function TfrmEditFactor.GetTickType1: TIABTickType;
begin
  if (cbTickType1.ItemIndex > -1) then
    Result := TIABTickType(cbTickType1.ItemIndex)
  else
    Result := ttLast;
end;

function TfrmEditFactor.GetTickType2: TIABTickType;
begin
  if (cbTickType2.ItemIndex > -1) then
    Result := TIABTickType(cbTickType2.ItemIndex)
  else
    Result := ttNotSet;
end;

procedure TfrmEditFactor.cbTickType1Change(Sender: TObject);
begin
  inherited;
  if (cbTickType1.ItemIndex > -1) then
   lblValue1.Caption := CurrToStr(TMonitorLists.PriceCache.GetLastPrice(ContractId, TickType1));
end;

procedure TfrmEditFactor.cbTickType2Change(Sender: TObject);
begin
  inherited;
  if (cbTickType2.ItemIndex > -1) then
   lblValue2.Caption := CurrToStr(TMonitorLists.PriceCache.GetLastPrice(ContractId, TickType2));
end;

procedure TfrmEditFactor.SetUseInAutoOrder(const Value: Boolean);
begin
  cbUseInAutoOrder.Checked := Value;
end;

function TfrmEditFactor.GetUseInAutoOrder: Boolean;
begin
  Result := cbUseInAutoOrder.Checked;
end;

procedure TfrmEditFactor.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
begin
  if (Self.ContractId = Id) then
  begin
    if (Self.TickType1 = TickType) then
      lblValue1.Caption := CurrToStr(TMonitorLists.PriceCache.GetLastPrice(ContractId, TickType1));
    if (Self.TickType2 = TickType) then
      lblValue2.Caption := CurrToStr(TMonitorLists.PriceCache.GetLastPrice(ContractId, TickType2));
  end;
end;

procedure TfrmEditFactor.aCancelExecute(Sender: TObject);
begin
  inherited;
  ModalResult := mrCancel;
end;

procedure TfrmEditFactor.aGraphExecute(Sender: TObject);
var
  Monitor: IMonitor;
begin
  inherited;
  if Assigned(OwnerNode) and Supports(Application.MainForm, IMonitor, Monitor) then
    Monitor.ShowTradeChart(OwnerNode);
end;

procedure TfrmEditFactor.aGraphUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := TickType1 in [ttLast, ttAsk, ttBid, ttAskSize, ttBidSize];
end;

procedure TfrmEditFactor.aSaveExecute(Sender: TObject);
begin
  inherited;
  ModalResult := mrOk;
end;

procedure TfrmEditFactor.aSaveUpdate(Sender: TObject);
var
  Data: PTreeData;
begin
  inherited;
  if Assigned(OwnerNode) then
  begin
    Data := OwnerNode^.GetData;
    TAction(Sender).Enabled := Assigned(Data) and (Data.CreationType = ctUser);
  end
  else
    TAction(Sender).Enabled := False;
end;

procedure TfrmEditFactor.AssignFromDoc(const aDocument: TFactorDoc);
begin
  if Assigned(aDocument) then
  begin
    Self.LastPrice1     := aDocument.LastPrice1;
    Self.LastPrice2     := aDocument.LastPrice2;
    Self.BrokerType     := aDocument.BrokerType;
    Self.ChangeProc     := aDocument.ChangeProc;
    Self.ContractId     := aDocument.ContractId;
    Self.ContractType   := aDocument.ContractType;
    Self.Currency       := aDocument.Currency;
    Self.Decimals       := aDocument.Decimals;
    Self.Dirday         := aDocument.Dirday;
    Self.Exchange       := aDocument.Exchange;
    Self.IBId           := aDocument.IBId;
    Self.IdentifierList := aDocument.IdentifierList;
    Self.InstrumentName := aDocument.InstrumentName;
    Self.MarketList     := aDocument.MarketList;
    Self.NNId           := aDocument.NNId;
    Self.Omxs30_w       := aDocument.Omxs30_w;
    Self.OwnerNode      := aDocument.OwnerNode;
    Self.LastPrice1     := aDocument.LastPrice1;
    Self.LastPrice2     := aDocument.LastPrice2;
    Self.Symbol         := aDocument.Symbol;
    Self.TickType1      := aDocument.TickType1;
    Self.TickType2      := aDocument.TickType2;
    Self.UseIBFeeds     := aDocument.UseIBFeeds;
    Self.UseInAutoOrder := aDocument.UseInAutoOrder;
  end;
end;

procedure TfrmEditFactor.AssignToDoc(var aDocument: TFactorDoc);
begin
  if Assigned(aDocument) then
  begin
    aDocument.BrokerType     := Self.BrokerType;
    aDocument.ChangeProc     := Self.ChangeProc;
    aDocument.ContractId     := Self.ContractId;
    aDocument.ContractType   := Self.ContractType;
    aDocument.Currency       := Self.Currency;
    aDocument.Decimals       := Self.Decimals;
    aDocument.Dirday         := Self.Dirday;
    aDocument.Exchange       := Self.Exchange;
    aDocument.IBId           := Self.IBId;
    aDocument.IdentifierList := Self.IdentifierList;
    aDocument.InstrumentName := Self.InstrumentName;
    aDocument.MarketList     := Self.MarketList;
    aDocument.NNId           := Self.NNId;
    aDocument.Omxs30_w       := Self.Omxs30_w;
    aDocument.LastPrice1     := Self.LastPrice1;
    aDocument.LastPrice2     := Self.LastPrice2;
    aDocument.Symbol         := Self.Symbol;
    aDocument.TickType1      := Self.TickType1;
    aDocument.TickType2      := Self.TickType2;
    aDocument.UseIBFeeds     := Self.UseIBFeeds;
    aDocument.UseInAutoOrder := Self.UseInAutoOrder;
  end;
end;

end.
