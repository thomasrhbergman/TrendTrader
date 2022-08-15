unit Qualifiers.Edit;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin, System.Math, Vcl.DBCtrls,
  System.Generics.Collections, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABFunctions, IABSocketAPI,
  IABSocketAPI_const, Qualifiers.Types, Vcl.ComCtrls, MessageDialog, DaModule, System.Actions, Vcl.ActnList,
  Search.Instruments,  VirtualTrees, Entity.Sokid, Data.DB, Scanner.Types, Monitor.Types, Document,
  System.DateUtils, BrokerHelperAbstr, Common.Types, DaImages, Global.Types, Vcl.Imaging.pngimage, Vcl.VirtualImage,
  Global.Resources, IABFunctions.Helpers, Vcl.NumberBox, Publishers.Interfaces, Publishers, InstrumentList,
  IABFunctions.MarketData;
{$ENDREGION}

type
  TfrmQualifierEdit = class(TCustomForm, IUpdateFeeds)
    aCalculateInstrument1: TAction;
    aCalculateInstrument2: TAction;
    ActionListMain: TActionList;
    aSave: TAction;
    aShowAutoTrades: TAction;
    aShowSearchInstruments: TAction;
    btnCalculateInstrument1: TButton;
    btnCalculateInstrument2: TButton;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    btnShowSearchForm: TBitBtn;
    cbBypass: TCheckBox;
    cbEnabled: TCheckBox;
    cbInequalityValue: TComboBox;
    cbInstrument1IBValue1: TComboBox;
    cbInstrument1IBValue2: TComboBox;
    cbInstrument2IBValue1: TComboBox;
    cbInstrument2IBValue2: TComboBox;
    cbTypeCondition: TComboBox;
    cbTypeOperation1: TComboBox;
    cbTypeOperation2: TComboBox;
    edEveryDayTime: TDateTimePicker;
    edSpecificDate: TDateTimePicker;
    edSpecificTime: TDateTimePicker;
    edtName: TEdit;
    imgWarning: TVirtualImage;
    lblCalculateTotal1: TLabel;
    lblCalculateTotal2: TLabel;
    lblCalculateValue1: TLabel;
    lblCalculateValue2: TLabel;
    lblContract1: TLabel;
    lblContract2: TLabel;
    lblContractVal1: TLabel;
    lblContractVal2: TLabel;
    lblEveryDayTime: TLabel;
    lblInequalityInstrument1: TLabel;
    lblInequalityInstrument2: TLabel;
    lblInfo: TLabel;
    lblInstrument1IBValue1: TLabel;
    lblInstrument1IBValue2: TLabel;
    lblInstrument2IBValue1: TLabel;
    lblInstrument2IBValue2: TLabel;
    lblInstrumentName1: TLabel;
    lblInstrumentName2: TLabel;
    lblInstrumentNameVal1: TLabel;
    lblInstrumentNameVal2: TLabel;
    lblName: TLabel;
    lblSpecificDate: TLabel;
    lblSpecificTime: TLabel;
    lblSymbol1: TLabel;
    lblSymbol2: TLabel;
    lblSymbolVal1: TLabel;
    lblSymbolVal2: TLabel;
    lblTypeCondition: TLabel;
    lblValue1: TLabel;
    lblValue2: TLabel;
    pcConditions: TPageControl;
    pnlAutoTrades: TPanel;
    pnlBottom: TPanel;
    pnlComparePrice: TPanel;
    pnlComparePriceTotal: TPanel;
    pnlInfo: TPanel;
    pnlInstrument1: TPanel;
    pnlInstrument2: TPanel;
    pnlSearchInstrument: TPanel;
    pnlTypeCondition: TPanel;
    tcCompare: TTabSheet;
    tsEveryDay: TTabSheet;
    tsSpecificDateTime: TTabSheet;
    procedure aCalculateInstrument1Execute(Sender: TObject);
    procedure aCalculateInstrument1Update(Sender: TObject);
    procedure aCalculateInstrument2Execute(Sender: TObject);
    procedure aCalculateInstrument2Update(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aShowSearchInstrumentsExecute(Sender: TObject);
    procedure cbInstrument1IBValue1Change(Sender: TObject);
    procedure cbInstrument1IBValue2Change(Sender: TObject);
    procedure cbInstrument2IBValue1Change(Sender: TObject);
    procedure cbInstrument2IBValue2Change(Sender: TObject);
    procedure cbTypeConditionChange(Sender: TObject);
    procedure cbTypeOperation1Change(Sender: TObject);
    procedure cbTypeOperation2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnInstrumentDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure OnInstrumentDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IUpdateFeeds
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
  private
    FQualifierItem: TQualifier;
    function CheckData: Boolean;
    function GetCalcValue(aQualifierInstrument: TQualifierInstrument): Double;
    procedure CalculateInstrument1;
    procedure CalculateInstrument2;
    procedure QualifierInstrumentToGUI(Sender: TObject; aQualifierInstrument: TQualifierInstrument);
    procedure SokidInfoToGUI(Sender: TObject; aSokidInfo: TSokidInfo);
  public
    class function ShowDocument(var aQualifierItem: TQualifier; aDialogMode: TDialogMode): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmQualifierEdit.ShowDocument(var aQualifierItem: TQualifier; aDialogMode: TDialogMode): TModalResult;
begin
  with TfrmQualifierEdit.Create(nil) do
  try
    DialogMode := aDialogMode;
    FQualifierItem.AssignFrom(aQualifierItem);
    Initialize;
    Result := ShowModal;
    if (Result = mrOk) then
    begin
      Denitialize;
      aQualifierItem.AssignFrom(FQualifierItem);
    end;
  finally
    Free;
  end;
end;

procedure TfrmQualifierEdit.FormCreate(Sender: TObject);
begin
  FQualifierItem := TQualifier.Create;
  TPublishers.FeedPublisher.Subscribe(Self);
end;

procedure TfrmQualifierEdit.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FQualifierItem);
  TPublishers.FeedPublisher.Unsubscribe(Self);
end;

procedure TfrmQualifierEdit.Initialize;
resourcestring
  rsCaption = '%s (v.%s)';
begin
  lblInfo.Caption := rsChangingDocument;
  cbInequalityValue.Items.Clear;
  for var it := Low(TInequalityType) to High(TInequalityType) do
    if not (it in [iqBetween]) then
      cbInequalityValue.Items.Add(it.ToFullName);
  cbInequalityValue.ItemIndex := Ord(FQualifierItem.InequalityType);
  pcConditions.ActivePageIndex := cbInequalityValue.ItemIndex;

  edEveryDayTime.Time := TimeOf(FQualifierItem.StartupDate);
  edSpecificTime.Time := TimeOf(FQualifierItem.StartupDate);
  if DateOf(FQualifierItem.StartupDate) = 0 then
    edSpecificDate.Date := Date
  else
    edSpecificDate.Date := DateOf(FQualifierItem.StartupDate);

  for var i := 0 to pcConditions.PageCount - 1 do
    pcConditions.Pages[i].TabVisible := False;

  cbTypeOperation1.Items.Clear;
  cbTypeOperation2.Items.Clear;
  for var TypeOperation := Low(TTypeOperation) to High(TTypeOperation) do
  begin
    cbTypeOperation1.Items.Add(TypeOperation.ToString);
    cbTypeOperation2.Items.Add(TypeOperation.ToString);
  end;

  cbInstrument1IBValue1.Items.Clear;
  cbInstrument1IBValue2.Items.Clear;
  cbInstrument2IBValue1.Items.Clear;
  cbInstrument2IBValue2.Items.Clear;
  for var TickType := ttBidSize to ttFuturesOpenInterest do
  begin
    cbInstrument1IBValue1.Items.Add(TickType.ToString);
    cbInstrument1IBValue2.Items.Add(TickType.ToString);
    cbInstrument2IBValue1.Items.Add(TickType.ToString);
    cbInstrument2IBValue2.Items.Add(TickType.ToString);
  end;
  if (DialogMode = dmInsert) then
  begin
    FQualifierItem.Instrument1.TickType1 := ttLast;
    FQualifierItem.Instrument1.TickType2 := ttClose;
    FQualifierItem.Instrument2.TickType1 := ttLast;
    FQualifierItem.Instrument2.TickType2 := ttClose;
    FQualifierItem.Instrument1.TypeOperation := toDivide;
    FQualifierItem.Instrument2.TypeOperation := toDivide;
  end;

  cbTypeCondition.Items.Clear;
  for var tc := Low(TTypeCondition) to High(TTypeCondition) do
    cbTypeCondition.Items.Add(tc.ToString);
  cbTypeCondition.ItemIndex := Ord(FQualifierItem.TypeCondition);
  pcConditions.ActivePageIndex := cbTypeCondition.ItemIndex;

  cbEnabled.Checked := FQualifierItem.Enabled;
  cbBypass.Checked := FQualifierItem.Bypass;
  QualifierInstrumentToGUI(pnlInstrument1, FQualifierItem.Instrument1);
  QualifierInstrumentToGUI(pnlInstrument2, FQualifierItem.Instrument2);

  if (FQualifierItem.Instrument1.SokidInfo.ContractId > 0) then
    TIABMarket.RequestMarketData(FQualifierItem.Instrument1.SokidInfo.ContractId);
  if (FQualifierItem.Instrument2.SokidInfo.ContractId > 0) then
    TIABMarket.RequestMarketData(FQualifierItem.Instrument2.SokidInfo.ContractId);

  if FQualifierItem.Name.IsEmpty then
    edtName.Text := 'Qualifier'
  else
    edtName.Text := FQualifierItem.Name;

  case DialogMode of
    dmInsert:
      begin
        cbEnabled.Checked := True;
        Self.Caption := Format(rsCaption, ['New Qualifier', General.ModuleVersion]);
      end;
    dmUpdate:
      Self.Caption := Format(rsCaption, ['Edit Qualifier', General.ModuleVersion]);
  end;
end;

procedure TfrmQualifierEdit.SokidInfoToGUI(Sender: TObject; aSokidInfo: TSokidInfo);
begin
  if (Sender = pnlInstrument1) then
  begin
    FQualifierItem.Instrument1.SokidInfo := aSokidInfo;
    lblInstrumentNameVal1.Caption        := aSokidInfo.Name;
    lblContractVal1.Caption              := aSokidInfo.ContractId.ToString;
    lblSymbolVal1.Caption                := aSokidInfo.Symbol;
    lblInequalityInstrument1.Caption     := aSokidInfo.Symbol;
  end
  else if (Sender = pnlInstrument2) then
  begin
    FQualifierItem.Instrument2.SokidInfo := aSokidInfo;
    lblInstrumentNameVal2.Caption        := aSokidInfo.Name;
    lblContractVal2.Caption              := aSokidInfo.ContractId.ToString;
    lblSymbolVal2.Caption                := aSokidInfo.Symbol;
    lblInequalityInstrument2.Caption     := aSokidInfo.Symbol;
  end;
  SokidList.SetValue(aSokidInfo);
end;

procedure TfrmQualifierEdit.QualifierInstrumentToGUI(Sender: TObject; aQualifierInstrument: TQualifierInstrument);
begin
  if (Sender = pnlInstrument1) then
  begin
    FQualifierItem.Instrument1      := aQualifierInstrument;
    lblInstrumentNameVal1.Caption   := aQualifierInstrument.SokidInfo.Name;
    lblContractVal1.Caption         := aQualifierInstrument.SokidInfo.ContractId.ToString;
    lblSymbolVal1.Caption           := aQualifierInstrument.SokidInfo.Symbol;
    cbInstrument1IBValue1.ItemIndex := Ord(aQualifierInstrument.TickType1);
    cbInstrument1IBValue2.ItemIndex := Ord(aQualifierInstrument.TickType2);
    cbTypeOperation1.ItemIndex      := Ord(aQualifierInstrument.TypeOperation);
  end
  else if (Sender = pnlInstrument2) then
  begin
    FQualifierItem.Instrument2      := aQualifierInstrument;
    lblInstrumentNameVal2.Caption   := aQualifierInstrument.SokidInfo.Name;
    lblContractVal2.Caption         := aQualifierInstrument.SokidInfo.ContractId.ToString;
    lblSymbolVal2.Caption           := aQualifierInstrument.SokidInfo.Symbol;
    cbInstrument2IBValue1.ItemIndex := Ord(aQualifierInstrument.TickType1);
    cbInstrument2IBValue2.ItemIndex := Ord(aQualifierInstrument.TickType2);
    cbTypeOperation2.ItemIndex      := Ord(aQualifierInstrument.TypeOperation);
  end;
end;

procedure TfrmQualifierEdit.Denitialize;
begin
  FQualifierItem.Enabled       := cbEnabled.Checked;
  FQualifierItem.TypeCondition := TTypeCondition(cbTypeCondition.ItemIndex);

  if (cbInstrument1IBValue1.ItemIndex > -1) then
    FQualifierItem.Instrument1.TickType1 := TIABTickType(cbInstrument1IBValue1.ItemIndex)
  else
    FQualifierItem.Instrument1.TickType1 := ttLast;

  if (cbInstrument1IBValue2.ItemIndex > -1) then
    FQualifierItem.Instrument1.TickType2 := TIABTickType(cbInstrument1IBValue2.ItemIndex)
  else
    FQualifierItem.Instrument1.TickType2 := ttNotSet;

  if (cbTypeOperation1.ItemIndex > -1) then
    FQualifierItem.Instrument1.TypeOperation := TTypeOperation(cbTypeOperation1.ItemIndex)
  else
    FQualifierItem.Instrument1.TypeOperation := toNone;

  if (cbInstrument2IBValue1.ItemIndex > -1) then
    FQualifierItem.Instrument2.TickType1 := TIABTickType(cbInstrument2IBValue1.ItemIndex)
  else
    FQualifierItem.Instrument2.TickType1 := ttLast;

  if (cbInstrument2IBValue2.ItemIndex > -1) then
    FQualifierItem.Instrument2.TickType2 := TIABTickType(cbInstrument2IBValue2.ItemIndex)
  else
    FQualifierItem.Instrument2.TickType2 := ttNotSet;

  if (cbTypeOperation2.ItemIndex > -1) then
    FQualifierItem.Instrument2.TypeOperation := TTypeOperation(cbTypeOperation2.ItemIndex)
  else
    FQualifierItem.Instrument2.TypeOperation := toNone;

  case FQualifierItem.TypeCondition of
    tcRealtime:
      begin
        FQualifierItem.StartupDate := 0;
        if (FQualifierItem.Instrument1.SokidInfo.ContractId > 0) then
          TIABMarket.CancelMarketData(FQualifierItem.Instrument1.SokidInfo.ContractId);
        if (FQualifierItem.Instrument2.SokidInfo.ContractId > 0) then
          TIABMarket.CancelMarketData(FQualifierItem.Instrument2.SokidInfo.ContractId);
      end;
    tcEveryDay:
      FQualifierItem.StartupDate := edEveryDayTime.Time;
    tcSpecificDate:
      FQualifierItem.StartupDate := edSpecificDate.Date + edSpecificTime.Time;
  end;

  if (cbInequalityValue.ItemIndex > -1) then
    FQualifierItem.InequalityType := TInequalityType(cbInequalityValue.ItemIndex)
  else
    FQualifierItem.InequalityType := iqBelow;

  FQualifierItem.Bypass := cbBypass.Checked;
  FQualifierItem.Name   := edtName.Text;
end;

procedure TfrmQualifierEdit.OnInstrumentDragDrop(Sender, Source: TObject; X, Y: Integer);
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
      if (Sender = pnlInstrument1) then
      begin
        if (FQualifierItem.Instrument1.SokidInfo.ContractId > 0) then
          TIABMarket.CancelMarketData(FQualifierItem.Instrument1.SokidInfo.ContractId);
      end
      else
      if (Sender = pnlInstrument2) then
      begin
        if (FQualifierItem.Instrument2.SokidInfo.ContractId > 0) then
          TIABMarket.CancelMarketData(FQualifierItem.Instrument2.SokidInfo.ContractId);
      end;
      TIABMarket.RequestMarketData(Data^.ContractId);
      SokidInfoToGUI(Sender, Data^);
    end;
  end;
end;

procedure TfrmQualifierEdit.OnInstrumentDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Assigned(Source) and (Source is TVirtualStringTree);
end;

procedure TfrmQualifierEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(frmSearchInstruments) and (frmSearchInstruments.StickForm = Self) then
    frmSearchInstruments.Close;
end;

procedure TfrmQualifierEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if (ModalResult = mrOk) then
  begin
    Denitialize;
    CanClose := CheckData;
  end;
end;

procedure TfrmQualifierEdit.aShowSearchInstrumentsExecute(Sender: TObject);
begin
  TfrmSearchInstruments.ShowDocument(Self);
end;

procedure TfrmQualifierEdit.cbInstrument1IBValue1Change(Sender: TObject);
begin
  if (cbInstrument1IBValue1.ItemIndex > -1) then
    FQualifierItem.Instrument1.TickType1 := TIABTickType(cbInstrument1IBValue1.ItemIndex)
  else
    FQualifierItem.Instrument1.TickType1 := ttLast;
  CalculateInstrument1;
end;

procedure TfrmQualifierEdit.cbInstrument1IBValue2Change(Sender: TObject);
begin
  if (cbInstrument1IBValue2.ItemIndex > -1) then
    FQualifierItem.Instrument1.TickType2 := TIABTickType(cbInstrument1IBValue2.ItemIndex)
  else
    FQualifierItem.Instrument1.TickType2 := ttNotSet;
  CalculateInstrument1;
end;

procedure TfrmQualifierEdit.cbTypeOperation1Change(Sender: TObject);
begin
  if (cbTypeOperation1.ItemIndex > -1) then
    FQualifierItem.Instrument1.TypeOperation := TTypeOperation(cbTypeOperation1.ItemIndex)
  else
    FQualifierItem.Instrument1.TypeOperation := toNone;
  CalculateInstrument1;
end;

procedure TfrmQualifierEdit.cbTypeOperation2Change(Sender: TObject);
begin
  if (cbTypeOperation2.ItemIndex > -1) then
    FQualifierItem.Instrument2.TypeOperation := TTypeOperation(cbTypeOperation2.ItemIndex)
  else
    FQualifierItem.Instrument2.TypeOperation := toNone;
  CalculateInstrument2;
end;

procedure TfrmQualifierEdit.cbInstrument2IBValue1Change(Sender: TObject);
begin
  if (cbInstrument2IBValue1.ItemIndex > -1) then
    FQualifierItem.Instrument2.TickType1 := TIABTickType(cbInstrument2IBValue1.ItemIndex)
  else
    FQualifierItem.Instrument2.TickType1 := ttLast;
  CalculateInstrument2;
end;

procedure TfrmQualifierEdit.cbInstrument2IBValue2Change(Sender: TObject);
begin
  if (cbInstrument2IBValue2.ItemIndex > -1) then
    FQualifierItem.Instrument2.TickType2 := TIABTickType(cbInstrument2IBValue2.ItemIndex)
  else
    FQualifierItem.Instrument2.TickType2 := ttNotSet;
  CalculateInstrument2;
end;

procedure TfrmQualifierEdit.cbTypeConditionChange(Sender: TObject);
begin
  if Showing then
    pcConditions.ActivePageIndex := cbTypeCondition.ItemIndex;
end;

function TfrmQualifierEdit.CheckData: Boolean;
var
  Msg: string;
begin
  Msg := '';
  if (FQualifierItem.TypeCondition = tcRealtime) then
  begin
    if (cbInequalityValue.ItemIndex < 0) then
      Msg := Msg + 'Condition not selected! ' + sLineBreak;
    if (FQualifierItem.Instrument1.SokidInfo.ContractId <= 0) then
      Msg := Msg + 'Instrument1 not selected! ' + sLineBreak;
    if (FQualifierItem.Instrument2.SokidInfo.ContractId <= 0) then
      Msg := Msg + 'Instrument1 not selected! ' + sLineBreak;
    if (cbInstrument1IBValue2.ItemIndex > -1) and (cbTypeOperation1.ItemIndex = -1) then
      Msg := Msg + 'TypeOperation1 not selected! ' + sLineBreak;
    if (cbInstrument2IBValue2.ItemIndex > -1) and (cbTypeOperation2.ItemIndex = -1) then
      Msg := Msg + 'TypeOperation2 not selected! ' + sLineBreak;
  end;

  Result := Msg.IsEmpty;
  if not Result then
    TMessageDialog.ShowWarning(Msg);
end;

procedure TfrmQualifierEdit.aSaveExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

function TfrmQualifierEdit.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmQualifierEdit.OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
begin
  if (FQualifierItem.Instrument1.SokidInfo.ContractId = Id) then
    if (FQualifierItem.Instrument1.TickType1 = TickType) or (FQualifierItem.Instrument1.TickType2 = TickType) then
      CalculateInstrument1;
  if (FQualifierItem.Instrument2.SokidInfo.ContractId = Id) then
    if (FQualifierItem.Instrument2.TickType1 = TickType) or (FQualifierItem.Instrument2.TickType2 = TickType) then
      CalculateInstrument2;
end;

function TfrmQualifierEdit.GetCalcValue(aQualifierInstrument: TQualifierInstrument): Double;
var
  Value1: Double;
  Value2: Double;
begin
  Value1 := TPriceCache.PriceCache.GetLastPrice(aQualifierInstrument.SokidInfo.ContractId, aQualifierInstrument.TickType1);
  if (aQualifierInstrument.TickType2 < ttNotSet) then
  begin
    Value2 := TPriceCache.PriceCache.GetLastPrice(aQualifierInstrument.SokidInfo.ContractId, aQualifierInstrument.TickType2);
    Result := aQualifierInstrument.TypeOperation.Calc(Value1, Value2);
  end
  else
    Result := Value1;
end;

procedure TfrmQualifierEdit.CalculateInstrument1;
begin
  if (FQualifierItem.Instrument1.SokidInfo.ContractId > 0) then
  begin
    if (cbTypeOperation1.ItemIndex <= 0) and (cbInstrument1IBValue2.ItemIndex > -1) then
      TMessageDialog.ShowWarning(rsTypeOperationNotSelected)
    else
    begin
      lblCalculateTotal1.Caption :=  FormatFloat('0.00', GetCalcValue(FQualifierItem.Instrument1));
      lblCalculateValue1.Caption := '=' + lblCalculateTotal1.Caption;
      lblInstrument1IBValue1.Caption := FormatFloat('0.00', TPriceCache.PriceCache.GetLastPrice(FQualifierItem.Instrument1.SokidInfo.ContractId, FQualifierItem.Instrument1.TickType1));
      lblInstrument1IBValue2.Caption := FormatFloat('0.00', TPriceCache.PriceCache.GetLastPrice(FQualifierItem.Instrument1.SokidInfo.ContractId, FQualifierItem.Instrument1.TickType2));
    end;
  end;
end;

procedure TfrmQualifierEdit.CalculateInstrument2;
begin
  if (FQualifierItem.Instrument2.SokidInfo.ContractId > 0) then
  begin
    if (cbTypeOperation2.ItemIndex <= 0) and (cbInstrument2IBValue2.ItemIndex > -1) then
      TMessageDialog.ShowWarning(rsTypeOperationNotSelected)
    else
    begin
      lblCalculateTotal2.Caption := FormatFloat('0.00', GetCalcValue(FQualifierItem.Instrument2));
      lblCalculateValue2.Caption := '=' + lblCalculateTotal2.Caption;
      lblInstrument2IBValue1.Caption := FormatFloat('0.00', TPriceCache.PriceCache.GetLastPrice(FQualifierItem.Instrument2.SokidInfo.ContractId, FQualifierItem.Instrument2.TickType1));
      lblInstrument2IBValue2.Caption := FormatFloat('0.00', TPriceCache.PriceCache.GetLastPrice(FQualifierItem.Instrument2.SokidInfo.ContractId, FQualifierItem.Instrument2.TickType2));
    end;
  end;
end;

procedure TfrmQualifierEdit.aCalculateInstrument1Execute(Sender: TObject);
begin
  CalculateInstrument1;
end;

procedure TfrmQualifierEdit.aCalculateInstrument2Execute(Sender: TObject);
begin
  CalculateInstrument2;
end;

procedure TfrmQualifierEdit.aCalculateInstrument2Update(Sender: TObject);
begin
  TAction(Sender).Enabled := (FQualifierItem.Instrument2.SokidInfo.ContractId > 0);
end;

procedure TfrmQualifierEdit.aCalculateInstrument1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := (FQualifierItem.Instrument1.SokidInfo.ContractId > 0);
end;

end.
