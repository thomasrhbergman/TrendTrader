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
  IABFunctions.MarketData, ListForm;
{$ENDREGION}

type
  TfrmQualifierEdit = class(TCustomForm, IUpdateFeeds)
    aCalculateInstrument1: TAction;
    aCalculateInstrument2: TAction;
    ActionListMain: TActionList;
    aSave: TAction;
    aShowAutoTrades: TAction;
    aShowSearchInstruments: TAction;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    btnShowSearchCompare: TBitBtn;
    cbInequalityCompare: TComboBox;
    cbInstrument1IBValue1: TComboBox;
    cbInstrument2IBValue1: TComboBox;
    edtName: TEdit;
    lblCalculateTotal1: TLabel;
    lblCalculateTotal2: TLabel;
    lblContract1: TLabel;
    lblContract2: TLabel;
    lblContractVal1: TLabel;
    lblContractVal2: TLabel;
    lblInequalityInstrument1: TLabel;
    lblInequalityInstrument2: TLabel;
    lblInstrumentName1: TLabel;
    lblInstrumentName2: TLabel;
    lblInstrumentNameVal1: TLabel;
    lblInstrumentNameVal2: TLabel;
    lblName: TLabel;
    lblSymbol1: TLabel;
    lblSymbol2: TLabel;
    lblSymbolVal1: TLabel;
    lblSymbolVal2: TLabel;
    lblValue1: TLabel;
    lblValue2: TLabel;
    pnlBottom: TPanel;
    pnlComparePriceTotal: TPanel;
    pnlInstrument1: TPanel;
    pnlInstrument2: TPanel;
    pnlCompareSearch: TPanel;
    pnlHeader: TPanel;
    pnlCompareClient: TPanel;
    pnlCompare: TPanel;
    pnlCompareLeft: TPanel;
    cbCompare: TCheckBox;
    pnlValue: TPanel;
    pnlValueClient: TPanel;
    pnlValuePriceTotal: TPanel;
    cbInequalityValue: TComboBox;
    pnlInstrument: TPanel;
    lblInstrumentName: TLabel;
    lblInstrumentNameVal: TLabel;
    lblContractVal: TLabel;
    lblContract: TLabel;
    lblSymbolVal: TLabel;
    lblSymbol: TLabel;
    lblValue: TLabel;
    cbInstrumentIBValue: TComboBox;
    pnlValueSearch: TPanel;
    btnShowSearchValue: TBitBtn;
    pnlValueLeft: TPanel;
    cbValue: TCheckBox;
    edComparisonValue: TNumberBox;
    pnlTime: TPanel;
    pnlTimeCenter: TPanel;
    pnlTimeLeft: TPanel;
    cbTime: TCheckBox;
    lblFromTime: TLabel;
    edFromTime: TDateTimePicker;
    lblToTime: TLabel;
    edToTime: TDateTimePicker;
    lbComparisonValue: TLabel;
    procedure aCalculateInstrument1Execute(Sender: TObject);
    procedure aCalculateInstrument1Update(Sender: TObject);
    procedure aCalculateInstrument2Execute(Sender: TObject);
    procedure aCalculateInstrument2Update(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aShowSearchInstrumentsExecute(Sender: TObject);
    procedure cbInstrument1IBValue1Change(Sender: TObject);
    procedure cbInstrument2IBValue1Change(Sender: TObject);
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
    procedure cbInstrumentIBValueChange(Sender: TObject);
  private
    FQualifierItem: TQualifier;
    function CheckData: Boolean;
    //function GetCalcValue(aQualifierInstrument: TQualifierInstrument): Double;
    procedure CalculateInstrument1;
    procedure CalculateInstrument2;
    procedure QualifierInstrumentToGUI(Sender: TObject; aQualifierInstrument: TQualifierInstrument);
    procedure SokidInfoToGUI(Sender: TObject; aSokidInfo: TSokidInfo);
  public
    class function ShowEditForm(aItem: TBaseClass; aDialogMode: TDialogMode): TModalResult; override;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmQualifierEdit.ShowEditForm(aItem: TBaseClass; aDialogMode: TDialogMode): TModalResult;
begin
  with TfrmQualifierEdit.Create(nil) do
  try
    DialogMode := aDialogMode;
    FQualifierItem.AssignFrom(TQualifier(aItem));
    Initialize;
    Result := ShowModal;
    if (Result = mrOk) then
    begin
      Denitialize;
      TQualifier(aItem).AssignFrom(FQualifierItem);
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
  cbInequalityCompare.Items.Clear;
  cbInequalityValue.Items.Clear;
  for var it := Low(TInequalityType) to High(TInequalityType) do
    if not (it in [iqBetween]) then
    begin
      cbInequalityCompare.Items.Add(it.ToFullName);
      cbInequalityValue.Items.Add(it.ToFullName);
    end;
  cbInequalityCompare.ItemIndex := Ord(FQualifierItem.InequalityCompare);
  cbInequalityValue.ItemIndex := Ord(FQualifierItem.InequalityValue);

  cbInstrument1IBValue1.Items.Clear;
  cbInstrument2IBValue1.Items.Clear;
  cbInstrumentIBValue.Items.Clear;
  for var TickType := ttBidSize to ttFuturesOpenInterest do
  begin
    cbInstrument1IBValue1.Items.Add(TickType.ToString);
    cbInstrument2IBValue1.Items.Add(TickType.ToString);
    cbInstrumentIBValue.Items.Add(TickType.ToString);
  end;
  if (DialogMode = dmInsert) then
  begin
    FQualifierItem.Instrument1.TickType1 := ttLast;
    FQualifierItem.Instrument1.TickType2 := ttClose;
    FQualifierItem.Instrument2.TickType1 := ttLast;
    FQualifierItem.Instrument2.TickType2 := ttClose;
    FQualifierItem.Instrument1.TypeOperation := toDivide;
    FQualifierItem.Instrument2.TypeOperation := toDivide;
    FQualifierItem.Instrument.TickType1 := ttLast;
    FQualifierItem.Instrument.TickType2 := ttClose;
    FQualifierItem.Instrument.TypeOperation := toDivide;
  end;

  QualifierInstrumentToGUI(pnlInstrument1, FQualifierItem.Instrument1);
  QualifierInstrumentToGUI(pnlInstrument2, FQualifierItem.Instrument2);
  QualifierInstrumentToGUI(pnlInstrument, FQualifierItem.Instrument);

  if (FQualifierItem.Instrument1.SokidInfo.ContractId > 0) then
    TIABMarket.RequestMarketData(FQualifierItem.Instrument1.SokidInfo.ContractId);
  if (FQualifierItem.Instrument2.SokidInfo.ContractId > 0) then
    TIABMarket.RequestMarketData(FQualifierItem.Instrument2.SokidInfo.ContractId);
  if (FQualifierItem.Instrument.SokidInfo.ContractId > 0) then
    TIABMarket.RequestMarketData(FQualifierItem.Instrument.SokidInfo.ContractId);

  if FQualifierItem.Name.IsEmpty then
    edtName.Text := 'Qualifier'
  else
    edtName.Text := FQualifierItem.Name;

  cbCompare.Checked := FQualifierItem.IsCompare;
  cbValue.Checked := FQualifierItem.IsValue;
  cbTime.Checked := FQualifierItem.IsTime;
  edFromTime.Time := FQualifierItem.FromTime;
  edToTime.Time := FQualifierItem.ToTime;
  edComparisonValue.Value := FQualifierItem.ComparisonValue;

  case DialogMode of
    dmInsert:
      begin
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
  end
  else if (Sender = pnlInstrument) then
  begin
    FQualifierItem.Instrument.SokidInfo  := aSokidInfo;
    lblInstrumentNameVal.Caption         := aSokidInfo.Name;
    lblContractVal.Caption               := aSokidInfo.ContractId.ToString;
    lblSymbolVal.Caption                 := aSokidInfo.Symbol;
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
  end
  else if (Sender = pnlInstrument2) then
  begin
    FQualifierItem.Instrument2      := aQualifierInstrument;
    lblInstrumentNameVal2.Caption   := aQualifierInstrument.SokidInfo.Name;
    lblContractVal2.Caption         := aQualifierInstrument.SokidInfo.ContractId.ToString;
    lblSymbolVal2.Caption           := aQualifierInstrument.SokidInfo.Symbol;
    cbInstrument2IBValue1.ItemIndex := Ord(aQualifierInstrument.TickType1);
  end
  else if (Sender = pnlInstrument) then
  begin
    FQualifierItem.Instrument       := aQualifierInstrument;
    lblInstrumentNameVal.Caption    := aQualifierInstrument.SokidInfo.Name;
    lblContractVal.Caption          := aQualifierInstrument.SokidInfo.ContractId.ToString;
    lblSymbolVal.Caption            := aQualifierInstrument.SokidInfo.Symbol;
    cbInstrumentIBValue.ItemIndex   := Ord(aQualifierInstrument.TickType1);
  end;
end;

procedure TfrmQualifierEdit.Denitialize;
begin
  if (cbInstrument1IBValue1.ItemIndex > -1) then
    FQualifierItem.Instrument1.TickType1 := TIABTickType(cbInstrument1IBValue1.ItemIndex)
  else
    FQualifierItem.Instrument1.TickType1 := ttLast;

  if (cbInstrument2IBValue1.ItemIndex > -1) then
    FQualifierItem.Instrument2.TickType1 := TIABTickType(cbInstrument2IBValue1.ItemIndex)
  else
    FQualifierItem.Instrument2.TickType1 := ttLast;

  if (cbInstrumentIBValue.ItemIndex > -1) then
    FQualifierItem.Instrument.TickType1 := TIABTickType(cbInstrumentIBValue.ItemIndex)
  else
    FQualifierItem.Instrument.TickType1 := ttLast;

  case FQualifierItem.TypeCondition of
    tcRealtime:
      begin
        FQualifierItem.StartupDate := 0;
        if (FQualifierItem.Instrument1.SokidInfo.ContractId > 0) then
          TIABMarket.CancelMarketData(FQualifierItem.Instrument1.SokidInfo.ContractId);
        if (FQualifierItem.Instrument2.SokidInfo.ContractId > 0) then
          TIABMarket.CancelMarketData(FQualifierItem.Instrument2.SokidInfo.ContractId);
        if (FQualifierItem.Instrument.SokidInfo.ContractId > 0) then
          TIABMarket.CancelMarketData(FQualifierItem.Instrument.SokidInfo.ContractId);
      end;
    tcEveryDay: ;
      //FQualifierItem.StartupDate := edEveryDayTime.Time;
    tcSpecificDate: ;
      //FQualifierItem.StartupDate := edSpecificDate.Date + edSpecificTime.Time;
  end;

  if (cbInequalityCompare.ItemIndex > -1) then
    FQualifierItem.InequalityCompare := TInequalityType(cbInequalityCompare.ItemIndex)
  else
    FQualifierItem.InequalityCompare := iqBelow;
  if (cbInequalityValue.ItemIndex > -1) then
    FQualifierItem.InequalityValue := TInequalityType(cbInequalityValue.ItemIndex)
  else
    FQualifierItem.InequalityValue := iqBelow;

  FQualifierItem.Name             := edtName.Text;
  FQualifierItem.IsCompare        := cbCompare.Checked;
  FQualifierItem.IsValue          := cbValue.Checked;
  FQualifierItem.IsTime           := cbTime.Checked;
  FQualifierItem.FromTime         := edFromTime.Time;
  FQualifierItem.ToTime           := edToTime.Time;
  FQualifierItem.ComparisonValue  := edComparisonValue.Value;
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
      end
      else
      if (Sender = pnlInstrument) then
      begin
        if (FQualifierItem.Instrument.SokidInfo.ContractId > 0) then
          TIABMarket.CancelMarketData(FQualifierItem.Instrument.SokidInfo.ContractId);
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

procedure TfrmQualifierEdit.cbInstrument2IBValue1Change(Sender: TObject);
begin
  if (cbInstrument2IBValue1.ItemIndex > -1) then
    FQualifierItem.Instrument2.TickType1 := TIABTickType(cbInstrument2IBValue1.ItemIndex)
  else
    FQualifierItem.Instrument2.TickType1 := ttLast;
  CalculateInstrument2;
end;

procedure TfrmQualifierEdit.cbInstrumentIBValueChange(Sender: TObject);
begin
  if (cbInstrumentIBValue.ItemIndex > -1) then
    FQualifierItem.Instrument.TickType1 := TIABTickType(cbInstrumentIBValue.ItemIndex)
  else
    FQualifierItem.Instrument.TickType1 := ttLast;
  //CalculateInstrument2;
end;

function TfrmQualifierEdit.CheckData: Boolean;
var
  Msg: string;
begin
  Msg := '';
  if (FQualifierItem.TypeCondition = tcRealtime) then
  begin
    if cbCompare.Checked then
    begin
      if (cbInequalityCompare.ItemIndex < 0) then
        Msg := Msg + 'Condition not selected! ' + sLineBreak;
      if (FQualifierItem.Instrument1.SokidInfo.ContractId <= 0) then
        Msg := Msg + 'Instrument1 not selected! ' + sLineBreak;
      if (FQualifierItem.Instrument2.SokidInfo.ContractId <= 0) then
        Msg := Msg + 'Instrument1 not selected! ' + sLineBreak;
    end;
    if cbValue.Checked then
    begin
      if (cbInequalityValue.ItemIndex < 0) then
        Msg := Msg + 'Condition not selected! ' + sLineBreak;
      if (FQualifierItem.Instrument.SokidInfo.ContractId <= 0) then
        Msg := Msg + 'Instrument not selected! ' + sLineBreak;
    end;
    if cbTime.Checked then
    begin
      if edFromTime.Time = 0 then
        Msg := Msg + 'From time is not entered! ' + sLineBreak;
      if edToTime.Time = 0 then
        Msg := Msg + 'To time is not entered! ' + sLineBreak;
    end;
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

{function TfrmQualifierEdit.GetCalcValue(aQualifierInstrument: TQualifierInstrument): Double;
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
end;}

procedure TfrmQualifierEdit.CalculateInstrument1;
begin
//  if (FQualifierItem.Instrument1.SokidInfo.ContractId > 0) then
//  begin
//    if (cbTypeOperation1.ItemIndex <= 0) and (cbInstrument1IBValue2.ItemIndex > -1) then
//      TMessageDialog.ShowWarning(rsTypeOperationNotSelected)
//    else
//    begin
//      lblCalculateTotal1.Caption :=  FormatFloat('0.00', GetCalcValue(FQualifierItem.Instrument1));
//      lblCalculateValue1.Caption := '=' + lblCalculateTotal1.Caption;
//      lblInstrument1IBValue1.Caption := FormatFloat('0.00', TPriceCache.PriceCache.GetLastPrice(FQualifierItem.Instrument1.SokidInfo.ContractId, FQualifierItem.Instrument1.TickType1));
//      lblInstrument1IBValue2.Caption := FormatFloat('0.00', TPriceCache.PriceCache.GetLastPrice(FQualifierItem.Instrument1.SokidInfo.ContractId, FQualifierItem.Instrument1.TickType2));
//    end;
//  end;
end;

procedure TfrmQualifierEdit.CalculateInstrument2;
begin
//  if (FQualifierItem.Instrument2.SokidInfo.ContractId > 0) then
//  begin
//    if (cbTypeOperation2.ItemIndex <= 0) and (cbInstrument2IBValue2.ItemIndex > -1) then
//      TMessageDialog.ShowWarning(rsTypeOperationNotSelected)
//    else
//    begin
//      lblCalculateTotal2.Caption := FormatFloat('0.00', GetCalcValue(FQualifierItem.Instrument2));
//      lblCalculateValue2.Caption := '=' + lblCalculateTotal2.Caption;
//      lblInstrument2IBValue1.Caption := FormatFloat('0.00', TPriceCache.PriceCache.GetLastPrice(FQualifierItem.Instrument2.SokidInfo.ContractId, FQualifierItem.Instrument2.TickType1));
//      lblInstrument2IBValue2.Caption := FormatFloat('0.00', TPriceCache.PriceCache.GetLastPrice(FQualifierItem.Instrument2.SokidInfo.ContractId, FQualifierItem.Instrument2.TickType2));
//    end;
//  end;
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

initialization
  ListFormFactory.RegisterList(ntQualifier, TQualifier, TfrmQualifierEdit);

end.
