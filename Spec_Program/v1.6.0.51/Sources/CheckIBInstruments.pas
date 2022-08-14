unit CheckIBInstruments;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.Samples.Spin, Data.DB, 
  Vcl.ComCtrls, System.Math, System.IOUtils, Document, DaModule, IABFunctions, IABSocketAPI,
  IABSocketAPI_const, HtmlLib, DebugWriter, CustomForms, Entity.Sokid, VirtualTrees, System.Actions, Vcl.ActnList,
  System.ImageList, Vcl.ImgList, MessageDialog, DaImages, Edit.Instrument, Global.Types, System.Win.TaskbarCore,
  Vcl.Taskbar, InstrumentList, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} VirtualTrees.ExportHelper,
  BrokerHelperAbstr, CustomJournalForm, Vcl.Menus, IBInstrumentParser, InformationDialog, ArrayHelper, Utils,
  System.Generics.Collections, System.Generics.Defaults, Publishers.Interfaces, IABFunctions.RequestsQueue,
  Monitor.EventController, Common.Types, Global.Resources, Publishers, IABFunctions.Helpers;
{$ENDREGION}

type
  TfrmCheckIBInstruments = class(TfrmCustomJournalForm, IInstrumentSpecDetails)
    aCheckInstruments: TAction;
    aCheckLastPrice: TAction;
    aClearFilter: TAction;
    aDelete: TAction;
    aEdit: TAction;
    aExit: TAction;
    aFilter: TAction;
    aImportFromCSV: TAction;
    aInfo: TAction;
    aRevertIsolate: TAction;
    btnCheckDependencies: TButton;
    btnCheckInstruments: TBitBtn;
    btnCheckLastPrice: TBitBtn;
    btnDelete: TBitBtn;
    btnEdit: TBitBtn;
    btnExit: TBitBtn;
    btnFilter: TButton;
    btnFilterClear: TButton;
    btnImportFromCSV: TBitBtn;
    btnRevertIsolate: TBitBtn;
    cbCurrency: TComboBox;
    cbIndustry: TComboBox;
    cbSecurityType: TComboBox;
    edName: TEdit;
    edSymbol: TEdit;
    gbDependence: TGroupBox;
    gbFilter: TGroupBox;
    lbDependence: TListBox;
    lblContractID: TLabel;
    lblCurrency: TLabel;
    lblIndustry: TLabel;
    lblName: TLabel;
    lblSecurityType: TLabel;
    lblSymbol: TLabel;
    OpenDialog: TFileOpenDialog;
    pnlBottom: TPanel;
    pnlButtons: TPanel;
    pnlFilter: TPanel;
    rgIsolate: TRadioGroup;
    seContractID: TSpinEdit;
    procedure aCheckInstrumentsExecute(Sender: TObject);
    procedure aCheckLastPriceExecute(Sender: TObject);
    procedure aClearFilterExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aDeleteUpdate(Sender: TObject);
    procedure aEditExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aFilterExecute(Sender: TObject);
    procedure aImportFromCSVExecute(Sender: TObject);
    procedure aInfoExecute(Sender: TObject);
    procedure aRevertIsolateExecute(Sender: TObject);
    procedure btnCheckDependenciesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    FNodeList: TDictionary<Integer, PVirtualNode>;
    procedure FilterSokid;

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IInstrumentSpecDetails
    procedure OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
//    procedure OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
  private const
    COL_CONTRACT_ID      = 0;
    COL_SYMBOL           = 1;
    COL_LOCAL_SYMBOL     = 2;
    COL_NAME             = 3;
    COL_CURRENCY         = 4;
    COL_DESCRIPTION      = 5;
    COL_SECURITY_TYPE    = 6;
    COL_EXCHANGE         = 7;
    COL_PRIMARY_EXCHANGE = 8;
    COL_DECIMALS         = 9;
    COL_MULTIPLIER       = 10;
    COL_LAST_PRICE       = 11;
    COL_EXPIRY           = 12;
    COL_ERROR_CODE       = 13;
    COL_ERROR_MSG        = 14;
    COL_ISOLATE          = 15;
    COL_UNDERLYING_CONID = 16;
    COL_INDUSTRY         = 17;
    COL_CATEGORY         = 18;
    COL_SUBCATEGORY      = 19;

    C_IDENTITY_NAME = 'CheckIBInstruments';
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure Deinitialize; override;
    class function ShowDocument: TModalResult;
  end;

implementation

{$R *.dfm}

class function TfrmCheckIBInstruments.ShowDocument: TModalResult;
begin
  with TfrmCheckIBInstruments.Create(nil) do
  try
    Initialize;
    Result := ShowModal;
    Deinitialize;
  finally
    Free;
  end;
end;

procedure TfrmCheckIBInstruments.FormCreate(Sender: TObject);
begin
  inherited;
  TPublishers.InstrumentSpecDetailsPublisher.Subscribe(Self);
  FNodeList := TDictionary<Integer, PVirtualNode>.Create;
end;

procedure TfrmCheckIBInstruments.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FNodeList);
  TPublishers.InstrumentSpecDetailsPublisher.Unsubscribe(Self);
end;

procedure TfrmCheckIBInstruments.Initialize;
begin
  inherited;
  cbSecurityType.Items.Clear;
  for var st := stStock to stAll do
    cbSecurityType.Items.Add(st.ToString);

  cbCurrency.Items.Text := General.XMLFile.ReadString('ScannerMain', 'OrderCurrencyList', C_DEFAULT_CURRENCY);
  cbCurrency.Text       := General.XMLFile.ReadString(C_IDENTITY_NAME, 'Currency', C_DEFAULT_CURRENCY);
  seContractID.Value    := General.XMLFile.ReadInteger(C_IDENTITY_NAME, 'ContractID', 0);
  edSymbol.Text         := General.XMLFile.ReadString(C_IDENTITY_NAME, 'Symbol', '');
  edName.Text           := General.XMLFile.ReadString(C_IDENTITY_NAME, 'Name', '');
  cbIndustry.Text       := General.XMLFile.ReadString(C_IDENTITY_NAME, 'Industry', '');
  cbSecurityType.Text   := General.XMLFile.ReadString(C_IDENTITY_NAME, 'SecurityType', '');
end;

procedure TfrmCheckIBInstruments.Deinitialize;
begin
  General.XMLFile.WriteInteger(C_IDENTITY_NAME, 'ContractID', seContractID.Value);
  General.XMLFile.WriteString(C_IDENTITY_NAME, 'Currency', cbCurrency.Text);
  General.XMLFile.WriteString(C_IDENTITY_NAME, 'Industry', cbIndustry.Text);
  General.XMLFile.WriteString(C_IDENTITY_NAME, 'Name', edName.Text);
  General.XMLFile.WriteString(C_IDENTITY_NAME, 'Symbol', edSymbol.Text);
  General.XMLFile.WriteString(C_IDENTITY_NAME, 'SecurityType', cbSecurityType.Text);
  inherited;
end;

procedure TfrmCheckIBInstruments.FilterSokid;
var
  Data: PSokidInfo;
  SokidInfo: TSokidInfo;
  Node: PVirtualNode;
  IsVisible: Boolean;
begin
  inherited;
  cbIndustry.Items.BeginUpdate;
  vstTree.BeginUpdate;
  try
    vstTree.Clear;
    FNodeList.Clear;
    for SokidInfo in SokidList.Values do
    begin
      if not SokidInfo.Industry.IsEmpty and (cbIndustry.Items.IndexOf(SokidInfo.Industry) = -1) then
        cbIndustry.Items.Add(SokidInfo.Industry);

      IsVisible := True;
      if (seContractID.Value > 0) then
        IsVisible := SokidInfo.ContractId.ToString.StartsWith(seContractID.Value.ToString);
      if not string(cbSecurityType.Text).IsEmpty then
        IsVisible := IsVisible and SokidInfo.SecurityType.StartsWith(cbSecurityType.Text);
      if not string(edSymbol.Text).IsEmpty then
        IsVisible := IsVisible and SokidInfo.Symbol.ToUpper.Contains(string(edSymbol.Text).ToUpper);
      if not string(edName.Text).IsEmpty then
        IsVisible := IsVisible and SokidInfo.Name.ToUpper.Contains(string(edName.Text).ToUpper);
      if not string(cbCurrency.Text).IsEmpty then
        IsVisible := IsVisible and SokidInfo.Currency.ToUpper.Contains(string(cbCurrency.Text).ToUpper);
      if not string(cbIndustry.Text).IsEmpty then
        IsVisible := IsVisible and SokidInfo.Industry.ToUpper.Contains(string(cbIndustry.Text).ToUpper);

      if (rgIsolate.ItemIndex = 1) then
        IsVisible := IsVisible and (SokidInfo.Isolate = 1)
      else if (rgIsolate.ItemIndex = 2) then
        IsVisible := IsVisible and (SokidInfo.Isolate = 0);

      if IsVisible then
      begin
        Node := vstTree.AddChild(nil);
        Data := Node^.GetData;
        Data^.AssignFrom(SokidInfo);
        FNodeList.Add(SokidInfo.ContractId, Node);
      end;
    end;
  finally
    vstTree.EndUpdate;
    cbIndustry.Items.EndUpdate;
  end;
end;

function TfrmCheckIBInstruments.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

function TfrmCheckIBInstruments.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmCheckIBInstruments.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if (Data^.TWSMessageItem.ErrorCode > 0) then
    TargetCanvas.Font.Color := clRed;

  case Column of
    COL_LAST_PRICE:
      if (Data^.LastPrice <= 0) then
        TargetCanvas.Font.Color := clRed
      else
        TargetCanvas.Font.Color := clBlack;
     COL_SYMBOL:
       TargetCanvas.Font.Style := [fsBold];
  end;
end;

procedure TfrmCheckIBInstruments.vstTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
resourcestring
  C_DEPEND = '%d-[%s] %s';
var
  Data: PSokidInfo;
  SokidInfo: TSokidInfo;
  arr: TArray<TSokidInfo>;
begin
  inherited;
  if Assigned(Node) then
  begin
    lbDependence.Items.BeginUpdate;
    try
      lbDependence.Clear;
      Data := Node^.GetData;
      if Assigned(Data) then
      begin
        arr := SokidList.GetDependencies(Data^.ContractId);
        for SokidInfo in arr do
          lbDependence.Items.Add(Format(C_DEPEND, [SokidInfo.ContractId, SokidInfo.LocalSymbol, FormatDateTime('YYYYMMDD', SokidInfo.Expiry)]));
      end;
    finally
      lbDependence.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmCheckIBInstruments.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmCheckIBInstruments.vstTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TSokidInfo);
end;

procedure TfrmCheckIBInstruments.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PSokidInfo;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    COL_CONTRACT_ID:
      CellText := Data.ContractId.ToString;
    COL_SYMBOL:
      CellText := Data.Symbol;
    COL_LOCAL_SYMBOL:
      CellText := Data.LocalSymbol;
    COL_NAME:
      CellText := Data.Name;
    COL_CURRENCY:
      CellText := Data.Currency;
    COL_DESCRIPTION:
      CellText := Data.Description;
    COL_SECURITY_TYPE:
      CellText := Data.SecurityType;
    COL_EXCHANGE:
      CellText := Data.Exchange;
    COL_PRIMARY_EXCHANGE:
      CellText := Data.PrimaryExchange;
    COL_DECIMALS:
      CellText := Data.Decimals.ToString;
    COL_MULTIPLIER:
      CellText := Data.Multiplier;
    COL_LAST_PRICE:
      CellText := Format('%.5f', [Data.LastPrice]);
    COL_EXPIRY:
      if (Data.Expiry > 0) then
        CellText := FormatDateTime('DD.MM.YYYY hh:nn', Data.Expiry)
      else
        CellText := '';
    COL_ERROR_CODE:
      if (Data.TWSMessageItem.ErrorCode > -1) then
        CellText := Data.TWSMessageItem.ErrorCode.ToString
      else
        CellText := '';
    COL_ERROR_MSG:
      CellText := Data.TWSMessageItem.ErrorMsg;
    COL_ISOLATE:
      CellText := Data.Isolate.ToString;
    COL_UNDERLYING_CONID:
      if (Data.UnderlyingConId > 0) then
        CellText := Data.UnderlyingConId.ToString
      else
        CellText := '';
    COL_INDUSTRY:
      CellText := Data.Industry;
    COL_CATEGORY:
      CellText := Data.Category;
    COL_SUBCATEGORY:
      CellText := Data.Subcategory;
  end;
end;

procedure TfrmCheckIBInstruments.vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PSokidInfo;
begin
  Data := Node^.GetData;
  if (Data^.Isolate = 1) then
  begin
    TargetCanvas.Brush.Color := $00EEEEEE;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TfrmCheckIBInstruments.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PSokidInfo;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    COL_CONTRACT_ID:
      Result := CompareValue(Data1^.ContractId, Data2^.ContractId);
    COL_SYMBOL:
      Result := CompareText(Data1^.Symbol, Data2^.Symbol);
    COL_LOCAL_SYMBOL:
      Result := CompareText(Data1^.LocalSymbol, Data2^.LocalSymbol);
    COL_NAME:
      Result := CompareText(Data1^.Name, Data2^.Name);
    COL_CURRENCY:
      Result := CompareText(Data1^.Currency, Data2^.Currency);
    COL_DESCRIPTION:
      Result := CompareText(Data1^.Description, Data2^.Description);
    COL_SECURITY_TYPE:
      Result := CompareText(Data1^.SecurityType, Data2^.SecurityType);
    COL_EXCHANGE:
      Result := CompareText(Data1^.Exchange, Data2^.Exchange);
    COL_PRIMARY_EXCHANGE:
      Result := CompareText(Data1^.PrimaryExchange, Data2^.PrimaryExchange);
    COL_DECIMALS:
      Result := CompareValue(Data1^.Decimals, Data2^.Decimals);
    COL_MULTIPLIER:
      Result := CompareText(Data1^.Multiplier, Data2^.Multiplier);
    COL_LAST_PRICE:
      Result := CompareValue(Data1^.LastPrice, Data2^.LastPrice);
    COL_EXPIRY:
      Result := CompareValue(Data1^.Expiry, Data2^.Expiry);
    COL_ERROR_CODE:
      Result := CompareValue(Data1^.TWSMessageItem.ErrorCode, Data2^.TWSMessageItem.ErrorCode);
    COL_ERROR_MSG:
      Result := CompareText(Data1^.TWSMessageItem.ErrorMsg, Data2^.TWSMessageItem.ErrorMsg);
    COL_ISOLATE:
      Result := CompareValue(Data1^.Isolate, Data2^.Isolate);
    COL_UNDERLYING_CONID:
      Result := CompareValue(Data1^.UnderlyingConId, Data2^.UnderlyingConId);
    COL_INDUSTRY:
      Result := CompareText(Data1^.Industry, Data2^.Industry);
    COL_CATEGORY:
      Result := CompareText(Data1^.Category, Data2^.Category);
    COL_SUBCATEGORY:
      Result := CompareText(Data1^.Subcategory, Data2^.Subcategory);
  end;
end;

//procedure TfrmCheckIBInstruments.OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
//begin
//
//end;

procedure TfrmCheckIBInstruments.OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
//var
//  Data: PSokidInfo;
//  Node: PVirtualNode;
begin
//  if (IABClient.InstrumentSpecs.Items[Index].DataID = TSokidList.C_DATA_ID) and
//    SokidList.ContainsKey(IABClient.InstrumentSpecs.Items[Index].ContractId) then
//  begin
//    Node := vstTree.AddChild(nil);
//    Data := Node^.GetData;
//    Data^.AssignFrom(SokidList.Items[IABClient.InstrumentSpecs.Items[Index].ContractId]);
//    if not Data^.Industry.IsEmpty and (cbIndustry.Items.IndexOf(Data^.Industry) = -1) then
//      cbIndustry.Items.Add(Data^.Industry);
//  end;
end;

procedure TfrmCheckIBInstruments.aDeleteExecute(Sender: TObject);
resourcestring
  rsDeletedFocused = 'Instrument %d "%s" (%s) be deleted. Continue?';
  rsDeletedSelected = '%d Instruments will be deleted. Continue?';
var
  Data: PSokidInfo;
  Node: PVirtualNode;
  NextNode: PVirtualNode;
begin
  Data := nil;
  vstTree.BeginUpdate;
  try
    if (vstTree.SelectedCount > 1) and (TMessageDialog.ShowQuestion((Format(rsDeletedSelected, [vstTree.SelectedCount]))) = mrYes) then
    begin
      Node := vstTree.GetFirstSelected;
      while Assigned(Node) do
      begin
        NextNode := vstTree.GetNextSelected(Node, False);
        Data := Node^.GetData;
        SokidList.DeleteItem(Data.ContractId);
        vstTree.DeleteNode(Node);
        Node := NextNode;
      end;
    end
    else
    begin
      Node := vstTree.FocusedNode;
      if Assigned(Node) then
        Data := Node^.GetData;
      if Assigned(Data) and (TMessageDialog.ShowQuestion(Format(rsDeletedFocused, [Data^.ContractId, Data^.Name, Data^.Symbol])) = mrYes) then
      begin
        SokidList.DeleteItem(Data.ContractId);
        vstTree.DeleteNode(Node);
      end;
    end;
  finally
    vstTree.EndUpdate;
    SetFocusSafely(vstTree);
  end;
end;

procedure TfrmCheckIBInstruments.aEditExecute(Sender: TObject);
var
  Data: PSokidInfo;
  Node: PVirtualNode;
begin
  Node := vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if TfrmEditInstrument.ShowDocument(Data.ContractId) = mrOk then
    begin
      Data^.AssignFrom(SokidList.Items[Data.ContractId]);
      vstTree.InvalidateNode(Node);
      SetFocusSafely(vstTree);
    end;
  end;
end;

procedure TfrmCheckIBInstruments.aCheckInstrumentsExecute(Sender: TObject);
begin
  inherited;
  if (TMessageDialog.ShowQuestion(rsCheckInstruments) = mrYes) then
  begin
    TfrmEventController.ShowDocument(teCheckInstruments);
    SokidList.CheckInstruments;
  end;
end;

procedure TfrmCheckIBInstruments.aCheckLastPriceExecute(Sender: TObject);
begin
  inherited;
  if (TMessageDialog.ShowQuestion(rsUpdatePrice) = mrYes) then
  begin
    TfrmEventController.ShowDocument(teCheckLastPrice);
    SokidList.CheckLastPrice;
  end;
end;

procedure TfrmCheckIBInstruments.aDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not vstTree.IsEmpty;
end;

procedure TfrmCheckIBInstruments.aExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmCheckIBInstruments.aFilterExecute(Sender: TObject);
begin
  inherited;
  FilterSokid;
end;

procedure TfrmCheckIBInstruments.aImportFromCSVExecute(Sender: TObject);
resourcestring
  rsImportSuccessfully = 'Import completed successfully. ' + sLineBreak + '%d records imported';
  rsImportErrors       = 'Errors occurred while importing.' + sLineBreak + 'Imported %d records out of %d.' + sLineBreak + 'Look at the log file';
var
  arr: TArray<string>;
  SokidInfo: TArray<string>;
  Order : TIABOrder;
  Counter: Integer;
begin
  inherited;
  if OpenDialog.Execute then
  begin
    Counter := 0;
    arr := TFile.ReadAllLines(OpenDialog.FileName);
    vstTree.Clear;

    Order := TIABOrder.Create;
    try
      for var str in arr do
      begin
        SokidInfo := str.Split([';']); //197123382;ALIG;SEK;STK
        if (Length(SokidInfo) >= 5) and
          not (SokidInfo[0].IsEmpty or SokidInfo[1].IsEmpty or SokidInfo[2].IsEmpty or SokidInfo[3].IsEmpty or SokidInfo[4].IsEmpty) then
        begin
          IABClient.ClearOrder(Order);
          try
            Order.ContractId   := SokidInfo[0].ToInteger;
            Order.Symbol       := SokidInfo[1];
            Order.Currency     := SokidInfo[2];
            Order.SecurityType := TIABSecurityType.FromString(SokidInfo[3]);
            Order.Exchange     := SokidInfo[4];
            IABClient.SendRequest(ibGetInstrumentSpecs, Order.ContractId, Order);
          except
            TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ImportFromCSVExecute', 'Line error: ' + str);
          end;
          Inc(Counter);
        end
        else
          TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'ImportFromCSVExecute', 'Line error: ' + str);

        Application.ProcessMessages;
      end;
    finally
      FreeAndNil(Order);
    end;
    TMessageDialog.ShowInfo(Format(rsImportSuccessfully, [Counter]));
  end;
end;

procedure TfrmCheckIBInstruments.aRevertIsolateExecute(Sender: TObject);
resourcestring
  rsIsolatedFocused = 'Isolate for instrument %d "%s" (%s) will be reverted. Continue?';
  rsIsolatedSelected = 'Isolate for %d instruments will be reverted. Continue?';
var
  Data: PSokidInfo;
  Node: PVirtualNode;
  SokidInfo: TSokidInfo;
begin
  Data := nil;
  vstTree.BeginUpdate;
  try
    if (vstTree.SelectedCount > 1) and (TMessageDialog.ShowQuestion((Format(rsIsolatedSelected, [vstTree.SelectedCount]))) = mrYes) then
    begin
      Node := vstTree.GetFirstSelected;
      while Assigned(Node) do
      begin
        Data := Node^.GetData;
        if (Data^.Isolate = 0) then
          Data^.Isolate := 1
        else
          Data^.Isolate := 0;
        if SokidList.ContainsKey(Data^.ContractId) then
        begin
          SokidInfo := SokidList.Items[Data^.ContractId];
          SokidInfo.Isolate := Data^.Isolate;
          SokidList.SetValue(SokidInfo);
        end;
        Node := vstTree.GetNextSelected(Node, False);
      end;
    end
    else
    begin
      Node := vstTree.FocusedNode;
      if Assigned(Node) then
        Data := Node^.GetData;
      if Assigned(Data) and (TMessageDialog.ShowQuestion(Format(rsIsolatedFocused, [Data^.ContractId, Data^.Name, Data^.Symbol])) = mrYes) then
      begin
        if (Data^.Isolate = 0) then
          Data^.Isolate := 1
        else
          Data^.Isolate := 0;
        if SokidList.ContainsKey(Data^.ContractId) then
        begin
          SokidInfo := SokidList.Items[Data^.ContractId];
          SokidInfo.Isolate := Data^.Isolate;
          SokidList.SetValue(SokidInfo);
        end;
      end;
    end;
  finally
    vstTree.EndUpdate;
    SetFocusSafely(vstTree);
  end;
end;

procedure TfrmCheckIBInstruments.aClearFilterExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  inherited;
  vstTree.BeginUpdate;
  try
    Node := vstTree.GetFirst;
    while Assigned(Node) do
    begin
      Node^.States := Node^.States + [vsVisible];
      Node := Node.NextSibling;
    end;
  finally
    vstTree.EndUpdate;
  end;
  seContractID.Value  := 0;
  edSymbol.Text       := '';
  cbCurrency.Text     := '';
  cbSecurityType.Text := '';
  edName.Text         := '';
end;

procedure TfrmCheckIBInstruments.aInfoExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PSokidInfo;
begin
  inherited;
  Node := vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    TInformationDialog.ShowMessage(TIBHtmlParser.GetContent(Data^.ContractId), 'CheckIBInstruments');
  end;
end;

procedure TfrmCheckIBInstruments.btnCheckDependenciesClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PSokidInfo;
begin
  inherited;
  Node := vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    Data^.CheckDependencies;
  end;
end;

end.
