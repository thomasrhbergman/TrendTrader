unit Edit.Condition;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls, DateUtils, ParametersStore,
  DaModule, VirtualTrees, System.Math, IABFunctions, IABSocketAPI, Document, System.UITypes, System.Actions,
  Vcl.ActnList, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABSocketAPI_const, MessageDialog,
  Monitor.Types, Monitor.Interfaces, Common.Types, DaImages, Vcl.VirtualImage, Global.Resources, System.Types,
  IABFunctions.Helpers, MonitorTree.Document, InstrumentList, Vcl.NumberBox, Entity.Sokid, IABFunctions.MarketData,
  Publishers, System.Generics.Collections, Frame.Custom, Frame.ConditionHistory, OpenDialog.Conditions, XmlFiles,
  Search.Instruments;
{$ENDREGION}

type
  TfrmEditCondition = class(TFormDocument)
    aAddFactor: TAction;
    aCalculateCond: TAction;
    aGraph: TAction;
    alMain: TActionList;
    aOpenTemplate: TAction;
    aRefreshPrice: TAction;
    aSave: TAction;
    aSaveTemplate: TAction;
    aSaveTemplateAs: TAction;
    aSyncChildFactors: TAction;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    cbConditionType: TComboBox;
    cbPriority: TComboBox;
    edtName: TEdit;
    lblDescription: TLabel;
    lblPriorityCaption: TLabel;
    lblTypeCaption: TLabel;
    pcMain: TPageControl;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    lblInstrument: TLabel;
    lblInstrumentName: TLabel;
    btnShowSearchForm: TBitBtn;
    aShowSearchInstruments: TAction;
    tabRealTime: TTabSheet;
    gbMode: TGroupBox;
    rbPercent: TRadioButton;
    rbValue: TRadioButton;
    rbMOAP: TRadioButton;
    lblDivisionTickTypeCaption1: TLabel;
    cbTickType1: TComboBox;
    lblDivisionTickTypeCaption2: TLabel;
    cbTickType2: TComboBox;
    Label1: TLabel;
    lblComparisonRt: TLabel;
    cbInequalityRt: TComboBox;
    lblConditionLimit: TLabel;
    edtCondLimit: TNumberBox;
    tabTimeGap: TTabSheet;
    Label2: TLabel;
    dtpTimeStartValid: TDateTimePicker;
    Label3: TLabel;
    dtpTimeEndValid: TDateTimePicker;
    tabGradient: TTabSheet;
    lblMonitoringCaption: TLabel;
    edtMonitoring: TEdit;
    lblMonitoringSec: TLabel;
    cbInequalityGr: TComboBox;
    lblComparisonGr: TLabel;
    edtGradientValue: TNumberBox;
    lblGradientPercent: TLabel;
    procedure aAddFactorExecute(Sender: TObject);
    procedure aAddFactorUpdate(Sender: TObject);
    procedure aCalculateCondExecute(Sender: TObject);
    procedure aGraphExecute(Sender: TObject);
    procedure aRefreshPriceExecute(Sender: TObject);
    procedure aRefreshPriceUpdate(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
    procedure aSyncChildFactorsExecute(Sender: TObject);
    procedure cbConditionTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnTickTypeChange(Sender: TObject);
    procedure cbInequalityRtChange(Sender: TObject);
    procedure aShowSearchInstrumentsExecute(Sender: TObject);
    procedure pnlTopDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pnlTopDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private const
    C_IDENTITY_NAME = 'EditCondition';
  private
    FIsCondition: Boolean;
    //FMinMaxValueArray: TConditionDoc.TMinMaxValueArray;
    FModyfied: Boolean;
    FParametersStore: TParametersStore;
    FTemplateId: Integer;
    FTree: TVirtualStringTree;
    FUseInTemplate: Boolean;
    FValueArray: TConditionDoc.TValueArray;
    function AddFactor(const aParentNode: PVirtualNode; const aContractId: Integer): PVirtualNode;
    function GetCondLimit: Currency;
    function GetCondType: TConditionType;
    function GetCondName: string;
    function GetEndTime: TTime;
    function GetInequalityGr: TInequalityType;
    function GetInequalityRt: TInequalityType;
    function GetMonitoring: Integer;
    function GetPriority: TConditionDoc.TPriority;
    function GetStartTime: TTime;
    function GetTickType1: TIABTickType;
    function GetTickType2: TIABTickType;
    function GetValueArrayItem(Index: TConditionType): Double;
    function GetXmlParams: string;
    procedure SetCondLimit(const Value: Currency);
    procedure SetCondType(const Value: TConditionType);
    procedure SetCondName(const Value: string);
    procedure SetEndTime(const Value: TTime);
    procedure SetInequalityGr(const Value: TInequalityType);
    procedure SetInequalityRt(const Value: TInequalityType);
    procedure SetMonitoring(const Value: Integer);
    procedure SetPriority(const Value: TConditionDoc.TPriority);
    procedure SetStartTime(const Value: TTime);
    procedure SetTickType1(const Value: TIABTickType);
    procedure SetTickType2(const Value: TIABTickType);
    procedure SetTickTypesForChildFactors;
    procedure SetValueArrayItem(Index: TConditionType; Value: Double);
    procedure SetXmlParams(const Value: string);
    function GetGradientValue: Currency;
    procedure SetGradientValue(const Value: Currency);

    property Modyfied                 : Boolean        read FModyfied                   write FModyfied;
  public
    Instrument: TInstrument;
    procedure Initialize(aUseInTemplate: Boolean);
    procedure Deinitialize;
    procedure AssignFromDoc(const aDocument: TConditionDoc);
    procedure AssignToDoc(var aDocument: TConditionDoc);
    procedure SetSokidInfo(aSokidInfo: TSokidInfo);
    class function ShowDocument(aDocument: TConditionDoc): TModalResult; overload;
    class function ShowDocument(aDocument: TConditionDoc; const aTree: TVirtualStringTree): TModalResult; overload;

    property CondLimit         : Currency                read GetCondLimit         write SetCondLimit;
    property CondType          : TConditionType          read GetCondType          write SetCondType;
    property Name              : string                  read GetCondName          write SetCondName;
    property EndTime           : TTime                   read GetEndTime           write SetEndTime;
    property InequalityGr      : TInequalityType         read GetInequalityGr      write SetInequalityGr;
    property InequalityRt      : TInequalityType         read GetInequalityRt      write SetInequalityRt;
    property IsCondition       : Boolean                 read FIsCondition         write FIsCondition;
    property Monitoring        : Integer                 read GetMonitoring        write SetMonitoring;
    property Priority          : TConditionDoc.TPriority read GetPriority          write SetPriority;
    property StartTime         : TTime                   read GetStartTime         write SetStartTime;
    property TickType1         : TIABTickType            read GetTickType1         write SetTickType1;
    property TickType2         : TIABTickType            read GetTickType2         write SetTickType2;
    property XmlParams         : string                  read GetXmlParams         write SetXmlParams;
    property GradientValue     : Currency                read GetGradientValue     write SetGradientValue;
    property ValueArray[Index: TConditionType]: Double   read GetValueArrayItem    write SetValueArrayItem;
  end;

var
  frmEditCondition: TfrmEditCondition;

implementation

{$R *.dfm}

class function TfrmEditCondition.ShowDocument(aDocument: TConditionDoc): TModalResult;
begin
  Result := mrCancel;
  if Assigned(aDocument) then
  begin
    frmEditCondition := TfrmEditCondition.Create(nil);
    try
      frmEditCondition.Initialize(False);
      frmEditCondition.AssignFromDoc(aDocument);
      if (frmEditCondition.ShowModal = mrOk) then
      begin
        Result := mrOk;
        frmEditCondition.SetTickTypesForChildFactors;
        frmEditCondition.AssignToDoc(aDocument);
        frmEditCondition.Deinitialize;
      end;
    finally
      FreeAndNil(frmEditCondition);
    end;
  end;
end;

class function TfrmEditCondition.ShowDocument(aDocument: TConditionDoc; const aTree: TVirtualStringTree): TModalResult;
begin
  Result := mrCancel;
  if Assigned(aDocument) then
  begin
    frmEditCondition := TfrmEditCondition.Create(nil);
    try
      frmEditCondition.Initialize(True);
      frmEditCondition.FTree := aTree;
      frmEditCondition.AssignFromDoc(aDocument);
      if (frmEditCondition.ShowModal = mrOk) then
      begin
        Result := mrOk;
        frmEditCondition.SetTickTypesForChildFactors;
        frmEditCondition.AssignToDoc(aDocument);
        frmEditCondition.Deinitialize;
      end;
    finally
      FreeAndNil(frmEditCondition);
    end;
  end;
end;

procedure TfrmEditCondition.FormCreate(Sender: TObject);
var
  CondType: TConditionType;
begin
  inherited;
  cbConditionType.Items.Clear;
  //for CondType := System.Low(TConditionType) to System.High(TConditionType) do
  for CondType in [ctRealtimeValue, ctTimeGap, ctGradient] do
    cbConditionType.Items.Add(CondType.ToString);

  cbTickType1.Items.Clear;
  cbTickType2.Items.Clear;
  for var TickType := ttBidSize to ttMotherFilledPrice do
  begin
    cbTickType1.Items.Add(TickType.ToString);
    cbTickType2.Items.Add(TickType.ToString);
  end;

  {cbTypeOperation.Items.Clear;
  for var TypeOperation := Low(TTypeOperation) to toMult do
  begin
    cbTypeOperation.Items.Add(TypeOperation.ToString);
  end;}

  for CondType := System.Low(FValueArray) to System.High(FValueArray) do
    FValueArray[CondType] := 0;
  FTemplateId    := -1;
  Modyfied       := False;
  FUseInTemplate := False;

  dtpTimeStartValid.Time := Time;
  dtpTimeEndValid.Time   := IncMinute(Time, 10);

//  SetLength(FPriceArray, 0);
  FParametersStore := TParametersStore.Create;
  DMod.CheckConnect;
  FParametersStore.Connection     := DMod.ConnectionStock;
  FParametersStore.StoreComponent := Self;
  FParametersStore.IdentityName   := C_IDENTITY_NAME;
  FParametersStore.PropertiesList.Add('Active');
  FParametersStore.PropertiesList.Add('CondType');
  FParametersStore.PropertiesList.Add('CondValue');
  FParametersStore.PropertiesList.Add('CondWidth');
  FParametersStore.PropertiesList.Add('Description');
  FParametersStore.PropertiesList.Add('EndDate');
  FParametersStore.PropertiesList.Add('EndTime');
  FParametersStore.PropertiesList.Add('Gradient');
  FParametersStore.PropertiesList.Add('IsBreakUp');
  FParametersStore.PropertiesList.Add('MainFieldIndex');
  FParametersStore.PropertiesList.Add('Monitoring');
  FParametersStore.PropertiesList.Add('Priority');
  FParametersStore.PropertiesList.Add('SecondFieldIndex');
  FParametersStore.PropertiesList.Add('StartDate');
  FParametersStore.PropertiesList.Add('StartTime');
  FParametersStore.PropertiesList.Add('UpProc');
end;

procedure TfrmEditCondition.Initialize(aUseInTemplate: Boolean);
begin
  FUseInTemplate := aUseInTemplate;
  edtCondLimit.CurrencyString   := '';

  cbInequalityRt.Items.Clear;
  cbInequalityGr.Items.Clear;

  for var it := Low(TInequalityType) to High(TInequalityType) do
    if not(it in [iqBetween]) then
    begin
      cbInequalityRt.Items.Add(it.ToFullName);
      cbInequalityGr.Items.Add(it.ToFullName);
    end;
  cbInequalityRt.ItemIndex  := Ord(Low(TInequalityType));
  cbInequalityGr.ItemIndex  := Ord(Low(TInequalityType));
end;

procedure TfrmEditCondition.Deinitialize;
begin
  frmEditCondition.FParametersStore.Store;
end;

procedure TfrmEditCondition.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FParametersStore);
end;

procedure TfrmEditCondition.aSaveExecute(Sender: TObject);
begin
  inherited;
  {if (cbTypeOperation.ItemIndex <= 0) and ((Self.TickType2 < ttNotSet) or (Self.TickType2 = ttMotherFilledPrice)) then
  begin
    TMessageDialog.ShowWarning(rsTypeOperationNotSelected);
    ModalResult := mrNone;
  end
  else}
    ModalResult := mrOk;
end;

procedure TfrmEditCondition.aSaveUpdate(Sender: TObject);
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
    TAction(Sender).Enabled := Modyfied;
end;

procedure TfrmEditCondition.aShowSearchInstrumentsExecute(Sender: TObject);
begin
  inherited;
  TfrmSearchInstruments.ShowDocument(Self);
end;

procedure TfrmEditCondition.OnTickTypeChange(Sender: TObject);
begin
  inherited;
  //CalcValueChildNodes;
end;

procedure TfrmEditCondition.pnlTopDragDrop(Sender, Source: TObject; X,
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
      if (Sender = pnlTop) then
      begin
        if (Instrument.SokidInfo.ContractId > 0) then
          TIABMarket.CancelMarketData(Instrument.SokidInfo.ContractId);
      end;
      TIABMarket.RequestMarketData(Data^.ContractId);
      SetSokidInfo(Data^);
    end;
  end;
end;

procedure TfrmEditCondition.pnlTopDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited;
  Accept := Assigned(Source) and (Source is TVirtualStringTree);
end;

procedure TfrmEditCondition.aRefreshPriceExecute(Sender: TObject);
begin
  inherited;
  {if (cbTypeOperation.ItemIndex <= 0) and ((Self.TickType2 < ttNotSet) or (Self.TickType2 = ttMotherFilledPrice)) then
    TMessageDialog.ShowWarning(rsTypeOperationNotSelected)
  else
    CalcValueChildNodes;}
end;

procedure TfrmEditCondition.aRefreshPriceUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Visible := not FUseInTemplate;
end;

procedure TfrmEditCondition.cbConditionTypeChange(Sender: TObject);
begin

  case TConditionType(cbConditionType.ItemIndex) of
    ctRealtimeValue:
      begin
        pcMain.ActivePage := tabRealTime;
      end;
    ctTimeGap:
      begin
        pcMain.ActivePage := tabTimeGap;
      end;
    ctGradient:
      begin
        pcMain.ActivePage := tabGradient;
      end;
  end;
end;

procedure TfrmEditCondition.FormShow(Sender: TObject);
begin
  cbConditionTypeChange(cbConditionType);
end;

procedure TfrmEditCondition.SetCondType(const Value: TConditionType);
begin
  cbConditionType.ItemIndex := Integer(Value);
  cbConditionTypeChange(nil);
end;

function TfrmEditCondition.GetCondType: TConditionType;
begin
  Result := TConditionType(cbConditionType.ItemIndex);
end;

function TfrmEditCondition.GetCondName: string;
begin
  Result := edtName.Text;
end;

procedure TfrmEditCondition.SetCondName(const Value: string);
begin
  edtName.Text := Value;
end;

procedure TfrmEditCondition.SetEndTime(const Value: TTime);
begin
  dtpTimeEndValid.Time := Value;
end;

procedure TfrmEditCondition.SetGradientValue(const Value: Currency);
begin
  edtGradientValue.ValueFloat := Value;
end;

function TfrmEditCondition.GetEndTime: TTime;
begin
  Result := dtpTimeEndValid.Time;
end;

function TfrmEditCondition.GetGradientValue: Currency;
begin
  Result := edtGradientValue.ValueFloat;
end;

procedure TfrmEditCondition.cbInequalityRtChange(Sender: TObject);
begin
  inherited;
  InequalityRt := GetInequalityRt;
end;

function TfrmEditCondition.GetInequalityGr: TInequalityType;
begin
  if (cbInequalityGr.ItemIndex > -1) then
    Result := TInequalityType(cbInequalityGr.ItemIndex)
  else
    Result := Low(TInequalityType);
end;

function TfrmEditCondition.GetInequalityRt: TInequalityType;
begin
  if (cbInequalityRt.ItemIndex > -1) then
    Result := TInequalityType(cbInequalityRt.ItemIndex)
  else
    Result := Low(TInequalityType);
end;

procedure TfrmEditCondition.SetInequalityGr(const Value: TInequalityType);
begin
  cbInequalityGr.ItemIndex := Ord(Value);
end;

procedure TfrmEditCondition.SetInequalityRt(const Value: TInequalityType);
begin
  cbInequalityRt.ItemIndex := Ord(Value);
  //CheckComparisonResult;
end;

function TfrmEditCondition.GetStartTime: TTime;
begin
  Result := dtpTimeStartValid.Time;
end;

procedure TfrmEditCondition.SetTickType1(const Value: TIABTickType);
begin
  cbTickType1.ItemIndex := Integer(Value);
end;

procedure TfrmEditCondition.SetTickType2(const Value: TIABTickType);
begin
  cbTickType2.ItemIndex := Integer(Value);
end;

function TfrmEditCondition.GetTickType1: TIABTickType;
begin
  if (cbTickType1.ItemIndex > -1) then
    Result := TIABTickType(cbTickType1.ItemIndex)
  else
    Result := ttLast;
end;

function TfrmEditCondition.GetTickType2: TIABTickType;
begin
  if (cbTickType2.ItemIndex > -1) then
    Result := TIABTickType(cbTickType2.ItemIndex)
  else
    Result := ttNotSet;
end;

procedure TfrmEditCondition.SetSokidInfo(aSokidInfo: TSokidInfo);
begin
  Instrument.SokidInfo := aSokidInfo;
  if Instrument.SokidInfo.ContractId > 0 then
    lblInstrumentName.Caption := aSokidInfo.Name
  else
    lblInstrumentName.Caption := '<Replace from AutoOrder>';
end;

procedure TfrmEditCondition.SetStartTime(const Value: TTime);
begin
  dtpTimeStartValid.Time := Value;
end;

procedure TfrmEditCondition.SetValueArrayItem(Index: TConditionType; Value: Double);
begin
  FValueArray[Index] := Value;
  case Index of
    ctGradient:
      begin
        //pnlGradientRight.Caption := FormatFloat('0.00', Value);
      end;
  end;
end;

function TfrmEditCondition.GetValueArrayItem(Index: TConditionType): Double;
begin
  Result := FValueArray[Index];
end;

procedure TfrmEditCondition.SetXmlParams(const Value: string);
var
  XMLFile: TXMLFile;
begin
  XMLFile := TXMLFile.Create;
  try
    XMLFile.XMLText := Value;
  finally
    FreeAndNil(XMLFile);
  end;
end;

function TfrmEditCondition.GetXmlParams: string;
var
  XMLFile: TXMLFile;
begin
  XMLFile := TXMLFile.Create;
  try
    Result := XMLFile.XMLText;
  finally
    FreeAndNil(XMLFile);
  end;
end;

function TfrmEditCondition.GetCondLimit: Currency;
begin
  Result := edtCondLimit.ValueCurrency;
end;

procedure TfrmEditCondition.SetCondLimit(const Value: Currency);
begin
  edtCondLimit.ValueCurrency := Value;
  //CheckComparisonResult;
end;

function TfrmEditCondition.GetMonitoring: Integer;
begin
  Result := StrToIntDef(edtMonitoring.Text, 0);
end;

procedure TfrmEditCondition.SetMonitoring(const Value: Integer);
begin
  edtMonitoring.Text := Value.ToString;
end;

procedure TfrmEditCondition.SetPriority(const Value: TConditionDoc.TPriority);
begin
  cbPriority.ItemIndex := Integer(Value);
end;

function TfrmEditCondition.GetPriority: TConditionDoc.TPriority;
begin
  Result := TConditionDoc.TPriority(cbPriority.ItemIndex);
end;

procedure TfrmEditCondition.aAddFactorExecute(Sender: TObject);
var
  Data: PTreeData;
  Monitor: IMonitor;
  Tree: TVirtualStringTree;
  OrderNode: PVirtualNode;
begin
  inherited;
  if Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    Tree := Monitor.GetMainTree;
    OrderNode := TTreeDocument.GetParentNode(Tree, Self.OwnerNode, TDocType.ntOrder);
    if Assigned(OrderNode) then
    begin
      Data := OrderNode.GetData;
      AddFactor(Self.OwnerNode, Data.OrderDoc.Id);
    end;
  end;
end;

procedure TfrmEditCondition.aAddFactorUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Visible := not FUseInTemplate;
end;

procedure TfrmEditCondition.aCalculateCondExecute(Sender: TObject);
{var
  Value: Currency;}
begin
  inherited;
  (*if (cbTypeOperation.ItemIndex <= 0) and ((Self.TickType2 < ttNotSet) or (Self.TickType2 = ttMotherFilledPrice)) then
    TMessageDialog.ShowWarning(rsTypeOperationNotSelected);
  CalcValueChildNodes;
  {if (FCompiledValue <> 0) then
    Value := FCompiledValue
  else
    Value := FValueArray[ctRealtimeValue];}
  case InequalityRt of
    iqCrosses:
      lblComparisonResult.Caption := FormatFloat('0.00', Abs(FValueArray[ctRealtimeValue] - FCompiledValue) / Max(FValueArray[ctRealtimeValue], FCompiledValue));
    iqCrossesDown:
      if (FCompiledValue <> 0) then
        lblComparisonResult.Caption := FormatFloat('0.00', (FValueArray[ctRealtimeValue] - FCompiledValue) / FCompiledValue);
    iqCrossesUp:
      if (FValueArray[ctRealtimeValue] <> 0) then
        lblComparisonResult.Caption := FormatFloat('0.00', (FCompiledValue - FValueArray[ctRealtimeValue]) / FValueArray[ctRealtimeValue]);
  else
    lblComparisonResult.Caption := '0.00';
  end; *)
end;

procedure TfrmEditCondition.SetTickTypesForChildFactors;
var
  Monitor: IMonitor;
  ConditionDoc: TConditionDoc;
begin
  inherited;
  if Assigned(OwnerNode) then
    if FUseInTemplate and Assigned(FTree) then
    begin
      ConditionDoc := TConditionDoc.Create;
      try
        Self.AssignToDoc(ConditionDoc);
        TTreeDocument.SetTickTypesForChildFactors(FTree, OwnerNode, ConditionDoc);
      finally
        FreeAndNil(ConditionDoc);
        FTree.Invalidate;
      end;
    end
    else if Supports(Application.MainForm, IMonitor, Monitor) then
    begin
      ConditionDoc := TConditionDoc.Create;
      try
        Self.AssignToDoc(ConditionDoc);
        TTreeDocument.SetTickTypesForChildFactors(Monitor.GetMainTree, OwnerNode, ConditionDoc);
      finally
        FreeAndNil(ConditionDoc);
        Monitor.GetMainTree.Invalidate;
      end;
    end;
end;

procedure TfrmEditCondition.aSyncChildFactorsExecute(Sender: TObject);
begin
  inherited;
  SetTickTypesForChildFactors;
end;

function TfrmEditCondition.AddFactor(const aParentNode: PVirtualNode; const aContractId: Integer): PVirtualNode;
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
    SokidInfo := SokidList.GetItem(aContractId);
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

procedure TfrmEditCondition.aGraphExecute(Sender: TObject);
var
  Monitor: IMonitor;
begin
  inherited;
  if Assigned(OwnerNode) and Supports(Application.MainForm, IMonitor, Monitor) then
    Monitor.ShowConditionChart(OwnerNode);
end;

procedure TfrmEditCondition.AssignFromDoc(const aDocument: TConditionDoc);
resourcestring
  rsUpdateCond = 'UPDATE CONDITION SET IS_TEMPLATE=1 WHERE ID=';
begin
  if Assigned(aDocument) then
  begin
    Self.Name              := aDocument.Name;
    Self.CondType          := aDocument.CondType;
    Self.CondLimit         := aDocument.CondLimit;
    Self.EndTime           := aDocument.EndTime;
    Self.InequalityGr      := aDocument.InequalityGr;
    Self.InequalityRt      := aDocument.InequalityRt;
    Self.IsCondition       := aDocument.IsCondition;
    Self.Monitoring        := aDocument.Monitoring;
    Self.OwnerNode         := aDocument.OwnerNode;
    Self.Priority          := aDocument.Priority;
    Self.StartTime         := aDocument.StartTime;
    Self.TickType1         := aDocument.TickType1;
    Self.TickType2         := aDocument.TickType2;
    Self.XmlParams         := aDocument.XmlParams;
    for var TickType := System.Low(TIABTickType) to System.High(TIABTickType) do
      Self.Value[TickType] := aDocument.TickValue[TickType];

    for var CondType := System.Low(TConditionType) to System.High(TConditionType)  do
      Self.ValueArray[CondType] := aDocument.ValueArray[CondType];
    Self.GradientValue     := aDocument.GradientValue;
    SetSokidInfo(aDocument.Instrument.SokidInfo);
  end;
end;

procedure TfrmEditCondition.AssignToDoc(var aDocument: TConditionDoc);
begin
  if Assigned(aDocument) then
  begin
    aDocument.Name              := Self.Name;
    aDocument.CondType          := Self.CondType;
    aDocument.CondLimit         := Self.CondLimit;
    aDocument.EndTime           := Self.EndTime;
    aDocument.InequalityGr      := Self.InequalityGr;
    aDocument.InequalityRt      := Self.InequalityRt;
    aDocument.IsCondition       := Self.IsCondition;
    aDocument.Monitoring        := Self.Monitoring;
    aDocument.Priority          := Self.Priority;
    aDocument.StartTime         := Self.StartTime;
    aDocument.TickType1         := Self.TickType1;
    aDocument.TickType2         := Self.TickType2;
    aDocument.XmlParams         := Self.XmlParams;
    aDocument.Instrument.AssignFrom(Self.Instrument);
    aDocument.GradientValue     := Self.GradientValue;
  end;
end;

end.
