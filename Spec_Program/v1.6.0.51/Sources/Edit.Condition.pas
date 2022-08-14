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
  Publishers, System.Generics.Collections, Frame.Custom, Frame.ConditionHistory, OpenDialog.Conditions, XmlFiles;
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
    btnAddFactor: TBitBtn;
    btnCancel: TBitBtn;
    btnOpenTemplate: TBitBtn;
    btnRefreshPrice: TButton;
    btnSave: TBitBtn;
    btnSaveTemplate: TBitBtn;
    btnSaveTemplateAs: TBitBtn;
    btnShowChart: TBitBtn;
    btnSyncChildFactors: TBitBtn;
    cbActivate: TCheckBox;
    cbBypass: TCheckBox;
    cbConditionType: TComboBox;
    cbDivisionTickType1: TComboBox;
    cbDivisionTickType2: TComboBox;
    cbDivisionTypeOperation: TComboBox;
    cbInequalityCor: TComboBox;
    cbInequalityGr: TComboBox;
    cbInequalityRt: TComboBox;
    cbIsCalculateDivisionValue: TCheckBox;
    cbPriority: TComboBox;
    cbTickType1: TComboBox;
    cbTickType2: TComboBox;
    cbTypeOperation: TComboBox;
    dtpDateEndValid: TDateTimePicker;
    dtpDateStartValid: TDateTimePicker;
    dtpDuration: TDateTimePicker;
    dtpTimeEndValid: TDateTimePicker;
    dtpTimeStartValid: TDateTimePicker;
    edCurrentTemplateName: TEdit;
    edtCalculatedValue: TNumberBox;
    edtCondLimit: TNumberBox;
    edtDescription: TEdit;
    edtGradient: TEdit;
    edtMonitoring: TEdit;
    edtTrailBuy: TEdit;
    edtTrailSell: TEdit;
    edtUpProc: TEdit;
    edtWidth: TEdit;
    frameConditionHistory: TframeConditionHistory;
    imgWarning: TVirtualImage;
    lblBreakCaption: TLabel;
    lblCalculatedValue: TLabel;
    lblComparisonCor: TLabel;
    lblComparisonGr: TLabel;
    lblComparisonRt: TLabel;
    lblCompiledValue: TLabel;
    lblCompiledValueCaption: TLabel;
    lblCompiledValueSmall: TLabel;
    lblConditionLimit: TLabel;
    lblCorridorCaption: TLabel;
    lblCorridorPositionCaption: TLabel;
    lblCorridorRight: TLabel;
    lblDescription: TLabel;
    lblDivisionCalculateValue: TLabel;
    lblDivisionCalculateValueSmall: TLabel;
    lblDivisionTickTypeCaption1: TLabel;
    lblDivisionTickTypeCaption2: TLabel;
    lblDivisionValue: TLabel;
    lblDivisionValue1: TLabel;
    lblDivisionValue2: TLabel;
    lblDivisionValueCaption: TLabel;
    lblDivisionValueSmall: TLabel;
    lblDuration: TLabel;
    lblGradientCaption: TLabel;
    lblGradientRight: TLabel;
    lblInfo: TLabel;
    lblInitTime: TLabel;
    lblMonitoringCaption: TLabel;
    lblMonitoringSec: TLabel;
    lblPriorityCaption: TLabel;
    lblTemplate: TLabel;
    lblTickTypeCaption1: TLabel;
    lblTickTypeCaption2: TLabel;
    lblTrailBuyEntry: TLabel;
    lblTrailBuyEntryCaption: TLabel;
    lblTrailBuyFrom: TLabel;
    lblTrailBuyIf: TLabel;
    lblTrailBuyMax: TLabel;
    lblTrailBuyMaxCaption: TLabel;
    lblTrailBuyMin: TLabel;
    lblTrailBuyMinCaption: TLabel;
    lblTrailSellEntry: TLabel;
    lblTrailSellEntryCaption: TLabel;
    lblTrailSellFrom: TLabel;
    lblTrailSellIf: TLabel;
    lblTrailSellMax: TLabel;
    lblTrailSellMaxCaption: TLabel;
    lblTrailSellMin: TLabel;
    lblTrailSellMinCaption: TLabel;
    lblTypeCaption: TLabel;
    lblValidDates: TLabel;
    lblValidDatesTo: TLabel;
    lblValidTime: TLabel;
    lblValidTimeTo: TLabel;
    lblValue1: TLabel;
    lblValue2: TLabel;
    pcMain: TPageControl;
    pnlBottom: TPanel;
    pnlBreak: TPanel;
    pnlCorridor: TPanel;
    pnlCorridorPosition: TPanel;
    pnlCorridorPositionRight: TPanel;
    pnlCorridorRight: TPanel;
    pnlDivisionValue: TPanel;
    pnlDuration: TPanel;
    pnlGradient: TPanel;
    pnlGradientRight: TPanel;
    pnlInfo: TPanel;
    pnlMain: TPanel;
    pnlOptions: TPanel;
    pnlRealTimeValue: TPanel;
    pnlTimeGap: TPanel;
    pnlTop: TPanel;
    pnlTrailBuy: TPanel;
    pnlTrailSell: TPanel;
    rbBreakDown: TRadioButton;
    rbBreakUp: TRadioButton;
    tsCondition: TTabSheet;
    tsHistory: TTabSheet;
    lblComparisonResult: TLabel;
    procedure aAddFactorExecute(Sender: TObject);
    procedure aAddFactorUpdate(Sender: TObject);
    procedure aCalculateCondExecute(Sender: TObject);
    procedure aGraphExecute(Sender: TObject);
    procedure aOpenTemplateExecute(Sender: TObject);
    procedure aRefreshPriceExecute(Sender: TObject);
    procedure aRefreshPriceUpdate(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveTemplateAsExecute(Sender: TObject);
    procedure aSaveTemplateExecute(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
    procedure aSyncChildFactorsExecute(Sender: TObject);
    procedure cbConditionTypeChange(Sender: TObject);
    procedure cbIsCalculateDivisionValueClick(Sender: TObject);
    procedure edtTrailSellExit(Sender: TObject);
    procedure edtTrailSellKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnTickTypeChange(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure cbInequalityRtChange(Sender: TObject);
  private const
    C_IDENTITY_NAME = 'EditCondition';
  private
    FCalcValue: Currency;
    FCompiledValue: Currency;
    FDivisionValue: Currency;
    FIsCondition: Boolean;
    FMinMaxValueArray: TConditionDoc.TMinMaxValueArray;
    FModyfied: Boolean;
    FParametersStore: TParametersStore;
    FTemplateId: Integer;
    FTree: TVirtualStringTree;
    FUseInTemplate: Boolean;
    FValueArray: TConditionDoc.TValueArray;
    function AddFactor(const aParentNode: PVirtualNode; const aContractId: Integer): PVirtualNode;
    function GetActive: Boolean;
    function GetBypass: Boolean;
    function GetCondLimit: Currency;
    function GetCondType: TConditionType;
    function GetCondWidth: Currency;
    function GetDescription: string;
    function GetDivisionTickType1: TIABTickType;
    function GetDivisionTickType2: TIABTickType;
    function GetDivisionTypeOperation: TTypeOperation;
    function GetDuration: TTime;
    function GetEndDate: TDate;
    function GetEndTime: TTime;
    function GetGradient: Currency;
    function GetInequalityCor: TInequalityType;
    function GetInequalityGr: TInequalityType;
    function GetInequalityRt: TInequalityType;
    function GetInitTime: TTime;
    function GetIsBreakUp: Boolean;
    function GetIsCalculateDivisionValue: Boolean;
    function GetMinMaxValueArrayItem(Index: TConditionDoc.TMinMaxValue): Double;
    function GetMonitoring: Integer;
    function GetPriority: TConditionDoc.TPriority;
    function GetStartDate: TDate;
    function GetStartTime: TTime;
    function GetTickType1: TIABTickType;
    function GetTickType2: TIABTickType;
    function GetTrailBuy: Double;
    function GetTrailSell: Double;
    function GetTypeOperation: TTypeOperation;
    function GetUpProc: Integer;
    function GetValueArrayItem(Index: TConditionType): Double;
    function GetXmlParams: string;
    procedure SetActive(const Value: Boolean);
    procedure SetBypass(const Value: Boolean);
    procedure SetCalcValue(const Value: Currency);
    procedure SetCondLimit(const Value: Currency);
    procedure SetCondType(const Value: TConditionType);
    procedure SetCondWidth(const Value: Currency);
    procedure SetDescription(const Value: string);
    procedure SetDivisionTickType1(const Value: TIABTickType);
    procedure SetDivisionTickType2(const Value: TIABTickType);
    procedure SetDivisionTypeOperation(const Value: TTypeOperation);
    procedure SetDivisionValue(const Value: Currency);
    procedure SetDuration(const Value: TTime);
    procedure SetEndDate(const Value: TDate);
    procedure SetEndTime(const Value: TTime);
    procedure SetGradient(const Value: Currency);
    procedure SetInequalityCor(const Value: TInequalityType);
    procedure SetInequalityGr(const Value: TInequalityType);
    procedure SetInequalityRt(const Value: TInequalityType);
    procedure SetInitTime(const Value: TTime);
    procedure SetIsBreakUp(const Value: Boolean);
    procedure SetIsCalculateDivisionValue(const Value: Boolean);
    procedure SetMinMaxValueArrayItem(Index: TConditionDoc.TMinMaxValue; const Value: Double);
    procedure SetMonitoring(const Value: Integer);
    procedure SetPriority(const Value: TConditionDoc.TPriority);
    procedure SetStartDate(const Value: TDate);
    procedure SetStartTime(const Value: TTime);
    procedure SetTickType1(const Value: TIABTickType);
    procedure SetTickType2(const Value: TIABTickType);
    procedure SetTickTypesForChildFactors;
    procedure SetTrailBuy(const Value: Double);
    procedure SetTrailSell(const Value: Double);
    procedure SetTypeOperation(const Value: TTypeOperation);
    procedure SetUpProc(const Value: Integer);
    procedure SetValueArrayItem(Index: TConditionType; Value: Double);
    procedure SetXmlParams(const Value: string);

    procedure CalcDivisionValue;
    procedure CalcValueChildNodes;
    procedure CheckComparisonResult;

    property Modyfied                 : Boolean        read FModyfied                   write FModyfied;
    property DivisionTickType1        : TIABTickType   read GetDivisionTickType1        write SetDivisionTickType1;
    property DivisionTickType2        : TIABTickType   read GetDivisionTickType2        write SetDivisionTickType2;
    property DivisionTypeOperation    : TTypeOperation read GetDivisionTypeOperation    write SetDivisionTypeOperation;
    property IsCalculateDivisionValue : Boolean        read GetIsCalculateDivisionValue write SetIsCalculateDivisionValue;
  public
    procedure Initialize(aUseInTemplate: Boolean);
    procedure Deinitialize;
    procedure AssignFromDoc(const aDocument: TConditionDoc);
    procedure AssignToDoc(var aDocument: TConditionDoc);
    class function ShowDocument(aDocument: TConditionDoc): TModalResult; overload;
    class function ShowDocument(aDocument: TConditionDoc; const aTree: TVirtualStringTree): TModalResult; overload;

    property Active            : Boolean                 read GetActive            write SetActive;
    property Bypass            : Boolean                 read GetBypass            write SetBypass;
    property CalcValue         : Currency                read FCalcValue           write SetCalcValue;
    property CondLimit         : Currency                read GetCondLimit         write SetCondLimit;
    property DivisionValue     : Currency                read FDivisionValue       write SetDivisionValue;
    property CondType          : TConditionType          read GetCondType          write SetCondType;
    property CondWidth         : Currency                read GetCondWidth         write SetCondWidth;
    property Description       : string                  read GetDescription       write SetDescription;
    property Duration          : TTime                   read GetDuration          write SetDuration;
    property EndDate           : TDate                   read GetEndDate           write SetEndDate;
    property EndTime           : TTime                   read GetEndTime           write SetEndTime;
    property Gradient          : Currency                read GetGradient          write SetGradient;
    property InequalityCor     : TInequalityType         read GetInequalityCor     write SetInequalityCor;
    property InequalityGr      : TInequalityType         read GetInequalityGr      write SetInequalityGr;
    property InequalityRt      : TInequalityType         read GetInequalityRt      write SetInequalityRt;
    property InitTime          : TTime                   read GetInitTime          write SetInitTime;
    property IsBreakUp         : Boolean                 read GetIsBreakUp         write SetIsBreakUp;
    property IsCondition       : Boolean                 read FIsCondition         write FIsCondition;
    property Monitoring        : Integer                 read GetMonitoring        write SetMonitoring;
    property Priority          : TConditionDoc.TPriority read GetPriority          write SetPriority;
    property StartDate         : TDate                   read GetStartDate         write SetStartDate;
    property StartTime         : TTime                   read GetStartTime         write SetStartTime;
    property TickType1         : TIABTickType            read GetTickType1         write SetTickType1;
    property TickType2         : TIABTickType            read GetTickType2         write SetTickType2;
    property TrailBuy          : Double                  read GetTrailBuy          write SetTrailBuy;
    property TrailSell         : Double                  read GetTrailSell         write SetTrailSell;
    property TypeOperation     : TTypeOperation          read GetTypeOperation     write SetTypeOperation;
    property UpProc            : Integer                 read GetUpProc            write SetUpProc;
    property XmlParams         : string                  read GetXmlParams         write SetXmlParams;
    property ValueArray[Index: TConditionType]: Double   read GetValueArrayItem    write SetValueArrayItem;
    property MinMaxValueArray[Index: TConditionDoc.TMinMaxValue]: Double read GetMinMaxValueArrayItem  write SetMinMaxValueArrayItem;
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
      frmEditCondition.CalcValueChildNodes;
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
  for CondType := System.Low(TConditionType) to System.High(TConditionType) do
    cbConditionType.Items.Add(CondType.ToString);

  cbTickType1.Items.Clear;
  cbTickType2.Items.Clear;
  cbDivisionTickType1.Items.Clear;
  cbDivisionTickType2.Items.Clear;
  for var TickType := ttBidSize to ttMotherFilledPrice do
  begin
    cbTickType1.Items.Add(TickType.ToString);
    cbTickType2.Items.Add(TickType.ToString);
    cbDivisionTickType1.Items.Add(TickType.ToString);
    cbDivisionTickType2.Items.Add(TickType.ToString);
  end;

  cbTypeOperation.Items.Clear;
  cbDivisionTypeOperation.Items.Clear;
  for var TypeOperation := Low(TTypeOperation) to toMult do
  begin
    cbTypeOperation.Items.Add(TypeOperation.ToString);
    cbDivisionTypeOperation.Items.Add(TypeOperation.ToString);
  end;

  for CondType := System.Low(FValueArray) to System.High(FValueArray) do
    FValueArray[CondType] := 0;
  Active         := True;
  FTemplateId    := -1;
  Modyfied       := False;
  FUseInTemplate := False;

  dtpDateStartValid.Date := Today;
  dtpDateEndValid.Date   := Today;
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
  frameConditionHistory.Initialize;
  FUseInTemplate := aUseInTemplate;
  edtCondLimit.CurrencyString   := '';
  lblCompiledValue.Visible      := not FUseInTemplate;
  lblCompiledValueSmall.Visible := not FUseInTemplate;
  lblValue1.Visible             := not FUseInTemplate;
  lblValue2.Visible             := not FUseInTemplate;
  lblInfo.Caption               := rsChangingDocument;
  pnlInfo.Visible               := FUseInTemplate;
  // FParametersStore.Restore;

  cbInequalityRt.Items.Clear;
  cbInequalityGr.Items.Clear;
  cbInequalityCor.Items.Clear;

  for var it := Low(TInequalityType) to High(TInequalityType) do
    if not(it in [iqBetween]) then
    begin
      cbInequalityRt.Items.Add(it.ToFullName);
      cbInequalityGr.Items.Add(it.ToFullName);
      cbInequalityCor.Items.Add(it.ToFullName);
    end;
  cbInequalityRt.ItemIndex  := Ord(Low(TInequalityType));
  cbInequalityGr.ItemIndex  := Ord(Low(TInequalityType));
  cbInequalityCor.ItemIndex := Ord(Low(TInequalityType));
end;

procedure TfrmEditCondition.Deinitialize;
begin
  frameConditionHistory.Deinitialize;
  frmEditCondition.FParametersStore.Store;
end;

procedure TfrmEditCondition.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FParametersStore);
end;

procedure TfrmEditCondition.aSaveExecute(Sender: TObject);
begin
  inherited;
  if (cbTypeOperation.ItemIndex <= 0) and ((Self.TickType2 < ttNotSet) or (Self.TickType2 = ttMotherFilledPrice)) then
  begin
    TMessageDialog.ShowWarning(rsTypeOperationNotSelected);
    ModalResult := mrNone;
  end
  else
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

procedure TfrmEditCondition.CalcDivisionValue;
var
  Monitor: IMonitor;
  ConditionDoc: TConditionDoc;
  CompiledValue: TCompiledValue;
begin
  DivisionValue := 1;
  if IsCalculateDivisionValue then
    if Assigned(OwnerNode) and Supports(Application.MainForm, IMonitor, Monitor) then
    begin
      CompiledValue := Default(TCompiledValue);
      ConditionDoc := TConditionDoc.Create;
      try
        Self.AssignToDoc(ConditionDoc);
        ConditionDoc.OwnerNode     := Self.OwnerNode;
        ConditionDoc.TickType1     := Self.DivisionTickType1;
        ConditionDoc.TickType2     := Self.DivisionTickType2;
        ConditionDoc.TypeOperation := Self.DivisionTypeOperation;
        CompiledValue := TTreeDocument.CalcCompiledValue(Monitor.GetMainTree, ConditionDoc);
      finally
        FreeAndNil(ConditionDoc);
      end;

      DivisionValue := CompiledValue.TotalValue;
      lblDivisionValue1.Caption := FormatFloat('0.00', CompiledValue.SummValue1);
      lblDivisionValue2.Caption := FormatFloat('0.00', CompiledValue.SummValue2);
      lblDivisionCalculateValue.Caption := '=' + FormatFloat('0.00', DivisionValue);
      lblDivisionCalculateValueSmall.Caption := SimpleRoundTo(DivisionValue, -C_DECIMALS).ToString;
    end;
end;

procedure TfrmEditCondition.CalcValueChildNodes;
var
  Monitor: IMonitor;
  ConditionDoc: TConditionDoc;
  CompiledValue: TCompiledValue;
begin
  inherited;
  CalcDivisionValue;
  if Assigned(OwnerNode) and Supports(Application.MainForm, IMonitor, Monitor) then
  begin
    CompiledValue := Default(TCompiledValue);
    ConditionDoc := TConditionDoc.Create;
    try
      Self.AssignToDoc(ConditionDoc);
      ConditionDoc.OwnerNode := Self.OwnerNode;
      CompiledValue := TTreeDocument.CalcCompiledValue(Monitor.GetMainTree, ConditionDoc);
    finally
      FreeAndNil(ConditionDoc);
    end;

    FCompiledValue := CompiledValue.TotalValue;
    lblValue1.Caption := FormatFloat('0.00', CompiledValue.SummValue1);
    lblValue2.Caption := FormatFloat('0.00', CompiledValue.SummValue2);
    lblCompiledValue.Caption      := '=' + FormatFloat('0.00', FCompiledValue);
    lblCompiledValueSmall.Caption := SimpleRoundTo(FCompiledValue, -C_DECIMALS).ToString;
    if (DivisionValue <> 0) then
      CalcValue :=  FCompiledValue / DivisionValue
    else
      CalcValue := 0;
  end;
end;

procedure TfrmEditCondition.OnTickTypeChange(Sender: TObject);
begin
  inherited;
  CalcValueChildNodes;
end;

procedure TfrmEditCondition.pcMainChange(Sender: TObject);
resourcestring
  rsHistory = 'TimeStamp=%s, CondType=%s, CompiledValue=%s [%s] CondValue=%s, TickType1=%s, TickType2=%s, IsCondition=%s, TypeOperation=%s';
begin
  inherited;
  if (pcMain.ActivePage = tsHistory) then
  begin
    frameConditionHistory.OwnerNode := Self.OwnerNode;
    frameConditionHistory.RefreshHistory;
  end;
end;

procedure TfrmEditCondition.aRefreshPriceExecute(Sender: TObject);
begin
  inherited;
  if (cbTypeOperation.ItemIndex <= 0) and ((Self.TickType2 < ttNotSet) or (Self.TickType2 = ttMotherFilledPrice)) then
    TMessageDialog.ShowWarning(rsTypeOperationNotSelected)
  else
    CalcValueChildNodes;
end;

procedure TfrmEditCondition.aRefreshPriceUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Visible := not FUseInTemplate;
end;

procedure TfrmEditCondition.cbConditionTypeChange(Sender: TObject);
begin
  pnlBreak.Visible            := False;
  pnlCorridorPosition.Visible := False;
  pnlCorridor.Visible         := False;
  pnlGradient.Visible         := False;
  pnlRealTimeValue.Visible    := False;
  pnlTimeGap.Visible          := False;
  pnlTrailBuy.Visible         := False;
  pnlTrailSell.Visible        := False;
  pnlDuration.Visible         := False;

  case TConditionType(cbConditionType.ItemIndex) of
    ctRealtimeValue:
      begin
        pnlRealTimeValue.Visible := True;
        pnlDuration.Visible      := True;
        pnlDuration.Top          := 1000;
      end;
    ctRealtimeAndTimeGap:
      begin
        pnlTimeGap.Visible       := True;
        pnlRealTimeValue.Visible := True;
        pnlDuration.Visible      := True;
        pnlDuration.Top          := 1000;
      end;
    ctTimeGap:
      begin
        pnlTimeGap.Visible := True;
      end;
    ctGradient:
      begin
        pnlGradient.Visible := True;
      end;
    ctCorridor:
      begin
        pnlCorridor.Visible := True;
      end;
    ctGradientAndCorridor:
      begin
        pnlGradient.Visible := True;
        pnlCorridor.Visible := True;
      end;
    ctCorridorPosition:
      begin
        pnlGradient.Visible := True;
        pnlCorridor.Visible := True;
        pnlCorridorPosition.Visible := True;
      end;
    ctTrailBuy:
      begin
        pnlTrailBuy.Visible := True;
      end;
    ctTrailSell:
      begin
        pnlTrailSell.Visible := True;
      end;
  end;
end;

procedure TfrmEditCondition.edtTrailSellKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if not(CharInSet(Key, ['0' .. '9', '-', #08, FormatSettings.DecimalSeparator])) then
    Key := #0
end;

procedure TfrmEditCondition.edtTrailSellExit(Sender: TObject);
var
  Value: Double;
begin
  inherited;
  Value := StrToFloatDef(edtTrailSell.Text, MaxDouble);
  if (Value = MaxDouble) then
    Value := 0;
  if (Value > 0) then
    edtTrailSell.Text := (-Abs(Value)).ToString
  else
    edtTrailSell.Text := Value.ToString;
end;

procedure TfrmEditCondition.FormShow(Sender: TObject);
begin
  cbConditionTypeChange(cbConditionType);
end;

procedure TfrmEditCondition.SetActive(const Value: Boolean);
begin
  cbActivate.Checked := Value;
end;

procedure TfrmEditCondition.SetBypass(const Value: Boolean);
begin
  cbBypass.Checked := Value;
end;

function TfrmEditCondition.GetActive: Boolean;
begin
  Result := cbActivate.Checked;
end;

function TfrmEditCondition.GetBypass: Boolean;
begin
  Result := cbBypass.Checked;
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

function TfrmEditCondition.GetDescription: string;
begin
  Result := edtDescription.Text;
end;

function TfrmEditCondition.GetDuration: TTime;
begin
  Result := dtpDuration.Time;
end;

procedure TfrmEditCondition.SetDuration(const Value: TTime);
begin
  dtpDuration.Time := Value;
end;

procedure TfrmEditCondition.SetDescription(const Value: string);
begin
  edtDescription.Text := Value;
end;

function TfrmEditCondition.GetDivisionTickType1: TIABTickType;
begin
  if (cbDivisionTickType1.ItemIndex > -1) then
    Result := TIABTickType(cbDivisionTickType1.ItemIndex)
  else
    Result := ttLast;
end;

function TfrmEditCondition.GetDivisionTickType2: TIABTickType;
begin
  if (cbDivisionTickType2.ItemIndex > -1) then
    Result := TIABTickType(cbDivisionTickType2.ItemIndex)
  else
    Result := ttNotSet;
end;

function TfrmEditCondition.GetDivisionTypeOperation: TTypeOperation;
begin
  if (cbDivisionTypeOperation.ItemIndex > -1) then
    Result := TTypeOperation(cbDivisionTypeOperation.ItemIndex)
  else
    Result := toNone;
end;

procedure TfrmEditCondition.SetDivisionTickType1(const Value: TIABTickType);
begin
  cbDivisionTickType1.ItemIndex := Integer(Value);
end;

procedure TfrmEditCondition.SetDivisionTickType2(const Value: TIABTickType);
begin
  cbDivisionTickType2.ItemIndex := Integer(Value);
end;

procedure TfrmEditCondition.SetDivisionTypeOperation(const Value: TTypeOperation);
begin
  cbDivisionTypeOperation.ItemIndex := Integer(Value);
end;

procedure TfrmEditCondition.SetDivisionValue(const Value: Currency);
begin
  if (Value = 0) then
    FDivisionValue := 1
  else
    FDivisionValue := Value;
  lblDivisionValue.Caption      := FormatFloat('0.00', DivisionValue);
  lblDivisionValueSmall.Caption := SimpleRoundTo(FDivisionValue, -C_DECIMALS).ToString;
end;

procedure TfrmEditCondition.SetEndDate(const Value: TDate);
begin
  dtpDateEndValid.Date := Value;
end;

function TfrmEditCondition.GetEndDate: TDate;
begin
  Result := dtpDateEndValid.Date;
end;

procedure TfrmEditCondition.SetEndTime(const Value: TTime);
begin
  dtpTimeEndValid.Time := Trunc(EndDate) + Value;
end;

function TfrmEditCondition.GetEndTime: TTime;
begin
  Result := dtpTimeEndValid.Time;
end;

procedure TfrmEditCondition.SetGradient(const Value: Currency);
begin
  edtGradient.Text := FormatFloat('0.######', Value);
end;

function TfrmEditCondition.GetGradient: Currency;
begin
  Result := StrToFloatDef(edtGradient.Text, 0);
end;

procedure TfrmEditCondition.SetIsBreakUp(const Value: Boolean);
begin
  rbBreakUp.Checked := Value;
  rbBreakDown.Checked := not Value;
end;

procedure TfrmEditCondition.SetIsCalculateDivisionValue(const Value: Boolean);
begin
  cbIsCalculateDivisionValue.Checked := Value;
end;

function TfrmEditCondition.GetIsCalculateDivisionValue: Boolean;
begin
  Result := cbIsCalculateDivisionValue.Checked;
end;

procedure TfrmEditCondition.cbInequalityRtChange(Sender: TObject);
begin
  inherited;
  InequalityRt := GetInequalityRt;
end;

procedure TfrmEditCondition.cbIsCalculateDivisionValueClick(Sender: TObject);
begin
  inherited;
  pnlDivisionValue.Visible := IsCalculateDivisionValue;
end;

procedure TfrmEditCondition.CheckComparisonResult;
var
  CompareResult: Boolean;
begin
  CompareResult := InequalityRt.IsCondition(CalcValue, CondLimit);
  if CompareResult then
    lblComparisonResult.Font.Color := clGreen
  else
    lblComparisonResult.Font.Color := clRed;
  lblComparisonResult.Caption := CurrToStr(CalcValue) + InequalityRt.ToString + CurrToStr(CondLimit);
end;

function TfrmEditCondition.GetIsBreakUp: Boolean;
begin
  Result := rbBreakUp.Checked;
end;

function TfrmEditCondition.GetInequalityCor: TInequalityType;
begin
  if (cbInequalityCor.ItemIndex > -1) then
    Result := TInequalityType(cbInequalityCor.ItemIndex)
  else
    Result := Low(TInequalityType);
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

function TfrmEditCondition.GetInitTime: TTime;
begin
  Result := 0;
end;

procedure TfrmEditCondition.SetInitTime(const Value: TTime);
begin
  lblInitTime.Caption := 'Init Time: ' + FormatDateTime('hh:nn:ss', Value);
end;

procedure TfrmEditCondition.SetInequalityCor(const Value: TInequalityType);
begin
  cbInequalityCor.ItemIndex := Ord(Value);
end;

procedure TfrmEditCondition.SetInequalityGr(const Value: TInequalityType);
begin
  cbInequalityGr.ItemIndex := Ord(Value);
end;

procedure TfrmEditCondition.SetInequalityRt(const Value: TInequalityType);
begin
  cbInequalityRt.ItemIndex := Ord(Value);
  CheckComparisonResult;
end;

procedure TfrmEditCondition.SetStartDate(const Value: TDate);
begin
  dtpDateStartValid.Date := Value;
end;

function TfrmEditCondition.GetStartDate: TDate;
begin
  Result := dtpDateStartValid.Date;
end;

function TfrmEditCondition.GetStartTime: TTime;
begin
  Result := dtpTimeStartValid.Time;
end;

function TfrmEditCondition.GetTrailBuy: Double;
begin
  Result := StrToFloatDef(edtTrailBuy.Text, 0);
  if (Result < 0) then
    Result := 0;
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

procedure TfrmEditCondition.SetTrailBuy(const Value: Double);
begin
  edtTrailBuy.Text := SimpleRoundTo(Value, -C_DECIMALS).ToString;
end;

function TfrmEditCondition.GetTrailSell: Double;
begin
  Result := StrToFloatDef(edtTrailSell.Text, 0);
  if (Result > 0) then
    Result := 0;
end;

function TfrmEditCondition.GetTypeOperation: TTypeOperation;
begin
  if (cbTypeOperation.ItemIndex > -1) then
    Result := TTypeOperation(cbTypeOperation.ItemIndex)
  else
    Result := toNone;
end;

procedure TfrmEditCondition.SetTypeOperation(const Value: TTypeOperation);
begin
  cbTypeOperation.ItemIndex := Integer(Value);
end;

procedure TfrmEditCondition.SetTrailSell(const Value: Double);
begin
  edtTrailSell.Text := SimpleRoundTo(Value, -C_DECIMALS).ToString;
end;

procedure TfrmEditCondition.SetStartTime(const Value: TTime);
begin
  dtpTimeStartValid.Time := Trunc(StartDate) + Value;
end;

procedure TfrmEditCondition.SetUpProc(const Value: Integer);
begin
  edtUpProc.Text := Value.ToString;
end;

procedure TfrmEditCondition.SetValueArrayItem(Index: TConditionType; Value: Double);
begin
  FValueArray[Index] := Value;
  case Index of
    ctGradient:
      begin
        pnlGradientRight.Caption := FormatFloat('0.00', Value);
      end;
    ctRealtimeValue:
      begin
//        if IsCondition then
//        begin
//          lblActualValue.Font.Color := clGreen;
//          lblActualValueSmall.Font.Color := clGreen;
//        end
//        else
//        begin
//          lblActualValue.Font.Color := clRed;
//          lblActualValueSmall.Font.Color := clRed;
//        end;

//        lblActualValue.Caption := FormatFloat('0.00', Value);
//        lblActualValueSmall.Caption := SimpleRoundTo(Value, -C_DECIMALS).ToString;
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, 'SetValue', 'Condition Value=' + FloatToStr(Self.CondLimit) + ', Compiled Value=' + FormatFloat('0.00', Value));
      end;
    ctCorridor:
      begin
        pnlCorridorRight.Caption := FormatFloat(C_CURRENCY_FORMAT, Value);
      end;
    ctCorridorPosition:
      begin
        pnlCorridorPositionRight.Caption := FormatFloat('0%', Value);
      end;
    ctTimeGap:
      begin
      end;
    ctTrailBuy:
      begin
      end;
    ctTrailSell:
      begin
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
    IsCalculateDivisionValue := XMLFile.ReadBool(TConditionDoc.C_SECTION_CONDITION,'IsCalculateDivisionValue', False);
    DivisionTickType1        := TIABTickType(XMLFile.ReadInteger(TConditionDoc.C_SECTION_CONDITION, 'DivisionTickType1', Ord(TIABTickType.ttLast)));
    DivisionTickType2        := TIABTickType(XMLFile.ReadInteger(TConditionDoc.C_SECTION_CONDITION, 'DivisionTickType2', Ord(TIABTickType.ttNotSet)));
    DivisionTypeOperation    := TTypeOperation(XMLFile.ReadInteger(TConditionDoc.C_SECTION_CONDITION, 'DivisionTypeOperation', Ord(TTypeOperation.toNone)));
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
    XMLFile.WriteBool(TConditionDoc.C_SECTION_CONDITION, 'IsCalculateDivisionValue', IsCalculateDivisionValue);
    XMLFile.WriteInteger(TConditionDoc.C_SECTION_CONDITION, 'DivisionTickType1', Ord(DivisionTickType1));
    XMLFile.WriteInteger(TConditionDoc.C_SECTION_CONDITION, 'DivisionTickType2', Ord(DivisionTickType2));
    XMLFile.WriteInteger(TConditionDoc.C_SECTION_CONDITION, 'DivisionTypeOperation', Ord(DivisionTypeOperation));
    Result := XMLFile.XMLText;
  finally
    FreeAndNil(XMLFile);
  end;
end;

function TfrmEditCondition.GetUpProc: Integer;
begin
  Result := StrToIntDef(edtUpProc.Text, 0);
end;

function TfrmEditCondition.GetCondLimit: Currency;
begin
  Result := edtCondLimit.ValueCurrency;
end;

procedure TfrmEditCondition.SetCalcValue(const Value: Currency);
begin
  FCalcValue := Value;
  edtCalculatedValue.ValueCurrency := Value;
  CheckComparisonResult;
end;

procedure TfrmEditCondition.SetCondLimit(const Value: Currency);
begin
  edtCondLimit.ValueCurrency := Value;
  CheckComparisonResult;
end;

procedure TfrmEditCondition.SetCondWidth(const Value: Currency);
begin
  edtWidth.Text := FormatFloat('0.######', Value);
end;

function TfrmEditCondition.GetCondWidth: Currency;
begin
  Result := StrToFloatDef(edtWidth.Text, 0);
end;

function TfrmEditCondition.GetMinMaxValueArrayItem(Index: TConditionDoc.TMinMaxValue): Double;
begin
  Result := FMinMaxValueArray[Index];
end;

procedure TfrmEditCondition.SetMinMaxValueArrayItem(Index: TConditionDoc.TMinMaxValue; const Value: Double);
begin
  FMinMaxValueArray[Index] := Value;
  case Index of
    mmMinPayed:
      begin
        lblTrailBuyMin.Caption  := FormatFloat(C_CURRENCY_FORMAT, Value);
        lblTrailSellMin.Caption := FormatFloat(C_CURRENCY_FORMAT, Value);
      end;
    mmMaxPayed:
      begin
        lblTrailBuyMax.Caption  := FormatFloat(C_CURRENCY_FORMAT, Value);
        lblTrailSellMax.Caption := FormatFloat(C_CURRENCY_FORMAT, Value);
      end;
    mmLastPayed:
      begin

      end;
    mmEntryPrice:
      begin
        lblTrailBuyEntry.Caption  := FormatFloat(C_CURRENCY_FORMAT, Value);
        lblTrailSellEntry.Caption := FormatFloat(C_CURRENCY_FORMAT, Value);
      end;
  end;
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
  if (cbTypeOperation.ItemIndex <= 0) and ((Self.TickType2 < ttNotSet) or (Self.TickType2 = ttMotherFilledPrice)) then
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
  end;
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

resourcestring
  rsUpdateCond = 'UPDATE CONDITION SET IS_TEMPLATE=1 WHERE ID=';

procedure TfrmEditCondition.aSaveTemplateExecute(Sender: TObject);
var
  Cond: TConditionDoc;
begin
  inherited;
  Cond := TConditionDoc.Create;
  try
    Self.AssignToDoc(Cond);
    if (FTemplateId > -1) then
      Cond.RecordId := FTemplateId;
    if string(edCurrentTemplateName.Text).Trim.IsEmpty then
      edCurrentTemplateName.Text := Cond.Description;
    Cond.Description := edCurrentTemplateName.Text;
    Cond.SaveToDB;
    DMod.ExecuteSQL(rsUpdateCond + Cond.RecordId.ToString);
    FTemplateId := Cond.RecordId;
  finally
    FreeAndNil(Cond);
  end;
end;

procedure TfrmEditCondition.aSaveTemplateAsExecute(Sender: TObject);
var
  Cond: TConditionDoc;
begin
  inherited;
  Cond := TConditionDoc.Create;
  try
    Self.AssignToDoc(Cond);
    Cond.RecordId := -1;
    if string(edCurrentTemplateName.Text).Trim.IsEmpty then
      edCurrentTemplateName.Text := Cond.Description
    else
      edCurrentTemplateName.Text := 'Copy ' + edCurrentTemplateName.Text;
    Cond.Description := edCurrentTemplateName.Text;
    Cond.SaveToDB;
    DMod.ExecuteSQL(rsUpdateCond + Cond.RecordId.ToString);
    FTemplateId := Cond.RecordId;
  finally
    FreeAndNil(Cond);
  end;
end;

procedure TfrmEditCondition.aOpenTemplateExecute(Sender: TObject);
var
  RecordId: Integer;
  Cond: TConditionDoc;
  Node: PVirtualNode;
begin
  inherited;
  RecordId := TfrmOpenCondition.GetId;
  if (RecordId > 0) then
  begin
    Node := Self.OwnerNode;
    FTemplateId := RecordId;
    Cond := TConditionDoc.Create;
    try
      Cond.FromDB(RecordId);
      Self.AssignFromDoc(Cond);
      Self.OwnerNode := Node;
      edCurrentTemplateName.Text := Cond.Description;
    finally
      FreeAndNil(Cond);
    end;
  end;
end;

procedure TfrmEditCondition.AssignFromDoc(const aDocument: TConditionDoc);
begin
  if Assigned(aDocument) then
  begin
    Self.Active            := aDocument.Active;
    Self.CalcValue         := aDocument.CalcValue;
    Self.Bypass            := aDocument.Bypass;
    Self.CondType          := aDocument.CondType;
    Self.CondLimit         := aDocument.CondLimit;
    Self.CondWidth         := aDocument.CondWidth;
    Self.Description       := aDocument.Description;
    Self.DivisionValue     := aDocument.DivisionValue;
    Self.Duration          := aDocument.Duration;
    Self.EndDate           := aDocument.EndDate;
    Self.EndTime           := aDocument.EndTime;
    Self.Gradient          := aDocument.Gradient;
    Self.InequalityCor     := aDocument.InequalityCor;
    Self.InequalityGr      := aDocument.InequalityGr;
    Self.InequalityRt      := aDocument.InequalityRt;
    Self.InitTime          := aDocument.InitTime;
    Self.IsBreakUp         := aDocument.IsBreakUp;
    Self.IsCondition       := aDocument.IsCondition;
    Self.Monitoring        := aDocument.Monitoring;
    Self.OwnerNode         := aDocument.OwnerNode;
    Self.Priority          := aDocument.Priority;
    Self.StartDate         := aDocument.StartDate;
    Self.StartTime         := aDocument.StartTime;
    Self.TickType1         := aDocument.TickType1;
    Self.TickType2         := aDocument.TickType2;
    Self.TrailBuy          := aDocument.TrailBuy;
    Self.TrailSell         := aDocument.TrailSell;
    Self.TypeOperation     := aDocument.TypeOperation;
    Self.UpProc            := aDocument.UpProc;
    Self.XmlParams         := aDocument.XmlParams;
    for var TickType := System.Low(TIABTickType) to System.High(TIABTickType) do
      Self.Value[TickType] := aDocument.TickValue[TickType];

    for var CondType := System.Low(TConditionType) to System.High(TConditionType)  do
      Self.ValueArray[CondType] := aDocument.ValueArray[CondType];

    for var MinMax := System.Low(TConditionDoc.TMinMaxValueArray) to System.High(TConditionDoc.TMinMaxValueArray) do
      Self.MinMaxValueArray[MinMax] := aDocument.MinMaxValueArray[MinMax];
  end;
end;

procedure TfrmEditCondition.AssignToDoc(var aDocument: TConditionDoc);
begin
  if Assigned(aDocument) then
  begin
    aDocument.Active            := Self.Active;
    aDocument.CalcValue         := Self.CalcValue;
    aDocument.Bypass            := Self.Bypass;
    aDocument.CondType          := Self.CondType;
    aDocument.CondLimit         := Self.CondLimit;
    aDocument.CondWidth         := Self.CondWidth;
    aDocument.Description       := Self.Description;
    aDocument.DivisionValue     := Self.DivisionValue;
    aDocument.Duration          := TimeOf(Self.Duration);
    aDocument.EndDate           := Self.EndDate;
    aDocument.EndTime           := Self.EndTime;
    aDocument.Gradient          := Self.Gradient;
    aDocument.InequalityCor     := Self.InequalityCor;
    aDocument.InequalityGr      := Self.InequalityGr;
    aDocument.InequalityRt      := Self.InequalityRt;
    aDocument.IsBreakUp         := Self.IsBreakUp;
    aDocument.IsCondition       := Self.IsCondition;
    aDocument.Monitoring        := Self.Monitoring;
    aDocument.Priority          := Self.Priority;
    aDocument.StartDate         := Self.StartDate;
    aDocument.StartTime         := Self.StartTime;
    aDocument.TickType1         := Self.TickType1;
    aDocument.TickType2         := Self.TickType2;
    aDocument.TrailBuy          := Self.TrailBuy;
    aDocument.TrailSell         := Self.TrailSell;
    aDocument.TypeOperation     := Self.TypeOperation;
    aDocument.UpProc            := Self.UpProc;
    aDocument.XmlParams         := Self.XmlParams;
  end;
end;

end.
