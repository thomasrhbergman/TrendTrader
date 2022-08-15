unit Edit.Algos;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Data.DB, Vcl.CheckLst, Vcl.ExtCtrls,
  ParametersStore, DaModule, VirtualTrees, XmlFiles, BrokerHelperAbstr, BrokerHelperFactory,
  {Search.Instruments,} {IABFunctions, IABSocketAPI,} Document, System.UITypes, System.Actions, Vcl.ActnList,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Global.Types, IABSocketAPI_const, MessageDialog,
  Monitor.Types,{ InstrumentList,} Monitor.Interfaces, Common.Types, DaImages;
{$ENDREGION}

type
  TfrmEditAlgos = class(TFormDocument)
    ActionList: TActionList;
    aGraph: TAction;
    aSave: TAction;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    btnShowChart: TBitBtn;
    edAlgosName: TEdit;
    lblAlgosName: TLabel;
    lblValue: TLabel;
    lblValueCaption: TLabel;
    pnlBottom: TPanel;
    procedure aGraphExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private const
    C_VALUE_SEPARATOR: Char = ',';
    C_IDENTITY_NAME = 'EditAlgos';
    C_FACTORS_SECTION_NAME = 'Factors';
  private
    { Private declarations }
    FDecimals: Integer;
    FDivisor: Double;
    FModyfied: Boolean;
    FName: string;
    FParametersStore: TParametersStore;
    function GetAlgosName: string;
    function GetDecimalst: Integer;
    function GetDivisor: Double;
    procedure SetAlgosName(const Value: string);
    procedure SetDecimals(const Value: Integer);
    procedure SetDivisor(const Value: Double);
    procedure SetMainValue(const Value: Double);
    property Modyfied: Boolean read FModyfied write FModyfied;
  public
    procedure AssignFromDoc(const aDocument: TAlgosDoc);
    procedure AssignToDoc(var aDocument: TAlgosDoc);
    class function ShowDocument(aDocument: TAlgosDoc): TModalResult;
    procedure Initialize;
    property Name      : string  read GetAlgosName write SetAlgosName;
    property Decimals  : Integer read GetDecimalst write SetDecimals;
    property Divisor   : Double  read GetDivisor   write SetDivisor;
    property MainValue : Double                    write SetMainValue;
  end;

var
  frmEditAlgos: TfrmEditAlgos;

implementation

{$R *.dfm}

class function TfrmEditAlgos.ShowDocument(aDocument: TAlgosDoc): TModalResult;
begin
  Result := mrCancel;
  if Assigned(aDocument) then
  begin
    frmEditAlgos := TfrmEditAlgos.Create(nil);
    try
      frmEditAlgos.AssignFromDoc(aDocument);
      frmEditAlgos.Initialize;
      if (frmEditAlgos.ShowModal = mrOk) then
      begin
        Result := mrOk;
        frmEditAlgos.AssignToDoc(aDocument);
      end;
    finally
      FreeAndNil(frmEditAlgos);
    end;
  end;
end;

procedure TfrmEditAlgos.FormCreate(Sender: TObject);
begin
  inherited;
  FModyfied   := False;
  FParametersStore := TParametersStore.Create;
  DMod.CheckConnect;
  FParametersStore.Connection := DMod.ConnectionStock;
  FParametersStore.StoreComponent := Self;
  FParametersStore.IdentityName := C_IDENTITY_NAME;
  FParametersStore.PropertiesList.Add('Name');
  FParametersStore.PropertiesList.Add('Decimals');
  FParametersStore.PropertiesList.Add('Divisor');
end;

procedure TfrmEditAlgos.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FParametersStore);
end;

procedure TfrmEditAlgos.Initialize;
begin
  lblValue.Caption := Value[ttLast].ToString;
end;

procedure TfrmEditAlgos.aGraphExecute(Sender: TObject);
var
  Monitor: IMonitor;
begin
  inherited;
  if Assigned(OwnerNode) and Supports(Application.MainForm, IMonitor, Monitor) then
    Monitor.ShowTradeChart(OwnerNode);
end;

function TfrmEditAlgos.GetAlgosName: string;
begin
  Result := edAlgosName.Text;
end;

procedure TfrmEditAlgos.SetAlgosName(const Value: string);
begin
  FName := Value;
  edAlgosName.Text := Value;
end;

function TfrmEditAlgos.GetDecimalst: Integer;
begin
  Result := FDecimals;
end;

procedure TfrmEditAlgos.SetDecimals(const Value: Integer);
begin
  FDecimals := Value;
end;

procedure TfrmEditAlgos.SetDivisor(const Value: Double);
begin
  FDivisor := Value;
end;

procedure TfrmEditAlgos.SetMainValue(const Value: Double);
begin
  lblValue.Caption := Value.ToString;
end;

function TfrmEditAlgos.GetDivisor: Double;
begin
  Result := FDivisor;
end;

procedure TfrmEditAlgos.aSaveExecute(Sender: TObject);
var
  Monitor: IMonitor;
begin
  inherited;
  if Modyfied and
    (TMessageDialog.ShowQuestion('Algos has been changed. Duplicate a algos?') = mrYes) then
  begin
    FName := edAlgosName.Text;
    if Supports(Application.MainForm, IMonitor, Monitor) then
      Monitor.GetDuplicateAlgos(OwnerNode);
  end;
  Modyfied := True;
end;

procedure TfrmEditAlgos.aSaveUpdate(Sender: TObject);
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

procedure TfrmEditAlgos.AssignFromDoc(const aDocument: TAlgosDoc);
var
  TickType: TIABTickType;
begin
  if Assigned(aDocument) then
  begin
    for TickType := System.Low(TIABTickType) to System.High(TIABTickType) do
      Self.Value[TickType] := aDocument.TickValue[TickType];
    Self.Name      := aDocument.Name;
    Self.Decimals  := aDocument.Decimals;
    Self.Divisor   := aDocument.Divisor;
    Self.OwnerNode := aDocument.OwnerNode;
  end;
end;

procedure TfrmEditAlgos.AssignToDoc(var aDocument: TAlgosDoc);
begin
  if Assigned(aDocument) then
  begin
    aDocument.Name     := Self.Name;
    aDocument.Decimals := Self.Decimals;
    aDocument.Divisor  := Self.Divisor;
  end;
end;

end.
