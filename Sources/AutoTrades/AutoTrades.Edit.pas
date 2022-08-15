unit AutoTrades.Edit;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids, DaModule,
  System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, System.ImageList, Vcl.Menus, Vcl.Mask,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging,{$ENDIF} Vcl.ImgList, System.Math, Vcl.DBCtrls, Winapi.ActiveX, System.UITypes,
  VirtualTrees, BrokerHelperAbstr, DebugWriter, HtmlLib, Scanner.Types, CustomForms, MessageDialog, Global.Resources,
  Vcl.Samples.Spin, Global.Types, AutoTrades.Types, DaImages, Vcl.Imaging.pngimage, Vcl.VirtualImage, Utils,
  Vcl.NumberBox, IABSocketAPI_const, Common.Types;
{$ENDREGION}

type
  TfrmAutoTradesEdit = class(TCustomForm, IAutoTrade)
    aCancel: TAction;
    ActionList: TActionList;
    aSave: TAction;
    aShowScanner: TAction;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    btnShowScannerMain: TBitBtn;
    cbAllowSendDuplicateOrder: TCheckBox;
    cbDurationTimeUnits: TComboBox;
    cbEnabled: TCheckBox;
    cbHistDataKeepUpdated: TCheckBox;
    cbOrderCurrency: TComboBox;
    cbOrderCurrencyAdd: TSpeedButton;
    cbSubscribeHistoricalData: TCheckBox;
    cbValidBarSize: TComboBox;
    edDuration: TNumberBox;
    edMaxNumberOrder: TNumberBox;
    edMaxRows: TNumberBox;
    edName: TEdit;
    edNote: TMemo;
    edSingleOrderAmount: TNumberBox;
    edTotalOrderAmount: TNumberBox;
    gbHistoricalOptions: TGroupBox;
    gbRules: TGroupBox;
    GroupBox1: TGroupBox;
    imgWarning: TVirtualImage;
    lblDuration: TLabel;
    lblInfo: TLabel;
    lblMaxNumberOrder: TLabel;
    lblMaxRows: TLabel;
    lblName: TLabel;
    lblSingleOrderAmount: TLabel;
    lblTotalOrderAmount: TLabel;
    lblValidBarSize: TLabel;
    pnlBottom: TPanel;
    pnlInfo: TPanel;
    pnlTop: TPanel;
    procedure aCancelExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aShowScannerExecute(Sender: TObject);
    procedure aShowScannerUpdate(Sender: TObject);
    procedure cbOrderCurrencyAddClick(Sender: TObject);
    procedure edMaxNumberOrderChangeValue(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OnGUIToAutoTradeInfo(Sender: TObject);
  private const
    C_SECTION_SCANNER_MAIN     = 'ScannerMain';
    C_KEY_ORDER_CURRENCY       = 'OrderCurrency';
    C_KEY_ORDER_CURRENCY_LIST  = 'OrderCurrencyList';
  private
    FLoaded: Boolean;
    FAutoTradeInfo: TAutoTradeInfo;
    [weak] FScannerMain: IAutoTrade;
    function CheckData: Boolean;
    procedure LoadParamsFromXml;
    procedure SaveParamsToXml;
    procedure AutoTradeInfoToGUI;

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IAutoTrade
    function GetAutoTradeInfo: TAutoTradeInfo;
    function GetTradesState: TTradesState;
    procedure CloseAutoTrade(const aSilenceMode: Boolean = False);
    procedure SetAutoTradeInfo(const aAutoTradeInfo: TAutoTradeInfo);
    procedure SetTradesState(const aValue: TTradesState);
  public
    class function ShowDocument(var aAutoTradeInfo: TAutoTradeInfo): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

uses
  Scanner.Main;

{$R *.dfm}

class function TfrmAutoTradesEdit.ShowDocument(var aAutoTradeInfo: TAutoTradeInfo): TModalResult;
var
  frmAutoTradesEdit: TfrmAutoTradesEdit;
begin
  frmAutoTradesEdit := TfrmAutoTradesEdit.Create(nil);
  try
    frmAutoTradesEdit.FAutoTradeInfo.AssignFrom(aAutoTradeInfo);
    frmAutoTradesEdit.FLoaded := True;
    try
      frmAutoTradesEdit.Initialize;
    finally
      frmAutoTradesEdit.FLoaded := False;
    end;
    Result := frmAutoTradesEdit.ShowModal;
    frmAutoTradesEdit.SaveParamsToXml;
    if (Result = mrOk) then
    begin
      frmAutoTradesEdit.Denitialize;
      aAutoTradeInfo.AssignFrom(frmAutoTradesEdit.FAutoTradeInfo);
    end;
  finally
    FreeAndNil(frmAutoTradesEdit);
  end;
end;

procedure TfrmAutoTradesEdit.Initialize;
begin
  LoadParamsFromXml;
  if (FAutoTradeInfo.InstanceNum <= 0) then
    FAutoTradeInfo.InstanceNum := -General.GetNextInstanceNum;
  lblInfo.Caption := rsChangingDocument;

  cbValidBarSize.Items.Clear;
  for var ChartBarSize := Low(TIABChartBarSize) to High(TIABChartBarSize) do
    cbValidBarSize.Items.AddObject(ChartBarSizeString[ChartBarSize], TObject(ChartBarSize));
  cbValidBarSize.ItemIndex := Ord(bs30sec);

  cbDurationTimeUnits.Items.Clear;
  cbDurationTimeUnits.Items.AddObject('S Seconds', TObject(IAB_TIME_UNIT_SEC)); //IAB_TIME_UNIT_SEC   = 0;
  cbDurationTimeUnits.Items.AddObject('D Day', TObject(IAB_TIME_UNIT_DAY));     //IAB_TIME_UNIT_DAY   = 1;
  cbDurationTimeUnits.Items.AddObject('W Week', TObject(IAB_TIME_UNIT_WEEK));   //IAB_TIME_UNIT_WEEK  = 2;
  cbDurationTimeUnits.Items.AddObject('M Month', TObject(IAB_TIME_UNIT_MONTH)); //IAB_TIME_UNIT_MONTH = 3;
  cbDurationTimeUnits.Items.AddObject('Y Year', TObject(IAB_TIME_UNIT_YEAR));   //IAB_TIME_UNIT_YEAR  = 4;
  cbDurationTimeUnits.ItemIndex := 0;
  AutoTradeInfoToGUI;
end;

procedure TfrmAutoTradesEdit.Denitialize;
begin
  OnGUIToAutoTradeInfo(nil);
end;

procedure TfrmAutoTradesEdit.edMaxNumberOrderChangeValue(Sender: TObject);
begin
  if Showing then
    edTotalOrderAmount.ValueInt := edSingleOrderAmount.ValueInt * edMaxNumberOrder.ValueInt;
end;

procedure TfrmAutoTradesEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
//  if Assigned(FScannerMain) then
//  FScannerMain.GetAutoTradeInfo;
  if (ModalResult = mrOk) then
  begin
    Denitialize;
    CanClose := CheckData;
  end;
end;

function TfrmAutoTradesEdit.CheckData: Boolean;
resourcestring
  rsTotalOrderAmount = 'Total order amount is 0. AutoTrade is not active.';

  function CheckRequired: Boolean;
  var
    Problems: string;
  begin
    Problems := '';
    if (FAutoTradeInfo.MaxRows = 0) then
    begin
      SetFocusSafely(edMaxRows);
      Problems := Format(rcRequiredValue, ['Max rows']);
    end;
    if (FAutoTradeInfo.OrderAmount = 0) then
    begin
      SetFocusSafely(edSingleOrderAmount);
      Problems := Format(rcRequiredValue, ['Single order amount']);
    end;
//    if (FAutoTradeInfo.TotalOrderAmount = 0) then
//    begin
//      SetFocusSafely(seTotalOrderAmount);
//      Problems := Format(rcRequiredValue, ['Total order amount']);
//    end;
    if FAutoTradeInfo.OrderCurrency.IsEmpty then
    begin
      SetFocusSafely(cbOrderCurrency);
      Problems := Format(rcRequiredValue, ['Order Currency']);
    end;
    if (FAutoTradeInfo.MaxNumberOrder = 0) then
    begin
      SetFocusSafely(edMaxNumberOrder);
      Problems := Format(rcRequiredValue, ['Max number of order']);
    end;

    Result := Problems.IsEmpty;
    if not Result then
      TMessageDialog.ShowWarning(Problems);
  end;

begin
  Result := CheckRequired;
  if Result then
    if (FAutoTradeInfo.TotalOrderAmount = 0) then
      TMessageDialog.ShowWarning(rsTotalOrderAmount);
end;

procedure TfrmAutoTradesEdit.LoadParamsFromXml;
begin
  cbOrderCurrency.Items.Text := General.XMLFile.ReadString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY_LIST, C_DEFAULT_CURRENCY);
  cbOrderCurrency.Text       := General.XMLFile.ReadString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY, C_DEFAULT_CURRENCY);
end;

procedure TfrmAutoTradesEdit.SaveParamsToXml;
begin
  General.XMLFile.WriteString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY, cbOrderCurrency.Text);
  General.XMLFile.WriteString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY_LIST, cbOrderCurrency.Items.Text);
end;

procedure TfrmAutoTradesEdit.aSaveExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmAutoTradesEdit.aShowScannerExecute(Sender: TObject);
begin
  if not Assigned(FScannerMain) then
  begin
    OnGUIToAutoTradeInfo(nil);
    FAutoTradeInfo.Active := False;
    FScannerMain := TfrmScannerMain.ShowDocument(Self);
  end;
end;

procedure TfrmAutoTradesEdit.aShowScannerUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Assigned(FScannerMain);
end;

procedure TfrmAutoTradesEdit.AutoTradeInfoToGUI;
begin
  edName.Text                       := FAutoTradeInfo.Name;
  edSingleOrderAmount.ValueInt      := FAutoTradeInfo.OrderAmount;
  edTotalOrderAmount.ValueInt       := FAutoTradeInfo.TotalOrderAmount;
  cbOrderCurrency.Text              := FAutoTradeInfo.OrderCurrency;
  edMaxNumberOrder.ValueInt         := FAutoTradeInfo.MaxNumberOrder;
  edMaxRows.ValueInt                := FAutoTradeInfo.MaxRows;
  cbAllowSendDuplicateOrder.Checked := FAutoTradeInfo.AllowSendDuplicateOrder;
  cbEnabled.Checked                 := FAutoTradeInfo.Enabled;
  cbSubscribeHistoricalData.Checked := FAutoTradeInfo.HistoricalDataParams.SubscribeHistData;
  cbHistDataKeepUpdated.Checked     := FAutoTradeInfo.HistoricalDataParams.KeepUpdated;
  cbEnabled.Checked                 := FAutoTradeInfo.Active;
  cbValidBarSize.ItemIndex          := Ord(FAutoTradeInfo.HistoricalDataParams.BarSize);
  cbSubscribeHistoricalData.Checked := FAutoTradeInfo.HistoricalDataParams.SubscribeHistData;
  cbHistDataKeepUpdated.Checked     := FAutoTradeInfo.HistoricalDataParams.KeepUpdated;
  cbDurationTimeUnits.ItemIndex     := FAutoTradeInfo.HistoricalDataParams.DurationTimeUnits;
  edDuration.ValueInt               := FAutoTradeInfo.HistoricalDataParams.DataDuration;

  edNote.Lines.BeginUpdate;
  try
    edNote.Lines.Text := FAutoTradeInfo.Note;
  finally
    edNote.Lines.EndUpdate;
  end;
end;

procedure TfrmAutoTradesEdit.OnGUIToAutoTradeInfo(Sender: TObject);
begin
  if not FLoaded then
  begin
    FAutoTradeInfo.Name                    := edName.Text;
    FAutoTradeInfo.Note                    := edNote.Lines.Text;
    FAutoTradeInfo.OrderAmount             := edSingleOrderAmount.ValueInt;
    FAutoTradeInfo.TotalOrderAmount        := edTotalOrderAmount.ValueInt;
    FAutoTradeInfo.OrderCurrency           := cbOrderCurrency.Text;
    FAutoTradeInfo.MaxNumberOrder          := edMaxNumberOrder.ValueInt;
    FAutoTradeInfo.MaxRows                 := edMaxRows.ValueInt;
    FAutoTradeInfo.AllowSendDuplicateOrder := cbAllowSendDuplicateOrder.Checked;
    FAutoTradeInfo.Enabled                 := cbEnabled.Checked;
    FAutoTradeInfo.Active                  := cbEnabled.Checked;
    FAutoTradeInfo.HistoricalDataParams.SubscribeHistData := cbSubscribeHistoricalData.Checked;
    FAutoTradeInfo.HistoricalDataParams.KeepUpdated       := cbHistDataKeepUpdated.Checked;
    FAutoTradeInfo.HistoricalDataParams.DataDuration      := edDuration.ValueInt;
    if (cbValidBarSize.ItemIndex > -1) then
      FAutoTradeInfo.HistoricalDataParams.BarSize := TIABChartBarSize(cbValidBarSize.ItemIndex);
    if (cbDurationTimeUnits.ItemIndex > -1) then
      FAutoTradeInfo.HistoricalDataParams.DurationTimeUnits := cbDurationTimeUnits.ItemIndex;
    if Assigned(FScannerMain) then
      FScannerMain.SetAutoTradeInfo(FAutoTradeInfo);
  end;
end;

procedure TfrmAutoTradesEdit.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmAutoTradesEdit.cbOrderCurrencyAddClick(Sender: TObject);
begin
  if (cbOrderCurrency.Items.IndexOf(UpperCase(cbOrderCurrency.Text)) = -1) then
    cbOrderCurrency.Items.Add(UpperCase(cbOrderCurrency.Text));
end;

function TfrmAutoTradesEdit.GetAutoTradeInfo: TAutoTradeInfo;
begin
  Result := FAutoTradeInfo;
end;

procedure TfrmAutoTradesEdit.SetAutoTradeInfo(const aAutoTradeInfo: TAutoTradeInfo);
begin
  FLoaded := True;
  try
    FAutoTradeInfo.AssignFrom(aAutoTradeInfo);
    AutoTradeInfoToGUI;
  finally
    FLoaded := False;
  end;
end;

function TfrmAutoTradesEdit.GetInstance: TObject;
begin
  Result := Self;
end;

function TfrmAutoTradesEdit.GetTradesState: TTradesState;
begin
  Result := TTradesState.tsSuspended;
end;

procedure TfrmAutoTradesEdit.CloseAutoTrade(const aSilenceMode: Boolean);
begin
  //nothing
end;

procedure TfrmAutoTradesEdit.SetTradesState(const aValue: TTradesState);
begin
  //nothing
end;

end.
