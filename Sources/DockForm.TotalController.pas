unit DockForm.TotalController;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons, Vcl.ExtCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, Document, System.Generics.Collections,
  System.Generics.Defaults, DebugWriter, Global.Types, System.IniFiles, IABSocketAPI_const, IABSocketAPI,
  System.Math, InstrumentList, Edit.OrderStatus, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ExtDlgs, BrokerHelperAbstr,
  Vcl.Printers, DaModule, Scanner.Types, MessageDialog, Qualifiers.Types, System.DateUtils, DaImages, IABFunctions,
  VirtualTrees.ExportHelper, Monitor.Interfaces, Common.Types, AutoTrades.Types, Monitor.Types, CustomDockForm,
  Qualifiers.Frame, AutoTrades.Frame, Vcl.ComCtrls, Frame.ActiveOrders, Frame.OrderStatus, Frame.Custom;
{$ENDREGION}

type
  TfrmDockFormTotalController = class(TfrmCustomDockForm)
    aArmQualifiers: TAction;
    aCancelActiveOrder: TAction;
    aCancelAllActiveOrder: TAction;
    aColumnSettingsActiveOrders: TAction;
    aColumnSettingsAutotrades: TAction;
    aColumnSettingsOrderStatus: TAction;
    aDeleteAllAutotrades: TAction;
    aDeleteAllQualifier: TAction;
    aDeleteAutotrades: TAction;
    aDeleteQualifier: TAction;
    aExportToCSVActiveOrders: TAction;
    aExportToCSVAutotrades: TAction;
    aExportToCSVOrderStatus: TAction;
    aExportToExcelActiveOrders: TAction;
    aExportToExcelAutotrades: TAction;
    aExportToExcelOrderStatus: TAction;
    aLoadFromDBOrderStatus: TAction;
    aPrintActiveOrders: TAction;
    aPrintAutotrades: TAction;
    aPrintOrderStatus: TAction;
    aShowActiveOrder: TAction;
    aStartAutotrade: TAction;
    aStartQualifier: TAction;
    aStopAutotrade: TAction;
    aStopQualifier: TAction;
    aUnArmQualifiers: TAction;
    btnCancelActiveOrder: TBitBtn;
    btnColumnSettingsActiveOrders: TBitBtn;
    btnColumnSettingsAutotrades: TBitBtn;
    btnColumnSettingsOrderStatus: TBitBtn;
    btnDelete: TBitBtn;
    btnDeleteAllAutotrades: TBitBtn;
    btnDeleteAllQualifier: TBitBtn;
    btnDeleteAutotrades: TBitBtn;
    btnExportToCSVActiveOrders: TBitBtn;
    btnExportToCSVAutotrades: TBitBtn;
    btnExportToCSVOrderStatus: TBitBtn;
    btnExportToExcelActiveOrders: TBitBtn;
    btnExportToExcelAutotrades: TBitBtn;
    btnExportToExcelOrderStatus: TBitBtn;
    btnLoadFromDBOrderStatus: TBitBtn;
    btnPrintActiveOrders: TBitBtn;
    btnPrintAutotrades: TBitBtn;
    btnPrintOrderStatus: TBitBtn;
    btnStart: TBitBtn;
    btnStartAutotrades: TBitBtn;
    btnStop: TBitBtn;
    btnStopAutotrades: TBitBtn;
    cbGroupOrderStatus: TComboBox;
    cbOptionsActiveOrders: TComboBox;
    cbShowAllActiveOrders: TCheckBox;
    cbShowAllAutotrades: TCheckBox;
    cbShowAllOrderStatus: TCheckBox;
    cbShowSleepingOrders: TCheckBox;
    edDateFromOrderStatus: TDateTimePicker;
    edDateToOrderStatus: TDateTimePicker;
    edTimeFromOrderStatus: TDateTimePicker;
    edTimeToOrderStatus: TDateTimePicker;
    frameActiveOrders: TframeActiveOrders;
    frameAutoTrades: TframeAutoTrades;
    frameOrderStatus: TframeOrderStatus;
    frameQualifiers: TframeQualifiers;
    lblDateFrom: TLabel;
    lblDateTo: TLabel;
    lblGroupByOrderStatus: TLabel;
    lblOptionsActiveOrders: TLabel;
    miCancelActiveOrder: TMenuItem;
    miShowActiveOrder: TMenuItem;
    pcOrders: TPageControl;
    pmActiveOrders: TPopupMenu;
    pnlOptionsActiveOrders: TPanel;
    pnlOptionsAutotrades: TPanel;
    pnlOptionsOrderStatus: TPanel;
    splAutoTrades: TSplitter;
    splOrderStatus: TSplitter;
    tsActiveOrders: TTabSheet;
    tsOrderStatus: TTabSheet;
    procedure aCancelActiveOrderExecute(Sender: TObject);
    procedure aCancelAllActiveOrderExecute(Sender: TObject);
    procedure aCancelAllActiveOrderUpdate(Sender: TObject);
    procedure aColumnSettingsActiveOrdersExecute(Sender: TObject);
    procedure aColumnSettingsAutotradesExecute(Sender: TObject);
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aColumnSettingsOrderStatusExecute(Sender: TObject);
    procedure aDeleteAllAutotradesExecute(Sender: TObject);
    procedure aDeleteAllQualifierExecute(Sender: TObject);
    procedure aDeleteAutotradesExecute(Sender: TObject);
    procedure aDeleteQualifierExecute(Sender: TObject);
    procedure aExportToCSVActiveOrdersExecute(Sender: TObject);
    procedure aExportToCSVAutotradesExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToCSVOrderStatusExecute(Sender: TObject);
    procedure aExportToExcelActiveOrdersExecute(Sender: TObject);
    procedure aExportToExcelAutotradesExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aExportToExcelOrderStatusExecute(Sender: TObject);
    procedure aLoadFromDBOrderStatusExecute(Sender: TObject);
    procedure aPrintActiveOrdersExecute(Sender: TObject);
    procedure aPrintAutotradesExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aPrintOrderStatusExecute(Sender: TObject);
    procedure aShowActiveOrderExecute(Sender: TObject);
    procedure aStartAutotradeExecute(Sender: TObject);
    procedure aStartQualifierExecute(Sender: TObject);
    procedure aStopAutotradeExecute(Sender: TObject);
    procedure aStopQualifierExecute(Sender: TObject);
    procedure cbGroupOrderStatusChange(Sender: TObject);
    procedure cbOptionsActiveOrdersChange(Sender: TObject);
    procedure cbShowAllActiveOrdersClick(Sender: TObject);
    procedure cbShowAllAutotradesClick(Sender: TObject);
    procedure cbShowAllOrderStatusClick(Sender: TObject);
    procedure cbShowSleepingOrdersClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure frameAutoTradesvstTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure frameQualifiersvstTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  private const
    C_IDENTITY_NAME = 'DockFormTotalController';
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure Deinitialize; override;
  end;

var
  frmDockFormTotalController: TfrmDockFormTotalController;

implementation

{$R *.dfm}

{ TfrmDockFormTotalController }

procedure TfrmDockFormTotalController.FormDestroy(Sender: TObject);
begin
  frmDockFormTotalController := nil;
  inherited;
end;

procedure TfrmDockFormTotalController.Initialize;
begin
  inherited Initialize;
  Caption := 'Total Controller';
  frameQualifiers.Initialize;
  frameAutoTrades.Initialize;
  frameActiveOrders.Initialize;
  frameOrderStatus.Initialize;

  frameAutoTrades.Parameters   := [fpFiltered];
  frameActiveOrders.Parameters := [fpFiltered];
  frameOrderStatus.Parameters  := [fpFiltered];

  cbOptionsActiveOrders.Items.Clear;
  for var go := Low(TActiveOrdersGroupOption) to High(TActiveOrdersGroupOption) do
    cbOptionsActiveOrders.Items.Add(ActiveOrdersGroupOptionName[go]);
  cbOptionsActiveOrders.ItemIndex := 0;

  edDateFromOrderStatus.DateTime := StartOfTheDay(Date);
  edDateToOrderStatus.DateTime   := EndOfTheDay(Date);
  cbGroupOrderStatus.Items.Clear;
  for var st := Low(TOrderStatusGroupOption) to High(TOrderStatusGroupOption) do
    cbGroupOrderStatus.Items.Add(st.ToString);
  cbGroupOrderStatus.ItemIndex := 0;
end;

procedure TfrmDockFormTotalController.Deinitialize;
begin
  inherited;
  frameQualifiers.Deinitialize;
  frameAutoTrades.Deinitialize;
  frameActiveOrders.Deinitialize;
  frameOrderStatus.Deinitialize;
end;

function TfrmDockFormTotalController.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmDockFormTotalController.aColumnSettingsExecute(Sender: TObject);
begin
  frameOrderStatus.ColumnSettings;
end;

procedure TfrmDockFormTotalController.aPrintExecute(Sender: TObject);
begin
  frameQualifiers.Print;
end;

procedure TfrmDockFormTotalController.aStartQualifierExecute(Sender: TObject);
begin
  inherited;
  frameQualifiers.Start;
end;

procedure TfrmDockFormTotalController.aStopQualifierExecute(Sender: TObject);
begin
  inherited;
  frameQualifiers.Stop;
end;

procedure TfrmDockFormTotalController.aExportToCSVExecute(Sender: TObject);
begin
  frameQualifiers.ExportToCSV;
end;

procedure TfrmDockFormTotalController.aExportToExcelExecute(Sender: TObject);
begin
  frameQualifiers.ExportToExcel;
end;

procedure TfrmDockFormTotalController.aDeleteQualifierExecute(Sender: TObject);
begin
  frameQualifiers.Delete;
end;

procedure TfrmDockFormTotalController.aDeleteAllQualifierExecute(Sender: TObject);
begin
  frameQualifiers.DeleteAll;
end;

procedure TfrmDockFormTotalController.aDeleteAutotradesExecute(Sender: TObject);
begin
  inherited;
  frameAutoTrades.Delete;
end;

procedure TfrmDockFormTotalController.aStartAutotradeExecute(Sender: TObject);
begin
  inherited;
  frameAutoTrades.Start;
end;

procedure TfrmDockFormTotalController.aStopAutotradeExecute(Sender: TObject);
begin
  inherited;
  frameAutoTrades.Stop;
end;

procedure TfrmDockFormTotalController.aDeleteAllAutotradesExecute(Sender: TObject);
begin
  inherited;
  frameAutoTrades.DeleteAll;
end;

procedure TfrmDockFormTotalController.aExportToExcelAutotradesExecute(Sender: TObject);
begin
  inherited;
  frameAutoTrades.ExportToExcel;
end;

procedure TfrmDockFormTotalController.aExportToCSVAutotradesExecute(Sender: TObject);
begin
  inherited;
  frameAutoTrades.ExportToCSV;
end;

procedure TfrmDockFormTotalController.aPrintAutotradesExecute(Sender: TObject);
begin
  inherited;
  frameAutoTrades.Print;
end;

procedure TfrmDockFormTotalController.aColumnSettingsAutotradesExecute(Sender: TObject);
begin
  inherited;
  frameAutoTrades.ColumnSettings;
end;

procedure TfrmDockFormTotalController.aExportToExcelActiveOrdersExecute(Sender: TObject);
begin
  frameActiveOrders.ExportToExcel;
end;

procedure TfrmDockFormTotalController.aExportToCSVActiveOrdersExecute(Sender: TObject);
begin
  frameActiveOrders.ExportToCSV;
end;

procedure TfrmDockFormTotalController.aCancelActiveOrderExecute(Sender: TObject);
begin
  frameActiveOrders.CancelOrder;
end;

procedure TfrmDockFormTotalController.aCancelAllActiveOrderExecute(Sender: TObject);
begin
  frameActiveOrders.CancelAllOrders;
end;

procedure TfrmDockFormTotalController.aCancelAllActiveOrderUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := not frameActiveOrders.vstTree.IsEmpty;
end;

procedure TfrmDockFormTotalController.aColumnSettingsActiveOrdersExecute(Sender: TObject);
begin
  frameActiveOrders.ColumnSettings;
end;

procedure TfrmDockFormTotalController.aPrintActiveOrdersExecute(Sender: TObject);
begin
  frameActiveOrders.Print;
end;

procedure TfrmDockFormTotalController.aShowActiveOrderExecute(Sender: TObject);
begin
  frameActiveOrders.ShowOrder;
end;

procedure TfrmDockFormTotalController.cbOptionsActiveOrdersChange(Sender: TObject);
begin
  inherited;
  if Showing then
    frameActiveOrders.GroupOption := TActiveOrdersGroupOption(cbOptionsActiveOrders.ItemIndex);
end;

procedure TfrmDockFormTotalController.aExportToExcelOrderStatusExecute(Sender: TObject);
begin
  inherited;
  frameOrderStatus.ExportToExcel;
end;

procedure TfrmDockFormTotalController.aExportToCSVOrderStatusExecute(Sender: TObject);
begin
  frameOrderStatus.ExportToCSV;
end;

procedure TfrmDockFormTotalController.aPrintOrderStatusExecute(Sender: TObject);
begin
  frameOrderStatus.Print;
end;

procedure TfrmDockFormTotalController.aColumnSettingsOrderStatusExecute(Sender: TObject);
begin
  frameOrderStatus.ColumnSettings;
end;

procedure TfrmDockFormTotalController.cbGroupOrderStatusChange(Sender: TObject);
begin
  if Showing then
    frameOrderStatus.GroupOption := TOrderStatusGroupOption(cbGroupOrderStatus.ItemIndex);
end;

procedure TfrmDockFormTotalController.aLoadFromDBOrderStatusExecute(Sender: TObject);
begin
  frameOrderStatus.LoadFromDB(edDateFromOrderStatus.Date + edTimeFromOrderStatus.Time, edDateToOrderStatus.Date + edTimeToOrderStatus.Time);
end;

procedure TfrmDockFormTotalController.frameQualifiersvstTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PQualifierData;
begin
  inherited;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if (Data^.DocType = ntQualifier) then
      frameAutoTrades.SetFilter(TAutoTradesCommon.Create(0, Data^.Qualifier.InstanceNum, Data^.Qualifier.RecordId, 0, 0, False));
    if not frameAutoTrades.vstTree.IsEmpty then
      frameAutoTradesvstTreeFocusChanged(frameAutoTrades.vstTree, frameAutoTrades.GetFirstVisibleNode, 0);
  end
  else
    frameAutoTrades.SetFilter(Default(TAutoTradesCommon));
end;

procedure TfrmDockFormTotalController.frameAutoTradesvstTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PTradesData;
begin
  inherited;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    frameActiveOrders.SetFilter(TAutoTradesCommon.Create(0, Data.QualifierInstance, Data.QualifierId, Data.InstanceNum, Data.AutoTradesId, False));
    frameOrderStatus.SetFilter(TAutoTradesCommon.Create(0, Data.QualifierInstance, Data.QualifierId, Data.InstanceNum, Data.AutoTradesId, False));
  end
  else
  begin
    frameAutoTrades.SetFilter(Default(TAutoTradesCommon));
    frameOrderStatus.SetFilter(Default(TAutoTradesCommon));
  end;
end;

procedure TfrmDockFormTotalController.cbShowAllActiveOrdersClick(Sender: TObject);
begin
  inherited;
  if cbShowAllActiveOrders.Checked then
  begin
    frameActiveOrders.Parameters := frameActiveOrders.Parameters - [fpFiltered];
    frameActiveOrders.SetFilter(Default(TAutoTradesCommon));
  end
  else
  begin
    frameActiveOrders.Parameters := frameActiveOrders.Parameters + [fpFiltered];
    frameAutoTradesvstTreeFocusChanged(frameActiveOrders.vstTree, frameActiveOrders.GetFirstVisibleNode, 0);
  end;
end;

procedure TfrmDockFormTotalController.cbShowAllAutotradesClick(Sender: TObject);
begin
  inherited;
  if cbShowAllAutotrades.Checked then
  begin
    frameAutoTrades.Parameters := frameAutoTrades.Parameters - [fpFiltered];
    frameAutoTrades.SetFilter(Default(TAutoTradesCommon));
  end
  else
  begin
    frameAutoTrades.Parameters := frameAutoTrades.Parameters + [fpFiltered];
    frameQualifiersvstTreeFocusChanged(frameQualifiers.vstTree, frameQualifiers.GetFirstVisibleNode, 0);
  end;
end;

procedure TfrmDockFormTotalController.cbShowAllOrderStatusClick(Sender: TObject);
begin
  inherited;
  if cbShowAllOrderStatus.Checked then
  begin
    frameOrderStatus.Parameters := frameOrderStatus.Parameters - [fpFiltered];
    frameOrderStatus.SetFilter(Default(TAutoTradesCommon));
  end
  else
  begin
    frameOrderStatus.Parameters := frameOrderStatus.Parameters + [fpFiltered];
    frameAutoTradesvstTreeFocusChanged(frameOrderStatus.vstTree, frameOrderStatus.GetFirstVisibleNode, 0);
  end;
end;

procedure TfrmDockFormTotalController.cbShowSleepingOrdersClick(Sender: TObject);
begin
  inherited;
  frameOrderStatus.IsShowSleeping := cbShowSleepingOrders.Checked;
end;

end.
