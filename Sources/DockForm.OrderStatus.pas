unit DockForm.OrderStatus;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons, Vcl.ExtCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, Document, System.Generics.Collections,
  System.Generics.Defaults, DebugWriter, CustomForms, Global.Types, System.IniFiles, IABSocketAPI_const, IABSocketAPI,
  System.Math, InstrumentList, Edit.OrderStatus, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ExtDlgs, Common.Types,
  Vcl.Printers, DaModule, Vcl.DBCtrls, Data.DB, XmlFiles, HtmlLib, Monitor.Types,
  Entity.OrderStatus, Vcl.ComCtrls, System.DateUtils, ArrayHelper, VirtualTrees.ExportHelper, DaImages, CustomDockForm,
  MessageDialog, Publishers.Interfaces, DaModule.Utils, Publishers, Frame.Custom, Frame.OrderStatus;
{$ENDREGION}

type
  TfrmDockFormOrderStatus = class(TfrmCustomDockForm)
    aLoadFromDB: TAction;
    aShowOrderStatus: TAction;
    btnLoadFromDB: TBitBtn;
    cbGroup: TComboBox;
    cbShowSleepingOrders: TCheckBox;
    edDateFrom: TDateTimePicker;
    edDateTo: TDateTimePicker;
    edTimeFrom: TDateTimePicker;
    edTimeTo: TDateTimePicker;
    frameOrderStatus: TframeOrderStatus;
    lblDateFrom: TLabel;
    lblDateTo: TLabel;
    lblGroupBy: TLabel;
    miShowOrderStatus: TMenuItem;
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aLoadFromDBExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aShowOrderStatusUpdate(Sender: TObject);
    procedure cbGroupChange(Sender: TObject);
    procedure cbShowSleepingOrdersClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private const
    C_IDENTITY_NAME = 'DockFormOrderStatus';
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure Deinitialize; override;
  end;

var
  frmDockFormOrderStatus: TfrmDockFormOrderStatus;

implementation

{$R *.dfm}

{ TfrmDockFormOrderStatus }

procedure TfrmDockFormOrderStatus.FormCreate(Sender: TObject);
begin
  inherited;
  cbGroup.Items.Clear;
  for var st := Low(TOrderStatusGroupOption) to High(TOrderStatusGroupOption) do
    cbGroup.Items.Add(st.ToString);
  cbGroup.ItemIndex := 0;
end;

procedure TfrmDockFormOrderStatus.Initialize;
begin
  inherited Initialize;
  Caption := 'Order Log';
  edDateFrom.DateTime := StartOfTheDay(Date);
  edDateTo.DateTime   := EndOfTheDay(Date);
  frameOrderStatus.Initialize;
end;

procedure TfrmDockFormOrderStatus.Deinitialize;
begin
  inherited;
end;

function TfrmDockFormOrderStatus.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmDockFormOrderStatus.aShowOrderStatusUpdate(Sender: TObject);
var
  Data: PStatusData;
  Node: PVirtualNode;
begin
  TAction(Sender).Visible := False;
  Node := frameOrderStatus.vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    TAction(Sender).Visible := Assigned(Data) and (Data^.NodeType = ntNode) and Assigned(Data^.NodeOrder);
  end;
end;

procedure TfrmDockFormOrderStatus.cbGroupChange(Sender: TObject);
begin
  if Showing then
    frameOrderStatus.GroupOption := TOrderStatusGroupOption(cbGroup.ItemIndex);
end;

procedure TfrmDockFormOrderStatus.cbShowSleepingOrdersClick(Sender: TObject);
begin
  inherited;
  frameOrderStatus.IsShowSleeping := cbShowSleepingOrders.Checked;
end;

procedure TfrmDockFormOrderStatus.aColumnSettingsExecute(Sender: TObject);
begin
  frameOrderStatus.ColumnSettings;
end;

procedure TfrmDockFormOrderStatus.aPrintExecute(Sender: TObject);
begin
  frameOrderStatus.Print;
end;

procedure TfrmDockFormOrderStatus.aExportToCSVExecute(Sender: TObject);
begin
  frameOrderStatus.ExportToCSV;
end;

procedure TfrmDockFormOrderStatus.aExportToExcelExecute(Sender: TObject);
begin
  frameOrderStatus.ExportToExcel;
end;

procedure TfrmDockFormOrderStatus.aLoadFromDBExecute(Sender: TObject);
begin
  frameOrderStatus.LoadFromDB(edDateFrom.Date + edTimeFrom.Time, edDateTo.Date + edTimeTo.Time);
end;

end.
