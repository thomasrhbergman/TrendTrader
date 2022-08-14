unit DockForm.AutoTradesController;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons, Vcl.ExtCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, System.Generics.Collections, Publishers,
  System.Generics.Defaults, DebugWriter, CustomDockForm, Global.Types, System.IniFiles, IABSocketAPI_const, IABSocketAPI,
  System.Math, InstrumentList, Edit.OrderStatus, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ExtDlgs, BrokerHelperAbstr,
  Vcl.Printers, DaModule, Scanner.Types, MessageDialog, Monitor.Types, Document, VirtualTrees.ExportHelper,
  IABFunctions.RequestsQueue, Monitor.Interfaces, AutoTrades.Types, Common.Types, DaImages, Frame.Custom,
  AutoTrades.Frame;
{$ENDREGION}

type
  TfrmDockFormAutoTradesController = class(TfrmCustomDockForm)
    aDelete: TAction;
    aDeleteAll: TAction;
    aStart: TAction;
    aStop: TAction;
    btnDelete: TBitBtn;
    btnDeleteAll: TBitBtn;
    btnStart: TBitBtn;
    btnStop: TBitBtn;
    cbGroup: TComboBox;
    frameAutoTrades: TframeAutoTrades;
    lblGroupBy: TLabel;
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aDeleteAllExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aStartExecute(Sender: TObject);
    procedure aStopExecute(Sender: TObject);
    procedure cbGroupChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private const
    C_IDENTITY_NAME = 'DockFormAutoTradesController';
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
  end;

var
  frmDockFormAutoTradesController: TfrmDockFormAutoTradesController;

implementation

{$R *.dfm}

{ TfrmOrderStatusView }

procedure TfrmDockFormAutoTradesController.FormCreate(Sender: TObject);
begin
  vstTree.NodeDataSize := SizeOf(TTradesData);
  inherited;
  cbGroup.Items.Clear;
  for var st in AutoTradeGroupOptionString do
    cbGroup.Items.Add(st);
  cbGroup.ItemIndex := 0;
  AutoTradesControllerPublisher.Subscribe(Self);
end;

procedure TfrmDockFormAutoTradesController.FormDestroy(Sender: TObject);
begin
  if Assigned(AutoTradesControllerPublisher) then
    AutoTradesControllerPublisher.Unsubscribe(Self);
  frmDockFormAutoTradesController := nil;
  inherited;
end;

procedure TfrmDockFormAutoTradesController.Initialize;
begin
  inherited Initialize;
  Caption := 'AutoTrades Controller';
end;

procedure TfrmDockFormAutoTradesController.cbGroupChange(Sender: TObject);
begin
  if Showing then
    frameAutoTrades.GroupOption := TAutoTradeGroupOption(cbGroup.ItemIndex);
end;

function TfrmDockFormAutoTradesController.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmDockFormAutoTradesController.aExportToCSVExecute(Sender: TObject);
begin
  frameAutoTrades.ExportToCSV;
end;

procedure TfrmDockFormAutoTradesController.aExportToExcelExecute(Sender: TObject);
begin
  frameAutoTrades.ExportToExcel;
end;

procedure TfrmDockFormAutoTradesController.aPrintExecute(Sender: TObject);
begin
  frameAutoTrades.Print;
end;

procedure TfrmDockFormAutoTradesController.aColumnSettingsExecute(Sender: TObject);
begin
  frameAutoTrades.ColumnSettings;
end;

procedure TfrmDockFormAutoTradesController.aDeleteAllExecute(Sender: TObject);
begin
  inherited;
  frameAutoTrades.DeleteAll;
end;

procedure TfrmDockFormAutoTradesController.aDeleteExecute(Sender: TObject);
begin
  frameAutoTrades.Delete;
end;

procedure TfrmDockFormAutoTradesController.aStartExecute(Sender: TObject);
begin
  frameAutoTrades.SetState(tsWorking);
end;

procedure TfrmDockFormAutoTradesController.aStopExecute(Sender: TObject);
begin
  frameAutoTrades.SetState(tsSuspended);
end;

end.
