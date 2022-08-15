unit DockForm.ActiveOrders;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ExtCtrls, System.Generics.Collections, System.UITypes, DebugWriter,
  Global.Types, Document, InstrumentList, Winapi.ActiveX, System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.Buttons, System.Generics.Defaults, Vcl.Menus,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Monitor.Types, MessageDialog, Common.Types, CustomDockForm,
  DaImages, Publishers.Interfaces, System.Math, Publishers, Frame.ActiveOrders, Frame.Custom, IABFunctions;
{$ENDREGION}

type
  TfrmDockFormActiveOrders = class(TfrmCustomDockForm)
    aCancelAllOrders: TAction;
    aCancelOrder: TAction;
    aGlobalCancel: TAction;
    aShowOrder: TAction;
    btnCancelOrder: TBitBtn;
    btnGlobalCancel: TBitBtn;
    cbOptions: TComboBox;
    frameActiveOrders: TframeActiveOrders;
    lblOptions: TLabel;
    miCancelOrder: TMenuItem;
    miShowOrder: TMenuItem;
    procedure aCancelAllOrdersExecute(Sender: TObject);
    procedure aCancelAllOrdersUpdate(Sender: TObject);
    procedure aCancelOrderExecute(Sender: TObject);
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aGlobalCancelExecute(Sender: TObject);
    procedure aGlobalCancelUpdate(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aShowOrderExecute(Sender: TObject);
    procedure aShowOrderUpdate(Sender: TObject);
    procedure cbOptionsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private const
    C_IDENTITY_NAME = 'DockFormActiveOrders';
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure Deinitialize; override;
  end;

var
  frmDockFormActiveOrders: TfrmDockFormActiveOrders;

implementation

{$R *.dfm}

{ TfrmDockFormActiveOrders }

procedure TfrmDockFormActiveOrders.FormCreate(Sender: TObject);
begin
  inherited;
  cbOptions.Items.Clear;
  for var go := Low(TActiveOrdersGroupOption) to High(TActiveOrdersGroupOption) do
    cbOptions.Items.Add(ActiveOrdersGroupOptionName[go]);
  cbOptions.ItemIndex := -1;
end;

procedure TfrmDockFormActiveOrders.FormDestroy(Sender: TObject);
begin
  frmDockFormActiveOrders := nil;
  inherited;  
end;

function TfrmDockFormActiveOrders.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmDockFormActiveOrders.Initialize;
begin
  inherited Initialize;
  Caption := 'Active Orders';
  frameActiveOrders.Initialize;
end;

procedure TfrmDockFormActiveOrders.Deinitialize;
begin
  inherited;
  frameActiveOrders.Deinitialize;
end;

procedure TfrmDockFormActiveOrders.cbOptionsChange(Sender: TObject);
begin
  if Showing then
    frameActiveOrders.GroupOption := TActiveOrdersGroupOption(cbOptions.ItemIndex);
end;

procedure TfrmDockFormActiveOrders.aColumnSettingsExecute(Sender: TObject);
begin
  frameActiveOrders.ColumnSettings;
end;

procedure TfrmDockFormActiveOrders.aExportToCSVExecute(Sender: TObject);
begin
  frameActiveOrders.ExportToCSV;
end;

procedure TfrmDockFormActiveOrders.aExportToExcelExecute(Sender: TObject);
begin
  frameActiveOrders.ExportToExcel;
end;

procedure TfrmDockFormActiveOrders.aGlobalCancelExecute(Sender: TObject);
resourcestring
  rsWarning = 'All orders will be cancelled! Continue?';
begin
  inherited;
  if (TMessageDialog.ShowQuestion(rsWarning) = mrYes) then
  begin
    IABClient.RequestGlobalCancel;
    TPublishers.LogPublisher.Write([ltLogWriter, ltLogView], ddWarning, 'OnGlobalCancel', 'All orders have been canceled');
  end;
end;

procedure TfrmDockFormActiveOrders.aGlobalCancelUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := IABClient.Connected;
end;

procedure TfrmDockFormActiveOrders.aPrintExecute(Sender: TObject);
begin
  frameActiveOrders.Print;
end;

procedure TfrmDockFormActiveOrders.aCancelAllOrdersExecute(Sender: TObject);
begin
  frameActiveOrders.CancelAllOrders;
end;

procedure TfrmDockFormActiveOrders.aCancelAllOrdersUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := not frameActiveOrders.vstTree.IsEmpty;
end;

procedure TfrmDockFormActiveOrders.aCancelOrderExecute(Sender: TObject);
begin
  frameActiveOrders.CancelOrder;
end;

procedure TfrmDockFormActiveOrders.aShowOrderExecute(Sender: TObject);
begin
  frameActiveOrders.ShowOrder;
end;

procedure TfrmDockFormActiveOrders.aShowOrderUpdate(Sender: TObject);
var
  Data: POrderData;
begin
  if Assigned(frameActiveOrders.vstTree.FocusedNode) then
  begin
    Data := frameActiveOrders.vstTree.FocusedNode^.GetData;
    TAction(Sender).Enabled := (Data.NodeType = ntNode) and (frameActiveOrders.vstTree.FocusedNode.CheckState = csCheckedNormal);
  end
  else
    TAction(Sender).Enabled := False;
end;


end.
