unit DockForm.QualifiersController;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons, Vcl.ExtCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, Document, System.Generics.Collections,
  System.Generics.Defaults, DebugWriter, Global.Types, System.IniFiles, IABSocketAPI_const, IABSocketAPI,
  System.Math, InstrumentList, Edit.OrderStatus, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ExtDlgs, BrokerHelperAbstr,
  Vcl.Printers, DaModule, Scanner.Types, MessageDialog, Qualifiers.Types,
  VirtualTrees.ExportHelper, Monitor.Interfaces, Common.Types, AutoTrades.Types, DaImages, IABFunctions, Monitor.Types,
  CustomDockForm, Qualifiers.Frame, Frame.Custom;
{$ENDREGION}

type
  TfrmDockFormQualifiersController = class(TfrmCustomDockForm)
    aDelete: TAction;
    aDeleteAll: TAction;
    aStart: TAction;
    aStop: TAction;
    btnDelete: TBitBtn;
    btnDeleteAll: TBitBtn;
    btnStart: TBitBtn;
    btnStop: TBitBtn;
    frameQualifiers: TframeQualifiers;
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aDeleteAllExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aStartExecute(Sender: TObject);
    procedure aStopExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private const
    C_IDENTITY_NAME = 'DockFormQualifiersControlle';
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure Deinitialize; override;
  end;

var
  frmDockFormQualifiersController: TfrmDockFormQualifiersController;

implementation

{$R *.dfm}

{ TfrmDockFormQualifiersController }

procedure TfrmDockFormQualifiersController.FormDestroy(Sender: TObject);
begin
  frmDockFormQualifiersController := nil;
  inherited;
end;

procedure TfrmDockFormQualifiersController.Initialize;
begin
  inherited Initialize;
  Caption := 'Qualifiers Controller';
  frameQualifiers.Initialize;
end;

procedure TfrmDockFormQualifiersController.Deinitialize;
begin
  inherited;
  frameQualifiers.Deinitialize;
end;

function TfrmDockFormQualifiersController.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmDockFormQualifiersController.aColumnSettingsExecute(Sender: TObject);
begin
  frameQualifiers.ColumnSettings;
end;

procedure TfrmDockFormQualifiersController.aPrintExecute(Sender: TObject);
begin
  frameQualifiers.Print;
end;

procedure TfrmDockFormQualifiersController.aStartExecute(Sender: TObject);
begin
  inherited;
  frameQualifiers.Start;
end;

procedure TfrmDockFormQualifiersController.aStopExecute(Sender: TObject);
begin
  inherited;
  frameQualifiers.Stop;
end;

procedure TfrmDockFormQualifiersController.aExportToCSVExecute(Sender: TObject);
begin
  frameQualifiers.ExportToCSV;
end;

procedure TfrmDockFormQualifiersController.aExportToExcelExecute(Sender: TObject);
begin
  frameQualifiers.ExportToExcel;
end;

procedure TfrmDockFormQualifiersController.aDeleteAllExecute(Sender: TObject);
begin
  inherited;
  frameQualifiers.DeleteAll;
end;

procedure TfrmDockFormQualifiersController.aDeleteExecute(Sender: TObject);
begin
  frameQualifiers.Delete;
end;

end.
