unit CustomJournalForm;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, DaImages, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, System.UITypes, System.ImageList, Vcl.ImgList, Vcl.StdCtrls,
  Vcl.Printers, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Vcl.ExtCtrls, Vcl.Menus, Vcl.Buttons,
  VirtualTrees.ExportHelper, System.Actions, Vcl.ActnList, Monitor.Types, MonitorTree.Helper;
{$ENDREGION}

type
  TfrmCustomJournalForm = class(TCustomForm)
    aColumnSettings: TAction;
    ActionList: TActionList;
    aExportToCSV: TAction;
    aExportToExcel: TAction;
    aPrint: TAction;
    btnColumnSettings: TBitBtn;
    btnExportToCSV: TBitBtn;
    btnExportToExcel: TBitBtn;
    btnPrint: TBitBtn;
    pmTree: TPopupMenu;
    pnlMain: TPanel;
    pnlOptions: TPanel;
    vstTree: TVirtualStringTree;
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure vstTreeColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure vstTreeHeaderDragged(Sender: TVTHeader; Column: TColumnIndex; OldPosition: Integer);
    procedure vstTreeMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
  protected
    function GetIdentityName: string; override;
    function GetIdentityColumns: string; virtual;
  public
    procedure Initialize; virtual;
    procedure Deinitialize; virtual;
  end;

implementation

{$R *.dfm}

procedure TfrmCustomJournalForm.Initialize;
begin
  TMonitorTree.Initialize(vstTree);
  TStoreHelper.LoadFromXml(vstTree, GetIdentityColumns);
  LoadFormPosition;
end;

procedure TfrmCustomJournalForm.Deinitialize;
begin
  TStoreHelper.SaveToXml(vstTree, GetIdentityColumns);
//  SaveFormPosition;
end;

procedure TfrmCustomJournalForm.vstTreeColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  TStoreHelper.SaveToXml(vstTree, GetIdentityColumns);
end;

procedure TfrmCustomJournalForm.vstTreeHeaderDragged(Sender: TVTHeader; Column: TColumnIndex; OldPosition: Integer);
begin
  TStoreHelper.SaveToXml(vstTree, GetIdentityColumns);
end;

procedure TfrmCustomJournalForm.vstTreeMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  NodeHeight := 20;
end;

function TfrmCustomJournalForm.GetIdentityColumns: string;
begin
  Result := GetIdentityName + '.Columns';
end;

function TfrmCustomJournalForm.GetIdentityName: string;
begin
  Result := Self.Name;
end;

procedure TfrmCustomJournalForm.aColumnSettingsExecute(Sender: TObject);
begin
  if TStoreHelper.ShowColumnSettings(vstTree, GetIdentityName, 0) = mrOk then
    TStoreHelper.SaveToXml(vstTree, GetIdentityColumns);
end;

procedure TfrmCustomJournalForm.aExportToCSVExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToCSV(vstTree, Self.Caption);
end;

procedure TfrmCustomJournalForm.aExportToExcelExecute(Sender: TObject);
begin
  TExcelExportHelper.ExportToExcel(vstTree, Self.Caption);
end;

procedure TfrmCustomJournalForm.aPrintExecute(Sender: TObject);
begin
  Printer.Orientation := poLandscape;
  vstTree.Print(Printer, True);
end;

end.
