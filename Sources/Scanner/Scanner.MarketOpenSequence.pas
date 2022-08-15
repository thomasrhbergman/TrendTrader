unit Scanner.MarketOpenSequence;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids, DaModule, System.Actions,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms,
  DaImages, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client;
{$ENDREGION}

type
  TfrmScannerMarketOpenSequence = class(TCustomForm)
    aCancel: TAction;
    ActionListMain: TActionList;
    aDelete: TAction;
    aOpen: TAction;
    btnCancel: TBitBtn;
    btnDelete: TBitBtn;
    btnOpen: TBitBtn;
    dsScan: TDataSource;
    grdScan: TDBGrid;
    pnlBottom: TPanel;
    fbqScan: TFDQuery;
    fbqScanID: TIntegerField;
    fbqScanNAME: TStringField;
    procedure aCancelExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aDeleteUpdate(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aOpenUpdate(Sender: TObject);
  public
    class function GetScanRecordID: Integer;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmScannerMarketOpenSequence.GetScanRecordID: Integer;
var
  frmScannerMarketOpenSequence: TfrmScannerMarketOpenSequence;
begin
  Result := -1;
  frmScannerMarketOpenSequence := TfrmScannerMarketOpenSequence.Create(nil);
  try
    frmScannerMarketOpenSequence.Initialize;
    if (frmScannerMarketOpenSequence.ShowModal = mrOk) and
       (not frmScannerMarketOpenSequence.fbqScan.IsEmpty) then
      Result := frmScannerMarketOpenSequence.fbqScan.FieldByName('ID').AsInteger;
  finally
    frmScannerMarketOpenSequence.Denitialize;
    FreeAndNil(frmScannerMarketOpenSequence);
  end;
end;

procedure TfrmScannerMarketOpenSequence.Initialize;
begin
  DMod.CheckConnect;
  fbqScan.Open;
end;

procedure TfrmScannerMarketOpenSequence.Denitialize;
begin
  fbqScan.Close;
end;

procedure TfrmScannerMarketOpenSequence.aDeleteExecute(Sender: TObject);
resourcestring
  C_SQL_DELETE = 'DELETE FROM SCAN_MARKET WHERE ID=';
var
  i: Integer;
begin
  for i := 0 to grdScan.SelectedRows.Count - 1 do
  begin
    fbqScan.GotoBookmark(grdScan.SelectedRows.Items[i]);
    DMod.ExecuteSQL(C_SQL_DELETE + fbqScan.FieldByName('ID').AsString);
  end;
  fbqScan.Close;
  fbqScan.Open;
end;

procedure TfrmScannerMarketOpenSequence.aOpenExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmScannerMarketOpenSequence.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmScannerMarketOpenSequence.aOpenUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not fbqScan.IsEmpty;
end;

procedure TfrmScannerMarketOpenSequence.aDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := grdScan.SelectedRows.Count > 0;
end;

end.
