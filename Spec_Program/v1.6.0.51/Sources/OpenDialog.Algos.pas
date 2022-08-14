unit OpenDialog.Algos;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, 
  Vcl.Buttons, Vcl.DBGrids, DaModule, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, DaImages, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;
{$ENDREGION}

type
  TfrmOpenAlgos = class(TCustomForm)
    ActionListMain: TActionList;
    aOpen: TAction;
    btnCancel: TBitBtn;
    btnOpen: TBitBtn;
    dsAlgos: TDataSource;
    grdAlgos: TDBGrid;
    pnlBottom: TPanel;
    fbqAlgos: TFDQuery;
    procedure aOpenUpdate(Sender: TObject);
  public
    class function GetAlgosId: Integer;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmOpenAlgos.GetAlgosId: Integer;
var
  frmOpenAlgos: TfrmOpenAlgos;
begin
  Result := -1;
  frmOpenAlgos := TfrmOpenAlgos.Create(nil);
  try
    frmOpenAlgos.Initialize;
    if (frmOpenAlgos.ShowModal = mrOk) and not(frmOpenAlgos.fbqAlgos.IsEmpty) then
      Result := frmOpenAlgos.fbqAlgos.FieldByName('ID').AsInteger;
  finally
    frmOpenAlgos.Denitialize;
    FreeAndNil(frmOpenAlgos);
  end;
end;

procedure TfrmOpenAlgos.Initialize;
begin
  DMod.CheckConnect;
  fbqAlgos.Open;
end;

procedure TfrmOpenAlgos.Denitialize;
begin
  fbqAlgos.Close;
end;

procedure TfrmOpenAlgos.aOpenUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := grdAlgos.SelectedRows.Count <= 1;
end;

end.
