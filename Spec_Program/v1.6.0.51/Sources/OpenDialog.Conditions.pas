unit OpenDialog.Conditions;

interface

{$REGION 'Region uses'}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, 
  Vcl.Buttons, Vcl.DBGrids, DaModule, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, DaImages,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client;
{$ENDREGION}

type
  TfrmOpenCondition = class(TCustomForm)
    ActionListMain: TActionList;
    aDelete: TAction;
    aOpen: TAction;
    btnCancel: TBitBtn;
    btnDelete: TBitBtn;
    btnOpen: TBitBtn;
    dsConditions: TDataSource;
    grdConditions: TDBGrid;
    pnlBottom: TPanel;
    fbqConditions: TFDQuery;
    updConditions: TFDUpdateSQL;
    procedure aOpenUpdate(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure grdConditionsDblClick(Sender: TObject);
  public
    class function GetId: Integer;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmOpenCondition.GetId: Integer;
var
  frmOpenCondition: TfrmOpenCondition;
begin
  Result := -1;
  frmOpenCondition := TfrmOpenCondition.Create(nil);
  try
    frmOpenCondition.Initialize;
    if (frmOpenCondition.ShowModal = mrOk) and not(frmOpenCondition.fbqConditions.IsEmpty) then
      Result := frmOpenCondition.fbqConditions.FieldByName('ID').AsInteger;
  finally
    frmOpenCondition.Denitialize;
    FreeAndNil(frmOpenCondition);
  end;
end;

procedure TfrmOpenCondition.grdConditionsDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmOpenCondition.Initialize;
begin
  DMod.CheckConnect;
  fbqConditions.Open;
end;

procedure TfrmOpenCondition.Denitialize;
begin
  fbqConditions.Close;
end;

procedure TfrmOpenCondition.aDeleteExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to grdConditions.SelectedRows.Count - 1 do
  begin
    fbqConditions.GotoBookmark(grdConditions.SelectedRows.Items[i]);
    fbqConditions.Delete;
    fbqConditions.Transaction.CommitRetaining;
  end;
end;

procedure TfrmOpenCondition.aOpenUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := grdConditions.SelectedRows.Count <= 1;
end;

end.
