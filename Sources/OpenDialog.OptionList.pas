unit OpenDialog.OptionList;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids, DaModule, System.Actions,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms,
  Vcl.Menus, Edit.OrderGroupSet, System.StrUtils, Monitor.Types, DaImages,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client;
{$ENDREGION}

type
  TfrmOpenOptionList = class(TCustomForm)
    ActionListMain: TActionList;
    aDelete: TAction;
    aOpen: TAction;
    btnCancel: TBitBtn;
    btnDelete: TBitBtn;
    btnOpen: TBitBtn;
    dsOptionList: TDataSource;
    grOptionList: TDBGrid;
    miDelete: TMenuItem;
    miOpen: TMenuItem;
    pnlBottom: TPanel;
    PopupMenu: TPopupMenu;
    fbqOptionList: TFDQuery;
    procedure aDeleteExecute(Sender: TObject);
    procedure aDeleteUpdate(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aOpenUpdate(Sender: TObject);
  private
  public
    procedure Initialize;
    class function ShowDocument: Integer;
  end;

implementation

{$R *.dfm}

class function TfrmOpenOptionList.ShowDocument: Integer;
begin
  Result := -1;
  with TfrmOpenOptionList.Create(nil) do
  try
    Initialize;
    ShowModal;
    if (ModalResult = mrOk) then
      Result := fbqOptionList.FieldByName('ID').AsInteger;
  finally
    Free;
  end;
end;

procedure TfrmOpenOptionList.Initialize;
begin
  fbqOptionList.Open;
end;

procedure TfrmOpenOptionList.aDeleteExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to grOptionList.SelectedRows.Count - 1 do
  begin
    fbqOptionList.GotoBookmark(grOptionList.SelectedRows.Items[i]);
    fbqOptionList.Delete;
  end;
end;

procedure TfrmOpenOptionList.aOpenExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmOpenOptionList.aOpenUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := grOptionList.SelectedRows.Count <= 1;
end;

procedure TfrmOpenOptionList.aDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := grOptionList.SelectedRows.Count >= 1;
end;

end.
