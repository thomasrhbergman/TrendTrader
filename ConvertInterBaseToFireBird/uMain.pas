unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IBX.IBDatabase,
  Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait, FireDAC.Comp.Client,
  FireDAC.Phys.FBDef, FireDAC.Phys.IBBase, FireDAC.Phys.FB,
  IBX.IBCustomDataSet, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, System.StrUtils, Vcl.Buttons;

type
  TMain = class(TForm)
    labIB: TLabel;
    labFB: TLabel;
    edIB: TEdit;
    edFB: TEdit;
    btnCopy: TButton;
    labTable: TLabel;
    edFBServer: TEdit;
    labFBServer: TLabel;
    labPort: TLabel;
    edFBPort: TEdit;
    labFBFile: TLabel;
    labIBServer: TLabel;
    labIBPort: TLabel;
    edIBPort: TEdit;
    edIBServer: TEdit;
    labIBFile: TLabel;
    labIBUser: TLabel;
    labIBPassword: TLabel;
    labFBUser: TLabel;
    labFBPassword: TLabel;
    edIBUser: TEdit;
    edIBPassword: TEdit;
    edFBUser: TEdit;
    edFBPassword: TEdit;
    OpenIBDialog: TOpenDialog;
    btnDest: TSpeedButton;
    btnSource: TSpeedButton;
    OpenFBDialog: TOpenDialog;
    procedure btnCopyClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnSourceClick(Sender: TObject);
    procedure btnDestClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure OnMessage(const AMessage: String);
    procedure OnShowMessage(const AMessage: String);
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

uses
  uDM;

procedure TMain.btnCopyClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if (Components[I] is TEdit) or (Components[I] is TButton) then
      (Components[I] as TControl).Enabled := False;

  Application.ProcessMessages;

  try
    DM.CopyData(edIBServer.Text, edIBPort.Text, edIB.Text, edIBUser.Text, edIBPassword.Text,
                edFBServer.Text, edFBPort.Text, edFB.Text, edFBUser.Text, edFBPassword.Text, OnMessage, OnShowMessage);
  finally
    for I := 0 to ComponentCount - 1 do
      if (Components[I] is TEdit) or (Components[I] is TButton) then
        (Components[I] as TControl).Enabled := True;

    labTable.Caption := EmptyStr;
    Application.ProcessMessages;
    ShowMessage('Data copied');
  end;
end;

procedure TMain.btnDestClick(Sender: TObject);
begin
  if not String(edFB.Text).IsEmpty then
    OpenFBDialog.FileName := edFB.Text;
  if OpenFBDialog.Execute then
    edFB.Text := OpenIBDialog.FileName;
end;

procedure TMain.btnSourceClick(Sender: TObject);
begin
  if not String(edIB.Text).IsEmpty then
    OpenIBDialog.FileName := edIB.Text;
  if OpenIBDialog.Execute then
  begin
    edIB.Text := OpenIBDialog.FileName;
    edFB.Text := ChangeFileExt(edIB.Text, '.FDB');
  end;
end;

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := btnCopy.Enabled;
end;

procedure TMain.FormShow(Sender: TObject);
begin
  OpenIBDialog.InitialDir := ExtractFilePath(Application.ExeName);
  OpenFBDialog.InitialDir := OpenIBDialog.InitialDir;
end;

procedure TMain.OnMessage(const AMessage: String);
begin
  labTable.Caption := AMessage;
  Application.ProcessMessages;
end;

procedure TMain.OnShowMessage(const AMessage: String);
begin
  ShowMessage(AMessage);
end;

end.
