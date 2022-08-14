unit CustomDockForm;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, CustomJournalForm, Vcl.Menus, System.Actions, Vcl.ActnList, VirtualTrees, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Monitor.Interfaces, DaImages;
{$ENDREGION}

type
  TfrmCustomDockForm = class(TfrmCustomJournalForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  public
  end;

var
  frmCustomDockForm: TfrmCustomDockForm;

implementation

{$R *.dfm}

procedure TfrmCustomDockForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Monitor: IMonitor;
begin
  inherited;
  CanClose := False;
  if Supports(Application.MainForm, IMonitor, Monitor) then
    Self.ManualDock(Monitor.GetDockControl, Monitor.GetDockControl, alClient);
end;

end.
