unit DaModule.ExecuteScript;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Data.DB, System.IOUtils, DebugWriter, CustomForms, MessageDialog, Global.Types, Vcl.StdCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IBX.IBDatabase, Utils.LocalInformation, DaModule.Constants,
  Vcl.Buttons, Vcl.Dialogs, Vcl.ExtCtrls, DaImages, IBX.IBScript, DaModule.Resources, HtmlLib, System.Actions,
  Vcl.ActnList, Global.Resources, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.UI.Intf, FireDAC.Comp.ScriptCommands,
  FireDAC.Stan.Util, FireDAC.Comp.Script;
{$ENDREGION}

type
  TfrmExecuteScript = class(TCustomForm)
    ActionList: TActionList;
    aExecuteScript: TAction;
    aOpenScript: TAction;
    aSaveScript: TAction;
    btnExecuteScript: TBitBtn;
    btnOpenScript: TBitBtn;
    btnSaveScript: TBitBtn;
    FileSaveDialog: TFileSaveDialog;
    memLog: TMemo;
    memScript: TMemo;
    OpenDialog: TFileOpenDialog;
    pnlOptions: TPanel;
    splLog: TSplitter;
    FBQuery: TFDQuery;
    procedure aOpenScriptExecute(Sender: TObject);
    procedure aExecuteScriptExecute(Sender: TObject);
    procedure aExecuteScriptUpdate(Sender: TObject);
    procedure aSaveScriptExecute(Sender: TObject);
  private const
    C_IDENTITY_NAME = 'ExecuteScript';
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize;
    procedure Deinitialize;
    class function ShowDocument: TModalResult;
  end;

implementation

{$R *.dfm}

class function TfrmExecuteScript.ShowDocument: TModalResult;
begin
  with TfrmExecuteScript.Create(nil) do
    try
      Initialize;
      Result := ShowModal;
      if (Result = mrOk) then
        Deinitialize;
    finally
      Free;
    end;
end;

procedure TfrmExecuteScript.Initialize;
begin

end;

procedure TfrmExecuteScript.Deinitialize;
begin

end;

function TfrmExecuteScript.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmExecuteScript.aExecuteScriptExecute(Sender: TObject);
begin
  memLog.Clear;
  try
    FBQuery.SQL.Text    := memScript.Lines.Text;
    FBQuery.ExecSQL;
    memLog.Lines.Add(rsSuccessful);
    TMessageDialog.ShowInfo(rsSuccessful);
  except on E: Exception do
    memLog.Lines.Add(E.Message);
  end;
end;

procedure TfrmExecuteScript.aExecuteScriptUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not memScript.Lines.Text.IsEmpty;
end;

procedure TfrmExecuteScript.aOpenScriptExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    memLog.Clear;
    memScript.Lines.LoadFromFile(OpenDialog.FileName);
    FileSaveDialog.FileName := OpenDialog.FileName;
  end;
end;

procedure TfrmExecuteScript.aSaveScriptExecute(Sender: TObject);
begin
  if FileSaveDialog.Execute then
    memScript.Lines.SaveToFile(FileSaveDialog.FileName);
end;

end.
