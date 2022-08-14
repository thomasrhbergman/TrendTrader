unit Edit.SokidList;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, DaModule, Vcl.ComCtrls, IABFunctions, IABSocketAPI,
  Vcl.ExtCtrls, Vcl.Samples.Spin, Document, System.Actions, Vcl.ActnList, VirtualTrees, BrokerHelperAbstr, Search.Types,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Monitor.Types, MessageDialog, DaImages;
{$ENDREGION}

type
  TfrmEditSokidList = class(TCustomForm)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    edDescription: TEdit;
    edName: TEdit;
    lblDescription: TLabel;
    lblName: TLabel;
    pnlBottom: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function CheckInstrument: Boolean;
  public
    class function ShowDocument(var aSokidGroup: TSokidGroup): Boolean;
  end;

implementation

{$R *.dfm}

{ TfrmEditSokidList }

class function TfrmEditSokidList.ShowDocument(var aSokidGroup: TSokidGroup): Boolean;
begin
  with TfrmEditSokidList.Create(nil) do
    try
      Result := False;
      edName.Text := aSokidGroup.Name;
      edDescription.Text := aSokidGroup.Description;
      ShowModal;
      if (ModalResult = mrOk) then
      begin
        Result := True;
        aSokidGroup.Name := edName.Text;
        aSokidGroup.Description := edDescription.Text;
      end;
    finally
      Free;
    end;
end;

procedure TfrmEditSokidList.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if (ModalResult = mrOk) then
    CanClose := CheckInstrument;
end;

function TfrmEditSokidList.CheckInstrument: Boolean;
var
  Msg: string;
begin
  Msg := '';
  if string(edName.Text).Trim.IsEmpty  then
    Msg := Msg + 'Name is empty! ' + sLineBreak;
  Result := Msg.IsEmpty;
  if not Result then
    TMessageDialog.ShowWarning(Msg);
end;

end.
