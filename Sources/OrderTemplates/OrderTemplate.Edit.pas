unit OrderTemplate.Edit;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin, System.Math, Vcl.DBCtrls,
  System.Generics.Collections, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABFunctions, IABSocketAPI,
  IABSocketAPI_const, OrderTemplate.Types, Vcl.ComCtrls, MessageDialog, DaModule, System.Actions, Vcl.ActnList,
  Search.Instruments,  VirtualTrees, Entity.Sokid, Data.DB, Scanner.Types, Monitor.Types, Document,
  System.DateUtils, BrokerHelperAbstr, Common.Types, DaImages, Global.Types, Vcl.Imaging.pngimage, Vcl.VirtualImage,
  Global.Resources, IABFunctions.Helpers, Vcl.NumberBox, Publishers.Interfaces, Publishers, InstrumentList,
  IABFunctions.MarketData, Utils;
{$ENDREGION}

type
  TfrmOrderTemplateEdit = class(TCustomForm)
    ActionListMain: TActionList;
    aSave: TAction;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    edtName: TEdit;
    lblName: TLabel;
    pnlBottom: TPanel;
    pnlTypeCondition: TPanel;
    procedure aSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FOrderTemplate: TOrderTemplate;
    function CheckData: Boolean;
  public
    class function ShowDocument(aOrderTemplate: TOrderTemplate; aDialogMode: TDialogMode): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmOrderTemplateEdit.ShowDocument(aOrderTemplate: TOrderTemplate; aDialogMode: TDialogMode): TModalResult;
begin
  with TfrmOrderTemplateEdit.Create(nil) do
  try
    DialogMode := aDialogMode;
    FOrderTemplate.AssignFrom(aOrderTemplate);
    Initialize;
    Result := ShowModal;
    if (Result = mrOk) then
    begin
      Denitialize;
      aOrderTemplate.AssignFrom(FOrderTemplate);
    end;
  finally
    Free;
  end;
end;

procedure TfrmOrderTemplateEdit.FormCreate(Sender: TObject);
begin
  FOrderTemplate := TOrderTemplate.Create;
end;

procedure TfrmOrderTemplateEdit.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FOrderTemplate);
end;

procedure TfrmOrderTemplateEdit.Initialize;
resourcestring
  rsCaption = '%s (v.%s)';
begin

  if FOrderTemplate.Name.IsEmpty then
    edtName.Text := 'Order Template'
  else
    edtName.Text := FOrderTemplate.Name;

  case DialogMode of
    dmInsert:
      Self.Caption := Format(rsCaption, ['New Qualifier', General.ModuleVersion]);
    dmUpdate:
      Self.Caption := Format(rsCaption, ['Edit Qualifier', General.ModuleVersion]);
  end;
end;

procedure TfrmOrderTemplateEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if (ModalResult = mrOk) then
  begin
    Denitialize;
    CanClose := CheckData;
  end;
end;

function TfrmOrderTemplateEdit.CheckData: Boolean;

  function CheckRequired: Boolean;
  var
    Problems: string;
  begin
    Problems := '';
    if FOrderTemplate.Name.IsEmpty then
    begin
      SetFocusSafely(edtName);
      Problems := Format(rcRequiredValue, ['Name']);
    end;

    Result := Problems.IsEmpty;
    if not Result then
      TMessageDialog.ShowWarning(Problems);
  end;

begin
  Result := CheckRequired;
end;

procedure TfrmOrderTemplateEdit.Denitialize;
begin
  FOrderTemplate.Name := edtName.Text;
end;

procedure TfrmOrderTemplateEdit.aSaveExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
