unit Qualifiers.Edit;

interface

{$REGION 'Region uses'}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin, System.Math, Vcl.ActnList,
  System.Generics.Collections, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Qualifiers.Types,
  Vcl.ComCtrls, MessageDialog, DaModule, System.Actions, DaImages, Scanner.Types, Vcl.Menus, VirtualTrees, Document,
  Monitor.Types, Common.Types, Vcl.Printers, Global.Resources, MonitorTree.Document, Vcl.VirtualImage;
{$ENDREGION}

type
  TfrmQualifiersEdit = class(TCustomForm)
    ActionListMain: TActionList;
    aSaveQualifier: TAction;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    cbEnabled: TCheckBox;
    edName: TEdit;
    edNote: TMemo;
    imgWarning: TVirtualImage;
    lblInfo: TLabel;
    lblName: TLabel;
    lblNote: TLabel;
    pnlBottom: TPanel;
    pnlInfo: TPanel;
    pnlMain: TPanel;
    pnlName: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure aSaveQualifierExecute(Sender: TObject);
  private const
    C_IDENTITY_NAME = 'QualifiersEdit';
  private
    FQualifier: TQualifier;
    function CheckData: Boolean;
    procedure SaveQualifier;
  protected
    function GetIdentityName: string; override;
  public
    class function ShowDocument(const aNode: PVirtualNode): TModalResult;
    procedure Initialize;
    procedure Deinitialize;
  end;

implementation

{$R *.dfm}

class function TfrmQualifiersEdit.ShowDocument(const aNode: PVirtualNode): TModalResult;
var
  Data: PTreeData;
begin
  Result := mrCancel;
  if Assigned(aNode) then
  begin
    Data := aNode^.GetData;
    if (Data^.DocType = ntQualifier) then
      with TfrmQualifiersEdit.Create(nil) do
        try
          FQualifier.FromDB(Data^.RecordId);
          FQualifier.OwnerNode := aNode;
          Initialize;
          if (ShowModal = mrOk) then
          begin
            SaveQualifier;
            Deinitialize;
            Result := mrOk;
          end;
        finally
          Free;
        end;
  end;
end;

procedure TfrmQualifiersEdit.Initialize;
begin
  if FQualifier.Name.IsEmpty then
  begin
    FQualifier.Name    := 'Qualifier nr ' + FQualifier.RecordId.ToString;
    FQualifier.Enabled := True;
  end;
  edName.Text := FQualifier.Name;
  edNote.Text := FQualifier.Description;
  cbEnabled.Checked := FQualifier.Enabled;
  lblInfo.Caption := rsChangingDocument;
end;

procedure TfrmQualifiersEdit.Deinitialize;
begin
end;

procedure TfrmQualifiersEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (ModalResult = mrCancel) or CheckData;
end;

procedure TfrmQualifiersEdit.SaveQualifier;
var
  OwnerData: PTreeData;
begin
  FQualifier.Name := edName.Text;
  FQualifier.Description := edNote.Text;
  FQualifier.Enabled := cbEnabled.Checked;

  if not string(edName.Text).IsEmpty then
    FQualifier.Name := edName.Text;
  if Assigned(FQualifier.OwnerNode) then
  begin
    OwnerData := FQualifier.OwnerNode.GetData;
    OwnerData.Qualifier.Name        := FQualifier.Name;
    OwnerData.Qualifier.Description := FQualifier.Description;
    OwnerData.Qualifier.Enabled     := FQualifier.Enabled;
  end;
end;

procedure TfrmQualifiersEdit.aSaveQualifierExecute(Sender: TObject);
begin
  inherited;
  ModalResult := mrOk;
end;

function TfrmQualifiersEdit.CheckData: Boolean;
resourcestring
  C_SQL_CHECK_TEXT = 'SELECT COUNT(*) AS CNT FROM QUALIFIERS ' + sLineBreak + 'WHERE (NAME = ''%s'') AND (ID <> :ID);';
var
  Msg: string;
begin
  Msg := '';
  if FQualifier.Name.Trim.IsEmpty then
    Msg := Msg + 'Name is empty! ' + sLineBreak;
  if (DMod.GetIntegerValueFromSQL(Format(C_SQL_CHECK_TEXT, [FQualifier.Name.Trim, FQualifier.RecordId]), 'CNT') > 0)
  then
    Msg := Msg + 'Name is not unique! ' + sLineBreak;
  Result := Msg.IsEmpty;
  if not Result then
    TMessageDialog.ShowWarning(Msg);
end;

function TfrmQualifiersEdit.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

end.
