unit Edit.OrderGroup;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin, Document, System.Actions,
  Vcl.ActnList, VirtualTrees, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Monitor.Types, Common.Types,
  DaImages, Vcl.VirtualImage, Global.Resources, Vcl.NumberBox;
{$ENDREGION}

type
  TfrmEditOrderGroup = class(TFormDocument)
    ActionList            : TActionList;
    aSave                 : TAction;
    btnCancel             : TBitBtn;
    btnSave               : TBitBtn;
    edOrderGroupName      : TEdit;
    lblName               : TLabel;
    lblType               : TLabel;
    pnlBottom             : TPanel;
    rgType                : TRadioGroup;
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
  private
    FRecordId  : Integer;
    function GetKind: TOrderKind;
    function GetOrderGroupName: string;
    procedure SetKind(const Value: TOrderKind);
    procedure SetOrderGroupName(const Value: string);
  public
    procedure AssignFromDoc(const aDocument: TOrderGroupDoc);
    procedure AssignToDoc(var aDocument: TOrderGroupDoc);
    class function ShowDocument(aDocument: TOrderGroupDoc; const aUseInTemplate: Boolean = False): TModalResult;

    property Kind             : TOrderKind read GetKind           write SetKind;
    property RecordId         : Integer    read FRecordId         write FRecordId;
    property Name             : string     read GetOrderGroupName write SetOrderGroupName;
  end;

implementation

{$R *.dfm}

class function TfrmEditOrderGroup.ShowDocument(aDocument: TOrderGroupDoc; const aUseInTemplate: Boolean = False): TModalResult;
begin
  Result := mrCancel;
  if Assigned(aDocument) then
  begin
    with TfrmEditOrderGroup.Create(nil) do
    try
      AssignFromDoc(aDocument);
      if (ShowModal = mrOk) then
      begin
        Result := mrOk;
        AssignToDoc(aDocument);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmEditOrderGroup.aSaveExecute(Sender: TObject);
begin
  inherited;
  ModalResult := mrOk;
end;

procedure TfrmEditOrderGroup.aSaveUpdate(Sender: TObject);
var
  Data: PTreeData;
begin
  inherited;
  if Assigned(OwnerNode) then
  begin
    Data := OwnerNode^.GetData;
    TAction(Sender).Enabled := Assigned(Data) and (Data.CreationType = ctUser);
  end
  else
    TAction(Sender).Enabled := False;
end;

function TfrmEditOrderGroup.GetOrderGroupName: string;
begin
  Result := edOrderGroupName.Text;
end;

function TfrmEditOrderGroup.GetKind: TOrderKind;
begin
  Result := TOrderKind(rgType.ItemIndex);
end;

procedure TfrmEditOrderGroup.SetKind(const Value: TOrderKind);
begin
  rgType.ItemIndex := Ord(Value);
end;

procedure TfrmEditOrderGroup.SetOrderGroupName(const Value: string);
begin
  edOrderGroupName.Text := Value;
end;

procedure TfrmEditOrderGroup.AssignFromDoc(const aDocument: TOrderGroupDoc);
begin
  if Assigned(aDocument) then
  begin
    Self.Kind             := aDocument.Kind;
    Self.RecordId         := aDocument.RecordId;
    Self.Name             := aDocument.Name;
    Self.OwnerNode        := aDocument.OwnerNode;
  end;
end;

procedure TfrmEditOrderGroup.AssignToDoc(var aDocument: TOrderGroupDoc);
begin
  if Assigned(aDocument) then
  begin
    aDocument.Kind             := Self.Kind;
    aDocument.RecordId         := Self.RecordId;
    aDocument.Name             := Self.Name;
  end;
end;

end.
