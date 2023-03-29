unit Candidate.TickColumn;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin,
  System.Math, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABFunctions, IABSocketAPI,
  IABSocketAPI_const, Candidate.Types, DaImages, Vcl.NumberBox, IABFunctions.Helpers, Common.Types;
{$ENDREGION}

type
  TfrmCandidateTickColumn = class(TCustomForm)
    btnAddColumn: TBitBtn;
    btnCancel: TBitBtn;
    cbIBValue1: TComboBox;
    cbIBValue2: TComboBox;
    cbTypeOperation: TComboBox;
    edWeight: TNumberBox;
    lblTickType: TLabel;
    lblWeight: TLabel;
    pnlBottom: TPanel;
    procedure cbTypeOperationChange(Sender: TObject);
  private
    FColumnsInfo: TColumnsInfo;
  public
    class function ShowDocument(aDialogMode: TDialogMode; var ColumnsInfo: TColumnsInfo): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmCandidateTickColumn.ShowDocument(aDialogMode: TDialogMode; var ColumnsInfo: TColumnsInfo): TModalResult;
begin
  Result := mrCancel;
  with TfrmCandidateTickColumn.Create(nil) do
  try
    DialogMode := aDialogMode;
    if (aDialogMode = dmInsert) then
      FColumnsInfo := TColumnsInfo.Create(stTickColumn, iabIdle)
    else
      FColumnsInfo := ColumnsInfo;
    Initialize;
    if (ShowModal = mrOk) and (aDialogMode in [dmInsert, dmUpdate]) then
    begin
      Denitialize;
      ColumnsInfo.AssignFrom(FColumnsInfo);
      Result := mrOk;
    end;
  finally
    Free;
  end;
end;

procedure TfrmCandidateTickColumn.Initialize;
var
  TickType: TIABTickType;
begin
  cbIBValue1.Items.Clear;
  for TickType := ttBidSize to ttFuturesOpenInterest do
    cbIBValue1.Items.Add(TickType.ToString);

  cbIBValue2.Items.Clear;
  for TickType := ttBidSize to ttNotSet do
    cbIBValue2.Items.Add(TickType.ToString);

  cbTypeOperation.Items.Clear;
  for var TypeOperation := Low(TTypeOperation) to High(TTypeOperation) do
    cbTypeOperation.Items.Add(TypeOperation.ToString);

  case DialogMode of
    dmInsert:
      begin
        Self.Caption              := 'Add Column';
        cbIBValue1.ItemIndex      := Integer(ttLast);
        cbIBValue2.ItemIndex      := Integer(ttClose);
        cbTypeOperation.ItemIndex := Ord(TTypeOperation.toDivide);
      end;
    dmUpdate:
      begin
        Self.Caption              := 'Edit Column Info';
        btnAddColumn.Enabled      := True;
        btnAddColumn.Caption      := 'Ok';
        cbIBValue1.ItemIndex      := Ord(FColumnsInfo.TickColumn.IBValue1);
        cbIBValue2.ItemIndex      := Ord(FColumnsInfo.TickColumn.IBValue2);
        cbTypeOperation.ItemIndex := Ord(FColumnsInfo.TickColumn.TypeOperation);
        edWeight.ValueFloat       := FColumnsInfo.Weight;
      end;
    dmView:
      begin
        Self.Caption              := 'View Column Info';
        btnAddColumn.Enabled      := False;
        cbIBValue1.ItemIndex      := Ord(FColumnsInfo.TickColumn.IBValue1);
        cbIBValue2.ItemIndex      := Ord(FColumnsInfo.TickColumn.IBValue2);
        cbTypeOperation.ItemIndex := Ord(FColumnsInfo.TickColumn.TypeOperation);
        edWeight.ValueFloat       := FColumnsInfo.Weight;
      end;
  end;

  cbIBValue2.Visible := cbTypeOperation.ItemIndex > 0;
end;

procedure TfrmCandidateTickColumn.cbTypeOperationChange(Sender: TObject);
begin
  inherited;
  cbIBValue2.Visible := cbTypeOperation.ItemIndex > 0;
  if cbTypeOperation.ItemIndex = 0 then
  begin
    edWeight.ValueFloat := 0;
    cbIBValue2.ItemIndex := 0;
  end
  else
    edWeight.ValueFloat := 1;
end;

procedure TfrmCandidateTickColumn.Denitialize;
begin
  FColumnsInfo.TickColumn.TypeOperation := TTypeOperation(cbTypeOperation.ItemIndex);
  FColumnsInfo.TickColumn.IBValue1      := TIABTickType(cbIBValue1.ItemIndex);
  FColumnsInfo.TickColumn.IBValue2      := TIABTickType(cbIBValue2.ItemIndex);
  FColumnsInfo.Weight                   := edWeight.ValueFloat;
end;

end.
