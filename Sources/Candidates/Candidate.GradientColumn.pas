unit Candidate.GradientColumn;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin,
  System.Math, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABFunctions, IABSocketAPI,
  IABSocketAPI_const, Candidate.Types, DaImages, Vcl.NumberBox, IABFunctions.Helpers, Common.Types;
{$ENDREGION}

type
  TfrmCandidateGradientColumn = class(TCustomForm)
    btnAddColumn: TBitBtn;
    btnCancel: TBitBtn;
    edWeight: TNumberBox;
    lblWeight: TLabel;
    pnlBottom: TPanel;
    cbCalcType: TComboBox;
    seDuration: TSpinEdit;
    lblDuration: TLabel;
    procedure OnChangeVisibility(Sender: TObject);
  private
    FColumnsInfo: TColumnsInfo;
  public
    class function ShowDocument(aDialogMode: TDialogMode; var ColumnsInfo: TColumnsInfo): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmCandidateGradientColumn.ShowDocument(aDialogMode: TDialogMode; var ColumnsInfo: TColumnsInfo): TModalResult;
begin
  Result := mrCancel;
  with TfrmCandidateGradientColumn.Create(nil) do
  try
    DialogMode := aDialogMode;
    if (aDialogMode = dmInsert) then
      FColumnsInfo := TColumnsInfo.Create(stCalcColumn, iabIdle)
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

procedure TfrmCandidateGradientColumn.Initialize;
var
  CalcType: TCalculationType;
begin
  cbCalcType.Items.Clear;
  for CalcType := Low(TCalculationType) to High(TCalculationType) do
    cbCalcType.Items.Add(CalcType.ToString);

  case DialogMode of
    dmInsert:
      begin
        Self.Caption         := 'Add Column';
        cbCalcType.ItemIndex := 0;
      end;
    dmUpdate:
      begin
        Self.Caption         := 'Edit Column Info';
        btnAddColumn.Enabled := True;
        btnAddColumn.Caption := 'Ok';
        cbCalcType.ItemIndex := Ord(FColumnsInfo.CalcColumn.CalculationType);
        seDuration.Value     := FColumnsInfo.CalcColumn.Duration;
        edWeight.ValueFloat  := FColumnsInfo.Weight;
      end;
    dmView:
      begin
        Self.Caption         := 'View Column Info';
        btnAddColumn.Enabled := False;
        cbCalcType.ItemIndex := Ord(FColumnsInfo.CalcColumn.CalculationType);
        seDuration.Value     := FColumnsInfo.CalcColumn.Duration;
        edWeight.ValueFloat  := FColumnsInfo.Weight;
      end;
  end;
  OnChangeVisibility(nil);
end;

procedure TfrmCandidateGradientColumn.Denitialize;
begin
  FColumnsInfo.CalcColumn.CalculationType := TCalculationType(cbCalcType.ItemIndex);
  FColumnsInfo.CalcColumn.Duration        := seDuration.Value;
  FColumnsInfo.Weight                     := edWeight.ValueFloat;
end;

procedure TfrmCandidateGradientColumn.OnChangeVisibility(Sender: TObject);
begin
  seDuration.Visible     := (cbCalcType.ItemIndex = Ord(ctGradientLogTerm)) or (cbCalcType.ItemIndex = Ord(ctGradientToday));
  lblDuration.Visible    := seDuration.Visible;
  if cbCalcType.ItemIndex = Ord(ctGradientLogTerm) then
    lblDuration.Caption := 'Weeks:'
  else if cbCalcType.ItemIndex = Ord(ctGradientToday) then
    lblDuration.Caption := 'Monitoring (sec):';
end;

end.
