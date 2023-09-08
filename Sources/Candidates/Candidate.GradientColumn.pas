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
    pnlValues: TPanel;
    Label1: TLabel;
    edValue2: TNumberBox;
    edValue1: TNumberBox;
    Label2: TLabel;
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
  for CalcType in [ctCorridorWidth, ctGradientCalc, ctLastPosition] do
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
        cbCalcType.ItemIndex := cbCalcType.Items.IndexOf(FColumnsInfo.CalcColumn.CalculationType.ToString);
        seDuration.Value     := FColumnsInfo.CalcColumn.Duration;
        edValue1.Value       := FColumnsInfo.CalcColumn.Value1;
        edValue2.Value       := FColumnsInfo.CalcColumn.Value2;
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
  FColumnsInfo.CalcColumn.CalculationType := TCalculationType.FromString(cbCalcType.Text);
  FColumnsInfo.CalcColumn.Duration        := seDuration.Value;
  FColumnsInfo.CalcColumn.Value1          := edValue1.Value;
  FColumnsInfo.CalcColumn.Value2          := edValue2.Value;
  FColumnsInfo.Weight                     := edWeight.ValueFloat;
end;

procedure TfrmCandidateGradientColumn.OnChangeVisibility(Sender: TObject);
var CalculationType: TCalculationType;
begin
  CalculationType        := TCalculationType.FromString(cbCalcType.Text);
  pnlValues.Visible      := CalculationType in [ctCorridorWidth, ctGradientCalc, ctLastPosition];
  if CalculationType = ctLastPosition then
  begin
    edValue1.MinValue := 0;
    edValue1.MaxValue := 100;
    edValue2.MinValue := 0;
    edValue2.MaxValue := 100;
    edValue1.Value := edValue1.Value;
    edValue2.Value := edValue2.Value;
  end
  else
  begin
    edValue1.MinValue := 0;
    edValue1.MaxValue := 0;
    edValue2.MinValue := 0;
    edValue2.MaxValue := 0;
  end;
end;

end.
