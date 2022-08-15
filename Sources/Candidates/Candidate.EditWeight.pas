unit Candidate.EditWeight;
 
interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin,
  System.Math, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, DaImages, Vcl.NumberBox;
{$ENDREGION}

type
  TfrmCandidateEditWeight = class(TCustomForm)
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    edtWeight: TNumberBox;
    lblWeight: TLabel;
    pnlBottom: TPanel;
  public
    class function ShowDocument(var aWeight: Double): TModalResult;
  end;

implementation

{$R *.dfm}

class function TfrmCandidateEditWeight.ShowDocument(var aWeight: Double): TModalResult;
var
  frmCandidaterEditWeight: TfrmCandidateEditWeight;
begin
  Result := mrCancel;
  frmCandidaterEditWeight := TfrmCandidateEditWeight.Create(nil);
  try
    frmCandidaterEditWeight.edtWeight.ValueFloat := aWeight;
    if (frmCandidaterEditWeight.ShowModal = mrOk) then
    begin
      aWeight := frmCandidaterEditWeight.edtWeight.ValueFloat;
      Result := mrOk;
    end;
  finally
    FreeAndNil(frmCandidaterEditWeight);
  end;
end;

end.
