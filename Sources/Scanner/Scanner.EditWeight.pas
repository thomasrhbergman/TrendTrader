unit Scanner.EditWeight;
 
interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin,
  System.Math, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, DaImages, Vcl.NumberBox;
{$ENDREGION}

type
  TfrmScannerEditWeight = class(TCustomForm)
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

class function TfrmScannerEditWeight.ShowDocument(var aWeight: Double): TModalResult;
var
  frmScannerrEditWeight: TfrmScannerEditWeight;
begin
  Result := mrCancel;
  frmScannerrEditWeight := TfrmScannerEditWeight.Create(nil);
  try
    frmScannerrEditWeight.edtWeight.ValueFloat := aWeight;
    if (frmScannerrEditWeight.ShowModal = mrOk) then
    begin
      aWeight := frmScannerrEditWeight.edtWeight.ValueFloat;
      Result := mrOk;
    end;
  finally
    FreeAndNil(frmScannerrEditWeight);
  end;
end;

end.
