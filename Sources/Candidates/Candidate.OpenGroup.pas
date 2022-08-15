unit Candidate.OpenGroup;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.ExtCtrls, System.Actions, Vcl.ActnList,
  VirtualTrees, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Document, DaImages;
{$ENDREGION}

type
  TfrmCandidateOpenGroup = class(TCustomForm)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    pnlBottom: TPanel;
    rgTemplateGroup: TRadioGroup;
  public
    class function GetGroup: Integer;
  end;

implementation

{$R *.dfm}

class function TfrmCandidateOpenGroup.GetGroup: Integer;
begin
  Result := -1;
  with TfrmCandidateOpenGroup.Create(nil) do
    try
      if (ShowModal = mrOk) then
        Result := rgTemplateGroup.ItemIndex;
    finally
      Free;
    end;
end;

end.
