unit Edit.OrderGroupSet;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, DaModule, Vcl.ComCtrls, IABFunctions, IABSocketAPI,
  Vcl.ExtCtrls, Vcl.Samples.Spin, Document, System.Actions, Vcl.ActnList, VirtualTrees, BrokerHelperAbstr,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Monitor.Types, Common.Types, DaImages,
  Vcl.Imaging.pngimage, Vcl.VirtualImage, Global.Resources;
{$ENDREGION}

type
  TfrmEditOrderGroupSet = class(TCustomForm)
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    edtName: TEdit;
    imgWarning: TVirtualImage;
    lblInfo: TLabel;
    lblName: TLabel;
    pnlBottom: TPanel;
    pnlInfo: TPanel;
    pnlTypeUse: TPanel;
    rbAutoOrderTemplate: TRadioButton;
    rbInstrumentTemplate: TRadioButton;
  private
    FOrderGroupSetDoc: TOrderGroupSetDoc;
    procedure Initialize(aUseInTemplate: Boolean);
    procedure Deinitialize;
  public
    class function ShowDocument(var aOrderGroupSet: TOrderGroupSetDoc; const aUseInTemplate: Boolean = False): TModalResult;
  end;

implementation

{$R *.dfm}

{ TfrmEditOrderGroupSet }

class function TfrmEditOrderGroupSet.ShowDocument(var aOrderGroupSet: TOrderGroupSetDoc; const aUseInTemplate: Boolean = False): TModalResult;
begin
  with TfrmEditOrderGroupSet.Create(nil) do
    try
      Result := mrCancel;
      FOrderGroupSetDoc := aOrderGroupSet;
      Initialize(aUseInTemplate);
      ShowModal;
      if (ModalResult = mrOk) then
      begin
        Result := mrOk;
        Deinitialize;
        aOrderGroupSet := FOrderGroupSetDoc;
      end;
    finally
      Free;
    end;
end;

procedure TfrmEditOrderGroupSet.Initialize(aUseInTemplate: Boolean);
begin
  lblInfo.Caption              := rsChangingDocument;
  edtName.Text                 := FOrderGroupSetDoc.Name;
  pnlTypeUse.Visible           := aUseInTemplate;
  pnlInfo.Visible              := aUseInTemplate;
  rbAutoOrderTemplate.Checked  := Ord(FOrderGroupSetDoc.TypeUse) = 1;
  rbInstrumentTemplate.Checked := Ord(FOrderGroupSetDoc.TypeUse) = 2;
end;

procedure TfrmEditOrderGroupSet.Deinitialize;
begin
  FOrderGroupSetDoc.Name := edtName.Text;
  if rbAutoOrderTemplate.Checked then
    FOrderGroupSetDoc.TypeUse := tuTemplate
  else if rbInstrumentTemplate.Checked then
    FOrderGroupSetDoc.TypeUse := tuBaseOrder;
end;

end.
