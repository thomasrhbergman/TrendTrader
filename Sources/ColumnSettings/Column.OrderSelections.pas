unit Column.OrderSelections;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Column.CustomSelections, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, IABFunctions, IABSocketAPI,
  IABSocketAPI_const, Global.Types, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Common.Types,
  System.Actions, Vcl.ActnList, IABFunctions.Helpers;
{$ENDREGION}

type
  TfrmColumnSelections = class(TfrmCustomColumnSelections)
//  private const
//    TickTypeColumns: set of TIABTickType= [ttBidSize, ttBid, ttAsk, ttAskSize, ttLast, ttLastSize, ttHigh, ttLow, ttVolume, ttClose, ttLow13Week, ttHigh13Week, ttLow26Week, ttHigh26Week, ttLow52Week, ttHigh52Week];
  protected
    procedure LoadUniqueParams; override;
  public
    { Public declarations }
    class function ShowDialog: TModalResult;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

class function TfrmColumnSelections.ShowDialog: TModalResult;
begin
  with TfrmColumnSelections.Create(nil) do
  try
    LoadParamsFromXml;
    Result := ShowModal;
    if (Result = mrOk) then
      SaveParamsToXml;
  finally
    Free;
  end;
end;

constructor TfrmColumnSelections.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColumnSettings := TGeneral.C_SECTION_COLUMN_SETTINGS
end;

procedure TfrmColumnSelections.LoadUniqueParams;
begin
  inherited;
  for var TickType := Low(TIABTickType) to High(TIABTickType) do
    lbSrc.Items.Add(TickType.ToString);
end;

end.
