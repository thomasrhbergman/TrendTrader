unit Candidate.PriceChangeColumn;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin, System.Math, Vcl.ComCtrls,
  System.Generics.Collections, Common.Types, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABFunctions,
  IABSocketAPI, IABSocketAPI_const, Candidate.Types, DaImages, Utils, Vcl.NumberBox;
{$ENDREGION}

type
  TfrmCandidatePriceChangeColumn = class(TCustomForm)
    btnAddColumn: TBitBtn;
    btnCancel: TBitBtn;
    pnlBottom: TPanel;
    lblLastTicks: TLabel;
    pnlNumberTicks: TPanel;
    seLastTickCount: TSpinEdit;
    pnlLastPrice: TPanel;
    cbLastPrice: TCheckBox;
    pnlWeight: TPanel;
    lblWeight: TLabel;
    edWeight: TNumberBox;
    lblLastTickType: TLabel;
    lblLastPriceType: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FColumnsInfo: TColumnsInfo;
    function CheckData: Boolean;
  public
    class function ShowDocument(aDialogMode: TDialogMode; var ColumnsInfo: TColumnsInfo; ACandidate: TCandidate): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmCandidatePriceChangeColumn.ShowDocument(aDialogMode: TDialogMode; var ColumnsInfo: TColumnsInfo; ACandidate: TCandidate): TModalResult;
begin
  Result := mrCancel;
  with TfrmCandidatePriceChangeColumn.Create(nil) do
  try
    DialogMode := aDialogMode;
    if (aDialogMode = dmInsert) then
      FColumnsInfo := TColumnsInfo.Create(stPriceChangeColumn, ACandidate.MotherOrderAction)
    else
      FColumnsInfo := ColumnsInfo;
    Initialize;
    if (ShowModal = mrOk) and (aDialogMode in [dmInsert, dmUpdate]) then
    begin
      Denitialize;
      ColumnsInfo := FColumnsInfo;
      Result := mrOk;
    end;
  finally
    Free;
  end;
end;

procedure TfrmCandidatePriceChangeColumn.Initialize;
begin

  case DialogMode of
    dmInsert:
      begin
        Self.Caption                := 'Add PriceChange';

      end;
    dmUpdate, dmView:
      begin
        if (DialogMode = dmUpdate) then
        begin
          Self.Caption          := 'Edit PriceChange';
          btnAddColumn.Caption  := 'Save Column';
        end
        else
        begin
          Self.Caption          := 'View PriceChange Info';
          btnAddColumn.Enabled  := False;
        end;
      end;
  end;
  lblLastTickType.Caption                := FColumnsInfo.PriceChangeColumn.LastTickType.ToString;
  cbLastPrice.Checked                    := FColumnsInfo.PriceChangeColumn.LastPrice;
  lblLastPriceType.Caption               := FColumnsInfo.PriceChangeColumn.LastPriceType.ToString;
  seLastTickCount.Value                  := FColumnsInfo.PriceChangeColumn.LastTickCount;
  edWeight.ValueFloat                    := FColumnsInfo.PriceChangeColumn.Weight;
end;

procedure TfrmCandidatePriceChangeColumn.Denitialize;
begin
  FColumnsInfo.PriceChangeColumn.LastPrice := cbLastPrice.Checked;
  FColumnsInfo.PriceChangeColumn.LastTickCount := seLastTickCount.Value;
  FColumnsInfo.PriceChangeColumn.Weight := edWeight.ValueFloat;

  FColumnsInfo.Weight := 0;
end;

procedure TfrmCandidatePriceChangeColumn.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if (ModalResult = mrOk) then
    CanClose := CheckData;
end;

function TfrmCandidatePriceChangeColumn.CheckData: Boolean;
var
  Problems: string;
begin
  Problems := '';

  Result := Problems.IsEmpty;
  if not Result then
    ShowMessage(Problems);
end;

end.
