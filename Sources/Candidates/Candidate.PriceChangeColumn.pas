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
    lblTimeWeight: TLabel;
    edTimeWeight: TNumberBox;
    lblWeight: TLabel;
    edWeight: TNumberBox;
    pnlLastTickType: TPanel;
    rbUp: TRadioButton;
    rbDown: TRadioButton;
    Panel1: TPanel;
    rbHigh: TRadioButton;
    rbLow: TRadioButton;
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
  if FColumnsInfo.PriceChangeColumn.LastTickType = lttUp then
    rbUp.Checked := true
  else
    rbDown.Checked := true;
  cbLastPrice.Checked                    := FColumnsInfo.PriceChangeColumn.LastPrice;
  if FColumnsInfo.PriceChangeColumn.LastPriceType = lptHigh then
    rbHigh.Checked := true
  else
    rbLow.Checked := true;
  seLastTickCount.Value                  := FColumnsInfo.PriceChangeColumn.LastTickCount;
  edTimeWeight.ValueFloat                := FColumnsInfo.PriceChangeColumn.Weight;
  edWeight.ValueFloat                    := FColumnsInfo.Weight;
end;

procedure TfrmCandidatePriceChangeColumn.Denitialize;
begin
  if rbUp.Checked then
    FColumnsInfo.PriceChangeColumn.LastTickType := lttUp
  else
    FColumnsInfo.PriceChangeColumn.LastTickType := lttDown;
  if rbHigh.Checked then
    FColumnsInfo.PriceChangeColumn.LastPriceType := lptHigh
  else
    FColumnsInfo.PriceChangeColumn.LastPriceType := lptLow;
  FColumnsInfo.PriceChangeColumn.LastPrice      := cbLastPrice.Checked;
  FColumnsInfo.PriceChangeColumn.LastTickCount  := seLastTickCount.Value;
  FColumnsInfo.PriceChangeColumn.Weight         := edTimeWeight.ValueFloat;
  FColumnsInfo.Weight                           := edWeight.ValueFloat;
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
