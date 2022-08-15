unit Scanner.AllFilters;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IABSocketAPI, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, Vcl.Samples.Spin,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} IABFunctions, DebugWriter, CustomForms, IABSocketAPI_const,
  DaImages;
{$ENDREGION}

type
  TfrmScannerAllFilters = class(TCustomForm)
    btnAddColumn: TBitBtn;
    btnCancel: TBitBtn;
    btnClear: TBitBtn;
    edtAbovePrice: TEdit;
    edtAboveVolume: TSpinEdit;
    edtAverageOptionVolumeAbove: TSpinEdit;
    edtBelowPrice: TEdit;
    edtCouponRateAbove: TEdit;
    edtCouponRateBelow: TEdit;
    edtExcludeConvertible: TSpinEdit;
    edtMarketCapAbove: TEdit;
    edtMarketCapBelow: TEdit;
    edtMaturityDateAbove: TEdit;
    edtMaturityDateBelow: TEdit;
    edtMoodyRatingAbove: TEdit;
    edtMoodyRatingBelow: TEdit;
    edtScannerSettingPairs: TEdit;
    edtSPRatingAbove: TEdit;
    edtSPRatingBelow: TEdit;
    edtStockTypeFilter: TEdit;
    lblAbovePrice: TLabel;
    lblAboveVolume: TLabel;
    lblAverageOptionVolumeAbove: TLabel;
    lblBelowPrice: TLabel;
    lblCouponRateAbove: TLabel;
    lblCouponRateBelow: TLabel;
    lblExcludeConvertible: TLabel;
    lblInfo: TLabel;
    lblMarketCapAbove: TLabel;
    lblMarketCapBelow: TLabel;
    lblMaturityDateAbove: TLabel;
    lblMaturityDateBelow: TLabel;
    lblMoodyRatingAbove: TLabel;
    lblMoodyRatingBelow: TLabel;
    lblScannerSettingPairs: TLabel;
    lblSPRatingAbove: TLabel;
    lblSPRatingBelow: TLabel;
    lblStockTypeFilter: TLabel;
    pnlBottom: TPanel;
    pnlMain: TGridPanel;
    procedure DoKeyPress(Sender: TObject; var Key: Char);
    procedure btnClearClick(Sender: TObject);
  private
    FScanCriteria: TIABScanCriteria;
  public
    class function ShowDocument(aScanCriteria: PTIABScanCriteria): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.DFM}

{ TfrmScannerAllFilters }

class function TfrmScannerAllFilters.ShowDocument(aScanCriteria: PTIABScanCriteria): TModalResult;
var
  frmScannerAllFilters: TfrmScannerAllFilters;
begin
  frmScannerAllFilters := TfrmScannerAllFilters.Create(nil);
  try
    frmScannerAllFilters.FScanCriteria := aScanCriteria^;
    frmScannerAllFilters.Initialize;
    Result := frmScannerAllFilters.ShowModal;
    if (Result = mrOk) then
    begin
      frmScannerAllFilters.Denitialize;
      aScanCriteria^ := frmScannerAllFilters.FScanCriteria;
    end;
  finally
    FreeAndNil(frmScannerAllFilters);
  end;
end;

procedure TfrmScannerAllFilters.Initialize;
begin
  if (FScanCriteria.AbovePrice <> UNSET_DOUBLE) and (FScanCriteria.AbovePrice <> 0) then
    edtAbovePrice.Text := FScanCriteria.AbovePrice.ToString;
  if (FScanCriteria.AboveVolume <> UNSET_INTEGER) and (FScanCriteria.AboveVolume <> 0) then
    edtAboveVolume.Value := FScanCriteria.AboveVolume;
  edtAverageOptionVolumeAbove.Value := FScanCriteria.AverageOptionVolumeAbove;
  if (FScanCriteria.BelowPrice <> UNSET_DOUBLE) and (FScanCriteria.BelowPrice <> 0) then
    edtBelowPrice.Text := FScanCriteria.BelowPrice.ToString;
  if (FScanCriteria.CouponRateAbove <> UNSET_DOUBLE) and (FScanCriteria.CouponRateAbove <> 0) then
    edtCouponRateAbove.Text := FScanCriteria.CouponRateAbove.ToString;
  if (FScanCriteria.CouponRateBelow <> UNSET_DOUBLE) and (FScanCriteria.CouponRateBelow <> 0) then
    edtCouponRateBelow.Text := FScanCriteria.CouponRateBelow.ToString;
  if (FScanCriteria.MarketCapAbove <> UNSET_DOUBLE) and (FScanCriteria.MarketCapAbove <> 0) then
    edtMarketCapAbove.Text := FScanCriteria.MarketCapAbove.ToString
  else
    edtMarketCapAbove.Text := '100';
  if (FScanCriteria.MarketCapBelow <> UNSET_DOUBLE) and (FScanCriteria.MarketCapBelow <> 0) then
    edtMarketCapBelow.Text := FScanCriteria.MarketCapBelow.ToString;

  edtExcludeConvertible.Value := FScanCriteria.ExcludeConvertible;
  edtMaturityDateAbove.Text   := FScanCriteria.MaturityDateAbove;
  edtMaturityDateBelow.Text   := FScanCriteria.MaturityDateBelow;
  edtMoodyRatingAbove.Text    := FScanCriteria.MoodyRatingAbove;
  edtMoodyRatingBelow.Text    := FScanCriteria.MoodyRatingBelow;
  edtScannerSettingPairs.Text := FScanCriteria.ScannerSettingPairs;
  edtSPRatingAbove.Text       := FScanCriteria.SPRatingAbove;
  edtSPRatingBelow.Text       := FScanCriteria.SPRatingBelow;
  edtStockTypeFilter.Text     := FScanCriteria.StockTypeFilter;
end;

procedure TfrmScannerAllFilters.btnClearClick(Sender: TObject);
begin
  edtAbovePrice.Text                := '';
  edtBelowPrice.Text                := '';
  edtAboveVolume.Value              := 0;
  edtMarketCapAbove.Text            := '';
  edtMarketCapBelow.Text            := '';
  edtCouponRateAbove.Text           := '';
  edtCouponRateBelow.Text           := '';
  edtAverageOptionVolumeAbove.Value := 0;
  edtExcludeConvertible.Value       := 0;
  edtMaturityDateAbove.Text         := '';
  edtMaturityDateBelow.Text         := '';
  edtMoodyRatingAbove.Text          := '';
  edtMoodyRatingBelow.Text          := '';
  edtScannerSettingPairs.Text       := '';
  edtSPRatingAbove.Text             := '';
  edtSPRatingBelow.Text             := '';
  edtStockTypeFilter.Text           := 'ALL';
end;

procedure TfrmScannerAllFilters.Denitialize;
begin
  FScanCriteria.AbovePrice               := UNSET_DOUBLE;
  FScanCriteria.BelowPrice               := UNSET_DOUBLE;
  FScanCriteria.AboveVolume              := UNSET_INTEGER;
  FScanCriteria.MarketCapAbove           := UNSET_DOUBLE;
  FScanCriteria.MarketCapBelow           := UNSET_DOUBLE;
  FScanCriteria.CouponRateAbove          := UNSET_DOUBLE;
  FScanCriteria.CouponRateBelow          := UNSET_DOUBLE;
  FScanCriteria.ExcludeConvertible       := 0;
  FScanCriteria.AverageOptionVolumeAbove := 0;

  FScanCriteria.Instrument          := '';
  FScanCriteria.LocationCode        := '';
  FScanCriteria.ScanCode            := '';
  FScanCriteria.MoodyRatingAbove    := '';
  FScanCriteria.MoodyRatingBelow    := '';
  FScanCriteria.SPRatingAbove       := '';
  FScanCriteria.SPRatingBelow       := '';
  FScanCriteria.MaturityDateAbove   := '';
  FScanCriteria.MaturityDateBelow   := '';
  FScanCriteria.ScannerSettingPairs := '';
  FScanCriteria.StockTypeFilter     := 'ALL';

  if (StrToIntDef(edtAbovePrice.Text, 0) <> 0) then
    FScanCriteria.AbovePrice := StrToFloatDef(edtAbovePrice.Text, UNSET_DOUBLE);
  if (edtAboveVolume.Value > 0) then
    FScanCriteria.AboveVolume := edtAboveVolume.Value;
  if (edtAverageOptionVolumeAbove.Value > 0) then
    FScanCriteria.AverageOptionVolumeAbove := edtAverageOptionVolumeAbove.Value;
  if (StrToIntDef(edtBelowPrice.Text, 0) <> 0) then
    FScanCriteria.BelowPrice := StrToFloatDef(edtBelowPrice.Text, UNSET_DOUBLE);
  if (StrToIntDef(edtCouponRateAbove.Text, 0) <> 0) then
    FScanCriteria.CouponRateAbove := StrToFloatDef(edtCouponRateAbove.Text, UNSET_DOUBLE);
  if (StrToIntDef(edtCouponRateBelow.Text, 0) <> 0) then
    FScanCriteria.CouponRateBelow := StrToFloatDef(edtCouponRateBelow.Text, UNSET_DOUBLE);
  if (StrToIntDef(edtMoodyRatingAbove.Text, 0) <> 0) then
    FScanCriteria.MoodyRatingAbove := edtMoodyRatingAbove.Text;
  if (StrToIntDef(edtMoodyRatingBelow.Text, 0) <> 0) then
    FScanCriteria.MoodyRatingBelow := edtMoodyRatingBelow.Text;
  if (edtExcludeConvertible.Value > 0) then
    FScanCriteria.ExcludeConvertible := edtExcludeConvertible.Value;
  if (edtMarketCapAbove.Text <> '') then
    FScanCriteria.MarketCapAbove := StrToFloatDef(edtMarketCapAbove.Text, UNSET_DOUBLE);
  if (edtMarketCapBelow.Text <> '') then
    FScanCriteria.MarketCapBelow := StrToFloatDef(edtMarketCapBelow.Text, UNSET_DOUBLE);
  if (edtMaturityDateAbove.Text <> '') then
    FScanCriteria.MaturityDateAbove := edtMaturityDateAbove.Text;
  if (edtMaturityDateBelow.Text <> '') then
    FScanCriteria.MaturityDateBelow := edtMaturityDateBelow.Text;
  if (edtScannerSettingPairs.Text <> '') then
    FScanCriteria.ScannerSettingPairs := edtScannerSettingPairs.Text;
  if (edtSPRatingAbove.Text <> '') then
    FScanCriteria.SPRatingAbove := edtSPRatingAbove.Text;
  if (edtSPRatingBelow.Text <> '') then
    FScanCriteria.SPRatingBelow := edtSPRatingBelow.Text;
  if (edtStockTypeFilter.Text <> '') then
    FScanCriteria.StockTypeFilter := edtStockTypeFilter.Text;
  FScanCriteria.NumberOfRows := 10;
end;

procedure TfrmScannerAllFilters.DoKeyPress(Sender: TObject; var Key: Char);
begin
  if not(CharInSet(Key, ['0' .. '9', #08, FormatSettings.DecimalSeparator])) then
    Key := #0;
end;



end.
