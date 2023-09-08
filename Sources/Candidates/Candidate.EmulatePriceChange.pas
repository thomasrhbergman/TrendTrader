unit Candidate.EmulatePriceChange;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin, System.Math, Vcl.ComCtrls,
  System.Generics.Collections, Common.Types, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABFunctions,
  IABSocketAPI, IABSocketAPI_const, Candidate.Types, DaImages, Utils, Vcl.NumberBox, Entity.Sokid,
  InstrumentList, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, Publishers,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Grids, Vcl.DBGrids, DateUtils;
{$ENDREGION}

type
  TfrmCandidateEmulatePriceChange = class(TCustomForm)
    btnRun: TBitBtn;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    lblNumberOfTicks: TLabel;
    edNumberOfTicks: TNumberBox;
    rbUp: TRadioButton;
    rbDown: TRadioButton;
    edSeconds: TNumberBox;
    lblSeconds: TLabel;
    pnlInstrument: TPanel;
    lblInstrument: TLabel;
    lblLastPrice: TLabel;
    MemTable: TFDMemTable;
    MemTableTimeStamp: TDateTimeField;
    MemTablePrice: TCurrencyField;
    MemTableApplied: TBooleanField;
    dsMemTable: TDataSource;
    grPrices: TDBGrid;
    btnGeneratePrices: TBitBtn;
    TimerPriceChange: TTimer;
    cbLastPrice: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnGeneratePricesClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure TimerPriceChangeTimer(Sender: TObject);
    procedure grPricesDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure MemTableBeforePost(DataSet: TDataSet);
  private
    FInstrumentId: Integer;
    FLastPrice: currency;
    procedure SetLastPrice(const Value: currency);
    procedure SetControlsEnabled(AEnabled: boolean);
  public
    procedure Initialize;
    class procedure ShowDocument(aInstrumentId: Integer);
    property LastPrice: currency read FLastPrice write SetLastPrice;
  end;

implementation

{$R *.dfm}

procedure TfrmCandidateEmulatePriceChange.btnGeneratePricesClick(
  Sender: TObject);
const PRICE_INC = 0.1;
var LStartDate, LLastPriceTimeStamp: TDateTime;
    I, LSecondInc, LLastSecondInc: integer;
    LPrice: currency;
    PriceList: TPriceList;
    arrPrices: TArray<TPrice>;
begin
  inherited;
  if edNumberOfTicks.ValueInt <= 1 then
  begin
    ShowMessage('Number of ticks must be greater than 1');
    Exit;
  end;
  if edSeconds.ValueInt <= 1 then
  begin
    ShowMessage('Seconds number must be greater than 1');
    Exit;
  end;

  PriceList := TMonitorLists.PriceCache.GetPriceList(FInstrumentId);
  arrPrices := PriceList.GetLastPricesBroken(
                  function(const aPrice: TPrice): Boolean
                  begin
                    Result := (DateOf(aPrice.TimeStamp) = Date);
                  end);
  if Length(arrPrices) = 0 then
    LLastPriceTimeStamp := Date
  else
    LLastPriceTimeStamp := arrPrices[Length(arrPrices) - 1].TimeStamp;

  LStartDate := IncSecond(Now, -1 * edSeconds.ValueInt);
  if LStartDate < LLastPriceTimeStamp then
    LStartDate := LLastPriceTimeStamp;
  LPrice := LastPrice;
  if cbLastPrice.Checked then
  begin

    if Length(arrPrices) > 0 then
    begin
      LPrice := arrPrices[0].Value;
      for I := 1 to Length(arrPrices) - 1 do
        if (rbUp.Checked and (LPrice < arrPrices[I].Value))
           or
           (rbDown.Checked and (LPrice > arrPrices[I].Value)) then
          LPrice := arrPrices[I].Value;

    end;
    if rbUp.Checked then
      LPrice := LPrice - (edNumberOfTicks.ValueInt - 0) * PRICE_INC + PRICE_INC
    else
      LPrice := LPrice + (edNumberOfTicks.ValueInt - 0) * PRICE_INC - PRICE_INC;
  end;
  LSecondInc := Floor(edSeconds.ValueInt / (edNumberOfTicks.ValueInt - 1));
  LLastSecondInc := edSeconds.ValueInt - Floor(edSeconds.ValueInt / (edNumberOfTicks.ValueInt - 1)) * (edNumberOfTicks.ValueInt - 1);
  MemTable.Close;
  MemTable.Open;
  for I := 0 to edNumberOfTicks.ValueInt - 1 do
  begin
    LStartDate := IncSecond(LStartDate, LSecondInc);
    if I = edNumberOfTicks.ValueInt - 1 then
      LStartDate := IncSecond(LStartDate, LLastSecondInc);
    if rbUp.Checked then
      LPrice := LPrice + PRICE_INC
    else
      LPrice := LPrice - PRICE_INC;
    MemTable.Append;
    MemTable.FieldByName('TimeStamp').Value := LStartDate;
    MemTable.FieldByName('Price').Value := LPrice;
    MemTable.FieldByName('Applied').Value := false;
    MemTable.Post;
  end;
  MemTable.First;
end;

procedure TfrmCandidateEmulatePriceChange.btnRunClick(Sender: TObject);
begin
  inherited;
  if MemTable.RecordCount = 0 then
  begin
    ShowMessage('There is nothing to add');
    Exit;
  end;
  SetControlsEnabled(false);
  MemTable.First;
  TimerPriceChange.Enabled := true;
end;

procedure TfrmCandidateEmulatePriceChange.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;

procedure TfrmCandidateEmulatePriceChange.grPricesDrawColumnCell(
  Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
begin
  inherited;
  if MemTable.FieldByName('Applied').AsBoolean  then
    grPrices.Canvas.Brush.Color := clMoneyGreen;
  grPrices.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TfrmCandidateEmulatePriceChange.Initialize;
var PriceList: TPriceList;
    arrPrices: TArray<TPrice>;
begin
  if SokidList.ContainsKey(FInstrumentId) then
    lblInstrument.Caption := 'Instrument: ' + SokidList.Items[FInstrumentId].Symbol + ' ' + SokidList.Items[FInstrumentId].Name;

  PriceList := TMonitorLists.PriceCache.GetPriceList(FInstrumentId);
  arrPrices := PriceList.GetLastPricesBroken(
                  function(const aPrice: TPrice): Boolean
                  begin
                    Result := (DateOf(aPrice.TimeStamp) = Date);
                  end);
  if Length(arrPrices) = 0 then
    LastPrice := TMonitorLists.PriceCache.GetLastPrice(FInstrumentId, ttLast)
  else
    LastPrice := arrPrices[Length(arrPrices) - 1].Value;
end;

procedure TfrmCandidateEmulatePriceChange.MemTableBeforePost(DataSet: TDataSet);
begin
  inherited;
  if MemTable.FieldByName('Price').AsFloat < 0 then
    MemTable.FieldByName('Price').Value := 0.0001;
end;

procedure TfrmCandidateEmulatePriceChange.SetControlsEnabled(AEnabled: boolean);
begin
  edNumberOfTicks.Enabled := AEnabled;
  rbUp.Enabled := AEnabled;
  rbDown.Enabled := AEnabled;
  edSeconds.Enabled := AEnabled;
  grPrices.Enabled := AEnabled;
  btnGeneratePrices.Enabled := AEnabled;
  btnRun.Enabled := AEnabled;
end;

procedure TfrmCandidateEmulatePriceChange.SetLastPrice(const Value: currency);
begin
  FLastPrice := Value;
  lblLastPrice.Caption := 'Last Price: ' + FloatToStr(LastPrice);
end;

class procedure TfrmCandidateEmulatePriceChange.ShowDocument(aInstrumentId: Integer);
begin
  with TfrmCandidateEmulatePriceChange.Create(nil) do
  begin
    FInstrumentId := aInstrumentId;
    Initialize;
    Show;
  end;
end;

procedure TfrmCandidateEmulatePriceChange.TimerPriceChangeTimer(
  Sender: TObject);
begin
  inherited;
  TimerPriceChange.Enabled := false;
  if not MemTable.FieldByName('Applied').AsBoolean then
  begin
    TPublishers.FeedPublisher.UpdatePrice(FInstrumentId, ttLast, MemTable.FieldByName('Price').AsCurrency, MemTable.FieldByName('Timestamp').AsDateTime);
    MemTable.Edit;
    MemTable.FieldByName('Applied').AsBoolean := true;
    MemTable.Post;
    Application.ProcessMessages;
  end;

  MemTable.Next;
  if MemTable.Eof then
  begin
    SetControlsEnabled(true);
    MemTable.First;
  end
  else
    TimerPriceChange.Enabled := true;
end;

end.
