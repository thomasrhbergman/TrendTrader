unit Edit.Instrument;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids, DaModule,
  System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging,{$ENDIF}
  Vcl.ImgList, System.ImageList, Vcl.Menus, Vcl.Mask, System.Math, Vcl.DBCtrls, Winapi.ActiveX, System.UITypes,
  VirtualTrees, BrokerHelperAbstr,  DebugWriter, HtmlLib, Scanner.Types, CustomForms, MessageDialog, Vcl.Samples.Spin,
  Global.Types, Entity.Sokid, IABSocketAPI_const, DaImages, IABFunctions, IABFunctions.Helpers;
{$ENDREGION}

type
  TfrmEditInstrument = class(TCustomForm)
    aCancel: TAction;
    ActionList: TActionList;
    aSave: TAction;
    btnCancel: TBitBtn;
    btnOpen: TBitBtn;
    cbSecurityType: TComboBox;
    edContractID: TEdit;
    edCurrency: TEdit;
    edDescription: TEdit;
    edExchange: TEdit;
    edLocalSymbol: TEdit;
    edName: TEdit;
    edPrimaryExchange: TEdit;
    edSymbol: TEdit;
    lblContractID: TLabel;
    lblCurrency: TLabel;
    lblDecimals: TLabel;
    lblDescription: TLabel;
    lblExchange: TLabel;
    lblLocalSymbol: TLabel;
    lblName: TLabel;
    lblPrimaryExchange: TLabel;
    lblSecurityType: TLabel;
    lblSymbol: TLabel;
    pnlBottom: TPanel;
    seDecimals: TSpinEdit;
    seMultiplier: TSpinEdit;
    lblMultiplier: TLabel;
    procedure aCancelExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FConId: Integer;
    function CheckInstrument: Boolean;
    procedure LoadParamsFromXml;
    procedure SaveParamsToXml;
  public
    class function ShowDocument(aConId: Integer): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmEditInstrument.ShowDocument(aConId: Integer): TModalResult;
begin
  Result := mrCancel;
  if SokidList.ContainsKey(aConId) then
    with TfrmEditInstrument.Create(nil) do
    try
      FConId := aConId;
      Initialize;
      Result := ShowModal;
      if (Result = mrOk) then
        Denitialize;
    finally
      Free;
    end;
end;

procedure TfrmEditInstrument.Initialize;
var
  SokidInfo: TSokidInfo;
  st: TIABSecurityType;
begin
  LoadParamsFromXml;
  SokidInfo := SokidList.Items[FConId];
  edContractID.Text      := FConId.ToString;
  edSymbol.Text          := SokidInfo.Symbol;
  edLocalSymbol.Text     := SokidInfo.LocalSymbol;
  edName.Text            := SokidInfo.Name;
  edCurrency.Text        := SokidInfo.Currency;
  edExchange.Text        := SokidInfo.Exchange;
  seDecimals.Value       := SokidInfo.Decimals;
  edDescription.Text     := SokidInfo.Description;
  edPrimaryExchange.Text := SokidInfo.PrimaryExchange;
  seMultiplier.Text      := SokidInfo.Multiplier;

  cbSecurityType.Items.Clear;
  for st := stStock to stAll do
    cbSecurityType.Items.Add(st.ToString);
  cbSecurityType.ItemIndex := cbSecurityType.Items.IndexOf(SokidInfo.SecurityType);
end;

procedure TfrmEditInstrument.Denitialize;
var
  SokidInfo: TSokidInfo;
begin
  SaveParamsToXml;
  SokidInfo := SokidList.Items[FConId];
  SokidInfo.Symbol          := edSymbol.Text;
  SokidInfo.LocalSymbol     := edLocalSymbol.Text;
  SokidInfo.Name            := edName.Text;
  SokidInfo.Currency        := edCurrency.Text;
  SokidInfo.Exchange        := edExchange.Text;
  SokidInfo.Decimals        := seDecimals.Value;
  SokidInfo.Description     := edDescription.Text;
  SokidInfo.SecurityType    := cbSecurityType.Text;
  SokidInfo.PrimaryExchange := edPrimaryExchange.Text;
  SokidInfo.Check;
  SokidList.SetValue(SokidInfo);
end;

procedure TfrmEditInstrument.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if (ModalResult = mrOk) then
    CanClose := CheckInstrument;
end;

function TfrmEditInstrument.CheckInstrument: Boolean;
var
  Msg: string;
begin
  Msg := '';
  if string(edSymbol.Text).Trim.IsEmpty then
    Msg := Msg + 'Symbol is empty! ' + sLineBreak;
  if string(edName.Text).Trim.IsEmpty  then
    Msg := Msg + 'Name is empty! ' + sLineBreak;
  if string(edCurrency.Text).Trim.IsEmpty  then
    Msg := Msg + 'Currency is empty! ' + sLineBreak;
  if string(edExchange.Text).Trim.IsEmpty  then
    Msg := Msg + 'Exchange is empty! ' + sLineBreak;
  if string(cbSecurityType.Text).Trim.IsEmpty  then
    Msg := Msg + 'Security Type is empty! ' + sLineBreak;
  Result := Msg.IsEmpty;
  if not Result then
    TMessageDialog.ShowWarning(Msg);
end;

procedure TfrmEditInstrument.LoadParamsFromXml;
begin

end;

procedure TfrmEditInstrument.SaveParamsToXml;
begin

end;

procedure TfrmEditInstrument.aSaveExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmEditInstrument.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
