unit Quantities.Edit;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin, System.Math, Vcl.DBCtrls,
  System.Generics.Collections, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABFunctions, IABSocketAPI,
  IABSocketAPI_const, Quantities.Types, Vcl.ComCtrls, MessageDialog, DaModule, System.Actions, Vcl.ActnList,
  Search.Instruments,  VirtualTrees, Entity.Sokid, Data.DB, Scanner.Types, Monitor.Types, Document,
  System.DateUtils, BrokerHelperAbstr, Common.Types, DaImages, Global.Types, Vcl.Imaging.pngimage, Vcl.VirtualImage,
  Global.Resources, IABFunctions.Helpers, Vcl.NumberBox, Publishers.Interfaces, Publishers, InstrumentList,
  IABFunctions.MarketData, Utils, ListForm;
{$ENDREGION}

type
  TfrmQuantityEdit = class(TCustomForm)
    ActionListMain: TActionList;
    aSave: TAction;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    edtName: TEdit;
    lblName: TLabel;
    pnlBottom: TPanel;
    pnlTypeCondition: TPanel;
    edTotalOrderAmount: TNumberBox;
    edSingleOrderAmount: TNumberBox;
    lblSingleOrderAmount: TLabel;
    lblTotalOrderAmount: TLabel;
    cbOrderCurrency: TComboBox;
    lblCurrency: TLabel;
    procedure aSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FQuantity: TQuantity;
    function CheckData: Boolean;
    procedure LoadParamsFromXml;
    procedure SaveParamsToXml;
  private const
    C_SECTION_SCANNER_MAIN     = 'ScannerMain';
    C_KEY_ORDER_CURRENCY       = 'OrderCurrency';
    C_KEY_ORDER_CURRENCY_LIST  = 'OrderCurrencyList';
  public
    class function ShowEditForm(aItem: TBaseClass; aDialogMode: TDialogMode): TModalResult; override;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmQuantityEdit.ShowEditForm(aItem: TBaseClass; aDialogMode: TDialogMode): TModalResult;
begin
  with TfrmQuantityEdit.Create(nil) do
  try
    DialogMode := aDialogMode;
    FQuantity.AssignFrom(TQuantity(aItem));
    Initialize;
    Result := ShowModal;
    SaveParamsToXml;
    if (Result = mrOk) then
    begin
      Denitialize;
      TQuantity(aItem).AssignFrom(FQuantity);
    end;
  finally
    Free;
  end;
end;

procedure TfrmQuantityEdit.FormCreate(Sender: TObject);
begin
  FQuantity := TQuantity.Create;
end;

procedure TfrmQuantityEdit.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FQuantity);
end;

procedure TfrmQuantityEdit.Initialize;
resourcestring
  rsCaption = '%s (v.%s)';
begin
  LoadParamsFromXml;

  if FQuantity.Name.IsEmpty then
    edtName.Text := 'Quantity'
  else
    edtName.Text := FQuantity.Name;
  edTotalOrderAmount.ValueInt := FQuantity.TotalOrderAmount;
  edSingleOrderAmount.ValueInt := FQuantity.OrderAmount;
  if not FQuantity.Currency.IsEmpty then
    cbOrderCurrency.Text := FQuantity.Currency;

  case DialogMode of
    dmInsert:
      Self.Caption := Format(rsCaption, ['New Quantity', General.ModuleVersion]);
    dmUpdate:
      Self.Caption := Format(rsCaption, ['Edit Quantity', General.ModuleVersion]);
  end;
end;

procedure TfrmQuantityEdit.SaveParamsToXml;
begin
  General.XMLFile.WriteString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY, cbOrderCurrency.Text);
  General.XMLFile.WriteString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY_LIST, cbOrderCurrency.Items.Text);
end;

procedure TfrmQuantityEdit.LoadParamsFromXml;
begin
  cbOrderCurrency.Items.Text := General.XMLFile.ReadString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY_LIST, C_DEFAULT_CURRENCY);
  cbOrderCurrency.Text       := General.XMLFile.ReadString(C_SECTION_SCANNER_MAIN, C_KEY_ORDER_CURRENCY, C_DEFAULT_CURRENCY);
end;

procedure TfrmQuantityEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if (ModalResult = mrOk) then
  begin
    Denitialize;
    CanClose := CheckData;
  end;
end;

function TfrmQuantityEdit.CheckData: Boolean;
resourcestring
  rsTotalOrderAmount = 'Total order amount is 0';

  function CheckRequired: Boolean;
  var
    Problems: string;
  begin
    Problems := '';
    if (FQuantity.TotalOrderAmount = 0) then
    begin
      SetFocusSafely(edSingleOrderAmount);
      Problems := Format(rcRequiredValue, ['Total order amount']);
    end;
    if (FQuantity.OrderAmount = 0) then
    begin
      SetFocusSafely(edSingleOrderAmount);
      Problems := Format(rcRequiredValue, ['Order amount']);
    end;
    if FQuantity.Currency.IsEmpty then
    begin
      SetFocusSafely(cbOrderCurrency);
      Problems := Format(rcRequiredValue, ['Order Currency']);
    end;


    Result := Problems.IsEmpty;
    if not Result then
      TMessageDialog.ShowWarning(Problems);
  end;

begin
  Result := CheckRequired;
end;

procedure TfrmQuantityEdit.Denitialize;
begin
  FQuantity.Name := edtName.Text;
  FQuantity.TotalOrderAmount := edTotalOrderAmount.ValueInt;
  FQuantity.OrderAmount := edSingleOrderAmount.ValueInt;
  FQuantity.Currency := cbOrderCurrency.Text;
end;

procedure TfrmQuantityEdit.aSaveExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

initialization
  ListFormFactory.RegisterList(ntQuantities, TQuantity, TfrmQuantityEdit);

end.
