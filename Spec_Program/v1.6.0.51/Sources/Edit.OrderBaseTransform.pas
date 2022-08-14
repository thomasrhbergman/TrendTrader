unit Edit.OrderBaseTransform;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ExtCtrls, System.Generics.Collections,
  System.UITypes, System.Actions, Data.DB, System.Math, Vcl.ActnList, System.ImageList,
  Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Samples.Spin, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.Menus, System.DateUtils, Winapi.ActiveX, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  IABSocketAPI, IABFunctions, BrokerHelperAbstr, DaModule, XmlFiles, HtmlLib, CustomForms, DebugWriter,
  Global.Types, Document, Scanner.EditWeight, Utils, HtmlConsts, InformationDialog, Order.Utils,
  IABSocketAPI_const, InstrumentList, MessageDialog, DaImages, FireDAC.Comp.Client;
{$ENDREGION}

type
  TfrmOrderBaseTransform = class(TCustomForm)
    pnlTop: TPanel;
    cbBaseOrderTemplate: TComboBox;
    lblBaseOrderTemplate: TLabel;
    pnlBottom: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    lblSymbol: TLabel;
    lblSymbolInfo: TLabel;
    lblInstrumentInfo: TLabel;
    lblInstrument: TLabel;
    lblLastPriceInfo: TLabel;
    lblLastPrice: TLabel;
    edtQuantity: TSpinEdit;
    lblQuantity: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure FillConditionTemplate;
    function CheckData: Boolean;
  public
    class function ShowDocument(aDocument: TFactorDoc; out aQuantity: Integer): Integer;
  end;

implementation

{$R *.dfm}

class function TfrmOrderBaseTransform.ShowDocument(aDocument: TFactorDoc; out aQuantity: Integer): Integer;
var
  frmOrderBaseTransform: TfrmOrderBaseTransform;
begin
  Result := -1;
  if Assigned(aDocument) then
  begin
    frmOrderBaseTransform := TfrmOrderBaseTransform.Create(nil);
    with frmOrderBaseTransform do
    try
      FillConditionTemplate;
      lblSymbolInfo.Caption := aDocument.Symbol;
      lblInstrumentInfo.Caption := aDocument.InstrumentName;
      lblLastPriceInfo.Caption := CurrToStr(TMonitorLists.PriceCache.GetLastPrice(aDocument.ContractId, ttLast));
      if (frmOrderBaseTransform.ShowModal = mrOk) then
      begin
        if (cbBaseOrderTemplate.ItemIndex > -1) then
          Result := Integer(cbBaseOrderTemplate.Items.Objects[cbBaseOrderTemplate.ItemIndex]);
        aQuantity := edtQuantity.Value;
      end;
    finally
      FreeAndNil(frmOrderBaseTransform);
    end;
  end;
end;

procedure TfrmOrderBaseTransform.FillConditionTemplate;
resourcestring
  C_SQL_SELECT_TEXT = 'SELECT ID, NAME FROM ORDER_GROUP_SET WHERE TYPE_USE=2 ORDER BY NAME';
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    DMod.CheckConnect;
    Query.Connection := DMod.ConnectionStock;
    Query.SQL.Text := C_SQL_SELECT_TEXT;
    Query.Open;
    cbBaseOrderTemplate.Items.Clear;
    while not Query.Eof do
    begin
      cbBaseOrderTemplate.Items.AddObject(Query.FieldByName('NAME').AsString, TObject(Query.FieldByName('ID').AsInteger));
      Query.Next;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

function TfrmOrderBaseTransform.CheckData: Boolean;
resourcestring
  rcSelectBaseOrderTemplate = 'Please select "Base Order Template"' + sLineBreak;
  rcQuantityRequired        = 'Please set a value for the Quantity' + sLineBreak;
var
  Problems: string;
begin
  Problems := '';
  if (cbBaseOrderTemplate.ItemIndex = -1) then
    Problems := Problems + rcSelectBaseOrderTemplate;
  if (edtQuantity.Value <= 0) then
    Problems := Problems + rcQuantityRequired;
  Result := Problems.IsEmpty;
  if not Result then
    TMessageDialog.ShowWarning(Problems);
end;

procedure TfrmOrderBaseTransform.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if ModalResult = mrOk then
    CanClose := CheckData;
end;

end.
