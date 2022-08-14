unit Edit.OrderStatus;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VirtualTrees, InstrumentList, Vcl.Buttons, Vcl.ExtCtrls, Data.Bind.EngExt,
  Vcl.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors, Data.Bind.Components, DaModule,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, DaImages, Monitor.Types, Document, IABSocketAPI_const,
  System.Actions, Vcl.ActnList, System.IOUtils, IABFunctions, IABFunctions.Helpers;
{$ENDREGION}

type
  TfrmEditOrderStatus = class(TCustomForm)
    ActionList: TActionList;
    aExportToXML: TAction;
    aImportFromXML: TAction;
    btnExportToCSV: TBitBtn;
    btnImportFromCSV: TBitBtn;
    btnOk: TBitBtn;
    FileSaveDialog: TFileSaveDialog;
    lblAutoTradesName: TLabel;
    lblAutoTradesNameCaption: TLabel;
    lblOrderAction: TLabel;
    lblOrderActionCaption: TLabel;
    lblQualifier: TLabel;
    lblQualifierCaption: TLabel;
    lblSecurityType: TLabel;
    lblSecurityTypeCaption: TLabel;
    lblSymbol: TLabel;
    lblSymbolCaption: TLabel;
    lbOrders: TListBox;
    lbStatus: TListBox;
    meInfo: TMemo;
    OpenDialog: TFileOpenDialog;
    pnlBottom: TPanel;
    pnlDetail: TPanel;
    pnlDetailTop: TPanel;
    pnlInfo: TPanel;
    pnlMain: TPanel;
    pnlOptions: TPanel;
    pnlOrders: TPanel;
    pnlOrdersTop: TPanel;
    pnlStatus: TPanel;
    pnlStatusTop: TPanel;
    splOrders: TSplitter;
    splStatus: TSplitter;
    procedure aExportToXMLExecute(Sender: TObject);
    procedure aImportFromXMLExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbOrdersClick(Sender: TObject);
    procedure lbStatusClick(Sender: TObject);
    procedure lbStatusDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  private
    FOrderItem: TOrderItem;
    FNode: PVirtualNode;
    procedure ImportFromXml(const aText: string);
    procedure Initialize(const aNode: PVirtualNode);
  public
    class function ShowDocument(const aNode: PVirtualNode): TModalResult; overload;
    class function ShowDocument(const aXmlText: string): TModalResult; overload;
  end;

implementation

{$R *.dfm}

{ TfrmEditOrderStatus }

class function TfrmEditOrderStatus.ShowDocument(const aNode: PVirtualNode): TModalResult;
begin
  with TfrmEditOrderStatus.Create(nil) do
    try
      Initialize(aNode);
      ShowModal;
      Result := ModalResult;
    finally
      Free;
    end;
end;

class function TfrmEditOrderStatus.ShowDocument(const aXmlText: string): TModalResult;
begin
  with TfrmEditOrderStatus.Create(nil) do
    try
      ImportFromXml(aXmlText);
      if (lbOrders.Items.Count > 0) then
      begin
        lbOrders.Selected[0] := True;
        lbOrdersClick(nil);
      end;
      ShowModal;
      Result := ModalResult;
    finally
      Free;
    end;
end;


procedure TfrmEditOrderStatus.FormCreate(Sender: TObject);
begin
  FNode := nil;
  FOrderItem := TOrderItem.Create;
end;

procedure TfrmEditOrderStatus.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FOrderItem);
end;

procedure TfrmEditOrderStatus.Initialize(const aNode: PVirtualNode);
resourcestring
  rsInfo = '%s (%d)';
  rsQualifiers = 'SELECT NAME FROM QUALIFIERS WHERE ID=%d';
  rsAutotrades = 'SELECT NAME FROM AUTOTRADES WHERE ID=%d';
  rsBaseTemplate = 'SELECT NAME FROM ORDER_GROUP_SET WHERE ID=%d';
var
  OrderItem : TOrderItem;
  NumOrderItem  : Integer;
  Data: PTreeData;
  Nam: string;
begin
  lbOrders.Clear;
  lbStatus.Clear;
  if Assigned(aNode) then
  begin
    FNode := aNode;
    Data := aNode^.GetData;
    if Assigned(Data^.OrderDoc) then
    begin
      lblSymbol.Caption      := Concat(Data^.OrderDoc.Symbol, ' (', Data^.OrderDoc.InstrumentName, ')');
      lblOrderAction.Caption := Data^.OrderDoc.OrderAction.ToString;
      case Data^.OrderDoc.OrderAction of
        iabBuy:
          lblOrderAction.Font.Color := clNavy;
        iabSell:
          lblOrderAction.Font.Color := clRed;
      end;
      if (Data^.OrderDoc is TOrderIBDoc) then
        lblSecurityType.Caption := TOrderIBDoc(Data^.OrderDoc).SecurityType.ToString;

      if (Data^.OrderDoc.QualifierID > 0) then
      begin
        Nam := DMod.GetStringValueFromSQL(Format(rsQualifiers, [Data^.OrderDoc.QualifierID]), 'NAME');
        lblQualifier.Caption := Format(rsInfo, [Nam, Data^.OrderDoc.QualifierID]);
      end;
      if (Data^.OrderDoc.AutoTradesID > 0) then
      begin
        Nam := DMod.GetStringValueFromSQL(Format(rsAutotrades, [Data^.OrderDoc.AutoTradesID]), 'NAME');
        lblAutoTradesName.Caption := Format(rsInfo, [Nam, Data^.OrderDoc.AutoTradesID]);
      end;
    end;
  end;

  if TMonitorLists.OrderList.ContainsKey(aNode) then
  begin
    OrderItem := TMonitorLists.OrderList.Items[aNode];
    for NumOrderItem in OrderItem.APIOrderList.Keys do
      lbOrders.AddItem(NumOrderItem.ToString, OrderItem.APIOrderList[NumOrderItem]);
  end;
  if (lbOrders.Count > 0) then
  begin
    lbOrders.Selected[0] := True;
    lbOrdersClick(nil);
  end;
end;

procedure TfrmEditOrderStatus.lbOrdersClick(Sender: TObject);
var
  APIOrderItem : TOrderItem.TAPIOrderItem;
  StatusItem: TOrderItem.TStatusItem;
begin
  lbStatus.Clear;
  for var i := 0 to lbOrders.Items.Count - 1 do
    if lbOrders.Selected[i] and Assigned(lbOrders.Items.Objects[i]) then
    begin
      APIOrderItem := lbOrders.Items.Objects[i] as TOrderItem.TAPIOrderItem;
      for StatusItem in APIOrderItem.StatusList do
        lbStatus.AddItem(StatusItem.Event, StatusItem);
      Break;
    end;
  if (lbStatus.Count > 0) then
  begin
    lbStatus.Selected[0] := True;
    lbStatusClick(nil);
  end;
end;

procedure TfrmEditOrderStatus.lbStatusClick(Sender: TObject);
var
  loStatusItem: TOrderItem.TStatusItem;
  i: Integer;
begin
  for i := 0 to lbStatus.Items.Count - 1 do
    if lbStatus.Selected[i] and Assigned(lbStatus.Items.Objects[i]) then
    begin
      loStatusItem := lbStatus.Items.Objects[i] as TOrderItem.TStatusItem;
      meInfo.Lines.Text := loStatusItem.Info.Replace(',', sLineBreak);
      Break;
    end;
end;

procedure TfrmEditOrderStatus.lbStatusDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ListBox: TListBox;
  Canvas: TCanvas;
begin
  ListBox := Control as TListBox;
  Canvas := ListBox.Canvas;
  Canvas.FillRect(Rect);
  if (ListBox.Items[Index].ToUpper.StartsWith('ERROR')) then
    Canvas.Font.Color := clRed
  else if (ListBox.Items[Index].ToUpper.StartsWith('ONEXECUTION')) then
    Canvas.Font.Color := clWebDarkgreen;

  Canvas.TextOut(Rect.Left, Rect.Top, ListBox.Items[Index]);
  if odFocused in State then
    Canvas.DrawFocusRect(Rect);
end;

procedure TfrmEditOrderStatus.aImportFromXMLExecute(Sender: TObject);
var
  XmlText: string;
begin
  inherited;
  if OpenDialog.Execute then
  begin
    XmlText := TFile.ReadAllText(OpenDialog.FileName);
    ImportFromXml(XmlText);
    if lbOrders.Items.Count > 0 then
    begin
      lbOrders.Selected[0] := True;
      lbOrdersClick(nil);
    end;
  end;
end;

procedure TfrmEditOrderStatus.ImportFromXml(const aText: string);
begin
  lbOrders.Clear;
  lbStatus.Clear;
  meInfo.Clear;
  FOrderItem.Clear;
  if not aText.IsEmpty then
  begin
    FOrderItem.FromXml(aText);
    for var APIOrderItem in FOrderItem.APIOrderList.Keys do
      lbOrders.AddItem(APIOrderItem.ToString, FOrderItem.APIOrderList[APIOrderItem]);
  end;
end;

procedure TfrmEditOrderStatus.aExportToXMLExecute(Sender: TObject);
var
  Data: PTreeData;
  OrderItem : TOrderItem;
  OrderId: Integer;
begin
  OrderItem := nil;
  OrderId := 0;
  if FileSaveDialog.Execute then
    if Assigned(FNode) then
    begin
      Data := FNode^.GetData;
      if (lbStatus.Items.Count > 0) then
        OrderId := StrToIntDef(lbStatus.Items[0], 0)
      else
        OrderId := 0;
      if TMonitorLists.OrderList.ContainsKey(FNode) then
        OrderItem := TMonitorLists.OrderList.Items[FNode];
      if Assigned(OrderItem) and Assigned(Data^.OrderDoc) then
        TFile.WriteAllText(FileSaveDialog.FileName, OrderItem.ToXml(Data^.OrderDoc.OrderIBId))
      else
        TFile.WriteAllText(FileSaveDialog.FileName, FOrderItem.ToXml(OrderId));
    end
    else
      TFile.WriteAllText(FileSaveDialog.FileName, FOrderItem.ToXml(OrderId));
end;

end.
