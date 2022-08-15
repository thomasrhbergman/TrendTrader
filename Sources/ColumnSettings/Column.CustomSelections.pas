unit Column.CustomSelections;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, VirtualTrees, Utils,
  Global.Types, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, DaImages, System.Actions,
  Vcl.ActnList, VirtualTrees.Types, Vcl.TitleBarCtrls;
{$ENDREGION}

type
  TfrmCustomColumnSelections = class(TCustomForm)
    ActionListMain: TActionList;
    aDown: TAction;
    aInsert: TAction;
    aRemove: TAction;
    aUp: TAction;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    cbShowAllFactors: TCheckBox;
    cbShowFreezedColumns: TCheckBox;
    lbDest: TListBox;
    lbSrc: TListBox;
    pnlBottom: TPanel;
    pnlCenter: TPanel;
    pnlLeft: TPanel;
    pnlMain: TPanel;
    pnlRight: TPanel;
    pnlTop: TPanel;
    spbDown: TSpeedButton;
    spbInsert: TSpeedButton;
    spbRemove: TSpeedButton;
    spbUp: TSpeedButton;
    procedure aDownExecute(Sender: TObject);
    procedure aInsertExecute(Sender: TObject);
    procedure aRemoveExecute(Sender: TObject);
    procedure aUpExecute(Sender: TObject);
    procedure lbDestDblClick(Sender: TObject);
    procedure lbSrcDblClick(Sender: TObject);
  protected
    FColumnSettings: string;
    procedure LoadUniqueParams; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; AColumnSettings: string); reintroduce; overload;
    function Execute(AHeader: TVTHeader): Boolean;
    procedure LoadParamsFromXml(AHeader: TVTHeader = nil);
    procedure SaveParamsToXml;
  end;

implementation

{$R *.dfm}

function TfrmCustomColumnSelections.Execute(AHeader: TVTHeader): Boolean;
var
  Column: TVirtualTreeColumn;
  i: Integer;
begin
  Result := False;
  lbSrc.Items.Clear;
  lbDest.Items.Clear;

  for i := 0 to AHeader.Columns.Count - 1 do
  begin
    Column := AHeader.Columns[i];
    if coVisible in Column.Options then
      lbDest.AddItem(Column.Text, TObject(Pointer(Column.Index)))
    else
      lbSrc.AddItem(Column.Text, TObject(Pointer(Column.Index)))
  end;

  if (ShowModal = mrOk) then
  begin
    Result := True;
    for i := 0 to lbSrc.Items.Count - 1 do
    begin
      Column := AHeader.Columns[Integer(lbSrc.Items.Objects[i])];
      if coVisible in Column.Options then
        Column.Options := Column.Options - [coVisible];
    end;
    for i := 0 to lbDest.Items.Count - 1 do
    begin
      Column := AHeader.Columns[Integer(lbDest.Items.Objects[i])];
      if not(coVisible in Column.Options) then
        Column.Options := Column.Options + [coVisible];
    end;
    SaveParamsToXml;
  end;
end;

constructor TfrmCustomColumnSelections.Create(AOwner: TComponent; AColumnSettings: string);
begin
  inherited Create(AOwner);
  FColumnSettings := AColumnSettings;
end;

procedure TfrmCustomColumnSelections.aDownExecute(Sender: TObject);
var
  i: Integer;
begin
  if lbDest.SelCount > 0 then
    if not lbDest.Selected[lbDest.Items.Count - 1] then
      for i := lbDest.Items.Count - 2 downto 0 do
        if lbDest.Selected[i] then
        begin
          lbDest.Items.Move(i, i + 1);
          lbDest.Selected[i + 1] := True;
        end;
end;

procedure TfrmCustomColumnSelections.aInsertExecute(Sender: TObject);
var
  i: Integer;
begin
  if lbSrc.SelCount > 0 then
    for i := lbSrc.Items.Count - 1 downto 0 do
      if lbSrc.Selected[i] then
      begin
        lbDest.AddItem(lbSrc.Items[i], lbSrc.Items.Objects[i]);
        lbSrc.Items.Delete(i);
      end;
  SetFocusSafely(lbSrc);
end;

procedure TfrmCustomColumnSelections.aRemoveExecute(Sender: TObject);
var
  i: Integer;
begin
  if lbDest.SelCount > 0 then
    for i := lbDest.Items.Count - 1 downto 0 do
      if lbDest.Selected[i] then
      begin
        lbSrc.AddItem(lbDest.Items[i], lbDest.Items.Objects[i]);
        lbDest.Items.Delete(i);
      end;
  SetFocusSafely(lbDest);
end;

procedure TfrmCustomColumnSelections.aUpExecute(Sender: TObject);
var
  i: Integer;
begin
  if lbDest.SelCount > 0 then
    if not lbDest.Selected[0] then
      for i := 1 to lbDest.Items.Count - 1 do
        if lbDest.Selected[i] then
        begin
          lbDest.Items.Move(i, i - 1);
          lbDest.Selected[i - 1] := True;
        end;
end;

procedure TfrmCustomColumnSelections.lbDestDblClick(Sender: TObject);
begin
  lbSrc.Items.Add(lbDest.Items[lbDest.ItemIndex]);
  lbDest.Items.Delete(lbDest.ItemIndex);
end;

procedure TfrmCustomColumnSelections.lbSrcDblClick(Sender: TObject);
begin
  lbDest.Items.Add(lbSrc.Items[lbSrc.ItemIndex]);
  lbSrc.Items.Delete(lbSrc.ItemIndex);
end;

procedure TfrmCustomColumnSelections.SaveParamsToXml;
var
  i: Integer;
begin
  try
    General.XmlFile.EraseSection(FColumnSettings);

    General.XmlFile.Attributes.Node := General.XmlFile.GetNode(General.XmlFile.GetXPath(FColumnSettings));
    General.XmlFile.Attributes.SetAttributeValue('ShowAllFactors', cbShowAllFactors.Checked);
    General.XmlFile.Attributes.SetAttributeValue('ShowFreezedColumns', cbShowFreezedColumns.Checked);
    General.XmlFile.WriteAttributes;

    General.XmlFile.CurrentSection := FColumnSettings;
    for i := 0 to lbDest.Items.Count - 1 do
    begin
      General.XmlFile.Attributes.AddNode;
      General.XmlFile.Attributes.SetAttributeValue('Column', lbDest.Items[i]);
      General.XmlFile.WriteAttributes;
    end;
  finally
    General.XmlFile.Save;
    General.XmlFile.CurrentSection := '';
  end;
end;

procedure TfrmCustomColumnSelections.LoadParamsFromXml(AHeader: TVTHeader);
var
  i, j: Integer;
  Column: string;
begin
  lbSrc.Items.Clear;
  lbDest.Items.Clear;
  if General.XmlFile.ReadAttributes(General.XmlFile.GetXPath(FColumnSettings)) then
  begin
    cbShowAllFactors.Checked := General.XmlFile.Attributes.GetAttributeValue('ShowAllFactors', False);
    cbShowFreezedColumns.Checked := General.XmlFile.Attributes.GetAttributeValue('ShowFreezedColumns', True);
  end;

  LoadUniqueParams;

  General.XmlFile.CurrentSection := FColumnSettings;
  while not General.XmlFile.IsLastKey do
  begin
    if General.XmlFile.ReadAttributes then
    begin
      Column := General.XmlFile.Attributes.GetAttributeValue('Column', '');
      if not Column.IsEmpty then
        lbDest.Items.Add(Column)
    end;
    General.XmlFile.NextKey;
  end;
  General.XmlFile.CurrentSection := '';

  for i := lbDest.Items.Count - 1 downto 0 do
    if lbDest.Items[i] = '' then
      lbDest.Items.Delete(i)
    else
    begin
      j := lbSrc.Items.IndexOf(lbDest.Items[i]);
      if j >= 0 then
        lbSrc.Items.Delete(j);
    end;
  if Assigned(AHeader) then
    for i := 0 to AHeader.Columns.Count - 1 do
      if lbDest.Items.IndexOf(AHeader.Columns[i].Text) = -1 then
        AHeader.Columns[i].Options := AHeader.Columns[i].Options - [coVisible]
end;

procedure TfrmCustomColumnSelections.LoadUniqueParams;
begin

end;

end.
