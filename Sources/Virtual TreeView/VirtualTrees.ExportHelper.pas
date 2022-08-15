unit VirtualTrees.ExportHelper;

{$WARN UNSAFE_CODE OFF}
{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

interface

{$REGION 'Region uses'}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Win.ComObj, Winapi.ActiveX,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.IOUtils,
  VirtualTrees, VirtualTrees.Classes, Global.Types, XmlFiles, Column.Settings, IABSocketAPI_const, Column.Types,
  System.Generics.Collections, System.Generics.Defaults, Monitor.Types, Utils, System.Types, MonitorTree.Helper,
  MonitorTree.Document, VirtualTrees.Types;
{$ENDREGION}

type
  TExcelExportHelper = class
  private const
    ExcelApp = 'Excel.Application';
  public
    class var Excel: OleVariant;
    class function CheckExcelInstall: Boolean;
    class function CheckExcelRun: Boolean;
    class function CloseExcel: Boolean;
    class function RunExcel: Boolean;
    class function ContentToVariantArray(aTree: TVirtualStringTree): Variant;
    class procedure ExportToCSV(const aTree: TVirtualStringTree; const aFileName: string = '');
    class procedure ExportToExcel(const aTree: TVirtualStringTree; const aFileName: string = '');
    class procedure ExportToHTML(const aTree: TVirtualStringTree; const aFileName: string = '');
  end;

  TStoreHelper = class
  const
    C_ATTR_INDEX = 'Index';
    C_ATTR_NAME = 'Name';
    C_ATTR_POSITION = 'Position';
    C_ATTR_VISIBLE = 'Visible';
    C_ATTR_WIDTH = 'Width';
    C_ATTR_TAG = 'Tag';
  public
    class function GetColumnSettings(const aTree: TVirtualStringTree; const aIdentityName: string): string;
    class function ShowColumnSettings(const aTree: TVirtualStringTree; const aIdentityName: string;
      const aFixedColumn: Integer): TModalResult;
    class procedure LoadColumnSettings(const aTree: TVirtualStringTree; const aIdentityName: string;
      const aXmlText: string);
    class procedure LoadFromXml(const aTree: TVirtualStringTree; const aIdentityName: string);
    class procedure SaveToXml(const aTree: TVirtualStringTree; const aIdentityName: string);
  end;

implementation

class function TExcelExportHelper.CheckExcelInstall: Boolean;
var
  ClassID: TCLSID;
begin
  Result := CLSIDFromProgID(ExcelApp, ClassID) = S_OK;
end;

class function TExcelExportHelper.CheckExcelRun: Boolean;
begin
  try
    Excel := GetActiveOleObject(ExcelApp);
    Result := True;
  except
    Result := False;
  end;
end;

class function TExcelExportHelper.RunExcel: Boolean;
begin
  try
    if CheckExcelInstall then
    begin
      Excel := CreateOleObject(ExcelApp);
      Excel.Application.EnableEvents := True;
      Excel.Visible := True;
      Result := True;
    end
    else
      Result := False;
  except
    Result := False;
  end;
end;

class function TExcelExportHelper.CloseExcel: Boolean;
begin
  try
    if Excel.Visible then
      Excel.Visible := False;
    Excel.Quit;
    Excel := Unassigned;
    Result := True;
  except
    Result := False;
  end;
end;

class function TExcelExportHelper.ContentToVariantArray(aTree: TVirtualStringTree): Variant;
var
  ColumnCount: Integer;
  Columns: TColumnsArray;
  i: Integer;
  Node: PVirtualNode;
  nRow: Integer;
  RowCount: Integer;
  arr: TNodeArray;
begin
  Columns := aTree.Header.Columns.GetVisibleColumns;
  ColumnCount := Length(Columns);
  arr := TTreeDocument.GetNodesList(aTree.RootNode);
  RowCount := Length(arr) + 1; // +1 for the header
  Result := VarArrayCreate([0, RowCount - 1, 0, ColumnCount - 1], varOleStr);
  nRow := 0;
  for i := 0 to ColumnCount - 1 do
    Result[nRow, i] := aTree.Header.Columns.Items[Columns[i].Index].Text;

  for Node in arr do
    if Assigned(Node) and (Node <> aTree.RootNode) then
    begin
      Inc(nRow);
      for i := 0 to ColumnCount - 1 do
      begin
        if Utils.IsFloat(aTree.Text[Node, Columns[i].Index]) then
          Result[nRow, i] := aTree.Text[Node, Columns[i].Index].Replace('.', ',')
        else
          Result[nRow, i] := aTree.Text[Node, Columns[i].Index];
      end;
    end;
end;

class procedure TExcelExportHelper.ExportToCSV(const aTree: TVirtualStringTree; const aFileName: string);
var
  col: Integer;
  SaveDialog: TFileSaveDialog;
  str: string;
  TypeItem: TFileTypeItem;
  VArray: Variant;
begin
  if Assigned(aTree) then
  begin
    SaveDialog := TFileSaveDialog.Create(nil);
    try
      SaveDialog.FileTypes.Clear;
      SaveDialog.DefaultExtension := '*.csv';
      if not aFileName.IsEmpty then
        SaveDialog.FileName := aFileName + '.csv'
      else
        SaveDialog.FileName := aTree.Name + '.csv';
      TypeItem := SaveDialog.FileTypes.Add;
      TypeItem.DisplayName := 'CSV-file';
      TypeItem.FileMask := '*.csv';

      TypeItem := SaveDialog.FileTypes.Add;
      TypeItem.DisplayName := 'TXT-file';
      TypeItem.FileMask := '*.txt';

      TypeItem := SaveDialog.FileTypes.Add;
      TypeItem.DisplayName := 'All files';
      TypeItem.FileMask := '*.*';

      if SaveDialog.Execute then
      begin
        VArray := ContentToVariantArray(aTree);
        if not VarIsEmpty(VArray) and VarIsArray(VArray) then
        begin
          str := '';
          col := VarArrayDimCount(VArray);
          for var i := VarArrayLowBound(VArray, 1) to VarArrayHighBound(VArray, 1) do
          begin
            for var j := VarArrayLowBound(VArray, col) to VarArrayHighBound(VArray, col) do
              str := Concat(str, '"', VarToStr(VArray[i, j]), '",');
            str := Concat(str, sLineBreak);
          end;
          TFile.WriteAllText(SaveDialog.FileName, str);
        end;
      end;

    finally
      FreeAndNil(SaveDialog);
    end;
  end;
end;

class procedure TExcelExportHelper.ExportToExcel(const aTree: TVirtualStringTree; const aFileName: string);
var
  VArray: Variant;
  Sheet, Range, WorkBooks: OleVariant;
  SaveDialog : TFileSaveDialog;
  TypeItem   : TFileTypeItem;
begin
  if Assigned(aTree) then
  begin
    SaveDialog := TFileSaveDialog.Create(nil);
    try
      SaveDialog.FileTypes.Clear;
      SaveDialog.DefaultExtension := '*.xls';
      if not aFileName.IsEmpty then
        SaveDialog.FileName := aFileName + '.xls'
      else
        SaveDialog.FileName := aTree.Name + '.xls';
      TypeItem := SaveDialog.FileTypes.Add;
      TypeItem.DisplayName := 'XLS-file';
      TypeItem.FileMask := '*.xls';

      TypeItem := SaveDialog.FileTypes.Add;
      TypeItem.DisplayName := 'XLSX-file';
      TypeItem.FileMask := '*.xlsx';

      TypeItem := SaveDialog.FileTypes.Add;
      TypeItem.DisplayName := 'All files';
      TypeItem.FileMask := '*.*';
      if SaveDialog.Execute then
      begin
        VArray := ContentToVariantArray(aTree);
        if not VarIsEmpty(VArray) and VarIsArray(VArray) then
          if RunExcel then
          begin
            WorkBooks := Excel.WorkBooks.Add;
            Sheet := WorkBooks.WorkSheets[1];
            Range := Sheet.Range[Sheet.Cells[1, 1], Sheet.Cells[VarArrayHighBound(VArray, 1), VarArrayHighBound(VArray, 2)]];
            Range.Value := VArray;
            Range.EntireColumn.Autofit;
            WorkBooks.SaveAs(SaveDialog.FileName);
            Excel.Visible := True;
          end;
      end;
    finally
      FreeAndNil(SaveDialog);
    end;
  end;
end;

class procedure TExcelExportHelper.ExportToHTML(const aTree: TVirtualStringTree; const aFileName: string);
var
  SaveDialog : TFileSaveDialog;
  TypeItem   : TFileTypeItem;
  str        : string;
begin
  if Assigned(aTree) then
  begin
    SaveDialog := TFileSaveDialog.Create(nil);
    try
      SaveDialog.FileTypes.Clear;
      SaveDialog.DefaultExtension := '*.html';
      if not aFileName.IsEmpty then
        SaveDialog.FileName := aFileName + '.html'
      else
        SaveDialog.FileName := aTree.Name + '.html';
      TypeItem := SaveDialog.FileTypes.Add;
      TypeItem.DisplayName := 'html-file';
      TypeItem.FileMask := '*.html';

      TypeItem := SaveDialog.FileTypes.Add;
      TypeItem.DisplayName := 'All files';
      TypeItem.FileMask := '*.*';
      if SaveDialog.Execute then
      begin
        str := aTree.ContentToHTML(tstSelected);
        TFile.WriteAllText(SaveDialog.FileName, str);
      end;
    finally
      FreeAndNil(SaveDialog);
    end;
  end;
end;

{ TStoreHelper }

class procedure TStoreHelper.SaveToXml(const aTree: TVirtualStringTree; const aIdentityName: string);
var
  ColumnIndex: Integer;
  Column: TVirtualTreeColumn;
begin
  Column := nil;
  General.XMLFile.EraseSection(aIdentityName);
  General.XMLFile.CurrentSection := aIdentityName;
  try
    ColumnIndex := aTree.Header.Columns.GetFirstColumn;
    if (ColumnIndex > -1) then
      Column := aTree.Header.Columns[ColumnIndex];
    while Assigned(Column) do
    begin
      General.XMLFile.Attributes.AddNode;
      General.XMLFile.Attributes.SetAttributeValue(C_ATTR_INDEX, ColumnIndex);
      General.XMLFile.Attributes.SetAttributeValue(C_ATTR_POSITION, Column.Position);
      General.XMLFile.Attributes.SetAttributeValue(C_ATTR_NAME, Column.Text);
      General.XMLFile.Attributes.SetAttributeValue(C_ATTR_WIDTH, Column.Width);
      General.XMLFile.Attributes.SetAttributeValue(C_ATTR_VISIBLE, coVisible in Column.Options);
      General.XMLFile.Attributes.SetAttributeValue(C_ATTR_TAG, Column.Tag);
      General.XMLFile.WriteAttributes;
      ColumnIndex := aTree.Header.Columns.GetNextColumn(ColumnIndex);
      if (ColumnIndex > -1) then
        Column := aTree.Header.Columns[ColumnIndex]
      else
        Column := nil;
    end;
  finally
    General.XMLFile.CurrentSection := '';
    General.XMLFile.Save;
  end;
end;

class procedure TStoreHelper.LoadFromXml(const aTree: TVirtualStringTree; const aIdentityName: string);
var
  ColumnIndex: Integer;
  Column: TVirtualTreeColumn;
begin
  General.XMLFile.CurrentSection := aIdentityName;
  try
    while not General.XMLFile.IsLastKey do
    begin
      if General.XMLFile.ReadAttributes then
      begin
        ColumnIndex := General.XMLFile.Attributes.GetAttributeValue(C_ATTR_INDEX, -1);
        if (ColumnIndex > -1) and (ColumnIndex <= aTree.Header.Columns.Count - 1) then
        begin
          Column := aTree.Header.Columns[ColumnIndex];
          if Assigned(Column) then
          begin
            Column.Position := General.XMLFile.Attributes.GetAttributeValue(C_ATTR_POSITION, Column.Position);
            Column.Width    := General.XMLFile.Attributes.GetAttributeValue(C_ATTR_WIDTH, Column.Width);
            Column.Tag      := General.XMLFile.Attributes.GetAttributeValue(C_ATTR_TAG, Column.Tag);
            if StrToBool(General.XMLFile.Attributes.GetAttributeValue(C_ATTR_VISIBLE, True)) then
              Column.Options := Column.Options + [coVisible]
            else
              Column.Options := Column.Options - [coVisible];
          end;
        end;
        General.XMLFile.NextKey;
      end;
    end;
  finally
    General.XMLFile.CurrentSection := '';
  end;
end;

class function TStoreHelper.GetColumnSettings(const aTree: TVirtualStringTree; const aIdentityName: string): string;
var
  ColumnIndex: Integer;
  Column: TVirtualTreeColumn;
  XMLFile: TXMLFile;
begin
  XMLFile := TXMLFile.Create;
  try
    Column := nil;
    XMLFile.CurrentSection := aIdentityName;
    aTree.BeginUpdate;
    try
      ColumnIndex := aTree.Header.Columns.GetFirstColumn;
      if (ColumnIndex > -1) then
        Column := aTree.Header.Columns[ColumnIndex];
      while Assigned(Column) do
      begin
        XMLFile.Attributes.AddNode;
        XMLFile.Attributes.SetAttributeValue('ColumnIndex', ColumnIndex);
        XMLFile.Attributes.SetAttributeValue('ColumnPosition', Column.Position);
        XMLFile.Attributes.SetAttributeValue('ColumnWidth', Column.Width);
        XMLFile.WriteAttributes;
        ColumnIndex := aTree.Header.Columns.GetNextColumn(ColumnIndex);
        if (ColumnIndex > -1) then
          Column := aTree.Header.Columns[ColumnIndex]
        else
          Column := nil;
      end;
    finally
      aTree.EndUpdate;
    end;
    Result := XMLFile.XMLText;
  finally
    FreeAndNil(XMLFile);
  end;
end;

class procedure TStoreHelper.LoadColumnSettings(const aTree: TVirtualStringTree; const aIdentityName: string; const aXmlText: string);
var
  ColumnIndex: Integer;
  Column: TVirtualTreeColumn;
  XMLFile: TXMLFile;
begin
  XMLFile := TXMLFile.Create;
  aTree.BeginUpdate;
  try
    XMLFile.XMLText := aXmlText;
    XMLFile.CurrentSection := aIdentityName;
    while not XMLFile.IsLastKey do
    begin
      if XMLFile.ReadAttributes then
      begin
        ColumnIndex := XMLFile.Attributes.GetAttributeValue('ColumnIndex', -1);
        if (ColumnIndex > 0) and (ColumnIndex <= aTree.Header.Columns.Count - 1) then
        begin
          Column := aTree.Header.Columns[ColumnIndex];
          if Assigned(Column) then
          begin
            Column.Position := XMLFile.Attributes.GetAttributeValue('ColumnPosition', Column.Position);
            Column.Width    := XMLFile.Attributes.GetAttributeValue('ColumnWidth', Column.Width);
          end;
        end;
        XMLFile.NextKey;
      end;
    end;
  finally
    FreeAndNil(XMLFile);
    aTree.EndUpdate;
  end;
end;

class function TStoreHelper.ShowColumnSettings(const aTree: TVirtualStringTree; const aIdentityName: string; const aFixedColumn: Integer): TModalResult;
var
  ArrayColumns: TArrayColumns;
  ColumnIndex: Integer;
  Column: TVirtualTreeColumn;
begin
  Result := mrCancel;
  SetLength(ArrayColumns, 0);
  if (aTree.Header.Columns.Count - aFixedColumn > 0) then
  begin
    SetLength(ArrayColumns, aTree.Header.Columns.Count - aFixedColumn);
    for ColumnIndex := aFixedColumn to aTree.Header.Columns.Count - 1 do
    begin
      Column := aTree.Header.Columns[ColumnIndex];
      if Assigned(Column) then
      begin
        ArrayColumns[ColumnIndex - aFixedColumn].Name     := Column.Text;
        ArrayColumns[ColumnIndex - aFixedColumn].Position := Column.Position;
        ArrayColumns[ColumnIndex - aFixedColumn].Index    := Column.Index;
        ArrayColumns[ColumnIndex - aFixedColumn].Width    := Column.Width;
        ArrayColumns[ColumnIndex - aFixedColumn].TickType := TIABTickType(Column.Tag);
        ArrayColumns[ColumnIndex - aFixedColumn].Visible  := coVisible in Column.Options;
        if (Column.Tag > -1) then
          ArrayColumns[ColumnIndex - aFixedColumn].TickType := TIABTickType(Column.Tag);
      end;
    end;
  end;

  if (TfrmColumnSettings.ShowDocument(@ArrayColumns, aIdentityName) = mrOk) then
  begin
    Result := mrOk;
    TArray.Sort<TColumnSetting>(ArrayColumns, TComparer<TColumnSetting>.Construct(
      function(const Left, Right: TColumnSetting): Integer
      begin
        if (Left.Position > Right.Position) then
          Result := GreaterThanValue
        else if (Left.Position = Right.Position) then
          Result := EqualsValue
        else
          Result := LessThanValue;
      end));

    for var i := Low(ArrayColumns) to High(ArrayColumns) do
    begin
      ColumnIndex := ArrayColumns[i].Index;
      if (ColumnIndex <= aTree.Header.Columns.Count - 1) then
      begin
        Column := aTree.Header.Columns[ColumnIndex];
        if Assigned(Column) then
        begin
          Column.Position := ArrayColumns[i].Position;
          Column.Width    := ArrayColumns[i].Width;
          Column.Tag      := Integer(ArrayColumns[i].TickType);
          if ArrayColumns[i].Visible then
            Column.Options := Column.Options + [coVisible]
          else
            Column.Options := Column.Options - [coVisible];
        end;
      end;
    end;
  end;
end;

end.
