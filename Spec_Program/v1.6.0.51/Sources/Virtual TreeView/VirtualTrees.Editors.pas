unit VirtualTrees.Editors;

interface

{$REGION 'Region uses'}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, VirtualTrees, ExtDlgs,
  ImgList, Buttons, ExtCtrls, ComCtrls, Mask, Data.DB, Variants, System.UITypes, MessageDialog, Monitor.Types,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Scanner.Types, Vcl.Samples.Spin, System.Math, Document,
  Common.Types, Monitor.Interfaces, MonitorTree.Helper, MonitorTree.Document;
{$ENDREGION}

type
  TGridEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TWinControl;        // One of the property editor classes.
    FTree: TVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  TFloatEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FColumn: Integer;
    FDecimals: Integer;          // The column of the node being edited.
    FEdit: TEdit;        // One of the property editor classes.
    FNode: PVirtualNode;       // The node being edited.
    FTree: TVirtualStringTree; // A back reference to the tree calling.
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
  public
    destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(ATree: TBaseVirtualTree; ANode: PVirtualNode; AColumn: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
    property Column: Integer read FColumn write FColumn;
    property Decimals: Integer read FDecimals write FDecimals;
  end;

implementation

function TGridEditLink.BeginEdit: Boolean;
begin
  Result := True;
  FEdit.Show;
  FEdit.SetFocus;
end;

function TGridEditLink.CancelEdit: Boolean;
begin
  Result := True;
  FEdit.Hide;
  FTree.SetFocus;
end;

destructor TGridEditLink.Destroy;
begin
  if FEdit.HandleAllocated then
    PostMessage(FEdit.Handle, CM_RELEASE, 0, 0);
  inherited;
end;

procedure TGridEditLink.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CanAdvance: Boolean;
begin
  CanAdvance := true;
  case Key of
    VK_ESCAPE:
      begin
        Key := 0;//ESC will be handled in EditKeyUp()
      end;
    VK_RETURN:
      if CanAdvance then
      begin
        FTree.EndEditNode;
        Key := 0;
      end;
    VK_UP,
    VK_DOWN:
      begin
        CanAdvance := Shift = [];
        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

procedure TGridEditLink.EditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        FTree.CancelEditNode;
        Key := 0;
      end;
  end;
end;

function TGridEditLink.EndEdit: Boolean;
var
  Data: PTreeData;
  Buffer: array[0..1024] of Char;
  S: UnicodeString;
  V: Variant;
  FDataChanged : boolean;
  Monitor: IMonitor;

  function ValidData(AText: string; out AData: Variant): boolean;
  var
    FloatValue: Double;
  begin
    Result := True;
    AText := trim(AText);
    case FColumn of
      2 :
      begin
        if TryStrToFloat(AText, FloatValue) then
          AData := FloatValue
        else
          Exit(False);
      end
      else
        AData := AText;
    end;
  end;

begin
  FDataChanged := false;

  Data := FTree.GetNodeData(FNode);
  GetWindowText(FEdit.Handle, Buffer, 1024);
  S := Buffer;
  result := ValidData(S, V);
  if not Result then
  begin
    TMessageDialog.ShowError('Invalid value!');
    Exit(False);
  end;

  if FColumn = 2 then
    if Data.ReValue <> V then
    begin
      Data.ReValue := V;

      if Supports(Application.MainForm, IMonitor, Monitor) then
      begin
        if TTreeDocument.GetDocType(FTree, FNode.Parent) = ntAlgos then
          Monitor.UpdateAlgos(FNode.Parent)
        else if TTreeDocument.GetDocType(FTree, FNode.Parent) = ntCondition then
          Monitor.UpdateCondition(FNode.Parent);
      end;
      FDataChanged := true;
    end;

  if FDataChanged then
    FTree.InvalidateNode(FNode);
  FEdit.Hide;
  FTree.SetFocus;
end;

function TGridEditLink.GetBounds: TRect;
begin
  Result := FEdit.BoundsRect;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGridEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
var
  Data: PTreeData;
begin
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  freeAndNil(FEdit);
  Data := FTree.GetNodeData(Node);
  case FColumn of
    2:
      begin
        if Assigned(Data.FactorDoc) or Assigned(Data.AlgosDoc) then
        begin
          FEdit := TSpinEdit.Create(nil);
          with FEdit as TEdit do
          begin
            Visible := False;
            Parent := Tree;
            Text := VarToStr(Data.ReValue);
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
          end;
        end;
      end;
  else
    Result := False;
  end;
end;

procedure TGridEditLink.ProcessMessage(var Message: TMessage);
begin
  FEdit.WindowProc(Message);
end;

procedure TGridEditLink.SetBounds(R: TRect);
begin
  FTree.Header.Columns.GetColumnBounds(FColumn, R.Left, R.Right);
  FEdit.BoundsRect := R;
end;

{ TIntegerEditLink }

destructor TFloatEditLink.Destroy;
begin
  if Assigned(FEdit) then
    FreeAndNil(FEdit);
  inherited;
end;

function TFloatEditLink.BeginEdit: Boolean;
begin
  Result := True;
  FEdit.Show;
  FEdit.SetFocus;
end;

function TFloatEditLink.EndEdit: Boolean;
var
  Data: PInstrumentData;
  ColumnsItem: TExtraColumns.TColumnsItem;
begin
  Result := True;
  Data := FTree.GetNodeData(FNode);

  Data^.ExtraColumns.Items.TryGetValue(-1, ColumnsItem);
  if (FEdit.Text <> ColumnsItem.Rank.ToString) then
  begin
    ColumnsItem.Rank := SimpleRoundTo(StrToFloatDef(FEdit.Text, 0), -Decimals);
    Data^.ExtraColumns.Items.AddOrSetValue(-1, ColumnsItem);
    FTree.InvalidateNode(FNode);
  end;
  FEdit.Hide;
  FTree.SetFocus;
end;

function TFloatEditLink.CancelEdit: Boolean;
begin
  Result := True;
  FEdit.Hide;
end;

procedure TFloatEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        Key := 0;
      end;
    VK_RETURN:
      begin
        EndEdit;
        Key := 0;
      end;
    VK_UP, VK_DOWN:
      if (Shift = []) then
      begin
        PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
  end;
end;

procedure TFloatEditLink.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if not(CharInSet(Key, ['0' .. '9', #08, FormatSettings.DecimalSeparator])) then
    Key := #0;
end;

procedure TFloatEditLink.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        CancelEdit;
        Key := 0;
      end;//VK_ESCAPE
  end;//case
end;

function TFloatEditLink.PrepareEdit(ATree: TBaseVirtualTree; ANode: PVirtualNode; AColumn: TColumnIndex): Boolean;
var
  Data: PInstrumentData;
  ColumnsItem: TExtraColumns.TColumnsItem;
begin
  Result := True;
  FTree := ATree as TVirtualStringTree;
  FNode := ANode;
//  FColumn := Column;

  Data := FTree.GetNodeData(ANode);
  if not Assigned(FEdit) then
  begin
    FEdit := TEdit.Create(nil);
    FEdit.Visible := False;
    FEdit.Parent := ATree;
    FEdit.OnKeyDown := EditKeyDown;
    FEdit.OnKeyPress := EditKeyPress;
    FEdit.OnKeyUp := EditKeyUp;
  end;
  Data^.ExtraColumns.Items.TryGetValue(-1, ColumnsItem);
  FEdit.Text := SimpleRoundTo(ColumnsItem.Rank, -Decimals).ToString;
end;

procedure TFloatEditLink.ProcessMessage(var Message: TMessage);
begin
  FEdit.WindowProc(Message);
end;

procedure TFloatEditLink.SetBounds(R: TRect);
begin
  FTree.Header.Columns.GetColumnBounds(Column, R.Left, R.Right);
  FEdit.BoundsRect := R;
end;

function TFloatEditLink.GetBounds: TRect;
begin
  Result := FEdit.BoundsRect;
end;

end.
