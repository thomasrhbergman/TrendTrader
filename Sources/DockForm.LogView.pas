unit DockForm.LogView;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons, Vcl.ExtCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, DebugWriter, System.IniFiles, CustomDockForm,
  Vcl.Menus, System.Actions, Vcl.ActnList, DaImages, Common.Types, Publishers.Interfaces, Global.Types, IABSocketAPI,
  IABSocketAPI_const, Publishers, System.Math, IABFunctions, IABFunctions.Helpers;
{$ENDREGION}

type
  PLogData = ^TLogData;
  TLogData = record
    Time       : TDateTime;
    LastPrice  : Single;
    Action     : string;
    DetailType : TLogDetailType;
    Info       : string;
    Method     : string;
    OrderID    : string;
    OrderType  : string;
    OCAGroup   : string;
    procedure Clear;
  end;

  TfrmLogView = class(TfrmCustomDockForm, ILogger)
    aClearLog: TAction;
    aFilter: TAction;
    aFilterClear: TAction;
    btnClearLog: TBitBtn;
    btnFilter: TBitBtn;
    btnFilterClear: TBitBtn;
    gbInfo: TGroupBox;
    lbEvents: TCheckListBox;
    lblOrderID: TLabel;
    memInfo: TMemo;
    pnlEvents: TPanel;
    pnlEventsTop: TPanel;
    pnlFilter: TPanel;
    seOrderID: TSpinEdit;
    splInfo: TSplitter;
    procedure aClearLogExecute(Sender: TObject);
    procedure aFilterClearExecute(Sender: TObject);
    procedure aFilterExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;var Result: Integer);
  private const
    COL_TIME       = 0;
    COL_TYPE       = 1;
    COL_ACTION     = 2;
    COL_METHOD     = 3;
    COL_ORDER_ID   = 4;
    COL_ORDER_TYPE = 5;
    COL_OCA_GROUP  = 6;
    COL_INFO       = 7;
    COL_LAST_PRICE = 8;
    C_IDENTITY_NAME = 'LogView';
  private
    FEventsList: THashedStringList;
    FCheckedList: THashedStringList;
    //implementation ILogger
    function GetInstance: TObject;
    function GetLogListenerType: TLogListenerType;
    procedure Write(const aDetailType: TLogDetailType; const aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aMethod, aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aMethod, aUnit, aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aObject: TObject; const aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aObject: TObject; const aMethod, aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aOrderID, aQuantity, aFilled: Integer; const aMethod, aSymbol, aInfo: string; const aStatus: TIABOrderState; const aAction: TIABAction; const aPrice: Double); overload;
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
  end;

var
  frmLogView: TfrmLogView;

implementation

{$R *.dfm}

{ TfrmLogView }

procedure TfrmLogView.FormCreate(Sender: TObject);
begin
  vstTree.NodeDataSize := SizeOf(TLogData);
  FEventsList := THashedStringList.Create;
  FEventsList.Duplicates := dupIgnore;
  FCheckedList := THashedStringList.Create;
  inherited;
end;

procedure TfrmLogView.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEventsList);
  FreeAndNil(FCheckedList);
  frmLogView := nil;
  inherited;
end;

function TfrmLogView.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

function TfrmLogView.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TfrmLogView.Initialize;
begin
  inherited Initialize;
  Caption := 'Log View';
  TPublishers.LogPublisher.Subscribe(Self);
end;

procedure TfrmLogView.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Deinitialize;
  TPublishers.LogPublisher.Unsubscribe(Self);
end;

procedure TfrmLogView.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PLogData;
begin
  inherited;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if (Data^.DetailType = ddError) then
      TargetCanvas.Font.Color := clRed
    else if (Data^.DetailType = ddWarning) then
      TargetCanvas.Font.Color := clMaroon;
    if (Column = COL_ACTION) then
    begin
      if Data.Action.StartsWith('SELL') then
        TargetCanvas.Font.Color := clRed
      else if Data.Action.StartsWith('BUY') then
        TargetCanvas.Font.Color := clBlue;
      TargetCanvas.Font.Style := [fsBold];
    end;
  end;
end;

procedure TfrmLogView.vstTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLogData;
begin
  inherited;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    memInfo.Lines.BeginUpdate;
    try
      memInfo.Lines.Text := Data^.Info;
    finally
      memInfo.Lines.EndUpdate;
    end;
  end;
end;

procedure TfrmLogView.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PLogData;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmLogView.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLogData;
begin
  inherited;
  Data := Sender.GetNodeData(Node);
  case Column of
    COL_TIME:
      CellText := FormatDateTime('hh:mm:ss.zzz', Data^.Time);
    COL_TYPE:
      CellText := Data^.DetailType.ToString;
    COL_METHOD:
      CellText := Data^.Method;
    COL_ACTION:
      CellText := Data^.Action;
    COL_INFO:
      CellText := Data^.Info;
    COL_ORDER_ID:
      CellText := Data^.OrderID;
    COL_ORDER_TYPE:
      CellText := Data^.OrderType;
    COL_OCA_GROUP:
      CellText := Data^.OCAGroup;
    COL_LAST_PRICE:
      if (Data^.LastPrice > 0) then
        CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.LastPrice)
      else
        CellText := '';
  end;
end;

procedure TfrmLogView.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PLogData;
  Data2: PLogData;
begin
  inherited;
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    COL_TIME:
      Result := CompareValue(Data1^.Time, Data2^.Time);
    COL_TYPE:
      Result := CompareText(Data1^.DetailType.ToString, Data2^.DetailType.ToString);
    COL_METHOD:
      Result := CompareText(Data1^.Method, Data2^.Method);
    COL_ACTION:
      Result := CompareText(Data1^.Action, Data2^.Action);
    COL_INFO:
      Result := CompareText(Data1^.Info, Data2^.Info);
    COL_ORDER_ID:
      Result := CompareValue(StrToIntDef(Data1^.OrderID, 0), StrToIntDef(Data2^.OrderID, 0));
    COL_ORDER_TYPE:
      Result := CompareText(Data1^.OrderType, Data2^.OrderID);
    COL_OCA_GROUP:
      Result := CompareValue(StrToIntDef(Data1^.OCAGroup, 0), StrToIntDef(Data2^.OCAGroup, 0));
    COL_LAST_PRICE:
      Result := CompareValue(Data1^.LastPrice, Data2^.LastPrice);
  end;
end;

procedure TfrmLogView.Write(const aDetailType: TLogDetailType; const aOrderID, aQuantity, aFilled: Integer; const aMethod, aSymbol, aInfo: string; const aStatus: TIABOrderState; const aAction: TIABAction; const aPrice: Double);
resourcestring
  rsInfo = 'OrderID=%d, LastPrice=%f, Action=%s, Quantity=%d, Filled=%d, Symbol=%s, Status=%s, %s';
var
  Info: string;
begin
  Info := Format(rsInfo, [aOrderID, aPrice, aAction.ToString, aQuantity, aFilled, aSymbol, aStatus.ToString, aInfo]);
  Write(aDetailType, aMethod, aInfo);
end;

function TfrmLogView.GetLogListenerType: TLogListenerType;
begin
  Result := ltLogView;
end;

procedure TfrmLogView.Write(const aDetailType: TLogDetailType; const aInfo: string);
begin
  Write(aDetailType, '', aInfo);
end;

procedure TfrmLogView.Write(const aDetailType: TLogDetailType; const aMethod, aUnit, aInfo: string);
begin
  Write(aDetailType, '', aInfo);
end;

procedure TfrmLogView.Write(const aDetailType: TLogDetailType; const aObject: TObject; const aInfo: string);
begin
  Write(aDetailType, '', aInfo);
end;

procedure TfrmLogView.Write(const aDetailType: TLogDetailType; const aObject: TObject; const aMethod, aInfo: string);
begin
  Write(aDetailType, '', aInfo);
end;

procedure TfrmLogView.Write(const aDetailType: TLogDetailType; const aMethod, aInfo: string);

  procedure RefreshEventList;
  begin
    with frmLogView do
      if (FEventsList.IndexOf(aMethod.Trim) = -1) then
      begin
        FEventsList.Add(aMethod.Trim);
        lbEvents.Items.BeginUpdate;
        lbEvents.Items.Add(aMethod.Trim);
        lbEvents.Items.EndUpdate;
      end;
  end;

begin
  if Assigned(frmLogView) then
  begin
    RefreshEventList;
    TThread.Queue(nil,
      procedure()
      var
        Data: PLogData;
        Node: PVirtualNode;
        stList: THashedStringList;
      begin
        with frmLogView do
        begin
          vstTree.BeginUpdate;
          try
            Node := vstTree.AddChild(nil);
            Data := Node^.GetData;
            Data^.Time := Now;
            Data^.DetailType := aDetailType;
            Data^.Info := aInfo.Trim;
            Data^.Method := aMethod.Trim;
            stList := THashedStringList.Create;
            try
              stList.CommaText := aInfo.Trim;
              vstTree.IsVisible[Node] := True;
              if (stList.IndexOfName('OrderID') > -1) then
              begin
                Data^.OrderID := stList.Values['OrderID'];
                if (seOrderID.Value > 0) then
                  vstTree.IsVisible[Node] := seOrderID.Value.ToString.Equals(Data^.OrderID);
              end;
              if (FCheckedList.Count > 0) and vstTree.IsVisible[Node] then
                if (FCheckedList.IndexOf(Data^.Method) = -1) then
                  vstTree.IsVisible[Node] := False;
              if (stList.IndexOfName('OrderType') > -1) then
                Data^.OrderType := stList.Values['OrderType'];
              if (stList.IndexOfName('LastPrice') > -1) then
                Data^.LastPrice := StrToFloat(stList.Values['LastPrice']);
              if (stList.IndexOfName('Action') > -1) then
                Data^.Action := stList.Values['Action'].Trim.ToUpper;
              if (stList.IndexOfName('OCAGroup') > -1) and (StrToIntDef(stList.Values['OCAGroup'], -1) > 0) then
                Data^.OCAGroup := stList.Values['OCAGroup'];
            finally
              FreeAndNil(stList);
            end;
            vstTree.InvalidateNode(Node);
          finally
            vstTree.EndUpdate;
          end;
        end;
      end);
  end;
end;

procedure TfrmLogView.aClearLogExecute(Sender: TObject);
begin
  inherited;
  vstTree.BeginUpdate;
  try
    vstTree.Clear;
  finally
    vstTree.EndUpdate;
  end;

  memInfo.Clear;
end;

procedure TfrmLogView.aFilterClearExecute(Sender: TObject);
var
  i: Integer;
  Node: PVirtualNode;
begin
  inherited;
  FCheckedList.Clear;
  seOrderID.Value := 0;
  for i := 0 to lbEvents.Items.Count - 1 do
    lbEvents.Checked[i] := False;

  vstTree.BeginUpdate;
  try
    Node := vstTree.GetFirst;
    while Assigned(Node) do
    begin
      vstTree.IsVisible[Node] := True;
      Node := Node.NextSibling;
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TfrmLogView.aFilterExecute(Sender: TObject);
var
  Data: PLogData;
  Node: PVirtualNode;
  i: Integer;
begin
  inherited;
  FCheckedList.Clear;
  for i := 0 to lbEvents.Items.Count - 1 do
    if lbEvents.Checked[i] then
      FCheckedList.Add(lbEvents.Items[i]);

  vstTree.BeginUpdate;
  try
    Node := vstTree.GetFirst;
    while Assigned(Node) do
    begin
      vstTree.IsVisible[Node] := True;
      Data := Node^.GetData;
      if (seOrderID.Value > 0) then
        vstTree.IsVisible[Node] := seOrderID.Value.ToString.Equals(Data^.OrderID.Trim);
      if (FCheckedList.Count > 0) and vstTree.IsVisible[Node] then
        vstTree.IsVisible[Node] := (FCheckedList.IndexOf(Data^.Method) > -1);
      Node := Node.NextSibling;
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

{ TLogData }

procedure TLogData.Clear;
begin
  Self := Default(TLogData);
end;

end.
