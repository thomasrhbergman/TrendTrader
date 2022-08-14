unit Frame.ActivityLog;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Column.Settings,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, VirtualTrees, Vcl.ExtCtrls,
  Monitor.Types, IABFunctions.MarketData, VirtualTrees.ExportHelper, Winapi.ActiveX, Publishers.Interfaces,
  IABFunctions.RequestsQueue, DaModule.Utils, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Document, DaModule,
  Global.Types, InstrumentList, IABFunctions, DebugWriter, IABSocketAPI, IABSocketAPI_const, System.Math,
  Common.Types, IABFunctions.Helpers, MonitorTree.Helper;
{$ENDREGION}

type
  PActivityData = ^TActivityData;
  TActivityData = record
    Symbol: string;
    Status: TIABOrderState;
    Action: TIABAction;
    Quantity: Integer;
    Filled: Integer;
    TempId: Integer;
    Price: Double;
    TradeTime: TDateTime;
    Info: string;
    procedure Clear;
  end;

  TframeActivityLog = class(TFrame, ILogger)
    btnClear: TBitBtn;
    btnClearAvailableFilters: TBitBtn;
    edtAvailableFilters: TEdit;
    lblAvailableFilters: TLabel;
    pnlActivityLog: TPanel;
    pnlAvailableFilters: TPanel;
    vstTree: TVirtualStringTree;
    procedure btnClearAvailableFiltersClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure edtAvailableFiltersChange(Sender: TObject);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private const
    C_IDENTITY_NAME = 'frameActivityLog';

    COL_SYMBOL     = 0;
    COL_STATUS     = 1;
    COL_ACTION     = 2;
    COL_QUANTITY   = 3;
    COL_FILLED     = 4;
    COL_PRICE      = 5;
    COL_TEMP_ID    = 6;
    COL_TRADE_TIME = 7;
    COL_INFO       = 8;
  private
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation ILogger
    function GetLogListenerType: TLogListenerType;
    procedure Write(const aDetailType: TLogDetailType; const aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aMethod, aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aMethod, aUnit, aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aObject: TObject; const aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aObject: TObject; const aMethod, aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aOrderID, aQuantity, aFilled: Integer; const aMethod, aSymbol, aInfo: string; const aStatus: TIABOrderState; const aAction: TIABAction; const aPrice: Double); overload;
  public
    procedure Deinitialize;
    procedure Initialize;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TframeActivityLog }

constructor TframeActivityLog.Create(AOwner: TComponent);
begin
  inherited;
  Name := C_IDENTITY_NAME;
  vstTree.NodeDataSize := SizeOf(TActivityData);
end;

destructor TframeActivityLog.Destroy;
begin
  Deinitialize;
  inherited;
end;

procedure TframeActivityLog.Initialize;
begin
  TMonitorTree.Initialize(vstTree);
  TStoreHelper.LoadFromXml(vstTree, C_IDENTITY_NAME + '.' + vstTree.Name);
end;

procedure TframeActivityLog.Deinitialize;
begin
  TStoreHelper.SaveToXml(vstTree, C_IDENTITY_NAME + '.' + vstTree.Name);
end;

function TframeActivityLog.GetInstance: TObject;
begin
  Result := Self;
end;

function TframeActivityLog.GetLogListenerType: TLogListenerType;
begin
  Result := ltActivityLog;
end;

procedure TframeActivityLog.Write(const aDetailType: TLogDetailType; const aOrderID, aQuantity, aFilled: Integer; const aMethod, aSymbol, aInfo: string; const aStatus: TIABOrderState; const aAction: TIABAction; const aPrice: Double);
begin
  TThread.Queue(nil,
    procedure()
    var
      Node: PVirtualNode;
      Data: PActivityData;
    begin
      vstTree.BeginUpdate;
      try
        Node := vstTree.AddChild(nil);
        Data := Node^.GetData;
        Data^.Symbol    := aSymbol;
        Data^.Status    := aStatus;
        Data^.Action    := aAction;
        Data^.Quantity  := aQuantity;
        Data^.Filled    := aFilled;
        Data^.TempId    := aOrderID;
        Data^.TradeTime := Now;
        Data^.Price     := aPrice;
        Data^.Info      := aInfo;

        vstTree.NodeHeight[Node] := 14;
        vstTree.InvalidateNode(Node);
        vstTree.ScrollIntoView(Node, False, False);
        vstTree.IsVisible[Node] := string(edtAvailableFilters.Text).IsEmpty or Data^.Symbol.StartsWith(edtAvailableFilters.Text);
      finally
        vstTree.EndUpdate;
      end;
    end);
end;

procedure TframeActivityLog.Write(const aDetailType: TLogDetailType; const aInfo: string);
begin
  TThread.Queue(nil,
    procedure()
    var
      Node: PVirtualNode;
      Data: PActivityData;
    begin
      vstTree.BeginUpdate;
      try
        Node := vstTree.AddChild(nil);
        Data := Node^.GetData;
        Data^.TradeTime := Now;
        Data^.Info      := aInfo;
        if (aDetailType = ddError) then
          Data^.Status := osError;

        vstTree.NodeHeight[Node] := 14;
        vstTree.InvalidateNode(Node);
        vstTree.ScrollIntoView(Node, False, False);
        vstTree.IsVisible[Node] := string(edtAvailableFilters.Text).IsEmpty or Data^.Symbol.StartsWith(edtAvailableFilters.Text);
      finally
        vstTree.EndUpdate;
      end;
    end);
end;

procedure TframeActivityLog.Write(const aDetailType: TLogDetailType; const aMethod, aInfo: string);
begin
  Write(aDetailType, aInfo);
end;

procedure TframeActivityLog.Write(const aDetailType: TLogDetailType; const aObject: TObject; const aMethod, aInfo: string);
begin
  Write(aDetailType, aInfo);
end;

procedure TframeActivityLog.Write(const aDetailType: TLogDetailType; const aObject: TObject; const aInfo: string);
begin
  Write(aDetailType, aInfo);
end;

procedure TframeActivityLog.Write(const aDetailType: TLogDetailType; const aMethod, aUnit, aInfo: string);
begin
  Write(aDetailType, aInfo);
end;

procedure TframeActivityLog.btnClearClick(Sender: TObject);
begin
  vstTree.BeginUpdate;
  try
    vstTree.Clear;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TframeActivityLog.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PActivityData;
begin
  Data := Node^.GetData;
  case Column of
    COL_ACTION:
      if (Data^.Action = iabBuy) then
        TargetCanvas.Font.Color := clBlue
      else
        TargetCanvas.Font.Color := clRed;
    COL_STATUS:
      if (Data^.Status in [osCancelled, osPendCancel]) then
        TargetCanvas.Font.Color := clMaroon
      else if (Data^.Status = osError) then
        TargetCanvas.Font.Color := clRed;
  end;
end;

procedure TframeActivityLog.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PActivityData;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TframeActivityLog.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PActivityData;
  Data2: PActivityData;
begin
  Result := 0;
  Data1  := Node1^.GetData;
  Data2  := Node2^.GetData;
  case Column of
    COL_SYMBOL:
      Result := CompareText(Data1.Symbol, Data2.Symbol);
    COL_STATUS:
      Result := CompareText(Data1^.Status.ToString, Data2^.Status.ToString);
    COL_ACTION:
      Result := CompareText(Data1^.Action.ToString, Data2^.Action.ToString);
    COL_QUANTITY:
      Result := CompareValue(Data1^.Quantity, Data2^.Quantity);
    COL_FILLED:
      Result := CompareValue(Data1^.Filled, Data2^.Filled);
    COL_PRICE:
      Result := CompareValue(Data1^.Price, Data2^.Price);
    COL_TEMP_ID:
      Result := CompareValue(Data1^.TempId, Data2^.TempId);
    COL_TRADE_TIME:
      Result := CompareValue(Data1^.TradeTime, Data2^.TradeTime);
    COL_INFO:
      Result := CompareText(Data1^.Info, Data2^.Info);
  end;
end;

procedure TframeActivityLog.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PActivityData;
begin
  Data := Node^.GetData;
  case Column of
    COL_SYMBOL:
      CellText := Data^.Symbol;
    COL_STATUS:
      CellText := Data^.Status.ToString;
    COL_ACTION:
      CellText := Data^.Action.ToString;
    COL_QUANTITY:
      CellText := Data^.Quantity.ToString;
    COL_FILLED:
      CellText := Data^.Filled.ToString;
    COL_PRICE:
      CellText := Data^.Price.ToString;
    COL_TEMP_ID:
      CellText := Data^.TempId.ToString;
    COL_TRADE_TIME:
      CellText := FormatDateTime('hh:nn:ss.zzz', Data^.TradeTime);
    COL_INFO:
      CellText := Data^.Info;
  else
    CellText := '';
  end;
end;

procedure TframeActivityLog.edtAvailableFiltersChange(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PActivityData;
begin
  vstTree.BeginUpdate;
  try
    Node := vstTree.GetFirst;
    while Assigned(Node) do
    begin
      Data := Node^.GetData;
      vstTree.IsVisible[Node] := string(edtAvailableFilters.Text).IsEmpty or Data.Symbol.StartsWith(edtAvailableFilters.Text);
      Node := Node.NextSibling;
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

procedure TframeActivityLog.btnClearAvailableFiltersClick(Sender: TObject);
begin
  edtAvailableFilters.Text := '';
end;

{ TActivityData }

procedure TActivityData.Clear;
begin
  Self := Default(TActivityData);
end;

end.
