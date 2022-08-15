unit DockForm.AccountPnL;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, DaImages, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, System.UITypes, IABSocketAPI, System.ImageList, Vcl.ImgList,
  Document, InstrumentList, Utils, IABFunctions, DebugWriter, BrokerHelperAbstr, Vcl.ExtCtrls, Vcl.StdCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Vcl.Buttons, System.Actions, Vcl.ActnList,
  CustomDockForm, Vcl.Menus, IABFunctions.RequestsQueue, Common.Types, System.Math;
{$ENDREGION}

type
  PAccountPnL = ^TAccountPnL;
  TAccountPnL = record
    NodeType: TNodeType;
    Broker: TBrokerType;
    DailyPnL: Double;
    UnrealizedPnL: Double;
    TimeUpdate: TDateTime;
    procedure Clear;
  end;

  TfrmDockFormAccountPnL = class(TfrmCustomDockForm)
    aClear: TAction;
    btnClear: TBitBtn;
    procedure aClearExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private const
    COL_BROKER_NAME    = 0;
    COL_DAILY_PNL      = 1;
    COL_UNREALIZED_PNL = 2;
    COL_TIME           = 3;

    C_IDENTITY_NAME = 'DockFormAccountPnL';
  private
    FAccount: string;
    FDataId: Integer;
    FNodeIB: PVirtualNode;
    FNodeNN: PVirtualNode;
    procedure SetAccount(const Value: string);
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure ProfitLoss(DataId: Integer; DailyPnL, UnrealizedPnL: Double);
    property Account: string  read FAccount write SetAccount;
    property DataId : Integer read FDataId  write FDataId;
  end;

var
  frmDockFormAccountPnL: TfrmDockFormAccountPnL;

implementation

{$R *.dfm}

procedure TfrmDockFormAccountPnL.FormCreate(Sender: TObject);
begin
  vstTree.NodeDataSize := SizeOf(TAccountPnL);
  inherited;
end;

procedure TfrmDockFormAccountPnL.FormDestroy(Sender: TObject);
begin
  frmDockFormAccountPnL := nil;
  inherited;  
end;

function TfrmDockFormAccountPnL.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmDockFormAccountPnL.Initialize;
var
  Data: PAccountPnL;
begin
  inherited Initialize;
  Caption := 'PnL for account';
  FNodeIB := vstTree.AddChild(nil);
  Data := FNodeIB^.GetData;
  Data.Broker := TBrokerType.brIB;
  Data.NodeType := ntGroup;

  FNodeNN := vstTree.AddChild(nil);
  Data := FNodeNN^.GetData;
  Data.Broker := TBrokerType.brNN;
  Data.NodeType := ntGroup;
end;

procedure TfrmDockFormAccountPnL.ProfitLoss(DataId: Integer; DailyPnL, UnrealizedPnL: Double);
var
  Data: PAccountPnL;
  Node: PVirtualNode;
begin
  if (Self.DataId = DataId) then
  begin
    vstTree.BeginUpdate;
    try
      Node := vstTree.AddChild(FNodeIB);
      Data := Node^.GetData;
      Data^.DailyPnL      := DailyPnL;
      Data^.UnrealizedPnL := UnrealizedPnL;
      Data^.TimeUpdate    := Now;
      Data^.NodeType      := ntNode;
      Data^.Broker    := TBrokerType.brIB;
    finally
      vstTree.EndUpdate;
    end;
  end;
end;

procedure TfrmDockFormAccountPnL.SetAccount(const Value: string);
begin
  FAccount := Value;
end;

procedure TfrmDockFormAccountPnL.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PAccountPnL;
begin
  inherited;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  case Column of
    COL_BROKER_NAME:
      Result := CompareText(Data1^.Broker.ToString, Data2^.Broker.ToString);
    COL_DAILY_PNL:
      Result := CompareValue(Data1^.DailyPnL, Data2^.DailyPnL);
    COL_UNREALIZED_PNL:
      Result := CompareValue(Data1^.UnrealizedPnL, Data2^.UnrealizedPnL);
    COL_TIME:
      Result := CompareValue(Data1^.TimeUpdate, Data2^.TimeUpdate);
  end;
end;

procedure TfrmDockFormAccountPnL.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PAccountPnL;
begin
  inherited;
  if (Sender.FocusedNode <> Node) then
  begin
    Data := vstTree.GetNodeData(Node);
    if (Data^.NodeType = ntGroup) then
      TargetCanvas.Font.Style := [fsBold]
    else
      case Column of
        COL_DAILY_PNL:
          if (Data.DailyPnL < 0) then
            TargetCanvas.Font.Color := clRed
          else
            TargetCanvas.Font.Color := clBlack;
        COL_UNREALIZED_PNL:
          if (Data.UnrealizedPnL < 0) then
            TargetCanvas.Font.Color := clRed
          else
            TargetCanvas.Font.Color := clBlack;
      end;
  end;
end;

procedure TfrmDockFormAccountPnL.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PAccountPnL;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmDockFormAccountPnL.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PAccountPnL;
begin
  inherited;
  Data := Node^.GetData;
  if (Data^.NodeType = ntGroup) then
    case Column of
      COL_BROKER_NAME:
        CellText := Data^.Broker.ToString;
      else
        CellText := '';
    end
  else
    case Column of
      COL_BROKER_NAME:
        CellText := Data^.Broker.ToString;
      COL_DAILY_PNL:
        CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.DailyPnL);
      COL_UNREALIZED_PNL:
        CellText := FormatFloat(C_CURRENCY_FORMAT, Data^.UnrealizedPnL);
      COL_TIME:
        if (Data^.NodeType = ntNode) then
          CellText := DateTimeToStr(Data^.TimeUpdate);
    else
      CellText := '';
    end;
end;

procedure TfrmDockFormAccountPnL.aClearExecute(Sender: TObject);
begin
  vstTree.Clear;
  Initialize;
end;

procedure TfrmDockFormAccountPnL.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IABClient.SendRequest(ibCancelPnL, DataId);
end;

procedure TfrmDockFormAccountPnL.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;

end;

{ TAccountPnL }

procedure TAccountPnL.Clear;
begin
  Self := Default(TAccountPnL);
end;

end.
