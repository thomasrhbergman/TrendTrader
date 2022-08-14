unit DockForm.AccountInfo;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, System.UITypes, IABSocketAPI, System.ImageList, Vcl.ImgList, InstrumentList,
  Utils, IABFunctions, DebugWriter, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.StdCtrls, Vcl.ExtCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomDockForm, Document, IABFunctions.RequestsQueue,
  Monitor.Interfaces, Account.Types, Vcl.Buttons, Common.Types;
{$ENDREGION}

type
  TfrmDockFormAccountInfo = class(TfrmCustomDockForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private const
    COL_ACCOUNT = 0;
    COL_NAME    = 1;
    COL_VALUE   = 2;
    COL_INFO    = 3;
    C_IDENTITY_NAME = 'DockFormAccountInfo';
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure OnAccountValue(Index: Integer);
  end;

var
  frmDockFormAccountInfo: TfrmDockFormAccountInfo;

implementation

{$R *.dfm}

procedure TfrmDockFormAccountInfo.FormCreate(Sender: TObject);
begin
  vstTree.NodeDataSize := SizeOf(TAccount);
  inherited;
end;

procedure TfrmDockFormAccountInfo.FormDestroy(Sender: TObject);
begin
  frmDockFormAccountInfo := nil;
  inherited;
end;

function TfrmDockFormAccountInfo.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TfrmDockFormAccountInfo.Initialize;
begin
  inherited Initialize;
  Caption := 'Account';
end;

procedure TfrmDockFormAccountInfo.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PAccount;
begin
  inherited;
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  case Column of
    COL_ACCOUNT:
      Result := CompareText(Data1^.AccountName, Data2^.AccountName);
    COL_NAME:
      Result := CompareText(Data1^.ParameterName, Data2^.ParameterName);
    COL_VALUE:
      Result := CompareText(Data1^.Value, Data2^.Value);
    COL_INFO:
      Result := CompareText(Data1^.Info, Data2^.Info)
  end;
end;

procedure TfrmDockFormAccountInfo.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PAccount;
begin
  inherited;
  if (Sender.FocusedNode <> Node) then
  begin
    Data := Node^.GetData;
    case Column of
      COL_VALUE:
        if (StrToFloatDef(Data.Value, 0) < 0) then
          TargetCanvas.Font.Color := clRed
        else
          TargetCanvas.Font.Color := clBlack;
    end;
  end;
end;

procedure TfrmDockFormAccountInfo.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data : PAccount;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TfrmDockFormAccountInfo.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PAccount;
  FloatValue: Double;
begin
  inherited;
  Data := Node^.GetData;
  case Column of
    COL_NAME:
      CellText := Data.ParameterName;
    COL_VALUE:
     if IsFloat(Data^.Value) then
      begin
        FloatValue := StrToFloatEx(Data^.Value);
        CellText := FormatFloat(C_CURRENCY_FORMAT, FloatValue)
      end
      else
        CellText := Data^.Value;
    COL_INFO:
      CellText := Data.Info;
    COL_ACCOUNT:
      CellText := Data.AccountName;
  end;
end;

procedure TfrmDockFormAccountInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IABClient.SendRequest(ibCancelAccountUpdates, 0);
end;

procedure TfrmDockFormAccountInfo.OnAccountValue(Index: Integer);
var
  AccountItems: TArray<string>;
  Data: PAccount;
  Node: PVirtualNode;
  Root: PVirtualNode;
begin
  if not Assigned(IABClient) then
    Exit;
  vstTree.BeginUpdate;
  try
    AccountItems := IABClient.AccountValues[Index].Split([' ']);
    if (Length(AccountItems) >= 4) then
    begin
      Root := vstTree.GetFirst;
      if not Assigned(Root) then
      begin
        Root := vstTree.AddChild(nil);
        Data := vstTree.GetNodeData(Root);
        Data.AccountName := AccountItems[3];
      end;

      Node := vstTree.AddChild(Root);
      Data := Node^.GetData;
      Data.ParameterName := AccountItems[0];
      Data.Value         := AccountItems[1];
      Data.Info          := AccountItems[2];
    end;
  finally
    vstTree.EndUpdate;
  end;
end;

end.
