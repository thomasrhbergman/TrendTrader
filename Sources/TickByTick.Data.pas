unit TickByTick.Data;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids, DaModule,
  System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, Vcl.ComCtrls, System.ImageList, Vcl.ImgList, IABSocketAPI_const, IABfunctions, IABSocketAPI, DebugWriter,
  HtmlLib, Search.Instruments, VirtualTrees, BrokerHelperAbstr, Entity.Sokid, IABFunctions.RequestsQueue,
  Publishers.Interfaces, Global.Types, DaImages, Publishers;
{$ENDREGION}

type
  TfrmTickByTick = class(TCustomForm, IOnTickByTick)
    ActionListMain: TActionList;
    aReqTickByTickData: TAction;
    aSearch: TAction;
    btnCancelTT: TButton;
    btnReqTickByTickData: TButton;
    btnSearch: TBitBtn;
    edtConId: TEdit;
    ImageList: TImageList;
    lblConID: TLabel;
    lblSymbol: TLabel;
    lbTickCount: TListBox;
    memLog: TMemo;
    rbBarData: TRadioButton;
    rbTicks: TRadioButton;
    procedure aReqTickByTickDataExecute(Sender: TObject);
    procedure aReqTickByTickDataUpdate(Sender: TObject);
    procedure aSearchExecute(Sender: TObject);
    procedure btnCancelTTClick(Sender: TObject);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FHTDataId: Integer;
    FCDataId: Integer;
    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IOnTickByTick
    procedure OnHistoricalTickData(Sender: TObject; DataID: Integer; TickData: TIABTickData);
    procedure OnTickByTick(Sender: TObject; DataID: Integer; TickData: TIABTickData);
  public
    procedure Initialize;
    class function ShowDocument: TModalResult;
  end;

var
  frmTickByTick: TfrmTickByTick;

implementation

{$R *.dfm}

{ TfrmTickByTick }

class function TfrmTickByTick.ShowDocument: TModalResult;
begin
  frmTickByTick := TfrmTickByTick.Create(nil);
  try
    frmTickByTick.Initialize;
    Result := frmTickByTick.ShowModal;
  finally
    FreeAndNil(frmTickByTick);
  end;
end;

procedure TfrmTickByTick.FormCreate(Sender: TObject);
begin
  TPublishers.TickByTickPublisher.Subscribe(Self);
end;

procedure TfrmTickByTick.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (FHTDataId > 0) then
    IABClient.SendRequest(ibCancelTickByTickData, FHTDataId);
  TPublishers.TickByTickPublisher.Unsubscribe(Self);
end;

procedure TfrmTickByTick.Initialize;
begin
  FHTDataId := 0;
  FCDataId := 0;
  lbTickCount.Items.Add('0');
  lbTickCount.Items.Add('10');
  lbTickCount.Items.Add('100');
  lbTickCount.Items.Add('250');
  lbTickCount.Items.Add('500');
  lbTickCount.Items.Add('1000');
  lbTickCount.ItemIndex := 0;
end;

procedure TfrmTickByTick.aReqTickByTickDataExecute(Sender: TObject);
var
  Order : TIABOrder;
  Request: TIABRequest;
begin
  memLog.Clear;
  if rbTicks.Checked then // tick
  begin
    if (FHTDataId > 0) then
      IABClient.SendRequest(ibCancelTickByTickData, FHTDataId);
    Order := SokidList.GetOrderByConID(StrToIntDef(edtConId.Text, 0));
    try
      FHTDataId := Order.ContractId;
      Request := Default(TIABRequest);
      Request.Command      := ibGetTickByTickData;
      Request.DataId       := Order.ContractId;
      Request.Order        := Order;
      Request.TickDataType := TIABTickDataType.tdLast;
      Request.Option       := StrToIntDef(lbTickCount.Items[lbTickCount.ItemIndex], 0);
      IABClient.SendRequest(Request);
    finally
      FreeAndNil(Order);
    end;
  end
  else
  begin // bar data
    Order := SokidList.GetOrderByConID(StrToIntDef(edtConId.Text, 0));
    FHTDataId := Order.ContractId;
    try
      Request := Default(TIABRequest);
      Request.Command  := ibGetHistoricalData;
      Request.DataId   := FCDataId;
      Request.Order    := Order;
      IABClient.SendRequest(Request);
    finally
      FreeAndNil(Order);
    end;
  // history formats
  //  DateTimeToIABDateTimeStr(Now), 1, IAB_TIME_UNIT_DAY, bs5Min,   - today, 5 min bars
  //  DateTimeToIABDateTimeStr(Now), 1, IAB_TIME_UNIT_DAY, bs1Min,   - today, 1 min bars
  //  DateTimeToIABDateTimeStr(Now), 2, IAB_TIME_UNIT_DAY, bs1Min,  -  several days, 1 min bars
  end;
  memLog.Lines.Add('RequestTime=' + FormatDateTime('hh:nn:ss.zzz', Now));
  memLog.Lines.Add('');
end;

 procedure TfrmTickByTick.btnCancelTTClick(Sender: TObject);
 begin
   if (FHTDataId > 0) then
   begin
     IABClient.CancelTickByTickData(FHTDataId);
     memLog.Lines.Add('Cancel TickByTick Data ' + DateTimeToStr(Now));
   end;
 end;

procedure TfrmTickByTick.OnHistoricalTickData(Sender: TObject; DataID: Integer; TickData: TIABTickData);
const
  ForT: array [Boolean] of string = ('False', 'True');
var
  s: string;
begin
  s := 'OnHistoricalTickData, ' + 'RespondTime=' + FormatDateTime('hh:nn:ss.zzz', Now) + ', DataID=' + DataID.ToString;
  with TickData do
    case TickType of
      tdLast, tdAllLast:
        s := s + ', Price=' + FormatFloat('0.00#', Price) + ', Size=' + Size.ToString;
      tdBidAsk:
        s := s + ', BidPrice=' + FormatFloat('0.00#', BidPrice) + ', BidSize=' + BidSize.ToString + ', AskSize=' + AskSize.ToString + ', AskPrice=' + FormatFloat('0.00#', AskPrice);
      tdMidPoint:
        s := s + ', MidPoint=' + FormatFloat('0.00#', MidPoint);
    end;
  memLog.Lines.Add(s);
end;

procedure TfrmTickByTick.OnTickByTick(Sender: TObject; DataID: Integer; TickData: TIABTickData);
const
  ForT: array [Boolean] of string = ('False', 'True');
var
  s: string;
begin
  s := 'OnTickByTick, ' + 'RespondTime=' + FormatDateTime('hh:nn:ss.zzz', Now) + ', DataID=' + DataID.ToString;
  with TickData do
    case TickType of
      tdLast, tdAllLast:
        s := s + ', Price=' + FormatFloat('0.00#', Price) + ', Size=' + Size.ToString;
      tdBidAsk:
        s := s + ', BidPrice=' + FormatFloat('0.00#', BidPrice) + ', BidSize=' + BidSize.ToString + ', AskSize=' + AskSize.ToString + ', AskPrice=' + FormatFloat('0.00#', AskPrice);
      tdMidPoint:
        s := s + ', MidPoint=' + FormatFloat('0.00#', MidPoint);
    end;
  memLog.Lines.Add(s);
end;

procedure TfrmTickByTick.aReqTickByTickDataUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := edtConId.Text <> '';
end;

procedure TfrmTickByTick.aSearchExecute(Sender: TObject);
begin
  TfrmSearchInstruments.ShowDocument(Self);
end;

procedure TfrmTickByTick.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SourceNode: PVirtualNode;
  Data: PSokidInfo;
begin
  if Assigned(Source) and (Source is TVirtualStringTree) then
  begin
    SourceNode := TVirtualStringTree(Source).GetFirstSelected;
    if Assigned(SourceNode) then
    begin
      Data := SourceNode^.GetData;
      edtConId.Text := Data^.ContractId.ToString;
      lblSymbol.Caption := Data^.Symbol;
    end;
  end;
end;

procedure TfrmTickByTick.FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Assigned(Source) and (Source is TVirtualStringTree);
end;

function TfrmTickByTick.GetInstance: TObject;
begin
  Result := Self;
end;

end.
