unit Qualifiers.Frame;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons, Vcl.ExtCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, Document, System.Generics.Collections,
  System.Generics.Defaults, DebugWriter, Global.Types, System.IniFiles, IABSocketAPI_const, IABSocketAPI,
  System.Math, InstrumentList, Edit.OrderStatus, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ExtDlgs, BrokerHelperAbstr,
  Vcl.Printers, DaModule, Scanner.Types, MessageDialog, Qualifiers.Types, VirtualTrees.ExportHelper, Monitor.Interfaces,
  Common.Types, AutoTrades.Types, DaImages, IABFunctions, Monitor.Types, Frame.Custom, Publishers, Utils;
{$ENDREGION}

type
  PQualifierData = ^TQualifierData;
  TQualifierData = record
    DocType            : TDocType;
    Qualifier          : TQualifier;
    QualifierCondition : TQualifierCondition;
    procedure Clear;
  end;

  TframeQualifiers = class(TframeCustom, IQualifiersController)
    procedure vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private const
    COL_INSTANCE_NUM          = 0;
    COL_RECORD_ID             = 1;
    COL_NAME                  = 2;
    COL_TYPE_CONDITION        = 3;
    COL_ENABLED               = 4;
    COL_STARTUP_DATE          = 5;
    COL_INSTRUMENT1           = 6;
    COL_INSTRUMENT1_TICKTYPES = 7;
    COL_INSTRUMENT1_VALUES    = 8;
    COL_CONDITION             = 9;
    COL_INSTRUMENT2           = 10;
    COL_INSTRUMENT2_TICKTYPES = 11;
    COL_INSTRUMENT2_VALUES    = 12;
    COL_STATE                 = 13;
    COL_ACTIVATED_TIME        = 14;
    COL_CONDITION_TRUE_TIME   = 15;
    COL_CREATE_TIME           = 16;

    C_IDENTITY_NAME = 'frameQualifiers';
  protected
    function GetIdentityName: string; override;
  private
    FNodeList: TDictionary<Integer, PVirtualNode>;
    FQualifierList: TQualifierList;
    procedure SetQualifierList(const Value: TQualifierList);
    procedure SetState(const aState: TTradesState);

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IQualifiersController
    procedure UpdateState(const aQualifier: TQualifier);
    procedure DeleteQualifier(const aInstanceNum: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Deinitialize; override;

    procedure Delete;
    procedure DeleteAll;
    procedure Start;
    procedure Stop;

    property QualifierList: TQualifierList read FQualifierList write SetQualifierList;
  end;

implementation

{$R *.dfm}

{ TframeQualifiers }

constructor TframeQualifiers.Create(AOwner: TComponent);
begin
  inherited;
  vstTree.NodeDataSize := SizeOf(TQualifierData);
  FNodeList := TDictionary<Integer, PVirtualNode>.Create;
  QualifiersControllerPublisher.Subscribe(Self);
  AutoTradesControllerPublisher.Subscribe(Self);
end;

destructor TframeQualifiers.Destroy;
begin
  FreeAndNil(FNodeList);
  if Assigned(QualifiersControllerPublisher) then
    QualifiersControllerPublisher.Unsubscribe(Self);
  if Assigned(AutoTradesControllerPublisher) then
    AutoTradesControllerPublisher.Unsubscribe(Self);
  inherited;
end;

procedure TframeQualifiers.Initialize;
begin
  inherited Initialize;
  Self.Caption := C_IDENTITY_NAME;
end;

procedure TframeQualifiers.Deinitialize;
begin
  inherited;

end;

function TframeQualifiers.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

function TframeQualifiers.GetInstance: TObject;
begin
  Result := Self;
end;

procedure TframeQualifiers.vstTreeBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PQualifierData;
  State: TTradesState;
begin
  inherited;
  Data := Node^.GetData;
  if (Data^.DocType = ntQualifier) then
  begin
    if Assigned(Data^.Qualifier.AutoTradesInstance) then
      State := Data^.Qualifier.AutoTradesInstance.GetTradesState
    else
      State := Data^.Qualifier.State;
    TargetCanvas.Brush.Color := State.ToColor;
  end
  else if (Data^.DocType = ntQualifierCondition) then
  begin
    if Data^.QualifierCondition.IsCondition then
    begin
      TargetCanvas.Brush.Color := clWebMintcream;
      TargetCanvas.FillRect(CellRect);
    end
    else if not Data^.QualifierCondition.Enabled then
    begin
      TargetCanvas.Brush.Color := clWebMintcream;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
end;

procedure TframeQualifiers.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PQualifierData;
  State: TTradesState;
begin
  inherited;
  Data := Node^.GetData;
  if (Data^.DocType = ntQualifier) then
  begin
    if (Column = COL_STATE) then
    begin
      if Assigned(Data^.Qualifier.AutoTradesInstance) then
        State := Data^.Qualifier.AutoTradesInstance.GetTradesState
      else
        State := Data^.Qualifier.State;
      case State of
        tsSuspended:
          TargetCanvas.Font.Color := clWebOrange;
        tsWorking:
          TargetCanvas.Font.Color := clGreen;
        tsCancelled:
          TargetCanvas.Font.Color := clRed;
        tsNotConsidered:
          TargetCanvas.Font.Color := clBlack;
      end;
    end;
  end;
end;

procedure TframeQualifiers.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PQualifierData;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TframeQualifiers.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
resourcestring
  rsInstrument = '%s (%d)';
  rsValues = '%.2f / %.2f';
  rsTickTypes = '%s / %s';
var
  Data: PQualifierData;
begin
  inherited;
  if not Assigned(Node) then
    Exit;
  Data := Node^.GetData;
  case Data^.DocType of
    ntQualifier:
      begin
        case Column of
          COL_INSTANCE_NUM:
            CellText := Data^.Qualifier.InstanceNum.ToString;
          COL_RECORD_ID:
            CellText := Data^.Qualifier.RecordId.ToString;
          COL_ENABLED:
            CellText := BoolToChar(Data^.Qualifier.Enabled);
          COL_NAME:
            CellText := Data^.Qualifier.Name;
          COL_STATE:
            if Assigned(Data^.Qualifier.AutoTradesInstance) then
              CellText := Data^.Qualifier.AutoTradesInstance.GetTradesState.ToString
            else
              CellText := Data^.Qualifier.State.ToString;
          COL_ACTIVATED_TIME:
            CellText := FormatDateTime('hh:nn:ss.zzz', Data^.Qualifier.PreTime);
          COL_CONDITION_TRUE_TIME:
            CellText := FormatDateTime('hh:nn:ss.zzz', Data^.Qualifier.PerTime);
          COL_CREATE_TIME:
            CellText := FormatDateTime('hh:nn:ss.zzz', Data^.Qualifier.CreateTime);
        else
          CellText := '';
        end
      end;
    ntQualifierCondition:
      begin
        case Column of
          COL_NAME:
            begin
              CellText := Data^.QualifierCondition.Name;
              if not Data^.QualifierCondition.Enabled then
                CellText := CellText + ' [DISABLED]';
              if Data^.QualifierCondition.Bypass then
                CellText := CellText + ' [BYPASS]';
            end;

          COL_RECORD_ID:
            CellText := Data^.QualifierCondition.RecordId.ToString;
          COL_TYPE_CONDITION:
            CellText := Data^.QualifierCondition.TypeCondition.ToString;
          COL_STATE:
            if Data^.QualifierCondition.IsCondition then
              CellText := 'Is Condition'
            else
              CellText := '';
          COL_STARTUP_DATE:
            case Data^.QualifierCondition.TypeCondition of
              tcRealtime:
                CellText := '';
              tcEveryDay:
                CellText := FormatDateTime('hh:nn:ss', Data^.QualifierCondition.StartupDate);
              tcSpecificDate:
                CellText := FormatDateTime('DD.MM.YYYY hh:nn:ss', Data^.QualifierCondition.StartupDate);
            end;
          COL_ENABLED:
            CellText := BoolToChar(Data^.QualifierCondition.Enabled);
          COL_INSTRUMENT1:
            if (Data^.QualifierCondition.TypeCondition = tcRealtime) then
            begin
              if (Data^.QualifierCondition.Instrument1.SokidInfo.SecurityType = 'OPT') or
                 (Data^.QualifierCondition.Instrument1.SokidInfo.SecurityType = 'FUT') then
                CellText := Format(rsInstrument, [Data^.QualifierCondition.Instrument1.SokidInfo.LocalSymbol,
                                                  Data^.QualifierCondition.Instrument1.SokidInfo.ContractId])
              else
                CellText := Format(rsInstrument, [Data^.QualifierCondition.Instrument1.SokidInfo.Symbol,
                                                  Data^.QualifierCondition.Instrument1.SokidInfo.ContractId])
            end
            else
              CellText := '';
          COL_INSTRUMENT1_VALUES:
            if (Data^.QualifierCondition.TypeCondition = tcRealtime) then
            begin
              if (Data^.QualifierCondition.Instrument1.TickType2 = ttNotSet) then
                CellText := Format('%.2f', [Data^.QualifierCondition.Instrument1.PriceValue1])
              else
                CellText := Format(rsValues, [Data^.QualifierCondition.Instrument1.PriceValue1,
                                              Data^.QualifierCondition.Instrument1.PriceValue2])
            end
            else
              CellText := '';
          COL_INSTRUMENT1_TICKTYPES:
            if (Data^.QualifierCondition.TypeCondition = tcRealtime) then
            begin
              if (Data^.QualifierCondition.Instrument1.TickType2 = ttNotSet) then
                CellText := Data^.QualifierCondition.Instrument1.TickType1.ToString
              else
                CellText := Format(rsTickTypes, [Data^.QualifierCondition.Instrument1.TickType1.ToString,
                                                 Data^.QualifierCondition.Instrument1.TickType2.ToString])
            end
            else
              CellText := '';
          COL_CONDITION:
//            if (Data^.QualifierCondition.TypeCondition = tcRealtime) then
//              CellText := Data^.QualifierCondition.InequalityType.ToString
//            else
            CellText := BoolToStr(Data^.QualifierCondition.IsCondition, True);
          COL_INSTRUMENT2:
            if (Data^.QualifierCondition.TypeCondition = tcRealtime) then
            begin
              if (Data^.QualifierCondition.Instrument2.SokidInfo.SecurityType = 'OPT') or
                 (Data^.QualifierCondition.Instrument2.SokidInfo.SecurityType = 'FUT') then
                CellText := Format(rsInstrument, [Data^.QualifierCondition.Instrument2.SokidInfo.LocalSymbol,
                                                  Data^.QualifierCondition.Instrument2.SokidInfo.ContractId])
              else
                CellText := Format(rsInstrument, [Data^.QualifierCondition.Instrument2.SokidInfo.Symbol,
                                                  Data^.QualifierCondition.Instrument2.SokidInfo.ContractId])
            end
            else
              CellText := '';
          COL_INSTRUMENT2_VALUES:
            if (Data^.QualifierCondition.TypeCondition = tcRealtime) then
            begin
              if (Data^.QualifierCondition.Instrument2.TickType2 = ttNotSet) then
                CellText := Format('%.2f', [Data^.QualifierCondition.Instrument2.PriceValue1])
              else
                CellText := Format(rsValues, [Data^.QualifierCondition.Instrument2.PriceValue1,
                                              Data^.QualifierCondition.Instrument2.PriceValue2])
            end
            else
              CellText := '';
          COL_INSTRUMENT2_TICKTYPES:
            if (Data^.QualifierCondition.TypeCondition = tcRealtime) then
            begin
              if (Data^.QualifierCondition.Instrument2.TickType2 = ttNotSet) then
                CellText := Data^.QualifierCondition.Instrument2.TickType1.ToString
              else
                CellText := Format(rsTickTypes, [Data^.QualifierCondition.Instrument2.TickType1.ToString,
                                                 Data^.QualifierCondition.Instrument2.TickType2.ToString])
            end
            else
              CellText := '';
          COL_CREATE_TIME:
            CellText := FormatDateTime('hh:nn:ss.zzz', Data^.QualifierCondition.CreateTime);
        else
          CellText := '';
        end;
      end;
  end;
end;

procedure TframeQualifiers.SetQualifierList(const Value: TQualifierList);
begin
  FQualifierList := Value;
end;

procedure TframeQualifiers.UpdateState(const aQualifier: TQualifier);
var
  Data: PQualifierData;
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
  QualifierCondition: TQualifierCondition;
begin
  if Application.Terminated then
    Exit
  else if (not FNodeList.ContainsKey(aQualifier.InstanceNum)) and (aQualifier.State = tsNotConsidered) then
    Exit;

  if not FNodeList.ContainsKey(aQualifier.InstanceNum) then
  begin
    vstTree.BeginUpdate;
    try
      ParentNode := vstTree.AddChild(nil);
      ParentNode.CheckType := ctCheckBox;
      Data := ParentNode^.GetData;
      Data.Qualifier := aQualifier;
      Data.DocType := ntQualifier;
      FNodeList.Add(aQualifier.InstanceNum, ParentNode);
      vstTree.InvalidateNode(ParentNode);
      for QualifierCondition in Data^.Qualifier.Conditions do
      begin
        Node := vstTree.AddChild(ParentNode);
        Data := Node^.GetData;
        Data.DocType := ntQualifierCondition;
        Data.QualifierCondition := QualifierCondition;
        vstTree.InvalidateNode(Node);
      end;
      vstTree.FullExpand(ParentNode);
    finally
      vstTree.EndUpdate;
    end;
  end
  else
  begin
    ParentNode := FNodeList.Items[aQualifier.InstanceNum];
    Data := ParentNode^.GetData;
    Data^.Qualifier.AutoTradesInstance := aQualifier.AutoTradesInstance;
    vstTree.Invalidate;
  end;
end;

procedure TframeQualifiers.SetState(const aState: TTradesState);
resourcestring
  rsInfo = 'State changed manually to ';
var
  Data: PQualifierData;
  Node: PVirtualNode;
  Index: Integer;
  Qualifier: TQualifier;
begin
  Node := vstTree.FocusedNode;
  if Assigned(Node) then
    if ((Node.CheckType = ctCheckBox) and (Node.CheckState = csCheckedNormal)) or
        (Node.CheckType <> ctCheckBox) then
    begin
      Data := Node^.GetData;
      if (Data^.DocType = ntQualifierCondition) then
        Node := Node.Parent;

      if Assigned(Node) then
      begin
        Data := Node^.GetData;
        if (Data^.Qualifier.State in [tsWorking, tsSuspended]) and Assigned(Data^.Qualifier.AutoTradesInstance) then
          if Assigned(FQualifierList) then
          begin
            Index := FQualifierList.GetIndexByNum(Data^.Qualifier.InstanceNum);
            if (Index > -1) then
            begin
              Qualifier := FQualifierList.Items[Index];
              Qualifier.State := aState;
              FQualifierList.Items[Index] := Qualifier;
              Data^.Qualifier.State := aState;
              Data^.Qualifier.AutoTradesInstance.SetTradesState(aState);
              AutoTradesControllerPublisher.SetInfo(Data^.Qualifier.AutoTradesInstance.GetAutoTradeInfo.InstanceNum, rsInfo + aState.ToString);
            end;
          end;
      end;
    end;
end;

procedure TframeQualifiers.Start;
begin
  SetState(tsWorking);
end;

procedure TframeQualifiers.Stop;
begin
  SetState(tsSuspended);
end;

procedure TframeQualifiers.Delete;
resourcestring
  rsCanceled = 'Qualifier Instance "%s" will be stopped,' + sLineBreak +
               'all orders and autotrades will be canceled and removed from the Monitor.' + sLineBreak +
               'Continue?';
var
  Data: PQualifierData;
  Node: PVirtualNode;
  Index: Integer;
begin
  Node := vstTree.FocusedNode;
  if Assigned(Node) then
  begin
    Data := Node^.GetData;
    if (Data^.DocType = ntQualifierCondition) then
      Node := Node.Parent;

    if Assigned(Node) then
      if (TMessageDialog.ShowQuestion(Format(rsCanceled, [Data^.Qualifier.InstanceNum.ToString])) = mrYes) then
      begin
        Data := Node^.GetData;
        Data^.Qualifier.DisableAllConditions;
        if Assigned(Data^.Qualifier.AutoTradesInstance) then
          AutoTradesControllerPublisher.DeleteAutoTrade(Data^.Qualifier.AutoTradesInstance.GetAutoTradeInfo.InstanceNum);
        if Assigned(FQualifierList) then
        begin
          Index := FQualifierList.GetIndexByNum(Data^.Qualifier.InstanceNum);
          if (Index > -1) then
            FQualifierList.Delete(Index);
        end;
        QualifiersControllerPublisher.DeleteQualifier(Data^.Qualifier.InstanceNum);
      end;
  end;
end;

procedure TframeQualifiers.DeleteAll;
var
  Data: PQualifierData;
  IndexQ: Integer;
  Index: Integer;
  arr: TArray<Integer>;
begin
  vstTree.BeginUpdate;
  try
    TPublishers.LogPublisher.Write([ltLogWriter], ddEnterMethod, Self, 'DeleteAll', '');
    Index := 0;
    SetLength(arr, FNodeList.Count);
    for var Node in FNodeList.Values do
    begin
      if Assigned(Node) then
      begin
        Data := Node^.GetData;
        if (Data^.DocType = ntQualifier) then
        begin
          Data^.Qualifier.DisableAllConditions;
          if Assigned(Data^.Qualifier.AutoTradesInstance) then
          begin
            AutoTradesControllerPublisher.DeleteAutoTrade(Data^.Qualifier.AutoTradesInstance.GetAutoTradeInfo.InstanceNum);
            TPublishers.MonitorStructureChangePublisher.OnMonitorStructureChange(nil, nil, crNodeMoved);
          end;
          arr[Index] := Data^.Qualifier.InstanceNum;
          Inc(Index);
          if Assigned(FQualifierList) then
          begin
            IndexQ := FQualifierList.GetIndexByNum(Data^.Qualifier.InstanceNum);
            if (IndexQ > -1) then
              FQualifierList.Delete(IndexQ);
          end;
          TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'DeleteAll', 'Instance Num:' + Data^.Qualifier.InstanceNum.ToString);
        end;
      end;
    end;
    for Index in arr do
      if (Index > 0) then
      begin
        QualifiersControllerPublisher.DeleteQualifier(Index);
        TPublishers.MonitorStructureChangePublisher.OnMonitorStructureChange(nil, nil, crNodeMoved);
      end;
  finally
    vstTree.EndUpdate;
    TPublishers.LogPublisher.Write([ltLogWriter], ddExitMethod, Self, 'DeleteAll', '');
  end;
end;

procedure TframeQualifiers.DeleteQualifier(const aInstanceNum: Integer);
var
  Node: PVirtualNode;
begin
  if FNodeList.ContainsKey(aInstanceNum) then
  begin
    Node := FNodeList.ExtractPair(aInstanceNum).Value;
    if Assigned(Node) then
    begin
      vstTree.BeginUpdate;
      try
        vstTree.DeleteNode(Node);
      finally
        vstTree.EndUpdate;
      end;
    end;
  end;
end;

{ TQualifierData }

procedure TQualifierData.Clear;
begin
  case Self.DocType of
    ntQualifier:
      Self.Qualifier.Clear;
    ntQualifierCondition:
      Self.QualifierCondition := nil;
  end;
end;

end.
