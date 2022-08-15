unit MonitorTree.Helper;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, IABSocketAPI_const, IABFunctions, DebugWriter, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, HtmlLib, Document, Qualifiers.Types, Scanner.Types, VirtualTrees, BrokerHelperAbstr, DaModule, Data.DB,
  HtmlConsts, AutoTrades.Types, Common.Types, System.StrUtils, Utils, Global.Types,
  InstrumentList, Entity.Sokid, IABFunctions.MarketRules, System.Generics.Defaults, System.Generics.Collections,
  System.DateUtils, DaModule.Utils, ArrayHelper, Publishers, DaImages, IABFunctions.Helpers,  Monitor.Types,
  MonitorTree.Document, System.UITypes, VirtualTrees.Types;
{$ENDREGION}

type
  TMonitorTree = class
  private const
    COL_ITEMS    = 0;
    COL_VALUE    = 1;
    COL_CALCTYPE = 2;
    COL_LAST_CLOSE = 4;

    C_MOTHER_ORDER = 'ᴍ';//'ᴹ';
  public
    class procedure OnAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    class procedure OnBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    class procedure OnDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    class procedure OnDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    class procedure OnGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    class procedure OnGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    class procedure OnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    class procedure Initialize(Sender: TVirtualStringTree);
  end;

  TMarkedNode = record
    RecordId : Integer;
    DocType  : TDocType;
    function Equal(aTreeData: TTreeData): Boolean;
    constructor Create(aRecordId: Integer; aDocType: TDocType);
  end;

implementation

{ TMonitorTree }

class procedure TMonitorTree.Initialize(Sender: TVirtualStringTree);
begin
  Sender.CheckImageKind    := ckCustom;
  Sender.CustomCheckImages := DMImage.ilCustomCheckImages;
  Sender.Colors.FocusedSelectionColor   := clWebLightSteelBlue;
  Sender.Colors.UnfocusedSelectionColor := clWebLightSteelBlue;
  Sender.Colors.SelectionTextColor      := clBlack;
  Sender.Colors.GridLineColor           := clSilver;
  Sender.TreeOptions.AutoOptions      := Sender.TreeOptions.AutoOptions + [toAutoDropExpand, toAutoExpand, toAutoTristateTracking, toAutoChangeScale];
  Sender.TreeOptions.MiscOptions      := Sender.TreeOptions.MiscOptions + [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick];
  Sender.TreeOptions.PaintOptions     := Sender.TreeOptions.PaintOptions + [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware];
  Sender.TreeOptions.SelectionOptions := Sender.TreeOptions.SelectionOptions + [toExtendedFocus, toAlwaysSelectNode] - [toFullRowSelect];
end;

class procedure TMonitorTree.OnAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
var
  Data: PTreeData;
begin
  Data := Node^.GetData;
  if Assigned(Data^.AlgosDoc) then
    TargetCanvas.Brush.Color := clHighlight
  else if Assigned(Data^.OrderDoc) then
    TargetCanvas.Brush.Color := clRed;
end;

class procedure TMonitorTree.OnBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PTreeData;
begin
  Data := Node^.GetData;
  if (Data^.DocType = ntOrder) then
  begin
    TargetCanvas.Brush.Color := clWebWhiteSmoke;
    TargetCanvas.FillRect(CellRect);
  end;
end;

class procedure TMonitorTree.OnDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  MoveNode: PVirtualNode;
  ToNode: PVirtualNode;
begin
  inherited;
  ToNode := Sender.GetNodeAt(Pt.X, Pt.Y);
  if (Source is TVirtualStringTree) then
  begin
    MoveNode := TVirtualStringTree(Source).FocusedNode;
    case TTreeDocument.GetDocType(TVirtualStringTree(Source), MoveNode) of
      ntQualifier:
        Accept := True;
      ntAutoTrade:
        Accept := TTreeDocument.GetDocType(Sender, ToNode) in [ntQualifier, ntUnknow];
      ntOrderGroupSet:
        Accept := TTreeDocument.GetDocType(Sender, ToNode) in [ntAutoTrade, ntUnknow];
      ntOrderGroup:
        Accept := TTreeDocument.GetDocType(Sender, ToNode) in [ntOrderGroupSet, ntOrderGroup, ntOrder, ntUnknow];
      ntFactor:
        Accept := TTreeDocument.GetDocType(Sender, ToNode) in [ntCondition, ntAlgos, ntFactor];
      ntOrder:
        Accept := TTreeDocument.GetDocType(Sender, ToNode) in [ntOrderGroup, ntOrder];
      ntAlgos:
        Accept := TTreeDocument.GetDocType(Sender, ToNode) in [ntCondition, ntAlgos];
      ntCondition:
        Accept := TTreeDocument.GetDocType(Sender, ToNode) in [ntCondition, ntOrder];
    else
      Accept := False;
    end;
  end;
end;

class procedure TMonitorTree.OnDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PTreeData;
begin
  if Assigned(Node) and (Sender.FocusedNode <> Node) then
  begin
    Data := Node^.GetData;
    if (Data^.CreationType = ctMirror) then
    begin
      TargetCanvas.Font.Style := [TFontStyle.fsBold]
    end
    else
      case Column of
        COL_ITEMS:
          begin
            case Data^.DocType of
              ntQualifier:
                ;
              ntAutoTrade:
                begin
                  TargetCanvas.Font.Style := [TFontStyle.fsBold];
                  if not Data^.AutoTrade.Enabled then
                    TargetCanvas.Font.Color := clDkGray;
                end;
              ntOrderGroupSet:
                begin
                  TargetCanvas.Font.Style := [TFontStyle.fsItalic];
                  if (Data^.ImageNo = -1) then
                    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [TFontStyle.fsBold]
                  else
                  begin
                    case Data^.OrderGroupSetDoc.TypeUse of
                      tuMonitor:
                        TargetCanvas.Font.Color := clBlack;
                      tuTemplate:
                        TargetCanvas.Font.Color := clNavy;
                      tuBaseOrder:
                        TargetCanvas.Font.Color := clMaroon;
                    end;
                    TargetCanvas.Font.Style := TargetCanvas.Font.Style - [TFontStyle.fsBold]
                  end;
                end;
              ntOrderGroup:
                TargetCanvas.Font.Style := [TFontStyle.fsItalic];
              ntOrder:
                begin
                  if (Data^.OrderDoc.OrderAction = iabBuy) then
                    TargetCanvas.Font.Color := clGreen
                  else if (Data^.OrderDoc.OrderStatus = osSubmitted) then
                    TargetCanvas.Font.Color := C_ORDER_SUBMITTED_COLOUR
                  else
                    TargetCanvas.Font.Color := clMaroon;

                  if (Data^.OrderDoc.OrderStatus in [osFilled]) then
                  begin
                    TargetCanvas.Font.Style := [fsBold];
                    if (Data^.OrderDoc.OrderAction = iabBuy) then
                      TargetCanvas.Font.Color := C_ORDER_FILLED_COLOUR;
                  end
                  else if (Data^.OrderDoc.OrderStatus in [osPartlyFilled, osPendSubmit, osPreSubmit, osSubmitted]) then
                    TargetCanvas.Font.Style := [fsBold]
                  else if (Node.CheckState = csCheckedNormal) then
                    TargetCanvas.Font.Style := [fsItalic];
                end;
              ntCondition:
                TargetCanvas.Font.Color := clBlack;
              ntAlgos:
                TargetCanvas.Font.Color := clMaroon;
              ntFactor:
                ;
            end;
          end;
      end;
  end;
end;

class procedure TMonitorTree.OnGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

  function GetHintString: string;
  var
    Data: PTreeData;
  begin
    Result := '';
    if Assigned(Node) then
    begin
      Data := Node^.GetData;
      if Assigned(Data) then
        case Data^.DocType of
          ntQualifier:
            Result := Data^.Qualifier.Name;
          ntAutoTrade:
            Result := Data^.AutoTrade.Name;
          ntOrderGroupSet:
            Result := Data^.OrderGroupSetDoc.Name;
          ntOrderGroup:
            Result := Data^.OrderGroupDoc.Name;
          ntOrder:
            Result := Data^.OrderDoc.Description;
          ntCondition:
            Result := Data^.ConditionDoc.Description;
          ntAlgos:
            Result := Data^.AlgosDoc.Name;
          ntFactor:
            Result := Data^.FactorDoc.InstrumentName;
        end;
    end;
  end;

begin
  inherited;
  HintText := GetHintString;
end;

class procedure TMonitorTree.OnGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Data: PTreeData;
begin
  inherited;
  if (Column = COL_ITEMS) and (Kind in [ikNormal, ikSelected]) then
  begin
    Data := Node^.GetData;
    ImageIndex := Data.ImageNo;
  end;
end;

class procedure TMonitorTree.OnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
resourcestring
  rsUseInAutoorder = '<Replace from AutoOrder>';
var
  Data: PTreeData;
begin
  inherited;
  Data := Node^.GetData;
  CellText := '';
  if (Data^.CreationType = ctMirror) then
  begin
    case Column of
      COL_ITEMS:
        CellText := Data^.Caption;
    end;
  end
  else
    case TTreeDocument.GetDocType(Sender, Node) of
      ntQualifier:
        case Column of
          COL_ITEMS:
            CellText := Data^.Qualifier.Name.ToUpper;
          COL_VALUE:
            CellText := IfThen(Data^.Qualifier.Enabled, '[BYPASS]')
        end;
      {ntQualifierCondition:
        case Column of
          COL_ITEMS:
            CellText := Data^.QualifierCondition.Name.ToUpper + IfThen(Data^.QualifierCondition.Bypass, ' [BYPASS]');
          COL_CALCTYPE:
            CellText := Data^.QualifierCondition.TypeCondition.ToString;
          COL_VALUE:
            CellText := Data^.QualifierCondition.ToString;
        end;}
      ntAutoTrade:
        case Column of
          COL_ITEMS:
            CellText := Data^.AutoTrade.Name;
          COL_VALUE:
            begin
              CellText := Data^.AutoTrade.ToValueString;
              if not Data^.AutoTrade.Enabled then
                CellText := CellText + ' DEACTIVATED';
            end;
        end;
      ntOrderGroupSet:
        case Column of
          COL_ITEMS:
            CellText := Data^.OrderGroupSetDoc.Name;
        end;
      ntOrderGroup:
        case Column of
          COL_ITEMS:
            begin
              case Data^.OrderGroupDoc.Kind of
                okNormal:
                  CellText := '[NORM';
                okOneCancelAll:
                  CellText := '[OCA';
                okSequentialContinue:
                  CellText := '[SEQ';
                okSequentialStop:
                  CellText := '[SEQ STOP';
                okModifyOrder:
                  CellText := '[MOD';
              end;

              if Data^.OrderGroupDoc.IsRepetitive then
                CellText := CellText + ',REP] '
              else
                CellText := CellText + ',ONCE] ';
              if Data^.OrderGroupDoc.IsAutoOrder then
                CellText := CellText + '[AUTO] ';
              CellText := CellText + Data^.OrderGroupDoc.Name;
            end;
          COL_VALUE:
            CellText := Data^.OrderGroupDoc.ToValueString;
        end;
      ntOrder:
        case Column of
          COL_ITEMS:
            begin
              CellText := Format('%s %s[%s] [%s] %s', [Data^.OrderDoc.InstrumentName,
                                                       Data^.OrderDoc.OrderAction.ToString,
                                                       Data^.OrderDoc.BrokerType.ToAbbrevName,
                                                       Data^.OrderDoc.OrderType.ToString,
                                                       Data^.OrderDoc.Description]);
              if (Data^.OrderDoc.ExtendedOptions.Subordination = suMotherOrder) then
                CellText := CellText + C_MOTHER_ORDER;
            end;
          COL_CALCTYPE:
            CellText := '';
          COL_VALUE:
            CellText := Data^.OrderDoc.ToValueString;
        end;
      ntCondition:
        case Column of
          COL_ITEMS:
            if (Data^.ConditionDoc.CondType = ctRealtimeValue) then
              CellText := Data^.ConditionDoc.Description + ' (' + Data^.ConditionDoc.ValueArray[ctRealtimeValue].ToString + ') / ' + Data^.ConditionDoc.CondType.ToString + IfThen(Data^.ConditionDoc.Bypass, ' [BYPASS]')
            else
              CellText := Data^.ConditionDoc.Description + ' / ' + Data^.ConditionDoc.CondType.ToString + IfThen(Data^.ConditionDoc.Bypass, ' [BYPASS]');
          COL_CALCTYPE:
            CellText := Data^.ConditionDoc.ToString;
          COL_VALUE:
            CellText := Data^.ConditionDoc.ToValueString;
        end;
      ntAlgos:
        case Column of
          COL_ITEMS:
            CellText := Data^.AlgosDoc.Name + ' ' + Data^.RecordId.ToString;
          COL_CALCTYPE:
            CellText := '';
          COL_VALUE:
            CellText := Format('%.5f', [Data^.ReValue]);
        end;
      ntFactor:
        case Column of
          COL_ITEMS:
            if Data^.FactorDoc.UseInAutoOrder then
              CellText := rsUseInAutoorder
            else
            begin
              CellText := Data^.FactorDoc.InstrumentName;
              if (Data^.FactorDoc.BrokerType = TBrokerType.brIB) then
                if (Data^.FactorDoc.ContractType = stOption.ToString) or
                  (Data^.FactorDoc.ContractType = stFuture.ToString) then
                  CellText := Data^.FactorDoc.LocalSymbol;
            end;
          COL_VALUE:
            CellText := Data^.FactorDoc.ToValueString;
        end;
    end;
end;

{ TMarkedNode }

constructor TMarkedNode.Create(aRecordId: Integer; aDocType: TDocType);
begin
  RecordId := aRecordId;
  DocType := aDocType;
end;

function TMarkedNode.Equal(aTreeData: TTreeData): Boolean;
begin
  Result := (Self.DocType = aTreeData.DocType);
  if Result then
    case Self.DocType of
      ntQualifier:
        Result := Self.RecordId = aTreeData.Qualifier.RecordId;
      ntAutoTrade:
        Result := Self.RecordId = aTreeData.AutoTrade.RecordId;
      ntOrderGroupSet:
        Result := Self.RecordId = aTreeData.OrderGroupSetDoc.RecordId;
    else
      Result := Self.RecordId = aTreeData.RecordId;
    end;
end;


end.
