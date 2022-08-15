unit Frame.ConditionHistory;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, IABFunctions, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons,
  Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, Document, System.Generics.Collections,
  System.Generics.Defaults, DebugWriter, CustomForms, Global.Types, System.IniFiles, System.Math, System.Actions,
  Vcl.ActnList, Vcl.Menus, Vcl.ExtDlgs, Vcl.Printers, MessageDialog, VirtualTrees.ExportHelper, DaImages, Common.Types,
  Monitor.Types, Publishers, Utils, DaModule.Utils, Global.Resources, HtmlLib, HtmlConsts,InformationDialog,
  System.DateUtils, Frame.Custom, System.IOUtils, Vcl.ComCtrls, VirtualTrees.Types, IABFunctions.Helpers;
{$ENDREGION}

type
  TframeConditionHistory = class(TframeCustom)
    aColumnSettings: TAction;
    aExportToCSV: TAction;
    aExportToExcel: TAction;
    alMain: TActionList;
    aPrint: TAction;
    aRefresh: TAction;
    btnColumnSettings: TBitBtn;
    btnExportToCSV: TBitBtn;
    btnExportToExcel: TBitBtn;
    btnPrint: TBitBtn;
    btnRefresh: TBitBtn;
    pnlOptions: TPanel;
    procedure aColumnSettingsExecute(Sender: TObject);
    procedure aExportToCSVExecute(Sender: TObject);
    procedure aExportToExcelExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private const
    C_COL_COND_TYPE       = 0;
    C_COL_TIME_STAMP      = 1;
    C_COL_ACTIVE          = 2;
    C_COL_IS_CONDITION    = 3;
    C_COL_CALC_VALUE      = 4;
    C_COL_COND_LIMIT      = 5;
    C_COL_INEQUALITY_TYPE = 6;
    C_COL_VALUE1          = 7;
    C_COL_VALUE2          = 8;
    C_COL_TICK_TYPE1      = 9;
    C_COL_TICK_TYPE2      = 10;
    C_COL_TYPE_OPERATION  = 11;
    C_COL_FACTORS         = 12;

    C_IDENTITY_NAME = 'frameConditionHistory';
  private
    FOwnerNode: PVirtualNode;
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize; override;
    procedure Deinitialize; override;
    procedure RefreshHistory;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OwnerNode : PVirtualNode read FOwnerNode write FOwnerNode;
  end;

implementation

{$R *.dfm}

{ TframeConditionHistory }

constructor TframeConditionHistory.Create(AOwner: TComponent);
begin
  inherited;
  vstTree.NodeDataSize := SizeOf(TConditionHistory);
end;

destructor TframeConditionHistory.Destroy;
begin
  vstTree.Clear;
  inherited;
end;

procedure TframeConditionHistory.Initialize;
begin
  inherited;

end;

procedure TframeConditionHistory.RefreshHistory;
var
  OwnerData: PTreeData;
  Data: PConditionHistory;
  Node: PVirtualNode;
begin
  inherited;
  if Assigned(OwnerNode) then
  begin
    OwnerData := OwnerNode^.GetData;
    if Assigned(OwnerData) and Assigned(OwnerData.ConditionDoc) then
    begin
      vstTree.BeginUpdate;
      vstTree.Clear;
      try
        for var hist in OwnerData.ConditionDoc.History do
        begin
          Node := vstTree.AddChild(nil);
          Data := Node.GetData;
          Data.Active         := hist.Active;
          Data.IsCondition    := hist.IsCondition;
          Data.TimeStamp      := hist.TimeStamp;
          Data.CondType       := hist.CondType;
          Data.CalcValue      := hist.CalcValue;
          Data.InequalityType := hist.InequalityType;
          Data.CondLimit      := hist.CondLimit;
          Data.TickType1      := hist.TickType1;
          Data.TickType2      := hist.TickType2;
          Data.TypeOperation  := hist.TypeOperation;
          Data.Value1         := hist.Value1;
          Data.Value2         := hist.Value2;
          Data.Factors        := hist.Factors;
        end;
      finally
        vstTree.EndUpdate;
      end;
    end;
  end
end;

procedure TframeConditionHistory.Deinitialize;
begin

  inherited;
end;

function TframeConditionHistory.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TframeConditionHistory.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PConditionHistory;
begin
  inherited;
  Data := Node^.GetData;
  if Assigned(Data) then
    Data^.Clear;
end;

procedure TframeConditionHistory.vstTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PConditionHistory;
begin
  Data := Node^.GetData;
  case Column of
    C_COL_COND_TYPE:
      CellText := Data.CondType.ToString;
    C_COL_TIME_STAMP:
      CellText := FormatDateTime('hh:nn:ss.zzz', Data.TimeStamp);
    C_COL_CALC_VALUE:
      CellText := SimpleRoundTo(Data.CalcValue, -C_DECIMALS).ToString;
    C_COL_COND_LIMIT:
      CellText := SimpleRoundTo(Data.CondLimit, -C_DECIMALS).ToString;
    C_COL_INEQUALITY_TYPE:
      CellText := Data.InequalityType.ToString;
    C_COL_VALUE1:
      CellText := SimpleRoundTo(Data.Value1, -C_DECIMALS).ToString;
    C_COL_VALUE2:
      CellText := SimpleRoundTo(Data.Value2, -C_DECIMALS).ToString;
    C_COL_TICK_TYPE1:
      CellText := Data.TickType1.ToString;
    C_COL_TICK_TYPE2:
      CellText := Data.TickType2.ToString;
    C_COL_IS_CONDITION:
      CellText := BoolToStr(Data.IsCondition, True);
    C_COL_ACTIVE:
      CellText := BoolToStr(Data.Active, True);
    C_COL_TYPE_OPERATION:
      CellText := Data.TypeOperation.ToString;
    C_COL_FACTORS:
      CellText := Data.Factors;
  end;
end;

procedure TframeConditionHistory.vstTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PConditionHistory;
begin
  Data1 := Node1^.GetData;
  Data2 := Node2^.GetData;
  case Column of
    C_COL_COND_TYPE:
      Result := CompareText(Data1.CondType.ToString, Data2.CondType.ToString);
    C_COL_TIME_STAMP:
      Result := CompareValue(Data1.TimeStamp, Data2.TimeStamp);
    C_COL_CALC_VALUE:
      Result := CompareValue(Data1.CalcValue, Data2.CalcValue);
    C_COL_COND_LIMIT:
      Result := CompareValue(Data1.CondLimit, Data2.CondLimit);
    C_COL_INEQUALITY_TYPE:
      Result := CompareText(Data1.InequalityType.ToString, Data2.InequalityType.ToString);
    C_COL_VALUE1:
      Result := CompareValue(Data1.Value1, Data2.Value1);
    C_COL_VALUE2:
      Result := CompareValue(Data1.Value2, Data2.Value2);
    C_COL_TICK_TYPE1:
      Result := CompareText(Data1.TickType1.ToString, Data2.TickType1.ToString);
    C_COL_TICK_TYPE2:
      Result := CompareText(Data1.TickType2.ToString, Data2.TickType2.ToString);
    C_COL_IS_CONDITION:
      Result := CompareText(BoolToStr(Data1.IsCondition, True), BoolToStr(Data2.IsCondition, True));
    C_COL_ACTIVE:
      Result := CompareText(BoolToStr(Data1.Active, True), BoolToStr(Data2.Active, True));
    C_COL_TYPE_OPERATION:
      Result := CompareText(Data1.TypeOperation.ToString, Data2.TypeOperation.ToString);
    C_COL_FACTORS:
      Result := CompareText(Data1.Factors, Data2.Factors);
  end;
end;

procedure TframeConditionHistory.vstTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PConditionHistory;
begin
  inherited;
  Data := Node^.GetData;
  case Column of
    C_COL_IS_CONDITION:
      if Data.IsCondition then
        TargetCanvas.Font.Color := clGreen
      else
        TargetCanvas.Font.Color := clRed;
    C_COL_ACTIVE:
      if Data.Active then
        TargetCanvas.Font.Color := clGreen
      else
        TargetCanvas.Font.Color := clRed;
  end;
end;

procedure TframeConditionHistory.aColumnSettingsExecute(Sender: TObject);
begin
  TStoreHelper.ShowColumnSettings(vstTree, C_IDENTITY_NAME, 0);
end;

procedure TframeConditionHistory.aExportToCSVExecute(Sender: TObject);
begin
  ExportToCSV;
end;

procedure TframeConditionHistory.aExportToExcelExecute(Sender: TObject);
begin
  ExportToExcel;
end;

procedure TframeConditionHistory.aPrintExecute(Sender: TObject);
begin
  Print;
end;

procedure TframeConditionHistory.aRefreshExecute(Sender: TObject);
begin
  inherited;
  RefreshHistory;
end;

end.
