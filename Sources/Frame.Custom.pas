unit Frame.Custom;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.CheckLst, Vcl.Samples.Spin, Vcl.Buttons, Vcl.ExtCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Threading, System.Generics.Collections, Vcl.ActnList,
  System.Generics.Defaults, DebugWriter, Global.Types, System.IniFiles, System.Math, System.Actions, Vcl.Menus,
  Vcl.ExtDlgs, Vcl.Printers, MessageDialog, VirtualTrees.ExportHelper, DaImages, Monitor.Types, Common.Types,
  MonitorTree.Helper;
{$ENDREGION}

type
  TFrameParameter = (fpFiltered);
  TFrameParameters = set of TFrameParameter;

  TframeCustom = class(TFrame)
    vstTree: TVirtualStringTree;
    procedure vstTreeColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure vstTreeHeaderDragged(Sender: TVTHeader; Column: TColumnIndex; OldPosition: Integer);
    procedure vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private const
    C_IDENTITY_NAME = 'frameCustom';
    C_IDENTITY_COLUMNS_NAME = '.Columns';
  protected
    FFrameParameters: TFrameParameters;

    function GetIdentityName: string; virtual;
    procedure SetFilter(const aAutoTradesCommon: TAutoTradesCommon); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize; virtual;
    procedure Deinitialize; virtual;

    function GetFirstVisibleNode: PVirtualNode;
    procedure Print;
    procedure ExportToCSV;
    procedure ExportToExcel;
    procedure ColumnSettings;
    property Parameters: TFrameParameters read FFrameParameters write FFrameParameters;
  end;

implementation

{$R *.dfm}

{ TframeCustom }

constructor TframeCustom.Create(AOwner: TComponent);
begin
  inherited;
//  vstTree.NodeDataSize := SizeOf();
end;

destructor TframeCustom.Destroy;
begin

  inherited;  
end;

procedure TframeCustom.Initialize;
begin
  TMonitorTree.Initialize(vstTree);
  TStoreHelper.LoadFromXml(vstTree, GetIdentityName + C_IDENTITY_COLUMNS_NAME);
  Self.Caption := GetIdentityName;
end;

procedure TframeCustom.Deinitialize;
begin
  TStoreHelper.SaveToXml(vstTree, GetIdentityName + C_IDENTITY_COLUMNS_NAME);
end;

function TframeCustom.GetFirstVisibleNode: PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result := nil;
  Node := vstTree.GetFirstChild(vstTree.RootNode);
  while Assigned(Node) do
  begin
    if vstTree.IsVisible[Node] then
      Exit(Node);
    Node := Node.NextSibling;
  end;
end;

function TframeCustom.GetIdentityName: string;
begin
  Result := C_IDENTITY_NAME;
end;

procedure TframeCustom.vstTreeColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  TStoreHelper.SaveToXml(vstTree, GetIdentityName + C_IDENTITY_COLUMNS_NAME);
end;

procedure TframeCustom.vstTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
//
end;

procedure TframeCustom.vstTreeHeaderDragged(Sender: TVTHeader; Column: TColumnIndex; OldPosition: Integer);
begin
  TStoreHelper.SaveToXml(vstTree, GetIdentityName + C_IDENTITY_COLUMNS_NAME);
end;

procedure TframeCustom.ColumnSettings;
begin
  if TStoreHelper.ShowColumnSettings(vstTree, GetIdentityName, 0) = mrOk then
    TStoreHelper.SaveToXml(vstTree, GetIdentityName + C_IDENTITY_COLUMNS_NAME);
end;

procedure TframeCustom.ExportToCSV;
begin
  TExcelExportHelper.ExportToCSV(vstTree, Self.Caption);
end;

procedure TframeCustom.ExportToExcel;
begin
  TExcelExportHelper.ExportToExcel(vstTree, Self.Caption);
end;

procedure TframeCustom.Print;
begin
  Printer.Orientation := poLandscape;
  vstTree.Print(Printer, True);
end;

procedure TframeCustom.SetFilter(const aAutoTradesCommon: TAutoTradesCommon);
begin

end;

end.
