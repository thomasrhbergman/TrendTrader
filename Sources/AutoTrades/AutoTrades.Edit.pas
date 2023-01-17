unit AutoTrades.Edit;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids, DaModule,
  System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, System.ImageList, Vcl.Menus, Vcl.Mask,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging,{$ENDIF} Vcl.ImgList, System.Math, Vcl.DBCtrls, Winapi.ActiveX, System.UITypes,
  VirtualTrees, BrokerHelperAbstr, DebugWriter, HtmlLib, Scanner.Types, CustomForms, MessageDialog, Global.Resources,
  Vcl.Samples.Spin, Global.Types, AutoTrades.Types, DaImages, Vcl.Imaging.pngimage, Vcl.VirtualImage, Utils,
  Vcl.NumberBox, IABSocketAPI_const, Common.Types, ListForm;
{$ENDREGION}

type
  TfrmAutoTradesEdit = class(TCustomForm, IAutoTrade)
    aCancel: TAction;
    ActionList: TActionList;
    aSave: TAction;
    aShowScanner: TAction;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    edName: TEdit;
    lblName: TLabel;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    cbActive: TCheckBox;
    lblQualifier: TLabel;
    edQualifier: TEdit;
    btnSelectQualifier: TSpeedButton;
    lblCandidate: TLabel;
    edCandidate: TEdit;
    btnSelectCandidate: TSpeedButton;
    lblQuantity: TLabel;
    edQuantity: TEdit;
    btnSelectQuantity: TSpeedButton;
    lblOrderTemplate: TLabel;
    edOrderTemplate: TEdit;
    btnSelectOrderTemplate: TSpeedButton;
    procedure aCancelExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OnGUIToAutoTradeInfo(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSelectQualifierClick(Sender: TObject);
    procedure btnSelectCandidateClick(Sender: TObject);
    procedure btnSelectQuantityClick(Sender: TObject);
    procedure btnSelectOrderTemplateClick(Sender: TObject);
  private
    FLoaded: Boolean;
    FAutoTradeInfo: TAutoTradeInfo;
    function CheckData: Boolean;
    procedure AutoTradeInfoToGUI;

    //implementation ICustomInterface
    function GetInstance: TObject;
    //implementation IAutoTrade
    function GetAutoTradeInfo: TAutoTradeInfo;
    function GetTradesState: TTradesState;
    procedure CloseAutoTrade(const aSilenceMode: Boolean = False);
    procedure SetAutoTradeInfo(const aAutoTradeInfo: TAutoTradeInfo);
    procedure SetTradesState(const aValue: TTradesState);
  public
    class function ShowEditForm(aItem: TBaseClass; aDialogMode: TDialogMode): TModalResult; override;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

uses
  Scanner.Main;

{$R *.dfm}

class function TfrmAutoTradesEdit.ShowEditForm(aItem: TBaseClass; aDialogMode: TDialogMode): TModalResult;
var
  frmAutoTradesEdit: TfrmAutoTradesEdit;
begin
  frmAutoTradesEdit := TfrmAutoTradesEdit.Create(nil);
  try
    frmAutoTradesEdit.DialogMode := aDialogMode;
    frmAutoTradesEdit.FAutoTradeInfo.AssignFrom(TAutoTradeInfo(aItem));
    frmAutoTradesEdit.FLoaded := True;
    try
      frmAutoTradesEdit.Initialize;
    finally
      frmAutoTradesEdit.FLoaded := False;
    end;
    Result := frmAutoTradesEdit.ShowModal;
    if (Result = mrOk) then
    begin
      frmAutoTradesEdit.Denitialize;
      TAutoTradeInfo(aItem).AssignFrom(frmAutoTradesEdit.FAutoTradeInfo);
    end;
  finally
    FreeAndNil(frmAutoTradesEdit);
  end;
end;

procedure TfrmAutoTradesEdit.Initialize;
begin
  if (FAutoTradeInfo.InstanceNum <= 0) then
    FAutoTradeInfo.InstanceNum := -General.GetNextInstanceNum;
  AutoTradeInfoToGUI;
end;

procedure TfrmAutoTradesEdit.Denitialize;
begin
  OnGUIToAutoTradeInfo(nil);
end;

procedure TfrmAutoTradesEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if (ModalResult = mrOk) then
  begin
    Denitialize;
    CanClose := CheckData;
  end;
end;

procedure TfrmAutoTradesEdit.FormCreate(Sender: TObject);
begin
  inherited;
  FAutoTradeInfo := TAutoTradeInfo.Create;
end;

procedure TfrmAutoTradesEdit.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FAutoTradeInfo);
end;

function TfrmAutoTradesEdit.CheckData: Boolean;
resourcestring
  rsTotalOrderAmount = 'Total order amount is 0. AutoTrade is not active.';

  function CheckRequired: Boolean;
  var
    Problems: string;
  begin
    Problems := '';
//    if (FAutoTradeInfo.TotalOrderAmount = 0) then
//    begin
//      SetFocusSafely(seTotalOrderAmount);
//      Problems := Format(rcRequiredValue, ['Total order amount']);
//    end;


    Result := Problems.IsEmpty;
    if not Result then
      TMessageDialog.ShowWarning(Problems);
  end;

begin
  Result := CheckRequired;
  {if Result then
    if (FAutoTradeInfo.TotalOrderAmount = 0) then
      TMessageDialog.ShowWarning(rsTotalOrderAmount); }
end;

procedure TfrmAutoTradesEdit.aSaveExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmAutoTradesEdit.AutoTradeInfoToGUI;
begin
  edName.Text                       := FAutoTradeInfo.Name;
  cbActive.Checked                  := FAutoTradeInfo.Active;
  edQualifier.Text                  := FAutoTradeInfo.Qualifier.Name;
  edQuantity.Text                   := FAutoTradeInfo.Quantity.Name;
  edCandidate.Text                  := FAutoTradeInfo.Candidate.Name;
  edOrderTemplate.Text              := FAutoTradeInfo.OrderTemplate.Name;
end;

procedure TfrmAutoTradesEdit.btnSelectCandidateClick(Sender: TObject);
var lRecordId: Integer;
begin
  inherited;
  lRecordId := ListFormFactory.Select(ntCandidates, FAutoTradeInfo.Candidate.RecordId);
  if lRecordId > 0 then
  begin
    FAutoTradeInfo.Candidate.FromDB(lRecordId);
    edCandidate.Text := FAutoTradeInfo.Candidate.Name;
  end;
end;

procedure TfrmAutoTradesEdit.btnSelectOrderTemplateClick(Sender: TObject);
var lRecordId: Integer;
begin
  inherited;
  lRecordId := ListFormFactory.Select(ntOrderTemplate, FAutoTradeInfo.OrderTemplate.RecordId);
  if lRecordId > 0 then
  begin
    FAutoTradeInfo.OrderTemplate.FromDB(lRecordId);
    edOrderTemplate.Text := FAutoTradeInfo.OrderTemplate.Name;
  end;
end;

procedure TfrmAutoTradesEdit.btnSelectQualifierClick(Sender: TObject);
var lRecordId: Integer;
begin
  inherited;
  lRecordId := ListFormFactory.Select(ntQualifier, FAutoTradeInfo.Qualifier.RecordId);
  if lRecordId > 0 then
  begin
    FAutoTradeInfo.Qualifier.FromDB(lRecordId);
    edQualifier.Text := FAutoTradeInfo.Qualifier.Name;
  end;
end;

procedure TfrmAutoTradesEdit.btnSelectQuantityClick(Sender: TObject);
var lRecordId: Integer;
begin
  inherited;
  lRecordId := ListFormFactory.Select(ntQuantities, FAutoTradeInfo.Quantity.RecordId);
  if lRecordId > 0 then
  begin
    FAutoTradeInfo.Quantity.FromDB(lRecordId);
    edQuantity.Text := FAutoTradeInfo.Quantity.Name;
  end;
end;

procedure TfrmAutoTradesEdit.OnGUIToAutoTradeInfo(Sender: TObject);
begin
  if not FLoaded then
  begin
    FAutoTradeInfo.Name                    := edName.Text;
    FAutoTradeInfo.Active                  := cbActive.Checked;
  end;
end;

procedure TfrmAutoTradesEdit.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TfrmAutoTradesEdit.GetAutoTradeInfo: TAutoTradeInfo;
begin
  Result := FAutoTradeInfo;
end;

procedure TfrmAutoTradesEdit.SetAutoTradeInfo(const aAutoTradeInfo: TAutoTradeInfo);
begin
  FLoaded := True;
  try
    FAutoTradeInfo.AssignFrom(aAutoTradeInfo);
    AutoTradeInfoToGUI;
  finally
    FLoaded := False;
  end;
end;

function TfrmAutoTradesEdit.GetInstance: TObject;
begin
  Result := Self;
end;

function TfrmAutoTradesEdit.GetTradesState: TTradesState;
begin
  Result := TTradesState.tsSuspended;
end;

procedure TfrmAutoTradesEdit.CloseAutoTrade(const aSilenceMode: Boolean);
begin
  //nothing
end;

procedure TfrmAutoTradesEdit.SetTradesState(const aValue: TTradesState);
begin
  //nothing
end;

initialization
  ListFormFactory.RegisterList(ntAutotrade, TAutoTradeInfo, TfrmAutoTradesEdit);

end.
