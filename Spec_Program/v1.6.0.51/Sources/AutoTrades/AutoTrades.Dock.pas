unit AutoTrades.Dock;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Buttons, Vcl.DBGrids, DaModule,
  System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, Vcl.ComCtrls, System.ImageList, Vcl.ImgList, DaImages;
{$ENDREGION}

type
  TfrmAutoTradesDock = class(TCustomForm)
    ActionListMain: TActionList;
    ImageList: TImageList;
    pcTrades: TPageControl;
    pnlTop: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure pcTradesUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
  private
    FCanClose: Boolean;
  public
    class function GetDock: TfrmAutoTradesDock;
  end;

implementation

{$R *.dfm}

var
  frmAutoTradesDock: TfrmAutoTradesDock;

procedure TfrmAutoTradesDock.FormShow(Sender: TObject);
var
  MainForm: TForm;
begin
  inherited;
  MainForm  := Application.MainForm;
  Self.Left := MainForm.Left + MainForm.Width div 2;
  Self.Top  := MainForm.Top + MainForm.Height div 3 * 2;
end;

class function TfrmAutoTradesDock.GetDock: TfrmAutoTradesDock;
begin
  if not Assigned(frmAutoTradesDock) then
    frmAutoTradesDock := TfrmAutoTradesDock.Create(Application);
  Result := frmAutoTradesDock;
end;

procedure TfrmAutoTradesDock.pcTradesUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  if (pcTrades.PageCount <= 1) then
  begin
    FCanClose := True;
    Close;
  end
  else
    FCanClose := False;
end;

procedure TfrmAutoTradesDock.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (pcTrades.PageCount = 0) or FCanClose;
end;

end.
