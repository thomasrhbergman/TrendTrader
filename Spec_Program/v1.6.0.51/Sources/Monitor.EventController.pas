unit Monitor.EventController;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.ActnList,
  System.Generics.Defaults, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.ComCtrls,
  System.Generics.Collections, System.Actions, Vcl.StdCtrls,Vcl.ExtCtrls, Vcl.Taskbar, System.Win.TaskbarCore,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging,{$ENDIF} CustomForms, DebugWriter, Entity.Sokid, Common.Types, DaImages,
  Vcl.Menus, Global.Types;
{$ENDREGION}

type
  TfrmEventController = class(TCustomForm)
    aAbortCheckInstruments: TAction;
    aAbortPriceUpdate: TAction;
    ActionList: TActionList;
    btnAbortCheckInstruments: TButton;
    btnAbortUpdate: TButton;
    lblCheckInstruments: TLabel;
    lblPriceUpdate: TLabel;
    pbCheckInstruments: TProgressBar;
    pbPriceUpdate: TProgressBar;
    pnlCheckInstruments: TPanel;
    pnlPriceUpdate: TPanel;
    Taskbar: TTaskbar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure OnAbortExecute(Sender: TObject);
    procedure OnAbortUpdate(Sender: TObject);
  private
    function DoAbort(const aTypeEvent: TTypeEvent): Boolean;
    procedure DoFinishingProgress(const aMaxPosition: Integer; const aTypeEvent: TTypeEvent);
    procedure DoProgress(const aPosition: Integer; const aTypeEvent: TTypeEvent);
    procedure DoStartProgress(const aMaxPosition: Integer; const aTypeEvent: TTypeEvent);
  public
    procedure Initialize(const aTypeEvent: TTypeEvent);
    class procedure ShowDocument(const aTypeEvent: TTypeEvent);
  end;

implementation

var
  frmEventController: TfrmEventController;

{$R *.dfm}

{ TfrmEventController }

class procedure TfrmEventController.ShowDocument(const aTypeEvent: TTypeEvent);
begin
  if not Assigned(frmEventController) then
  begin
    frmEventController := TfrmEventController.Create(Application);
    frmEventController.Width := Application.MainForm.Width;
    frmEventController.Top   := Application.MainForm.Top + Application.MainForm.Height;
    frmEventController.Left  := Application.MainForm.Left;
  end;
  frmEventController.Initialize(aTypeEvent);
  frmEventController.AutoSize := True;
  frmEventController.Show;
end;

procedure TfrmEventController.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Taskbar.ProgressState := TTaskBarProgressState.None;
  Action := caFree;
end;

procedure TfrmEventController.FormDestroy(Sender: TObject);
begin
  SokidList.OnAbort             := nil;
  SokidList.OnFinishingProgress := nil;
  SokidList.OnProgress          := nil;
  SokidList.OnStartProgress     := nil;
  frmEventController := nil;
end;

procedure TfrmEventController.Initialize(const aTypeEvent: TTypeEvent);
begin
  SokidList.OnAbort             := DoAbort;
  SokidList.OnFinishingProgress := DoFinishingProgress;
  SokidList.OnProgress          := DoProgress;
  SokidList.OnStartProgress     := DoStartProgress;
  case aTypeEvent of
    teCheckLastPrice:
      begin
        pnlPriceUpdate.Visible := True;
        aAbortPriceUpdate.Tag := 0;
      end;
    teCheckInstruments:
      begin
        pnlCheckInstruments.Visible := True;
        aAbortCheckInstruments.Tag := 0;
      end;
  end;
  Taskbar.ProgressState := TTaskBarProgressState.Normal;
end;

procedure TfrmEventController.OnAbortExecute(Sender: TObject);
begin
  TAction(Sender).Tag := 1;
end;

procedure TfrmEventController.OnAbortUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := TAction(Sender).Tag = 0;
end;

function TfrmEventController.DoAbort(const aTypeEvent: TTypeEvent): Boolean;
begin
  Result := False;
  case aTypeEvent of
    teCheckLastPrice:
      Result := aAbortPriceUpdate.Tag = 1;
    teCheckInstruments:
      Result := aAbortCheckInstruments.Tag = 1;
  end;
end;

procedure TfrmEventController.DoFinishingProgress(const aMaxPosition: Integer; const aTypeEvent: TTypeEvent);
begin
  case aTypeEvent of
    teCheckLastPrice:
      begin
        pbPriceUpdate.Position := 0;
        aAbortPriceUpdate.Tag := 1;
        aAbortPriceUpdate.Caption := 'Сompleted';
      end;
    teCheckInstruments:
      begin
        pbCheckInstruments.Position := 0;
        aAbortCheckInstruments.Tag := 1;
        aAbortCheckInstruments.Caption := 'Сompleted';
      end;
  end;
end;

procedure TfrmEventController.DoProgress(const aPosition: Integer; const aTypeEvent: TTypeEvent);
begin
  case aTypeEvent of
    teCheckLastPrice:
      begin
        if (aPosition > pbPriceUpdate.Max) then
          pbPriceUpdate.Max := Trunc(aPosition * 1.5);
        if (aPosition mod 10 = 0) then
          pbPriceUpdate.Position := aPosition;
      end;
    teCheckInstruments:
      begin
        if (aPosition > pbCheckInstruments.Max) then
          pbCheckInstruments.Max := Trunc(aPosition * 1.5);
        if (aPosition mod 10 = 0) then
          pbCheckInstruments.Position := aPosition;
      end;
  end;
  Taskbar.ProgressValue    := pbPriceUpdate.Position + pbCheckInstruments.Position;
  Taskbar.ProgressMaxValue := pbPriceUpdate.Max + pbCheckInstruments.Max;
end;

procedure TfrmEventController.DoStartProgress(const aMaxPosition: Integer; const aTypeEvent: TTypeEvent);
begin
  if (aMaxPosition > 0) then
    case aTypeEvent of
      teCheckLastPrice:
        begin
          pbPriceUpdate.Max := aMaxPosition;
          aAbortPriceUpdate.Caption := 'Abort';
        end;
      teCheckInstruments:
        begin
          pbCheckInstruments.Max := aMaxPosition;
          aAbortCheckInstruments.Caption := 'Abort';
        end;
    end;
end;

end.
