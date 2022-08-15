unit MessageDialog;
{$WARN SYMBOL_PLATFORM OFF}

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Dialogs, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  Vcl.Forms, System.SysUtils, Vcl.Imaging.jpeg, System.IOUtils, Vcl.ExtDlgs;
{$ENDREGION}

type
  TMessageDialog = class(TTaskDialog)
  private
    FDialogType: TMsgDlgType;
    procedure DoSave(aText: string);
  public
    class procedure ShowError(AErrorText: string); overload;
    class procedure ShowError(AErrorText: string; ADetailsText: string); overload;
    class procedure ShowWarning(ANotifyText: string); overload;
    class procedure ShowWarning(ANotifyText, ADetailsText: string); overload;
    class procedure ShowInfo(AInfoText: string); overload;
    class procedure ShowInfo(AInfoText, ADetailsText: string); overload;
    class function ShowQuestion(AMessageText: string): TModalResult; overload;
    class function ShowQuestion(AMessageText: string; AButtons: TMsgDlgButtons; ADefaultCancel: Boolean = False): TModalResult; overload;
    class function ShowDialog(AMessageText: string; ADialogType: TMsgDlgType; AButtons: TMsgDlgButtons;
                              ADetailsText: string = ''; ADefaultCancel: Boolean = False): TModalResult;

    property DialogType: TMsgDlgType read FDialogType write FDialogType;
  end;

implementation

const
  mrSave = 101;

{ TMessageDialog }

class function TMessageDialog.ShowDialog(AMessageText: string; ADialogType: TMsgDlgType; AButtons: TMsgDlgButtons; ADetailsText: string = '';
  ADefaultCancel: Boolean = False): TModalResult;
var
  DlgBtn: TMsgDlgBtn;
begin
  Result := mrNone;
  with TMessageDialog.Create(nil) do
    try
      Caption := Application.Title;
      CommonButtons := [];
      Title   := '';
      Text    := AMessageText;
      DialogType := ADialogType;

      for DlgBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
        if DlgBtn in AButtons then
          case DlgBtn of
            mbCancel : CommonButtons := CommonButtons + [tcbCancel];
            mbClose  : CommonButtons := CommonButtons + [tcbClose];
            mbNo     : CommonButtons := CommonButtons + [tcbNo];
            mbOK     : CommonButtons := CommonButtons + [tcbOk];
            mbRetry  : CommonButtons := CommonButtons + [tcbRetry];
            mbYes    : CommonButtons := CommonButtons + [tcbYes];
          end;

      if ADefaultCancel then
      begin
        if (mbCancel in AButtons) then
          DefaultButton := tcbCancel
        else if (mbNo in AButtons) then
          DefaultButton := tcbNo;
      end
      else
      begin
        if (mbOk in AButtons) then
          DefaultButton := tcbOk
        else if (mbYes in AButtons) then
          DefaultButton := tcbYes;
      end;

      case ADialogType of
        mtError        : MainIcon := tdiError;
        mtInformation  : MainIcon := tdiInformation;
        mtWarning      : MainIcon := tdiWarning;
        mtConfirmation :
          begin
            CustomMainIcon.ReleaseHandle;
            CustomMainIcon.Handle := LoadIcon(0, IDI_QUESTION);
            Flags := [tfUseHiconMain];
          end;
      end;

      if not ADetailsText.IsEmpty then
      begin
        ExpandButtonCaption := 'Technical Information';
        ExpandedText := ADetailsText;
        with TTaskDialogButtonItem(Buttons.Add) do
        begin
          Caption     := 'Save';
          ModalResult := mrSave;
        end;
      end;

      if Execute then
      begin
        Result := ModalResult;
        if (ModalResult = mrSave) then
          DoSave(ADetailsText);
      end;
    finally
      Free;
    end;
end;

class procedure TMessageDialog.ShowError(AErrorText, ADetailsText: string);
begin
  ShowDialog(AErrorText, mtError, [mbOk], ADetailsText);
end;

class procedure TMessageDialog.ShowError(AErrorText: string);
begin
  ShowDialog(AErrorText, mtError, [mbOk]);
end;

class procedure TMessageDialog.ShowInfo(AInfoText: string);
begin
  ShowDialog(AInfoText, mtInformation, [mbOk]);
end;

class procedure TMessageDialog.ShowInfo(AInfoText, ADetailsText: string);
begin
  ShowDialog(AInfoText, mtInformation, [mbOk], ADetailsText);
end;

class procedure TMessageDialog.ShowWarning(ANotifyText: string);
begin
  ShowDialog(ANotifyText, mtWarning, [mbOk]);
end;

class procedure TMessageDialog.ShowWarning(ANotifyText, ADetailsText: string);
begin
  ShowDialog(ANotifyText, mtWarning, [mbOk], ADetailsText);
end;

class function TMessageDialog.ShowQuestion(AMessageText: string; AButtons: TMsgDlgButtons; ADefaultCancel: Boolean = False): TModalResult;
begin
  Result := ShowDialog(AMessageText, mtConfirmation, AButtons, '', ADefaultCancel);
end;

class function TMessageDialog.ShowQuestion(AMessageText: string): TModalResult;
begin
  Result := ShowQuestion(AMessageText, [mbYes, mbNo]);
end;

procedure TMessageDialog.DoSave(aText: string);
begin
  with TSaveTextFileDialog.Create(nil) do
  try
    DefaultExt := 'txt';
    Filter := '*.txt';
    FileName := 'Technical InformationDialog.txt';
    if Execute then
      TFile.WriteAllText(FileName, aText);
  finally
    Free;
  end;
end;

end.

