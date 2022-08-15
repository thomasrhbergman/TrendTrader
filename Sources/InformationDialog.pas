unit InformationDialog;

interface

{$REGION 'Region uses'}
uses
  Windows, ActiveX, Buttons, Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms, Graphics, Messages, SHDocVw,
  System.SysUtils, System.Variants, Vcl.ActnList, System.Actions, Vcl.OleCtrls, System.IOUtils, MessageDialog,
  DebugWriter, HtmlConsts, HtmlLib, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Vcl.ExtDlgs,
  Vcl.StdCtrls, DaImages, Common.Types, Global.Types, Publishers;
{$ENDREGION}

type
  TInformationDialog = class(TCustomForm)
    ActionList         : TActionList;
    aExit              : TAction;
    aSave              : TAction;
    btnOk              : TBitBtn;
    btnSave            : TBitBtn;
    pnlBottom          : TPanel;
    pnlMain            : TPanel;
    SaveTextFileDialog : TSaveTextFileDialog;
    wbMessage          : TWebBrowser;
    procedure aExitExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure wbMessageBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private const
    C_ERROR_ACTION_RECOMMENDATION = 'Recommendation';
    C_IDENTITY_NAME = 'InformationDialog';
  private
    FMessageText : string;
    FIdentityName : string;
  protected
    function GetIdentityName: string; override;
  public
    procedure Initialize;
    procedure Deinitialize;

    property MessageText: string read FMessageText write FMessageText;
    property IdentityName: string read FIdentityName write FIdentityName;

    class procedure ShowXML(const AMessageText: string; AIdentityName: string);
    class procedure ShowMessage(AMessageText: string; AIdentityName: string);
  end;

implementation

{$R *.dfm}

class procedure TInformationDialog.ShowMessage(AMessageText: string; AIdentityName: string);
begin
  if AMessageText.IsEmpty then
    TMessageDialog.ShowWarning('No data to display')
  else
  begin
    with TInformationDialog.Create(Application) do
    try
      MessageText  := AMessageText;
      IdentityName := AIdentityName;
      Caption      := Application.Title;
      Initialize;
      Show;
    finally
//      Free;
    end;
  end;
end;

class procedure TInformationDialog.ShowXML(const AMessageText: string; AIdentityName: string);
begin
  if AMessageText.IsEmpty then
    ShowMessage(AMessageText, AIdentityName)
  else
    with TInformationDialog.Create(Application) do
    begin
      IdentityName := AIdentityName;
      Caption := Application.Title;
      Initialize;
      MessageText := THtmlLib.XmlToHtml(AMessageText);
      THtmlLib.LoadStringToBrowser(wbMessage, MessageText);
      wbMessage.OleObject.Document.bgColor := THtmlLib.ColorToHtml(clWebWhiteSmoke);
      if wbMessage.Showing and Assigned(wbMessage.Document) then
        with wbMessage.Application as IOleobject do
          DoVerb(OLEIVERB_UIACTIVATE, nil, wbMessage, 0, Handle, GetClientRect);
      Show;
    end;
end;

procedure TInformationDialog.aExitExecute(Sender: TObject);
begin
  Self.Close;
end;

function TInformationDialog.GetIdentityName: string;
begin
  if FIdentityName.IsEmpty then
    Result := C_IDENTITY_NAME
  else
    Result := Concat(C_IDENTITY_NAME, '.', FIdentityName);
end;

procedure TInformationDialog.Initialize;
var
  html: string;
begin
  inherited;
  TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'Initialize', FMessageText);
  LoadFormPosition;

//  if (Description <> '') then
//    html := Concat(FMessageText, C_HTML_BREAK, C_HTML_BREAK, THtmlLib.GetBoldText(C_ERROR_ACTION_RECOMMENDATION + ':'), C_HTML_BREAK, Description)
//  else
    html := Concat(C_HTML_OPEN,
                     C_HTML_HEAD_OPEN,
                       C_HTML_STYLE_OPEN,
                         'h2{font-size:2px}',
                       C_HTML_STYLE_CLOSE,
                     C_HTML_HEAD_CLOSE,
                     C_HTML_BODY_OPEN,
                       '<FONT size=2>',
                         FMessageText,
                       '</FONT>',
                     C_HTML_BODY_CLOSE,
                   C_HTML_CLOSE);

  THtmlLib.LoadStringToBrowser(wbMessage, html);
  wbMessage.OleObject.Document.bgColor := THtmlLib.ColorToHtml(clWebWhiteSmoke);
  if wbMessage.Showing and Assigned(wbMessage.Document) then
    with wbMessage.Application as IOleobject do
      DoVerb(OLEIVERB_UIACTIVATE, nil, wbMessage, 0, Handle, GetClientRect);
end;

procedure TInformationDialog.aSaveExecute(Sender: TObject);
begin
  if not IdentityName.IsEmpty then
    SaveTextFileDialog.FileName := IdentityName + '.html';
  if SaveTextFileDialog.Execute then
    TFile.WriteAllText(SaveTextFileDialog.FileName, MessageText);
end;

procedure TInformationDialog.Deinitialize;
begin
  SaveFormPosition;
end;

procedure TInformationDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Deinitialize;
end;

procedure TInformationDialog.wbMessageBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
begin
  inherited;
  Cancel := URL <> C_HTML_BLANK;
end;

end.
