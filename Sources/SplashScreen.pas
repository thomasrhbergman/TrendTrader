unit SplashScreen;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage, Global.Types,
  XmlFiles, System.IOUtils, System.Threading;
{$ENDREGION}

type
  TfrmSplashScreen = class(TForm)
    imgLogo: TImage;
    lblVersion: TLabel;
    lblInfo: TLabel;
  private
    { Private declarations }
  public
    class procedure ShowSplashScreen;
    class procedure HideSplashScreen;
    class procedure HideSplash;
  end;

implementation

var
  frmSplashScreen: TfrmSplashScreen;

{$R *.dfm}

{ TfrmSplashScreen }

class procedure TfrmSplashScreen.ShowSplashScreen;
var
  pngLogo : TPngImage;
  RStream : TResourceStream;
  XMLFile : TXMLFile;
begin
  frmSplashScreen := TfrmSplashScreen.Create(Application);
  RStream := TResourceStream.Create(HInstance, 'IMG_LOGO', RT_RCDATA);
  try
    pngLogo := TPngImage.Create;
    try
      pngLogo.LoadFromStream(RStream);
      frmSplashScreen.imgLogo.Picture.Graphic := pngLogo;
    finally
      pngLogo.Free;
    end;
  finally
    RStream.Free;
  end;

  XMLFile := TXMLFile.Create(TPath.Combine(GetEnvironmentVariable('USERPROFILE'), TPath.ChangeExtension(TPath.GetFileName(Application.ExeName), '.xml')));
  try
    frmSplashScreen.lblInfo.Font.Color := clWebDarkSlateBlue;
    frmSplashScreen.lblInfo.Caption := 'Last/Close price is scheduled to ' + FormatDateTime('h:nn am/pm', XmlFile.ReadTime(TGeneral.C_SECTION_GLOBAL, TGeneral.C_KEY_PRICE_SCHEDULE_CHECK, StrToTime('17:40:00')));
  finally
    FreeAndNil(XMLFile);
  end;
  frmSplashScreen.lblVersion.Font.Color := clWebDarkSlateBlue;
  frmSplashScreen.lblVersion.Caption    := 'Ver: ' + TGeneral.GetAppVersion;
  {$IFDEF RELEASE}
  frmSplashScreen.FormStyle := fsStayOnTop;
  {$ELSE}
  frmSplashScreen.FormStyle := fsNormal;
  {$ENDIF}
  frmSplashScreen.Show;
  frmSplashScreen.BringToFront;
  frmSplashScreen.Refresh;
end;

class procedure TfrmSplashScreen.HideSplash;
begin
  if Assigned(frmSplashScreen) then
  begin
    frmSplashScreen.Close;
    frmSplashScreen := nil;
  end;
end;

class procedure TfrmSplashScreen.HideSplashScreen;
begin
  if Assigned(frmSplashScreen) then
  begin
    TTask.Create(
      procedure()
      begin
        TThread.NameThreadForDebugging('TfrmSplashScreen.HideSplashScreen');
        // let splash screen live for some seconds if we want to add some status message..
        Sleep(2000);
        TThread.Synchronize(nil,
          procedure
          begin
            frmSplashScreen.Close;
            frmSplashScreen := nil;
          end);
      end).Start;
  end;
end;

end.
