unit LoginNN;

interface

{$REGION 'Region uses'}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  Data.DB, Global.Types, Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF}
  CustomForms, DaImages;
{$ENDREGION}

type
  TfrmLoginNN = class(TCustomForm)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    Ed_pw: TEdit;
    Ed_User: TEdit;
    Lb_1: TLabel;
    Lb_2: TLabel;
    pnlBottom: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    function GetUserLogin: string;
    function GetUserPassword: string;
    procedure SetUserLogin(const Value: string);
    procedure SetUserPassword(const Value: string);
  public
    app_exe, params: array of String;

    procedure SaveParamsToXml;
    procedure LoadParamsFromXml;
    property UserLogin    : string read GetUserLogin    write SetUserLogin;
    property UserPassword : string read GetUserPassword write SetUserPassword;
  end;

implementation

{$R *.dfm}

{ TfrmLoginNN }

procedure TfrmLoginNN.FormCreate(Sender: TObject);
begin
  Self.UserLogin := General.NordNetParams.C_LOGIN;
  Self.UserPassword := General.NordNetParams.C_PASSWORD;
end;

function TfrmLoginNN.GetUserLogin: string;
begin
  Result := Ed_User.Text;
end;

function TfrmLoginNN.GetUserPassword: string;
begin
  Result := Ed_pw.Text;
end;

procedure TfrmLoginNN.SetUserLogin(const Value: string);
begin
  Ed_User.Text := Value;
end;

procedure TfrmLoginNN.SetUserPassword(const Value: string);
begin
  Ed_pw.Text := Value;
end;

procedure TfrmLoginNN.SaveParamsToXml;
begin
  try
    General.XmlFile.Attributes.Node := General.XmlFile.GetNode(General.XmlFile.GetXPath(General.C_SECTION_CONNECTIONS, General.NordNetParams.C_SECTION));
    General.XmlFile.Attributes.SetAttributeValue(General.C_ATTR_LOGIN, UserLogin);
    General.XmlFile.Attributes.SetAttributeValue(General.C_ATTR_PASSWORD, UserPassword);
    General.XmlFile.WriteAttributes;
  finally
    General.XmlFile.Save;
  end;
end;

procedure TfrmLoginNN.LoadParamsFromXml;
begin
  if General.XmlFile.ReadAttributes(General.C_SECTION_CONNECTIONS, General.NordNetParams.C_SECTION) then
  begin
    UserLogin    := General.XmlFile.Attributes.GetAttributeValue(General.C_ATTR_LOGIN, General.NordNetParams.C_LOGIN);
    UserPassword := General.XmlFile.Attributes.GetAttributeValue(General.C_ATTR_PASSWORD, General.NordNetParams.C_PASSWORD);
  end
  else
  begin
    UserLogin    := General.NordNetParams.C_LOGIN;
    UserPassword := General.NordNetParams.C_PASSWORD;
  end;
end;

end.
