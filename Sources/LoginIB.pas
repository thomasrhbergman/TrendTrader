unit LoginIB;

interface

{$REGION 'Region uses'}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Utils, Global.Types,
  Vcl.ExtCtrls, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, Vcl.Buttons, MessageDialog, DaImages;
{$ENDREGION}

type
  TfrmLoginIB = class (TCustomForm)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    eClientID  : TEdit;
    eIPAddress : TEdit;
    ePort      : TEdit;
    lblClientID: TLabel;
    lblIPAddress: TLabel;
    lblPort: TLabel;
    pnlBottom: TPanel;
    procedure FormCreate(Sender : TObject);
    procedure btnOkClick(Sender: TObject);
  private
    class var LastClientID : Integer;
  public
    ClientID  : Integer;
    Port      : Integer;
    IPAddress : string;
    procedure SaveParamsToXml;
    procedure LoadParamsFromXml;
  end;

implementation

{$R *.dfm}

procedure TfrmLoginIB.btnOkClick(Sender: TObject);
begin
  IPAddress := Trim(eIPAddress.Text);
  try
    ClientID := StrToInt(eClientID.Text)
  except
    TMessageDialog.ShowWarning('Client ID is invalid');
    Exit
  end;

  try
    Port := StrToIntDef(ePort.Text, 0);
    if (Port <= 0) or (Port >= 65535) then
      raise Exception.Create('Invalid port number')
  except
    TMessageDialog.ShowWarning('Port is invalid (enter value between 1 and 65534)');
    Exit
  end;

  LastClientID := ClientID;
  ModalResult := mrOk
end;

procedure TfrmLoginIB.FormCreate;
begin
  IPAddress := General.InteractiveParams.C_IP_ADDRESS;
  ClientID  := LastClientID;
  Port      := General.InteractiveParams.C_PORT;

  eClientID.Text  := IntToStr(ClientID);
  ePort.Text      := IntToStr(Port);
  eIPAddress.Text := IPAddress;
end;

procedure TfrmLoginIB.SaveParamsToXml;
begin
  try
    General.XmlFile.Attributes.Node := General.XmlFile.GetNode(General.XmlFile.GetXPath(General.C_SECTION_CONNECTIONS, General.InteractiveParams.C_SECTION));
    General.XmlFile.Attributes.SetAttributeValue(General.C_ATTR_IP_ADDRESS, eIPAddress.Text);
    General.XmlFile.Attributes.SetAttributeValue(General.C_ATTR_CLIENT_ID, eClientID.Text);
    General.XmlFile.Attributes.SetAttributeValue(General.C_ATTR_PORT, ePort.Text);
    General.XmlFile.WriteAttributes;
  finally
    General.XmlFile.Save;
  end;
end;

procedure TfrmLoginIB.LoadParamsFromXml;
begin
  if General.XmlFile.ReadAttributes(General.C_SECTION_CONNECTIONS, General.InteractiveParams.C_SECTION) then
  begin
    eIPAddress.Text := General.XmlFile.Attributes.GetAttributeValue(General.C_ATTR_LOGIN, General.InteractiveParams.C_IP_ADDRESS);
    eClientID.Text  := General.XmlFile.Attributes.GetAttributeValue(General.C_ATTR_CLIENT_ID, General.InteractiveParams.C_CLIENT_ID);
    ePort.Text      := General.XmlFile.Attributes.GetAttributeValue(General.C_ATTR_PORT, General.InteractiveParams.C_PORT);
  end
  else
  begin
    IPAddress := General.InteractiveParams.C_IP_ADDRESS;
    ClientID  := General.InteractiveParams.C_CLIENT_ID;
    Port      := General.InteractiveParams.C_PORT;
  end;
end;

end.


