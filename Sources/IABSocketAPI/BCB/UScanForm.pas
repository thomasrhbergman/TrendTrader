unit UScanForm;

interface

{$IF CompilerVersion < 24.0}  // XE3
uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Shellapi, Menus, ComCtrls, UTextForm,
{$ELSE}
uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes, Vcl.Graphics,
     Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, UTextForm, ShellAPI,
{$IFEND}
IABSocketAPI, IABSocketAPI_const;


type
  TFScanForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Bevel1: TBevel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Edit19: TEdit;
    Edit20: TEdit;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    FScanId: Integer;
  public
    ScanId: Integer;
    ScanCriteria: TIABScanCriteria;
  end;

var
  FScanForm: TFScanForm;

implementation

{$R *.DFM}

procedure TFScanForm.FormShow(Sender: TObject);
begin
  if FScanId < 2000 then FScanId := 2000;
  Tag := 0;
end;

procedure TFScanForm.Button1Click(Sender: TObject);
begin   // get scan param
  Tag := 1;
  Close;
end;

procedure TFScanForm.Button3Click(Sender: TObject);
var s: string;
begin
  s := IntToStr(FScanId);
  if InputQuery('Cancel a Scan','ScanID number?',s) then
    begin
      ScanId := StrToInt(s);
      Tag := 2;
      Close;
    end;
end;

procedure TFScanForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TFScanForm.Button4Click(Sender: TObject);
begin
  Tag := 3;
  FScanId := FScanId + 1;
  ScanId := FScanId;
  with ScanCriteria do
    begin
      if Edit1.Text <> '' then NumberOfRows := StrToInt(Edit1.Text);
      if Edit2.Text <> '' then Instrument := Edit2.Text;
      if Edit3.Text <> '' then LocationCode := Edit3.Text;
      if Edit4.Text <> '' then ScanCode := Edit4.Text;
      if Edit5.Text <> '' then AbovePrice := StrToFloat(Edit5.Text);
      if Edit6.Text <> '' then BelowPrice := StrToFloat(Edit6.Text);
      if Edit7.Text <> '' then AboveVolume := StrToInt(Edit7.Text);
      if Edit8.Text <> '' then MarketCapAbove := StrToFloat(Edit8.Text);
      if Edit9.Text <> '' then MarketCapBelow := StrToFloat(Edit9.Text);
      if Edit10.Text <> '' then MoodyRatingAbove := Edit10.Text;
      if Edit11.Text <> '' then MoodyRatingBelow := Edit11.Text;
      if Edit12.Text <> '' then SPRatingAbove := Edit12.Text;
      if Edit13.Text <> '' then SPRatingBelow := Edit13.Text;
      if Edit14.Text <> '' then MaturityDateAbove := Edit14.Text;
      if Edit15.Text <> '' then MaturityDateBelow := Edit15.Text;
      if Edit16.Text <> '' then CouponRateAbove := StrToFloat(Edit16.Text);
      if Edit17.Text <> '' then CouponRateBelow := StrToFloat(Edit17.Text);
      if Edit19.Text <> '' then AverageOptionVolumeAbove := StrToInt(Edit19.Text);
      if Edit20.Text <> '' then ScannerSettingPairs := Edit20.Text;
    end;
  Close;
end;

end.
