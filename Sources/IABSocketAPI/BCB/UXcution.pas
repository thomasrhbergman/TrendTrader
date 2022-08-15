unit UXcution;

interface

{$IF CompilerVersion < 24.0}  // XE3
uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Shellapi, Menus, ComCtrls, Mask, UTextForm,
{$ELSE}
uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes, Vcl.Graphics,
     Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask, UTextForm, ShellAPI,
{$IFEND}
IABSocketAPI, IABSocketAPI_const;



type
  TFExecution = class(TForm)
    EditClient: TEdit;
    Label1: TLabel;
    EditAcctCode: TEdit;
    Label2: TLabel;
    MaskEditDateTime: TMaskEdit;
    Label4: TLabel;
    Label5: TLabel;
    EditSymbol: TEdit;
    Label6: TLabel;
    ComboBoxSecType: TComboBox;
    Label3: TLabel;
    EditExch: TEdit;
    Label7: TLabel;
    ComboBoxAction: TComboBox;
    Button1: TButton;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    ExecFilter: TIABExecutionFilter;
  end;

var
  FExecution: TFExecution;

implementation
uses UIABsocket;

{$R *.DFM}

procedure TFExecution.FormCreate(Sender: TObject);
var i: Integer;
begin
  EditClient.Text := IntToStr(FIABSocket.IABSocket1.ClientId);
  Label8.Caption := '(' + EditClient.Text + ')';
  MaskEditDateTime.Text := FormatDateTime('mm"/"dd"/"yyyy" "hh":"nn',Date + 0.25);
  ComboBoxSecType.Items.Clear;
  for i := 0 to Ord(High(TIABSecurityType)) do
    ComboBoxSecType.Items.Add(SecurityTypeString[TIABSecurityType(i)]);
  ComboBoxSecType.ItemIndex := ComboBoxSecType.Items.Count -1;

  ComboBoxAction.Items.Clear;
  for i := 0 to Ord(High(TIABAction)) do
    ComboBoxAction.Items.Add(ActionString[TIABAction(i)]);
  ComboBoxAction.Items[0] := 'All';
  ComboBoxAction.ItemIndex := 0;
end;

procedure TFExecution.Button1Click(Sender: TObject);
var ShortD, T, s: string; DSep, TSep: char;
begin
  {$IF CompilerVersion >= 22.0}   // XE1
  DSep := FormatSettings.DateSeparator;
  TSep := FormatSettings.TimeSeparator;
  ShortD := FormatSettings.ShortDateFormat;
  t := FormatSettings.ShortTimeFormat;
  try
    FormatSettings.DateSeparator := '/';
    FormatSettings.TimeSeparator := ':';
    FormatSettings.ShortDateFormat := 'MM/dd/yyyy';
    FormatSettings.ShortTimeFormat := 'hh:mm';
  {$ELSE}
  DSep := DateSeparator;
  TSep := TimeSeparator;
  ShortD := ShortDateFormat;
  t := ShortTimeFormat;
  try
    DateSeparator := '/';
    TimeSeparator := ':';
    ShortDateFormat := 'MM/dd/yyyy';
    ShortTimeFormat := 'hh:mm';
  {$IFEND}


    with ExecFilter do
      begin
        ClientId := StrToInt(EditClient.Text);
        AccountCode := EditAcctCode.Text;
        s := MaskEditDateTime.Text;
        FromTime := StrToDateTime(s);
        Symbol := EditSymbol.Text;
        SecurityType := TIABSecurityType(ComboBoxSecType.ItemIndex);
        Exchange := EditExch.Text;
        Action := TIABAction(ComboBoxAction.ItemIndex);
      end;
  finally
  {$IF CompilerVersion >= 22.0}   // XE1
    FormatSettings.DateSeparator := DSep;
    FormatSettings.TimeSeparator := TSep;
    FormatSettings.ShortDateFormat := ShortD;
    FormatSettings.ShortTimeFormat := t;
  {$ELSE}
    DateSeparator := DSep;
    TimeSeparator := TSep;
    ShortDateFormat := ShortD;
    ShortTimeFormat := t;
  {$IFEND}
  end;
end;

end.
