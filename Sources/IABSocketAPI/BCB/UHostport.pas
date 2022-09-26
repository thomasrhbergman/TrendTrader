unit UHostport;

interface

uses
{$IF CompilerVersion < 24.0}  // 24 = XE3
Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls
{$ELSE}
Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
     Vcl.Controls, Vcl.Forms, Vcl.StdCtrls
{$IFEND}
;

type
  TFHostPort = class(TForm)
    ComboBoxIPHost: TComboBox;
    LabelIPHost: TLabel;
    ComboBoxPort: TComboBox;
    LabelPort: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FHostPort: TFHostPort;

implementation

{$R *.dfm}

end.
