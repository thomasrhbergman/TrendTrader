unit UCombo;

interface

{$IF CompilerVersion < 24.0}  // XE3
uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Shellapi, IABSocketAPI, Menus, ComCtrls, UTextForm;
{$ELSE}
uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes, Vcl.Graphics,
     Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;
{$IFEND}


type
  TFComboOrder = class(TForm)
    Edit1Id: TEdit;
    Edit1Ratio: TEdit;
    Combo1Side: TComboBox;
    Edit2Id: TEdit;
    Edit2Ratio: TEdit;
    Combo2side: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Combo1OC: TComboBox;
    Combo2OC: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FComboOrder: TFComboOrder;

implementation

{$R *.DFM}

procedure TFComboOrder.FormCreate(Sender: TObject);
begin
  Combo1Side.ItemIndex := 0;
  Combo2Side.ItemIndex := 1;
  Combo1OC.ItemIndex := 0;
  Combo2OC.ItemIndex := 0;
end;

end.
