unit UTextForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFTextForm = class(TForm)
    Memo: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FTextForm: TFTextForm;

implementation

{$R *.DFM}

end.
