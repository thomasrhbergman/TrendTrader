unit Relations;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, IABSocketAPI, Vcl.ExtCtrls,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, DaImages;
{$ENDREGION}

type
  TfrmRelation = class(TCustomForm)
    Lb_ObjType1: TLabel;
    Lb_ObjName1: TLabel;
    Lb_ObjName2: TLabel;
    Lb_ObjType2: TLabel;
    eCalcFactor: TEdit;
    Label1: TLabel;
    pnlBottom: TPanel;
    BitBtn1: TBitBtn;
    Bb_Cancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRelation: TfrmRelation;

implementation

{$R *.dfm}

end.
