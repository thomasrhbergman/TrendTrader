program ConvertInterBaseToFireBird;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Main},
  uDM in 'uDM.pas' {DM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TDM, DM);
  Application.Run;
end.
