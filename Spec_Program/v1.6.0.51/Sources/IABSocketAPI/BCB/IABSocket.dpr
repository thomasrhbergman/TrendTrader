program IABSocket;

uses
  Forms,
  UIABsocket in 'UIABsocket.pas' {FIABSocket},
  UDepth in 'UDepth.pas' {FDepth},
  UCombo in 'UCombo.pas' {FComboOrder},
  UXcution in 'UXcution.pas' {FExecution},
  UScanForm in 'UScanForm.pas' {FScanForm},
  UTextForm in 'UTextForm.pas' {FTextForm},
  UHistoricalData in 'UHistoricalData.pas' {FHistoricalData};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TWS API demo';
  Application.CreateForm(TFIABSocket, FIABSocket);
  Application.CreateForm(TFDepth, FDepth);
  Application.CreateForm(TFComboOrder, FComboOrder);
  Application.CreateForm(TFExecution, FExecution);
  Application.CreateForm(TFScanForm, FScanForm);
  Application.CreateForm(TFTextForm, FTextForm);
  Application.CreateForm(TFHistoricalData, FHistoricalData);
  Application.Run;
end.
