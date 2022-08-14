program IABConsoleAPI;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  IABSocketAPI,
  IABSocketAPI_const,
  UIABConsole in 'UIABConsole.pas';


var
  s: string;
  IABConsole: TIABConsole;
  MDataId: Integer;

begin
  WriteLn('');
  WriteLn('IABSocketAPI - console demo app.  Copyright(c)2006 HHS Software Corp.');
  WriteLn('');
  WriteLn('  This is a very simple demo of a console app that shows the IAB Socket API');
  WriteLn('  in use and communicating with the TWS.  It will open a connection to TWS,');
  WriteLn('  request market data for the current ES mini.');
  WriteLn('');
  WriteLn('Command ref:');
  WriteLn('');
  WriteLn('  C <enter>    Connect and disconnect');
  WriteLn('  D <enter>    Start market data');
  WriteLn('  Q <enter>    Quit');
  WriteLn('  6 <enter>    Sets TWS port to 7496 (live account port) - default.');
  WriteLn('  7 <enter>    Sets TWS port to 7497 (paper account port).');
  WriteLn('');

  IABConsole := TIABConsole.Create;
  MDataId := 500;

  repeat
    ReadLn(s);
    s := UpperCase(s);
    if s = 'Q' then Break;
    if s = 'C' then
      begin
        WriteLn('Requesting change to connection state...');
        IABConsole.IABSocket.Connected := not IABConsole.IABSocket.Connected;
      end;
    if s = 'D' then
      begin
        if IABConsole.IABSocket.TWSPort = 7497 then
          begin
            WriteLn('Requesting market data on ' + IABConsole.IABSocket.Deforder.LocalSymbol + ', using delayed data format.');
            IABConsole.IABSocket.RequestMarketDataType(mdtDelayed);
          end
        else
          WriteLn('Requesting market data on ' + IABConsole.IABSocket.Deforder.LocalSymbol);
        if MDataId > 500 then IABConsole.IABSocket.CancelMarketData(MDataId -1);
        IABConsole.IABSocket.GetMarketData(MDataId,IABConsole.IABSocket.DefOrder);
        inc(MDataId);
      end;

    if (s = '6') or (s = '7') then
      begin
        if IABConsole.IABSocket.Connected then
          WriteLn('Error: Cannot change TIABSocket API port while connected.')
        else
          begin
            IABConsole.IABSocket.TWSPort := 7490 + StrToInt(s);
            WriteLn('Setting TIABSocket API port to ' + IntToStr(IABConsole.IABSocket.TWSPort));
          end;  
      end;

  until false;

  IABConsole.Free;

end.


