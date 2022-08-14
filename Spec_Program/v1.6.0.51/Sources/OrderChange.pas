unit OrderChange;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Imaging.pngimage, System.ImageList,
  Vcl.ImgList, System.Math, System.Types, DebugWriter, HtmlLib, Document, Global.Types, Utils,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, DaImages;
{$ENDREGION}

type
  TfrmOrderChange = class(TCustomForm)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    grdLimit: TStringGrid;
    grdVolume: TStringGrid;
    imgLimitDown: TImage;
    imgLimitUp: TImage;
    imgVolumeDown: TImage;
    imgVolumeUp: TImage;
    pnlBottom: TPanel;
    pnlLimit: TPanel;
    pnlLimitTop: TPanel;
    pnlVolume: TPanel;
    pnlVolumeTop: TPanel;
    timerScroll: TTimer;
    procedure grdLimitMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure grdVolumeDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure grdVolumeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure grdVolumeMouseEnter(Sender: TObject);
    procedure grdVolumeMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure grdVolumeMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure Scrolling(aGrid: TStringGrid; aNr: Integer);
    procedure timerScrollTimer(Sender: TObject);
  const
    C_FAST_INTERV  = 50;   // Fast milliseconds between scrolling
    C_INTERV       = 100;  // Normal milliseconds between scrolling
    C_MAX_FAST_SCR = 14;   // Fast scrolls before accelerating when pointing on first or last cell
    C_MAX_SLOW_SCR = 7;    // Normal scrolls before accelerating when pointing on first or last cell
    C_SPEED_INTERV = 10;   // Speed milliseconds between scrolling
    C_TSIZE        = 0.25; // The tick size
    C_VOLUME       = 1000; // The orders current volume
  private
    FContScrolls   : Integer;
    FCurrentLimit  : string;
    FCurrentVolume : string;
    FLimit         : Double;
    FVertStep      : Integer;
    FVolume        : Integer;
    function GetLimit: Double;
    function GetVolume: Integer;
  public
    procedure Initialize;
    class function ShowDocument(var aLimit: Double; var aVolume: Integer): TModalResult;
    property Limit  : Double  read GetLimit  write FLimit;   // The orders current limit
    property Volume : Integer read GetVolume write FVolume;     // The orders current volume
  end;

implementation

{$R *.dfm}

class function TfrmOrderChange.ShowDocument(var aLimit: Double; var aVolume: Integer): TModalResult;
var
  frmOrderChange: TfrmOrderChange;
begin
  Result := mrCancel;
  frmOrderChange := TfrmOrderChange.Create(nil);
  try
    frmOrderChange.Limit  := aLimit;
    frmOrderChange.Volume := aVolume;
    frmOrderChange.Initialize;
    if (frmOrderChange.ShowModal = mrOk) then
    begin
      Result  := mrOk;
      aLimit  := frmOrderChange.Limit;
      aVolume := frmOrderChange.Volume;
    end;
  finally
    FreeAndNil(frmOrderChange);
  end;
end;

function Rep1(s: string): string;
begin
  Result := s.Replace('.', FormatSettings.DecimalSeparator);
end;

function Rep2(s: string): string;
begin
  Result := s.Replace(FormatSettings.DecimalSeparator, '.');
end;

procedure TfrmOrderChange.Initialize;
var
  i: Integer;
  s, dec: String;
begin
  timerScroll.Interval := C_INTERV;
  if (Volume < 100) then
    FVertStep := 1
  else if (Volume < 1000) then
    FVertStep := 10
  else if (Volume < 10000 )then
    FVertStep := 100
  else if (Volume < 100000 )then
    FVertStep := 1000
  else
    FVertStep := 10000;
  if C_TSIZE < 1.0 then
    dec := '2'
  else
    dec := '0';
  grdVolume.ColWidths[0] := grdVolume.Width - 4;
  s := '';
  for i := 8 downto - 8 do
  begin
    s := s + Format('%d', [Volume + i * FVertStep]);
    if i > -8 then
      s := s + ',';
  end;
  grdVolume.Cols[0].CommaText := s;
  grdVolume.Row := 8;
  FCurrentVolume := grdVolume.Cells[0, 8];

  grdLimit.ColWidths[0] := grdLimit.Width - 4;
  s := '';
  for i := 8 downto -8 do
  begin
    s := s + Rep2(Format('%.' + dec + 'f', [Limit + i * C_TSIZE]));
    if i > -8 then
      s := s + ',';
  end;
  grdLimit.Cols[0].CommaText := s;
  grdLimit.Row := 8;
  FCurrentLimit := grdLimit.Cells[0, 8];
end;

procedure TfrmOrderChange.grdVolumeDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  Grid: TStringGrid;
  Value: string;
begin
  if (Sender is TStringGrid) then
  begin
    Grid := Sender as TStringGrid;
    if Grid = grdVolume then
      Value := FCurrentVolume
    else
      Value := FCurrentLimit;
    if (Grid.Cells[ACol, ARow] = Value) then
      Grid.Canvas.Brush.Color := clGray
    else
      Grid.Canvas.Brush.Color := clWhite;
    Grid.Canvas.FillRect(Rect);
    DrawText(Grid.Canvas.Handle, Grid.Cells[ACol, ARow], Length(Grid.Cells[ACol, ARow]), Rect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TfrmOrderChange.grdVolumeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Grid: TStringGrid;
begin
  if (Sender is TStringGrid) then
  begin
    Grid := Sender as TStringGrid;
    if (Grid.Row = 0) and (Key = VK_UP) then
      Scrolling(Grid, 1)
    else if (Grid.Row = (Grid.RowCount - 1)) and (Key = VK_DOWN) then
      Scrolling(Grid, -1);
  end;
end;

procedure TfrmOrderChange.grdVolumeMouseEnter(Sender: TObject);
var
  Grid: TStringGrid;
begin
  if (Sender is TStringGrid) then
  begin
    Grid := Sender as TStringGrid;
    SetFocusSafely(Grid);
  end;
end;

procedure TfrmOrderChange.grdVolumeMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  Grid: TStringGrid;
begin
  if (Sender is TStringGrid) then
  begin
    Grid := Sender as TStringGrid;
    if (Grid.Row = Grid.RowCount - 1) then
      Scrolling(Grid, -1);
  end;
end;

procedure TfrmOrderChange.grdVolumeMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  Grid: TStringGrid;
begin
  if (Sender is TStringGrid) then
  begin
    Grid := Sender as TStringGrid;
    if (Grid.Row = 0) then
      Scrolling(Grid, 1);
  end;
end;

procedure TfrmOrderChange.Scrolling(aGrid: TStringGrid; aNr: Integer);
var
  i    : Integer;
  s    : string;
  step : Double;
  v    : Double;
  v2   : Double;
begin
  s := aGrid.Cells[0, 0];
  if (Pos('.', s) > 0) then
    s := '%.' + IntToStr(Length(s) - Pos('.', s)) + 'f'
  else
    s := '%.0f';
  if (aGrid = grdVolume) then
    step := FVertStep
  else
    step := C_TSIZE;
  v2 := StrToFloat(Rep1(aGrid.Cells[0, aGrid.RowCount - 1]));
  v := Max(step, v2 + aNr * step);
  if (v <> v2) then
    for i := 0 to aGrid.RowCount - 1 do
      aGrid.Cells[0, aGrid.RowCount - i - 1] := Rep2(Format(s, [v + i * step]));
end;

function TfrmOrderChange.GetLimit: Double;
begin
  Result := StrToFloatDef(FCurrentLimit, FLimit);
end;

function TfrmOrderChange.GetVolume: Integer;
begin
  Result := StrToIntDef(FCurrentVolume, FVolume);
end;

procedure TfrmOrderChange.grdLimitMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Grid: TStringGrid;
begin
  if (Sender is TStringGrid) then
  begin
    Grid := Sender as TStringGrid;
    if (X < 0) or (Y < 0) or (X > Grid.Width) or (Y > Grid.Height) then
      Exit;
    if (Grid = grdVolume) and (FCurrentVolume <> Grid.Cells[Grid.Col, Grid.Row]) then
    begin
      FCurrentVolume := Grid.Cells[Grid.Col, Grid.Row];
      Grid.Refresh;
      // Change order volume to current_volume
    end
    else if (FCurrentLimit <> Grid.Cells[Grid.Col, Grid.Row]) then
    begin
      FCurrentLimit := Grid.Cells[Grid.Col, Grid.Row];
      Grid.Refresh;
      // Change order limit to current_limit
    end;
  end;
end;

procedure TfrmOrderChange.timerScrollTimer(Sender: TObject);
begin
  if PtInRect(imgLimitUp.ClientRect, imgLimitUp.ScreenToClient(Mouse.CursorPos)) then
    Scrolling(grdLimit, 1)
  else if PtInRect(imgLimitDown.ClientRect, imgLimitDown.ScreenToClient(Mouse.CursorPos)) then
    Scrolling(grdLimit, -1)
  else if PtInRect(imgVolumeUp.ClientRect, imgVolumeUp.ScreenToClient(Mouse.CursorPos)) then
    Scrolling(grdVolume, 1)
  else if PtInRect(imgVolumeDown.ClientRect, imgVolumeDown.ScreenToClient(Mouse.CursorPos)) then
    Scrolling(grdVolume, -1)
  else
  begin
    FContScrolls := 0;
    timerScroll.Interval := C_INTERV;
    Exit;
  end;
  Inc(FContScrolls);
  if FContScrolls = C_MAX_SLOW_SCR then
    timerScroll.Interval := C_FAST_INTERV
  else if FContScrolls = C_MAX_FAST_SCR then
    timerScroll.Interval := C_SPEED_INTERV;
end;

end.
