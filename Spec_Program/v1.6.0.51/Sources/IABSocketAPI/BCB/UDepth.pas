unit UDepth;

interface

{$IF CompilerVersion < 24.0}  // XE3
uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Shellapi, Menus, ComCtrls, UTextForm, Grids,
{$ELSE}
uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes, Vcl.Graphics,
     Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, UTextForm, ShellAPI, Vcl.Grids,
{$IFEND}
IABSocketAPI, IABSocketAPI_const
{$IFDEF USE_BIGDECIMAL}
, Velthuis.BigDecimals    // see top of IABSocketAPI_const.pas for information on this.
{$ENDIF}
;



type
  TFDepth = class(TForm)
    Grid: TStringGrid;
    Button1: TButton;
    Button2: TButton;
    EditSymbol: TEdit;
    EditExch: TEdit;
    EditExpiry: TEdit;
    ComboSecType: TComboBox;
    EditRows: TEdit;
    UpDownRows: TUpDown;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    DataId: Integer;
  public
    procedure AmendGrid(Index, Operation, Side: Integer; Size: BigDecimal; Price: Double; MMId: string);
    procedure EmptyGrid;
  end;

var
  FDepth: TFDepth;

implementation
{$R *.DFM}
uses UIABSocket;

procedure TFDepth.FormCreate(Sender: TObject);
var i: Integer;
begin
  Grid.Cells[0,0] := 'MM Id';
  Grid.Cells[1,0] := 'Price';
  Grid.Cells[2,0] := 'Size';
  Grid.Cells[3,0] := 'Size';
  Grid.Cells[4,0] := 'Price';
  Grid.Cells[5,0] := 'MM Id';
  ComboSecType.Items.Clear;
  for i := 0 to Ord(High(TIABSecurityType)) do
    ComboSecType.Items.Add(SecurityTypeString[TIABSecurityType(i)]);
  ComboSecType.ItemIndex := 2;
end;

procedure TFDepth.Button1Click(Sender: TObject);
var Rows: Integer;
begin
  Rows := StrToInt(EditRows.Text);
  Button2.Enabled := true;
  Button1.Enabled := false;
  EditRows.Enabled := false;
  DataId := DataId + 1;
  FIABSocket.IABSocket1.SmartDepthMarketData := false;
  if EditExpiry.Text = 'EXPIRY' then EditExpiry.Text := '';
  if EditExpiry.Text = '' then
    FIABSocket.IABSocket1.GetMarketDepth(DataId,EditSymbol.Text,'',EditExch.Text,'',EditExpiry.Text,FIABSocket.EditCur.Text,TIABSecurityType(ComboSecType.ItemIndex),rtNone,0.0, Rows)
  else
    FIABSocket.IABSocket1.GetMarketDepth(DataId,'',EditSymbol.Text,EditExch.Text,'',EditExpiry.Text,FIABSocket.EditCur.Text,TIABSecurityType(ComboSecType.ItemIndex),rtNone,0.0, Rows)
end;

procedure TFDepth.Button2Click(Sender: TObject);
begin
  Button1.Enabled := true;
  Button2.Enabled := false;
  EditRows.Enabled := true;
  FIABSocket.IABSocket1.CancelMarketDepth(DataId);
  EmptyGrid;
end;

procedure TFDepth.AmendGrid(Index, Operation, Side: Integer; Size: BigDecimal; Price: Double; MMId: string);
var i: Integer;  Sizestr, Pricestr: string;
begin
  {$IFDEF USE_BIGDECIMAL}
  Sizestr := Size.ToPlainString;
  {$ELSE}
  Sizestr := FormatFloat('0.#', Size);
  {$ENDIF}
  Pricestr := FormatFloat('0.00###', Price);

  Index := Index + 1;
  if Index > Grid.RowCount -1 then Exit;
  if Index < 1 then Exit;
  if Operation = OPERATION_DELETE then
    case Side of
      SIDE_ASK:begin
        for i := Index to Grid.RowCount - 2 do
          begin
            Grid.Cells[3,i] := Grid.Cells[3,i + 1];
            Grid.Cells[4,i] := Grid.Cells[4,i + 1];
            Grid.Cells[5,i] := Grid.Cells[5,i + 1];
          end;
        Grid.Cells[3,Grid.RowCount -1] := '';
        Grid.Cells[4,Grid.RowCount -1] := '';
        Grid.Cells[5,Grid.RowCount -1] := '';
      end;
      SIDE_BID:begin
        for i := Index to Grid.RowCount - 2 do
          begin
            Grid.Cells[0,i] := Grid.Cells[0,i + 1];
            Grid.Cells[1,i] := Grid.Cells[1,i + 1];
            Grid.Cells[2,i] := Grid.Cells[2,i + 1];
          end;
        Grid.Cells[0,Grid.RowCount -1] := '';
        Grid.Cells[1,Grid.RowCount -1] := '';
        Grid.Cells[2,Grid.RowCount -1] := '';
      end;
    end;
  if Operation = OPERATION_INSERT then
    case Side of
      SIDE_ASK:begin
          for i := Grid.RowCount - 2 downto Index do
            begin
              Grid.Cells[3,i + 1] := Grid.Cells[3,i];
              Grid.Cells[4,i + 1] := Grid.Cells[4,i];
              Grid.Cells[5,i + 1] := Grid.Cells[5,i];
            end;
        Grid.Cells[3,Index] := Sizestr;
        Grid.Cells[4,Index] := Pricestr;
        Grid.Cells[5,Index] := MMId;
      end;
      SIDE_BID:begin
          for i := Grid.RowCount - 2 downto Index do
            begin
              Grid.Cells[2,i + 1] := Grid.Cells[2,i];
              Grid.Cells[1,i + 1] := Grid.Cells[1,i];
              Grid.Cells[0,i + 1] := Grid.Cells[0,i];
            end;
        Grid.Cells[2,Index] := Sizestr;
        Grid.Cells[1,Index] := Pricestr;
        Grid.Cells[0,Index] := MMId;
      end;
    end;
  if Operation = OPERATION_UPDATE then
    case Side of
      SIDE_ASK:begin
        Grid.Cells[3,Index] := Sizestr;
        Grid.Cells[4,Index] := Pricestr;
      end;
      SIDE_BID:begin
        Grid.Cells[2,Index] := Sizestr;
        Grid.Cells[1,Index] := Pricestr;
      end;
    end;
end;

procedure TFDepth.EmptyGrid;
var r, c: Integer;
begin
  for r := 1 to Grid.RowCount -1 do
    for c := 0 to Grid.ColCount -1 do
      Grid.Cells[c,r] := '';
end;

procedure TFDepth.FormShow(Sender: TObject);
begin
  EditSymbol.Text := FIABSocket.EditSym.Text + ExpiryDateToContractName(FIABSocket.EditExp.Text);
  EditExch.Text := FIABSocket.EditExch.Text;
  if FIABSocket.EditExp.Text <> '' then
    EditExpiry.Text := FIABSocket.EditExp.Text;
  ComboSecType.ItemIndex := FIABSocket.ComboSecType.ItemIndex;
end;

end.
