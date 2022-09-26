unit UHistoricalData;

interface

uses
{$IF CompilerVersion < 24.0}  // 24 = XE3
Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Shellapi, Menus, ComCtrls,
{$ELSE}
Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes, Vcl.Graphics,
     Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
{$IFEND}
IABSocketAPI, IABSocketAPI_const;


type
  TFHistoricalData = class(TForm)
    Button1: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Bevel1: TBevel;
    Label1: TLabel;
    ListBoxBidAsk: TListBox;
    ListBoxTickCount: TListBox;
    ListBoxBars: TListBox;
    ListBoxBarPeriod: TListBox;
    Button2: TButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    TickType: TIABTickDataType;
    TickCount: Integer;
    BarData: TIABHistoricalDataType;
    BarPeriod: TIABChartBarSize;
  end;

var
  FHistoricalData: TFHistoricalData;

implementation

{$R *.dfm}

procedure TFHistoricalData.Button1Click(Sender: TObject);
begin
  TickType := TIABTickDataType(ListboxBidAsk.ItemIndex);
  TickCount := StrToIntDef(ListboxTickCount.Items[ListBoxTickCount.ItemIndex], 100);
  BarData := TIABHistoricalDataType(ListBoxBars.ItemIndex);
  BarPeriod := TIABChartBarSize(ListboxBarPeriod.ItemIndex);
end;

procedure TFHistoricalData.FormCreate(Sender: TObject);
var i: Integer;
begin

  for i := 0 to Ord(High(TIABHistoricalDataType)) do
    ListboxBars.Items.Add(HistoricalDataTypeString[TIABHistoricalDataType(i)]);
  ListboxBars.ItemIndex := 0;


  for i := 0 to Ord(High(TIABChartBarSize)) do
    ListboxBarPeriod.Items.Add(ChartBarSizeString[TIABChartBarSize(i)]);
  ListboxBarPeriod.ItemIndex := 7;


  for i := 0 to Ord(High(TIABTickDataType)) do
    ListboxBidAsk.Items.Add(TickDataTypeString[TIABTickDataType(i)]);
  ListboxBidAsk.ItemIndex := 1;

  ListboxTickcount.Items.Add('0');
  ListboxTickcount.Items.Add('100');
  ListboxTickcount.Items.Add('250');
  ListboxTickcount.Items.Add('500');
  ListboxTickcount.Items.Add('1000');
  ListboxTickCount.ItemIndex := 1;
end;

procedure TFHistoricalData.RadioButton1Click(Sender: TObject);
begin
  ListBoxBidAsk.Enabled := false;
  ListBoxTickCount.Enabled := false;
  ListBoxBars.Enabled := true;
  ListBoxBarPeriod.Enabled := true;
end;

procedure TFHistoricalData.RadioButton2Click(Sender: TObject);
begin
  ListBoxBidAsk.Enabled := true;
  ListBoxTickCount.Enabled := true;
  ListBoxBars.Enabled := false;
  ListBoxBarPeriod.Enabled := false;
end;

end.
