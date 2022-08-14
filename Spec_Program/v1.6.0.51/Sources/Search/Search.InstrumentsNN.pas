unit Search.InstrumentsNN;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Search.Instruments, Vcl.Menus, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.ComCtrls, VirtualTrees, BrokerHelperAbstr, Global.Types, IABFunctions, Entity.Sokid,
  IABSocketAPI, DebugWriter, System.Actions, Vcl.ActnList,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} Vcl.CheckLst, Frame.Custom, Frame.RealtimeFeeds;

type
  TfrmSearchInstrumentsNN = class(TfrmSearchInstruments)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    pnlBottom: TPanel;
    procedure aExecuteExecute(Sender: TObject);
    procedure aExecuteUpdate(Sender: TObject);
  private
    FBrokerType     : Integer;
    FExchange       : string;
    FId             : Integer;
    FIdentifierList : string;
    FMarketList     : string;
    FSymbol         : string;
    procedure SetBrokerType(const Value: Integer);
  protected
    procedure LoadParamsFromXml; override;
    procedure SaveParamsToXml; override;
  public
    class function ShowDocument(const BrokerType: Integer; var Id: Integer; var Symbol, Exchange, IdentifierList, MarketList: string): TModalResult;

    property BrokerType     : Integer read FBrokerType     write SetBrokerType;
    property Exchange       : string  read FExchange       write FExchange;
    property Id             : Integer read FId             write FId;
    property IdentifierList : string  read FIdentifierList write FIdentifierList;
    property MarketList     : string  read FMarketList     write FMarketList;
    property Symbol         : string  read FSymbol         write FSymbol;
  end;

implementation

{$R *.dfm}

{ TfrmSearchInstrumentsIB }

class function TfrmSearchInstrumentsNN.ShowDocument(const BrokerType: Integer; var Id: Integer; var Symbol, Exchange, IdentifierList, MarketList: string): TModalResult;
var
  frmSearchInstrumentsNN: TfrmSearchInstrumentsNN;
begin
  Result := mrCancel;
  frmSearchInstrumentsNN := TfrmSearchInstrumentsNN.Create(nil);
  try
    frmSearchInstrumentsNN.BrokerType := BrokerType;
    if (frmSearchInstrumentsNN.ShowModal = mrOk) then
    begin
      Exchange       := frmSearchInstrumentsNN.Exchange;
      Id             := frmSearchInstrumentsNN.Id;
      IdentifierList := frmSearchInstrumentsNN.IdentifierList;
      MarketList     := frmSearchInstrumentsNN.MarketList;
      Symbol         := frmSearchInstrumentsNN.Symbol;
      Result := mrOk;
    end;
  finally
    FreeAndNil(frmSearchInstrumentsNN);
  end;
end;

procedure TfrmSearchInstrumentsNN.aExecuteExecute(Sender: TObject);
var
  Data: PSokidInfo;
  i: Integer;
begin
  inherited;
  if Assigned(vstInstruments.FocusedNode) then
  begin
    Data := vstInstruments.FocusedNode^.GetData;
    if Assigned(Data) then
    begin
      Self.Id := Data.ContractId;
      Self.Symbol := Data.Symbol;
      Self.Exchange := Data.Exchange;
      if Assigned(Data.Tradables) then
        for i := 0 to Length(Data.Tradables) - 1 do
        begin
          if Self.IdentifierList.IsEmpty then
          begin
            Self.IdentifierList := Data.Tradables[i].Identifier;
            Self.MarketList := Data.Tradables[i].MarketId.ToString;
          end
          else
          begin
            Self.IdentifierList := Self.IdentifierList + ';' + Data.Tradables[i].Identifier;
            Self.MarketList := Self.MarketList + ';' + Data.Tradables[i].MarketId.ToString;
          end;
        end;
      ModalResult := mrOk;
    end;
  end;
end;

procedure TfrmSearchInstrumentsNN.aExecuteUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled := Assigned(vstInstruments.FocusedNode)
end;

procedure TfrmSearchInstrumentsNN.SetBrokerType(const Value: Integer);
begin
  FBrokerType := Value;
  case Value of
    0:
      tsNordNetBroker.TabVisible := False;
    1:
      tsInteractiveBroker.TabVisible := False;
  end;
end;

procedure TfrmSearchInstrumentsNN.LoadParamsFromXml;
begin
  inherited;

end;

procedure TfrmSearchInstrumentsNN.SaveParamsToXml;
begin
  inherited;

end;

end.

