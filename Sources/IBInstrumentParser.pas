unit IBInstrumentParser;

interface

{$REGION 'Region uses'}
uses
  System.SysUtils, System.Types, Generics.Collections, Generics.Defaults, System.Variants, System.Classes,
  System.RegularExpressions, MSHTML, Winapi.ActiveX, Vcl.Forms, Common.Types, REST.Utils, REST.Types, REST.Client,
  Global.Types, Publishers;
{$ENDREGION}

type
  TIBHtmlParser = class(TObject)
  type
    TParseResult = record
      Currency: string;
      Description: string;
      Exchange: string;
      Symbol: string;
      IsExists: Boolean;
    end;
  private
    FHTMLDoc: IHTMLDocument2;
//    FParseResult: TParseResult;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    function GetHTMLText: WideString;
    function LoadFromHttpPage(const aConId: Integer): string;
    function Parse: TParseResult;
    procedure SetHTMLText(const Value: WideString);
//    procedure LoadFromFile(aFileName: string);
    property HTMLText: WideString read GetHTMLText write SetHTMLText;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetContractInfo(const aConId: Integer): TParseResult;
    class function GetContent(const aConId: Integer): string;
  end;

implementation

{ TIBHtmlParser }

class function TIBHtmlParser.GetContractInfo(const aConId: Integer): TParseResult;
var
  Parser : TIBHtmlParser;
begin
  Parser := TIBHtmlParser.Create;
  try
    Parser.HTMLText := Parser.LoadFromHttpPage(aConId);
    Result := Parser.Parse;
  finally
    FreeAndNil(Parser);
  end;
end;

class function TIBHtmlParser.GetContent(const aConId: Integer): string;
var
  Parser : TIBHtmlParser;
begin
  Parser := TIBHtmlParser.Create;
  try
    Result := Parser.LoadFromHttpPage(aConId);
  finally
    FreeAndNil(Parser);
  end;
end;

constructor TIBHtmlParser.Create;
begin
  inherited Create;
  FHTMLDoc := CoHTMLDocument.Create as IHTMLDocument2;

  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.Accept := 'text/html';
  FRESTClient.AcceptCharset := 'UTF-8,*;q=0.8';
  FRESTClient.HandleRedirects := True;
  FRESTClient.RaiseExceptionOn500 := False;
  FRESTClient.AutoCreateParams := True;

  FRESTResponse := TRESTResponse.Create(nil);
  FRESTResponse.ContentType := 'text/html';

  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.SynchronizedEvents := False;
end;

destructor TIBHtmlParser.Destroy;
begin
  FHTMLDoc := nil;
  FreeAndNil(FRESTRequest);
  FreeAndNil(FRESTResponse);
  FreeAndNil(FRESTClient);
  inherited;
end;

function TIBHtmlParser.LoadFromHttpPage(const aConId: Integer): string;
begin
  Result := '';
  FRESTClient.BaseURL := 'https://contract.ibkr.info/v3.10/index.php?action=Details&site=GEN&conid={ConId}';
  FRESTClient.Params.ParameterByName('ConId').Value := IntToStr(aConId);
  FRESTRequest.Method := rmGET;
  FRESTRequest.Body.ClearBody;
  FRESTRequest.Params.Clear;
  FRESTRequest.Params.AddHeader('Accept-Language', 'en');
  try
    FRESTRequest.Execute;
    if (FRESTResponse.StatusCode >= 200) and (FRESTResponse.StatusCode <= 299) then
      Result := FRESTResponse.Content;
  except
    on E: Exception do
      TPublishers.LogPublisher.Write([ltLogWriter], ddError, Self, 'LoadFromHttpPage', E.Message);
  end;
end;

{procedure TIBHtmlParser.LoadFromFile(aFileName: string);
var
  iPersist: IPersistFile;
begin
  if Succeeded(FHTMLDoc.QueryInterface(IPersistFile, iPersist)) then
    try
      if Succeeded(iPersist.Load(PWideChar(aFileName), STGM_READ)) then
      begin
        while (FHTMLDoc.ReadyState <> 'complete') do
          Application.HandleMessage;
        FHTMLDoc.Charset := 'utf-8';
      end;
    finally
      iPersist := nil;
    end;
end;}

function TIBHtmlParser.Parse: TParseResult;
var
  i: Integer;
  j: Integer;
  iDocTable: IHTMLTable;
  iDocTableCell: IHTMLElement;
  iDocTableRow: IHTMLTableRow;
  iDocTables: IHTMLElementCollection;
  iDocTableCells: IHTMLElementCollection;
begin
  with Result do
  begin
    Currency := '';
    Description := '';
    Exchange := '';
    Symbol := '';
    IsExists := False;
  end;

  iDocTables := FHTMLDoc.All.Tags('TABLE') as IHTMLElementCollection;
  for i := 0 to iDocTables.Length - 1 do
  begin
    iDocTable := iDocTables.Item(i, 0) as IHTMLTable;
    if Assigned(iDocTable) then
      for j := 0 to iDocTable.Rows.Length - 1 do
      begin
        iDocTableRow := iDocTable.Rows.Item(j, EmptyParam) as IHTMLTableRow;
        iDocTableCells := iDocTableRow.cells as IHTMLElementCollection;
        if (iDocTableCells.Length > 1) then
        begin
          iDocTableCell := iDocTableCells.Item(0, EmptyParam) as IHTMLElement;
          if SameText(iDocTableCell.innerText, 'Symbol') then
            Result.Symbol := (iDocTableCells.Item(1, EmptyParam) as IHTMLElement).innerText
          else if SameText(iDocTableCell.innerText, 'Exchange') then
            Result.Exchange := (iDocTableCells.Item(1, EmptyParam) as IHTMLElement).innerText
          else if SameText(iDocTableCell.innerText, 'Currency') then
            Result.Currency := (iDocTableCells.Item(1, EmptyParam) as IHTMLElement).innerText
          else if SameText(iDocTableCell.innerText, 'Description/Name') then
            Result.Description := (iDocTableCells.Item(1, EmptyParam) as IHTMLElement).innerText;
          Result.IsExists := not Result.Symbol.IsEmpty and not Result.Exchange.IsEmpty
        end;
      end;
    if Result.IsExists then
      Exit;
  end;
end;

procedure TIBHtmlParser.SetHTMLText(const Value: WideString);
var
  vArray: OleVariant;
begin
  FHTMLDoc.Close;
  vArray := VarArrayCreate([0, 0], varVariant);
  vArray[0] := Value;
  FHTMLDoc.Write(PSafeArray(TVarData(vArray).vArray));
end;

function TIBHtmlParser.GetHTMLText: WideString;
begin
  Result := FHTMLDoc.Body.ParentElement.OuterHTML;
end;

end.
