{*******************************************************************************}
{                                                                               }
{               unit HtmlLib                                                    }
{                  v.3.0.0.7                                                    }
{             created 10/05/2012                                                }
{                                                                               }
{  procedures and functions for working with Html-objects                       }
{                                                                               }
{*******************************************************************************}

unit HtmlLib;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.ActiveX, System.Classes, Vcl.Forms, Vcl.Graphics, MSHTML, System.RegularExpressions,
  SHDocVw, System.SysUtils, System.Variants, System.StrUtils, HtmlConsts;
{$ENDREGION}

type
  THtmlLib = class(TObject)
  public
    class function EncodeXMLStr(const aValue: string): string;
    {
     Description:
       converts TColor type to html color palette
     Parameters:
       aWebBrowser - component TWebBrowser
    }
    class function ColorToHtml(aColor: TColor): string;
    {
     Description:
       converts SQL query text to html text with syntax highlighting
     Parameters:
       aSqlText - SQL-text
    }
    class function SqlToHtml(const aSqlText: string): string;
    {
     Description:
       converts XML text to html text with syntax highlighting
     Parameters:
       aXmlText - Xml-text
    }
    class function XmlToHtml(const aXmlText: string): string;
    {
     Description:
       Gets selected text in aWebBrowser
     Parameters:
       aWebBrowser - component TWebBrowser
    }
    class function GetSelectionText(var aWebBrowser: TWebBrowser): string;
    {
     Description:
       Writes html-text to clipboard
     Parameters:
       aWebBrowser - component TWebBrowser
    }
    class procedure CopyToClipboard(var aWebBrowser: TWebBrowser);
    {
     Description:
       Copies html text to clipboard
     Parameters:
       aWebBrowser - component TWebBrowser
       aHtmlText   - html-text
    }
    class procedure LoadStringToBrowser(var aWebBrowser: TWebBrowser; const aHtmlText: string);
    {
     Description:
       Write html text to an external file
     Parameters:
       aWebBrowser - component TWebBrowser
    }
    class procedure SaveHTMLSourceToFile(var aWebBrowser: TWebBrowser; const aFileName: string);
    {
     Description:
       Sets border color of TWebBrowser
     Parameters:
       aWebBrowser  - component TWebBrowser
       aBorderColor - color
    }
    class procedure SetBorderColor(var aWebBrowser: TWebBrowser; aBorderColor: TColor);
    {
     Description:
       Sets border style of TWebBrowser
     Parameters:
       aWebBrowser  - component TWebBrowser
       aBorderStyle - style: 'none'         No border is drawn
                             'dashed'       Border is a dashed line. (as of IE 5.5)
                             'dotted'       Border is a dotted line. (as of IE 5.5)
                             'double'       Border is a double line
                             'groove'       3-D groove is drawn
                             'inset'        3-D inset is drawn
                             'outset'       3-D outset is drawn
                             'ridge'        3-D ridge is drawn
                             'solid'        Border is a solid line
    }
    class procedure SetBorderStyle(var aWebBrowser: TWebBrowser; aBorderStyle: string);
    {
     Description:
       Returns filled table tag <TABLE>
     Parameters:
       aCaptionHead - variant array with table headers: VarArrayOf(['Caption 1','Caption 2'])
    }
   class function GetTableTag(aColumns: Variant; aTableCaption: string = ''): string;
    {
     Description:
       Returns a filled row of table <TR><TD>
     Parameters:
       aLineText - variant array with table row: VarArrayOf(['Row text 1','Row text 2'])
    }
    class function GetTableLineTag(aLineText: Variant): string;
    {
     Description:
       Returns colored text
     Parameters:
       aText  - text
       aColor - text color: red, green, blue or
                type of TColor: clRed, clGreen, clBlue
    }
    class function GetColorTag(aText: string; aColor: TColor): string; overload;
    class function GetColorTag(aText: string; aColor: string): string; overload;
    class function GetSpoilerTag(aCaption, aText: string; aTdStyle: string = ''): string;
    class function GetSrcSQLTag(aCaption, aText: string): string;

    class function GetBoldText(aText: string): string;
    class function GetCenterText(aText: string): string;
    class function GetH1(aText: string): string;
    class function GetH2(aText: string): string;
  end;

  THtmlBuilder = class

  end;

implementation

class procedure THtmlLib.LoadStringToBrowser(var aWebBrowser: TWebBrowser; const aHtmlText: string);
var
  iDocument : IHTMLDocument2;
  vHtmlText : OleVariant;
begin
  if (aWebBrowser.Document = nil) then
    aWebBrowser.Navigate(C_HTML_BLANK);
  while (aWebBrowser.Document = nil) do
    Application.ProcessMessages;
  iDocument    := aWebBrowser.Document as IHTMLDocument2;
  vHtmlText    := VarArrayCreate([0, 0], varVariant);
  vHtmlText[0] := aHtmlText;
  iDocument.Write(PSafeArray(TVarData(vHtmlText).VArray));
  iDocument.Close;
end;

class procedure THtmlLib.CopyToClipboard(var aWebBrowser: TWebBrowser);
begin
  if (aWebBrowser.Document <> nil) then
  begin
    aWebBrowser.ExecWB(OLECMDID_SELECTALL,      OLECMDEXECOPT_DONTPROMPTUSER);
    aWebBrowser.ExecWB(OLECMDID_COPY,           OLECMDEXECOPT_DONTPROMPTUSER);
    aWebBrowser.ExecWB(OLECMDID_CLEARSELECTION, OLECMDEXECOPT_DONTPROMPTUSER);
  end;
end;

class function THtmlLib.GetColorTag(aText: string; aColor: TColor): string;
begin
  Result := GetColorTag(aText, ColorToHtml(aColor));
end;

class function THtmlLib.GetBoldText(aText: string): string;
begin
  Result := Concat('<b>', aText, '</b>');
end;

class function THtmlLib.GetCenterText(aText: string): string;
begin
  Result := Concat('<span class="center">', aText, '</span>');
end;

class function THtmlLib.GetColorTag(aText, aColor: string): string;
begin
  Result := Concat('<span style="color:', aColor, '">', aText, '</span>');
end;

class function THtmlLib.GetH1(aText: string): string;
begin
  Result := Concat('<h1>', aText, '</h1>');
end;

class function THtmlLib.GetH2(aText: string): string;
begin
  Result := Concat('<h2>', aText, '</h2>');
end;

class function THtmlLib.GetSelectionText(var aWebBrowser: TWebBrowser): string;
var
  vDocument: Variant;
begin
  if (aWebBrowser.Document <> nil) then
  begin
    vDocument := aWebBrowser.Document;
    try
      Result := vDocument.Selection.CreateRange.Text
    finally
      vDocument := Unassigned;
    end;
  end
  else
    Result := '';
end;

class function THtmlLib.EncodeXMLStr(const aValue: string): string;
const
  HSym : array[0..3] of string = ('&amp;','&lt;','&gt;','&quot;');
  TSym : array[0..3] of string = ('&'    ,'<'   ,'>'   ,'"'     );
var
  i    : integer;
  sTmp : string;
begin
  for i := 1 to Length(aValue) do
    if (Ord(aValue[i]) in [14, 32, 35, 37, 40..59]) or (Ord(aValue[i]) >= 61)then
      sTmp := Concat(sTmp, aValue[i])
    else if (Ord(aValue[i]) in [10, 13]) then
      sTmp := Concat(sTmp, '<br>')
    else
    begin
      case aValue[i] of
       ' '  : sTmp := Concat(sTmp, '&nbsp;');        // space
       '<'  : sTmp := Concat(sTmp, '&lt;');     // <
       '>'  : sTmp := Concat(sTmp, '&gt;');     // >
       '&'  : sTmp := Concat(sTmp, '&amp;');    // &
       '"'  : sTmp := Concat(sTmp, '&quot;');   // "
       '''' : sTmp := Concat(sTmp, '&apos;');   // '
      else
        sTmp := Concat(sTmp, '&#' + IntToStr(Ord(aValue[i])) + ';');
    end;
  end;
  Result := sTmp.Replace('#xD;', '<br>').Replace('#xA;', '<br>');
end;

class function THtmlLib.ColorToHtml(aColor: TColor): string;
var
  nRGB: TColorRef;
begin
  nRGB   := ColorToRGB(aColor);
  Result := Format('#%.2x%.2x%.2x', [GetRValue(nRGB), GetGValue(nRGB), GetBValue(nRGB)]);
end;

class procedure THtmlLib.SetBorderStyle(var aWebBrowser: TWebBrowser; aBorderStyle: string);
var
  iDocument : IHTMLDocument2;
  iElement  : IHTMLElement;
begin
  iDocument := aWebBrowser.Document as IHTMLDocument2;
  if Assigned(iDocument) then
  begin
    iElement := iDocument.Body;
    if (iElement <> nil) then
      iElement.Style.BorderStyle := aBorderStyle;
  end;
end;

class procedure THtmlLib.SaveHTMLSourceToFile(var aWebBrowser: TWebBrowser; const aFileName: string);
var
  iPersistStream : IPersistStreamInit;
  iStreamAdapter : IStream;
  loFileStream   : TFileStream;
begin
  iPersistStream := aWebBrowser.Document as IPersistStreamInit;
  loFileStream   := TFileStream.Create(aFileName, fmCreate);
  try
    iStreamAdapter := TStreamAdapter.Create(loFileStream, soReference) as IStream;
    iPersistStream.Save(iStreamAdapter, True);
  finally
    loFileStream.Free;
  end;
end;

class procedure THtmlLib.SetBorderColor(var aWebBrowser: TWebBrowser; aBorderColor: TColor);
var
  iDocument : IHTMLDocument2;
  iElement  : IHTMLElement;
begin
  iDocument := aWebBrowser.Document as IHTMLDocument2;
  if Assigned(iDocument) then
  begin
    iElement := iDocument.Body;
    if (iElement <> nil) then
      iElement.Style.BorderColor := ColorToHtml(aBorderColor);
  end;
end;

class function THtmlLib.SqlToHtml(const aSqlText: string): string;
const
  C_COMMENTS_PATTERN = '(?is)((/\*.*?\*/)|(--.*?\n))';
  C_LEXEM_PATTERN = '(?i)(' +
    '\bAGGREGATE\b|\bALL\b|\bALTER\b|\bAND\b|\bANY\b|\bAS\b|\bASC\b|\bAVG\b|\bBEFORE\b|\bBEGIN\b|' +
    '\bBETWEEN\b|\bBULK\b|\bBY\b|\bCASE\b|\bCAST\b|\bCHAR\b|\bCHECK\b|\bCOLLECT\b|\bCOMMENT\b|' +
    '\bCOMMIT\b|\bCOUNT\b|\bCURRENT\b|\bCURRENT_USER\b|\bCURSOR\b|\bDATE\b\bDAY\b|\bDEC\b|' +
    '\bDECIMAL\b|\bDECLARE\b|\bDEFAULT\b|\bDELETE\b|\bDESC\b|\bDISTINCT\b|\bEACH\b|\bELSE\b|' +
    '\bELSIF\b|\bEND\b|\bEXCEPTION\b|\bEXECUTE\b|\bEXISTS\b|\bFALSE\b|\bFETCH\b|\bFIRST\b|' +
    '\bFOR\b|\bFORALL\b|\bFOUND\b|\bFROM\b|\bFULL\b|\bFUNCTION\b|\bGROUPING\b|\bHAVING\b|\bIF\b|' +
    '\bIN\b|\bINNER\b|\bINSERT\b|\bINTEGER\b|\bINTERSECT\b|\bINTERVAL\b|\bINTO\b|\bIS\b|\bJOIN\b|' +
    '\bLAST\b|\bLEFT\b|\bLEVEL\b|\bLIKE\b|\bLOOP\b|\bMAX\b|\bMIN\b|\bMONTH\b|\bNEXT\b|\bNEXTVAL\b|' +
    '\bNOT\b|\bNOTFOUND\b|\bNOWAIT\b|\bNULL\b|\bNULLS\b|\bNUMBER\b|\bNUMERIC\b|\bOF\b|\bOLD\b|' +
    '\bON\b|\bOR\b|\bORDER\b|\bOUT\b|\bOUTER\b|\bPLS_INTEGER\b|\bPOSITIVE\b|\bPRIOR\b|\bPROCEDURE\b|' +
    '\bRAISE\b|\bRANGE\b|\bRAW\b|\bREPLACE\b|\bRESULT\b|\bRETURN\b|\bRIGHT\b|\bROLLBACK\b|\bROW\b| ' +
    '\bROWCOUNT\b|\bROWID\b|\bROWTYPE\b|\bSELECT\b|\bSELF\b|\bSET\b|\bSETS\b|\bSTRING\b|\bSUBTYPE\b|' +
    '\bSUM\b|\bSYSDATE\b|\bTABLE\b|\bTHEN\b|\bTIME\b|\bTIMESTAMP\b|\bTO\b|\bTRANSACTION\b|' +
    '\bTRIGGER\b|\bTRIM\b|\bTRUE\b|\bTYPE\b|\bUNDER\b|\bUNION\b|\bUNIQUE\b|\bUPDATE\b|\bUROWID\b|' +
    '\bUSE\b|\bUSER\b|\bUSING\b|\bVALUE\b|\bVALUES\b|\bVARCHAR\b|\bVARCHAR2\b|\bVARIABLE\b|\bWHEN\b|' +
    '\bWHERE\b|\bWHILE\b|\bWITH\b|\bXOR\b|\bYEAR\b|\bVALUES\b||\bGENERATOR\b||\bCHARACTER\b||\bNONE\b|' +
    '\bFLOAT\b|\bCREATE\b|\bBLOB\b|\bSUB_TYPE\b|\bSEGMENT\b|\bSIZE\b||\bADD\b||\bCONSTRAINT\b||\bPRIMARY\b|' +
    '\bKEY\b|\bDOMAIN\b|\bSMALLINT\b|' +
    ')';
begin
  Result := aSqlText.Replace(sLineBreak, C_HTML_BREAK);
  Result := TRegEx.Replace(Result, C_LEXEM_PATTERN, '<b>$1</b>');
  Result := TRegEx.Replace(Result, C_COMMENTS_PATTERN, '<span style="color:DarkCyan">$1</span>');
end;

class function THtmlLib.XmlToHtml(const aXmlText: string): string;
begin
  Result := aXmlText;
  Result := Concat('<pre class="xml-code">', THtmlLib.EncodeXMLStr(Result), '</pre>');
end;

class function THtmlLib.GetTableTag(aColumns: Variant; aTableCaption: string = ''): string;
const
  C_TABLE_TAG = '<table>';
var
  i           : Integer;
  nArrayBound : Byte;
  sTableTag   : string;
  sTrTag      : string;
begin
  if VarIsArray(aColumns) then
  begin
    nArrayBound := VarArrayHighBound(aColumns, 1);
    sTableTag   := C_TABLE_TAG;
    if not aTableCaption.IsEmpty then
      sTableTag := Concat(sTableTag, '<caption>', aTableCaption, '</caption>');
    sTrTag := '<thead><tr>';
    for i := VarArrayLowBound(aColumns, 1) to nArrayBound do
      sTrTag := Concat(sTrTag, '<th>', VarToStr(aColumns[i]), '</th>');
    sTrTag := Concat(sTrTag,  '</tr></thead>');
  end
  else
  begin
    sTableTag := C_TABLE_TAG;
    if not aTableCaption.IsEmpty then
      sTableTag := Concat(sTableTag, '<caption>', aTableCaption, '</caption>');
    sTrTag := Concat('<thead><tr><th>', VarToStr(aColumns), '</th></tr></thead>')
  end;
  Result := Concat(sTableTag, sTrTag);
end;

class function THtmlLib.GetSpoilerTag(aCaption, aText: string; aTdStyle: string = ''): string;
begin
  if (Trim(aText) <> '') then
    Result := Concat(
                   '<tr><td ', IfThen(aTdStyle.IsEmpty, '', aTdStyle), ' class="msgBody">',
                     '<table width="100&#37;" border="0" bgcolor="#E0E0E0" cellspacing="0" cellpadding="4" style="border:solid 1px #888888;margin:0px">',
                       '<tr>',
                         '<td>',
                           '<span style="font-family:monospace;padding:1px;cursor:pointer;background-color:#E8E8E8;border:1px solid #888888;txt-align:center;" ', 'onclick="var el=this.parentNode.parentNode.parentNode.rows[1]; el.style.display=el.style.display==''none''?'''':''none'';this.innerHTML=this.innerHTML==''+''?''-'':''+'';">+</span> ',
                           aCaption,
                         '</td>',
                       '</tr>',
                       '<tr style="display:none">',
                         '<td bgcolor="#E8E8E8">',
                           aText,
                         '</td>',
                      '</tr>',
                    '</table>',
                   '</td></tr>');
end;

class function THtmlLib.GetSrcSQLTag(aCaption, aText: string): string;
begin
  if (Trim(aText) <> '') then
    Result := Concat(
                     '<table>',
                       '<tr>',
                         '<td>',
                           aCaption,
                         '</td>',
                       '</tr>',
                       '<tr>',
                         '<td class="sql-code"><pre>',
                           SqlToHtml(aText),
                         '</pre></td>',
                      '</tr>',
                    '</table>'
                    );
end;

class function THtmlLib.GetTableLineTag(aLineText: Variant): string;
var
  i      : Integer;
  sTrTag : string;
begin
  if VarIsArray(aLineText) then
  begin
    sTrTag := '<TR>';
    for i := VarArrayLowBound(aLineText, 1) to VarArrayHighBound(aLineText, 1) do
      sTrTag := Concat(sTrTag, '<TD>', IfThen(VarToStr(aLineText[i]).IsEmpty, C_HTML_NBSP, VarToStr(aLineText[i])), '</TD>');
    sTrTag := Concat(sTrTag, '</TR>');
  end
  else
    sTrTag := Concat('<TR><TD>', IfThen(VarToStr(aLineText).IsEmpty, C_HTML_NBSP, VarToStr(aLineText)), '</TD></TR>');
  Result := sTrTag;
end;

end.
