unit ParametersStore;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.Classes, Generics.Collections, Messages, System.Rtti, System.TypInfo, System.Variants,
  System.SysUtils, Data.DB, Vcl.StdCtrls, XmlFiles, DebugWriter,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} HtmlLib, DaModule.Utils, Common.Types,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Comp.DataSet;
{$ENDREGION}

type
  TParametersStore = class
  private
    FConnection: TFDConnection;
    FDescription: string;
    FIdentityName: string;
    FPropertiesList: TStringList;
    FStoreComponent: TComponent;
    FXmlFile: TXMLFile;
    function GetXMLText: string;
    function ReadParamsFromDB: string;
    procedure SetXMLText(const Value: string);
    procedure WriteParamsToDB;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Restore;
    procedure Store;

    property Description    : string        read FDescription    write FDescription;
    property Connection     : TFDConnection read FConnection     write FConnection;
    property IdentityName   : string        read FIdentityName   write FIdentityName;
    property PropertiesList : TStringList   read FPropertiesList write FPropertiesList;
    property StoreComponent : TComponent    read FStoreComponent write FStoreComponent;
    property XMLText        : string        read GetXMLText      write SetXMLText;
  end;

implementation

constructor TParametersStore.Create;
begin
  inherited;
  FPropertiesList := TStringList.Create;
  FXmlFile        := TXMLFile.Create;
end;

destructor TParametersStore.Destroy;
begin
  FreeAndNil(FPropertiesList);
  FreeAndNil(FXmlFile);
  inherited;
end;

function TParametersStore.GetXMLText: string;
begin
  Result := FXmlFile.XMLText;
end;

procedure TParametersStore.SetXMLText(const Value: string);
begin
  if not Value.IsEmpty then
    FXmlFile.XMLText := Value;
end;

procedure TParametersStore.Store;
var
  i: Integer;
  loContext: TRttiContext;
  loProperty: TRttiProperty;
  vValue: Variant;
begin
  if FXmlFile.XMLText.IsEmpty then
    FXmlFile.XMLText := FXmlFile.GetXMLDeclaration;
  if Assigned(StoreComponent) then
  begin
    for i := 0 to FPropertiesList.Count - 1 do
    begin
      loContext := TRttiContext.Create;
      try
        loProperty := loContext.GetType(StoreComponent.ClassType).GetProperty(FPropertiesList[i]);
        if Assigned(loProperty) and loProperty.IsReadable then
        begin
          try
            vValue := '';
            case loProperty.PropertyType.TypeKind of
              tkEnumeration:
                begin
                  if SameText(loProperty.PropertyType.Name, 'Boolean') then
                    vValue := Integer(loProperty.GetValue(StoreComponent).AsBoolean)
                  else
                    vValue := loProperty.GetValue(StoreComponent).AsOrdinal;
                end;
              tkUnknown:
                vValue := '';
              tkInteger:
                vValue := loProperty.GetValue(StoreComponent).AsInteger;
              tkInt64 :
                vValue := loProperty.GetValue(StoreComponent).AsInt64;
              tkFloat:
                vValue := loProperty.GetValue(StoreComponent).AsExtended;
              tkVariant:
                vValue := loProperty.GetValue(StoreComponent).AsVariant;
              tkInterface:
                vValue := loProperty.GetValue(StoreComponent).AsInterface;
              tkSet:
                vValue := GetSetProp(StoreComponent, FPropertiesList[i]);
              tkUString, tkLString, tkWString, tkString, tkChar, tkWChar:
                vValue := loProperty.GetValue(StoreComponent).AsString;
            else
              vValue := loProperty.GetValue(StoreComponent).AsVariant;
            end;
            FXmlFile.WriteValue('values', FPropertiesList[i], vValue);
          except
          end;
        end;
      finally
        loContext.Free;
      end;
    end;
    WriteParamsToDB;
  end;
end;

procedure TParametersStore.Restore;
var
  i, k: Integer;
  loContext: TRttiContext;
  loProperty: TRttiProperty;
  loValue: TValue;
  sXmlText: string;
  vValue: Variant;
begin
  if Assigned(StoreComponent) then
  begin
    sXmlText := ReadParamsFromDB;
    if not sXmlText.IsEmpty then
    begin
      FXmlFile.XMLText := sXmlText;
      loContext := TRttiContext.Create;
      try
        for i := 0 to FPropertiesList.Count - 1 do
        begin
          vValue := FXmlFile.ReadValue('values', FPropertiesList[i], Unassigned);
          if (vValue <> Unassigned) then
          begin
            loProperty := loContext.GetType(StoreComponent.ClassType).GetProperty(FPropertiesList[i]);
            if Assigned(loProperty) and loProperty.IsWritable then
              try
                loValue := TValue.Empty;
                case loProperty.PropertyType.TypeKind of
                  tkUnknown:
                    loValue := TValue.Empty;
                  tkInteger:
                    loValue := TValue.From<Integer>(vValue);
                  tkInt64:
                    loValue := TValue.From<Int64>(vValue);
                  tkInterface:
                    loValue := TValue.From<IInterface>(vValue);
                  tkEnumeration:
                    begin
                      if SameText(loProperty.PropertyType.Name, 'Boolean') then
                        loValue := TValue.From<Boolean>(not SameText(vValue, '0'))
                      else
                        loValue := TValue.FromOrdinal(loProperty.PropertyType.Handle, GetEnumValue(loProperty.PropertyType.Handle, vValue));
                    end;
                  tkFloat:
                    loValue := StrToFloat(vValue);
                  tkVariant:
                    loValue := TValue.FromVariant(vValue);
                  tkUString, tkWChar, tkLString, tkWString, tkString, tkChar:
                    loValue := VarToStr(vValue);
                  tkSet:
                    begin
                      k := StringToSet(loProperty.PropertyType.Handle, vValue);
                      TValue.Make(@k, loProperty.PropertyType.Handle, loValue);
                    end;
                end;
                loProperty.SetValue(StoreComponent, loValue);
              except

              end;
          end;
        end;
      finally
        loContext.Free;
      end;
    end;
  end;
end;

function TParametersStore.ReadParamsFromDB: string;
var
  Query: TFDQuery;
begin
  Result := '';
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SELECT XML_PARAMS, ID FROM STORE_TREE WHERE IDENTITY = :IDENTITY';
    Query.ParamByName('IDENTITY').Value := IdentityName;
    Query.Prepare;
    Query.Open;
    if not Query.IsEmpty then
      Result := Query.FieldByName('XML_PARAMS').AsString;
    Query.Close;
  finally
    FreeAndNil(Query);
  end;
end;

procedure TParametersStore.WriteParamsToDB;
resourcestring
  C_SQL_EXISTS_TEXT = 'SELECT ID FROM STORE_TREE WHERE IDENTITY = :IDENTITY';
  C_SQL_UPDATE_TEXT = 'UPDATE STORE_TREE SET XML_PARAMS = :XML_PARAMS WHERE ID = :ID';
  C_SQL_INSERT_TEXT = 'INSERT INTO STORE_TREE (XML_PARAMS, IDENTITY, DESCRIPTION, ID) VALUES (:XML_PARAMS, :IDENTITY, :DESCRIPTION, NULL)';
var
  RecordId: Integer;
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := C_SQL_EXISTS_TEXT;
    try
      Query.ParamByName('IDENTITY').AsString := IdentityName;
      Query.Prepare;
      Query.Open;
      RecordId := Query.FieldByName('ID').AsInteger;
      Query.Close;
    except
      on E: Exception do
      begin
        LogWriter.Write(ddError, Self, 'WriteParamsToDB', E.Message + TDModUtils.GetQueryInfo(Query));
        raise;
      end;
    end;

    try
      if (RecordId > 0) then
      begin
        Query.SQL.Text := C_SQL_UPDATE_TEXT;
        Query.ParamByName('ID').Value         := RecordId;
        Query.ParamByName('XML_PARAMS').Value := FXmlFile.XMLText;
        Query.Prepare;
        try
          Query.ExecSQL;
        except
          on Ee:Exception do
          begin
            LogWriter.Write(ddError, Self, 'WriteParamsToDB', Ee.Message);
            Exit;
          end;
        end;
      end
      else
      begin
        Query.SQL.Text := C_SQL_INSERT_TEXT;
        Query.ParamByName('IDENTITY').Value    := IdentityName;
        Query.ParamByName('XML_PARAMS').Value  := FXmlFile.XMLText;
        Query.ParamByName('DESCRIPTION').Value := Description;
        try
          Query.Prepare;
          Query.ExecSQL;
        except
          on Er:Exception do
          begin
            LogWriter.Write(ddError, Self, 'WriteParamsToDB', Er.Message);
            Exit;
          end;
        end;
      end;

      if Connection.Transaction.Active then
        Connection.Transaction.CommitRetaining;
    except
      on En: Exception do
      begin
        LogWriter.Write(ddError, Self, 'WriteParamsToDB', En.Message + TDModUtils.GetQueryInfo(Query));
        Connection.Transaction.Rollback;
        //Connection.Transaction.StartTransaction;
      end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;

end.
