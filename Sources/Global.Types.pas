unit Global.Types;

interface

{$REGION 'Region uses'}
uses
  // Standart units
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.Forms, System.Variants, XmlFiles,
  ParametersStore, Vcl.StdCtrls, Vcl.Buttons, Vcl.Controls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Dialogs, Vcl.Samples.Spin,
  {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} System.Generics.Defaults, System.Generics.Collections,
  DebugWriter, System.IOUtils, DaImages, IABSocketAPI_const, Vcl.NumberBox, Vcl.Grids, Vcl.ValEdit, Xml.XMLDoc,
  Common.Types, System.Actions, Vcl.ActnList, MessageDialog, Vcl.CheckLst, IABFunctions.MessageCodes, IABFunctions.Helpers,
  FireDAC.Comp.Client;
{$ENDREGION}

type
  TfrmParameters = class(TForm)
    aCopyToAll: TAction;
    ActionList: TActionList;
    aGetPathToBackup: TAction;
    aGetPathToContractInspector: TAction;
    btnCancel: TBitBtn;
    btnCopyToAll: TButton;
    btnOk: TBitBtn;
    cbAllowEnterAutoorder: TCheckBox;
    cbBackupIsActive: TCheckBox;
    cbIsShowCancelBox: TCheckBox;
    cbShowQbalgo: TCheckBox;
    cbUseAdjustment: TCheckBox;
    cbUseVolatility: TCheckBox;
    cbViewMarketScanner: TCheckBox;
    edAlgorithmSizeLimit: TNumberBox;
    edAlgorithmTotalValueLimit: TNumberBox;
    edBackupScheduleTime: TDateTimePicker;
    edCountOfDays: TSpinEdit;
    edMaxAllowedPrice: TNumberBox;
    edMaxSizeOfLogFile: TSpinEdit;
    edMinAllowedPrice: TNumberBox;
    edMinMaxGradientValue: TNumberBox;
    edNumberOfTicks: TNumberBox;
    edOrderQuantityMax: TNumberBox;
    edPathToBackup: TButtonedEdit;
    edPathToContractInspector: TButtonedEdit;
    edPercent: TSpinEdit;
    edPercentage: TNumberBox;
    edPriceScheduleCheck: TDateTimePicker;
    edTimeCloseExchange: TDateTimePicker;
    edTimeOpenExchange: TDateTimePicker;
    edTotalValueLimit: TNumberBox;
    gbAutoorder: TGroupBox;
    gbEmergencySettings: TGroupBox;
    lbCodeErrors: TCheckListBox;
    lbGenericTickRequired: TCheckListBox;
    lblAdjustmentCoef: TLabel;
    lblAdjustmentCoefNumberOfHours: TLabel;
    lblAdjustmentCoefPrice: TLabel;
    lblAdjustmentCoefVolatility: TLabel;
    lblAlgorithmSizeLimit: TLabel;
    lblAlgorithmTotalValueLimit: TLabel;
    lblBackupScheduleTime: TLabel;
    lblClosingTime: TLabel;
    lblCountOfDays: TLabel;
    lblDeleteInstruments: TLabel;
    lblMaxAdjustmentCoef: TLabel;
    lblMaxAllowedPrice: TLabel;
    lblMaxSizeOfLogFile: TLabel;
    lblMinAllowedPrice: TLabel;
    lblMinMaxGradientValue: TLabel;
    lblNumberOfTicks: TLabel;
    lblOpeningTime: TLabel;
    lblOrderQuantityMax: TLabel;
    lblPathToBackup: TLabel;
    lblPathToContractInspector: TLabel;
    lblPercentage: TLabel;
    lblPriceScheduleCheck: TLabel;
    lblTotalValueLimit: TLabel;
    lblWaitingTime: TLabel;
    lbSecurityType: TListBox;
    OpenDialog: TFileOpenDialog;
    pcMain: TPageControl;
    pnlAdjustment: TPanel;
    pnlBottom: TPanel;
    pnlVolatility: TPanel;
    rbLMT: TRadioButton;
    rbMKT: TRadioButton;
    seAdjustmentCoef: TSpinEdit;
    seAdjustmentCoefMax: TSpinEdit;
    seAdjustmentCoefNumberOfHours: TSpinEdit;
    seAdjustmentCoefPrice: TSpinEdit;
    seAdjustmentCoefVolatility: TSpinEdit;
    seWaitingTime: TSpinEdit;
    tsCoefficients: TTabSheet;
    tsCommon: TTabSheet;
    tsDatabaseBackup: TTabSheet;
    tsEmergencySettings: TTabSheet;
    tsExchange: TTabSheet;
    tsGenericTickRequired: TTabSheet;
    tsLogFile: TTabSheet;
    tsPrecautionarySettings: TTabSheet;
    tsSchedule: TTabSheet;
    procedure aCopyToAllExecute(Sender: TObject);
    procedure aGetPathToBackupExecute(Sender: TObject);
    procedure aGetPathToContractInspectorExecute(Sender: TObject);
    procedure cbUseAdjustmentClick(Sender: TObject);
    procedure cbUseVolatilityClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbSecurityTypeClick(Sender: TObject);
    procedure OnPrecautionaryChangeValue(Sender: TObject);
    procedure rbLMTClick(Sender: TObject);
  public const
    C_TAB_COMMON                 = 0;
    C_TAB_PRECAUTIONARY_SETTINGS = 1;
    C_TAB_EMERGENCY_SETTINGS     = 2;
    C_TAB_COEFFICIENTS           = 3;
    C_TAB_EXCHANGE               = 4;
    C_TAB_LOGFILE                = 5;
    C_TAB_SCHEDULE               = 6;
    C_TAB_DATABASE_BACKUP        = 7;
  public
    procedure SetTabVisible(const aNumTab: Integer);
    procedure Initialize;
    procedure Deinitialize;
  end;

  TNordNetParams = record
  const
    C_LOGIN    = 'benman';
    C_PASSWORD = 'pappa567';
    C_SECTION  = 'NordNetBroker';
  end;

  TInteractiveParams = record
  const
    C_CLIENT_ID  = 0;
    C_PORT       = 7496;
    C_IP_ADDRESS = '';
    C_SECTION    = 'InteractiveBroker';
    C_LOGIN      = 'thober567';
    C_PASSWORD   = 'hannathomas639';
  end;

  TAdjustmentCoefficients = record
    Coef              : Integer;
    CoefMax           : Integer;
    UseVolatilityCoef : Boolean;
    UseAdjustmentCoef : Boolean;
    Price             : Integer;
    Volatility        : Integer;
    NumberOfHours     : Integer;
  end;

  TExchange = record
    TimeCloseExchange : TTime;
    TimeOpenExchange  : TTime;
    ShowQBALGO        : Boolean;
  end;

  TEmergencySettings = record
    OrderType : TIABOrderType;
    Percent   : Double;
  end;

  TPrecautionarySettings = TDictionary<TIABSecurityType, TPrecautionarySettingArray>;

  TGeneral = class(TComponent)
  const
    C_ATTR_CLIENT_ID  = 'ClientID';
    C_ATTR_IP_ADDRESS = 'IpAddress';
    C_ATTR_LOGIN      = 'Login';
    C_ATTR_PASSWORD   = 'Password';
    C_ATTR_PORT       = 'Port';

    C_KEY_ALLOW_ENTER_AUTOORDER = 'AllowEnterAutoorder';
    C_KEY_AUTO_TRADES_NUM       = 'AutoTradesNum';
    C_KEY_QUALIFIER_NUM         = 'QualifierNum';
    C_KEY_WAITING_TIME          = 'WaitingTime';

    C_KEY_AUTOORDER_ADJ_COEF            = 'AdjustmentCoef';
    C_KEY_AUTOORDER_ADJ_COEF_MAX        = 'AdjustmentCoefMax';
    C_KEY_AUTOORDER_ADJ_USE_VOLAT_COEF  = 'AdjustmentUseVolatilityCoef';
    C_KEY_AUTOORDER_ADJ_USE_ADJ_COEF    = 'AdjustmentUseAdjustmentCoef';
    C_KEY_AUTOORDER_ADJ_COEF_PRICE      = 'AdjustmentCoefPrice';
    C_KEY_AUTOORDER_ADJ_COEF_VOLATILITY = 'AdjustmentCoefVolatility';
    C_KEY_AUTOORDER_ADJ_NUMBER_HOURS    = 'AdjustmentNumberOfHours';

    C_KEY_PRECAUTIONARY_PERCENTAGE               = 'Percentage';
    C_KEY_PRECAUTIONARY_TOTALVALUELIMIT          = 'TotalValueLimit';
    C_KEY_PRECAUTIONARY_NUMBEROFTICKS            = 'NumberOfTicks';
    C_KEY_PRECAUTIONARY_ALGORITHMTOTALVALUELIMIT = 'AlgorithmTotalValueLimit';
    C_KEY_PRECAUTIONARY_ALGORITHMSIZELIMIT       = 'AlgorithmSizeLimit';
    C_KEY_PRECAUTIONARY_ORDERQUANTITYMAX         = 'OrderQuantityMax';
    C_KEY_PRECAUTIONARY_MAXALLOWEDPRICE          = 'MaxAllowedPrice';
    C_KEY_PRECAUTIONARY_MINALLOWEDPRICE          = 'MinAllowedPrice';

    C_KEY_BACKUP_IS_ACTIVE   = 'IsActiveBackup';
    C_KEY_PATH_BACKUP_FOLDER = 'PathToBackupFolder';
    C_KEY_BACKUP_TIME        = 'BackupTime';

    C_KEY_EMERGENCY_CLOSE_ORDERTYPE = 'OrderType';
    C_KEY_EMERGENCY_PERCENT         = 'Percent';

    C_KEY_GENERIC_TICK_REQUIRED   = 'GenericTickRequired';
    C_KEY_IS_SHOW_CANCEL_BOX      = 'IsShowCancelBox';
    C_KEY_MIN_MAX_GRADIENT_VALUE  = 'MinMaxGradientValue';
    C_KEY_NODE_ID                 = 'NodeID';
    C_KEY_OCA_GROUP_NUMBER        = 'OCAGroupNumber';
    C_KEY_PATH_CONTRACT_INSPECTOR = 'PathToContractInspector';
    C_KEY_PERCENT                 = 'Percent';
    C_KEY_PRICE_SCHEDULE_CHECK    = 'PriceScheduleCheck';
    C_KEY_PRICE_SCHEDULE_ERRORS   = 'PriceScheduleCodeErrors';
    C_KEY_PRICE_SCHEDULE_SAVE     = 'PriceScheduleSave';
    C_KEY_RECORD_ID               = 'RecordID';
    C_KEY_SCAN_ID                 = 'ScanID';
    C_KEY_SCENARIO_NUMBER         = 'ScenarioNumber';
    C_KEY_SELECT_INST_ORDER_NN    = 'SelectInstrumentInOrderNN';
    C_KEY_SHOW_QBALGO             = 'ShowQbalgo';
    C_KEY_TIME_CLOSE_EXCHANGE     = 'TimeClose';
    C_KEY_TIME_OPEN_EXCHANGE      = 'TimeOpen';
    C_KEY_USE_IB                  = 'UseIB';
    C_KEY_USE_NN                  = 'UseNN';
    C_KEY_VIEW_MARKET_SCANNER     = 'ViewMarketScanner';

    C_SECTION_GLOBAL                = 'GlobalParams';
    C_SECTION_AUTOORDERS            = 'Autoorders';
    C_SECTION_BACKUP                = 'Backup';
    C_SECTION_COLUMN_SETTINGS       = 'ColumnSettings';
    C_SECTION_COMMON_PARAMS         = 'CommonParams';
    C_SECTION_CONNECTIONS           = 'Connections';
    C_SECTION_EMERGENCY_SETTINGS    = 'EmergencySettings';
    C_SECTION_EXCHANGE              = 'Exchange';
    C_SECTION_PRECAUTIONARY_SETTING = 'PrecautionarySetting';
    C_ERROR_CODES = '200;203;20004;';
  private
    FAdjustmentCoefficients: TAdjustmentCoefficients;
    FAllowEnterAutoorder: Boolean;
    FAppStartTime: TDateTime;
    FBackupTime: TTime;
    FChartStartTime: TDateTime;
    FCountOfDays: Integer;
    FDBVersion: string;
    FEmergencySettings: TEmergencySettings;
    FExchange: TExchange;
    FConnection: TFDConnection;
    FID: Integer;
    FInstanceNum: Integer;
    FInteractiveParams: TInteractiveParams;
    FIsActiveBackup: Boolean;
    FIsBackuped: Boolean;
    FIsRealTrades: Boolean;
    FIsShowCancelBox: Boolean;
    FMaxSizeOfLogFile: Integer;
    FMinMaxGradientValue: Integer;
    FModuleVersion: string;
    FNodeID: Integer;
    FNordNetParams: TNordNetParams;
    FParametersStore: TParametersStore;
    FPathToBackupFolder: string;
    FPathToContractInspector: string;
    FPrecautionarySettings: TPrecautionarySettings;
    FPriceScheduleCheck: TTime;
    FPriceScheduleCodeErrors: string;
    FGenericTickRequired: TIABExMktDataSet;
    FRecordID: Integer;
    FScanID: Integer;
    FViewMarketScanner: Boolean;
    FWaitingTime: Integer;
    FXMLFile: TXMLFile;
    function GetModuleVersion: string;
    procedure LoadParamsFromXml;
    procedure SaveParamsToXml;
    procedure SetConnection(const Value: TFDConnection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize;
    procedure Deinitialize;
    class procedure ShowParametersDialog;
    class procedure ShowParametersTab(const aNumTab: Integer);
    class function GetAppVersion: string;

    function GetNextID: Integer;
    function GetNextInstanceNum: Integer;
    function GetNextNodeID: Integer;
    function GetNextRecordID: Integer;
    function GetNextScanID: Integer;
    procedure NoHibernate;

    property AdjustmentCoefficients  : TAdjustmentCoefficients read FAdjustmentCoefficients  write FAdjustmentCoefficients;
    property AllowEnterAutoorder     : Boolean                 read FAllowEnterAutoorder     write FAllowEnterAutoorder;
    property AppStartTime            : TDateTime               read FAppStartTime;
    property BackupTime              : TTime                   read FBackupTime              write FBackupTime;
    property ChartStartTime          : TDateTime               read FChartStartTime          write FChartStartTime;
    property CountOfDays             : Integer                 read FCountOfDays             write FCountOfDays;
    property DBVersion               : string                  read FDBVersion               write FDBVersion;
    property EmergencySettings       : TEmergencySettings      read FEmergencySettings       write FEmergencySettings;
    property Exchange                : TExchange               read FExchange                write FExchange;
    property Connection              : TFDConnection           read FConnection              write SetConnection;
    property InteractiveParams       : TInteractiveParams      read FInteractiveParams       write FInteractiveParams;
    property IsActiveBackup          : Boolean                 read FIsActiveBackup          write FIsActiveBackup;
    property IsBackuped              : Boolean                 read FIsBackuped              write FIsBackuped;
    property IsRealTrades            : Boolean                 read FIsRealTrades            write FIsRealTrades;
    property IsShowCancelBox         : Boolean                 read FIsShowCancelBox         write FIsShowCancelBox;
    property MaxSizeOfLogFile        : Integer                 read FMaxSizeOfLogFile        write FMaxSizeOfLogFile;
    property MinMaxGradientValue     : Integer                 read FMinMaxGradientValue     write FMinMaxGradientValue;
    property ModuleVersion           : string                  read GetModuleVersion         write FModuleVersion;
    property NordNetParams           : TNordNetParams          read FNordNetParams           write FNordNetParams;
    property PathToBackupFolder      : string                  read FPathToBackupFolder      write FPathToBackupFolder;
    property PathToContractInspector : string                  read FPathToContractInspector write FPathToContractInspector;
    property PrecautionarySettings   : TPrecautionarySettings  read FPrecautionarySettings   write FPrecautionarySettings;
    property PriceScheduleCheck      : TTime                   read FPriceScheduleCheck      write FPriceScheduleCheck;
    property PriceScheduleCodeErrors : string                  read FPriceScheduleCodeErrors write FPriceScheduleCodeErrors;
    property GenericTickRequired     : TIABExMktDataSet        read FGenericTickRequired     write FGenericTickRequired;
    property ViewMarketScanner       : Boolean                 read FViewMarketScanner       write FViewMarketScanner;
    property WaitingTime             : Integer                 read FWaitingTime             write FWaitingTime;
    property XMLFile                 : TXMLFile                read FXMLFile                 write FXMLFile;
  end;

  TBaseClass = class
  private
    FRecordId: Integer;
    FName: String;
  public
    constructor Create; virtual;
    procedure FromDB(aID: Integer); virtual;
    procedure SaveToDB; virtual;
    class procedure DeleteFromDB(aID: Integer); virtual;
    class function GetListSQL: string; virtual;
    class function GetListCaption: string; virtual;

    property RecordId: Integer read FRecordId write FRecordId;
    property Name: String read FName write FName;
  end;

  TCustomBaseClass = class of TBaseClass;

var
  General: TGeneral;

implementation

{$R *.dfm}

{ TGeneral }

constructor TGeneral.Create(AOwner: TComponent);
const
  PreArr: TPrecautionarySettingArray = (0,0,0,0,0,0,0,0);
begin
  inherited;
  FXMLFile := TXMLFile.Create(TPath.Combine(GetEnvironmentVariable('USERPROFILE'), 'RoboTrade.xml'));
  FPrecautionarySettings := TPrecautionarySettings.Create(Ord(High(TIABSecurityType)) + 1);

  for var st := Low(TIABSecurityType) to High(TIABSecurityType) do
    FPrecautionarySettings.AddOrSetValue(st, PreArr);

  FParametersStore := TParametersStore.Create;
  FParametersStore.StoreComponent := Self;
  FParametersStore.IdentityName := C_SECTION_GLOBAL;
  FParametersStore.PropertiesList.Add('DBVersion');
  FParametersStore.PropertiesList.Add('IsRealTrades');
  FID := 0;
  FIsBackuped   := False;
  FAppStartTime := Date;
end;

destructor TGeneral.Destroy;
begin
  FreeAndNil(FParametersStore);
  FreeAndNil(FPrecautionarySettings);
  FreeAndNil(FXMLFile);
  inherited;
end;

procedure TGeneral.Initialize;
begin
  LoadParamsFromXml;
end;

procedure TGeneral.Deinitialize;
begin
  SaveParamsToXml;
end;

class function TGeneral.GetAppVersion: string;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(Application.ExeName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(Application.ExeName), 0, VerInfoSize, PVerInfo) then
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
          Result := Format('%d.%d.%d.%d', [
            HiWord(dwFileVersionMS),   //Major
            LoWord(dwFileVersionMS),   //Minor
            HiWord(dwFileVersionLS),   //Release
            LoWord(dwFileVersionLS)]); //Build
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;

function TGeneral.GetModuleVersion: string;
begin
  if FModuleVersion.IsEmpty then
    FModuleVersion := GetAppVersion;
  Result := FModuleVersion;
end;

procedure TGeneral.SetConnection(const Value: TFDConnection);
begin
  if (FConnection <> Value) then
    FConnection := Value;
  FParametersStore.Connection := FConnection;
end;

procedure TGeneral.SaveParamsToXml;
var
  PreArr: TPrecautionarySettingArray;
begin
  try
    //C_SECTION_AUTOORDERS
    XmlFile.WriteBool(C_SECTION_AUTOORDERS, C_KEY_ALLOW_ENTER_AUTOORDER, FAllowEnterAutoorder);
    XmlFile.WriteBool(C_SECTION_AUTOORDERS, C_KEY_VIEW_MARKET_SCANNER, FViewMarketScanner);
    XmlFile.WriteInteger(C_SECTION_AUTOORDERS, C_KEY_SCAN_ID, FScanID, 'Last ScanID');
    XmlFile.WriteInteger(C_SECTION_AUTOORDERS, C_KEY_AUTO_TRADES_NUM, FInstanceNum, 'Last InstanceNum');
    XmlFile.WriteInteger(C_SECTION_AUTOORDERS, C_KEY_WAITING_TIME, FWaitingTime, 'Waiting time since receiving the mother''s part (ms)');

    XmlFile.WriteInteger(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_COEF, FAdjustmentCoefficients.Coef);
    XmlFile.WriteInteger(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_COEF_MAX, FAdjustmentCoefficients.CoefMax);
    XmlFile.WriteInteger(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_COEF_PRICE, FAdjustmentCoefficients.Price);
    XmlFile.WriteInteger(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_COEF_VOLATILITY, FAdjustmentCoefficients.Volatility);
    XmlFile.WriteInteger(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_NUMBER_HOURS, FAdjustmentCoefficients.NumberOfHours);
    XmlFile.WriteBool(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_USE_VOLAT_COEF, FAdjustmentCoefficients.UseVolatilityCoef);
    XmlFile.WriteBool(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_USE_ADJ_COEF, FAdjustmentCoefficients.UseAdjustmentCoef);

    //C_SECTION_BACKUP
    XmlFile.WriteBool(C_SECTION_BACKUP, C_KEY_BACKUP_IS_ACTIVE, FIsActiveBackup);
    XmlFile.WriteString(C_SECTION_BACKUP, C_KEY_PATH_BACKUP_FOLDER, FPathToBackupFolder);
    XmlFile.WriteTime(C_SECTION_BACKUP, C_KEY_BACKUP_TIME, FBackupTime);
    //C_CFG_SECTION_DEBUG
    XmlFile.WriteInt64(TLogWriter.C_CFG_SECTION_DEBUG, TLogWriter.C_CFG_KEY_MAX_SIZE, FMaxSizeOfLogFile, 'Max Size Of Log File (Mb)');
    XmlFile.WriteInteger(TLogWriter.C_CFG_SECTION_DEBUG, TLogWriter.C_CFG_COUNT_OF_DAYS, FCountOfDays, 'Number of days during which logs are stored');
    //C_SECTION_GLOBAL
    XmlFile.WriteInteger(C_SECTION_GLOBAL, C_KEY_RECORD_ID, FRecordID, 'Last RecordID');
    XmlFile.WriteInteger(C_SECTION_GLOBAL, C_KEY_MIN_MAX_GRADIENT_VALUE, FMinMaxGradientValue, 'Min/Max gradient value');
    XmlFile.WriteTime(C_SECTION_GLOBAL, C_KEY_PRICE_SCHEDULE_CHECK, FPriceScheduleCheck);
    XmlFile.WriteString(C_SECTION_GLOBAL, C_KEY_PRICE_SCHEDULE_ERRORS, FPriceScheduleCodeErrors);
    XmlFile.WriteString(C_SECTION_GLOBAL, C_KEY_PATH_CONTRACT_INSPECTOR, FPathToContractInspector);
    XmlFile.WriteBool(C_SECTION_GLOBAL, C_KEY_IS_SHOW_CANCEL_BOX, FIsShowCancelBox);
    XmlFile.WriteInteger(C_SECTION_GLOBAL, C_KEY_NODE_ID, FNodeID, 'Last NodeID');
    XMLFile.WriteString(C_SECTION_GLOBAL, C_KEY_GENERIC_TICK_REQUIRED, FGenericTickRequired.ToString);

    //C_SECTION_EMERGENCY_SETTINGS
    XmlFile.WriteInteger(C_SECTION_EMERGENCY_SETTINGS, C_KEY_EMERGENCY_CLOSE_ORDERTYPE, Ord(FEmergencySettings.OrderType));
    XmlFile.WriteFloat(C_SECTION_EMERGENCY_SETTINGS, C_KEY_EMERGENCY_PERCENT, FEmergencySettings.Percent);
    //C_SECTION_EXCHANGE
    XmlFile.WriteTime(C_SECTION_EXCHANGE, C_KEY_TIME_CLOSE_EXCHANGE, FExchange.TimeCloseExchange);
    XmlFile.WriteTime(C_SECTION_EXCHANGE, C_KEY_TIME_OPEN_EXCHANGE, FExchange.TimeOpenExchange);
    //C_SECTION_PRECAUTIONARY_SETTING
    XMLFile.EraseSection(C_SECTION_PRECAUTIONARY_SETTING);
    XMLFile.CurrentSection := C_SECTION_PRECAUTIONARY_SETTING;
    try
      for var st := Low(TIABSecurityType) to High(TIABSecurityType) do
        if FPrecautionarySettings.TryGetValue(st, PreArr) then
        begin
          XMLFile.Attributes.AddNode;
          XMLFile.Attributes.SetAttributeValue('SecurityTypeStr', SecurityTypeString[st]);
          XMLFile.Attributes.SetAttributeValue('SecurityType', Ord(st));
          for var PreType := Low(PreArr) to High(PreArr) do
            XMLFile.Attributes.SetAttributeValue(PreType.ToString, PreArr[PreType]);
          XMLFile.WriteAttributes;
        end;
    finally
      XMLFile.CurrentSection := '';
    end;
  finally
    XmlFile.Save;
  end;

  DBVersion := FModuleVersion;
  if Assigned(FConnection) then
    FParametersStore.Store;
end;

procedure TGeneral.LoadParamsFromXml;
var
  PreArr: TPrecautionarySettingArray;
  RequiredList: string;
begin
  //C_SECTION_AUTOORDERS
  FAllowEnterAutoorder := XmlFile.ReadBool(C_SECTION_AUTOORDERS, C_KEY_ALLOW_ENTER_AUTOORDER, False);
  FViewMarketScanner   := XmlFile.ReadBool(C_SECTION_AUTOORDERS, C_KEY_VIEW_MARKET_SCANNER, True);

  FAdjustmentCoefficients.Coef              := XmlFile.ReadInteger(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_COEF, 100);
  FAdjustmentCoefficients.CoefMax           := XmlFile.ReadInteger(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_COEF_MAX, 2);
  FAdjustmentCoefficients.Price             := XmlFile.ReadInteger(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_COEF_PRICE, 100);
  FAdjustmentCoefficients.Volatility        := XmlFile.ReadInteger(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_COEF_VOLATILITY, 60);
  FAdjustmentCoefficients.NumberOfHours     := XmlFile.ReadInteger(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_NUMBER_HOURS, 24);
  FAdjustmentCoefficients.UseVolatilityCoef := XmlFile.ReadBool(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_USE_VOLAT_COEF, False);
  FAdjustmentCoefficients.UseAdjustmentCoef := XmlFile.ReadBool(C_SECTION_AUTOORDERS, C_KEY_AUTOORDER_ADJ_USE_ADJ_COEF, False);

  FScanID        := XmlFile.ReadInteger(C_SECTION_AUTOORDERS, C_KEY_SCAN_ID, 0);
  FInstanceNum   := XmlFile.ReadInteger(C_SECTION_AUTOORDERS, C_KEY_AUTO_TRADES_NUM, 0);
  FWaitingTime   := XmlFile.ReadInteger(C_SECTION_AUTOORDERS, C_KEY_WAITING_TIME, 3000);

  //C_SECTION_BACKUP
  FIsActiveBackup      := XmlFile.ReadBool(C_SECTION_BACKUP, C_KEY_BACKUP_IS_ACTIVE, True);
  FPathToBackupFolder  := XmlFile.ReadString(C_SECTION_BACKUP, C_KEY_PATH_BACKUP_FOLDER, TPath.Combine(TDirectory.GetCurrentDirectory, 'Backup'));
  FBackupTime          := XmlFile.ReadTime(C_SECTION_BACKUP, C_KEY_BACKUP_TIME, StrToTime('15:00:00'));
  //C_SECTION_GLOBAL
  FIsShowCancelBox           := XmlFile.ReadBool(C_SECTION_GLOBAL, C_KEY_IS_SHOW_CANCEL_BOX, True);
  FMinMaxGradientValue       := XmlFile.ReadInteger(C_SECTION_GLOBAL, C_KEY_MIN_MAX_GRADIENT_VALUE, 10);
  FNodeID                    := XmlFile.ReadInteger(C_SECTION_GLOBAL, C_KEY_NODE_ID, 0);
  FPathToContractInspector   := XmlFile.ReadString(C_SECTION_GLOBAL, C_KEY_PATH_CONTRACT_INSPECTOR, TPath.Combine(TDirectory.GetCurrentDirectory, 'ContractInspector\ContractInspector.exe'));
  FPriceScheduleCheck        := XmlFile.ReadTime(C_SECTION_GLOBAL, C_KEY_PRICE_SCHEDULE_CHECK, StrToTime('17:40:00'));
  FPriceScheduleCodeErrors   := XmlFile.ReadString(C_SECTION_GLOBAL, C_KEY_PRICE_SCHEDULE_ERRORS, '');
  FRecordID                  := XmlFile.ReadInteger(C_SECTION_GLOBAL, C_KEY_RECORD_ID, 0);

  FGenericTickRequired := [];
  RequiredList := XMLFile.ReadString(C_SECTION_GLOBAL, C_KEY_GENERIC_TICK_REQUIRED, '');
  for var md := Low(TIABExMktData) to High(TIABExMktData) do
    if Pos(md.ToInteger.ToString, RequiredList) > 0 then
      FGenericTickRequired := FGenericTickRequired + [md];

  //C_SECTION_EMERGENCY_SETTINGS
  FEmergencySettings.OrderType := TIABOrderType(XmlFile.ReadInteger(C_SECTION_EMERGENCY_SETTINGS, C_KEY_EMERGENCY_CLOSE_ORDERTYPE, Ord(otMarket)));
  FEmergencySettings.Percent   := XmlFile.ReadFloat(C_SECTION_EMERGENCY_SETTINGS, C_KEY_EMERGENCY_PERCENT, 0);
  //C_CFG_SECTION_DEBUG
  FMaxSizeOfLogFile := XmlFile.ReadInt64(TLogWriter.C_CFG_SECTION_DEBUG, TLogWriter.C_CFG_KEY_MAX_SIZE, 0);
  FCountOfDays      := XmlFile.ReadInteger(TLogWriter.C_CFG_SECTION_DEBUG, TLogWriter.C_CFG_COUNT_OF_DAYS, 30);
  //C_SECTION_EXCHANGE
  FExchange.TimeOpenExchange  := XmlFile.ReadTime(C_SECTION_EXCHANGE, C_KEY_TIME_OPEN_EXCHANGE, StrToTime('7:00:00'));
  FExchange.TimeCloseExchange := XmlFile.ReadTime(C_SECTION_EXCHANGE, C_KEY_TIME_CLOSE_EXCHANGE, StrToTime('17:30:00'));
  FExchange.ShowQBALGO        := XmlFile.ReadBool(C_SECTION_EXCHANGE, C_KEY_SHOW_QBALGO, False);

  //C_SECTION_PRECAUTIONARY_SETTING
  XMLFile.CurrentSection := C_SECTION_PRECAUTIONARY_SETTING;
  try
    while not XMLFile.IsLastKey do
    begin
      if XMLFile.ReadAttributes then
      begin
        for var PreType := Low(PreArr) to High(PreArr) do
          PreArr[PreType] := XMLFile.Attributes.GetAttributeValue(PreType.ToString, 0);
        FPrecautionarySettings.AddOrSetValue(TIABSecurityType(XMLFile.Attributes.GetAttributeValue('SecurityType', TIABSecurityType.stStock)), PreArr);
      end;
      XMLFile.NextKey;
    end;
  finally
    XMLFile.CurrentSection := '';
  end;

  if Assigned(FConnection) then
    FParametersStore.Restore;
end;

procedure TGeneral.NoHibernate;
begin
  if Assigned(@SetThreadExecutionState) then
    SetThreadExecutionState(ES_SYSTEM_REQUIRED or ES_CONTINUOUS {or ES_DISPLAY_REQUIRED});
end;

class procedure TGeneral.ShowParametersDialog;
begin
  with TfrmParameters.Create(nil) do
  try
    Initialize;
    if (ShowModal = mrOk) then
      Deinitialize;
  finally
    Free;
  end;
end;

class procedure TGeneral.ShowParametersTab(const aNumTab: Integer);
begin
  with TfrmParameters.Create(nil) do
  try
    Initialize;
    SetTabVisible(aNumTab);
    if (ShowModal = mrOk) then
      Deinitialize;
  finally
    Free;
  end;
end;

function TGeneral.GetNextID: Integer;
begin
  Inc(FID);
  Result := FID;
end;

function TGeneral.GetNextInstanceNum: Integer;
begin
  Inc(FInstanceNum);
  Result := FInstanceNum;
  XmlFile.WriteInteger(C_SECTION_AUTOORDERS, C_KEY_AUTO_TRADES_NUM, FInstanceNum, 'Last Instance Num');
end;

function TGeneral.GetNextScanID: Integer;
begin
  Inc(FScanID);
  Result := FScanID;
  XmlFile.WriteInteger(C_SECTION_AUTOORDERS, C_KEY_SCAN_ID, FScanID, 'Last ScanID');
end;

function TGeneral.GetNextRecordID: Integer;
begin
  Inc(FRecordID);
  Result := FRecordID;
  XmlFile.WriteInteger(C_SECTION_GLOBAL, C_KEY_RECORD_ID, FRecordID, 'Last RecordID');
end;

function TGeneral.GetNextNodeID: Integer;
begin
  Inc(FNodeID);
  Result := FNodeID;
  XmlFile.WriteInteger(C_SECTION_GLOBAL, C_KEY_NODE_ID, FNodeID, 'Last NodeID');
end;

{ TfrmParameters }

procedure TfrmParameters.FormCreate(Sender: TObject);
resourcestring
  rsPercentageHint = 'This option was created as a safety net to prevent you from transmitting a limit order' + sLineBreak +
                     'that has a mistyped limit price. If you attempt to transmit a limit order with a price outside' + sLineBreak +
                     'of this calculated percent off the market price, you will receive a message asking you to verify' + sLineBreak +
                     'that you meant to enter that off-percentage price before it will transmit the order.' + sLineBreak +
                     'The market price used is the price displayed in either the bid (sell) or ask (buy) field' + sLineBreak +
                     'at the time you transmit the order.' + sLineBreak +
                     'This option can be left blank.';
  rsTotalValueLimitHint =
                     'If you transmit an order with a price that exceeds the total dollar amount you specify,' + sLineBreak +
                     'you will receive a message informing you that the Total Value constraint has been violated,' + sLineBreak +
                     'and you must confirm that you want to submit the order regardless.';
  rsOrderQuantityMaxHint =
                     'Limits the value in the Quantity field.' + sLineBreak +
                     'If you enter a higher value, you can override the default size limit by clicking Yes in the warning box.';
  rsNumberOfTicksHint =
                     'Like the Percentage option above, number of ticks is also a safety net to prevent you from transmitting a limit order' + sLineBreak +
                     'that has a mistyped limit price. If you attempt to transmit a limit order with a price outside' + sLineBreak +
                     'of this calculated number of ticks off the market price, you will get a message asking you to verify' + sLineBreak +
                     'that you meant to enter the off-percentage price before it will transmit the order.' + sLineBreak +
                     'The market price used is the price displayed in either the bid (sell) or ask (buy) field at the time' + sLineBreak +
                     'you transmit the order. This option can be left blank.';
  rsAlgorithmTotalValueLimitHint =
                     'If you transmit an algo order with a price that exceeds the total dollar amount you specify,' + sLineBreak +
                     'you will receive a message informing you that the Total Value constraint has been violated,' + sLineBreak +
                     'and you must confirm that you want to submit the order regardless.' + sLineBreak +
                     'Enter "0" to disable the limit.' + sLineBreak +
                     'If you do not enter a value, the algo value limit defaults to the Total Value Limit (above) x 10.';
  rsAlgorithmSizeLimitHint =
                     'Limits the value in the Quantity field for algo orders. If you enter a higher value,' + sLineBreak +
                     'you can override the default size limit by clicking Yes in the warning box.' + sLineBreak +
                     'Enter "0" to disable the limit.' + sLineBreak +
                     'If you do not enter a value, the algo size limit defaults to the Size Limit (above) x 10.';
  rsMaxAllowedPriceHint  = 'Order price for one unit max';
  rsMinAllowedPriceHint  = 'Order price for one unit min';
begin
  edAlgorithmSizeLimit.Hint       := rsAlgorithmSizeLimitHint;
  edAlgorithmTotalValueLimit.Hint := rsAlgorithmTotalValueLimitHint;
  edMaxAllowedPrice.Hint          := rsMaxAllowedPriceHint;
  edMinAllowedPrice.Hint          := rsMinAllowedPriceHint;
  edNumberOfTicks.Hint            := rsNumberOfTicksHint;
  edOrderQuantityMax.Hint         := rsOrderQuantityMaxHint;
  edPercentage.Hint               := rsPercentageHint;
  edTotalValueLimit.Hint          := rsTotalValueLimitHint;
end;

procedure TfrmParameters.Initialize;
resourcestring
  rsItem = '[%s] %s';
  rsTickRequired = '[%d] %s';
var
  arrCodes: TArray<string>;
  Index: Integer;
begin
  cbAllowEnterAutoorder.Checked  := General.AllowEnterAutoorder;
  cbBackupIsActive.Checked       := General.IsActiveBackup;
  cbIsShowCancelBox.Checked      := General.IsShowCancelBox;
  cbShowQbalgo.Checked           := General.Exchange.ShowQBALGO;
  cbViewMarketScanner.Checked    := General.ViewMarketScanner;
  edBackupScheduleTime.Time      := General.BackupTime;
  edCountOfDays.Value            := General.CountOfDays;
  edMaxSizeOfLogFile.Value       := General.MaxSizeOfLogFile;
  edMinMaxGradientValue.ValueInt := General.MinMaxGradientValue;
  edPathToBackup.Text            := General.PathToBackupFolder;
  edPathToContractInspector.Text := General.PathToContractInspector;
  edPriceScheduleCheck.Time      := General.PriceScheduleCheck;
  edTimeCloseExchange.Time       := General.Exchange.TimeCloseExchange;
  edTimeOpenExchange.Time        := General.Exchange.TimeOpenExchange;
  seWaitingTime.Value            := General.WaitingTime;

  lbCodeErrors.Clear;
  arrCodes := General.C_ERROR_CODES.Split([';']);
  for var str in arrCodes do
    if not str.IsEmpty then
    begin
      Index := lbCodeErrors.Items.AddObject(Format(rsItem, [str, GetTWSMessageItem(str.ToInteger).ErrorMsg]),
                                            TStringObject.Create(str.ToInteger, str));
      lbCodeErrors.Checked[Index] := Pos(str, General.PriceScheduleCodeErrors) > 0;
    end;

  lbGenericTickRequired.Clear;
  for var md := Low(TIABExMktData) to High(TIABExMktData) do
  begin
    Index := lbGenericTickRequired.Items.AddObject(Format(rsTickRequired, [md.ToInteger, md.ToString]),
                                                   TStringObject.Create(Ord(md), md.ToString));
    lbGenericTickRequired.Checked[Index] := md in General.GenericTickRequired;
  end;
  lbGenericTickRequired.Sorted := True;

  cbUseAdjustment.Checked             := General.AdjustmentCoefficients.UseAdjustmentCoef;
  cbUseVolatility.Checked             := General.AdjustmentCoefficients.UseVolatilityCoef;
  seAdjustmentCoef.Value              := General.AdjustmentCoefficients.Coef;
  seAdjustmentCoefMax.Value           := General.AdjustmentCoefficients.CoefMax;
  seAdjustmentCoefNumberOfHours.Value := General.AdjustmentCoefficients.NumberOfHours;
  seAdjustmentCoefPrice.Value         := General.AdjustmentCoefficients.Price;
  seAdjustmentCoefVolatility.Value    := General.AdjustmentCoefficients.Volatility;
  pnlVolatility.Enabled               := cbUseVolatility.Checked;
  pnlAdjustment.Enabled               := cbUseAdjustment.Checked;

  rbLMT.Checked     := General.EmergencySettings.OrderType = otLimit;
  rbMKT.Checked     := General.EmergencySettings.OrderType = otMarket;
  edPercent.Text    := General.EmergencySettings.Percent.ToString;
  edPercent.Enabled := rbLMT.Checked;

  edAlgorithmSizeLimit.Tag       := Ord(psAlgorithmSizeLimit);
  edAlgorithmTotalValueLimit.Tag := Ord(psAlgorithmTotalValueLimit);
  edMaxAllowedPrice.Tag          := Ord(psMaxAllowedPrice);
  edMinAllowedPrice.Tag          := Ord(psMinAllowedPrice);
  edNumberOfTicks.Tag            := Ord(psNumberOfTicks);
  edOrderQuantityMax.Tag         := Ord(psOrderQuantityMax);
  edPercentage.Tag               := Ord(psPercentage);
  edTotalValueLimit.Tag          := Ord(psTotalValueLimit);

  lbSecurityType.Clear;
  for var st := Low(TIABSecurityType) to High(TIABSecurityType) do
    lbSecurityType.Items.Add(SecurityTypeString[st]);
  lbSecurityType.ItemIndex := Ord(stAll);
  lbSecurityTypeClick(nil);
end;

procedure TfrmParameters.Deinitialize;
var
  Coefficients: TAdjustmentCoefficients;
  EmergencySettings: TEmergencySettings;
  Exchange: TExchange;
begin
  Coefficients.Coef                  := seAdjustmentCoef.Value;
  Coefficients.CoefMax               := seAdjustmentCoefMax.Value;
  Coefficients.NumberOfHours         := seAdjustmentCoefNumberOfHours.Value;
  Coefficients.Price                 := seAdjustmentCoefPrice.Value;
  Coefficients.UseAdjustmentCoef     := cbUseAdjustment.Checked;
  Coefficients.UseVolatilityCoef     := cbUseVolatility.Checked;
  Coefficients.Volatility            := seAdjustmentCoefVolatility.Value;
  General.AdjustmentCoefficients     := Coefficients;
  General.PriceScheduleCodeErrors    := '';
  General.GenericTickRequired        := [];  

  for var i := 0 to lbCodeErrors.Items.Count - 1 do
    if lbCodeErrors.Checked[i] then
      General.PriceScheduleCodeErrors := General.PriceScheduleCodeErrors + TStringObject(lbCodeErrors.Items.Objects[i]).Id.ToString + ';';

  for var i := 0 to lbGenericTickRequired.Items.Count - 1 do
    if lbGenericTickRequired.Checked[i] then
      General.GenericTickRequired := General.GenericTickRequired + [TIABExMktData(TStringObject(lbGenericTickRequired.Items.Objects[i]).Id)];

  Exchange.ShowQBALGO        := cbShowQbalgo.Checked;
  Exchange.TimeCloseExchange := edTimeCloseExchange.Time;
  Exchange.TimeOpenExchange  := edTimeOpenExchange.Time;
  General.Exchange := Exchange;

  General.AllowEnterAutoorder     := cbAllowEnterAutoorder.Checked;
  General.BackupTime              := edBackupScheduleTime.Time;
  General.CountOfDays             := edCountOfDays.Value;
  General.IsActiveBackup          := cbBackupIsActive.Checked;
  General.IsShowCancelBox         := cbIsShowCancelBox.Checked;
  General.MaxSizeOfLogFile        := edMaxSizeOfLogFile.Value;
  General.MinMaxGradientValue     := edMinMaxGradientValue.ValueInt;
  General.PathToBackupFolder      := edPathToBackup.Text;
  General.PathToContractInspector := edPathToContractInspector.Text;
  General.PriceScheduleCheck      := edPriceScheduleCheck.Time;
  General.ViewMarketScanner       := cbViewMarketScanner.Checked;
  General.WaitingTime             := seWaitingTime.Value;

  if rbLMT.Checked then
    EmergencySettings.OrderType := otLimit
  else
    EmergencySettings.OrderType := otMarket;
  EmergencySettings.Percent := StrToFloatDef(edPercent.Text, 0);
  General.EmergencySettings := EmergencySettings;
  General.SaveParamsToXml;
end;

procedure TfrmParameters.rbLMTClick(Sender: TObject);
begin
  edPercent.Enabled := rbLMT.Checked;
end;

procedure TfrmParameters.SetTabVisible(const aNumTab: Integer);
begin
  for var i := 0 to pcMain.PageCount - 1 do
    if (aNumTab <> i) then
      pcMain.Pages[i].TabVisible := False;
end;

procedure TfrmParameters.OnPrecautionaryChangeValue(Sender: TObject);
var
  PreArr: TPrecautionarySettingArray;
begin
  if Showing then
    if (Sender is TNumberBox) and (lbSecurityType.ItemIndex > -1) then
    begin
      General.PrecautionarySettings.TryGetValue(TIABSecurityType(lbSecurityType.ItemIndex), PreArr);
      PreArr[TPrecautionarySettingType(TNumberBox(Sender).Tag)] := TNumberBox(Sender).Value;
      General.PrecautionarySettings.AddOrSetValue(TIABSecurityType(lbSecurityType.ItemIndex), PreArr);
    end;
end;

procedure TfrmParameters.aCopyToAllExecute(Sender: TObject);
resourcestring
  rsQuestion = 'The current Precautionary Settings will be copied to all security types.' + sLineBreak + 'Continue?';
var
  PreArr: TPrecautionarySettingArray;
begin
  if (TMessageDialog.ShowQuestion(rsQuestion) = mrYes) then
    for var st := Low(TIABSecurityType) to High(TIABSecurityType) do
    begin
      General.PrecautionarySettings.TryGetValue(st, PreArr);
      PreArr[psAlgorithmTotalValueLimit] := edAlgorithmTotalValueLimit.ValueFloat;
      PreArr[psNumberOfTicks]            := edNumberOfTicks.ValueFloat;
      PreArr[psPercentage]               := edPercentage.ValueFloat;
      PreArr[psTotalValueLimit]          := edTotalValueLimit.ValueFloat;
      PreArr[psAlgorithmSizeLimit]       := edAlgorithmSizeLimit.ValueFloat;
      PreArr[psOrderQuantityMax]         := edOrderQuantityMax.ValueInt;
      PreArr[psMaxAllowedPrice]          := edMaxAllowedPrice.ValueFloat;
      PreArr[psMinAllowedPrice]          := edMinAllowedPrice.ValueFloat;
      General.PrecautionarySettings.AddOrSetValue(st, PreArr);
    end;
end;

procedure TfrmParameters.aGetPathToBackupExecute(Sender: TObject);
begin
  with OpenDialog do
  begin
    FileName := '';
    FileTypes.Clear;
    Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem];
    DefaultFolder := TPath.Combine(TDirectory.GetCurrentDirectory, 'Backup');
    if Execute then
      edPathToBackup.Text := FileName;
  end;
end;

procedure TfrmParameters.aGetPathToContractInspectorExecute(Sender: TObject);
var
  FileTypeItem: TFileTypeItem;
begin
  with OpenDialog do
  begin
    FileName := 'ContractInspector.exe';
    FileTypes.Clear;
    FileTypeItem := FileTypes.Add;
    FileTypeItem.DisplayName := 'ContractInspector';
    FileTypeItem.FileMask    := 'ContractInspector.exe';
    FileTypeItem := FileTypes.Add;
    FileTypeItem.DisplayName := 'All Files';
    FileTypeItem.FileMask    := '*.*';
    Options := [fdoPathMustExist, fdoFileMustExist];
    DefaultFolder := TPath.Combine(TDirectory.GetCurrentDirectory, 'ContractInspector');
    if Execute then
      edPathToContractInspector.Text := FileName;
  end;
end;

procedure TfrmParameters.cbUseAdjustmentClick(Sender: TObject);
begin
  pnlAdjustment.Enabled := cbUseAdjustment.Checked;
end;

procedure TfrmParameters.cbUseVolatilityClick(Sender: TObject);
begin
  pnlVolatility.Enabled := cbUseVolatility.Checked;
end;

procedure TfrmParameters.lbSecurityTypeClick(Sender: TObject);
var
  PreArr: TPrecautionarySettingArray;
begin
  if (lbSecurityType.ItemIndex > -1) then
  begin
    General.PrecautionarySettings.TryGetValue(TIABSecurityType(lbSecurityType.ItemIndex), PreArr);
    edAlgorithmTotalValueLimit.ValueFloat := PreArr[psAlgorithmTotalValueLimit];
    edNumberOfTicks.ValueFloat            := PreArr[psNumberOfTicks];
    edPercentage.ValueFloat               := PreArr[psPercentage];
    edTotalValueLimit.ValueFloat          := PreArr[psTotalValueLimit];
    edAlgorithmSizeLimit.ValueFloat       := PreArr[psAlgorithmSizeLimit];
    edOrderQuantityMax.ValueInt           := Trunc(PreArr[psOrderQuantityMax]);
    edMaxAllowedPrice.ValueFloat          := PreArr[psMaxAllowedPrice];
    edMinAllowedPrice.ValueFloat          := PreArr[psMinAllowedPrice];
  end;
end;

{ TBaseClass }

constructor TBaseClass.Create;
begin
end;

class procedure TBaseClass.DeleteFromDB(aID: Integer);
begin
end;

procedure TBaseClass.FromDB(aID: Integer);
begin
end;

class function TBaseClass.GetListCaption: string;
begin
  Result := '';
end;

class function TBaseClass.GetListSQL: string;
begin
  Result := '';
end;

procedure TBaseClass.SaveToDB;
begin
end;

initialization
  if not Assigned(General) then
    General := TGeneral.Create(Application);

end.
