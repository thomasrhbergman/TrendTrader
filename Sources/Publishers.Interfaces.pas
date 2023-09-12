unit Publishers.Interfaces;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  Winapi.Messages, VirtualTrees, IABSocketAPI, IABSocketAPI_const, NNfunctions.Types, Common.Types;
{$ENDREGION}

type
  ICustomInterface = interface
    function GetInstance: TObject;
  end;

  IOrderStatus = interface(ICustomInterface)
    ['{9D724490-CE13-4EC3-A66E-1682FD3A2823}']
    procedure OnStatusUpdate(const aStatus: TIABOrderState; const aOrder: TIABOrder; aNodeOrder: PVirtualNode; const aInfo: string);
  end;

  ITickOptionComputation = interface(ICustomInterface)
    ['{9E10A4D2-A571-43F4-AE62-8458E7EABC25}']
    procedure OnTickOptionComputation(Sender: TObject; DataId: Integer; TickType: TIABTickType; ImpliedVol, Delta, OptPrice, pvDividend, Gamma, Vega, Theta, undPrice: Double);
  end;

  IScanner = interface(ICustomInterface)
    ['{3C40FA01-3A91-42FF-BB76-0F70E3CF8330}']
    procedure OnScannerData(Sender: TObject; Scan: TIABScan);
    procedure OnScannerParam(Sender: TObject; Parameters: string);
    procedure OnScannerAdd(Sender: TObject; ScanId: Integer);
    procedure OnScannerCancel(Sender: TObject; ScanId: Integer);
  end;

  IInstrumentSpecDetails = interface(ICustomInterface)
    ['{CA980336-D8D8-4CD2-B2A2-7965E6139B1D}']
    procedure OnInstrumentSpecDetails(Sender: TObject; Index: Integer);
  end;

  IUpdateFeeds = interface(ICustomInterface)
    ['{4149BA1B-F7CC-4341-B5C5-5939C4760E96}']
    procedure OnPriceChange(Sender: TObject; Id: Integer; TickType: TIABTickType; Value: Currency; TimeStamp: TDateTime);
  end;

  ISecurityDefinitionOptionalParameter = interface(ICustomInterface)
    ['{C111723B-1098-407C-83D2-16EF7C722C3E}']
    procedure OnSecurityDefinitionOptionalParameter(Sender: TObject; DataID: Integer; Exchange: string; UnderlyingConId: Integer; TradingClass, Multiplier, Expirations, Strikes: string);
  end;

  IOnTickByTick = interface(ICustomInterface)
    ['{17D65708-0C08-4707-B9B5-8981B902BCD6}']
    procedure OnTickByTick(Sender: TObject; DataID: Integer; TickData: TIABTickData);
    procedure OnHistoricalTickData(Sender: TObject; DataId: Integer; TickData: TIABTickData);
  end;

  IConnectionState = interface(ICustomInterface)
    ['{D523F537-5140-484D-99E4-0C8B3652CAAE}']
    procedure OnConnectionState(Sender: TObject; State: TIABConnection);
  end;

  IOrderState = interface(ICustomInterface)
    ['{9B6D4326-8046-4BAD-ACB9-EBCF2AF9C29A}']
    procedure OnCloseOrder(const aTempId: Integer);
    procedure OnExecution(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrder(Sender: TObject; Order: TIABOrder);
    procedure OnOpenOrderNN(const aOrderList: array of TOrder);
    procedure OnOrderStatus(Sender: TObject; Order: TIABOrder; Status: TIABOrderState);
    procedure OnRebuildFromTWS(Sender: TObject);
  end;

  ILogger = interface(ICustomInterface)
    ['{F4E082DE-2CF1-4C8A-ADB2-41F90294EB15}']
    function GetLogListenerType: TLogListenerType;
    procedure Write(const aDetailType: TLogDetailType; const aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aMethod, aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aMethod, aUnit, aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aObject: TObject; const aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aObject: TObject; const aMethod, aInfo: string); overload;
    procedure Write(const aDetailType: TLogDetailType; const aOrderID, aQuantity, aFilled: Integer; const aMethod, aSymbol, aInfo: string; const aStatus: TIABOrderState; const aAction: TIABAction; const aPrice: Double); overload;
  end;

  IError = interface(ICustomInterface)
    ['{98EEAB80-DB02-4F8F-9B9F-99AFDF365761}']
    procedure OnError(Sender: TObject; TempId, ErrorCode: Integer; ErrorMsg: string);
  end;

  IHistoricalData = interface(ICustomInterface)
    ['{7B4CFBFA-18A8-4CAE-8705-4F26D1DC8F9C}']
    procedure OnHistoricalData(Sender: TObject; DataId, Item, Count: Integer; HistoricalChartDataElement: TIABHistoricalChartData);
  end;

  IMonitorStructureChange = interface(ICustomInterface)
    ['{A234B6B8-2694-4DD3-BA61-1BDE739779EE}']
    procedure OnMonitorStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
  end;

  IMotherFilledPriceChange = interface(ICustomInterface)
   ['{D64EB6B2-FAF4-444D-A277-F7C2EC7DDD82}']
    procedure OnMotherFilledPriceChange(const MotherNode: PVirtualNode);
  end;

  IGradientChange = interface(ICustomInterface)
   ['{138153D1-B286-486F-B5EA-6FE3D6590122}']
    procedure OnGradientChange(const aId, aDuration: integer; const aGradientRec: TGradientRecord);
  end;


implementation

end.
