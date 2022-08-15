unit Monitor.Interfaces;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  Winapi.Messages, IABSocketAPI, IABSocketAPI_const, Scanner.Types, VirtualTrees, Vcl.Controls, Common.Types,
  AutoTrades.Types;
{$ENDREGION}

type
  IMonitor = interface
    ['{C8DFAFE6-925A-4632-B939-7ED519F7487E}']
    function ExistsChildConditions(aNode: PVirtualNode): Boolean;
    function GetDockControl: TWinControl;
    function GetDuplicateAlgos(const aNode: PVirtualNode): PVirtualNode;
    function GetDuplicateCondition(const aNode: PVirtualNode): PVirtualNode;
    function GetDuplicateOrderIB(const aNode: PVirtualNode): PVirtualNode;
    function GetDuplicateOrderNN(const aNode: PVirtualNode): PVirtualNode;
    function GetFocusedNode: PVirtualNode;
    function GetIABClient: TIABSocket;
    function GetMainTree: TVirtualStringTree;
    function GetParentNode(aNode: PVirtualNode; aDocType: TDocType): PVirtualNode;
    function CreateTemplateStructure(const aOrderGroupId: Integer; aInstrumentData: PInstrumentData; const aAutoTradesCommon: TAutoTradesCommon): PVirtualNode;

    function IsCreateOrModify(const aNode, aOrderGroupNode: PVirtualNode): Boolean;
    function UseIBFeeds: Boolean;
    procedure AddInstrumentFromSearch(aTargetNode: PVirtualNode; aSourceTree: TVirtualStringTree);
    procedure DeleteNode(aNode: PVirtualNode; aUpdateAlgos: Boolean = True);
    procedure OrdersExecuteOcaGroup(const aOcaGroupNumber: Integer);
    procedure OrdersExecuteOneCancelAll(const aNode: PVirtualNode);
    procedure SellContractPosition(const aSymbol, aCurrency: string; aQuantity, aInstrumentId: Integer; aSecType: TIABSecurityType; aAction: TIABAction);
    procedure SetChildsEnabled(const aNode: PVirtualNode; const aEnabled: Boolean);
    procedure SetChildsFreeze(const aNode: PVirtualNode);
    procedure ShowAlgosChart(aNode: PVirtualNode);
    procedure ShowConditionAlgosChart(aNode: PVirtualNode);
    procedure ShowConditionChart(aNode: PVirtualNode);
    procedure ShowTradeChart(aNode: PVirtualNode);
    procedure SubscribeChildNNFeed(aNode: PVirtualNode);
    procedure UnsubscribeChildNNFeed(aNode: PVirtualNode);
    procedure UpdateAlgos(const aNode: PVirtualNode);
    procedure UpdateCondition(const aNode: PVirtualNode);
  end;

implementation

end.
