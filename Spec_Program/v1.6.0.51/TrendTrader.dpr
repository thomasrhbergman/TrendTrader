program TrendTrader;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$IFDEF DEBUG}
{$DEFINE USE_LOGGING}
// {$DEFINE USE_CODE_SITE}
{$ENDIF}
{$R 'Logo.res' 'Resources\Logo.rc'}

uses
  Vcl.Forms,
  System.SysUtils,
  Utils.Exceptions in 'Sources\Utils.Exceptions.pas',
  Jcl8087 in 'Sources\JCL\Jcl8087.pas',
  JclAnsiStrings in 'Sources\JCL\JclAnsiStrings.pas',
  JclBase in 'Sources\JCL\JclBase.pas',
  JclCharsets in 'Sources\JCL\JclCharsets.pas',
  JclConsole in 'Sources\JCL\JclConsole.pas',
  JclDateTime in 'Sources\JCL\JclDateTime.pas',
  JclDebug in 'Sources\JCL\JclDebug.pas',
  JclFileUtils in 'Sources\JCL\JclFileUtils.pas',
  JclHookExcept in 'Sources\JCL\JclHookExcept.pas',
  JclIniFiles in 'Sources\JCL\JclIniFiles.pas',
  JclLogic in 'Sources\JCL\JclLogic.pas',
  JclMath in 'Sources\JCL\JclMath.pas',
  JclPeImage in 'Sources\JCL\JclPeImage.pas',
  JclRegistry in 'Sources\JCL\JclRegistry.pas',
  JclResources in 'Sources\JCL\JclResources.pas',
  JclSecurity in 'Sources\JCL\JclSecurity.pas',
  JclShell in 'Sources\JCL\JclShell.pas',
  JclStreams in 'Sources\JCL\JclStreams.pas',
  JclStringConversions in 'Sources\JCL\JclStringConversions.pas',
  JclStrings in 'Sources\JCL\JclStrings.pas',
  JclSynch in 'Sources\JCL\JclSynch.pas',
  JclSysInfo in 'Sources\JCL\JclSysInfo.pas',
  JclSysUtils in 'Sources\JCL\JclSysUtils.pas',
  JclTD32 in 'Sources\JCL\JclTD32.pas',
  JclUnicode in 'Sources\JCL\JclUnicode.pas',
  JclWideStrings in 'Sources\JCL\JclWideStrings.pas',
  JclWin32 in 'Sources\JCL\JclWin32.pas',
  Snmp in 'Sources\JCL\Snmp.pas',
  OpenSSL.Core in 'Sources\OpenSSL\OpenSSL.Core.pas',
  OpenSSL.libeay32 in 'Sources\OpenSSL\OpenSSL.libeay32.pas',
  OpenSSL.RSAUtils in 'Sources\OpenSSL\OpenSSL.RSAUtils.pas',
  Account.Types in 'Sources\Account.Types.pas',
  ArrayHelper in 'Sources\ArrayHelper.pas',
  AutoTrades in 'Sources\AutoTrades\AutoTrades.pas' {frmAutoTrades} ,
  AutoTrades.ComputeOrders in 'Sources\AutoTrades\AutoTrades.ComputeOrders.pas' {frmDockFormComputeOrders} ,
  AutoTrades.Dock in 'Sources\AutoTrades\AutoTrades.Dock.pas' {frmAutoTradesDock} ,
  AutoTrades.Edit in 'Sources\AutoTrades\AutoTrades.Edit.pas' {frmAutoTradesEdit} ,
  AutoTrades.Frame in 'Sources\AutoTrades\AutoTrades.Frame.pas' {frameAutoTrades: TFrame} ,
  AutoTrades.Types in 'Sources\AutoTrades\AutoTrades.Types.pas',
  Bleeper in 'Sources\Bleeper.pas',
  BrokerHelperAbstr in 'Sources\Brokers\BrokerHelperAbstr.pas',
  BrokerHelperFactory in 'Sources\Brokers\BrokerHelperFactory.pas',
  BrokerHelperIB in 'Sources\Brokers\BrokerHelperIB.pas',
  BrokerHelperNN in 'Sources\Brokers\BrokerHelperNN.pas',
  BrokerHelperTB in 'Sources\Brokers\BrokerHelperTB.pas',
  Chart.Condition in 'Sources\Charts\Chart.Condition.pas' {frmConditionChart} ,
  Chart.ConditionAlgos in 'Sources\Charts\Chart.ConditionAlgos.pas' {frmConditionAlgosChart} ,
  Chart.ConditionRealtime in 'Sources\Charts\Chart.ConditionRealtime.pas' {frmConditionRealtimeChart} ,
  Chart.Trade in 'Sources\Charts\Chart.Trade.pas' {frmTradeChartForm} ,
  CheckIBInstruments in 'Sources\CheckIBInstruments.pas' {frmCheckIBInstruments} ,
  Column.CustomSelections in 'Sources\ColumnSettings\Column.CustomSelections.pas' {frmCustomColumnSelections} ,
  Column.OrderSelections in 'Sources\ColumnSettings\Column.OrderSelections.pas' {frmColumnSelections} ,
  Column.Settings in 'Sources\ColumnSettings\Column.Settings.pas' {frmColumnSettings} ,
  Column.Types in 'Sources\ColumnSettings\Column.Types.pas',
  Common.Types in 'Sources\Common.Types.pas',
  CustomDockForm in 'Sources\CustomDockForm.pas' {frmCustomDockForm} ,
  CustomForms in 'Sources\CustomForms.pas' {TvmsCustomForm} ,
  CustomJournalForm in 'Sources\CustomJournalForm.pas' {frmCustomJournalForm} ,
  DaImages in 'Sources\DataModules\DaImages.pas' {DMImage: TDataModule} ,
  DaModule in 'Sources\DataModules\DaModule.pas' {DMod} ,
  DaModule.Constants in 'Sources\DataModules\DaModule.Constants.pas',
  DaModule.ExecuteScript in 'Sources\DataModules\DaModule.ExecuteScript.pas' {frmExecuteScript} ,
  DaModule.Resources in 'Sources\DataModules\DaModule.Resources.pas',
  DaModule.Utils in 'Sources\DataModules\DaModule.Utils.pas',
  DatabaseProperties in 'Sources\DatabaseProperties.pas' {frmDatabaseProperties} ,
  DebugWriter in 'Sources\DebugWriter.pas',
  DefinitionOptionalParameter in 'Sources\DefinitionOptionalParameter.pas' {frmDefOptionalParameter} ,
  DockForm.AccountInfo in 'Sources\DockForm.AccountInfo.pas' {frmDockFormAccountInfo} ,
  DockForm.AccountPnL in 'Sources\DockForm.AccountPnL.pas' {frmDockFormAccountPnL} ,
  DockForm.ActiveOrders in 'Sources\DockForm.ActiveOrders.pas' {frmDockFormActiveOrders} ,
  DockForm.AutoTradesController in 'Sources\DockForm.AutoTradesController.pas' {frmDockFormAutoTradesController} ,
  DockForm.LogView in 'Sources\DockForm.LogView.pas' {frmLogView} ,
  DockForm.MonitorFilter in 'Sources\DockForm.MonitorFilter.pas' {frmMonitorFilter} ,
  DockForm.OrderStatus in 'Sources\DockForm.OrderStatus.pas' {frmDockFormOrderStatus} ,
  DockForm.Position in 'Sources\DockForm.Position.pas' {frmDockFormPosition} ,
  DockForm.QualifiersController in 'Sources\DockForm.QualifiersController.pas' {frmDockFormQualifiersController} ,
  DockForm.TemplateCreator in 'Sources\DockForm.TemplateCreator.pas' {frmDockFormTemplateCreator} ,
  DockForm.TotalController in 'Sources\DockForm.TotalController.pas' {frmDockFormTotalController} ,
  Document in 'Sources\Document.pas' {FormDocument} ,
  Edit.Algos in 'Sources\Edit.Algos.pas' {frmEditAlgos} ,
  Edit.Condition in 'Sources\Edit.Condition.pas' {frmEditCondition} ,
  Edit.Factor in 'Sources\Edit.Factor.pas' {frmEditFactor} ,
  Edit.Instrument in 'Sources\Edit.Instrument.pas' {frmEditInstrument} ,
  Edit.OrderBaseTransform in 'Sources\Edit.OrderBaseTransform.pas' {frmOrderBaseTransform} ,
  Edit.OrderGroup in 'Sources\Edit.OrderGroup.pas' {frmEditOrderGroup} ,
  Edit.OrderGroupSet in 'Sources\Edit.OrderGroupSet.pas' {frmEditOrderGroupSet} ,
  Edit.OrderIB in 'Sources\Edit.OrderIB.pas' {frmEditOrderIB} ,
  Edit.OrderNN in 'Sources\Edit.OrderNN.pas' {frmEditOrderNN} ,
  Edit.OrderStatus in 'Sources\Edit.OrderStatus.pas' {frmOrderStatus} ,
  Edit.OrderTemplate in 'Sources\Edit.OrderTemplate.pas' {frmOrderBaseCustom} ,
  Edit.SokidList in 'Sources\Edit.SokidList.pas' {frmEditSokidList} ,
  Entity.OrderStatus in 'Sources\Entities\Entity.OrderStatus.pas',
  Entity.Price in 'Sources\Entities\Entity.Price.pas',
  Entity.Sokid in 'Sources\Entities\Entity.Sokid.pas',
  Frame.ActiveOrders in 'Sources\Frame.ActiveOrders.pas' {frameActiveOrders: TFrame} ,
  Frame.ActivityLog in 'Sources\Frame.ActivityLog.pas' {frameActivityLog: TFrame} ,
  Frame.ConditionHistory in 'Sources\Frame.ConditionHistory.pas' {frameConditionHistory: TFrame} ,
  Frame.Custom in 'Sources\Frame.Custom.pas' {frameCustom: TFrame} ,
  Frame.DocumentsTree in 'Sources\Frame.DocumentsTree.pas' {frameDocumentsTree: TFrame} ,
  Frame.Future in 'Sources\Frame.Future.pas' {frameFuture: TFrame} ,
  Frame.HistoricalData in 'Sources\Frame.HistoricalData.pas' {frameHistoricalData: TFrame} ,
  Frame.Option in 'Sources\Frame.Option.pas' {frameOption: TFrame} ,
  Frame.OrderStatus in 'Sources\Frame.OrderStatus.pas' {frameOrderStatus: TFrame} ,
  Frame.RealtimeFeeds in 'Sources\Frame.RealtimeFeeds.pas' {frameRealtimeFeeds: TFrame} ,
  Generics.Helper in 'Sources\Generics.Helper.pas',
  Global.Resources in 'Sources\Global.Resources.pas',
  Global.Types in 'Sources\Global.Types.pas' {frmParameters} ,
  HtmlConsts in 'Sources\HtmlConsts.pas',
  HtmlLib in 'Sources\HtmlLib.pas',
  IABFunctions in 'Sources\IABSocketAPI\IABFunctions.pas',
  IABFunctions.Helpers in 'Sources\IABSocketAPI\IABFunctions.Helpers.pas',
  IABFunctions.MarketData in 'Sources\IABSocketAPI\IABFunctions.MarketData.pas',
  IABFunctions.MarketRules in 'Sources\IABSocketAPI\IABFunctions.MarketRules.pas',
  IABFunctions.MessageCodes in 'Sources\IABSocketAPI\IABFunctions.MessageCodes.pas',
  IABFunctions.RequestsQueue in 'Sources\IABSocketAPI\IABFunctions.RequestsQueue.pas',
  IABSocketAPI in 'Sources\IABSocketAPI\API_all_platform\IABSocketAPI.pas',
  IABSocketAPI_const in 'Sources\IABSocketAPI\API_all_platform\IABSocketAPI_const.pas',
  IBInstrumentParser in 'Sources\IBInstrumentParser.pas',
  InformationDialog in 'Sources\InformationDialog.pas' {TInformation} ,
  InstrumentList in 'Sources\InstrumentList.pas',
  LoginIB in 'Sources\LoginIB.pas' {frmLoginIB} ,
  LoginNN in 'Sources\LoginNN.pas' {TfrmLoginNN} ,
  MessageDialog in 'Sources\MessageDialog.pas',
  Monitor in 'Sources\Monitor.pas' {frmMonitor} ,
  Monitor.EventController in 'Sources\Monitor.EventController.pas' {frmEventController} ,
  Monitor.Info in 'Sources\Monitor.Info.pas',
  Monitor.Interfaces in 'Sources\Monitor.Interfaces.pas',
  Monitor.Types in 'Sources\Monitor.Types.pas',
  MonitorTree.Document in 'Sources\MonitorTree.Document.pas',
  MonitorTree.EditDocuments in 'Sources\MonitorTree.EditDocuments.pas',
  MonitorTree.Factory in 'Sources\MonitorTree.Factory.pas',
  MonitorTree.Helper in 'Sources\MonitorTree.Helper.pas',
  NNfunctions in 'Sources\NordNetAPI\NNfunctions.pas',
  NNfunctions.Types in 'Sources\NordNetAPI\NNfunctions.Types.pas',
  OpenDialog.Algos in 'Sources\OpenDialog.Algos.pas' {TfrmOpenAlgos} ,
  OpenDialog.Conditions in 'Sources\OpenDialog.Conditions.pas' {frmOpenCondition} ,
  OpenDialog.OptionList in 'Sources\OpenDialog.OptionList.pas' {frmOpenOptionList} ,
  OpenDialog.OrderGroupSet in 'Sources\OpenDialog.OrderGroupSet.pas' {frmOrderGroupSet} ,
  Order.Utils in 'Sources\Order.Utils.pas',
  OrderChange in 'Sources\OrderChange.pas' {frmOrderChange} ,
  ParametersStore in 'Sources\ParametersStore.pas',
  Publishers in 'Sources\Publishers.pas',
  Publishers.Interfaces in 'Sources\Publishers.Interfaces.pas',
  Qualifiers in 'Sources\Qualifiers\Qualifiers.pas' {frmQualifiers} ,
  Qualifiers.Edit in 'Sources\Qualifiers\Qualifiers.Edit.pas' {frmQualifiersEdit} ,
  Qualifiers.EditCondition in 'Sources\Qualifiers\Qualifiers.EditCondition.pas' {frmQualifierConditionEdit} ,
  Qualifiers.Frame in 'Sources\Qualifiers\Qualifiers.Frame.pas' {frameQualifiers: TFrame} ,
  Qualifiers.Types in 'Sources\Qualifiers\Qualifiers.Types.pas',
  Records.Helper in 'Sources\Records.Helper.pas',
  Relations in 'Sources\Relations.pas' {frmRelation} ,
  Scanner.AllFilters in 'Sources\Scanner\Scanner.AllFilters.pas' {frmScannerAllFilters} ,
  Scanner.DragDropOptions in 'Sources\Scanner\Scanner.DragDropOptions.pas' {frmDragDropOptions} ,
  Scanner.EditWeight in 'Sources\Scanner\Scanner.EditWeight.pas' {frmScannerEditWeight} ,
  Scanner.EmbargoColumn in 'Sources\Scanner\Scanner.EmbargoColumn.pas' {frmScannerEmbargoColumn} ,
  Scanner.FilterList in 'Sources\Scanner\Scanner.FilterList.pas' {frmScannerFilterList} ,
  Scanner.Filters in 'Sources\Scanner\Scanner.Filters.pas',
  Scanner.GradientColumn in 'Sources\Scanner\Scanner.GradientColumn.pas' {frmScannerGradientColumn} ,
  Scanner.Main in 'Sources\Scanner\Scanner.Main.pas' {frmScannerMain} ,
  Scanner.Market in 'Sources\Scanner\Scanner.Market.pas' {frmScannerMarket} ,
  Scanner.MarketOpenSequence in 'Sources\Scanner\Scanner.MarketOpenSequence.pas' {frmScannerMarketOpenSequence} ,
  Scanner.OpenGroup in 'Sources\Scanner\Scanner.OpenGroup.pas' {frmScannerOpenGroup} ,
  Scanner.StaticList in 'Sources\Scanner\Scanner.StaticList.pas' {frmScannerStaticList} ,
  Scanner.StaticLists in 'Sources\Scanner\Scanner.StaticLists.pas' {frmScannerStaticLists} ,
  Scanner.TickColumn in 'Sources\Scanner\Scanner.TickColumn.pas' {frmScannerTickColumn} ,
  Scanner.Types in 'Sources\Scanner\Scanner.Types.pas',
  Search.Instruments in 'Sources\Search\Search.Instruments.pas' {frmSearchInstruments} ,
  Search.InstrumentsNN in 'Sources\Search\Search.InstrumentsNN.pas' {frmSearchInstrumentsNN} ,
  Search.RequestToIB in 'Sources\Search\Search.RequestToIB.pas' {frmRequestToIB} ,
  Search.Types in 'Sources\Search\Search.Types.pas',
  SplashScreen in 'Sources\SplashScreen.pas' {frmSplashScreen} ,
  TBfunctions in 'Sources\TBfunctions.pas',
  TickByTick.Data in 'Sources\TickByTick.Data.pas' {frmTickByTick} ,
  UCombo in 'Sources\IABSocketAPI\BCB\UCombo.pas' {FComboOrder} ,
  UDepth in 'Sources\IABSocketAPI\BCB\UDepth.pas' {FDepth} ,
  UHistoricalData in 'Sources\IABSocketAPI\BCB\UHistoricalData.pas' {FHistoricalData} ,
  UIABsocket in 'Sources\IABSocketAPI\BCB\UIABsocket.pas' {FIABSocket} ,
  UScanForm in 'Sources\IABSocketAPI\BCB\UScanForm.pas' {FScanForm} ,
  UTextForm in 'Sources\IABSocketAPI\BCB\UTextForm.pas' {FTextForm} ,
  Utils in 'Sources\Utils.pas',
  Utils.LocalInformation in 'Sources\Utils.LocalInformation.pas',
  Utils.VerInfo in 'Sources\Utils.VerInfo.pas',
  UXcution in 'Sources\IABSocketAPI\BCB\UXcution.pas' {FExecution} ,
  VirtualTrees in 'Sources\Virtual TreeView\VirtualTrees.pas',
  VirtualTrees.Editors in 'Sources\Virtual TreeView\VirtualTrees.Editors.pas' {TGridEditLink} ,
  VirtualTrees.ExportHelper in 'Sources\Virtual TreeView\VirtualTrees.ExportHelper.pas',
  VirtualTrees.Helper in 'Sources\Virtual TreeView\VirtualTrees.Helper.pas',
  XmlFiles in 'Sources\XmlFiles.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
{$IFDEF DEBUG}
  // ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$ENDIF}
  try
    DeleteFile('CrntStreamIn.txt');
    DeleteFile('CrntStreamOut.txt');
    DeleteFile('LastStreamIn.txt');
    Application.Initialize;
    IsMultiThread := True;
    Application.MainFormOnTaskbar := True;
    Application.HintPause := 50;
    Application.CreateForm(TDMod, DMod);
    Application.CreateForm(TDMImage, DMImage);
    Application.CreateForm(TfrmMonitor, frmMonitor);
    TfrmSplashScreen.ShowSplashScreen;
    try
      DMod.Initialize;
      frmMonitor.Initialize;
      Application.ShowMainForm := True;
    finally
      TfrmSplashScreen.HideSplashScreen;
    end;
    Application.Run;
  except
    on E: Exception do
    begin
      TfrmSplashScreen.HideSplashScreen;
      if Assigned(LogWriter) then
        LogWriter.Write(ddError, E.Message);
    end;
  end;

end.
