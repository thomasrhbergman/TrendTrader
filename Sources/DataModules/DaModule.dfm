object DMod: TDMod
  OldCreateOrder = True
  Height = 691
  Width = 752
  object dsInstruments: TDataSource
    DataSet = fbtInstruments
    Left = 516
    Top = 72
  end
  object dsOrderGr: TDataSource
    DataSet = fbqOrderGr
    Left = 32
    Top = 136
  end
  object dsAktieKurs: TDataSource
    AutoEdit = False
    DataSet = fbqAktieKurs
    Left = 416
    Top = 128
  end
  object dsQualifiers: TDataSource
    DataSet = fbqQualifiers
    Left = 512
    Top = 188
  end
  object dsAutoTrades: TDataSource
    DataSet = fbqAutoTrades
    Left = 576
    Top = 188
  end
  object dsAccounts: TDataSource
    DataSet = fbtAccounts
    Left = 642
    Top = 188
  end
  object NotificationCenter: TNotificationCenter
    Left = 132
    Top = 200
  end
  object ConnectionStock: TFDConnection
    Params.Strings = (
      'Database=STOCKXROBOT.FDB'
      'Password=masterkey'
      'User_Name=SYSDBA'
      'Port=3051'
      'DriverID=FB')
    TxOptions.AutoStop = False
    LoginPrompt = False
    Transaction = TransactionStock
    AfterDisconnect = ConnectionStockAfterDisconnect
    BeforeDisconnect = ConnectionStockBeforeDisconnect
    Left = 40
    Top = 336
  end
  object ConnectionFeed: TFDConnection
    Params.Strings = (
      'Database=DB_FEED.FDB'
      'Password=masterkey'
      'User_Name=SYSDBA'
      'Port=3051'
      'DriverID=FB')
    TxOptions.AutoStop = False
    LoginPrompt = False
    Transaction = TransactionFeed
    Left = 152
    Top = 336
  end
  object TransactionStock: TFDTransaction
    Options.AutoStop = False
    Connection = ConnectionStock
    Left = 40
    Top = 400
  end
  object TransactionFeed: TFDTransaction
    Options.AutoStop = False
    Connection = ConnectionFeed
    Left = 152
    Top = 400
  end
  object FBStoredProc: TFDStoredProc
    Connection = ConnectionStock
    Transaction = TransactionStock
    Left = 280
    Top = 336
  end
  object FBQuery: TFDQuery
    Connection = ConnectionStock
    Transaction = TransactionStock
    Left = 280
    Top = 400
  end
  object fbqOrderGr: TFDQuery
    Connection = ConnectionStock
    Transaction = TransactionStock
    SQL.Strings = (
      'SELECT * FROM ORDER_GROUP;')
    Left = 24
    Top = 528
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 24
    Top = 600
  end
  object FBScript: TFDScript
    SQLScripts = <>
    Connection = ConnectionStock
    Transaction = TransactionStock
    Params = <>
    Macros = <>
    Left = 424
    Top = 560
  end
  object fbqAutoTrades: TFDQuery
    Connection = ConnectionStock
    Transaction = TransactionStock
    SQL.Strings = (
      'select ID, NAME, NOTE, ENABLED'
      'from AUTOTRADES'
      'where ENABLED = true'
      'union all'
      
        'select cast(-1 as INTEGER), cast('#39'Autotrade - Exploration'#39' as va' +
        'rchar(100)), cast('#39'NOTE'#39' as varchar(500)), true as ENABLED'
      'from ACCOUNTS  ')
    Left = 576
    Top = 528
  end
  object fbqQualifiers: TFDQuery
    Connection = ConnectionStock
    Transaction = TransactionStock
    SQL.Strings = (
      'select * '
      'from Qualifiers'
      'order by Id')
    Left = 512
    Top = 528
  end
  object fbtAccounts: TFDTable
    Connection = ConnectionStock
    Transaction = TransactionStock
    TableName = 'ACCOUNTS'
    Left = 640
    Top = 528
    object fbtAccountsACCOUNT_ID: TIntegerField
      FieldName = 'ACCOUNT_ID'
      Origin = 'ACCOUNT_ID'
      Required = True
    end
    object fbtAccountsUSER_ID: TIntegerField
      FieldName = 'USER_ID'
      Origin = 'USER_ID'
      Required = True
    end
    object fbtAccountsBROKER_ID: TIntegerField
      FieldName = 'BROKER_ID'
      Origin = 'BROKER_ID'
    end
    object fbtAccountsACC_USERNAME: TStringField
      FieldName = 'ACC_USERNAME'
      Origin = 'ACC_USERNAME'
    end
    object fbtAccountsACC_PWORD: TStringField
      FieldName = 'ACC_PWORD'
      Origin = 'ACC_PWORD'
    end
    object fbtAccountsACC_PARAMS: TStringField
      FieldName = 'ACC_PARAMS'
      Origin = 'ACC_PARAMS'
      Size = 30
    end
    object fbtAccountsAUTOTRADESID: TIntegerField
      FieldName = 'AUTOTRADESID'
      Origin = 'AUTOTRADESID'
    end
  end
  object fbtInstruments: TFDTable
    Connection = ConnectionStock
    TableName = 'SOKID_IB'
    Left = 528
    Top = 408
    object fbtInstrumentsCONID: TIntegerField
      FieldName = 'CONID'
      Origin = 'CONID'
      Required = True
    end
    object fbtInstrumentsSYMBOL: TStringField
      FieldName = 'SYMBOL'
      Origin = 'SYMBOL'
      Required = True
    end
    object fbtInstrumentsOMXS30_W: TSingleField
      FieldName = 'OMXS30_W'
      Origin = 'OMXS30_W'
    end
    object fbtInstrumentsCONTRACTYPE: TStringField
      FieldName = 'CONTRACTYPE'
      Origin = 'CONTRACTYPE'
      Size = 10
    end
    object fbtInstrumentsCOUNTRY_REGION: TStringField
      FieldName = 'COUNTRY_REGION'
      Origin = 'COUNTRY_REGION'
      Size = 15
    end
    object fbtInstrumentsCURRENCY: TStringField
      FieldName = 'CURRENCY'
      Origin = 'CURRENCY'
      Size = 10
    end
    object fbtInstrumentsASSETID: TStringField
      FieldName = 'ASSETID'
      Origin = 'ASSETID'
      Size = 12
    end
    object fbtInstrumentsNOSHARES: TStringField
      FieldName = 'NOSHARES'
      Origin = 'NOSHARES'
      Size = 12
    end
    object fbtInstrumentsISIN: TStringField
      FieldName = 'ISIN'
      Origin = 'ISIN'
      Size = 15
    end
    object fbtInstrumentsSTOCKTYPE: TStringField
      FieldName = 'STOCKTYPE'
      Origin = 'STOCKTYPE'
      Size = 15
    end
    object fbtInstrumentsSECTOR: TStringField
      FieldName = 'SECTOR'
      Origin = 'SECTOR'
      Size = 10
    end
    object fbtInstrumentsGROUP: TStringField
      FieldName = 'GROUP'
      Origin = '"GROUP"'
      Size = 10
    end
    object fbtInstrumentsFUTURESTYPE: TStringField
      FieldName = 'FUTURESTYPE'
      Origin = 'FUTURESTYPE'
      Size = 10
    end
    object fbtInstrumentsLASTTRADINGDATE: TDateField
      FieldName = 'LASTTRADINGDATE'
      Origin = 'LASTTRADINGDATE'
    end
    object fbtInstrumentsEXPIRATIONDATE: TDateField
      FieldName = 'EXPIRATIONDATE'
      Origin = 'EXPIRATIONDATE'
    end
    object fbtInstrumentsCONTRACTMONTH: TDateField
      FieldName = 'CONTRACTMONTH'
      Origin = 'CONTRACTMONTH'
    end
    object fbtInstrumentsMULTIPLIER: TIntegerField
      FieldName = 'MULTIPLIER'
      Origin = 'MULTIPLIER'
    end
    object fbtInstrumentsDECIMALS: TSmallintField
      FieldName = 'DECIMALS'
      Origin = 'DECIMALS'
    end
    object fbtInstrumentsPRIMARYINDEX: TStringField
      FieldName = 'PRIMARYINDEX'
      Origin = 'PRIMARYINDEX'
      Size = 10
    end
    object fbtInstrumentsMARKET_RULE_IDS: TStringField
      FieldName = 'MARKET_RULE_IDS'
      Origin = 'MARKET_RULE_IDS'
      Size = 100
    end
    object fbtInstrumentsMARKET_LIST: TStringField
      FieldName = 'MARKET_LIST'
      Origin = 'MARKET_LIST'
      Size = 100
    end
    object fbtInstrumentsDESCRIPTION: TStringField
      FieldName = 'DESCRIPTION'
      Origin = 'DESCRIPTION'
      Size = 200
    end
    object fbtInstrumentsEXCHANGE: TStringField
      FieldName = 'EXCHANGE'
      Origin = 'EXCHANGE'
      Size = 40
    end
    object fbtInstrumentsNAME: TStringField
      FieldName = 'NAME'
      Origin = 'NAME'
      Size = 200
    end
    object fbtInstrumentsBROKER: TIntegerField
      FieldName = 'BROKER'
      Origin = 'BROKER'
    end
    object fbtInstrumentsLAST_PRICE: TSingleField
      FieldName = 'LAST_PRICE'
      Origin = 'LAST_PRICE'
    end
    object fbtInstrumentsPRIMARY_EXCHANGE: TStringField
      FieldName = 'PRIMARY_EXCHANGE'
      Origin = 'PRIMARY_EXCHANGE'
      Size = 100
    end
    object fbtInstrumentsISOLATE: TIntegerField
      FieldName = 'ISOLATE'
      Origin = 'ISOLATE'
    end
    object fbtInstrumentsLOCAL_SYMBOL: TStringField
      FieldName = 'LOCAL_SYMBOL'
      Origin = 'LOCAL_SYMBOL'
      Size = 100
    end
    object fbtInstrumentsMINIMUM_TICK: TSingleField
      FieldName = 'MINIMUM_TICK'
      Origin = 'MINIMUM_TICK'
    end
    object fbtInstrumentsERROR_CODE: TIntegerField
      FieldName = 'ERROR_CODE'
      Origin = 'ERROR_CODE'
    end
    object fbtInstrumentsCATEGORY: TStringField
      FieldName = 'CATEGORY'
      Origin = 'CATEGORY'
      Size = 100
    end
    object fbtInstrumentsSUBCATEGORY: TStringField
      FieldName = 'SUBCATEGORY'
      Origin = 'SUBCATEGORY'
      Size = 100
    end
    object fbtInstrumentsUNDERLYING_CONID: TIntegerField
      FieldName = 'UNDERLYING_CONID'
      Origin = 'UNDERLYING_CONID'
    end
    object fbtInstrumentsINDUSTRY: TStringField
      FieldName = 'INDUSTRY'
      Origin = 'INDUSTRY'
      Size = 100
    end
    object fbtInstrumentsSTRIKE: TSingleField
      FieldName = 'STRIKE'
      Origin = 'STRIKE'
    end
  end
  object fbqAktie: TFDQuery
    Connection = ConnectionFeed
    Transaction = TransactionFeed
    SQL.Strings = (
      'SELECT * '
      'FROM AKTIE '
      'ORDER BY A_NR')
    Left = 432
    Top = 408
    object fbqAktieA_NAMN: TStringField
      FieldName = 'A_NAMN'
      Origin = 'A_NAMN'
      Required = True
      Size = 30
    end
    object fbqAktieA_NR: TSmallintField
      FieldName = 'A_NR'
      Origin = 'A_NR'
      Required = True
    end
    object fbqAktieI_INDEX: TStringField
      FieldName = 'I_INDEX'
      Origin = 'I_INDEX'
      Size = 10
    end
  end
  object fbqAktieKurs: TFDQuery
    Connection = ConnectionFeed
    Transaction = TransactionFeed
    SQL.Strings = (
      'SELECT * '
      'FROM AKTIEKURS'
      '')
    Left = 432
    Top = 464
    object fbqAktieKursK_DBNR: TSmallintField
      FieldName = 'K_DBNR'
      Origin = 'K_DBNR'
      Required = True
    end
    object fbqAktieKursK_TID: TSQLTimeStampField
      FieldName = 'K_TID'
      Origin = 'K_TID'
      Required = True
    end
    object fbqAktieKursK_FIELD: TStringField
      FieldName = 'K_FIELD'
      Origin = 'K_FIELD'
      Required = True
      Size = 10
    end
    object fbqAktieKursK_KURSEN: TCurrencyField
      FieldName = 'K_KURSEN'
      Origin = 'K_KURSEN'
      Required = True
    end
  end
  object FDFBNBackup: TFDFBNBackup
    DriverLink = FDPhysFBDriverLink1
    Left = 272
    Top = 584
  end
end
