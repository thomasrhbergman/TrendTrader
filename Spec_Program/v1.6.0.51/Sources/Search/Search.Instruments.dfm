object frmSearchInstruments: TfrmSearchInstruments
  Left = 0
  Top = 0
  ActiveControl = frameRealtimeFeeds.vstTree
  Caption = 'Search'
  ClientHeight = 686
  ClientWidth = 1229
  Color = clBtnFace
  Constraints.MinHeight = 630
  Constraints.MinWidth = 750
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poDefault
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object sbMain: TStatusBar
    Left = 0
    Top = 667
    Width = 1229
    Height = 19
    Panels = <>
  end
  object pnlSearchParam: TPanel
    Left = 0
    Top = 0
    Width = 291
    Height = 667
    Align = alLeft
    BevelOuter = bvNone
    PopupMenu = pmSearchParam
    TabOrder = 0
    object pcBrokers: TPageControl
      Left = 0
      Top = 0
      Width = 291
      Height = 556
      ActivePage = tsInteractiveBroker
      Align = alClient
      TabOrder = 0
      OnChange = pcBrokersChange
      object tsInteractiveBroker: TTabSheet
        Caption = 'Interactive Broker'
        ImageIndex = 1
        object pnlInteractiveBroker: TPanel
          Left = 0
          Top = 0
          Width = 283
          Height = 528
          Align = alClient
          BevelOuter = bvNone
          Constraints.MinHeight = 230
          TabOrder = 0
          object pnlIBSearch: TPanel
            Left = 0
            Top = 0
            Width = 283
            Height = 241
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object lblSecurityType: TLabel
              Left = 12
              Top = 142
              Width = 83
              Height = 16
              Alignment = taRightJustify
              Caption = 'Security Type:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object lblSearchString: TLabel
              Left = 25
              Top = 32
              Width = 70
              Height = 16
              Alignment = taRightJustify
              Caption = 'Search text:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object lblSearchFunction: TLabel
              Left = 1
              Top = 4
              Width = 94
              Height = 16
              Alignment = taRightJustify
              Caption = 'Search function:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object cbSecurityType: TCheckListBox
              Left = 98
              Top = 140
              Width = 180
              Height = 96
              Enabled = False
              ItemHeight = 13
              TabOrder = 0
            end
            object cbUseSecurityType: TCheckBox
              Left = 4
              Top = 119
              Width = 155
              Height = 17
              Caption = 'Filter by security type'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 1
              OnClick = cbUseSecurityTypeClick
            end
            object edMatchingSymbol: TComboBox
              Left = 98
              Top = 29
              Width = 180
              Height = 24
              CharCase = ecUpperCase
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              OnKeyDown = DoEditKeyDown
            end
            object cbSearchFunction: TComboBox
              Left = 98
              Top = 1
              Width = 180
              Height = 24
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 3
              OnChange = cbSearchFunctionChange
              Items.Strings = (
                'Search From DB'
                'Request Matching Symbols'
                'Search Contract Details')
            end
            object rbTypeSearch: TRadioGroup
              Left = 98
              Top = 57
              Width = 180
              Height = 62
              Caption = ' Search '
              ItemIndex = 0
              Items.Strings = (
                'by part of Symbol'
                'by part of Name')
              TabOrder = 4
            end
          end
          object gbContractDetails: TGroupBox
            Left = 0
            Top = 301
            Width = 283
            Height = 227
            Align = alBottom
            Caption = 'Request Contract Details'
            Color = clBtnHighlight
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentBackground = False
            ParentColor = False
            ParentFont = False
            TabOrder = 1
            object lblExtExpiryMonth: TLabel
              Left = 14
              Top = 130
              Width = 78
              Height = 16
              Alignment = taRightJustify
              Caption = 'Expiry Month:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object lblExtCurrency: TLabel
              Left = 36
              Top = 103
              Width = 56
              Height = 16
              Alignment = taRightJustify
              Caption = 'Currency:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object lblExtExchange: TLabel
              Left = 33
              Top = 76
              Width = 59
              Height = 16
              Alignment = taRightJustify
              Caption = 'Exchange:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object lblExtSymbol: TLabel
              Left = 45
              Top = 49
              Width = 47
              Height = 16
              Alignment = taRightJustify
              Caption = 'Symbol:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object lblExtSecurityTypeCaption: TLabel
              Left = 9
              Top = 22
              Width = 83
              Height = 16
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Security Type:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object cbExtCurrency: TComboBox
              Left = 98
              Top = 100
              Width = 100
              Height = 24
              CharCase = ecUpperCase
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              Sorted = True
              TabOrder = 3
            end
            object cbExtExchange: TComboBox
              Left = 98
              Top = 73
              Width = 100
              Height = 24
              CharCase = ecUpperCase
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              Sorted = True
              TabOrder = 2
              Items.Strings = (
                'GLOBEX'
                'INDU'
                'OMXS30')
            end
            object cbExtSymbol: TComboBox
              Left = 98
              Top = 46
              Width = 100
              Height = 24
              CharCase = ecUpperCase
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              Sorted = True
              TabOrder = 1
              OnKeyDown = DoEditKeyDown
              Items.Strings = (
                '')
            end
            object rbNextRolling: TRadioButton
              Left = 98
              Top = 179
              Width = 113
              Height = 17
              Caption = 'Next Rolling'
              TabOrder = 5
            end
            object rbFuturesCalendar: TRadioButton
              Left = 98
              Top = 202
              Width = 113
              Height = 17
              Caption = 'Futures Calendar'
              TabOrder = 6
            end
            object edExtExpiryMonth: TEdit
              Left = 98
              Top = 127
              Width = 100
              Height = 24
              TabOrder = 4
            end
            object cbExtSecurityType: TComboBox
              Left = 98
              Top = 19
              Width = 100
              Height = 24
              CharCase = ecUpperCase
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              OnChange = cbExtSecurityTypeChange
            end
            object rbOnlySecurityType: TRadioButton
              Left = 98
              Top = 156
              Width = 143
              Height = 17
              Caption = 'Only Security Type'
              Checked = True
              TabOrder = 7
              TabStop = True
            end
          end
        end
      end
      object tsNordNetBroker: TTabSheet
        Caption = 'Nordnet Broker'
        object lblSymbolName: TLabel
          Left = 19
          Top = 74
          Width = 78
          Height = 16
          Caption = 'Symbol name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblInstrumentGroup: TLabel
          Left = 20
          Top = 124
          Width = 100
          Height = 16
          Caption = 'Instrument Group'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblInstrumentType: TLabel
          Left = 20
          Top = 273
          Width = 94
          Height = 16
          Caption = 'Instrument Type'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblMarket: TLabel
          Left = 20
          Top = 328
          Width = 39
          Height = 16
          Caption = 'Market'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblCurrency: TLabel
          Left = 20
          Top = 172
          Width = 51
          Height = 16
          Caption = 'Currency'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblSector: TLabel
          Left = 20
          Top = 382
          Width = 37
          Height = 16
          Caption = 'Sector'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblTradablesMarkets: TLabel
          Left = 20
          Top = 219
          Width = 115
          Height = 16
          Caption = 'Tradables / Markets'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object edSearch: TEdit
          Left = 20
          Top = 44
          Width = 212
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
        end
        object rgSearchType: TRadioGroup
          Left = 20
          Top = 6
          Width = 261
          Height = 35
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Columns = 2
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemIndex = 0
          Items.Strings = (
            'Instrument search'
            'Select list')
          ParentFont = False
          TabOrder = 0
          OnClick = rgSearchTypeClick
        end
        object cbInstrList: TComboBox
          Left = 20
          Top = 44
          Width = 227
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          Text = 'cbInstrList'
          Visible = False
          Items.Strings = (
            'All'
            'display_order,list_id,symbol,country,name'
            '10,16314763,Large Cap Stockholm SEK,SE,Large Cap Stockholm SEK'
            '20,16314764,Mid Cap Stockholm SEK,,Mid Cap Stockholm SEK'
            '30,16314765,Small Cap Stockholm SEK,,Small Cap Stockholm SEK'
            '40,16314713,First North STO,,First North STO'
            '50,16314711,AktieTorget-cotr,,AktieTorget-cotr'
            '60,16314596,OMXS30,SE,OMX Stockholm 30 Index'
            '70,16314753,RIGHTS ETC.,,RIGHTS ETC.'
            '80,16314880,Premium Bonds 2013,SE,Premium Bonds 2013'
            '210,16314757,Large Cap Helsinki EUR,SE,Large Cap Helsinki EUR'
            '220,16314758,Mid Cap Helsinki EUR,,Mid Cap Helsinki EUR'
            '230,16314759,Small Cap Helsinki EUR,,Small Cap Helsinki EUR'
            '240,16314788,First North Finland,,First North Finland'
            '250,16314503,OMXH25,FI,OMX Helsinki 25'
            '310,16314769,Large Cap Copenhagen DKK,,Large Cap Copenhagen DKK'
            '320,16314770,Mid Cap Copenhagen DKK,,Mid Cap Copenhagen DKK'
            '330,16314771,Small Cap Copenhagen DKK,,Small Cap Copenhagen DKK'
            '340,16314751,First North CSE DKK,,First North CSE DKK'
            '350,16314593,OMXC20,DK,OMX Copenhagen 20'
            '110,16384829,OSLO_LARGE,NO,Large Cap Oslo'
            '120,16384830,OSLO_MID,NO,Mid Cap Oslo'
            '130,16384831,OSLO_SMALL,NO,Small Cap Oslo'
            '510,16384832,DE_DAX,DE,DAX'
            '520,16384833,DE_H-DAX,DE,H-DAX'
            '530,16384834,DE_MDAX,DE,MDAX'
            '540,16384835,DE_SDAX,DE,SDAX'
            '550,16384836,DE_TecDAX,DE,TecDAX'
            '560,16384837,DE_ETC,DE,ETC'
            '570,16384838,DE_ETF,DE,ETF'
            '510,16384840,US_SP100,US,S&P 100'
            '520,16384841,US_DJIA,US,DJIA'
            '530,16384843,US_NAS100,US,Nasdaq 100'
            '540,16384842,US_NAS100F,US,Nasdaq 100 Financial'
            '575,16384839,US_NORDIC,US,Norden'
            '580,16384848,US_SE,US,Sverige'
            '585,16384849,US_NO,US,Norge'
            '590,16384850,US_DK,US,Danmark'
            '595,16384851,US_FI,US,Finland'
            '610,16384844,CA_TSX_COMP,CA,TSX COMPOSITE INDX'
            '630,16384845,CA_TSX_GOLD,CA,TSX GOLD INDX'
            '640,16384846,CA_TSX_MET,CA,TSX MET & MIN INDX'
            '650,16384847,CA_TSX_60,CA,TSX 60 INDX'
            '660,16314727,Index funds,,Index funds')
        end
        object eSymbolFilter: TEdit
          Left = 20
          Top = 94
          Width = 227
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnKeyDown = DoEditKeyDown
        end
        object cbInstrumentGroupFilter: TComboBox
          Left = 20
          Top = 142
          Width = 227
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 4
          Text = 'All'
          Items.Strings = (
            'All'
            'YE,60,Yield enhancement'
            'KCLC,45,Constant Leverage Certicate'
            'DCUR,101,Money market instruments'
            'INAT,82,National'
            'OTH,99,Other'
            'YEC,61,ExpressCertificate'
            'KC,42,Knock out warrant call'
            'WNT,20,Warrant'
            'CPKO,52,Capital Protection with Knock-Out'
            'DCORP,101,Corporate bonds'
            'YDS,62,Discount certificate'
            'DOTH,102,Other'
            'FMX,18,Mixed fund'
            'CPOTH,53,Miscellaneous Capital Protection'
            'CPU,54,Uncapped Capital Protection (aktieindex)'
            'CERT,0,Other certificates'
            'IGLOB,81,Global'
            'FND,11,Fund'
            'KO,40,Leverage with knock-out'
            'LOTH,0,Other list'
            'PETN,0,ETN without leverage'
            'KP,42,Knock out warrant put'
            'FOTH,19,Other fund'
            'DB,101,Bonds'
            'FORW,93,Forward'
            'IOTH,84,Other'
            'OOTH,99,Other'
            'LMAN,0,Manual list'
            'ISEC,83,Sector'
            'EPR,5,Purchase rights'
            'POPC,73,OutperformanceCertificate'
            'ESR,4,Subscription right'
            'PINV,0,Danish investment fund'
            'KMS,44,Mini short'
            'PBC,72,Bonus certificate'
            'YOTH,65,Miscellaneous Yield Enhancement'
            'DSRB,101,Subscription right bond'
            'EOTC,3,Share OTC'
            'CPWC,55,Capital protection with Coupon'
            'AUND,0,Underlying asset'
            'WC,31,Warrant call'
            'KML,44,Mini long'
            'IDX,80,Index'
            'PTC,74,Tracker certificate (open-end)'
            'EAR,7,Allotment rights'
            'CALL,91,Option call'
            'COMB,93,Standard Combination'
            'KETN,0,ETN with leverage'
            'FFI,13,Fixed income fund'
            'WOKO,30,Leverage without knock-out'
            'KETF,46,ETF with leverage'
            'WP,31,Warrant put'
            'LIST,0,List'
            'PETF,71,ETF without leverage'
            'DMTN,101,Medium-term notes'
            'DCB,101,Convertible bonds'
            'FEQ,12,Equity fund'
            'KBE,43,Bear certificate'
            'LLIST,0,List of lists'
            'PAR,70,Participation'
            'FUT,93,Future'
            'AST,0,Asset'
            'DEBT,100,Debt'
            'DGB,101,Agencies of government bond'
            'PUT,92,Option put'
            'CP,50,Capital Protection'
            'CPC,51,Capped Capital Protected'
            'ESH,2,Share'
            'KBU,43,Bull certificate'
            'DBW,101,Bonds with warrants attached'
            'KTL,41,Turbo warrant long'
            'LEXT,0,External list'
            'EQ,1,Equity'
            'FCF,16,Commodity fund'
            'DER,90,Derivative'
            'YRC,64,Reverse Convertibles'
            'FH,14,Hedge fund'
            'DLGB,101,Municipal and local government bond'
            'KTS,41,Turbo warrant short'
            'EIS,6,Interim share'
            'FCUR,17,Currency fund'
            'YBRC,63,Barrier Reverse Convertibles'
            'DTB,101,T-bill')
        end
        object cbMarketFilter: TComboBox
          Left = 20
          Top = 355
          Width = 227
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 5
          Text = 'All'
          Items.Strings = (
            'All'
            '80,true,Smart order,'
            '40,,Saxess SE SOX,SE'
            '23,,OM Denmark,DK'
            '12,,OM Sweden,SE'
            '14,,Nasdaq OMX Copenhagen,DK'
            '30,,Burgundy Sweden,SE'
            '33,,Burgundy Finland,FI'
            '26,,Canadian Venture Exchange,CA'
            '35,,NDX Sweden,SE'
            '24,,Nasdaq OMX Helsinki,FI'
            '17,,New York Stock Exchange,US'
            '34,,Sola,NO'
            '115,,EUREX,DE'
            '13,,NGM,SE'
            '42,,Saxess FI SOX,FI'
            '19,,Nasdaq,US'
            '11,,Nasdaq OMX Stockholm,SE'
            '4,,Xetra,DE'
            '15,,Millennium OSE,NO'
            '37,,NDX Norway,NO'
            '36,,NDX Finland,FI'
            '18,,American Stock Exchange,US'
            '25,,Toronto Stock Exchange,CA'
            '32,,Burgundy Denmark,DK'
            '21,,Nasdaq OTC Foreign,US'
            '20,,Nasdaq OTC Domestic,US')
        end
        object cbCurrencyFilter: TComboBox
          Left = 20
          Top = 192
          Width = 227
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 6
          Text = 'All'
          Items.Strings = (
            'All'
            'SEK'
            'USD'
            'EUR'
            '')
        end
        object cbSectorFilter: TComboBox
          Left = 20
          Top = 410
          Width = 227
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 7
          Text = 'All'
          Items.Strings = (
            'All'
            'AEROSPACE_AND_DEFENCE'
            'AIR_FREIGHT_AND_LOGISTICS'
            'AIRLINES'
            'AUTO_COMPONENTS'
            'AUTOMOBILES'
            'AUTOMOBILES_AND_COMPONENTS'
            'BEVERAGES'
            'BIOTECHNOLOGY'
            'BUILDING_PRODUCTS'
            'CAPITAL_GOODS'
            'CAPITAL_MARKETS'
            'CHEMICALS'
            'COMMERCIAL_BANKS'
            'COMMERCIAL_SERVICES_AND_SUPPLIES'
            'COMMUNICATIONS_EQUIPMENT'
            'COMPUTERS_AND_PERIPHERALS'
            'CONSTRUCTION_AND_ENGINEERING'
            'CONSTRUCTION_MATERIALS'
            'CONSUMER_FINANCE'
            'CONSUMER_LEISURE_PRODUCTS'
            'CONSUMER_PRODUCTS'
            'CONTAINERS_AND_PACKAGING'
            'DIVERSIFIED_FINANCIAL_SERVICES'
            'ELECTRICAL_EQUIPMENT'
            'ENERGY'
            'ENERGY_EQUIPMENT_AND_SERVICES'
            'ENERGY_PRODUCERS_AND_TRADERS'
            'ENERGY_UTILITIES'
            'FINANCIAL'
            'FOOD_PRODUCTS'
            'HEALTH_CARE'
            'HEALTH_CARE_EQUIPMENT_AND_SUPPLIES'
            'HEALTH_CARE_PROVIDERS_AND_SERVICES'
            'HOTEL_RESTAURANT_AND_LEISURE'
            'HOUSEHOLD_DURABLES'
            'HOUSEHOLD_SERVICES'
            'INDUSTRIAL_CONGLOMERATE'
            'INSTRUMENTS_AND_COMPONENTS'
            'INSURANCE'
            'IT'
            'IT_SERVICES'
            'LIFE_SCIENCES_TOOLS_AND_SERVICES'
            'MACHINERY'
            'MARINE'
            'MATERIALS'
            'MEDIA'
            'METALS_AND_MINING'
            'OIL_GAS_AND_CONSUMABLE_FUELS'
            'PAPER_AND_FOREST'
            'PERSONAL_PRODUCTS'
            'PHARMACEUTICALS'
            'PROFESSIONAL_SERVICES'
            'REAL_ESTATE'
            'REAL_ESTATE_INVESTMENT_TRUSTS'
            'REAL_ESTATE_MANAGEMENT_AND_DEVELOPMENT'
            'RETAIL'
            'ROAD_AND_RAIL'
            'SEMICONDUCTOR'
            'SERVICES'
            'SOFTWARE'
            'TELECOM'
            'TEXTILE_APPAREL_AND_LUXURY_PRODUCTS'
            'TOBACCO'
            'TRADING_AND_DISTRIBUTION'
            'TRANSPORTATION'
            'TRANSPORTATION_INFRASTRUCTURE')
        end
        object cbTradablesFilter: TComboBox
          Left = 20
          Top = 246
          Width = 227
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 8
          Text = 'All'
          Items.Strings = (
            'All'
            '80,true,Smart order,'
            '40,,Saxess SE SOX,SE'
            '23,,OM Denmark,DK'
            '12,,OM Sweden,SE'
            '14,,Nasdaq OMX Copenhagen,DK'
            '30,,Burgundy Sweden,SE'
            '33,,Burgundy Finland,FI'
            '26,,Canadian Venture Exchange,CA'
            '35,,NDX Sweden,SE'
            '24,,Nasdaq OMX Helsinki,FI'
            '17,,New York Stock Exchange,US'
            '34,,Sola,NO'
            '115,,EUREX,DE'
            '13,,NGM,SE'
            '42,,Saxess FI SOX,FI'
            '19,,Nasdaq,US'
            '11,,Nasdaq OMX Stockholm,SE'
            '4,,Xetra,DE'
            '15,,Millennium OSE,NO'
            '37,,NDX Norway,NO'
            '36,,NDX Finland,FI'
            '18,,American Stock Exchange,US'
            '25,,Toronto Stock Exchange,CA'
            '32,,Burgundy Denmark,DK'
            '21,,Nasdaq OTC Foreign,US'
            '20,,Nasdaq OTC Domestic,US')
        end
        object cbInstrumentTypeFilter: TComboBox
          Left = 20
          Top = 301
          Width = 227
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          TabOrder = 9
          Text = 'All'
          Items.Strings = (
            'All'
            'YE,60,Yield enhancement'
            'KCLC,45,Constant Leverage Certicate'
            'DCUR,101,Money market instruments'
            'INAT,82,National'
            'OTH,99,Other'
            'YEC,61,ExpressCertificate'
            'KC,42,Knock out warrant call'
            'WNT,20,Warrant'
            'CPKO,52,Capital Protection with Knock-Out'
            'DCORP,101,Corporate bonds'
            'YDS,62,Discount certificate'
            'DOTH,102,Other'
            'FMX,18,Mixed fund'
            'CPOTH,53,Miscellaneous Capital Protection'
            'CPU,54,Uncapped Capital Protection (aktieindex)'
            'CERT,0,Other certificates'
            'IGLOB,81,Global'
            'FND,11,Fund'
            'KO,40,Leverage with knock-out'
            'LOTH,0,Other list'
            'PETN,0,ETN without leverage'
            'KP,42,Knock out warrant put'
            'FOTH,19,Other fund'
            'DB,101,Bonds'
            'FORW,93,Forward'
            'IOTH,84,Other'
            'OOTH,99,Other'
            'LMAN,0,Manual list'
            'ISEC,83,Sector'
            'EPR,5,Purchase rights'
            'POPC,73,OutperformanceCertificate'
            'ESR,4,Subscription right'
            'PINV,0,Danish investment fund'
            'KMS,44,Mini short'
            'PBC,72,Bonus certificate'
            'YOTH,65,Miscellaneous Yield Enhancement'
            'DSRB,101,Subscription right bond'
            'EOTC,3,Share OTC'
            'CPWC,55,Capital protection with Coupon'
            'AUND,0,Underlying asset'
            'WC,31,Warrant call'
            'KML,44,Mini long'
            'IDX,80,Index'
            'PTC,74,Tracker certificate (open-end)'
            'EAR,7,Allotment rights'
            'CALL,91,Option call'
            'COMB,93,Standard Combination'
            'KETN,0,ETN with leverage'
            'FFI,13,Fixed income fund'
            'WOKO,30,Leverage without knock-out'
            'KETF,46,ETF with leverage'
            'WP,31,Warrant put'
            'LIST,0,List'
            'PETF,71,ETF without leverage'
            'DMTN,101,Medium-term notes'
            'DCB,101,Convertible bonds'
            'FEQ,12,Equity fund'
            'KBE,43,Bear certificate'
            'LLIST,0,List of lists'
            'PAR,70,Participation'
            'FUT,93,Future'
            'AST,0,Asset'
            'DEBT,100,Debt'
            'DGB,101,Agencies of government bond'
            'PUT,92,Option put'
            'CP,50,Capital Protection'
            'CPC,51,Capped Capital Protected'
            'ESH,2,Share'
            'KBU,43,Bull certificate'
            'DBW,101,Bonds with warrants attached'
            'KTL,41,Turbo warrant long'
            'LEXT,0,External list'
            'EQ,1,Equity'
            'FCF,16,Commodity fund'
            'DER,90,Derivative'
            'YRC,64,Reverse Convertibles'
            'FH,14,Hedge fund'
            'DLGB,101,Municipal and local government bond'
            'KTS,41,Turbo warrant short'
            'EIS,6,Interim share'
            'FCUR,17,Currency fund'
            'YBRC,63,Barrier Reverse Convertibles'
            'DTB,101,T-bill')
        end
      end
      object tsTestBroker: TTabSheet
        Caption = 'Test Broker'
        ImageIndex = 2
        object lblSearchStringTestBroker: TLabel
          Left = 19
          Top = 15
          Width = 63
          Height = 13
          Caption = 'Search string'
        end
        object eSearchStringTestBroker: TEdit
          Left = 19
          Top = 37
          Width = 228
          Height = 21
          TabOrder = 0
          OnKeyDown = DoEditKeyDown
        end
      end
    end
    object pnlSearchBottom: TPanel
      Left = 0
      Top = 615
      Width = 291
      Height = 52
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnSearch: TBitBtn
        AlignWithMargins = True
        Left = 8
        Top = 4
        Width = 277
        Height = 43
        Action = aSearch
        Caption = 'Search and Show Result in Treeview'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Images = DMImage.vil32
        ParentFont = False
        TabOrder = 0
      end
    end
    object rgActions: TRadioGroup
      Left = 0
      Top = 556
      Width = 291
      Height = 59
      Align = alBottom
      Caption = 'Action'
      Color = clWhite
      Columns = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        'Clear'
        'Add'
        'Substract'
        'Intersection')
      ParentBackground = False
      ParentColor = False
      ParentFont = False
      TabOrder = 2
    end
  end
  object pnlCentral: TPanel
    Left = 291
    Top = 0
    Width = 938
    Height = 667
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object splContractDetails: TSplitter
      Left = 0
      Top = 240
      Width = 938
      Height = 3
      Cursor = crVSplit
      Align = alTop
      Visible = False
      ExplicitTop = 810
      ExplicitWidth = 845
    end
    object splCategory: TSplitter
      Left = 611
      Top = 243
      Width = 5
      Height = 424
      Align = alRight
      Color = clWhite
      ParentColor = False
      ExplicitLeft = 613
    end
    object pnlContractDetails: TPanel
      Left = 0
      Top = 0
      Width = 938
      Height = 240
      Align = alTop
      BevelOuter = bvNone
      Constraints.MinHeight = 240
      TabOrder = 0
      Visible = False
      object pcDerivatives: TPageControl
        Left = 0
        Top = 0
        Width = 938
        Height = 240
        Align = alClient
        TabOrder = 0
      end
    end
    object pnlInstruments: TPanel
      Left = 0
      Top = 243
      Width = 611
      Height = 424
      Align = alClient
      BevelOuter = bvNone
      PopupMenu = pmInstruments
      TabOrder = 1
      object splRealtimeFeeds: TSplitter
        Left = 0
        Top = 307
        Width = 611
        Height = 5
        Cursor = crVSplit
        Align = alTop
        Color = clWhite
        ParentColor = False
        ExplicitLeft = -1
        ExplicitTop = 258
      end
      object vstInstruments: TVirtualStringTree
        Left = 0
        Top = 36
        Width = 611
        Height = 271
        Align = alTop
        DragOperations = []
        DragType = dtVCL
        Header.AutoSizeIndex = 0
        Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
        Header.SortColumn = 14
        LineStyle = lsSolid
        PopupMenu = pmInstruments
        TabOrder = 0
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toFullRowSelect, toMultiSelect, toAlwaysSelectNode]
        OnCompareNodes = vstInstrumentsCompareNodes
        OnDragAllowed = vstInstrumentsDragAllowed
        OnDragOver = vstInstrumentsDragOver
        OnDragDrop = vstInstrumentsDragDrop
        OnDrawText = vstInstrumentsDrawText
        OnFreeNode = vstInstrumentsFreeNode
        OnGetText = vstInstrumentsGetText
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 0
            Text = 'Broker'
            Width = 80
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 1
            Text = 'Id'
            Width = 70
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 2
            Text = 'IsIn'
            Width = 90
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 3
            Text = 'Name'
            Width = 110
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 4
            Text = 'Symbol'
            Width = 55
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 5
            Text = 'Local Symbol'
            Width = 74
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 6
            Text = 'Type'
            Width = 55
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 7
            Text = 'Group'
            Width = 55
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 8
            Text = 'Currency'
            Width = 60
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 9
            Text = 'Tradables'
            Width = 90
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 10
            Text = 'Sector'
            Width = 60
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 11
            Text = 'Exchange'
            Width = 70
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 12
            Text = 'Primary Exchange'
            Width = 70
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 13
            Text = 'Market'
            Width = 45
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 14
            Text = 'Expiry'
            Width = 90
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 15
            Text = 'Last Price'
            Width = 100
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 16
            Text = 'Error Code'
            Width = 70
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 17
            Text = 'Error Msg'
            Width = 100
          end>
      end
      object pnlInstrumentsTop: TPanel
        Left = 0
        Top = 0
        Width = 611
        Height = 36
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          611
          36)
        object btnExportToExcel: TBitBtn
          Left = 571
          Top = 0
          Width = 36
          Height = 36
          ParentCustomHint = False
          Action = aInstExportToExcel
          Anchors = [akTop, akRight]
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object btnInstExportToCSV: TBitBtn
          Left = 535
          Top = 0
          Width = 36
          Height = 36
          ParentCustomHint = False
          Action = aInstExportToCSV
          Anchors = [akTop, akRight]
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object btnInstPrint: TBitBtn
          Left = 499
          Top = 0
          Width = 36
          Height = 36
          ParentCustomHint = False
          Action = aInstPrint
          Anchors = [akTop, akRight]
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object btnColumnSettings: TBitBtn
          Left = 463
          Top = 0
          Width = 36
          Height = 36
          Action = aColumnSettings
          Anchors = [akTop, akRight]
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object btnEditInstruments: TBitBtn
          Left = 38
          Top = 0
          Width = 36
          Height = 36
          ParentCustomHint = False
          Action = aEditInstruments
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
        end
        object btnShowContractInspector: TBitBtn
          Left = 74
          Top = 0
          Width = 36
          Height = 36
          Action = aShowContractInspector
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
        end
        object btnClear: TBitBtn
          Left = 2
          Top = 0
          Width = 36
          Height = 36
          Action = aClear
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
        end
      end
      object pnlRealtimeFeeds: TGroupBox
        Left = 0
        Top = 312
        Width = 611
        Height = 112
        Align = alClient
        Caption = 'Realtime Feeds'
        Color = clWhite
        ParentBackground = False
        ParentColor = False
        TabOrder = 2
        inline frameRealtimeFeeds: TframeRealtimeFeeds
          Left = 2
          Top = 15
          Width = 607
          Height = 95
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          ExplicitLeft = 2
          ExplicitTop = 15
          ExplicitWidth = 607
          ExplicitHeight = 95
          inherited vstTree: TVirtualStringTree
            Width = 607
            Height = 58
            Header.Height = 21
            ExplicitWidth = 607
            ExplicitHeight = 58
          end
          inherited pnlOptions: TPanel
            Width = 607
            ExplicitWidth = 607
            inherited lblShowFrom: TLabel
              Width = 110
              Height = 13
              ExplicitWidth = 110
              ExplicitHeight = 13
            end
            inherited lblShowTo: TLabel
              Height = 13
              ExplicitHeight = 13
            end
            inherited btnExportToExcel: TBitBtn
              Left = 570
              ExplicitLeft = 570
            end
            inherited btnExportToCSV: TBitBtn
              Left = 534
              ExplicitLeft = 534
            end
            inherited btnPrint: TBitBtn
              Left = 498
              ExplicitLeft = 498
            end
            inherited btnColumnSettings: TBitBtn
              Left = 462
              ExplicitLeft = 462
            end
          end
        end
      end
    end
    object pnlCategory: TPanel
      Left = 616
      Top = 243
      Width = 322
      Height = 424
      Align = alRight
      TabOrder = 2
      object pnlCategoryTop: TPanel
        Left = 1
        Top = 1
        Width = 320
        Height = 59
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          320
          59)
        object lblSearchLists: TLabel
          Left = 4
          Top = 40
          Width = 73
          Height = 13
          Caption = 'Filter By Name:'
        end
        object btnColumnSettingsLists: TBitBtn
          Left = 283
          Top = -1
          Width = 36
          Height = 36
          Action = aColumnSettingsLists
          Anchors = [akTop, akRight]
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object btnEditList: TBitBtn
          Left = 36
          Top = -1
          Width = 36
          Height = 36
          ParentCustomHint = False
          Action = aEditList
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object btnNewList: TBitBtn
          Left = 0
          Top = -1
          Width = 36
          Height = 36
          ParentCustomHint = False
          Action = aNewList
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object btnDeleteList: TBitBtn
          Left = 72
          Top = -1
          Width = 36
          Height = 36
          ParentCustomHint = False
          Action = aDeleteList
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object btnClearSearchText: TBitBtn
          Left = 297
          Top = 35
          Width = 22
          Height = 22
          Anchors = [akTop, akRight]
          ImageIndex = 43
          ImageName = 'RemovePivotField_32x32'
          Images = DMImage.vil16
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = btnClearSearchTextClick
        end
        object edtSearchLists: TEdit
          AlignWithMargins = True
          Left = 79
          Top = 36
          Width = 217
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 5
          OnChange = edtSearchListsChange
        end
        object btnSaveAsNewList: TBitBtn
          Left = 108
          Top = -1
          Width = 36
          Height = 36
          ParentCustomHint = False
          Action = aSaveAsNewList
          Images = DMImage.vil32
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
        end
      end
      object vstSokidLists: TVirtualStringTree
        Left = 1
        Top = 60
        Width = 320
        Height = 363
        Align = alClient
        DragMode = dmAutomatic
        DragOperations = [doCopy, doMove, doLink]
        DragType = dtVCL
        Header.AutoSizeIndex = 0
        Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoVisible]
        IncrementalSearch = isVisibleOnly
        IncrementalSearchStart = ssAlwaysStartOver
        IncrementalSearchTimeout = 1500
        LineStyle = lsSolid
        TabOrder = 0
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toMultiSelect]
        OnCompareNodes = vstSokidListsCompareNodes
        OnDblClick = aEditListExecute
        OnDragAllowed = vstInstrumentsDragAllowed
        OnDragOver = vstInstrumentsDragOver
        OnDragDrop = vstSokidListsDragDrop
        OnFreeNode = vstInstrumentsFreeNode
        OnGetText = vstSokidListsGetText
        OnPaintText = vstSokidListsPaintText
        OnIncrementalSearch = vstSokidListsIncrementalSearch
        OnNodeMoved = vstSokidListsNodeMoved
        OnNodeMoving = vstSokidListsNodeMoving
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 0
            Text = 'Name'
            Width = 225
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 1
            Text = 'Symbol'
            Width = 100
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 2
            Text = 'Local Symbol'
            Width = 100
          end
          item
            CaptionAlignment = taCenter
            DefaultSortDirection = sdDescending
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 3
            Text = 'ConId'
            Width = 80
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 4
            Text = 'Currency'
            Width = 80
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 5
            Text = 'Exchange'
            Width = 80
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 6
            Text = 'Security Type '
            Width = 80
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 7
            Text = 'Broker'
            Width = 80
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 8
            Text = 'Expiry'
            Width = 80
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 9
            Text = 'Underlying ConId'
            Width = 80
          end
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
            Position = 10
            Text = 'Description'
            Width = 20
          end>
      end
    end
  end
  object pmSearchParam: TPopupMenu
    Left = 566
    Top = 162
    object miSaveSearchConfiguration: TMenuItem
      Caption = 'Save search configuration'
      OnClick = miSaveSearchConfigurationClick
    end
    object miOpenSearchConfiguration: TMenuItem
      Caption = 'Open search configuration'
      OnClick = miOpenSearchConfigurationClick
    end
  end
  object pmInstruments: TPopupMenu
    OnPopup = pmInstrumentsPopup
    Left = 564
    Top = 104
    object miClearResultList: TMenuItem
      Action = aInstrumentsClear
    end
    object miMoveToMonitor: TMenuItem
      Action = aInstrumentsMoveToMonitor
    end
    object miDeleteItem: TMenuItem
      Action = aInstrumentsDeleteItem
    end
    object miOpenResultInANewWindow: TMenuItem
      Action = aOpenResultInANewWindow
    end
    object miAddInstrumentToDB: TMenuItem
      Action = aAddInstrumentToDB
    end
    object miSeparator01: TMenuItem
      Caption = '-'
    end
    object miSearchChain: TMenuItem
      Caption = 'Search Chain'
      object miSearchFuture: TMenuItem
        Action = aSearchFuture
      end
      object miSearchOption: TMenuItem
        Action = aSearchOption
      end
    end
  end
  object sdInstruments: TSaveDialog
    DefaultExt = '.csv'
    Filter = '*.csv|*.csv'
    Left = 568
    Top = 224
  end
  object ActionList: TActionList
    Images = DMImage.vil32
    Left = 435
    Top = 104
    object aSearch: TAction
      Category = 'Search'
      Caption = 'Search and Show Result in Treeview'
      ImageIndex = 21
      ImageName = 'Zoom_32x32'
      OnExecute = aSearchExecute
    end
    object aInstPrint: TAction
      Category = 'Search'
      Hint = 'Print'
      ImageIndex = 15
      ImageName = 'Print_32x32'
      OnExecute = aInstPrintExecute
    end
    object aInstExportToCSV: TAction
      Category = 'Search'
      Hint = 'Export To CSV'
      ImageIndex = 17
      ImageName = 'ExportToCSV_32x32'
      OnExecute = aInstExportToCSVExecute
    end
    object aInstExportToExcel: TAction
      Category = 'Search'
      Hint = 'Export To Excel'
      ImageIndex = 16
      ImageName = 'ExportToXLS_32x32'
      OnExecute = aInstExportToExcelExecute
    end
    object aAddInstrumentToDB: TAction
      Category = 'Search'
      Caption = 'Add Instrument To DB'
      OnExecute = aAddInstrumentToDBExecute
    end
    object aInstrumentsClear: TAction
      Category = 'Search'
      Caption = 'Clear Result List'
      OnExecute = aInstrumentsClearExecute
    end
    object aInstrumentsDeleteItem: TAction
      Category = 'Search'
      Caption = 'Delete Item'
      OnExecute = aInstrumentsDeleteItemExecute
    end
    object aInstrumentsMoveToMonitor: TAction
      Category = 'Search'
      Caption = 'Move To Monitor/Order List'
      OnExecute = aInstrumentsMoveToMonitorExecute
      OnUpdate = aInstrumentsMoveToMonitorUpdate
    end
    object aOpenResultInANewWindow: TAction
      Category = 'Search'
      Caption = 'Open Result In a New Window'
      OnExecute = aOpenResultInANewWindowExecute
    end
    object aSearchFuture: TAction
      Category = 'Search'
      Caption = 'Future'
      OnExecute = aSearchFutureExecute
    end
    object aSearchOption: TAction
      Category = 'Search'
      Caption = 'Option'
      OnExecute = aSearchOptionExecute
    end
    object aColumnSettings: TAction
      Category = 'Search'
      Hint = 'Column Settings'
      ImageIndex = 36
      ImageName = 'ListBullets_32x32'
      OnExecute = aColumnSettingsExecute
    end
    object aColumnSettingsLists: TAction
      Category = 'Lists'
      Hint = 'Column Settings '
      ImageIndex = 36
      ImageName = 'ListBullets_32x32'
      OnExecute = aColumnSettingsListsExecute
    end
    object aNewList: TAction
      Category = 'Lists'
      Hint = 'New List'
      ImageIndex = 52
      ImageName = 'AddItem_32x32'
      OnExecute = aNewListExecute
    end
    object aEditList: TAction
      Category = 'Lists'
      Hint = 'Edit'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
      OnExecute = aEditListExecute
    end
    object aDeleteList: TAction
      Category = 'Lists'
      Hint = 'Delete'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aDeleteListExecute
    end
    object aEditInstruments: TAction
      Category = 'Search'
      Hint = 'Edit Instrument'
      ImageIndex = 1
      ImageName = 'Edit_32x32'
      OnExecute = aEditInstrumentsExecute
    end
    object aSaveAsNewList: TAction
      Category = 'Lists'
      Hint = 'Save As New List'
      ImageIndex = 26
      ImageName = 'SaveAll_32x32'
      OnExecute = aSaveAsNewListExecute
    end
    object aShowContractInspector: TAction
      Category = 'Search'
      Hint = 'Show Contract Inspector'
      ImageIndex = 22
      ImageName = 'ContractInspector'
      OnExecute = aShowContractInspectorExecute
    end
    object aClear: TAction
      Category = 'Search'
      Hint = 'Clear All'
      ImageIndex = 65
      ImageName = 'DeleteList2_32x32'
      OnExecute = aClearExecute
    end
  end
end
