object DM: TDM
  Height = 159
  Width = 213
  PixelsPerInch = 96
  object FDConnection: TFDConnection
    Params.Strings = (
      'DriverID=FB'
      'Port=3051'
      'User_Name=sysdba')
    Left = 40
    Top = 16
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 128
    Top = 16
  end
  object IBDatabase: TIBDatabase
    LoginPrompt = False
    DefaultTransaction = IBTransaction
    ServerType = 'IBServer'
    Left = 40
    Top = 80
  end
  object IBTransaction: TIBTransaction
    DefaultDatabase = IBDatabase
    Left = 128
    Top = 80
  end
end
