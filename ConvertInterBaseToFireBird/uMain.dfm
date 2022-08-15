object Main: TMain
  Left = 0
  Top = 0
  Caption = 'Copy data'
  ClientHeight = 324
  ClientWidth = 767
  Color = clBtnFace
  Constraints.MaxHeight = 463
  Constraints.MaxWidth = 783
  Constraints.MinHeight = 363
  Constraints.MinWidth = 783
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI Semibold'
  Font.Style = [fsBold]
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 20
  object labIB: TLabel
    Left = 24
    Top = 8
    Width = 80
    Height = 20
    Caption = 'IB Database'
  end
  object labFB: TLabel
    Left = 23
    Top = 144
    Width = 92
    Height = 20
    Caption = 'FB3 Database'
  end
  object labTable: TLabel
    Left = 24
    Top = 288
    Width = 4
    Height = 20
  end
  object labFBServer: TLabel
    Left = 23
    Top = 173
    Width = 45
    Height = 20
    Caption = 'Server'
  end
  object labPort: TLabel
    Left = 23
    Top = 207
    Width = 28
    Height = 20
    Caption = 'Port'
  end
  object labFBFile: TLabel
    Left = 24
    Top = 241
    Width = 24
    Height = 20
    Caption = 'File'
  end
  object labIBServer: TLabel
    Left = 23
    Top = 37
    Width = 45
    Height = 20
    Caption = 'Server'
  end
  object labIBPort: TLabel
    Left = 24
    Top = 71
    Width = 28
    Height = 20
    Caption = 'Port'
  end
  object labIBFile: TLabel
    Left = 24
    Top = 105
    Width = 24
    Height = 20
    Caption = 'File'
  end
  object labIBUser: TLabel
    Left = 375
    Top = 37
    Width = 31
    Height = 20
    Caption = 'User'
  end
  object labIBPassword: TLabel
    Left = 375
    Top = 71
    Width = 64
    Height = 20
    Caption = 'Password'
  end
  object labFBUser: TLabel
    Left = 375
    Top = 173
    Width = 31
    Height = 20
    Caption = 'User'
  end
  object labFBPassword: TLabel
    Left = 376
    Top = 207
    Width = 64
    Height = 20
    Caption = 'Password'
  end
  object btnDest: TSpeedButton
    Left = 732
    Top = 238
    Width = 24
    Height = 28
    Caption = '..'
    OnClick = btnDestClick
  end
  object btnSource: TSpeedButton
    Left = 732
    Top = 102
    Width = 24
    Height = 28
    Caption = '..'
    OnClick = btnSourceClick
  end
  object edIB: TEdit
    Left = 83
    Top = 102
    Width = 646
    Height = 28
    TabOrder = 0
  end
  object edFB: TEdit
    Left = 83
    Top = 238
    Width = 646
    Height = 28
    TabOrder = 1
  end
  object btnCopy: TButton
    Left = 625
    Top = 272
    Width = 131
    Height = 33
    Caption = 'Convert'
    TabOrder = 2
    OnClick = btnCopyClick
  end
  object edFBServer: TEdit
    Left = 83
    Top = 170
    Width = 209
    Height = 28
    TabOrder = 3
    Text = 'localhost'
  end
  object edFBPort: TEdit
    Left = 83
    Top = 204
    Width = 70
    Height = 28
    NumbersOnly = True
    TabOrder = 4
    Text = '3051'
  end
  object edIBPort: TEdit
    Left = 83
    Top = 68
    Width = 70
    Height = 28
    NumbersOnly = True
    TabOrder = 5
    Text = '3050'
  end
  object edIBServer: TEdit
    Left = 83
    Top = 34
    Width = 209
    Height = 28
    TabOrder = 6
    Text = 'localhost'
  end
  object edIBUser: TEdit
    Left = 459
    Top = 34
    Width = 126
    Height = 28
    TabOrder = 7
    Text = 'SYSDBA'
  end
  object edIBPassword: TEdit
    Left = 459
    Top = 68
    Width = 126
    Height = 28
    PasswordChar = '*'
    TabOrder = 8
    Text = 'masterkey'
  end
  object edFBUser: TEdit
    Left = 459
    Top = 170
    Width = 126
    Height = 28
    TabOrder = 9
    Text = 'SYSDBA'
  end
  object edFBPassword: TEdit
    Left = 459
    Top = 204
    Width = 126
    Height = 28
    PasswordChar = '*'
    TabOrder = 10
    Text = 'masterkey'
  end
  object OpenIBDialog: TOpenDialog
    Filter = 'InterBase Files (*.IB)|*.IB|All Files (*.*)|*.*'
    Left = 632
    Top = 16
  end
  object OpenFBDialog: TOpenDialog
    Left = 632
    Top = 152
  end
end
