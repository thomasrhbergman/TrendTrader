object FHostPort: TFHostPort
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Host and Port'
  ClientHeight = 182
  ClientWidth = 331
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelIPHost: TLabel
    Left = 19
    Top = 38
    Width = 129
    Height = 13
    Caption = 'IP or Host (select or enter)'
  end
  object LabelPort: TLabel
    Left = 180
    Top = 38
    Width = 112
    Height = 13
    Caption = 'Port # (select or enter)'
  end
  object Label1: TLabel
    Left = 19
    Top = 100
    Width = 291
    Height = 13
    Caption = 'Default port # for paper trades:  TWS 7497, Gateway 4002.'
  end
  object Label2: TLabel
    Left = 19
    Top = 119
    Width = 279
    Height = 13
    Caption = 'Default port # for live trades:  TWS 7496, Gateway 4001.'
  end
  object Label3: TLabel
    Left = 19
    Top = 8
    Width = 237
    Height = 13
    Caption = 'Set host address and port # of TWS or Gateway.'
  end
  object ComboBoxIPHost: TComboBox
    Left = 19
    Top = 57
    Width = 145
    Height = 21
    ItemIndex = 0
    TabOrder = 0
    Text = 'localhost'
    Items.Strings = (
      'localhost'
      '127.0.0.1'
      '::1'
      'www.something.com'
      'fe80::e0d:babe:24ea:3')
  end
  object ComboBoxPort: TComboBox
    Left = 198
    Top = 57
    Width = 77
    Height = 21
    ItemIndex = 3
    TabOrder = 1
    Text = '7497'
    Items.Strings = (
      '4001'
      '4002'
      '7496'
      '7497')
  end
  object ButtonOK: TButton
    Left = 169
    Top = 149
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 73
    Top = 149
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
