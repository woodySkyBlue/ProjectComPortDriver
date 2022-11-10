object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Client'
  ClientHeight = 274
  ClientWidth = 507
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Meiryo UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 23
  object Memo1: TMemo
    Left = 0
    Top = 58
    Width = 507
    Height = 216
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitWidth = 497
    ExplicitHeight = 214
  end
  object ButtonSendData: TButton
    Left = 195
    Top = 10
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = #36865#20449
    TabOrder = 1
    OnClick = ButtonSendDataClick
  end
  object CheckBoxConnect: TCheckBox
    Left = 10
    Top = 16
    Width = 124
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Connect'
    TabOrder = 2
    OnClick = CheckBoxConnectClick
  end
  object CommPortDriver1: TCommPortDriver
    Port = pnCustom
    PortName = '\\.\COM5'
    OnReceiveData = CommPortDriver1ReceiveData
    OnReceivePacket = CommPortDriver1ReceivePacket
    Left = 84
    Top = 148
  end
  object OpenDialog1: TOpenDialog
    Left = 244
    Top = 148
  end
end
