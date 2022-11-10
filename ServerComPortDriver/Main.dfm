object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Server'
  ClientHeight = 240
  ClientWidth = 371
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
  object MemoMessage: TMemo
    Left = 0
    Top = 58
    Width = 371
    Height = 182
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitWidth = 361
    ExplicitHeight = 180
  end
  object Button1: TButton
    Left = 10
    Top = 10
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Button1'
    TabOrder = 1
  end
  object CommPortDriver1: TCommPortDriver
    Port = pnCustom
    PortName = '\\.\COM5'
    OnReceiveData = CommPortDriver1ReceiveData
    Left = 84
    Top = 76
  end
end
