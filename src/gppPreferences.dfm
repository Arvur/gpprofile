object frmPreferences: TfrmPreferences
  Left = 374
  Top = 176
  BorderStyle = bsDialog
  Caption = 'GpProfile - Preferences'
  ClientHeight = 370
  ClientWidth = 605
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlFrameHolder: TPanel
    AlignWithMargins = True
    Left = 129
    Top = 0
    Width = 476
    Height = 314
    Margins.Left = 6
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    Color = clWindow
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 2
    object TPanel
      Left = 1
      Top = 264
      Width = 472
      Height = 47
      Align = alBottom
      BevelOuter = bvNone
      Color = clWindow
      ParentBackground = False
      TabOrder = 0
      object bvlFrameUnderline: TBevel
        AlignWithMargins = True
        Left = 6
        Top = 0
        Width = 460
        Height = 17
        Margins.Left = 6
        Margins.Top = 0
        Margins.Right = 6
        Align = alTop
        Shape = bsTopLine
        Style = bsRaised
        ExplicitLeft = 8
        ExplicitWidth = 409
      end
      object btnDefinesDefaults: TButton
        Left = 328
        Top = 11
        Width = 131
        Height = 25
        Caption = 'Revert to defaults'
        TabOrder = 0
      end
    end
  end
  object TPanel
    Left = 0
    Top = 314
    Width = 605
    Height = 56
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object oxButton1: TButton
      Left = 415
      Top = 14
      Width = 77
      Height = 27
      Caption = 'OK'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
    object oxButton2: TButton
      Left = 502
      Top = 14
      Width = 77
      Height = 27
      Cancel = True
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ModalResult = 2
      ParentFont = False
      TabOrder = 1
    end
  end
  object TPanel
    Left = 0
    Top = 0
    Width = 123
    Height = 314
    Align = alLeft
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clWindow
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 1
    object sbInstrumentation: TSpeedButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 115
      Height = 22
      Align = alTop
      BiDiMode = bdRightToLeftReadingOnly
      GroupIndex = 1
      Down = True
      Caption = 'Instrumentation'
      Flat = True
      ParentBiDiMode = False
      OnClick = DisplaySettingsPage
      ExplicitLeft = 2
    end
    object sbAnalysis: TSpeedButton
      AlignWithMargins = True
      Left = 3
      Top = 87
      Width = 115
      Height = 22
      Align = alTop
      GroupIndex = 1
      Caption = 'Analysis'
      Flat = True
      OnClick = DisplaySettingsPage
      ExplicitLeft = 19
      ExplicitTop = 127
    end
    object sbExcludedUnits: TSpeedButton
      AlignWithMargins = True
      Left = 3
      Top = 59
      Width = 115
      Height = 22
      Align = alTop
      GroupIndex = 1
      Caption = 'Excluded units'
      Flat = True
      OnClick = DisplaySettingsPage
      ExplicitLeft = 5
      ExplicitTop = 87
    end
    object sbConditionalDefines: TSpeedButton
      AlignWithMargins = True
      Left = 3
      Top = 31
      Width = 115
      Height = 22
      Align = alTop
      GroupIndex = 1
      Caption = 'Conditional defines'
      Flat = True
      OnClick = DisplaySettingsPage
      ExplicitLeft = 2
      ExplicitTop = 3
    end
  end
end
