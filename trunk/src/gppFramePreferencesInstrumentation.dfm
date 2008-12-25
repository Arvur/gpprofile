object frmPreferencesInstrumentation: TfrmPreferencesInstrumentation
  Left = 0
  Top = 0
  Width = 473
  Height = 240
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentBackground = False
  ParentFont = False
  TabOrder = 0
  TabStop = True
  object pnlInstrumentation: TPanel
    Left = 0
    Top = 0
    Width = 473
    Height = 240
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      473
      240)
    object bvlSettings: TBevel
      Left = 50
      Top = 14
      Width = 415
      Height = 14
      Anchors = [akLeft, akTop, akRight]
      Shape = bsTopLine
      Style = bsRaised
    end
    object lblMarkerStyle: TLabel
      Left = 16
      Top = 34
      Width = 63
      Height = 13
      Margins.Bottom = 0
      Caption = '&Marker style:'
      FocusControl = cbxMarker
    end
    object lblSettings: TLabel
      Tag = 1
      Left = 5
      Top = 8
      Width = 39
      Height = 13
      Margins.Bottom = 0
      Caption = 'Settings'
    end
    object TLabel
      Tag = 1
      Left = 5
      Top = 67
      Width = 43
      Height = 13
      Margins.Bottom = 0
      Caption = 'Browsing'
    end
    object TBevel
      Left = 54
      Top = 74
      Width = 411
      Height = 14
      Anchors = [akLeft, akTop, akRight]
      Shape = bsTopLine
      Style = bsRaised
    end
    object TBevel
      Left = 84
      Top = 130
      Width = 381
      Height = 14
      Anchors = [akLeft, akTop, akRight]
      Shape = bsTopLine
      Style = bsRaised
    end
    object TLabel
      Tag = 1
      Left = 6
      Top = 123
      Width = 77
      Height = 13
      Margins.Bottom = 0
      Caption = 'Instrumentation'
    end
    object cbxMarker: TComboBox
      Left = 104
      Top = 30
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbxMarkerChange
      Items.Strings = (
        '{>>GpProfile}'
        '{$IFDEF GpProfile}')
    end
    object cbProfilingAutostart: TCheckBox
      Left = 16
      Top = 142
      Width = 257
      Height = 17
      Caption = 'Start &profiling on target startup'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbProfilingAutostartClick
    end
    object cbShowAllFolders: TCheckBox
      Left = 16
      Top = 86
      Width = 257
      Height = 17
      Caption = 'Show &all folders'
      TabOrder = 1
      OnClick = cbShowAllFoldersClick
    end
    object cbInstrumentAssembler: TCheckBox
      Left = 16
      Top = 165
      Width = 257
      Height = 17
      Caption = '&Instrument pure assembler procedures'
      TabOrder = 3
      OnClick = cbInstrumentAssemblerClick
    end
    object cbKeepFileDateUnchanged: TCheckBox
      Left = 16
      Top = 188
      Width = 257
      Height = 17
      Caption = 'Keep file date/time unchanged'
      TabOrder = 4
      OnClick = cbKeepFileDateUnchangedClick
    end
  end
end
