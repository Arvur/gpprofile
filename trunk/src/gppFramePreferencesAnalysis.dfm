object frmPreferencesAnalysis: TfrmPreferencesAnalysis
  Left = 0
  Top = 0
  Width = 468
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
  object pnlAnalysis: TPanel
    Left = 0
    Top = 0
    Width = 468
    Height = 240
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      468
      240)
    object lblOptions: TLabel
      Tag = 1
      Left = 5
      Top = 8
      Width = 37
      Height = 13
      Caption = 'Options'
    end
    object bvlOptions: TBevel
      Left = 50
      Top = 14
      Width = 415
      Height = 14
      Anchors = [akLeft, akTop, akRight]
      Shape = bsTopLine
      Style = bsRaised
    end
    object cbHideNotExecuted: TCheckBox
      Left = 16
      Top = 34
      Width = 241
      Height = 17
      Caption = ' &Hide methods that were never executed'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbHideNotExecutedClick
    end
  end
end
