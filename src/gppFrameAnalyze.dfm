object frmAnalyze: TfrmAnalyze
  Left = 0
  Top = 0
  Width = 647
  Height = 488
  TabOrder = 0
  TabStop = True
  OnResize = FrameResize
  object pbGradientTop: TPaintBox
    Left = 0
    Top = 0
    Width = 647
    Height = 24
    Align = alTop
    ExplicitLeft = -90
    ExplicitWidth = 737
  end
  object lblViewCaption: TRotateLabel
    Left = 282
    Top = 4
    Width = 81
    Height = 14
    Escapement = 0
    TextStyle = tsNone
    Alignment = taCenter
    Caption = '<view caption>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
end
