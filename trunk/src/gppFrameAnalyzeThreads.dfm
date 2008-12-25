object frmAnalyzeThreads: TfrmAnalyzeThreads
  Left = 0
  Top = 0
  Width = 826
  Height = 344
  TabOrder = 0
  TabStop = True
  object lvThreads: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 826
    Height = 344
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.SortDirection = sdDescending
    Header.Style = hsFlatButtons
    HintAnimation = hatNone
    NodeDataSize = 4
    TabOrder = 0
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    Columns = <
      item
        Position = 0
        Width = 275
        WideText = 'Thread'
      end
      item
        Alignment = taRightJustify
        Position = 1
        Width = 75
        WideText = '% Time'
      end
      item
        Position = 2
        WideText = 'Time'
      end
      item
        Position = 3
        WideText = 'Calls'
      end>
  end
end
