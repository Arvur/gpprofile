object frmAnalyzeMethods: TfrmAnalyzeMethods
  Left = 0
  Top = 0
  Width = 627
  Height = 252
  TabOrder = 0
  TabStop = True
  object splitCallees: TSplitter
    Left = 0
    Top = 159
    Width = 627
    Height = 2
    Cursor = crVSplit
    Align = alBottom
    Color = clBtnFace
    ParentColor = False
    Visible = False
    ExplicitLeft = -109
    ExplicitTop = 180
    ExplicitWidth = 736
  end
  object pnlTopTwo: TPanel
    Left = 0
    Top = 0
    Width = 627
    Height = 159
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object splitCallers: TSplitter
      Left = 0
      Top = 82
      Width = 627
      Height = 2
      Cursor = crVSplit
      Align = alTop
      Color = clBtnFace
      ParentColor = False
      Visible = False
      ExplicitWidth = 596
    end
    object pnlCallers: TPanel
      Left = 0
      Top = 0
      Width = 627
      Height = 82
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      ParentBackground = False
      TabOrder = 0
      DesignSize = (
        627
        82)
      object pbSubmenuGradient1: TPaintBox
        Left = 0
        Top = 0
        Width = 24
        Height = 82
        Align = alLeft
        ExplicitLeft = -4
        ExplicitTop = -103
        ExplicitHeight = 185
      end
      object lblCallers: TRotateLabel
        Left = 1
        Top = 19
        Width = 14
        Height = 33
        Escapement = 90
        TextStyle = tsNone
        Caption = 'Callers'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lvCallers: TVirtualStringTree
        Tag = 79
        Left = 16
        Top = 1
        Width = 611
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Header.AutoSizeIndex = -1
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'MS Sans Serif'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
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
            Width = 96
            WideText = 'Procedure'
          end
          item
            Alignment = taRightJustify
            Position = 1
            Width = 70
            WideText = '% Time'
          end
          item
            Alignment = taRightJustify
            Position = 2
            Width = 70
            WideText = 'Time'
          end
          item
            Alignment = taRightJustify
            Position = 3
            Width = 70
            WideText = 'w/Child'
          end
          item
            Alignment = taRightJustify
            Position = 4
            Width = 70
            WideText = 'Calls'
          end
          item
            Alignment = taRightJustify
            Position = 5
            Width = 70
            WideText = 'Min/Call'
          end
          item
            Alignment = taRightJustify
            Position = 6
            Width = 70
            WideText = 'Max/Call'
          end
          item
            Alignment = taRightJustify
            Position = 7
            Width = 91
            WideText = 'Avg/Call'
          end>
      end
    end
    object pnlCurrent: TPanel
      Left = 0
      Top = 84
      Width = 627
      Height = 75
      Align = alClient
      BevelOuter = bvNone
      Color = clWindow
      ParentBackground = False
      TabOrder = 1
      DesignSize = (
        627
        75)
      object pbGradientMethods: TPaintBox
        Left = 0
        Top = 0
        Width = 24
        Height = 75
        Align = alLeft
        ExplicitLeft = -4
        ExplicitTop = -103
        ExplicitHeight = 185
      end
      object lvProcs: TVirtualStringTree
        Tag = 68
        Left = 16
        Top = 0
        Width = 611
        Height = 69
        Anchors = [akLeft, akTop, akRight]
        Header.AutoSizeIndex = -1
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'MS Sans Serif'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
        Header.SortDirection = sdDescending
        Header.Style = hsFlatButtons
        HintAnimation = hatNone
        NodeDataSize = 4
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect]
        Columns = <
          item
            Position = 0
            Width = 96
            WideText = 'Procedure'
          end
          item
            Alignment = taRightJustify
            Position = 1
            Width = 70
            WideText = '% Time'
          end
          item
            Alignment = taRightJustify
            Position = 2
            Width = 70
            WideText = 'Time'
          end
          item
            Alignment = taRightJustify
            Position = 3
            Width = 70
            WideText = 'w/Child'
          end
          item
            Alignment = taRightJustify
            Position = 4
            Width = 70
            WideText = 'Calls'
          end
          item
            Alignment = taRightJustify
            Position = 5
            Width = 70
            WideText = 'Min/Call'
          end
          item
            Alignment = taRightJustify
            Position = 6
            Width = 70
            WideText = 'Max/Call'
          end
          item
            Alignment = taRightJustify
            Position = 7
            Width = 91
            WideText = 'Avg/Call'
          end>
      end
    end
  end
  object pnlCallees: TPanel
    Left = 0
    Top = 161
    Width = 627
    Height = 91
    Align = alBottom
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      627
      91)
    object pbGradientCalled: TPaintBox
      Left = 0
      Top = 0
      Width = 24
      Height = 91
      Align = alLeft
      ExplicitLeft = -4
      ExplicitTop = -103
      ExplicitHeight = 185
    end
    object lblCalled: TRotateLabel
      Left = 1
      Top = 19
      Width = 14
      Height = 29
      Escapement = 90
      TextStyle = tsNone
      Caption = 'Called'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object lvCallees: TVirtualStringTree
      Tag = 89
      Left = 16
      Top = 1
      Width = 611
      Height = 90
      Anchors = [akLeft, akTop, akRight]
      Header.AutoSizeIndex = -1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
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
          Width = 96
          WideText = 'Procedure'
        end
        item
          Alignment = taRightJustify
          Position = 1
          Width = 70
          WideText = '% Time'
        end
        item
          Alignment = taRightJustify
          Position = 2
          Width = 70
          WideText = 'Time'
        end
        item
          Alignment = taRightJustify
          Position = 3
          Width = 70
          WideText = 'w/Child'
        end
        item
          Alignment = taRightJustify
          Position = 4
          Width = 70
          WideText = 'Calls'
        end
        item
          Alignment = taRightJustify
          Position = 5
          Width = 70
          WideText = 'Min/Call'
        end
        item
          Alignment = taRightJustify
          Position = 6
          Width = 70
          WideText = 'Max/Call'
        end
        item
          Alignment = taRightJustify
          Position = 7
          Width = 91
          WideText = 'Avg/Call'
        end>
    end
  end
end
