object frmInstrument: TfrmInstrument
  Left = 0
  Top = 0
  Width = 693
  Height = 381
  ParentBackground = False
  TabOrder = 0
  TabStop = True
  object splitMethods: TSplitter
    Left = 266
    Top = 0
    Height = 381
  end
  object pnlProcs: TPanel
    Left = 269
    Top = 0
    Width = 424
    Height = 381
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlProcedures'
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object TPaintBox
      Left = 0
      Top = 0
      Width = 424
      Height = 24
      Align = alTop
    end
    object lblProcs: TRotateLabel
      Left = 2
      Top = 4
      Width = 41
      Height = 14
      Escapement = 0
      TextStyle = tsNone
      Caption = 'Methods'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object vtMethods: TVirtualStringTree
      Left = 0
      Top = 24
      Width = 424
      Height = 357
      Align = alClient
      CheckImageKind = ckDarkTick
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      HintAnimation = hatNone
      NodeDataSize = 8
      ScrollBarOptions.ScrollBars = ssVertical
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      OnChecked = vtMethodsChecked
      OnGetText = vtMethodsGetText
      Columns = <>
    end
  end
  object pnlClasses: TPanel
    Left = 129
    Top = 0
    Width = 137
    Height = 381
    Align = alLeft
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 1
    object TPaintBox
      Left = 0
      Top = 0
      Width = 137
      Height = 24
      Align = alTop
    end
    object lblClasses: TRotateLabel
      Left = 3
      Top = 4
      Width = 39
      Height = 14
      Escapement = 0
      TextStyle = tsNone
      Caption = 'Classes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object vtClasses: TVirtualStringTree
      Left = 0
      Top = 24
      Width = 137
      Height = 357
      Align = alClient
      CheckImageKind = ckDarkTick
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      HintAnimation = hatNone
      NodeDataSize = 8
      ParentShowHint = False
      ScrollBarOptions.ScrollBars = ssVertical
      ShowHint = False
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
      OnChange = vtClassesChange
      OnChecked = vtClassesChecked
      OnGetText = vtClassesGetText
      Columns = <>
    end
  end
  object pnlUnits: TPanel
    Left = 0
    Top = 0
    Width = 129
    Height = 381
    Align = alLeft
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 2
    object pbGradientUnits: TPaintBox
      Left = 0
      Top = 0
      Width = 129
      Height = 24
      Align = alTop
    end
    object lblUnits: TRotateLabel
      Left = 4
      Top = 4
      Width = 24
      Height = 14
      Escapement = 0
      TextStyle = tsNone
      Caption = 'Units'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object vtUnits: TVirtualStringTree
      Left = 0
      Top = 24
      Width = 129
      Height = 357
      Align = alClient
      CheckImageKind = ckSystem
      Header.AutoSizeIndex = -1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      HintAnimation = hatNone
      NodeDataSize = 8
      ScrollBarOptions.ScrollBars = ssVertical
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages]
      OnChange = vtUnitsChange
      OnChecked = vtUnitsChecked
      OnGetText = vtUnitsGetText
      Columns = <>
    end
  end
end
