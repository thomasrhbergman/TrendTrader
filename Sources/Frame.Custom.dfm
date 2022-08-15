object frameCustom: TframeCustom
  Left = 0
  Top = 0
  Width = 918
  Height = 370
  TabOrder = 0
  object vstTree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 918
    Height = 370
    Align = alClient
    Header.AutoSizeIndex = -1
    Header.Height = 18
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Images = DMImage.ilDocumentState
    LineStyle = lsSolid
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toDisableDrawSelection, toFullRowSelect, toMultiSelect, toAlwaysSelectNode]
    OnColumnResize = vstTreeColumnResize
    OnFreeNode = vstTreeFreeNode
    OnHeaderDragged = vstTreeHeaderDragged
    Columns = <>
  end
end
