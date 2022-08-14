inherited frmDockFormAccountPnL: TfrmDockFormAccountPnL
  Caption = 'PnL for account'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  
  TextHeight = 13
  inherited pnlOptions: TPanel
    object btnClear: TBitBtn
      Left = 2
      Top = 1
      Width = 36
      Height = 36
      Action = aClear
      Images = DMImage.vil32
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
  end
  inherited pnlMain: TPanel
    inherited vstTree: TVirtualStringTree
      Header.Height = 21
      Header.MainColumn = 0
      Header.SortColumn = 3
      Header.SortDirection = sdDescending
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
      OnCompareNodes = vstTreeCompareNodes
      OnDrawText = vstTreeDrawText
      OnFreeNode = vstTreeFreeNode
      OnGetText = vstTreeGetText
      Columns = <
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 0
          Text = 'Broker'
          Width = 164
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 1
          Text = 'DailyPnL'
          Width = 140
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 2
          Text = 'UnrealizedPnL'
          Width = 140
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          DefaultSortDirection = sdDescending
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 3
          Text = 'Time'
          Width = 140
        end>
    end
  end
  inherited ActionList: TActionList
    object aClear: TAction
      Hint = 'Clear'
      ImageIndex = 0
      ImageName = 'DeleteList_32x32'
      OnExecute = aClearExecute
    end
  end
end
