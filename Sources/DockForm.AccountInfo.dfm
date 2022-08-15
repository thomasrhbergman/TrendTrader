inherited frmDockFormAccountInfo: TfrmDockFormAccountInfo
  Caption = 'Account'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  
  TextHeight = 13
  inherited pnlMain: TPanel
    inherited vstTree: TVirtualStringTree
      Alignment = taRightJustify
      Header.MainColumn = 0
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
          Text = 'Number'
          Width = 80
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 1
          Text = 'Name'
          Width = 216
        end
        item
          Alignment = taRightJustify
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 2
          Text = 'Value'
          Width = 164
        end
        item
          CaptionAlignment = taCenter
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment, coEditable, coStyleColor]
          Position = 3
          Text = 'Info / Currency'
          Width = 190
        end>
    end
  end
end
