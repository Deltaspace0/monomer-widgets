module Monomer.EnhancedSlider.UI
    ( buildUI
    , makeTitle
    ) where

import Control.Applicative ((<|>))
import Data.Maybe
import Data.Text (Text)
import Monomer.Core.Combinators
import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Single
import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Slider
import qualified Data.Text as T

import Monomer.EnhancedSlider.EnhancedSliderCfg
import Monomer.EnhancedSlider.EnhancedSliderEvent

buildUI
    :: (SliderValue a)
    => (EnhancedSliderCfg s e a)
    -> a
    -> a
    -> UIBuilder a (EnhancedSliderEvent a)
buildUI config a b _ model = tree where
    tree = if labelVisible
        then vstack_ [childSpacing_ 16]
            [ box_ [alignLeft] $ label $ makeTitle config model
            , box_ [alignLeft] mainStack
            ]
        else mainStack
    labelVisible = not $ fromMaybe False $ _escHideLabel config
    mainStack = if null (_escAlignV config)
        then hstack_ [childSpacing_ 32] arrangementH
        else vstack_ [childSpacing_ 24] arrangementV
    arrangementH = case fromMaybe ALeft (_escAlignH config) of
        ALeft -> [hsliderWidget, minusButton, plusButton]
        ACenter -> [minusButton, hsliderWidget, plusButton]
        ARight -> [minusButton, plusButton, hsliderWidget]
    arrangementV = case fromMaybe ABottom (_escAlignV config) of
        ATop -> [vsliderWidget, plusButton, minusButton]
        AMiddle -> [plusButton, vsliderWidget, minusButton]
        ABottom -> [plusButton, minusButton, vsliderWidget]
    hsliderWidget = hslider_ id a b sliderConfig
    vsliderWidget = vslider_ id a b sliderConfig
    minusButton = button' "-" $ EventSetField $ model-changeRate
    plusButton = button' "+" $ EventSetField $ model+changeRate
    sliderConfig =
        [ wheelRate 0
        , dragRate $ toRational changeRate
        , onChange EventSetField
        , onFocus EventFocus
        , onBlur EventBlur
        ]
    button' c e = button_ c e buttonConfig `styleBasic`
        [ width 32
        , height 24
        ]
    buttonConfig =
        [ onFocus EventFocus
        , onBlur EventBlur
        ]
    changeRate = fromFractional $ fromMaybe 1 $ _escDragRate config

makeTitle
    :: (SliderValue a)
    => (EnhancedSliderCfg s e a)
    -> a
    -> Text
makeTitle config value = fromMaybe showValue customTitle where
    customTitle = withMethod <|> titleValue
    withMethod = ($ value) <$> _escTitleMethod config
    titleValue = (<> ": " <> showValue) <$> _escTitle config
    showValue = T.pack $ show value
