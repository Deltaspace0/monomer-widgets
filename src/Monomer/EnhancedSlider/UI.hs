module Monomer.EnhancedSlider.UI
    ( buildUI
    ) where

import Data.Maybe
import Monomer.Core.Combinators
import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Stack
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
    tree = vstack_ [childSpacing_ 16]
        [ label $ fromMaybe showValue titleValue
        , hstack_ [childSpacing_ 32]
            [ hslider_ id a b sliderConfig
            , button' "-" $ EventSetField $ model-changeRate
            , button' "+" $ EventSetField $ model+changeRate
            ]
        ]
    showValue = T.pack $ show model
    titleValue = (<> ": " <> showValue) <$> _escTitle config
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
