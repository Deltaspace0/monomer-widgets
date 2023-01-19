module Monomer.EnhancedSlider.EnhancedSliderEvent
    ( EnhancedSliderEvent(..)
    , handleEvent
    ) where

import Monomer.Widgets.Composite
import Monomer.Widgets.Singles.Slider

import Monomer.EnhancedSlider.EnhancedSliderCfg

data EnhancedSliderEvent a
    = EventSetField a
    deriving Eq

handleEvent
    :: (SliderValue a)
    => (EnhancedSliderCfg s e a)
    -> a
    -> a
    -> EventHandler a (EnhancedSliderEvent a) sp ep
handleEvent _ a b _ _ _ event = case event of
    EventSetField value -> setFieldHandle a b value

setFieldHandle
    :: (SliderValue a)
    => a
    -> a
    -> a
    -> [EventResponse a (EnhancedSliderEvent a) sp ep]
setFieldHandle a b value = [Model $ min b $ max a value]
