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
    => (EnhancedSliderCfg sp ep a)
    -> a
    -> a
    -> EventHandler a (EnhancedSliderEvent a) sp ep
handleEvent config a b _ _ _ event = case event of
    EventSetField value -> setFieldHandle config a b value

setFieldHandle
    :: (SliderValue a)
    => (EnhancedSliderCfg sp ep a)
    -> a
    -> a
    -> a
    -> [EventResponse a (EnhancedSliderEvent a) sp ep]
setFieldHandle config a b value = [Model newValue] <> report where
    report = RequestParent <$> (($ newValue) <$> req)
    newValue = min b $ max a value
    req = _escOnChangeReq config
