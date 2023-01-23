module Monomer.EnhancedSlider
    ( EnhancedSliderCfg
    , enhancedSlider
    , enhancedSlider_
    ) where

import Control.Lens
import Data.Default
import Monomer.Widgets.Composite
import Monomer.Widgets.Singles.Slider

import Monomer.EnhancedSlider.EnhancedSliderCfg
import Monomer.EnhancedSlider.EnhancedSliderEvent
import Monomer.EnhancedSlider.UI

enhancedSlider
    :: (WidgetModel s, WidgetEvent e, SliderValue a)
    => ALens' s a
    -> a
    -> a
    -> WidgetNode s e
enhancedSlider field a b = enhancedSlider_ field a b def

enhancedSlider_
    :: (WidgetModel s, WidgetEvent e, SliderValue a)
    => ALens' s a
    -> a
    -> a
    -> [EnhancedSliderCfg s e a]
    -> WidgetNode s e
enhancedSlider_ field a b configs = node where
    node = composite "enhancedSlider" field uiBuilder eventHandler
    uiBuilder = buildUI config a b
    eventHandler = handleEvent config a b
    config = mconcat configs
