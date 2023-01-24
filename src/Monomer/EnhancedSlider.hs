module Monomer.EnhancedSlider
    ( EnhancedSliderCfg
    , enhancedSlider
    , enhancedSlider_
    , enhancedSliderV
    , enhancedSliderV_
    , enhancedSliderD_
    ) where

import Control.Lens
import Data.Default
import Monomer.Core.Combinators
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
    node = enhancedSliderD_ wlens a b configs []
    wlens = WidgetLens field

enhancedSliderV
    :: (WidgetModel s, WidgetEvent e, SliderValue a)
    => a
    -> (a -> e)
    -> a
    -> a
    -> WidgetNode s e
enhancedSliderV v handler a b = enhancedSliderV_ v handler a b def

enhancedSliderV_
    :: (WidgetModel s, WidgetEvent e, SliderValue a)
    => a
    -> (a -> e)
    -> a
    -> a
    -> [EnhancedSliderCfg s e a]
    -> WidgetNode s e
enhancedSliderV_ v handler a b configs = node where
    node = enhancedSliderD_ (WidgetValue v) a b newConfigs []
    newConfigs = onChange handler : configs

enhancedSliderD_
    :: (WidgetModel s, WidgetEvent e, SliderValue a)
    => WidgetData s a
    -> a
    -> a
    -> [EnhancedSliderCfg s e a]
    -> [CompositeCfg a (EnhancedSliderEvent a) s e]
    -> WidgetNode s e
enhancedSliderD_ wdata a b configs cmpConfigs = node where
    node = compositeD_ wt wdata uiBuilder eventHandler cmpConfigs
    wt = "enhancedSlider"
    uiBuilder = buildUI config a b
    eventHandler = handleEvent config a b
    config = mconcat configs
