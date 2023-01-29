{-|
This is a slider with a label, which shows current value, and
buttons to increase and decrease value.

@
enhancedSlider lens 0 100
@
-}

module Monomer.EnhancedSlider
    ( -- * Re-exported modules
      module Monomer.EnhancedSlider.EnhancedSliderCfg
      -- * Constructors
    , enhancedSlider
    , enhancedSlider_
    , enhancedSliderV
    , enhancedSliderV_
    , enhancedSliderD_
    ) where

import Control.Lens
import Data.Default
import Data.Typeable
import Monomer.Core.Combinators
import Monomer.Widgets.Composite
import Monomer.Widgets.Singles.Slider
import TextShow

import Monomer.EnhancedSlider.EnhancedSliderCfg
import Monomer.EnhancedSlider.EnhancedSliderEvent
import Monomer.EnhancedSlider.UI

{-|
Creates an enhanced slider using the given lens, providing minimum
and maximum values.
-}
enhancedSlider
    :: (WidgetModel s, WidgetEvent e, SliderValue a)
    => ALens' s a      -- ^ The lens into the model.
    -> a               -- ^ Minimum value.
    -> a               -- ^ Maximum value.
    -> WidgetNode s e  -- ^ The created enhanced slider.
enhancedSlider field a b = enhancedSlider_ field a b def

{-|
Creates an enhanced slider using the given lens, providing minimum
and maximum values. Accepts config.
-}
enhancedSlider_
    :: (WidgetModel s, WidgetEvent e, SliderValue a)
    => ALens' s a                 -- ^ The lens into the model.
    -> a                          -- ^ Minimum value.
    -> a                          -- ^ Maximum value.
    -> [EnhancedSliderCfg s e a]  -- ^ The config options.
    -> WidgetNode s e             -- ^ The created enhanced slider.
enhancedSlider_ field a b configs = node where
    node = enhancedSliderD_ wlens a b configs []
    wlens = WidgetLens field

{-|
Creates an enhanced slider using the given value and 'onChange'
event handler, providing minimum and maximum values.
-}
enhancedSliderV
    :: (WidgetModel s, WidgetEvent e, SliderValue a)
    => a               -- ^ The current value.
    -> (a -> e)        -- ^ The event to raise on change.
    -> a               -- ^ Minimum value.
    -> a               -- ^ Maximum value.
    -> WidgetNode s e  -- ^ The created enhanced slider.
enhancedSliderV v handler a b = enhancedSliderV_ v handler a b def

{-|
Creates an enhanced slider using the given value and 'onChange'
event handler, providing minimum and maximum values. Accepts config.
-}
enhancedSliderV_
    :: (WidgetModel s, WidgetEvent e, SliderValue a)
    => a                          -- ^ The current value.
    -> (a -> e)                   -- ^ The event to raise on change.
    -> a                          -- ^ Minimum value.
    -> a                          -- ^ Maximum value.
    -> [EnhancedSliderCfg s e a]  -- ^ The config options.
    -> WidgetNode s e             -- ^ The created enhanced slider.
enhancedSliderV_ v handler a b configs = node where
    node = enhancedSliderD_ (WidgetValue v) a b newConfigs []
    newConfigs = onChange handler : configs

{-|
Creates an enhanced slider providing a 'WidgetData' instance,
minimum and maximum values and config.
-}
enhancedSliderD_
    :: (WidgetModel s, WidgetEvent e, SliderValue a)
    => WidgetData s a
    -- ^ The 'WidgetData' to retrieve the value from.
    -> a
    -- ^ Minimum value.
    -> a
    -- ^ Maximum value.
    -> [EnhancedSliderCfg s e a]
    -- ^ The config options.
    -> [CompositeCfg a (EnhancedSliderEvent a) s e]
    -- ^ The composite config options.
    -> WidgetNode s e
    -- ^ The created enhanced slider.
enhancedSliderD_ wdata a b configs cmpConfigs = node where
    node = compositeD_ wt wdata uiBuilder eventHandler cmpConfigs
    wt = WidgetType $ "enhancedSlider-" <> (showt $ typeOf a)
    uiBuilder = buildUI config a b
    eventHandler = handleEvent config a b
    config = mconcat configs
