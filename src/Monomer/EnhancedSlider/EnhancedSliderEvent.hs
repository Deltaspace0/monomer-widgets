{-# LANGUAGE RecordWildCards #-}

module Monomer.EnhancedSlider.EnhancedSliderEvent
    ( EnhancedSliderEvent(..)
    , handleEvent
    ) where

import Monomer.Widgets.Composite
import Monomer.Widgets.Singles.Slider

import Monomer.EnhancedSlider.EnhancedSliderCfg

data EnhancedSliderEvent a
    = EventSetField a
    | EventFocus Path
    | EventBlur Path
    deriving Eq

handleEvent
    :: (SliderValue a)
    => (EnhancedSliderCfg sp ep a)
    -> a
    -> a
    -> EventHandler a (EnhancedSliderEvent a) sp ep
handleEvent config a b _ node _ event = case event of
    EventSetField value -> setFieldHandle config a b value
    EventFocus prev -> focusHandle node prev config
    EventBlur next -> blurHandle node next config

setFieldHandle
    :: (SliderValue a)
    => (EnhancedSliderCfg sp ep a)
    -> a
    -> a
    -> a
    -> [EventResponse a (EnhancedSliderEvent a) sp ep]
setFieldHandle EnhancedSliderCfg{..} a b value = response where
    response = [Model newValue] <> report
    report = RequestParent <$> (($ newValue) <$> _escOnChangeReq)
    newValue = min b $ max a value

focusHandle
    :: WidgetNode s e
    -> Path
    -> EnhancedSliderCfg sp ep a
    -> [EventResponse a (EnhancedSliderEvent a) sp ep]
focusHandle node prev EnhancedSliderCfg{..} = response where
    response = if valid
        then RequestParent <$> (($ prev) <$> _escOnFocusReq)
        else []
    valid = not $ isNodeParentOfPath node prev

blurHandle
    :: WidgetNode s e
    -> Path
    -> EnhancedSliderCfg sp ep a
    -> [EventResponse a (EnhancedSliderEvent a) sp ep]
blurHandle node next EnhancedSliderCfg{..} = response where
    response = if valid
        then RequestParent <$> (($ next) <$> _escOnBlurReq)
        else []
    valid = not $ isNodeParentOfPath node next
