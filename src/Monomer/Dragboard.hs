module Monomer.Dragboard
    ( module Monomer.Dragboard.DragboardCfg
    , dragboard
    , dragboard_
    , dragboardV
    , dragboardV_
    , dragboardD_
    ) where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Data.Typeable
import Monomer.Core.Combinators
import Monomer.Widgets.Composite

import Monomer.Dragboard.DragboardCfg
import Monomer.Dragboard.DragboardEvent
import Monomer.Dragboard.UI

dragboard
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int
    -> Int
    -> ALens' s [[a]]
    -> (a -> Text)
    -> WidgetNode s e
dragboard c r field f = dragboard_ c r field f def

dragboard_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int
    -> Int
    -> ALens' s [[a]]
    -> (a -> Text)
    -> [DragboardCfg s e a]
    -> WidgetNode s e
dragboard_ c r field f configs = node where
    node = dragboardD_ c r wlens f configs []
    wlens = WidgetLens field

dragboardV
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int
    -> Int
    -> [[a]]
    -> ([[a]] -> e)
    -> (a -> Text)
    -> WidgetNode s e
dragboardV c r v handler f = dragboardV_ c r v handler f def

dragboardV_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int
    -> Int
    -> [[a]]
    -> ([[a]] -> e)
    -> (a -> Text)
    -> [DragboardCfg s e a]
    -> WidgetNode s e
dragboardV_ c r v handler f configs = node where
    node = dragboardD_ c r (WidgetValue v) f newConfigs []
    newConfigs = onChange handler : configs

dragboardD_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int
    -> Int
    -> WidgetData s [[a]]
    -> (a -> Text)
    -> [DragboardCfg s e a]
    -> [CompositeCfg [[a]] DragboardEvent s e]
    -> WidgetNode s e
dragboardD_ c r wdata f configs cmpConfigs = node where
    node = compositeD_ wt wdata uiBuilder eventHandler cmpConfigs
    wt = WidgetType "dragboard"
    uiBuilder = buildUI config c r f
    eventHandler = handleEvent config
    config = mconcat configs
