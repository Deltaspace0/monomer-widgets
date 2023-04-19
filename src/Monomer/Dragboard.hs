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
import Monomer.Graphics.Types
import Monomer.Widgets.Composite

import Monomer.Dragboard.DragboardCfg
import Monomer.Dragboard.DragboardEvent
import Monomer.Dragboard.UI

dragboard
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int
    -> Int
    -> ALens' s [[a]]
    -> (a -> Either Text Color)
    -> WidgetNode s e
dragboard c r field f = dragboard_ c r field f def

dragboard_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int
    -> Int
    -> ALens' s [[a]]
    -> (a -> Either Text Color)
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
    -> (([[a]], Int, Int) -> e)
    -> (a -> Either Text Color)
    -> WidgetNode s e
dragboardV c r v handler f = dragboardV_ c r v handler f def

dragboardV_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int
    -> Int
    -> [[a]]
    -> (([[a]], Int, Int) -> e)
    -> (a -> Either Text Color)
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
    -> (a -> Either Text Color)
    -> [DragboardCfg s e a]
    -> [CompositeCfg [[a]] DragboardEvent s e]
    -> WidgetNode s e
dragboardD_ c r wdata f configs cmpConfigs = node where
    node = compositeD_ wt wdata uiBuilder eventHandler cmpConfigs'
    wt = WidgetType "dragboard"
    uiBuilder = buildUI config c r f
    eventHandler = handleEvent config
    config = mconcat configs
    cmpConfigs' = mergeRequired (\_ _ _ -> True) : cmpConfigs
