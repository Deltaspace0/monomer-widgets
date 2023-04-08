module Monomer.Checkerboard
    ( module Monomer.Checkerboard.CheckerboardCfg
    , checkerboard
    , checkerboard_
    ) where

import Data.Default
import Monomer.Widgets.Composite
import TextShow

import Monomer.Checkerboard.CheckerboardCfg
import Monomer.Checkerboard.CheckerboardEvent
import Monomer.Checkerboard.UI

checkerboard
    :: (WidgetModel s, WidgetEvent e, Traversable t)
    => Int
    -> Int
    -> t (WidgetNode () CheckerboardEvent)
    -> WidgetNode s e
checkerboard c r children = checkerboard_ c r def children

checkerboard_
    :: (WidgetModel s, WidgetEvent e, Traversable t)
    => Int
    -> Int
    -> [CheckerboardCfg s e]
    -> t (WidgetNode () CheckerboardEvent)
    -> WidgetNode s e
checkerboard_ c r configs children = node where
    node = compositeD_ wt wdata uiBuilder eventHandler def
    wt = WidgetType $ "checkerboard-" <> dimensions
    dimensions = (showt c) <> "-" <> (showt r)
    wdata = WidgetValue ()
    uiBuilder = buildUI config c r children
    eventHandler = handleEvent config
    config = mconcat configs
