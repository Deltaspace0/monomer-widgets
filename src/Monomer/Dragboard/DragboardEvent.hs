module Monomer.Dragboard.DragboardEvent
    ( DragboardEvent(..)
    , handleEvent
    ) where

import Monomer.Widgets.Composite

import Monomer.Dragboard.DragboardCfg

data DragboardEvent
    = EventDrop Int Int
    deriving Eq

type EventHandle a sp ep
    = (DragboardCfg sp ep a)
    -> [[a]]
    -> [EventResponse [[a]] DragboardEvent sp ep]

handleEvent
    :: (DragboardCfg sp ep a)
    -> EventHandler [[a]] DragboardEvent sp ep
handleEvent config _ _ model event = case event of
    EventDrop ixTo ixFrom -> dropHandle ixTo ixFrom config model

dropHandle
    :: Int
    -> Int
    -> EventHandle a sp ep
dropHandle ixTo ixFrom config model = response where
    response = [Model newModel] <> report
    newModel = zipWith f [0..] model
    f i xs = if i == ixTo
        then [dragged]
        else if i == ixFrom
            then tail xs
            else xs
    dragged = head $ model!!ixFrom
    report = RequestParent <$> (($ newModel) <$> req)
    req = _dcOnChangeReq config
