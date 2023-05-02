module Monomer.Dragboard.DragboardEvent
    ( DragId(..)
    , DragboardEvent(..)
    , handleEvent
    ) where

import Monomer.Widgets.Composite

import Monomer.Dragboard.DragboardCfg

newtype DragId = DragId Int deriving Eq

data DragboardEvent
    = EventDrop Int DragId
    | EventFocus Path
    | EventBlur Path
    deriving Eq

type EventHandle a sp ep
    = (DragboardCfg sp ep a)
    -> [[a]]
    -> [EventResponse [[a]] DragboardEvent sp ep]

handleEvent
    :: (DragboardCfg sp ep a)
    -> EventHandler [[a]] DragboardEvent sp ep
handleEvent config _ node model event = case event of
    EventDrop ixTo dragId -> dropHandle ixTo dragId config model
    EventFocus prev -> focusHandle node prev config model
    EventBlur next -> blurHandle node next config model

dropHandle :: Int -> DragId -> EventHandle a sp ep
dropHandle ixTo (DragId ixFrom) config model = response where
    response = if valid == Just False
        then []
        else [Model newModel] <> report
    valid = ($ changeInfo) <$> _dcMoveValidator config
    changeInfo = (model, ixTo, ixFrom)
    newModel = zipWith f [0..] model
    f i xs = if i == ixTo
        then [dragged]
        else if i == ixFrom
            then tail xs
            else xs
    dragged = head $ model!!ixFrom
    report = RequestParent <$> (($ changeInfo) <$> req)
    req = _dcOnChangeReq config

focusHandle :: WidgetNode s e -> Path -> EventHandle a sp ep
focusHandle node prev config _ = response where
    response = if valid
        then RequestParent <$> (($ prev) <$> _dcOnFocusReq config)
        else []
    valid = not $ isNodeParentOfPath node prev

blurHandle :: WidgetNode s e -> Path -> EventHandle a sp ep
blurHandle node next config _ = response where
    response = if valid
        then RequestParent <$> (($ next) <$> _dcOnBlurReq config)
        else []
    valid = not $ isNodeParentOfPath node next
