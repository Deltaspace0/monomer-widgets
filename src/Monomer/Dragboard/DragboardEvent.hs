module Monomer.Dragboard.DragboardEvent
    ( DragId(..)
    , DragboardEvent(..)
    , handleEvent
    ) where

import Data.Maybe
import Monomer.Main.UserUtil
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
        else [responseIf validFrom $ Model newModel] <> report
    valid = ($ changeInfo) <$> _dcValidator config
    changeInfo = (model, ixTo, ixFrom)
    validFrom = ixFrom' >= 0 && ixFrom' < length model
    newModel = zipWith f [offset..] model
    f i xs
        | i == ixTo = [head $ model!!ixFrom']
        | i == ixFrom = tail xs
        | otherwise = xs
    ixFrom' = ixFrom-offset
    report = RequestParent <$> (($ changeInfo) <$> req)
    req = _dcOnChangeReq config
    offset = fromMaybe 0 $ _dcOffset config

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
