module Monomer.Dragboard.DragboardEvent
    ( DragId(..)
    , DragboardEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Data.Maybe
import Monomer.Main.UserUtil
import Monomer.Widgets.Composite

import Monomer.Dragboard.DragboardCfg
import Monomer.Dragboard.DragboardModel

newtype DragId = DragId Int deriving Eq

data DragboardEvent
    = EventDrop Int DragId
    | EventClick Int
    | EventFocus Path
    | EventBlur Path
    deriving Eq

type EventHandle a sp ep
    = (DragboardCfg sp ep a)
    -> (DragboardModel a)
    -> [EventResponse (DragboardModel a) DragboardEvent sp ep]

handleEvent
    :: (WidgetData sp [[a]])
    -> (DragboardCfg sp ep a)
    -> EventHandler (DragboardModel a) DragboardEvent sp ep
handleEvent wdata config _ node model event = case event of
    EventDrop i d -> dropHandle i d wdata config model
    EventClick i -> clickHandle i config model
    EventFocus prev -> focusHandle node prev config model
    EventBlur next -> blurHandle node next config model

dropHandle
    :: Int
    -> DragId
    -> (WidgetData sp [[a]])
    -> EventHandle a sp ep
dropHandle ixTo (DragId ixFrom) wdata config model = response where
    response = if valid == Just False || emptySource
        then []
        else (responseIf validFrom <$> dataReq) <> report
    valid = ($ changeInfo) <$> _dcValidator config
    changeInfo = (boardState', ixTo, ixFrom)
    emptySource = validFrom && length sourceSquare == 0
    validFrom = ixFrom' >= 0 && ixFrom' < length boardState'
    report = RequestParent <$> (($ changeInfo) <$> req)
    req = _dcOnChangeReq config
    dataReq = RequestParent <$> (widgetDataSet wdata newBoardState)
    newBoardState = zipWith f [offset..] boardState'
    f i xs
        | i == ixTo = [head sourceSquare]
        | i == ixFrom = tail xs
        | otherwise = xs
    sourceSquare = boardState'!!ixFrom'
    ixFrom' = ixFrom-offset
    offset = fromMaybe 0 $ _dcOffset config
    boardState' = model ^. boardState

clickHandle :: Int -> EventHandle a sp ep
clickHandle i config model = response where
    response
        | null selected = setSelectedSquare $ Just i
        | selected == Just i = setSelectedSquare Nothing
        | otherwise =
            [ Model $ model & selectedSquare .~ newSelected
            , Event $ EventDrop i d
            ]
    newSelected = if null dropResponses
        then Just i
        else Nothing
    dropResponses = dropHandle i d (WidgetValue []) config model
    d = DragId $ fromJust selected
    selected = model ^. selectedSquare
    setSelectedSquare v = [Model $ model & selectedSquare .~ v]

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
