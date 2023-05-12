{-# LANGUAGE RecordWildCards #-}

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
    valid = ($ changeInfo) <$> _dcValidator
    changeInfo = (_dmBoardState, ixTo, ixFrom)
    emptySource = validFrom && length sourceSquare == 0
    validFrom = ixFrom' >= 0 && ixFrom' < length _dmBoardState
    report = RequestParent <$> (($ changeInfo) <$> _dcOnChangeReq)
    dataReq = RequestParent <$> (widgetDataSet wdata newBoardState)
    newBoardState = zipWith f [offset..] _dmBoardState
    f i xs
        | i == ixTo = [head sourceSquare]
        | i == ixFrom = tail xs
        | otherwise = xs
    sourceSquare = _dmBoardState!!ixFrom'
    ixFrom' = ixFrom-offset
    offset = fromMaybe 0 _dcOffset
    DragboardCfg{..} = config
    DragboardModel{..} = model

clickHandle :: Int -> EventHandle a sp ep
clickHandle i config model@(DragboardModel{..}) = response where
    response
        | null _dmSelectedSquare = setSelectedSquare $ Just i
        | _dmSelectedSquare == Just i = setSelectedSquare Nothing
        | otherwise =
            [ Model $ model & selectedSquare .~ newSelected
            , Event $ EventDrop i d
            ]
    newSelected = if null dropResponses
        then Just i
        else Nothing
    dropResponses = dropHandle i d (WidgetValue []) config model
    d = DragId $ fromJust _dmSelectedSquare
    setSelectedSquare v = [Model $ model & selectedSquare .~ v]

focusHandle :: WidgetNode s e -> Path -> EventHandle a sp ep
focusHandle node prev DragboardCfg{..} _ = response where
    response = if valid
        then RequestParent <$> (($ prev) <$> _dcOnFocusReq)
        else []
    valid = not $ isNodeParentOfPath node prev

blurHandle :: WidgetNode s e -> Path -> EventHandle a sp ep
blurHandle node next DragboardCfg{..} _ = response where
    response = if valid
        then RequestParent <$> (($ next) <$> _dcOnBlurReq)
        else []
    valid = not $ isNodeParentOfPath node next
