{-# LANGUAGE RecordWildCards #-}

module Monomer.Dragboard.DragboardEvent
    ( DragId(..)
    , DragboardEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Data.Maybe
import Monomer.Main.UserUtil
import Monomer.Widgets.Animation
import Monomer.Widgets.Composite
import Monomer.Widgets.Single
import TextShow
import qualified Data.Map as Map
import qualified Monomer.Lens as L

import Monomer.Dragboard.DragboardCfg
import Monomer.Dragboard.DragboardModel

newtype DragId = DragId Int deriving Eq

data DragboardEvent
    = EventDrop Int DragId
    | EventClick Int
    | EventFinished Int
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
handleEvent wdata config wenv node model event = case event of
    EventDrop i d -> dropHandle i d wdata config model
    EventClick i -> clickHandle i wenv config model
    EventFinished i -> finishedHandle i config model
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

clickHandle
    :: Int
    -> WidgetEnv (DragboardModel a) DragboardEvent
    -> EventHandle a sp ep
clickHandle i wenv config model@(DragboardModel{..}) = resp where
    resp
        | null _dmSelectedSquare = setSelectedSquare $ Just i
        | _dmSelectedSquare == Just i = setSelectedSquare Nothing
        | otherwise =
            [ Model $ model
                & selectedSquare .~ newSelected
                & animationSources %~ insertSource
            , Event $ EventDrop i d
            , responseIf (not $ null dropResponses) $
                Message destinationKey AnimationStart
            ]
    newSelected = if null dropResponses
        then Just i
        else Nothing
    insertSource = if null sourceRect
        then id
        else Map.insert i $ fromJust sourceRect
    sourceRect = view L.viewport <$> sourceInfo
    sourceInfo = nodeInfoFromKey wenv $ WidgetKey sourceKey
    destinationKey = WidgetKey $ "dragItem" <> showt i
    sourceKey = "dragItem" <> (showt $ fromJust _dmSelectedSquare)
    dropResponses = dropHandle i d (WidgetValue []) config' model
    config' = config <> (onChangeReq $ const RenderOnce)
    d = DragId $ fromJust _dmSelectedSquare
    setSelectedSquare v = [Model $ model & selectedSquare .~ v]

finishedHandle :: Int -> EventHandle a sp ep
finishedHandle i _ _ = response where
    response = [Message destinationKey AnimationStop]
    destinationKey = WidgetKey $ "dragItem" <> (showt i)

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
