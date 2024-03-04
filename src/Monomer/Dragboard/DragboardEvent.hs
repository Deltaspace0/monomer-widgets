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

newtype DragId = DragId Int deriving (Eq, Show)

data DragboardEvent a
    = EventDrop Int DragId
    | EventClick Int
    | EventUpdateLegalSquares
    | EventFinished Int
    | EventMerge (DragboardModel a)
    | EventFocus Path
    | EventBlur Path
    deriving Eq

type EventHandle a sp ep
    = (DragboardCfg sp ep a)
    -> (DragboardModel a)
    -> [EventResponse (DragboardModel a) (DragboardEvent a) sp ep]

handleEvent
    :: (Eq a)
    => (WidgetData sp [[a]])
    -> (DragboardCfg sp ep a)
    -> EventHandler (DragboardModel a) (DragboardEvent a) sp ep
handleEvent wdata config wenv node model event = case event of
    EventDrop i d -> dropHandle i d wdata config model
    EventClick i -> clickHandle i wenv config model
    EventUpdateLegalSquares -> updateLegalSquaresHandle config model
    EventFinished i -> finishedHandle i config model
    EventMerge m -> mergeHandle m wenv config model
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
        else setBoardState <> report
    setBoardState = if validFrom
        then
            [ Model $ model
                & boardState .~ newState
                & selectedSquare .~ Nothing
            , Event EventUpdateLegalSquares
            ] <> dataReq
        else []
    valid = ($ changeInfo) <$> _dcValidator
    changeInfo = (_dmBoardState, ixTo, ixFrom)
    emptySource = validFrom && length sourceSquare == 0
    validFrom = ixFrom' >= 0 && ixFrom' < length _dmBoardState
    report = RequestParent <$> (($ changeInfo) <$> _dcOnChangeReq)
    dataReq = RequestParent <$> (widgetDataSet wdata newState)
    newState = zipWith f [offset..] _dmBoardState
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
    -> WidgetEnv (DragboardModel a) (DragboardEvent a)
    -> EventHandle a sp ep
clickHandle i wenv config model@(DragboardModel{..}) = response where
    response
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
    insertSource = maybe id (Map.insert i) sourceRect
    sourceRect = view L.viewport <$> sourceInfo
    sourceInfo = nodeInfoFromKey wenv $ WidgetKey sourceKey
    destinationKey = WidgetKey $ "dragItem" <> showt i
    sourceKey = "dragItem" <> (showt $ fromJust _dmSelectedSquare)
    dropResponses = dropHandle i d (WidgetValue []) config' model
    config' = config <> (onChangeReq $ const RenderOnce)
    d = DragId $ fromJust _dmSelectedSquare
    setSelectedSquare v =
        [ Model $ model & selectedSquare .~ v
        , Event EventUpdateLegalSquares
        ]

updateLegalSquaresHandle :: EventHandle a sp ep
updateLegalSquaresHandle config model = response where
    response = [Model $ model & legalSquares .~ newLegalSquares]
    newLegalSquares = case _dmSelectedSquare of
        Nothing -> []
        Just i -> filter (f i) [offset..(offset+(length _dmBoardState)-1)]
    f ixFrom ixTo = (($ changeInfo) <$> _dcValidator) == Just True where
        changeInfo = (_dmBoardState, ixTo, ixFrom)
    offset = fromMaybe 0 _dcOffset
    DragboardCfg{..} = config
    DragboardModel{..} = model

finishedHandle :: Int -> EventHandle a sp ep
finishedHandle i _ _ = response where
    response = [Message destinationKey AnimationStop]
    destinationKey = WidgetKey $ "dragItem" <> (showt i)

mergeHandle
    :: (Eq a)
    => DragboardModel a
    -> WidgetEnv (DragboardModel a) (DragboardEvent a)
    -> EventHandle a sp ep
mergeHandle DragboardModel{..} wenv config model = response where
    response = setSources:(flip Message AnimationStart <$> newKeys)
    setSources = Model $ model & animationSources .~ newSources
    (newSources, newKeys, _) = foldl f initState models
    initState = (_dmAnimationSources, [], [])
    f (sources, keys, kk) (i, a, b) = (sources', keys', kk') where
        sources' = if noAnimation
            then sources
            else Map.insert i (fromJust sourceRect) sources
        keys' = if noAnimation
            then keys
            else (WidgetKey $ "dragItem" <> (showt i)):keys
        kk' = if noAnimation || null sourceKey
            then kk
            else (fromJust sourceKey):kk
        noAnimation = (same a b) || null sourceRect
        sourceRect = sourceKey >>= getRect
        sourceKey = findSource models
        findSource [] = Nothing
        findSource ((j, a', b'):xs) = result where
            result = if (same a' b') || (not $ same a' b) || usedKey
                then findSource xs
                else Just j
            usedKey = j `elem` kk
    getRect i = view L.viewport <$> sourceInfo where
        sourceInfo = nodeInfoFromKey wenv $ WidgetKey sourceKey
        sourceKey = "dragItem" <> (showt i)
    same a b = sameHead || (null a && null b) where
        sameHead = not (null a) && not (null b) && head a == head b
    models = zip3 [offset..] _dmBoardState boardState'
    boardState' = model ^. boardState
    offset = fromMaybe 0 $ _dcOffset config

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
