{-# LANGUAGE RecordWildCards #-}

module Monomer.Dragboard.UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Monomer.Checkerboard
import Monomer.Core.Combinators
import Monomer.Graphics.ColorTable
import Monomer.Graphics.Types
import Monomer.Graphics.Util
import Monomer.Widgets.Animation
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Draggable
import Monomer.Widgets.Containers.DropTarget
import Monomer.Widgets.Singles.Image
import Monomer.Widgets.Singles.Spacer
import Monomer.Widgets.Composite
import TextShow
import qualified Data.Map as Map

import Monomer.Dragboard.DragboardCfg
import Monomer.Dragboard.DragboardEvent
import Monomer.Dragboard.DragboardModel

buildUI
    :: (Typeable a)
    => DragboardCfg s e a
    -> Int
    -> Int
    -> (a -> Either Text Color)
    -> UIBuilder (DragboardModel a) (DragboardEvent a)
buildUI DragboardCfg{..} c r getPathOrColor _ model = node where
    node = box_
        [ onFocus EventFocus
        , onBlur EventBlur
        ] $ checkerboard_ c r cc $ zipWith f [offset..] boardState'
    cc = _dcCheckerCfg
    offset = fromMaybe 0 _dcOffset
    f i xs = clickBox i $ makeDrop i $ if null xs
        then filler
        else draggable_ (DragId i) draggableConfigs $ managed xs
    makeDrop i = dropTarget (EventDrop i) . makeAnim i
    clickBox i x = if _dcNoClick == Just True
        then x
        else paint i $ box_ [onBtnPressed $ \_ _ -> EventClick i] x
    paint i x
        | legal i = x `styleBasic` [bgColor $ rgba 4 42 4 0.8]
        | model ^. selectedSquare == Just i = x `styleBasic` [bgColor sColor]
        | otherwise = x
    legal i = _dcShowLegal == Just True && i `elem` model ^. legalSquares
    sColor = fromMaybe yellow _dcSelectColor
    draggableConfigs = [draggableRenderSource_ renderS]
    renderS = fromMaybe False _dcRenderS
    makeAnim i x = animTransform_
        [ duration dur
        , onFinished $ EventFinished i
        ] (fa i) x `nodeKey` ("dragItem" <> showt i)
    fa i t (Rect x2 y2 _ _) = transformation where
        transformation = if null sourceRect
            then []
            else
                [ animTranslation $ Point x y
                , animScissor $ Rect (-10000) (-10000) 20000 20000
                ]
        (x, y) = ((x2-x1)*prog-x2+x1, (y2-y1)*prog-y2+y1)
        prog = (sin $ (t/dur'-0.5)*pi)/2+0.5
        Rect x1 y1 _ _ = fromJust sourceRect
        sourceRect = Map.lookup i $ model ^. animationSources
    managed = makeWidget . getPathOrColor . head
    makeWidget (Left path) = image_ path [fitEither, noDispose]
    makeWidget (Right color) = filler `styleBasic` [bgColor color]
    boardState' = model ^. boardState
    dur' = fromIntegral dur
    dur = fromMaybe 500 _dcDuration
