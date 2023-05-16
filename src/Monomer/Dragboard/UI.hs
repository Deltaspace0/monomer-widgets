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
        else paint i $ box_ [onBtnReleased $ \_ _ -> EventClick i] x
    paint i x = if model ^. selectedSquare == Just i
        then x `styleBasic` [bgColor selectedColor]
        else x
    draggableConfigs = [draggableRenderSource_ renderS]
    renderS = fromMaybe False _dcRenderS
    selectedColor = fromMaybe yellow _dcSelectColor
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
        (x, y) = ((x2-x1)*t/dur'-x2+x1, (y2-y1)*t/dur'-y2+y1)
        Rect x1 y1 _ _ = fromJust sourceRect
        sourceRect = Map.lookup i $ model ^. animationSources
    managed = makeWidget . getPathOrColor . head
    makeWidget (Left path) = image_ path [fitEither]
    makeWidget (Right color) = filler `styleBasic` [bgColor color]
    boardState' = model ^. boardState
    dur' = fromIntegral dur
    dur = fromMaybe 500 _dcDuration
