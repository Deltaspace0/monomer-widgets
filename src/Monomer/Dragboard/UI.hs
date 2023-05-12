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
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Draggable
import Monomer.Widgets.Containers.DropTarget
import Monomer.Widgets.Singles.Image
import Monomer.Widgets.Singles.Spacer
import Monomer.Widgets.Composite

import Monomer.Dragboard.DragboardCfg
import Monomer.Dragboard.DragboardEvent
import Monomer.Dragboard.DragboardModel

buildUI
    :: (Typeable a)
    => DragboardCfg s e a
    -> Int
    -> Int
    -> (a -> Either Text Color)
    -> UIBuilder (DragboardModel a) DragboardEvent
buildUI DragboardCfg{..} c r getPathOrColor _ model = node where
    node = box_
        [ onFocus EventFocus
        , onBlur EventBlur
        ] $ checkerboard_ c r cc $ zipWith f [offset..] boardState'
    cc = _dcCheckerCfg
    offset = fromMaybe 0 _dcOffset
    f i xs = clickBox i $ dropTarget (EventDrop i) $ if null xs
        then filler
        else draggable_ (DragId i) draggableConfigs $ managed xs
    clickBox i x = if _dcNoClick == Just True
        then x
        else paint i $ box_ [onBtnReleased $ \_ _ -> EventClick i] x
    paint i x = if model ^. selectedSquare == Just i
        then x `styleBasic` [bgColor selectedColor]
        else x
    draggableConfigs = [draggableRenderSource_ renderS]
    renderS = fromMaybe False _dcRenderS
    selectedColor = fromMaybe yellow _dcSelectColor
    managed = makeWidget . getPathOrColor . head
    makeWidget (Left path) = image_ path [fitEither]
    makeWidget (Right color) = filler `styleBasic` [bgColor color]
    boardState' = model ^. boardState
