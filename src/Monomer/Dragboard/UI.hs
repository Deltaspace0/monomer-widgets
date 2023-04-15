module Monomer.Dragboard.UI
    ( buildUI
    ) where

import Data.Text (Text)
import Data.Typeable
import Monomer.Checkerboard
import Monomer.Core.Combinators
import Monomer.Widgets.Containers.Draggable
import Monomer.Widgets.Containers.DropTarget
import Monomer.Widgets.Singles.Image
import Monomer.Widgets.Singles.Spacer
import Monomer.Widgets.Composite

import Monomer.Dragboard.DragboardCfg
import Monomer.Dragboard.DragboardEvent

buildUI
    :: (Typeable a)
    => DragboardCfg s e a
    -> Int
    -> Int
    -> (a -> Text)
    -> UIBuilder [[a]] DragboardEvent
buildUI config c r getPath _ model = node where
    node = checkerboard_ c r cc $ zipWith f [0..] model
    cc = _dcCheckerCfg config
    f i xs = dropTarget (EventDrop i) $ if null xs
        then filler
        else draggable i $ image_ (getPath $ head xs) [fitEither]
