module Monomer.Checkerboard.UI
    ( buildUI
    ) where

import Data.Foldable (toList)
import Data.List.Split
import Data.Maybe
import Monomer.Core.Combinators
import Monomer.Graphics.ColorTable
import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Containers.Grid
import Monomer.Widgets.Singles.Spacer

import Monomer.Checkerboard.CheckerboardCfg
import Monomer.Checkerboard.CheckerboardEvent

buildUI
    :: (Traversable t)
    => (CheckerboardCfg s e)
    -> Int
    -> Int
    -> t (WidgetNode () CheckerboardEvent)
    -> UIBuilder () CheckerboardEvent
buildUI config c r children _ _ = tree where
    tree = vgrid $ zipWith makeRow boxGrid $ cs <> repeat []
    makeRow f x = hgrid $ zipWith ($) f $ x <> repeat spacer
    cs = chunksOf c $ toList children
    boxGrid = take r $ cycle
        [ take c $ cycle [lightBox, darkBox]
        , take c $ cycle [darkBox, lightBox]
        ]
    lightBox x = box x `styleBasic` [bgColor lc]
    darkBox x = box x `styleBasic` [bgColor dc]
    lc = fromMaybe white $ _ccBgLightColor config
    dc = fromMaybe black $ _ccBgDarkColor config
