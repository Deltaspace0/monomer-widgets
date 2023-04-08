module Monomer.Checkerboard
    ( module Monomer.Checkerboard.CheckerboardCfg
    , checkerboard
    , checkerboard_
    ) where

import Data.Default
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

checkerboard
    :: (WidgetModel s, WidgetEvent e, Traversable t)
    => Int
    -> Int
    -> t (WidgetNode s e)
    -> WidgetNode s e
checkerboard c r children = checkerboard_ c r def children

checkerboard_
    :: (WidgetModel s, WidgetEvent e, Traversable t)
    => Int
    -> Int
    -> [CheckerboardCfg s e]
    -> t (WidgetNode s e)
    -> WidgetNode s e
checkerboard_ c r configs children = tree where
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
    config = mconcat configs
