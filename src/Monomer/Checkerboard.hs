{-|
This is a checkerboard container (grid of boxes with alternating
background colors). Provided widgets are distributed from left to
right, top to bottom. If the are more provided widgets than there
are boxes in the grid then last widgets are ignored.

@
checkerboard 3 3 [filler, filler, label ":D"]
@
-}

module Monomer.Checkerboard
    ( -- * Re-exported modules
      module Monomer.Checkerboard.CheckerboardCfg
      -- * Constructors
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

{-|
Creates a checkerboard container for multiple items using number
of columns and rows.
-}
checkerboard
    :: (WidgetModel s, WidgetEvent e, Traversable t)
    => Int                 -- ^ Number of columns.
    -> Int                 -- ^ Number of rows.
    -> t (WidgetNode s e)  -- ^ The list of items.
    -> WidgetNode s e      -- ^ The created checkerboard.
checkerboard c r children = checkerboard_ c r def children

{-|
Creates a checkerboard container for multiple items using number
of columns and rows. Accepts config.
-}
checkerboard_
    :: (WidgetModel s, WidgetEvent e, Traversable t)
    => Int                 -- ^ Number of columns.
    -> Int                 -- ^ Number of rows.
    -> [CheckerboardCfg]   -- ^ The config options.
    -> t (WidgetNode s e)  -- ^ The list of items.
    -> WidgetNode s e      -- ^ The created checkerboard.
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
