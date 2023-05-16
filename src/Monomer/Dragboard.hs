{-|
This is a dragboard which uses checkerboard with draggable images or
colored fillers. It needs to be provided with number of rows and
columns, lens to the board state of type '[[a]]' (each square of the
board would thus contain a list of items '[a]' and show only the
first item) and the function which converts item 'a' to either color
of a filler or path to the image.

@
dragboard 3 3 boardState getPath
@
-}

{-# LANGUAGE FlexibleContexts #-}

module Monomer.Dragboard
    ( -- * Re-exported modules
      module Monomer.Dragboard.DragboardCfg
      -- * Constructors
    , dragboard
    , dragboard_
    , dragboardV
    , dragboardV_
    , dragboardD_
    ) where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Data.Typeable
import Monomer.Core.Combinators
import Monomer.Graphics.Types
import Monomer.Widgets.Composite

import Monomer.Dragboard.DragboardCfg
import Monomer.Dragboard.DragboardEvent
import Monomer.Dragboard.DragboardModel
import Monomer.Dragboard.UI

{-|
Creates a dragboard using the given lens, providing number of
columns and rows and the function to get image path or color from
the value.
-}
dragboard
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int                       -- ^ Number of columns.
    -> Int                       -- ^ Number of rows.
    -> ALens' s [[a]]            -- ^ The lens into the model.
    -> (a -> Either Text Color)  -- ^ The path or color function.
    -> WidgetNode s e            -- ^ The created dragboard.
dragboard c r field f = dragboard_ c r field f def

{-|
Creates a dragboard using the given lens, providing number of
columns and rows and the function to get image path or color from
the value. Accepts config.
-}
dragboard_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int                       -- ^ Number of columns.
    -> Int                       -- ^ Number of rows.
    -> ALens' s [[a]]            -- ^ The lens into the model.
    -> (a -> Either Text Color)  -- ^ The path or color function.
    -> [DragboardCfg s e a]      -- ^ The config options.
    -> WidgetNode s e            -- ^ The created dragboard.
dragboard_ c r field f configs = node where
    node = dragboardD_ c r wlens f configs []
    wlens = WidgetLens field

{-|
Creates a dragboard using the given board state and 'onChange' event
handler, providing number of columns and rows and the function to
get image path or color from the value.
-}
dragboardV
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int                       -- ^ Number of columns.
    -> Int                       -- ^ Number of rows.
    -> [[a]]                     -- ^ The current board state.
    -> (([[a]], Int, Int) -> e)  -- ^ The event to raise on change.
    -> (a -> Either Text Color)  -- ^ The path or color function.
    -> WidgetNode s e            -- ^ The created dragboard.
dragboardV c r v handler f = dragboardV_ c r v handler f def

{-|
Creates a dragboard using the given board state and 'onChange' event
handler, providing number of columns and rows and the function to
get image path or color from the value. Accepts config.
-}
dragboardV_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int                       -- ^ Number of columns.
    -> Int                       -- ^ Number of rows.
    -> [[a]]                     -- ^ The current board state.
    -> (([[a]], Int, Int) -> e)  -- ^ The event to raise on change.
    -> (a -> Either Text Color)  -- ^ The path or color function.
    -> [DragboardCfg s e a]      -- ^ The config options.
    -> WidgetNode s e            -- ^ The created dragboard.
dragboardV_ c r v handler f configs = node where
    node = dragboardD_ c r (WidgetValue v) f newConfigs []
    newConfigs = onChange handler : configs

{-|
Creates a dragboard providing a 'WidgetData' instance, number of
columns and rows, the function to get image path or color from the
value and config.
-}
dragboardD_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => Int
    -- ^ Number of columns.
    -> Int
    -- ^ Number of rows.
    -> WidgetData s [[a]]
    -- ^ The 'WidgetData' to retrieve the board state from.
    -> (a -> Either Text Color)
    -- ^ The path or color function.
    -> [DragboardCfg s e a]
    -- ^ The config options.
    -> [CompositeCfg (DragboardModel a) (DragboardEvent a) s e]
    -- ^ The composite config options.
    -> WidgetNode s e
    -- ^ The created dragboard.
dragboardD_ c r wdata f configs cmpConfigs = node where
    node = compositeD_ wt wdata' uiBuilder eventHandler cmpConfigs'
    wt = WidgetType "dragboard"
    wdata' = WidgetValue initDragboardModel
    uiBuilder = buildUI config c r f
    eventHandler = handleEvent wdata config
    config = mconcat configs
    cmpConfigs' =
        [ mergeRequired (\_ _ _ -> True)
        , compositeMergeModel mergeHandler
        , compositeMergeEvents $ \_ _ _ _ m _ -> [EventMerge m]
        ] <> cmpConfigs
    mergeHandler _ pm m _ = m & boardState .~ widgetDataGet pm wdata
