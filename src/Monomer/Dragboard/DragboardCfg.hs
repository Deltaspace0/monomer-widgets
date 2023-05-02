{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Dragboard.DragboardCfg
    ( -- * Re-exported modules
      module Monomer.Checkerboard.CheckerboardCfg
      -- * Configuration
    , DragboardCfg(..)
    , moveValidator
    , dragIdOffset
    , checkerConfig
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Monomer.Checkerboard.CheckerboardCfg
import Monomer.Widgets.Single

type Info a = ([[a]], Int, Int)

{-|
Configuration options for dragboard:

- 'moveValidator': function to check validity of a move.
- 'dragIdOffset': offset for drag and drop event messages.
- 'checkerConfig': config options for checkerboard container.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when the board changes.
- 'onChangeReq': 'WidgetRequest' to generate when the board changes.
-}
data DragboardCfg s e a = DragboardCfg
    { _dcMoveValidator :: Maybe (Info a -> Bool)
    , _dcDragIdOffset :: Maybe Int
    , _dcCheckerCfg :: [CheckerboardCfg]
    , _dcOnFocusReq :: [Path -> WidgetRequest s e]
    , _dcOnBlurReq :: [Path -> WidgetRequest s e]
    , _dcOnChangeReq :: [Info a -> WidgetRequest s e]
    }

instance Default (DragboardCfg s e a) where
    def = DragboardCfg
        { _dcMoveValidator = Nothing
        , _dcDragIdOffset = Nothing
        , _dcCheckerCfg = []
        , _dcOnFocusReq = []
        , _dcOnBlurReq = []
        , _dcOnChangeReq = []
        }

instance Semigroup (DragboardCfg s e a) where
    (<>) a1 a2 = def
        { _dcMoveValidator =
            _dcMoveValidator a1 <|> _dcMoveValidator a2
        , _dcDragIdOffset =
            _dcDragIdOffset a1 <|> _dcDragIdOffset a2
        , _dcCheckerCfg = _dcCheckerCfg a1 <> _dcCheckerCfg a2
        , _dcOnFocusReq = _dcOnFocusReq a1 <> _dcOnFocusReq a2
        , _dcOnBlurReq = _dcOnBlurReq a1 <> _dcOnBlurReq a2
        , _dcOnChangeReq = _dcOnChangeReq a1 <> _dcOnChangeReq a2
        }

instance Monoid (DragboardCfg s e a) where
    mempty = def

instance WidgetEvent e =>
    CmbOnFocus (DragboardCfg s e a) e Path where
        onFocus fn = def
            { _dcOnFocusReq = [RaiseEvent . fn]
            }

instance CmbOnFocusReq (DragboardCfg s e a) s e Path where
    onFocusReq req = def
        { _dcOnFocusReq = [req]
        }

instance WidgetEvent e =>
    CmbOnBlur (DragboardCfg s e a) e Path where
        onBlur fn = def
            { _dcOnBlurReq = [RaiseEvent . fn]
            }

instance CmbOnBlurReq (DragboardCfg s e a) s e Path where
    onBlurReq req = def
        { _dcOnBlurReq = [req]
        }

instance WidgetEvent e =>
    CmbOnChange (DragboardCfg s e a) (Info a) e where
        onChange fn = def
            { _dcOnChangeReq = [RaiseEvent . fn]
            }

instance CmbOnChangeReq (DragboardCfg s e a) s e (Info a) where
    onChangeReq req = def
        { _dcOnChangeReq = [req]
        }

{-|
Receives previous board, index of a square where item has been
dragged to and index of a square where item has been dragged from
and returns whether this move is valid or not. If move is not valid
then the board state will not change.
-}
moveValidator :: (Info a -> Bool) -> DragboardCfg s e a
moveValidator validateMove = def
    { _dcMoveValidator = Just validateMove
    }

{-|
When there are multiple dragboards, it possible to drag an item from
one dragboard to another. In order to ignore drop events caused by
foreign items or to process them properly, the dragboards should use
different offsets (for example, if each dragboard has less than
1000 squares, then multiples of 1000 can be used as offsets):

@
vgrid
    [ dragboard 3 3 boardState f
    , dragboard_ 3 3 anotherBoardState f [dragIdOffset 1000]
    , dragboard_ 3 3 yetAnotherBoardState f [dragIdOffset 2000]
    ]
@
-}
dragIdOffset :: Int -> DragboardCfg s e a
dragIdOffset offset = def
    { _dcDragIdOffset = Just offset
    }

{-|
Config options for checkerboard container which is used by
dragboard.
-}
checkerConfig :: [CheckerboardCfg] -> DragboardCfg s e a
checkerConfig config = def
    { _dcCheckerCfg = config
    }
