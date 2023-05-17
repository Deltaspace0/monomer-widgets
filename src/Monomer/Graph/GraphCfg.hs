{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Graph.GraphCfg
    ( -- * Configuration
      GraphCfg(..)
    , lockX
    , lockX_
    , lockY
    , lockY_
    , hideMinorGridlines
    , hideMinorGridlines_
    , hideAxisNumbers
    , hideAxisNumbers_
    , graphColors
    , onRightClick
    , onRightClickReq
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Monomer.Widgets.Single

{-|
Configuration options for graph:

- 'wheelRate': speed of scaling.
- 'lockX': lock X-axis (scale only Y-axis).
- 'lockY': lock Y-axis (scale only X-axis).
- 'hideMinorGridlines': whether to hide minor gridlines.
- 'hideAxisNumbers': whether to hide axis numbers.
- 'graphColors': which colors should be used to plot graphs.
- 'onRightClick': event to raise on right click.
- 'onRightClickReq': 'WidgetRequest' to generate on right click.
-}
data GraphCfg s e = GraphCfg
    { _gcWheelRate :: Maybe Double
    , _gcLockX :: Maybe Bool
    , _gcLockY :: Maybe Bool
    , _gcHideMinor :: Maybe Bool
    , _gcHideNumbers :: Maybe Bool
    , _gcGraphColors :: Maybe [Color]
    , _gcOnRightClickReq :: [(Double, Double) -> WidgetRequest s e]
    }

instance Default (GraphCfg s e) where
    def = GraphCfg
        { _gcWheelRate = Nothing
        , _gcLockX = Nothing
        , _gcLockY = Nothing
        , _gcHideMinor = Nothing
        , _gcHideNumbers = Nothing
        , _gcGraphColors = Nothing
        , _gcOnRightClickReq = []
        }

instance Semigroup (GraphCfg s e) where
    (<>) a1 a2 = def
        { _gcWheelRate = _gcWheelRate a2 <|> _gcWheelRate a1
        , _gcLockX = _gcLockX a2 <|> _gcLockX a1
        , _gcLockY = _gcLockY a2 <|> _gcLockY a1
        , _gcHideMinor = _gcHideMinor a2 <|> _gcHideMinor a1
        , _gcHideNumbers = _gcHideNumbers a2 <|> _gcHideNumbers a1
        , _gcGraphColors = _gcGraphColors a2 <|> _gcGraphColors a1
        , _gcOnRightClickReq =
            _gcOnRightClickReq a1 <> _gcOnRightClickReq a2
        }

instance Monoid (GraphCfg s e) where
    mempty = def

instance CmbWheelRate (GraphCfg s e) Double where
    wheelRate rate = def {
        _gcWheelRate = Just rate
    }

{-|
Lock X-axis (scale only Y-axis).
-}
lockX :: GraphCfg s e
lockX = lockX_ True

{-|
Whether X-axis is locked and only Y-axis is scaled.
-}
lockX_ :: Bool -> GraphCfg s e
lockX_ lock = def
    { _gcLockX = Just lock
    }

{-|
Lock Y-axis (scale only X-axis).
-}
lockY :: GraphCfg s e
lockY = lockY_ True

{-|
Whether Y-axis is locked and only X-axis is scaled.
-}
lockY_ :: Bool -> GraphCfg s e
lockY_ lock = def
    { _gcLockY = Just lock
    }

{-|
Hide minor gridlines.
-}
hideMinorGridlines :: GraphCfg s e
hideMinorGridlines = hideMinorGridlines_ True

{-|
Whether to hide minor gridlines.
-}
hideMinorGridlines_ :: Bool -> GraphCfg s e
hideMinorGridlines_ hide = def
    { _gcHideMinor = Just hide
    }

{-|
Hide axis numbers.
-}
hideAxisNumbers :: GraphCfg s e
hideAxisNumbers = hideAxisNumbers_ True

{-|
Whether to hide axis numbers.
-}
hideAxisNumbers_ :: Bool -> GraphCfg s e
hideAxisNumbers_ hide = def
    { _gcHideNumbers = Just hide
    }

{-|
List of colors which are used to plot graphs. This list is then
cycled when plotting graphs (in case there are more graphs than
provided colors).
-}
graphColors :: [Color] -> GraphCfg s e
graphColors colors = def
    { _gcGraphColors = Just colors
    }

{-|
Event to raise on right click.
-}
onRightClick
    :: WidgetEvent e
    => ((Double, Double) -> e)
    -> GraphCfg s e
onRightClick f = def
    { _gcOnRightClickReq = [RaiseEvent . f]
    }

{-|
'WidgetRequest' to generate on right click.
-}
onRightClickReq
    :: ((Double, Double) -> WidgetRequest s e)
    -> GraphCfg s e
onRightClickReq req = def
    { _gcOnRightClickReq = [req]
    }
