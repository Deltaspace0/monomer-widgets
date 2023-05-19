{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Graph.GraphCfg
    ( -- * Configuration
      GraphCfg(..)
    , minTranslationX
    , maxTranslationX
    , minTranslationY
    , maxTranslationY
    , minScaleX
    , maxScaleX
    , minScaleY
    , maxScaleY
    , lockX
    , lockX_
    , lockY
    , lockY_
    , hideMinorGridlines
    , hideMinorGridlines_
    , hideAxisNumbers
    , hideAxisNumbers_
    , hideGrid
    , hideGrid_
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
- 'minTranslationX': minimum translation along X-axis.
- 'maxTranslationX': maximum translation along X-axis.
- 'minTranslationY': minimum translation along Y-axis.
- 'maxTranslationY': maximum translation along Y-axis.
- 'minScaleX': minimum scale along X-axis.
- 'maxScaleX': maximum scale along X-axis.
- 'minScaleY': minimum scale along Y-axis.
- 'maxScaleY': maximum scale along Y-axis.
- 'lockX': lock X-axis (scale only Y-axis).
- 'lockY': lock Y-axis (scale only X-axis).
- 'hideMinorGridlines': whether to hide minor gridlines.
- 'hideAxisNumbers': whether to hide axis numbers.
- 'hideGrid': whether to hide all gridlines and axis numbers.
- 'graphColors': which colors should be used to plot graphs.
- 'onRightClick': event to raise on right click.
- 'onRightClickReq': 'WidgetRequest' to generate on right click.
-}
data GraphCfg s e = GraphCfg
    { _gcWheelRate :: Maybe Double
    , _gcMinTransX :: Maybe Double
    , _gcMaxTransX :: Maybe Double
    , _gcMinTransY :: Maybe Double
    , _gcMaxTransY :: Maybe Double
    , _gcMinScaleX :: Maybe Double
    , _gcMaxScaleX :: Maybe Double
    , _gcMinScaleY :: Maybe Double
    , _gcMaxScaleY :: Maybe Double
    , _gcLockX :: Maybe Bool
    , _gcLockY :: Maybe Bool
    , _gcHideMinor :: Maybe Bool
    , _gcHideNumbers :: Maybe Bool
    , _gcHideGrid :: Maybe Bool
    , _gcGraphColors :: Maybe [Color]
    , _gcOnRightClickReq :: [(Double, Double) -> WidgetRequest s e]
    }

instance Default (GraphCfg s e) where
    def = GraphCfg
        { _gcWheelRate = Nothing
        , _gcMinTransX = Nothing
        , _gcMaxTransX = Nothing
        , _gcMinTransY = Nothing
        , _gcMaxTransY = Nothing
        , _gcMinScaleX = Nothing
        , _gcMaxScaleX = Nothing
        , _gcMinScaleY = Nothing
        , _gcMaxScaleY = Nothing
        , _gcLockX = Nothing
        , _gcLockY = Nothing
        , _gcHideMinor = Nothing
        , _gcHideNumbers = Nothing
        , _gcHideGrid = Nothing
        , _gcGraphColors = Nothing
        , _gcOnRightClickReq = []
        }

instance Semigroup (GraphCfg s e) where
    (<>) a1 a2 = def
        { _gcWheelRate = _gcWheelRate a2 <|> _gcWheelRate a1
        , _gcMinTransX = _gcMinTransX a2 <|> _gcMinTransX a1
        , _gcMaxTransX = _gcMaxTransX a2 <|> _gcMaxTransX a1
        , _gcMinTransY = _gcMinTransY a2 <|> _gcMinTransY a1
        , _gcMaxTransY = _gcMaxTransY a2 <|> _gcMaxTransY a1
        , _gcMinScaleX = _gcMinScaleX a2 <|> _gcMinScaleX a1
        , _gcMaxScaleX = _gcMaxScaleX a2 <|> _gcMaxScaleX a1
        , _gcMinScaleY = _gcMinScaleY a2 <|> _gcMinScaleY a1
        , _gcMaxScaleY = _gcMaxScaleY a2 <|> _gcMaxScaleY a1
        , _gcLockX = _gcLockX a2 <|> _gcLockX a1
        , _gcLockY = _gcLockY a2 <|> _gcLockY a1
        , _gcHideMinor = _gcHideMinor a2 <|> _gcHideMinor a1
        , _gcHideNumbers = _gcHideNumbers a2 <|> _gcHideNumbers a1
        , _gcHideGrid = _gcHideGrid a2 <|> _gcHideGrid a1
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
Minimum translation along X-axis.
-}
minTranslationX :: Double -> GraphCfg s e
minTranslationX v = def
    { _gcMinTransX = Just v
    }

{-|
Maximum translation along X-axis.
-}
maxTranslationX :: Double -> GraphCfg s e
maxTranslationX v = def
    { _gcMaxTransX = Just v
    }

{-|
Minimum translation along Y-axis.
-}
minTranslationY :: Double -> GraphCfg s e
minTranslationY v = def
    { _gcMinTransY = Just v
    }

{-|
Maximum translation along Y-axis.
-}
maxTranslationY :: Double -> GraphCfg s e
maxTranslationY v = def
    { _gcMaxTransY = Just v
    }

{-|
Minimum scale along X-axis.
-}
minScaleX :: Double -> GraphCfg s e
minScaleX v = def
    { _gcMinScaleX = Just v
    }

{-|
Maximum scale along X-axis.
-}
maxScaleX :: Double -> GraphCfg s e
maxScaleX v = def
    { _gcMaxScaleX = Just v
    }

{-|
Minimum scale along Y-axis.
-}
minScaleY :: Double -> GraphCfg s e
minScaleY v = def
    { _gcMinScaleY = Just v
    }

{-|
Maximum scale along Y-axis.
-}
maxScaleY :: Double -> GraphCfg s e
maxScaleY v = def
    { _gcMaxScaleY = Just v
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
Hide all gridlines and axis numbers.
-}
hideGrid :: GraphCfg s e
hideGrid = hideGrid_ True

{-|
Whether to hide all gridlines and axis numbers.
-}
hideGrid_ :: Bool -> GraphCfg s e
hideGrid_ hide = def
    { _gcHideGrid = Just hide
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
