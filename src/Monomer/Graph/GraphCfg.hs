{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Graph.GraphCfg
    ( -- * Configuration
      GraphCfg(..)
    , limitX
    , limitY
    , minimumX
    , maximumX
    , minimumY
    , maximumY
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
- 'limitX': limits along X-axis.
- 'limitY': limits along Y-axis.
- 'minimumX': left limit along X-axis.
- 'maximumX': right limit along X-axis.
- 'minimumY': bottom limit along Y-axis.
- 'maximumY': top limit along Y-axis.
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
    , _gcMinX :: Maybe Double
    , _gcMaxX :: Maybe Double
    , _gcMinY :: Maybe Double
    , _gcMaxY :: Maybe Double
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
        , _gcMinX = Nothing
        , _gcMaxX = Nothing
        , _gcMinY = Nothing
        , _gcMaxY = Nothing
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
        , _gcMinX = _gcMinX a2 <|> _gcMinX a1
        , _gcMaxX = _gcMaxX a2 <|> _gcMaxX a1
        , _gcMinY = _gcMinY a2 <|> _gcMinY a1
        , _gcMaxY = _gcMaxY a2 <|> _gcMaxY a1
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
Limits along X-axis.
-}
limitX :: (Double, Double) -> GraphCfg s e
limitX (a, b) = def
    { _gcMinX = Just a
    , _gcMaxX = Just b
    }

{-|
Limits along Y-axis.
-}
limitY :: (Double, Double) -> GraphCfg s e
limitY (a, b) = def
    { _gcMinY = Just a
    , _gcMaxY = Just b
    }

{-|
Left limit along X-axis.
-}
minimumX :: Double -> GraphCfg s e
minimumX v = def
    { _gcMinX = Just v
    }

{-|
Right limit along X-axis.
-}
maximumX :: Double -> GraphCfg s e
maximumX v = def
    { _gcMaxX = Just v
    }

{-|
Bottom limit along Y-axis.
-}
minimumY :: Double -> GraphCfg s e
minimumY v = def
    { _gcMinY = Just v
    }

{-|
Top limit along Y-axis.
-}
maximumY :: Double -> GraphCfg s e
maximumY v = def
    { _gcMaxY = Just v
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
