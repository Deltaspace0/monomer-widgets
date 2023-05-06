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
    , graphColors
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
- 'graphColors': which colors should be used to plot graphs.
-}
data GraphCfg = GraphCfg
    { _gcWheelRate :: Maybe Double
    , _gcLockX :: Maybe Bool
    , _gcLockY :: Maybe Bool
    , _gcHideMinor :: Maybe Bool
    , _gcGraphColors :: Maybe [Color]
    }

instance Default GraphCfg where
    def = GraphCfg
        { _gcWheelRate = Nothing
        , _gcLockX = Nothing
        , _gcLockY = Nothing
        , _gcHideMinor = Nothing
        , _gcGraphColors = Nothing
        }

instance Semigroup GraphCfg where
    (<>) a1 a2 = def
        { _gcWheelRate = _gcWheelRate a2 <|> _gcWheelRate a1
        , _gcLockX = _gcLockX a2 <|> _gcLockX a1
        , _gcLockY = _gcLockY a2 <|> _gcLockY a1
        , _gcHideMinor = _gcHideMinor a2 <|> _gcHideMinor a1
        , _gcGraphColors = _gcGraphColors a2 <|> _gcGraphColors a1
        }

instance Monoid GraphCfg where
    mempty = def

instance CmbWheelRate GraphCfg Double where
    wheelRate rate = def {
        _gcWheelRate = Just rate
    }

{-|
Lock X-axis (scale only Y-axis).
-}
lockX :: GraphCfg
lockX = lockX_ True

{-|
Whether X-axis is locked and only Y-axis is scaled.
-}
lockX_ :: Bool -> GraphCfg
lockX_ lock = def
    { _gcLockX = Just lock
    }

{-|
Lock Y-axis (scale only X-axis).
-}
lockY :: GraphCfg
lockY = lockY_ True

{-|
Whether Y-axis is locked and only X-axis is scaled.
-}
lockY_ :: Bool -> GraphCfg
lockY_ lock = def
    { _gcLockY = Just lock
    }

{-|
Hide minor gridlines.
-}
hideMinorGridlines :: GraphCfg
hideMinorGridlines = hideMinorGridlines_ True

{-|
Whether to hide minor gridlines.
-}
hideMinorGridlines_ :: Bool -> GraphCfg
hideMinorGridlines_ hide = def
    { _gcHideMinor = Just hide
    }

{-|
List of colors which are used to plot graphs. This list is then
cycled when plotting graphs (in case there are more graphs than
provided colors).
-}
graphColors :: [Color] -> GraphCfg
graphColors colors = def
    { _gcGraphColors = Just colors
    }
