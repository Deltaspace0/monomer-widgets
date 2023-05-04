{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Graph.GraphCfg
    ( GraphCfg(..)
    , lockX
    , lockX_
    , lockY
    , lockY_
    , graphColors
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Monomer.Widgets.Single

data GraphCfg = GraphCfg
    { _gcWheelRate :: Maybe Double
    , _gcLockX :: Maybe Bool
    , _gcLockY :: Maybe Bool
    , _gcGraphColors :: Maybe [Color]
    }

instance Default GraphCfg where
    def = GraphCfg
        { _gcWheelRate = Nothing
        , _gcLockX = Nothing
        , _gcLockY = Nothing
        , _gcGraphColors = Nothing
        }

instance Semigroup GraphCfg where
    (<>) a1 a2 = def
        { _gcWheelRate = _gcWheelRate a2 <|> _gcWheelRate a1
        , _gcLockX = _gcLockX a2 <|> _gcLockX a1
        , _gcLockY = _gcLockY a2 <|> _gcLockY a1
        , _gcGraphColors = _gcGraphColors a2 <|> _gcGraphColors a1
        }

instance Monoid GraphCfg where
    mempty = def

instance CmbWheelRate GraphCfg Double where
    wheelRate rate = def {
        _gcWheelRate = Just rate
    }

lockX :: GraphCfg
lockX = lockX_ True

lockX_ :: Bool -> GraphCfg
lockX_ lock = def
    { _gcLockX = Just lock
    }

lockY :: GraphCfg
lockY = lockY_ True

lockY_ :: Bool -> GraphCfg
lockY_ lock = def
    { _gcLockY = Just lock
    }

graphColors :: [Color] -> GraphCfg
graphColors colors = def
    { _gcGraphColors = Just colors
    }
