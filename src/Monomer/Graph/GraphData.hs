module Monomer.Graph.GraphData
    ( GraphData(..)
    , graphPoint
    , graphPoints
    , graphColor
    , graphWidth
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Monomer.Graphics.Types

data GraphData = GraphData
    { _gdPoints :: [(Double, Double)]
    , _gdColor :: Maybe Color
    , _gdWidth :: Maybe Double
    }

instance Default GraphData where
    def = GraphData
        { _gdPoints = []
        , _gdColor = Nothing
        , _gdWidth = Nothing
        }

instance Semigroup GraphData where
    (<>) a1 a2 = def
        { _gdPoints = _gdPoints a1 <> _gdPoints a2
        , _gdColor = _gdColor a2 <|> _gdColor a1
        , _gdWidth = _gdWidth a2 <|> _gdWidth a1
        }

instance Monoid GraphData where
    mempty = def

graphPoint :: (Double, Double) -> GraphData
graphPoint point = graphPoints [point]

graphPoints :: [(Double, Double)] -> GraphData
graphPoints points = def
    { _gdPoints = points
    }

graphColor :: Color -> GraphData
graphColor color = def
    { _gdColor = Just color
    }

graphWidth :: Double -> GraphData
graphWidth width = def
    { _gdWidth = Just width
    }