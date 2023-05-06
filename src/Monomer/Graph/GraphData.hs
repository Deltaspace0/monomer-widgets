module Monomer.Graph.GraphData
    ( GraphData(..)
    , graphPoint
    , graphPoints
    , graphColor
    , graphWidth
    , graphSeparate
    , graphSeparate_
    , graphFill
    , graphFill_
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Monomer.Graphics.Types

{-|
Options for graph data:

- 'graphPoint': render single point.
- 'graphPoints': use multiple points.
- 'graphColor': set the color.
- 'graphWidth': set the width of the line.
- 'graphSeparate': whether the points should be rendered separately.
- 'graphFill': whether to fill the area surrounded by points.
-}
data GraphData = GraphData
    { _gdPoints :: [(Double, Double)]
    , _gdColor :: Maybe Color
    , _gdWidth :: Maybe Double
    , _gdSeparate :: Maybe Bool
    , _gdFill :: Maybe Bool
    }

instance Default GraphData where
    def = GraphData
        { _gdPoints = []
        , _gdColor = Nothing
        , _gdWidth = Nothing
        , _gdSeparate = Nothing
        , _gdFill = Nothing
        }

instance Semigroup GraphData where
    (<>) a1 a2 = def
        { _gdPoints = _gdPoints a1 <> _gdPoints a2
        , _gdColor = _gdColor a2 <|> _gdColor a1
        , _gdWidth = _gdWidth a2 <|> _gdWidth a1
        , _gdSeparate = _gdSeparate a2 <|> _gdSeparate a1
        , _gdFill = _gdFill a2 <|> _gdFill a1
        }

instance Monoid GraphData where
    mempty = def

{-|
Render single point.
-}
graphPoint :: (Double, Double) -> GraphData
graphPoint point = graphPoints [point]

{-|
Use multiple points.
-}
graphPoints :: [(Double, Double)] -> GraphData
graphPoints points = def
    { _gdPoints = points
    }

{-|
Set the color (if this option is not used then the graph will not be
rendered).
-}
graphColor :: Color -> GraphData
graphColor color = def
    { _gdColor = Just color
    }

{-|
Width of the line connecting provided points. If only single point
is rendered then its radius will be twice the width.
-}
graphWidth :: Double -> GraphData
graphWidth width = def
    { _gdWidth = Just width
    }

{-|
Do not connect the points and render them separately. Used when all
points in the collection must have the same color.
-}
graphSeparate :: GraphData
graphSeparate = graphSeparate_ True

{-|
Whether the points should be rendered separately.
-}
graphSeparate_ :: Bool -> GraphData
graphSeparate_ separate = def
    { _gdSeparate = Just separate
    }

{-|
Fill the area surrounded by provided points with the color.
-}
graphFill :: GraphData
graphFill = graphFill_ True

{-|
Whether to fill the area surrounded by provided points.
-}
graphFill_ :: Bool -> GraphData
graphFill_ v = def
    { _gdFill = Just v
    }
