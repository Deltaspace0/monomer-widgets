module Monomer.Graph.GraphData
    ( GraphData(..)
    , graphPoint
    , graphPoints
    , graphColor
    , graphHoverColor
    , graphActiveColor
    , graphWidth
    , graphSeparate
    , graphSeparate_
    , graphFill
    , graphFill_
    , graphFillAlpha
    , graphOnChange
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Monomer.Graphics.Types

{-|
Options for graph data:

- 'graphPoint': render single point.
- 'graphPoints': use multiple points.
- 'graphColor': set the color.
- 'graphHoverColor': set the color of hovered point.
- 'graphActiveColor': set the color of dragged point.
- 'graphWidth': set the width of the line.
- 'graphSeparate': whether the points should be rendered separately.
- 'graphFill': whether to fill the area surrounded by points.
- 'graphFillAlpha': transparency level of the filled area.
- 'graphOnChange': event to raise when a point is dragged.
-}
data GraphData e = GraphData
    { _gdPoints :: [(Double, Double)]
    , _gdColor :: Maybe Color
    , _gdHoverColor :: Maybe Color
    , _gdActiveColor :: Maybe Color
    , _gdWidth :: Maybe Double
    , _gdSeparate :: Maybe Bool
    , _gdFill :: Maybe Bool
    , _gdFillAlpha :: Maybe Double
    , _gdOnChange :: Maybe (Int -> (Double, Double) -> e)
    }

instance Default (GraphData e) where
    def = GraphData
        { _gdPoints = []
        , _gdColor = Nothing
        , _gdHoverColor = Nothing
        , _gdActiveColor = Nothing
        , _gdWidth = Nothing
        , _gdSeparate = Nothing
        , _gdFill = Nothing
        , _gdFillAlpha = Nothing
        , _gdOnChange = Nothing
        }

instance Semigroup (GraphData e) where
    (<>) a1 a2 = def
        { _gdPoints = _gdPoints a1 <> _gdPoints a2
        , _gdColor = _gdColor a2 <|> _gdColor a1
        , _gdHoverColor = _gdHoverColor a2 <|> _gdHoverColor a1
        , _gdActiveColor = _gdActiveColor a2 <|> _gdActiveColor a1
        , _gdWidth = _gdWidth a2 <|> _gdWidth a1
        , _gdSeparate = _gdSeparate a2 <|> _gdSeparate a1
        , _gdFill = _gdFill a2 <|> _gdFill a1
        , _gdFillAlpha = _gdFillAlpha a2 <|> _gdFillAlpha a1
        , _gdOnChange = _gdOnChange a2 <|> _gdOnChange a1
        }

instance Monoid (GraphData e) where
    mempty = def

{-|
Render single point.
-}
graphPoint :: (Double, Double) -> GraphData e
graphPoint point = graphPoints [point]

{-|
Use multiple points.
-}
graphPoints :: [(Double, Double)] -> GraphData e
graphPoints points = def
    { _gdPoints = points
    }

{-|
Set the color (if this option is not used then the graph will not be
rendered).
-}
graphColor :: Color -> GraphData e
graphColor color = def
    { _gdColor = Just color
    }

{-|
Set the color of hovered point (if this option is not used then
the color set by 'graphColor' is used).
-}
graphHoverColor :: Color -> GraphData e
graphHoverColor color = def
    { _gdHoverColor = Just color
    }

{-|
Set the color of dragged point (if this option is not used then
the color set by 'graphColor' is used).
-}
graphActiveColor :: Color -> GraphData e
graphActiveColor color = def
    { _gdActiveColor = Just color
    }

{-|
Width of the line connecting provided points. If only single point
is rendered then its radius will be twice the width.
-}
graphWidth :: Double -> GraphData e
graphWidth width = def
    { _gdWidth = Just width
    }

{-|
Do not connect the points and render them separately. Used when all
points in the collection must have the same color.
-}
graphSeparate :: GraphData e
graphSeparate = graphSeparate_ True

{-|
Whether the points should be rendered separately.
-}
graphSeparate_ :: Bool -> GraphData e
graphSeparate_ separate = def
    { _gdSeparate = Just separate
    }

{-|
Fill the area surrounded by provided points with the color.
-}
graphFill :: GraphData e
graphFill = graphFill_ True

{-|
Whether to fill the area surrounded by provided points.
-}
graphFill_ :: Bool -> GraphData e
graphFill_ v = def
    { _gdFill = Just v
    }

{-|
Transparency level of the filled area.
-}
graphFillAlpha :: Double -> GraphData e
graphFillAlpha alpha = def
    { _gdFillAlpha = Just alpha
    }

{-|
Raises an event when a point is dragged by passing its index and
new coordinates. This option is ignored if 'graphSeparate' is not
enabled.
-}
graphOnChange :: (Int -> (Double, Double) -> e) -> GraphData e
graphOnChange v = def
    { _gdOnChange = Just v
    }
