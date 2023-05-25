module Monomer.Graph.GraphData
    ( GraphData(..)
    , graphPoint
    , graphPoints
    , graphColor
    , graphHoverColor
    , graphActiveColor
    , graphBorderColor
    , graphWidth
    , graphRadius
    , graphSeparate
    , graphSeparate_
    , graphFill
    , graphFill_
    , graphFillAlpha
    , graphDuration
    , graphOnFinished
    , graphOnFinishedReq
    , graphOnChange
    , graphOnChangeReq
    , graphOnEnter
    , graphOnEnterReq
    , graphOnLeave
    , graphOnLeaveReq
    , graphOnClick
    , graphOnClickReq
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Monomer.Graphics.Types
import Monomer.Widgets.Single

{-|
Options for graph data:

- 'graphPoint': render single point.
- 'graphPoints': use multiple points.
- 'graphColor': set the color.
- 'graphHoverColor': set the color of hovered point.
- 'graphActiveColor': set the color of dragged point.
- 'graphBorderColor': set the color of point border.
- 'graphWidth': set the width of the line.
- 'graphRadius': set the radius of the points.
- 'graphSeparate': whether the points should be rendered separately.
- 'graphFill': whether to fill the area surrounded by points.
- 'graphFillAlpha': transparency level of the filled area.
- 'graphDuration': how long the animation lasts in ms.
- 'graphOnFinished': event to raise when animation is complete.
- 'graphOnFinishedReq': 'WidgetRequest' to generate when animation
is complete.
- 'graphOnChange': event to raise when a point is dragged.
- 'graphOnChangeReq': 'WidgetRequest' to generate when a point is
dragged.
- 'graphOnEnter': event to raise when mouse enters point area.
- 'graphOnEnterReq': 'WidgetRequest' to generate when mouse enters
point area.
- 'graphOnLeave': event to raise when mouse leaves point area.
- 'graphOnLeaveReq': 'WidgetRequest' to generate when mouse leaves
point area.
- 'graphOnClick': event to raise when a point is clicked.
- 'graphOnClickReq': 'WidgetRequest' to generate when a point is
clicked.
-}
data GraphData s e = GraphData
    { _gdPoints :: [(Double, Double)]
    , _gdColor :: Maybe Color
    , _gdHoverColor :: Maybe Color
    , _gdActiveColor :: Maybe Color
    , _gdBorderColor :: Maybe Color
    , _gdWidth :: Maybe Double
    , _gdRadius :: Maybe Double
    , _gdSeparate :: Maybe Bool
    , _gdFill :: Maybe Bool
    , _gdFillAlpha :: Maybe Double
    , _gdDuration :: Maybe Millisecond
    , _gdFinishedReq :: [WidgetRequest s e]
    , _gdChangeReq :: [Int -> (Double, Double) -> WidgetRequest s e]
    , _gdEnterReq :: [Int -> WidgetRequest s e]
    , _gdLeaveReq :: [Int -> WidgetRequest s e]
    , _gdClickReq :: [Int -> WidgetRequest s e]
    }

instance Default (GraphData s e) where
    def = GraphData
        { _gdPoints = []
        , _gdColor = Nothing
        , _gdHoverColor = Nothing
        , _gdActiveColor = Nothing
        , _gdBorderColor = Nothing
        , _gdWidth = Nothing
        , _gdRadius = Nothing
        , _gdSeparate = Nothing
        , _gdFill = Nothing
        , _gdFillAlpha = Nothing
        , _gdDuration = Nothing
        , _gdFinishedReq = []
        , _gdChangeReq = []
        , _gdEnterReq = []
        , _gdLeaveReq = []
        , _gdClickReq = []
        }

instance Semigroup (GraphData s e) where
    (<>) a1 a2 = def
        { _gdPoints = _gdPoints a1 <> _gdPoints a2
        , _gdColor = _gdColor a2 <|> _gdColor a1
        , _gdHoverColor = _gdHoverColor a2 <|> _gdHoverColor a1
        , _gdActiveColor = _gdActiveColor a2 <|> _gdActiveColor a1
        , _gdBorderColor = _gdBorderColor a2 <|> _gdBorderColor a1
        , _gdWidth = _gdWidth a2 <|> _gdWidth a1
        , _gdRadius = _gdRadius a2 <|> _gdRadius a1
        , _gdSeparate = _gdSeparate a2 <|> _gdSeparate a1
        , _gdFill = _gdFill a2 <|> _gdFill a1
        , _gdFillAlpha = _gdFillAlpha a2 <|> _gdFillAlpha a1
        , _gdDuration = _gdDuration a2 <|> _gdDuration a1
        , _gdFinishedReq = _gdFinishedReq a1 <> _gdFinishedReq a2
        , _gdChangeReq = _gdChangeReq a1 <> _gdChangeReq a2
        , _gdEnterReq = _gdEnterReq a1 <> _gdEnterReq a2
        , _gdLeaveReq = _gdLeaveReq a1 <> _gdLeaveReq a2
        , _gdClickReq = _gdClickReq a1 <> _gdClickReq a2
        }

instance Monoid (GraphData s e) where
    mempty = def

{-|
Render single point.
-}
graphPoint :: (Double, Double) -> GraphData s e
graphPoint point = graphPoints [point]

{-|
Use multiple points.
-}
graphPoints :: [(Double, Double)] -> GraphData s e
graphPoints points = def
    { _gdPoints = points
    }

{-|
Set the color (if this option is not used then the graph will not be
rendered).
-}
graphColor :: Color -> GraphData s e
graphColor color = def
    { _gdColor = Just color
    }

{-|
Set the color of hovered point (if this option is not used then
the color set by 'graphColor' is used).
-}
graphHoverColor :: Color -> GraphData s e
graphHoverColor color = def
    { _gdHoverColor = Just color
    }

{-|
Set the color of dragged point (if this option is not used then
the color set by 'graphColor' is used).
-}
graphActiveColor :: Color -> GraphData s e
graphActiveColor color = def
    { _gdActiveColor = Just color
    }

{-|
Set the color of point border. The width of the border will be half
the width set by 'graphWidth'.
-}
graphBorderColor :: Color -> GraphData s e
graphBorderColor color = def
    { _gdBorderColor = Just color
    }

{-|
Width of the line connecting provided points. If only single point
is rendered then its radius will be twice the width.
-}
graphWidth :: Double -> GraphData s e
graphWidth w = def
    { _gdWidth = Just w
    }

{-|
Radius of the provided points if they are rendered separately. The
difference between `graphWidth` and `graphRadius` is that the former
is given in pixels and renders the same when the scale changes while
the latter is given in units of the Cartesian coordinate system and
hence the points become bigger or smaller when the scale changes.
-}
graphRadius :: Double -> GraphData s e
graphRadius r = def
    { _gdRadius = Just r
    }

{-|
Do not connect the points and render them separately. Used when all
points in the collection must have the same color.
-}
graphSeparate :: GraphData s e
graphSeparate = graphSeparate_ True

{-|
Whether the points should be rendered separately.
-}
graphSeparate_ :: Bool -> GraphData s e
graphSeparate_ separate = def
    { _gdSeparate = Just separate
    }

{-|
Fill the area surrounded by provided points with the color.
-}
graphFill :: GraphData s e
graphFill = graphFill_ True

{-|
Whether to fill the area surrounded by provided points.
-}
graphFill_ :: Bool -> GraphData s e
graphFill_ v = def
    { _gdFill = Just v
    }

{-|
Transparency level of the filled area.
-}
graphFillAlpha :: Double -> GraphData s e
graphFillAlpha alpha = def
    { _gdFillAlpha = Just alpha
    }

{-|
How long the animation lasts in ms. Animation starts when graph data
changes (for example, point positions or color).
-}
graphDuration :: Millisecond -> GraphData s e
graphDuration dur = def
    { _gdDuration = Just dur
    }

{-|
Raises an event when animation is complete.
-}
graphOnFinished :: WidgetEvent e => e -> GraphData s e
graphOnFinished handler = def
    { _gdFinishedReq = [RaiseEvent handler]
    }

{-|
Generates a 'WidgetRequest' when animation is complete.
-}
graphOnFinishedReq :: WidgetRequest s e -> GraphData s e
graphOnFinishedReq req = def
    { _gdFinishedReq = [req]
    }

{-|
Raises an event when a point is dragged by passing its index and
new coordinates. This option is ignored if 'graphSeparate' is not
enabled.
-}
graphOnChange
    :: WidgetEvent e
    => (Int -> (Double, Double) -> e)
    -> GraphData s e
graphOnChange f = def
    { _gdChangeReq = [(RaiseEvent .) . f]
    }

{-|
Generates a 'WidgetRequest' when a point is dragged by passing its
index and new coordinates. This option is ignored if 'graphSeparate'
is not enabled.
-}
graphOnChangeReq
    :: (Int -> (Double, Double) -> WidgetRequest s e)
    -> GraphData s e
graphOnChangeReq req = def
    { _gdChangeReq = [req]
    }

{-|
Raises an event when mouse enters point area by passing its index.
This option is ignored if 'graphSeparate' is not enabled.
-}
graphOnEnter :: WidgetEvent e => (Int -> e) -> GraphData s e
graphOnEnter f = def
    { _gdEnterReq = [RaiseEvent . f]
    }

{-|
Generates a 'WidgetRequest' when mouse enters point area by passing
its index. This option is ignored if 'graphSeparate' is not enabled.
-}
graphOnEnterReq :: (Int -> WidgetRequest s e) -> GraphData s e
graphOnEnterReq req = def
    { _gdEnterReq = [req]
    }

{-|
Raises an event when mouse leaves point area by passing its index.
This option is ignored if 'graphSeparate' is not enabled.
-}
graphOnLeave :: WidgetEvent e => (Int -> e) -> GraphData s e
graphOnLeave f = def
    { _gdLeaveReq = [RaiseEvent . f]
    }

{-|
Generates a 'WidgetRequest' when mouse leaves point area by passing
its index. This option is ignored if 'graphSeparate' is not enabled.
-}
graphOnLeaveReq :: (Int -> WidgetRequest s e) -> GraphData s e
graphOnLeaveReq req = def
    { _gdLeaveReq = [req]
    }

{-|
Raises an event when a point is clicked by passing its index. This
option is ignored if 'graphSeparate' is not enabled.
-}
graphOnClick :: WidgetEvent e => (Int -> e) -> GraphData s e
graphOnClick f = def
    { _gdClickReq = [RaiseEvent . f]
    }

{-|
Generates a 'WidgetRequest' when a point is clicked by passing its
index. This option is ignored if 'graphSeparate' is not enabled.
-}
graphOnClickReq :: (Int -> WidgetRequest s e) -> GraphData s e
graphOnClickReq req = def
    { _gdClickReq = [req]
    }
