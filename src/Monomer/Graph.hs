module Monomer.Graph
    ( graph
    ) where

import Control.Monad
import Data.Default
import Monomer.Graphics.ColorTable
import Monomer.Widgets.Single

graph
    :: [(Double, Double)]
    -> WidgetNode s e
graph points = node where
    node = defaultWidgetNode (WidgetType "graph") widget
    widget = makeGraph points

makeGraph
    :: [(Double, Double)]
    -> Widget s e
makeGraph points = widget where
    widget = createSingle () def
        { singleRender = render
        }

    render wenv node renderer = do
        let style = currentStyle wenv node
            rect@(Rect gx gy gw gh) = getContentArea node style
            (ox, oy) = (gx+gw/2, gy+gh/2)
            (p1, p2) = (Point ox gy, Point ox (gy+gh))
            (p3, p4) = (Point gx oy, Point (gx+gw) oy)
            line a b w c = drawLine renderer a b w $ Just c
            scissor = drawInScissor renderer True rect
        drawRect renderer rect (Just white) Nothing
        line p1 p2 2 black
        line p3 p4 2 black
        let (sx, sy) = (90, 90)
            p (x, y) = Point (sx*x+ox) (sy*(-y)+oy)
            f (a, b) = line (p a) (p b) 2 red
            l = length points
            verLine x = line (Point x gy) (Point x (gy+gh)) 1 black
            horLine y = line (Point gx y) (Point (gx+gw) y) 1 black
        drawInAlpha renderer 0.25 $ do
            forM_ [ox, ox-sx/5..gx] verLine
            forM_ [ox, ox+sx/5..(gx+gw)] verLine
            forM_ [oy, oy-sy/5..gy] horLine
            forM_ [oy, oy+sy/5..(gy+gh)] horLine
        drawInAlpha renderer 0.5 $ do
            forM_ [ox, ox-sx..gx] verLine
            forM_ [ox, ox+sx..(gx+gw)] verLine
            forM_ [oy, oy-sy..gy] horLine
            forM_ [oy, oy+sy..(gy+gh)] horLine
        when (l > 1) $ scissor $ forM_ (zip points (tail points)) f
