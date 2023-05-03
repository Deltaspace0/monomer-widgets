{-# LANGUAGE FlexibleContexts #-}

module Monomer.Graph
    ( graph
    ) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Monomer.Graphics.ColorTable
import Monomer.Widgets.Single
import qualified Monomer.Lens as L

import Monomer.Graph.GraphState

graph
    :: [(Double, Double)]
    -> WidgetNode s e
graph points = node where
    node = defaultWidgetNode (WidgetType "graph") widget
    widget = makeGraph points def

makeGraph
    :: [(Double, Double)]
    -> GraphState
    -> Widget s e
makeGraph points state = widget where
    widget = createSingle state def
        { singleMerge = merge
        , singleHandleEvent = handleEvent
        , singleRender = render
        }

    merge _ newNode _ oldState = resultNode resNode where
        resNode = newNode & L.widget .~ makeGraph points oldState

    handleEvent wenv node _ event = result where
        result = case event of
            ButtonAction p _ BtnPressed _ -> resultPoint p
            ButtonAction _ _ BtnReleased _ -> resultReleased
            Move p | isNodePressed wenv node -> resultPoint p
            WheelScroll _ (Point _ wy) _ -> resultScroll wy
            _ -> Nothing
        resultPoint = resultRender . newPoint
        resultScroll = resultRender . newScroll
        resultReleased = Just $ resultNode $ newNode $ state
            { _gsMousePosition = Nothing
            }
        newPoint point@(Point mx my) = newNode $ if null mp
            then state {_gsMousePosition = Just point}
            else state
                { _gsTranslation = Point (tx+mx-mx0) (ty+my-my0)
                , _gsMousePosition = Just point
                }
        newScroll wy = newNode $ state
            { _gsScale = Point (cx*1.1**wy) (cy*1.1**wy)
            }
        Point tx ty = _gsTranslation state
        Point cx cy = _gsScale state
        Point mx0 my0 = fromJust mp
        mp = _gsMousePosition state
        resultRender n = Just $ resultReqs n [RenderOnce]
        newNode s = node & L.widget .~ makeGraph points s

    render wenv node renderer = do
        let style = currentStyle wenv node
            rect@(Rect gx gy gw gh) = getContentArea node style
            Point tx ty = _gsTranslation state
            Point cx cy = _gsScale state
            (ox, oy) = (gx+gw/2+tx, gy+gh/2+ty)
            (p1, p2) = (Point ox gy, Point ox (gy+gh))
            (p3, p4) = (Point gx oy, Point (gx+gw) oy)
            line a b w c = drawLine renderer a b w $ Just c
        drawRect renderer rect (Just white) Nothing
        saveContext renderer
        intersectScissor renderer rect
        line p1 p2 2 black
        line p3 p4 2 black
        let (sx, sy) = (100*cx, 100*cy)
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
        when (l > 1) $ forM_ (zip points (tail points)) f
        restoreContext renderer
