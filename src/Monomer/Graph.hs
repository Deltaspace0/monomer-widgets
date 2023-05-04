{-# LANGUAGE FlexibleContexts #-}

module Monomer.Graph
    ( graph
    ) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Fixed
import Data.Maybe
import Data.Text (pack)
import Monomer.Graphics.ColorTable
import Monomer.Widgets.Single
import Numeric
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
        , singleGetSizeReq = getSizeReq
        , singleRender = render
        }

    merge _ newNode _ oldState = resultNode resNode where
        resNode = newNode & L.widget .~ makeGraph points oldState

    handleEvent wenv node _ event = result where
        result = case event of
            ButtonAction p _ BtnPressed _ -> resultPoint p
            ButtonAction _ _ BtnReleased _ -> resultReleased
            Move p | isNodePressed wenv node -> resultPoint p
            WheelScroll p (Point _ wy) _ -> resultScroll p wy
            _ -> Nothing
        resultPoint = resultRender . newPoint
        resultScroll p = resultRender . newScroll p
        resultReleased = Just $ resultNode $ newNode $ state
            { _gsMousePosition = Nothing
            }
        newPoint point@(Point mx my) = newNode $ if null mp
            then state {_gsMousePosition = Just point}
            else state
                { _gsTranslation = Point (tx+mx-mx0) (ty+my-my0)
                , _gsMousePosition = Just point
                }
        newScroll (Point mx my) wy = newNode $ state
            { _gsTranslation = Point tx' ty'
            , _gsScale = Point cx' cy'
            , _gsUnit = Point ux uy
            , _gsSections = Point (getSec cx') (getSec cy')
            } where
                tx' = mx'-(mx'-tx)*cx'/cx
                ty' = my'-(my'-ty)*cy'/cy
                (mx', my') = (mx-gx-gw/2, my-gy-gh/2)
                (ux, uy) = (getUnit cx', getUnit cy')
                (cx', cy') = (cx*1.05**wy, cy*1.05**wy)
        Rect gx gy gw gh = getContentArea node style
        style = currentStyle wenv node
        getSec x = let l = 10**(mod' (logBase 10 x) 1) in
            if l >= 5 then 4 else 5
        getUnit x
            | l >= 5 = 2*d
            | l >= 2 = 5*d
            | otherwise = 10*d
            where
                l = 10**(mod' (logBase 10 x) 1)
                d = 10**(-1-(floor' $ logBase 10 x))
        Point tx ty = _gsTranslation state
        Point cx cy = _gsScale state
        Point mx0 my0 = fromJust mp
        mp = _gsMousePosition state
        resultRender n = Just $ resultReqs n [RenderOnce]
        newNode s = node & L.widget .~ makeGraph points s

    getSizeReq _ _ = (minSize 100 1, minSize 100 1)

    render wenv node renderer = do
        let style = currentStyle wenv node
            rect@(Rect gx gy gw gh) = getContentArea node style
            Point tx ty = _gsTranslation state
            Point cx cy = _gsScale state
            Point ux uy = _gsUnit state
            Point vs hs = _gsSections state
            (ox, oy) = (gx+gw/2+tx, gy+gh/2+ty)
            (p1, p2) = (Point ox gy, Point ox (gy+gh))
            (p3, p4) = (Point gx oy, Point (gx+gw) oy)
            line a b w c = drawLine renderer a b w $ Just c
            font = styleFont style
            fsize = FontSize 16
            printText p t = renderText renderer p font fsize def t
            show' n = s where
                s = if round' n == n
                    then show $ (round n :: Int)
                    else showFFloat Nothing r ""
                r = (round' $ n*bn)/bn
                bn = 1000000000
            printN p = printText p . pack . show'
        drawRect renderer rect (Just white) Nothing
        saveContext renderer
        intersectScissor renderer rect
        line p1 p2 2 black
        line p3 p4 2 black
        let (sx, sy) = (64*cx*ux, 64*cy*uy)
            verLine x = line (Point x gy) (Point x (gy+gh)) 1 black
            horLine y = line (Point gx y) (Point (gx+gw) y) 1 black
            uxl x = fromIntegral $ length $ show' $ uy*x
            clampX x = max (gx+8) $ min (gx+gw-8*(uxl x)-4) $ ox+8
            clampY = max (gy+16) $ min (gy+gh-16) $ oy+16
            verN x = printN (Point (ox+sx*x+4) clampY) $ ux*x
            horN x = printN (Point (clampX x) (oy-sy*x)) $ uy*x
        drawInAlpha renderer 0.25 $ do
            forM_ [ox, ox-sx/vs..gx] verLine
            forM_ [ox, ox+sx/vs..(gx+gw)] verLine
            forM_ [oy, oy-sy/hs..gy] horLine
            forM_ [oy, oy+sy/hs..(gy+gh)] horLine
        drawInAlpha renderer 0.5 $ do
            forM_ [ox, ox-sx..gx] verLine
            forM_ [ox, ox+sx..(gx+gw)] verLine
            forM_ [oy, oy-sy..gy] horLine
            forM_ [oy, oy+sy..(gy+gh)] horLine
        let p (x, y) = Point (64*cx*x+ox) (64*cy*(-y)+oy)
            connect (a, b) = line (p a) (p b) 2 red
            l = length points
        when (l > 1) $ forM_ (zip points (tail points)) connect
        setFillColor renderer black
        drawInAlpha renderer 0.62 $ do
            forM_ [0..100] verN
            forM_ [-1,(-2)..(-100)] verN
            forM_ [1..100] horN
            forM_ [-1,(-2)..(-100)] horN
        restoreContext renderer

    floor' :: Double -> Double
    floor' x = fromIntegral $ (floor x :: Int)

    round' :: Double -> Double
    round' x = fromIntegral $ (round x :: Int)
