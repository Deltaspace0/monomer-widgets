{-|
This widget renders a Cartesian coordinate system and plots function
graphs by connecting provided points. Coordinate system can be
dragged and scaled. It is possible to render single points too.

This widget can receive 'GraphMsg' messages:

- 'GraphSetTranslation' 'Point'
- 'GraphSetScale' 'Point'
- 'GraphReset'

@
graph [[(1,2), (1,3)], [(0,0), (1,1)]]
graphWithColors [(red, [(1,2), (1,3)]), (blue, [(0,0), (1,1)])]
graphWithData [[graphPoint (0, 0), graphColor red]]
@
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Monomer.Graph
    ( -- * Re-exported modules
      module Monomer.Graph.GraphCfg
    , module Monomer.Graph.GraphData
    , module Monomer.Graph.GraphMsg
      -- * Constructors
    , graph
    , graph_
    , graphWithColors
    , graphWithColors_
    , graphWithData
    , graphWithData_
    ) where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad
import Data.Default
import Data.Fixed
import Data.Maybe
import Data.Text (pack)
import Data.Typeable
import Monomer.Graphics.ColorTable
import Monomer.Widgets.Single
import Numeric
import qualified Monomer.Lens as L

import Monomer.Graph.GraphCfg
import Monomer.Graph.GraphData
import Monomer.Graph.GraphMsg
import Monomer.Graph.GraphState

{-|
Creates a graph plotter using the list with points.
-}
graph
    :: (WidgetEvent e)
    => [[(Double, Double)]]  -- ^ The list with points.
    -> WidgetNode s e        -- ^ The created graph plotter.
graph points = graph_ points def

{-|
Creates a graph plotter using the list with points. Accepts config.
-}
graph_
    :: (WidgetEvent e)
    => [[(Double, Double)]]  -- ^ The list with points.
    -> [GraphCfg]            -- ^ The config options.
    -> WidgetNode s e        -- ^ The created graph plotter.
graph_ points configs = graphWithColors_ colorPoints configs where
    colorPoints = zip (cycle colors') points
    colors' = fromMaybe colors _gcGraphColors
    colors = [red, green, blue, violet, yellow]
    GraphCfg{..} = mconcat configs

{-|
Creates a graph plotter using the list with colors and points.
-}
graphWithColors
    :: (WidgetEvent e)
    => [(Color, [(Double, Double)])]
    -- ^ The list with colors and points.
    -> WidgetNode s e
    -- ^ The created graph plotter.
graphWithColors colorPoints = graphWithColors_ colorPoints def

{-|
Creates a graph plotter using the list with colors and points.
Accepts config.
-}
graphWithColors_
    :: (WidgetEvent e)
    => [(Color, [(Double, Double)])]
    -- ^ The list with colors and points.
    -> [GraphCfg]
    -- ^ The config options.
    -> WidgetNode s e
    -- ^ The created graph plotter.
graphWithColors_ colorPoints configs = node where
    node = graphWithData_ (makeData <$> colorPoints) configs
    makeData (color, points) =
        [ graphPoints points
        , graphColor color
        ]

{-|
Creates a graph plotter using the list with 'GraphData'.
-}
graphWithData
    :: (WidgetEvent e)
    => [[GraphData e]]  -- ^ The list with 'GraphData'.
    -> WidgetNode s e   -- ^ The created graph plotter.
graphWithData dataList = graphWithData_ dataList def

{-|
Creates a graph plotter using the list with 'GraphData'. Accepts
config.
-}
graphWithData_
    :: (WidgetEvent e)
    => [[GraphData e]]  -- ^ The list with 'GraphData'.
    -> [GraphCfg]       -- ^ The config options.
    -> WidgetNode s e   -- ^ The created graph plotter.
graphWithData_ dataList configs = node where
    node = defaultWidgetNode (WidgetType "graph") widget
    widget = makeGraph (mconcat <$> dataList) config def
    config = mconcat configs

makeGraph
    :: (WidgetEvent e)
    => [GraphData e]
    -> GraphCfg
    -> GraphState
    -> Widget s e
makeGraph graphDatas config@(GraphCfg{..}) state = widget where
    widget = createSingle state def
        { singleGetCurrentStyle = getCurrentStyle
        , singleMerge = merge
        , singleHandleEvent = handleEvent
        , singleHandleMessage = handleMessage
        , singleGetSizeReq = getSizeReq
        , singleRender = render
        }

    getCurrentStyle wenv node = style where
        style = currentStyle wenv node & L.cursorIcon .~ cursor
        cursor = if null (_gsHoverPoint state)
            then Nothing
            else Just CursorHand

    merge _ newNode _ oldState = resultNode resNode where
        resNode = makeNodeWithState oldState newNode

    handleEvent wenv node _ event = result where
        result = case event of
            ButtonAction p _ BtnPressed _ -> resultPressed p
            ButtonAction _ _ BtnReleased _ -> resultReleased
            Move p -> handleMove wenv node p
            WheelScroll p (Point _ wy) _ -> resultScroll p wy
            _ -> Nothing
        resultPressed p = resultRender $ newNode $ state
            { _gsMousePosition = Just p
            , _gsActivePoint = _gsHoverPoint state
            }
        resultReleased = Just $ resultNode $ newNode $ state
            { _gsMousePosition = Nothing
            , _gsActivePoint = Nothing
            }
        resultScroll p = resultRender . newScroll p
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
                (cx', cy') = (cx*rateX**wy, cy*rateY**wy)
                rateX = if _gcLockX == Just True
                    then 1
                    else 1.05**wr
                rateY = if _gcLockY == Just True
                    then 1
                    else 1.05**wr
                wr = fromMaybe 1 _gcWheelRate
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
        newNode s = makeNodeWithState s node

    handleMove wenv node p@(Point x y) = result where
        result = Just $ resultReqs newNode reqs
        newNode = makeNodeWithState newState node
        newState
            | isNodePressed wenv node && dragPoint = state
            | isNodePressed wenv node && (not $ null mp) = state
                { _gsTranslation = Point (tx+x-mx0) (ty+y-my0)
                , _gsMousePosition = Just p
                }
            | otherwise = state
                { _gsHoverPoint = hp
                }
        reqs = if not dragPoint
            then [RenderOnce]
            else
                [ RaiseEvent $ (fromJust report) dj (dx, dy)
                , RenderOnce
                ]
        dragPoint = not $ (null dp || null report)
        Point tx ty = _gsTranslation state
        Point cx cy = _gsScale state
        Point mx0 my0 = fromJust mp
        mp = _gsMousePosition state
        (di, dj) = fromJust dp
        dp = _gsActivePoint state
        report = _gdOnChange $ graphDatas!!di
        hp = hoverPointData $ zip [0..] graphDatas
        hoverPointData [] = Nothing
        hoverPointData ((i, graphData):xs)
            | _gdSeparate graphData /= Just True = hoverPointData xs
            | null hp' = hoverPointData xs
            | otherwise = (,) i <$> hp'
            where
                hp' = getHP r $ zip [0..] $ _gdPoints graphData
                r = 2*(fromMaybe 2 $ _gdWidth graphData)
        getHP _ [] = Nothing
        getHP r ((j, pp):xs) = if checkHover pp r
            then Just j
            else getHP r xs
        checkHover (px, py) r = distance <= r**2 where
            distance = ((px-dx)*cx*64)**2+((py-dy)*cy*64)**2
        (dx, dy) = ((x-ox)/64/cx, (oy-y)/64/cy)
        (ox, oy) = (gx+gw/2+tx, gy+gh/2+ty)
        Rect gx gy gw gh = getContentArea node style
        style = currentStyle wenv node

    resultRender node = Just $ resultReqs node [RenderOnce]

    handleMessage _ node _ message = do
        s <- getNewState <$> cast message
        return $ resultReqs (makeNodeWithState s node) [RenderOnce]

    getNewState (GraphSetTranslation p) = state {_gsTranslation = p}
    getNewState (GraphSetScale p) = state {_gsScale = p}
    getNewState GraphReset = def

    getSizeReq _ _ = (rangeSize 100 2000 1, rangeSize 100 2000 1)

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
            horN' x = when (x /= 0) $ horN x
            (fox, foy) = (-(round' $ tx/sx), round' $ ty/sy)
            ovx = ox-(round' $ tx*vs/sx)*sx/vs
            ovy = oy-(round' $ ty*hs/sy)*sy/hs
            ovx1 = ox+fox*sx
            ovy1 = oy-foy*sy
        when (_gcHideMinor /= Just True) $
            drawInAlpha renderer 0.2 $ do
                forM_ [ovx, ovx-sx/vs..gx] verLine
                forM_ [ovx, ovx+sx/vs..(gx+gw)] verLine
                forM_ [ovy, ovy-sy/hs..gy] horLine
                forM_ [ovy, ovy+sy/hs..(gy+gh)] horLine
        drawInAlpha renderer 0.5 $ do
            forM_ [ovx1, ovx1-sx..gx] verLine
            forM_ [ovx1, ovx1+sx..(gx+gw)] verLine
            forM_ [ovy1, ovy1-sy..gy] horLine
            forM_ [ovy1, ovy1+sy..(gy+gh)] horLine
        let p (x, y) = (64*cx*x+ox, 64*cy*(-y)+oy)
        forM_ (zip [0..] graphDatas) $ \(i, graphData) -> do
            let GraphData{..} = graphData
                ps = p <$> _gdPoints
                newGraphData = graphData {_gdPoints = ps}
            when (not $ null _gdColor) $
                renderGraphData renderer newGraphData i
        when (_gcHideNumbers /= Just True) $ do
            setFillColor renderer black
            drawInAlpha renderer 0.62 $ do
                forM_ [fox..(fox+20)] verN
                forM_ [(fox-1),(fox-2)..(fox-20)] verN
                forM_ [foy..(foy+20)] horN'
                forM_ [(foy-1),(foy-2)..(foy-20)] horN'
        restoreContext renderer

    renderGraphData renderer GraphData{..} i = do
        let GraphState{..} = state
            ps = _gdPoints
            c = _gdColor
            w = fromMaybe 2 _gdWidth
            alpha = fromMaybe 0.32 _gdFillAlpha
            p (x, y) = Point x y
            connect (a, b) = drawLine renderer (p a) (p b) w c
            drawDot cs (Point x y) = drawEllipse renderer r cs where
                r = Rect (x-w*2) (y-w*2) (w*4) (w*4)
            getSeparateColor j
                | _gsActivePoint == Just (i, j) = _gdActiveColor
                | _gsHoverPoint == Just (i, j) = _gdHoverColor
                | otherwise = Nothing
            l = length ps
        if _gdSeparate == Just True
            then forM_ (zip [0..] ps) $ \(j, q) ->
                drawDot (getSeparateColor j <|> c) $ p q
            else do
                when (l > 1) $ forM_ (zip ps (tail ps)) connect
                when (l == 1) $ drawDot c $ p $ head ps
        when (_gdFill == Just True && l > 2) $ do
            beginPath renderer
            moveTo renderer $ p $ head ps
            forM_ (tail ps) $ renderLineTo renderer . p
            saveContext renderer
            setGlobalAlpha renderer alpha
            setFillColor renderer $ fromJust c
            fill renderer
            restoreContext renderer

    floor' :: Double -> Double
    floor' x = fromIntegral $ (floor x :: Int)

    round' :: Double -> Double
    round' x = fromIntegral $ (round x :: Int)

    makeNodeWithState newState = L.widget .~ newWidget where
        newWidget = makeGraph graphDatas config newState
