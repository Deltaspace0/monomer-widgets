module Monomer.Graph.GraphState
    ( GraphState(..)
    ) where

import Data.Default
import Monomer.Common.BasicTypes
import Monomer.Core.WidgetTypes

import Monomer.Graph.GraphData

data GraphState s e = GraphState
    { _gsTranslation :: Point
    , _gsScale :: Point
    , _gsUnit :: Point
    , _gsSections :: Point
    , _gsMousePosition :: Maybe Point
    , _gsHoverPoint :: Maybe (Int, Int)
    , _gsActivePoint :: Maybe (Int, Int)
    , _gsViewport :: Rect
    , _gsGraphDatas :: [GraphData s e]
    , _gsPrevGraphDatas :: [GraphData s e]
    , _gsAnimationStates :: [(Bool, Millisecond)]
    }

instance Default (GraphState s e) where
    def = GraphState
        { _gsTranslation = Point 0 0
        , _gsScale = Point 1 1
        , _gsUnit = Point 1 1
        , _gsSections = Point 5 5
        , _gsMousePosition = Nothing
        , _gsHoverPoint = Nothing
        , _gsActivePoint = Nothing
        , _gsViewport = Rect 0 0 100 100
        , _gsGraphDatas = []
        , _gsPrevGraphDatas = []
        , _gsAnimationStates = []
        }
