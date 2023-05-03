module Monomer.Graph.GraphState
    ( GraphState(..)
    ) where

import Data.Default
import Monomer.Common.BasicTypes

data GraphState = GraphState
    { _gsTranslation :: Point
    , _gsScale :: Point
    , _gsMousePosition :: Maybe Point
    } deriving (Eq, Show)

instance Default GraphState where
    def = GraphState
        { _gsTranslation = Point 0 0
        , _gsScale = Point 1 1
        , _gsMousePosition = Nothing
        }
