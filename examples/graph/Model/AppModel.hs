{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( AppModel(..)
    , parameter
    , initModel
    , getPoints
    ) where

import Control.Lens
import Monomer
import Monomer.Graph

data AppModel = AppModel
    { _amParameter :: Double
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel 1

getPoints :: AppModel -> [[GraphData]]
getPoints model = points where
    points =
        [
            [ graphPoints $ (\x -> (x, cos $ p*x)) <$> xs
            , graphColor red
            ]
        ,   [ graphPoint (0, 0)
            , graphColor yellow
            , graphWidth 10
            ]
        ,   [ graphPoints [(-1, 4), (0, 5), (1, 4), (0, 3)]
            , graphColor blue
            , graphSeparate
            , graphFill
            ]
        ]
    xs = [-10, -9.98..10]
    p = model ^. parameter
