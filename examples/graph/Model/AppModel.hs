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

data AppModel = AppModel
    { _amParameter :: Double
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel 1

getPoints :: AppModel -> [[(Double, Double)]]
getPoints model = points where
    points =
        [ (\x -> (x, cos $ p*x)) <$> [-10, -9.98..10]
        , (\x -> (x, 2.718**x)) <$> [-10, -9.98..10]
        , (\x -> (x, 1/x)) <$> [0.01,0.02..10]
        , [(2, 2)]
        , [(3, 4)]
        ]
    p = model ^. parameter
