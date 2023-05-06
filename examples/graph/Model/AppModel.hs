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

data AppModel = AppModel
    { _amParameter :: Double
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel 1

getPoints :: AppModel -> [(Color, [(Double, Double)])]
getPoints model = points where
    points =
        [ (red, (\x -> (x, cos $ p*x)) <$> [-10, -9.98..10])
        , (green, (\x -> (x, 2.718**x)) <$> [-10, -9.98..10])
        , (blue, (\x -> (x, 1/x)) <$> [0.01,0.02..10])
        , (violet, [(2, 2)])
        , (yellow, [(3, 4)])
        , (black, [(3, 3)])
        , (black, [(5, 3)])
        , (black, [(4, 4)])
        , (black, [(4, 2)])
        ]
    p = model ^. parameter
