{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( AppModel(..)
    , parameter
    , yellowPos
    , manyPoints
    , initModel
    ) where

import Control.Lens

data AppModel = AppModel
    { _amParameter :: Double
    , _amYellowPos :: (Double, Double)
    , _amManyPoints :: [(Double, Double)]
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel 1 (0, 0) []
