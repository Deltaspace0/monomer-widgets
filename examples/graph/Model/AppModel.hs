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
initModel = AppModel 0

getPoints :: AppModel -> [(Double, Double)]
getPoints model = (\x -> (x, (x-p)**2)) <$> [-10, -9.9..10] where
    p = model ^. parameter
