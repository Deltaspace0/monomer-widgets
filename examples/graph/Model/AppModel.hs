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

getPoints :: AppModel -> [(Double, Double)]
getPoints model = (\x -> (x, cos $ p*x)) <$> [-10, -9.98..10] where
    p = model ^. parameter
