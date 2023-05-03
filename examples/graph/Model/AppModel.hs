{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( AppModel(..)
    , points
    , initModel
    ) where

import Control.Lens

data AppModel = AppModel
    { _amPoints :: [(Double, Double)]
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel $ (\x -> (x, x**2)) <$> [-10, -9.9..10]
