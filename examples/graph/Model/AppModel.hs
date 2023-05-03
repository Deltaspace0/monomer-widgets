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
initModel = AppModel [(0, 1), (2, 2)]
