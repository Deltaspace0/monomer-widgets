{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( AppModel(..)
    , boardRows
    , boardCols
    , initModel
    ) where

import Control.Lens

data AppModel = AppModel
    { _amBoardRows :: Int
    , _amBoardCols :: Int
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel 3 3
