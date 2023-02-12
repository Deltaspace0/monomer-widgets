module Model.AppModel
    ( AppModel(..)
    , initModel
    ) where

data AppModel = AppModel
    { _amField :: Int
    } deriving (Eq, Show)

initModel :: AppModel
initModel = AppModel 42
