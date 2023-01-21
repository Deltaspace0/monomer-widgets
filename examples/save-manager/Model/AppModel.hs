{-# LANGUAGE OverloadedStrings #-}

module Model.AppModel
    ( AppModel(..)
    , initModel
    ) where

import Data.Text

data AppModel = AppModel
    { _amField :: Text
    } deriving (Eq, Show)

initModel :: AppModel
initModel = AppModel "[save manager]"
