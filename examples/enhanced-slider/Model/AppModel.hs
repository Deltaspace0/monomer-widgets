{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( AppModel(..)
    , field
    , number
    , initModel
    ) where

import Control.Lens
import Data.Text

data AppModel = AppModel
    { _amField :: Text
    , _amNumber :: Double
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel "[enhanced slider]" 42
