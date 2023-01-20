{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( AppModel(..)
    , field
    , number
    , changes
    , initModel
    ) where

import Control.Lens
import Data.Text

data AppModel = AppModel
    { _amField :: Text
    , _amNumber :: Double
    , _amChanges :: Int
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel "[enhanced slider]" 42 0
