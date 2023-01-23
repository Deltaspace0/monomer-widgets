{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( AppModel(..)
    , initModel
    , saves
    ) where

import Control.Lens
import Monomer.SaveManager

data AppModel = AppModel
    { _amSaves :: SaveManagerModel Int
    } deriving Eq

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel $ initSaveManagerModel 0
