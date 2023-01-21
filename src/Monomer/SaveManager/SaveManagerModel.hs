{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.SaveManager.SaveManagerModel
    ( SaveManagerModel(..)
    , savedData
    , currentData
    , selectedData
    , initSaveManagerModel
    ) where

import Control.Lens
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Sequence as Seq

data SaveManagerModel a = SaveManagerModel
    { _smmSavedData :: Seq (a, Text)
    , _smmCurrentData :: a
    , _smmSelectedData :: Maybe Int
    } deriving Eq

makeLensesWith abbreviatedFields 'SaveManagerModel

initSaveManagerModel :: a -> (SaveManagerModel a)
initSaveManagerModel initData = SaveManagerModel
    { _smmSavedData = Seq.empty
    , _smmCurrentData = initData
    , _smmSelectedData = Nothing
    }
