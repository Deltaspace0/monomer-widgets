{-|
The composite works with the field of type 'SaveManagerModel' 'a'.
This structure contains three fields:

- Saved data - sequence of saved objects: 'Seq' ('a', 'Text').
- Current data - the active value 'a'.
- Selected data - 'Maybe' 'Int' position of the selected slot.
When there are no slots, it is 'Nothing'.

It should be initialized with 'initSaveManagerModel' 'a' if there is
no need to initialize slots (e.g. from file).
-}

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

{-|
Receives a value and returns composite model with no slots.
-}
initSaveManagerModel :: a -> (SaveManagerModel a)
initSaveManagerModel initData = SaveManagerModel
    { _smmSavedData = Seq.empty
    , _smmCurrentData = initData
    , _smmSelectedData = Nothing
    }
