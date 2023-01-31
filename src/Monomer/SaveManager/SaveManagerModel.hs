{-|
The composite works with the field of type 'SaveManagerModel' 'a'.
This structure contains four fields:

- Saved data - sequence of saved objects: 'Seq' ('a', 'Text').
- Current data - the active value 'a'.
- Selected data - 'Maybe' 'Int' position of the selected slot.
When there are no slots, it is 'Nothing'.
- Whether confirmation dialog is currently shown. Used internally.

It should be initialized with 'initSaveManagerModel' 'a' if there is
no need to initialize slots (e.g. from file). Otherwise
'initSaveManagerModel_' should be used.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.SaveManager.SaveManagerModel
    ( SaveManagerModel(..)
    , savedData
    , currentData
    , selectedData
    , showConfirmRemove
    , initSaveManagerModel
    , initSaveManagerModel_
    ) where

import Control.Lens
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Sequence as Seq

data SaveManagerModel a = SaveManagerModel
    { _smmSavedData :: Seq (a, Text)
    , _smmCurrentData :: a
    , _smmSelectedData :: Maybe Int
    , _smmShowConfirmRemove :: Bool
    } deriving Eq

makeLensesWith abbreviatedFields 'SaveManagerModel

{-|
Receives a value and returns composite model with no slots.
-}
initSaveManagerModel :: a -> (SaveManagerModel a)
initSaveManagerModel v = initSaveManagerModel_ v Seq.empty

{-|
Receives a value with slots and returns composite model.
-}
initSaveManagerModel_ :: a -> Seq (a, Text) -> (SaveManagerModel a)
initSaveManagerModel_ initData slots = SaveManagerModel
    { _smmSavedData = slots
    , _smmCurrentData = initData
    , _smmSelectedData = Nothing
    , _smmShowConfirmRemove = False
    }
