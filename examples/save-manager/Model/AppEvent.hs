module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Monomer
import Monomer.SaveManager

import Model.AppModel

data AppEvent
    = AppInit
    | AppIncrease
    deriving (Eq, Show)

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppIncrease -> [Model $ model & saves . currentData +~ 1]
