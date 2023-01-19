module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Monomer

import Model.AppModel

data AppEvent
    = AppInit
    deriving (Eq, Show)

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ _ event = case event of
    AppInit -> []
