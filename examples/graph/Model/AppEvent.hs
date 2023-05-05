module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Monomer
import Monomer.Graph

import Model.AppModel

data AppEvent
    = AppInit
    | AppResetGraph
    deriving (Eq, Show)

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ _ event = case event of
    AppInit -> []
    AppResetGraph -> [Message "mainGraph" GraphReset]
