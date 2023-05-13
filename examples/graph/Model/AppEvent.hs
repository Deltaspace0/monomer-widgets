module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Monomer
import Monomer.Graph

import Model.AppModel

data AppEvent
    = AppInit
    | AppResetGraph
    | AppYellowChange Int (Double, Double)
    deriving (Eq, Show)

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetGraph -> [Message "mainGraph" GraphReset]
    AppYellowChange _ (x, y) ->
        [ Model $ model
            & parameter .~ x+1
            & yellowPos .~ (x, y)
        ]
