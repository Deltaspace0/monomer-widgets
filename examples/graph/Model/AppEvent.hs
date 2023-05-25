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
    | AppAddPoint (Double, Double)
    | AppRemovePoints
    | AppOrangeChange Int (Double, Double)
    | AppStopAnimations
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
    AppAddPoint p -> [Model $ model & manyPoints %~ (p:)]
    AppRemovePoints -> [Model $ model & manyPoints .~ []]
    AppOrangeChange i p -> [Model $ model & manyPoints . ix i .~ p]
    AppStopAnimations -> [Message "mainGraph" GraphStopAnimations]
