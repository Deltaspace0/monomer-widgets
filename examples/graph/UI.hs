module UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.Graph

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 64]
        [ box $ plot `styleBasic`
            [ sizeReqW $ fixedSize 400
            , sizeReqH $ fixedSize 400
            ]
        , separatorLine
        , vstack_ [childSpacing_ 64]
            [ label "Graph"
            ]
        ]
    plot = graph $ model ^. points
