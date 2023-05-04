module UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.Graph
import TextShow

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 64]
        [ box $ plot `styleBasic` [sizeReqW $ fixedSize 600]
        , separatorLine
        , vstack_ [childSpacing_ 64]
            [ label $ "Parameter: " <> showt (model ^. parameter)
            , hslider parameter (-2) 2
            ]
        ] `styleBasic` [padding 64]
    plot = graph_ (getPoints model)
        [ wheelRate 2
        ]
