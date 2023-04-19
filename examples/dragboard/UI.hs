module UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.Dragboard
import TextShow

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 64]
        [ box $ gameBoard `styleBasic`
            [ sizeReqW $ fixedSize 400
            , sizeReqH $ fixedSize 400
            ]
        , separatorLine
        , vstack_ [childSpacing_ 64]
            [ label $ "Rows: " <> (showt r)
            , hslider boardRows 2 12
            , label $ "Cols: " <> (showt c)
            , hslider boardCols 2 12
            ]
        ] `styleBasic` [padding 64]
    gameBoard = dragboard_ c r boardState getPathOrColor
        [ checkerConfig [lightColor gray]
        ]
    c = model ^. boardCols
    r = model ^. boardRows
