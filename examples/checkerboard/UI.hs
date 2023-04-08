module UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.Checkerboard
import TextShow

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 64]
        [ box $ checker `styleBasic`
            [ sizeReqW $ fixedSize 300
            , sizeReqH $ fixedSize 300
            ]
        , separatorLine
        , vstack_ [childSpacing_ 64]
            [ label $ "Rows: " <> (showt r)
            , hslider boardRows 2 12
            , label $ "Cols: " <> (showt c)
            , hslider boardCols 2 12
            ]
        ] `styleBasic` [padding 64]
    checker = checkerboard_ c r [lightColor gray]
        [ label ":D"
        , spacer
        , label ":)"
        , spacer
        , spacer
        , spacer
        , spacer
        , label "HI"
        ]
    c = model ^. boardCols
    r = model ^. boardRows
