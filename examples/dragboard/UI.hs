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
    checker = checkerboard_ c r [lightColor gray] $ makeImage <$>
        [ "bR", "bN", "bB", "bQ", "bK", "bB", "bN", "bR"
        , "bP", "bP", "bP", "bP", "bP", "bP", "bP", "bP"
        , "", "", "", "", "", "", "", ""
        , "", "", "", "", "", "", "", ""
        , "", "", "", "", "", "", "", ""
        , "", "", "", "", "", "", "", ""
        , "wP", "wP", "wP", "wP", "wP", "wP", "wP", "wP"
        , "wR", "wN", "wB", "wQ", "wK", "wB", "wN", "wR"
        ]
    c = model ^. boardCols
    r = model ^. boardRows
    makeImage x = if x == ""
        then filler
        else image_ (f x) [fitEither]
    f x = "assets/chess-pieces/" <> x <> ".png"
