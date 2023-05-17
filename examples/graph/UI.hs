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
        [ graphWithData_ points
            [ wheelRate 2
            , onRightClick AppAddPoint
            ] `nodeKey` "mainGraph"
        , separatorLine
        , vstack_ [childSpacing_ 16]
            [ hslider parameter (-2) 2
            , button "Reset" AppResetGraph
            , button "Remove orange points" AppRemovePoints
            ]
        ] `styleBasic` [padding 16]
    points =
        [
            [ graphPoints $ (\x -> (x, cos $ p*x)) <$> xs
            , graphColor red
            ]
        ,   [ graphPoint $ model ^. yellowPos
            , graphColor yellow
            , graphWidth 10
            , graphHoverColor lightYellow
            , graphActiveColor black
            , graphSeparate
            , graphOnChange $ AppYellowChange
            ]
        ,   [ graphPoints [(-1, 4), (0, 5), (1, 4), (0, 3)]
            , graphColor blue
            , graphSeparate
            , graphFill
            , graphFillAlpha 0.64
            ]
        ,   [ graphPoints $ model ^. manyPoints
            , graphColor orange
            , graphSeparate
            , graphOnChange $ AppOrangeChange
            ]
        ]
    xs = [-10, -9.98..10]
    p = model ^. parameter
