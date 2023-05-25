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
            , button "Stop animations" AppStopAnimations
            ]
        ] `styleBasic` [padding 16]
    points =
        [
            [ graphPoints $ (\x -> (x, cos $ p*x)) <$> xs
            , graphColor red
            ]
        ,   [ graphPoint yp
            , graphColor yellow
            , graphRadius 0.42
            , graphHoverColor lightYellow
            , graphActiveColor black
            , graphSeparate
            , graphOnChange $ AppYellowChange
            ]
        ,   [ graphPoint (-3, -2)
            , graphSeparate
            , graphRadius 0.34
            , graphColor violet
            , graphHoverColor pink
            , graphActiveColor purple
            , graphOnClick $ const AppRemovePoints
            ]
        ,   [ graphPoints [(-1, 4), (0, 5), (1, 4), (0, 3)]
            , graphColor $ rgb (round $ yx*50) (round yy*50) 255
            , graphSeparate
            , graphFill
            , graphFillAlpha 0.64
            , graphDuration 500
            ]
        ,   [ graphPoints $ model ^. manyPoints
            , graphColor orange
            , graphSeparate
            , graphOnChange $ AppOrangeChange
            ]
        ]
    xs = [-10, -9.98..10]
    p = model ^. parameter
    yp@(yx, yy) = model ^. yellowPos
