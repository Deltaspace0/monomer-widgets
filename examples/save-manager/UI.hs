module UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.SaveManager
import TextShow
import qualified Data.Text as T

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = hstack_ [childSpacing_ 128]
    [ saveManager_ saves
        [ captionMethod $
            \x z -> (showt x) <> " -- " <> (T.pack $ show z)
        ]
    , separatorLine
    , label $ "Value: " <> (showt $ model ^. saves . currentData)
    , button "Increase" AppIncrease
    ] `styleBasic` [padding 64]
