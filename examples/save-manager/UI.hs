{-# LANGUAGE OverloadedStrings #-}

module UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.SaveManager
import TextShow

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = hstack_ [childSpacing_ 128]
    [ saveManager saves
    , separatorLine
    , label $ "Value: " <> (showt $ model ^. saves . currentData)
    , button "Increase" AppIncrease
    ] `styleBasic` [padding 64]
