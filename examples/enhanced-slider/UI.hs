{-# LANGUAGE OverloadedStrings #-}

module UI
    ( buildUI
    ) where

import Monomer
import Monomer.EnhancedSlider

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ _ = tree where
    tree = vstack_ [childSpacing_ 16]
        [ enhancedSlider_ number 0 50
            [ titleCaption "Current value"
            , dragRate 0.5
            , onChange fn
            ]
        , enhancedSlider_ changes 0 1000
            [ titleCaption "Changes"
            ]
        ]
    fn :: Double -> AppEvent
    fn = const AppIncrementChanges
