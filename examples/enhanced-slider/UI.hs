{-# LANGUAGE OverloadedStrings #-}

module UI
    ( buildUI
    ) where

import Monomer
import Monomer.EnhancedSlider

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ _ = enhancedSlider_ number 0 50
    [ titleCaption "Current value"
    , dragRate 0.5
    ]
