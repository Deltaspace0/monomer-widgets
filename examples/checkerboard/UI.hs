module UI
    ( buildUI
    ) where

import Monomer

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = labelS $ _amField model
