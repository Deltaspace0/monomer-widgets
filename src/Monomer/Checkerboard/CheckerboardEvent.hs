module Monomer.Checkerboard.CheckerboardEvent
    ( CheckerboardEvent(..)
    , handleEvent
    ) where

import Monomer.Widgets.Composite

import Monomer.Checkerboard.CheckerboardCfg

data CheckerboardEvent
    = EventFocus Path
    | EventBlur Path
    deriving Eq

handleEvent
    :: (CheckerboardCfg sp ep)
    -> EventHandler () CheckerboardEvent sp ep
handleEvent config _ node _ event = case event of
    EventFocus prev -> focusHandle node prev config
    EventBlur next -> blurHandle node next config

focusHandle
    :: WidgetNode s e
    -> Path
    -> CheckerboardCfg sp ep
    -> [EventResponse () CheckerboardEvent sp ep]
focusHandle _ _ _ = []

blurHandle
    :: WidgetNode s e
    -> Path
    -> CheckerboardCfg sp ep
    -> [EventResponse () CheckerboardEvent sp ep]
blurHandle _ _ _ = []
