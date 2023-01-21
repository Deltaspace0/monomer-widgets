{-# LANGUAGE OverloadedStrings #-}

module Monomer.SaveManager
    ( module Monomer.SaveManager.SaveManagerModel
    , module Monomer.SaveManager.SaveManagerCfg
    , SaveManagerEvent
    , saveManager
    , saveManager_
    , saveManagerV
    , saveManagerV_
    , saveManagerD_
    ) where

import Control.Lens
import Data.Default
import Data.Typeable
import Monomer.Core.Combinators
import Monomer.Widgets.Composite

import Monomer.SaveManager.SaveManagerCfg
import Monomer.SaveManager.SaveManagerEvent
import Monomer.SaveManager.SaveManagerModel
import Monomer.SaveManager.UI

saveManager
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => ALens' s (SaveManagerModel a)
    -> WidgetNode s e
saveManager field = saveManager_ field def

saveManager_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => ALens' s (SaveManagerModel a)
    -> [SaveManagerCfg s e a]
    -> WidgetNode s e
saveManager_ field configs = saveManagerD_ wlens configs [] where
    wlens = WidgetLens field

saveManagerV
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => SaveManagerModel a
    -> (SaveManagerModel a -> e)
    -> WidgetNode s e
saveManagerV value handler = saveManagerV_ value handler def

saveManagerV_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => SaveManagerModel a
    -> (SaveManagerModel a -> e)
    -> [SaveManagerCfg s e a]
    -> WidgetNode s e
saveManagerV_ value handler configs = node where
    node = saveManagerD_ wdata newConfigs []
    wdata = WidgetValue value
    newConfigs = onChange handler : configs

saveManagerD_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => WidgetData s (SaveManagerModel a)
    -> [SaveManagerCfg s e a]
    -> [CompositeCfg (SaveManagerModel a) (SaveManagerEvent a) s e]
    -> WidgetNode s e
saveManagerD_ wdata configs cmpConfigs = node where
    node = compositeD_ wt wdata uiBuilder eventHandler cmpConfigs
    wt = "saveManager"
    uiBuilder = buildUI config
    eventHandler = handleEvent config
    config = mconcat configs
