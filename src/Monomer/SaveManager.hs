{-|
This is a composite which consists of 'selectList' and buttons:

- New slot
- Save
- Load
- Remove

@
saveManager field
@
-}

module Monomer.SaveManager
    ( -- * Re-exported modules
      module Monomer.SaveManager.SaveManagerModel
    , module Monomer.SaveManager.SaveManagerCfg
      -- * Configuration
    , SaveManagerEvent
      -- * Constructors
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

{-|
Creates a save manager using the given lens.
-}
saveManager
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => ALens' s (SaveManagerModel a)  -- ^ The lens into the model.
    -> WidgetNode s e                 -- ^ The created save manager.
saveManager field = saveManager_ field def

{-|
Creates a save manager using the given lens. Accepts config.
-}
saveManager_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => ALens' s (SaveManagerModel a)  -- ^ The lens into the model.
    -> [SaveManagerCfg s e a]         -- ^ The config options.
    -> WidgetNode s e                 -- ^ The created save manager.
saveManager_ field configs = saveManagerD_ wlens configs [] where
    wlens = WidgetLens field

{-|
Creates a save manager using the given composite model and
'onChange' event handler.
-}
saveManagerV
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => SaveManagerModel a  -- ^ The composite model.
    -> (a -> e)            -- ^ The event to raise on change.
    -> WidgetNode s e      -- ^ The created save manager.
saveManagerV value handler = saveManagerV_ value handler def

{-|
Creates a save manager using the given composite model and
'onChange' event handler. Accepts config.
-}
saveManagerV_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => SaveManagerModel a      -- ^ The composite model.
    -> (a -> e)                -- ^ The event to raise on change.
    -> [SaveManagerCfg s e a]  -- ^ The config options.
    -> WidgetNode s e          -- ^ The created save manager.
saveManagerV_ value handler configs = node where
    node = saveManagerD_ wdata newConfigs []
    wdata = WidgetValue value
    newConfigs = onChange handler : configs

{-|
Creates a save manager providing a 'WidgetData' instance and config.
-}
saveManagerD_
    :: (WidgetModel s, WidgetEvent e, Eq a, Typeable a)
    => WidgetData s (SaveManagerModel a)
    -- ^ The 'WidgetData' to retrieve the model from.
    -> [SaveManagerCfg s e a]
    -- ^ The config options.
    -> [CompositeCfg (SaveManagerModel a) (SaveManagerEvent a) s e]
    -- ^ The composite config options.
    -> WidgetNode s e
    -- ^ The created save manager.
saveManagerD_ wdata configs cmpConfigs = node where
    node = compositeD_ wt wdata uiBuilder eventHandler cmpConfigs
    wt = "saveManager"
    uiBuilder = buildUI config
    eventHandler = handleEvent config
    config = mconcat configs
