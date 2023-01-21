module Monomer.SaveManager.SaveManagerEvent
    ( SaveManagerEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Data.Maybe
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.LocalTime
import Monomer.Widgets.Composite
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.SaveManager.SaveManagerCfg
import Monomer.SaveManager.SaveManagerModel

data SaveManagerEvent a
    = EventNewSlot
    | EventSave
    | EventLoad
    | EventRemove
    | EventSetSavedData (Saves a)
    | EventSetSelectedData (Maybe Int)
    | EventFocus Path
    | EventBlur Path
    deriving Eq

type EventHandle a sp ep
    = (SaveManagerCfg sp ep a)
    -> (SaveManagerModel a)
    -> [EventResponse
            (SaveManagerModel a) (SaveManagerEvent a) sp ep]

handleEvent
    :: (SaveManagerCfg sp ep a)
    -> EventHandler (SaveManagerModel a) (SaveManagerEvent a) sp ep
handleEvent config _ node model event = case event of
    EventNewSlot -> newSlotHandle config model
    EventSave -> saveHandle config model
    EventLoad -> loadHandle config model
    EventRemove -> removeHandle config model
    EventSetSavedData s -> setSavedDataHandle s config model
    EventSetSelectedData s -> setSelectedDataHandle s config model
    EventFocus prev -> focusHandle node prev config model
    EventBlur next -> blurHandle node next config model

newSlotHandle :: EventHandle a sp ep
newSlotHandle config model = [Producer handler] where
    handler raiseEvent = do
        x <- makeCaption config $ model ^. currentData
        raiseEvent $ EventSetSavedData $ x <| (model ^. savedData)
        raiseEvent $ EventSetSelectedData $ Just 0

saveHandle :: EventHandle a sp ep
saveHandle config model = [Producer handler | selected] where
    handler raiseEvent = do
        x <- makeCaption config $ model ^. currentData
        let newSavedData = Seq.update i x $ model ^. savedData
        raiseEvent $ EventSetSavedData newSavedData
    selected = not $ null $ model ^. selectedData
    i = fromJust $ model ^. selectedData

loadHandle :: EventHandle a sp ep
loadHandle _ model = [Model model' | selected] where
    model' = model & currentData .~ d
    selected = not $ null $ model ^. selectedData
    d = fst $ Seq.index (model ^. savedData) i
    i = fromJust $ model ^. selectedData

removeHandle :: EventHandle a sp ep
removeHandle _ model = responses where
    responses = if selected
        then
            [ Event $ EventSetSelectedData newSelectedData
            , Event $ EventSetSavedData newSavedData
            ]
        else []
    selected = not $ null $ model ^. selectedData
    newSavedData = Seq.deleteAt i $ model ^. savedData
    i = fromJust $ model ^. selectedData
    newSelectedData = if null newSavedData
        then Nothing
        else Just $ min i $ Seq.length newSavedData - 1

setSavedDataHandle :: Saves a -> EventHandle a sp ep
setSavedDataHandle newSavedData config model = responses where
    responses = (Model $ model & savedData .~ newSavedData):req
    req = RequestParent <$> (($ newSavedData) <$> onSavesChangeReq)
    onSavesChangeReq = _smcOnSavesChangeReq config

setSelectedDataHandle :: Maybe Int -> EventHandle a sp ep
setSelectedDataHandle newSelectedData _ model = [Model model'] where
    model' = model & selectedData .~ newSelectedData

focusHandle :: WidgetNode s e -> Path -> EventHandle a sp ep
focusHandle node prev config model = response where
    response = if valid
        then RequestParent <$> (($ prev) <$> _smcOnFocusReq config)
        else []
    valid = not $ isNodeParentOfPath node prev

blurHandle :: WidgetNode s e -> Path -> EventHandle a sp ep
blurHandle node next config model = response where
    response = if valid
        then RequestParent <$> (($ next) <$> _smcOnBlurReq config)
        else []
    valid = not $ isNodeParentOfPath node next

makeCaption :: (SaveManagerCfg s e a) -> a -> IO (a, Text)
makeCaption config d = do
    time <- getZonedTime
    let defCaption = T.pack $ show time
        cfgCaption = (($ time) . ($ d)) <$> _smcCaptionMethod config
    return (d, fromMaybe defCaption cfgCaption)
