{-# LANGUAGE RecordWildCards #-}

module Monomer.SaveManager.SaveManagerEvent
    ( SaveManagerEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Data.Maybe
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
    | EventConfirmRemove
    | EventCancel
    | EventSetCurrentData a
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
    EventConfirmRemove -> confirmRemoveHandle config model
    EventCancel -> cancelHandle config model
    EventSetCurrentData s -> setCurrentDataHandle s config model
    EventSetSavedData s -> setSavedDataHandle s config model
    EventSetSelectedData s -> setSelectedDataHandle s config model
    EventFocus prev -> focusHandle node prev config model
    EventBlur next -> blurHandle node next config model

newSlotHandle :: EventHandle a sp ep
newSlotHandle config SaveManagerModel{..} = [Producer handler] where
    handler raiseEvent = do
        x <- makeCaption config _smmCurrentData
        _ <- raiseEvent $ EventSetSavedData $ x <| _smmSavedData
        raiseEvent $ EventSetSelectedData $ Just 0

saveHandle :: EventHandle a sp ep
saveHandle config SaveManagerModel{..} = response where
    response = [Producer handler | selected]
    handler raiseEvent = do
        x <- makeCaption config _smmCurrentData
        let newSavedData = Seq.update i x _smmSavedData
        raiseEvent $ EventSetSavedData newSavedData
    selected = not $ null _smmSelectedData
    i = fromJust _smmSelectedData

loadHandle :: EventHandle a sp ep
loadHandle _ SaveManagerModel{..} = response where
    response = [Event setCurrentData | selected]
    setCurrentData = EventSetCurrentData d
    selected = not $ null _smmSelectedData
    d = fst $ Seq.index _smmSavedData i
    i = fromJust _smmSelectedData

removeHandle :: EventHandle a sp ep
removeHandle _ SaveManagerModel{..} = responses where
    responses = if selected
        then
            [ Event $ EventSetSelectedData newSelectedData
            , Event $ EventSetSavedData newSavedData
            , Event $ EventCancel
            ]
        else []
    selected = not $ null _smmSelectedData
    newSavedData = Seq.deleteAt i _smmSavedData
    i = fromJust _smmSelectedData
    newSelectedData = if null newSavedData
        then Nothing
        else Just $ min i $ Seq.length newSavedData - 1

confirmRemoveHandle :: EventHandle a sp ep
confirmRemoveHandle _ model = responses where
    responses = [Model $ model & showConfirmRemove .~ True]

cancelHandle :: EventHandle a sp ep
cancelHandle _ model = [Model $ model & showConfirmRemove .~ False]

setCurrentDataHandle :: a -> EventHandle a sp ep
setCurrentDataHandle newCurrentData config model = responses where
    responses = (Model $ model & currentData .~ newCurrentData):req
    req = RequestParent <$> (($ newCurrentData) <$> onChangeReq')
    onChangeReq' = _smcOnChangeReq config

setSavedDataHandle :: Saves a -> EventHandle a sp ep
setSavedDataHandle newSavedData config model = responses where
    responses = (Model $ model & savedData .~ newSavedData):req
    req = RequestParent <$> (($ newSavedData) <$> onSavesChangeReq')
    onSavesChangeReq' = _smcOnSavesChangeReq config

setSelectedDataHandle :: Maybe Int -> EventHandle a sp ep
setSelectedDataHandle newSelectedData _ model = [Model model'] where
    model' = model & selectedData .~ newSelectedData

focusHandle :: WidgetNode s e -> Path -> EventHandle a sp ep
focusHandle node prev config _ = response where
    response = if valid
        then RequestParent <$> (($ prev) <$> _smcOnFocusReq config)
        else []
    valid = not $ isNodeParentOfPath node prev

blurHandle :: WidgetNode s e -> Path -> EventHandle a sp ep
blurHandle node next SaveManagerCfg{..} _ = response where
    response = if valid
        then RequestParent <$> (($ next) <$> _smcOnBlurReq)
        else []
    valid = not $ isNodeParentOfPath node next

makeCaption :: (SaveManagerCfg s e a) -> a -> IO (a, Text)
makeCaption SaveManagerCfg{..} d = do
    time <- getZonedTime
    let defCaption = T.pack $ show time
        cfgCaption = (($ time) . ($ d)) <$> _smcCaptionMethod
    return (d, fromMaybe defCaption cfgCaption)
