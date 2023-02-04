module Monomer.SaveManager.UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Data.Typeable
import Monomer.Core.Combinators
import Monomer.Main.UserUtil
import Monomer.Widgets.Composite
import Monomer.Widgets.Containers.Confirm
import Monomer.Widgets.Containers.Grid
import Monomer.Widgets.Containers.SelectList
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Containers.ZStack
import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.Label
import qualified Data.Sequence as Seq

import Monomer.SaveManager.SaveManagerCfg
import Monomer.SaveManager.SaveManagerEvent
import Monomer.SaveManager.SaveManagerModel

buildUI
    :: (Typeable a)
    => (SaveManagerCfg s e a)
    -> UIBuilder (SaveManagerModel a) (SaveManagerEvent a)
buildUI config _ model = widgetTree where
    widgetTree = zstack
        [ mainTree
        , widgetIf (model ^. showConfirmRemove) $
            confirmMsg_ "Are you sure?" EventRemove EventCancel
                [acceptCaption "Remove"]
        ]
    mainTree = vstack_ [childSpacing_ 16]
        [ hgrid_ [childSpacing_ 16]
            [ button' "New slot" EventNewSlot
            , buttonIfSelected "Save" EventSave
            , buttonIfSelected "Load" EventLoad
            , if fromMaybe False (_smcNoConfirm config)
                then buttonIfSelected "Remove" EventRemove
                else buttonIfSelected "Remove" EventConfirmRemove
            ]
        , selectListV_ selectedCaption f savedDataList makeRow
            [ onFocus EventFocus
            , onBlur EventBlur
            ]
        ]
    buttonIfSelected c e = button' c e `nodeEnabled` selected
    button' c e = button_ c e
        [ onFocus EventFocus
        , onBlur EventBlur
        ]
    selected = not $ null selectedData'
    selectedCaption = selectedData' >>= Seq.index savedDataList
    f i _ = EventSetSelectedData $ Just i
    savedDataList = Just . snd <$> model ^. savedData
    selectedData' = model ^. selectedData
    makeRow = label . fromJust
