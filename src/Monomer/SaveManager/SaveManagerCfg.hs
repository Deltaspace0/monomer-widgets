{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Monomer.SaveManager.SaveManagerCfg
    ( -- * Configuration
      Saves
    , SaveManagerCfg(..)
    , onSavesChange
    , onSavesChangeReq
    , captionMethod
    , noConfirm
    , noConfirm_
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.LocalTime (ZonedTime)
import Monomer.Core
import Monomer.Core.Combinators

type Saves a = Seq (a, Text)

{-|
Configuration options for saveManager:

- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when the value changes.
- 'onChangeReq': 'WidgetRequest' to generate when the value changes.
- 'onSavesChange': event to raise when the saves change.
- 'onSavesChangeReq': 'WidgetRequest' to generate when the saves change.
- 'captionMethod': function to generate the caption for the slot.
- 'noConfirm': don't show confirmation dialog.
-}
data SaveManagerCfg s e a = SaveManagerCfg
    { _smcOnFocusReq :: [Path -> WidgetRequest s e]
    , _smcOnBlurReq :: [Path -> WidgetRequest s e]
    , _smcOnChangeReq :: [a -> WidgetRequest s e]
    , _smcOnSavesChangeReq :: [Saves a -> WidgetRequest s e]
    , _smcCaptionMethod :: Maybe (a -> ZonedTime -> Text)
    , _smcNoConfirm :: Maybe Bool
    }

instance Default (SaveManagerCfg s e a) where
    def = SaveManagerCfg
        { _smcOnFocusReq = []
        , _smcOnBlurReq = []
        , _smcOnChangeReq = []
        , _smcOnSavesChangeReq = []
        , _smcCaptionMethod = Nothing
        , _smcNoConfirm = Nothing
        }

instance Semigroup (SaveManagerCfg s e a) where
    (<>) a1 a2 = def
        { _smcOnFocusReq = _smcOnFocusReq a1 <> _smcOnFocusReq a2
        , _smcOnBlurReq = _smcOnBlurReq a1 <> _smcOnBlurReq a2
        , _smcOnChangeReq = _smcOnChangeReq a1 <> _smcOnChangeReq a2
        , _smcOnSavesChangeReq =
            _smcOnSavesChangeReq a1 <> _smcOnSavesChangeReq a2
        , _smcCaptionMethod =
            _smcCaptionMethod a1 <|> _smcCaptionMethod a2
        , _smcNoConfirm = _smcNoConfirm a1 <|> _smcNoConfirm a2
        }

instance Monoid (SaveManagerCfg s e a) where
    mempty = def

instance WidgetEvent e =>
    CmbOnFocus (SaveManagerCfg s e a) e Path where
        onFocus fn = def
            { _smcOnFocusReq = [RaiseEvent . fn]
            }

instance CmbOnFocusReq (SaveManagerCfg s e a) s e Path where
    onFocusReq req = def
        { _smcOnFocusReq = [req]
        }

instance WidgetEvent e =>
    CmbOnBlur (SaveManagerCfg s e a) e Path where
        onBlur fn = def
            { _smcOnBlurReq = [RaiseEvent . fn]
            }

instance CmbOnBlurReq (SaveManagerCfg s e a) s e Path where
    onBlurReq req = def
        { _smcOnBlurReq = [req]
        }

instance WidgetEvent e =>
    CmbOnChange (SaveManagerCfg s e a) a e where
        onChange fn = def
            { _smcOnChangeReq = [RaiseEvent . fn]
            }

instance CmbOnChangeReq
    (SaveManagerCfg s e a) s e a where
        onChangeReq req = def
            { _smcOnChangeReq = [req]
            }

{-|
On saves change event.
-}
onSavesChange
    :: (WidgetEvent e)
    => (Saves a -> e)
    -> SaveManagerCfg s e a
onSavesChange fn = def
    { _smcOnSavesChangeReq = [RaiseEvent . fn]
    }

{-|
On saves change 'WidgetRequest'.
-}
onSavesChangeReq
    :: (Saves a -> WidgetRequest s e)
    -> SaveManagerCfg s e a
onSavesChangeReq req = def
    { _smcOnSavesChangeReq = [req]
    }

{-|
Receives function which converts the value and current time into
text and uses it to generate the caption for the slot. Should be
used to show more information than just modification time about the
stored value.
-}
captionMethod :: (a -> ZonedTime -> Text) -> SaveManagerCfg s e a
captionMethod makeCaption = def
    { _smcCaptionMethod = Just makeCaption
    }

{-|
Should be used when the confirmation dialog is not needed.
-}
noConfirm :: SaveManagerCfg s e a
noConfirm = noConfirm_ True

noConfirm_ :: Bool -> SaveManagerCfg s e a
noConfirm_ v = def
    { _smcNoConfirm = Just v
    }
