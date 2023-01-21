{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Monomer.SaveManager.SaveManagerCfg
    ( Saves
    , SaveManagerCfg(..)
    , onSavesChange
    , onSavesChangeReq
    , captionMethod
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.LocalTime (ZonedTime)
import Monomer.Core
import Monomer.Core.Combinators

import Monomer.SaveManager.SaveManagerModel

type Saves a = Seq (a, Text)

data SaveManagerCfg s e a = SaveManagerCfg
    { _smcOnFocusReq :: [Path -> WidgetRequest s e]
    , _smcOnBlurReq :: [Path -> WidgetRequest s e]
    , _smcOnChangeReq :: [SaveManagerModel a -> WidgetRequest s e]
    , _smcOnSavesChangeReq :: [Saves a -> WidgetRequest s e]
    , _smcCaptionMethod :: Maybe (a -> ZonedTime -> Text)
    }

instance Default (SaveManagerCfg s e a) where
    def = SaveManagerCfg
        { _smcOnFocusReq = []
        , _smcOnBlurReq = []
        , _smcOnChangeReq = []
        , _smcOnSavesChangeReq = []
        , _smcCaptionMethod = Nothing
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
    CmbOnChange (SaveManagerCfg s e a) (SaveManagerModel a) e where
        onChange fn = def
            { _smcOnChangeReq = [RaiseEvent . fn]
            }

instance CmbOnChangeReq
    (SaveManagerCfg s e a) s e (SaveManagerModel a) where
        onChangeReq req = def
            { _smcOnChangeReq = [req]
            }

onSavesChange
    :: (WidgetEvent e)
    => (Saves a -> e)
    -> SaveManagerCfg s e a
onSavesChange fn = def
    { _smcOnSavesChangeReq = [RaiseEvent . fn]
    }

onSavesChangeReq
    :: (Saves a -> WidgetRequest s e)
    -> SaveManagerCfg s e a
onSavesChangeReq req = def
    { _smcOnSavesChangeReq = [req]
    }

captionMethod :: (a -> ZonedTime -> Text) -> SaveManagerCfg s e a
captionMethod makeCaption = def
    { _smcCaptionMethod = Just makeCaption
    }
