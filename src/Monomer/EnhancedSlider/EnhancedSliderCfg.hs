{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.EnhancedSlider.EnhancedSliderCfg
    ( EnhancedSliderCfg(..)
    , titleMethod
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Text (Text)
import Monomer.Widgets.Single

data EnhancedSliderCfg s e a = EnhancedSliderCfg
    { _escDragRate :: Maybe Rational
    , _escTitle :: Maybe Text
    , _escTitleMethod :: Maybe (a -> Text)
    , _escOnFocusReq :: [Path -> WidgetRequest s e]
    , _escOnBlurReq :: [Path -> WidgetRequest s e]
    , _escOnChangeReq :: [a -> WidgetRequest s e]
    }

instance Default (EnhancedSliderCfg s e a) where
    def = EnhancedSliderCfg
        { _escDragRate = Nothing
        , _escTitle = Nothing
        , _escTitleMethod = Nothing
        , _escOnFocusReq = []
        , _escOnBlurReq = []
        , _escOnChangeReq = []
        }

instance Semigroup (EnhancedSliderCfg s e a) where
    (<>) c1 c2 = EnhancedSliderCfg
        { _escDragRate = _escDragRate c1 <|> _escDragRate c2
        , _escTitle = _escTitle c1 <|> _escTitle c2
        , _escTitleMethod =
            _escTitleMethod c1 <|> _escTitleMethod c2
        , _escOnFocusReq = _escOnFocusReq c1 <> _escOnFocusReq c2
        , _escOnBlurReq = _escOnBlurReq c1 <> _escOnBlurReq c2
        , _escOnChangeReq = _escOnChangeReq c1 <> _escOnChangeReq c2
        }

instance Monoid (EnhancedSliderCfg s e a) where
    mempty = def

instance CmbDragRate (EnhancedSliderCfg s e a) Rational where
    dragRate rate = def
        { _escDragRate = Just rate
        }

instance CmbTitleCaption (EnhancedSliderCfg s e a) where
    titleCaption title = def
        { _escTitle = Just title
        }

instance WidgetEvent e =>
    CmbOnFocus (EnhancedSliderCfg s e a) e Path where
        onFocus fn = def
            { _escOnFocusReq = [RaiseEvent . fn]
            }

instance CmbOnFocusReq (EnhancedSliderCfg s e a) s e Path where
    onFocusReq req = def
        { _escOnFocusReq = [req]
        }

instance WidgetEvent e =>
    CmbOnBlur (EnhancedSliderCfg s e a) e Path where
        onBlur fn = def
            { _escOnBlurReq = [RaiseEvent . fn]
            }

instance CmbOnBlurReq (EnhancedSliderCfg s e a) s e Path where
    onBlurReq req = def
        { _escOnBlurReq = [req]
        }

instance WidgetEvent e =>
    CmbOnChange (EnhancedSliderCfg s e a) a e where
        onChange fn = def
            { _escOnChangeReq = [RaiseEvent . fn]
            }

instance CmbOnChangeReq (EnhancedSliderCfg s e a) s e a where
    onChangeReq req = def
        { _escOnChangeReq = [req]
        }

titleMethod :: (a -> Text) -> EnhancedSliderCfg s e a
titleMethod makeTitle = def
    { _escTitleMethod = Just makeTitle
    }
