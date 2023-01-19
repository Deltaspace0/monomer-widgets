{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.EnhancedSlider.EnhancedSliderCfg
    ( EnhancedSliderCfg(..)
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Text (Text)
import Monomer.Widgets.Single

data EnhancedSliderCfg s e a = EnhancedSliderCfg
    { _escDragRate :: Maybe Rational
    , _escTitle :: Maybe Text
    }

instance Default (EnhancedSliderCfg s e a) where
    def = EnhancedSliderCfg
        { _escDragRate = Nothing
        , _escTitle = Nothing
        }

instance Semigroup (EnhancedSliderCfg s e a) where
    (<>) c1 c2 = EnhancedSliderCfg
        { _escDragRate = _escDragRate c1 <|> _escDragRate c2
        , _escTitle = _escTitle c1 <|> _escTitle c2
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
