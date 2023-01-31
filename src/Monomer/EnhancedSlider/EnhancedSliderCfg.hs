{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.EnhancedSlider.EnhancedSliderCfg
    ( -- * Configuration
      EnhancedSliderCfg(..)
    , titleMethod
    , hideLabel
    , hideLabel_
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Data.Text (Text)
import Monomer.Widgets.Single

{-|
Configuration options for enhancedSlider:

- 'dragRate': the rate at which drag movement affects the number.
- 'hideLabel': don't show the label, leave only slider and buttons.
- 'titleCaption': the title for the shown value.
- 'titleMethod': function to generate the label with value.
- 'alignLeft': put horizontal slider to the left of the buttons.
This is default.
- 'alignCenter': put horizontal slider between the buttons.
- 'alignRight': put horizontal slider to the right of the buttons.
- 'alignTop': put vertical slider to the top of the buttons.
- 'alignMiddle': put vertical slider between the buttons.
- 'alignBottom': put vertical slider to the bottom of the buttons.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onChange': event to raise when the value changes.
- 'onChangeReq': 'WidgetRequest' to generate when the value changes.
-}
data EnhancedSliderCfg s e a = EnhancedSliderCfg
    { _escDragRate :: Maybe Rational
    , _escHideLabel :: Maybe Bool
    , _escTitle :: Maybe Text
    , _escTitleMethod :: Maybe (a -> Text)
    , _escAlignH :: Maybe AlignH
    , _escAlignV :: Maybe AlignV
    , _escOnFocusReq :: [Path -> WidgetRequest s e]
    , _escOnBlurReq :: [Path -> WidgetRequest s e]
    , _escOnChangeReq :: [a -> WidgetRequest s e]
    }

instance Default (EnhancedSliderCfg s e a) where
    def = EnhancedSliderCfg
        { _escDragRate = Nothing
        , _escHideLabel = Nothing
        , _escTitle = Nothing
        , _escTitleMethod = Nothing
        , _escAlignH = Nothing
        , _escAlignV = Nothing
        , _escOnFocusReq = []
        , _escOnBlurReq = []
        , _escOnChangeReq = []
        }

instance Semigroup (EnhancedSliderCfg s e a) where
    (<>) c1 c2 = EnhancedSliderCfg
        { _escDragRate = _escDragRate c1 <|> _escDragRate c2
        , _escHideLabel = _escHideLabel c1 <|> _escHideLabel c2
        , _escTitle = _escTitle c1 <|> _escTitle c2
        , _escTitleMethod =
            _escTitleMethod c1 <|> _escTitleMethod c2
        , _escAlignH = _escAlignH c1 <|> _escAlignH c2
        , _escAlignV = _escAlignV c1 <|> _escAlignV c2
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

instance CmbAlignLeft (EnhancedSliderCfg s e a) where
    alignLeft_ False = def
    alignLeft_ True = def
        { _escAlignH = Just ALeft
        }

instance CmbAlignCenter (EnhancedSliderCfg s e a) where
    alignCenter_ False = def
    alignCenter_ True = def
        { _escAlignH = Just ACenter
        }

instance CmbAlignRight (EnhancedSliderCfg s e a) where
    alignRight_ False = def
    alignRight_ True = def
        { _escAlignH = Just ARight
        }

instance CmbAlignTop (EnhancedSliderCfg s e a) where
    alignTop_ False = def
    alignTop_ True = def
        { _escAlignV = Just ATop
        }

instance CmbAlignMiddle (EnhancedSliderCfg s e a) where
    alignMiddle_ False = def
    alignMiddle_ True = def
        { _escAlignV = Just AMiddle
        }

instance CmbAlignBottom (EnhancedSliderCfg s e a) where
    alignBottom_ False = def
    alignBottom_ True = def
        { _escAlignV = Just ABottom
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

{-|
Receives function which converts the value into text and uses it to
generate the label. Should be used if the title depends on the value
or different formatting is needed.
-}
titleMethod :: (a -> Text) -> EnhancedSliderCfg s e a
titleMethod makeTitle = def
    { _escTitleMethod = Just makeTitle
    }

{-|
Should be used when the label with the current value is not needed.
-}
hideLabel :: EnhancedSliderCfg s e a
hideLabel = hideLabel_ True

hideLabel_ :: Bool -> EnhancedSliderCfg s e a
hideLabel_ v = def
    { _escHideLabel = Just v
    }
