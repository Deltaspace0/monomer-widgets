{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Dragboard.DragboardCfg
    ( module Monomer.Checkerboard.CheckerboardCfg
    , DragboardCfg(..)
    , checkerConfig
    ) where

import Data.Default
import Monomer.Checkerboard.CheckerboardCfg
import Monomer.Widgets.Single

data DragboardCfg s e a = DragboardCfg
    { _dcCheckerCfg :: [CheckerboardCfg]
    , _dcOnFocusReq :: [Path -> WidgetRequest s e]
    , _dcOnBlurReq :: [Path -> WidgetRequest s e]
    , _dcOnChangeReq :: [[[a]] -> WidgetRequest s e]
    }

instance Default (DragboardCfg s e a) where
    def = DragboardCfg
        { _dcCheckerCfg = []
        , _dcOnFocusReq = []
        , _dcOnBlurReq = []
        , _dcOnChangeReq = []
        }

instance Semigroup (DragboardCfg s e a) where
    (<>) a1 a2 = def
        { _dcCheckerCfg = _dcCheckerCfg a1 <> _dcCheckerCfg a2
        , _dcOnFocusReq = _dcOnFocusReq a1 <> _dcOnFocusReq a2
        , _dcOnBlurReq = _dcOnBlurReq a1 <> _dcOnBlurReq a2
        , _dcOnChangeReq = _dcOnChangeReq a1 <> _dcOnChangeReq a2
        }

instance Monoid (DragboardCfg s e a) where
    mempty = def

instance WidgetEvent e =>
    CmbOnFocus (DragboardCfg s e a) e Path where
        onFocus fn = def
            { _dcOnFocusReq = [RaiseEvent . fn]
            }

instance CmbOnFocusReq (DragboardCfg s e a) s e Path where
    onFocusReq req = def
        { _dcOnFocusReq = [req]
        }

instance WidgetEvent e =>
    CmbOnBlur (DragboardCfg s e a) e Path where
        onBlur fn = def
            { _dcOnBlurReq = [RaiseEvent . fn]
            }

instance CmbOnBlurReq (DragboardCfg s e a) s e Path where
    onBlurReq req = def
        { _dcOnBlurReq = [req]
        }

instance WidgetEvent e =>
    CmbOnChange (DragboardCfg s e a) [[a]] e where
        onChange fn = def
            { _dcOnChangeReq = [RaiseEvent . fn]
            }

instance CmbOnChangeReq (DragboardCfg s e a) s e [[a]] where
    onChangeReq req = def
        { _dcOnChangeReq = [req]
        }

checkerConfig :: [CheckerboardCfg] -> DragboardCfg s e a
checkerConfig config = def
    { _dcCheckerCfg = config
    }
