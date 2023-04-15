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
    , _dcOnChangeReq :: [[[a]] -> WidgetRequest s e]
    }

instance Default (DragboardCfg s e a) where
    def = DragboardCfg
        { _dcCheckerCfg = []
        , _dcOnChangeReq = []
        }

instance Semigroup (DragboardCfg s e a) where
    (<>) a1 a2 = def
        { _dcCheckerCfg = _dcCheckerCfg a1 <> _dcCheckerCfg a2
        , _dcOnChangeReq = _dcOnChangeReq a1 <> _dcOnChangeReq a2
        }

instance Monoid (DragboardCfg s e a) where
    mempty = def

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
