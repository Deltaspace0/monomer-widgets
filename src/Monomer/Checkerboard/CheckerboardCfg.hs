module Monomer.Checkerboard.CheckerboardCfg
    ( CheckerboardCfg(..)
    , lightColor
    , darkColor
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Monomer.Graphics.Types

data CheckerboardCfg = CheckerboardCfg
    { _ccBgLightColor :: Maybe Color
    , _ccBgDarkColor :: Maybe Color
    }

instance Default CheckerboardCfg where
    def = CheckerboardCfg
        { _ccBgLightColor = Nothing
        , _ccBgDarkColor = Nothing
        }

instance Semigroup CheckerboardCfg where
    (<>) a1 a2 = def
        { _ccBgLightColor =
            _ccBgLightColor a1 <|> _ccBgLightColor a2
        , _ccBgDarkColor = _ccBgDarkColor a1 <|> _ccBgDarkColor a2
        }

instance Monoid CheckerboardCfg where
    mempty = def

lightColor :: Color -> CheckerboardCfg
lightColor c = def
    { _ccBgLightColor = Just c
    }

darkColor :: Color -> CheckerboardCfg
darkColor c = def
    { _ccBgDarkColor = Just c
    }
