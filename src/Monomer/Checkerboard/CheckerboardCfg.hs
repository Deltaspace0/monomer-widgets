module Monomer.Checkerboard.CheckerboardCfg
    ( -- * Configuration
      CheckerboardCfg(..)
    , lightColor
    , darkColor
    ) where

import Control.Applicative ((<|>))
import Data.Default
import Monomer.Graphics.Types

{-|
Configuration options for checkerboard:

- 'lightColor': color of light squares in the checkerboard pattern.
- 'darkColor': color of dark squares in the checkerboard pattern.
-}
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
            _ccBgLightColor a2 <|> _ccBgLightColor a1
        , _ccBgDarkColor = _ccBgDarkColor a2 <|> _ccBgDarkColor a1
        }

instance Monoid CheckerboardCfg where
    mempty = def

{-|
Sets color of light squares in the checkerboard pattern.
-}
lightColor :: Color -> CheckerboardCfg
lightColor c = def
    { _ccBgLightColor = Just c
    }

{-|
Sets color of dark squares in the checkerboard pattern.
-}
darkColor :: Color -> CheckerboardCfg
darkColor c = def
    { _ccBgDarkColor = Just c
    }
