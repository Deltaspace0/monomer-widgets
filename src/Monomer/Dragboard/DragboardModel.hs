{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Dragboard.DragboardModel
    ( DragboardModel(..)
    , boardState
    , selectedSquare
    , animationSources
    , initDragboardModel
    ) where

import Control.Lens
import Data.Map (Map)
import Monomer.Common.BasicTypes
import qualified Data.Map as Map

data DragboardModel a = DragboardModel
    { _dmBoardState :: [[a]]
    , _dmSelectedSquare :: Maybe Int
    , _dmAnimationSources :: Map Int Rect
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'DragboardModel

initDragboardModel :: DragboardModel a
initDragboardModel = DragboardModel
    { _dmBoardState = []
    , _dmSelectedSquare = Nothing
    , _dmAnimationSources = Map.empty
    }
