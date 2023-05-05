{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Dragboard.DragboardModel
    ( DragboardModel(..)
    , boardState
    , initDragboardModel
    ) where

import Control.Lens

data DragboardModel a = DragboardModel
    { _dmBoardState :: [[a]]
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'DragboardModel

initDragboardModel :: DragboardModel a
initDragboardModel = DragboardModel
    { _dmBoardState = []
    }
