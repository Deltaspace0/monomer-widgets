module Monomer.Graph.GraphMsg
    ( GraphMsg(..)
    ) where

import Monomer.Common.BasicTypes
import Monomer.Core.WidgetTypes

data GraphMsg
    = GraphSetTranslation Point
    | GraphSetScale Point
    | GraphReset
    | GraphStopAnimations
    | GraphFinished Int Millisecond
    deriving (Eq, Show)
