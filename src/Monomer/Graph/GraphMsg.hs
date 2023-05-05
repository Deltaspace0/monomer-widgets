module Monomer.Graph.GraphMsg
    ( GraphMsg(..)
    ) where

import Monomer.Common.BasicTypes

data GraphMsg
    = GraphSetTranslation Point
    | GraphSetScale Point
    | GraphReset
    deriving (Eq, Show)
