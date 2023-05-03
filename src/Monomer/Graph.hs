module Monomer.Graph
    ( graph
    ) where

import Data.Default
import Monomer.Widgets.Single

graph
    :: [(Double, Double)]
    -> WidgetNode s e
graph points = node where
    node = defaultWidgetNode (WidgetType "graph") widget
    widget = makeGraph points

makeGraph
    :: [(Double, Double)]
    -> Widget s e
makeGraph _ = widget where
    widget = createSingle () def
