module Graphene.Class where

import Lens.Family2

-- | Graph with edge type `e` and vertex type `v`
data Graph e v = Graph
  { _vertices :: [v]           -- list of vertices
  , _edges    :: [(e, (v, v))] -- list of edges and their associated vertex pairs
  } deriving (Show, Eq)

vertices :: Lens' (Graph e v) [v]
vertices k (Graph vs es)  = fmap (\vs' -> Graph vs' es) (k vs)

edges :: Lens' (Graph e v) [(e, (v, v))]
edges k (Graph vs es)  = fmap (\es' -> Graph vs es') (k es)