module Graphene.Class where

-- Graph with edge type `e` and vertex type `v`
data Graph e v = Graph
  { _vertices :: [v]           -- list of vertices
  , _edges    :: [(e, (v, v))] -- list of edges and their associated vertex pairs
  } deriving (Show, Eq)
