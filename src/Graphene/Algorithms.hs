{-# LANGUAGE TemplateHaskell #-}
module Graphene.Algorithms (
  kruskal
) where

import Data.List
import Graphene.Graph
import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Data.Ord

makeLenses ''Graph

-- Kruskal's minimum spanning tree algorithm
kruskal :: (Ord v, Ord e) => Graph v e -> Graph v e
kruskal g = view _3 $ execState go (vertexSets, sortedEdges, emptyGraph)
  where vertexSets = map (:[]) $ g^.vertices
        sortedEdges = sortBy (comparing fst) $ g^.edges
        go = do
          (vs, es, _) <- get
          unless (null es) $ do
            let e@(w, (v1, v2)) = head es
                ss = filter (\s -> any (`elem` s) [v1, v2]) vs
            case ss of 
              [s1, s2] -> do
                _3.vertices %= union ([v1, v2])
                _3.edges    %= insert e
                _1          %= delete s1 . delete s2 . insert (s1 `union` s2)
              _        -> return ()
            _2 %= tail
            go

-- depth first search for connections of `v`
dfs :: v  -> Graph v e -> (Graph v e, [v])
dfs = undefined

-- breadth first search for connections of `v`
bfs :: v -> Graph v e -> (Graph v e, [v])
bfs = undefined

-- shortest path length from `v` to `w`
dijkstra :: (Num e) => v -> v -> Graph v e -> e
dijkstra = undefined

