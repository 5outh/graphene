{-# LANGUAGE TemplateHaskell #-}
module Graphene.Algorithms (
  kruskal,
  dfs,
  bfs,
  dijkstra
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
dfs :: Eq v => v -> Graph e v -> [v]
dfs v g = case ns of 
  [] -> [v] -- only v if empty
  _  -> v : concatMap (\w -> dfs w (g' w)) ns     -- dfs each graph with w and neighbors removed
  where ns   = neighbors v g                      -- neighbor vertices of v
        g' w = removeVertices (v : delete w ns) g -- remove neighbors (except next hop)

-- breadth first search for connections of `v`
bfs :: Eq v => v -> Graph e v -> [v]
bfs v g = go [v] g
  where go []     _ = []
        go (x:xs) g = x : go (xs ++ ns) (removeVertex x g)
         where ns = neighbors x g

-- shortest path length from `v` to `w`
dijkstra :: (Num e) => v -> v -> Graph e v -> e
dijkstra = undefined

