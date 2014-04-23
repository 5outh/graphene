{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
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
import Data.Bifunctor

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

-- Label each edge with distance of shortest path from v
-- dijkstra :: (Num e) => v -> Graph e v -> e
dijkstra v g' = setup
  where setup = flip second g' $ \w -> case w == v of -- initialize weights
                  True -> (w, 0)
                  _    -> (w, maxBound :: Int) -- "infinity"
        go = undefined


-- Graph, visited, unvisited
runDijkstra :: (Eq v) => State (Graph Int (v, Int), [(v, Int)], [(v, Int)]) [(v, Int)]
runDijkstra = do
  (g, visited, unvisited) <- get
  if null unvisited 
    then return visited
    else do
      let (v@(vertex, weight):_) = sortBy (comparing snd) unvisited
          conns = map (\(e, (v1, v2)) -> (e, if v1 == v then v2 else v1) ) $ connections v g -- [(eWeight, (v, vWeight))]
          g'    = foldr f g conns
          f (eWeight, w@(v, vWeight)) graph = modifyVertex (const (v, min (eWeight + weight) vWeight)) w graph
      do 
        _1 .= (removeVertex v g')
        _2 %= (v:)
        _3 %= tail
        runDijkstra

-- propagate edge weights to each neighbor's weight, and delete the start vertex
propagate :: (Eq v, Eq a, Num a, Ord a) => (v, a) -> Graph a (v, a) -> Graph a (v, a)
propagate v@(_, weight) g = 
    removeVertex v 
  $ foldr (\(e, w) -> modifyVertex (_2 %~ (min (weight + e))) w) g conns
    where conns =  map (\(e, (v1, v2)) -> (e, if v1 == v then v2 else v1) ) 
                 $ connections v g
