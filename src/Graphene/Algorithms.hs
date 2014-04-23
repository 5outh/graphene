{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, TupleSections #-}
module Graphene.Algorithms (
  kruskal,
  dfs,
  bfs,
  dijkstra
) where

import Data.List
import qualified Data.Map as M
import Graphene.Graph
import Control.Lens
import Control.Monad.Writer
import Control.Monad.Trans.State
import Data.Ord
import Data.Bifunctor
import Data.Maybe

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

-- test
sg :: Graph Int Char
sg = fromLists ['a'..'e'] (zip3 [1..5] ['a'..'d'] ['b'..'e'])

data DijkstraState e v = DijkstraState{
    _underlyingGraph :: Graph e v
  , _distancePairings :: M.Map v Int
  , _unvisited :: [v]
  , _visited :: [v]
  , _from :: v
} deriving (Show, Eq)

makeLenses ''DijkstraState

-- smart constructor for dijkstra state
mkDijkstra :: (Eq v, Ord v) => Graph e v -> v -> DijkstraState e v
mkDijkstra g@(Graph vs es) v = 
  DijkstraState 
    g 
    ( M.fromList ( (v, 0) : (map (, (maxBound :: Int) ) $ delete v vs) ) )
    vs  
    [] 
    v

dijkstra g = runStateT runDijkstra . mkDijkstra g

-- Graph, visited, unvisited
-- NB. ints are weights
runDijkstra :: (Eq v, Show v, Ord v) => StateT (DijkstraState Int v) IO ()
runDijkstra = do
  q   <- use unvisited
  unless (null q) $ do
    dists <- use distancePairings
    g     <- use underlyingGraph
    let ordered@(u:_)  = sortBy (comparing (flip M.lookup dists)) q
        (Just uWeight) = M.lookup u dists -- weight stored at `u`
        ns             = neighbors u g
        conns          = map (\(e, (v1, v2)) -> (e, if v1 == u then v2 else v1) ) $ connections u g
    unvisited %= (delete u) -- remove u from q
    unless ( uWeight == (maxBound :: Int) ) $ do
      zoom distancePairings $ updateWeights uWeight conns
      return ()

updateWeights :: (Ord v, Monad m) => Int -> [(Int, v)] -> StateT (M.Map v Int) m ()
updateWeights vWeight conns = do
  dists <- get
  forM_ conns $ \(eWeight, v) -> modify (M.adjust (min (eWeight + vWeight)) v)
