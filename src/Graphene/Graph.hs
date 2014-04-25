
{-# LANGUAGE BangPatterns, TemplateHaskell, NoMonomorphismRestriction #-}
module Graphene.Graph(
  Graph(..),
  emptyGraph,
  insertVertex,
  removeVertex,
  removeVertices,
  removeEdge,
  insertEdge,
  insertVertices,
  insertEdges,
  modifyVertex,
  modifyEdge,
  connections,
  neighbors,
  fromLists,
  degree,
  subgraph,
  moveFromTo,
  moveFromThrough,
  module Graphene.Class
) where

import Data.Hashable
import Data.List
import Data.Function
import Data.Maybe(catMaybes)
import Control.Lens
import Data.Bifunctor
import Graphene.Class
import Graphene.Instances

makeLenses ''Graph

-- | Insert a vertex into a graph
insertVertex :: (Eq v) => v -> Graph e v -> Graph e v
insertVertex !v g@(Graph vs es) 
  | v `elem` vs = g
  | otherwise   = Graph (v:vs) es

-- | Remove a vertex V from a graph
-- | (Also removes edges connected to V)
removeVertex :: Eq v => v -> Graph e v -> Graph e v
removeVertex !v g = vertices %~ (delete v) 
  $ edges %~ (filter (\(_, (v1, v2)) -> not $ any (==v) [v1, v2])) $ g

-- | Remove a list of vertices from a graph
removeVertices :: Eq v => [v] -> Graph e v -> Graph e v
removeVertices vs g = foldl' (flip removeVertex) g vs

-- | Inset an edge into a graph connected to two vertices
insertEdge :: Eq v => e -> (v, v) -> Graph e v -> Graph e v 
insertEdge !e !(v, v') (Graph vs es) = 
  foldr insertVertex (Graph vs ((e, (v, v')):es)) [v, v']

-- | Remove an edge from a graph
removeEdge :: Eq e => e -> Graph e v -> Graph e v
removeEdge !e = edges %~ (deleteBy ((==) `on` fst) (e, undefined))

-- | Modify a vertex in a graph by an automorphism 
-- | If such a vertex doesn't exist, modifyVertex = id
modifyVertex :: Eq v => (v -> v) -> v -> Graph e v -> Graph e v
modifyVertex f !v = second (\w -> if v == w then f v else v)

-- | Modify an edge in a graph by an automorphism
-- | If such an edge doesn't exist, modifyEdge = id
modifyEdge :: Eq e => (e -> e) -> e -> Graph e v -> Graph e v
modifyEdge f !e = first (\e' -> if e == e' then f e' else e')

-- | Insert a list of edges into a graph
insertVertices :: Eq b => [b] -> Graph e b -> Graph e b
insertVertices vs g = foldl' (flip insertVertex) g vs

-- | Insert a list of edges into a graph
insertEdges :: Eq v => [(e, v, v)] -> Graph e v -> Graph e v
insertEdges es g = foldl' (\g (e, v1, v2) -> insertEdge e (v1, v2) g) g es 

-- | find edge connections and vertex neighbors to a vertex
connections :: (Eq v) => v -> Graph e v -> [(e, v)]
connections !v (Graph _ es) = catMaybes $ 
  map (\(e, (v1, v2)) -> 
        if v == v1 then Just (e, v2) 
          else if v == v2 then Just (e, v1) 
            else Nothing) es

-- | find all vertices connected to a given vertex
neighbors :: Eq v => v -> Graph e v -> [v]
neighbors !v (Graph _ es) = foldl' f [] es
  where f acc (e, (v1, v2)) = if v == v2 then (v1:acc) else if v == v1 then (v2:acc) else acc

-- | Generate a graph froma list of edges and a list of edge / 2 vertex pairs
fromLists :: (Eq v) => [v] -> [(e, v, v)] -> Graph e v
fromLists vs es = insertEdges es $ insertVertices vs emptyGraph

-- | find the degree of a vertex
degree :: Eq v => v -> Graph e v -> Int
degree !v = length . connections v

-- | subgraph generated by a list of vertices
subgraph :: Eq v => [v] -> Graph e v -> Graph e v
subgraph ws (Graph vs es) = Graph vs' es'
  where vs' = filter (`elem` ws) vs
        es' = filter (\(e, (v1, v2)) -> all (`elem` ws) [v1, v2]) es

-- | move to a adjacent vertex (returns the next vertex if it really is connected)
moveFromTo :: Eq v => v -> v -> Graph e v -> Maybe v
moveFromTo v w g = if w `elem` ns then Just w else Nothing 
  where ns = neighbors v g

-- | follow an edge to a new adjancent vertex (returns the new vertex)
moveFromThrough :: (Eq v, Eq e) => v -> e -> Graph e v -> Maybe v
moveFromThrough v e g = lookup e $ connections v g
