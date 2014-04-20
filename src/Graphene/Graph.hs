
{-# LANGUAGE BangPatterns, TemplateHaskell, NoMonomorphismRestriction #-}
module Graphene.Graph where

import qualified Data.HashSet      as HS
import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import Data.List(foldl')
import Control.Lens

data Graph e v = Graph
  { _vertices :: HS.HashSet v
  , _edges    :: HM.HashMap e (v, v)
  } deriving (Show, Eq)

makeLenses ''Graph

emptyGraph :: Graph e v
emptyGraph = Graph HS.empty HM.empty

insertVertex :: (Eq v, Hashable v) => v -> Graph e v -> Graph e v
insertVertex !v (Graph vs es) = Graph (HS.insert v vs) es

removeVertex :: (Eq v, Hashable v) => v -> Graph e v -> Graph e v
removeVertex v g =  vertices %~ (HS.delete v) $ edges %~ (HM.filterWithKey (\_ (v1, v2) -> not $ any (==v) [v1, v2])) $ g

removeEdge :: (Eq e, Hashable e) => e -> Graph e v -> Graph e v
removeEdge e = edges %~ (HM.delete e)

insertEdge :: (Eq v, Eq e, Hashable v, Hashable e) => 
  e -> (v, v) -> Graph e v -> Graph e v 
insertEdge !e !(v, v') (Graph vs es) = 
  foldr insertVertex (Graph vs (HM.insert e (v, v') es)) [v, v']
  
insertVertices :: (Eq b, Hashable b) => [b] -> Graph e b -> Graph e b
insertVertices vs g = foldl' (flip insertVertex) g vs

insertEdges :: (Eq v, Eq e, Hashable v, Hashable e) =>
     [(e, v, v)] -> Graph e v -> Graph e v
insertEdges es g = foldl' (\g (e, v1, v2) -> insertEdge e (v1, v2) g) g es 

connections :: (Eq v) => v -> Graph e v -> [(e, (v, v))]
connections !v (Graph _ es) = 
  HM.toList $ HM.filter (\(v1, v2) -> any (==v) [v1, v2]) es

neighbors :: Eq v => v -> Graph e v -> [v]
neighbors !v (Graph _ es) = 
  HM.foldlWithKey'
  (\acc e (v1, v2) -> if v == v2 then (v1:acc) else if v == v1 then (v2:acc) else acc)
  []
  es

adjacentVertices :: (Eq e, Hashable e) => e -> Graph e v -> Maybe (v, v)
adjacentVertices !e (Graph _ es) = HM.lookup e es

fromLists :: (Hashable e, Hashable v, Eq e, Eq v) => [v] -> [(e, v, v)] -> Graph e v
fromLists vs es = insertEdges es $ insertVertices vs emptyGraph

degree :: Eq v => v -> Graph e v -> Int
degree v = length . connections v