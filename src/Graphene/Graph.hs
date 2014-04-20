
{-# LANGUAGE BangPatterns, TemplateHaskell #-}
module Graphene.Graph where

import qualified Data.HashSet      as HS
import qualified Data.HashMap.Lazy as HM
import Data.Hashable
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

insertEdge :: (Eq v, Eq e, Hashable v, Hashable e) => 
  e -> (v, v) -> Graph e v -> Graph e v 
insertEdge !e !(v, v') (Graph vs es) = 
  foldr insertVertex (Graph vs (HM.insert e (v, v') es)) [v, v']

connections :: (Eq v) => v -> Graph e v -> [(e, (v, v))]
connections !v (Graph _ es) = 
  HM.toList $ HM.filter (\(v1, v2) -> any (==v) [v1, v2]) es

neighbors :: Eq v => v -> Graph e v -> [v]
neighbors v (Graph _ es) = 
  HM.foldlWithKey'
  (\acc e (v1, v2) -> if v == v2 then (v1:acc) else if v == v1 then (v2:acc) else acc)
  []
  es

adjacentVertices :: (Eq e, Hashable e) => e -> Graph e v -> Maybe (v, v)
adjacentVertices !e (Graph _ es) = HM.lookup e es

fromLists :: (Hashable e, Hashable v, Eq e, Eq v) => [v] -> [(e, v, v)] -> Graph e v
fromLists vs es = foldr insertVertex edgeGraph vs
  where edgeGraph = foldr (\(e, v1, v2) -> insertEdge e (v1, v2)) emptyGraph es