
{-# LANGUAGE BangPatterns #-}
module Graphene.Graph where

import qualified Data.HashSet      as HS
import qualified Data.HashMap.Lazy as HM
import Data.Hashable

data Graph e v = Graph
  { vertices :: HS.HashSet v
  , edges    :: HM.HashMap e (v, v)
  } deriving (Show, Eq)

emptyGraph :: Graph e v
emptyGraph = Graph HS.empty HM.empty

insertVertex :: (Eq v, Hashable v) => v -> Graph e v -> Graph e v
insertVertex !v (Graph vs es) = Graph (HS.insert v vs) es

insertEdge :: (Eq v, Eq e, Hashable v, Hashable e) => 
  e -> (v, v) -> Graph e v -> Graph e v 
insertEdge !e (v, v') (Graph vs es) = 
  foldr insertVertex (Graph vs (HM.insert e (v, v') es)) [v, v']