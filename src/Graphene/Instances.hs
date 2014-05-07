{-# LANGUAGE TemplateHaskell #-}
module Graphene.Instances(
  Graph(..),
  emptyGraph
)where

import Lens.Family2
import Data.Bifunctor
import qualified Data.Foldable as F
import Data.Bifoldable
import Data.Traversable
import Data.Monoid
import Graphene.Class

-- | a graph with no vertices or edges
emptyGraph :: Graph e v
emptyGraph = Graph [] []

-- | map over vertices
instance Functor (Graph e) where
  fmap f (Graph vs es) = Graph (map f vs) (map (\(e, (v1, v2)) -> (e, (f v1, f v2))) es)

-- | map over both vertices and edges
instance Bifunctor Graph where
  bimap f g (Graph vs es) = Graph (map g vs) (map (\(e, (v1, v2)) -> (f e, (g v1, g v2))) es)

-- | fold over vertices
instance F.Foldable (Graph e) where
  foldMap f = F.foldMap f . view vertices

-- | fold over both vertices and edges
instance Bifoldable Graph where
  bifoldMap f g (Graph vs es) = F.foldMap (f . fst) es <> (F.foldMap g vs)

-- | identity + binary function (`mappend`)
instance Monoid (Graph v e) where
  mempty        = emptyGraph
  g `mappend` h = Graph (g^.vertices ++ h^.vertices) (g^.edges ++ h^.edges)