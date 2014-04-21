{-# LANGUAGE TemplateHaskell #-}
module Graphene.Algorithms (
  kruskal
) where

import Data.List
import Graphene.Graph
import Control.Lens
import Control.Monad.State
import Data.Ord

makeLenses ''Graph

kruskal :: (Ord v, Ord e) => Graph v e -> Graph v e
kruskal g = view _3 $ execState go
            ( map (:[]) (g^.vertices)
              , sortBy (comparing (view _1)) $ g^.edges
              , emptyGraph )
  where go = do
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