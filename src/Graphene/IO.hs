{-# LANGUAGE NoMonomorphismRestriction #-}
module Graphene.IO(
  exploreFrom
) where

import Graphene.Graph
import Control.Monad(liftM)
import Control.Monad.Trans(lift)
import Control.Monad.Trans.State
import Control.Lens

exploreFrom :: (Eq v, Eq e, Show v, Show e, Read e) => v -> Graph e v -> IO ()
exploreFrom v g = evalStateT go (v, g)

go :: (Eq v, Eq e, Show v, Show e, Read e) => StateT (v, Graph e v) IO ()
go = do
  (v, g) <- get
  lift $ putStrLn $ "You are at vertex: " ++ show v
  lift $ putStrLn "Enter a command:"
  let connEdges = map fst $ connections v g
  cmd <- liftM words (lift getLine)
  case cmd of
    []          -> go
    ["end"]     -> lift $ putStrLn "goodbye!"
    ["move", edge] -> do
      case moveFromThrough v (read edge) g of
        Nothing -> lift $ putStrLn "You cannot move there!"
        Just v  -> _1 .= v
      go
    _ -> (lift $ putStrLn "Command unknown!") >> go