{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Map as Map

type Key = String
type Container = Map.Map Key

newtype Tangle b a = Tangle { unTangle :: ReaderT (Container (Tangle b b)) (State (Container b)) a }
  deriving (Functor, Applicative, Monad)

hitch :: Key -> Tangle b b
hitch key = Tangle $ ReaderT $ \env -> state $ \cache -> case Map.lookup key cache of
  -- If the requested key exists in the cache, just return the value
  Just a -> (a, cache)
  Nothing -> case Map.lookup key env of
    Nothing -> error "Not found"
    Just m ->
      -- Compute the result
      let (result, cache') = unTangle m `runReaderT` env `runState` cache
      -- Insert the result to the cache
      in (result, Map.insert key result cache')

evalTangle :: Tangle b a -> Container (Tangle b b) -> a
evalTangle m env = unTangle m `runReaderT` env `evalState` Map.empty

tangles :: Container (Tangle Int Int)
tangles = Map.fromList
  [ ("foo", pure 6)
  , ("bar", pure 7)
  , ("baz", (*) <$> hitch "foo" <*> hitch "bar")
  ]

main :: IO ()
main = print $ evalTangle (hitch "baz") tangles