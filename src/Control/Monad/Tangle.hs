{-# LANGUAGE RankNTypes, LambdaCase, GeneralizedNewtypeDeriving, DeriveFunctor, BangPatterns #-}
module Control.Monad.Tangle (TangleT(..), hitch, evalTangleT) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity

newtype TangleT t m a = TangleT
  { runTangleT :: t (TangleT t m) -> t Maybe -> m (t Maybe, a) }
  deriving Functor

instance Monad m => Applicative (TangleT t m) where
  pure a = TangleT $ \_ mem -> pure (mem, a)
  TangleT m <*> TangleT n = TangleT $ \ts mem -> m ts mem
    >>= \(mem', f) -> (\(mem'', a) -> (mem'', f a)) <$> n ts mem'
instance Monad m => Monad (TangleT t m) where
  TangleT m >>= k = TangleT $ \ts mem -> m ts mem >>= \(mem', a) -> runTangleT (k a) ts mem'

instance (Monad m, Semigroup a) => Semigroup (TangleT t m a) where
  (<>) = liftA2 (<>)

instance (Monad m, Monoid a) => Monoid (TangleT t m a) where
    mempty = pure mempty

instance MonadTrans (TangleT t) where
  lift m = TangleT $ \_ mem -> fmap ((,) mem) m 

instance MonadIO m => MonadIO (TangleT t m) where
  liftIO m = TangleT $ \_ mem -> fmap ((,) mem) (liftIO m)

hitch :: Monad m
  => (forall h f. Functor f => (h a -> f (h a)) -> t h -> f (t h))
  -> TangleT t m a
hitch l = TangleT $ \ts mem -> getConst $ flip l mem $ \case
  Just a -> Const $ pure (mem, a)
  Nothing -> Const
    $ fmap (\(mem', a) -> let !(Identity mem'') = l (const $ pure $ Just a) mem' in (mem'', a))
    $ runTangleT (getConst $ l Const ts) ts mem
{-# INLINE hitch #-}

evalTangleT :: Functor m => TangleT t m a -> t (TangleT t m) -> t Maybe -> m a
evalTangleT m t s = snd <$> runTangleT m t s
{-# INLINE evalTangleT #-}