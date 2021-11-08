{-# LANGUAGE RankNTypes, LambdaCase, GeneralizedNewtypeDeriving, DeriveFunctor, BangPatterns #-}
module Control.Monad.Tangle (TangleT(..), hitch, evalTangleT) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Functor.Compose

newtype TangleT t f m a = TangleT
  { runTangleT :: t (Compose (TangleT t f m) f)
    -> t (Compose Maybe f)
    -> m (t (Compose Maybe f), a) }
  deriving Functor

instance Monad m => Applicative (TangleT t f m) where
  pure a = TangleT $ \_ mem -> pure (mem, a)
  TangleT m <*> TangleT n = TangleT $ \ts mem -> m ts mem
    >>= \(mem', f) -> (\(mem'', a) -> (mem'', f a)) <$> n ts mem'
instance Monad m => Monad (TangleT t f m) where
  TangleT m >>= k = TangleT $ \ts mem -> m ts mem >>= \(mem', a) -> runTangleT (k a) ts mem'

instance (Monad m, Semigroup a) => Semigroup (TangleT t f m a) where
  (<>) = liftA2 (<>)

instance (Monad m, Monoid a) => Monoid (TangleT t f m a) where
  mempty = pure mempty

instance MonadTrans (TangleT t f) where
  lift m = TangleT $ \_ mem -> fmap ((,) mem) m 

instance MonadIO m => MonadIO (TangleT t f m) where
  liftIO m = TangleT $ \_ mem -> fmap ((,) mem) (liftIO m)

-- | Obtain a value from the tangle. The result gets memoised.
hitch :: Monad m
  => (forall h g. Functor g => (h a -> g (h a)) -> t h -> g (t h)) -- ^ van Laarhoven lens
  -> TangleT t f m (f a)
hitch l = TangleT $ \ts mem -> getConst $ flip l mem $ \case
  Compose (Just a) -> Const $ pure (mem, a)
  Compose Nothing -> Const
    $ fmap (\(mem', a) -> let !(Identity mem'') = l (const $ pure $ Compose $ Just a) mem' in (mem'', a))
    $ runTangleT (getCompose $ getConst $ l Const ts) ts mem
{-# INLINE hitch #-}

evalTangleT :: Functor m => TangleT t f m a -> t (Compose (TangleT t f m) f) -> t (Compose Maybe f) -> m a
evalTangleT m t s = snd <$> runTangleT m t s
{-# INLINE evalTangleT #-}