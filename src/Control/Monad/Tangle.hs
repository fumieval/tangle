{-# LANGUAGE RankNTypes, LambdaCase, DeriveFunctor, BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
module Control.Monad.Tangle
  (TangleFT(..), hitchF
  , evalTangleFT
  , liftTangles
  , blank
  , hitch
  , TangleF
  , evalTangleF
  , TangleT
  , evalTangleT
  , Tangle
  , evalTangle
  ) where

import Barbies
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Monoid

-- | 'TangleFT' is a higher-kinded heterogeneous memoisation monad transformer.
-- @t@ represents the shape of the underlying data structure, and @f@ is the wrapper type of each field.
-- This monad represents computations that depend on the contents of @t f@.
newtype TangleFT t f m a = TangleFT
  { runTangleFT :: t (Compose (TangleFT t f m) f)
    -> t (Compose Maybe f)
    -> m (t (Compose Maybe f), a) }
  deriving Functor
  deriving (Semigroup, Monoid) via Ap (TangleFT t f m) a

instance Monad m => Applicative (TangleFT t f m) where
  pure a = TangleFT $ \_ mem -> pure (mem, a)
  TangleFT m <*> TangleFT n = TangleFT $ \ts mem -> m ts mem
    >>= \(mem', f) -> (\(mem'', a) -> (mem'', f a)) <$> n ts mem'
instance Monad m => Monad (TangleFT t f m) where
  TangleFT m >>= k = TangleFT $ \ts mem -> m ts mem >>= \(mem', a) -> runTangleFT (k a) ts mem'

instance MonadTrans (TangleFT t f) where
  lift m = TangleFT $ \_ mem -> fmap ((,) mem) m

instance MonadIO m => MonadIO (TangleFT t f m) where
  liftIO m = TangleFT $ \_ mem -> fmap ((,) mem) (liftIO m)

-- | Obtain a value from the tangle. The result gets memoised.
hitchF :: Monad m
  => (forall h g. Functor g => (h a -> g (h a)) -> t h -> g (t h)) -- ^ van Laarhoven lens
  -> TangleFT t f m (f a)
hitchF l = TangleFT $ \ts mem -> getConst $ flip l mem $ \case
  Compose (Just a) -> Const $ pure (mem, a)
  Compose Nothing -> Const
    $ fmap (\(mem', a) -> let !(Identity mem'') = l (const $ pure $ Compose $ Just a) mem' in (mem'', a))
    $ runTangleFT (getCompose $ getConst $ l Const ts) ts mem
{-# INLINE hitchF #-}

evalTangleFT :: (ApplicativeB t, Functor m) => TangleFT t f m a -> t (Compose (TangleFT t f m) f) -> m a
evalTangleFT m t = snd <$> runTangleFT m t blank
{-# INLINE evalTangleFT #-}

-- | Lift a collection of 'TangleT's so that it fits the argument of 'runTangleFT'.
liftTangles :: (FunctorB b, Functor m) => b (TangleT b m) -> b (Compose (TangleT b m) Identity)
liftTangles = bmap (Compose . fmap Identity)
{-# INLINE liftTangles #-}

-- | A product where all the elements are 'Compose' 'Nothing'
blank :: ApplicativeB b => b (Compose Maybe f)
blank = bpure $ Compose Nothing

-- | Bare version of 'TangleFT'
type TangleT t = TangleFT t Identity

-- | Non-transformer version of 'TangleFT'
type TangleF t f = TangleFT t f Identity

-- | Bare non-transformer tangle
type Tangle t = TangleFT t Identity Identity

-- | Bare variant of 'hitchF'
hitch :: Monad m
  => (forall h g. Functor g => (h a -> g (h a)) -> t h -> g (t h)) -- ^ van Laarhoven lens
  -> TangleT t m a
hitch l = runIdentity <$> hitchF l
{-# INLINE hitch #-}

evalTangleF :: ApplicativeB t => TangleF t f a -> t (Compose (TangleF t f) f) -> a
evalTangleF m t = snd $ runIdentity $ runTangleFT m t blank
{-# INLINE evalTangleF #-}

evalTangleT :: (Functor m, ApplicativeB t) => TangleT t m a -> t (TangleT t m) -> m a
evalTangleT m t = fmap snd $ runTangleFT m (liftTangles t) blank
{-# INLINE evalTangleT #-}

evalTangle :: (ApplicativeB t) => Tangle t a -> t (Tangle t) -> a
evalTangle m t = runIdentity $ evalTangleT m t
{-# INLINE evalTangle #-}
