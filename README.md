tangle
----

This package implements an abstraction of record construction where each field may depend on other fields.
It is a reimplementation of [extensible's Tangle](https://hackage.haskell.org/package/extensible-0.8.3/docs/Data-Extensible-Tangle.html) refined for more general HKDs.

```haskell
evalTangleT :: Tangle t m a -- ^ computation
  -> t (Tangle t m) -- ^ collection of tangles
  -> m a

hitch
 :: Monad m
  => (forall h. Lens' (t h) (h a)) -- ^ the lens of the field
  -> Tangle t m a
```

A computation tangle is a higher-kinded record of `Tangle t`. Each field may fetch other fields by using `hitch`; the result is memoised so the computation is performed at most once for each field. Recursive `hitch` is not supported (becomes an infinite loop).

`examples/weight.hs` demonstrates a simple program that calculates a body mass index.

First, we define a highed-kinded record of parameters.
```haskell
data Info h = Info
  { _height :: h Double
  , _mass :: h Double
  , _bmi :: h Double
  , _isHealthy :: h Bool
  } deriving Generic
instance FunctorB Info
instance ApplicativeB Info
makeLenses ''Info
```

Then describe how to compute each field as a record of `TangleT`s.
Note the `_bmi` field fetching `height` and `mass`.

```haskell
buildInfo :: Info (TangleT Info IO)
buildInfo = Info
  { _height = liftIO $ prompt "Height(m): "
  , _mass = liftIO $ prompt "Mass(kg): "
  , _bmi = do
    h <- hitch height
    m <- hitch mass
    return $! m / (h * h)
  , _isHealthy = do
    x <- hitch bmi
    pure $ x >= 18 && x < 22
  }
```

Finally, we can run a computation `go` with `evalTangleT`.
Behold, it asks for height and mass only once, even though the `bmi` field is used twice (via `isHealthy`).

```haskell
main :: IO ()
main = evalTangleT go buildInfo >>= print where
  go = (,) <$> hitch bmi <*> hitch isHealthy
```

What are tangles?
----

Tangle is a __memoisation__ monad for __heterogenous collections__ such as records.
If the collection were just a homogenous container, it is equivalent to

```haskell
type Key = String
type Container = Map.Map Key

newtype Tangle b a = Tangle { unTangle :: ReaderT (Container (Tangle b b)) (State (Container b)) a }
  deriving (Functor, Applicative, Monad)
```

where `Container (Tangle b)` is a collection of computations which would yield the final results,
and `Container b` is the cache of intermediate results.

With these in mind, the following function can be defined:

```haskell
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
```

Each computation in `Container (Tangle b b)` may `hitch` a value from other keys.
Since `hitch` caches its result, subsequent calls just return the same value;
that's how `Tangle` makes dependent computation composable.

In `Control.Monad.Tangle`, the container type is generalised to any higher-kinded datatypes.
HKD allows the container type to be in two forms: `t (Tangle t)` (collection of computations) and `t Maybe` (cache of results).
Not just it's much more flexible, the `ApplicativeB` constaint also ensures that the collection and the cache have the same shape.

Application
----

Tangles can split dependent computations into smaller pieces of code without explicity passing values by function arguments.
It implies that it's trivial to add/remove dependencies between computation, because they are automatically handled in the memoisation mechanism.
One useful property is that the cache can be reused as the second argument of `runTangleFT`.
If some fields are expensive to calculate and the rest is expensive to serialise,
the server application could compute the former, distribute the cache, and then let the clients fill the rest.

See also
----

* [Tangle: the dynamic programming monad](https://discourse.haskell.org/t/tangle-the-dynamic-programming-monad/2406)
* [拡張可能タングルでDo記法レスプログラミング♪ (Haskell)](https://matsubara0507.github.io/posts/2018-02-22-fun-of-extensible-3.html)