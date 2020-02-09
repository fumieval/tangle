tangle
----

This package implements an abstraction of record construction where each field may depend on other fields.

```haskell
evalTangleT :: TangleT t m a -- ^ computation
  -> t (TangleT t m) -- ^ collection of tangles
  -> t Maybe -- ^ initial fields (usually all Nothing)
  -> m a

hitch
 :: Monad m
  => (forall h. Lens' (t h) (h a)) -- ^ the lens of the field 
  -> TangleT t m a
```

A computation tangle is a higher-kinded record of `Tangle t m`. Each field can fetch other fields by using `hitch`; the result is memoised so the computation is performed only once for each field. Recursive `hitch` is not supported.

See `examples/weight.hs` for an example.
