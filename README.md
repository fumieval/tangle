tangle
----

This package implements an abstraction of record construction where each field may depend on other fields.
It is a reimplementation of [extensible's Tangle](https://hackage.haskell.org/package/extensible-0.8.3/docs/Data-Extensible-Tangle.html) refined for more general HKDs.

```haskell
evalTangle :: Tangle t a -- ^ computation
  -> t (Tangle t) -- ^ collection of tangles
  -> a

hitch
 :: Monad m
  => (forall h. Lens' (t h) (h a)) -- ^ the lens of the field
  -> Tangle t a
```

A computation tangle is a higher-kinded record of `Tangle t`. Each field can fetch other fields by using `hitch`; the result is memoised so the computation is performed only once for each field. Recursive `hitch` is not supported.

See `examples/weight.hs` for an example.

See also
----

* [Tangle: the dynamic programming monad](https://discourse.haskell.org/t/tangle-the-dynamic-programming-monad/2406)
* [拡張可能タングルでDo記法レスプログラミング♪ (Haskell)](https://matsubara0507.github.io/posts/2018-02-22-fun-of-extensible-3.html)