# jq-optics

A toy implementation of the [jq](https://jqlang.github.io/jq/) language using optics.

> [!WARNING]
> This is a work in progress. There is no parser yet, let alone a CLI. The source code might be hard to understand if you're not already familiar with [lens](https://github.com/ekmett/lens/).

```haskell
jq :: Env -> Filter -> IndexedJourney' Path Value Value
```

A [traversal](https://hackage.haskell.org/package/lens-5.2.2/docs/Control-Lens-Combinators.html#t:Traversal) is a view of several disjoint targets in a data structure. Traversals obey laws that ensure the same target can't be visited twice, and are hard to compose side-by-side. Journeys are lawless traversals that are allowed to visit the same target multiple times, so that all the targets are updated sequentially. This allows representing jq filters as monomorphic journeys on JSON values.

In the [CPS / van Laarhoven representation](https://www.twanvl.nl/blog/haskell/cps-functional-references) of optics, journeys are obtained by promoting Traversal's constraint from `Applicative` to `Monad` (and the indexed variant is obtained as usual):

```haskell
type Journey s t a b = forall m. Monad m => (a -> m b) -> s -> m t
type IndexedJourney i s t a b = forall p m. (Indexable i p, Monad m) => p a (m b) -> s -> m t
```

More concretely, `Journey s t a b` is isomorphic to `s -> Free (PStore a b) t`, where `Free` is the free monad construction and

```haskell
data PStore a b t = PStore a (b -> t)
```

So a journey takes an initial value and produces a series of "interaction points" that each produce a value (and possibly an index) and expect a replacement value, and in the end returns a final value that is the result of performing the replacements on the initial value. Journeys are composed side-by-side straightforwardly using `(>=>)`. Every traversal is a journey, and every journey is a setter, but journeys have no subtyping relationship with folds. Most of the lens combinators for folds have to be reimplemented and they behave slightly differently.

> [!NOTE]
> This project does not aim to be an *exact* clone of jq; in fact, the implementation departs from jq's in several ways:
> - Assignment is sequential, i.e. `(a, b) = c` is equivalent to `(a = c) | (b = c)`. See `[0, 2, 3] | (.[.[0]], .[.[0]]) = 1`. On the other hand, this means that `.[a, b]` is not in general equivalent to `.[a], .[b]`.
> - `|=` has cartesian product semantics instead of jq's behaviour, which is to take the first produced value, or delete the entry if no values are produced.
> - Assigning to a non-path-expression is a no-op instead of an error.
