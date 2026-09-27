---
title: mercury_dynamic_stubs
description: "A Typeable-keyed registry of stub modules: production code wraps an effect in `withDynStub`, tests swap in a `Stub` that also records every argument it was called with"
---

**Source:** `local-packages/mercury-stubs-internal/src/Stubs/{Class,Types,StubModule}.hs`,
`docs/stubs.md`.

```haskell
data Stub m arg result = Stub
  { stubArguments   :: IORef [arg]     -- every call is recorded, for assertions
  , stubbedFunction :: arg -> m result
  }

class Monad m => HasDynStubs m where
  fetchDynStub :: forall k. Typeable k => m (Maybe k)

stubbed :: (Typeable k, HasDynStubs m, MonadIO m)
        => (k -> Maybe (Stub m a r)) -> a -> m r -> m r
stubbed accessor arg realImpl = ...   -- runs realImpl unless a stub is installed
```

The design decisions worth copying:

- **The call site names the real implementation.** `withDynStub @FooStubs stubDoBar arg
  realImpl` reads top-to-bottom as "do the real thing, unless stubbed" — no interface
  indirection, no mock object, and production always takes the real branch.
- **Stubs are grouped into domain records** (`Stubs.Notifications.Push`) with a
  `StubModule` instance giving `emptyStubs`; the registry is keyed by `Typeable`, so
  adding a new stub module touches no central type.
- **The stub records its arguments** in an `IORef`, so "was this called, and with what"
  is an assertion rather than a separate spy mechanism.
- **Type synonyms cover the common shapes** (`StubResult`, `StubRequestResultIO`, …) so
  the `forall m. Applicative m => Maybe (Stub m () r)` noise stays out of user code.
- `hoistStub` and a `Functor` instance let one stub be reused across monads.

**How to apply here:** the test harness fights exactly this problem — see
`project_test_harness_gotchas` and `project_test_playcard_helper_skips_attack_of_opportunity`
in memory. Chaos-bag draws, shuffles, the RNG and "now" are the natural stub points; the
argument-recording IORef is the part that would make "assert this card queued that
message" a one-liner instead of a queue scan.
Related: [[mercury_capability_classes_by_symbol]].
