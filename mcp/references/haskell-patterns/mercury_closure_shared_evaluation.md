---
title: mercury_closure_shared_evaluation
description: "A CPS-encoded free monad (`Closure var input t`) with rank-2 `var` bindings that evaluate at most once — one rule definition runs purely (property tests), against a DB, or with a trace"
---

**Source:** `local-packages/mercury-decision-engine-simple/` — `README.md`,
`src/Mercury/DecisionEngine/Simple/Closure.hs`,
`src/Mercury/DecisionEngine/Experimental/Core/Closure.hs`.

The shape:

```haskell
newtype Closure var ann input t = Closure
  { unClosure
      :: forall eval. Monad eval
      => (forall x. SourceLocationInfo -> var x -> eval x)                              -- use a binding
      -> (forall x. SourceLocationInfo -> input x -> eval x)                            -- use an input
      -> (forall x. SourceLocationInfo -> Maybe (ann x) -> Closure var ann input x -> eval (var x))  -- bind
      -> eval t
  }

input :: input t -> Closure var input t          -- a GADT constructor names each "argument"
let_  :: Closure var input t -> Closure var input (var t)   -- suspend; evaluated at most once
get   :: var t -> Closure var input t                       -- force the suspension
```

Four ideas worth stealing independently of each other:

1. **Inputs are a GADT, not a record.** `data Input t where ArgX :: Input Int; ...`.
   A *valuation* `forall x. Input x -> m x` supplies them, so the same expression can
   be fed constants, `Gen`s, or database reads.
2. **`let_`/`get` make sharing explicit and interpreter-controlled.** The interpreter
   decides whether a binding is memoised — so a rule can bundle expensive reads and
   *reuse* them across rules, which plain `let` in `m` cannot express.
3. **Rank-2 `var` (the `runST` trick).** `runClosureWithInputs :: (forall var. Closure var input t) -> ...`
   forces every binding to stay local to the closure, so the interpreter may pick
   whatever representation it wants (`STRef`, `IORef`, pure thunk).
4. **Annotations on bindings (`ann`) are free tracing.** They never change meaning;
   they let one interpreter emit a `Trace`/hedgehog doc of what was evaluated and why.
   The README's rule: give as much annotation as you can — you can strip it later,
   you can't add it retroactively.

**How to apply here:** this is the shape that would let `Arkham.Matcher` /
`HasModifiersFor` be defined once and run (a) against the live `GameT`, (b) purely
against a fixture board in tests, and (c) with a trace explaining *why* a card was or
wasn't selectable — which is the recurring debugging pain. The `let_` sharing is the
answer to repeated `select`/`getModifiers` inside one window check
(cf. [[project_act_advance_window_check_cost]] in engine-gotchas).
Related: [[mercury_typelevel_state_machine]].
