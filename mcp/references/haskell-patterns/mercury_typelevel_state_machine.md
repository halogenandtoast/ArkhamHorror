---
title: mercury_typelevel_state_machine
description: "Singletons + type families make illegal state transitions unrepresentable, and the same type-level description generates documentation, diagrams and exhaustive path enumeration for free"
---

**Source:** `local-packages/mercury-glassbox/` — `README.md` (mermaid architecture),
`src/Mercury/Banking/Framework/StateMachine/{Class,Types,Introspection}.hs`,
`src/Mercury/Typelevel/Utils.hs`.

Each business process `w` is a kind. Its states, initializations and transitions are
promoted data, and type families wire them together:

```haskell
type TransitionStateDef w t =
  StateMachine w
  => Sing t                                     -- which transition (singleton tag)
  -> TransitionPayload w t                      -- its payload, determined by the tag
  -> StateData w (TransitionFrom w t)           -- source state, determined by the tag
  -> StateData w (TransitionTo w t)             -- target state, determined by the tag

class StateMachineConstraints w => StateMachine w where
  initializationDefinitions :: InitializationStateDef w i
  transitionDefinitions     :: TransitionStateDef w t
  describeStateKind         :: StateKind w -> Maybe Text   -- docs live with the machine
```

Techniques on display:

- **`Sing t` as the branch tag.** Matching on the singleton refines the payload *and*
  the source/target state types simultaneously — a transition physically cannot be
  applied from the wrong state.
- **Custom type errors** via `Unsatisfiable`: `Contains ks k` reports
  `"X is not legal. Allowed kinds: [...]"` instead of a wall of unification failure
  (`Mercury/Typelevel/Utils.hs`, with type-level `++` and `<$>` over lists).
- **`describe*Kind` methods put prose next to the machine**, and `Introspection.hs`
  turns the whole thing into readable output — the README's pitch is "check
  ExampleOutput.md to see the introspection we get generically".
- **Embeddings**: a state can encapsulate a child state machine, with typed
  initialization-in / termination-out — composition without flattening.
- `Enumerable` (`Mercury/Common/Enumerable.hs`) replaces `(Enum, Bounded)` because it
  also handles uninhabited types — needed when enumerating all legal paths for tests.

**How to apply here:** this is heavy machinery and almost certainly the wrong trade for
card code. What *is* transferable: the idea that the timing structure (phases, windows,
skill-test steps ST.1–ST.8) should be a single declarative description that both drives
the runtime and generates the documentation we currently maintain by hand in
`engine-gotchas/`, plus exhaustive path enumeration as a test oracle.
Related: [[mercury_closure_shared_evaluation]], [[mercury_kind_codec]].
