---
title: mercury_generic_is_slow_prefer_th
description: "`deriving Generic` + anyclass JSON is the biggest avoidable compile-time cost in a large Haskell codebase; Mercury lints against it and routes every use to a TH or hand-written alternative"
---

**Source:** `local-packages/mercury-generics/src/Mercury/Generics.hs` — a module whose
entire content is `module GHC.Generics` re-exported, wrapped in a long Haddock that hlint
points you at when you write `deriving Generic`. Also `openapi3-th/README.md`.

The documented alternatives, per use:

| Instead of deriving via `Generic` | Use |
| --- | --- |
| `ToJSON`/`FromJSON` | `$(deriveJSON opts ''Foo)` from `Data.Aeson.TH` |
| `Arbitrary` for an enum | `deriving stock (Enum, Bounded)` + `arbitraryBoundedEnum` |
| `Arbitrary` for a sum | hand-written `oneof [...]` — boilerplate-y but far cheaper |
| `ToSchema` (openapi) | `$(deriveToSchema ''Foo)` — they wrote `openapi3-th` purely because Generic derivation was "extremely slow" |
| `ParseRecord` (options) | go through `ParseFields`/`ParseField` instead |

Two caveats they state honestly: TH incurs a **recompilation** cost (a change to a TH-using
module rebuilds its dependents), so the ideal is to put TH-derived types in small modules
with few dependencies. And a `Generic` instance that genuinely pulls its weight (several
derived classes) is fine — the rule is about the ones deriving a single instance.

**How to apply here:** `arkham-api/library` has ~6400 `deriving anyclass` sites and ~370
`Generic` derivations, on a codebase where build time is a standing complaint
(cf. `project_build_memory_simplified_core` in memory). This is a measurable experiment,
not a rewrite: convert the JSON derivation of one heavily-instantiated module tree (the
card attrs types, or `Arkham.Message`) to `Data.Aeson.TH` and time a clean build. It
also composes with [[mercury_typescript_codegen]], which requires TH derivation anyway.
