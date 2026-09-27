---
title: Haskell patterns index
description: "Design patterns and advanced Haskell techniques observed in mercury-web-backend, noted as references for this codebase"
---

# Haskell patterns worth borrowing

Extracted from `~/Code/Mercury/mercury-web-backend` (a ~11,800-module Yesod/Persistent
monolith plus ~90 in-repo local packages) on 2026-09-11. Each entry states the pattern,
where it lives over there, and where it would land here. **These are references, not
decisions** — nothing here has been adopted.

Read the source alongside an entry; paths are relative to that repo's root.

## Interface & architecture

- [mercury_module_interface_design](mercury_module_interface_design.md) — the whole
  checklist: parse-don't-validate, tighten inputs/constraints/outputs, one input/one
  output, avoid type blindness, general identifiers, test the API through the API.
  **Read this one first.**
- [mercury_hlint_module_frontiers](mercury_hlint_module_frontiers.md) — module
  boundaries enforced in `.hlint.yaml` (`modules:` + `within:`), with the error message
  naming the blessed API and its owning team.
- [mercury_local_packages_split](mercury_local_packages_split.md) — ~90 local packages
  bound rebuilds and force dependency direction; class packages depend on nothing.
- [mercury_capability_classes_by_symbol](mercury_capability_classes_by_symbol.md) —
  `HasSetting (name :: Symbol) a m`, a Symbol-indexed capability class backed by
  `HasField`, so a function depends on one field instead of a whole record.
- [mercury_custom_prelude_discipline](mercury_custom_prelude_discipline.md) — a prelude
  that imports nothing from its own app, and doubles as the lever for codebase-wide changes.

## Making rules unforgettable

- [mercury_hlint_ratchet_migration](mercury_hlint_ratchet_migration.md) — ban the
  pattern, list every current offender under `within:`, chunk with ticket comments,
  delete a chunk per PR. How a repo-wide refactor actually lands.
- [mercury_th_gated_derivation](mercury_th_gated_derivation.md) — a deriving splice that
  fails the build on missing Haddocks or wrong casing, with an embedded CSV of
  grandfathered names so the rule ships before the cleanup.
- [mercury_require_callstack](mercury_require_callstack.md) — a constraint GHC won't
  auto-solve, forcing every throwing function to propagate or explicitly discharge it.

## Types & codegen

- [mercury_typescript_codegen](mercury_typescript_codegen.md) — one splice derives the
  aeson instances *and* the TypeScript declaration; plus `TotalMap`, branded newtypes,
  string-literal unions. Directly relevant to our 55 hand-written `.ts` type files.
- [mercury_generic_is_slow_prefer_th](mercury_generic_is_slow_prefer_th.md) — `deriving
  Generic` is the big avoidable compile cost; the table of TH/hand-written alternatives.
- [mercury_bounded_enum_th](mercury_bounded_enum_th.md) — derive the exhaustive list
  from the type. The fix shape for our hand-maintained `allAssets`/`allEvents`.
- [mercury_kind_codec](mercury_kind_codec.md) — namespaced text encoding for trees of
  promoted kinds, with a `Show`-based default instance.
- [mercury_this_or_that_untagged_union](mercury_this_or_that_untagged_union.md) — an
  `Either` whose JSON carries no constructor tag, and why that belongs in the type.
- [mercury_display_vs_show](mercury_display_vs_show.md) — `Display` for humans vs `Show`
  for Haskell, and the dependency-inversion rule for class-providing packages.

## Evaluation, effects & state

- [mercury_closure_shared_evaluation](mercury_closure_shared_evaluation.md) — a
  CPS-encoded free monad with rank-2 bindings evaluated at most once, so one rule
  definition runs purely, against a DB, or with a trace. **The most interesting idea in
  the repo** and the closest fit to our matcher/modifier layer.
- [mercury_typelevel_state_machine](mercury_typelevel_state_machine.md) — singletons +
  type families making illegal transitions unrepresentable, with generic introspection
  producing docs and diagrams. Heavy; the transferable part is the declarative description.
- [mercury_dynamic_stubs](mercury_dynamic_stubs.md) — Typeable-keyed stub registry where
  each stub also records every argument it was called with.
- [mercury_cascade_fallback](mercury_cascade_fallback.md) — `Stop` vs `Continue` as
  distinct failure kinds, and keeping the list of options you walked past.

## Operations

- [mercury_exception_design](mercury_exception_design.md) — one record-shaped exception
  per failure mode, carrying every id needed to debug it; no stringly invariants.
- [mercury_structured_logging](mercury_structured_logging.md) — `MercuryLog` class,
  `(-:)` structured fields, `withLogContext` scoping.
- [mercury_typed_config_and_flags](mercury_typed_config_and_flags.md) — named flag
  constants, safe-by-default values, stub hooks so both branches are tested.
