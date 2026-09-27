---
title: mercury_th_gated_derivation
description: "A deriving splice that fails the build when a type breaks house rules (missing Haddocks, wrong constructor casing), with a checked-in CSV of grandfathered names so the rule can land before the cleanup"
---

**Source:** `local-packages/mercury-typescript/src/Mercury/TypeScript.hs`,
`.../TypeScript/RequireDocs.hs`, `.../TypeScript/ValidateOptions.hs`.

`deriveJSONAndTypeScript` is not a thin wrapper — before delegating it runs checks in `Q`:

```haskell
deriveJSONAndTypeScriptWithOverrides opts overrides name = do
  when overrides.derivingOverrideCheckHaddock          (checkHaddock name)
  when overrides.derivingOverrideRequireLowerCamelCase (validateOptions opts name)
  TS.deriveJSONAndTypeScript opts name
```

`checkHaddock` uses `reifyType`/doc retrieval to fail compilation if the type or its
fields are undocumented — every type crossing the API boundary is forced to explain
itself. Two details make it survivable in a big codebase:

- **A grandfathering allowlist**, `data/MissingDocs/top-level-types.csv`, `$(embedFile)`d
  at compile time into a `HashSet`. Existing offenders are listed; new ones fail. The
  file only ever shrinks.
- **Explicit escape hatches** (`DerivingOverride`) with documented legitimate reasons,
  rather than "add a pragma and hope".
- **Exempt module prefixes** (emails, PDFs) where the boundary doesn't warrant the cost.
- A `CPP` flag (`BUCK2_SKIP_CHECK`) so the alternate build can skip the checks.

The general technique: **put the lint inside the thing everyone already has to call.**
A rule attached to the deriving splice cannot be forgotten the way a separate linter can.

**How to apply here:** the card-implementation conventions we keep re-learning are
exactly this shape — e.g. a splice/helper that refuses to compile an ability whose
`Forced` window has a location-only `Who` (cf. [[project_forced_window_who_must_be_you]]),
or that requires a Haddock on each new `Message` constructor. The CSV ratchet is the
part to copy first; it's what makes a new rule landable in one PR.
Related: [[mercury_hlint_ratchet_migration]], [[mercury_typescript_codegen]].
