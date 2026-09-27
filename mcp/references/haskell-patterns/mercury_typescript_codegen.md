---
title: mercury_typescript_codegen
description: "`deriveJSONAndTypeScript` derives the aeson instances and the matching TypeScript declaration from one TH splice, so backend and frontend types cannot drift"
---

**Source:** `local-packages/mercury-typescript/` (wraps `aeson-typescript`),
`.cursor/rules/json-derivation-pattern.mdc`.

One splice per type emits both instances:

```haskell
$(deriveJSONAndTypeScript (jsonDeriveWithAffix "adminOrgBalance" jsonDeriveOptions) ''AdminOrgBalance)
```

`jsonDeriveWithAffix "prefix"` *validates* that every field starts with the prefix
before stripping it — a renamed field is a compile error, not a silently changed wire
name. The house rule bans inline `fieldLabelModifier` lambdas in favour of these named
helpers.

Supporting types in that package are each a small trick worth knowing:

- **`TotalMap k v`** — a newtype over `Map` whose only purpose is to emit
  `{[k in Key]: V}` instead of `{[k in Key]?: V}`. A type used purely to steer codegen.
- **`TypeScriptBranded (name :: Symbol) a`** — emits a TS `unique symbol` brand plus a
  `mkFoo` smart constructor, so a Haskell newtype id stays a distinct type in TS
  instead of collapsing to `string`.
- **`StringLiteralUnion`** — derives a TS string-literal union *and* exports the array
  of all values, so the frontend can iterate the enum exhaustively.
- One hard rule from `best_practices.mdc`: **never derive TS from database/model types.**
  Generate from a dedicated wire type so the schema and the API can move independently.

**How to apply here:** `frontend/src/arkham/types/` is ~55 hand-written `.ts` files
mirroring backend JSON — every field rename is a silent runtime decode failure
(cf. [[project_other_modifier_decoder_drops_contents]], where an unlisted modifier tag
decoded fine but lost its payload). Generating even the leaf enums and the
`Message`/`Modifier`/`Window` tag unions from the Haskell definitions would close a
whole bug class. Note this repo derives JSON with `deriving anyclass`, not TH, so
adopting it means moving those types to a TH splice
— see [[mercury_generic_is_slow_prefer_th]].
Related: [[mercury_th_gated_derivation]].
