---
title: mercury_hlint_module_frontiers
description: "Architectural boundaries enforced by hlint `modules:` + `within:` rules — an `.Internal` module is importable only by its siblings, and the error message names the blessed API and the owning team"
---

**Source:** `docs/frontiers/`, `hlint-rules/*.yaml` (split per domain:
`frontiers.yaml`, `conversion-frontiers.yaml`, `accounting-integrations.yaml`,
`exceptions.yaml`, `partial-functions.yaml`, `handler-usage.yaml`, `list-literals.yaml`).

The whole "Frontier" concept is one rule: *if a module is Internal, only sibling
modules may import it* — enforced entirely in `.hlint.yaml`:

```yaml
- modules:
  - name: [PersistentModels.RutterVendor]
    within:
      - Mercury.Accounting.Rutter.Vendor.**
      - Mercury.FakeData.Populate.RutterVendor
    message: |
      Please use the safe interface exposed at Mercury.Accounting.Rutter.Vendor
```

What makes it work in practice:

- **The message is the documentation.** It names the replacement API and the Slack
  channel of the team that owns it. Engineers meet the rule at the moment they'd
  otherwise reach past it.
- **Rules are split into per-domain files**, so a team owns its boundary file and
  merge conflicts stay local.
- **`within:` accepts function-level scope** (`Module.someFunctionName`), so a
  permanent exception can be pinned to exactly one call site.
- Boundaries are drawn primarily around **database tables**: the goal is that systems
  coordinate through code, never through a shared table. That's the deeper rule —
  the hlint entries are just its enforcement.

**How to apply here:** `backend/.hlint.yaml` is 69 lines and its `modules:` block is
only qualified-import aliases. Candidates for real boundary rules: keep `Arkham.Game`'s
internals out of card modules; force the `*.Lifted` variant where one exists (already a
CLAUDE.md convention, currently unenforced); stop cards importing another card's module;
keep `Arkham.Message` constructors out of the frontend-facing serialization layer.
Related: [[mercury_hlint_ratchet_migration]], [[mercury_module_interface_design]].
