---
title: mercury_hlint_ratchet_migration
description: "How to land a codebase-wide refactor without one giant PR: ban the old pattern in hlint, list every current offender under `within:`, chunk the list with ticket comments, then delete chunks one PR at a time"
---

**Source:** `docs/best-practices/incremental-code-migration.md` (Matt Parsons, 2025-04-10).

```yaml
- functions:
  - name: throwWithCallStackDB
    message: |
      This function can throw but doesn't say so in its type. Use
      `throwWithCallStack`, which incurs `MonadThrow`. Questions: #mighty-dux
    within:
      - Mercury.Persistent.Operation      # permanent, legitimate
      # DUX-1234
      - Some.Cool.Module
      - Some.Other.Module
      # DUX-1235
      - Mercury.Banking.Core.Types
```

The workflow: write the rule → `make hlint-all` → paste the failures into `within:` →
chunk them into groups of 5-15 with a ticket-number comment → clear one chunk per PR.

Why each part matters:

- The rule **stops the bleeding immediately** even though the cleanup takes months.
- The `message:` **teaches the new practice** at the exact moment someone would have
  written the old one — better reach than any announcement.
- **Chunking by comment block** keeps PRs small, reviewable by the owning team, and —
  critically — keeps the exclusion list from becoming one giant merge-conflict magnet.
- Some exclusions turn out to be **legitimately permanent**; document those in place,
  ideally scoped to a single function rather than a whole module.

**How to apply here:** the ready-made target is bare `error "..."` in the engine — 911
call sites in `arkham-api/library`, each one a 500 with no context. Also: hand-written
frontend types, `deriving anyclass (ToJSON)` on hot types, direct `field` reads where a
`select` is required. Any of these is a "ban + list + chunk" job rather than a rewrite.
Related: [[mercury_require_callstack]], [[mercury_hlint_module_frontiers]], [[mercury_th_gated_derivation]].
