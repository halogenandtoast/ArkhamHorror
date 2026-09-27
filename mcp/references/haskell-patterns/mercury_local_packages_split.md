---
title: mercury_local_packages_split
description: "~90 in-repo local packages carve a monolith into units with declared dependency direction — bounding rebuilds, forcing layering, and making extraction to OSS a later decision rather than a rewrite"
---

**Source:** `local-packages/` (~90 packages), `local-packages/working-with-local-packages.md`,
`docs/splitting-up-mwb.md`.

`src/` holds ~11,800 modules; the interesting engineering is in how much has been pulled
*out* of it. Observed rules:

- **A package per coherent concern**, including very small ones: `mercury-dollar`,
  `mercury-uuid`, `mercury-country`, `mercury-casing`, `mercury-time`, `this-or-that`,
  `bounded-enum-th`. Small is fine; the point is the dependency edge, not the line count.
- **Class packages are separate from implementation packages** and depend on almost
  nothing, so instances flow toward the class rather than the class accreting imports
  ([[mercury_display_vs_show]]).
- **READMEs state intent and trajectory**, not just usage: `mercury-http-client` lays out
  a four-step plan to become the only HTTP interface; `mercury-decision-engine-simple`
  opens with "**EXPERIMENTAL**, this is an iteration on -core"; `mercury-banking-base`
  exists solely to own migrations shared by two sibling packages.
- **"Don't depend on internal packages here, we want to OSS this eventually"**
  (`openapi3-th`, `mercury-display`) — extraction stays cheap by construction.
- The split is also a **build-time strategy**: a change inside one package rebuilds that
  package and its dependents, not the world.

**How to apply here:** `backend/` is one `arkham-api` package plus `cards-discover` and
`validate`, and build time is a standing complaint. The natural seams are the ones
already conceptually separate: the card *data*/`CardDef` layer, the matcher/modifier DSL,
the message/queue core, the Yesod API layer, and the campaign/scenario content. Each has
a clear dependency direction, and content changes (the bulk of the churn) would stop
rebuilding the engine.
Related: [[mercury_module_interface_design]], [[mercury_generic_is_slow_prefer_th]].
