---
title: mercury_module_interface_design
description: "Mercury's Module Interface Design checklist — parse-don't-validate, tighten inputs/constraints/outputs, one-input-one-output, no type blindness, test the API through the API"
---

**Source:** `~/Code/Mercury/mercury-web-backend/docs/best-practices/Module-Interface-Design.md`
(plus `docs/frontiers/` and `.cursor/rules/best_practices.mdc`).

The single most transferable document in that repo. The principles, with the ones
that bite hardest in this codebase first:

- **Use data as proof (parse, don't validate).** Export the *type* without its
  constructor and export only a `validate :: Raw -> Either (NonEmpty Err) Validated`
  that produces it. Then the action takes `Validated` and cannot be called with
  unchecked input. Validation can also *carry forward* data it had to fetch anyway.
- **Tighten constraints.** Ask for `HasGame m` (or narrower) rather than `GameT`;
  a narrower constraint is callable from more places, documents what the function
  *could* do, and is stubbable in tests.
- **Tighten inputs.** Don't take a value you can derive internally. Every extra
  input is a thing the caller must learn to fetch and a new validation failure mode.
- **Tighten outputs.** Anything you return becomes API. Project internal records
  down to the fields callers should depend on.
- **One input, one output.** Named param/result records on API boundaries, not
  positional tuples: adding a field stays backwards compatible.
- **One method per action.** A few powerful entry points beat a method per
  use-case; overlapping methods drift apart and callers pick the wrong one.
- **Avoid type blindness.** Prefer a two-constructor sum over `Bool`, a newtype
  over a bare `Int`/`Text`, a purpose-built sum over `Maybe`.
- **Use more general identifiers.** Take `OrganizationId` (here: `InvestigatorId`,
  `LocationId`) rather than the internal row id, and hide the row id behind an
  opaque key type you can only get from a lookup.
- **Handle all inputs gracefully** — illegal states unrepresentable, sensible
  defaults, or explicit errors; never "caller must ensure".
- **Test your API with your API.** Assert through the public entry points, not by
  reaching into internal state. The tests then survive refactors and double as docs.
- **Pair get with update subscriber.** When other systems need to react to a state
  change, publish an event from the writer rather than letting them poll or trigger.

**How to apply here:** engine helper modules (`Arkham/Helpers/*.hs`) are the natural
fit — most already lean this way with `HasGame m` constraints. The parse-don't-validate
and general-identifier points are the ones we most often skip.
Related: [[mercury_hlint_module_frontiers]], [[mercury_capability_classes_by_symbol]].
