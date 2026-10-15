---
title: project_expose_route_is_raised_at_initiation_not_through_flip
description: An expose's route (fight/evade/discover) cannot reach `exposed`, because `Flip` sits between the site that knows it and the exposure — so `exposedVia` is raised at the initiation site; and `ScenarioModifier "doNotRemove"` is already claimed by Congress of the Keys
---

A card that reacts to *how* a concealed mini-card was exposed — The Phantom of
the Opera's three Always Watching treacheries ("via investigating", "via
evading", "via fighting") and its three Conspirator enemies ("after you expose
**this card's** mini-card via …") — cannot be served by `exposed` /
`exposedDecoy` in `Campaigns/TheScarletKeys/Helpers.hs`. Those raise only
`exposed[enemy]`, `exposed[decoy]`, `exposed[<cardcode>]` and
`exposed[<enemyId>]`, and `Matcher.CampaignEvent` matches on the key text alone
— the payload is not matchable.

The route cannot be threaded to them either, and the reason is structural:
`exposed`/`exposedDecoy` run inside `DoStep 1 (Flip …)`, on the far side of a
`Message` boundary from every site that knows the route. Passing it as an
argument means widening the core `Flip` constructor, which is used engine-wide
and serialized into every stored step — an undo/replay-shaped change. Ambient
state means a new persisted field or a modifier with a lifetime problem, and
both widen `gameActionDiff`.

So the tag is raised at the **expose-initiation site** instead, which costs no
state: `exposeConcealedVia route iid source cid` emits the byte-identical `Flip`
and then one extra `#after` window. Correctness rests on one invariant worth
knowing:

> `push (Flip …)` followed by `checkAfter w` resolves `w` after the *entire*
> `Flip` subtree, because `runQueueT` flushes its buffer in order to the front of
> the queue and each handler prepends its own messages ahead of what follows.

That is the same ordering `exposedDecoy` already depends on when it brackets its
pushed `scenarioSpecific "exposed[decoy]"` with when/after checks, so it is
load-bearing today rather than a new assumption.

Consequences to keep in mind:

* **No `#when` route window exists, deliberately.** At initiation the exposure
  has not begun, so a `when` there would be a genuinely different moment from
  `exposed`'s own `when`. One would have to go through `Flip`.
* **Card-code is the finest grain available.** `exposed` can raise
  `exposed[<enemyId>]` because the in-shadows enemy is chosen inside
  `DoStep 1 (Flip …)`; at initiation that choice has not happened. Fine for
  unique enemies; a non-unique one needing per-copy targeting is a different
  change. The code used is the **mini-card's** def (`concealedToCardDef`), which
  differs from the in-play enemy for La Chica Roja.
* **The raise belongs inside the choice continuation**, not around the prompt,
  or declining `doNotExposeConcealed` would raise the window anyway.
* **An untagged expose raises nothing** — the route is never inferred. Roughly
  fifteen official expose sites are still untagged, so "a card effect that deals
  damage / automatically evades" does not yet wake a route-sensitive card from
  them. `InspectorFlintWithPrideAndCare` should stay untagged: it re-exposes in
  reaction to another exposure and does not know the original route.

## Do not overload `ScenarioModifier "doNotRemove"`

Suppressing the automatic sweep in `Concealed/Runner.hs`'s `DoStep 2 (Flip …)`
(`unless inShadows $ push RemoveAllConcealed`) looks like a job for the existing
`ScenarioModifier "doNotRemove"` that `ConcealedCard`'s `RemoveAllConcealed`
case already honours. **It is not.** That string is live official content:
`GravityDefyingClimb` and `TheToweringVertexRuinousConflux` stamp it on grid
mini-cards while revealed, and Congress of the Keys reads it at three sites.
Reusing it stops those cards being swept.

The separate flag is `ScenarioModifier "keepConcealedInPlay"`
(`getKeepsConcealedInPlay`), gating only the automatic sweep; an explicit
`RemoveAllConcealed` still removes everything. A card sets it from
`HasModifiersFor`, not from a reaction: the suppression must be live when
`DoStep 2` runs, which is **before** the after-expose windows.

That ordering is also why "return this enemy to the shadows when exposed" has to
be an `#after` ability. `exposed`'s body calls `enemyMoveToIfInPlay`, which
pushes `IfEnemyExists eid [Move …]` ignoring placement, so a return done in the
`#when` window is undone by that move a moment later — and `#after` is past the
sweep, which is what makes the suppression necessary rather than optional.
