---
name: project_materialised_initiations_outlive_their_window_match
description: A second copy of a Forced card still initiates after the first copy has invalidated the window; the re-filter checks source + cost only, never the window matcher, so the handler must tolerate a gone entity (#5842)
metadata:
  type: project
---

`runWindow` materialises the whole initiation set when the window opens, and
`ResolveWindowInitiations` re-filters it each round with `initiationIsLive`
(`Investigator/Runner.hs:459`) — which is `sourceCanClaimUseAbility` plus
`getCanAffordAbility`, and deliberately **not** the window matcher: re-deriving would lose
initiations whose source left play mid-resolution (#5764, #5743). Ability *criteria* are not
re-checked there either.

So two copies of a card whose Forced ability consumes the window's subject both resolve, even
though the first one destroyed the subject. Memory Variant is quantity 2, sits
`NextToAgenda`, and on `PlayEventDiscarding #after You ActiveEvent` sets the event aside as a
hollow. With both copies out, one played event opened one window (shared `windowBatchId`),
the first copy pushed `RemoveEvent`, and the second crashed on `fetchCard eid` —
`Unknown event: …` (#5842). `cancelWindowBatch` does not drop the sibling initiation.

**How to apply:** a Forced/reaction handler that removes the window's subject must tolerate
being run a second time against a subject that is already gone. Guard on the entity, don't
assume the matcher still holds. For Memory Variant the no-op is also the rules answer: the
replacement is "*instead of discarding it*", and the event is no longer being discarded.

**To stop the dead button as well, share the bucket across copies.** `GroupLimit PerWindow`
matches `(== ability) . usedAbility` — full `Ability` equality, which includes
`abilitySource` — so each copy gets its own bucket. `MaxPer CardDef` matches by card
*title*, so it spans copies, and the limit check lives in `getCanAffordUse` →
`getCanAffordAbility`, which is half of `initiationIsLive` — so a full shared bucket drops
the sibling at the next re-filter round instead of re-offering it. `MaxPer _ PerWindow` did
not work before #5842: its evaluation had no `ws` intersection (so it read as once per
*game*) and `EndCheckWindow`'s `filterAbility` pruned only the `PlayerLimit`/`GroupLimit`
PerWindow records. Both were filled in; no card used that combination before.

Note the set is still derived before any use is recorded, so **both buttons appear in the
first ask** — the saving is the second round, not the first. And the limit records a
*use*, not an *effect*: if the first copy's effect ever fizzled, the second would be blocked
even though the subject survived. That is why the entity guard stays as the fallback.

Related: `fetchCardMaybe` on an entity id used to **throw** rather than answer `Nothing` —
every id-keyed instance was `fieldMap Field.XCard Just`, i.e. `field` → `getEvent` →
`fromJustNote`. They now use `fieldMay`, which checks `project` first. The `Nothing ->`
branches in `sourceTypes` (`Helpers/Source.hs:398-420`) were dead code until that fix.
