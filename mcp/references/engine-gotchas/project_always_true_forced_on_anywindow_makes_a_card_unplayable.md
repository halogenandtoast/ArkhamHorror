---
title: project_always_true_forced_on_anywindow_makes_a_card_unplayable
description: A Forced ability on AnyWindow whose criterion never stops holding does not merely loop — the card carrying it cannot be played at all, and nothing says why
---

A **Forced** ability whose window is `AnyWindow` and whose criterion is permanently
true makes its own card unplayable. The card is still offered in the player window;
answering the choice does nothing at all. No error, no log line, and the scenario
step counter does not move -- the only tell that anything was attempted.

A forced ability must resolve at every timing point where it is eligible, and
`AnyWindow` matches every window, including the ones opened while the card is being
played. The criterion never falsifies itself, so there is no point at which the
ability has finished, and the play never completes.

This is the opposite failure from [[project_anywindow_reaction_nests_in_its_own_activation]],
where a `freeReaction AnyWindow` is the CORRECT reading of a condition-only `{reaction}`.
A reaction is offered and may be declined; a forced one cannot be, so the two differ
exactly on whether the ability is allowed to not fire.

What made it easy to write: a card that says "if X, discard it" reads like a Forced
ability with X as the criterion. For a card whose X is a *state* rather than a
*moment* -- "if this has no supplies", "while this has no damage" -- there are three
honest shapes instead:

* a step at the end of whatever changes the state, which is where the card usually
  means it ("spend the last supply: ... then discard it");
* a Forced on the window where the state CAN change (`SpentUses`, `PlaceTokens`),
  so it is asked once per change rather than once per window;
* the entity's own `whenNoUses` field, for an asset -- `DiscardWhenNoUses`. There is
  no def-level equivalent, and no equivalent at all for an event, which is what
  pushes an event toward the first shape.

Found on a custom card (Safety Net, Circus Ex Mortis), whose supplies were also
missing -- making the criterion true from the moment it entered play -- but the
unplayability does not depend on that: any always-true criterion does it.

Nothing static catches this. The def validates, every constructor exists, no binding
is unbound, and the ability is well formed. It is only visible by playing the card
and noticing the step counter stay put.

## What actually stops a Forced repeating

`defaultAbilityLimit` gives every Forced shape a *group* limit, and the window
decides the period: a `SkillTestResult` window is `PerTest`, `Moves` and a
non-after `Enters` are `PerMove`, `EnemySpawns` is `PerSpawn`, everything else
`PerWindow`. The period is what a permanently-true criterion runs up against --
`PerWindow` only intersects `usedAbilityWindows`, so if the same window is
re-raised the bucket is empty again and the ability fires once more.

`SilentForcedAbility` used to take a bare `PerWindow 1` whatever its window,
which is why a silent forced on a `SkillTestResult` window looped: the test
re-raises its result window while resolving, the bucket never matched, and the
message loop ran until `RunMessagesTimeout` (30s) turned the request into a
**500**. All three Forced shapes now share one table.

So the two symptoms of an always-true Forced differ by where it is:

* in a *play* window -- the card is simply unplayable, 200 with nothing moving;
* in a window that is re-raised during resolution -- the request hangs for 30
  seconds and comes back 500 with `RunMessagesTimeout` in the log.
