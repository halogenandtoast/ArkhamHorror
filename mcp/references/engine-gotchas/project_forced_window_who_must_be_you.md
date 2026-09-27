---
name: project_forced_window_who_must_be_you
description: A Forced window whose effect says "that investigator" must scope Who with You — UseThisAbility's iid is whoever's CheckWindows ran, not the window's subject
metadata:
  type: project
---

`Do (CheckWindows ws)` is dispatched to **every** investigator entity
(`Arkham/Investigator/Runner.hs:2395` → `runWindow`), and each one pushes
`UseAbility iid … windows` with *itself* as `iid`. `matchWho iid who whoMatcher`
(`Arkham/Helpers/Window.hs`) only ties the window's subject to the checking
investigator when the `Who` matcher contains `You`.

So a Forced ability written as `EnemyAttacked #after (at_ $ locationWithEnemy attrs) …`
is offered to **every** investigator at the location — and `UseThisAbility`'s `iid` is
whoever's prompt is answered first, not the attacker. Gang Enforcer (11513) attacked
Agatha after Marion fought a Criminal enemy (#5685); Rookie Cop (71020) had the same
shape with no group limit, so it damaged *everyone* there. Fixed by
`You <> at_ (locationWithEnemy attrs)`.

**Why:** the Who matcher looks like it's describing the card text ("an investigator at
this location"), but it's really answering "does this window belong to the investigator
being polled?" — a purely location-scoped matcher answers that for everyone.

**How to apply:** if the effect says "**that investigator**" and the handler uses
`UseThisAbility`'s `iid`, the window's `Who` must include `You` (the location matcher
stays, as the card's extra restriction). Reference idiom: `MiasmaticShadow.hs`,
`OtherworldlyMimic.hs`, `ReturnToXochimilco.hs`. Alternatively read the subject out of
the window (`attackingInvestigator`/`evadingInvestigator` in
`Arkham/Helpers/Window/Enemy.hs`), but that still leaves the duplicate prompt.

This is the mirror image of [[project_yourlocation_window_handler_fanout]]: there the
window was `You`-scoped and the *handler* over-fanned; here the window itself is
under-scoped. Related: [[project_scarlet_key_shift_needs_youexist]],
[[project_playability_uses_active_investigator]].
