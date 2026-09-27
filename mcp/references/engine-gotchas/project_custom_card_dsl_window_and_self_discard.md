---
name: project_custom_card_dsl_window_and_self_discard
description: "Custom-card abilities bind their triggering window as $window/$w0…; CannotLeavePlay no longer blocks an asset's self-sourced Discard"
metadata:
  type: project
---

Two seams added 2026-09-06 while making Bizarro Agnes's set buildable in the card builder.

**1. A custom ability can read the window it triggered on.** `UseThisAbility` is a pattern
synonym over `UseCardAbility iid source n ws _`, so it *drops the windows* — which is why a
data-driven "heal that many horror" had nothing to read. `Arkham.Custom.Ability`'s
`ZonedUseThisAbility` now yields `[Window]` as a fourth field, `runCustomAbility` takes it, and
`triggeringWindow` binds `$window` plus `$w0`, `$w1`, … (the window type's positional fields, the
way a handler binds a message's `$0`, `$1`).

The window bound is the one **the ability's own matcher accepts** — `defaultAbilityWindow` on the
decoded `AbilityType`, then `findM (windowMatches iid source w matcher) ws`. Several windows are
open whenever an ability is offered, so "the first one" would be arbitrary; this asks the same
question the engine asked to offer the ability at all. An action or fast ability matches nothing
payload-bearing and binds nothing.

Payload shapes worth knowing: `TakeHorror Source Target Int` (amount at `$w2`),
`Healed DamageType Target Source Int` (amount at `$w3`).

**2. `CannotLeavePlay` does not stop an asset discarding itself.** `Asset/Runner.hs`'s `Discard`
branch used to check the modifier unconditionally, so a card reading "it cannot leave play except
using the ability below" was stuck forever — its own ability's `Discard` was swallowed too. The
check is now skipped when `isSource a source` (which sees through `AbilitySource`). No printed
asset carries `CannotLeavePlay` today (only locations and two Drowned City treacheries), so this
only ever affects a card that gives itself the modifier and its own way out.

**How to apply:** reach for `$wN` instead of a `_handlers` entry whenever a custom card says "that
many" — a handler fires on the raw message and cannot carry an ability limit or a log line.
Related: [[project_card_options_system]], [[project_window_condition_tick_vs_open_tick]].
