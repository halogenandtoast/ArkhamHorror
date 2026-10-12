---
title: project_forced_ability_only_for_printed_forced
description: "ForcedAbility is only for a card that prints 'Forced'; anything else silently tells the player something untrue about their card"
---

`ForcedAbility` is not "an ability that happens on its own". The word **Forced**
is printed on the card, the UI shows it, and a player reads it as part of the
card's text. Using `ForcedAbility` for an effect whose card does not say
"Forced" puts a word on their card that is not there.

Use, in order of preference:

1. **No ability at all.** If the effect is a consequence of a message
   ("when you commit this card to a skill test, ..."), handle the message.
   For a custom card that is a `_handlers` entry; for a Haskell card it is a
   `runMessage` case. Nothing is offered, nothing is labelled, nothing shows up
   in the ability list.
2. **`SilentForcedAbility`** (`silent`) when you do need a window-triggered
   ability but the card prints no "Forced" — it behaves as forced without
   claiming to be.
3. **`ForcedAbility`** (`forced`) only when the card prints __Forced__.

Applies equally to `ForcedAbilityWithCost` and to `_abilities` written in the
custom card builder, where the type is chosen as JSON and nothing checks it.

## The converse, which is just as strict

The ladder above only applies to an effect the card states as a plain fact of
play. It is **not** a menu for an ability the card does print. A printed
**Forced** — or `[action]`, `[fast]`, `[reaction]`, an Objective — must be a
`HasAbilities` entry with that ability type and its window. Dropping to step 1
and handling the message instead is wrong even when the message is a cleaner
discriminator than any window matcher: the player loses the ability from the
card's ability list and from the log, and the next reader cannot tell the
implementation from an effect the card never printed.

When the window really cannot express the trigger, narrow the ability rather
than relocating it: `restricted` criteria, `onlyOnce`, a meta flag, or omitting
it from the list while it must not fire. Norman Falkner (Against the Wendigo) is
the worked example — "Forced - If Norman Falkner is defeated or discarded"
cannot be told apart from his own parley putting him out of play, because
`AssetDefeated`, `AssetDiscarded` and `AssetLeavesPlay` all resolve to the one
`Window.LeavePlay`. The answer is a meta flag set by the parley that drops the
ability from the list, not a `Discard` case in `RunMessage`.

Found when Royal Joker (a custom signature whose text is a plain "When you
commit this card to a skill test, you may ...") was written as a
`ForcedAbility`, which would have displayed a Forced prompt the card never had.
See also [[project_activate_dual_action_type]] for the other place an ability's
declared type changes what the rules say about it.
