---
name: project_max_per_round_is_per_card_code_all_players
description: "\"Max per round\" is keyed per CARD CODE and applies to ALL players — not per investigator; do not re-scope a card-code semaphore or limit to the controller"
metadata:
  type: project
---

A "max once/N per round" limit on an Arkham card is tracked **per card code, across every
player**. It is not a per-investigator allowance: if one investigator has used the card's
once-per-round effect, no other investigator may use it that round either.

**Why:** stated by the user (2026-11-16) while fixing Quick Thinking (#6005), correcting an
assumption that a plain "max" without "group" would be per player.

**How to apply:** a round-duration `Semaphore` on `CardCodeTarget "<code>"`, or
`cdLimits = [MaxPerRound n]`, is the right shape — leave it global. Don't add
investigator scoping (e.g. reading `modifierCard`'s owner via `getFullModifiers`) to such a
limit. Note where the limit attaches, though: on the *card* it is a play limit
(`cdLimits`, e.g. Ace in the Hole 02266), while "at the end of its ability, this card
gains: (max once per round)" limits the **ability resolving** — that is the semaphore case.
See [[project_semaphore_set_must_outrank_sibling_priority]].
