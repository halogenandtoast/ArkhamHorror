---
name: project_heal_wording_source_matcher
description: Card wording picks the source matcher — "after YOU heal/deal" = SourceUsedBy, "one of your card effects" = SourceOwnedBy
metadata:
  type: project
---

Reaction windows scoped to an investigator's own effects have two matchers, and the **printed
card wording** decides which one is correct. Both live in `Arkham/Helpers/Source.hs`
(`checkSourceOwner creditUser`):

- **`SourceOwnedBy You`** (`creditUser = False`) — strict card ownership. An `AbilitySource`
  resolves down to the underlying card's controller/owner, so a location / encounter / other
  scenario card resolves to **nobody**.
- **`SourceUsedBy You`** (`creditUser = True`) — additionally credits the investigator who
  *used* the ability (`UseAbilitySource iid …`, else `getActiveInvestigatorId`).
  The `getActiveInvestigatorId` half is a **guess** and goes wrong whenever the active
  investigator has been switched away from the performer — Carson Sinclair's granted action,
  or a non-active player answering a prompt mid-skill-test (the API wraps those answers in
  `SetActivePlayer`, which also writes `activeInvestigatorIdL`). Since #5530 an investigator's
  attack damage carries `UseAbilitySource <attacker>` for **every** ability index, not just
  the basic-attack `100` (`Investigator/Runner/Damage.hs`, `handleInvestigatorDamageEnemy`), so
  the guess is no longer consulted for "when you deal damage to an enemy". Damage/heal/token
  effects that still hand `checkSourceOwner` a bare `AbilitySource` remain on the guess.

Mapping (verified against every call site, issue #5250):

| Printed wording | Matcher |
|---|---|
| "After **you** heal / deal / defeat …" | `SourceUsedBy You` |
| "After **one of your card effects** …" | `SourceOwnedBy You` |
| "After **a card you own** …" | `SourceOwnedBy You` |

FAQ v2.5 Q033 is the authority for the second/third rows: *"your cards" are the cards you
currently control*. So Carolyn Fern `05001`, Vincent Lee, Hypnotic Therapy, Surgical Kit (3) and
Diana Stanley correctly stay on `SourceOwnedBy`, while Carolyn Fern (2) `60251`, Do No Harm
`11758a` and Do No Harm — Reliable Support `11758b` were wrong and moved to `SourceUsedBy`
(#5250). Private Practice `60257` and Experimental Psychology were already right.

The failure is silent: the heal/damage lands normally and the reaction window just never opens.
Chapter 2 rewrote a lot of Chapter 1 text into the "after you …" form, so re-check the wording
against `.claude/data/cards.json` (`real_text`) rather than assuming the Chapter 1 implementation
carried over — see `project_scarletkey_source_ownership.md` for the same helper's other gap.
