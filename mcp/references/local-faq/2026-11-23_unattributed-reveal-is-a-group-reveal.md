---
title: A reveal with no investigator named is a GROUP reveal, not just for acts and agendas
date_added: 2026-11-23
source: internal decision (user instruction) — generalising FAQ v2.5 Q&A 088; raised by GitHub issue #6048
affects:
  - No Place Like Home
  - Mouse Mask
  - Whitton Greene
  - Whitton Greene (2)
  - Obscure (2)
  - Jake Williams
  - Core of the Vault
  - RevealLocation / PutLocationIntoPlay windows
  - RevealLocationByGroup / PutLocationIntoPlayByGroup
---

# A reveal with no investigator named is a GROUP reveal, not just for acts and agendas

When **any** scenario card instructs a location to be revealed, or put into
play, without naming an investigator, **each investigator is considered to have
revealed it / put it into play**. Every seat's "after you reveal a location or
put a new location into play" card is therefore eligible.

FAQ v2.5 Q&A 088 is the parent ruling:

> Q: When an act or agenda instructs the players to reveal or put a new location
> into play, who is considered to have revealed it or put it into play for the
> purposes of card effects such as Mouse Mask (43) or Whitton Greene (13) that
> trigger "after you put a location into play?"
>
> A: Because acts and agendas are advanced by the players, as a group, each
> investigator is considered to have put the location(s) into play.

The clause "because acts and agendas are advanced by the players, as a group"
explains **why**; it does not scope the ruling to those two card types. 088
names acts and agendas because that is what the asker hit. A location's own
**Forced** ability reveals a location the same way, and resolves the same way:
the table resolves a scenario card's instruction as a group.

Note which way the designers leaned. Where the rules do address ambiguous
agency elsewhere, they name the lead investigator — the Grimoire's codex-entry
rule ("if it is unclear who triggered the codex entry, the lead investigator
resolves the entry") and FAQ (1.5) *Choices, and the Grim Rule* ("the lead
investigator decides between those options"). For reveal attribution 088 had
that convention available and deliberately chose "each investigator is
considered" instead. So the correct reading is **group** — not the lead, and
not "nobody."

Two engine-shaped consequences:

- There is no third "the scenario revealed it and nobody did" state. An attempt
  to add one (a `Revealer` sum type plus `RevealLocationByScenario` /
  `PutLocationIntoPlayByScenario` windows, so that `You` matched nobody) was
  written and then reverted as wrong. Do not reintroduce it.
- Falling back to the **lead** for an unattributed reveal is also wrong, and is
  what `RevealLocationByGroup` was introduced to fix: it fired "after you reveal
  a location" for the lead alone. Resolving `Who` against the investigator being
  asked (`matchWho iid iid`) is what makes `You` hold for each of them in turn.

## The case that raised it (#6048)

**The Grand Vault**. Core of the Vault (11595), unrevealed side:

> **Forced** – At the end of the round, if each [[Vault]] location has been
> activated: Reveal Core of the Vault.

A reporter expected No Place Like Home (11753a) *not* to remove a discovery for
this, since the card reads "When **you** reveal a location or put a location
into play." It should remove one. The reveal is a scenario-card instruction with
no investigator named, so all investigators are considered the revealer —
**working as intended**.

Two further points specific to this card, both supporting the group reading:

- Its trigger condition, "if each [[Vault]] location has been activated," is the
  sum of the players' own actions over the round. If anything in the scenario is
  "advanced by the players, as a group," this reveal is.
- Core of the Vault deliberately carries `[Rlyeh, Sanctum, Glyph]` and **not**
  the `Vault` trait, which is what lets its own criteria
  (`not_ $ exists $ LocationWithTrait Vault <> LocationWithResources (atMost 0)`)
  ever be satisfied.

The same scenario also reveals Sealed Chamber (11604) from the act *Careful
Navigation*, which is squarely 088's own case.

## Affected cards / systems

- No Place Like Home (11753a) — `backend/arkham-api/library/Arkham/Asset/Assets/NoPlaceLikeHome.hs`
- Mouse Mask (10043) — `backend/arkham-api/library/Arkham/Asset/Assets/MouseMaskTheMeekWatcher.hs`
- Whitton Greene (60213) — `backend/arkham-api/library/Arkham/Asset/Assets/WhittonGreene.hs`
- Whitton Greene (2) (60223) — `backend/arkham-api/library/Arkham/Asset/Assets/WhittonGreene2.hs`
- Obscure (2) — `backend/arkham-api/library/Arkham/Asset/Assets/Obscure2.hs`
- Jake Williams (04008) — `backend/arkham-api/library/Arkham/Asset/Assets/JakeWilliams.hs`
- Core of the Vault (11595) — `backend/arkham-api/library/Arkham/Location/Cards/TheDrownedCity/TheGrandVault/CoreOfTheVaultHeartOfTheMachine.hs`
- Window construction — `backend/arkham-api/library/Arkham/Location/Runner.hs`
- Window matching — `backend/arkham-api/library/Arkham/Helpers/Window.hs`

A location's own "when this is revealed" ability is keyed on `Anyone`, not `You`
(~40 cards: Pallid Mask, Devil Reef, Heart of Madness, Horror in High Gear …).
Those are unaffected either way, since `Anyone` matches regardless of who the
revealer is.

## Implementation status

- **All six `You`-keyed cards**: ✅ already match. Each uses
  `oneOf [RevealLocation #after You Anywhere, PutLocationIntoPlay #after You Anywhere]`,
  verified unchanged.
- **Core of the Vault (11595)**: ✅ already matches — pushes
  `Msg.RevealLocation Nothing attrs.id`, which becomes the group window. No
  change; the implementation is correct as printed.
- **`Arkham/Location/Runner.hs`**: ✅ already matches — maps an absent
  investigator to `Window.RevealLocationByGroup` /
  `Window.UnrevealedRevealLocationByGroup` / `Window.PutLocationIntoPlayByGroup`.
- **`Arkham/Helpers/Window.hs`**: ✅ already matches — resolves those windows'
  `Who` with `matchWho iid iid whoMatcher`, so `You` holds for each investigator
  asked rather than for the lead alone.
- **No files changed.** This entry records the ruling; the engine was already
  correct. The commit that introduced a third revealer state was reverted.
