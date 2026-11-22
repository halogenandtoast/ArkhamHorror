---
title: Idol of Bokrug is "a dreaming card you control" for MOVING dreams only
date_added: 2026-11-22
source: internal decision (project owner ruling)
affects:
  - dreaming
  - Idol of Bokrug
  - Sigil of Thalarion
  - Dream Mastery
  - Veil of Thalarion
  - Grimoire of Ages
  - Lunar Charts
  - Staff of Atal
---

# Idol of Bokrug is "a dreaming card you control" for MOVING dreams only

Idol of Bokrug (14028) prints *"Dreams may be moved to and from Idol of Bokrug as if it
were a dreaming card under your control."* That as-if is **scoped to dream movement and
nothing else**, and inside that scope it is **complete**.

The split, explicitly:

**For MOVING a dream, the Idol is fully a dreaming card you control.** It is a legal
source and a legal destination, for every card that moves dreams — not just its own
abilities. Grimoire of Ages (14016) and Lunar Charts (14017) may move a dream to or from
it; Staff of Atal (14070) may take its dream from it. "To and from" is bidirectional. Its
printed *"(Limit 5 dreams on Idol of Bokrug.)"* caps it as a destination.

**For everything that is not moving a dream, it is not a dreaming card at all.**

- it does **not** count against the limit of 3 dreaming cards you may control;
- it does **not** satisfy Sigil of Thalarion's *"if you control a dreaming card with 5 or
  more dreams"*, however many dreams sit on it;
- it is **not** a legal target for Dream Mastery or for Veil of Thalarion's cost
  reduction.

Sigil of Thalarion's trigger stays reachable without the Idol: Grimoire of Ages and Lunar
Charts stack dreams onto a single dreaming card, and Enchanting Fife (14043) adds them. Do
not widen this ruling to "rescue" Sigil.

**Placing a dream and removing a dream are not moving one**, so they do not reach the Idol
either: Knight of Celephaïs's *removal*, and the "place 1 dream on a dreaming card you
control" of Lunar Sight (14054), Reality Fracture (14055) and Enchanting Fife's
`PlaceDreamCost`, all stay on the dream zone.

## Affected cards / systems

- Idol of Bokrug (14028) — `backend/arkham-api/library/Arkham/Asset/Assets/IdolOfBokrug5.hs`
- Sigil of Thalarion (14052) — `backend/arkham-api/library/Arkham/Asset/Assets/SigilOfThalarion2.hs`
- Dream Mastery — `backend/arkham-api/library/Arkham/Skill/Cards/DreamMastery.hs`
- Veil of Thalarion (14048) — `backend/arkham-api/library/Arkham/Event/Events/VeilOfThalarion.hs`
- Grimoire of Ages (14016) — `backend/arkham-api/library/Arkham/Asset/Assets/GrimoireOfAges.hs`
- Lunar Charts (14017) — `backend/arkham-api/library/Arkham/Asset/Assets/LunarCharts.hs`
- Staff of Atal (14070) — `backend/arkham-api/library/Arkham/Asset/Assets/StaffOfAtal3.hs`
- the endpoint declaration — `IsDreamEndpoint (Maybe Int)` in
  `backend/arkham-api/library/Arkham/Modifier.hs`
- the movement layer — `DreamPlace` / `getDreamPlaces` / `getDreamMoves` /
  `canMoveADream` in `backend/arkham-api/library/Arkham/Helpers/Dreaming.hs`, asked for
  through `Criteria.CanMoveADream`
- the dream-zone matchers — `DreamingCardControlledBy` / `DreamingCardWithDreams` in
  `backend/arkham-api/library/Arkham/Matcher/DreamingCard.hs`

## Implementation status

- **the non-movement half, structurally**: ✅ the Idol is an in-play asset holding `Dream`
  tokens, never a dream-zone record. `DreamingCardControlledBy` ranges only over
  `InvestigatorDreamingCards`, so every "a dreaming card you control" reader — the cap in
  `canCreateDreamingCard`, Sigil's criterion, Dream Mastery, Veil of Thalarion — cannot see
  the Idol at all. Nothing is excluded by hand, so there is no per-site exception to keep
  in step.
- **the movement half**: ✏️ one layer, read everywhere movement happens. `DreamPlace` is
  either a dream-zone record or an `IsDreamEndpoint` asset the investigator controls;
  `getDreamPlaces` collects both, `getDreamMoves` pairs every place holding a dream with
  the other places that have room, and `dreamPlaceHasRoom` enforces the endpoint's own
  capacity **where the destination is offered**, so an over-capacity move is never
  presented (#6017). No card reads `IsDreamEndpoint` itself.
- **Idol of Bokrug (14028)**: ✅ declares `IsDreamEndpoint (Just 5)` on itself for the
  printed limit, and its own Forced ability is gated on the Idol holding fewer than 5
  dreams. That Forced ability keeps its printed source clause, *"from a dreaming card you
  control"*, rather than ranging over dream places — the only other place the Idol could
  take a dream from is itself, which is a no-op.
- **Grimoire of Ages (14016) / Lunar Charts (14017)**: ✏️ both move dreams through
  `getDreamMoves`, so the Idol is now offered as a source and as a destination. Grimoire's
  criterion is now `Criteria.CanMoveADream You`; the old
  `DreamingCardCount (atLeast 2) <> exists (... atLeast 1)` was wrong once
  Idol-plus-one-dreaming-card is a legal move, because that is a dream-zone count of 1.
- **Staff of Atal (14070)**: ✏️ its *"move 1 dream from a dreaming card you control to your
  resource pool"* ranges over `getDreamPlaces`, so a dream on the Idol can be cashed as a
  resource.
- **Sigil of Thalarion (14052)**: ✅ its criterion is
  `DreamingCardControlledBy You <> DreamingCardWithDreams (atLeast 5)`, which the Idol
  cannot satisfy — 5 dreams on the Idol does not arm it.
- **Dream Mastery / Veil of Thalarion**: ✅ already matched — both read the dream zone only.
