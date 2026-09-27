---
title: Agenda side A constant/forced abilities remain in effect during side B resolution
date_added: 2026-05-18
source: Designer ruling (FFG Game Support email reply, May 2026) — generalises the Grimoire's act-card note to agenda cards
affects:
  - Restricted Access
  - Shadows Deepen
  - rule: agenda flip / advancement
  - rule: act flip / advancement
---

# Agenda side A constant/forced abilities remain in effect during side B resolution

The Grimoire (1.0, 2026) explicitly notes for **act cards**:

> Any constant abilities and/or forced abilities on an act card's "a" side are in effect while resolving the text on its "b" side.

The equivalent clarification is **omitted** for agenda cards in the Grimoire. A player wrote in asking whether the omission was intentional. The designer reply:

> This is true for the agenda deck as well; any constant abilities and/or forced abilities on the "a" side remain in effect while resolving the "b" side. We will update the Grimoire to clarify this.

So the rule is symmetric between acts and agendas:

- While resolving the **b** side of an act or agenda, any **constant** and **forced** abilities printed on the **a** side are still active.
- This means: forced abilities on the a-side can be triggered by events that happen during the b-side's resolution; constant abilities on the a-side still grant their modifiers during the b-side's resolution.

This matters in any scenario whose b-side resolution causes a game event that would trigger a forced ability printed on the a-side. The canonical case is *The Miskatonic Museum* (Dunwich Legacy):

- **Agenda 1a Restricted Access** has a forced ability: "When the Hunting Horror enters play: Place 1 doom on it."
- **Agenda 1b** says to find the Hunting Horror from the encounter deck / discard / void and put it into play if it isn't already.
- Under this ruling, the side-1a forced ability triggers when the Hunting Horror is put into play by the b-side's resolution.

The same logic applies to **Agenda 2a Shadows Deepen** and its b-side spawn.

## Affected cards / systems

- **Restricted Access** (`02161`) — `backend/arkham-api/library/Arkham/Agenda/Cards/RestrictedAccess.hs`
- **Shadows Deepen** (`02162`) — `backend/arkham-api/library/Arkham/Agenda/Cards/ShadowsDeepen.hs`
- Engine: agenda flip flow (`backend/arkham-api/library/Arkham/Agenda/Runner.hs`) — the engine flips the agenda's side to `B` *before* the per-card `AdvanceAgenda` handler runs, so any side-A ability gated by `AgendaWithSide A` is silently switched off during the b-side resolution. We honour the ruling per-card by relaxing those gates rather than reordering the flip.
- Engine: act flip flow (`backend/arkham-api/library/Arkham/Act/Runner.hs`) — same architectural pattern as agendas; no act card currently relies on `ActWithSide A` for ability gating, so no act changes were needed.

## Related agendas (not changed)

- **The Red Depths** (`04345`) and **Fury That Shakes the Earth** (`04209`) use `AgendaWithSide A` inside a `PlacedCounterOnAgenda` *window matcher*. That's checking the agenda's side at the moment doom is placed on it (mythos), not gating the ability by current side. It is unaffected by this ruling.

## Implementation status

- **Restricted Access (02161)**: ✏️ updated. Removed the `oneOf [thisExists a (AgendaWithSide A), IsReturnTo]` restriction from the side-A forced ability so it still triggers when the b-side resolution spawns the Hunting Horror. (`backend/arkham-api/library/Arkham/Agenda/Cards/RestrictedAccess.hs`)
- **Shadows Deepen (02162)**: ✏️ updated. Same change as Restricted Access. (`backend/arkham-api/library/Arkham/Agenda/Cards/ShadowsDeepen.hs`)
- **Engine (agenda flip)**: ⚠️ not changed. The agenda's side is flipped before the per-card b-side handler runs. This makes side-A `AgendaWithSide A` ability gates inactive during the b-side resolution. The two known cards relying on this are fixed individually; if a future agenda re-introduces a side-A `AgendaWithSide A`-gated constant/forced ability, it should follow the same pattern.
- **Engine (act flip)**: ✅ no change needed — no act card currently uses `ActWithSide` to gate its abilities.

## Regression tests

- `backend/arkham-api/tests/Arkham/Agenda/Cards/RestrictedAccessSpec.hs` — asserts the forced ability fires on both side A and side B of Restricted Access.
- `backend/arkham-api/tests/Arkham/Agenda/Cards/ShadowsDeepenSpec.hs` — same coverage for Shadows Deepen.

Before the fix, the "side B" cases would fail because the ability was gated by `AgendaWithSide A`. Both files use `lookupAgenda` to construct the real agenda (rather than the `WhatsGoingOn`-backed `testAgenda` helper) so the card's actual `HasAbilities` instance is exercised.
