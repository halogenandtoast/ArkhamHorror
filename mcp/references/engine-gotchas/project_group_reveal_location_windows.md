---
name: project_group_reveal_location_windows
description: Act/agenda-driven location reveals and placements raise investigator-agnostic ByGroup windows so every investigator counts as the revealer, per the official ruling
metadata:
  type: project
---

**Ruling:** "When an act or agenda instructs the players to reveal or put a new
location into play, who is considered to have revealed it…? Because acts and agendas
are advanced by the players, as a group, **each investigator** is considered to have
put the location(s) into play."

`matchWho iid who You = iid == who`, so a window that bakes in one investigator can
only ever fire for that one. Two window types now exist for these:

- `Window.RevealLocation iid lid` — a specific investigator revealed it (moved in,
  Dr. Rosa Marquez, …). Raised when `Msg.RevealLocation` carries `Just iid`, i.e.
  from `revealBy` / `unsafeRevealBy`.
- `Window.RevealLocationByGroup lid` — no specific revealer (`Msg.RevealLocation
  Nothing`, from `reveal` / `unsafeReveal` / `revealMatching`, and acts/agendas).
- `Window.UnrevealedRevealLocation iid lid` / `…ByGroup lid` — the `#when` half of
  the same reveal, split the same way.
- `Window.PutLocationIntoPlay iid lid` — retained for save compatibility; nothing
  raises it any more. Gate Box and Detached from Reality kept pushing their own
  `checkAfter $ PutLocationIntoPlay iid dreamGate` on top of `placeLocationCard`, so a
  single Dream-Gate placement opened two placement windows and Mouse Mask replenished
  twice (#5617). **After `placeLocationCard` / `PlaceLocation`, never raise a placement
  window yourself** — `PlacedLocation` already does.
- `Window.PutLocationIntoPlayByGroup lid` — always raised now, because
  `Msg.PlacedLocation` carries no investigator at all.

The `Matcher.RevealLocation` / `Matcher.UnrevealedRevealLocation` / `Matcher.PutLocationIntoPlay` matchers accept both
shapes. For the `ByGroup` ones they resolve `Who` as `matchWho iid iid whoMatcher` —
against the investigator being asked — so `You` passes for each in turn and narrower
matchers (`Anyone`, `InvestigatorAt …`) still filter. `NotYou` would never pass on a
group window; no card uses it on these, so that is untested rather than decided.

**Before this**, `Location/Runner.hs` did `revealer <- maybe getLead pure miid` and
`selectOne ActiveInvestigator >>= traverse_ …`, so group reveals fired for the lead
alone, placements for the active investigator alone — and for nobody when that select
came back empty. Affected No Place Like Home (TDC Task), Whitton Greene, Whitton
Greene (2), Mouse Mask, Obscure (2), Jake Williams.

**If you add a window type here, grep for code that destructures the old one** — the
location is pulled straight out of the window in several places, and a missed case is
silent (a wrong location or a fallthrough `error`): `Helpers/Window/Clue.hs`
(`getRevealedLocation`), `Event/Events/VantagePoint.hs`, `Agenda/Cards/Awakening.hs`,
`Agenda/Cards/TheWaterRises.hs`, both Vale Lanterns.

`RevealLocationForcedAbilities` deliberately keeps a single revealer: its `mFromLid`
encodes "moved into this location and revealed it", which is about one investigator's
movement. **Setup reveals do fire.** Reveal windows are not in `isSetupSkippableWindow`, and
the `AmongSearchedCards` gate in `Helpers/Window.hs` deliberately keeps player-card
searches (its comment names Whitton Greene's reveal-location search) working during
setup. A setup reveal goes through `reveal` → `unsafeReveal` → `RevealLocation
Nothing`, so it raises the ByGroup windows like any other unattributed reveal.

Placements are the exception: `isSetupSkippableWindow` drops
`PutLocationIntoPlayByGroup` (and `LocationEntersPlay`, and clue placements) while
`gameInSetup`, on the stated grounds that no triggered ability resolves during setup.
So "after you put a location into play" does not fire for locations placed during
setup, even though "after you reveal" does.
