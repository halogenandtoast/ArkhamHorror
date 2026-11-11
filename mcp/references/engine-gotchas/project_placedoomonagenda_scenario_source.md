---
title: project_placedoomonagenda_scenario_source
description: "PlaceDoomOnAgenda carries its own Source; the mythos step passes GameSource and every card must pass its own, or SourceIsCardEffect doom redirects break"
---

`PlaceDoomOnAgenda Source Int CanAdvance` carries the placing source, and the Scenario
runner hands it straight to `PlaceTokens source (AgendaTarget x) Doom n`. The agenda's
`PlaceDoom source (isTarget a) n` handler then raises a `WouldPlaceDoom` window with that
source, so `WouldPlaceDoomCounter ... SourceIsCardEffect` abilities — The Onslaught
redirecting agenda doom onto The Captives — can tell a card effect apart from the mythos
phase's own step 1.2 placement.

Both helpers in `Arkham.Message.Lifted` therefore **require** the source:

```haskell
placeDoomOnAgenda :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeDoomOnAgendaAndCheckAdvance :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
```

`Arkham.Helpers.Message.placeDoomOnAgenda :: Sourceable source => source -> Message` is the
non-monadic form for `pushAll` lists.

**How to apply:** pass the card's own attrs (or `attrs.ability N`) from any
enemy/treachery/location/act/agenda/story/asset/event effect. Only two callers are not
card effects and must stay that way:

- the mythos phase step 1.2 in `Arkham/Game/Runner.hs` passes **`GameSource`**
- scenario scripts (`Scenario/Scenarios/**` setup and resolution bodies) pass the scenario
  `attrs`, i.e. `ScenarioSource`

Both map to `False` in `SourceIsCardEffect`, so neither is redirected. The `Doomed` keyword
in `Enemy/Runner.hs` *is* printed card text and passes the defeated enemy's source.

## History (#5982)

The source parameter did not exist originally: `PlaceDoomOnAgenda Int CanAdvance` was
turned into `PlaceTokens (toSource scenario) ...`, so every card looked like the scenario.
A first pass added opt-in `placeDoomOnAgendaBy` helpers and converted the three *player*
cards (Fickle Fortune (3), Dark Memory, Dark Memory Advanced), on the reasoning that no
encounter card placing agenda doom appears in The Longest Night's sets. That reasoning was
wrong — Silver Twilight Acolyte (Night of the Zealot) turned up in a Hemlock Vale game and
put its doom on The Onslaught instead of The Captives. The `*By` helpers are gone: they
also bypassed the message and selected `AnyAgenda`, so they placed doom on all three
agendas in War of the Outer Gods and skipped Black Stars Rise's "choose an agenda" prompt.

Making the source a required argument is the point — an unconverted call site is now a
type error rather than a silent scenario-sourced placement.

## The inverse trap: `NotSource SourceIsPlayerCard` is NOT "a scenario card" (#5896)

The mythos-phase doom carries a non-card source, so a window scoped by the double negative
`NotSource SourceIsPlayerCard` **accepts the normal rules doom from mythos step 1.2**.
Future Evils (`c:dark-matter:013`) fired every mythos phase because of exactly this.

Use the positive matcher **`ScenarioCardSource`** (`Arkham/Matcher/Source.hs`, evaluated in
`Arkham/Helpers/Source.hs`) for "after a scenario card places X": it admits act, agenda,
enemy, location, treachery, story and chaos-token sources (unwrapping non-player
`AbilitySource`/`ProxySource`) and returns `False` for both `ScenarioSource` and
`GameSource`. `EncounterCardSource` is the stricter sibling when the card says "encounter
card" — it also admits uncontrolled encounter-card assets.

Keep any self-exclusion: an ability-sourced `placeDoom (attrs.ability 1) …` still reads as
a scenario card source (index 1 satisfies `notPlayerAbilityIndex`), so a card reacting to
doom and placing doom retriggers forever without `NotSource (SourceIsTreacheryEffect …)`.
