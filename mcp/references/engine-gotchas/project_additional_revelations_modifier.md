# "Resolve its revelation an additional time" = the `AdditionalRevelations` modifier

`AdditionalRevelations Int` (`Arkham/Modifier.hs`) makes a card's revelation resolve
N extra times. Put it on the **card** (`CardIdTarget`) before the card is drawn/played:

```haskell
temporaryModifier card attrs (AdditionalRevelations 1) $ drawCard iid card
```

Both revelation sites in `Game/Runner.hs` — `ResolveTreachery` (encounter/drawn
treacheries) and the `PlayerTreacheryType` branch of the play-card path — read it
through the same helper:

```haskell
resolveRevelations :: [ModifierType] -> Message -> [Message]
resolveRevelations modifiers' revelation =
  replicate (1 + max 0 (sum [n | AdditionalRevelations n <- modifiers'])) revelation
```

**Only the bare `revelation` message repeats.** The surrounding `When revelation` and
`MoveWithSkillTest (Run [After revelation, AfterRevelation ...])` stay single, which is
the whole point: `After (Revelation ...)` is what discards the treachery (placement
`Limbo`) or claims its victory points, and `ResolvedCard` is what re-fires `Surge`. Emit
the After-block twice and the card gets discarded twice, resolved twice, and surges
twice. Never "resolve again" by re-pushing the whole `[When, rev, After]` triple.

**Repeating the revelation is safe with revelation skill tests.** Each `revelation`
pushes its skill test at the front of the queue, so test #1 fully resolves before
revelation #2 runs. `handleSkillTestNesting` (`Helpers/Message.hs`) unwraps the pending
`MoveWithSkillTest` on the first test; the resulting bare `Run [After revelation, …]`
still sits behind revelation #2, so the After-block lands after *both* tests.

**Don't do it by hand.** The obvious-looking alternative — deferring past the draw, then
`obtainCard` + `CreateTreacheryAt … Limbo` + a second `ResolveTreachery` — needs
`obtainCard` (or the card lands in the encounter discard twice) *and* a
`temporaryModifier card … NoSurge` wrapper (or `ResolvedCard` surges twice), and it
still runs the whole `#when`/`#after` pair a second time. The modifier exists so no card
has to reproduce that.

First user: Dark Matter's Duplication ("If it is a treachery, resolve its revelation
effect an additional time"). See also [[project_swarm_cards_are_enemy_entities]] for the
other half of that card.
