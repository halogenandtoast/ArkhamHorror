# Swarm cards are enemy entities, not `AsSwarm` placements of the original card

A swarm card is rules-wise "a copy of its host enemy," and the engine models it that
way: `PlacedSwarmCard host card` is handled by the **host enemy** (`Arkham/Enemy/Runner.hs`),
which calls `createEnemyWithPlacement` on **its own card** re-stamped with the swarm
card's `CardId`, placed `AsSwarm host card`. So every swarm card in play is an *Enemy*
entity; `SwarmOf`, `IsSwarm`, engagement, movement and defeat all key off enemies with
an `AsSwarm` placement.

Setting `AsSwarm` on a non-enemy entity — e.g. `PlaceTreachery tid (AsSwarm eid card)` —
compiles and stores the placement, but creates no swarm enemy. Nothing matches it, and
the card just sits in play with a placement that only `Helpers.Location`/`Helpers.Placement`
know how to read. That was the bug in Dark Matter's **Duplication** ("If it is an enemy,
place this card under it as a swarm card"): it never became a swarm.

**How to place an arbitrary card as a swarm card:** remove whatever entity currently
represents it, then hand the card to the host.

```haskell
-- asset:     Scenarios/WarOfTheOuterGods/Helpers.hs placeAssetAsSwarm
push $ RemoveAsset aid
push $ PlacedSwarmCard insect card

-- treachery: Homebrew/DarkMatter/Treacheries/Duplication.hs
push $ RemoveTreachery attrs.id
push $ PlacedSwarmCard eid (toCard attrs)
```

Two things make the treachery case work:

- The host enemy must already be in play, so a revelation that draws the enemy has to
  defer the placement until after the spawn resolves (Duplication round-trips through a
  `CampaignSpecific` message carrying the treachery id + drawn card id).
- `RemoveTreachery` in `Game/Runner.hs` `popMessageMatching_`s the pending
  `After (Revelation _ source)` for that treachery. Without that pop, the treachery's
  `After Revelation` handler sees `placement == Limbo` and discards the card out from
  under the swarm. Do **not** replace `RemoveTreachery` with a discard here.

See also [[project_enemy_movement_placement]], [[project_remove_from_game_skips_leaveplay]].
