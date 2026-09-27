# `CanMoveWith` followers are gated on connectivity in the engine, not by the card

The `CanMoveWith InvestigatorMatcher` modifier (only granted by **Safeguard (2)**, `06196`)
is consumed in exactly one place: `Arkham.Investigator.Runner.Movement`, in the
`ToLocation lid` branch of `handleDoResolveMovement`. That site selects every
investigator who both carries the modifier and is `colocatedWith` the mover, and pushes
`ForInvestigator iid' (ForTarget (LocationTarget lid) (MoveTo movement))`.

The handler for that message (`Arkham.Investigator.Runner`) gates on
`getCanMoveTo iid' (moveSource movement) lid` — which checks `CannotMove`, barricades,
`canEnterLocation`, and additional enter/leave costs, but **not** whether `lid` connects
to where the follower is standing. `getCanMoveToLocations` starts from
`Matcher.canEnterLocation iid` over the whole board, so any revealed location passes.

Result before #5407: an investigator moved by a teleport-style effect (Pendant of the
Queen, "move to any revealed location") dragged their Safeguard (2) partner along, even
though the card reads "as they move from your location **to a connecting location**".

The gate lives at the producer in `Movement.hs`:

```haskell
for_ moveWith \iid' -> do
  connected <- lid <=~> AccessibleFrom ForMovement (locationWithInvestigator iid')
  when connected $ push $ ForInvestigator iid' $ ForTarget (LocationTarget lid) (MoveTo movement)
```

Two things make that placement correct:

- The mover's placement is only rewritten later, by the queued
  `Do (WhenWillEnterLocation iid lid)`. At the selection site `iid` is still at the
  origin, so `colocatedWith iid` and the follower's own location both mean "origin".
- `AccessibleFrom ForMovement` is the same predicate Safeguard (0) uses in its
  `CanMoveToLocation` trigger criterion, so both versions of the card agree on what
  "connecting" means (it also filters `Unblocked` and honours `ConnectedToWhen` /
  `ForMovementConnectedToWhen` modifiers).

If a future card ever grants "move with" **without** the connecting restriction, it needs
a new modifier — the connectivity check is baked into the engine, not into Safeguard (2).

See [[project_cannotenter_enemy_side_ban]] and
[[project_move_restriction_must_be_cannotenter]] for the other movement-restriction seams.

Regression coverage: `tests/Arkham/Asset/Assets/Safeguard2Spec.hs`.
