---
name: project_addtovictory_leaveplay_window_ordering
description: "Enemy AddToVictory must raise #when/#at LeavePlay BEFORE RemovedFromPlay and #after INSIDE Do (AddToVictory) — two opposing constraints, one from #5554, one from Corner the Suspect"
metadata: 
  node_type: memory
  type: project
  originSessionId: bbea5346-ab0b-4c6a-94ad-11d27f0fb6c5
  modified: 2026-08-29T23:05:46.225Z
---

The enemy victory-display path in `Enemy/Runner.hs` has **two opposing timing constraints**, and
`ccc527d34a` (2026-08-22, "River of Blood progress") satisfied only the second, breaking the first
until #5554.

1. **`#when`/`#at LeavePlay` must fire while the enemy is still in play.** `Matcher.EnemyLeavesPlay`
   resolves `#when` with a plain `select` (`Helpers/Window.hs:2233`); only the `#after` branch wraps
   it in `IncludeOutOfPlayEnemy`. `RemovedFromPlay` also discards every `EnemyAsset` first. So raising
   the window after removal makes `EnemyWithModifier`/`EnemyWithAsset`/`EnemyWithAttachedEvent`
   select nothing. Recover the Relic's Objective never fired and the Relic of Ages went to the
   discard (#5554); same latent break for Alejandro's Plight, the five Rot events, Hunting Horror,
   Bianca die Katz, the two Drowned City parasites. Impossible Pursuit had already hand-patched
   around it with `IncludeOutOfPlayEnemy`.
2. **`#after LeavePlay` must fire from inside `Do (AddToVictory …)`.** The scenario's
   `victoryDisplay` is appended by the **Scenario** runner's clause for the same message,
   `Scenario/Runner.hs:784` (`Do (AddToVictory _ (EnemyTarget eid))`). Corner the Suspect (River of
   Blood act 2a) reacts on `EnemyLeavesPlay #after "Julia Stern"` and its `AdvanceAct` branches on
   `VictoryDisplayCardMatch "Julia Stern"` — R1/R2 vs `ResetActDeckToStage 1`. Raised earlier it
   reads False and resets the act deck.

Correct shape (verified trace order: `AddToVictory` → `#when/#at LeavePlay` → `RemovedFromPlay` →
`Do (AddToVictory …)` → `#when/#at AddedToVictory` → `#after LeavePlay + #after AddedToVictory`):

```haskell
AddToVictory _miid (isTarget a -> True) -> do
  whenLeavePlay <- checkWindows $ (`Window.mkWindow` Window.LeavePlay (toTarget a)) <$> [#when, #at]
  pushAll [whenLeavePlay, RemovedFromPlay (toSource a), Do msg]
Do (AddToVictory miid (isTarget a -> True)) -> do
  pushAll
    [ CheckWindows [mkWhen $ Window.AddedToVictory miid card]
    , CheckWindows [Window.mkWindow #at $ Window.AddedToVictory miid card]
    , CheckWindows [mkAfter $ Window.LeavePlay (toTarget a), mkAfter $ Window.AddedToVictory miid card]
    ]
```

**Why:** removing to the victory display *is* leaving play, but it is also *arriving somewhere the
reaction can see* — the two halves need different anchors. Before `ccc527d34a` the path pushed
`RemoveFromPlay` **and** `windows [LeavePlay, AddedToVictory]`, i.e. every LeavePlay window fired
**twice**; deduplicating by dropping the `RemoveFromPlay` bracket kept the wrong copy.

**How to apply:** never "simplify" this back to a single `RemoveFromPlay` or a single
`windows [LeavePlay, …]` call — either direction re-breaks one of the two cards. Keep the expanded
`CheckWindows` list rather than the `windows` helper: the helper does not stamp `windowBatchId`, so
writing it out preserves the `AddedToVictory` batching exactly. `Window.LeavePlay (EnemyTarget …)`
is consumed only by `Matcher.EnemyLeavesPlay`, and there are zero `EnemyLeavesPlay #at` consumers,
so the blast radius of the `#when`/`#at` leg is exactly that matcher's users.

**Third-order consequence — the resolution net.** Moving `#when`/`#at` ahead of `RemovedFromPlay`
means an act that ends the scenario from its leave-play Objective now pushes `R1` while the enemy
is **still in play** and `Do (AddToVictory …)` is still queued. `Scenario.hs`'s `ScenarioResolution`
handler captures in-flight adds before `clearQueue`, but it selected
`OutOfPlayEnemy RemovedZone EnemyWithVictory` — which then matched nothing, dropping the enemy's
points. It now matches the *queued message* instead:

```haskell
addToVictoryMsgs <- Lifted.capture $ Lifted.doNow \case
  Do (AddToVictory _ (EnemyTarget _)) -> True
  _ -> False
```

Widening is safe because `doNow` is a queue-pop, not an add. This also closes a pre-existing hole:
`EnemyWithVictory` is `getHasVictoryPoints` (victory only), so vengeance-only enemies — Harbinger of
Valusia `04062`, vengeance 5 — were never covered, though `Do Defeated` sends them to the victory
display via `isJust (victory <|> vengeance)`.

Acts on this path: `RecoverTheRelic` guards itself with `addToVictoryIfNeeded` before `push R1`
(`doNow` pops the queued message into the same `runQueueT` buffer, so it lands ahead of the
resolution); `AlejandrosPlight` (Henry Deveau `04130b`, victory 1) and `ImpossiblePursuit`
(Harbinger) do **not** and depend entirely on the net.

See [[project_remove_from_game_skips_leaveplay]] (#5309), [[project_removedlocation_inverted_sweep]]
(#5426), [[project_leave_play_tombstones]] (#5518).
