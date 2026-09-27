---
name: project_simultaneously_isolates_the_queue
description: A Simultaneously branch runs with a CLEARED queue — read-modify-write store bumps read stale values, `Priority` is unwrapped (losing its queue-jumping protection), and a nested `Simultaneously` used to be dropped whole
metadata:
  type: project
---

`Game.hs`'s `Simultaneously msgs` handler runs each message through the full pipeline with the
queue **cleared**, captures what that message pushed, and only restores the interleaved result
after every branch has run (`interleaveSimultaneously`). Game *state* threads across branches
(`overGameM`); the *queue* does not. Consequences:

1. **Read-modify-write through the queue loses every bump but one.**
   `storedInt k >>= setStore k . (+1)` in a `Defeated` handler read `0` in all three branches
   when Stir the Pot killed three Whippoorwills at once, so Bird Hunting never fired (#5691).
   Fixed by `IncrementGlobal` / `InsertGlobal` (`Arkham/Message.hs`, handled in
   `Campaign/Runner.hs`), which do the arithmetic at *processing* time — and by moving the
   threshold check onto the trailing `Do`, matched with the `CounterBumped` / `GlobalInserted`
   patterns in `Arkham/Achievement.hs`. `InsertGlobal` PREPENDS and nubs, because the
   `achievementProgress` payload order is asserted by specs.

2. **`Priority` was inert inside a branch, and is still only a *plain* push there.**
   `Priority`/`Run`/`Retain` are unwrapped by the main loop's `go'` dispatcher, but a branch
   calls `runMessage` directly. `Run` has a `runGameMessage` case; `Priority` did not, so every
   `Priority $ EarnAchievement` / `Priority $ SetGlobal` pushed from a simultaneous defeat was
   silently swallowed. `runGameMessage` now has `Priority msg' -> g <$ push msg'` — which keeps
   the message but **strips the priority**. After the first interleave a `bumpCounter` write is
   an ordinary queue message again, so any later `clearQueue` or queue filter in the cascade can
   still eat it. Do not treat `Priority` as protection for anything crossing a `Simultaneously`.

3. **A nested `Simultaneously` was dropped whole.** `interleaveSimultaneously` splices each
   branch's captured output verbatim into `Simultaneously allPres`, so a `Simultaneously` pushed
   from inside a branch becomes an *element* of another one and reaches `runMessage`, which had
   no case for it — only the main loop can run one (it needs to capture each branch's queue).
   Everything inside it vanished with no log line. `runGameMessage` now has
   `Simultaneously {} -> g <$ push msg` (#5694). Probe: replaying
   `Simultaneously [Simultaneously [DealDamage …]]` through `arkham-replay` dealt zero damage.

4. **Don't tally anything important through the campaign store.** #5694 is the same Bird Hunting
   achievement failing again on the same card: three Whippoorwills died in one action, all three
   landed in `gameTurnHistory`, and the store counter still read `1`. Counters that must be
   reliable should be derived from state the engine writes synchronously in `runGameMessage` —
   `getAllHistoryField #turn HistoryEnemiesDefeated` and friends — not from a queued message.
   Note the ordering: the campaign entity runs **first** in `instance RunMessage Game` and
   `runGameMessage` **last**, so on `Defeated` the campaign sees the earlier defeats recorded but
   not the current one; add it in (`+ 1` unless this `eid` is already in the list). The check may
   then fire once per simultaneous defeat — harmless, the API layer `ordNub`s earns and
   `insertUnique`s the row. Turn history is cleared on `After (EndTurn _)`, not on `BeginTurn`.

**How to apply:** anything pushed from a handler that can run inside a `Simultaneously` batch
must be self-contained — no value computed from a read that a sibling branch also writes, and no
queue-control wrapper the branch runner cannot interpret. See [[project_divided_damage_must_batch_per_enemy]]
for the other half of this shape, and [[project_achievements]] for the counter helpers. Regression
specs: "is earned when the three are defeated simultaneously" (ReturnToTheDunwichLegacySpec) and
"…three Ghouls at once" (ReturnToNightOfTheZealotSpec).
