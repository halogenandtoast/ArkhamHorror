---
name: project_end_of_turn_effect_created_after_endturn_is_orphaned
description: EndOfTurnEffect only LISTENS for EndTurn, so an atEndOfTurn created after the turn closed sat inert until a later turn's EndTurn fired every orphan at once (#5945)
metadata:
  type: project
---

`atEndOfTurn` (`Message/Lifted.hs`) pushes `CreateEndOfTurnEffect`, which parks the captured messages
on an `eotef` effect whose only trigger is `EndTurn iid | isTarget iid attrs.target`
(`Effect/Effects/EndOfTurnEffect.hs`). **It is a listener, not a scheduler** — it has no notion of
*which* turn. Created after `EndTurn iid` has already been processed, it is simply inert, and stays
inert until some **later** `EndTurn iid` comes along and fires every accumulated orphan at once.

Nothing errors and nothing logs. The symptom surfaces one or more turns later as a *duplicate*.

#5945: Spiritual Echo (2) (`11075`, `Mutated25`/`TabooList25`, "Return Spiritual Echo to your hand at
the end of your turn") pushes `atEndOfTurn` **behind** the echoed ability in the same `runQueueT`
block. The echoed ability was Rite of Seeking's Investigate — an action with a skill test — so
`handleDoUseAbility` (`Investigator/Runner/Action.hs`) wraps its resolution in `MoveWithSkillTest` and
the handler's tail resolves far downstream, after the player's last action had already closed the turn.
State at the orphan's birth (`arkham-replay --undo 77`): `endedTurn: True`,
`remainingActions: 0`, and `gameTurnPlayerInvestigatorId` **already the next player**. Trace:

```
> ResolvedAbility (… abilityCardCode = "11075" …)
> MoveWithSkillTest (ResolvedAbility (… "02028" …))   ← echoed Rite of Seeking
> CreateEndOfTurnEffect (EventSource …) "09011" [ReturnToHand …]
> ChooseTurnPlayer
> BeginTurn "60101"
```

The orphan then rode along to the *next* turn's `EndTurn`, where it fired beside that turn's
legitimate effect: two `ReturnToHand`s for one event, the first deleting it and the second crashing in
`getEvent` ("Unknown event"). `--undo 80 --replay-all` had accumulated **8** never-consumed `eotef`
effects.

**The limit was NOT the bug.** The two effects were from two different rounds and `usedTimes` was
correctly `1`; `playerLimit PerRound` and the card's `CannotTriggerAbilityMatching` self-echo guard
both work. Checking `actionsPerformed` is what distinguishes "same turn, limit broken" from "different
turns, effect orphaned" — here `[[Resource],[Resource],[Activate,Investigate],[Activate,Investigate]]`
vs `[[Play],[Activate,Fight],[Activate,Fight]]`.

Fixed in the `CreateEndOfTurnEffect` handler (`Game/Runner.hs`): if the turn is already over, `pushAll`
the messages inline instead of parking them.

```haskell
turnOver <- case gamePhase g of
  InvestigationPhase {} -> maybe False (attr investigatorEndedTurn) <$> getInvestigatorMaybe iid
  _ -> pure False
```

The `InvestigationPhase` guard is load-bearing: `investigatorEndedTurn` is only cleared by
`Do BeginRound` / `Begin InvestigationPhase`, so it stays `True` through Enemy/Upkeep/Mythos — without
the guard, a card scheduling an end-of-turn effect in a later phase would fire it immediately.

All 8 `atEndOfTurn` call sites are "return this to hand at end of your turn" *after a skill test*, so
every one of them could be born late the same way: `SpiritualEcho2`, `Pilfer3`, `SlipAway2`,
`BreakingAndEntering2`, `SpectralRazor2`, `ReadTheSigns2`, `ShoveOff`,
`TheBookOfWarSunTzusLegacy` — the same card set as [[project_addtohand_resurrects_removed_from_game_cards]].

Also fixed alongside: `ReturnToHand iid (EventTarget …)` was the only arm of that `case` that still
hard-faulted via `getEvent`; the `SkillTarget`, `CardIdTarget` and `AssetTarget` arms already no-op
when the entity is gone. It now uses `getEventMaybe`.

**Still open (deliberately):** why the resolution lands after `EndTurn` at all —
`Do (ChooseEndTurn)` winning the race against work still owed behind a `MoveWithSkillTest` deferral.
Changing that reorders every reaction window. See [[project_liftrunmessage_escapes_runqueuet_ordering]].
