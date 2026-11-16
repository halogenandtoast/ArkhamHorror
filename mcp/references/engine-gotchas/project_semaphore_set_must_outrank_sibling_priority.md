---
name: project_semaphore_set_must_outrank_sibling_priority
description: A semaphore whose SET is a plain push loses to sibling Priority messages — popMessageWithPriority pulls a later Priority ahead of the queue head, so Quick Thinking's two copies both passed the once-per-round guard (#6005)
metadata:
  type: project
---

`semaphore target body` (`Arkham.Helpers.Modifiers`) reads a `Semaphore` modifier, and the
body typically *sets* it with `roundModifier` — which only **queues**
`CreateWindowModifierEffect`; `runQueueT` flushes it to the front when the handler returns.
That makes check-and-set non-atomic, and the gap is wide open because
`popMessageWithPriority` (`Arkham/Game.hs:6968`) does not merely honour a `Priority` at the
**head** — when the head is not `Priority` it scans the rest of the queue and pulls the
first `Priority` forward:

```haskell
(m : ms) -> case find isPriority ms of
  Just p@(Priority pm) -> (m : delete p ms, Just pm)
```

`Arkham/SkillTest/Runner.hs` fans a result out as **one `Priority (PassedSkillTest …)` per
subscriber**, so with two copies of a card committed to one test, copy B's handler runs
*before* copy A's queued effect, reads the semaphore as free, and does the thing twice.
Quick Thinking (02229) under taboo 18+ granted two additional actions in one round this way
(#6005); the export showed **two** simultaneous `Semaphore` modifiers on
`CardCodeTarget "c02229"`, one per copy.

**How to apply:** when a shared semaphore key can be reached by more than one entity in the
same message batch (a card code, an investigator — anything not the entity's own id), push
the SET through the `Priority` lane so it outranks the siblings:
`push . Priority =<< Modifiers.roundModifier attrs (toCardCode attrs) Semaphore` (the
unlifted `Arkham.Helpers.Modifiers.roundModifier` returns `m Message` instead of pushing).
Same idiom as [[project_setglobal_priority_victory_clearqueue]]. A semaphore keyed on the
entity itself (`semaphore attrs`, as `onlyOnceDuringSkillTest` does) cannot race.

Scope note: a "max per round" limit is **per card code and applies to all players**, so the
bare `CardCodeTarget` key is correct — see [[project_max_per_round_is_per_card_code_all_players]].
