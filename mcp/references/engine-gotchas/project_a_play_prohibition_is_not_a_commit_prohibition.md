---
name: project_a_play_prohibition_is_not_a_commit_prohibition
description: CannotPlay is read only by preventsPlay, never on the commit path, so the hand badge stamped "cannot play" over the commit the game was asking for (#5959)
metadata:
  type: project
---

The two prohibition predicates are genuinely separate, and nothing bridges them:

| predicate | where | honors |
| --- | --- | --- |
| `preventsPlay` | `Helpers/Playable.hs:283` | `CannotPlay`, `CannotPutIntoPlay`, `CanOnlyUseCardsInRole` |
| `preventsCommit` | `Helpers/SkillTest.hs:766` | `CannotCommitCards`, `CanOnlyUseCardsInRole` |

`git log -S CannotPlay -- library/Arkham/Helpers/SkillTest.hs` is **empty**: committing has never
consulted `CannotPlay`, which is the right reading — committing a card to a skill test is not
playing it. A card that prints "cannot play cards" and nothing about committing therefore wants
`CannotPlay`, full stop; reaching for `CannotCommitCards` as well would be inventing text.

**The trap is on the client, not in the engine.** `Game.hs:681` serializes `playProhibitions` and
`commitProhibitions` per investigator, and `getPlayProhibitions` is ungated (rightly — a fast card
can be played during a test). `HandCard.vue` greyed the card and `ProhibitedBadge.vue` stamped a red
`ban` on it whenever **either** list named it. So at ST.2 under Antarctic Wind (`08692`,
"Investigators at attached location cannot play cards or draw cards from their deck") every card in
hand read as forbidden — including the one card sitting in the question as a
`TargetLabel (CardIdTarget …)` commit choice. The board contradicted its own prompt, and #5959 came
in as "Antarctic Wind will stop you commit cards to a skill test" with nothing wrong in the engine.

Fixed by the rule `cardProhibitions.ts`'s own header already states — a prohibition is worth badging
*because* it is a flat ban invisible until you try, as against the moment-to-moment reasons "which
the client already learns from whether the card is offered as a choice." So a card the active
question is offering is not marked: `HandCard.vue` computes `onOffer = cardAction !== -1` (the index
it already had) and both the greying and `ProhibitedBadge`'s new `suppressed` prop hang off it, so
the two cannot disagree. Same fix covers Dissonant Voices, The Tower · XVI and Directive: Red Tape
at a commit step.

**Verifying needs no EotE game and no export.** `CreateWindowModifierEffect` through the `/raw`
endpoint puts any modifier on any target for the life of a skill test — it is what the client's own
`debug.skillTestModifier` sends. Drive a standalone game to ST.2 and post:

```json
{"gameMessage":{"tag":"CreateWindowModifierEffect","contents":[
  {"tag":"EffectSkillTestWindow","contents":"<skillTestId>"},
  {"tag":"EffectModifiers","contents":[
    {"source":{"tag":"GameSource"},"type":{"tag":"CannotDrawCards"},"activeDuringSetup":false,"card":null},
    {"source":{"tag":"GameSource"},"type":{"tag":"CannotPlay","contents":{"tag":"AnyCard"}},"activeDuringSetup":false,"card":null}]},
  {"tag":"GameSource"},
  {"tag":"InvestigatorTarget","contents":"<iid>"}]}}
```

The commit `TargetLabel` survives untouched, `commitProhibitions` stays `[]`, `playProhibitions`
names the whole hand, and answering the choice commits normally. That table **is** the diagnosis:
when a report says "X stops me doing Y", check whether the engine still offers Y before hunting for
the block — a missing choice and a choice the board has greyed out look identical to a player.
