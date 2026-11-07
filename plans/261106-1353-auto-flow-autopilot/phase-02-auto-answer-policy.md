# Phase 2: Pure answer policy

## Files
- New `frontend/src/arkham/autoAnswerPolicy.ts` (pure, no Vue).
- New `frontend/tests/autoAnswerPolicy.test.mjs` (same pattern as `locationGraph.test.mjs`).
- `frontend/src/arkham/views/Game.vue`: the keyboard "continue" handler (~2220-2245) calls the shared helper instead of its inline rule.

## API
`decideAutoAnswer(game, playerId, question, level, stops) -> { index, reason } | null`

`reason` is a stable key (`encounterDraw`, `applyResults`, `nothingToCommit`, `passFastWindow`, `autoLabel`, `singleValid`, `soloPlayerOrder`) used by the reveal feed and log.

## Rules (allow-list)
Standard:
- Single valid choice (excluding `Info`, `InvalidLabel`), unless the question is narrative/story or a campaign choice.
- Encounter draw whose only valid choice is `TargetLabel EncounterDeckTarget`.
- `SkillTestApplyResultsButton` alone.
- `ChooseOneAtATimeWithAuto` -> its auto label.
- Solo `ChoosePlayerOrder`.

Full adds:
- Fast windows whose only options are `SkipTriggersButton` plus non-blocking reactions -> pass, unless that phase has a stop set.
- Start skill test / done committing when the seat has no committable card.
- Enemy attack order when all attacking enemies are identical in effect (same damage/horror, no attack-trigger text).

Never: 2+ meaningful options, commit with committable cards, damage/horror assignment with soak assets, hand-size discard, mulligan, hunter ties, story/campaign/resolution choices, deck upgrade, `Info` screens.

## Done when
Tests cover every rule, every "never" case and an unknown question shape (returns null). `npm test` green.
