---
name: project_passedskilltest_type_is_post_mutation
description: "PassedSkillTest/FailedSkillTest carry the MUTATED SkillTestType, so chaining tests by matching SkillSkillTest SkillX silently dies under Money Talks / Carnevale masks (#5539)"
metadata: 
  node_type: memory
  type: project
  originSessionId: e4cea490-1290-4830-862a-efb48943131f
  modified: 2026-08-28T13:04:13.577Z
---

`ChangeSkillTestType` rewrites the live test's type in place
(`Arkham/SkillTest/Runner.hs`, `typeL .~ newSkillTestType`), and `PassedSkillTest` /
`FailedSkillTest` are emitted carrying that already-mutated `skillTestType`. So a card that
sequences two tests by pattern-matching
`PassedSkillTest ... (SkillSkillTest SkillCombat) _` stops matching the moment anything
retypes the test: **Money Talks** (05029/08054 → `ResourceSkillTest`) and the Carnevale masks
(Pantalone, Bauta, Gilded Volto, Medico della Peste). The message falls through to
`liftRunMessage`, the chain dies with no error, and the ability just ends.

Fixed in Seafloor Frieze (11531, #5539) by tracking the stage in the treachery's `meta`
(`setMeta (stage :: Int, passedFirst :: Bool)` + `toResultDefault`) and matching `_` for the
skill test type. Note `TreacheryAttrs` has no `setMetaKey`/`getMetaKeyDefault` — those are
Asset/Scenario/Investigator only.

Also fixed, same root cause, different symptoms — **neither runs both tests**:
- `Location/Cards/TheDreamEaters/WakingNightmare/PrivateRoom.hs` (06077) — "Test [willpower]
  (2). *If you succeed*, test [intellect] (2)." Conditional chain; retyping the first test
  meant the intellect test never started. Now chained with `onSucceedByEffect sid AnyValue …
  $ doStep n msg` riders keyed to each test's id, which fire only on success, so the
  conditionality is preserved without reading the type.
- `Treachery/Cards/MurderAtTheExcelsiorHotel/NoxiousFumes.hs` (84023; the Midwinter Gala
  71058 is a newtype delegating to it) — the investigator picks *one* of two tests, and all
  three outcome branches were keyed on `SkillSkillTest SkillAgility`/`SkillCombat` with no
  fallback, so retyping lost the **outcome** (no move on a passed agility test, no damage on
  a failure). Now each option routes through its own `DoStep` that records `(iid, skill)` in
  the treachery's meta; the branches match `_` and guard on that. Per-investigator, so the
  "in player order, each investigator" fan-out is safe, and the combat branch still reads `n`
  off `FailedSkillTest` for "each point you fail by".

Separate, Seafloor-Frieze-only point: "test A, **then** test B. If you succeed at both…"
means both tests always happen — only the payoff is conditional. Driving that chain solely
from `PassedSkillTest` skipped the second test (and its chaos-token effects) on a failure.
Do not generalise this to the two cards above.

**Why:** nothing warns you; the branch just never fires.

**How to apply:** never discriminate chained/branching skill tests by the `SkillTestType`
field on `PassedSkillTest`/`FailedSkillTest`. Track stage in `meta` (or key a rider to the
`SkillTestId` via [[project_onsucceedby_rider_repeat_skilltest]]) and match `_` for the type.
