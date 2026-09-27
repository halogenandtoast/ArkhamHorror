---
name: project_onsucceedby_rider_repeat_skilltest
description: "On-succeed/fail/reveal riders must disable at SkillTestEnded (not SkillTestEnds) and follow RepeatSkillTest, or Live and Learn drops them"
metadata: 
  node_type: memory
  type: project
  originSessionId: bf072b0b-c5e5-4394-a738-d9d34fe61d08
  modified: 2026-07-28T12:29:31.877Z
---

`OnSucceedByEffect` / `OnFailedByEffect` / `OnRevealChaosTokenEffect` used to disable on
`SkillTestEnds`, which the runner pushes *before* `windows [Window.SkillTestEnded]`. A repeat
declared in that window (Live and Learn, Daniel Jameson, Old Compass, Token of Faith (3)) therefore
found the rider already gone — Act of Desperation's "gain X resources" never paid out on the
repeated test, while its `SkillModifier #combat` / `DamageDealt 1` did carry (issue #5274).

Window-modifier effects disable on `Msg.SkillTestEnded sid` — one step later, *after* that window —
which is why they survive. Fixed by aligning the three riders to the same point and giving each a
`RepeatSkillTest sid stId | Just stId == attrs.skillTest` case that re-points `effectSkillTest`.

**Why:** the queue order inside `Do (SkillTestEnds …)` is `AfterSkillTest …` → `windows
[SkillTestEnded]` → `AfterSkillTestEnds` → `SkillTestEnded sid`. Anything that must outlive a
possible repeat has to key its teardown off the last of those, not the first.

**How to apply:** don't route this through `Effect.Runner`'s generic `RepeatSkillTest` handler — it
additionally requires `st.source == a.source`, which holds when the card itself initiated the test
(Act of Desperation) but silently drops riders built from an ability source (String Along, Breath of
the Sleeper, Uncanny Specimen). `SkillTestEnded` also collides with an `Arkham.Matcher` window
constructor, so those modules need it in the `hiding` list.

Related: [[project_window_entry_tick_timing]], [[project_deferred_enemymove_superseded]]

**Bespoke card effects need the same treatment, plus a window (#5524).** A `createCardEffect` with
`effectSkillTest = Nothing` and no `effectWindow` is invisible to *both* generic handlers, so
`SkillTestEnds {} -> disableReturn e` was the only teardown — and it fires before the repeat window.
Spectral Razor's `DamageDealt 2` died there while its `AddSkillValue #willpower` (a
`skillTestModifier`, i.e. a `WindowModifierEffect` carrying `EffectSkillTestWindow`) survived and was
re-pointed. Fix: `createSkillTestCardEffect sid def mMeta source target`
(`Arkham/Message/Lifted.hs`) sets `effectBuilderSkillTest` **and**
`effectBuilderWindow = EffectSkillTestWindow sid`; then delete the card's `SkillTestEnds` clause and
let `Effect/Runner.hs:102` (disable at ST.8) and `:188` (re-point) do the work. Applied to Spectral
Razor, Spectral Razor (2), Bind Monster (2), Storm of Spirits (3), Dreamer's Chronicle.

**The source gate *is* the ruling.** `Effect/Runner.hs:188` re-points only when
`st.source == a.source`, which encodes the project ruling that a repeat carries over only what is
inherent to the test — the ability/card that produced it — never a separate reaction triggered on
the original test. That is why Gregory Gry, Crystal Pendulum, Kymani Jones, William Yorick, Nathaniel
Cho, Yaotl (1) and Lucky Dice (2) deliberately keep their `SkillTestEnds` teardown. Prefer the
generic path over a per-card `RepeatSkillTest` clause precisely because it can't bypass that gate.

**An effect whose payload self-disables needs a latch, not `disable`.** Dreamer's Chronicle used
`disable attrs` on first commit, so nothing remained to carry; use `finishedEffect` as a
once-per-attempt latch and clear it with `unfinishedEffect` in a `RepeatSkillTest` clause that
*delegates* via `liftRunMessage` so the generic re-point still runs.

Related: [[project_stack_work_cache_poisoned_by_cancelled_build]]
