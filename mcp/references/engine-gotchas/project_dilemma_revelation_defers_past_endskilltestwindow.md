---
title: project_dilemma_revelation_defers_past_endskilltestwindow
description: "A Dilemma drawn during a skill test must have its Revelation inserted after EndSkillTestWindow (FAQ 2.25) — and the read/insert MUST be lifted out of runQueueT or it silently inspects an empty inbox"
---

FAQ v2.5 §2.25: *"If a **Dilemma** card is drawn while resolving a skill test, resolve the
skill test in full before resolving the Dilemma's **Revelation** effect."* Nothing in the
engine consulted the skill test: `handleDoInvestigatorDrewPlayerCardFrom` and
`handleDoAddToHand` (`Investigator/Runner/Card.hs`) pushed `Revelation iid (CardIdSource …)`
the moment the draw finalized (#5974).

It matters only for Dilemmas, and it matters a lot for them. All six — Making Preparations
`09106`, Predator or Prey `09107`, At a Crossroads `09109`, Nature of the Beast `09111`,
Heed the Dream `09115`, Fickle Fortune `09118` — are heavyweight revelations: At a
Crossroads grants an investigator **an action as if it were their turn**, Nature of the
Beast searches the encounter deck and makes someone draw an encounter card, Predator or
Prey moves enemies and investigators, Heed the Dream poses a three-pile deck choice,
Fickle Fortune places/removes doom and deals direct damage. At a Crossroads already
carried a `shouldMoveWithSkillTest_` band-aid on its random discard, which was the symptom.

**The anchor is `EndSkillTestWindow`, not `inSkillTest`.** `SkillTest/Runner.hs:217` queues
that sentinel last when the test is declared, and every later part of the test — the token
reveal, the success/failure resolution, `SkillTestEnds`, `AfterSkillTestEnds`,
`SkillTestEnded` — is pushed *in front of* it. So `insertAfterMatching msgs (== EndSkillTestWindow)`
literally means "after the test resolves in full", and the sentinel being gone is what
makes a re-queued resolution self-terminating. `inSkillTest` (`Helpers/SkillTest.hs:95`) is
the wrong gate twice over: between the message that installs the skill test and
`BeginSkillTestAfterFast` the test exists while the sentinel does not, and
`insertAfterMatching` **errors** when nothing matches. With nested tests the FIRST sentinel
is the innermost (current) test, which is the one the FAQ means.

**`MoveWithSkillTest` is not this.** It means "travel with a *nested* test that begins",
consumed by `handleSkillTestNesting` (`Helpers/Message.hs:698`). For "wait until the
current test is over" the existing idioms are `afterSkillTestQuiet` and `delayIfSkillTest`
(`Message/Lifted.hs:2569`, `:2597`) — both anchored on the same sentinel.

**The trap that actually cost the time:** these handlers run inside
`runInvestigatorMessage`, which is `runQueueT $ case msg of …`. An unlifted
`fromQueue (elem EndSkillTestWindow)` there reads `runQueueT`'s **fresh local inbox**, which
is empty — so the check answers `False`, the deferral silently never fires, and nothing
errors. Both the read and the insert need `lift`; the enclosing `do` must then carry
`(MonadTrans t, HasQueue Message m, HasQueue Message (t m))`. Same hazard as
[[project_cancelenemydefeat_queue_layer]].

Keep `Revelation` and `ResolvedCard` together when deferring: `ResolvedCard`
(`Game/Runner.hs:4056`) does the surge check, clears `resolvingCard`/`activeCard`, pops the
`cardPlayStack` log block and ends `EffectCardResolutionWindow` (`Effect/Runner.hs:157`).
The `#after DrawCard` window stays at its normal time — the card *was* drawn during the test.

§2.25's other clause ("a weakness and/or encounter card in hand resolves before a Dilemma")
is the setup handler, `ForInvestigator iid BeginGame` in `Investigator/Runner.hs`: it offered
one `chooseOrRunOneAtATime` over every revelation card in hand, so the player could pick
either order. Now partitioned into two asks, non-Dilemma first. **Still not covered:** a
multi-card *draw* that hits a weakness and a Dilemma together resolves in draw order,
because each card arrives as its own `Do (InvestigatorDrewPlayerCardFrom …)` and no single
place sees the batch; sorting `handleDoDrawCardsV2`'s deferred finalizers risks the #4867
phantom-duplicate behaviour `AtACrossroads1Spec` guards.

**Testing note.** Driving this from a spec is subtle: `runMessages` does *not* stop at a
pending `gameQuestion` — `Ask` sets the question and returns, and the next `run` drains
straight through whatever was parked behind it. So after
`run $ beginSkillTest sid self #intellect 2`, a `drawCards` drains the rest of the test's
queue *including* the sentinel, and "the event entity does not exist yet" is false even
with the fix working. The discriminator that does work: the revelation parks an ask, so
check that `EndSkillTestWindow` is **no longer** in the queue when it does
(`peekQueue shouldSatisfyM notElem EndSkillTestWindow`) — pre-fix the revelation cuts in
line and its ask arrives with the sentinel still queued.
