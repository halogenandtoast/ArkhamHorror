# `SkillTestAt` is about the test's TARGET, not the investigator

`Matcher.SkillTestAt loc` evaluates to
`targetMatches st.target (TargetAtLocation loc)` (`Helpers/SkillTest.hs`), i.e. it
asks *where the thing being tested against is*, never where the testing
investigator is. `SkillTestAtYourLocation` is the one that compares the two
investigators' locations.

That makes `SkillTestAt (locationWithEnemy a)` on an enemy's own ability
**vacuously true** whenever the test targets that enemy: the Beast's location
always contains the Beast. The Beast in a Cowl of Crimson (09655b / 09755) —
"After you fail a skill test **while at** The Beast's location, if it is ready" —
fired off Swift Retreat (09728), which picks the *nearest* Coterie enemy and
tests agility against it from a different location (#5698).

Printed "while at X's location" is a constraint on **you**, so it belongs in the
window's `Who`:

```haskell
$ forced
$ SkillTestResult #after (You <> at_ (locationWithEnemy a)) AnySkillTest #failure
```

Keep the `You` — see [[project_forced_window_who_must_be_you]].

`SkillTestAt` is still right when the test's target *is* the location in question
(investigate), or when the action already forces co-location (fight/evade, e.g.
Cavern Moss 10585).

Dropping `SkillTestAt` for `AnySkillTest` loses nothing on the failure side:
`Investigator/Runner.hs` always pushes `Window.FailSkillTest iid n` alongside the
action-specific `FailInvestigationSkillTest` / `FailAttackEnemy` /
`FailEvadeEnemy` windows, and the generic `FailSkillTest` branch is the one that
runs when the skill matcher is not a `While*` matcher.
