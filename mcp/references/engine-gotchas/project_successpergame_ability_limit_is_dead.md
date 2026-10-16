---
title: project_successpergame_ability_limit_is_dead
description: AbilityLimitType's SuccessPerGame constructor is declared and never consumed, so reaching for it to implement "group limit 1 success per game" compiles clean and imposes no limit at all
---

`Arkham.Ability.Limit` declares `SuccessPerGame` and **nothing anywhere reads
it** — the declaration at `Ability/Limit.hs:44` is the only hit in the tree. A
card that uses it for the printed phrase "(Group limit 1 success per game.)"
compiles, type-checks, and silently allows unlimited successes.

The working idiom is a **Semaphore modifier** gating the effect:
`Asset/Assets/StasisCube.hs` ("Group limit one success per game") and
`Asset/Assets/OmniTransmitters.hs` ("Max one success per game").

Note what the semaphore does and does not do: it gates the *effect*, not the
ability, so the ability stays activatable after the success is spent. That is
legal — the failure branch can still change state — but a player can burn an
action for nothing. Gating the ability too (`exists (be a <> LocationWithoutModifier Semaphore)`)
departs from both precedents.

Contrast the plain "(Group limit once per game.)", which is just `onlyOnce`.
