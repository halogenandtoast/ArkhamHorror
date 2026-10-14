---
title: project_a_decoded_payload_is_substituted_never_evaluated
description: Every custom-card payload the engine decodes is substituted only, so an expression inside a modify target or a pushed message stays an object and the step does nothing
---

`decodeWith env = parseMaybe parseJSON . substitute env`, and `substitute`
replaces `$bindings` and **evaluates nothing** (`Custom/Env.hs:22`). So an
expression left inside any payload the engine decodes — a `push`, a `modify`'s
`target` or `window`, a matcher, a placement, a modifier — stays a JSON object,
the value fails to parse, and the step silently does nothing. A `push` reports
`reportBadPayload`, which only reaches the websocket; a `modify` reports nothing
at all.

Read an id out with a `let` first, then name the binding:

```json
{"let": "committedCardId", "be": {"field": "id", "of": {"field": "contents", "of": "$w1"}}},
{"modify": {"target": {"tag": "CardIdTarget", "contents": "$committedCardId"}, "modifiers": [...]}}
```

Five Circus Ex Mortis printings had a dead `modify` target this way, all of them
reading an id off a window or a bound card: Memorization 0/1, Barbell 0/1, Réka
Zilahy 0/3 and Leap of Faith 0/3. Every one of them validated, and every one
looked like a card whose effect was simply not noticeable.

`validate.py` now reports an expression in a decoded payload as an error. It is
deliberately scoped to the keys the engine decodes rather than to every value:
a step's own `times`, `query` or `be` ARE expression contexts, and checking
those flagged 224 legitimate uses.

Related, on the same card: a `[reaction]` on `WouldDiscardFromHand` cannot
`cancelBatch` the upkeep hand-size discard — `handleDiscardFromHand`
(`Investigator/Runner/Card.hs:249`) raises the window with `checkWhen` and
pushes `Do msg` with **no batch at all**, so there is nothing to cancel (the
same shape as [[project_cancelbatch_does_not_cancel_a_move]]). The discard's
count is also fixed before the window opens, so pulling a card out of hand from
inside it costs the player an extra card rather than saving one. "Shuffle it
back into your deck instead" is said to the CARD:
`ShuffleIntoDeckInsteadOfDiscard` on its `CardIdTarget`, which `AddToDiscard`
now honours.
