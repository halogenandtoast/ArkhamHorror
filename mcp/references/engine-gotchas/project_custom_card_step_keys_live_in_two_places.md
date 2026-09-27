---
name: project_custom_card_step_keys_live_in_two_places
description: A custom-card step's keys sit INSIDE its value or BESIDE it depending on how runSteps dispatches it; the wrong place is silently never read
metadata:
  type: project
---

`runSteps` (`Arkham/Custom/Steps.hs`) dispatches a step two different ways, and
where the step's keys live follows from which:

* **Handed to a handler** (`| Just spec <- KeyMap.lookup "chooseFrom" o -> runChooseFrom env spec`).
  The handler does `o = specObject spec`, so `o` is the step's **own value** and
  every key it reads lives inside it:

  ```json
  {"chooseFrom": {"bind": "card", "query": {...}, "steps": [...]}}
  ```

* **Handled in the guard arm itself** (`| Just (String name) <- KeyMap.lookup "let" o -> …`).
  There `o` is still the **step object**, so the keys sit beside the step key:

  ```json
  {"let": "cid", "be": {"get": "id", "kind": "card", "of": "$card"}}
  ```

Beside-the-step is the minority: `query` (`mode`, `bind`), `let` (`be`),
`if`/`when` (`then`, `else`, via the `branch` helper) and `case` (`else`). Every
other step's keys are inside its value.

A key in the wrong place is not an error — it is simply never looked up, so the
step runs with that option defaulted. `{"chooseFrom": {...}, "bind": "card"}`
binds nothing and the steps beneath it see `$card` unsubstituted, which then fails
to decode and drops whatever contained it.

Two further places keys hide from a naive scan of the step's own function:

* every step that starts a skill test (`fight`, `investigate`, `evade`, `parley`,
  `test`) shares `beginTest`, which is where `modifiers`, `onReveal`, `onSuccess`,
  `onFailure` and `by` are read;
* `withTestSkill` reads `skill` and `insteadOf` through a local
  `field k = KeyMap.lookup k o`, so neither appears as a literal lookup anywhere.

`request` is the odd one out: `runRequest` reads only `push`, while `on` and
`steps` are read later by `runCustomHandlers` walking the def for `request`
blocks (the answer arrives long after the step that asked has finished).

The generated reference in `.claude/mcp/arkham-cards/dsl.json` records this per
step (`keysIn`, `stepKeys`, `payloadKeys`); ask that server's `guide` tool for
section `steps`, or re-run its `extract_dsl.py` after changing the dispatch.
