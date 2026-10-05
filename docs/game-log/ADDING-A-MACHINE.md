# Adding a narrator machine

How to teach `Arkham.Log.Narrator` a new event. Read `JOURNAL.md` first.

## Observe the message lifecycle before writing anything

This is not optional, and it is the step that is tempting to skip. The skill
test machine is correct only because its sequence was read off a real trace;
the obvious guess would have been wrong in two ways at once.

What guessing would have got wrong:

- **`PassedSkillTest_` fires once per participant, not once per test.** 36 times
  for three tests in one real trace — one copy to each committed skill, the
  investigator, and every revealed chaos token, so each can react. Only the copy
  addressed to `SkillTestInitiatorTarget` is the result.
- **The same payload appears wrapped.** 4 of 10 copies were inside
  `Do (After (…))`. `After`, `When` and `Would` surround the real occurrence;
  matching them narrates one event three times.

Neither is visible in the type. Both are obvious in a trace.

**`--replay-all` will not do this.** It reinstalls each step's *saved queue* —
what remained after the action — so the messages that actually ran are gone.
Replaying 8 steps around a completed skill test showed **zero** skill-test
messages. Use it for timing, not for lifecycles.

The way that works is live instrumentation: a temporary `hPutStrLn stderr` in
`narrate` (the dev server's stderr is teed into `.claude/build.log`), then play
the event in the browser. Log the **decision** each step makes, not just the
message — and do not truncate, or a field you are matching on will look absent
when it was only cut off.

```haskell
-- in narrate, temporarily
let decision = case before of
      [] -> if isJust (opener msg) then "OPEN" else "none"
      (f : _) -> case step msg f.frameMachine of
        Ignore -> "Ignore"; Wait _ -> "Wait"; Emit _ -> "EMIT"; Bail -> "BAIL"
hPutStrLn stderr $ "NARRATOR[" <> show (length before) <> "/" <> decision <> "] " <> take 420 (show msg)
```

Read the run for the event start to finish. Write down, concretely:

1. Which message **opens** it, and what that message already carries.
2. Which messages **add** something you want (commits, reveals, modifiers).
3. Which message means it is **finished** — and check it fires exactly once.
   If several look like candidates, count them in the trace.
4. What it looks like when the event is **cancelled or abandoned**.
5. **Whether it spans an ask.** This is the one that bit hardest. Frames live
   for a single action, and a skill test spans four — so its frame was gone
   before the result arrived. If the event crosses an ask, it cannot use a
   frame: emit from one message and read the engine's own state for the rest.

Getting an export: `GET /api/v1/arkham/games/:id/export` (any logged-in user;
the last 30 steps). `/full-export` needs admin and currently truncates — see
`FINDINGS.md`.

## Then write the machine

In `Arkham/Log/Narrator.hs`:

1. A state record holding **only what the finishing message will not carry**.
   Everything else is noise to maintain.
2. A `Machine` constructor for it. (Drop the `HLINT ignore` on `Machine` once
   there is more than one.)
3. A case in `opener` for the opening message.
4. A case in `step`, dispatching to your own `stepX`.
5. Your `stepX :: Message -> XFrame -> Step`:
   - `Wait` while accumulating,
   - `Emit` exactly once, on the finishing message,
   - `Bail` when the event is cancelled or stops making sense,
   - `Ignore` for anything that is not yours — **including the messages that
     open other events**, or they get swallowed by your catch-all instead of
     nesting. This is a real bug that was caught in review, not a theoretical
     one.
6. A `renderX` returning a `LogEntry`, with the detail as children.

## Name collisions to expect

`SkillTest`'s own record fields occupy every obvious name a skill-test helper
would want — `skillTestStep`, `skillTestResult`, `skillTestType`. Three
collisions in one small module, each a `GHC-87543 Ambiguous occurrence` that
only appears once the whole library has recompiled. Prefix helpers with
`step`/`render` (`stepSkillTest`, `renderSkillTestResult`) and the problem goes
away. The same will be true of any entity whose record fields share its name.

Also: `Arkham/Game.hs-boot` re-declares `runMessages`. Change one signature
without the other and GHC-11890 lands at module ~8556 of 8640.

## Rules the module keeps

- **Silence is the default.** Never add a catch-all rendering. Roughly 64% of
  messages are plumbing (`Do`, `CheckWindows`, `ClearUI`, `Ask`); a fallback
  would bury the log in them.
- **Emit once.** A frame closes when it emits, so duplicate triggers are
  dropped for free. Rely on that rather than a seen-set.
- **Never break the game.** This runs inside `runMessages`. No throwing, no
  pushing messages, no assuming an entity still exists. Take what you need off
  the messages.
- **No English in Haskell.** Every string is an `ikeyPart` with a key in
  `frontend/src/locales/en/log.json`. Terse, present tense, no filler: "Daisy
  fails by 2", never "Unfortunately, Daisy was unable to…".
- **No names server-side.** Use `investigatorRefById` and friends; the client
  resolves display names from the card store, which localizes and follows a
  card whose identity changed later.
- A variable named `count` drives vue-i18n pluralization. Name it something
  else if the message has no `|` branches.
- **Never name a variable after an Arkham icon.** `frontend/src/arkham/icons.ts`
  lists them — `action`, `skull`, `cultist`, `tablet`, `combat`, `intellect`,
  `willpower`, `agility`, `fast`, `bless`, `curse`, `elderSign`, … The client
  rewrites `{action}` into a literal glyph *before* vue-i18n sees it
  (`escapeIconPlaceholders` in `locales/messages.ts`), so a variable by that
  name is **silently never substituted** — the placeholder just appears in the
  log as `{action}`. Cost an hour to find; `{verb}` instead of `{action}`.

## Check it

```sh
arkham-replay <export.json> --undo 1 --trace --output /dev/null 2>t.txt
grep -A 6 '^client> log' t.txt
```

`--trace` prints the narration indented beside the messages it came from, which
is the only way to see whether the grouping is right. Note `--replay-all` is
answer-free and dies on most exports (see `FINDINGS.md`); `--undo N` alone only
re-runs a parked queue. The reliable check is still a live game.
