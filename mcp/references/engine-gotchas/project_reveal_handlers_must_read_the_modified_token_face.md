---
title: project_reveal_handlers_must_read_the_modified_token_face
description: "A RunMessage handler on RevealChaosToken that pattern-guards on token.face reads the PRINTED face, so every \"cancel that token and treat it as X\" effect is invisible to it — Eucatastrophe (3) could not stop Old Hunting Rifle (3) from jamming (#5892). Use ChaosTokenFaceIs / getModifiedChaosTokenFace, and ChaosTokenOriginalFaceIs when you really want the printed face"
---

"Treat it as an [elder_sign] token, instead" does **not** rewrite the token. Eucatastrophe (3)
attaches a `ChaosTokenFaceModifier [ElderSign]` to it (`Event/Events/Eucatastrophe3.hs`); the
token's `chaosTokenFace` stays `AutoFail` for the rest of the test. Same for Blessing of Isis (3)
(→ `Bless`), Curse of Aeons (3) (→ `Curse`), The Necronomicon, Jim Culver, Snipe (1),
Lucky Penny — and for `ForcedChaosTokenChange`.

So a handler written as a pattern guard is reading the wrong thing:

```haskell
-- WRONG: t.face is the printed face; the "treat as" modifier is invisible
Msg.RevealChaosToken (SkillTestSource _) _ t | t.face `elem` [Skull, AutoFail] -> do
  failSkillTest
```

That was #5892: the player revealed [auto_fail], played Eucatastrophe (3) to treat it as an
[elder_sign], and Old Hunting Rifle (3) jammed and auto-failed the attack anyway.

## The timing is not the problem — the read is

It is tempting to blame ordering, but the modifier really is in place. The `#when
RevealChaosToken` fast window (where Eucatastrophe's `cdFastWindow` fires) is raised in
`ChaosBag.hs` inside `RunDrawFromBag`, **before** `RequestedChaosTokens` ever reaches
`SkillTest/Runner.hs`. Only then does the runner push

```haskell
[When revealMsg, CheckWindows [AtIf RevealChaosToken], revealMsg, After revealMsg]
```

By the time a card sees the bare `RevealChaosToken`, `getModifiers (ChaosTokenTarget token)`
already holds the substituted face. Verified on the #5892 export: `gameModifiers` carried
`ChaosTokenFaceModifier ["ElderSign"]` on the token while the rifle was `Jammed`.

## Use the matchers

`Arkham.Matcher`'s chaos-token matchers already fold modifiers in (`Game.hs`, `Query
ChaosTokenMatcher`):

| matcher | reads |
| --- | --- |
| `ChaosTokenFaceIs face` | `elem face <$> getModifiedChaosTokenFace` — the **treated** face |
| `ChaosTokenFaceIsNot face` | `not` of the above |
| `ChaosTokenOriginalFaceIs face` | `chaosTokenFace` — the **printed** face |

Since the check is monadic, the guard has to move out of the pattern and into the body:

```haskell
Msg.RevealChaosToken (SkillTestSource _) _ t -> do
  jams <- t <=~> mapOneOf ChaosTokenFaceIs [Skull, AutoFail]
  ...
```

`getModifiedChaosTokenFace` (`Helpers/ChaosToken.hs`) is the equivalent direct call and returns a
**list** — a face modifier can expand one token into several faces — so fold it with `any`/`elem`
rather than `==`, and be careful with negated conditions (`all (/= f)`, not `head`).

**Dropping the pattern guard is safe**: no entity runner handles `RevealChaosToken`
(`Asset/Runner.hs`, `Skill/Runner.hs`, `Location/Runner.hs`, `Enemy/Runner.hs`,
`Treachery/Runner.hs`, `Event/Runner.hs`, `Effect/Runner.hs` all have zero cases, only a
`hiding (RevealChaosToken)` import), so a branch that now matches every reveal and returns
`pure a` loses nothing. Do not route the non-matching case back through `liftRunMessage` —
that pushes to the real queue mid-handler while `runQueueT` flushes afterwards.

## When the printed face IS what you want

The sweep in #5892 deliberately left two sites alone, and the distinction is worth keeping:

- **`Investigator/Cards/FatherMateoParallel.hs`** — its `RevealChaosToken … | drawnToken.face == #bless`
  branch is sealed-token bookkeeping: it unseals the *physical* bless token he sealed and prunes
  the `father_mateo_pending` meta key. A token merely *treated as* bless must not match, or it
  unseals a token he never sealed.
- **`Homebrew/CircusExMortis/Achievements.hs`** — "Shoot the Moon" counts moons actually drawn.

Rule of thumb: if the card text says a **symbol is revealed** (skull, auto-fail, zero, cultist…),
read the modified face. If the handler is manipulating a specific *physical* token — unsealing it,
returning it to a pool, counting real draws for an achievement — read the printed face, and say so
with `ChaosTokenOriginalFaceIs` rather than `.face` so the intent is legible.

Related: [[project_rereveal_uses_test_investigator]].
