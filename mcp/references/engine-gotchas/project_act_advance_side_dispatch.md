---
title: project_act_advance_side_dispatch
description: "AdvanceAct dispatches twice (side A then side B); card handlers must guard on `isSide B`, and every DoStep pushed from that branch must too — a stale `isSide A` guard silently no-ops"
---

`isSide side attrs aid = aid == attrs.id && onSide side attrs` (Act/Types.hs) reads the act's
**current** side, not anything carried in the message. `AdvanceAct` therefore reaches a card twice:

1. **Side A dispatch.** The card's own `runMessage` sees it first. If no branch matches, it falls
   through to `liftRunMessage` → `ActAttrs`'s runner (Act/Runner.hs ~80), which is guarded on
   `onFrontSide`. That flips the act to B **and** pushes `advanceActSideA`'s messages: the
   `ActAdvance` when-window, a `chooseOne` re-pushing `AdvanceAct`, and the after-window.
2. **Side B dispatch.** The re-pushed `AdvanceAct` arrives with the act now on B, which is where
   the card is meant to do its work.

So `AdvanceAct (isSide B attrs -> True)` is the norm — 393 acts use it, 4 use `isSide A`.

**The trap:** a branch matching on the side-A dispatch that ends in `pure a` never calls
`liftRunMessage`, so the runner never runs — the act never flips to its B side and the
`ActAdvance` windows never fire. It still "works" if the branch calls `advanceActDeck` itself,
which is why side-A acts can sit around looking fine.

**The regression shape:** converting an act from the A pattern to the B pattern means converting
*every* handler in the chain, including the `DoStep n (AdvanceAct (isSide _ attrs -> True) _ _)`
steps the advance branch pushes — those run while the act is still on B. Miss one and it silently
never matches; there is no error, the effect just never happens.

Real bug: The Sheldon Gang (One Last Job) was migrated A→B in `d035e97f09`, but
`DoStep 2 (AdvanceAct (isSide A attrs -> True) _ _)` was left behind, so
`selectEach EnemyWithAnyClues disengageFromAll` never ran and enemies holding clues stayed
engaged for the rest of the scenario. Its sibling The O'Bannion Gang is still entirely on the
A pattern (self-consistent, so it runs — but skips the flip and the advance windows).

**How to apply:** when touching an act's advance path, grep the file for `isSide` and check every
occurrence agrees; a lone `isSide A` next to `isSide B` siblings is a bug, not a style choice.
Related: [[project_must_advance_objective_forced]].
