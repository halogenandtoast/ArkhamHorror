---
name: project_asset_addtovictory_keeps_tokens
description: Assets added to the victory display kept every token (doom included) and still counted toward the agenda; enemies already cleared theirs (#5680)
metadata:
  type: project
---

`AddToVictory _ (AssetTarget aid)` in `Arkham/Asset/Runner.hs` only set
`placementL .~ OutOfPlay VictoryDisplayZone` and `controllerL .~ Nothing` — the entity stayed
in `entitiesL.assetsL` with its tokens intact. `OutOfPlay VictoryDisplayZone` is **not**
`Placement.outOfGame`, so `getAssetsMatching'` still returns it, and `getDoomCount`
(`Helpers/Doom.hs`) aggregates `AssetDoom` over `AssetWithoutModifier DoomSubtracts` with **no
in-play filter** (the enemy line right below it uses `InPlayEnemy`). A defeated Key Locus
(`c09653`, Victory 1) in Dogs of War kept its doom and kept pushing the agenda.

Fixed by mirroring `Enemy/Runner.hs`'s `Do (AddToVictory …)`, which already does
`tokensL .~ mempty` (and `keysL .~ mempty`).

**Why:** assets are the only victory path that keeps the entity alive — location goes through
`RemoveLocation`, treachery through `RemoveTreachery`, skill/event/act/story are deleted from
their maps. So the "tokens are removed when a card leaves play" rule had to be applied by hand.

**How to apply:** when a token-bearing entity can be added to the victory display, check the
runner clears `tokensL`. And remember `getDoomCount`'s asset aggregation has no in-play filter
at all — any out-of-play asset zone that retains doom will leak into the agenda total.
See [[project_resetgame_wipes_assets_but_not_slots]] and
[[project_addtovictory_leaveplay_window_ordering]].
