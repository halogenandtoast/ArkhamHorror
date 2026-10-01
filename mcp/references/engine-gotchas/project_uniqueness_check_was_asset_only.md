---
name: project_uniqueness_check_was_asset_only
description: "The playability uniqueness check matched on (cdUnique, AssetType), so unique EVENTS that stay in play (The Raven Quill, Shrine of the Moirai) let both copies be played at once"
metadata:
  type: project
---

`getPlayabilityChecksWithResources` (`Arkham/Helpers/Playable.hs` ~328) gated uniqueness on the
card type:

```haskell
uniquenessOk <- case (cdUnique pcDef, cdCardType pcDef) of
  (True, AssetType) -> not <$> selectAny (InPlayAsset $ AssetWithTitle title)   -- or full title
  _ -> pure True
```

Events fell into `_`, but the Rules Reference applies uniqueness to *putting a unique card into
play*, not to assets specifically. Two player events stay in play and so need the check:

- **The Raven Quill** (09042) — attaches to an asset; both copies were playable simultaneously (#5796)
- **Shrine of the Moirai** (07310, level 3) — attaches to a location

The check is safe to make generic because `getEventsMatching` (`Game.hs` ~3574) only walks
`entitiesL . eventsL`, filtered for `placement.outOfGame` — i.e. **in-play** events. A one-shot
unique event that is discarded on resolution leaves nothing behind, so it stays playable.

**How to apply:** add an `(True, EventType)` arm mirroring the asset arm with
`EventWithTitle` / `EventWithFullTitle`. `CardWithoutUniqueCopyInPlay` (`Game.hs` ~5993) keeps
its `cdCardType def == AssetType` guard on purpose — its only users (A Chance Encounter and
friends) put **Ally assets** into play.

Verify with the #5796 export: at `--undo 105` both `c09042` card ids appear as
`TargetLabel`/`CardIdTarget` choices in Monterey's window; play one and the other must be gone
from the next window (watch resources too — the quill costs 3, so unaffordable cards drop out
for an unrelated reason).

Related: [[project_assetslots_ignored_card_target_suppression]].
