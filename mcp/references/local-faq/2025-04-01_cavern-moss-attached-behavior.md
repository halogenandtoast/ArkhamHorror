---
title: Cavern Moss — behavior while attached to an asset
date_added: 2026-05-25
source: Designer ruling, April 2025
affects:
  - Cavern Moss
---

# Cavern Moss — behavior while attached to an asset

**Q: How does Cavern Moss behave when attached to an asset?**

A: While Cavern Moss is attached to an asset, it is not considered engaged with you, and it does not make attacks of opportunity nor attacks during the enemy phase. You cannot evade it if it's attached.

**Q: When Cavern Moss is attached, can it still trigger its Forced ability? Can I target it with abilities?**

A: 1) Yes, Cavern Moss's Forced ability still triggers while it's attached to an Item asset. If there is another Item asset under your control besides the one it's attached to, it will attach to that other asset. 2) Yes, you can target Cavern Moss with abilities that target enemies at your location. (Note that while attached, it is not considered engaged with you unless you're attacking it; abilities that don't require the enemy to be engaged can still target it.)

## Affected cards / systems

- Cavern Moss (10585) — `backend/arkham-api/library/Arkham/Enemy/Cards/CavernMoss.hs`

## Implementation status

- **Cavern Moss (10585)**: ✏️ updated. `HasModifiersFor` now applies `CannotBeEvaded`, `CannotAttack`, `CannotMakeAttacksOfOpportunity`, `CannotBeEngaged`, and `RemoveKeyword Aloof` when attached; removed incorrect `AsIfEngagedWith` modifier. (`backend/arkham-api/library/Arkham/Enemy/Cards/CavernMoss.hs`)
