---
title: project_hallowed_mirror_errata_bonded
description: "Hallowed Mirror / Occult Lexicon / Miss Doyle v2.0 errata — leave-play sets bonded copies aside (placeInBonded), never removes from game"
---

v2.0 errata (`.claude/references/faq/card_errata.md:207`) re-words Hallowed Mirror (05313), Occult Lexicon (05316), Miss Doyle (30): their Forced leave-play line is now "set them aside, out of play" instead of "remove them from the game." So the bonded copies (Soothing Melody / Blood Rite / cats) always return to the bonded pool via `placeInBonded iid` on `RemovedFromPlay` — for EVERY leave-play (hollow, discard, destroy), no carve-out. searchBonded re-finds them, so a re-played card works again.

Pattern (identical across all three): `RemovedFromPlay (isSource attrs -> True) -> for_ attrs.owner \iid -> do { cs <- select $ basic $ CardOwnedBy iid <> cardIs Events.<bonded>; for_ cs $ placeInBonded iid }`.

Issue #4968: Hallowed Mirror was the lone straggler still calling `RemoveAllCopiesOfCardFromGame`; the Scarlet Keys hollow mechanic (set-aside via `obtainCard` → asset `RemoveFromPlay`, `Asset/Runner.hs:713`) exposed it. Fix = align with siblings, no hollow-detection needed. Errata is authoritative (chapter-one card: Local FAQ → FAQ → Rules → Grimoire). Level-3 Hallowed Mirror has no removal handler — leave it.
