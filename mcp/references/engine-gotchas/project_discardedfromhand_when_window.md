---
name: project_discardedfromhand_when_window
description: "handleDiscardCard now fires a #when DiscardedFromHand window (before Do msg) as well as #after; needed for InYourHand-restricted reactions like Worry Rock"
metadata: 
  node_type: memory
  type: project
  originSessionId: 811cdb6f-e350-4a9f-86e3-0abfd7d216ae
  modified: 2026-07-22T03:13:01.401Z
---

`handleDiscardCard` (Arkham/Investigator/Runner/Card.hs) historically emitted only the `#after` `DiscardedFromHand` window. Worry Rock — Token of Safety (10713/c10713) triggers on `DiscardedFromHand #when` with `restricted … InYourHand` and only an `InHandEffect` out-of-play entity, so it never fired (issue #5224: "encounter card discarded it without triggering").

**Fix (chosen over changing the card to #after/InYourDiscard):** add `beforeHandWindowMsg <- checkWindows [mkWhen (Window.DiscardedFromHand iid source card)]` and push it **before `Do msg`** — `pushAll [beforeWindowMsg, beforeHandWindowMsg, Do msg, afterWindowMsg, afterHandWindowMsg]`. Must be before `Do msg` because after the discard resolves the card leaves hand and its `InHandEffect` entity is gone, so an `InYourHand` reaction can't even be collected.

Worry Rock is the only `#when DiscardedFromHand` listener, so the new window prompts nothing else. `SourceIsScenarioCardEffect` matches Treachery/Enemy/Location/Act/Agenda/Story/ChaosToken sources (not bare ScenarioSource). Verified via arkham-replay `--undo 3` + Raw `DiscardCard` from a LocationSource: window now offers "Draw 3 cards", hand 4→6, card ends in discard. Contrast the `#after`+`InYourDiscard` pattern used by Moonstone/Little Sylvie (which add `InDiscardEffect`).
