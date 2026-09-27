---
title: project_test_stale_modifiers_first_message
description: Entities built in a test body have no modifiers until the next message — runMessages preloads AFTER runMessage, so the first message reads a stale gameModifiers
---

`gameTest` / `gameTestWith` / `scenarioTest*` call `overGameM preloadModifiers` **once, before the body runs** (`tests/TestImport.hs` ~822/835/860). Every `test*` builder (`testLocationWithDef`, `testEnemy`, `testAsset`, …) then inserts its entity with a raw `overGame`, which does not preload.

`Game.runMessages` preloads in the wrong order for this. Per message it does `preloadEntities` → `runPreGameMessage` → **`runMessage msg >=> preloadModifiers`** (`library/Arkham/Game.hs` ~6738-6741). The preload is a *post*-pass, so the first message after the body creates an entity sees a `gameModifiers` map that predates that entity. Any handler doing `mods <- getModifiers a` reads `[]`.

Symptom: a `HasModifiersFor` / `modifySelf` modifier "does nothing" on the first message but works on every later one — which reads as an engine bug rather than a harness artifact.

Concrete case: `UndergroundRiverSpec` builds the river with `testLocationWithDef` and immediately runs `SetFloodLevel river FullyFlooded`. The `CannotBeFullyFlooded` clamp in `Location/Runner.hs` (~491) saw no modifiers, so the level wrote through as `FullyFlooded` — exactly the defect the spec was written to guard against.

**How to apply:** after building an entity whose own modifiers matter to the very next message, `tick` (= `run Noop`; `Noop` is not in `shouldPreloadModifiers`' False list, so it triggers a preload). Fold it into a local builder so no example can forget it:

```haskell
undergroundRiver :: TestAppT Location
undergroundRiver = testLocationWithDef Locations.undergroundRiver (revealedL .~ True) <* tick
```

Same reason `TheBlackCat5Spec`'s `asScenario` ticks after its `overTest`. Real games never hit this — some earlier message always preloaded.

Related: [project_test_addinvestigator_playerorder](project_test_addinvestigator_playerorder.md), [project_test_skip_stale_question](project_test_skip_stale_question.md).
