---
title: project-map-pan-click-exclusion
description: Map pan handler used to eat clicks on board entities via eager setPointerCapture; now deferred
---

Scenario.vue `.location-cards-scroller` has `@pointerdown="onStagePointerDown"` — a click-drag pan feature. It used to call `scroller.setPointerCapture` on pointerdown, which retargets the browser-synthesized click to the scroller and swallows it. Any board entity that doesn't handle its own pointerdown (concealed cards `.concealed-card`, keys `.key`, investigator portraits `.portrait`) became unclickable whenever the map was scrollable (`scrollWidth>clientWidth || scrollHeight>clientHeight`).

**Fix (issue #4994, root cause):** defer `setPointerCapture` until a real drag crosses `DRAG_THRESHOLD_PX` inside `onStagePointerMove`. A stationary click is never captured → reaches whatever it lands on; drags still capture+pan. This removed the need to allowlist every clickable class (the exclusion list in `onStagePointerDown` remains only to skip pan-tracking on `.card`/`.enemy`/etc).

**How to apply / test:** synthetic `dispatchEvent('click')` does NOT reproduce pointer-capture swallowing — only a real trusted click (chrome-devtools click on a snapshot uid; tag the element with `role="button"` first since map imgs aren't in the a11y tree). Verify pan still works by dragging an empty map cell (stub `setPointerCapture` to no-op in the test since a synthetic pointerId can't be captured). Separately, the skill-test `Draggable` can physically cover a board card — it has an `avoid-selector` like ChoiceModal.
