# `SetFloodLevel` clamped the level, then deferred the *unclamped* message

`Arkham.Location.Runner`'s `SetFloodLevel` handler computes a `maxFloodLevel` from
`CannotBeFlooded` / `CannotBeFullyFlooded`, uses it for the change check and the
`#when FloodLevelChanged` window — and then re-pushed `Do msg`, i.e. the original
message with the **unclamped** level. `Do (SetFloodLevel lid level)` writes
`floodLevelL ?~ level` verbatim, so the clamp was purely decorative on this path.

Symptom: The Water Rises (07043b, "each time a location is revealed, it becomes
fully flooded") fully flooded **Underground River** (07104), whose printed text is
"Underground River cannot be fully flooded." Same for Lighthouse Stairwell,
Falcon Point Cliffside/Gatehouse, the Ruined Arkham locations, etc.

`IncreaseFloodLevel` never showed the bug because it clamps first and then calls
`liftRunMessage (SetFloodLevel lid newFloodLevel)` with an already-legal value —
so only *direct* `SetFloodLevel` callers (`setThisFloodLevel`,
`increaseThisFloodLevelTo`, agenda/effect pushes) were affected.

Fix: defer the clamped value.

```haskell
pushAll [before, Do (SetFloodLevel lid newFloodLevel)]   -- not `Do msg`
```

General rule: when a handler validates/clamps a value and then defers the work to
a `Do` variant, the deferred message must carry the *derived* value. `Do msg`
re-sends the caller's original arguments and silently discards everything the
outer handler computed. See also [[project_window_key_vs_payload_guard]].
