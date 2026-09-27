---
name: project_window_key_vs_payload_guard
description: A reaction whose real condition lives only in the window payload is still offered after every event; encode the condition in the window KEY (bracketed-key family) instead of guarding in the handler
metadata:
  type: project
---

When a homebrew/custom event is announced as one broad window key
(`Window.CampaignEvent "scan" iid payload`), it is tempting to let every
interested card match that one key and then discriminate inside the handler:

```haskell
getAbilities a = [restricted a 1 ControlsThis $ freeReaction (CampaignEvent #after Nothing scanEvent)]

runMessage (UseCardAbility _ _ 1 ws _) = do
  let r = getScanResult ws
  unless (scanSuccessful r) do ...   -- the REAL condition
```

**This does not work as a condition.** Ability availability is decided by the
window matcher alone — the handler runs far too late:

- a `freeReaction` is **offered to the player** after every occurrence of the
  broad event, and choosing it does nothing;
- a `forced` ability **enters the trigger-ordering prompt** every time, so the
  player is asked to order triggers that will all no-op;
- a `SilentForcedAbility` still queues a `UseCardAbility` per occurrence.

The fix is the bracketed-key convention Scarlet Keys uses for concealed cards
(`noConcealed[<kind>]`): fire a **family** of keys for one event — the general
key plus one narrower key per property a card might care about — and let each
card match the narrowest key matching its printed text. All keys carry the same
payload, so the handler can still read details it needs (who, which card).

Dark Matter's `checkScanWindows` (`Homebrew/DarkMatter/Helpers.hs`) is the
worked example: `scan`, `scan[<Icon>]`, `scan[successful]` /
`scan[unsuccessful]`, `scan[<card code>]`, `scan[AssetType]`. Six cards were
mis-triggering on the broad key before it existed.

Two details worth copying:

- Fire the whole family in **one** `checkWindows [Window]` call rather than N
  `checkAfter` calls, so reactions to the same event are simultaneous and the
  player orders them, instead of the narrow keys resolving strictly after the
  general one.
- Keep one shared payload reader (`getScanResult`) that accepts any key in the
  family (`key == base || (base <> "[") isPrefixOf key`), rather than a copy per
  card hardcoded to the base key — otherwise a card that moves to a narrow key
  silently stops finding its own payload.

A second worked example, Dark Matter's Quantum Collapse (`drewFacedown`): its
`Forced` reads "After you draw **Quantum Collapse** from your threat area", i.e.
a condition on *which entity* was drawn, so the narrow key is per-entity —
`drewFacedown[<uuid>]`, built in `getAbilities` from `a.id`. A per-card-code key
would not have been enough, since every face-down copy shares a card code.

Two extra traps that version hit:

- The base key's payload is an id of **whatever kind was drawn** — treachery,
  enemy or asset. The old handler did `toResult v :: TreacheryId` unconditionally;
  because every id is a bare UUID this never crashed, it just silently never
  matched after an enemy draw.
- The no-op forced abilities were anchored to cards in `FacedownInThreatArea`,
  which `Player.vue` renders as a plain `<img>` with no click handler — so the
  mandatory prompt had **no clickable choice at all** and the game looked frozen.
  See [[project_unrenderable_ability_anchor_freezes_ui]].

Related: [[project_yourlocation_window_handler_fanout]],
[[project_during_your_action_window]].
