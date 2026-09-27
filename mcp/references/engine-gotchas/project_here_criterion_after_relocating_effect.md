# `Here` is evaluated when the window fires, not when the action was taken

A `Here` criterion on a reaction/forced ability asks "is the investigator at this
location **now**". If the effect that fired the window can move them, "now" is
the destination, not the origin — so the ability triggers on the wrong location.

Dark Matter's scan is the worked example. `runPendingScan` pushes the scanned
card's `DrewCards` *before* `checkScanWindows`, so a scanned **location** is put
into play and the investigator moves onto it, and only then do the `scan` windows
resolve. Ice Spires and Ship's Bridge both print "After you scan at this
location" and both used

```haskell
restricted a 1 Here $ forced $ CampaignEvent #after (Just You) scanEvent
```

Scanning at Q-Crystal Mines and drawing Ice Spires therefore triggered *Ice
Spires*, the location the scan produced.

## The fix

Capture the location at the time of the action and put it in the window key, not
in a criterion:

- `ScanResult` gained `scannedAt :: Maybe LocationId`, read via `getMaybeLocation`
  **before** `drawScannedCard`.
- `scanEventAt :: LocationId -> Text` adds `scan[at:<uuid>]` to the key family.
- The two locations match `scanEventAt a.id` and drop `Here` entirely.

`Maybe` matters for more than "an investigator can be nowhere": window payloads
are persisted in `gameQuestion` and the window stack, and aeson's generic decoder
tolerates an omitted `Maybe` field, so in-flight games still parse.

## Generalizing

Before writing `Here` (or any `You`-relative criterion) on a window, ask whether
anything between the action and the window can move the investigator — scanned
locations, `MoveTo` riders, teleports. If so, the location is part of *what
happened* and belongs in the event, not in a criterion re-evaluated later.

Related: [[project_window_key_vs_payload_guard]],
[[project_yourlocation_window_handler_fanout]].
