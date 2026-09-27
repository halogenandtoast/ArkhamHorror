---
name: project_revelation_player_enemy_silent_draw
description: Revelation player enemies bypass DrewPlayerEnemy, so they drew with no card popup until the CardIdSource PlayerEnemyType branch learned to send one
metadata:
  type: project
---

`handleInvestigatorDrewPlayerCardFrom` (`Arkham/Investigator/Runner/Card.hs`) forks a drawn
player card on `hasRevelation`:

- **no revelation** → `DrewPlayerEnemy iid card`
- **has revelation** → `Revelation iid (CardIdSource card.id)`

`DrewPlayerEnemy` (`Arkham/Game/Runner.hs`) was the **only** place that emitted the
"<investigator> drew <card>" client popup (`sendEnemy` / `sendEnemyOnly` for Peril). The
`Revelation iid (CardIdSource cid)` handler sends a display for its `AssetType` and
`EventType` branches (`sendRevelation`) but its `PlayerEnemyType` branch sent nothing — it
only pushed `SetBearer` / `RemoveCardFromHand` / `InvestigatorDrawEnemy`.

Net effect: **every player-enemy weakness with `cdRevelation = IsRevelation` landed in the
threat area silently** — Serpents of Yig (`04014`), Serpents of Yig Advanced (`90083`),
Watcher from Another Dimension (`06017`), Unbound Beast (`06283`). Fixed in #5304 by sending
the same peril-aware notification from the `PlayerEnemyType` branch.

Deliberately *not* mirrored into the sibling `EnemyType` branch: encounter enemies already
get their popup from `InvestigatorDrewEncounterCardFrom`, so adding one there double-displays.

**How to apply:** when a card visibly "does nothing" on draw, check which of the two draw
forks it takes. Revelation and non-revelation draws run through disjoint handlers, and UI
notifications are duplicated between them rather than shared — a notification added to one
side is silently missing from the other. Related: [[project_wrapper_ability_double_accounting]].

## Client messages are invisible to arkham-replay by default

`ClientMessage` never touches `Game` state, so a dropped popup produces a **byte-identical**
`--output` before and after the fix. `app-replay/Main.hs` used to install `const (pure ())`
as `appLogger`; it now prints `client> <kind> <payload>` to stderr under `--trace`
(`formatClientMessage`). Grep the trace for `^client>` to assert on UI notifications
headlessly. Verification for #5304:

```
> Revelation "90081" (CardIdSource bfde7141-…)
client> card $drew … name=s:"Serpents of Yig" …
> SetBearer (EnemyTarget 08e2ee83-…) "90081"
```

with `DrewPlayerEnemy` appearing **zero** times in the whole trace — proof the old code had
no other sender.
