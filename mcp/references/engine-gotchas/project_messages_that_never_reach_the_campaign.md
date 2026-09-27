---
name: project_messages_that_never_reach_the_campaign
description: "EnterLocation and a key's own shift never dispatch as top-level messages, so campaign-level detection cannot see them"
metadata: 
  node_type: memory
  type: project
  originSessionId: 54a7f2c8-fc7d-428c-8c39-574a154b22ca
  modified: 2026-08-08T11:50:23.482Z
---

Two messages that look dispatchable but never reach the campaign entity (and so are
invisible to achievement detection, which hangs off the campaign's runMessage):

- **`EnterLocation iid lid`** — `handleDoResolveMovement`
  (`Investigator/Runner/Movement.hs`) pushes it *nested* inside
  `Simultaneously $ Run [Do (WhenWillEnterLocation ...), EnterLocation ...]`. Use
  **`After (MoveTo movement)`** instead and read `movement.destination`
  (`ToLocation lid`); it is pushed at top level in the same batch and also covers
  the scenario's own `startAt` placement (`means = Place` goes through the same
  path).
- **A Scarlet Key's own fast shift ability** — each key does
  `UseThisAbility … -> liftRunMessage (CampaignSpecific "shift[<code>]" …)`, i.e.
  runs the message **on itself** rather than pushing it. Cards/enemies that call
  `Campaigns.TheScarletKeys.Helpers.shift` DO push it. Both converge on
  `shiftKey` (`Key/Import/Lifted.hs`), which brackets the shift with a
  `CampaignEvent "shiftKey"` window carrying the `ScarletKeyId` — key on the
  **After** half of that window (the When half fires too).

**Why:** both shipped as broken achievements. Specs that pushed the synthetic
message (`run $ EnterLocation …`, `run $ CampaignSpecific "shift[…]"`) passed
while the real game did nothing.

**How to apply:** when detecting a player action from the campaign, drive the REAL
flow in the spec — `moveTo`, `UseCardAbility` on the actual entity — never a
hand-built inner message. If the real-flow spec is hard to write, that is the
signal the hook is wrong. Related: [[project_scarlet_keys_achievements]],
[[feedback_derive_card_pools_mechanically]].

Also: `Flip` on a Scarlet Key sets `shifted = True`, and a key's `getAbilities`
returns `[]` while shifted — flipping a key removes its own ability until EndRound.
