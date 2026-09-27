---
name: project_move_restriction_must_be_cannotenter
description: "'You must X in order to move here' must be a CannotEnter modifier, not criteria on the location's own Move ability — else Safeguard/Elusive/Shortcut bypass it (#5398)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 16a0de39-9b4b-46c1-8aa5-9e6b33d56bac
  modified: 2026-08-15T04:42:14.379Z
---

A location that reads *"You must have at least 2 clues in order to move to The Edge of the
Universe"* (02321) restricts **every** move to it. Encoding it as extra `abilityCriteriaL`
on the location's own `AbilityMove` covers one path and leaves the rest open.

#5398: `TheEdgeOfTheUniverse.hs` patched only its own move ability (with a standing
`-- TODO: This should be some sort of restriction encoded in attrs`), so **Safeguard**
(60110) let a 0-clue Zoey follow a 3-clue Monterey Jack in. Same hole for Elusive,
Shortcut, Pathfinder, Ethereal Slip, Ellsworth's Boots, Dakota Garofalo.

Correct encoding:

```haskell
modifySelect a (InvestigatorWithClues $ lessThan 2) [CannotEnter a.id]
```

**Why:** `getCanMoveToLocations` (`Helpers/Location.hs`) opens with
`Matcher.canEnterLocation iid`, and `CanEnterLocation` (`Game.hs`) reads `_CannotEnter` off
the *investigator's* modifiers — so every move query, including the `CanMoveToLocation` /
`CanEnterLocation` criteria that follow-me cards gate themselves on, honors it. The base
move action already carries `noModifier (CannotEnter l.id)`, so the hand-rolled criteria is
pure loss.

**How to apply:** never express a move-to restriction as ability criteria. `CannotEnter` is
consulted only on *move* queries, so placement/setup/enemy spawns stay unaffected (matches
RAW). Applied to investigators it never touches enemy movement — enemies read `CannotEnter`
off themselves, see [[project_cannotenter_enemy_side_ban]]. When deleting such a
`HasAbilities` override, move `HasAbilities` into `deriving newtype (...)` — the class
default is `const []` and would strip the location's move/investigate abilities.

Precedent: `Scenarios/TheWesternWall/Helpers.hs`, `SunkenGrottoUpperDepths.hs`, `Vault.hs`,
`InnerSanctum.hs`, `DreamGate*.hs`.

**"…except through <card>" needs the source-aware sibling.** `CannotEnter` is sourceless, so
it also blocks the very ability the card carves out. Use
`CannotEnterExcept LocationId SourceMatcher` (added 2026-08-15 for Dark Matter's A Shimmer in
the Wall / Entrance Hall, whose unrevealed side reads *"You cannot enter A Shimmer in the Wall
except through the [fast] ability on Maja"*):

```haskell
whenUnrevealed a
  $ modifySelect a Anyone [CannotEnterExcept a.id (SourceIsAsset $ assetIs Assets.maja)]
```

It is honored **only** in `getCanMoveToLocations_` (`Helpers/Location.hs`) — the one move
query that receives the moving effect's `source` — alongside the entry-cost check. That is
enough: the basic move action reaches it through its `CanMoveTo` criterion, whose source is
the destination location itself, so the action disappears; follow-me cards reach it through
`getCanMoveTo`. The sourceless `noModifier (CannotEnter l.id)` guards do *not* see it, by
design. Known gap: a Safeguard follower inherits the leader's `moveSource`, so if the leader
came in via the excepted ability the follower rides along without paying its cost.

Related: [[project_cannotenter_enemy_side_ban]], [[project_arkham_replay_tool]]
