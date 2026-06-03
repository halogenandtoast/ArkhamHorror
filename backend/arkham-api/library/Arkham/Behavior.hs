{- | Cross-cutting behavior helpers.

Each sub-module captures the reusable handler logic for one capability —
fightable, evadable, investigatable, defeatable, damageable — so that any
entity runner that wants to expose that capability can route through the same
helper instead of re-implementing the message flow.

Today these are plain functions: an entity runner pattern-matches the
behavior's incoming message and calls into the helper. The intent is that as
more hosts adopt a given behavior (3 + hosts is the natural threshold), these
helpers become the bodies of a future @class Behavior b@ typeclass, with each
entity attrs type providing a @HasBehavior b@ instance describing how the
behavior reads / writes state on that entity.

Currently consumed by:

  * "Arkham.Enemy.Runner"        — Fight, Evade, Heal
  * "Arkham.EnemyLocation.Runner" — Fight, Evade, Investigate, Defeat, Damage
  * "Arkham.Location.Runner"     — Investigate
  * "Arkham.Asset.Runner"        — Heal
-}
module Arkham.Behavior (
  module X,
) where

import Arkham.Behavior.Damage as X
import Arkham.Behavior.Defeat as X
import Arkham.Behavior.Evade as X
import Arkham.Behavior.Fight as X
import Arkham.Behavior.Heal as X
import Arkham.Behavior.Investigate as X
