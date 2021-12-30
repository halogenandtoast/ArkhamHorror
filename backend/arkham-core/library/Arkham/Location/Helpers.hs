module Arkham.Location.Helpers
  ( module X
  , module Arkham.Location.Helpers
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes.Entity
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers as X
import Arkham.Matcher

resignAction :: SourceEntity a => a -> Ability
resignAction a =
  mkAbility a 99 $ ActionAbility (Just Action.Resign) (ActionCost 1)

drawCardUnderneathAction :: SourceEntity a => a -> Ability
drawCardUnderneathAction a =
  (restrictedAbility
        a
        100
        (Here <> LocationExists (YourLocation <> LocationWithoutClues))
    $ FastAbility Free
    )
    { abilityLimit = GroupLimit PerGame 1
    }
