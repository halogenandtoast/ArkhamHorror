module Arkham.Location.Helpers
  ( module X
  , module Arkham.Location.Helpers
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes.Entity
import Arkham.Direction
import Arkham.Game.Helpers as X
import Arkham.Matcher

resignAction :: Sourceable a => a -> Ability
resignAction a =
  mkAbility a 99 $ ActionAbility (Just Action.Resign) (ActionCost 1)

drawCardUnderneathAction :: Sourceable a => a -> Ability
drawCardUnderneathAction a =
  (restrictedAbility
        a
        100
        (Here <> LocationExists (YourLocation <> LocationWithoutClues))
    $ FastAbility Free
    )
    { abilityLimit = GroupLimit PerGame 1
    }

adjacentLocations :: HashSet Direction
adjacentLocations = setFromList [minBound .. maxBound]
