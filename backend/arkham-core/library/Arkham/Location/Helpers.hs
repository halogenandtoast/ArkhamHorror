module Arkham.Location.Helpers (
  module X,
  module Arkham.Location.Helpers,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Direction
import Arkham.Game.Helpers as X
import Arkham.Matcher
import Arkham.Source

resignAction :: Sourceable a => a -> Ability
resignAction a =
  mkAbility a 99 $ ActionAbility [Action.Resign] (ActionCost 1)

drawCardUnderneathAction :: Sourceable a => a -> Ability
drawCardUnderneathAction a =
  limitedAbility (GroupLimit PerGame 1)
    $ restrictedAbility
      a
      100
      (Here <> LocationExists (YourLocation <> LocationWithoutClues))
    $ FastAbility Free

adjacentLocations :: Set Direction
adjacentLocations = setFromList [minBound .. maxBound]
