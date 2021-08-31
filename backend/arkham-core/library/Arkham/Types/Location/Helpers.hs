module Arkham.Types.Location.Helpers
  ( module X
  , module Arkham.Types.Location.Helpers
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes.Entity
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers as X
import Arkham.Types.Matcher

resignAction :: SourceEntity a => a -> Ability
resignAction a =
  mkAbility a 99 $ ActionAbility (Just Action.Resign) (ActionCost 1)

drawCardUnderneathAction :: SourceEntity a => a -> Ability
drawCardUnderneathAction a =
  (restrictedAbility
        a
        98
        (Here <> LocationExists (YourLocation <> LocationWithoutClues))
    $ FastAbility Free
    )
    { abilityLimit = GroupLimit PerGame 1
    }
