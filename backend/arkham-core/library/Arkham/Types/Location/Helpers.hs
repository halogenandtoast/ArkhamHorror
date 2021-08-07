module Arkham.Types.Location.Helpers
  ( module X
  , module Arkham.Types.Location.Helpers
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes.Entity
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers as X
import Arkham.Types.InvestigatorId
import Arkham.Types.Message

resignAction :: SourceEntity a => a -> Ability
resignAction a =
  mkAbility a 99 $ ActionAbility (Just Action.Resign) (ActionCost 1)

drawCardUnderneathAction :: SourceEntity a => a -> Ability
drawCardUnderneathAction a =
  (mkAbility a 98 $ FastAbility Free) { abilityLimit = GroupLimit PerGame 1 }
