module Arkham.Types.Location.Helpers
  ( module X
  , module Arkham.Types.Location.Helpers
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes.Entity
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers as X
import Arkham.Types.InvestigatorId
import Arkham.Types.Message

resignAction :: SourceEntity a => InvestigatorId -> a -> Message
resignAction iid a = UseAbility
  iid
  (mkAbility (toSource a) 99 (ActionAbility (Just Action.Resign) (ActionCost 1))
  )

drawCardUnderneathAction :: SourceEntity a => InvestigatorId -> a -> Message
drawCardUnderneathAction iid a = UseAbility
  iid
  ((mkAbility (toSource a) 100 (FastAbility Free))
    { abilityLimit = GroupLimit PerGame 1
    }
  )
