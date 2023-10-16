module Arkham.GameEnv where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.ActiveCost.Base
import {-# SOURCE #-} Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Distance
import Arkham.Game.Settings
import Arkham.History
import Arkham.Id
import Arkham.Modifier
import Arkham.Phase
import Arkham.SkillTest.Base
import Arkham.Target

withModifiers'
  :: (Targetable target, HasGame m) => target -> [Modifier] -> (forall n. HasGame n => n a) -> m a
getAllModifiers :: HasGame m => m (Map Target [Modifier])
getActiveAbilities :: HasGame m => m [Ability]
getPhase :: HasGame m => m Phase
getWindowDepth :: HasGame m => m Int
getDepthLock :: HasGame m => m Int
getSkillTest :: HasGame m => m (Maybe SkillTest)
getActiveCosts :: HasGame m => m [ActiveCost]
getDistance :: HasGame m => LocationId -> LocationId -> m (Maybe Distance)
getAllAbilities :: HasGame m => m [Ability]
getActionCanBeUndone :: HasGame m => m Bool
getGameInAction :: HasGame m => m Bool
getIgnoreCanModifiers :: HasGame m => m Bool
getHistory :: HasGame m => HistoryType -> InvestigatorId -> m History
getJustSkillTest :: (HasGame m, HasCallStack) => m SkillTest
getCard :: HasGame m => CardId -> m Card
findCard :: HasGame m => (Card -> Bool) -> m (Maybe Card)
getSettings :: HasGame m => m Settings
getAllPlayers :: HasGame m => m [PlayerId]
getActivePlayer :: HasGame m => m PlayerId
getCardUses :: HasGame m => CardCode -> m Int
