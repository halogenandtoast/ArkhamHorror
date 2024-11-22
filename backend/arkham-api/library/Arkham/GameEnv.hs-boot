module Arkham.GameEnv where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.ActiveCost.Base
import {-# SOURCE #-} Arkham.Card (Card, CardCode, CardId)
import Arkham.Card.CardDef
import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Distance
import {-# SOURCE #-} Arkham.Game
import Arkham.Game.Settings
import Arkham.GameT
import Arkham.History
import Arkham.Id
import {-# SOURCE #-} Arkham.Message
import Arkham.Modifier
import Arkham.Phase
import Arkham.Random
import Arkham.SkillTest.Base
import Arkham.Target
import Arkham.Window
import Control.Monad.Random

withActiveInvestigator
  :: HasGame m => InvestigatorId -> (forall t. (MonadTrans t, HasGame (t m)) => t m a) -> m a
withModifiers'
  :: (Targetable target, HasGame m)
  => target
  -> m [Modifier]
  -> (forall t. (MonadTrans t, HasGame (t m)) => t m a)
  -> m a
getAllModifiers :: HasGame m => m (Map Target [Modifier])
getActiveAbilities :: HasGame m => m [Ability]
getPhase :: HasGame m => m Phase
getCurrentBatchId :: HasGame m => m (Maybe BatchId)
getWindowDepth :: HasGame m => m Int
getDepthLock :: HasGame m => m Int
getSkillTest :: HasGame m => m (Maybe SkillTest)
getActiveCosts :: HasGame m => m [ActiveCost]
getDistance :: HasGame m => LocationId -> LocationId -> m (Maybe Distance)
getAllAbilities :: HasGame m => m [Ability]
getWindowStack :: HasGame m => m [[Window]]
getActionCanBeUndone :: HasGame m => m Bool
getGameInAction :: HasGame m => m Bool
getIgnoreCanModifiers :: HasGame m => m Bool
getInSetup :: HasGame m => m Bool
getHistory :: HasGame m => HistoryType -> InvestigatorId -> m History
getHistoryField :: HasGame m => HistoryType -> InvestigatorId -> HistoryField k -> m k
getJustSkillTest :: (HasGame m, HasCallStack) => m SkillTest
getCard :: (HasCallStack, HasGame m) => CardId -> m Card
findCard :: HasGame m => (Card -> Bool) -> m (Maybe Card)
getSettings :: HasGame m => m Settings
getAllPlayers :: HasGame m => m [PlayerId]
getActivePlayer :: HasGame m => m PlayerId
getCardUses :: HasGame m => CardCode -> m Int
getAllCardUses :: HasGame m => m [CardDef]
runWithEnv
  :: ( HasGameRef env
     , HasQueue Message m
     , HasStdGen env
     , HasGameLogger m
     , MonadReader env m
     )
  => GameT a
  -> m a
