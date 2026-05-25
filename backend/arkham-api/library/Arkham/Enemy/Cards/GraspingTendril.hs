module Arkham.Enemy.Cards.GraspingTendril (graspingTendril) where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (dayNumber, getCampaignDay)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype GraspingTendril = GraspingTendril EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

graspingTendril :: EnemyCard GraspingTendril
graspingTendril =
  enemy GraspingTendril Cards.graspingTendril (2, Static 0, 4) (1, 0)
    & setSpawnAt (LocationWithEnemy $ enemyIs Cards.chelydranHybrid)

instance HasModifiersFor GraspingTendril where
  getModifiersFor (GraspingTendril a) = do
    day <- getCampaignDay
    modifySelf a [HealthModifier $ dayNumber day]
