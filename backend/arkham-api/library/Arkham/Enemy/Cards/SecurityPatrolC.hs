module Arkham.Enemy.Cards.SecurityPatrolC (securityPatrolC) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype SecurityPatrolC = SecurityPatrolC EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityPatrolC :: EnemyCard SecurityPatrolC
securityPatrolC =
  enemy SecurityPatrolC Cards.securityPatrolC (3, Static 2, 3) (1, 0)
    & setSpawnAt (NearestLocationToYou $ oneOf ["Casino Lounge", "Security Office"])

instance HasAbilities SecurityPatrolC where
  getAbilities (SecurityPatrolC a) =
    extend1 a
      $ restricted a 1 (DuringPhase #enemy)
      $ forced
      $ EnemyMovedTo #after YourLocation MovedViaAny (be a)

instance RunMessage SecurityPatrolC where
  runMessage msg e@(SecurityPatrolC attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      raiseAlarmLevel (attrs.ability 1) =<< select (InvestigatorAt (locationWithEnemy attrs))
      pure e
    _ -> SecurityPatrolC <$> liftRunMessage msg attrs
