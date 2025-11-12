module Arkham.Enemy.Cards.SecurityPatrolB (securityPatrolB) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype SecurityPatrolB = SecurityPatrolB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityPatrolB :: EnemyCard SecurityPatrolB
securityPatrolB =
  enemy SecurityPatrolB Cards.securityPatrolB (3, Static 2, 3) (1, 0)
    & setSpawnAt (NearestLocationToYou $ oneOf ["Casino Lounge", "Security Office"])

instance HasAbilities SecurityPatrolB where
  getAbilities (SecurityPatrolB a) =
    extend1 a
      $ restricted a 1 (DuringPhase #enemy)
      $ forced
      $ EnemyMovedTo #after YourLocation MovedViaAny (be a)

instance RunMessage SecurityPatrolB where
  runMessage msg e@(SecurityPatrolB attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      raiseAlarmLevel (attrs.ability 1) =<< select (InvestigatorAt (locationWithEnemy attrs))
      pure e
    _ -> SecurityPatrolB <$> liftRunMessage msg attrs
