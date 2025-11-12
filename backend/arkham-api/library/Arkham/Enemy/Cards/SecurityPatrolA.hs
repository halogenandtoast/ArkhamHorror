module Arkham.Enemy.Cards.SecurityPatrolA (securityPatrolA) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype SecurityPatrolA = SecurityPatrolA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityPatrolA :: EnemyCard SecurityPatrolA
securityPatrolA =
  enemy SecurityPatrolA Cards.securityPatrolA (3, Static 2, 3) (1, 0)
    & setSpawnAt (NearestLocationToYou $ oneOf ["Casino Lounge", "Security Office"])

instance HasAbilities SecurityPatrolA where
  getAbilities (SecurityPatrolA a) =
    extend1 a
      $ restricted a 1 (DuringPhase #enemy)
      $ forced
      $ EnemyMovedTo #after YourLocation MovedViaAny (be a)

instance RunMessage SecurityPatrolA where
  runMessage msg e@(SecurityPatrolA attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      raiseAlarmLevel (attrs.ability 1) =<< select (InvestigatorAt (locationWithEnemy attrs))
      pure e
    _ -> SecurityPatrolA <$> liftRunMessage msg attrs
