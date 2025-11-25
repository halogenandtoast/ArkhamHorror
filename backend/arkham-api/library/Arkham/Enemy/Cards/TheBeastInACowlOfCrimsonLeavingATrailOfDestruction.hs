module Arkham.Enemy.Cards.TheBeastInACowlOfCrimsonLeavingATrailOfDestruction (theBeastInACowlOfCrimsonLeavingATrailOfDestruction)
where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype TheBeastInACowlOfCrimsonLeavingATrailOfDestruction
  = TheBeastInACowlOfCrimsonLeavingATrailOfDestruction EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBeastInACowlOfCrimsonLeavingATrailOfDestruction
  :: EnemyCard TheBeastInACowlOfCrimsonLeavingATrailOfDestruction
theBeastInACowlOfCrimsonLeavingATrailOfDestruction =
  enemy
    TheBeastInACowlOfCrimsonLeavingATrailOfDestruction
    Cards.theBeastInACowlOfCrimsonLeavingATrailOfDestruction
    (3, Static 5, 4)
    (2, 2)
    & setSpawnAt (FarthestLocationFromYou Anywhere)

instance HasAbilities TheBeastInACowlOfCrimsonLeavingATrailOfDestruction where
  getAbilities (TheBeastInACowlOfCrimsonLeavingATrailOfDestruction a) =
    extend1 a
      $ restricted a 1 (thisExists a $ ReadyEnemy <> EnemyWithAnyScarletKey)
      $ forced
      $ SkillTestResult #after You (SkillTestAt $ locationWithEnemy a) #failure

instance RunMessage TheBeastInACowlOfCrimsonLeavingATrailOfDestruction where
  runMessage msg e@(TheBeastInACowlOfCrimsonLeavingATrailOfDestruction attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
      chooseOneAtATimeM iid $ targets skeys shift
      pure e
    _ -> TheBeastInACowlOfCrimsonLeavingATrailOfDestruction <$> liftRunMessage msg attrs
