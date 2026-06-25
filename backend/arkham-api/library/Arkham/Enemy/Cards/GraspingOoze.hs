module Arkham.Enemy.Cards.GraspingOoze (graspingOoze) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Ref (sourceToMaybeCard)
import Arkham.Helpers.Window (attackSource)
import Arkham.Matcher
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Trait (Trait (Melee, Oozified))

newtype GraspingOoze = GraspingOoze EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graspingOoze :: EnemyCard GraspingOoze
graspingOoze =
  enemy GraspingOoze Cards.graspingOoze
    & setSpawnAt (NearestLocationToYou $ LocationWithTrait Oozified)

instance HasAbilities GraspingOoze where
  getAbilities (GraspingOoze a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacked #after You (SourceWithTrait Melee) (be a)

instance RunMessage GraspingOoze where
  runMessage msg e@(GraspingOoze attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (attackSource -> source) _ -> do
      sourceToMaybeCard source >>= traverse_ \card -> devour [card]
      pure e
    _ -> GraspingOoze <$> liftRunMessage msg attrs
