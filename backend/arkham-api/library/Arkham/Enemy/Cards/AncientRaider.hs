module Arkham.Enemy.Cards.AncientRaider (ancientRaider) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Scenarios.DeadHeat.Helpers
import Arkham.Token

newtype AncientRaider = AncientRaider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientRaider :: EnemyCard AncientRaider
ancientRaider = enemy AncientRaider Cards.ancientRaider (3, Static 3, 2) (1, 0)

instance HasAbilities AncientRaider where
  getAbilities (AncientRaider a) =
    extend1 a
      $ restricted a 1 (exists $ YourLocation <> LocationWithToken Civilian)
      $ forced
      $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage AncientRaider where
  runMessage msg e@(AncientRaider attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid slayCivilian
      pure e
    _ -> AncientRaider <$> liftRunMessage msg attrs
