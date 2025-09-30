module Arkham.Enemy.Cards.GavriellaMizrah (gavriellaMizrah) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype GavriellaMizrah = GavriellaMizrah EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gavriellaMizrah :: EnemyCard GavriellaMizrah
gavriellaMizrah = enemy GavriellaMizrah Cards.gavriellaMizrah (5, Static 4, 2) (2, 0)

instance HasAbilities GavriellaMizrah where
  getAbilities (GavriellaMizrah a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyAttackedSuccessfully #after You AnySource (be a)

instance RunMessage GavriellaMizrah where
  runMessage msg e@(GavriellaMizrah attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> GavriellaMizrah <$> liftRunMessage msg attrs
