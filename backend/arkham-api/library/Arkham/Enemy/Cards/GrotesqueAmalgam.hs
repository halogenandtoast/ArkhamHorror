module Arkham.Enemy.Cards.GrotesqueAmalgam (grotesqueAmalgam) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Ref
import Arkham.Helpers.Window (attackSource)
import Arkham.Matcher
import Arkham.Trait (Trait (Weapon))

newtype GrotesqueAmalgam = GrotesqueAmalgam EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grotesqueAmalgam :: EnemyCard GrotesqueAmalgam
grotesqueAmalgam = enemy GrotesqueAmalgam Cards.grotesqueAmalgam

instance HasAbilities GrotesqueAmalgam where
  getAbilities (GrotesqueAmalgam a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyAttacked #after You (SourceWithTrait Weapon) (be a)

instance RunMessage GrotesqueAmalgam where
  runMessage msg e@(GrotesqueAmalgam attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (attackSource -> source) _ -> do
      placeDoom (attrs.ability 1) (sourceToTarget source) 1
      pure e
    _ -> GrotesqueAmalgam <$> liftRunMessage msg attrs
