module Arkham.Enemy.Cards.HeraldOfFlame (heraldOfFlame) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype HeraldOfFlame = HeraldOfFlame EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heraldOfFlame :: EnemyCard HeraldOfFlame
heraldOfFlame = enemy HeraldOfFlame Cards.heraldOfFlame (4, Static 5, 2) (0, 2)

instance HasAbilities HeraldOfFlame where
  getAbilities (HeraldOfFlame a) =
    extend1 a
      $ restricted a 1 (exists $ EnemyAt (locationWithEnemy a) <> EnemyWithAnyDamage)
      $ forced
      $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage HeraldOfFlame where
  runMessage msg e@(HeraldOfFlame attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      selectEach (EnemyAt (locationWithEnemy attrs) <> EnemyWithAnyDamage)
        $ healDamageOn (attrs.ability 1) 1
      pure e
    _ -> HeraldOfFlame <$> liftRunMessage msg attrs
