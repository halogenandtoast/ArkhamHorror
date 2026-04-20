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
        extend1 a $ forcedAbility a 1 $ EnemyAttacks #when Anyone AnyEnemyAttack (be a)

instance RunMessage HeraldOfFlame where
  runMessage msg e@(HeraldOfFlame attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
        x <- selectCount $ EnemyAt (locationWithEnemy attrs)
        healDamage attrs (attrs.ability 1) x
        pure e
    _ -> HeraldOfFlame <$> liftRunMessage msg attrs
