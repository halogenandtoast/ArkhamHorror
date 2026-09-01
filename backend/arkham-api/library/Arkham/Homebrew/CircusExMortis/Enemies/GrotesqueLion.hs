module Arkham.Homebrew.CircusExMortis.Enemies.GrotesqueLion (grotesqueLion) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Modifier

newtype GrotesqueLion = GrotesqueLion EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grotesqueLion :: EnemyCard GrotesqueLion
grotesqueLion = enemy GrotesqueLion Cards.grotesqueLion

instance HasAbilities GrotesqueLion where
  getAbilities (GrotesqueLion a) = extend1 a $ mkAbility a 1 $ silent $ EnemyEntersPlay #after (be a)

instance RunMessage GrotesqueLion where
  runMessage msg e@(GrotesqueLion attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) attrs (EnemyFight 1)
      roundModifier (attrs.ability 1) attrs (EnemyEvade (-1))
      pure e
    _ -> GrotesqueLion <$> liftRunMessage msg attrs
