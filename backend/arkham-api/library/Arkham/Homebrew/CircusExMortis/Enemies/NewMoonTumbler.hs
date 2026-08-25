module Arkham.Homebrew.CircusExMortis.Enemies.NewMoonTumbler (newMoonTumbler) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Enemy.Types.Attrs (enemyDoom)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype NewMoonTumbler = NewMoonTumbler EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newMoonTumbler :: EnemyCard NewMoonTumbler
newMoonTumbler = enemy NewMoonTumbler Cards.newMoonTumbler

instance HasModifiersFor NewMoonTumbler where
  getModifiersFor (NewMoonTumbler a) = do
    let doom = enemyDoom a
    modifySelf a
      $ [AddKeyword Keyword.Hunter, AddKeyword Keyword.Retaliate]
      <> [HorrorDealt 1 | doom >= 1]
      <> [DamageDealt 1 | doom >= 2]

instance HasAbilities NewMoonTumbler where
  getAbilities (NewMoonTumbler a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage NewMoonTumbler where
  runMessage msg e@(NewMoonTumbler attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> NewMoonTumbler <$> liftRunMessage msg attrs
