module Arkham.Homebrew.CircusExMortis.Locations.ReinforcedCar (reinforcedCar) where

import Arkham.Ability
import Arkham.Enemy.Types (Field (EnemyHealthDamage, EnemySanityDamage))
import Arkham.Helpers.Window.Enemy (getEnemy)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype ReinforcedCar = ReinforcedCar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reinforcedCar :: LocationCard ReinforcedCar
reinforcedCar = symbolLabel $ location ReinforcedCar Cards.reinforcedCar 3 (Static 2)

instance HasAbilities ReinforcedCar where
  getAbilities (ReinforcedCar a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> thisExists a LocationWithoutClues <> exists (AttackingEnemy <> EnemyDealsDamageOrHorror))
      $ freeReaction (EnemyAttacks #when You AnyEnemyAttack AnyEnemy)

instance RunMessage ReinforcedCar where
  runMessage msg l@(ReinforcedCar attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEnemy -> enemy) _ -> do
      -- nothing to reduce if the attack does not deal that kind of damage
      damage <- field EnemyHealthDamage enemy
      horror <- field EnemySanityDamage enemy
      chooseOrRunOneM iid $ scenarioI18n "allPointsWest" $ scope "reinforcedCar" do
        when (damage > 0)
          $ labeled "reduceDamage"
          $ enemyAttackModifier (attrs.ability 1) enemy (DamageDealt (-1))
        when (horror > 0)
          $ labeled "reduceHorror"
          $ enemyAttackModifier (attrs.ability 1) enemy (HorrorDealt (-1))
      pure l
    _ -> ReinforcedCar <$> liftRunMessage msg attrs
