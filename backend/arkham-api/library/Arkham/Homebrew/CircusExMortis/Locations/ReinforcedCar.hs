module Arkham.Homebrew.CircusExMortis.Locations.ReinforcedCar (reinforcedCar) where

import Arkham.Ability
import Arkham.Helpers.Window.Enemy (getEnemy)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype ReinforcedCar = ReinforcedCar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reinforcedCar :: LocationCard ReinforcedCar
reinforcedCar = location ReinforcedCar Cards.reinforcedCar 3 (Static 2)

instance HasAbilities ReinforcedCar where
  getAbilities (ReinforcedCar a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithoutClues)
      $ freeReaction (EnemyAttacks #when You AnyEnemyAttack AnyEnemy)

instance RunMessage ReinforcedCar where
  runMessage msg l@(ReinforcedCar attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEnemy -> enemy) _ -> do
      chooseOneM iid $ scenarioI18n "allPointsWest" $ scope "reinforcedCar" do
        labeled "reduceDamage" $ enemyAttackModifier (attrs.ability 1) enemy (DamageDealt (-1))
        labeled "reduceHorror" $ enemyAttackModifier (attrs.ability 1) enemy (HorrorDealt (-1))
      pure l
    _ -> ReinforcedCar <$> liftRunMessage msg attrs
