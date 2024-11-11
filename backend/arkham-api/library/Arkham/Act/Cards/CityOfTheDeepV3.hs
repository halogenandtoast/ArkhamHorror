module Arkham.Act.Cards.CityOfTheDeepV3 (CityOfTheDeepV3 (..), cityOfTheDeepV3) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CityOfTheDeepV3 = CityOfTheDeepV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheDeepV3 :: ActCard CityOfTheDeepV3
cityOfTheDeepV3 = act (1, A) CityOfTheDeepV3 Cards.cityOfTheDeepV3 Nothing

instance HasAbilities CityOfTheDeepV3 where
  getAbilities (CityOfTheDeepV3 a) =
    extend
      a
      [ restricted
          a
          1
          ( exists (EnemyWithDamage (AtLeast $ PerPlayer 4) <> enemyIs Enemies.hydraAwakenedAndEnraged)
              <> exists
                (EnemyWithDamage (AtLeast $ PerPlayer 4) <> enemyIs Enemies.dagonAwakenedAndEnragedIntoTheMaelstrom)
          )
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage CityOfTheDeepV3 where
  runMessage msg a@(CityOfTheDeepV3 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      addToVictory attrs
      otherActs <- selectAny $ NotAct $ ActWithId attrs.id
      if otherActs
        then do
          lead <- getLead
          chooseOneM lead do
            labeled "Continue playing" nothing
            labeled "Proceed immediately to (→R1)" $ push R1
        else push R1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    _ -> CityOfTheDeepV3 <$> liftRunMessage msg attrs
