module Arkham.Act.Cards.NewWorldOrder (newWorldOrder) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype NewWorldOrder = NewWorldOrder ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newWorldOrder :: ActCard NewWorldOrder
newWorldOrder = act (2, A) NewWorldOrder Cards.newWorldOrder Nothing

instance HasAbilities NewWorldOrder where
  getAbilities (NewWorldOrder x) =
    withBaseAbilities
      x
      [ fastAbility x 1 Free $ if maybe False (>= 3) (actBreaches x) then NoRestriction else Never
      , mkAbility x 2
          $ Objective
          $ forced
          $ Matcher.EnemyDefeated #after Anyone ByAny
          $ enemyIs Enemies.carlSanfordDeathlessFanatic
      ]

instance RunMessage NewWorldOrder where
  runMessage msg a@(NewWorldOrder attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      location <- sampleLocation
      push $ RemoveBreaches (toTarget attrs) 3
      placeClues (attrs.ability 1) location 1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      selectEach (enemyIs Enemies.carlSanfordDeathlessFanatic) addToVictory
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> NewWorldOrder <$> liftRunMessage msg attrs
