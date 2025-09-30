module Arkham.Act.Cards.BeyondTheGrave (beyondTheGrave) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype BeyondTheGrave = BeyondTheGrave ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beyondTheGrave :: ActCard BeyondTheGrave
beyondTheGrave = act (2, A) BeyondTheGrave Cards.beyondTheGrave Nothing

instance HasAbilities BeyondTheGrave where
  getAbilities (BeyondTheGrave x) =
    extend
      x
      [ fastAbility x 1 Free $ if maybe False (>= 3) (actBreaches x) then NoRestriction else Never
      , mkAbility x 2
          $ Objective
          $ forced
          $ Matcher.EnemyDefeated #after Anyone ByAny
          $ enemyIs Enemies.anetteMasonReincarnatedEvil
      ]

instance RunMessage BeyondTheGrave where
  runMessage msg a@(BeyondTheGrave attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      location <- sampleLocation
      removeBreaches attrs 3
      placeClues (attrs.ability 1) location 1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> BeyondTheGrave <$> liftRunMessage msg attrs
