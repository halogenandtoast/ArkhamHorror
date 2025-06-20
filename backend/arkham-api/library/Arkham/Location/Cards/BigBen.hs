module Arkham.Location.Cards.BigBen (bigBen, BigBen(..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.RiddlesAndRain.Helpers (scenarioI18n)

newtype BigBen = BigBen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bigBen :: LocationCard BigBen
bigBen = location BigBen Cards.bigBen 4 (PerPlayer 1)

instance HasAbilities BigBen where
  getAbilities (BigBen a) =
    extendRevealed
      a
      [ scenarioI18n
          $ skillTestAbility
          $ groupLimit PerTurn
          $ restricted a 1 Here
          $ FastAbility Free
      ]

instance RunMessage BigBen where
  runMessage msg l@(BigBen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      pure l
    _ -> BigBen <$> liftRunMessage msg attrs
