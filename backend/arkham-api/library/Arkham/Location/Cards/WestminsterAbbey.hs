module Arkham.Location.Cards.WestminsterAbbey (westminsterAbbey, WestminsterAbbey(..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.RiddlesAndRain.Helpers (scenarioI18n)

newtype WestminsterAbbey = WestminsterAbbey LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westminsterAbbey :: LocationCard WestminsterAbbey
westminsterAbbey = location WestminsterAbbey Cards.westminsterAbbey 1 (PerPlayer 1)

instance HasAbilities WestminsterAbbey where
  getAbilities (WestminsterAbbey a) =
    extendRevealed
      a
      [ scenarioI18n
          $ skillTestAbility
          $ groupLimit PerGame
          $ restricted a 1 Here
          $ ActionAbility [Action.Parley] (ActionCost 1)
      ]

instance RunMessage WestminsterAbbey where
  runMessage msg l@(WestminsterAbbey attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 1)
      pure l
    _ -> WestminsterAbbey <$> liftRunMessage msg attrs
