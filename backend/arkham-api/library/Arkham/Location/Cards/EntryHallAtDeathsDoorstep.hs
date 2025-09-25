module Arkham.Location.Cards.EntryHallAtDeathsDoorstep (entryHallAtDeathsDoorstep) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.AtDeathsDoorstep.Helpers

newtype EntryHallAtDeathsDoorstep = EntryHallAtDeathsDoorstep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entryHallAtDeathsDoorstep :: LocationCard EntryHallAtDeathsDoorstep
entryHallAtDeathsDoorstep = location EntryHallAtDeathsDoorstep Cards.entryHallAtDeathsDoorstep 3 (Static 0)

instance HasAbilities EntryHallAtDeathsDoorstep where
  getAbilities (EntryHallAtDeathsDoorstep a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "entryHall.resign" (locationResignAction a)

instance RunMessage EntryHallAtDeathsDoorstep where
  runMessage msg (EntryHallAtDeathsDoorstep attrs) =
    EntryHallAtDeathsDoorstep <$> runMessage msg attrs
