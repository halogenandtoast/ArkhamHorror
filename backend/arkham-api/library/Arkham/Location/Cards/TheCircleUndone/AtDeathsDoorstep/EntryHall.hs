module Arkham.Location.Cards.TheCircleUndone.AtDeathsDoorstep.EntryHall (entryHall) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.CardDefs.TheCircleUndone.AtDeathsDoorstep qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.TheCircleUndone.AtDeathsDoorstep.Helpers

newtype EntryHall = EntryHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entryHall :: LocationCard EntryHall
entryHall = location EntryHall Cards.entryHall 3 (Static 0)

instance HasAbilities EntryHall where
  getAbilities (EntryHall a) =
    extendRevealed1 a $ scenarioI18n $ withI18nTooltip "entryHall.resign" (locationResignAction a)

instance RunMessage EntryHall where
  runMessage msg (EntryHall attrs) =
    EntryHall <$> runMessage msg attrs
