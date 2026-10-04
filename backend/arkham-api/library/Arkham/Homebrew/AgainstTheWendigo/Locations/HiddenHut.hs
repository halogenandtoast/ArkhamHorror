module Arkham.Homebrew.AgainstTheWendigo.Locations.HiddenHut (hiddenHut) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HiddenHut = HiddenHut LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenHut :: LocationCard HiddenHut
hiddenHut = location HiddenHut Cards.hiddenHut 3 (PerPlayer 1)

instance HasModifiersFor HiddenHut where
  -- The unrevealed side prints "You cannot move into this location."
  getModifiersFor (HiddenHut a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance RunMessage HiddenHut where
  runMessage msg (HiddenHut attrs) = runQueueT $ HiddenHut <$> liftRunMessage msg attrs
