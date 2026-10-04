module Arkham.Homebrew.AgainstTheWendigo.Locations.MadProspector (madProspector) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MadProspector = MadProspector LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madProspector :: LocationCard MadProspector
madProspector = location MadProspector Cards.madProspector 3 (Static 0)

instance HasModifiersFor MadProspector where
  -- The unrevealed Mountain Range prints "You cannot move into the Mountain Range."
  getModifiersFor (MadProspector a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance RunMessage MadProspector where
  runMessage msg (MadProspector attrs) = runQueueT $ MadProspector <$> liftRunMessage msg attrs
