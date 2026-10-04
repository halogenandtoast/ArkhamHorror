module Arkham.Homebrew.AgainstTheWendigo.Locations.Swamp (swamp) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Swamp = Swamp LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

swamp :: LocationCard Swamp
swamp = location Swamp Cards.swamp 3 (PerPlayer 2)

instance HasModifiersFor Swamp where
  -- The unrevealed side prints "You cannot move into this location."
  getModifiersFor (Swamp a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance RunMessage Swamp where
  runMessage msg (Swamp attrs) = runQueueT $ Swamp <$> liftRunMessage msg attrs
