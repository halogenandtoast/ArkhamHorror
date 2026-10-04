module Arkham.Homebrew.AgainstTheWendigo.Locations.ImpenetrableForest (impenetrableForest) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ImpenetrableForest = ImpenetrableForest LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

impenetrableForest :: LocationCard ImpenetrableForest
impenetrableForest = location ImpenetrableForest Cards.impenetrableForest 4 (PerPlayer 2)

instance HasModifiersFor ImpenetrableForest where
  -- The unrevealed side prints "You cannot move into this location."
  getModifiersFor (ImpenetrableForest a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance RunMessage ImpenetrableForest where
  runMessage msg (ImpenetrableForest attrs) = runQueueT $ ImpenetrableForest <$> liftRunMessage msg attrs
