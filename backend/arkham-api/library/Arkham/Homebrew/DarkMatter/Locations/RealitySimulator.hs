module Arkham.Homebrew.DarkMatter.Locations.RealitySimulator (realitySimulator) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype RealitySimulator = RealitySimulator LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "(Reminder - Reality Simulator is not in play while there is a card on top of
it)" — purely a reminder; the covering card owns that behaviour.
-}
realitySimulator :: LocationCard RealitySimulator
realitySimulator = symbolLabel $ location RealitySimulator Cards.realitySimulator 3 (Static 0)

instance RunMessage RealitySimulator where
  runMessage msg (RealitySimulator attrs) = RealitySimulator <$> runMessage msg attrs
