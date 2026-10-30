-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Treacheries.RainOfBullets (rainOfBullets) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RainOfBullets = RainOfBullets TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rainOfBullets :: TreacheryCard RainOfBullets
rainOfBullets = treachery RainOfBullets Cards.rainOfBullets

instance RunMessage RainOfBullets where
  runMessage msg (RainOfBullets attrs) = RainOfBullets <$> runMessage msg attrs
