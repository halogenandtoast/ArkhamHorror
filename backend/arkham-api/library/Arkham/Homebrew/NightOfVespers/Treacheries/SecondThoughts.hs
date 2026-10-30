-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Treacheries.SecondThoughts (secondThoughts) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SecondThoughts = SecondThoughts TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondThoughts :: TreacheryCard SecondThoughts
secondThoughts = treachery SecondThoughts Cards.secondThoughts

instance RunMessage SecondThoughts where
  runMessage msg (SecondThoughts attrs) = SecondThoughts <$> runMessage msg attrs
