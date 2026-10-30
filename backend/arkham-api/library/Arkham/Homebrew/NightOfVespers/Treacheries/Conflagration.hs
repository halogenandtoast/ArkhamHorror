-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Treacheries.Conflagration (conflagration) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Conflagration = Conflagration TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conflagration :: TreacheryCard Conflagration
conflagration = treachery Conflagration Cards.conflagration

instance RunMessage Conflagration where
  runMessage msg (Conflagration attrs) = Conflagration <$> runMessage msg attrs
