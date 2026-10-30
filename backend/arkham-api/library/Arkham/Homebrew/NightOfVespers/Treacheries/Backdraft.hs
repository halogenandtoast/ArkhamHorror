-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Treacheries.Backdraft (backdraft) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Backdraft = Backdraft TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backdraft :: TreacheryCard Backdraft
backdraft = treachery Backdraft Cards.backdraft

instance RunMessage Backdraft where
  runMessage msg (Backdraft attrs) = Backdraft <$> runMessage msg attrs
