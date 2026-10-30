-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Treacheries.CrumblingFoundations (crumblingFoundations) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CrumblingFoundations = CrumblingFoundations TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crumblingFoundations :: TreacheryCard CrumblingFoundations
crumblingFoundations = treachery CrumblingFoundations Cards.crumblingFoundations

instance RunMessage CrumblingFoundations where
  runMessage msg (CrumblingFoundations attrs) = CrumblingFoundations <$> runMessage msg attrs
