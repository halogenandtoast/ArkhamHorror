-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Treacheries.CatastrophicDrop (catastrophicDrop) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CatastrophicDrop = CatastrophicDrop TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catastrophicDrop :: TreacheryCard CatastrophicDrop
catastrophicDrop = treachery CatastrophicDrop Cards.catastrophicDrop

instance RunMessage CatastrophicDrop where
  runMessage msg (CatastrophicDrop attrs) = CatastrophicDrop <$> runMessage msg attrs
