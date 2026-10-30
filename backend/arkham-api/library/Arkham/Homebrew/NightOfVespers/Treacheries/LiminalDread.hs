-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Treacheries.LiminalDread (liminalDread) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LiminalDread = LiminalDread TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liminalDread :: TreacheryCard LiminalDread
liminalDread = treachery LiminalDread Cards.liminalDread

instance RunMessage LiminalDread where
  runMessage msg (LiminalDread attrs) = LiminalDread <$> runMessage msg attrs
