-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Treacheries.InHotPursuit (inHotPursuit) where

import Arkham.Homebrew.NightOfVespers.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InHotPursuit = InHotPursuit TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inHotPursuit :: TreacheryCard InHotPursuit
inHotPursuit = treachery InHotPursuit Cards.inHotPursuit

instance RunMessage InHotPursuit where
  runMessage msg (InHotPursuit attrs) = InHotPursuit <$> runMessage msg attrs
