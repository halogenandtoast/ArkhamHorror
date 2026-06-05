module Arkham.Treachery.Cards.StickyFeet (stickyFeet) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StickyFeet = StickyFeet TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stickyFeet :: TreacheryCard StickyFeet
stickyFeet = treachery StickyFeet Cards.stickyFeet

instance RunMessage StickyFeet where
  runMessage msg (StickyFeet attrs) = StickyFeet <$> runMessage msg attrs
