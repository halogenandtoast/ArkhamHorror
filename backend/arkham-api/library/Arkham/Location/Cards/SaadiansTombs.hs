module Arkham.Location.Cards.SaadiansTombs (saadiansTombs) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SaadiansTombs = SaadiansTombs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saadiansTombs :: LocationCard SaadiansTombs
saadiansTombs = symbolLabel $ location SaadiansTombs Cards.saadiansTombs 1 (PerPlayer 3)

instance HasAbilities SaadiansTombs where
  getAbilities (SaadiansTombs attrs) =
    extendRevealed attrs []

instance RunMessage SaadiansTombs where
  runMessage msg (SaadiansTombs attrs) = runQueueT $ case msg of
    _ -> SaadiansTombs <$> liftRunMessage msg attrs
