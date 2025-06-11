module Arkham.Location.Cards.TheHastingsEstate (theHastingsEstate) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheHastingsEstate = TheHastingsEstate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHastingsEstate :: LocationCard TheHastingsEstate
theHastingsEstate = location TheHastingsEstate Cards.theHastingsEstate 3 (PerPlayer 2)

instance HasAbilities TheHastingsEstate where
  getAbilities (TheHastingsEstate attrs) =
    extendRevealed attrs []

instance RunMessage TheHastingsEstate where
  runMessage msg (TheHastingsEstate attrs) = runQueueT $ case msg of
    _ -> TheHastingsEstate <$> liftRunMessage msg attrs
