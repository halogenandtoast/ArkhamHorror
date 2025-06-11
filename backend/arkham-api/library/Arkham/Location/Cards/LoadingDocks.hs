module Arkham.Location.Cards.LoadingDocks (loadingDocks) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LoadingDocks = LoadingDocks LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loadingDocks :: LocationCard LoadingDocks
loadingDocks = location LoadingDocks Cards.loadingDocks 4 (PerPlayer 2)

instance HasAbilities LoadingDocks where
  getAbilities (LoadingDocks attrs) =
    extendRevealed attrs []

instance RunMessage LoadingDocks where
  runMessage msg (LoadingDocks attrs) = runQueueT $ case msg of
    _ -> LoadingDocks <$> liftRunMessage msg attrs
