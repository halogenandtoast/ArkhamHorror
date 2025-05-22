module Arkham.Location.Cards.RecessesOfYourOwnMind (recessesOfYourOwnMind) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RecessesOfYourOwnMind = RecessesOfYourOwnMind LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recessesOfYourOwnMind :: LocationCard RecessesOfYourOwnMind
recessesOfYourOwnMind = location RecessesOfYourOwnMind Cards.recessesOfYourOwnMind 3 (PerPlayer 2)

instance HasAbilities RecessesOfYourOwnMind where
  getAbilities (RecessesOfYourOwnMind attrs) =
    extendRevealed attrs []

instance RunMessage RecessesOfYourOwnMind where
  runMessage msg (RecessesOfYourOwnMind attrs) = runQueueT $ case msg of
    _ -> RecessesOfYourOwnMind <$> liftRunMessage msg attrs
