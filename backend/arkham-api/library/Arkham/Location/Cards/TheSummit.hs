module Arkham.Location.Cards.TheSummit (theSummit) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheSummit = TheSummit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSummit :: LocationCard TheSummit
theSummit = location TheSummit Cards.theSummit 3 (PerPlayer 3)

instance HasAbilities TheSummit where
  getAbilities (TheSummit attrs) =
    extendRevealed attrs []

instance RunMessage TheSummit where
  runMessage msg (TheSummit attrs) = runQueueT $ case msg of
    _ -> TheSummit <$> liftRunMessage msg attrs
