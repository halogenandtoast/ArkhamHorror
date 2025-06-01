module Arkham.Location.Cards.EntrywayRearrangedByTime (entrywayRearrangedByTime) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype EntrywayRearrangedByTime = EntrywayRearrangedByTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entrywayRearrangedByTime :: LocationCard EntrywayRearrangedByTime
entrywayRearrangedByTime = location EntrywayRearrangedByTime Cards.entrywayRearrangedByTime 2 (PerPlayer 1)

instance HasAbilities EntrywayRearrangedByTime where
  getAbilities (EntrywayRearrangedByTime attrs) =
    extendRevealed attrs []

instance RunMessage EntrywayRearrangedByTime where
  runMessage msg (EntrywayRearrangedByTime attrs) = runQueueT $ case msg of
    _ -> EntrywayRearrangedByTime <$> liftRunMessage msg attrs
