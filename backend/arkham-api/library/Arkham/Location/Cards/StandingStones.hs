module Arkham.Location.Cards.StandingStones (standingStones) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype StandingStones = StandingStones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

standingStones :: LocationCard StandingStones
standingStones = location StandingStones Cards.standingStones 2 (PerPlayer 4)

instance HasAbilities StandingStones where
  getAbilities (StandingStones attrs) =
    extendRevealed attrs []

instance RunMessage StandingStones where
  runMessage msg (StandingStones attrs) = runQueueT $ case msg of
    _ -> StandingStones <$> liftRunMessage msg attrs
