module Arkham.Location.Cards.StageOfTheWardTheatre (stageOfTheWardTheatre) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype StageOfTheWardTheatre = StageOfTheWardTheatre LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stageOfTheWardTheatre :: LocationCard StageOfTheWardTheatre
stageOfTheWardTheatre = location StageOfTheWardTheatre Cards.stageOfTheWardTheatre 0 (Static 0)

instance HasAbilities StageOfTheWardTheatre where
  getAbilities (StageOfTheWardTheatre attrs) =
    extendRevealed attrs []

instance RunMessage StageOfTheWardTheatre where
  runMessage msg (StageOfTheWardTheatre attrs) = runQueueT $ case msg of
    _ -> StageOfTheWardTheatre <$> liftRunMessage msg attrs
