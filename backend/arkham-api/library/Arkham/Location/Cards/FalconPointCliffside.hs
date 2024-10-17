module Arkham.Location.Cards.FalconPointCliffside (falconPointCliffside, FalconPointCliffside (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype FalconPointCliffside = FalconPointCliffside LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falconPointCliffside :: LocationCard FalconPointCliffside
falconPointCliffside = location FalconPointCliffside Cards.falconPointCliffside 1 (PerPlayer 1)

instance HasModifiersFor FalconPointCliffside where
  getModifiersFor target (FalconPointCliffside attrs) = preventDrawConnections target attrs

instance HasAbilities FalconPointCliffside where
  getAbilities (FalconPointCliffside attrs) =
    extendRevealed attrs []

instance RunMessage FalconPointCliffside where
  runMessage msg l@(FalconPointCliffside attrs) = runQueueT $ case msg of
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> FalconPointCliffside <$> liftRunMessage msg attrs
